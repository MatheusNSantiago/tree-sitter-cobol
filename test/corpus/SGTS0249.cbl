      *-----------------------------------------------------------------
      * Programa..: SGTS0249
      * Analista..: F8785286 - Roseli Akemi Kawahata
      * Autor.....: F8785286 - Roseli Akemi Kawahata
      * Data......: 20/05/2019
      * Objetivo..: Atualização de Código Arquivo GED da Ordem Negocia-
      *               ção de Compra.
      * Compilacao: Cobol/CICS
      *-----------------------------------------------------------------
      * VRS003 09/11/2020 - F8650584 - DE 2020/155748 -
      *                     Ajuste Monitoração de Variáveis
      *                     F2020988 - DE 2020/155748 -
      *                     Alteração no Book do Tamanho da Variável:
      *                     SGTS0249-CD-ARQ-OPR-ETRD
      * VRS002 18/10/2019 - F8785286 - DE 2016/145195 -
      *                     Recompilação Alteração no Book SGTKLA75.
      *                     Sem Alterações neste Programa.
      *        19/09/2019 - F8785286 DE 2016/145195 - Re-compilação por
      *                     Recompilação devido  Alterações nos Books
      *                     SGTKLA75 e SGTKA075.
      *                     Sem Alterações neste Programa.
      * VRS001 20/05/2019 - F8785286 - DE 2016/145195 - Implantação
      *        28/06/2019 - F8090555 - Inibe validação CD-ARQ-OPR-ETRD
      *                     menor ou igual a ZERO.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTS0249.

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
                                       '*** S.S. Comeca Aqui ***'.
       77  CTE-PROG                    PIC  X(18) VALUE
                                       '*** SGTS0249 ***'.
       77  CTE-VERS                    PIC  X(06) VALUE 'VRS003'.

      *--> Definição dos Nomes dos Programas Acionados
       01  TAB-PROGRAMAS-CHAMADOS.
           03  SGTSLA75                PIC  X(08) VALUE 'SGTSLA75'.
           03  SGTSA075                PIC  X(08) VALUE 'SGTSA075'.

      *--> Definição dos Variaveis do Cobolito
-INC ALMKCBL1

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

      *--> Areas de trabalho

      *--> Auxiliares para movimentação na monitoração de variáveis     .
       01  CD-CTRA-TVM-TX.
           03  CD-CTRA-TVM-DISP        PIC  9(9).

       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP-DISP    PIC  9(11).

       01  TX-MSG-ORD-NEGO             PIC X(600).
       01  TX-MSG-ORD-NEGO-R           REDEFINES TX-MSG-ORD-NEGO.
           03 LOCAL                    PIC  X(50).
           03 QT-TAB-ITEM              PIC  9(03).
           03 PRIM-DELIM               PIC  X(01).
           03 TAB-OCR                  OCCURS 100 TIMES.
              05 TAB-ITEM              PIC  9(03).
              05 DELIM-OCC             PIC  X(01).

       01  CPOS-AUX.
           03  IC-TAB                  PIC S9(4) COMP-5.
           03  VL-PU-PAPL-VND          PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99.
           03  VL-TAXA-NEGO-VND        PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99.

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

000820* Book do container de Servicos (para acionamento de servicos)
-INC IIBKSRVC
000420
      *--> Book para captura de abend cics da bloco codigo HLPKW999
-INC HLPKW999

      *--> Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1

      *--> Book para captura de erro via MSG1
-INC MSGKWMG1

      *--> Book do container de requisição e resposta deste modulo
       01 SGTS0249-DADOS.
-INC SGTK0249

      *-----------------------------------------------------------------
      * Book dos Containers e das subrotinas chamadas
      *-----------------------------------------------------------------
      * Alteração Ordem de Negociação - DB2SGT.ORD_NEGO_OPR
-INC SGTKA075

      * Lista Ordens de Negociação - DB2SGT.ORD_NEGO_OPR - SGTSLA75
-INC SGTKLA75

      * Verificar em qual estado está a operação -  SGTST009
       01 SGTST009-DADOS.
-INC SGTKT009

       77  CTE-FINAL-SS                PIC X(40) VALUE
                                       '*** S.S. Termina Aqui ***'.

      *-----------------------------------------------------------------
       PROCEDURE                       DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------

      *--> Habilita Testes no Cobolito e Debug de Operacoes IIB
-INC ALMKCBL2

      *--> Habilita captura automatica de abend e erros CICS
-INC HLPKE999

           PERFORM  100000-INICIAR

           PERFORM  200000-PROCESSAR

           PERFORM  999999-ENCERRAR

           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-INICIAR                  SECTION.
      *-----------------------------------------------------------------
           MOVE  0001                  TO  MSG1-CD-SEQL-ERRO

           PERFORM  970000-LER-CANAL

      *--> Define o nome do programa p/ fins manipulacao container
           MOVE  'SGTS0249'            TO  NM-PGM

      *--> Limpa a resposta e erro antes de executar o modulo
           PERFORM  983000-EXCLUIR-RESPOSTA-ERRO

           PERFORM  971000-LER-REQUISICAO

           PERFORM  110000-VALIDAR-REQUISICAO
           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-VALIDAR-REQUISICAO       SECTION.
      *--> Valida Campo a Campo da Requisicao
      *-----------------------------------------------------------------
           MOVE  0002                  TO  MSG1-CD-SEQL-ERRO

      *--> Validar Chave da Operação Estruturada
           IF  SGTS0249-CD-CTRA-TVM        LESS  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0001              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' inválido.'      TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0249-CD-CTRA-TVM        EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0002              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0249-CD-OPR-ETRD-MCP    NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0003              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada do mercado de capit
      -              'ais não numérico.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0249-CD-OPR-ETRD-MCP    LESS  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0004              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada do mercado de capit
      -              'ais inválido.'   TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0249-CD-OPR-ETRD-MCP    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0005              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada do mercado de capit
      -              'ais não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0249-NR-VRS-OPR-ETRD    LESS  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0006              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação estruturada inválido.
      -              ''                TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0249-NR-VRS-OPR-ETRD    EQUAL  ZEROS
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

           PERFORM  210000-BUSCAR-ORD-NEGO

           PERFORM  220000-ATUALIZAR-ORD-NEGO
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       210000-BUSCAR-ORD-NEGO          SECTION.
      *    Busca informações da ordem de compra (versão mais atual asso-
      *    ciada à cart/operação/versão).
      *-----------------------------------------------------------------
           MOVE  0004                  TO  MSG1-CD-SEQL-ERRO

           MOVE SGTSLA75                   TO NM-PGM
           SET  SGTSLA75-LISTA-ORD-ATV     TO TRUE
           MOVE SGTS0249-CD-CTRA-TVM       TO SGTSLA75-E-CD-CTRA-TIT-VL
           MOVE SGTS0249-CD-OPR-ETRD-MCP   TO SGTSLA75-E-CD-OPR-ETRD-MCP
           MOVE SGTS0249-NR-VRS-OPR-ETRD   TO SGTSLA75-E-NR-VRS-OPR-ETRD

           PERFORM 900000-EXECUTAR-MODULO

           IF SGTSLA75-QT-OCR-TAB      EQUAL  ZEROES
              SET   ERRO-PROGRAMADO    TO  TRUE
              MOVE  0932               TO  MSG1-CD-ERRO-TS
              MOVE  'Favor vincular um modelo de negociação a esta opera
      -              'ção estruturada.'
                                       TO  MSG1-TX-ERRO-TS
              PERFORM  969000-MONITORAR-VARIAVEIS
              PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       210000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       220000-ATUALIZAR-ORD-NEGO       SECTION.
      *--> Atualiza Dados da Ordem de Negociação
      *    Data Emissão, Texto Observação e
      *    Local + Itens Selecionados Modelo
      *-----------------------------------------------------------------
           MOVE  0006                  TO  MSG1-CD-SEQL-ERRO

           MOVE SGTSA075               TO NM-PGM
           MOVE SGTS0249-CD-CTRA-TVM   TO SGTSA075-CD-CTRA-TIT-VL
           MOVE SGTS0249-CD-OPR-ETRD-MCP
                                       TO SGTSA075-CD-OPR-ETRD-MCP
           MOVE SGTS0249-NR-VRS-OPR-ETRD
                                       TO SGTSA075-NR-VRS-OPR-ETRD
           MOVE SGTSLA75-NR-SEQL-ORD-NEGO(1)
                                       TO SGTSA075-NR-SEQL-ORD-NEGO
           MOVE 'CURRENT'              TO SGTSA075-TS-ULT-ATL
           MOVE SGTSLA75-CD-MOD-ORD-NEGO(1)
                                       TO SGTSA075-CD-MOD-ORD-NEGO
           MOVE CORR SGTSLA75-DT-EMS-ORD-NEGO-R (1)
                                       TO SGTSA075-DT-EMS-ORD-NEGO-R
           MOVE SGTSLA75-TX-OBS-ORD-NEGO(1)
                                       TO SGTSA075-TX-OBS-ORD-NEGO
           MOVE SGTSLA75-TX-MSG-ORD-NEGO(1)
                                       TO SGTSA075-TX-MSG-ORD-NEGO
           MOVE SGTS0249-CD-ARQ-OPR-ETRD
                                       TO SGTSA075-CD-ARQ-OPR-ETRD

           PERFORM 900000-EXECUTAR-MODULO
           .
       230000-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *-----------------------------------------------------------------
      *   Section da Faixa de 900000 a 969900 para Demais Rotinas
      *-----------------------------------------------------------------
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       900000-EXECUTAR-MODULO          SECTION.
      *--> Executa módulos implementados com CONTAINER
      *-----------------------------------------------------------------

      *    Grava o container de requisição do módulo a executar
           PERFORM 982000-GRAVAR-CONTAINER-RQSC

      *    Limpa a resposta e erro antes de executar o módulo
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO

      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EVALUATE NM-PGM

               WHEN SGTSA075
                    EXEC CICS LINK
                              PROGRAM ( SGTSA075   )
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

      *-----------------------------------------------------------------
       969000-MONITORAR-VARIAVEIS      SECTION.
      *--> Monitora as variáveis dos módulos chamados
      *-----------------------------------------------------------------

           EVALUATE  NM-PGM
               WHEN  SGTSA075
                     MOVE 'SGTSA075-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSA075-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSA075-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSA075-CD-OPR-ETRD-MCP
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSA075-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSA075-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSA075-NR-SEQL-ORD-NEGO'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSA075-NR-SEQL-ORD-NEGO
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSA075-TS-ULT-ATL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSA075-TS-ULT-ATL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSA075-CD-MOD-ORD-NEGO'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSA075-CD-MOD-ORD-NEGO
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSA075-CD-ARQ-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSA075-CD-ARQ-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSA075-CD-MTV-NEGO-VND'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSA075-CD-MTV-NEGO-VND
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSA075-CD-TIP-APL-TAXA'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSA075-CD-TIP-APL-TAXA
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSA075-CD-IEC-TAXA-VND'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSA075-CD-IEC-TAXA-VND
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSA075-DT-EMS-ORD-NEGO'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSA075-DT-EMS-ORD-NEGO
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSA075-DT-ESTV-TAXA-VND'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSA075-DT-ESTV-TAXA-VND
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSA075-VL-PU-PAPL-VND'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSA075-VL-PU-PAPL-VND
                                       TO  VL-PU-PAPL-VND
                     MOVE  VL-PU-PAPL-VND
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSA075-VL-TAXA-NEGO-VND'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSA075-VL-TAXA-NEGO-VND
                                       TO  VL-TAXA-NEGO-VND
                     MOVE  VL-TAXA-NEGO-VND
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSA075-TX-JST-VND-OPR'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSA075-TX-JST-VND-OPR
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSA075-TX-OBS-ORD-NEGO'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSA075-TX-OBS-ORD-NEGO
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSA075-TX-MSG-ORD-NEGO'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSA075-TX-MSG-ORD-NEGO
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  SGTSLA75
                     MOVE 'SGTSLA75-E-CD-TIP-SRVC'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSLA75-E-CD-TIP-SRVC
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSLA75-E-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSLA75-E-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
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
           MOVE  0018                  TO  MSG1-CD-SEQL-ERRO

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
           MOVE  'SGTS0249'            TO  NM-PGM
           MOVE  'SGTS0249-RQSC'       TO  NM-CTNR

      *--> Recupera o tamanho do container de requisição
           PERFORM  973100-LER-CONTAINER-NODATA

      *--> Obtem o tamanho esperado da requisicao
           MOVE  LENGTH OF SGTS0249-RQSC
                                       TO  QT-CCT-EPRO

      *--> Valida tamanho esperado da requisicao X tamanho recebido
      *
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
                     INTO      (SGTS0249-RQSC)
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
           MOVE  0020                  TO  MSG1-CD-SEQL-ERRO

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
               WHEN  MSG1-RESP1        EQUAL DFHRESP(CONTAINERERR)
                     CONTINUE
               WHEN  MSG1-RESP1        EQUAL DFHRESP(NORMAL)
                     SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
                     MOVE  NM-PGM      TO  MSG1-NM-PGM-ACND
                     IF  NM-PGM(1:3)   EQUAL 'SGT'
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
      *-----------------------------------------------------------------

      *    Monta o nome dos container de acordo com modulo a executar
           MOVE SPACES                 TO NM-CTNR

      *    Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *    nome_modulo -RPST
      *    EVALUATE NM-PGM
      *        WHEN ATCS6000
      *             MOVE 'PRM-DADOS-RPST' TO NM-CTNR
      *
      *        WHEN OTHER
                    MOVE NM-PGM        TO NM-CTNR(1:8)
                    MOVE '-RPST'       TO NM-CTNR(9:5)
      *    END-EVALUATE

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

               WHEN SGTSA075
                    EXEC CICS GET
                              CONTAINER ( NM-CTNR       )
                              INTO      ( SGTSA075-RPST )
                              FLENGTH   ( QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSLA75
                    EXEC CICS GET
                              CONTAINER ( NM-CTNR       )
                              INTO      ( SGTSLA75-RPST )
                              FLENGTH   ( QT-CCT-LIDO   )
                    END-EXEC

               WHEN OTHER
                    MOVE 9003          TO MSG1-CD-ERRO-TS
                    MOVE 'Erro na leitura de resposta do módulo.'
                                       TO MSG1-TX-ERRO-TS
                    PERFORM 969000-MONITORAR-VARIAVEIS
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *--> Ao chegar aqui, há certeza de que não houve erro algum
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
      *   Section da Faixa de 980000 a 989900 para Gravação de Dados
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       982000-GRAVAR-CONTAINER-RQSC    SECTION.
      *-----------------------------------------------------------------
      *    Postar container com dados de requisição (entrada)
      *-----------------------------------------------------------------

      *    Monta o nome dos container de acordo com modulo a executar
           MOVE SPACES                 TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE

      *    Quando módulos via IIB, fixo nome PRM-DADOS-RQSC, senão
      *    nome_modulo -RQSC

      *    EVALUATE NM-PGM
      *        WHEN ATCS6000
      *             MOVE 'PRM-DADOS-RQSC' TO NM-CTNR
      *
      *        WHEN OTHER
                    MOVE NM-PGM        TO NM-CTNR(1:8)
                    MOVE '-RQSC'       TO NM-CTNR(9:5)
      *    END-EVALUATE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Faz put da requisição do programa a executar
           EVALUATE NM-PGM
               WHEN SGTSA075
                    EXEC CICS PUT
                              CONTAINER ( NM-CTNR                 )
                              FROM      ( SGTSA075-RQSC           )
                              FLENGTH   ( LENGTH OF SGTSA075-RQSC )
                    END-EXEC

               WHEN SGTSLA75
                    EXEC CICS PUT
                              CONTAINER ( NM-CTNR                 )
                              FROM      ( SGTSLA75-RQSC           )
                              FLENGTH   ( LENGTH OF SGTSLA75-RQSC )
                    END-EXEC

               WHEN OTHER
                    MOVE 9004          TO MSG1-CD-ERRO-TS
                    MOVE 'Erro na gravação da requisição do módulo.'
                                       TO MSG1-TX-ERRO-TS
                    PERFORM 969000-MONITORAR-VARIAVEIS
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
      *    problemas em módulos executados mais de uma vez dentro
      *    da mesma sessão do CICS.
      *    Equivale ao initialize na área de resposta dos
      *    programas que utilizam COMMAREA.
      *-----------------------------------------------------------------

      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *    nome_modulo -RPST
      *    EVALUATE NM-PGM
      *        WHEN ATCS6000
      *             MOVE 'PRM-DADOS-RPST' TO NM-CTNR
      *
      *        WHEN OTHER
                    MOVE NM-PGM        TO NM-CTNR(1:8)
                    MOVE '-RPST'       TO NM-CTNR(9:5)
      *    END-EVALUATE

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
      *    As sections abaixo iniciadas com 999000 representam os
      *    tratamentos de erro através do Sistema MSG (rotina MSGS0001)
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
                     PERFORM  999004-TRATA-ERRO-CTNR
               WHEN  ERRO-TAMANHO-AREA
                     PERFORM  999005-TRATA-ERRO-AREA-DADOS
               WHEN  ERRO-CANAL
                     PERFORM  999006-TRATA-ERRO-CANAL
               WHEN  OTHER
                     CONTINUE
           END-EVALUATE

      *--> Adiciona as variáveis de entrada para monitoração
           MOVE '0249-001-CD-CTRA-TVM'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0249-CD-CTRA-TVM  TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE '0249-002-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
      *------> Verifica se o conteudo e numerico antes de adicionar
           IF  SGTS0249-CD-OPR-ETRD-MCP    NUMERIC
               MOVE  SGTS0249-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP-DISP
               MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE '0249-003-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0249-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE '0249-004-CD-ARQ-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0249-CD-ARQ-OPR-ETRD
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
      *---------------------- FIM SGTS0249 -----------------------------