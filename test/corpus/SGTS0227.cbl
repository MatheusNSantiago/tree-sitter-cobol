       CBL ARITH(EXTEND)
      *-----------------------------------------------------------------
      * PROGRAMA..: SGTS0227
      * ANALISTA..: F8785286 Roseli Akemi Kawahata
      * AUTOR.....: F8785286 Roseli Akemi Kawahata
      * DATA......: 02/10/2018
      * OBJETIVO..: Calcula saldo das operações encarteiradas.
      * COMPILACAO: COBOL/CICS
      *-----------------------------------------------------------------
      * VRS001 02/10/2018 F8785286 DE 2016/145195 - IMPLANTACAO
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID. SGTS0227.
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *----------------------------------------------------------------*
       DATA DIVISION.
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*
      
       77  CTE-INICIO-SS               PIC  X(040)  VALUE
                    '*** S.S. COMECA AQUI ***'.
      
       77  CTE-PROG                    PIC  X(018)  VALUE
                    '*** SGTS0227 ***'.
       77  CTE-VERS                    PIC  X(006)  VALUE 'VRS001'.
      
      * Módulos acionados
       77  SGTSL069                    PIC  X(008) VALUE 'SGTSL069'.
       77  SGTSLA04                    PIC  X(008) VALUE 'SGTSLA04'.
       77  SGTSLB05                    PIC  X(008) VALUE 'SGTSLB05'.
       77  SGTSC005                    PIC  X(008) VALUE 'SGTSC005'.
      *----------------------------------------------------------------*
       LOCAL-STORAGE                   SECTION.
      *----------------------------------------------------------------*
      * Areas de trabalho
       01  AREA-AUX.
           05  ACHOU-SDO               PIC  X(01) VALUE SPACES.
           05  IC-TAB                  PIC S9(4) COMP-5 VALUE 0.
      *
       01  AUX-CD-OPR.
           03 AUX-CD-OPR-ETRD-MCP      PIC S9(11)             VALUE 0.
      *
      *--> Auxiliares Movimentacao Monitoração
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD         PIC  9(11)            VALUE 0.
      *----------------------------------------------------------------*
      * Book dos Containers e das subrotinas chamadas
      *----------------------------------------------------------------*
      * Book das variáveis auxiliares do tratamento de erro via MSG
       COPY SGTKERR1.
      
      * Book para captura de abend cics da bloco código MSGKP999
       COPY HLPKW999.
      
      * Book para captura de erro via MSG1
       COPY MSGKWMG1.
      
      * Book do container de requisição e resposta deste módulo
       01 SGTS0227-DADOS.
       COPY SGTK0227.
      
      * Lista container da Tabela EST_VRS_OPR_ETRD -  SGTSL069
       01 SGTSL069-DADOS.
       COPY SGTKL069.
      *
      * Listar PSC_DRIA_SDO_OPR -  SGTSLA04
       COPY SGTKLA04.
      *
      * Lista Estado da Operação e Estado da Versão a Partir do Título.
      *                                        -  SGTSLB05
       COPY SGTKLB05.
      *
      * Consulta Operação  -  SGTSC005
       COPY SGTKC005.
      *
      * Book do Container de Erro
       COPY IIBKERRO.
      
      * Book do container de Serviços (para acionamento de serviços)
       COPY IIBKSRVC.
      
      * Book dos dados do usuário
       COPY IIBKOPRD.
      
       01  CTE-FINAL-SS                PIC  X(40)      VALUE
                    '*** S.S. TERMINA AQUI ***'.
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
       000000-ROTINA-PRINCIPAL         SECTION.
      *----------------------------------------------------------------*
      
           PERFORM 100000-INICIAR
      
           PERFORM 200000-PROCESSAR
      
           PERFORM 980000-GRAVAR-CONTAINER-RPST
      
           PERFORM 990000-ENCERRAR
      
           GOBACK
           .
       000000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       100000-INICIAR                  SECTION.
      *----------------------------------------------------------------*
           MOVE 0001                   TO MSG1-CD-SEQL-ERRO
      
      *    Ativa a captura automática de abend do CICS.
      *    WHENEVER CICS
       COPY HLPKE999.
      
           PERFORM 970000-LER-CANAL
      
      *    Define o nome do programa p/ fins manipulação container
           MOVE 'SGTS0227'             TO NM-PGM
      
      *    Limpa a resposta e erro antes de executar o módulo
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO
      
           PERFORM 971000-LER-REQUISICAO
      
           PERFORM 110000-VALIDAR-REQUISICAO
           .
       100000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       110000-VALIDAR-REQUISICAO       SECTION.
      *----------------------------------------------------------------*
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO
      
           IF  SGTS0227-CD-CTRA-TIT-VL    EQUAL ZEROES
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0002               TO MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de título e valores mobiliários
      -              'não informado.' TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
      
           MOVE ZEROES                 TO SGTS0227-SDO-OPR-ENCO
           .
       110000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       200000-PROCESSAR                SECTION.
      *----------------------------------------------------------------*
           MOVE 0003                   TO MSG1-CD-SEQL-ERRO
      
           PERFORM 210000-BUSCA-OPR-ENCO
      
           PERFORM
              VARYING SGTSLB05-IC-TAB  FROM 1 BY 1
              UNTIL SGTSLB05-IC-TAB > SGTSLB05-QT-OCR-TAB
              IF ((SGTSLB05-IC-TAB = 1)     OR
                  (SGTSLB05-CD-OPR-ETRD-MCP(SGTSLB05-IC-TAB)  NOT EQUAL
                   SGTSLB05-CD-OPR-ETRD-MCP(SGTSLB05-IC-TAB - 1)))
                  PERFORM 220000-BUSCA-EST-VRS
                  MOVE 'N'  TO  ACHOU-SDO
                  PERFORM 230000-SELEC-ULT-ENCO
                     VARYING SGTSL069-IC-TAB  FROM 1 BY 1
                     UNTIL SGTSL069-IC-TAB >   SGTSL069-QT-OCR-TAB
                  IF ACHOU-SDO = 'S'
                     PERFORM 240000-BUSCA-SDO-POS-DRIA
                  END-IF
              END-IF
           END-PERFORM
           .
       200000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       210000-BUSCA-OPR-ENCO           SECTION.
      * Lista cart/opr/vrs a partir da cart/titulo/divisão
      *----------------------------------------------------------------*
           MOVE 0004                   TO MSG1-CD-SEQL-ERRO
      
      *    Consulta as operações encarteiradas
           MOVE SGTSLB05               TO NM-PGM
      
           SET SGTSLB05-LISTA-POR-TIT  TO TRUE
      
           MOVE SGTS0227-CD-CTRA-TIT-VL
                                       TO SGTSLB05-E-CD-CTRA-TIT-VL
           MOVE SGTS0227-CD-TIT-OPR-ETRD
                                       TO SGTSLB05-E-CD-TIT-OPR-ETRD
           MOVE SGTS0227-NR-DVS-TIT    TO SGTSLB05-E-NR-DVS-TIT
      
           PERFORM 900000-EXECUTAR-MODULO
           .
       210000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       220000-BUSCA-EST-VRS            SECTION.
      * Busca estado da versao da operação estruturada
      *----------------------------------------------------------------*
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO
      
      *    Consulta estado da versão
           MOVE SGTSL069               TO NM-PGM
      
           SET SGTSL069-LISTA-OPR TO TRUE
      
           MOVE SGTS0227-CD-CTRA-TIT-VL
                                       TO SGTSL069-CD-CTRA-TIT-VL
           MOVE SGTSLB05-CD-OPR-ETRD-MCP(SGTSLB05-IC-TAB)
                                       TO SGTSL069-CD-OPR-ETRD-MCP
           MOVE ZEROES                 TO SGTSL069-NR-VRS-OPR-ETRD
           MOVE SPACES                 TO SGTSL069-TS-EST-VRS-PAG
      
           PERFORM 900000-EXECUTAR-MODULO
           .
       220000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       230000-SELEC-ULT-ENCO           SECTION.
      * Seleciona a maior versão da operação que esteja no estado 5-3
      *----------------------------------------------------------------*
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO
      
           IF SGTSL069-CD-TIP-EST-OPR(SGTSL069-IC-TAB)  EQUAL 5 AND
              SGTSL069-CD-TIP-EST-VRS(SGTSL069-IC-TAB)  EQUAL 3
                 MOVE 'S'  TO  ACHOU-SDO
                 MOVE SGTSL069-IC-TAB  TO  IC-TAB
                 ADD   1   TO  SGTSL069-QT-OCR-TAB
                                            GIVING SGTSL069-IC-TAB
           END-IF
           .
       230000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       240000-BUSCA-SDO-POS-DRIA       SECTION.
      * Lista saldo posição diária do título encarteirado
      *----------------------------------------------------------------*
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO
      
           MOVE SGTSLA04               TO NM-PGM
      
           SET SGTSLA04-LS-ULT-PSC     TO TRUE
      
           MOVE SGTS0227-CD-CTRA-TIT-VL
                                       TO SGTSLA04-E-CD-CTRA-TIT-VL
           MOVE SGTSLB05-E-CD-TIT-OPR-ETRD
                                       TO SGTSLA04-E-CD-TIT-OPR-ETRD
           MOVE SGTSLB05-E-NR-DVS-TIT  TO SGTSLA04-E-NR-DVS-TIT
      
           MOVE SPACES                 TO SGTSLA04-E-DT-PSC
           MOVE ZEROES                 TO SGTSLA04-E-CD-OPR-ETRD-MCP
                                          SGTSLA04-E-NR-VRS-OPR-ETRD
           PERFORM 900000-EXECUTAR-MODULO
      
           ADD  SGTSLA04-VL-SDO-TIT-DT-PSC(1) TO  SGTS0227-SDO-OPR-ENCO
           .
       240000-SAI.
           EXIT.
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
      
               WHEN SGTSL069
                   EXEC CICS LINK
                             PROGRAM ( SGTSL069  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC
      
               WHEN SGTSLA04
                   EXEC CICS LINK
                             PROGRAM ( SGTSLA04  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC
      
               WHEN SGTSLB05
                   EXEC CICS LINK
                             PROGRAM ( SGTSLB05  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC
      
               WHEN SGTSC005
                   EXEC CICS LINK
                             PROGRAM ( SGTSC005  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC
      
               WHEN OTHER
                   SET   ERRO-PROGRAMADO TO  TRUE
                    MOVE  9006           TO  MSG1-CD-ERRO-TS
                    STRING  'Não definida a execução do módulo'
                            NM-PGM '.'
                            DELIMITED BY SIZE
                            INTO            MSG1-TX-ERRO-TS
                   PERFORM 999000-ERRO
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
                    PERFORM 969000-MONITORAR-VARIAVEIS
                    PERFORM 999000-ERRO
           END-EVALUATE
      
      *    Efetua a leitura do Container de Erro
           PERFORM 972000-LER-CONTAINER-DE-ERRO
      
      *    Efetua a leitura do Container de Resposta
           PERFORM 973000-LER-RESPOSTA
           .
       900000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       969000-MONITORAR-VARIAVEIS      SECTION.
      *----------------------------------------------------------------*
      * Monitora as variáveis dos módulos chamados
      *----------------------------------------------------------------*
           EVALUATE NM-PGM
      
                WHEN SGTSL069
                    MOVE 'SGTSL069-CD-TIP-SRVC'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL069-CD-TIP-SRVC TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT
      
                    MOVE 'SGTSL069-CD-CTRA-TIT-VL'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL069-CD-CTRA-TIT-VL TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT
      
                    MOVE 'SGTSL069-CD-OPR-ETRD-MCP'
                                                 TO MSGKW999-NM-VRV-MNT
                    IF SGTSL069-CD-OPR-ETRD-MCP  IS NUMERIC
                       MOVE SGTSL069-CD-OPR-ETRD-MCP
                                                 TO CD-OPR-ETRD
                       MOVE CD-OPR-ETRD-MCP-TX   TO MSGKW999-VL-VRV-MNT
                    ELSE
                       MOVE 'Código de operação estruturada de mercado
      -              ' de capitais não numérico.'
                                                 TO MSGKW999-VL-VRV-MNT
                    END-IF
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
      
                WHEN SGTSLA04
                    MOVE 'SGTSLA04-CD-TIP-SRVC'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSLA04-CD-TIP-SRVC TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT
      
                    MOVE 'SGTSLA04-E-CD-CTRA-TIT-VL'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSLA04-E-CD-CTRA-TIT-VL
                                                 TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT
      
                    MOVE 'SGTSLA04-E-CD-OPR-ETRD-MCP'
                                                 TO MSGKW999-NM-VRV-MNT
                    IF SGTSLA04-E-CD-OPR-ETRD-MCP  IS NUMERIC
                       MOVE SGTSLA04-E-CD-OPR-ETRD-MCP
                                                 TO CD-OPR-ETRD
                       MOVE CD-OPR-ETRD-MCP-TX   TO MSGKW999-VL-VRV-MNT
                    ELSE
                       MOVE 'Código de operação estruturada de mercado
      -              ' de capitais não numérico.'
                                                 TO MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT
      
                    MOVE 'SGTSLA04-E-NR-VRS-OPR-ETRD'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSLA04-E-NR-VRS-OPR-ETRD
                                                 TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT
      
                    MOVE 'SGTSLA04-E-CD-TIT-OPR-ETRD'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSLA04-E-CD-TIT-OPR-ETRD
                                                 TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT
      
                    MOVE 'SGTSLA04-E-NR-DVS-TIT' TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSLA04-E-NR-DVS-TIT   TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT
      
                    MOVE 'SGTSLA04-E-DT-PSC'     TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSLA04-E-DT-PSC       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT
      
                WHEN SGTSLB05
                    MOVE 'SGTSLB05-CD-TIP-SRVC'  TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSLB05-CD-TIP-SRVC    TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT
      
                    MOVE 'SGTSLB05-E-CD-CTRA-TIT-VL'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSLB05-E-CD-CTRA-TIT-VL
                                                 TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT
      
                    MOVE 'SGTSLB05-E-CD-TIT-OPR-ETRD'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSLB05-E-CD-TIT-OPR-ETRD
                                                 TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT
      
                    MOVE 'SGTSLB05-E-NR-DVS-TIT' TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSLB05-E-NR-DVS-TIT   TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT
      
                WHEN SGTSC005
                    MOVE 'SGTSC005-CD-CTRA-TIT-VL'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSC005-CD-CTRA-TIT-VL
                                                 TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT
      
                    MOVE 'SGTSC005-CD-OPR-ETRD-MCP'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSC005-CD-OPR-ETRD-MCP
                                                 TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT
      
                    MOVE 'SGTSC005-NR-VRS-OPR-ETRD'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSC005-NR-VRS-OPR-ETRD
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
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO
      
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
      *----------------------------------------------------------------*
       971000-LER-REQUISICAO           SECTION.
      *----------------------------------------------------------------*
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO
      
      *    Monta o nome do container de requisição
           MOVE 'SGTS0227'             TO NM-PGM
           MOVE 'SGTS0227-RQSC'        TO NM-CTNR
      
      *    Recupera o tamanho do container de requisição
           PERFORM 973100-LER-CONTAINER-NODATA
      
      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE
      
      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE
      
      *    Efetua a leitura do container
           EXEC CICS GET
                     CONTAINER (NM-CTNR)
                     FLENGTH   (QT-CCT-LIDO)
                     INTO      (SGTS0227-RQSC)
           END-EXEC
      
      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       971000-SAI.
           EXIT.
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
      *        PARA O MÓDULO CHAMADO e tratar na section chamadora
               WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
                    CONTINUE
               WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
                    SET   ERRO-PROGRAMA-ACIONADO TO  TRUE
                    MOVE  NM-PGM       TO  MSG1-NM-PGM-ACND
      
                    IF NM-PGM(1:3) EQUAL 'SGT'
                       MOVE  IIBKERRO-CD-ERRO
                                       TO  MSG1-CD-RTN-PGM-ACND
                                           MSG1-CD-ERRO-TS
                       STRING  NM-PGM ' ' IIBKERRO-TX-ERRO
                             DELIMITED BY SIZE
                             INTO  MSG1-TX-RTN-PGM-ACND
                       MOVE MSG1-TX-RTN-PGM-ACND TO  MSG1-TX-ERRO-TS
                    ELSE
                       MOVE 3004          TO MSG1-CD-ERRO-TS
                       STRING  'Erro ao acionar o módulo ' NM-PGM '.'
                            DELIMITED BY SIZE
                            INTO           MSG1-TX-ERRO-TS
      
                       MOVE IIBKERRO-TX-ERRO   TO MSG1-TX-RTN-PGM-ACND
                       MOVE IIBKERRO-CD-ERRO   TO MSG1-CD-RTN-PGM-ACND
      
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
      *----------------------------------------------------------------*
       973000-LER-RESPOSTA             SECTION.
      *----------------------------------------------------------------*
      *    Recupera o container com os dados de resposta (saída).
      *
      *                        NOME DO CONTAINER RESPOSTA
      *                        --------------------------
      *
      *    Módulo CONTAINER  -     NOME_PGM-RPST
      *    Módulo IIB (FIXO) -     PRM-DADOS-RPST
      *
      *---------------------------------------------------------------*
      *    Monta o nome dos container de acordo com módulo a executar
           MOVE SPACES                 TO NM-CTNR
      
      *    Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *    nome_módulo -RPST
           MOVE NM-PGM        TO NM-CTNR(1:8)
           MOVE '-RPST'       TO NM-CTNR(9:5)
      
      *    Recupera o tamanho do container de resposta
           PERFORM 973100-LER-CONTAINER-NODATA
      
      *Configura dados para leitura do container de resposta
           SET  GET-CONTAINER          TO TRUE
      
      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE
      
      *    Lê a RESPOSTA de acordo com o programa que a gravou
      
           EVALUATE NM-PGM
      
               WHEN SGTSL069
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSL069-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC
      
               WHEN SGTSLA04
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSLA04-RPST    )
                              FLENGTH   (QT-CCT-LIDO     )
                    END-EXEC
      
               WHEN SGTSLB05
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSLB05-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC
      
               WHEN SGTSC005
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSC005-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC
      
               WHEN OTHER
                    SET   ERRO-PROGRAMADO      TO  TRUE
                    MOVE  9003                 TO  MSG1-CD-ERRO-TS
                    STRING  'Erro na leitura da resposta do módulo '
                            NM-PGM '.'
                            DELIMITED BY SIZE
                            INTO           MSG1-TX-ERRO-TS
                    PERFORM 999000-ERRO
           END-EVALUATE
      
      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       973000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       973100-LER-CONTAINER-NODATA     SECTION.
      *----------------------------------------------------------------*
      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE
      
      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE
      
      *    Obtém a quantidade de caracteres no container
           MOVE ZEROS                  TO QT-CCT-LIDO
           EXEC CICS GET
                     CONTAINER (NM-CTNR)
                     NODATA
                     FLENGTH   (QT-CCT-LIDO)
           END-EXEC
      
      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       973100-SAI.
           EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      * SECTION DA FAIXA DE 980000 A 989900 PARA GRAVAÇÃO DE DADOS     *
      *                                                                *
      *----------------------------------------------------------------*
       980000-GRAVAR-CONTAINER-RPST    SECTION.
      *----------------------------------------------------------------*
      
      *    Exclui eventual container de erro de outros programas
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO
      
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO
      
      *    Monta o nome do container de resposta
           MOVE 'SGTS0227-RPST'     TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE
      
      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE
      
           EXEC CICS PUT
                     CONTAINER (NM-CTNR                    )
                     FROM      (SGTS0227-RPST          )
                     FLENGTH   (LENGTH OF SGTS0227-RPST)
           END-EXEC
      
      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       980000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       982000-GRAVAR-CONTAINER-RQSC    SECTION.
      *----------------------------------------------------------------*
      *    Postar container com dados de requisição (entrada)
      *
      *                        NOME DO CONTAINER Requisição
      *                        ----------------------------
      *
      *    Módulo CONTAINER  -     NOME_PGM-RQSC
      *    Módulo IIB (FIXO) -     PRM-DADOS-RQSC
      *
      *----------------------------------------------------------------*
           MOVE 0011                   TO MSG1-CD-SEQL-ERRO
      
      *    Monta o nome dos container de acordo com módulo a executar
           MOVE SPACES                 TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE
      
      *    Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *    nome_módulo -RPST
           MOVE NM-PGM        TO NM-CTNR(1:8)
           MOVE '-RQSC'       TO NM-CTNR(9:5)
      
      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE
      
      *    Faz put da requisição do programa a executar
           EVALUATE NM-PGM
      
               WHEN SGTSL069
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (SGTSL069-RQSC )
                              FLENGTH   (LENGTH OF SGTSL069-RQSC)
                    END-EXEC
      
               WHEN SGTSLA04
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (SGTSLA04-RQSC )
                              FLENGTH   (LENGTH OF SGTSLA04-RQSC)
                    END-EXEC
      
               WHEN SGTSLB05
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (SGTSLB05-RQSC )
                              FLENGTH   (LENGTH OF SGTSLB05-RQSC)
                    END-EXEC
      
               WHEN SGTSC005
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (SGTSC005-RQSC )
                              FLENGTH   (LENGTH OF SGTSC005-RQSC)
                    END-EXEC
      
               WHEN OTHER
                    SET   ERRO-PROGRAMADO      TO  TRUE
                    MOVE  9004                 TO  MSG1-CD-ERRO-TS
                    STRING  'Erro na gravação da requisição do módulo '
                             NM-PGM '.'
                             DELIMITED BY SIZE
                             INTO           MSG1-TX-ERRO-TS
                    PERFORM 999000-ERRO
           END-EVALUATE
      
      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       982000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       983000-EXCLUIR-RESPOSTA-ERRO    SECTION.
      *----------------------------------------------------------------*
      * Exclui o container de resposta e de erro para evitar problemas *
      * em módulos executados mais de uma vez dentro da mesma sessão do*
      * CICS. Equivale ao initialize na área de resposta dos programas *
      * que utilizam COMMAREA.                                         *
      *----------------------------------------------------------------*
           MOVE 0012                   TO MSG1-CD-SEQL-ERRO
      
      *    Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *    nome_módulo -RPST
           MOVE NM-PGM        TO NM-CTNR(1:8)
           MOVE '-RPST'       TO NM-CTNR(9:5)
      
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
           .
       983100-SAI.
           EXIT.
      *----------------------------------------------------------------*
       984000-GRAVAR-CONTAINER-ERRO    SECTION.
      *----------------------------------------------------------------*
      *    Grava o erro gerado no container de erro
      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS PUT
                     CONTAINER ( 'PRM-ERRO'                  )
                     FROM      ( IIBKERRO-PRM-ERRO           )
                     FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                     RESP      (MSG1-RESP1                   )
                     RESP2     (MSG1-RESP2                   )
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
      *
      *----------------------------------------------------------------*
      *                                                                *
      * As sections abaixo iniciadas com 999000 representam os trata-  *
      * mentos de erro utilizando o Sistema MSG (rotina MSGS0001)      *
      *                                                                *
      *----------------------------------------------------------------*
       999000-ERRO                     SECTION.
      *----------------------------------------------------------------*
      
           IF MSG1-CD-ERRO-TS EQUAL ZEROS
              MOVE 8888                TO MSG1-CD-ERRO-TS
           END-IF
      
           IF MSG1-TX-ERRO-TS EQUAL SPACES
              MOVE 'Erro não especificado no módulo.' TO MSG1-TX-ERRO-TS
           END-IF
      
           PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           .
       999000-ERRO-SAI.
           EXIT.
      *----------------------------------------------------------------*
      *     As sections abaixo representam os tratamentos de erro
      *    utilizando o sistema MSG de mensageria (rotina MSGS0001)
      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------
      
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
           MOVE 'SGTS0227-CD-CTRA-TIT-VL'        TO  MSGKW999-NM-VRV-MNT
           MOVE SGTS0227-CD-CTRA-TIT-VL          TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT
      
           MOVE 'SGTS0227-CD-TIT-OPR-ETRD'       TO MSGKW999-NM-VRV-MNT
           MOVE SGTS0227-CD-TIT-OPR-ETRD         TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT
      
           MOVE 'SGTS0227-NR-DVS-TIT'            TO MSGKW999-NM-VRV-MNT
           MOVE SGTS0227-NR-DVS-TIT              TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT
      
      *    Grava o erro gerado no sistema MSG - Mensageria
           PERFORM MSG1-NTF-MSG
      
           MOVE MSG1-CD-ERRO-TS        TO IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS        TO IIBKERRO-TX-ERRO
      
      *    Grava o erro gerado no container de erro
           PERFORM 984000-GRAVAR-CONTAINER-ERRO
      
      *    Encerra a transação
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
      * --------------------------------------*
       COPY MSGKPMG1.
      *---------------------- Fim SGTS0227 ----------------------------*
