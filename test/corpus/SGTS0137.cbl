000010*-----------------------------------------------------------------
000020* PROGRAMA..: SGTS0137
000030* ANALISTA..: F8785286 Roseli Akemi Kawahata
000040* AUTOR.....: F8785286 Roseli Akemi Kawahata
000050* DATA......: 22/05/2017
000060* OBJETIVO..: Listar Nota de Classificação da Oper Estruturada
000080* COMPILACAO: COBOL/CICS
000090*-----------------------------------------------------------------
000100* VRS001 22/05/2017 F0175702 DE 2016/145195 - IMPLANTACAO
000130*-----------------------------------------------------------------
000140 IDENTIFICATION DIVISION.
000150*----------------------------------------------------------------*
000160 PROGRAM-ID. SGTS0137.
000170*
000180*----------------------------------------------------------------*
000190 ENVIRONMENT DIVISION.
000200*----------------------------------------------------------------*
000210*
000220*----------------------------------------------------------------*
000230 CONFIGURATION                   SECTION.
000240*----------------------------------------------------------------*
000250 SPECIAL-NAMES.
000260     DECIMAL-POINT IS COMMA.
000270*
000280*----------------------------------------------------------------*
000290 DATA DIVISION.
000300*----------------------------------------------------------------*
000330 WORKING-STORAGE                 SECTION.
000340*----------------------------------------------------------------*
000350
000360 77  CTE-INICIO-SS               PIC  X(040)  VALUE
000370              '*** S.S. COMECA AQUI ***'.
000380
000390 77  CTE-PROG                    PIC  X(018)  VALUE
000400              '*** SGTS0137 ***'.
000410 77  CTE-VERS                    PIC  X(006)  VALUE 'VRS001'.
000420
000430* Modulos acionados
000440 77  IIBP2000                    PIC  X(008) VALUE 'IIBP2000'.
000440 77  SGTSL074                    PIC  X(008) VALUE 'SGTSL074'.
000440 77  ATCS6000                    PIC  X(008) VALUE 'ATCS6000'.
000470*----------------------------------------------------------------*
000480 LOCAL-STORAGE                   SECTION.
000490*----------------------------------------------------------------*
000500
000510* Areas de trabalho
000500 01 IN-CONTAINER-ERRO            PIC  X(001) VALUE SPACES.
000550    88 TEM-CONTAINER-ERRO        VALUE 'S'.
000560    88 NAO-TEM-CONTAINER-ERRO    VALUE 'N'.
000550
       01  CD-OPR-ETRD-MCP-DISPLAY     PIC  9(11) VALUE 0.
000560*----------------------------------------------------------------*
000570* Book dos Containers e das subrotinas chamadas
000580*----------------------------------------------------------------*
000590
000600* Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1
000670
000620* Book para captura de abend cics da bloco Código MSGKP999
-INC HLPKW999
000650* Book para captura de erro via MSG1
000670
-INC MSGKWMG1
000670
000680* Book do container de requisicao e resposta deste modulo
-INC SGTK0137
000670
000720* Listar da Tabela NOTA_CLSC_OPR
-INC SGTKL074
000670
000760* Consultar dados de usuário - via IIB
000770 01  ATCS6000-DADOS.
-INC ATCK6000
000670
000800* Book do Container de Erro
-INC IIBKERRO
000670
000820* Book do container de Servicos (para acionamento de servicos)
-INC IIBKSRVC
000670
001400* Book dos dados do usurio
-INC IIBKOPRD
000840
000850 77  CTE-FINAL-SS                  PIC X(40)      VALUE
000860              '*** S.S. TERMINA AQUI ***'.
000870
000880*----------------------------------------------------------------*
000890 PROCEDURE DIVISION.
000900*----------------------------------------------------------------*
000910
000920*----------------------------------------------------------------*
000930 000000-ROTINA-PRINCIPAL         SECTION.
000940*----------------------------------------------------------------*
000950
000960     PERFORM 100000-INICIAR
000970
000980     PERFORM 200000-PROCESSAR
000990
001000     PERFORM 990000-ENCERRAR
001010
001020     .
001030 000000-SAI.
001040     EXIT.
001060*----------------------------------------------------------------*
001070 100000-INICIAR                  SECTION.
001080*----------------------------------------------------------------*
001090
001100     MOVE 0001                   TO  MSG1-CD-SEQL-ERRO
001110
001120*    Ativa a captura automatica de abend do CICS.
001130*    WHENEVER CICS
-INC HLPKE999
001150
001160     PERFORM 970000-LER-CANAL
001170
001180*    Define o nome do programa p/ fins manipulacao container
001190     MOVE 'SGTS0137'             TO  NM-PGM
001200
001210*    Limpa a resposta e erro antes de executar o modulo
001220     PERFORM 983000-EXCLUIR-RESPOSTA-ERRO
001230
001240     PERFORM 971000-LER-REQUISICAO
000440
000440     PERFORM 110000-VALIDAR-REQUISICAO
001260*
001270     .
001280 100000-SAI.
001370     EXIT.
001310*----------------------------------------------------------------*
001320 110000-VALIDAR-REQUISICAO       SECTION.
001330*----------------------------------------------------------------*
001340     MOVE 0002                   TO  MSG1-CD-SEQL-ERRO
000440
           IF  SGTS0137-E-CD-CTRA-TIT-VL   LESS ZEROS
               SET  ERRO-PROGRAMADO        TO  TRUE
               MOVE 001                    TO  MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de título e valores mobiliários
      -             'inválido.'            TO  MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0137-E-CD-CTRA-TIT-VL   EQUAL ZEROS
               SET  ERRO-PROGRAMADO        TO  TRUE
               MOVE 002                    TO  MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de título e valores mobiliários
      -             'não informado.'       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

000440     IF  SGTS0137-E-CD-OPR-ETRD-MCP IS NOT NUMERIC
001390         SET ERRO-PROGRAMADO     TO  TRUE
001400         MOVE 0003               TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -         ' numérico.'
                                       TO MSG1-TX-ERRO-TS
001440         PERFORM 999000-ERRO
001450     END-IF
001090
000440     IF  SGTS0137-E-CD-OPR-ETRD-MCP < ZEROS
001390         SET ERRO-PROGRAMADO     TO  TRUE
001400         MOVE 0004               TO  MSG1-CD-ERRO-TS
001410         MOVE 'Código operação estruturada mercado de capitais inv
      -             'álido.'           TO  MSG1-TX-ERRO-TS
001440         PERFORM 999000-ERRO
001450     END-IF

000440     IF  SGTS0137-E-CD-OPR-ETRD-MCP = ZEROS
001390         SET ERRO-PROGRAMADO     TO  TRUE
001400         MOVE 0005               TO  MSG1-CD-ERRO-TS
001410         MOVE 'Código operação estruturada mercado de capitais não
      -             'informado.'       TO  MSG1-TX-ERRO-TS
001440         PERFORM 999000-ERRO
001450     END-IF

000440     IF  SGTS0137-E-NR-VRS-OPR-ETRD < ZEROS
001390         SET ERRO-PROGRAMADO     TO  TRUE
001400         MOVE 0006               TO MSG1-CD-ERRO-TS
               MOVE 'Número da versão da operação estruturada inválido.'
001420                                 TO  MSG1-TX-ERRO-TS
001440         PERFORM 999000-ERRO
001450     END-IF

           IF  SGTS0137-E-NR-VRS-OPR-ETRD = ZEROS
001390         SET ERRO-PROGRAMADO     TO  TRUE
001400         MOVE 0007               TO MSG1-CD-ERRO-TS
               MOVE 'Número da versão da operação não informado.'
001420                                 TO  MSG1-TX-ERRO-TS
001440         PERFORM 999000-ERRO
001450     END-IF
001090
001470     .
001480 110000-SAI.
001490     EXIT.
001510*----------------------------------------------------------------*
001520 200000-PROCESSAR                SECTION.
001530*----------------------------------------------------------------*
001530
001530     PERFORM 210000-BUSCAR-NOTA-CLASSIF

001530     MOVE 0003                   TO  MSG1-CD-SEQL-ERRO
002530
004700     MOVE SGTSL074-IN-CTNC-PSQ   TO  SGTS0137-IN-CTNC-PSQ
004710     PERFORM 220000-FORMATA-SAIDA
004720       VARYING SGTSL074-IC-TAB FROM 1 BY 1
004730       UNTIL SGTSL074-IC-TAB GREATER SGTSL074-QT-OCR-TAB
001530
001530     PERFORM 980000-GRAVAR-CONTAINER-RPST
001530
001530     .
001530 200000-SAI.
001660     EXIT.
001680*----------------------------------------------------------------*
001530 210000-BUSCAR-NOTA-CLASSIF      SECTION.
001530*----------------------------------------------------------------*
001530* Pesquisa a existencia de Nota de Classificação ativa.
001530*----------------------------------------------------------------*
001530     MOVE 0004                     TO  MSG1-CD-SEQL-ERRO
001530
001530     INITIALIZE                    SGTSL074-RQSC
001530     MOVE SGTSL074                 TO  NM-PGM
001530     MOVE 1                        TO  SGTSL074-CD-TIP-SRVC
001530     MOVE SGTS0137-E-CD-CTRA-TIT-VL
001530                                   TO  SGTSL074-E-CD-CTRA-TIT-VL
001530     MOVE SGTS0137-E-CD-OPR-ETRD-MCP
001530                                   TO  SGTSL074-E-CD-OPR-ETRD-MCP
001530     MOVE SGTS0137-E-NR-VRS-OPR-ETRD
001530                                   TO  SGTSL074-E-NR-VRS-OPR-ETRD
001530     MOVE SGTS0137-E-NR-SEQL-NOTA-OPR
001530                                   TO  SGTSL074-E-NR-SEQL-NOTA-OPR
001530
001530     PERFORM 900000-EXECUTAR-MODULO
001530
001530     MOVE SGTSL074-QT-OCR-TAB      TO  SGTS0137-QT-OCR-TAB
001530     MOVE SGTSL074-IC-TAB          TO  SGTS0137-IC-TAB
001530     MOVE SGTSL074-IN-CTNC-PSQ     TO  SGTS0137-IN-CTNC-PSQ
001530
001530     .
001530 210000-SAI.
001530     EXIT.
001680*----------------------------------------------------------------*
001530 220000-FORMATA-SAIDA            SECTION.
001530*----------------------------------------------------------------*
001530     MOVE 0005                       TO  MSG1-CD-SEQL-ERRO
001530
001530     MOVE SGTSL074-NR-SEQL-NOTA-OPR(SGTSL074-IC-TAB)
001530                   TO  SGTS0137-NR-SEQL-NOTA-OPR(SGTSL074-IC-TAB)
001530     MOVE SGTSL074-CD-AG-CLSC-RSCO(SGTSL074-IC-TAB)
001530                   TO  SGTS0137-CD-AG-CLSC-RSCO(SGTSL074-IC-TAB)
001530     MOVE SGTSL074-NR-ETRD-AG-CLSC(SGTSL074-IC-TAB)
001530                   TO  SGTS0137-NR-ETRD-AG-CLSC(SGTSL074-IC-TAB)
001530     MOVE SGTSL074-NR-SEQL-NOTA-AG(SGTSL074-IC-TAB)
001530                   TO  SGTS0137-NR-SEQL-NOTA-AG(SGTSL074-IC-TAB)
001530     MOVE SGTSL074-CD-ARQ-OPR-ETRD(SGTSL074-IC-TAB)
001530                   TO  SGTS0137-CD-ARQ-OPR-ETRD(SGTSL074-IC-TAB)
001530     MOVE SGTSL074-DT-INC-ATBC-NOTA(SGTSL074-IC-TAB)
001530                   TO  SGTS0137-DT-INC-ATBC-NOTA(SGTSL074-IC-TAB)
001530     MOVE SGTSL074-DT-FIM-ATBC-NOTA(SGTSL074-IC-TAB)
001530                   TO  SGTS0137-DT-FIM-ATBC-NOTA(SGTSL074-IC-TAB)
001530     MOVE SGTSL074-CD-USU-ULT-ATL(SGTSL074-IC-TAB)
001530                   TO  SGTS0137-CD-USU-ULT-ATL(SGTSL074-IC-TAB)
001530     MOVE SGTSL074-TS-ULT-ATL(SGTSL074-IC-TAB)
001530                   TO  SGTS0137-TS-ULT-ATL(SGTSL074-IC-TAB)
001530     MOVE SGTSL074-CD-TIP-NOTA-AGPR(SGTSL074-IC-TAB)
001530                   TO  SGTS0137-CD-TIP-NOTA-AGPR(SGTSL074-IC-TAB)
001530     MOVE SGTSL074-SG-NOTA-AG-CLSC(SGTSL074-IC-TAB)
001430                   TO  SGTS0137-SG-NOTA-AG-CLSC(SGTSL074-IC-TAB)
001530     MOVE SGTSL074-TX-NOTA-AG-CLSC(SGTSL074-IC-TAB)
001530                   TO  SGTS0137-TX-NOTA-AG-CLSC(SGTSL074-IC-TAB)
001530     MOVE SGTSL074-NM-AG-CLSC-RSCO(SGTSL074-IC-TAB)
001530                   TO  SGTS0137-NM-AG-CLSC-RSCO(SGTSL074-IC-TAB)
001090
001530     IF SGTSL074-IN-NOTA-OPR-CNCD(SGTSL074-IC-TAB)  EQUAL 'S'
001530        MOVE SGTSL074-IN-NOTA-OPR-CNCD(SGTSL074-IC-TAB)
001530                   TO  SGTS0137-IN-NOTA-OPR-CNCD(SGTSL074-IC-TAB)
001430     ELSE
001530        MOVE 'N'  TO  SGTS0137-IN-NOTA-OPR-CNCD(SGTSL074-IC-TAB)
001430     END-IF
001090
001530     PERFORM 223000-OBTER-NOME-USU-ATL
001530
001530     .
001530 220000-SAI.
001530     EXIT.
002730*----------------------------------------------------------------*
002740 223000-OBTER-NOME-USU-ATL       SECTION.
002750* Recupera o nome do usuário que efetuou a atualização
002760*----------------------------------------------------------------*
002770     MOVE 0006                   TO  MSG1-CD-SEQL-ERRO
002780
002790*    Movimentar o nome do programa a ser executado
002800     INITIALIZE                  ATCS6000-DADOS.
002810
002820     MOVE ATCS6000               TO  NM-PGM
002050
002060*    Movimentar os campos de entrada
002730
002740     SET K6000-PSQ-INF-USU       TO  TRUE
002750     MOVE SGTS0137-CD-USU-ULT-ATL(SGTSL074-IC-TAB)
002750                                 TO  K6000-CD-USU
002770     PERFORM 910000-CHAMAR-SERVICO
002730
002780     IF  IIBKERRO-CD-ERRO     EQUAL  ZEROS
002790         MOVE K6000-NM-USU       TO
002790                       SGTS0137-NM-USU-ULT-ATL(SGTSL074-IC-TAB)
002810     ELSE
002730         MOVE ' '                TO
002730                       SGTS0137-NM-USU-ULT-ATL(SGTSL074-IC-TAB)
002740     END-IF
002750     .
002760 223000-SAI.
002770     EXIT.
002720*----------------------------------------------------------------*
002730*                                                                *
002740*  SECTION DA FAIXA DE 900000 A 969900 PARA DEMAIS ROTINAS       *
002750*                                                                *
002760*----------------------------------------------------------------*
002770* Executa um modulo implementado com CONTAINER                   *
002780* Nao incluir nessa section os modulos chamados via COMMAREA     *
002790*                                                                *
002800*----------------------------------------------------------------*
002810 900000-EXECUTAR-MODULO          SECTION.
002820*----------------------------------------------------------------*
002830
002850*    Grava o container de requisicao do modulo a executar
002860     PERFORM 982000-GRAVAR-CONTAINER-RQSC
002870
002880*    Limpa a resposta e erro antes de executar o modulo
002890     PERFORM 983000-EXCLUIR-RESPOSTA-ERRO
002900
002910*    Nao ha captura automatica de erro CICS por usar RESP e RESP2
002920     EVALUATE NM-PGM
003000         WHEN SGTSL074
002940             EXEC CICS LINK
002950                       PROGRAM ( SGTSL074  )
002960                       CHANNEL ( NM-CNL    )
002970                       RESP    ( MSG1-RESP1)
002980                       RESP2   ( MSG1-RESP2)
002990             END-EXEC
001090
001530         WHEN OTHER
                   SET   ERRO-PROGRAMADO
                                       TO  TRUE
                    MOVE  9006          TO  MSG1-CD-ERRO-TS
                    STRING  'Não definida a execução do módulo'
                            NM-PGM '.'
                            DELIMITED BY SIZE
                            INTO            MSG1-TX-ERRO-TS
                    PERFORM 999000-ERRO
003050     END-EVALUATE
003060*
003070     EVALUATE TRUE
003080         WHEN MSG1-RESP1        EQUAL DFHRESP(NORMAL)
003090              CONTINUE
003100         WHEN OTHER
                    SET   ERRO-CICS    TO  TRUE
                    MOVE  3004         TO  MSG1-CD-ERRO-TS
                    STRING
                    'Erro  ao acionar o módulo  ' NM-PGM  ', '
                    'RESP  = '  MSG1-RESP1  ', '
                    'RESP2 = '  MSG1-RESP2
                    DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
                    PERFORM 999000-ERRO
003190     END-EVALUATE
003200
003210*    Efetua a leitura do Container de Erro
003220     PERFORM 972000-LER-CONTAINER-DE-ERRO
003230
003240*    Efetua a leitura do Container de Resposta
003250     PERFORM 973000-LER-RESPOSTA
003260
003270     .
003280 900000-SAI.
003290     EXIT.
003310*---------------------------------------------------------------*
003320 910000-CHAMAR-SERVICO           SECTION.
003330*---------------------------------------------------------------*
003340*    Acionar programa IIBP2000 para requisição do serviço.
003350*---------------------------------------------------------------*
003380*    Grava o container com dados do serviço/operação a chamar
003390     PERFORM 981000-GRAVAR-CONTAINER-SRVC
003400
003410*    Grava o container de requisicao do modulo a executar
003420     PERFORM 982000-GRAVAR-CONTAINER-RQSC
003430
003440*    Limpa a resposta e erro antes de executar o modulo
003450     PERFORM 983000-EXCLUIR-RESPOSTA-ERRO
003460
003470*    Não há captura automática de erro CICS por usar RESP e RESP2
003480     EXEC CICS LINK
003490               PROGRAM   ( IIBP2000     )
003500               CHANNEL   ( NM-CNL       )
003510               RESP      ( MSG1-RESP1   )
003520               RESP2     ( MSG1-RESP2   )
003530     END-EXEC
003540
003550     EVALUATE TRUE
003560         WHEN MSG1-RESP1        EQUAL DFHRESP(NORMAL)
003570              CONTINUE
003580         WHEN OTHER
                    SET ERRO-CICS      TO TRUE
                    MOVE  9000         TO  MSG1-CD-ERRO-TS
                    STRING 'Erro ao acionar serviço: '
                           SRVC-ID-SRVC '.' SRVC-VRS-SRVC '.'
                           SRVC-ID-OPR  '.' SRVC-VRS-OPR
                           'via IIBP2000, '
                           'RESP=' MSG1-RESP1 ', RESP-2=' MSG1-RESP2
                           '.'         DELIMITED BY SIZE
                                       INTO MSG1-TX-ERRO-TS
                    PERFORM 999000-ERRO
003760     END-EVALUATE
003770
003780*    Efetua a leitura do container de erro
003790     PERFORM 972000-LER-CONTAINER-DE-ERRO
003800
003810     IF NOT TEM-CONTAINER-ERRO
003820*    EFETUA A LEITURA DO CONTAINER DE RESPOSTA
003830        PERFORM 973000-LER-RESPOSTA
003840     END-IF
003850     MOVE SPACES                 TO  IN-CONTAINER-ERRO
003860     .
003870 910000-SAI.
003780     EXIT.
003790*----------------------------------------------------------------*
003800 969000-MONITORAR-VARIAVEIS      SECTION.
003810*----------------------------------------------------------------*
003820* Monitora as variaveis dos modulos chamados
003830*----------------------------------------------------------------*
003850
003860     EVALUATE NM-PGM
001090
003930     WHEN ATCS6000
003880          MOVE 'K6000-CD-FUC'              TO MSGKW999-NM-VRV-MNT
003900          MOVE K6000-CD-FUC                TO MSGKW999-VL-VRV-MNT
003920          PERFORM 999003-ADICIONA-VRV-MNT
003930
003940          MOVE 'K6000-CD-USU'              TO MSGKW999-NM-VRV-MNT
003960          MOVE K6000-CD-USU                TO MSGKW999-VL-VRV-MNT
003980          PERFORM 999003-ADICIONA-VRV-MNT
001090
003940          MOVE 'K6000-CD-RTN'              TO MSGKW999-NM-VRV-MNT
003960          MOVE K6000-CD-RTN                TO MSGKW999-VL-VRV-MNT
003980          PERFORM 999003-ADICIONA-VRV-MNT
001090
003940          MOVE 'K6000-TX-LVRE'             TO MSGKW999-NM-VRV-MNT
003960          MOVE K6000-TX-LVRE               TO MSGKW999-VL-VRV-MNT
003980          PERFORM 999003-ADICIONA-VRV-MNT
001090
003940          MOVE 'K6000-CD-RTN-AUX'          TO MSGKW999-NM-VRV-MNT
003960          MOVE K6000-CD-RTN-AUX            TO MSGKW999-VL-VRV-MNT
003980          PERFORM 999003-ADICIONA-VRV-MNT
001090
003940          MOVE 'K6000-CD-REA-AUX'          TO MSGKW999-NM-VRV-MNT
003960          MOVE K6000-CD-REA-AUX            TO MSGKW999-VL-VRV-MNT
003980          PERFORM 999003-ADICIONA-VRV-MNT
001090
001530     WHEN SGTSL074
001530          MOVE 'SGTSL074-CD-TIP-SRVC'      TO MSGKW999-NM-VRV-MNT
003900          MOVE SGTSL074-CD-TIP-SRVC        TO MSGKW999-VL-VRV-MNT
003920          PERFORM 999003-ADICIONA-VRV-MNT
001090
003880          MOVE 'SGTSL074-E-CD-CTRA-TIT-VL' TO MSGKW999-NM-VRV-MNT
003900          MOVE SGTSL074-E-CD-CTRA-TIT-VL   TO MSGKW999-VL-VRV-MNT
003920          PERFORM 999003-ADICIONA-VRV-MNT
001090
001530          MOVE 'SGTSL074-E-CD-OPR-ETRD-MCP' TO MSGKW999-NM-VRV-MNT
                IF  SGTSL074-E-CD-OPR-ETRD-MCP  IS NUMERIC
                    MOVE SGTSL074-E-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP-DISPLAY
                    MOVE CD-OPR-ETRD-MCP-DISPLAY
                                       TO  MSGKW999-VL-VRV-MNT
                ELSE
                          MOVE 'Código não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
003880          END-IF
003920          PERFORM 999003-ADICIONA-VRV-MNT
001090
001530          MOVE 'SGTSL074-E-NR-VRS-OPR-ETRD' TO MSGKW999-NM-VRV-MNT
003900          MOVE SGTSL074-E-NR-VRS-OPR-ETRD   TO MSGKW999-VL-VRV-MNT
003920          PERFORM 999003-ADICIONA-VRV-MNT
001090
001530          MOVE 'SGTSL074-E-NR-SEQL-NOTA-OPR'
015300                                            TO MSGKW999-NM-VRV-MNT
003900          MOVE SGTSL074-E-NR-SEQL-NOTA-OPR  TO MSGKW999-VL-VRV-MNT
003920          PERFORM 999003-ADICIONA-VRV-MNT
001090
001530     WHEN OTHER
004190          CONTINUE
004200     END-EVALUATE
004210
004220     .
004230 969000-SAI.
004240     EXIT.
004250
004260*----------------------------------------------------------------*
004270*                                                                *
004280* SECTION DA FAIXA DE 970000 A 979900 PARA LEITURA DE DADOS      *
004290*                                                                *
004300*----------------------------------------------------------------*
004310 970000-LER-CANAL                SECTION.
004320*    Obtem o canal corrente
004330*----------------------------------------------------------------*
004340     MOVE 0007                   TO  MSG1-CD-SEQL-ERRO
004350
004360*Por causa da captura automática de erro CICS, deve ser previsto
004370*o erro de container
004380     SET  ERRO-CANAL             TO  TRUE
004390
004400     EXEC CICS ASSIGN
004410               CHANNEL ( NM-CNL )
004420     END-EXEC
004430
004440*    Ao chegar aqui, há certeza de que não houve erro algum
004450     SET  SEM-ERRO               TO  TRUE
004460     .
004470 970000-FIM.
004480     EXIT.
004490
004500*----------------------------------------------------------------*
004510 971000-LER-REQUISICAO           SECTION.
004520*----------------------------------------------------------------*
004530
004540     MOVE 0008                   TO  MSG1-CD-SEQL-ERRO
004550
004560*    Monta o nome do container de requisicao
004570     MOVE 'SGTS0137'             TO  NM-PGM
004580     MOVE 'SGTS0137-RQSC'        TO  NM-CTNR
004590
004600*    Recupera o tamanho do container de requisição
004610     PERFORM 973100-LER-CONTAINER-NODATA
004620
004630*    Obtem o tamanho esperado da requisicao
004640     MOVE LENGTH OF SGTS0137-RQSC    TO  QT-CCT-EPRO
004650
004730*--> Valida tamanho esperado da requisicao X tamanho recebido

           IF  QT-CCT-LIDO             NOT EQUAL QT-CCT-EPRO
               SET   ERRO-TAMANHO-AREA   TO  TRUE
               PERFORM 999000-ERRO
           END-IF
      *--> Fim
004740
004750*Configura dados para leitura do container de requisicao
004760     SET  GET-CONTAINER          TO  TRUE
004770
004780*Por causa da captura automática de erro CICS, deve ser previsto
004790*o erro de container
004800     SET  ERRO-CONTAINER         TO  TRUE
004810
004820*    Efetua a leitura do container
004830     EXEC CICS GET
004840               CONTAINER (NM-CTNR)
004850               FLENGTH   (QT-CCT-LIDO)
004860               INTO      (SGTS0137-RQSC)
004870     END-EXEC
004880
004890*Ao chegar aqui, há certeza de que não houve erro algum
004900     SET  SEM-ERRO               TO  TRUE
004910
004920     .
004930 971000-SAI.
004940     EXIT.
004950
004960*----------------------------------------------------------------*
004970 972000-LER-CONTAINER-DE-ERRO    SECTION.
004980*----------------------------------------------------------------*
004990*    A presença do container PRM-ERRO indica que houve erro de
005000*    integração, a ausência do container indica sucesso.
005010*----------------------------------------------------------------*
005050*    Le o container de erro dos modulos chamados
005060     SET GET-CONTAINER           TO  TRUE
005070     MOVE 'PRM-ERRO'             TO  NM-CTNR
005080     INITIALIZE IIBKERRO-PRM-ERRO
005090
005100*    Não há captura automática de erro CICS por usar RESP e RESP2
005110     EXEC CICS GET
005120               CONTAINER ( NM-CTNR                     )
005130               INTO      ( IIBKERRO-PRM-ERRO           )
005140               FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
005150               RESP      ( MSG1-RESP1                  )
005160               RESP2     ( MSG1-RESP2                  )
005170     END-EXEC
005180
005190     EVALUATE TRUE
005200*        Caso deseje personalizar ou não interromper o
005210*        processamento, alterar para não dar erro aqui SOMENTE
005220*        PARA O MODULO CHAMADO e tratar na section chamadora
005230         WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
005240              CONTINUE
001090
005250         WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
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

                       MOVE IIBKERRO-TX-ERRO
                                          TO MSG1-TX-RTN-PGM-ACND
                       MOVE IIBKERRO-CD-ERRO
                                          TO MSG1-CD-RTN-PGM-ACND

                    END-IF
005350*             Monitorar variáveis do módulo chamado
005360              PERFORM 969000-MONITORAR-VARIAVEIS
005370              PERFORM 999000-ERRO
001090
005380         WHEN OTHER
005390              SET ERRO-CONTAINER TO  TRUE
005400              PERFORM 999002-TRATA-CONDITION-CICS
005410     END-EVALUATE
005420     .
005430 972000-SAI.
005440     EXIT.
005450
005460*----------------------------------------------------------------*
005470 973000-LER-RESPOSTA             SECTION.
005480*----------------------------------------------------------------*
005610*    Monta o nome dos container de acordo com modulo a executar
005620     MOVE SPACES                 TO  NM-CTNR
005630
005640*    Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
005650*    nome_modulo -RPST
005660     EVALUATE NM-PGM
005670         WHEN ATCS6000
005680              MOVE 'PRM-DADOS-RPST'
005690                                TO  NM-CTNR
005700         WHEN OTHER
005710             MOVE NM-PGM        TO  NM-CTNR(1:8)
005720             MOVE '-RPST'       TO  NM-CTNR(9:5)
005730     END-EVALUATE
005740
005750*    Recupera o tamanho do container de resposta
005760     PERFORM 973100-LER-CONTAINER-NODATA
005770
005780*Configura dados para leitura do container de resposta
005790     SET  GET-CONTAINER          TO  TRUE
005800
005810*Por causa da captura automática de erro CICS, deve ser previsto
005820*o erro de container
005830     SET  ERRO-CONTAINER         TO  TRUE
005850
005860*    Le a RESPOSTA de acordo com o programa que a gravou
005870
005900     EVALUATE NM-PGM
005970         WHEN ATCS6000
006060              INITIALIZE ATCS6000-DADOS
006070              MOVE LENGTH OF ATCS6000-DADOS TO QT-CCT-EPRO
006080              EXEC CICS GET
006090                        CONTAINER (NM-CTNR          )
006100                        INTO      (ATCS6000-DADOS   )
006110                        FLENGTH   (QT-CCT-LIDO      )
006120              END-EXEC
001090
001530         WHEN SGTSL074
005990              MOVE LENGTH OF SGTSL074-RPST TO  QT-CCT-EPRO
006000              EXEC CICS GET
006010                        CONTAINER (NM-CTNR          )
006020                        INTO      (SGTSL074-RPST    )
006030                        FLENGTH   (QT-CCT-LIDO      )
006040              END-EXEC
001090
001530         WHEN OTHER
                    SET   ERRO-PROGRAMADO
                                       TO  TRUE
                    MOVE  9003         TO  MSG1-CD-ERRO-TS
                    STRING  'Erro na leitura da resposta do módulo '
                            NM-PGM '.'
                            DELIMITED BY SIZE
                            INTO           MSG1-TX-ERRO-TS
                    PERFORM 999000-ERRO
006180     END-EVALUATE
006190
006200*Ao chegar aqui, há certeza de que não houve erro algum
006210     SET  SEM-ERRO               TO TRUE
006220
006230     .
006240 973000-SAI.
006250     EXIT.
006260*----------------------------------------------------------------*
006270 973100-LER-CONTAINER-NODATA     SECTION.
006280*----------------------------------------------------------------*
006290
006320*Configura dados para leitura do container de requisição
006330     SET  GET-CONTAINER          TO  TRUE
006340
006350*Por causa da captura automática de erro CICS, deve ser previsto
006360*o erro de container
006370     SET  ERRO-CONTAINER         TO  TRUE
006380
006390*    Obtem a quantidade de caracteres no container
006400     MOVE ZEROS                  TO  QT-CCT-LIDO
006410     EXEC CICS GET
006420               CONTAINER (NM-CTNR)
006430               NODATA
006440               FLENGTH   (QT-CCT-LIDO)
006450     END-EXEC
006460
006470*    Ao chegar aqui, há certeza de que não houve erro algum
006480     SET  SEM-ERRO               TO  TRUE
006490     .
006500 973100-SAI.
006510     EXIT.
008160*----------------------------------------------------------------*
008170 974000-LER-DADOS-OPRD           SECTION.
008180*----------------------------------------------------------------*
008190     MOVE 0010                   TO  MSG1-CD-SEQL-ERRO
008200
008210*Configura dados para leitura do container de requisio
008220     SET  GET-CONTAINER          TO  TRUE
008230     MOVE 'PRM-INF-OPRD'         TO  NM-CTNR
008240
008250*Por causa da captura automtica de erro CICS, deve ser previsto
008260*o erro de container
008270     SET  ERRO-CONTAINER         TO  TRUE
008280
008290     INITIALIZE PRM-INF-OPRD
008300     EXEC CICS GET
008310          CONTAINER ( NM-CTNR                )
008320          INTO      ( PRM-INF-OPRD           )
008330          FLENGTH   ( LENGTH OF PRM-INF-OPRD )
008340     END-EXEC.
008350
008360*    Ao chegar aqui, h certeza de que no houve erro algum
008370     SET  SEM-ERRO               TO  TRUE
008380     .
008390 974000-SAI.
008400     EXIT.
006520*----------------------------------------------------------------*
006530*                                                                *
006540* SECTION DA FAIXA DE 980000 A 989900 PARA GRAVACAO DE DADOS     *
006550*                                                                *
006560*----------------------------------------------------------------*
006570 980000-GRAVAR-CONTAINER-RPST    SECTION.
006580*----------------------------------------------------------------*
006590
006600*    Exclui eventual container de erro de outros programas
006610     PERFORM 983100-EXCLUIR-CONTAINER-ERRO
006620
006630     MOVE 0011                   TO  MSG1-CD-SEQL-ERRO
006640
006650*    Monta o nome do container de resposta
006660     MOVE 'SGTS0137-RPST'        TO  NM-CTNR
006670     SET PUT-CONTAINER           TO  TRUE
006680
006690*Por causa da captura automática de erro CICS, deve ser previsto
006700*o erro de container
006710     SET  ERRO-CONTAINER         TO  TRUE
006720
006730     EXEC CICS PUT
006740               CONTAINER (NM-CTNR                )
006750               FROM      (SGTS0137-RPST          )
006760               FLENGTH   (LENGTH OF SGTS0137-RPST)
006770     END-EXEC
006780
006790*Ao chegar aqui, há certeza de que não houve erro algum
006800     SET  SEM-ERRO               TO  TRUE
006810     .
006820 980000-SAI.
006830     EXIT.
006840*---------------------------------------------------------------*
006850 981000-GRAVAR-CONTAINER-SRVC    SECTION.
006860*---------------------------------------------------------------*
006870*    Postar container de requisição de serviço.
006880*---------------------------------------------------------------*
006890     MOVE 0012                   TO MSG1-CD-SEQL-ERRO
006900
006910*    Monta o nome do container de requisicao do servico
006920     MOVE 'PRM-RQSC-SRVC'        TO NM-CTNR
006930     SET PUT-CONTAINER           TO TRUE
006940
006950     EVALUATE NM-PGM
006760         WHEN ATCS6000
006770              MOVE 71739         TO SRVC-ID-SRVC
006780              MOVE 1             TO SRVC-VRS-SRVC
006790              MOVE 292018        TO SRVC-ID-OPR
006800              MOVE 1             TO SRVC-VRS-OPR
006810     END-EVALUATE
006820
006830*POR CAUSA DA CAPTURA AUTOMáTICA DE ERRO CICS, DEVE SER PREVISTO
006840*O ERRO DE CONTAINER
006850     SET  ERRO-CONTAINER         TO TRUE
006860
006870     EXEC CICS PUT
006880               CONTAINER ( NM-CTNR                     )
006890               FROM      ( SRVC-PRM-RQSC-SRVC          )
006900               FLENGTH   ( LENGTH OF SRVC-PRM-RQSC-SRVC)
006910     END-EXEC
006920
006930*AO CHEGAR AQUI, Há CERTEZA DE QUE NãO HOUVE ERRO ALGUM
006940     SET  SEM-ERRO               TO TRUE
006950
007160     .
007170 981000-SAI.
007180     EXIT.
007190*----------------------------------------------------------------*
007200 982000-GRAVAR-CONTAINER-RQSC    SECTION.
007210*----------------------------------------------------------------*
007310
007320     MOVE 0013                   TO  MSG1-CD-SEQL-ERRO
007330
007340*    Monta o nome dos container de acordo com modulo a executar
007350     MOVE SPACES                 TO  NM-CTNR
007360     SET PUT-CONTAINER           TO  TRUE
007370
007450     MOVE NM-PGM        TO  NM-CTNR(1:8)
007460     MOVE '-RQSC'       TO  NM-CTNR(9:5)
007480
007490*Por causa da captura automática de erro CICS, deve ser previsto
007500*o erro de container
007510     SET  ERRO-CONTAINER         TO  TRUE
007520
007530*    Faz put da requisicao do programa a executar
007540     EVALUATE NM-PGM
001530         WHEN SGTSL074
007560             EXEC CICS PUT
007610                        CONTAINER (NM-CTNR)
007620                        FROM      (SGTSL074-RQSC )
007630                        FLENGTH   (LENGTH OF SGTSL074-RQSC )
007640              END-EXEC
001090
006760         WHEN ATCS6000
007420              MOVE 'PRM-DADOS-RQSC'
007400                                 TO NM-CTNR
007600              EXEC CICS PUT
007610                        CONTAINER (NM-CTNR)
007620                        FROM      (ATCS6000-DADOS )
007630                        FLENGTH   (LENGTH OF ATCS6000-DADOS )
007640              END-EXEC
001090
001530         WHEN OTHER
                    SET   ERRO-PROGRAMADO
                                       TO  TRUE
                    MOVE  9004         TO  MSG1-CD-ERRO-TS
                    STRING  'Erro na gravação da requisição do módulo '
                             NM-PGM '.'
                             DELIMITED BY SIZE
                             INTO           MSG1-TX-ERRO-TS
                    PERFORM 999000-ERRO
007700     END-EVALUATE
007710
007720*Ao chegar aqui, há certeza de que não houve erro algum
007730     SET  SEM-ERRO               TO  TRUE
007740
007750     .
007760 982000-SAI.
007770     EXIT.
007780
007790*----------------------------------------------------------------*
007800 983000-EXCLUIR-RESPOSTA-ERRO    SECTION.
007810*----------------------------------------------------------------*
007820* Exclui o container de resposta e de erro para evitar problemas *
007830* em modulos executados mais de uma vez dentro da mesma sessao do*
007840* CICS. Equivale ao initialize na area de resposta dos programas *
007850* que utilizam COMMAREA.                                         *
007860*----------------------------------------------------------------*
007870
007880     MOVE 0014                   TO  MSG1-CD-SEQL-ERRO
007890
007970     MOVE NM-PGM        TO  NM-CTNR(1:8)
007980     MOVE '-RPST'       TO  NM-CTNR(9:5)
008000
008010     SET DEL-CONTAINER           TO  TRUE
008020
008030*    Exclui o container de resposta
008040*    Não há captura automática de erro CICS por usar RESP e RESP2
008050     EXEC CICS DELETE
008060               CONTAINER (NM-CTNR)
008070               RESP      (MSG1-RESP1)
008080               RESP2     (MSG1-RESP2)
008090     END-EXEC
008100
008110     EVALUATE TRUE
008120         WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
001090
008130         WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
008140              CONTINUE
001090
008150         WHEN OTHER
008160              SET ERRO-CONTAINER TO  TRUE
008170              PERFORM 999002-TRATA-CONDITION-CICS
008180     END-EVALUATE
008190
008200*    Excluir o container de erro
008210     PERFORM 983100-EXCLUIR-CONTAINER-ERRO
008220     .
008230 983000-SAI.
008240     EXIT.
008250*
008260*----------------------------------------------------------------*
008270 983100-EXCLUIR-CONTAINER-ERRO   SECTION.
008280*----------------------------------------------------------------*
008290
008300*    Limpar variaveis container de erro
008310     INITIALIZE IIBKERRO-PRM-ERRO
008320
008330*    Exclui o container de erro
008350     MOVE 'PRM-ERRO'             TO  NM-CTNR
008360     SET DEL-CONTAINER           TO  TRUE
008370
008380*    Não há captura automática de erro CICS por usar RESP e RESP2
008390     EXEC CICS DELETE
008400               CONTAINER (NM-CTNR)
008410               RESP      (MSG1-RESP1)
008420               RESP2     (MSG1-RESP2)
008430     END-EXEC
008440
008450     EVALUATE TRUE
008460         WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
001090
008470         WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
008480              CONTINUE
001090
008490         WHEN OTHER
008500              SET ERRO-CONTAINER TO  TRUE
008510              PERFORM 999002-TRATA-CONDITION-CICS
008520     END-EVALUATE
008530     .
008540 983100-SAI.
008550     EXIT.
008560*----------------------------------------------------------------*
008570 984000-GRAVAR-CONTAINER-ERRO    SECTION.
008580*----------------------------------------------------------------*
008610*    Grava o erro gerado no container de erro
008620*    Não há captura automática de erro CICS por usar RESP e RESP2
008630     EXEC CICS PUT
008640               CONTAINER ( 'PRM-ERRO'                  )
008650               FROM      ( IIBKERRO-PRM-ERRO           )
008660               FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
008670               RESP      (MSG1-RESP1                   )
008680               RESP2     (MSG1-RESP2                   )
008690     END-EXEC
008700
008710     .
008720 984000-SAI.
008730     EXIT.
008740
008750*----------------------------------------------------------------*
008760 990000-ENCERRAR                 SECTION.
008770*----------------------------------------------------------------*
008780
008790     GOBACK
008820     .
008830 990000-ENCERRAR-SAI.
008840     EXIT.
008850*
008860*----------------------------------------------------------------*
008870*                                                                *
008880* As sections abaixo iniciadas com 999000 representam os tratamen*
008890* tos de erro utilizando o Sistema MSG (rotina MSGS0001)         *
008900*                                                                *
008910*----------------------------------------------------------------*
008920 999000-ERRO                     SECTION.
008930*----------------------------------------------------------------*
008940
008950     IF MSG1-CD-ERRO-TS EQUAL ZEROS
008960        MOVE 8888                TO MSG1-CD-ERRO-TS
008970     END-IF
001090
008980     IF MSG1-TX-ERRO-TS EQUAL SPACES
008990       MOVE 'Erro não especificado no módulo.' TO MSG1-TX-ERRO-TS
009000     END-IF
001090
009010     PERFORM 999000-GRAVA-ERRO-E-ENCERRA
009020     .
009030 999000-ERRO-SAI.
009040     EXIT.
009050
009060*----------------------------------------------------------------*
009070*     As sections abaixo representam os tratamentos de erro
009080*    utilizando o sistema MSG de mensageria (rotina MSGS0001)
009090*-----------------------------------------------------------------
009100 999000-GRAVA-ERRO-E-ENCERRA     SECTION.
009110*-----------------------------------------------------------------
009120
009130*    Em caso de erro, desfaz as atualizações em banco de dados
009140
009150     EXEC CICS SYNCPOINT
009160               ROLLBACK
009170     END-EXEC
009180
009190*Verifica se o tipo de erro gerado possui tratamento interno
009200     EVALUATE TRUE
009210         WHEN ERRO-CONTAINER
009220              PERFORM 999004-TRATA-ERRO-CTNR
001090
009230         WHEN ERRO-TAMANHO-AREA
009240              PERFORM 999005-TRATA-ERRO-AREA-DADOS
001090
009250         WHEN ERRO-CANAL
009260              PERFORM 999006-TRATA-ERRO-CANAL
001090
009270         WHEN OTHER
009280              CONTINUE
009290     END-EVALUATE
009300
009310*Adiciona as variáveis de entrada para monitoração
001530         MOVE 'SGTS0137-E-CD-CTRA-TIT-VL'  TO  MSGKW999-NM-VRV-MNT
009350         MOVE  SGTS0137-E-CD-CTRA-TIT-VL   TO  MSGKW999-VL-VRV-MNT
009370         PERFORM 999003-ADICIONA-VRV-MNT
001090
001530         MOVE 'SGTS0137-E-CD-OPR-ETRD-MCP' TO  MSGKW999-NM-VRV-MNT
               MOVE 'SGTS0137-E-CD-OPR-ETRD-MCP' TO  MSGKW999-NM-VRV-MNT
               IF  SGTS0137-E-CD-OPR-ETRD-MCP  IS NUMERIC
                    MOVE SGTS0137-E-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP-DISPLAY
                    MOVE CD-OPR-ETRD-MCP-DISPLAY
                                       TO  MSGKW999-VL-VRV-MNT
               ELSE
                    MOVE 'Código não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
003880         END-IF
009370         PERFORM 999003-ADICIONA-VRV-MNT
001090
001530         MOVE 'SGTS0137-E-NR-VRS-OPR-ETRD' TO  MSGKW999-NM-VRV-MNT
009350         MOVE  SGTS0137-E-NR-VRS-OPR-ETRD  TO  MSGKW999-VL-VRV-MNT
009370         PERFORM 999003-ADICIONA-VRV-MNT
001090
001530         MOVE 'SGTS0137-E-NR-SEQL-NOTA-OPR'
001530                                           TO  MSGKW999-NM-VRV-MNT
009350         MOVE  SGTS0137-E-NR-SEQL-NOTA-OPR TO  MSGKW999-VL-VRV-MNT
009370         PERFORM 999003-ADICIONA-VRV-MNT
009400
009410*    Grava o erro gerado no sistema MSG - Mensageria
009420     PERFORM MSG1-NTF-MSG
009430
009440     MOVE MSG1-CD-ERRO-TS        TO  IIBKERRO-CD-ERRO
009450     MOVE MSG1-TX-ERRO-TS        TO  IIBKERRO-TX-ERRO
009460
009470*    Grava o erro gerado no container de erro
009480     PERFORM 984000-GRAVAR-CONTAINER-ERRO
009490
009500*    Encerra a transacao
009510     PERFORM 990000-ENCERRAR
009520     .
009530 999000-SAI.
009540     EXIT.
009550* --------------------------------------*
009560* 999001-TRATA-ABEND-CICS        SECTION.
009570* 999002-TRATA-CONDITION-CICS    SECTION.
009580* 999003-ADICIONA-VRV-MNT        SECTION.
009590* --------------------------------------*
-INC HLPKP999
009610* --------------------------------------*
009620* 999004-TRATA-ERRO-CTNR         SECTION.
009630* 999005-TRATA-ERRO-AREA-DADOS   SECTION.
009640* 999006-TRATA-ERRO-CANAL        SECTION.
009650*---------------------------------------*
-INC SGTKP999
009670* --------------------------------------*
009680* MSG1-NTF-MSG                   SECTION.
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
009690* --------------------------------------*
-INC MSGKPMG1