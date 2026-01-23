000010*-----------------------------------------------------------------
000020* PROGRAMA..: SGTS0044
000030* ANALISTA..: F2054109 - Cintia Aparecida Soares Iglesias
000040* AUTOR.....: F2020377 - Christiano Mauricio Stockler Filho
000050* DATA......: 26/11/2015
000060* OBJETIVO..: Detalhar Agência de Classificação Externa de Risco
000070* COMPILACAO: COBOL/CICS
000080*-----------------------------------------------------------------
      * VRS002 20/03/2019 F9188847 de 2016/145195 - Adequação mensagens
000090* de erro ao padrão da mensageria.
      * VRS001 29/01/2019 F0950413 inclusão da sigla 'SGT' na section
      *                   972000-LER-CONTAINER-DE-ERRO
000090*        26/11/2015 F2020377  DE 2013/120821 - Implantação
000100*-----------------------------------------------------------------
000110
000120*-----------------------------------------------------------------
000130 IDENTIFICATION DIVISION.
000140*----------------------------------------------------------------*
000150 PROGRAM-ID. SGTS0044.
000160*
000170*----------------------------------------------------------------*
000180 ENVIRONMENT DIVISION.
000190*----------------------------------------------------------------*
000200*
000210*----------------------------------------------------------------*
000220 CONFIGURATION                   SECTION.
000230*----------------------------------------------------------------*
000240 SPECIAL-NAMES.
000250     DECIMAL-POINT IS COMMA.
000260*----------------------------------------------------------------*
000270 DATA DIVISION.
000280*----------------------------------------------------------------*
000290 WORKING-STORAGE                 SECTION.
000300*----------------------------------------------------------------*
000310
000320 77  CTE-INICIO-SS               PIC  X(040)    VALUE
000330                                 '*** S.S. COMECA AQUI ***'.
000340
000350 77  CTE-PROG                    PIC  X(018)    VALUE
000360                                 '*** SGTS0044 ***'.
000370 77  CTE-VERS                    PIC  X(006)    VALUE 'VRS002'.
000380
000390* Módulos acionados
000400 77  ATCS6000                    PIC  X(008)    VALUE 'ATCS6000'.
000410 77  IIBP2000                    PIC  X(008)    VALUE 'IIBP2000'.
000420 77  SGTST042                    PIC  X(008)    VALUE 'SGTST042'.
000430 77  SGTST043                    PIC  X(008)    VALUE 'SGTST043'.
000440*----------------------------------------------------------------*
000450 LOCAL-STORAGE                   SECTION.
000460*----------------------------------------------------------------*
000470* Areas de trabalho
000480
000490 01  CONTEUDO-NOME               PIC  X(001)    VALUE ' '.
000500     88  PREENCHER-NOME                         VALUE 'N'.
000510     88  NOME-EM-BRANCO                         VALUE 'S'.
000520*----------------------------------------------------------------*
000530* Book dos Containers e das subrotinas chamadas
000540*----------------------------------------------------------------*
000550* Book do container de requisição e resposta deste módulo
000560 01  SGTS0044-DADOS.
-INC SGTK0044                                                           00000570
000580* CRUD da tabela AG_CLSC_EXNO_RSCO
000590 01  SGTST042-DADOS.
-INC SGTKT042                                                           00000600
000610* CRUD da tabela EST_AG_CLSC_EXNO
000620 01  SGTST043-DADOS.
-INC SGTKT043                                                           00000630
000640* Módulo acionado via serviço IIB para consulta do nome do usuário
000650 01  ATCS6000-DADOS.
-INC ATCK6000                                                           00000660
000670* Book do Container de Erro
-INC IIBKERRO                                                           00000680
000690* Book do container de Serviços (para acionamento de serviços)
-INC IIBKSRVC                                                           00000700
000710*----------------------------------------------------------------*
000720* Book das variáveis auxiliares do tratamento de erro via MSG
000730*----------------------------------------------------------------*
-INC SGTKERR1                                                           00000740
000750* Book para captura de abend cics do bloco código MSGKP999
-INC HLPKW999                                                           00000760
000770* Book para captura de erro via MSG1
-INC MSGKWMG1                                                           00000780
000790
000800 77  CTE-FINAL-SS                PIC  X(040)    VALUE
000810                                 '*** S.S. TERMINA AQUI ***'.
000820
000830*----------------------------------------------------------------*
000840 PROCEDURE DIVISION.
000850*----------------------------------------------------------------*
000860*----------------------------------------------------------------*
000870 000000-ROTINA-PRINCIPAL         SECTION.
000880*----------------------------------------------------------------*
000890
000900     PERFORM 100000-INICIAR
000910
000920     PERFORM 200000-PROCESSAR
000930
000940     PERFORM 990000-ENCERRAR

           GOBACK
000950     .
000960 000000-SAI.
000970     EXIT.
000980*----------------------------------------------------------------*
000990 100000-INICIAR                  SECTION.
001000*----------------------------------------------------------------*
001010     MOVE 0001                   TO MSG1-CD-SEQL-ERRO
001020
001030*    Ativa a captura automática de abend do CICS.
001040*    WHENEVER CICS
-INC HLPKE999                                                           00001050
001060
001070     PERFORM 970000-LER-CANAL
001080
001090*    Define o nome do programa p/ fins manipulação container
001100     MOVE 'SGTS0044'             TO NM-PGM
001110     MOVE 'SGTS0044-RPST'        TO NM-CTNR
001120*    Limpa a resposta e erro antes de executar o módulo
001130     PERFORM 983000-EXCLUIR-RESPOSTA-ERRO
001140
001150     PERFORM 971000-LER-REQUISICAO
001160
001170     PERFORM 110000-VALIDAR-REQUISICAO
001180     .
001190 100000-SAI.
001200     EXIT.
001210*----------------------------------------------------------------*
001220 110000-VALIDAR-REQUISICAO       SECTION.
001230*----------------------------------------------------------------*
001240     MOVE 0002                   TO MSG1-CD-SEQL-ERRO
001250
001260*    Validação tamanho esperado da requisição X tamanho recebido
001270*    na section de leitura de container de requisição 971000
001280
001290     IF  SGTS0044-CD-AG-CLSC-RSCO
001300                                 < ZEROS
001310         SET ERRO-PROGRAMADO     TO TRUE
001320         MOVE 0269               TO MSG1-CD-ERRO-TS
001380         MOVE 'Código agencia classificação risco inválido.'
001390                                 TO MSG1-TX-ERRO-TS
001360         PERFORM 998000-ERRO
001370     END-IF

           IF  SGTS0044-CD-AG-CLSC-RSCO
001300                                 = ZEROS
001310         SET ERRO-PROGRAMADO     TO TRUE
001320         MOVE 0270               TO MSG1-CD-ERRO-TS
001380         MOVE 'Código da agência de classificação de risco não inf
      -             'ormado.'
001390                                 TO MSG1-TX-ERRO-TS
001360         PERFORM 998000-ERRO
001370     END-IF
001380
001390     IF  SGTS0044-NR-ETRD-AG-CLSC
001400                                 < ZEROS
001410         SET ERRO-PROGRAMADO     TO TRUE
001420         MOVE 0271               TO MSG1-CD-ERRO-TS
001430         MOVE 'Número estruturado da agência de classificação de r
      -             'isco inválido.'
001480                                 TO MSG1-TX-ERRO-TS
001470         PERFORM 998000-ERRO
001480     END-IF

           IF  SGTS0044-NR-ETRD-AG-CLSC
001400                                 = ZEROS
001410         SET ERRO-PROGRAMADO     TO TRUE
001420         MOVE 0272               TO MSG1-CD-ERRO-TS
001430         MOVE 'Número estruturado da agência de classificação de r
      -             'isco não informado.'
001480                                 TO MSG1-TX-ERRO-TS
001470         PERFORM 998000-ERRO
001480     END-IF
001490     .
001500 110000-SAI.
001510     EXIT.
001520*----------------------------------------------------------------*
001530 200000-PROCESSAR                SECTION.
001540*----------------------------------------------------------------*
001550     MOVE 0003                   TO MSG1-CD-SEQL-ERRO
001560
001570     PERFORM 210000-CONSULTA-NM-IND-AGENCIA
001580
001590     PERFORM 220000-CONSULTA-ESTADO-AGENCIA
001600
001610     PERFORM 230000-CHAMA-SRVC-IIB-ATCS6000
001620
001630*    Movimenta dados de saída e grava container de Resposta
001640
001650     PERFORM 980000-GRAVAR-CONTAINER-RSPT
001660     .
001670 200000-SAI.
001680     EXIT.
001690*----------------------------------------------------------------*
001700 210000-CONSULTA-NM-IND-AGENCIA  SECTION.
001710*----------------------------------------------------------------*
001720     MOVE 0004                   TO MSG1-CD-SEQL-ERRO
001730
001740     INITIALIZE                  SGTST042-DADOS
001750
001760     SET  SGTST042-CONSULTAR     TO TRUE
001770     MOVE SGTS0044-CD-AG-CLSC-RSCO
001780                                 TO SGTST042-CD-AG-CLSC-RSCO
001790     MOVE SGTS0044-NR-ETRD-AG-CLSC
001800                                 TO SGTST042-NR-ETRD-AG-CLSC
001810
001820     PERFORM 910000-CHAMAR-SGTST042
001830
001840     MOVE SGTST042-IN-AG-CLSC-ATV
001850                                 TO SGTS0044-IN-AG-CLSC-ATV
001860     MOVE SGTST042-NM-AG-CLSC-RSCO
001870                                 TO SGTS0044-NM-AG-CLSC-RSCO
001880     .
001890 210000-SAI.
001900     EXIT.
001910*----------------------------------------------------------------*
001920 220000-CONSULTA-ESTADO-AGENCIA  SECTION.
001930*----------------------------------------------------------------*
001940     MOVE 0005                   TO MSG1-CD-SEQL-ERRO
001950
001960     INITIALIZE                  SGTST043-DADOS
001970
001980     SET  SGTST043-CONSULTAR-ATU TO TRUE
001990     MOVE SGTS0044-CD-AG-CLSC-RSCO
002000                                 TO SGTST043-CD-AG-CLSC-RSCO
002010     MOVE SGTS0044-NR-ETRD-AG-CLSC
002020                                 TO SGTST043-NR-ETRD-AG-CLSC
002030
002040     PERFORM 920000-CHAMAR-SGTST043
002050
002060     MOVE SGTST043-CD-TIP-EST-DCS
002070                                 TO SGTS0044-CD-TIP-EST-DCS
002080
002090     EVALUATE SGTST043-CD-TIP-EST-DCS
002100         WHEN 1
002110              MOVE SGTST043-TS-EST-AG-CLSC
002120                                 TO SGTS0044-TS-INCL
002130              MOVE SGTST043-CD-USU-ATL-EST
002140                                 TO SGTS0044-CD-USU-RSP-INCL
002150         WHEN 2
002160         WHEN 3
002170              MOVE SGTST043-TS-EST-AG-CLSC
002180                                 TO SGTS0044-TS-CNFC
002190              MOVE SGTST043-CD-USU-ATL-EST
002200                                 TO SGTS0044-CD-USU-RSP-CNFC
002210              MOVE SGTST043-TX-OBS-EST-AG-CLSC
002220                                 TO SGTS0044-TX-OBS-EST-AG-CLSC
002230
002240              PERFORM 221000-TRATA-ESTADO-AG-INCL
002250
002260         WHEN 99
002270              MOVE SGTST043-TS-EST-AG-CLSC
002280                                 TO SGTS0044-TS-ECR
002290              MOVE SGTST043-CD-USU-ATL-EST
002300                                 TO SGTS0044-CD-USU-RSP-ECR
002310
002320              PERFORM 221000-TRATA-ESTADO-AG-INCL
002330
002340              PERFORM 222000-TRATA-ESTADO-AG-CNFC
002350
002360     END-EVALUATE
002370     .
002380 220000-SAI.
002390     EXIT.
002400*----------------------------------------------------------------*
002410 221000-TRATA-ESTADO-AG-INCL     SECTION.
002420*----------------------------------------------------------------*
002430     MOVE 0006                   TO MSG1-CD-SEQL-ERRO
002440
002450     INITIALIZE                  SGTST043-DADOS
002460
002470     SET  SGTST043-CONSULTAR-INCL
002480                                 TO TRUE
002490     MOVE SGTS0044-CD-AG-CLSC-RSCO
002500                                 TO SGTST043-CD-AG-CLSC-RSCO
002510     MOVE SGTS0044-NR-ETRD-AG-CLSC
002520                                 TO SGTST043-NR-ETRD-AG-CLSC
002530*    Consulta dados do estado de Inclusão
002540     MOVE 1                      TO SGTST043-CD-TIP-EST-DCS
002550
002560     PERFORM 920000-CHAMAR-SGTST043
002570
002580     MOVE SGTST043-TS-EST-AG-CLSC
002590                                 TO SGTS0044-TS-INCL
002600     MOVE SGTST043-CD-USU-ATL-EST
002610                                 TO SGTS0044-CD-USU-RSP-INCL
002620     .
002630 221000-SAI.
002640     EXIT.
002650*----------------------------------------------------------------*
002660 222000-TRATA-ESTADO-AG-CNFC     SECTION.
002670*----------------------------------------------------------------*
002680     MOVE 0007                   TO MSG1-CD-SEQL-ERRO
002690
002700     INITIALIZE                  SGTST043-DADOS
002710
002720     SET  SGTST043-CONSULTAR-INCL
002730                                 TO TRUE
002740     MOVE SGTS0044-CD-AG-CLSC-RSCO
002750                                 TO SGTST043-CD-AG-CLSC-RSCO
002760     MOVE SGTS0044-NR-ETRD-AG-CLSC
002770                                 TO SGTST043-NR-ETRD-AG-CLSC
002780*    Consulta dados do estado de Confirmação
002790     MOVE 2                      TO SGTST043-CD-TIP-EST-DCS
002800
002810     PERFORM 920000-CHAMAR-SGTST043
002820
002830     MOVE SGTST043-TS-EST-AG-CLSC
002840                                 TO SGTS0044-TS-CNFC
002850     MOVE SGTST043-CD-USU-ATL-EST
002860                                 TO SGTS0044-CD-USU-RSP-CNFC
002870     .
002880 222000-SAI.
002890     EXIT.
002900*----------------------------------------------------------------*
002910 230000-CHAMA-SRVC-IIB-ATCS6000  SECTION.
002920*----------------------------------------------------------------*
002930     MOVE 0008                   TO MSG1-CD-SEQL-ERRO
002940
002950     MOVE ATCS6000               TO NM-PGM
002960
002970*    Consulta nome do usuário responsável pela inclusão
002980     IF  SGTS0044-CD-USU-RSP-INCL
002990                                 NOT EQUAL SPACES
003000
003010         INITIALIZE              ATCS6000-DADOS
003020
003030         SET K6000-PSQ-INF-USU   TO TRUE
003040         MOVE SGTS0044-CD-USU-RSP-INCL
003050                                 TO K6000-CD-USU
003060
003070         PERFORM 900000-CHAMAR-SERVICO
003080
003090         IF  PREENCHER-NOME
003100             MOVE K6000-NM-USU   TO SGTS0044-NM-USU-RSP-INCL
003110         ELSE
003120             IF  NOME-EM-BRANCO
003130                 MOVE SPACES     TO SGTS0044-NM-USU-RSP-INCL
003140             END-IF
003150         END-IF
003160     END-IF
003170
003180*    Consulta nome do usuário responsável pela confirmação
003190     IF  SGTS0044-CD-USU-RSP-CNFC
003200                                 NOT EQUAL SPACES
003210
003220         INITIALIZE              ATCS6000-DADOS
003230
003240         SET K6000-PSQ-INF-USU   TO TRUE
003250
003260         MOVE SGTS0044-CD-USU-RSP-CNFC
003270                                 TO K6000-CD-USU
003280
003290         PERFORM 900000-CHAMAR-SERVICO
003300
003310         IF  PREENCHER-NOME
003320             MOVE K6000-NM-USU   TO SGTS0044-NM-USU-RSP-CNFC
003330         ELSE
003340             IF  NOME-EM-BRANCO
003350                 MOVE SPACES     TO SGTS0044-NM-USU-RSP-CNFC
003360             END-IF
003370         END-IF
003380     END-IF
003390
003400*    Consulta nome do usuário responsável pela confirmação
003410     IF  SGTS0044-CD-USU-RSP-ECR
003420                                 NOT EQUAL SPACES
003430
003440         INITIALIZE              ATCS6000-DADOS
003450
003460         SET K6000-PSQ-INF-USU   TO TRUE
003470
003480         MOVE SGTS0044-CD-USU-RSP-ECR
003490                                 TO K6000-CD-USU
003500
003510         PERFORM 900000-CHAMAR-SERVICO
003520
003530         IF  PREENCHER-NOME
003540             MOVE K6000-NM-USU   TO SGTS0044-NM-USU-RSP-ECR
003550         ELSE
003560             IF  NOME-EM-BRANCO
003570                 MOVE SPACES     TO SGTS0044-NM-USU-RSP-ECR
003580             END-IF
003590         END-IF
003600     END-IF
003610
003620     INITIALIZE                  CONTEUDO-NOME
003630     .
003640 230000-SAI.
003650     EXIT.
003660*---------------------------------------------------------------*
003670 900000-CHAMAR-SERVICO           SECTION.
003680*---------------------------------------------------------------*
003690*    Acionar programa IIBP2000 para requisição do serviço.
003700*---------------------------------------------------------------*
003730*    Grava o container com dados do serviço/operação a chamar
003740     PERFORM 981000-GRAVAR-CONTAINER-SRVC
003750
003760*    Grava o container de requisição do módulo a executar
003770     PERFORM 982000-GRAVAR-CONTAINER-RQSC
003780
003790*    Limpa a resposta e erro antes de executar o módulo
003800     PERFORM 983000-EXCLUIR-RESPOSTA-ERRO
003810
003820*    Não há captura automática de erro CICS por usar RESP e RESP2
003830     EXEC CICS LINK
003840               PROGRAM   ( IIBP2000     )
003850               CHANNEL   ( NM-CNL       )
003860               RESP      ( MSG1-RESP1   )
003870               RESP2     ( MSG1-RESP2   )
003880     END-EXEC
003890
003900     EVALUATE TRUE
003910         WHEN MSG1-RESP1        EQUAL DFHRESP(NORMAL)
003920              CONTINUE
003930         WHEN OTHER
                    SET   ERRO-PROGRAMA-ACIONADO
001000                                 TO  TRUE
001000              MOVE  9000         TO  MSG1-CD-ERRO-TS
001000              STRING 'Erro ao acionar serviço: '
001000                     SRVC-ID-SRVC '.' SRVC-VRS-SRVC '.'
001000                     SRVC-ID-OPR  '.' SRVC-VRS-OPR
001000                     'via IIBP2000, '
001000                     'RESP=' MSG1-RESP1 ', RESP-2=' MSG1-RESP2
001000                     '.'         DELIMITED BY SIZE
001000                                 INTO MSG1-TX-ERRO-TS
001000              SET ERRO-CICS      TO TRUE
004020              PERFORM 998000-ERRO
004030     END-EVALUATE
004040
004050*    Efetua a leitura do Container de Erro
004060     PERFORM 972000-LER-CONTAINER-DE-ERRO
004070
004080*    Efetua a leitura do Container de Resposta
004090     EVALUATE NM-PGM
004100         WHEN ATCS6000
004110              IF  PREENCHER-NOME
004120                  INITIALIZE     CONTEUDO-NOME
004130                  PERFORM 973000-LER-RESPOSTA
004140              ELSE
004150                  IF  NOME-EM-BRANCO
004160                      CONTINUE
004170                  END-IF
004180              END-IF
004190         WHEN OTHER
004200              PERFORM 973000-LER-RESPOSTA
004210     END-EVALUATE
004220     .
004230 900000-SAI.
004240     EXIT.
004250*----------------------------------------------------------------*
004260 910000-CHAMAR-SGTST042          SECTION.
004270*----------------------------------------------------------------*
004280     MOVE 0009                   TO MSG1-CD-SEQL-ERRO
004290
004300*    Movimento nome do módulo para monitoração de variáveis
004310     MOVE SGTST042               TO NM-PGM
004320
004330     EXEC CICS LINK
004340               PROGRAM  ( SGTST042       )
004350               COMMAREA ( SGTST042-DADOS )
004360     END-EXEC
004370
004380     IF  SGTST042-CD-RTN-PGM     NOT EQUAL ZEROS
004390         SET ERRO-PROGRAMA-ACIONADO TO TRUE
004410         MOVE SGTST042-MSG-ERRO-PGM
004420                                 TO MSG1-TX-RTN-PGM-ACND
004430                                    MSG1-TX-ERRO-TS
004440         MOVE NM-PGM             TO MSG1-NM-PGM-ACND
004450         MOVE SGTST042-CD-RTN-PGM
004460                                 TO MSG1-CD-RTN-PGM-ACND
                                          MSG1-CD-ERRO-TS
004480*    Monitorar variáveis do módulo chamado
004490         PERFORM 969000-MONITORAR-VARIAVEIS
004500         PERFORM 998000-ERRO
004510     END-IF
004520     .
004530 910000-SAI.
004540     EXIT.
004550*----------------------------------------------------------------*
004560 920000-CHAMAR-SGTST043          SECTION.
004570*----------------------------------------------------------------*
004580     MOVE 0010                   TO MSG1-CD-SEQL-ERRO
004590
004600*    Movimento nome do módulo para monitoração de variáveis
004610     MOVE SGTST043               TO NM-PGM
004620
004630     EXEC CICS LINK
004640               PROGRAM  ( SGTST043       )
004650               COMMAREA ( SGTST043-DADOS )
004660     END-EXEC
004670
004680     IF  SGTST043-CD-RTN-PGM     NOT EQUAL ZEROS
004690         SET ERRO-PROGRAMA-ACIONADO TO TRUE
004710         MOVE SGTST043-MSG-ERRO-PGM
004720                                 TO MSG1-TX-RTN-PGM-ACND
004730                                    MSG1-TX-ERRO-TS
004740         MOVE NM-PGM             TO MSG1-NM-PGM-ACND
004750         MOVE SGTST043-CD-RTN-PGM
004760                                 TO MSG1-CD-RTN-PGM-ACND
                                          MSG1-CD-ERRO-TS
004770
004780*    Monitorar variáveis do módulo chamado
004790         PERFORM 969000-MONITORAR-VARIAVEIS
004800         PERFORM 998000-ERRO
004810     END-IF
004820     .
004830 920000-SAI.
004840     EXIT.
004850*----------------------------------------------------------------*
004860 969000-MONITORAR-VARIAVEIS      SECTION.
004870*----------------------------------------------------------------*
004880*    Monitora as variáveis dos módulos chamados
004890*----------------------------------------------------------------*
004920     EVALUATE NM-PGM
004930         WHEN SGTST042
                    MOVE 'SGTST042-CD-CHV-USU'
003680                                 TO MSGKW999-NM-VRV-MNT
003690              MOVE SGTST042-CD-CHV-USU
003700                                 TO MSGKW999-VL-VRV-MNT
003710              PERFORM 999003-ADICIONA-VRV-MNT
004940              MOVE 'SGTST042-ACAO'
004950                                 TO MSGKW999-NM-VRV-MNT
004960              MOVE  SGTST042-ACAO
004970                                 TO MSGKW999-VL-VRV-MNT
004980              PERFORM 999003-ADICIONA-VRV-MNT
004990
005000              MOVE 'SGTST042-CD-AG-CLSC-RSCO'
005010                                 TO MSGKW999-NM-VRV-MNT
005020              MOVE  SGTST042-CD-AG-CLSC-RSCO
005030                                 TO MSGKW999-VL-VRV-MNT
005040              PERFORM 999003-ADICIONA-VRV-MNT
005050
005060              MOVE 'SGTST042-NR-ETRD-AG-CLSC'
005070                                 TO MSGKW999-NM-VRV-MNT
005080              MOVE  SGTST042-NR-ETRD-AG-CLSC
005090                                 TO MSGKW999-VL-VRV-MNT
005100              PERFORM 999003-ADICIONA-VRV-MNT
005110
005120         WHEN SGTST043
                    MOVE 'SGTST043-CD-CHV-USU'
003680                                 TO MSGKW999-NM-VRV-MNT
003690              MOVE  SGTST043-CD-CHV-USU
003700                                 TO MSGKW999-VL-VRV-MNT
003710              PERFORM 999003-ADICIONA-VRV-MNT
005130              MOVE 'SGTST043-ACAO'
005140                                 TO MSGKW999-NM-VRV-MNT
005150              MOVE  SGTST043-ACAO
005160                                 TO MSGKW999-VL-VRV-MNT
005170              PERFORM 999003-ADICIONA-VRV-MNT
005180
005190              MOVE 'SGTST043-CD-AG-CLSC-RSCO'
005200                                 TO MSGKW999-NM-VRV-MNT
005210              MOVE  SGTST043-CD-AG-CLSC-RSCO
005220                                 TO MSGKW999-VL-VRV-MNT
005230              PERFORM 999003-ADICIONA-VRV-MNT
005240
005250              MOVE 'SGTST043-NR-ETRD-AG-CLSC'
005260                                 TO MSGKW999-NM-VRV-MNT
005270              MOVE  SGTST043-NR-ETRD-AG-CLSC
005280                                 TO MSGKW999-VL-VRV-MNT
005290              PERFORM 999003-ADICIONA-VRV-MNT
005300
005310         WHEN OTHER
005320              CONTINUE
005330     END-EVALUATE
005340     .
005350 969000-SAI.
005360     EXIT.
005370*----------------------------------------------------------------*
005380 970000-LER-CANAL                SECTION.
005390*    Obtém o canal corrente
005400*----------------------------------------------------------------*
005410     MOVE 0011                   TO MSG1-CD-SEQL-ERRO
005420
005430*    Por causa da captura automática de erro CICS, deve ser
005440*    previsto o erro de container
005450     SET  ERRO-CANAL             TO TRUE
005460
005470     EXEC CICS ASSIGN
005480               CHANNEL ( NM-CNL )
005490     END-EXEC
005500
005510*    Ao chegar aqui, há certeza de que não houve erro algum
005520     SET  SEM-ERRO               TO TRUE
005530     .
005540 970000-FIM.
005550     EXIT.
005560*----------------------------------------------------------------*
005570 971000-LER-REQUISICAO           SECTION.
005580*----------------------------------------------------------------*
005590     MOVE 0012                   TO MSG1-CD-SEQL-ERRO
005600
005610*    Monta o nome do container de requisição
005620     MOVE 'SGTS0044'             TO NM-PGM
005630     MOVE 'SGTS0044-RQSC'        TO NM-CTNR
005640
005650*    Recupera o tamanho do container de requisição
005660     PERFORM 973100-LER-CONTAINER-NODATA
005670
005680*    Obtém o tamanho esperado da requisição
005690     MOVE LENGTH OF SGTS0044-RQSC TO QT-CCT-EPRO
005700
005710*    Valida tamanho esperado da requisição X tamanho recebido
005720
005730     IF  QT-CCT-LIDO             NOT EQUAL QT-CCT-EPRO
005740         SET ERRO-TAMANHO-AREA   TO TRUE
005750         PERFORM 999000-GRAVA-ERRO-E-ENCERRA
005760     END-IF
005770
005780     MOVE 0013                   TO MSG1-CD-SEQL-ERRO
005790
005800*    Configura dados para leitura do container de requisição
005810     SET  GET-CONTAINER          TO TRUE
005820
005830*    Por causa da captura automática de erro CICS, deve ser
005840*    previsto o erro de container
005850     SET  ERRO-CONTAINER         TO TRUE
005860
005870*    Efetua a leitura do container
005880     EXEC CICS GET
005890               CONTAINER ( NM-CTNR       )
005900               FLENGTH   ( QT-CCT-LIDO   )
005910               INTO      ( SGTS0044-RQSC )
005920     END-EXEC
005930
005940*    Ao chegar aqui, há certeza de que não houve erro algum
005950     SET  SEM-ERRO               TO TRUE
005960     .
005970 971000-SAI.
005980     EXIT.
005990*----------------------------------------------------------------*
006000 972000-LER-CONTAINER-DE-ERRO    SECTION.
006010*----------------------------------------------------------------*
006020*    A presença do container PRM-ERRO indica que houve erro de
006030*    integração, a ausência do container indica sucesso.
006040*----------------------------------------------------------------*
006070*    Lê o container de erro dos módulos chamados
006080     SET GET-CONTAINER           TO TRUE
006090     MOVE 'PRM-ERRO'             TO NM-CTNR
006100     INITIALIZE IIBKERRO-PRM-ERRO
006110
006120*    Não há captura automática de erro CICS por usar RESP e RESP2
006130     EXEC CICS GET
006140               CONTAINER ( NM-CTNR                     )
006150               INTO      ( IIBKERRO-PRM-ERRO           )
006160               FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
006170               RESP      ( MSG1-RESP1                  )
006180               RESP2     ( MSG1-RESP2                  )
006190     END-EXEC
006200
006210     EVALUATE TRUE
006220         WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
006230          IF  NM-PGM         EQUAL ATCS6000
006240              SET PREENCHER-NOME TO TRUE
006260          END-IF
006270          CONTINUE
006280         WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
006290*    Tratamento para continuar o processamento em caso de erro
006300          IF  NM-PGM         EQUAL ATCS6000
006310              SET NOME-EM-BRANCO TO TRUE
006330*         Excluir o container de erro
006340              PERFORM 983100-EXCLUIR-CONTAINER-ERRO
006350              CONTINUE
006360          ELSE
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
      *-->       Monitorar variáveis do módulo chamado
                 PERFORM 969000-MONITORAR-VARIAVEIS
                 PERFORM 998000-ERRO
006490          END-IF
006500         WHEN OTHER
006510              SET ERRO-CONTAINER TO TRUE
006520              PERFORM 999002-TRATA-CONDITION-CICS
006530     END-EVALUATE
006540     .
006550 972000-SAI.
006560     EXIT.
006570*----------------------------------------------------------------*
006580 973000-LER-RESPOSTA             SECTION.
006590*----------------------------------------------------------------*
006620     MOVE 'PRM-DADOS-RPST'       TO NM-CTNR
006630
006640*    Recupera o tamanho do container de resposta
006650     PERFORM 973100-LER-CONTAINER-NODATA
006660
006670*    Configura dados para leitura do container de resposta
006680     SET  GET-CONTAINER          TO TRUE
006690
006700*    Por causa da captura automática de erro CICS, deve ser
006710*    previsto o erro de container
006720     SET  ERRO-CONTAINER         TO TRUE
006730
006740*    Lê a RESPOSTA de acordo com o programa que a gravou
006750
006760     MOVE 0014                   TO MSG1-CD-SEQL-ERRO
006770
006780*    Calcular a quantidade de caracteres esperado e limpar
006790*    a área de dados antes de executar o GET
006800*    Para matrizes com dados variáveis (clausula depending
006810*    on) não há como utilizar o comando INITIALIZE
006820
006830     EVALUATE NM-PGM
006840*    Tratamento para continuar o processamento em caso de RC > 0
006850         WHEN ATCS6000
006860              INITIALIZE ATCS6000-DADOS
006870              MOVE LENGTH OF ATCS6000-DADOS TO QT-CCT-EPRO
006880              EXEC CICS GET
006890                        CONTAINER ( NM-CTNR        )
006900                        INTO      ( ATCS6000-DADOS )
006910                        FLENGTH   ( QT-CCT-LIDO    )
006920              END-EXEC
006930              IF  K6000-CD-RTN   EQUAL ZEROS
006940                  SET PREENCHER-NOME
006950                                 TO TRUE
006960              ELSE
006970                  SET NOME-EM-BRANCO
006980                                 TO TRUE
006990              END-IF
007000         WHEN OTHER
007010              STRING
007020              'Get não definido para resposta do módulo ' NM-PGM
007030              DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
007040              PERFORM 998000-ERRO
007050     END-EVALUATE
007060
007070*    Ao chegar aqui, há certeza de que não houve erro algum
007080     SET  SEM-ERRO               TO TRUE
007090     .
007100 973000-SAI.
007110     EXIT.
007120*----------------------------------------------------------------*
007130 973100-LER-CONTAINER-NODATA     SECTION.
007140*----------------------------------------------------------------*
007170*    Configura dados para leitura do container de requisição
007180     SET  GET-CONTAINER          TO TRUE
007190
007200*    Por causa da captura automática de erro CICS, deve ser
007210*    previsto o erro de container
007220     SET  ERRO-CONTAINER         TO TRUE
007230
007240*    Obtém a quantidade de caracteres no container
007250     MOVE ZEROS                  TO QT-CCT-LIDO
007260     EXEC CICS GET
007270               CONTAINER ( NM-CTNR     )
007280               NODATA
007290               FLENGTH   ( QT-CCT-LIDO )
007300     END-EXEC
007310
007320*    Ao chegar aqui, há certeza de que não houve erro algum
007330     SET  SEM-ERRO               TO TRUE
007340     .
007350 973100-SAI.
007360     EXIT.
007370*----------------------------------------------------------------*
007380 980000-GRAVAR-CONTAINER-RSPT    SECTION.
007390*----------------------------------------------------------------*
007400     MOVE 0015                   TO MSG1-CD-SEQL-ERRO
007410
007420*    Exclui eventual container de erro de outros programas
007430     PERFORM 983100-EXCLUIR-CONTAINER-ERRO
007440
007450*    Monta o nome do container de resposta
007460     MOVE 'SGTS0044-RPST'        TO NM-CTNR
007470     SET PUT-CONTAINER           TO TRUE
007480
007490*    Por causa da captura automática de erro CICS, deve ser
007500*    previsto o erro de container
007510     SET  ERRO-CONTAINER         TO TRUE
007520
007530     EXEC CICS PUT
007540               CONTAINER ( NM-CTNR                 )
007550               FROM      ( SGTS0044-RPST           )
007560               FLENGTH   ( LENGTH OF SGTS0044-RPST )
007570     END-EXEC
007580
007590*    Ao chegar aqui, há certeza de que não houve erro algum
007600     SET  SEM-ERRO               TO TRUE
007610     .
007620 980000-SAI.
007630     EXIT.
007640*---------------------------------------------------------------*
007650 981000-GRAVAR-CONTAINER-SRVC    SECTION.
007660*---------------------------------------------------------------*
007670*    Postar container de requisição de serviço.
007680*---------------------------------------------------------------*
007690     MOVE 0016                   TO MSG1-CD-SEQL-ERRO
007700
007710*    Monta o nome do container de requisição do servico
007720     MOVE 'PRM-RQSC-SRVC'        TO NM-CTNR
007730     SET PUT-CONTAINER           TO TRUE
007740
007750     MOVE 71739                  TO SRVC-ID-SRVC
007760     MOVE 1                      TO SRVC-VRS-SRVC
007770     MOVE 292018                 TO SRVC-ID-OPR
007780     MOVE 1                      TO SRVC-VRS-OPR
007790
007800*    Por causa da captura automática de erro CICS, deve ser
007810*    previsto o erro de container
007820     SET  ERRO-CONTAINER         TO TRUE
007830
007840     EXEC CICS PUT
007850               CONTAINER ( NM-CTNR                      )
007860               FROM      ( SRVC-PRM-RQSC-SRVC           )
007870               FLENGTH   ( LENGTH OF SRVC-PRM-RQSC-SRVC )
007880     END-EXEC
007890
007900*    Ao chegar aqui, há certeza de que não houve erro algum
007910     SET  SEM-ERRO               TO TRUE
007920     .
007930 981000-SAI.
007940     EXIT.
007950*----------------------------------------------------------------*
007960 982000-GRAVAR-CONTAINER-RQSC    SECTION.
007970*----------------------------------------------------------------*
007980*    Postar container com dados de requisição (entrada)
007990*----------------------------------------------------------------*
008000     MOVE 0017                   TO MSG1-CD-SEQL-ERRO
008010
008020*    Monta o nome dos container de acordo com módulo a executar
008030     MOVE SPACES                 TO NM-CTNR
008040     SET PUT-CONTAINER           TO TRUE
008050
008060              MOVE 'PRM-DADOS-RQSC'
008070                                 TO NM-CTNR
008080
008090*    Por causa da captura automática de erro CICS, deve ser
008100*    previsto o erro de container
008110     SET  ERRO-CONTAINER         TO TRUE
008120
008130*    Faz put da requisição do programa a executar
008140     EVALUATE NM-PGM
008150         WHEN ATCS6000
008160              EXEC CICS PUT
008170                        CONTAINER ( NM-CTNR                  )
008180                        FROM      ( ATCS6000-DADOS           )
008190                        FLENGTH   ( LENGTH OF ATCS6000-DADOS )
008200              END-EXEC
008210
008220         WHEN OTHER
                    SET   ERRO-PROGRAMADO
001000                                 TO  TRUE
001000              MOVE  9004         TO  MSG1-CD-ERRO-TS
001000              STRING  'Erro na gravação da requisição do módulo.'
001000                       NM-PGM '.'
001000                       DELIMITED BY SIZE
001000                       INTO          MSG1-TX-ERRO-TS
001000              END-STRING
008260              PERFORM 998000-ERRO
008270     END-EVALUATE
008280
008290*    Ao chegar aqui, há certeza de que não houve erro algum
008300     SET  SEM-ERRO               TO TRUE
008310     .
008320 982000-SAI.
008330     EXIT.
008340*----------------------------------------------------------------*
008350 983000-EXCLUIR-RESPOSTA-ERRO    SECTION.
008360*----------------------------------------------------------------*
008370*    Exclui o container de resposta e de erro para evitar        *
008380*    problemas em módulos executados mais de uma vez dentro      *
008390*    da mesma sessão do CICS. Equivale ao initialize na área     *
008400*    de resposta dos programas que utilizam COMMAREA.            *
008410*----------------------------------------------------------------*
008420     MOVE 0018                   TO MSG1-CD-SEQL-ERRO
008430
008440     MOVE 'PRM-DADOS-RPST'       TO NM-CTNR
008450
008460     SET DEL-CONTAINER           TO TRUE
008470
008480*    Exclui o container de resposta
008490*    Não há captura automática de erro CICS por usar RESP e RESP2
008500     EXEC CICS DELETE
008510               CONTAINER ( NM-CTNR    )
008520               RESP      ( MSG1-RESP1 )
008530               RESP2     ( MSG1-RESP2 )
008540     END-EXEC
008550
008560     EVALUATE TRUE
008570         WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
008580         WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
008590              CONTINUE
008600         WHEN OTHER
008610              SET ERRO-CONTAINER TO TRUE
008620              PERFORM 999002-TRATA-CONDITION-CICS
008630     END-EVALUATE
008640
008650*    Excluir o container de erro
008660     PERFORM 983100-EXCLUIR-CONTAINER-ERRO
008670     .
008680 983000-SAI.
008690     EXIT.
008700*----------------------------------------------------------------*
008710 983100-EXCLUIR-CONTAINER-ERRO   SECTION.
008720*----------------------------------------------------------------*
008740
008750*    Limpar variáveis container de erro
008760     INITIALIZE IIBKERRO-PRM-ERRO
008770
008780*    Exclui o container de erro
008790     MOVE 'PRM-ERRO'             TO NM-CTNR
008800     SET DEL-CONTAINER           TO TRUE
008810
008820*    Não há captura automática de erro CICS por usar RESP e RESP2
008830     EXEC CICS DELETE
008840               CONTAINER ( NM-CTNR    )
008850               RESP      ( MSG1-RESP1 )
008860               RESP2     ( MSG1-RESP2 )
008870     END-EXEC
008880
008890     EVALUATE TRUE
008900         WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
008910         WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
008920              CONTINUE
008930         WHEN OTHER
008940              SET ERRO-CONTAINER TO TRUE
008950              PERFORM 999002-TRATA-CONDITION-CICS
008960     END-EVALUATE
008970     .
008980 983100-SAI.
008990     EXIT.
009000*----------------------------------------------------------------*
009010 984000-GRAVAR-CONTAINER-ERRO    SECTION.
009020*----------------------------------------------------------------*
009040
009050*    Grava o erro gerado no container de erro
009060*    Não há captura automática de erro CICS por usar RESP e RESP2
009070     EXEC CICS PUT
009080               CONTAINER ( 'PRM-ERRO'                  )
009090               FROM      ( IIBKERRO-PRM-ERRO           )
009100               FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
009110               RESP      ( MSG1-RESP1                  )
009120               RESP2     ( MSG1-RESP2                  )
009130     END-EXEC
009140     .
009150 984000-SAI.
009160     EXIT.
009170*----------------------------------------------------------------*
009180 990000-ENCERRAR                 SECTION.
009190*----------------------------------------------------------------*
009200     MOVE 0019                   TO MSG1-CD-SEQL-ERRO
009210
009220     EXEC CICS
009230          RETURN
009240     END-EXEC
009250     .
009260 990000-ENCERRAR-SAI.
009270     EXIT.
009280*----------------------------------------------------------------*
009290*    As sections abaixo representam os tratamentos de erro
009300*    utilizando o sistema MSG de mensageria (rotina MSGS0001)
009310*----------------------------------------------------------------*
009320 998000-ERRO                     SECTION.
009330*----------------------------------------------------------------*
009340     MOVE 0020                   TO MSG1-CD-SEQL-ERRO
009350
009360     IF  MSG1-CD-ERRO-TS         EQUAL ZEROS
009370         MOVE 8888               TO MSG1-CD-ERRO-TS
009380     END-IF
009390
009400     IF  MSG1-TX-ERRO-TS         EQUAL SPACES
009410         MOVE 'Erro não especificado no módulo.'
009420                                 TO MSG1-TX-ERRO-TS
009430     END-IF
009440
009450     PERFORM 999000-GRAVA-ERRO-E-ENCERRA
009460     .
009470 998000-ERRO-SAI.
009480     EXIT.
009490*-----------------------------------------------------------------
009500 999000-GRAVA-ERRO-E-ENCERRA     SECTION.
009510*-----------------------------------------------------------------
009520     MOVE 0021                   TO MSG1-CD-SEQL-ERRO
009530
009540*    Em caso de erro, desfaz as atualizaçoes em banco de dados
009550
009560     EXEC CICS SYNCPOINT
009570               ROLLBACK
009580     END-EXEC
009590
009600*    Verifica se o tipo de erro gerado possui tratamento interno
009610     EVALUATE TRUE
009620         WHEN ERRO-CONTAINER
009630              PERFORM 999004-TRATA-ERRO-CTNR
009640         WHEN ERRO-TAMANHO-AREA
009650              PERFORM 999005-TRATA-ERRO-AREA-DADOS
009660         WHEN ERRO-CANAL
009670              PERFORM 999006-TRATA-ERRO-CANAL
009680         WHEN OTHER
009690              CONTINUE
009700     END-EVALUATE
009710
009720*    Adiciona as variáveis de entrada para monitoração

009740         MOVE 'SGTS0044-CD-AG-CLSC-RSCO'
009750                                 TO MSGKW999-NM-VRV-MNT
009760         MOVE  SGTS0044-CD-AG-CLSC-RSCO
009770                                 TO MSGKW999-VL-VRV-MNT
009780         PERFORM 999003-ADICIONA-VRV-MNT
009790
009800         MOVE 'SGTS0044-NR-ETRD-AG-CLSC'
009810                                 TO MSGKW999-NM-VRV-MNT
009820         MOVE  SGTS0044-NR-ETRD-AG-CLSC
009830                                 TO MSGKW999-VL-VRV-MNT
009840         PERFORM 999003-ADICIONA-VRV-MNT
009860
009870*    Grava o erro gerado no sistema MSG - Mensageria
009880     PERFORM MSG1-NTF-MSG
009890
009900*    Movimenta Mensagem e Código de Erro para o Container de Erro
009910     MOVE MSG1-CD-ERRO-TS        TO IIBKERRO-CD-ERRO
009920     MOVE MSG1-TX-ERRO-TS        TO IIBKERRO-TX-ERRO
009930
009940*    Grava o erro gerado no container de erro
009950     PERFORM 984000-GRAVAR-CONTAINER-ERRO
009960
009970*    Encerra a transação
009980     PERFORM 990000-ENCERRAR
009990     .
010000 999000-SAI.
010010     EXIT.
010020* --------------------------------------*
010030* 999001-TRATA-ABEND-CICS        SECTION.
010040* 999002-TRATA-CONDITION-CICS    SECTION.
010050* 999003-ADICIONA-VRV-MNT        SECTION.
010060* --------------------------------------*
-INC HLPKP999                                                           00010070
010080* --------------------------------------*
010090* 999004-TRATA-ERRO-CTNR         SECTION.
010100* 999005-TRATA-ERRO-AREA-DADOS   SECTION.
010110* 999006-TRATA-ERRO-CANAL        SECTION.
010120*---------------------------------------*
-INC SGTKP999                                                           00010130
010140* --------------------------------------*
010150* MSG1-NTF-MSG                   SECTION.
010160* --------------------------------------*
-INC MSGKPMG1                                                           00010170
010180*---------------------- FIM SGTS0044 -----------------------------