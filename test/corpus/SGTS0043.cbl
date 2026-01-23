000010*-----------------------------------------------------------------
000020* Programa..: SGTS0043
000030* Analista..: F8608799 - Ricardo Ramos
000040* Autor.....: F8650584 - Roberto Fernandes Lima
000050* Data......: 19/11/2015
000060* Objetivo..: Consultar Pessoa de Contato Externo.
000070*             Acessa os Modulos ATCS6000, MCIS0001 e SGTSLJ08
000080* Compilacao: Cobol/CICS
000090*-----------------------------------------------------------------
      *  VRS003 06/06/2019 F9188847  DE 2016/145195 - Acerto Mensagem
      *  VRS002 15/03/2018 F8658869  DE 2016/145195 - Tratamento de men-
      *                                               sageria
000100* VRS001 19/11/2015 F8650584  DE 2013/120821 - Implantacao
000110*-----------------------------------------------------------------
000120
000130*-----------------------------------------------------------------
000140 IDENTIFICATION DIVISION.
000150*-----------------------------------------------------------------
000160 PROGRAM-ID. SGTS0043.
000170
000180*-----------------------------------------------------------------
000190 ENVIRONMENT DIVISION.
000200*-----------------------------------------------------------------
000210
000220*-----------------------------------------------------------------
000230 CONFIGURATION                   SECTION.
000240*-----------------------------------------------------------------
000250 SPECIAL-NAMES.
000260     DECIMAL-POINT IS COMMA.
000270
000280*-----------------------------------------------------------------
000290 DATA DIVISION.
000300*-----------------------------------------------------------------
000310
000320*-----------------------------------------------------------------
000330 WORKING-STORAGE                 SECTION.
000340*-----------------------------------------------------------------
000350 77  CTE-INICIO-SS               PIC  X(40)  VALUE
000360              '*** S.S. Comeca Aqui ***'.
000370 77  CTE-PROG                    PIC  X(18)  VALUE
000380              '*** SGTS0043 ***'.
000390 77  CTE-VERS                    PIC  X(06)  VALUE 'VRS003'.
000400
000410*--> Módulos acionados
000420 77  IIBP2000                    PIC  X(08) VALUE 'IIBP2000'.
000430 77  ATCS6000                    PIC  X(08) VALUE 'ATCS6000'.
000440 77  MCIS0001                    PIC  X(08) VALUE 'MCIS0001'.
000450 77  SGTSLJ08                    PIC  X(08) VALUE 'SGTSLJ08'.
000460
000470*-----------------------------------------------------------------
000480 LOCAL-STORAGE                   SECTION.
000490*-----------------------------------------------------------------
000500
000510*--> Areas de trabalho
000520
000530*-----------------------------------------------------------------
000540* Book dos Containers e das subrotinas chamadas
000550*-----------------------------------------------------------------
000560
000570*--> Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1                                                           00000580
000590
000600*--> Book para captura de abend cics da bloco codigo MSGKP999
-INC HLPKW999                                                           00000610
000620
000630*--> Book para captura de erro via MSG1
-INC MSGKWMG1                                                           00000640
000650
000660*--> Book do Container de Erro
-INC IIBKERRO                                                           00000670
000680
000690*--> Book do container de Serviços (para acionamento de serviços)
-INC IIBKSRVC                                                           00000700
000710
000720*--> Book dos dados do usuário
-INC IIBKOPRD                                                           00000730
000740
000750*--> Book do ATCS6000 - via IIB
000760 01  ATCS6000-DADOS.
-INC ATCK6000                                                           00000770
000780
000790*--> Book do MCIS0001 - via IIB
000800 01  MCIS0001-DADOS.
-INC MCIB0001                                                           00000810
000820
000830*--> Book do SGTSLJ08 - via Container
000840*    SGTSLJ08-DADOS.
-INC SGTKLJ08                                                           00000850
000860
000870*--> Book do container de requisição e resposta deste modulo
000880 01  SGTS0043-DADOS.
-INC SGTK0043                                                           00000890
000900
000910 77  CTE-FINAL-SS                  PIC X(40)      VALUE
000920              '*** S.S. Termina Aqui ***'.
000930
000940*-----------------------------------------------------------------
000950 PROCEDURE DIVISION.
000960*-----------------------------------------------------------------
000970
000980*-----------------------------------------------------------------
000990 000000-ROTINA-PRINCIPAL         SECTION.
001000*-----------------------------------------------------------------
001010
001020     PERFORM 100000-INICIAR
001030
001040     PERFORM 200000-PROCESSAR
001050
001060     PERFORM 999999-ENCERRAR
001070
001080     .
001090 000000-SAI.
001100     EXIT.
001110
001120*-----------------------------------------------------------------
001130 100000-INICIAR                  SECTION.
001140*-----------------------------------------------------------------
001150     MOVE  0001                  TO  MSG1-CD-SEQL-ERRO
001160
001170*--> Ativa a captura automatica de abend do CICS.
001180*    WHENEVER CICS
-INC MSGKE999                                                           00001190
001200
001210     PERFORM 970000-LER-CANAL
001220
001230*--> Define o o nome do programa p/ fins manipulacao container
001240     MOVE  'SGTS0043'            TO  NM-PGM
001250     MOVE  'SGTS0043-RPST'       TO  NM-CTNR
001260
001270*--> Limpa a resposta e erro antes de executar o modulo
001280     PERFORM 983000-EXCLUIR-RESPOSTA-ERRO
001290
001300     PERFORM 971000-LER-REQUISICAO
001310
001320     PERFORM 110000-VALIDAR-REQUISICAO
001330     .
001340 100000-SAI.
001350     EXIT.
001360
001370*-----------------------------------------------------------------
001380 110000-VALIDAR-REQUISICAO       SECTION.
001390*-----------------------------------------------------------------
001400     MOVE  0002                  TO  MSG1-CD-SEQL-ERRO
001410
001420*--> Valida campo a campo da requisicao
001430
001440     IF  SGTS0043-CD-PSS-CTT-EXNO <= ZEROS
001450         SET   ERRO-PROGRAMADO   TO  TRUE
001460         MOVE  0059              TO  MSG1-CD-ERRO-TS
001470         MOVE  'Código da pessoa de contato externo inválido.'
001480                                 TO  MSG1-TX-ERRO-TS
001500         PERFORM 999000-ERRO
001510     END-IF
001520
001530     .
001540 110000-SAI.
001550     EXIT.
001560
001570*-----------------------------------------------------------------
001580 200000-PROCESSAR                SECTION.
001590*-----------------------------------------------------------------
001600     MOVE  0003                  TO  MSG1-CD-SEQL-ERRO
001610
001620     INITIALIZE                  SGTSLJ08-DADOS
001630                                 MCIS0001-DADOS
001640                                 ATCS6000-DADOS
001650
001660     PERFORM 210000-LISTAR-PESSOAS
001670
001680     IF  SGTSLJ08-CD-CLI-CIA-SBTC    NOT EQUAL  ZEROS
001690         PERFORM 220000-BUSCAR-NOME-CLIENTE
001700     END-IF
001710
001720     IF  SGTSLJ08-CD-USU-RSP-ATL     NOT EQUAL  SPACES
001730         PERFORM 230000-BUSCAR-NOME-USUARIO
001740     END-IF
001750
001760
001770     PERFORM 240000-MOVIMENTAR-SAIDA
001780     .
001790 200000-SAI.
001800     EXIT.
001810
001820*-----------------------------------------------------------------
001830 210000-LISTAR-PESSOAS           SECTION.
001840*-----------------------------------------------------------------
001850     MOVE  0004                  TO  MSG1-CD-SEQL-ERRO
001860
001870*--> Movimentar o nome do programa a ser executado
001880     MOVE  SGTSLJ08              TO  NM-PGM
001890
001900*--> Movimentar os campos de entrada
001910     MOVE  SGTS0043-CD-PSS-CTT-EXNO
001920                                 TO  SGTSLJ08-CD-PSS-CTT-EXNO
001930
001940     PERFORM 900000-EXECUTAR-MODULO
001950
001960     .
001970 210000-SAI.
001980     EXIT.
001990
002000*-----------------------------------------------------------------
002010 220000-BUSCAR-NOME-CLIENTE      SECTION.
002020*-----------------------------------------------------------------
002030     MOVE  0005                  TO  MSG1-CD-SEQL-ERRO
002040
002050     INITIALIZE                  MCIS0001-DADOS.
002060
002070*--> Movimentar o nome do programa a ser executado
002080     MOVE  MCIS0001              TO  NM-PGM
002090
002100*--> Movimentar os campos de entrada
002110     MOVE  SGTSLJ08-CD-CLI-CIA-SBTC
002120                                 TO  MCIS0001-CLIENTE
002130
002140     PERFORM 910000-CHAMAR-SERVICO
002150
002160     .
002170 220000-SAI.
002180     EXIT.
002190
002200*-----------------------------------------------------------------
002210 230000-BUSCAR-NOME-USUARIO      SECTION.
002220*-----------------------------------------------------------------
002230     MOVE  0006                  TO  MSG1-CD-SEQL-ERRO
002240
002250     INITIALIZE                  ATCS6000-DADOS.
002260
002270*--> Movimentar o nome do programa a ser executado
002280     MOVE  ATCS6000              TO  NM-PGM
002290
002300*--> Movimentar os campos de entrada
002310     SET   K6000-PSQ-INF-USU     TO  TRUE
002320     MOVE  SGTSLJ08-CD-USU-RSP-ATL
002330                                 TO  K6000-CD-USU
002340
002350     PERFORM 910000-CHAMAR-SERVICO
002360     .
002370 230000-SAI.
002380     EXIT.
002390
002400*-----------------------------------------------------------------
002410 240000-MOVIMENTAR-SAIDA         SECTION.
002420*-----------------------------------------------------------------
002430     MOVE  0007                  TO  MSG1-CD-SEQL-ERRO
002440
002450     MOVE  SGTSLJ08-IN-CTT-EXNO-ATI
002460                                 TO  SGTS0043-IN-CTT-EXNO-ATI
002470     MOVE  SGTSLJ08-TX-EMAIL-CTT-EXNO
002480                                 TO  SGTS0043-TX-EMAIL-CTT-EXNO
002490
002500     MOVE  SGTSLJ08-NM-PSS-CTT-EXNO
002510                                 TO  SGTS0043-NM-PSS-CTT-EXNO
002520     IF  MCIS0001-NOME               NOT EQUAL  SPACES
002530         MOVE  MCIS0001-NOME     TO  SGTS0043-NM-PSS-CTT-EXNO
002540     END-IF
002550
002560     MOVE  SGTSLJ08-NM-CIA-CTT-EXNO
002570                                 TO  SGTS0043-NM-CIA-CTT-EXNO
002580     MOVE  SGTSLJ08-CD-CNPJ
002590                                 TO  SGTS0043-CD-CNPJ
002600     MOVE  SGTSLJ08-CD-CLI-CIA-SBTC
002610                                 TO  SGTS0043-CD-CLI-CIA-SBTC
002620     MOVE  SGTSLJ08-NM-CRG-CTT-EXNO
002630                                 TO  SGTS0043-NM-CRG-CTT-EXNO
002640     MOVE  SGTSLJ08-CD-USU-RSP-ATL
002650                                 TO  SGTS0043-CD-USU-RSP-ATL
002660
002670     IF  K6000-NM-USU                NOT EQUAL  SPACES
002680         MOVE  K6000-NM-USU      TO  SGTS0043-NM-USU-RSP-ATL
002690     END-IF
002700
002710     MOVE  SGTSLJ08-TS-ATL-CTT-EXNO
002720                                 TO  SGTS0043-TS-ATL-CTT-EXNO
002730     MOVE  SGTSLJ08-NR-SEQL-CML
002740                                 TO  SGTS0043-NR-SEQL-CML
002750     MOVE  SGTSLJ08-CD-DDD-CML
002760                                 TO  SGTS0043-CD-DDD-CML
002770     MOVE  SGTSLJ08-NR-TEL-CML
002780                                 TO  SGTS0043-NR-TEL-CML
002790     MOVE  SGTSLJ08-NR-SEQL-CELR
002800                                 TO  SGTS0043-NR-SEQL-CELR
002810     MOVE  SGTSLJ08-CD-DDD-CELR
002820                                 TO  SGTS0043-CD-DDD-CELR
002830     MOVE  SGTSLJ08-NR-TEL-CELR
002840                                 TO  SGTS0043-NR-TEL-CELR
002850
002860*--> Gravando o container de resposta da subrotina
002870     PERFORM 980000-GRAVAR-CONTAINER-RSPT
002880
002890     .
002900 240000-SAI.
002910     EXIT.
002920
002930*-----------------------------------------------------------------
002940*    Section da Faixa de 900000 a 969900 para Demais Rotinas
002950*-----------------------------------------------------------------
002960
002970*-----------------------------------------------------------------
002980 900000-EXECUTAR-MODULO          SECTION.
002990*--> Executa um modulo implementado com CONTAINER
003000*    Nao incluir nessa section os modulos chamados via COMMAREA
003010*-----------------------------------------------------------------
003030
003040*--> Grava o container de requisicao do modulo a executar
003050     PERFORM 982000-GRAVAR-CONTAINER-RQSC
003060
003070*--> Limpa a resposta e erro antes de executar o modulo
003080     PERFORM 983000-EXCLUIR-RESPOSTA-ERRO
003090
003100*--> Não há captura automática de erro CICS por usar RESP e RESP2
003110     EVALUATE  NM-PGM
003120         WHEN  SGTSLJ08
003130               EXEC CICS LINK
003140                         PROGRAM ( SGTSLJ08                      )
003150                         CHANNEL ( NM-CNL                        )
003160                         RESP    ( MSG1-RESP1)
003170                         RESP2   ( MSG1-RESP2)
003180               END-EXEC
003190         WHEN  OTHER
                     SET ERRO-PROGRAMADO TO TRUE
003200               MOVE 9006           TO MSG1-CD-ERRO-TS
                     MOVE 'Não definida a execução do módulo.'
003220                                   TO MSG1-TX-ERRO-TS
003230               PERFORM 999000-ERRO
003240     END-EVALUATE
003250
003260     EVALUATE  TRUE
003270         WHEN  MSG1-RESP1        EQUAL  DFHRESP(NORMAL)
003280               CONTINUE
003290         WHEN  OTHER
003300               STRING
003310               'Erro na execução do módulo ' NM-PGM ', '
003320               'RESP  = ' MSG1-RESP1  ', '
003330               'RESP2 = ' MSG1-RESP2
003340               DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
003350               MOVE  3004        TO  MSG1-CD-ERRO-TS
003360               SET   ERRO-CICS   TO  TRUE
003370               PERFORM 999000-ERRO
003380     END-EVALUATE
003390
003400*--> Efetua a leitura do Container de Erro
003410     PERFORM 972000-LER-CONTAINER-DE-ERRO
003420
003430*--> Efetua a leitura do Container de Resposta
003440     PERFORM 973000-LER-RESPOSTA
003450
003460     .
003470 900000-SAI.
003480     EXIT.
003490
003500*-----------------------------------------------------------------
003510 910000-CHAMAR-SERVICO           SECTION.
003520*--> Acionar programa IIBP2000 para requisição do serviço.
003530*-----------------------------------------------------------------

003560*--> Grava o container com dados do serviço/operação a chamar
003570     PERFORM 981000-GRAVAR-CONTAINER-SRVC
003580
003590*--> Grava o container de requisicao do modulo a executar
003600     PERFORM 982000-GRAVAR-CONTAINER-RQSC
003610
003620*--> Limpa a resposta e erro antes de executar o modulo
003630     PERFORM 983000-EXCLUIR-RESPOSTA-ERRO
003640
003650*--> Não há captura automática de erro CICS por usar RESP e RESP2
003660     EXEC CICS LINK
003670               PROGRAM   ( IIBP2000                              )
003680               CHANNEL   ( NM-CNL                                )
003690               RESP      ( MSG1-RESP1                            )
003700               RESP2     ( MSG1-RESP2                            )
003710     END-EXEC
003720
003730     EVALUATE TRUE
003740         WHEN MSG1-RESP1        EQUAL DFHRESP(NORMAL)
003750              CONTINUE
003760         WHEN OTHER
                    MOVE  9000   TO  MSG1-CD-ERRO-TS
003770              STRING 'Erro ao acionar serviço: '
003780                     SRVC-ID-SRVC '.' SRVC-VRS-SRVC '.'
003790                     SRVC-ID-OPR  '.' SRVC-VRS-OPR
003800                     'via IIBP2000, '
003810                     'RESP=' MSG1-RESP1 ', RESP-2=' MSG1-RESP2
003820                     '.'         DELIMITED BY SIZE
003830                                 INTO MSG1-TX-ERRO-TS
003840              SET   ERRO-CICS    TO  TRUE
003850              PERFORM 999000-ERRO
003860     END-EVALUATE
003870
003880*--> Efetua a leitura do Container de Erro
003890     PERFORM 972000-LER-CONTAINER-DE-ERRO
003900
003910*--> Efetua a leitura do Container de Resposta
003920     PERFORM 973000-LER-RESPOSTA
003930     .
003940 910000-SAI.
003950     EXIT.
003960
003970*-----------------------------------------------------------------
003980 969000-MONITORAR-VARIAVEIS      SECTION.
003990*--> Monitora as variáveis dos módulos chamados
004000*-----------------------------------------------------------------
004020
004030     EVALUATE  NM-PGM
004040         WHEN  ATCS6000
004050               MOVE  'K6000-CD-USU'
004060                                 TO  MSGKW999-NM-VRV-MNT
004070               MOVE  K6000-CD-USU
004080                                 TO  MSGKW999-VL-VRV-MNT
004090               PERFORM  999003-ADICIONA-VRV-MNT
004100         WHEN  MCIS0001
004110               MOVE  'MCIS0001-CLIENTE'
004120                                 TO  MSGKW999-NM-VRV-MNT
004130               MOVE  MCIS0001-CLIENTE
004140                                 TO  MSGKW999-VL-VRV-MNT
004150               PERFORM  999003-ADICIONA-VRV-MNT
004160         WHEN  SGTSLJ08
004170               MOVE  'SGTSLJ08-CD-PSS-CTT-EXNO'
004180                                 TO  MSGKW999-NM-VRV-MNT
004190               MOVE  SGTSLJ08-CD-PSS-CTT-EXNO
004200                                 TO  MSGKW999-VL-VRV-MNT
004210               PERFORM  999003-ADICIONA-VRV-MNT
004220         WHEN  OTHER
004230               CONTINUE
004240     END-EVALUATE
004250
004260     .
004270 969000-SAI.
004280     EXIT.
004290
004300*-----------------------------------------------------------------
004310*    Section da Faixa de 970000 a 979900 para Leitura de Dados
004320*-----------------------------------------------------------------
004330
004340*-----------------------------------------------------------------
004350 970000-LER-CANAL                SECTION.
004360*--> Obtem o canal corrente
004370*-----------------------------------------------------------------
004380     MOVE  0008                  TO  MSG1-CD-SEQL-ERRO
004390
004400*--> Por causa da captura automática de erro CICS, deve ser
004410*    previsto o erro de container
004420     SET   ERRO-CANAL            TO  TRUE
004430
004440     EXEC CICS ASSIGN
004450               CHANNEL   ( NM-CNL                                )
004460     END-EXEC
004470
004480*--> Ao chegar aqui, há certeza de que não houve erro algum
004490     SET   SEM-ERRO              TO  TRUE
004500     .
004510 970000-FIM.
004520     EXIT.
004530
004540*-----------------------------------------------------------------
004550 971000-LER-REQUISICAO           SECTION.
004560*-----------------------------------------------------------------
004570     MOVE  0009                  TO  MSG1-CD-SEQL-ERRO
004580
004590*--> Monta o nome do container de requisicao
004600     MOVE  'SGTS0043'            TO  NM-PGM
004610     MOVE  'SGTS0043-RQSC'       TO  NM-CTNR
004620
004630*--> Recupera o tamanho do container de requisição
004640     PERFORM 973100-LER-CONTAINER-NODATA
004650
004660*--> Obtem o tamanho esperado da requisicao
004670     MOVE  LENGTH OF SGTS0043-RQSC   TO  QT-CCT-EPRO
004680
004690*--> Valida tamanho esperado da requisicao X tamanho recebido
004700
004710     IF  QT-CCT-LIDO                NOT EQUAL  QT-CCT-EPRO
004720         SET   ERRO-TAMANHO-AREA TO  TRUE
004730         PERFORM 999000-GRAVA-ERRO-E-ENCERRA
004740     END-IF
004750
004760     MOVE  0010                  TO  MSG1-CD-SEQL-ERRO
004770
004780*--> Configura dados para leitura do container de requisição
004790     SET   GET-CONTAINER         TO  TRUE
004800
004810*--> Por causa da captura automática de erro CICS, deve ser
004820*    previsto o erro de container
004830     SET   ERRO-CONTAINER        TO  TRUE
004840
004850*--> Efetua a leitura do container
004860     EXEC CICS GET
004870               CONTAINER ( NM-CTNR                               )
004880               FLENGTH   ( QT-CCT-LIDO                           )
004890               INTO      ( SGTS0043-RQSC                         )
004900     END-EXEC
004910
004920*--> Ao chegar aqui, há certeza de que não houve erro algum
004930     SET   SEM-ERRO              TO  TRUE
004940
004950     .
004960 971000-SAI.
004970     EXIT.
004980
004990*-----------------------------------------------------------------
005000 972000-LER-CONTAINER-DE-ERRO    SECTION.
005010*--> A presença do container PRM-ERRO indica que houve erro de
005020*    integração, a ausência do container indica sucesso.
005030*-----------------------------------------------------------------
005050
005060*--> Le o container de erro dos modulos chamados
005070     SET   GET-CONTAINER         TO  TRUE
005080     MOVE  'PRM-ERRO'            TO  NM-CTNR
005090     INITIALIZE  IIBKERRO-PRM-ERRO
005100
005110*--> Não há captura automática de erro CICS por usar RESP e RESP2
005120     EXEC CICS GET
005130               CONTAINER ( NM-CTNR                               )
005140               INTO      ( IIBKERRO-PRM-ERRO                     )
005150               FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO           )
005160               RESP      ( MSG1-RESP1                            )
005170               RESP2     ( MSG1-RESP2                            )
005180     END-EXEC
005190
005200     EVALUATE  TRUE
005210*------> Caso deseje personalizar ou não interromper o
005220*        processamento, alterar para não dar erro aqui SOMENTE
005230*        PARA O MODULO CHAMADO e tratar na section chamadora
005240         WHEN  MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
005250               CONTINUE
005260         WHEN  MSG1-RESP1 EQUAL DFHRESP(NORMAL)
005270*--> Trata o erro de programa acionado por ser módulo interno
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
005370*------------> Monitorar variáveis do módulo chamado
005380               PERFORM 969000-MONITORAR-VARIAVEIS
005390               PERFORM 999000-ERRO
005400         WHEN  OTHER
005410               SET   ERRO-CONTAINER
005420                                 TO  TRUE
005430               PERFORM 999002-TRATA-CONDITION-CICS
005440     END-EVALUATE
005450     .
005460 972000-SAI.
005470     EXIT.
005480
005490*-----------------------------------------------------------------
005500 973000-LER-RESPOSTA             SECTION.
005510*--> Recupera o container com os dados de resposta (saída).
005520*    Modulo Container  -     NOME_PGM-RPST
005530*    Modulo IIB (Fixo) -     PRM-DADOS-RPST
005540*-----------------------------------------------------------------
005560
005570*--> Monta o nome dos container de acordo com modulo a executar
005580     MOVE  SPACES                TO  NM-CTNR
005590
005600*--> Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
005610*    nome_modulo -RPST
005620     EVALUATE  NM-PGM
005630*------> Modulos via IIB
005640         WHEN  'ATCS6000'
005650         WHEN  'MCIS0001'
005660               MOVE  'PRM-DADOS-RPST'
005670                                 TO  NM-CTNR
005680         WHEN  OTHER
005690               MOVE  NM-PGM      TO  NM-CTNR(1:8)
005700               MOVE  '-RPST'     TO  NM-CTNR(9:5)
005710     END-EVALUATE
005720
005730*--> Recupera o tamanho do container de resposta
005740     PERFORM 973100-LER-CONTAINER-NODATA
005750
005760*--> Configura dados para leitura do container de resposta
005770     SET   GET-CONTAINER         TO  TRUE
005780
005790*--> Por causa da captura automática de erro CICS, deve ser
005800*    previsto o erro de container
005810     SET   ERRO-CONTAINER        TO  TRUE
005820
005830
005840*--> Le a RESPOSTA de acordo com o programa que a gravou
005850
005860     MOVE  0011                  TO  MSG1-CD-SEQL-ERRO
005870
005880     EVALUATE  NM-PGM
005890*------> Calcular a quantidade de caracteres esperado e
005900*        limpar a area de dados antes de executar o GET
005910*        Para matrizes com dados variaveis (clausula depending
005920*        on) nao ha como utilizar o comando INITIALIZE
005930         WHEN  ATCS6000
005940               INITIALIZE ATCS6000-DADOS
005950               MOVE  LENGTH OF ATCS6000-DADOS  TO  QT-CCT-EPRO
005960               EXEC CICS GET
005970                         CONTAINER ( NM-CTNR                     )
005980                         INTO      ( ATCS6000-DADOS              )
005990                         FLENGTH   ( QT-CCT-LIDO                 )
006000               END-EXEC
006010         WHEN  MCIS0001
006020               INITIALIZE MCIS0001-DADOS
006030               MOVE  LENGTH OF MCIS0001-DADOS  TO  QT-CCT-EPRO
006040               EXEC CICS GET
006050                         CONTAINER ( NM-CTNR                     )
006060                         INTO      ( MCIS0001-DADOS              )
006070                         FLENGTH   ( QT-CCT-LIDO                 )
006080               END-EXEC
006090         WHEN  SGTSLJ08
006100               INITIALIZE SGTSLJ08-RPST
006110               MOVE  LENGTH OF SGTSLJ08-RPST   TO  QT-CCT-EPRO
006120               EXEC CICS GET
006130                         CONTAINER ( NM-CTNR                     )
006140                         INTO      ( SGTSLJ08-RPST               )
006150                         FLENGTH   ( QT-CCT-LIDO                 )
006160               END-EXEC
006170         WHEN OTHER
                    SET ERRO-PROGRAMADO TO TRUE
006180              MOVE 9003           TO  MSG1-CD-ERRO-TS
006190              STRING  'Erro na leitura da resposta do módulo '
                             NM-PGM '.'
                             DELIMITED BY SIZE
                             INTO          MSG1-TX-ERRO-TS
                    END-STRING
006210              PERFORM 999000-ERRO
006220     END-EVALUATE
006230
006240*--> Ao chegar aqui, há certeza de que não houve erro algum
006250     SET   SEM-ERRO              TO  TRUE
006260     .
006270 973000-SAI.
006280     EXIT.
006290
006300*-----------------------------------------------------------------
006310 973100-LER-CONTAINER-NODATA     SECTION.
006320*-----------------------------------------------------------------
006340
006350*--> Configura dados para leitura do container de requisição
006360     SET   GET-CONTAINER         TO  TRUE
006370
006380*--> Por causa da captura automática de erro CICS, deve ser
006390*    previsto o erro de container
006400     SET   ERRO-CONTAINER        TO  TRUE
006410
006420*--> Obtem a quantidade de caracteres no container
006430     MOVE  ZEROS                 TO  QT-CCT-LIDO
006440     EXEC CICS GET
006450               CONTAINER (NM-CTNR)
006460               NODATA
006470               FLENGTH   (QT-CCT-LIDO)
006480     END-EXEC
006490
006500*--> Ao chegar aqui, há certeza de que não houve erro algum
006510     SET   SEM-ERRO              TO  TRUE
006520     .
006530 973100-SAI.
006540     EXIT.
006550
006560*-----------------------------------------------------------------
006570*    Section da Faixa de 980000 a 989900 para Gravacao de Dados
006580*-----------------------------------------------------------------
006590
006600*-----------------------------------------------------------------
006610 980000-GRAVAR-CONTAINER-RSPT    SECTION.
006620*-----------------------------------------------------------------
006630     MOVE  0012                  TO  MSG1-CD-SEQL-ERRO
006640
006650*--> Exclui eventual container de erro de outros programas
006660     PERFORM 983100-EXCLUIR-CONTAINER-ERRO
006670
006680*--> Monta o nome do container de resposta
006690     MOVE  'SGTS0043-RPST'       TO  NM-CTNR
006700     SET   PUT-CONTAINER         TO  TRUE
006710
006720*--> Por causa da captura automática de erro CICS, deve ser
006730*    previsto o erro de container
006740     SET   ERRO-CONTAINER        TO  TRUE
006750
006760     EXEC CICS PUT
006770               CONTAINER ( NM-CTNR                               )
006780               FROM      ( SGTS0043-RPST                         )
006790               FLENGTH   ( LENGTH OF SGTS0043-RPST               )
006800     END-EXEC
006810
006820*--> Ao chegar aqui, há certeza de que não houve erro algum
006830     SET   SEM-ERRO              TO  TRUE
006840     .
006850 980000-SAI.
006860     EXIT.
006870
006880*-----------------------------------------------------------------
006890 981000-GRAVAR-CONTAINER-SRVC    SECTION.
006900*--> Postar container de requisição de serviço.
006910*-----------------------------------------------------------------
006920     MOVE  0013                  TO  MSG1-CD-SEQL-ERRO
006930
006940*--> Monta o nome do container de requisicao do servico
006950     MOVE  'PRM-RQSC-SRVC'       TO  NM-CTNR
006960     SET   PUT-CONTAINER         TO  TRUE
006970
006980     EVALUATE  NM-PGM
006990         WHEN  ATCS6000
007000               MOVE  0071739     TO  SRVC-ID-SRVC
007010               MOVE  0001        TO  SRVC-VRS-SRVC
007020               MOVE  0292018     TO  SRVC-ID-OPR
007030               MOVE  0001        TO  SRVC-VRS-OPR
007040         WHEN  MCIS0001
007050               MOVE  0019485     TO  SRVC-ID-SRVC
007060               MOVE  0001        TO  SRVC-VRS-SRVC
007070               MOVE  0207827     TO  SRVC-ID-OPR
007080               MOVE  0001        TO  SRVC-VRS-OPR
007090     END-EVALUATE
007100
007110*--> Por causa da captura automática de erro CICS, deve ser
007120*    previsto erro de container
007130     SET   ERRO-CONTAINER        TO  TRUE
007140
007150     EXEC CICS PUT
007160               CONTAINER ( NM-CTNR                               )
007170               FROM      ( SRVC-PRM-RQSC-SRVC                    )
007180               FLENGTH   ( LENGTH OF SRVC-PRM-RQSC-SRVC)
007190     END-EXEC
007200
007210*--> Ao chegar aqui, há certeza de que não houve erro algum
007220     SET   SEM-ERRO              TO  TRUE
007230
007240     .
007250 981000-SAI.
007260     EXIT.
007270*-----------------------------------------------------------------
007280 982000-GRAVAR-CONTAINER-RQSC    SECTION.
007290*--> Postar container com dados de requisição (entrada)
007300*    Modulo CONTAINER  -     NOME_PGM-RQSC
007310*    Modulo IIB (FIXO) -     PRM-DADOS-RQSC
007320*-----------------------------------------------------------------
007330     MOVE  0014                  TO  MSG1-CD-SEQL-ERRO
007340
007350*--> Monta o nome dos container de acordo com modulo a executar
007360     MOVE  SPACES                TO  NM-CTNR
007370     SET   PUT-CONTAINER         TO  TRUE
007380
007390*--> Quando módulos via IIB, fixa nome PRM-DADOS-RQSC,
007400*    senão NOME_MODULO-RPST
007410     EVALUATE  NM-PGM
007420*------> Modulo via IIB
007430         WHEN  'ATCS6000'
007440         WHEN  'MCIS0001'
007450               MOVE 'PRM-DADOS-RQSC'
007460                                 TO  NM-CTNR
007470         WHEN  OTHER
007480               MOVE  NM-PGM      TO  NM-CTNR(1:8)
007490               MOVE  '-RQSC'     TO  NM-CTNR(9:5)
007500     END-EVALUATE
007510
007520*--> Por causa da captura automática de erro CICS, deve ser
007530*    previsto o erro de container
007540     SET   ERRO-CONTAINER        TO  TRUE
007550
007560*--> Faz PUT da requisicao do programa a executar
007570     EVALUATE  NM-PGM
007580         WHEN  ATCS6000
007590               EXEC CICS PUT
007600                         CONTAINER ( NM-CTNR                     )
007610                         FROM      ( ATCS6000-DADOS              )
007620                         FLENGTH   ( LENGTH OF ATCS6000-DADOS    )
007630               END-EXEC
007640         WHEN  MCIS0001
007650               EXEC CICS PUT
007660                         CONTAINER ( NM-CTNR                     )
007670                         FROM      ( MCIS0001-DADOS              )
007680                         FLENGTH   ( LENGTH OF MCIS0001-DADOS    )
007690               END-EXEC
007700         WHEN  SGTSLJ08
007710               EXEC CICS PUT
007720                         CONTAINER ( NM-CTNR                     )
007730                         FROM      ( SGTSLJ08-RQSC               )
007740                         FLENGTH   ( LENGTH OF SGTSLJ08-RQSC     )
007750               END-EXEC
007760
007770         WHEN OTHER
                    SET ERRO-PROGRAMADO  TO TRUE
007780              MOVE  9004         TO  MSG1-CD-ERRO-TS
                    STRING  'Erro na gravação da requisição do módulo.'
                             NM-PGM '.'
                             DELIMITED BY SIZE
                             INTO          MSG1-TX-ERRO-TS
                    END-STRING
007810              PERFORM 999000-ERRO
007820     END-EVALUATE
007830
007840*--> Ao chegar aqui, há certeza de que não houve erro algum
007850     SET   SEM-ERRO              TO  TRUE
007860     .
007870 982000-SAI.
007880     EXIT.
007890
007900*-----------------------------------------------------------------
007910 983000-EXCLUIR-RESPOSTA-ERRO    SECTION.
007920*-----------------------------------------------------------------
007930*--> Exclui o container de resposta e de erro para evitar
007940*    problemas em modulos executados mais de uma vez dentro da
007950*    mesma sessao do CICS.
007960*    Equivale ao initialize na area de resposta dos programas
007970*    que utilizam COMMAREA.
007980*-----------------------------------------------------------------
007990     MOVE  0015                  TO  MSG1-CD-SEQL-ERRO
008000
008010*--> Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
008020*    nome_modulo -RPST
008030     EVALUATE  NM-PGM
008040*------> Modulo via IIB
008050         WHEN  'ATCS6000'
008060         WHEN  'MCIS0001'
008070               MOVE  'PRM-DADOS-RPST'
008080                                 TO  NM-CTNR
008090         WHEN  OTHER
008100               MOVE  NM-PGM      TO  NM-CTNR(1:8)
008110               MOVE  '-RPST'     TO  NM-CTNR(9:5)
008120     END-EVALUATE
008130
008140     SET   DEL-CONTAINER         TO  TRUE
008150
008160*--> Exclui o container de resposta
008170*    Não há captura automática de erro CICS por usar RESP e RESP2
008180     EXEC CICS DELETE
008190               CONTAINER (NM-CTNR)
008200               RESP      (MSG1-RESP1)
008210               RESP2     (MSG1-RESP2)
008220     END-EXEC
008230
008240     EVALUATE  TRUE
008250         WHEN  MSG1-RESP1  EQUAL  DFHRESP(CONTAINERERR)
008260         WHEN  MSG1-RESP1  EQUAL  DFHRESP(NORMAL)
008270               CONTINUE
008280         WHEN  OTHER
008290               SET   ERRO-CONTAINER
008300                                 TO  TRUE
008310               PERFORM 999002-TRATA-CONDITION-CICS
008320     END-EVALUATE
008330
008340*--> Excluir o container de erro
008350     PERFORM 983100-EXCLUIR-CONTAINER-ERRO
008360     .
008370 983000-SAI.
008380     EXIT.
008390
008400*-----------------------------------------------------------------
008410 983100-EXCLUIR-CONTAINER-ERRO   SECTION.
008420*-----------------------------------------------------------------
008440
008450*--> Limpar variaveis containerde erro
008460     INITIALIZE IIBKERRO-PRM-ERRO
008470
008480*--> Exclui o container de erro
008490     MOVE  'PRM-ERRO'            TO  NM-CTNR
008500     SET   DEL-CONTAINER         TO  TRUE
008510
008520*--> Não há captura automática de erro CICS por usar RESP e RESP2
008530     EXEC CICS DELETE
008540               CONTAINER (NM-CTNR)
008550               RESP      (MSG1-RESP1)
008560               RESP2     (MSG1-RESP2)
008570     END-EXEC
008580
008590     EVALUATE  TRUE
008600         WHEN  MSG1-RESP1  EQUAL  DFHRESP(CONTAINERERR)
008610         WHEN  MSG1-RESP1  EQUAL  DFHRESP(NORMAL)
008620               CONTINUE
008630         WHEN  OTHER
008640               SET   ERRO-CONTAINER
008650                                 TO  TRUE
008660               PERFORM 999002-TRATA-CONDITION-CICS
008670     END-EVALUATE
008680     .
008690 983100-SAI.
008700     EXIT.
008710
008720*-----------------------------------------------------------------
008730 984000-GRAVAR-CONTAINER-ERRO    SECTION.
008740*-----------------------------------------------------------------
008760
008770*--> Grava o erro gerado no container de erro
008780*    Não há captura automática de erro CICS por usar RESP e RESP2
008790     EXEC CICS PUT
008800               CONTAINER ( 'PRM-ERRO'                            )
008810               FROM      ( IIBKERRO-PRM-ERRO                     )
008820               FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO           )
008830               RESP      ( MSG1-RESP1                            )
008840               RESP2     ( MSG1-RESP2                            )
008850     END-EXEC
008860     .
008870 984000-SAI.
008880     EXIT.
008890

009010
009020*-----------------------------------------------------------------
009030*--> As sections abaixo iniciadas com 999000 representam os
009040*    tratamentos de erro utilizando o Sistema MSG
009050*    (rotina MSGS0001)
009060*-----------------------------------------------------------------
009070 999000-ERRO                     SECTION.
009080*-----------------------------------------------------------------
009090
009100     IF  MSG1-CD-ERRO-TS EQUAL ZEROS
009110         MOVE  8888              TO  MSG1-CD-ERRO-TS
009120     END-IF
009130
009140     IF  MSG1-TX-ERRO-TS EQUAL SPACES
009150         MOVE  'Erro não especificado no módulo.'
009160                                 TO  MSG1-TX-ERRO-TS
009170     END-IF
009180     PERFORM 999000-GRAVA-ERRO-E-ENCERRA
009190     .
009200 999000-ERRO-SAI.
009210     EXIT.
009220
009230*-----------------------------------------------------------------
009240*--> As sections abaixo representam os tratamentos de erro
009250*    utilizando o sistema MSG de mensageria (rotina MSGS0001)
009260*-----------------------------------------------------------------
009270 999000-GRAVA-ERRO-E-ENCERRA     SECTION.
009280*-----------------------------------------------------------------
009290
009300*--> Em caso de erro, desfaz as atualizaç es em banco de dados
009310
009320     EXEC CICS SYNCPOINT
009330               ROLLBACK
009340     END-EXEC
009350
009360*--> Verifica se o tipo de erro gerado possui tratamento interno
009370     EVALUATE  TRUE
009380         WHEN  ERRO-CONTAINER
009390               PERFORM 999004-TRATA-ERRO-CTNR
009400         WHEN  ERRO-TAMANHO-AREA
009410               PERFORM 999005-TRATA-ERRO-AREA-DADOS
009420         WHEN  ERRO-CANAL
009430               PERFORM 999006-TRATA-ERRO-CANAL
009440         WHEN  OTHER
009450               CONTINUE
009460     END-EVALUATE
009470
009480*--> Adiciona as variáveis de entrada para monitoração
009500         MOVE  'SGTS0043-CD-PSS-CTT-EXNO'
009510                                 TO  MSGKW999-NM-VRV-MNT
009520         MOVE  SGTS0043-CD-PSS-CTT-EXNO
009530                                 TO  MSGKW999-VL-VRV-MNT
009540         PERFORM 999003-ADICIONA-VRV-MNT

009590
009600*--> Grava o erro gerado no sistema MSG - Mensageria
009610     PERFORM MSG1-NTF-MSG
009620
009630*--> Movimenta Mensagem e Código de Erro para o Container de Erro
009640     MOVE  MSG1-CD-ERRO-TS       TO  IIBKERRO-CD-ERRO
009650     MOVE  MSG1-TX-ERRO-TS       TO  IIBKERRO-TX-ERRO
009660
009670*    Grava o erro gerado no container de erro
009680     PERFORM 984000-GRAVAR-CONTAINER-ERRO
009690
009700*    Encerra a transacao
009710     PERFORM 999999-ENCERRAR
009720     .
009730 999000-SAI.
009740     EXIT.
009750* ---------------------------------------
009760* 999001-TRATA-ABEND-CICS        SECTION.
009770* 999002-TRATA-CONDITION-CICS    SECTION.
009780* 999003-ADICIONA-VRV-MNT        SECTION.
      * 999004-ALINHA-NUMERO-ESQUERDA  SECTION.
009790* ---------------------------------------
-INC HLPKP999                                                           00009800
009810* ---------------------------------------
009820* 999004-TRATA-ERRO-CTNR         SECTION.
009830* 999005-TRATA-ERRO-AREA-DADOS   SECTION.
009840* 999006-TRATA-ERRO-CANAL        SECTION.
009850*----------------------------------------
-INC SGTKP999                                                           00009860
009870* ---------------------------------------
009880* MSG1-NTF-MSG                   SECTION.
009890* ---------------------------------------
-INC MSGKPMG1

008900*-----------------------------------------------------------------
008910 999999-ENCERRAR                 SECTION.
008920*-----------------------------------------------------------------
008940
008950     EXEC CICS
008960          RETURN
008970     END-EXEC
008980     .
008990 999999-SAI.
009000     EXIT.                                                            0000
009910*---------------------- FIM SGTS0043 -----------------------------