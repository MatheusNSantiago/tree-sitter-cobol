000010 CBL ARITH(EXTEND)
000020**** INSTRUÇãO QUE ALTERA O TAMANHO MáXIMO DE CAMPOS NUMERICOS
000030**** NAO DELETAR
000040*-----------------------------------------------------------------
000050* PROGRAMA..: SGTS0040
000060* ANALISTA..: F2054109 - Cintia Ap. Soares Iglesias
000070* AUTOR.....: F2021264 - Christine Mitsue Akahoshi
000080* DATA......: 26.11.2015
000090* OBJETIVO..: Detalhar Característica de Operação Estruturada
000100* COMPILACAO: COBOL/CICS
000110*-----------------------------------------------------------------
000120* VRS001 26.11.2015 F2021264  DE 2013/120821 - Implantação
000100*        22.01.2019 F0950413 inclusão da sigla 'SGT' na section
000100*                   972000-LER-CONTAINER-DE-ERRO
000130*-----------------------------------------------------------------
000140
000150*-----------------------------------------------------------------
000160 IDENTIFICATION DIVISION.
000170*----------------------------------------------------------------*
000180 PROGRAM-ID. SGTS0040.
000190*
000200*----------------------------------------------------------------*
000210 ENVIRONMENT DIVISION.
000220*----------------------------------------------------------------*
000230*
000240*----------------------------------------------------------------*
000250 CONFIGURATION                   SECTION.
000260*----------------------------------------------------------------*
000270 SPECIAL-NAMES.
000280     DECIMAL-POINT IS COMMA.
000290*
000300*----------------------------------------------------------------*
000310 DATA DIVISION.
000320*----------------------------------------------------------------*
000330*
000340
000350 WORKING-STORAGE                 SECTION.
000360*----------------------------------------------------------------*
000370
000380 77  CTE-INICIO-SS               PIC  X(040)  VALUE
000390              '*** S.S. COMECA AQUI ***'.
000400
000410 77  CTE-PROG                    PIC  X(018)  VALUE
000420              '*** SGTS0040 ***'.
000430 77  CTE-VERS                    PIC  X(006)  VALUE 'VRS001'.
000440
000450* Módulos acionados
000460 77  IIBP2000                    PIC  X(008) VALUE 'IIBP2000'.
000470 77  SGTST039                    PIC  X(008) VALUE 'SGTST039'.
000480 77  SGTST040                    PIC  X(008) VALUE 'SGTST040'.
000490 77  SGTST041                    PIC  X(008) VALUE 'SGTST041'.
000500 77  SGTST055                    PIC  X(008) VALUE 'SGTST055'.
000510 77  ATCS6000                    PIC  X(008) VALUE 'ATCS6000'.
000520*----------------------------------------------------------------*
000530 LOCAL-STORAGE                   SECTION.
000540*----------------------------------------------------------------*
000550* Areas de trabalho
000560
000570 01  OPCAO                       PIC S9(004) COMP-5 VALUE 0.
000580 01  CD-USU-INCL                 PIC  X(008) VALUE ' '.
000590 01  CD-USU-CNFC                 PIC  X(008) VALUE ' '.
000600 01  CD-USU-ECR                  PIC  X(008) VALUE ' '.
000610
000620*----------------------------------------------------------------*
000630* Book dos Containers e das subrotinas chamadas
000640*----------------------------------------------------------------*
000650
000660* Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1                                                           0000067%
000680
000690* Book para captura de abend cics da bloco codigo MSGKP999
-INC HLPKW999                                                           0000070%
000710
000720* Book para captura de erro via MSG1
-INC MSGKWMG1                                                           0000073%
000740
000750* Book do container de requisição e resposta deste modulo
000760 01  SGTS0040-DADOS.
-INC SGTK0040                                                           0000077%
000780
000790* CRUD da tabela  TIP_ATB_VCLD_OPR
000800 01  SGTST039-DADOS.
-INC SGTKT039                                                           0000081%
000820
000830* CRUD da tabela  CRCT_OPR_ETRD
000840 01  SGTST040-DADOS.
-INC SGTKT040                                                           0000085%
000860
000870* CRUD da tabela  EST_CRCT_OPR
000880 01  SGTST041-DADOS.
-INC SGTKT041                                                           0000089%
000900
000910* CRUD da tabela  NOTA_TCN_OPR_ETRD
000920 01  SGTST055-DADOS.
-INC SGTKT055                                                           0000093%
000940
000950* Book do modulo  ATCS6000 - Via IIB
000960 01  ATCS6000-DADOS.
-INC ATCK6000                                                           0000097%
000980
000990* Book do Container de Erro
-INC IIBKERRO                                                           0000100%
001010
001020* Book do container de Serviços (para acionamento de serviços)
-INC IIBKSRVC                                                           0000103%
001040
001050* Book dos dados do usuário
-INC IIBKOPRD                                                           0000106%
001070
001080 77  CTE-FINAL-SS                PIC  X(040)  VALUE
001090              '*** S.S. TERMINA AQUI ***'.
001100
001110*----------------------------------------------------------------*
001120 PROCEDURE DIVISION.
001130*----------------------------------------------------------------*
001140
001150*----------------------------------------------------------------*
001160 000000-ROTINA-PRINCIPAL         SECTION.
001170*----------------------------------------------------------------*
001180
001190     PERFORM 100000-INICIAR
001200
001210     PERFORM 200000-PROCESSAR
001220
001230     PERFORM 990000-ENCERRAR

           GOBACK
001240     .
001250 000000-SAI.
001260     EXIT.
001270
001280*----------------------------------------------------------------*
001290 100000-INICIAR                  SECTION.
001300*----------------------------------------------------------------*
001310     MOVE 0001                   TO MSG1-CD-SEQL-ERRO
001320
001330* Ativa a captura automatica de abend do CICS.
001340* WHENEVER CICS
-INC HLPKE999                                                           0000135%
001360
001370     PERFORM 970000-LER-CANAL
001380
001390* Define o nome do programa p/ fins manipulacao container
001400     MOVE 'SGTS0040'             TO NM-PGM
001410     MOVE 'SGTS0040-RPST'        TO NM-CTNR
001420
001430* Limpa a resposta e erro antes de executar o modulo
001440     PERFORM 983000-EXCLUIR-RESPOSTA-ERRO
001450
001460     MOVE SPACES                 TO SGTS0040-RPST
001470     INITIALIZE  SGTS0040-RPST
001480         REPLACING NUMERIC       BY  ZEROS
001490
001500* Recupera a requisição do modulo
001510     PERFORM 971000-LER-REQUISICAO
001520
001530* Recupera usuário logado
001540     PERFORM 974000-LER-DADOS-OPRD
001550
001560     PERFORM 110000-VALIDAR-REQUISICAO
001570     .
001580 100000-SAI.
001590     EXIT.
001600
001610*----------------------------------------------------------------*
001620 110000-VALIDAR-REQUISICAO       SECTION.
001630*----------------------------------------------------------------*
001640* Valida campo a campo da requisicao
001650*----------------------------------------------------------------*
001660     MOVE 0002                   TO MSG1-CD-SEQL-ERRO
001670
001680     IF  SGTS0040-CD-TIP-CRCT-OPR  <= ZEROS
001690         SET  ERRO-PROGRAMADO    TO TRUE
001700         MOVE 0001               TO MSG1-CD-ERRO-TS
001710         MOVE 'Código tipo atributo da característica inválido.'
001720                                 TO MSG1-TX-ERRO-TS
001730         SET  MNT-VRV            TO TRUE
001740         PERFORM 999000-ERRO
001750     END-IF
001760
001770     IF  SGTS0040-NR-ETRD-CRCT-OPR  <= ZEROS
001780         SET  ERRO-PROGRAMADO    TO TRUE
001790         MOVE 0002               TO MSG1-CD-ERRO-TS
001800         MOVE 'Numero estruturado da caracteristica inválido.'
001810                                 TO MSG1-TX-ERRO-TS
001820         SET  MNT-VRV            TO TRUE
001830         PERFORM 999000-ERRO
001840     END-IF
001850     .
001860 110000-SAI.
001870     EXIT.
001880
001890*----------------------------------------------------------------*
001900 200000-PROCESSAR                SECTION.
001910*----------------------------------------------------------------*
001920     MOVE 0003                   TO MSG1-CD-SEQL-ERRO
001930
001940* Buscar dados do Tipo de atributo da caracteristica informado
001950     PERFORM 210000-BUSCAR-TIP-ATB
001960
001970     MOVE SGTST039-CD-TIP-ATB-CRCT
001980                                 TO SGTS0040-CD-TIP-ATB-CRCT
001990     MOVE SGTST039-TX-TIP-ATB-OPR
002000                                 TO SGTS0040-TX-TIP-ATB-OPR
002010     MOVE SGTST039-TX-RTL-ATB-OPR
002020                                 TO SGTS0040-TX-RTL-ATB-OPR
002030     MOVE SGTST039-TX-CMPT-ATB-OPR
002040                                 TO SGTS0040-TX-CMPT-ATB-OPR
002050
002060* Buscar dados da Caracteristica da operação
002070     PERFORM 220000-BUSCAR-CRCT-OPR
002080
002090     MOVE SGTST040-IN-VCLD-CRCT-OPR
002100                                 TO SGTS0040-IN-VCLD-CRCT-OPR
002110     MOVE SGTST040-IN-CRCT-OPR-ATV
002120                                 TO SGTS0040-IN-CRCT-OPR-ATV
002130     MOVE SGTST040-NR-NOTA-TCN-OPR
002140                                 TO SGTS0040-NR-NOTA-TCN-OPR
002150     EVALUATE  TRUE
002160         WHEN  SGTST040-VL-VCLD-CRCT-OPR NOT = ZEROS
002170             MOVE SGTST040-VL-VCLD-CRCT-OPR
002180                                 TO SGTS0040-VL-VCLD-CRCT-OPR
002190         WHEN  SGTST040-PC-VCLD-CRCT-OPR NOT = ZEROS
002200             MOVE SGTST040-PC-VCLD-CRCT-OPR
002210                                 TO SGTS0040-VL-VCLD-CRCT-OPR
002220         WHEN  SGTST040-QT-VCLD-CRCT-OPR NOT = ZEROS
002230             MOVE SGTST040-QT-VCLD-CRCT-OPR
002240                                 TO SGTS0040-VL-VCLD-CRCT-OPR
002250     END-EVALUATE
002260
002270     IF  SGTST040-NR-NOTA-TCN-OPR > ZEROS
002280* Buscar dados da Nota tecnica da operação
002290         PERFORM 230000-BUSCAR-NOTA-TCN
002300
002310         MOVE SGTST055-CD-REF-NOTA-TCN
002320                                 TO SGTS0040-CD-REF-NOTA-TCN
002330     END-IF
002340
002350* Tratar dados do Estado da caracteristica da operação
002360     PERFORM 240000-TRATAR-EST-CRCT
002370
002380* Tratar nomes de usuários
002390     PERFORM 250000-TRATAR-USUARIO
002400
002410     PERFORM 899000-MOVIMENTAR-SAIDA
002420     .
002430 200000-SAI.
002440     EXIT.
002450
002460*----------------------------------------------------------------*
002470 210000-BUSCAR-TIP-ATB           SECTION.
002480*----------------------------------------------------------------*
002490     MOVE 0004                   TO MSG1-CD-SEQL-ERRO
002500
002510     MOVE SPACES                 TO SGTST039-DADOS
002520     INITIALIZE  SGTST039-DADOS
002530         REPLACING NUMERIC       BY  ZEROS
002540
002550     MOVE KOPRD-CD-OPRD          TO SGTST039-CD-CHV-USU
002560* Opção 4 - Consultar
002570     MOVE 4                      TO SGTST039-ACAO
002580     MOVE SGTS0040-CD-TIP-CRCT-OPR
002590                                 TO SGTST039-CD-TIP-CRCT-OPR
002600
002610* Mover nome do módulo para monitoração de variáveis
002620     MOVE SGTST039               TO NM-PGM
002630
002640     EXEC CICS LINK
002650          PROGRAM  ( SGTST039       )
002660          COMMAREA ( SGTST039-DADOS )
002670     END-EXEC
002680
002690     IF  SGTST039-CD-RTN-PGM     NOT = ZEROS
002700         SET  ERRO-PROGRAMA-ACIONADO TO TRUE
002710         MOVE 0003               TO MSG1-CD-ERRO-TS
002720         MOVE SGTST039-MSG-ERRO-PGM
002730                                 TO MSG1-TX-RTN-PGM-ACND
002740                                    MSG1-TX-ERRO-TS
002750         MOVE NM-PGM             TO MSG1-NM-PGM-ACND
002760         MOVE SGTST039-CD-RTN-PGM
002770                                 TO MSG1-CD-RTN-PGM-ACND
002780         PERFORM 969000-MONITORAR-VARIAVEIS
002790         PERFORM 999000-ERRO
002800     END-IF
002810     .
002820 210000-SAI.
002830     EXIT.
002840
002850*----------------------------------------------------------------*
002860 220000-BUSCAR-CRCT-OPR          SECTION.
002870*----------------------------------------------------------------*
002880     MOVE 0005                   TO MSG1-CD-SEQL-ERRO
002890
002900     MOVE SPACES                 TO SGTST040-DADOS
002910     INITIALIZE  SGTST040-DADOS
002920         REPLACING NUMERIC       BY  ZEROS
002930
002940     MOVE KOPRD-CD-OPRD          TO SGTST040-CD-CHV-USU
002950* Opção 4 - Consultar
002960     MOVE 4                      TO SGTST040-ACAO
002970     MOVE SGTS0040-CD-TIP-CRCT-OPR
002980                                 TO SGTST040-CD-TIP-CRCT-OPR
002990     MOVE SGTS0040-NR-ETRD-CRCT-OPR
003000                                 TO SGTST040-NR-ETRD-CRCT-OPR
003010
003020* Mover nome do módulo para monitoração de variáveis
003030     MOVE SGTST040               TO NM-PGM
003040
003050     EXEC CICS LINK
003060          PROGRAM  ( SGTST040       )
003070          COMMAREA ( SGTST040-DADOS )
003080     END-EXEC
003090
003100     IF  SGTST040-CD-RTN-PGM     NOT = ZEROS
003110         SET  ERRO-PROGRAMA-ACIONADO TO TRUE
003120         MOVE 0004               TO MSG1-CD-ERRO-TS
003130         MOVE SGTST040-MSG-ERRO-PGM
003140                                 TO MSG1-TX-RTN-PGM-ACND
003150                                    MSG1-TX-ERRO-TS
003160         MOVE NM-PGM             TO MSG1-NM-PGM-ACND
003170         MOVE SGTST040-CD-RTN-PGM
003180                                 TO MSG1-CD-RTN-PGM-ACND
003190         PERFORM 969000-MONITORAR-VARIAVEIS
003200         PERFORM 999000-ERRO
003210     END-IF
003220     .
003230 220000-SAI.
003240     EXIT.
003250
003260*----------------------------------------------------------------*
003270 230000-BUSCAR-NOTA-TCN          SECTION.
003280*----------------------------------------------------------------*
003290     MOVE 0006                   TO MSG1-CD-SEQL-ERRO
003300
003310     MOVE SPACES                 TO SGTST055-DADOS
003320     INITIALIZE  SGTST055-DADOS
003330         REPLACING NUMERIC       BY  ZEROS
003340
003350     MOVE KOPRD-CD-OPRD          TO SGTST055-CD-CHV-USU
003360* Opção 4 - Consultar
003370     MOVE 4                      TO SGTST055-ACAO
003380     MOVE SGTST040-NR-NOTA-TCN-OPR
003390                                 TO SGTST055-NR-NOTA-TCN-OPR
003400
003410* Mover nome do módulo para monitoração de variáveis
003420     MOVE SGTST055               TO NM-PGM
003430
003440     EXEC CICS LINK
003450          PROGRAM  ( SGTST055       )
003460          COMMAREA ( SGTST055-DADOS )
003470     END-EXEC
003480
003490     IF  SGTST055-CD-RTN-PGM     NOT = ZEROS  AND 100
003500         SET  ERRO-PROGRAMA-ACIONADO TO TRUE
003510         MOVE 0005               TO MSG1-CD-ERRO-TS
003520         MOVE SGTST055-MSG-ERRO-PGM
003530                                 TO MSG1-TX-RTN-PGM-ACND
003540                                    MSG1-TX-ERRO-TS
003550         MOVE NM-PGM             TO MSG1-NM-PGM-ACND
003560         MOVE SGTST055-CD-RTN-PGM
003570                                 TO MSG1-CD-RTN-PGM-ACND
003580         PERFORM 969000-MONITORAR-VARIAVEIS
003590         PERFORM 999000-ERRO
003600     END-IF
003610     .
003620 230000-SAI.
003630     EXIT.
003640
003650*----------------------------------------------------------------*
003660 240000-TRATAR-EST-CRCT          SECTION.
003670*----------------------------------------------------------------*
003680     MOVE 0007                   TO MSG1-CD-SEQL-ERRO
003690
003700     MOVE SPACES                 TO SGTST041-DADOS
003710     INITIALIZE  SGTST041-DADOS
003720         REPLACING NUMERIC       BY  ZEROS
003730
003740* Opção 5 - Consultar Estado atual
003750     MOVE 5                      TO OPCAO
003760
003770     PERFORM 930000-CHAMAR-SGTST041
003780
003790     MOVE SGTST041-CD-TIP-EST-DCS
003800                                 TO SGTS0040-CD-TIP-EST-DCS
003810
003820* Se estado = 1 (pendente de confirmação), carregar dados da
003830* inclusão da caracteristica
003840* Senão, tratar o estado encontrado:
003850*  Estado = 2 - Aprovado
003860*  Estado = 3 - Rejeitado (carregar a justificativa da rejeição)
003870*  Estado = 99 - Cancelado
003880*  Outros = Erro
003890     EVALUATE  SGTST041-CD-TIP-EST-DCS
003900         WHEN  1
003910             MOVE SGTST041-TS-EST-CRCT-OPR
003920                                 TO SGTS0040-TS-EST-CRCT-INCL
003930             MOVE SGTST041-CD-USU-ATL-EST
003940                                 TO SGTS0040-CD-USU-ATL-INCL
003950                                    CD-USU-INCL
003960         WHEN  2
003970             PERFORM 920000-TRATAR-EST-CNFC
003980         WHEN  3
003990             MOVE SGTST041-TX-OBS-EST-CRCT
004000                                 TO SGTS0040-TX-OBS-EST-CNFC
004010             PERFORM 920000-TRATAR-EST-CNFC
004020         WHEN  99
004030* Salvar usuário e timestamp do cancelamento
004040             MOVE SGTST041-TS-EST-CRCT-OPR
004050                                 TO SGTS0040-TS-EST-CRCT-ECR
004060             MOVE SGTST041-CD-USU-ATL-EST
004070                                 TO SGTS0040-CD-USU-ATL-ECR
004080                                    CD-USU-ECR
004090             PERFORM 921000-BUSCAR-EST-INCL
004100             PERFORM 922000-BUSCAR-EST-APVC
004110         WHEN  OTHER
004120             SET  ERRO-PROGRAMADO TO TRUE
004130             MOVE 0006           TO MSG1-CD-ERRO-TS
004140             MOVE 'Status da característica inesperado.'
004150                                 TO MSG1-TX-ERRO-TS
004160             SET  MNT-VRV        TO TRUE
004170             PERFORM 999000-ERRO
004180     END-EVALUATE
004190     .
004200 240000-SAI.
004210     EXIT.
004220
004230*----------------------------------------------------------------*
004240 250000-TRATAR-USUARIO           SECTION.
004250*----------------------------------------------------------------*
004260     MOVE 0008                   TO MSG1-CD-SEQL-ERRO
004270
004280* Buscar nome do usuario responsavel pela inclusão
004290     MOVE SPACES                 TO ATCS6000-DADOS
004300     INITIALIZE  ATCS6000-DADOS
004310         REPLACING NUMERIC       BY  ZEROS
004320
004330* Movimentar o nome do programa a ser executado
004340     MOVE ATCS6000               TO NM-PGM
004350
004360* Pesquisar informaçoes do usuario da inclusão
004370     SET  K6000-PSQ-INF-USU      TO TRUE
004380     MOVE CD-USU-INCL            TO K6000-CD-USU
004390* Formatar código de erro para cada acesso ao ATCS6000
004400* que sera utilizado caso haja retorno de erro do modulo.
004410     MOVE 0007                   TO MSG1-CD-ERRO-TS
004420
004430     PERFORM 910000-CHAMAR-SERVICO
004440     MOVE K6000-NM-USU           TO SGTS0040-NM-USU-ATL-INCL
004450
004460* Buscar nome do usuario responsavel pela confirmação, se houver
004470     IF  CD-USU-CNFC             NOT = SPACES
004480         MOVE SPACES             TO ATCS6000-DADOS
004490         INITIALIZE  ATCS6000-DADOS
004500             REPLACING NUMERIC   BY  ZEROS
004510
004520         SET  K6000-PSQ-INF-USU  TO TRUE
004530         MOVE CD-USU-CNFC        TO K6000-CD-USU
004540* Formatar código de erro para cada acesso ao ATCS6000
004550* que sera utilizado caso haja retorno de erro do modulo.
004560         MOVE 0008               TO MSG1-CD-ERRO-TS
004570
004580         PERFORM 910000-CHAMAR-SERVICO
004590         MOVE K6000-NM-USU       TO SGTS0040-NM-USU-ATL-CNFC
004600     END-IF
004610
004620* Buscar nome do usuario responsavel pelo cancelamento, se houver
004630     IF  CD-USU-ECR              NOT = SPACES
004640         MOVE SPACES             TO ATCS6000-DADOS
004650         INITIALIZE  ATCS6000-DADOS
004660             REPLACING NUMERIC   BY  ZEROS
004670
004680         SET  K6000-PSQ-INF-USU  TO TRUE
004690         MOVE CD-USU-ECR         TO K6000-CD-USU
004700* Formatar código de erro para cada acesso ao ATCS6000
004710* que sera utilizado caso haja retorno de erro do modulo.
004720         MOVE 0009               TO MSG1-CD-ERRO-TS
004730
004740         PERFORM 910000-CHAMAR-SERVICO
004750         MOVE K6000-NM-USU       TO SGTS0040-NM-USU-ATL-ECR
004760     END-IF
004770     .
004780 240000-SAI.
004790     EXIT.
004800
004810*----------------------------------------------------------------*
004820 899000-MOVIMENTAR-SAIDA         SECTION.
004830*----------------------------------------------------------------*
004840     MOVE 0009                   TO MSG1-CD-SEQL-ERRO
004850
004860* Grava o container de resposta da subrotina
004870     PERFORM 980000-GRAVAR-CONTAINER-RPST
004880     .
004890 899000-SAI.
004900     EXIT.
004910
004920*----------------------------------------------------------------*
004930*                                                                *
004940*  SECTION DA FAIXA DE 900000 A 969900 PARA DEMAIS ROTINAS       *
004950*                                                                *
004960*----------------------------------------------------------------*
004970*---------------------------------------------------------------*
004980 910000-CHAMAR-SERVICO           SECTION.
004990*---------------------------------------------------------------*
005000* Acionar programa IIBP2000 para requisição do serviço.
005010*---------------------------------------------------------------*
005020     MOVE 0010                   TO MSG1-CD-SEQL-ERRO
005030
005040* Grava o container com dados do serviço/operação a chamar
005050     PERFORM 981000-GRAVAR-CONTAINER-SRVC
005060
005070* Grava o container de requisicao do modulo a executar
005080     PERFORM 982000-GRAVAR-CONTAINER-RQSC
005090
005100* Limpa a resposta e erro antes de executar o modulo
005110     PERFORM 983000-EXCLUIR-RESPOSTA-ERRO
005120
005130* Não há captura automática de erro CICS por usar RESP e RESP2
005140     EXEC CICS LINK
005150          PROGRAM ( IIBP2000   )
005160          CHANNEL ( NM-CNL     )
005170          RESP    ( MSG1-RESP1 )
005180          RESP2   ( MSG1-RESP2 )
005190     END-EXEC
005200
005210     EVALUATE  TRUE
005220         WHEN  MSG1-RESP1        EQUAL DFHRESP(NORMAL)
005230             CONTINUE
005240         WHEN  OTHER
005250             STRING 'Erro ao acionar serviço: '
005260                 SRVC-ID-SRVC '.' SRVC-VRS-SRVC '.'
005270                 SRVC-ID-OPR  '.' SRVC-VRS-OPR
005280                 'via IIBP2000, '
005290                 'RESP=' MSG1-RESP1 ', RESP-2=' MSG1-RESP2
005300                 '.'   DELIMITED BY  SIZE
005310                               INTO MSG1-TX-ERRO-TS
005320             SET  ERRO-CICS      TO TRUE
005330             PERFORM 999000-ERRO
005340     END-EVALUATE
005350
005360* Efetua a leitura do Container de Erro
005370     PERFORM 972000-LER-CONTAINER-DE-ERRO
005380
005390* Efetua a leitura do Container de Resposta
005400     PERFORM 973000-LER-RESPOSTA
005410     .
005420 910000-SAI.
005430     EXIT.
005440*----------------------------------------------------------------*
005450 920000-TRATAR-EST-CNFC          SECTION.
005460*----------------------------------------------------------------*
005470     MOVE 0011                   TO MSG1-CD-SEQL-ERRO
005480
005490* Salvar usuário e timestamp da confirmação
005500     MOVE SGTST041-TS-EST-CRCT-OPR
005510                                 TO SGTS0040-TS-EST-CRCT-CNFC
005520     MOVE SGTST041-CD-USU-ATL-EST
005530                                 TO SGTS0040-CD-USU-ATL-CNFC
005540                                    CD-USU-CNFC
005550
005560* Buscar Estado de inclusão
005570     PERFORM 921000-BUSCAR-EST-INCL
005580     .
005590 920000-SAI.
005600     EXIT.
005610
005620*----------------------------------------------------------------*
005630 921000-BUSCAR-EST-INCL          SECTION.
005640*----------------------------------------------------------------*
005650     MOVE 0012                   TO MSG1-CD-SEQL-ERRO
005660
005670     MOVE SPACES                 TO SGTST041-DADOS
005680     INITIALIZE  SGTST041-DADOS
005690         REPLACING NUMERIC       BY  ZEROS
005700
005710* Opção 6 - Consultar Estado (Estado = 1 - inclusão)
005720     MOVE 6                      TO OPCAO
005730     MOVE 1                      TO SGTST041-CD-TIP-EST-DCS
005740
005750     PERFORM 930000-CHAMAR-SGTST041
005760
005770     MOVE SGTST041-TS-EST-CRCT-OPR
005780                                 TO SGTS0040-TS-EST-CRCT-INCL
005790     MOVE SGTST041-CD-USU-ATL-EST
005800                                 TO SGTS0040-CD-USU-ATL-INCL
005810                                    CD-USU-INCL
005820     .
005830 921000-SAI.
005840     EXIT.
005850
005860*----------------------------------------------------------------*
005870 922000-BUSCAR-EST-APVC          SECTION.
005880*----------------------------------------------------------------*
005890     MOVE 0013                   TO MSG1-CD-SEQL-ERRO
005900
005910     MOVE SPACES                 TO SGTST041-DADOS
005920     INITIALIZE  SGTST041-DADOS
005930         REPLACING NUMERIC       BY  ZEROS
005940
005950* Opção 6 - Consultar Estado (Estado = 2 - aprovado)
005960     MOVE 6                      TO OPCAO
005970     MOVE 2                      TO SGTST041-CD-TIP-EST-DCS
005980
005990     PERFORM 930000-CHAMAR-SGTST041
006000
006010     MOVE SGTST041-TS-EST-CRCT-OPR
006020                                 TO SGTS0040-TS-EST-CRCT-CNFC
006030     MOVE SGTST041-CD-USU-ATL-EST
006040                                 TO SGTS0040-CD-USU-ATL-CNFC
006050                                    CD-USU-CNFC
006060     .
006070 922000-SAI.
006080     EXIT.
006090
006100*----------------------------------------------------------------*
006110 930000-CHAMAR-SGTST041          SECTION.
006120*----------------------------------------------------------------*
006130     MOVE 0014                   TO MSG1-CD-SEQL-ERRO
006140
006150     MOVE KOPRD-CD-OPRD          TO SGTST041-CD-CHV-USU
006150                                    SGTST041-CD-USU-ATL-EST
006160     MOVE OPCAO                  TO SGTST041-ACAO
006170     MOVE SGTS0040-CD-TIP-CRCT-OPR
006180                                 TO SGTST041-CD-TIP-CRCT-OPR
006190     MOVE SGTS0040-NR-ETRD-CRCT-OPR
006200                                 TO SGTST041-NR-ETRD-CRCT-OPR
006210
006220* Mover nome do módulo para monitoração de variáveis
006230     MOVE SGTST041               TO NM-PGM
006240
006250     EXEC CICS LINK
006260          PROGRAM  ( SGTST041       )
006270          COMMAREA ( SGTST041-DADOS )
006280     END-EXEC
006290
006300     IF  SGTST041-CD-RTN-PGM     NOT = ZEROS
006310         SET  ERRO-PROGRAMA-ACIONADO TO TRUE
006320         MOVE 0010               TO MSG1-CD-ERRO-TS
006330         MOVE SGTST041-MSG-ERRO-PGM
006340                                 TO MSG1-TX-RTN-PGM-ACND
006350                                    MSG1-TX-ERRO-TS
006360         MOVE NM-PGM             TO MSG1-NM-PGM-ACND
006370         MOVE SGTST041-CD-RTN-PGM
006380                                 TO MSG1-CD-RTN-PGM-ACND
006390         PERFORM 969000-MONITORAR-VARIAVEIS
006400         PERFORM 999000-ERRO
006410     END-IF
006420     .
006430 930000-SAI.
006440     EXIT.
006450
006460*----------------------------------------------------------------*
006470 969000-MONITORAR-VARIAVEIS      SECTION.
006480*----------------------------------------------------------------*
006490* Monitora as variáveis dos módulos chamados(Mensageria IIB)
006500*----------------------------------------------------------------*
006510     MOVE 0015                   TO MSG1-CD-SEQL-ERRO
006520
006530     EVALUATE  NM-PGM
006540         WHEN  SGTST039
006550             MOVE 'SGTST039-ACAO '
006560                                 TO MSGKW999-NM-VRV-MNT
006570             MOVE SGTST039-ACAO
006580                                 TO MSGKW999-VL-VRV-MNT
006590             PERFORM 999003-ADICIONA-VRV-MNT
006600
006610             MOVE 'SGTST039-CD-TIP-CRCT-OPR'
006620                                 TO MSGKW999-NM-VRV-MNT
006630             MOVE SGTST039-CD-TIP-CRCT-OPR
006640                                 TO MSGKW999-VL-VRV-MNT
006650             PERFORM 999003-ADICIONA-VRV-MNT
006660
006670         WHEN  SGTST040
006680             MOVE 'SGTST040-ACAO '
006690                                 TO MSGKW999-NM-VRV-MNT
006700             MOVE SGTST040-ACAO
006710                                 TO MSGKW999-VL-VRV-MNT
006720             PERFORM 999003-ADICIONA-VRV-MNT
006730
006740             MOVE 'SGTST040-CD-TIP-CRCT-OPR'
006750                                 TO MSGKW999-NM-VRV-MNT
006760             MOVE SGTST040-CD-TIP-CRCT-OPR
006770                                 TO MSGKW999-VL-VRV-MNT
006780             PERFORM 999003-ADICIONA-VRV-MNT
006790
006800             MOVE 'SGTST040-NR-ETRD-CRCT-OPR'
006810                                 TO MSGKW999-NM-VRV-MNT
006820             MOVE SGTST040-NR-ETRD-CRCT-OPR
006830                                 TO MSGKW999-VL-VRV-MNT
006840             PERFORM 999003-ADICIONA-VRV-MNT
006850
006860         WHEN  SGTST055
006870             MOVE 'SGTST055-ACAO '
006880                                 TO MSGKW999-NM-VRV-MNT
006890             MOVE SGTST055-ACAO
006900                                 TO MSGKW999-VL-VRV-MNT
006910             PERFORM 999003-ADICIONA-VRV-MNT
006920
006930             MOVE 'SGTST055-NR-NOTA-TCN-OPR'
006940                                 TO MSGKW999-NM-VRV-MNT
006950             MOVE SGTST055-NR-NOTA-TCN-OPR
006960                                 TO MSGKW999-VL-VRV-MNT
006970             PERFORM 999003-ADICIONA-VRV-MNT
006980
006990         WHEN  SGTST041
007000             MOVE 'SGTST041-ACAO '
007010                                 TO MSGKW999-NM-VRV-MNT
007020             MOVE SGTST041-ACAO
007030                                 TO MSGKW999-VL-VRV-MNT
007040             PERFORM 999003-ADICIONA-VRV-MNT
007050
007060             MOVE 'SGTST041-CD-TIP-CRCT-OPR'
007070                                 TO MSGKW999-NM-VRV-MNT
007080             MOVE SGTST041-CD-TIP-CRCT-OPR
007090                                 TO MSGKW999-VL-VRV-MNT
007100             PERFORM 999003-ADICIONA-VRV-MNT
007110
007120             MOVE 'SGTST041-NR-ETRD-CRCT-OPR'
007130                                 TO MSGKW999-NM-VRV-MNT
007140             MOVE SGTST041-NR-ETRD-CRCT-OPR
007150                                 TO MSGKW999-VL-VRV-MNT
007160             PERFORM 999003-ADICIONA-VRV-MNT
007170
007180             MOVE 'SGTST041-CD-TIP-EST-DCS'
007190                                 TO MSGKW999-NM-VRV-MNT
007200             MOVE SGTST041-CD-TIP-EST-DCS
007210                                 TO MSGKW999-VL-VRV-MNT
007220             PERFORM 999003-ADICIONA-VRV-MNT
007230
007240         WHEN  ATCS6000
007250             MOVE 'K6000-CD-FUC '
007260                                 TO MSGKW999-NM-VRV-MNT
007270             MOVE K6000-CD-FUC
007280                                 TO MSGKW999-VL-VRV-MNT
007290             PERFORM 999003-ADICIONA-VRV-MNT
007300
007310             MOVE 'K6000-CD-USU'
007320                                 TO MSGKW999-NM-VRV-MNT
007330             MOVE K6000-CD-USU
007340                                 TO MSGKW999-VL-VRV-MNT
007350             PERFORM 999003-ADICIONA-VRV-MNT
007360
007370         WHEN  OTHER
007380             CONTINUE
007390     END-EVALUATE
007400     .
007410 969000-SAI.
007420     EXIT.
007430
007440*----------------------------------------------------------------*
007450*                                                                *
007460* SECTION DA FAIXA DE 970000 A 979900 PARA LEITURA DE DADOS      *
007470*                                                                *
007480*----------------------------------------------------------------*
007490 970000-LER-CANAL                SECTION.
007500*----------------------------------------------------------------*
007510* Obtem o canal corrente
007520*----------------------------------------------------------------*
007530     MOVE 0016                   TO MSG1-CD-SEQL-ERRO
007540
007550* Por causa da captura automática de erro CICS, deve ser previsto
007560* o erro de container
007570     SET  ERRO-CANAL             TO TRUE
007580
007590     EXEC CICS ASSIGN
007600          CHANNEL ( NM-CNL )
007610     END-EXEC
007620
007630* Ao chegar aqui, há certeza de que não houve erro algum
007640     SET  SEM-ERRO               TO TRUE
007650     .
007660 970000-SAI.
007670     EXIT.
007680
007690*----------------------------------------------------------------*
007700 971000-LER-REQUISICAO           SECTION.
007710*----------------------------------------------------------------*
007720     MOVE 0017                   TO MSG1-CD-SEQL-ERRO
007730
007740* Formata o nome do container de requisicao
007750     MOVE 'SGTS0040'             TO NM-PGM
007760     MOVE 'SGTS0040-RQSC'        TO NM-CTNR
007770
007780* Recupera o tamanho do container de requisição
007790     PERFORM 975000-LER-CONTAINER-NODATA
007800
007810* Obtem o tamanho esperado da requisicao
007820     MOVE LENGTH OF SGTS0040-RQSC TO QT-CCT-EPRO
007830
007840* Valida tamanho esperado da requisicao X tamanho recebido
007850     IF  QT-CCT-LIDO             NOT = QT-CCT-EPRO
007860         SET  ERRO-TAMANHO-AREA  TO TRUE
007870         PERFORM 999000-GRAVA-ERRO-E-ENCERRA
007880     END-IF
007890
007900     MOVE 0018                   TO MSG1-CD-SEQL-ERRO
007910
007920* Configura dados para leitura do container de requisição
007930     SET  GET-CONTAINER          TO TRUE
007940
007950* Por causa da captura automática de erro CICS, deve ser previsto
007960* o erro de container
007970     SET  ERRO-CONTAINER         TO TRUE
007980
007990* Efetua a leitura do container
008000     EXEC CICS GET
008010          CONTAINER ( NM-CTNR       )
008020          FLENGTH   ( QT-CCT-LIDO   )
008030          INTO      ( SGTS0040-RQSC )
008040     END-EXEC
008050
008060* Ao chegar aqui, há certeza de que não houve erro algum
008070     SET  SEM-ERRO               TO TRUE
008080     .
008090 971000-SAI.
008100     EXIT.
008110
008120*----------------------------------------------------------------*
008130 972000-LER-CONTAINER-DE-ERRO    SECTION.
008140*----------------------------------------------------------------*
008150* A presença do container PRM-ERRO indica que houve erro de
008160* integração, a ausência do container indica sucesso.
008170*----------------------------------------------------------------*
008180     MOVE 0019                   TO MSG1-CD-SEQL-ERRO
008190
008200* Le o container de erro dos modulos chamados
008210     SET  GET-CONTAINER          TO TRUE
008220     MOVE 'PRM-ERRO'             TO NM-CTNR
008230     MOVE SPACES                 TO IIBKERRO-PRM-ERRO
008240     INITIALIZE IIBKERRO-PRM-ERRO
008250         REPLACING NUMERIC       BY  ZEROS
008260
008270* Não há captura automática de erro CICS por usar RESP e RESP2
008280     EXEC CICS GET
008290          CONTAINER ( NM-CTNR                     )
008300          INTO      ( IIBKERRO-PRM-ERRO           )
008310          FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
008320          RESP      ( MSG1-RESP1                  )
008330          RESP2     ( MSG1-RESP2                  )
008340     END-EXEC
008350
008360     EVALUATE  TRUE
008370* Caso deseje personalizar ou não interromper o
008380* processamento, alterar para não dar erro aqui SOMENTE
008390* PARA O MODULO CHAMADO e tratar na section chamadora
008400         WHEN  MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
008410             CONTINUE
008420         WHEN  MSG1-RESP1 EQUAL DFHRESP(NORMAL)


               IF  NM-PGM          EQUAL 'ATCS6000'
                   CONTINUE
               ELSE
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
009940*             Monitorar variáveis do módulo chamado
009950              PERFORM 969000-MONITORAR-VARIAVEIS
009960              PERFORM 999000-ERRO
               END-IF

008570         WHEN  OTHER
008580             SET  ERRO-CONTAINER TO TRUE
008590             PERFORM 999002-TRATA-CONDITION-CICS
008600     END-EVALUATE
008610     .
008620 972000-SAI.
008630     EXIT.
008640
008650*----------------------------------------------------------------*
008660 973000-LER-RESPOSTA             SECTION.
008670*----------------------------------------------------------------*
008680* Recupera o container com os dados de resposta (saída).
008690*----------------------------------------------------------------*
008700     MOVE 0020                   TO MSG1-CD-SEQL-ERRO
008710
008720* Monta o nome dos container de acordo com modulo a executar
008730     MOVE SPACES                 TO NM-CTNR
008740
008750* Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
008760* nome_modulo -RPST
008770     EVALUATE  NM-PGM
008780         WHEN  'ATCS6000'
008790             MOVE 'PRM-DADOS-RPST'
008800                                 TO NM-CTNR
008810         WHEN  OTHER
008820             MOVE NM-PGM         TO NM-CTNR(1:8)
008830             MOVE '-RPST'        TO NM-CTNR(9:5)
008840     END-EVALUATE
008850
008860* Recupera o tamanho do container de resposta
008870     PERFORM 975000-LER-CONTAINER-NODATA
008880
008890* Configura dados para leitura do container de resposta
008900     SET  GET-CONTAINER          TO TRUE
008910
008920* Por causa da captura automática de erro CICS, deve ser previsto
008930* o erro de container
008940     SET  ERRO-CONTAINER         TO TRUE
008950
008960* Le a RESPOSTA de acordo com o programa que a gravou
008970
008980     MOVE 0021                   TO MSG1-CD-SEQL-ERRO
008990
009000* Calcular a quantidade de caracteres esperado e limpar a area de
009010* dados antes de executar o GET
009020* Para matrizes com dados variaveis (clausula depending on) não
009030* ha como utilizar o comando INITIALIZE
009040
009050     EVALUATE  NM-PGM
009060         WHEN  ATCS6000
009070             MOVE SPACES         TO ATCS6000-DADOS
009080             INITIALIZE ATCS6000-DADOS
009090                 REPLACING NUMERIC   BY ZEROS
009100             MOVE LENGTH OF ATCS6000-DADOS   TO QT-CCT-EPRO
009110             EXEC CICS GET
009120                  CONTAINER ( NM-CTNR        )
009130                  INTO      ( ATCS6000-DADOS )
009140                  FLENGTH   ( QT-CCT-LIDO    )
009150             END-EXEC
009160             PERFORM 973100-TRATAR-RPST-ATC
009170         WHEN  OTHER
009180             STRING
009190                 'Get nao definido para resposta do modulo '
009200                 NM-PGM '.'
009210                 DELIMITED BY  SIZE
009220                               INTO MSG1-TX-ERRO-TS
009230             PERFORM 999000-ERRO
009240     END-EVALUATE
009250
009260* Ao chegar aqui, há certeza de que não houve erro algum
009270     SET  SEM-ERRO               TO TRUE
009280     .
009290 973000-SAI.
009300     EXIT.
009310
009320*----------------------------------------------------------------*
009330 973100-TRATAR-RPST-ATC          SECTION.
009340*----------------------------------------------------------------*
009350* Em caso de erro do módulo não interromper o processamento.
009360*----------------------------------------------------------------*
009370
009380     EVALUATE  K6000-CD-RTN
009390         WHEN  0
009400             CONTINUE
009410         WHEN  OTHER
009420             MOVE SPACES         TO K6000-NM-USU
009430     END-EVALUATE
009440     .
009450 973100-SAI.
009460     EXIT.
009470
009480*----------------------------------------------------------------*
009490 974000-LER-DADOS-OPRD           SECTION.
009500*----------------------------------------------------------------*
009510     MOVE 0022                   TO MSG1-CD-SEQL-ERRO
009520
009530* Configura dados para leitura do container de requisição
009540     SET  GET-CONTAINER          TO TRUE
009550     MOVE 'PRM-INF-OPRD'         TO NM-CTNR
009560
009570* Por causa da captura automática de erro CICS, deve ser previsto
009580* o erro de container
009590     SET  ERRO-CONTAINER         TO TRUE
009600
009610     MOVE SPACES                 TO PRM-INF-OPRD
009620     EXEC CICS GET
009630          CONTAINER ( NM-CTNR                )
009640          INTO      ( PRM-INF-OPRD           )
009650          FLENGTH   ( LENGTH OF PRM-INF-OPRD )
009660     END-EXEC.
009670
009680* Ao chegar aqui, há certeza de que não houve erro algum
009690     SET  SEM-ERRO               TO TRUE
009700     .
009710 974000-SAI.
009720     EXIT.
009730
009740*----------------------------------------------------------------*
009750 975000-LER-CONTAINER-NODATA     SECTION.
009760*----------------------------------------------------------------*
009770     MOVE 0023                   TO MSG1-CD-SEQL-ERRO
009780
009790* Configura dados para leitura do container de requisição
009800     SET  GET-CONTAINER          TO TRUE
009810
009820* Por causa da captura automática de erro CICS, deve ser previsto
009830* o erro de container
009840     SET  ERRO-CONTAINER         TO TRUE
009850
009860* Obtem a quantidade de caracteres no container
009870     MOVE ZEROS                  TO QT-CCT-LIDO
009880     EXEC CICS GET
009890          CONTAINER ( NM-CTNR     )
009900          NODATA
009910          FLENGTH   ( QT-CCT-LIDO )
009920     END-EXEC
009930
009940* Ao chegar aqui, há certeza de que não houve erro algum
009950     SET  SEM-ERRO               TO TRUE
009960     .
009970 975000-SAI.
009980     EXIT.
009990
010000*----------------------------------------------------------------*
010010*                                                                *
010020* SECTION DA FAIXA DE 980000 A 989900 PARA GRAVACAO DE DADOS     *
010030*                                                                *
010040*----------------------------------------------------------------*
010050 980000-GRAVAR-CONTAINER-RPST    SECTION.
010060*----------------------------------------------------------------*
010070
010080* Exclui eventual container de erro de outros programas
010090     PERFORM 983100-EXCLUIR-CONTAINER-ERRO
010100
010110     MOVE 0024                   TO MSG1-CD-SEQL-ERRO
010120
010130* Monta o nome do container de resposta
010140     MOVE 'SGTS0040-RPST'        TO NM-CTNR
010150     SET  PUT-CONTAINER          TO TRUE
010160
010170* Por causa da captura automática de erro CICS, deve ser previsto
010180* o erro de container
010190     SET  ERRO-CONTAINER         TO TRUE
010200
010210     EXEC CICS PUT
010220          CONTAINER ( NM-CTNR                 )
010230          FROM      ( SGTS0040-RPST           )
010240          FLENGTH   ( LENGTH OF SGTS0040-RPST )
010250     END-EXEC
010260
010270* Ao chegar aqui, há certeza de que não houve erro algum
010280     SET  SEM-ERRO               TO TRUE
010290     .
010300 980000-SAI.
010310     EXIT.
010320
010330*---------------------------------------------------------------*
010340 981000-GRAVAR-CONTAINER-SRVC    SECTION.
010350*---------------------------------------------------------------*
010360* Postar container de requisição de serviço.
010370*---------------------------------------------------------------*
010380     MOVE 0025                   TO MSG1-CD-SEQL-ERRO
010390
010400* Monta o nome do container de requisicao do servico
010410     MOVE 'PRM-RQSC-SRVC'        TO NM-CTNR
010420     SET  PUT-CONTAINER          TO TRUE
010430
010440     EVALUATE  NM-PGM
010450         WHEN  'ATCS6000'
010460             MOVE 71739          TO SRVC-ID-SRVC
010470             MOVE 1              TO SRVC-VRS-SRVC
010480             MOVE 292018         TO SRVC-ID-OPR
010490             MOVE 1              TO SRVC-VRS-OPR
010500         WHEN  OTHER
010510             STRING
010520                 'Serviço não definido para requisição do módulo '
010530                 NM-PGM  '.'
010540                 DELIMITED BY  SIZE
010550                               INTO MSG1-TX-ERRO-TS
010560             PERFORM 999000-ERRO
010570     END-EVALUATE
010580
010590* Por causa da captura automática de erro CICS, deve ser previsto
010600* o erro de container
010610     SET  ERRO-CONTAINER         TO TRUE
010620
010630     EXEC CICS PUT
010640          CONTAINER ( NM-CTNR                      )
010650          FROM      ( SRVC-PRM-RQSC-SRVC           )
010660          FLENGTH   ( LENGTH OF SRVC-PRM-RQSC-SRVC )
010670     END-EXEC
010680
010690* Ao chegar aqui, há certeza de que não houve erro algum
010700     SET  SEM-ERRO               TO TRUE
010710     .
010720 981000-SAI.
010730     EXIT.
010740
010750*----------------------------------------------------------------*
010760 982000-GRAVAR-CONTAINER-RQSC    SECTION.
010770*----------------------------------------------------------------*
010780* Postar container com dados de requisição (entrada)
010790*----------------------------------------------------------------*
010800     MOVE 0026                   TO MSG1-CD-SEQL-ERRO
010810
010820* Monta o nome dos container de acordo com modulo a executar
010830     MOVE SPACES                 TO NM-CTNR
010840     SET  PUT-CONTAINER          TO TRUE
010850
010860* Quando módulos via IIB, fixo nome PRM-DADOS-RQSC, senão
010870* nome_modulo-RQSC
010880     EVALUATE  NM-PGM
010890* ATCS6000 - acesso via IIB
010900         WHEN  'ATCS6000'
010910             MOVE 'PRM-DADOS-RQSC'
010920                                 TO NM-CTNR
010930         WHEN  OTHER
010940             MOVE NM-PGM        TO NM-CTNR(1:8)
010950             MOVE '-RQSC'       TO NM-CTNR(9:5)
010960     END-EVALUATE
010970
010980* Por causa da captura automática de erro CICS, deve ser previsto
010990* o erro de container
011000     SET  ERRO-CONTAINER         TO TRUE
011010
011020* Faz put da requisicao do programa a executar
011030     EVALUATE  NM-PGM
011040         WHEN  ATCS6000
011050             EXEC CICS PUT
011060                  CONTAINER ( NM-CTNR                  )
011070                  FROM      ( ATCS6000-DADOS           )
011080                  FLENGTH   ( LENGTH OF ATCS6000-DADOS )
011090             END-EXEC
011100         WHEN  OTHER
011110             STRING
011120                 'Put não definido para requisição do módulo '
011130                 NM-PGM  '.'
011140                 DELIMITED BY  SIZE
011150                               INTO MSG1-TX-ERRO-TS
011160             PERFORM 999000-ERRO
011170     END-EVALUATE
011180
011190* Ao chegar aqui, há certeza de que não houve erro algum
011200     SET  SEM-ERRO               TO TRUE
011210     .
011220 982000-SAI.
011230     EXIT.
011240
011250*---------------------------------------------------------------*
011260 983000-EXCLUIR-RESPOSTA-ERRO    SECTION.
011270*----------------------------------------------------------------*
011280* Exclui o container de resposta e de erro para evitar problemas
011290* em modulos executados mais de uma vez dentro da mesma sessao do
011300* CICS. Equivale ao initialize na area de resposta dos programas
011310* que utilizam COMMAREA.
011320*----------------------------------------------------------------*
011330     MOVE 0027                   TO MSG1-CD-SEQL-ERRO
011340
011350* Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
011360* (nome_modulo)-RPST
011370     EVALUATE  NM-PGM
011380         WHEN  'SGTS0040'
011390             MOVE NM-PGM         TO NM-CTNR(1:8)
011400             MOVE '-RPST'        TO NM-CTNR(9:5)
011410         WHEN  OTHER
011420             MOVE 'PRM-DADOS-RPST'
011430                                 TO NM-CTNR
011440     END-EVALUATE
011450
011460     SET  DEL-CONTAINER          TO TRUE
011470
011480* Exclui o container de resposta
011490* Não há captura automática de erro CICS por usar RESP e RESP2
011500     EXEC CICS DELETE
011510          CONTAINER ( NM-CTNR    )
011520          RESP      ( MSG1-RESP1 )
011530          RESP2     ( MSG1-RESP2 )
011540     END-EXEC
011550
011560     EVALUATE  TRUE
011570         WHEN  MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
011580         WHEN  MSG1-RESP1 EQUAL DFHRESP(NORMAL)
011590             CONTINUE
011600         WHEN  OTHER
011610             SET  ERRO-CONTAINER TO TRUE
011620             PERFORM 999002-TRATA-CONDITION-CICS
011630     END-EVALUATE
011640
011650* Exclui o container de erro
011660     PERFORM 983100-EXCLUIR-CONTAINER-ERRO
011670     .
011680 983000-SAI.
011690     EXIT.
011700*
011710*----------------------------------------------------------------*
011720 983100-EXCLUIR-CONTAINER-ERRO   SECTION.
011730*----------------------------------------------------------------*
011740
011750* Limpar variaveis container de erro
011760     MOVE SPACES                 TO IIBKERRO-PRM-ERRO
011770     INITIALIZE IIBKERRO-PRM-ERRO
011780         REPLACING NUMERIC       BY  ZEROS
011790
011800* Exclui o container de erro
011810     MOVE 0028                   TO MSG1-CD-SEQL-ERRO
011820     MOVE 'PRM-ERRO'             TO NM-CTNR
011830     SET  DEL-CONTAINER          TO TRUE
011840
011850* Não há captura automática de erro CICS por usar RESP e RESP2
011860     EXEC CICS DELETE
011870          CONTAINER ( NM-CTNR    )
011880          RESP      ( MSG1-RESP1 )
011890          RESP2     ( MSG1-RESP2 )
011900     END-EXEC
011910
011920     EVALUATE  TRUE
011930         WHEN  MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
011940         WHEN  MSG1-RESP1 EQUAL DFHRESP(NORMAL)
011950             CONTINUE
011960         WHEN  OTHER
011970             SET  ERRO-CONTAINER TO TRUE
011980             PERFORM 999002-TRATA-CONDITION-CICS
011990     END-EVALUATE
012000     .
012010 983100-SAI.
012020     EXIT.
012030
012040*----------------------------------------------------------------*
012050 984000-GRAVAR-CONTAINER-ERRO    SECTION.
012060*----------------------------------------------------------------*
012070     MOVE 0029                   TO MSG1-CD-SEQL-ERRO
012080
012090* Grava o erro gerado no container de erro
012100* Não há captura automática de erro CICS por usar RESP e RESP2
012110     EXEC CICS PUT
012120          CONTAINER ( 'PRM-ERRO'                  )
012130          FROM      ( IIBKERRO-PRM-ERRO           )
012140          FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
012150          RESP      ( MSG1-RESP1                  )
012160          RESP2     ( MSG1-RESP2                  )
012170     END-EXEC
012180     .
012190 984000-SAI.
012200     EXIT.
012210
012220*----------------------------------------------------------------*
012230 990000-ENCERRAR                 SECTION.
012240*----------------------------------------------------------------*
012250
012260     EXEC CICS
012270          RETURN
012280     END-EXEC
012290     .
012300 990000-ENCERRAR-SAI.
012310     EXIT.
012320*
012330*----------------------------------------------------------------*
012340*                                                                *
012350* As sections abaixo iniciadas com 999000 representam os tratamen*
012360* tos de erro utilizando o Sistema MSG (rotina MSGS0001)         *
012370*                                                                *
012380*----------------------------------------------------------------*
012390 999000-ERRO                     SECTION.
012400*----------------------------------------------------------------*
012410
012420     IF  MSG1-CD-ERRO-TS         EQUAL ZEROS
012430         MOVE 8888                TO MSG1-CD-ERRO-TS
012440     END-IF
012450     IF  MSG1-TX-ERRO-TS         EQUAL SPACES
012460         MOVE 'Erro não especificado no módulo,'
012470                                 TO MSG1-TX-ERRO-TS
012480     END-IF
012490     PERFORM 999000-GRAVA-ERRO-E-ENCERRA
012500     .
012510 999000-ERRO-SAI.
012520     EXIT.
012530
012540*----------------------------------------------------------------*
012550*    As sections abaixo representam os tratamentos de erro
012560*    utilizando o sistema MSG de mensageria (rotina MSGS0001)
012570*-----------------------------------------------------------------
012580 999000-GRAVA-ERRO-E-ENCERRA     SECTION.
012590*-----------------------------------------------------------------
012600
012610* Em caso de erro, desfaz as atualizaçoes em banco de dados
012620     EXEC CICS SYNCPOINT
012630          ROLLBACK
012640     END-EXEC
012650
012660* Verifica se o tipo de erro gerado possui tratamento interno
012670     EVALUATE  TRUE
012680         WHEN  ERRO-CONTAINER
012690             PERFORM 999004-TRATA-ERRO-CTNR
012700         WHEN  ERRO-TAMANHO-AREA
012710             PERFORM 999005-TRATA-ERRO-AREA-DADOS
012720         WHEN  ERRO-CANAL
012730             PERFORM 999006-TRATA-ERRO-CANAL
012740         WHEN  OTHER
012750             CONTINUE
012760     END-EVALUATE
012770
012780* Adiciona as variáveis de entrada para monitoração
012790     IF  MNT-VRV
012800         MOVE 'SGTS0040-CD-TIP-CRCT-OPR'
012810                                 TO MSGKW999-NM-VRV-MNT
012820         MOVE SGTS0040-CD-TIP-CRCT-OPR
012830                                 TO MSGKW999-VL-VRV-MNT
012840         PERFORM 999003-ADICIONA-VRV-MNT
012850
012860         MOVE 'SGTS0040-NR-ETRD-CRCT-OPR'
012870                                 TO MSGKW999-NM-VRV-MNT
012880         MOVE SGTS0040-NR-ETRD-CRCT-OPR
012890                                 TO MSGKW999-VL-VRV-MNT
012900         PERFORM 999003-ADICIONA-VRV-MNT
012910
012920     END-IF
012930
012940* Grava o erro gerado no sistema MSG - Mensageria
012950     PERFORM MSG1-NTF-MSG
012960
012970* Movimenta Mensagem e Código de Erro para o Container de Erro
012980     MOVE MSG1-CD-ERRO-TS        TO IIBKERRO-CD-ERRO
012990     MOVE MSG1-TX-ERRO-TS        TO IIBKERRO-TX-ERRO
013000
013010* Grava o erro gerado no container de erro
013020     PERFORM 984000-GRAVAR-CONTAINER-ERRO
013030
013040* Encerra a transacao
013050     PERFORM 990000-ENCERRAR
013060     .
013070 999000-SAI.
013080     EXIT.
013090* --------------------------------------*
013100* 999001-TRATA-ABEND-CICS        SECTION.
013110* 999002-TRATA-CONDITION-CICS    SECTION.
013120* 999003-ADICIONA-VRV-MNT        SECTION.
013130* --------------------------------------*
-INC HLPKP999                                                           0001314%
013150* --------------------------------------*
013160* 999004-TRATA-ERRO-CTNR         SECTION.
013170* 999005-TRATA-ERRO-AREA-DADOS   SECTION.
013180* 999006-TRATA-ERRO-CANAL        SECTION.
013190*---------------------------------------*
-INC SGTKP999                                                           0001320%
013210* --------------------------------------*
013220* MSG1-NTF-MSG                   SECTION.
013230* --------------------------------------*
-INC MSGKPMG1                                                           0001324%
013250*---------------------- FIM SGTS0040 -----------------------------