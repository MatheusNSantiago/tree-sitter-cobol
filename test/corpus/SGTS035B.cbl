000010*-----------------------------------------------------------------
000020* PROGRAMA..: SGTS035B
000030* ANALISTA..: F8608799 - Ricardo Ramos
000040* AUTOR.....: F8608799 - Ricardo Ramos
000050* DATA......: 11/01/2016
000060* OBJETIVO..: Consultar se UOR informada é uma Divisão da VP da
000070*             Carteira TVM
000080* COMPILACAO: COBOL/CICS
000090*-----------------------------------------------------------------
000100* VRS001 11/01/2016 F8608799 DE 2013/120821 - IMPLANTACAO
000110*-----------------------------------------------------------------
000120
000130*-----------------------------------------------------------------
000140 IDENTIFICATION DIVISION.
000150*----------------------------------------------------------------*
000160 PROGRAM-ID. SGTS035B.
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
000310*
000320
000330 WORKING-STORAGE                 SECTION.
000340*----------------------------------------------------------------*
000350
000360 77  CTE-INICIO-SS                 PIC  X(040)  VALUE
000370              '*** S.S. COMECA AQUI ***'.
000380
000390 77  CTE-PROG                      PIC  X(018)  VALUE
000400              '*** SGTS035B ***'.
000410 77  CTE-VERS                      PIC  X(006)  VALUE 'VRS001'.
000420
000430* Módulos acionados
000440 77  IIBP2000                      PIC  X(008) VALUE 'IIBP2000'.
000450 77  SGTS0068                      PIC  X(008) VALUE 'SGTS0068'.
000460 77  MSTS8000                      PIC  X(008) VALUE 'MSTS8000'.
000470*----------------------------------------------------------------*
000480 LOCAL-STORAGE                   SECTION.
000490*----------------------------------------------------------------*
000500
000510* Areas de trabalho
000520 01  IND                           PIC S9(09) COMP-5 VALUE 0.
000530 01  IN-DIVISAO                    PIC  X(01)        VALUE ' '.
000540     88  PERTENCE-VP                                 VALUE 'S'.
000550
000560*----------------------------------------------------------------*
000570* Book dos Containers e das subrotinas chamadas
000580*----------------------------------------------------------------*
000590
000600* Book das variáveis auxiliares do tratamento de erro via MSG
=INC SGTKERR1                                                           00000610
000620* Book para captura de abend cics da bloco codigo MSGKP999
=INC MSGKW999                                                           00000630
000640
000650* Book para captura de erro via MSG1
=INC MSGKWMG1                                                           00000660
000670
000680* Book do container de requisição e resposta deste modulo
000690 01 SGTS035B-DADOS.
=INC SGTK035B                                                           00000700
000710
000720* Recuperar UOR da Vice-Presidência da Carteira TVM
000730 01  SGTS0068-DADOS.
=INC SGTK0068                                                           00000740
000750
000760* Consulta Unidade Organizacional - via IIB
000770 01  MSTS8000-DADOS.
=INC MSTK8000                                                           00000780
000790
000800* Book do Container de Erro
=INC IIBKERRO                                                           00000810
000820* Book do container de Serviços (para acionamento de serviços)
=INC IIBKSRVC                                                           00000830
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
001050
001060*----------------------------------------------------------------*
001070 100000-INICIAR                  SECTION.
001080*----------------------------------------------------------------*
001090
001100     MOVE 0001                   TO MSG1-CD-SEQL-ERRO
001110
001120*    Ativa a captura automatica de abend do CICS.
001130*    WHENEVER CICS
=INC MSGKE999                                                           00001140
001150
001160     PERFORM 970000-LER-CANAL
001170
001180*    Define o nome do programa p/ fins manipulacao container
001190     MOVE 'SGTS035B'             TO NM-PGM
001200
001210*    Limpa a resposta e erro antes de executar o modulo
001220     PERFORM 983000-EXCLUIR-RESPOSTA-ERRO
001230
001240     PERFORM 971000-LER-REQUISICAO
001250
001260     PERFORM 110000-VALIDAR-REQUISICAO
001270     .
001280 100000-SAI.
001290     EXIT.
001300
001310*----------------------------------------------------------------*
001320 110000-VALIDAR-REQUISICAO       SECTION.
001330*----------------------------------------------------------------*
001340     MOVE 0002                   TO MSG1-CD-SEQL-ERRO
001350
001360*    Valida campo a campo da requisicao
001370
001380     IF  SGTS035B-CD-UOR <= ZEROS
001390         SET ERRO-PROGRAMADO     TO TRUE
001400         MOVE 0001               TO MSG1-CD-ERRO-TS
001410         MOVE 'Código UOR informado inválido.'
001420                                 TO MSG1-TX-ERRO-TS
001430         SET MNT-VRV             TO TRUE
001440         PERFORM 999000-ERRO
001450     END-IF
001460
001470     .
001480 110000-SAI.
001490     EXIT.
001500
001510*----------------------------------------------------------------*
001520 200000-PROCESSAR                SECTION.
001530*----------------------------------------------------------------*
001540     MOVE 0003                   TO MSG1-CD-SEQL-ERRO
001550
001560     PERFORM 210000-RECUPERAR-UOR-VP
001570
001580     PERFORM 220000-TRATAR-DIVISAO-VP
001590
001600*    Movimenta dados de saida e grava container de Resposta
001610
001620     PERFORM 899000-MOVIMENTAR-SAIDA
001630
001640     .
001650 200000-SAI.
001660     EXIT.
001670
001680*----------------------------------------------------------------*
001690 210000-RECUPERAR-UOR-VP         SECTION.
001700*----------------------------------------------------------------*
001710     MOVE 0004                   TO MSG1-CD-SEQL-ERRO
001720
001730*    Movimentar o nome do programa a ser executado
001740     MOVE SGTS0068               TO NM-PGM
001750
001760*    Este módulo não possui campos de entrada
001770
001780     PERFORM 900000-EXECUTAR-MODULO
001790
001800     .
001810 210000-SAI.
001820     EXIT.
001830
001840*----------------------------------------------------------------*
001850 220000-TRATAR-DIVISAO-VP        SECTION.
001860*----------------------------------------------------------------*
001870     MOVE 0005                   TO MSG1-CD-SEQL-ERRO
001880
001890     INITIALIZE                  MSTS8000-DADOS
001900
001910     PERFORM 221000-RECUPERAR-DIVISOES-VP
001920             UNTIL MSTS8000-IND-RECHAMADA EQUAL 'N'
001930             OR    PERTENCE-VP
001940
001950     .
001960 220000-SAI.
001970     EXIT.
001980*----------------------------------------------------------------*
001990 221000-RECUPERAR-DIVISOES-VP    SECTION.
002000*----------------------------------------------------------------*
002010     MOVE 0006                   TO MSG1-CD-SEQL-ERRO
002020
002030*    Movimentar o nome do programa a ser executado
002040     MOVE MSTS8000               TO NM-PGM
002050
002060*    Movimentar os campos de entrada
002070*    Função 3-Busca UOR(S) jurisdicionada(s) de uma UOR
002080*    jurisdicionante
002090     MOVE 3                      TO MSTS8000-CD-FUNCAO
002100     MOVE SGTS0068-CD-UOR-VICE-PRSA-TVM
002110                                 TO MSTS8000-CD-UOR-VCLR-E
002120*    13  - Em funcionamento
002130     MOVE 13                     TO MSTS8000-CD-CBN-EST-E
002140*    A - Ascendente
002150     MOVE 'A'                    TO MSTS8000-IND-ORDEM-LEITURA
002160*    5   - Divisão
002170     MOVE 5                      TO
002180                                 MSTS8000-CD-ITEM-AQTT-JRDD-E(1)
002190
002200     PERFORM 910000-CHAMAR-SERVICO
002210
002220     IF  MSTS8000-CD-RTN NOT EQUAL ZEROS
002230         SET ERRO-PROGRAMA-ACIONADO TO TRUE
002240         MOVE 0002               TO MSG1-CD-ERRO-TS
002250         MOVE MSTS8000-TX-RTN    TO MSG1-TX-RTN-PGM-ACND
002260                                    MSG1-TX-ERRO-TS
002270         MOVE NM-PGM             TO MSG1-NM-PGM-ACND
002280         MOVE MSTS8000-CD-RTN    TO MSG1-CD-RTN-PGM-ACND
002290
002300*        Monitorar variáveis do módulo chamado
002310         PERFORM 969000-MONITORAR-VARIAVEIS
002320         PERFORM 999000-ERRO
002330     END-IF
002340
002350     PERFORM VARYING IND FROM 1 BY 1
002360             UNTIL   IND > MSTS8000-NR-OCR-SAI
002370             OR      PERTENCE-VP
002380
002390        IF  SGTS035B-CD-UOR EQUAL MSTS8000-CD-UOR-VCLD-S(IND)
002400            SET PERTENCE-VP      TO TRUE
002410            MOVE MSTS8000-NM-UOR-RDZ-VCLD-S(IND)
002420                                 TO SGTS035B-NM-UOR-RLCD-VICE-PRSA
002430        END-IF
002440
002450     END-PERFORM
002460     .
002470 221000-SAI.
002480     EXIT.
002490*----------------------------------------------------------------*
002500 899000-MOVIMENTAR-SAIDA         SECTION.
002510*----------------------------------------------------------------*
002520     MOVE 0007                   TO MSG1-CD-SEQL-ERRO
002530
002540     IF  PERTENCE-VP
002550         SET SGTS035B-IN-RLCD-VICE-PRSA-S
002560                                 TO TRUE
002570*    Não é uma divisão da VP
002580     ELSE
002590         SET SGTS035B-IN-RLCD-VICE-PRSA-N
002600                                 TO TRUE
002610         MOVE SPACES             TO SGTS035B-NM-UOR-RLCD-VICE-PRSA
002620     END-IF
002630
002640*    Gravando o container de resposta da subrotina
002650
002660     PERFORM 980000-GRAVAR-CONTAINER-RPST
002670
002680     .
002690 899000-SAI.
002700     EXIT.
002710
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
002840     MOVE 0008                   TO MSG1-CD-SEQL-ERRO
002850*    Grava o container de requisicao do modulo a executar
002860     PERFORM 982000-GRAVAR-CONTAINER-RQSC
002870
002880*    Limpa a resposta e erro antes de executar o modulo
002890     PERFORM 983000-EXCLUIR-RESPOSTA-ERRO
002900
002910*    Não há captura automática de erro CICS por usar RESP e RESP2
002920     EVALUATE NM-PGM
002930         WHEN SGTS0068
002940             EXEC CICS LINK
002950                       PROGRAM ( SGTS0068  )
002960                       CHANNEL ( NM-CNL    )
002970                       RESP    ( MSG1-RESP1)
002980                       RESP2   ( MSG1-RESP2)
002990             END-EXEC
003000         WHEN OTHER
003010             STRING
003020             'Não definida a execução do módulo ' NM-PGM
003030             DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
003040             PERFORM 999000-ERRO
003050     END-EVALUATE
003060*
003070     EVALUATE TRUE
003080         WHEN MSG1-RESP1        EQUAL DFHRESP(NORMAL)
003090              CONTINUE
003100         WHEN OTHER
003110              STRING
003120              'Erro na execução do módulo ' NM-PGM ', '
003130              'RESP  ='   ' ' MSG1-RESP1  ', '
003140              'RESP2 =' ' ' MSG1-RESP2
003150              DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
003160              MOVE 3004          TO MSG1-CD-ERRO-TS
003170              SET ERRO-CICS      TO TRUE
003180              PERFORM 999000-ERRO
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
003300
003310*---------------------------------------------------------------*
003320 910000-CHAMAR-SERVICO           SECTION.
003330*---------------------------------------------------------------*
003340*    Acionar programa IIBP2000 para requisição do serviço.
003350*---------------------------------------------------------------*
003360     MOVE 0009                   TO MSG1-CD-SEQL-ERRO
003370
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
003590              STRING 'Erro ao acionar serviço: '
003600                     SRVC-ID-SRVC '.' SRVC-VRS-SRVC '.'
003610                     SRVC-ID-OPR  '.' SRVC-VRS-OPR
003620                     'via IIBP2000, '
003630                     'RESP=' MSG1-RESP1 ', RESP-2=' MSG1-RESP2
003640                     '.'         DELIMITED BY SIZE
003650                                 INTO MSG1-TX-ERRO-TS
003660              SET ERRO-CICS      TO TRUE
003670              PERFORM 999000-ERRO
003680     END-EVALUATE
003690
003700*    Efetua a leitura do Container de Erro
003710     PERFORM 972000-LER-CONTAINER-DE-ERRO
003720
003730*    Efetua a leitura do Container de Resposta
003740     PERFORM 973000-LER-RESPOSTA
003750
003760     .
003770 910000-SAI.
003780     EXIT.
003790*----------------------------------------------------------------*
003800 969000-MONITORAR-VARIAVEIS      SECTION.
003810*----------------------------------------------------------------*
003820* Monitora as variáveis dos módulos chamados
003830*----------------------------------------------------------------*
003840     MOVE 0010                   TO MSG1-CD-SEQL-ERRO
003850
003860     EVALUATE NM-PGM
003870         WHEN MSTS8000
003880              MOVE 'MSTS8000-CD-FUNCAO'
003890                                 TO MSGKW999-NM-VRV-MNT
003900              MOVE MSTS8000-CD-FUNCAO
003910                                 TO MSGKW999-VL-VRV-MNT
003920              PERFORM 999003-ADICIONA-VRV-MNT
003930
003940              MOVE 'MSTS8000-CD-UOR-VCLR-E'
003950                                 TO MSGKW999-NM-VRV-MNT
003960              MOVE MSTS8000-CD-UOR-VCLR-E
003970                                 TO MSGKW999-VL-VRV-MNT
003980              PERFORM 999003-ADICIONA-VRV-MNT
003990
004000              MOVE 'MSTS8000-CD-CBN-EST-E'
004010                                 TO MSGKW999-NM-VRV-MNT
004020              MOVE MSTS8000-CD-CBN-EST-E
004030                                 TO MSGKW999-VL-VRV-MNT
004040              PERFORM 999003-ADICIONA-VRV-MNT
004050
004060              MOVE 'MSTS8000-IND-ORDEM-LEITURA'
004070                                 TO MSGKW999-NM-VRV-MNT
004080              MOVE MSTS8000-IND-ORDEM-LEITURA
004090                                 TO MSGKW999-VL-VRV-MNT
004100              PERFORM 999003-ADICIONA-VRV-MNT
004110
004120              MOVE 'MSTS8000-CD-ITEM-AQTT-JRDD-E(1)'
004130                                 TO MSGKW999-NM-VRV-MNT
004140              MOVE MSTS8000-CD-ITEM-AQTT-JRDD-E(1)
004150                                 TO MSGKW999-VL-VRV-MNT
004160              PERFORM 999003-ADICIONA-VRV-MNT
004170
004180         WHEN OTHER
004190              CONTINUE
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
004340     MOVE 0011                   TO MSG1-CD-SEQL-ERRO
004350
004360*Por causa da captura automática de erro CICS, deve ser previsto
004370*o erro de container
004380     SET  ERRO-CANAL             TO TRUE
004390
004400     EXEC CICS ASSIGN
004410               CHANNEL ( NM-CNL )
004420     END-EXEC
004430
004440*    Ao chegar aqui, há certeza de que não houve erro algum
004450     SET  SEM-ERRO               TO TRUE
004460     .
004470 970000-FIM.
004480     EXIT.
004490
004500*----------------------------------------------------------------*
004510 971000-LER-REQUISICAO           SECTION.
004520*----------------------------------------------------------------*
004530
004540     MOVE 0012                   TO MSG1-CD-SEQL-ERRO
004550
004560*    Monta o nome do container de requisicao
004570     MOVE 'SGTS035B'             TO NM-PGM
004580     MOVE 'SGTS035B-RQSC'        TO NM-CTNR
004590
004600*    Recupera o tamanho do container de requisição
004610     PERFORM 973100-LER-CONTAINER-NODATA
004620
004630*    Obtem o tamanho esperado da requisicao
004640     MOVE LENGTH OF SGTS035B-RQSC TO QT-CCT-EPRO
004650
004660*    Valida tamanho esperado da requisicao X tamanho recebido
004670
004680     IF  QT-CCT-LIDO             NOT EQUAL QT-CCT-EPRO
004690         SET ERRO-TAMANHO-AREA   TO TRUE
004700         PERFORM 999000-GRAVA-ERRO-E-ENCERRA
004710     END-IF
004720
004730     MOVE 0013                   TO MSG1-CD-SEQL-ERRO
004740
004750*Configura dados para leitura do container de requisição
004760     SET  GET-CONTAINER          TO TRUE
004770
004780*Por causa da captura automática de erro CICS, deve ser previsto
004790*o erro de container
004800     SET  ERRO-CONTAINER         TO TRUE
004810
004820*    Efetua a leitura do container
004830     EXEC CICS GET
004840               CONTAINER (NM-CTNR)
004850               FLENGTH   (QT-CCT-LIDO)
004860               INTO      (SGTS035B-RQSC)
004870     END-EXEC
004880
004890*Ao chegar aqui, há certeza de que não houve erro algum
004900     SET  SEM-ERRO               TO TRUE
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
005020
005030     MOVE 0014                   TO MSG1-CD-SEQL-ERRO
005040
005050*    Le o container de erro dos modulos chamados
005060     SET GET-CONTAINER           TO TRUE
005070     MOVE 'PRM-ERRO'             TO NM-CTNR
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
005220*        PARA O MÓDULO CHAMADO e tratar na section chamadora
005230         WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
005240              CONTINUE
005250         WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
005260              SET ERRO-PROGRAMA-ACIONADO TO TRUE
005270              MOVE 0003          TO MSG1-CD-ERRO-TS
005280              MOVE IIBKERRO-TX-ERRO
005290                                 TO MSG1-TX-RTN-PGM-ACND
005300                                    MSG1-TX-ERRO-TS
005310              MOVE NM-PGM        TO MSG1-NM-PGM-ACND
005320              MOVE IIBKERRO-CD-ERRO
005330                                 TO MSG1-CD-RTN-PGM-ACND
005340
005350*             Monitorar variáveis do módulo chamado
005360              PERFORM 969000-MONITORAR-VARIAVEIS
005370              PERFORM 999000-ERRO
005380         WHEN OTHER
005390              SET ERRO-CONTAINER  TO TRUE
005400              PERFORM 999002-TRATA-CONDITION-CICS
005410     END-EVALUATE
005420     .
005430 972000-SAI.
005440     EXIT.
005450
005460*----------------------------------------------------------------*
005470 973000-LER-RESPOSTA             SECTION.
005480*----------------------------------------------------------------*
005490*    Recupera o container com os dados de resposta (saída).
005500*
005510*                        NOME DO CONTAINER RESPOSTA
005520*                        --------------------------
005530*
005540*    Modulo CONTAINER  -     NOME_PGM-RPST
005550*    Modulo IIB (FIXO) -     PRM-DADOS-RPST
005560*
005570*---------------------------------------------------------------*
005580
005590     MOVE 0015                   TO MSG1-CD-SEQL-ERRO
005600
005610*    Monta o nome dos container de acordo com modulo a executar
005620     MOVE SPACES                 TO NM-CTNR
005630
005640*    Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
005650*    nome_modulo -RPST
005660     EVALUATE NM-PGM
005670         WHEN MSTS8000
005680              MOVE 'PRM-DADOS-RPST'
005690                                 TO NM-CTNR
005700         WHEN OTHER
005710              MOVE NM-PGM        TO NM-CTNR(1:8)
005720              MOVE '-RPST'       TO NM-CTNR(9:5)
005730     END-EVALUATE
005740
005750*    Recupera o tamanho do container de resposta
005760     PERFORM 973100-LER-CONTAINER-NODATA
005770
005780*Configura dados para leitura do container de resposta
005790     SET  GET-CONTAINER          TO TRUE
005800
005810*Por causa da captura automática de erro CICS, deve ser previsto
005820*o erro de container
005830     SET  ERRO-CONTAINER         TO TRUE
005840
005850
005860*    Le a RESPOSTA de acordo com o programa que a gravou
005870
005880     MOVE 0016                   TO MSG1-CD-SEQL-ERRO
005890
005900     EVALUATE NM-PGM
005910
005920*        Calcular a quantidade de caracteres esperado e limpar
005930*        a area de dados antes de executar o GET
005940*        Para matrizes com dados variaveis (clausula depending
005950*        on) nao ha como utilizar o comando INITIALIZE
005960
005970         WHEN SGTS0068
005980              INITIALIZE SGTS0068-RPST
005990              MOVE LENGTH OF SGTS0068-RPST TO QT-CCT-EPRO
006000              EXEC CICS GET
006010                        CONTAINER (NM-CTNR          )
006020                        INTO      (SGTS0068-RPST    )
006030                        FLENGTH   (QT-CCT-LIDO      )
006040              END-EXEC
006050         WHEN MSTS8000
006060              INITIALIZE MSTS8000-DADOS
006070              MOVE LENGTH OF MSTS8000-DADOS TO QT-CCT-EPRO
006080              EXEC CICS GET
006090                        CONTAINER (NM-CTNR          )
006100                        INTO      (MSTS8000-DADOS   )
006110                        FLENGTH   (QT-CCT-LIDO      )
006120              END-EXEC
006130         WHEN OTHER
006140              STRING
006150              'Get nao definido para resposta do modulo ' NM-PGM
006160              DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
006170              PERFORM 999000-ERRO
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
006300     MOVE 0017                   TO MSG1-CD-SEQL-ERRO
006310
006320*Configura dados para leitura do container de requisição
006330     SET  GET-CONTAINER          TO TRUE
006340
006350*Por causa da captura automática de erro CICS, deve ser previsto
006360*o erro de container
006370     SET  ERRO-CONTAINER         TO TRUE
006380
006390*    Obtem a quantidade de caracteres no container
006400     MOVE ZEROS                  TO QT-CCT-LIDO
006410     EXEC CICS GET
006420               CONTAINER (NM-CTNR)
006430               NODATA
006440               FLENGTH   (QT-CCT-LIDO)
006450     END-EXEC
006460
006470*    Ao chegar aqui, há certeza de que não houve erro algum
006480     SET  SEM-ERRO               TO TRUE
006490     .
006500 973100-SAI.
006510     EXIT.
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
006630     MOVE 0019                   TO MSG1-CD-SEQL-ERRO
006640
006650*    Monta o nome do container de resposta
006660     MOVE 'SGTS035B-RPST'        TO NM-CTNR
006670     SET PUT-CONTAINER           TO TRUE
006680
006690*Por causa da captura automática de erro CICS, deve ser previsto
006700*o erro de container
006710     SET  ERRO-CONTAINER         TO TRUE
006720
006730     EXEC CICS PUT
006740               CONTAINER (NM-CTNR                    )
006750               FROM      (SGTS035B-RPST          )
006760               FLENGTH   (LENGTH OF SGTS035B-RPST)
006770     END-EXEC
006780
006790*Ao chegar aqui, há certeza de que não houve erro algum
006800     SET  SEM-ERRO               TO TRUE
006810     .
006820 980000-SAI.
006830     EXIT.
006840*---------------------------------------------------------------*
006850 981000-GRAVAR-CONTAINER-SRVC    SECTION.
006860*---------------------------------------------------------------*
006870*    Postar container de requisição de serviço.
006880*---------------------------------------------------------------*
006890     MOVE 0020                   TO MSG1-CD-SEQL-ERRO
006900
006910*    Monta o nome do container de requisicao do servico
006920     MOVE 'PRM-RQSC-SRVC'        TO NM-CTNR
006930     SET PUT-CONTAINER           TO TRUE
006940
006950     EVALUATE NM-PGM
006960         WHEN MSTS8000
006970              MOVE 21880         TO SRVC-ID-SRVC
006980              MOVE 1             TO SRVC-VRS-SRVC
006990              MOVE 79047         TO SRVC-ID-OPR
007000              MOVE 1             TO SRVC-VRS-OPR
007010     END-EVALUATE
007020
007030*Por causa da captura automática de erro CICS, deve ser previsto
007040*o erro de container
007050     SET  ERRO-CONTAINER         TO TRUE
007060
007070     EXEC CICS PUT
007080               CONTAINER ( NM-CTNR                     )
007090               FROM      ( SRVC-PRM-RQSC-SRVC          )
007100               FLENGTH   ( LENGTH OF SRVC-PRM-RQSC-SRVC)
007110     END-EXEC
007120
007130*Ao chegar aqui, há certeza de que não houve erro algum
007140     SET  SEM-ERRO               TO TRUE
007150
007160     .
007170 981000-SAI.
007180     EXIT.
007190*----------------------------------------------------------------*
007200 982000-GRAVAR-CONTAINER-RQSC    SECTION.
007210*----------------------------------------------------------------*
007220*    Postar container com dados de requisição (entrada)
007230*
007240*                        NOME DO CONTAINER REQUISICAO
007250*                        ----------------------------
007260*
007270*    Modulo CONTAINER  -     NOME_PGM-RQSC
007280*    Modulo IIB (FIXO) -     PRM-DADOS-RQSC
007290*
007300*----------------------------------------------------------------*
007310
007320     MOVE 0021                   TO MSG1-CD-SEQL-ERRO
007330
007340*    Monta o nome dos container de acordo com modulo a executar
007350     MOVE SPACES                 TO NM-CTNR
007360     SET PUT-CONTAINER           TO TRUE
007370
007380*    Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
007390*    nome_modulo -RPST
007400     EVALUATE NM-PGM
007410         WHEN MSTS8000
007420              MOVE 'PRM-DADOS-RQSC'
007430                                 TO NM-CTNR
007440         WHEN OTHER
007450              MOVE NM-PGM        TO NM-CTNR(1:8)
007460              MOVE '-RQSC'       TO NM-CTNR(9:5)
007470     END-EVALUATE
007480
007490*Por causa da captura automática de erro CICS, deve ser previsto
007500*o erro de container
007510     SET  ERRO-CONTAINER         TO TRUE
007520
007530*    Faz put da requisicao do programa a executar
007540     EVALUATE NM-PGM
007550         WHEN SGTS0068
007560*             Este módulo não tem entrada
007570              CONTINUE
007580
007590         WHEN MSTS8000
007600              EXEC CICS PUT
007610                        CONTAINER (NM-CTNR)
007620                        FROM      (MSTS8000-DADOS )
007630                        FLENGTH   (LENGTH OF MSTS8000-DADOS )
007640              END-EXEC
007650         WHEN OTHER
007660              STRING
007670              'Put não definido para requisição do módulo ' NM-PGM
007680              DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
007690              PERFORM 999000-ERRO
007700     END-EVALUATE
007710
007720*Ao chegar aqui, há certeza de que não houve erro algum
007730     SET  SEM-ERRO               TO TRUE
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
007880     MOVE 0022                   TO MSG1-CD-SEQL-ERRO
007890
007900*    Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
007910*    nome_modulo -RPST
007920     EVALUATE NM-PGM
007930         WHEN MSTS8000
007940              MOVE 'PRM-DADOS-RPST'
007950                                 TO NM-CTNR
007960         WHEN OTHER
007970              MOVE NM-PGM        TO NM-CTNR(1:8)
007980              MOVE '-RPST'       TO NM-CTNR(9:5)
007990     END-EVALUATE
008000
008010     SET DEL-CONTAINER           TO TRUE
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
008130         WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
008140              CONTINUE
008150         WHEN OTHER
008160              SET ERRO-CONTAINER TO TRUE
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
008340     MOVE 0023                   TO MSG1-CD-SEQL-ERRO
008350     MOVE 'PRM-ERRO'             TO NM-CTNR
008360     SET DEL-CONTAINER           TO TRUE
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
008470         WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
008480              CONTINUE
008490         WHEN OTHER
008500              SET ERRO-CONTAINER TO TRUE
008510              PERFORM 999002-TRATA-CONDITION-CICS
008520     END-EVALUATE
008530     .
008540 983100-SAI.
008550     EXIT.
008560*----------------------------------------------------------------*
008570 984000-GRAVAR-CONTAINER-ERRO    SECTION.
008580*----------------------------------------------------------------*
008590     MOVE 0024                   TO MSG1-CD-SEQL-ERRO
008600
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
008790     EXEC CICS
008800          RETURN
008810     END-EXEC
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
008980     IF MSG1-TX-ERRO-TS EQUAL SPACES
008990        MOVE 'ERRO NAO ESPECIFICADO NO MODULO' TO MSG1-TX-ERRO-TS
009000     END-IF
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
009230         WHEN ERRO-TAMANHO-AREA
009240              PERFORM 999005-TRATA-ERRO-AREA-DADOS
009250         WHEN ERRO-CANAL
009260              PERFORM 999006-TRATA-ERRO-CANAL
009270         WHEN OTHER
009280              CONTINUE
009290     END-EVALUATE
009300
009310*Adiciona as variáveis de entrada para monitoração
009320     IF  MNT-VRV
009330         MOVE 'SGTS035B-CD-UOR'
009340                                 TO MSGKW999-NM-VRV-MNT
009350         MOVE  SGTS035B-CD-UOR
009360                                 TO MSGKW999-VL-VRV-MNT
009370         PERFORM 999003-ADICIONA-VRV-MNT
009380
009390     END-IF
009400
009410*    Grava o erro gerado no sistema MSG - Mensageria
009420     PERFORM MSG1-NTF-MSG
009430
009440     MOVE MSG1-CD-ERRO-TS        TO IIBKERRO-CD-ERRO
009450     MOVE MSG1-TX-ERRO-TS        TO IIBKERRO-TX-ERRO
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
=INC MSGKP999                                                           00009600
009610* --------------------------------------*
009620* 999004-TRATA-ERRO-CTNR         SECTION.
009630* 999005-TRATA-ERRO-AREA-DADOS   SECTION.
009640* 999006-TRATA-ERRO-CANAL        SECTION.
009650*---------------------------------------*
=INC SGTKP999                                                           00009660
009670* --------------------------------------*
009680* MSG1-NTF-MSG                   SECTION.
009690* --------------------------------------*
=INC MSGKPMG1                                                           00009700
009710*---------------------- FIM SGTS035B -----------------------------