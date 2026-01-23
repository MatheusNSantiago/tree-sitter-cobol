000010*-----------------------------------------------------------------
000020* PROGRAMA..: SGTS0041
000030* ANALISTA..: F2054109 - Cintia Ap. Soares Iglesias
000040* AUTOR.....: F2021264 - Christine Mitsue Akahoshi
000050* DATA......: 16.11.2015
000060* OBJETIVO..: Confirmar Característica de Operação Estruturada
000070* COMPILACAO: COBOL/CICS
000080*-----------------------------------------------------------------
000090* VRS001 16.11.2015 F2021264  DE 2013/120821 - Implantação
000100*-----------------------------------------------------------------
000110
000120*-----------------------------------------------------------------
000130 IDENTIFICATION DIVISION.
000140*----------------------------------------------------------------*
000150 PROGRAM-ID. SGTS0041.
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
000260*
000270*----------------------------------------------------------------*
000280 DATA DIVISION.
000290*----------------------------------------------------------------*
000300*
000310
000320 WORKING-STORAGE                 SECTION.
000330*----------------------------------------------------------------*
000340
000350 77  CTE-INICIO-SS               PIC  X(040)  VALUE
000360              '*** S.S. COMECA AQUI ***'.
000370
000380 77  CTE-PROG                    PIC  X(018)  VALUE
000390              '*** SGTS0041 ***'.
000400 77  CTE-VERS                    PIC  X(006)  VALUE 'VRS001'.
000410
000420* Módulos acionados
000430 77  SGTST041                    PIC  X(008) VALUE 'SGTST041'.
000440*----------------------------------------------------------------*
000450 LOCAL-STORAGE                   SECTION.
000460*----------------------------------------------------------------*
000470* Areas de trabalho
000480
000490 01  OPCAO                       PIC S9(004) COMP-5 VALUE 0.
000500
000510*----------------------------------------------------------------*
000520* Book dos Containers e das subrotinas chamadas
000530*----------------------------------------------------------------*
000540
000550* Book das variáveis auxiliares do tratamento de erro via MSG
=INC SGTKERR1                                                           0000056%
000570
000580* Book para captura de abend cics da bloco codigo MSGKP999
=INC MSGKW999                                                           0000059%
000600
000610* Book para captura de erro via MSG1
=INC MSGKWMG1                                                           0000062%
000630
000640* Book do container de requisição e resposta deste modulo
000650 01  SGTS0041-DADOS.
=INC SGTK0041                                                           0000066%
000670
000680* CRUD da tabela  EST_CRCT_OPR
000690 01  SGTST041-DADOS.
=INC SGTKT041                                                           0000070%
000710
000720* Book do Container de Erro
=INC IIBKERRO                                                           0000073%
000740
000750* Book dos dados do usuário
=INC IIBKOPRD                                                           0000076%
000770
000780 77  CTE-FINAL-SS                  PIC X(40)      VALUE
000790              '*** S.S. TERMINA AQUI ***'.
000800
000810*----------------------------------------------------------------*
000820 PROCEDURE DIVISION.
000830*----------------------------------------------------------------*
000840
000850*----------------------------------------------------------------*
000860 000000-ROTINA-PRINCIPAL         SECTION.
000870*----------------------------------------------------------------*
000880
000890     PERFORM 100000-INICIAR
000900
000910     PERFORM 200000-PROCESSAR
000920
000930     PERFORM 990000-ENCERRAR
000940     .
000950 000000-SAI.
000960     EXIT.
000970
000980*----------------------------------------------------------------*
000990 100000-INICIAR                  SECTION.
001000*----------------------------------------------------------------*
001010
001020     MOVE 0001                   TO MSG1-CD-SEQL-ERRO
001030
001040* Ativa a captura automatica de abend do CICS.
001050* WHENEVER CICS
=INC MSGKE999                                                           0000106%
001070
001080     PERFORM 970000-LER-CANAL
001090
001100* Define o nome do programa p/ fins manipulacao container
001110     MOVE 'SGTS0041'             TO NM-PGM
001120     MOVE 'SGTS0041-RPST'        TO NM-CTNR
001130
001140* Limpa a resposta e erro antes de executar o modulo
001150     PERFORM 983000-EXCLUIR-RESPOSTA-ERRO
001160
001170* Recupera a requisição do modulo
001180     PERFORM 971000-LER-REQUISICAO
001190
001200* Recupera usuário logado
001210     PERFORM 974000-LER-DADOS-OPRD
001220
001230     PERFORM 110000-VALIDAR-REQUISICAO
001240     .
001250 100000-SAI.
001260     EXIT.
001270
001280*----------------------------------------------------------------*
001290 110000-VALIDAR-REQUISICAO       SECTION.
001300*----------------------------------------------------------------*
001310* Valida campo a campo da requisicao
001320*----------------------------------------------------------------*
001330     MOVE 0002                   TO MSG1-CD-SEQL-ERRO
001340
001350     IF  SGTS0041-CD-TIP-CRCT-OPR <= ZEROS
001360         SET ERRO-PROGRAMADO     TO TRUE
001370         MOVE 0001               TO MSG1-CD-ERRO-TS
001380         MOVE 'Código tipo atributo da característica inválido.'
001390                                 TO MSG1-TX-ERRO-TS
001400         SET MNT-VRV             TO TRUE
001410         PERFORM 999000-ERRO
001420     END-IF
001430
001440     IF  SGTS0041-NR-ETRD-CRCT-OPR  <= ZEROS
001450         SET ERRO-PROGRAMADO     TO TRUE
001460         MOVE 0002               TO MSG1-CD-ERRO-TS
001470         MOVE 'Numero estruturado da caracteristica inválido.'
001480                                 TO MSG1-TX-ERRO-TS
001490         SET MNT-VRV             TO TRUE
001500         PERFORM 999000-ERRO
001510     END-IF
001520
001530     IF  NOT SGTS0041-IN-ACTE-S
001540     AND NOT SGTS0041-IN-ACTE-N
001550         SET ERRO-PROGRAMADO     TO TRUE
001560         MOVE 0003               TO MSG1-CD-ERRO-TS
001570         MOVE 'Indicador aceite inválido.'
001580                                 TO MSG1-TX-ERRO-TS
001590         SET MNT-VRV             TO TRUE
001600         PERFORM 999000-ERRO
001610     END-IF
001620
001630     IF  SGTS0041-TX-OBS-EST-CRCT  = SPACES
001640         IF  SGTS0041-IN-ACTE-N
001650             SET ERRO-PROGRAMADO TO TRUE
001660             MOVE 0004           TO MSG1-CD-ERRO-TS
001670             MOVE 'Motivo de rejeição inválido.'
001680                                 TO MSG1-TX-ERRO-TS
001690             SET MNT-VRV         TO TRUE
001700             PERFORM 999000-ERRO
001710         END-IF
001720     END-IF
001730
001740     IF  SGTS0041-TS-ULT-EST-CRCT-OPR  = SPACES
001750         SET ERRO-PROGRAMADO     TO TRUE
001760         MOVE 0005               TO MSG1-CD-ERRO-TS
001770         MOVE 'Timestamp da atualização de estado inválido.'
001780                                 TO MSG1-TX-ERRO-TS
001790         SET MNT-VRV             TO TRUE
001800         PERFORM 999000-ERRO
001810     END-IF
001820     .
001830 110000-SAI.
001840     EXIT.
001850
001860*----------------------------------------------------------------*
001870 200000-PROCESSAR                SECTION.
001880*----------------------------------------------------------------*
001890     MOVE 0003                   TO MSG1-CD-SEQL-ERRO
001900
001910* Valida estado atual do Tipo atributo da caracteristica informado
001920     PERFORM 210000-VALIDAR-EST-ATUAL
001930
001940* Se indicador de aceite da versão atual = sim
001950* Trata ultima versão aprovada, se houver.
001960     IF  SGTS0041-IN-ACTE-S
001970        PERFORM 220000-TRATAR-VERSAO
001980     END-IF
001990
002000* Inclui Validação da Caracteristica (S/N)
002010     MOVE SPACES                 TO SGTST041-DADOS
002020     INITIALIZE  SGTST041-DADOS
002030         REPLACING NUMERIC       BY  ZEROS
002040
002050* Opção 1 - Incluir validação da versão
002060     MOVE 1                      TO OPCAO
002070     MOVE SGTS0041-NR-ETRD-CRCT-OPR
002080                                 TO SGTST041-NR-ETRD-CRCT-OPR
002090     MOVE 'CURRENT                   '
002100                                 TO SGTST041-TS-EST-CRCT-OPR
002110     MOVE KOPRD-CD-OPRD          TO SGTST041-CD-USU-ATL-EST
002120     MOVE SGTS0041-TX-OBS-EST-CRCT
002130                                 TO SGTST041-TX-OBS-EST-CRCT
002140* Estado 2 = Aprovado
002150* Estado 3 = Rejeitado
002160     IF  SGTS0041-IN-ACTE-S
002170         MOVE 2                  TO SGTST041-CD-TIP-EST-DCS
002180     ELSE
002190         MOVE 3                  TO SGTST041-CD-TIP-EST-DCS
002200     END-IF
002210
002220     PERFORM 920000-CHAMAR-SGTST041
002230
002240     PERFORM 899000-MOVIMENTAR-SAIDA
002250     .
002260 200000-SAI.
002270     EXIT.
002280
002290*----------------------------------------------------------------*
002300 210000-VALIDAR-EST-ATUAL        SECTION.
002310*----------------------------------------------------------------*
002320     MOVE 0004                   TO MSG1-CD-SEQL-ERRO
002330
002340     MOVE SPACES                 TO SGTST041-DADOS
002350     INITIALIZE  SGTST041-DADOS
002360         REPLACING NUMERIC       BY  ZEROS
002370
002380* Opção 5 - Consultar estado atual
002390     MOVE 5                      TO OPCAO
002400     MOVE SGTS0041-NR-ETRD-CRCT-OPR
002410                                 TO SGTST041-NR-ETRD-CRCT-OPR
002420
002430     PERFORM 920000-CHAMAR-SGTST041
002440
002450
002460* Se estado <>1 (pendente de confirmação), Caracteristica não pode
002470* ser validada
002480     IF  SGTST041-CD-TIP-EST-DCS  NOT EQUAL 1
002490         SET ERRO-PROGRAMADO     TO TRUE
002500         MOVE 0008               TO MSG1-CD-ERRO-TS
002510         MOVE 'Estado atual não permite validação.'
002520                                 TO MSG1-TX-ERRO-TS
002530         SET MNT-VRV             TO TRUE
002540         PERFORM 999000-ERRO
002550     END-IF
002560
002570     IF  SGTST041-TS-EST-CRCT-OPR NOT =
002580                                 SGTS0041-TS-ULT-EST-CRCT-OPR
002590         SET ERRO-PROGRAMADO     TO TRUE
002600         MOVE 0006               TO MSG1-CD-ERRO-TS
002610         MOVE 'Dados sofreram edição. Favor recarregar a tela.'
002620                                 TO MSG1-TX-ERRO-TS
002630         SET MNT-VRV             TO TRUE
002640         PERFORM 999000-ERRO
002650     END-IF
002660
002670     .
002680 210000-SAI.
002690     EXIT.
002700
002710*----------------------------------------------------------------*
002720 220000-TRATAR-VERSAO            SECTION.
002730*----------------------------------------------------------------*
002740     MOVE 0005                   TO MSG1-CD-SEQL-ERRO
002750
002760     MOVE SPACES                 TO SGTST041-DADOS
002770     INITIALIZE  SGTST041-DADOS
002780         REPLACING NUMERIC       BY  ZEROS
002790
002800* Opção 7 - Consultar ultima versão aprovada
002810     MOVE 7                      TO OPCAO
002820
002830     PERFORM 920000-CHAMAR-SGTST041
002840
002850     IF  SGTST041-NR-ETRD-CRCT-OPR  > 0
002860* Opção 1 - Incluir encerramento da ultima versão
002870         MOVE 1                  TO OPCAO
002880         MOVE 99                 TO SGTST041-CD-TIP-EST-DCS
002890         MOVE 'CURRENT                   '
002900                                 TO SGTST041-TS-EST-CRCT-OPR
002910         MOVE KOPRD-CD-OPRD      TO SGTST041-CD-USU-ATL-EST
002920         MOVE SPACES             TO SGTST041-TX-OBS-EST-CRCT
002930         PERFORM 920000-CHAMAR-SGTST041
002940     END-IF
002950     .
002960 220000-SAI.
002970     EXIT.
002980
002990*----------------------------------------------------------------*
003000 899000-MOVIMENTAR-SAIDA         SECTION.
003010*----------------------------------------------------------------*
003020     MOVE 0007                   TO MSG1-CD-SEQL-ERRO
003030
003040     MOVE 'Confirmação realizada com sucesso.'
003050                                 TO SGTS0041-TX-MSG-RTN-PRCT
003060
003070* Grava o container de resposta da subrotina
003080     PERFORM 980000-GRAVAR-CONTAINER-RPST
003090     .
003100 899000-SAI.
003110     EXIT.
003120
003130*----------------------------------------------------------------*
003140*                                                                *
003150*  SECTION DA FAIXA DE 900000 A 969900 PARA DEMAIS ROTINAS       *
003160*                                                                *
003170*----------------------------------------------------------------*
003180*----------------------------------------------------------------*
003190 920000-CHAMAR-SGTST041          SECTION.
003200*----------------------------------------------------------------*
003210     MOVE 0010                   TO MSG1-CD-SEQL-ERRO
003220
003230     MOVE KOPRD-CD-OPRD          TO SGTST041-CD-CHV-USU
003240     MOVE OPCAO                  TO SGTST041-ACAO
003250     MOVE SGTS0041-CD-TIP-CRCT-OPR
003260                                 TO SGTST041-CD-TIP-CRCT-OPR
003270
003280* Mover nome do módulo para monitoração de variáveis
003290     MOVE SGTST041               TO NM-PGM
003300
003310     EXEC CICS LINK
003320          PROGRAM  (SGTST041)
003330          COMMAREA (SGTST041-DADOS)
003340     END-EXEC
003350
003360     IF  SGTST041-CD-RTN-PGM     NOT = ZEROS
003370         IF  SGTST041-CD-RTN-PGM = 100
003380         AND OPCAO               = 7
003390             CONTINUE
003400         ELSE
003410             SET ERRO-PROGRAMA-ACIONADO TO TRUE
003420             MOVE 0009           TO MSG1-CD-ERRO-TS
003430             MOVE SGTST041-MSG-ERRO-PGM
003440                                 TO MSG1-TX-RTN-PGM-ACND
003450                                    MSG1-TX-ERRO-TS
003460             MOVE NM-PGM         TO MSG1-NM-PGM-ACND
003470             MOVE SGTST041-CD-RTN-PGM
003480                                 TO MSG1-CD-RTN-PGM-ACND
003490* Monitora variáveis do módulo chamado
003500             PERFORM 969000-MONITORAR-VARIAVEIS
003510             PERFORM 999000-ERRO
003520         END-IF
003530     END-IF
003540     .
003550 920000-SAI.
003560     EXIT.
003570
003580*----------------------------------------------------------------*
003590 969000-MONITORAR-VARIAVEIS      SECTION.
003600*----------------------------------------------------------------*
003610* Monitora as variáveis dos módulos chamados(Mensageria IIB)
003620*----------------------------------------------------------------*
003630     MOVE 0011                   TO MSG1-CD-SEQL-ERRO
003640
003650     EVALUATE NM-PGM
003660         WHEN SGTST041
003670             MOVE 'SGTST041-ACAO '
003680                                 TO MSGKW999-NM-VRV-MNT
003690             MOVE SGTST041-ACAO
003700                                 TO MSGKW999-VL-VRV-MNT
003710             PERFORM 999003-ADICIONA-VRV-MNT
003720
003730             MOVE 'SGTST041-CD-TIP-CRCT-OPR'
003740                                 TO MSGKW999-NM-VRV-MNT
003750             MOVE SGTST041-CD-TIP-CRCT-OPR
003760                                 TO MSGKW999-VL-VRV-MNT
003770             PERFORM 999003-ADICIONA-VRV-MNT
003780
003790             MOVE 'SGTST041-NR-ETRD-CRCT-OPR'
003800                                 TO MSGKW999-NM-VRV-MNT
003810             MOVE SGTST041-NR-ETRD-CRCT-OPR
003820                                 TO MSGKW999-VL-VRV-MNT
003830             PERFORM 999003-ADICIONA-VRV-MNT
003840
003850             MOVE 'SGTST041-TS-EST-CRCT-OPR'
003860                                 TO MSGKW999-NM-VRV-MNT
003870             MOVE SGTST041-TS-EST-CRCT-OPR
003880                                 TO MSGKW999-VL-VRV-MNT
003890             PERFORM 999003-ADICIONA-VRV-MNT
003900
003910             MOVE 'SGTST041-CD-TIP-EST-DCS'
003920                                 TO MSGKW999-NM-VRV-MNT
003930             MOVE SGTST041-CD-TIP-EST-DCS
003940                                 TO MSGKW999-VL-VRV-MNT
003950             PERFORM 999003-ADICIONA-VRV-MNT
003960
003970         WHEN OTHER
003980             CONTINUE
003990     END-EVALUATE
004000     .
004010 969000-SAI.
004020     EXIT.
004030
004040*----------------------------------------------------------------*
004050*                                                                *
004060* SECTION DA FAIXA DE 970000 A 979900 PARA LEITURA DE DADOS      *
004070*                                                                *
004080*----------------------------------------------------------------*
004090 970000-LER-CANAL                SECTION.
004100*----------------------------------------------------------------*
004110* Obtem o canal corrente
004120*----------------------------------------------------------------*
004130     MOVE 0012                   TO MSG1-CD-SEQL-ERRO
004140
004150* Por causa da captura automática de erro CICS, deve ser previsto
004160* o erro de container
004170     SET  ERRO-CANAL             TO TRUE
004180
004190     EXEC CICS ASSIGN
004200               CHANNEL ( NM-CNL )
004210     END-EXEC
004220
004230* Ao chegar aqui, há certeza de que não houve erro algum
004240     SET  SEM-ERRO               TO TRUE
004250     .
004260 970000-SAI.
004270     EXIT.
004280
004290*----------------------------------------------------------------*
004300 971000-LER-REQUISICAO           SECTION.
004310*----------------------------------------------------------------*
004320     MOVE 0013                   TO MSG1-CD-SEQL-ERRO
004330
004340* Formata o nome do container de requisicao
004350     MOVE 'SGTS0041'             TO NM-PGM
004360     MOVE 'SGTS0041-RQSC'        TO NM-CTNR
004370
004380* Recupera o tamanho do container de requisição
004390     PERFORM 975000-LER-CONTAINER-NODATA
004400
004410* Obtem o tamanho esperado da requisicao
004420     MOVE LENGTH OF SGTS0041-RQSC TO QT-CCT-EPRO
004430
004440* Valida tamanho esperado da requisicao X tamanho recebido
004450     IF  QT-CCT-LIDO             NOT = QT-CCT-EPRO
004460         SET ERRO-TAMANHO-AREA   TO TRUE
004470         PERFORM 999000-GRAVA-ERRO-E-ENCERRA
004480     END-IF
004490
004500     MOVE 0014                   TO MSG1-CD-SEQL-ERRO
004510
004520* Configura dados para leitura do container de requisição
004530     SET  GET-CONTAINER          TO TRUE
004540
004550* Por causa da captura automática de erro CICS, deve ser previsto
004560* o erro de container
004570     SET  ERRO-CONTAINER         TO TRUE
004580
004590* Efetua a leitura do container
004600     EXEC CICS GET
004610               CONTAINER ( NM-CTNR       )
004620               FLENGTH   ( QT-CCT-LIDO   )
004630               INTO      ( SGTS0041-RQSC )
004640     END-EXEC
004650
004660* Ao chegar aqui, há certeza de que não houve erro algum
004670     SET  SEM-ERRO               TO TRUE
004680     .
004690 971000-SAI.
004700     EXIT.
004710
004720*----------------------------------------------------------------*
004730 974000-LER-DADOS-OPRD           SECTION.
004740*----------------------------------------------------------------*
004750     MOVE 0019                   TO MSG1-CD-SEQL-ERRO
004760
004770* Configura dados para leitura do container de requisição
004780     SET  GET-CONTAINER          TO TRUE
004790     MOVE 'PRM-INF-OPRD'         TO NM-CTNR
004800
004810* Por causa da captura automática de erro CICS, deve ser previsto
004820* o erro de container
004830     SET  ERRO-CONTAINER         TO TRUE
004840
004850     MOVE SPACES                 TO PRM-INF-OPRD
004860     EXEC CICS GET
004870          CONTAINER ( NM-CTNR                )
004880          INTO      ( PRM-INF-OPRD           )
004890          FLENGTH   ( LENGTH OF PRM-INF-OPRD )
004900     END-EXEC.
004910
004920* Ao chegar aqui, há certeza de que não houve erro algum
004930     SET  SEM-ERRO               TO TRUE
004940     .
004950 974000-SAI.
004960     EXIT.
004970
004980*----------------------------------------------------------------*
004990 975000-LER-CONTAINER-NODATA     SECTION.
005000*----------------------------------------------------------------*
005010     MOVE 0018                   TO MSG1-CD-SEQL-ERRO
005020
005030* Configura dados para leitura do container de requisição
005040     SET  GET-CONTAINER          TO TRUE
005050
005060* Por causa da captura automática de erro CICS, deve ser previsto
005070* o erro de container
005080     SET  ERRO-CONTAINER         TO TRUE
005090
005100* Obtem a quantidade de caracteres no container
005110     MOVE ZEROS                  TO QT-CCT-LIDO
005120     EXEC CICS GET
005130               CONTAINER ( NM-CTNR     )
005140               NODATA
005150               FLENGTH   ( QT-CCT-LIDO )
005160     END-EXEC
005170
005180* Ao chegar aqui, há certeza de que não houve erro algum
005190     SET  SEM-ERRO               TO TRUE
005200     .
005210 975000-SAI.
005220     EXIT.
005230
005240*----------------------------------------------------------------*
005250*                                                                *
005260* SECTION DA FAIXA DE 980000 A 989900 PARA GRAVACAO DE DADOS     *
005270*                                                                *
005280*----------------------------------------------------------------*
005290 980000-GRAVAR-CONTAINER-RPST    SECTION.
005300*----------------------------------------------------------------*
005310
005320* Exclui eventual container de erro de outros programas
005330     PERFORM 983100-EXCLUIR-CONTAINER-ERRO
005340
005350     MOVE 0020                   TO MSG1-CD-SEQL-ERRO
005360
005370* Monta o nome do container de resposta
005380     MOVE 'SGTS0041-RPST'        TO NM-CTNR
005390     SET PUT-CONTAINER           TO TRUE
005400
005410* Por causa da captura automática de erro CICS, deve ser previsto
005420* o erro de container
005430     SET  ERRO-CONTAINER         TO TRUE
005440
005450     EXEC CICS PUT
005460               CONTAINER ( NM-CTNR                 )
005470               FROM      ( SGTS0041-RPST           )
005480               FLENGTH   ( LENGTH OF SGTS0041-RPST )
005490     END-EXEC
005500
005510* Ao chegar aqui, há certeza de que não houve erro algum
005520     SET  SEM-ERRO               TO TRUE
005530     .
005540 980000-SAI.
005550     EXIT.
005560
005570*---------------------------------------------------------------*
005580 983000-EXCLUIR-RESPOSTA-ERRO    SECTION.
005590*----------------------------------------------------------------*
005600* Exclui o container de resposta e de erro para evitar problemas
005610* em modulos executados mais de uma vez dentro da mesma sessao do
005620* CICS. Equivale ao initialize na area de resposta dos programas
005630* que utilizam COMMAREA.
005640*----------------------------------------------------------------*
005650     MOVE 0023                   TO MSG1-CD-SEQL-ERRO
005660
005670* Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
005680* (nome_modulo)-RPST
005690     EVALUATE NM-PGM
005700         WHEN 'SGTS0041'
005710             MOVE NM-PGM         TO NM-CTNR(1:8)
005720             MOVE '-RPST'        TO NM-CTNR(9:5)
005730         WHEN OTHER
005740             MOVE 'PRM-DADOS-RPST'
005750                                 TO NM-CTNR
005760     END-EVALUATE
005770
005780     SET DEL-CONTAINER           TO TRUE
005790
005800* Exclui o container de resposta
005810* Não há captura automática de erro CICS por usar RESP e RESP2
005820     EXEC CICS DELETE
005830               CONTAINER ( NM-CTNR    )
005840               RESP      ( MSG1-RESP1 )
005850               RESP2     ( MSG1-RESP2 )
005860     END-EXEC
005870
005880     EVALUATE TRUE
005890         WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
005900         WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
005910             CONTINUE
005920         WHEN OTHER
005930             SET ERRO-CONTAINER  TO TRUE
005940             PERFORM 999002-TRATA-CONDITION-CICS
005950     END-EVALUATE
005960
005970* Exclui o container de erro
005980     PERFORM 983100-EXCLUIR-CONTAINER-ERRO
005990     .
006000 983000-SAI.
006010     EXIT.
006020*
006030*----------------------------------------------------------------*
006040 983100-EXCLUIR-CONTAINER-ERRO   SECTION.
006050*----------------------------------------------------------------*
006060* Limpar variaveis container de erro
006070     MOVE SPACES                 TO IIBKERRO-PRM-ERRO
006080     INITIALIZE IIBKERRO-PRM-ERRO
006090         REPLACING NUMERIC       BY  ZEROS
006100
006110* Exclui o container de erro
006120     MOVE 0024                   TO MSG1-CD-SEQL-ERRO
006130     MOVE 'PRM-ERRO'             TO NM-CTNR
006140     SET DEL-CONTAINER           TO TRUE
006150
006160* Não há captura automática de erro CICS por usar RESP e RESP2
006170     EXEC CICS DELETE
006180               CONTAINER ( NM-CTNR    )
006190               RESP      ( MSG1-RESP1 )
006200               RESP2     ( MSG1-RESP2 )
006210     END-EXEC
006220
006230     EVALUATE TRUE
006240         WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
006250         WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
006260             CONTINUE
006270         WHEN OTHER
006280             SET ERRO-CONTAINER  TO TRUE
006290             PERFORM 999002-TRATA-CONDITION-CICS
006300     END-EVALUATE
006310     .
006320 983100-SAI.
006330     EXIT.
006340
006350*----------------------------------------------------------------*
006360 984000-GRAVAR-CONTAINER-ERRO    SECTION.
006370*----------------------------------------------------------------*
006380     MOVE 0025                   TO MSG1-CD-SEQL-ERRO
006390
006400* Grava o erro gerado no container de erro
006410* Não há captura automática de erro CICS por usar RESP e RESP2
006420     EXEC CICS PUT
006430               CONTAINER ( 'PRM-ERRO'                  )
006440               FROM      ( IIBKERRO-PRM-ERRO           )
006450               FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
006460               RESP      ( MSG1-RESP1                  )
006470               RESP2     ( MSG1-RESP2                  )
006480     END-EXEC
006490
006500     .
006510 984000-SAI.
006520     EXIT.
006530
006540*----------------------------------------------------------------*
006550 990000-ENCERRAR                 SECTION.
006560*----------------------------------------------------------------*
006570
006580     EXEC CICS
006590          RETURN
006600     END-EXEC
006610     .
006620 990000-ENCERRAR-SAI.
006630     EXIT.
006640*
006650*----------------------------------------------------------------*
006660*                                                                *
006670* As sections abaixo iniciadas com 999000 representam os tratamen*
006680* tos de erro utilizando o Sistema MSG (rotina MSGS0001)         *
006690*                                                                *
006700*----------------------------------------------------------------*
006710 999000-ERRO                     SECTION.
006720*----------------------------------------------------------------*
006730
006740     IF  MSG1-CD-ERRO-TS EQUAL ZEROS
006750         MOVE 8888                TO MSG1-CD-ERRO-TS
006760     END-IF
006770     IF  MSG1-TX-ERRO-TS EQUAL SPACES
006780         MOVE 'Erro não especificado no módulo,'
006790                                 TO MSG1-TX-ERRO-TS
006800     END-IF
006810     PERFORM 999000-GRAVA-ERRO-E-ENCERRA
006820     .
006830 999000-ERRO-SAI.
006840     EXIT.
006850
006860*----------------------------------------------------------------*
006870*    As sections abaixo representam os tratamentos de erro
006880*    utilizando o sistema MSG de mensageria (rotina MSGS0001)
006890*-----------------------------------------------------------------
006900 999000-GRAVA-ERRO-E-ENCERRA     SECTION.
006910*-----------------------------------------------------------------
006920
006930* Em caso de erro, desfaz as atualizaçoes em banco de dados
006940     EXEC CICS SYNCPOINT
006950               ROLLBACK
006960     END-EXEC
006970
006980* Verifica se o tipo de erro gerado possui tratamento interno
006990     EVALUATE TRUE
007000         WHEN ERRO-CONTAINER
007010             PERFORM 999004-TRATA-ERRO-CTNR
007020         WHEN ERRO-TAMANHO-AREA
007030             PERFORM 999005-TRATA-ERRO-AREA-DADOS
007040         WHEN ERRO-CANAL
007050             PERFORM 999006-TRATA-ERRO-CANAL
007060         WHEN OTHER
007070             CONTINUE
007080     END-EVALUATE
007090
007100* Adiciona as variáveis de entrada para monitoração
007110     IF  MNT-VRV
007120         MOVE 'SGTS0041-CD-TIP-CRCT-OPR'
007130                                 TO MSGKW999-NM-VRV-MNT
007140         MOVE SGTS0041-CD-TIP-CRCT-OPR
007150                                 TO MSGKW999-VL-VRV-MNT
007160         PERFORM 999003-ADICIONA-VRV-MNT
007170
007180         MOVE 'SGTS0041-NR-ETRD-CRCT-OPR'
007190                                 TO MSGKW999-NM-VRV-MNT
007200         MOVE SGTS0041-NR-ETRD-CRCT-OPR
007210                                 TO MSGKW999-VL-VRV-MNT
007220         PERFORM 999003-ADICIONA-VRV-MNT
007230
007240         MOVE 'SGTS0041-IN-ACTE'
007250                                 TO MSGKW999-NM-VRV-MNT
007260         MOVE SGTS0041-IN-ACTE
007270                                 TO MSGKW999-VL-VRV-MNT
007280         PERFORM 999003-ADICIONA-VRV-MNT
007290
007300         MOVE 'SGTS0041-TX-OBS-EST-CRCT'
007310                                 TO MSGKW999-NM-VRV-MNT
007320         MOVE SGTS0041-TX-OBS-EST-CRCT
007330                                 TO MSGKW999-VL-VRV-MNT
007340         PERFORM 999003-ADICIONA-VRV-MNT
007350
007360         MOVE 'SGTS0041-TS-ULT-EST-CRCT-OPR'
007370                                 TO MSGKW999-NM-VRV-MNT
007380         MOVE SGTS0041-TS-ULT-EST-CRCT-OPR
007390                                 TO MSGKW999-VL-VRV-MNT
007400         PERFORM 999003-ADICIONA-VRV-MNT
007410     END-IF
007420
007430* Grava o erro gerado no sistema MSG - Mensageria
007440     PERFORM MSG1-NTF-MSG
007450
007460* Movimenta Mensagem e Código de Erro para o Container de Erro
007470     MOVE MSG1-CD-ERRO-TS        TO IIBKERRO-CD-ERRO
007480     MOVE MSG1-TX-ERRO-TS        TO IIBKERRO-TX-ERRO
007490
007500* Grava o erro gerado no container de erro
007510     PERFORM 984000-GRAVAR-CONTAINER-ERRO
007520
007530* Encerra a transacao
007540     PERFORM 990000-ENCERRAR
007550     .
007560 999000-SAI.
007570     EXIT.
007580* --------------------------------------*
007590* 999001-TRATA-ABEND-CICS        SECTION.
007600* 999002-TRATA-CONDITION-CICS    SECTION.
007610* 999003-ADICIONA-VRV-MNT        SECTION.
007620* --------------------------------------*
=INC MSGKP999                                                           0000763%
007640* --------------------------------------*
007650* 999004-TRATA-ERRO-CTNR         SECTION.
007660* 999005-TRATA-ERRO-AREA-DADOS   SECTION.
007670* 999006-TRATA-ERRO-CANAL        SECTION.
007680*---------------------------------------*
=INC SGTKP999                                                           0000769%
007700* --------------------------------------*
007710* MSG1-NTF-MSG                   SECTION.
007720* --------------------------------------*
=INC MSGKPMG1                                                           0000773%
007740*---------------------- FIM SGTS0041 -----------------------------