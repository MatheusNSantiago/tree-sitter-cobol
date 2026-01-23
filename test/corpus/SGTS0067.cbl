000010*-----------------------------------------------------------------
000020* PROGRAMA..: SGTS0067
000030* ANALISTA..: F8785328 - Roselaine Ogrizek
000040* AUTOR.....: F2054109 - Cintia Ap. Soares Iglesias
000050* DATA......: 07.12.2015
000060* OBJETIVO..: Confirmar Tipo de Negociação
000070* COMPILACAO: COBOL/CICS
000080*-----------------------------------------------------------------
000090* VRS002 16/04/2019 F9188847 DE 2016/145195 - Inclusão do campo:
      *              SGTS0067-CD-CTRA-TVM, Exclusão área rpst e Acerto
      *              Mensageria.
000090* VRS001 07.12.2015 F8785328  DE 2013/120821 - Implantação
000100*-----------------------------------------------------------------
000110
000120*-----------------------------------------------------------------
000130 IDENTIFICATION DIVISION.
000140*----------------------------------------------------------------*
000150 PROGRAM-ID. SGTS0067.
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
000390              '*** SGTS0067 ***'.
000400 77  CTE-VERS                    PIC  X(006)  VALUE 'VRS002'.
000410
000420* Módulos acionados
000430 77  SGTST048                    PIC  X(008) VALUE 'SGTST048'.
000440*----------------------------------------------------------------*
000450 LOCAL-STORAGE                   SECTION.
000460*----------------------------------------------------------------*
000470* Areas de trabalho
000480
000490 01  OPCAO                       PIC S9(004) COMP-5 VALUE 0.
000500
      *--> Variáveis de trabalho
       01  CD-TIP-EST                  PIC 9(10) VALUE ZEROS.
000510*----------------------------------------------------------------*
000520* Book dos Containers e das subrotinas chamadas
000530*----------------------------------------------------------------*
000540
000550* Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1                                                           0000056%
000570
000580* Book para captura de abend cics da bloco codigo MSGKP999
-INC HLPKW999                                                           0000059%
000600
000610* Book para captura de erro via MSG1
-INC MSGKWMG1                                                           0000062%
000630
000640* Book do container de requisição e resposta deste modulo
000650 01  SGTS0067-DADOS.
-INC SGTK0067                                                           0000066%
000670
000680* CRUD DA TABELA  EST_TIP_NEGO
000690 01  SGTST048-DADOS.
-INC SGTKT048                                                           0000070%
000710
000720* Book do Container de Erro
-INC IIBKERRO                                                           0000073%
000740
000750* Book dos dados do usuário
-INC IIBKOPRD                                                           0000076%
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

           GOBACK
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
-INC MSGKE999                                                           0000106%
001070
001080     PERFORM 970000-LER-CANAL
001090
001100* Define o nome do programa p/ fins manipulacao container
001110     MOVE 'SGTS0067'             TO NM-PGM
001120     MOVE 'SGTS0067-RPST'        TO NM-CTNR
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

           IF  SGTS0067-CD-CTRA-TVM     LESS ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0001              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' inválido.'      TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF
001340
001350     IF  SGTS0067-CD-TIP-NEGO = ZEROS
001360         SET ERRO-PROGRAMADO     TO TRUE
001370         MOVE 0953               TO MSG1-CD-ERRO-TS
001380         MOVE 'Código tipo negociação não informado.'
001390                                 TO MSG1-TX-ERRO-TS
001410         PERFORM 999000-ERRO
001420     END-IF

001350     IF  SGTS0067-CD-TIP-NEGO < ZEROS
001360         SET ERRO-PROGRAMADO     TO TRUE
001370         MOVE 0714               TO MSG1-CD-ERRO-TS
001380         MOVE 'Código tipo negociação inválido.'
001390                                 TO MSG1-TX-ERRO-TS
001410         PERFORM 999000-ERRO
001420     END-IF
001430
001440     IF  NOT SGTS0067-IN-ACTE-S
001450     AND NOT SGTS0067-IN-ACTE-N
001460         SET ERRO-PROGRAMADO     TO TRUE
001470         MOVE 0533               TO MSG1-CD-ERRO-TS
001480         MOVE 'Indicador aceite inválido.'
001490                                 TO MSG1-TX-ERRO-TS
001510         PERFORM 999000-ERRO
001520     END-IF
001530
001540     IF  SGTS0067-TX-OBS-EST-TIP  = SPACES
001550         IF  SGTS0067-IN-ACTE-N
001560             SET ERRO-PROGRAMADO TO TRUE
001570             MOVE 0887           TO MSG1-CD-ERRO-TS
001580             MOVE 'Motivo de rejeição não informado.'
001590                                 TO MSG1-TX-ERRO-TS
001610             PERFORM 999000-ERRO
001620         END-IF
001630     END-IF
001640
001650     IF  SGTS0067-TS-ULT-EST-TIP-NEGO   = SPACES
001660         SET ERRO-PROGRAMADO     TO TRUE
001670         MOVE 0715               TO MSG1-CD-ERRO-TS
001680         MOVE 'Timestamp estado tipo negociação inválido.'
001690                                 TO MSG1-TX-ERRO-TS
001710         PERFORM 999000-ERRO
001720     END-IF
001730     .
001740 110000-SAI.
001750     EXIT.
001760
001770*----------------------------------------------------------------*
001780 200000-PROCESSAR                SECTION.
001790*----------------------------------------------------------------*
001800     MOVE 0003                   TO MSG1-CD-SEQL-ERRO
001810
001820* Valida estado atual do tipo de negociação
001830     PERFORM 210000-VALIDAR-EST-ATUAL
001840
001850* Inclui Validação do Tipo de Negociação
001860     MOVE SPACES                 TO SGTST048-DADOS
001870     INITIALIZE  SGTST048-DADOS
001880         REPLACING NUMERIC       BY  ZEROS
001890
001900* Opção 1 - Incluir estado tipo negociação
001910     MOVE 1                      TO OPCAO
001920     MOVE 'CURRENT                   '
001930                                 TO SGTST048-TS-EST-TIP-NEGO
001940     MOVE KOPRD-CD-OPRD          TO SGTST048-CD-USU-ATL-EST
001950     MOVE SGTS0067-TX-OBS-EST-TIP
001960                                 TO SGTST048-TX-OBS-EST-TIP
001970* Estado 2 = Aprovado
001980* Estado 3 = Rejeitado
001990     IF  SGTS0067-IN-ACTE-S
002000         MOVE 2                  TO SGTST048-CD-TIP-EST-DCS
002010     ELSE
002020         MOVE 3                  TO SGTST048-CD-TIP-EST-DCS
002030     END-IF
002040
002050     PERFORM 920000-CHAMAR-SGTST048
002060
002070     PERFORM 969000-MONITORAR-VARIAVEIS
002080
002090*    PERFORM 899000-MOVIMENTAR-SAIDA
002100     .
002110 200000-SAI.
002120     EXIT.
002130
002140*----------------------------------------------------------------*
002150 210000-VALIDAR-EST-ATUAL        SECTION.
002160*----------------------------------------------------------------*
002170     MOVE 0004                   TO MSG1-CD-SEQL-ERRO
002180
002190     MOVE SPACES                 TO SGTST048-DADOS
002200     INITIALIZE  SGTST048-DADOS
002210         REPLACING NUMERIC       BY  ZEROS
002220
002230* Opção 5 - Consultar estado atual
002240     MOVE 5                      TO OPCAO
002250
002260     PERFORM 920000-CHAMAR-SGTST048
002270
002280* Se estado <>1 (pendente de confirmação), Tipo de negociação
002290* não pode ser validado
002810     IF  SGTST048-CD-TIP-EST-DCS NOT EQUAL 1
002820         SET ERRO-PROGRAMADO     TO TRUE
002830         MOVE 0550               TO MSG1-CD-ERRO-TS
               MOVE SGTST048-CD-TIP-EST-DCS
                                       TO CD-TIP-EST
002840         STRING 'Estado atual  '
                       CD-TIP-EST
                      ' não permite validação.'
                        DELIMITED BY SIZE
                        INTO MSG1-TX-ERRO-TS
               END-STRING
002870         PERFORM 999000-ERRO
002880     END-IF
002380
002390     IF  SGTST048-TS-EST-TIP-NEGO  NOT =
002400                                 SGTS0067-TS-ULT-EST-TIP-NEGO
002410         SET ERRO-PROGRAMADO     TO TRUE
002420         MOVE 0548               TO MSG1-CD-ERRO-TS
002430         MOVE 'Dados sofreram edição. Favor recarregar a tela.'
002440                                 TO MSG1-TX-ERRO-TS
002460         PERFORM 999000-ERRO
002470     END-IF
002480
002490     .
002500 210000-SAI.
002510     EXIT.
002520
002530*----------------------------------------------------------------*
002540*899000-MOVIMENTAR-SAIDA         SECTION.
002550*----------------------------------------------------------------*
002560*    MOVE 0005                   TO MSG1-CD-SEQL-ERRO
002570*
002580*    MOVE 'Confirmação realizada com sucesso.'
002590*                                TO SGTS0067-TX-MSG-RTN-PRCT
002600*
002610* Grava o container de resposta da subrotina
002620*    PERFORM 980000-GRAVAR-CONTAINER-RPST
002630*    .
002640*899000-SAI.
002650*    EXIT.
002660
002670*----------------------------------------------------------------*
002680*                                                                *
002690*  SECTION DA FAIXA DE 900000 A 969900 PARA DEMAIS ROTINAS       *
002700*                                                                *
002710*----------------------------------------------------------------*
002720*----------------------------------------------------------------*
002730 920000-CHAMAR-SGTST048          SECTION.
002740*----------------------------------------------------------------*
002750     MOVE 0006                   TO MSG1-CD-SEQL-ERRO
002760
002770     MOVE KOPRD-CD-OPRD          TO SGTST048-CD-CHV-USU
002780     MOVE OPCAO                  TO SGTST048-ACAO
           MOVE SGTS0067-CD-CTRA-TVM   TO SGTST048-E-CD-CTRA-TIT-VL
002790     MOVE SGTS0067-CD-TIP-NEGO   TO SGTST048-CD-TIP-NEGO
002810
002820* Mover nome do módulo para monitoração de variáveis
002830     MOVE SGTST048               TO NM-PGM
002840
002850     EXEC CICS LINK
002860          PROGRAM  (SGTST048)
002870          COMMAREA (SGTST048-DADOS)
002880     END-EXEC
002890
002900     IF  SGTST048-CD-RTN-PGM     NOT = ZEROS
002910             SET ERRO-PROGRAMA-ACIONADO TO TRUE
002930             MOVE SGTST048-MSG-ERRO-PGM
002940                                 TO MSG1-TX-RTN-PGM-ACND
002950                                    MSG1-TX-ERRO-TS
002960             MOVE NM-PGM         TO MSG1-NM-PGM-ACND
002970             MOVE SGTST048-CD-RTN-PGM
002980                                 TO MSG1-CD-RTN-PGM-ACND
                                          MSG1-CD-ERRO-TS
002990* Monitora variáveis do módulo chamado
003000             PERFORM 969000-MONITORAR-VARIAVEIS
003010             PERFORM 999000-ERRO
003020     END-IF
003030     .
003040 920000-SAI.
003050     EXIT.
003060
003070*----------------------------------------------------------------*
003080 969000-MONITORAR-VARIAVEIS      SECTION.
003090*----------------------------------------------------------------*
003100* Monitora as variáveis dos módulos chamados(Mensageria IIB)
003110*----------------------------------------------------------------*
003130
003140     EVALUATE NM-PGM
003150         WHEN SGTST048
003160             MOVE 'SGTST048-ACAO '
003170                                 TO MSGKW999-NM-VRV-MNT
003180             MOVE SGTST048-ACAO
003190                                 TO MSGKW999-VL-VRV-MNT
003200             PERFORM 999003-ADICIONA-VRV-MNT
003210
                   MOVE 'SGTST048-E-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                   MOVE  SGTST048-E-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                   PERFORM  999003-ADICIONA-VRV-MNT

003220
003230             MOVE 'SGTST048-CD-TIP-NEGO'
003240                                 TO MSGKW999-NM-VRV-MNT
003250             MOVE SGTST048-CD-TIP-NEGO
003260                                 TO MSGKW999-VL-VRV-MNT
003270             PERFORM 999003-ADICIONA-VRV-MNT
003280
003290
003300             MOVE 'SGTST048-TS-EST-TIP-NEGO'
003310                                 TO MSGKW999-NM-VRV-MNT
003320             MOVE SGTST048-TS-EST-TIP-NEGO
003330                                 TO MSGKW999-VL-VRV-MNT
003340             PERFORM 999003-ADICIONA-VRV-MNT
003350
003360             MOVE 'SGTST048-CD-TIP-EST-DCS'
003370                                 TO MSGKW999-NM-VRV-MNT
003380             MOVE SGTST048-CD-TIP-EST-DCS
003390                                 TO MSGKW999-VL-VRV-MNT
003400             PERFORM 999003-ADICIONA-VRV-MNT
003410
003420         WHEN OTHER
003430             CONTINUE
003440     END-EVALUATE
003450     .
003460 969000-SAI.
003470     EXIT.
003480
003490*----------------------------------------------------------------*
003500*                                                                *
003510* SECTION DA FAIXA DE 970000 A 979900 PARA LEITURA DE DADOS      *
003520*                                                                *
003530*----------------------------------------------------------------*
003540 970000-LER-CANAL                SECTION.
003550*----------------------------------------------------------------*
003560* Obtem o canal corrente
003570*----------------------------------------------------------------*
003580     MOVE 0008                   TO MSG1-CD-SEQL-ERRO
003590
003600* Por causa da captura automática de erro CICS, deve ser previsto
003610* o erro de container
003620     SET  ERRO-CANAL             TO TRUE
003630
003640     EXEC CICS ASSIGN
003650               CHANNEL ( NM-CNL )
003660     END-EXEC
003670
003680* Ao chegar aqui, há certeza de que não houve erro algum
003690     SET  SEM-ERRO               TO TRUE
003700     .
003710 970000-SAI.
003720     EXIT.
003730
003740*----------------------------------------------------------------*
003750 971000-LER-REQUISICAO           SECTION.
003760*----------------------------------------------------------------*
003770     MOVE 0009                   TO MSG1-CD-SEQL-ERRO
003780
003790* Formata o nome do container de requisicao
003800     MOVE 'SGTS0067'             TO NM-PGM
003810     MOVE 'SGTS0067-RQSC'        TO NM-CTNR
003820
003830* Recupera o tamanho do container de requisição
003840     PERFORM 975000-LER-CONTAINER-NODATA
003850
003860* Obtem o tamanho esperado da requisicao
003870     MOVE LENGTH OF SGTS0067-RQSC TO QT-CCT-EPRO
003880
003890* Valida tamanho esperado da requisicao X tamanho recebido
003900     IF  QT-CCT-LIDO             NOT = QT-CCT-EPRO
003910         SET ERRO-TAMANHO-AREA   TO TRUE
003920         PERFORM 999000-GRAVA-ERRO-E-ENCERRA
003930     END-IF
003940
003950     MOVE 0010                   TO MSG1-CD-SEQL-ERRO
003960
003970* Configura dados para leitura do container de requisição
003980     SET  GET-CONTAINER          TO TRUE
003990
004000* Por causa da captura automática de erro CICS, deve ser previsto
004010* o erro de container
004020     SET  ERRO-CONTAINER         TO TRUE
004030
004040* Efetua a leitura do container
004050     EXEC CICS GET
004060               CONTAINER ( NM-CTNR       )
004070               FLENGTH   ( QT-CCT-LIDO   )
004080               INTO      ( SGTS0067-RQSC )
004090     END-EXEC
004100
004110* Ao chegar aqui, há certeza de que não houve erro algum
004120     SET  SEM-ERRO               TO TRUE
004130     .
004140 971000-SAI.
004150     EXIT.
004160
004170*----------------------------------------------------------------*
004180 974000-LER-DADOS-OPRD           SECTION.
004190*----------------------------------------------------------------*
004200     MOVE 0011                   TO MSG1-CD-SEQL-ERRO
004210
004220* Configura dados para leitura do container de requisição
004230     SET  GET-CONTAINER          TO TRUE
004240     MOVE 'PRM-INF-OPRD'         TO NM-CTNR
004250
004260* Por causa da captura automática de erro CICS, deve ser previsto
004270* o erro de container
004280     SET  ERRO-CONTAINER         TO TRUE
004290
004300     MOVE SPACES                 TO PRM-INF-OPRD
004310     EXEC CICS GET
004320          CONTAINER ( NM-CTNR                )
004330          INTO      ( PRM-INF-OPRD           )
004340          FLENGTH   ( LENGTH OF PRM-INF-OPRD )
004350     END-EXEC.
004360
004370* Ao chegar aqui, há certeza de que não houve erro algum
004380     SET  SEM-ERRO               TO TRUE
004390     .
004400 974000-SAI.
004410     EXIT.
004420
004430*----------------------------------------------------------------*
004440 975000-LER-CONTAINER-NODATA     SECTION.
004450*----------------------------------------------------------------*
004470
004480* Configura dados para leitura do container de requisição
004490     SET  GET-CONTAINER          TO TRUE
004500
004510* Por causa da captura automática de erro CICS, deve ser previsto
004520* o erro de container
004530     SET  ERRO-CONTAINER         TO TRUE
004540
004550* Obtem a quantidade de caracteres no container
004560     MOVE ZEROS                  TO QT-CCT-LIDO
004570     EXEC CICS GET
004580               CONTAINER ( NM-CTNR     )
004590               NODATA
004600               FLENGTH   ( QT-CCT-LIDO )
004610     END-EXEC
004620
004630* Ao chegar aqui, há certeza de que não houve erro algum
004640     SET  SEM-ERRO               TO TRUE
004650     .
004660 975000-SAI.
004670     EXIT.
004680
004690*----------------------------------------------------------------*
004700*                                                                *
004710* SECTION DA FAIXA DE 980000 A 989900 PARA GRAVACAO DE DADOS     *
004720*                                                                *
004730*----------------------------------------------------------------*
004740*980000-GRAVAR-CONTAINER-RPST    SECTION.
004750*----------------------------------------------------------------*
004760*
004770* Exclui eventual container de erro de outros programas
004780*    PERFORM 983100-EXCLUIR-CONTAINER-ERRO
004790*
004800*    MOVE 0013                   TO MSG1-CD-SEQL-ERRO
004810*
004820* Monta o nome do container de resposta
004830*    MOVE 'SGTS0067-RPST'        TO NM-CTNR
004840*    SET PUT-CONTAINER           TO TRUE
004850*
004860* Por causa da captura automática de erro CICS, deve ser previsto
004870* o erro de container
004880*    SET  ERRO-CONTAINER         TO TRUE
004890*
004900*    EXEC CICS PUT
004910*              CONTAINER ( NM-CTNR                 )
004920*              FROM      ( SGTS0067-RPST           )
004930*              FLENGTH   ( LENGTH OF SGTS0067-RPST )
004940*    END-EXEC
004950*
004960* Ao chegar aqui, há certeza de que não houve erro algum
004970*    SET  SEM-ERRO               TO TRUE
004980*    .
004990*980000-SAI.
005000*    EXIT.
005010*
005020*---------------------------------------------------------------*
005030 983000-EXCLUIR-RESPOSTA-ERRO    SECTION.
005040*----------------------------------------------------------------*
005050* Exclui o container de resposta e de erro para evitar problemas
005060* em modulos executados mais de uma vez dentro da mesma sessao do
005070* CICS. Equivale ao initialize na area de resposta dos programas
005080* que utilizam COMMAREA.
005090*----------------------------------------------------------------*
005100     MOVE 0014                   TO MSG1-CD-SEQL-ERRO
005110
005120* Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
005130* (nome_modulo)-RPST
005140     EVALUATE NM-PGM
005150         WHEN 'SGTS0067'
005160             MOVE NM-PGM         TO NM-CTNR(1:8)
005170             MOVE '-RPST'        TO NM-CTNR(9:5)
005180         WHEN OTHER
005190             MOVE 'PRM-DADOS-RPST'
005200                                 TO NM-CTNR
005210     END-EVALUATE
005220
005230     SET DEL-CONTAINER           TO TRUE
005240
005250* Exclui o container de resposta
005260* Não há captura automática de erro CICS por usar RESP e RESP2
005270     EXEC CICS DELETE
005280               CONTAINER ( NM-CTNR    )
005290               RESP      ( MSG1-RESP1 )
005300               RESP2     ( MSG1-RESP2 )
005310     END-EXEC
005320
005330     EVALUATE TRUE
005340         WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
005350         WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
005360             CONTINUE
005370         WHEN OTHER
005380             SET ERRO-CONTAINER  TO TRUE
005390             PERFORM 999002-TRATA-CONDITION-CICS
005400     END-EVALUATE
005410
005420* Exclui o container de erro
005430     PERFORM 983100-EXCLUIR-CONTAINER-ERRO
005440     .
005450 983000-SAI.
005460     EXIT.
005470*
005480*----------------------------------------------------------------*
005490 983100-EXCLUIR-CONTAINER-ERRO   SECTION.
005500*----------------------------------------------------------------*
005510* Limpar variaveis container de erro
005520     MOVE SPACES                 TO IIBKERRO-PRM-ERRO
005530     INITIALIZE IIBKERRO-PRM-ERRO
005540         REPLACING NUMERIC       BY  ZEROS
005550
005560* Exclui o container de erro
005580     MOVE 'PRM-ERRO'             TO NM-CTNR
005590     SET DEL-CONTAINER           TO TRUE
005600
005610* Não há captura automática de erro CICS por usar RESP e RESP2
005620     EXEC CICS DELETE
005630               CONTAINER ( NM-CTNR    )
005640               RESP      ( MSG1-RESP1 )
005650               RESP2     ( MSG1-RESP2 )
005660     END-EXEC
005670
005680     EVALUATE TRUE
005690         WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
005700         WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
005710             CONTINUE
005720         WHEN OTHER
005730             SET ERRO-CONTAINER  TO TRUE
005740             PERFORM 999002-TRATA-CONDITION-CICS
005750     END-EVALUATE
005760     .
005770 983100-SAI.
005780     EXIT.
005790
005800*----------------------------------------------------------------*
005810 984000-GRAVAR-CONTAINER-ERRO    SECTION.
005820*----------------------------------------------------------------*
005840
005850* Grava o erro gerado no container de erro
005860* Não há captura automática de erro CICS por usar RESP e RESP2
005870     EXEC CICS PUT
005880               CONTAINER ( 'PRM-ERRO'                  )
005890               FROM      ( IIBKERRO-PRM-ERRO           )
005900               FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
005910               RESP      ( MSG1-RESP1                  )
005920               RESP2     ( MSG1-RESP2                  )
005930     END-EXEC
005940
005950     .
005960 984000-SAI.
005970     EXIT.
005980
005990*----------------------------------------------------------------*
006000 990000-ENCERRAR                 SECTION.
006010*----------------------------------------------------------------*
006020
006030     EXEC CICS
006040          RETURN
006050     END-EXEC
006060     .
006070 990000-ENCERRAR-SAI.
006080     EXIT.
006090*
006100*----------------------------------------------------------------*
006110*                                                                *
006120* As sections abaixo iniciadas com 999000 representam os tratamen*
006130* tos de erro utilizando o Sistema MSG (rotina MSGS0001)         *
006140*                                                                *
006150*----------------------------------------------------------------*
006160 999000-ERRO                     SECTION.
006170*----------------------------------------------------------------*
006180
006190     IF  MSG1-CD-ERRO-TS EQUAL ZEROS
006200         MOVE 8888                TO MSG1-CD-ERRO-TS
006210     END-IF
006220     IF  MSG1-TX-ERRO-TS EQUAL SPACES
006230         MOVE 'Erro não especificado no módulo,'
006240                                 TO MSG1-TX-ERRO-TS
006250     END-IF
006260     PERFORM 999000-GRAVA-ERRO-E-ENCERRA
006270     .
006280 999000-ERRO-SAI.
006290     EXIT.
006300
006310*----------------------------------------------------------------*
006320*    As sections abaixo representam os tratamentos de erro
006330*    utilizando o sistema MSG de mensageria (rotina MSGS0001)
006340*-----------------------------------------------------------------
006350 999000-GRAVA-ERRO-E-ENCERRA     SECTION.
006360*-----------------------------------------------------------------
006370
006380* Em caso de erro, desfaz as atualizaçoes em banco de dados
006390     EXEC CICS SYNCPOINT
006400               ROLLBACK
006410     END-EXEC
006420
006430* Verifica se o tipo de erro gerado possui tratamento interno
006440     EVALUATE TRUE
006450         WHEN ERRO-CONTAINER
006460             PERFORM 999004-TRATA-ERRO-CTNR
006470         WHEN ERRO-TAMANHO-AREA
006480             PERFORM 999005-TRATA-ERRO-AREA-DADOS
006490         WHEN ERRO-CANAL
006500             PERFORM 999006-TRATA-ERRO-CANAL
006510         WHEN OTHER
006520             CONTINUE
006530     END-EVALUATE
006540
006550* Adiciona as variáveis de entrada para monitoração
006560         MOVE 'SGTS0067-CD-CTRA-TVM'
                                       TO MSGKW999-NM-VRV-MNT
004720         MOVE SGTS0067-CD-CTRA-TVM
                                       TO MSGKW999-VL-VRV-MNT
004740         PERFORM 999003-ADICIONA-VRV-MNT

006570         MOVE 'SGTS0067-CD-TIP-NEGO'
006580                                 TO MSGKW999-NM-VRV-MNT
006590         MOVE SGTS0067-CD-TIP-NEGO
006600                                 TO MSGKW999-VL-VRV-MNT
006610         PERFORM 999003-ADICIONA-VRV-MNT
006620
006630         MOVE 'SGTS0067-IN-ACTE'
006640                                 TO MSGKW999-NM-VRV-MNT
006650         MOVE SGTS0067-IN-ACTE
006660                                 TO MSGKW999-VL-VRV-MNT
006670         PERFORM 999003-ADICIONA-VRV-MNT
006680
006690         MOVE 'SGTS0067-TX-OBS-EST-TIP'
006700                                 TO MSGKW999-NM-VRV-MNT
006710         MOVE SGTS0067-TX-OBS-EST-TIP
006720                                 TO MSGKW999-VL-VRV-MNT
006730         PERFORM 999003-ADICIONA-VRV-MNT
006740
006750         MOVE 'SGTS0067-TS-ULT-EST-TIP-NEGO'
006760                                 TO MSGKW999-NM-VRV-MNT
006770         MOVE SGTS0067-TS-ULT-EST-TIP-NEGO
006780                                 TO MSGKW999-VL-VRV-MNT
006790         PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'CD-TIP-EST'  TO  MSGKW999-NM-VRV-MNT
001940         MOVE  CD-TIP-EST   TO  MSGKW999-VL-VRV-MNT
001940         PERFORM  999003-ADICIONA-VRV-MNT
006810
006820*    Grava o erro gerado no sistema MSG - Mensageria
006830     PERFORM MSG1-NTF-MSG
006840
006850     MOVE MSG1-CD-ERRO-TS        TO IIBKERRO-CD-ERRO
006860     MOVE MSG1-TX-ERRO-TS        TO IIBKERRO-TX-ERRO
006870
006880*    Grava o erro gerado no container de erro
006890     PERFORM 984000-GRAVAR-CONTAINER-ERRO
006900
006910*    Encerra a transacao
006920     PERFORM 990000-ENCERRAR
006930     .
006940 999000-SAI.
006950     EXIT.
006960* --------------------------------------*
006970* 999001-TRATA-ABEND-CICS        SECTION.
006980* 999002-TRATA-CONDITION-CICS    SECTION.
006990* 999003-ADICIONA-VRV-MNT        SECTION.
007000* --------------------------------------*
-INC HLPKP999                                                           0000701%
007020* --------------------------------------*
007030* 999004-TRATA-ERRO-CTNR         SECTION.
007040* 999005-TRATA-ERRO-AREA-DADOS   SECTION.
007050* 999006-TRATA-ERRO-CANAL        SECTION.
007060*---------------------------------------*
-INC SGTKP999                                                           0000707%
007080* --------------------------------------*
007090* MSG1-NTF-MSG                   SECTION.
007100* --------------------------------------*
-INC MSGKPMG1                                                           0000711%
007120*---------------------- FIM SGTS0067 -----------------------------