000010*-----------------------------------------------------------------
000020* PROGRAMA..: SGTS0060
000030* ANALISTA..: F9855969 - Wilson Aranda
000040* AUTOR.....: f2054109 - Cintia Ap. Soares Iglesias
000050* DATA......: 09.12.2015
000060* OBJETIVO..: Confirmar Modelo de Ordem de Negociação
000070* COMPILACAO: COBOL/CICS
000080*-----------------------------------------------------------------
000090* VRS002 28.05.2019 F9188847  DE 2016/145195 - Acerto mensageria
000090* VRS001 09.12.2015 F9855659  DE 2013/120821 - Implantação
000100*-----------------------------------------------------------------
000110
000120*-----------------------------------------------------------------
000130 IDENTIFICATION DIVISION.
000140*----------------------------------------------------------------*
000150 PROGRAM-ID. SGTS0060.
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
000310 WORKING-STORAGE                 SECTION.
000320*----------------------------------------------------------------*
000330
000340 77  CTE-INICIO-SS               PIC  X(040)  VALUE
000350              '*** S.S. COMECA AQUI ***'.
000360
000370 77  CTE-PROG                    PIC  X(018)  VALUE
000380              '*** SGTS0067 ***'.
000390 77  CTE-VERS                    PIC  X(006)  VALUE 'VRS002'.
000400
000410* Módulos acionados
000420 77  SGTST050                    PIC  X(008) VALUE 'SGTST050'.
000430*----------------------------------------------------------------*
000440 LOCAL-STORAGE                   SECTION.
000450*----------------------------------------------------------------*
000460* Areas de trabalho
000470*--> Variáveis de trabalho
       01  CD-TIP-EST                  PIC 9(10) VALUE ZEROS.
000480*----------------------------------------------------------------*
000490* Book dos Containers e das subrotinas chamadas
000500*----------------------------------------------------------------*
000510
000520* Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1                                                           0000053%
000540
000550* Book para captura de abend cics da bloco codigo MSGKP999
-INC HLPKW999                                                           0000056%
000570
000580* Book para captura de erro via MSG1
-INC MSGKWMG1                                                           0000059%
000600
000610* Book do container de requisição e resposta deste modulo
000620 01  SGTS0060-DADOS.
-INC SGTK0060                                                           0000063%
000640
000650* CRUD DA TABELA  EST_MOD_ORD_NEGO
000660 01  SGTST050-DADOS.
-INC SGTKT050                                                           0000067%
000680
000690* Book do Container de Erro
-INC IIBKERRO                                                           0000070%
000710
000720* Book dos dados do usuário
-INC IIBKOPRD                                                           0000073%
000740
000750 77  CTE-FINAL-SS                  PIC X(40)      VALUE
000760              '*** S.S. TERMINA AQUI ***'.
000770
000780*----------------------------------------------------------------*
000790 PROCEDURE DIVISION.
000800*----------------------------------------------------------------*
000810
000820*----------------------------------------------------------------*
000830 000000-ROTINA-PRINCIPAL         SECTION.
000840*----------------------------------------------------------------*
000850
000860     PERFORM 100000-INICIAR
000870
000880     PERFORM 200000-PROCESSAR
000890
000900     PERFORM 990000-ENCERRAR

           GOBACK
000910     .
000920 000000-SAI.
000930     EXIT.
000940
000950*----------------------------------------------------------------*
000960 100000-INICIAR                  SECTION.
000970*----------------------------------------------------------------*
000980     MOVE 0001                   TO MSG1-CD-SEQL-ERRO
000990
001000* Ativa a captura automatica de abend do CICS.
001010* WHENEVER CICS
-INC MSGKE999                                                           0000102%
001030
001040     PERFORM 970000-LER-CANAL
001050
001060* Define o nome do programa p/ fins manipulacao container
001070     MOVE 'SGTS0060'             TO NM-PGM
001080     MOVE 'SGTS0060-RPST'        TO NM-CTNR
001090
001100* Limpa a resposta e erro antes de executar o modulo
001110     PERFORM 983000-EXCLUIR-RESPOSTA-ERRO
001120
001130* Recupera a requisição do modulo
001140     PERFORM 971000-LER-REQUISICAO
001150
001160* Recupera usuário logado
001170     PERFORM 974000-LER-DADOS-OPRD
001180
001190     PERFORM 110000-VALIDAR-REQUISICAO
001200     .
001210 100000-SAI.
001220     EXIT.
001230
001240*----------------------------------------------------------------*
001250 110000-VALIDAR-REQUISICAO       SECTION.
001260*----------------------------------------------------------------*
001270* Valida campo a campo da requisicao
001280*----------------------------------------------------------------*
001290     MOVE 0002                   TO MSG1-CD-SEQL-ERRO
001300
001310     IF  SGTS0060-CD-MOD-ORD-NEGO  < ZEROS
001320         SET ERRO-PROGRAMADO     TO TRUE
001330         MOVE 1019               TO MSG1-CD-ERRO-TS
001340         MOVE 'Código do modelo de ordem de negociação inválido.'
001350                                 TO MSG1-TX-ERRO-TS
001370         PERFORM 999000-ERRO
001380     END-IF

           IF  SGTS0060-CD-MOD-ORD-NEGO  = ZEROS
001320         SET ERRO-PROGRAMADO     TO TRUE
001330         MOVE 0641               TO MSG1-CD-ERRO-TS
001610         MOVE 'Código do modelo de ordem de negociação não informa
      -             'do.'
                                       TO MSG1-TX-ERRO-TS
001370         PERFORM 999000-ERRO
001380     END-IF
001390
001400     IF  NOT SGTS0060-IN-ACTE-S
001410     AND NOT SGTS0060-IN-ACTE-N
001420         SET ERRO-PROGRAMADO     TO TRUE
001430         MOVE 0533               TO MSG1-CD-ERRO-TS
001440         MOVE 'Indicador aceite inválido.'
001450                                 TO MSG1-TX-ERRO-TS
001470         PERFORM 999000-ERRO
001480     END-IF
001490
001500     IF  SGTS0060-TX-OBS-EST-MOD   = SPACES
001510         IF  SGTS0060-IN-ACTE-N
001520             SET ERRO-PROGRAMADO TO TRUE
001530             MOVE 0887           TO MSG1-CD-ERRO-TS
001540             MOVE 'Motivo de rejeição não informado.'
001550                                 TO MSG1-TX-ERRO-TS
001570             PERFORM 999000-ERRO
001580         END-IF
001590     END-IF
001600
001610     IF  SGTS0060-TS-EST-MOD-ORD   = SPACES
001620         SET ERRO-PROGRAMADO     TO TRUE
001630         MOVE 0277               TO MSG1-CD-ERRO-TS
001640         MOVE 'Timestamp da ultima atualização inválido.'
001650                                 TO MSG1-TX-ERRO-TS
001670         PERFORM 999000-ERRO
001680     END-IF
001690     .
001700 110000-SAI.
001710     EXIT.
001720
001730*----------------------------------------------------------------*
001740 200000-PROCESSAR                SECTION.
001750*----------------------------------------------------------------*
001760     MOVE 0003                   TO MSG1-CD-SEQL-ERRO
001770
001780* Valida estado atual do tipo de negociação
001790     PERFORM 210000-VALIDAR-EST-ATUAL
001800
001810* Opção 1 - Incluir estado do modelo de ordem de negociação.
001820     MOVE 1                      TO SGTST050-ACAO
001830     MOVE 'CURRENT'              TO SGTST050-TS-EST-MOD-ORD
001840     MOVE KOPRD-CD-OPRD          TO SGTST050-CD-USU-ATL-EST
001850     MOVE SGTS0060-TX-OBS-EST-MOD
001860                                 TO SGTST050-TX-OBS-EST-MOD
001870     MOVE SGTS0060-CD-MOD-ORD-NEGO
001880                                 TO SGTST050-CD-MOD-ORD-NEGO
001890* Estado 2 = Aprovado
001900* Estado 3 = Rejeitado
001910     IF  SGTS0060-IN-ACTE-S
001920         MOVE 2                  TO SGTST050-CD-TIP-EST-DCS
001930     ELSE
001940         MOVE 3                  TO SGTST050-CD-TIP-EST-DCS
001950     END-IF
001960
001970     PERFORM 920000-CHAMAR-SGTST050
001980
001990     PERFORM 969000-MONITORAR-VARIAVEIS
002000
002010     PERFORM 899000-MOVIMENTAR-SAIDA
002020     .
002030 200000-SAI.
002040     EXIT.
002050
002060*----------------------------------------------------------------*
002070 210000-VALIDAR-EST-ATUAL        SECTION.
002080* Validar o Estado Atual do modelo de ordem de negociação.
002090*----------------------------------------------------------------*
002100     MOVE 0004                   TO MSG1-CD-SEQL-ERRO
002110
002120     INITIALIZE  SGTST050-DADOS
002130
002140* Opção 5-Consultar estado atual do modelo de ordem de negociação.
002150     MOVE 5                      TO SGTST050-ACAO
002160     MOVE KOPRD-CD-OPRD          TO SGTST050-CD-CHV-USU
002170     MOVE SGTS0060-CD-MOD-ORD-NEGO
002180                                 TO SGTST050-CD-MOD-ORD-NEGO
002190     MOVE SGTS0060-TS-EST-MOD-ORD
002200                                 TO SGTST050-TS-EST-MOD-ORD
002210
002220     PERFORM 920000-CHAMAR-SGTST050
002230
002240* Se estado <>1 (pendente de confirmação), Modelo de Ordem
002250* não pode ser validado
002260     IF  SGTST050-CD-TIP-EST-DCS  NOT EQUAL 1
002270         SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0550               TO MSG1-CD-ERRO-TS
               MOVE SGTST050-CD-TIP-EST-DCS
                                       TO CD-TIP-EST
002840         STRING 'Estado atual  '
                       CD-TIP-EST
                      ' não permite validação.'
                        DELIMITED BY SIZE
                        INTO MSG1-TX-ERRO-TS
               END-STRING
002320         PERFORM 999000-ERRO
002330     END-IF
002340
002350     IF  SGTST050-TS-EST-MOD-ORD  NOT EQUAL
002360                                 SGTS0060-TS-EST-MOD-ORD
002370         SET ERRO-PROGRAMADO     TO TRUE
002380         MOVE 0548               TO MSG1-CD-ERRO-TS
002390         MOVE 'Dados sofreram edição. Favor recarregar a tela.'
002400                                 TO MSG1-TX-ERRO-TS
002420         PERFORM 999000-ERRO
002430     END-IF
002440
002450     .
002460 210000-SAI.
002470     EXIT.
002480
002490*----------------------------------------------------------------*
002500 899000-MOVIMENTAR-SAIDA         SECTION.
002510*----------------------------------------------------------------*
002520     MOVE 0005                   TO MSG1-CD-SEQL-ERRO
002530
002540     MOVE 'Confirmação realizada com sucesso.'
002550                                 TO SGTS0060-TX-MSG-RTN-PRCT
002560
002570* Grava o container de resposta da subrotina
002580     PERFORM 980000-GRAVAR-CONTAINER-RPST
002590     .
002600 899000-SAI.
002610     EXIT.
002620
002630*----------------------------------------------------------------*
002640*                                                                *
002650*  SECTION DA FAIXA DE 900000 A 969900 PARA DEMAIS ROTINAS       *
002660*                                                                *
002670*----------------------------------------------------------------*
002680*----------------------------------------------------------------*
002690 920000-CHAMAR-SGTST050          SECTION.
002700*----------------------------------------------------------------*
002710     MOVE 0006                   TO MSG1-CD-SEQL-ERRO
002720
002730
002740* Mover nome do módulo para monitoração de variáveis
002750     MOVE SGTST050               TO NM-PGM
002760
002770     EXEC CICS LINK
002780          PROGRAM  (SGTST050)
002790          COMMAREA (SGTST050-DADOS)
002800     END-EXEC
002810
002820     IF  SGTST050-CD-RTN-PGM     NOT = ZEROS
002830             SET ERRO-PROGRAMA-ACIONADO TO TRUE
002850             MOVE SGTST050-MSG-ERRO-PGM
002860                                 TO MSG1-TX-RTN-PGM-ACND
002870                                    MSG1-TX-ERRO-TS
002880             MOVE NM-PGM         TO MSG1-NM-PGM-ACND
002890             MOVE SGTST050-CD-RTN-PGM
002900                                 TO MSG1-CD-RTN-PGM-ACND
                                          MSG1-CD-ERRO-TS
002910* Monitora variáveis do módulo chamado
002920             PERFORM 969000-MONITORAR-VARIAVEIS
002930             PERFORM 999000-ERRO
002940     END-IF
002950     .
002960 920000-SAI.
002970     EXIT.
002980
002990*----------------------------------------------------------------*
003000 969000-MONITORAR-VARIAVEIS      SECTION.
003010*----------------------------------------------------------------*
003020* Monitora as variáveis dos módulos chamados(Mensageria IIB)
003030*----------------------------------------------------------------*
003060     EVALUATE NM-PGM
003070         WHEN SGTST050
003080             MOVE 'SGTST050-ACAO'
003090                                 TO MSGKW999-NM-VRV-MNT
003100             MOVE SGTST050-ACAO
003110                                 TO MSGKW999-VL-VRV-MNT
003120             PERFORM 999003-ADICIONA-VRV-MNT
003130
003140             MOVE 'SGTST050-CD-MOD-ORD-NEGO'
003150                                 TO MSGKW999-NM-VRV-MNT
003160             MOVE SGTST050-CD-MOD-ORD-NEGO
003170                                 TO MSGKW999-VL-VRV-MNT
003180             PERFORM 999003-ADICIONA-VRV-MNT
003190
003200             MOVE 'SGTST050-TS-EST-MOD-ORD'
003210                                 TO MSGKW999-NM-VRV-MNT
003220             MOVE SGTST050-TS-EST-MOD-ORD
003230                                 TO MSGKW999-VL-VRV-MNT
003240             PERFORM 999003-ADICIONA-VRV-MNT
003250
003260             MOVE 'SGTST050-CD-TIP-EST-DCS'
003270                                 TO MSGKW999-NM-VRV-MNT
003280             MOVE SGTST050-CD-TIP-EST-DCS
003290                                 TO MSGKW999-VL-VRV-MNT
003300             PERFORM 999003-ADICIONA-VRV-MNT
003310
003320         WHEN OTHER
003330             CONTINUE
003340     END-EVALUATE
003350     .
003360 969000-SAI.
003370     EXIT.
003380
003390*----------------------------------------------------------------*
003400* SECTION DA FAIXA DE 970000 A 979900 PARA LEITURA DE DADOS      *
003410*----------------------------------------------------------------*
003420 970000-LER-CANAL                SECTION.
003430*----------------------------------------------------------------*
003440* Obtem o canal corrente
003450*----------------------------------------------------------------*
003460     MOVE 0007                   TO MSG1-CD-SEQL-ERRO
003470
003480* Por causa da captura automática de erro CICS, deve ser previsto
003490* o erro de container
003500     SET  ERRO-CANAL             TO TRUE
003510
003520     EXEC CICS ASSIGN
003530               CHANNEL ( NM-CNL )
003540     END-EXEC
003550
003560* Ao chegar aqui, há certeza de que não houve erro algum
003570     SET  SEM-ERRO               TO TRUE
003580     .
003590 970000-SAI.
003600     EXIT.
003610
003620*----------------------------------------------------------------*
003630 971000-LER-REQUISICAO           SECTION.
003640*----------------------------------------------------------------*
003650     MOVE 0008                   TO MSG1-CD-SEQL-ERRO
003660
003670* Formata o nome do container de requisicao
003680     MOVE 'SGTS0060'             TO NM-PGM
003690     MOVE 'SGTS0060-RQSC'        TO NM-CTNR
003700
003710* Recupera o tamanho do container de requisição
003720     PERFORM 975000-LER-CONTAINER-NODATA
003730
003740* Obtem o tamanho esperado da requisicao
003750     MOVE LENGTH OF SGTS0060-RQSC TO QT-CCT-EPRO
003760
003770* Valida tamanho esperado da requisicao X tamanho recebido
003780     IF  QT-CCT-LIDO             NOT = QT-CCT-EPRO
003790         SET ERRO-TAMANHO-AREA   TO TRUE
003800         PERFORM 999000-GRAVA-ERRO-E-ENCERRA
003810     END-IF
003820
003830     MOVE 0009                   TO MSG1-CD-SEQL-ERRO
003840
003850* Configura dados para leitura do container de requisição
003860     SET  GET-CONTAINER          TO TRUE
003870
003880* Por causa da captura automática de erro CICS, deve ser previsto
003890* o erro de container
003900     SET  ERRO-CONTAINER         TO TRUE
003910
003920* Efetua a leitura do container
003930     EXEC CICS GET
003940               CONTAINER ( NM-CTNR       )
003950               FLENGTH   ( QT-CCT-LIDO   )
003960               INTO      ( SGTS0060-RQSC )
003970     END-EXEC
003980
003990* Ao chegar aqui, há certeza de que não houve erro algum
004000     SET  SEM-ERRO               TO TRUE
004010     .
004020 971000-SAI.
004030     EXIT.
004040
004050*----------------------------------------------------------------*
004060 974000-LER-DADOS-OPRD           SECTION.
004070*----------------------------------------------------------------*
004080     MOVE 0010                   TO MSG1-CD-SEQL-ERRO
004090
004100* Configura dados para leitura do container de requisição
004110     SET  GET-CONTAINER          TO TRUE
004120     MOVE 'PRM-INF-OPRD'         TO NM-CTNR
004130
004140* Por causa da captura automática de erro CICS, deve ser previsto
004150* o erro de container
004160     SET  ERRO-CONTAINER         TO TRUE
004170
004180     MOVE SPACES                 TO PRM-INF-OPRD
004190     EXEC CICS GET
004200          CONTAINER ( NM-CTNR                )
004210          INTO      ( PRM-INF-OPRD           )
004220          FLENGTH   ( LENGTH OF PRM-INF-OPRD )
004230     END-EXEC.
004240
004250* Ao chegar aqui, há certeza de que não houve erro algum
004260     SET  SEM-ERRO               TO TRUE
004270     .
004280 974000-SAI.
004290     EXIT.
004300
004310*----------------------------------------------------------------*
004320 975000-LER-CONTAINER-NODATA     SECTION.
004330*----------------------------------------------------------------*
004350
004360* Configura dados para leitura do container de requisição
004370     SET  GET-CONTAINER          TO TRUE
004380
004390* Por causa da captura automática de erro CICS, deve ser previsto
004400* o erro de container
004410     SET  ERRO-CONTAINER         TO TRUE
004420
004430* Obtem a quantidade de caracteres no container
004440     MOVE ZEROS                  TO QT-CCT-LIDO
004450     EXEC CICS GET
004460               CONTAINER ( NM-CTNR     )
004470               NODATA
004480               FLENGTH   ( QT-CCT-LIDO )
004490     END-EXEC
004500
004510* Ao chegar aqui, há certeza de que não houve erro algum
004520     SET  SEM-ERRO               TO TRUE
004530     .
004540 975000-SAI.
004550     EXIT.
004560
004570*----------------------------------------------------------------*
004580*                                                                *
004590* SECTION DA FAIXA DE 980000 A 989900 PARA GRAVACAO DE DADOS     *
004600*                                                                *
004610*----------------------------------------------------------------*
004620 980000-GRAVAR-CONTAINER-RPST    SECTION.
004630*----------------------------------------------------------------*
004640
004650* Exclui eventual container de erro de outros programas
004660     PERFORM 983100-EXCLUIR-CONTAINER-ERRO
004670
004680     MOVE 0011                   TO MSG1-CD-SEQL-ERRO
004690
004700* Monta o nome do container de resposta
004710     MOVE 'SGTS0060-RPST'        TO NM-CTNR
004720     SET PUT-CONTAINER           TO TRUE
004730
004740* Por causa da captura automática de erro CICS, deve ser previsto
004750* o erro de container
004760     SET  ERRO-CONTAINER         TO TRUE
004770
004780     EXEC CICS PUT
004790               CONTAINER ( NM-CTNR                 )
004800               FROM      ( SGTS0060-RPST           )
004810               FLENGTH   ( LENGTH OF SGTS0060-RPST )
004820     END-EXEC
004830
004840* Ao chegar aqui, há certeza de que não houve erro algum
004850     SET  SEM-ERRO               TO TRUE
004860     .
004870 980000-SAI.
004880     EXIT.
004890
004900*---------------------------------------------------------------*
004910 983000-EXCLUIR-RESPOSTA-ERRO    SECTION.
004920*----------------------------------------------------------------*
004930* Exclui o container de resposta e de erro para evitar problemas
004940* em modulos executados mais de uma vez dentro da mesma sessao do
004950* CICS. Equivale ao initialize na area de resposta dos programas
004960* que utilizam COMMAREA.
004970*----------------------------------------------------------------*
004980     MOVE 0012                   TO MSG1-CD-SEQL-ERRO
004990
005000* Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
005010* (nome_modulo)-RPST
005020     EVALUATE NM-PGM
005030         WHEN 'SGTS0060'
005040             MOVE NM-PGM         TO NM-CTNR(1:8)
005050             MOVE '-RPST'        TO NM-CTNR(9:5)
005060         WHEN OTHER
005070             MOVE 'PRM-DADOS-RPST'
005080                                 TO NM-CTNR
005090     END-EVALUATE
005100
005110     SET DEL-CONTAINER           TO TRUE
005120
005130* Exclui o container de resposta
005140* Não há captura automática de erro CICS por usar RESP e RESP2
005150     EXEC CICS DELETE
005160               CONTAINER ( NM-CTNR    )
005170               RESP      ( MSG1-RESP1 )
005180               RESP2     ( MSG1-RESP2 )
005190     END-EXEC
005200
005210     EVALUATE TRUE
005220         WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
005230         WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
005240             CONTINUE
005250         WHEN OTHER
005260             SET ERRO-CONTAINER  TO TRUE
005270             PERFORM 999002-TRATA-CONDITION-CICS
005280     END-EVALUATE
005290
005300* Exclui o container de erro
005310     PERFORM 983100-EXCLUIR-CONTAINER-ERRO
005320     .
005330 983000-SAI.
005340     EXIT.
005350*
005360*----------------------------------------------------------------*
005370 983100-EXCLUIR-CONTAINER-ERRO   SECTION.
005380*----------------------------------------------------------------*
005390* Limpar variaveis container de erro
005400     MOVE SPACES                 TO IIBKERRO-PRM-ERRO
005410     INITIALIZE IIBKERRO-PRM-ERRO
005420         REPLACING NUMERIC       BY  ZEROS
005430
005440* Exclui o container de erro
005460     MOVE 'PRM-ERRO'             TO NM-CTNR
005470     SET DEL-CONTAINER           TO TRUE
005480
005490* Não há captura automática de erro CICS por usar RESP e RESP2
005500     EXEC CICS DELETE
005510               CONTAINER ( NM-CTNR    )
005520               RESP      ( MSG1-RESP1 )
005530               RESP2     ( MSG1-RESP2 )
005540     END-EXEC
005550
005560     EVALUATE TRUE
005570         WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
005580         WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
005590             CONTINUE
005600         WHEN OTHER
005610             SET ERRO-CONTAINER  TO TRUE
005620             PERFORM 999002-TRATA-CONDITION-CICS
005630     END-EVALUATE
005640     .
005650 983100-SAI.
005660     EXIT.
005670
005680*----------------------------------------------------------------*
005690 984000-GRAVAR-CONTAINER-ERRO    SECTION.
005700*----------------------------------------------------------------*
005720
005730* Grava o erro gerado no container de erro
005740* Não há captura automática de erro CICS por usar RESP e RESP2
005750     EXEC CICS PUT
005760               CONTAINER ( 'PRM-ERRO'                  )
005770               FROM      ( IIBKERRO-PRM-ERRO           )
005780               FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
005790               RESP      ( MSG1-RESP1                  )
005800               RESP2     ( MSG1-RESP2                  )
005810     END-EXEC
005820
005830     .
005840 984000-SAI.
005850     EXIT.
005860
005870*----------------------------------------------------------------*
005880 990000-ENCERRAR                 SECTION.
005890*----------------------------------------------------------------*
005900
005910     EXEC CICS
005920          RETURN
005930     END-EXEC
005940     .
005950 990000-ENCERRAR-SAI.
005960     EXIT.
005970*
005980*----------------------------------------------------------------*
005990*                                                                *
006000* As sections abaixo iniciadas com 999000 representam os tratamen*
006010* tos de erro utilizando o Sistema MSG (rotina MSGS0001)         *
006020*                                                                *
006030*----------------------------------------------------------------*
006040 999000-ERRO                     SECTION.
006050*----------------------------------------------------------------*
006060
006070     IF  MSG1-CD-ERRO-TS EQUAL ZEROS
006080         MOVE 8888                TO MSG1-CD-ERRO-TS
006090     END-IF
006100     IF  MSG1-TX-ERRO-TS EQUAL SPACES
006110         MOVE 'Erro não especificado no módulo,'
006120                                 TO MSG1-TX-ERRO-TS
006130     END-IF
006140     PERFORM 999000-GRAVA-ERRO-E-ENCERRA
006150     .
006160 999000-ERRO-SAI.
006170     EXIT.
006180
006190*----------------------------------------------------------------*
006200*    As sections abaixo representam os tratamentos de erro
006210*    utilizando o sistema MSG de mensageria (rotina MSGS0001)
006220*-----------------------------------------------------------------
006230 999000-GRAVA-ERRO-E-ENCERRA     SECTION.
006240*-----------------------------------------------------------------
006250
006260* Em caso de erro, desfaz as atualizaçoes em banco de dados
006270     EXEC CICS SYNCPOINT
006280               ROLLBACK
006290     END-EXEC
006300
006310* Verifica se o tipo de erro gerado possui tratamento interno
006320     EVALUATE TRUE
006330         WHEN ERRO-CONTAINER
006340             PERFORM 999004-TRATA-ERRO-CTNR
006350         WHEN ERRO-TAMANHO-AREA
006360             PERFORM 999005-TRATA-ERRO-AREA-DADOS
006370         WHEN ERRO-CANAL
006380             PERFORM 999006-TRATA-ERRO-CANAL
006390         WHEN OTHER
006400             CONTINUE
006410     END-EVALUATE
006420
006430* Adiciona as variáveis de entrada para monitoração
006450         MOVE 'SGTS0060-CD-MOD-ORD-NEGO'
006460                                 TO MSGKW999-NM-VRV-MNT
006470         MOVE SGTS0060-CD-MOD-ORD-NEGO
006480                                 TO MSGKW999-VL-VRV-MNT
006490         PERFORM 999003-ADICIONA-VRV-MNT
006500
006510         MOVE 'SGTS0060-IN-ACTE'
006520                                 TO MSGKW999-NM-VRV-MNT
006530         MOVE SGTS0060-IN-ACTE
006540                                 TO MSGKW999-VL-VRV-MNT
006550         PERFORM 999003-ADICIONA-VRV-MNT
006560
006570         MOVE 'SGTS0060-TX-OBS-EST-MOD'
006580                                 TO MSGKW999-NM-VRV-MNT
006590         MOVE SGTS0060-TX-OBS-EST-MOD
006600                                 TO MSGKW999-VL-VRV-MNT
006610         PERFORM 999003-ADICIONA-VRV-MNT
006620
006630         MOVE 'SGTS0060-TS-EST-MOD-ORD'
006640                                 TO MSGKW999-NM-VRV-MNT
006650         MOVE SGTS0060-TS-EST-MOD-ORD
006660                                 TO MSGKW999-VL-VRV-MNT
006670         PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'CD-TIP-EST'  TO  MSGKW999-NM-VRV-MNT
001940         MOVE  CD-TIP-EST   TO  MSGKW999-VL-VRV-MNT
001940         PERFORM  999003-ADICIONA-VRV-MNT
006690
006700*    Grava o erro gerado no sistema MSG - Mensageria
006710     PERFORM MSG1-NTF-MSG
006720
006730     MOVE MSG1-CD-ERRO-TS        TO IIBKERRO-CD-ERRO
006740     MOVE MSG1-TX-ERRO-TS        TO IIBKERRO-TX-ERRO
006750
006760*    Grava o erro gerado no container de erro
006770     PERFORM 984000-GRAVAR-CONTAINER-ERRO
006780
006790*    Encerra a transacao
006800     PERFORM 990000-ENCERRAR
006810     .
006820 999000-SAI.
006830     EXIT.
006840* --------------------------------------*
006850* 999001-TRATA-ABEND-CICS        SECTION.
006860* 999002-TRATA-CONDITION-CICS    SECTION.
006870* 999003-ADICIONA-VRV-MNT        SECTION.
006880* --------------------------------------*
-INC HLPKP999                                                           0000689%
006900* --------------------------------------*
006910* 999004-TRATA-ERRO-CTNR         SECTION.
006920* 999005-TRATA-ERRO-AREA-DADOS   SECTION.
006930* 999006-TRATA-ERRO-CANAL        SECTION.
006940*---------------------------------------*
-INC SGTKP999                                                           0000695%
006960* --------------------------------------*
006970* MSG1-NTF-MSG                   SECTION.
006980* --------------------------------------*
-INC MSGKPMG1                                                           0000699%
007000*---------------------- FIM SGTS0060 -----------------------------