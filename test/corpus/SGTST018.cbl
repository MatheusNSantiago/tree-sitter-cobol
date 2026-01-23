000010*-----------------------------------------------------------------
000020* Programa..: SGTS0054
000030* Analista..: F2054109 - CINTIA AP SOARES IGLESIAS
000040* Autor.....: F8779682 - ROSA APARECIDA HENRIQUES FOZ
000050* Data......: 12/11/2015
000060* Objetivo..: Negócio ---> Listar Tipo de Negociação
000070* Compilação: COBOL/CICS
000080* Processamento......: on-line
000090* Tipo de Comunicação: Container
000100* Tratamento de Erros: Padrão Messageria IIB
000110*-----------------------------------------------------------------
000120* VRS002 04/04/2019 F9188847 DE 2016/145195 - Inclusão de campos:
      *        - - SGTS0054-E-CD-CTRA-TIT-VL e SGTS0054-CD-CTRA-TIT-VL
      *        - Acerto de mensageria
000120* VRS001 12/11/2015 F8779682  de 2013/120821 - Implantação
000130*-----------------------------------------------------------------
000140
000150*-----------------------------------------------------------------
000160 IDENTIFICATION DIVISION.
000170*----------------------------------------------------------------*
000180 PROGRAM-ID. SGTS0054.
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
000380 77  CTE-INICIO-SS                 PIC  X(040)  VALUE
000390              '*** S.S. COMECA AQUI ***'.
000400
000410 77  CTE-PROG                      PIC  X(016)  VALUE
000420              '*** SGTS0054 ***'.
000430 77  CTE-VERS                      PIC  X(006)  VALUE 'VRS002'.
000440
000450* Módulos acionados
000460 77  SGTST048                      PIC  X(008) VALUE 'SGTST048'.
000470 77  SGTSL045                      PIC  X(008) VALUE 'SGTSL045'.
000480*----------------------------------------------------------------*
000490 LOCAL-STORAGE                   SECTION.
000500*----------------------------------------------------------------*
000510
000520* Areas de trabalho
000530*
000540*----------------------------------------------------------------*
000550* Book dos Containers e das subrotinas chamadas
000560*----------------------------------------------------------------*
000570
000580* Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1                                                           00000590
000600
000610* Book para captura de abend cics da bloco codigo MSGKP999
-INC HLPKW999                                                           00000620
000630
000640* Book para captura de erro via MSG1
-INC MSGKWMG1                                                           00000650
000660
000670* Book do container de requisição e resposta deste modulo
000680 01 SGTS0054-DADOS.
-INC SGTK0054                                                           00000690
000700
000710* Persistencia/Crud  da Tabela EST_TIP_NEGO
000720 01  SGTST048-DADOS.
-INC SGTKT048                                                           00000730
000740
000750* Lista da Tabela TIP_NEGO
000760 01  SGTSL045-DADOS.
-INC SGTKL045                                                           00000770
000780
000790* Book do Container de Erro
-INC IIBKERRO                                                           00000800
000810
000820* Book do container de Serviços (para acionamento de serviços)
-INC IIBKSRVC                                                           00000830
000840
000850* Book dos dados do usuário
-INC IIBKOPRD                                                           00000860
000870
000880 77  CTE-FINAL-SS                  PIC X(40)      VALUE
000890              '*** S.S. TERMINA AQUI ***'.
000900
000910*----------------------------------------------------------------*
000920 PROCEDURE DIVISION.
000930*----------------------------------------------------------------*
000940
000950*----------------------------------------------------------------*
000960 000000-ROTINA-PRINCIPAL         SECTION.
000970*----------------------------------------------------------------*
000980
000990     PERFORM 100000-INICIAR
001000
001010     PERFORM 200000-PROCESSAR
001020
001030     PERFORM 990000-ENCERRAR

           GOBACK
001050     .
001060 000000-SAI.
001070     EXIT.
001080
001090*----------------------------------------------------------------*
001100 100000-INICIAR                  SECTION.
001110*----------------------------------------------------------------*
001120
001130     MOVE 0001                   TO MSG1-CD-SEQL-ERRO
001140
001150*    Ativa a captura automatica de abend do CICS.
001160*    WHENEVER CICS
-INC HLPKE999
001180
001190     PERFORM 970000-LER-CANAL
001200
001210*    Define o o nome do programa p/ fins manipulacao container
001220     MOVE 'SGTS0054'             TO NM-PGM
001230     MOVE 'SGTS0054-RPST'        TO NM-CTNR
001240
001250*    Limpa a resposta e erro antes de executar o modulo
001260     PERFORM 983000-EXCLUIR-RESPOSTA-ERRO
001270
001280     PERFORM 971000-LER-REQUISICAO
001290
001300*    Caso não seja necessário utilização do Usuário Logado,
001310*    a section 974000-LER-DADOS-OPRD poderá ser excluída
001320*    PERFORM 974000-LER-DADOS-OPRD
001330
001340     PERFORM 110000-VALIDAR-REQUISICAO
001350
001360     .
001370 100000-SAI.
001380     EXIT.
001390
001400*----------------------------------------------------------------*
001410 110000-VALIDAR-REQUISICAO       SECTION.
001420*----------------------------------------------------------------*
001430     MOVE 0002                   TO MSG1-CD-SEQL-ERRO
001440
001450*    Validação tamanho esperado da requisicao X tamanho recebido
001460*    na section de leitura de container de requisicao 971000
001470
001480*    Valida campo a campo da requisicao

           IF  SGTS0054-E-CD-CTRA-TIT-VL     < ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0001              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' inválido.'      TO  MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

001500     IF  SGTS0054-CD-ULT-TIP-NEGO   <  0
001510         SET ERRO-PROGRAMADO     TO TRUE
001520         MOVE 0714               TO MSG1-CD-ERRO-TS
001530         MOVE  'Código do tipo de negociação inválido.'
001540                                 TO MSG1-TX-ERRO-TS
001560         PERFORM 999000-ERRO
001570     END-IF
001580
001590     .
001600 110000-SAI.
001610     EXIT.
001620
001630*----------------------------------------------------------------*
001640 200000-PROCESSAR                SECTION.
001650*----------------------------------------------------------------*
001660     MOVE 0003                   TO MSG1-CD-SEQL-ERRO
001670
001680     MOVE ZEROS                  TO SGTS0054-QT-REG
001690
001700     PERFORM 210000-TRATA-LISTA-TIP-NEGO

           PERFORM 980000-GRAVAR-CONTAINER-RSPT
001710
001760     .
001770 200000-SAI.
001780     EXIT.
001790
001800*----------------------------------------------------------------*
001810 210000-TRATA-LISTA-TIP-NEGO     SECTION.
001820*----------------------------------------------------------------*
001830     MOVE 0004                   TO MSG1-CD-SEQL-ERRO
001840
001850     INITIALIZE                  SGTSL045-DADOS
001860
001870*    Movimentar os campos de entrada
001880     SET  SGTSL045-LISTA-TIP        TO TRUE

001890     MOVE SGTS0054-E-CD-CTRA-TIT-VL TO SGTSL045-E-CD-CTRA-TIT-VL
001890     MOVE SGTS0054-CD-ULT-TIP-NEGO  TO SGTSL045-CD-ULT-TIP-NEGO
001900
001910     PERFORM 920000-CHAMAR-SGTSL045
001920
001930*    Movimentar os campos de saida
001940
001950     PERFORM UNTIL SGTS0054-QT-REG
001960                                 EQUAL SGTSL045-QT-OCR-TAB
001970        OR SGTS0054-QT-REG       EQUAL 100
001980
001990        ADD 1                    TO SGTS0054-QT-REG
002000        INITIALIZE SGTS0054-LS-TIP-NEGO (SGTS0054-QT-REG)
002010
002020        MOVE  SGTSL045-CD-CTRA-TIT-VL     (SGTS0054-QT-REG)
002030              TO  SGTS0054-CD-CTRA-TIT-VL (SGTS0054-QT-REG)

002020        MOVE  SGTSL045-CD-TIP-NEGO        (SGTS0054-QT-REG)
002030              TO  SGTS0054-CD-TIP-NEGO    (SGTS0054-QT-REG)
002040
002050        MOVE  SGTSL045-NM-TIP-NEGO        (SGTS0054-QT-REG)
002060              TO  SGTS0054-NM-TIP-NEGO    (SGTS0054-QT-REG)
002070
002080        PERFORM 900000-CONS-EST-ATU
002090     END-PERFORM
002100
002110     .
002120 210000-SAI.
002130     EXIT.
002140
002300
002310*----------------------------------------------------------------*
002320*                                                                *
002330*  SECTION DA FAIXA DE 900000 A 969900 PARA DEMAIS ROTINAS       *
002340*                                                                *
002350*----------------------------------------------------------------*
002360*----------------------------------------------------------------*
002370 900000-CONS-EST-ATU           SECTION.
002380*----------------------------------------------------------------*
002390
002400     MOVE 0006                   TO MSG1-CD-SEQL-ERRO
002410
002540     INITIALIZE                  SGTST048-DADOS
002550
002560*    Movimentar os campos de entrada
002570     SET SGTST048-CONS-EST-ATU   TO TRUE
002580     MOVE  SGTSL045-CD-CTRA-TIT-VL  (SGTS0054-QT-REG)
                                       TO SGTST048-E-CD-CTRA-TIT-VL
002580     MOVE  SGTSL045-CD-TIP-NEGO     (SGTS0054-QT-REG)
002590                                 TO  SGTST048-CD-TIP-NEGO
002600
002610     PERFORM 910000-CHAMAR-SGTST048
002620
002630*    Movimentar os campos de saida

002640     MOVE SGTST048-TS-EST-TIP-NEGO
002650                                 TO SGTS0054-TS-EST-TIP-NEGO
002660                                    (SGTS0054-QT-REG)
002670     MOVE SGTST048-CD-TIP-EST-DCS
002680                                 TO SGTS0054-CD-TIP-EST-DCS
002690                                    (SGTS0054-QT-REG)
002440     .
002450 900000-SAI.
002460     EXIT.
002470
002740*----------------------------------------------------------------*
002750 910000-CHAMAR-SGTST048          SECTION.
002760*----------------------------------------------------------------*
002770     MOVE 0007                   TO MSG1-CD-SEQL-ERRO
002780
002790*    Movimento nome do módulo para monitoração de variáveis
002800     MOVE SGTST048               TO NM-PGM
002810     EXEC CICS LINK
002820          PROGRAM  (SGTST048)
002830          COMMAREA (SGTST048-DADOS)
002840     END-EXEC
002850
002860     IF  SGTST048-CD-RTN-PGM     NOT EQUAL ZEROS
002870         SET ERRO-PROGRAMA-ACIONADO TO TRUE
002890         MOVE SGTST048-MSG-ERRO-PGM
002900                                 TO MSG1-TX-RTN-PGM-ACND
002910                                    MSG1-TX-ERRO-TS
002920         MOVE NM-PGM             TO MSG1-NM-PGM-ACND
002930         MOVE SGTST048-CD-RTN-PGM
002940                                 TO MSG1-CD-RTN-PGM-ACND
                                          MSG1-CD-ERRO-TS
002950
002960*        Monitorar variáveis do módulo chamado
002970         PERFORM 969000-MONITORAR-VARIAVEIS
002980         PERFORM 999000-ERRO
002990     END-IF
003000
003010     .
003020 910000-SAI.
003030     EXIT.
003040
003050*----------------------------------------------------------------*
003060 920000-CHAMAR-SGTSL045          SECTION.
003070*----------------------------------------------------------------*
003080     MOVE 0008                   TO MSG1-CD-SEQL-ERRO
003090
003100*    Movimento nome do módulo para monitoração de variáveis
003110     MOVE SGTSL045               TO NM-PGM
003120     EXEC CICS LINK
003130          PROGRAM  (SGTSL045)
003140          COMMAREA (SGTSL045-DADOS)
003150     END-EXEC
003160
003170     IF  SGTSL045-CD-RTN-PGM     NOT EQUAL ZEROS
003180         SET ERRO-PROGRAMA-ACIONADO TO TRUE
003200         MOVE SGTSL045-MSG-ERRO-PGM
003210                                 TO MSG1-TX-RTN-PGM-ACND
003220                                    MSG1-TX-ERRO-TS
003230         MOVE NM-PGM             TO MSG1-NM-PGM-ACND
003240         MOVE SGTSL045-CD-RTN-PGM
003250                                 TO MSG1-CD-RTN-PGM-ACND
                                          MSG1-CD-ERRO-TS
003260
003270*        Monitorar variáveis do módulo chamado
003280         PERFORM 969000-MONITORAR-VARIAVEIS
003290         PERFORM 999000-ERRO
003300     END-IF
003310     .
003320 930000-SAI.
003330     EXIT.
003340
003350*----------------------------------------------------------------*
003360 969000-MONITORAR-VARIAVEIS      SECTION.
003370*----------------------------------------------------------------*
003380* Monitora as variáveis dos módulos chamados
003390*----------------------------------------------------------------*
003420     EVALUATE NM-PGM
003430         WHEN SGTST048
                    MOVE 'SGTST048-ACAO'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTST048-ACAO
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST048-E-CD-CTRA-TIT-VL'
004710                                 TO MSGKW999-NM-VRV-MNT
004720              MOVE SGTST048-E-CD-CTRA-TIT-VL
004730                                 TO MSGKW999-VL-VRV-MNT
004740              PERFORM 999003-ADICIONA-VRV-MNT

003440              MOVE 'SGTST048-CD-TIP-NEGO'
003450                                 TO MSGKW999-NM-VRV-MNT
003460              MOVE SGTST048-CD-TIP-NEGO
003470                                 TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT
003480
003490              MOVE 'SGTST048-TS-EST-TIP-NEGO'
003500                                 TO MSGKW999-NM-VRV-MNT
003510              MOVE SGTST048-TS-EST-TIP-NEGO
003520                                 TO MSGKW999-VL-VRV-MNT
003530              PERFORM 999003-ADICIONA-VRV-MNT

003540         WHEN SGTSL045
                    MOVE 'SGTSL045-ACAO'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL045-ACAO
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL045-E-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL045-E-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

003550              MOVE 'SGTSL045-CD-ULT-TIP-NEGO'
003560                                 TO MSGKW999-NM-VRV-MNT
003570              MOVE SGTSL045-CD-ULT-TIP-NEGO
003580                                 TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT
003590         WHEN OTHER
003600              CONTINUE
003610     END-EVALUATE
003620
003630     .
003640 969000-SAI.
003650     EXIT.
003660
003670*----------------------------------------------------------------*
003680*                                                                *
003690* SECTION DA FAIXA DE 970000 A 979900 PARA LEITURA DE DADOS      *
003700*                                                                *
003710*----------------------------------------------------------------*
003720 970000-LER-CANAL                SECTION.
003730*    Obtem o canal corrente
003740*----------------------------------------------------------------*
003750     MOVE 0009                   TO MSG1-CD-SEQL-ERRO
003760
003770*Por causa da captura automática de erro CICS, deve ser previsto
003780*o erro de container
003790     SET  ERRO-CANAL             TO TRUE
003800
003810     EXEC CICS ASSIGN
003820               CHANNEL ( NM-CNL )
003830     END-EXEC
003840
003850*    Ao chegar aqui, há certeza de que não houve erro algum
003860     SET  SEM-ERRO               TO TRUE
003870     .
003880 970000-FIM.
003890     EXIT.
003900
003910*----------------------------------------------------------------*
003920 971000-LER-REQUISICAO           SECTION.
003930*----------------------------------------------------------------*
003940
003950     MOVE 0010                   TO MSG1-CD-SEQL-ERRO
003960
003970*    Monta o nome do container de requisicao
003980     MOVE 'SGTS0054'             TO NM-PGM
003990     MOVE 'SGTS0054-RQSC'        TO NM-CTNR
004000
004010*    Recupera o tamanho do container de requisição
004020     PERFORM 973100-LER-CONTAINER-NODATA
004030
004040*    Obtem o tamanho esperado da requisição
004050     MOVE LENGTH OF SGTS0054-RQSC TO QT-CCT-EPRO
004060
004070*    Valida tamanho esperado da requisicao X tamanho recebido
004080
004090     IF  QT-CCT-LIDO             NOT EQUAL QT-CCT-EPRO
004100         SET ERRO-TAMANHO-AREA   TO TRUE
004110         PERFORM 999000-GRAVA-ERRO-E-ENCERRA
004120     END-IF
004130
004140     MOVE 0011                   TO MSG1-CD-SEQL-ERRO
004150
004160*Configura dados para leitura do container de requisição
004170     SET  GET-CONTAINER          TO TRUE
004180
004190*Por causa da captura automática de erro CICS, deve ser previsto
004200*o erro de container
004210     SET  ERRO-CONTAINER         TO TRUE
004220
004230*    Efetua a leitura do container
004240     EXEC CICS GET
004250               CONTAINER (NM-CTNR)
004260               FLENGTH   (QT-CCT-LIDO)
004270               INTO      (SGTS0054-RQSC)
004280     END-EXEC
004290
004300*Ao chegar aqui, há certeza de que não houve erro algum
004310     SET  SEM-ERRO               TO TRUE
004320
004330     .
004340 971000-SAI.
004350     EXIT.
004360
004370*----------------------------------------------------------------*
004380 973100-LER-CONTAINER-NODATA     SECTION.
004390*----------------------------------------------------------------*
004420
004430*Configura dados para leitura do container de requisição
004440     SET  GET-CONTAINER          TO TRUE
004450
004460*Por causa da captura automática de erro CICS, deve ser previsto
004470*o erro de container
004480     SET  ERRO-CONTAINER         TO TRUE
004490
004500*    Obtem a quantidade de caracteres no container
004510     MOVE ZEROS                  TO QT-CCT-LIDO
004520     EXEC CICS GET
004530               CONTAINER (NM-CTNR)
004540               NODATA
004550               FLENGTH   (QT-CCT-LIDO)
004560     END-EXEC
004570
004580*    Ao chegar aqui, há certeza de que não houve erro algum
004590     SET  SEM-ERRO               TO TRUE
004600     .
004610 973100-SAI.
004620     EXIT.
004630*----------------------------------------------------------------*
004640 974000-LER-DADOS-OPRD           SECTION.
004650*----------------------------------------------------------------*
004660     MOVE 0012                   TO MSG1-CD-SEQL-ERRO
004670
004680*Configura dados para leitura do container de requisição
004690     SET  GET-CONTAINER          TO TRUE
004700     MOVE 'PRM-INF-OPRD'         TO NM-CTNR
004710
004720*Por causa da captura automática de erro CICS, deve ser previsto
004730*o erro de container
004740     SET  ERRO-CONTAINER         TO TRUE
004750
004760     INITIALIZE PRM-INF-OPRD
004770     EXEC CICS GET
004780          CONTAINER ( NM-CTNR                )
004790          INTO      ( PRM-INF-OPRD           )
004800          FLENGTH   ( LENGTH OF PRM-INF-OPRD )
004810     END-EXEC.
004820
004830*    Ao chegar aqui, há certeza de que não houve erro algum
004840     SET  SEM-ERRO               TO TRUE
004850     .
004860 974000-SAI.
004870     EXIT.
004880*----------------------------------------------------------------*
004890*                                                                *
004900* SECTION DA FAIXA DE 980000 A 989900 PARA GRAVACAO DE DADOS     *
004910*                                                                *
004920*----------------------------------------------------------------*
004930 980000-GRAVAR-CONTAINER-RSPT    SECTION.
004940*----------------------------------------------------------------*
004950
004960*    Exclui eventual container de erro de outros programas
004970     PERFORM 983100-EXCLUIR-CONTAINER-ERRO
004980
004990     MOVE 0013                   TO MSG1-CD-SEQL-ERRO
005000
005010*    Monta o nome do container de resposta
005020     MOVE 'SGTS0054-RPST'        TO NM-CTNR
005030     SET PUT-CONTAINER           TO TRUE
005040
005050*Por causa da captura automática de erro CICS, deve ser previsto
005060*o erro de container
005070     SET  ERRO-CONTAINER         TO TRUE
005080
005090     EXEC CICS PUT
005100               CONTAINER (NM-CTNR                )
005110               FROM      (SGTS0054-RPST          )
005120               FLENGTH   (LENGTH OF SGTS0054-RPST)
005130     END-EXEC
005140
005150*Ao chegar aqui, há certeza de que não houve erro algum
005160     SET  SEM-ERRO               TO TRUE
005170     .
005180 980000-SAI.
005190     EXIT.
005200
005210*----------------------------------------------------------------*
005220 983000-EXCLUIR-RESPOSTA-ERRO    SECTION.
005230*----------------------------------------------------------------*
005240* Exclui o container de resposta e de erro para evitar problemas *
005250* em modulos executados mais de uma vez dentro da mesma sessao do*
005260* CICS. Equivale ao initialize na area de resposta dos programas *
005270* que utilizam COMMAREA.                                         *
005280*----------------------------------------------------------------*
005290
005300     MOVE 0014                   TO MSG1-CD-SEQL-ERRO
005310
005320     MOVE NM-PGM                 TO NM-CTNR(1:8)
005330     MOVE '-RPST'                TO NM-CTNR(9:5)
005340
005350     SET DEL-CONTAINER           TO TRUE
005360
005370*    Exclui o container de resposta
005380*    Não há captura automática de erro CICS por usar RESP e RESP2
005390     EXEC CICS DELETE
005400               CONTAINER (NM-CTNR)
005410               RESP      (MSG1-RESP1)
005420               RESP2     (MSG1-RESP2)
005430     END-EXEC
005440
005450     EVALUATE TRUE
005460         WHEN MSG1-RESP1         EQUAL DFHRESP(CONTAINERERR)
005470         WHEN MSG1-RESP1         EQUAL DFHRESP(NORMAL)
005480              CONTINUE
005490         WHEN OTHER
005500              SET ERRO-CONTAINER TO TRUE
005510              PERFORM 999002-TRATA-CONDITION-CICS
005520     END-EVALUATE
005530
005540*    Excluir o container de erro
005550     PERFORM 983100-EXCLUIR-CONTAINER-ERRO
005560     .
005570 983000-SAI.
005580     EXIT.
005590*
005600*----------------------------------------------------------------*
005610 983100-EXCLUIR-CONTAINER-ERRO   SECTION.
005620*----------------------------------------------------------------*
005630
005640*    Limpar variaveis containerde erro
005650     INITIALIZE IIBKERRO-PRM-ERRO
005660
005670*    Exclui o container de erro
005680     MOVE 0015                   TO MSG1-CD-SEQL-ERRO
005690     MOVE 'PRM-ERRO'             TO NM-CTNR
005700     SET DEL-CONTAINER           TO TRUE
005710
005720*    Não há captura automática de erro CICS por usar RESP e RESP2
005730     EXEC CICS DELETE
005740               CONTAINER (NM-CTNR)
005750               RESP      (MSG1-RESP1)
005760               RESP2     (MSG1-RESP2)
005770     END-EXEC
005780
005790     EVALUATE TRUE
005800         WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
005810         WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
005820              CONTINUE
005830         WHEN OTHER
005840              SET ERRO-CONTAINER TO TRUE
005850              PERFORM 999002-TRATA-CONDITION-CICS
005860     END-EVALUATE
005870     .
005880 983100-SAI.
005890     EXIT.
005900*----------------------------------------------------------------*
005910 984000-GRAVAR-CONTAINER-ERRO    SECTION.
005920*----------------------------------------------------------------*
005940
005950*    Grava o erro gerado no container de erro
005960*    Não há captura automática de erro CICS por usar RESP e RESP2
005970     EXEC CICS PUT
005980               CONTAINER ( 'PRM-ERRO'                  )
005990               FROM      ( IIBKERRO-PRM-ERRO           )
006000               FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
006010               RESP      (MSG1-RESP1                   )
006020               RESP2     (MSG1-RESP2                   )
006030     END-EXEC
006040
006050     .
006060 984000-SAI.
006070     EXIT.
006080
006090*----------------------------------------------------------------*
006100 990000-ENCERRAR                 SECTION.
006110*----------------------------------------------------------------*
006120
006130     EXEC CICS
006140          RETURN
006150     END-EXEC
006160     .
006170 990000-ENCERRAR-SAI.
006180     EXIT.
006190*
006200*----------------------------------------------------------------*
006210*                                                                *
006220* As sections abaixo iniciadas com 999000 representam os tratamen*
006230* tos de erro utilizando o Sistema MSG (rotina MSGS0001)         *
006240*                                                                *
006250*----------------------------------------------------------------*
006260 999000-ERRO                     SECTION.
006270*----------------------------------------------------------------*
006280
006290     IF  MSG1-CD-ERRO-TS         EQUAL ZEROS
006300         MOVE 8888               TO MSG1-CD-ERRO-TS
006310     END-IF
006320
006330     IF  MSG1-TX-ERRO-TS         EQUAL SPACES
006340         MOVE 'ERRO NAO ESPECIFICADO NO MODULO'
006350                                 TO MSG1-TX-ERRO-TS
006360     END-IF
006370
006380     PERFORM 999000-GRAVA-ERRO-E-ENCERRA
006390
006400     .
006410 999000-ERRO-SAI.
006420     EXIT.
006430
006440*----------------------------------------------------------------*
006450*     As sections abaixo representam os tratamentos de erro
006460*    utilizando o sistema MSG de mensageria (rotina MSGS0001)
006470*-----------------------------------------------------------------
006480 999000-GRAVA-ERRO-E-ENCERRA     SECTION.
006490*-----------------------------------------------------------------
006500
006510*    Em caso de erro, desfaz as atualizações em banco de dados
006520
006530     EXEC CICS SYNCPOINT
006540               ROLLBACK
006550     END-EXEC
006560
006570*Verifica se o tipo de erro gerado possui tratamento interno
006580     EVALUATE TRUE
006590         WHEN ERRO-CONTAINER
006600              PERFORM 999004-TRATA-ERRO-CTNR
006610         WHEN ERRO-TAMANHO-AREA
006620              PERFORM 999005-TRATA-ERRO-AREA-DADOS
006630         WHEN ERRO-CANAL
006640              PERFORM 999006-TRATA-ERRO-CANAL
006650         WHEN OTHER
006660              CONTINUE
006670     END-EVALUATE
006680
006690*Adiciona as variáveis de entrada para monitoração

               MOVE 'SGTS0054-E-CD-CTRA-TIT-VL'
004710                                 TO MSGKW999-NM-VRV-MNT
004720         MOVE SGTS0054-E-CD-CTRA-TIT-VL
004730                                 TO MSGKW999-VL-VRV-MNT
004740         PERFORM 999003-ADICIONA-VRV-MNT

006710         MOVE 'SGTS0054-CD-ULT-TIP-NEGO'
006720                                 TO MSGKW999-NM-VRV-MNT
006730         MOVE SGTS0054-CD-ULT-TIP-NEGO
006740                                 TO MSGKW999-VL-VRV-MNT
006750         PERFORM 999003-ADICIONA-VRV-MNT
006760

006780
006790*    Grava o erro gerado no sistema MSG - Mensageria
006800     PERFORM MSG1-NTF-MSG
006810
006820*    Movimenta Mensagem e Código de Erro para o Container de Erro
006830     MOVE MSG1-CD-ERRO-TS        TO IIBKERRO-CD-ERRO
006840     MOVE MSG1-TX-ERRO-TS        TO IIBKERRO-TX-ERRO
006850
006860*    Grava o erro gerado no container de erro
006870     PERFORM 984000-GRAVAR-CONTAINER-ERRO
006880
006890*    Encerra a transacao
006900     PERFORM 990000-ENCERRAR
006910     .
006920 999000-SAI.
006930     EXIT.
006940* --------------------------------------*
006950* 999001-TRATA-ABEND-CICS        SECTION.
006960* 999002-TRATA-CONDITION-CICS    SECTION.
006970* 999003-ADICIONA-VRV-MNT        SECTION.
006980* --------------------------------------*
-INC HLPKP999                                                            0000699
007000* --------------------------------------*
007010* 999004-TRATA-ERRO-CTNR         SECTION.
007020* 999005-TRATA-ERRO-AREA-DADOS   SECTION.
007030* 999006-TRATA-ERRO-CANAL        SECTION.
007040*---------------------------------------*
-INC SGTKP999                                                           00007050
007060* --------------------------------------*
007070* MSG1-NTF-MSG                   SECTION.
007080* --------------------------------------*
-INC MSGKPMG1                                                           00007090
007100*---------------------- FIM SGTS0054 -----------------------------