000560*-----------------------------------------------------------------
000570* PROGRAMA..: SGTS0104
000580* ANALISTA..: F2054109 - CINTIA IGLESIAS
000590* AUTOR.....: F8785286 - Roseli Akemi Kawahata
000600* DATA......: 10/01/2017
000610* OBJETIVO..:Excluir Vínculo de Contato Externo da Operação Estru-
000620*              turada.
000630* COMPILACAO: COBOL/CICS
000640*-----------------------------------------------------------------
      * VRS001 28/02/2018 - F9158937 - Retirada container de resposta
000650*        10/01/2017 - F8785286 2016/145195 - Implatação
000660*-----------------------------------------------------------------
000670
000680*-----------------------------------------------------------------
000690 IDENTIFICATION DIVISION.
000700*----------------------------------------------------------------*
000710 PROGRAM-ID. SGTS0104.
000730*----------------------------------------------------------------*
001040 ENVIRONMENT DIVISION.
001040*----------------------------------------------------------------*
000780 CONFIGURATION                   SECTION.
000790*----------------------------------------------------------------*
000800 SPECIAL-NAMES.
000810     DECIMAL-POINT IS COMMA.
000830*----------------------------------------------------------------*
000840 DATA DIVISION.
000850*----------------------------------------------------------------*
000880 WORKING-STORAGE                 SECTION.
000890*----------------------------------------------------------------*
000900
000910 77  CTE-INICIO-SS               PIC  X(040)  VALUE
000920              '*** S.S. COMECA AQUI ***'.
000930
000940 77  CTE-PROG                    PIC  X(018)  VALUE
000950              '*** SGTS0104 ***'.
000960 77  CTE-VERS                    PIC  X(006)  VALUE 'VRS001'.
000970
000980* Módulos acionados
001000 77  SGTSE035                    PIC  X(008) VALUE 'SGTSE035'.
000970
001030*----------------------------------------------------------------*
001040 LOCAL-STORAGE                   SECTION.
001050*----------------------------------------------------------------*
001060
001070* Areas de trabalho01080

       01  CD-USU-RSP-ATL              PIC  X(008) VALUE SPACES.
       01  TS-ATL-CTT-EXNO             PIC  X(026) VALUE SPACES.
       01  EXCLUIR-REG                 PIC  X(001) VALUE SPACES.

      *--> Auxiliares para movimentação na monitoração de variáveis.
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).

001090*----------------------------------------------------------------*
001100* Book dos Containers e das subrotinas chamadas
001110*----------------------------------------------------------------*
001120
001130* Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1
001150* Book para captura de abend cics da bloco codigo HLPKP999
-INC HLPKW999
001170
001180* Book para captura de erro via MSG1
-INC MSGKWMG1
001200
001210* Book do container de requisição e resposta deste modulo
001220 01 SGTS0104-DADOS.
-INC SGTK0104
001240
001250* Book SGT - Excluir Clinte Externo
-INC SGTKE035
001240
001360* Book do Container de Erro
-INC IIBKERRO
001380* Book do container de Serviços (para acionamento de serviços)
-INC IIBKSRVC
001400* Book dos dados do usuário
-INC IIBKOPRD

      * Área SQL

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.
001420
001430 01  CTE-FINAL-SS                  PIC X(40)      VALUE
001440              '*** S.S. TERMINA AQUI ***'.
001450
001460*----------------------------------------------------------------*
001470 PROCEDURE DIVISION.
001480*----------------------------------------------------------------*
001490
001500*----------------------------------------------------------------*
001510 000000-ROTINA-PRINCIPAL         SECTION.
001520*----------------------------------------------------------------*
001530
001540     PERFORM 100000-INICIAR
001550
001560     PERFORM 200000-PROCESSAR
001570
001580     PERFORM 999999-ENCERRAR
001590
001600     .
001610 000000-SAI.
001620     EXIT.
001630
001640*----------------------------------------------------------------*
001650 100000-INICIAR                  SECTION.
001660*----------------------------------------------------------------*
001670
001680     MOVE 0001                   TO MSG1-CD-SEQL-ERRO
001690
      * Habilita tratamento automático de Erros DB2
-INC HLPKE997
001700*    Ativa a captura automatica de abend do CICS.
001710*    WHENEVER CICS
-INC HLPKE999
001730
001740     PERFORM 970000-LER-CANAL
001750
001770     MOVE 'SGTS0104'             TO NM-PGM
001780
001800     PERFORM 983000-EXCLUIR-RESPOSTA-ERRO
001810
001820     PERFORM 971000-LER-REQUISICAO
001830
001880     PERFORM 110000-VALIDAR-REQUISICAO
001880
001890     .
001900 100000-SAI.
001910     EXIT.
001920
001930*----------------------------------------------------------------*
001940 110000-VALIDAR-REQUISICAO       SECTION.
001950*----------------------------------------------------------------*
001960     MOVE 0002                   TO MSG1-CD-SEQL-ERRO
001970
002030     IF  SGTS0104-CD-OPR-ETRD-MCP IS NOT NUMERIC
002040         SET ERRO-PROGRAMADO     TO TRUE
002050         MOVE 0003               TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -             'numérico.'        TO MSG1-TX-ERRO-TS
002090         PERFORM 998100-ERRO
002100     END-IF

002030     IF  SGTS0104-CD-OPR-ETRD-MCP < ZEROS
002040         SET ERRO-PROGRAMADO     TO TRUE
002050         MOVE 0004               TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais inv
      -             'álido.'           TO MSG1-TX-ERRO-TS
002090         PERFORM 998100-ERRO
002100     END-IF

           IF  SGTS0104-CD-OPR-ETRD-MCP = ZEROS
002040         SET ERRO-PROGRAMADO     TO TRUE
002050         MOVE 0005               TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -             'informado.'       TO MSG1-TX-ERRO-TS
002090         PERFORM 998100-ERRO
002100     END-IF
002110
002120     IF  SGTS0104-NR-VRS-OPR-ETRD < ZEROS
002130         SET ERRO-PROGRAMADO     TO TRUE
002140         MOVE 0006               TO MSG1-CD-ERRO-TS
               MOVE 'Número da versão da operação estruturada inválido.'
                                       TO MSG1-TX-ERRO-TS
002180         PERFORM 998100-ERRO
002190     END-IF

           IF  SGTS0104-NR-VRS-OPR-ETRD = ZEROS
002130         SET ERRO-PROGRAMADO     TO TRUE
002140         MOVE 0007               TO MSG1-CD-ERRO-TS
               MOVE 'Número da versão da operação estruturada não inform
      -             'ado.'             TO MSG1-TX-ERRO-TS
002180         PERFORM 998100-ERRO
002190     END-IF
002200
002120     IF  SGTS0104-CD-PSS-CTT-EXNO   < ZEROS
002130         SET ERRO-PROGRAMADO     TO TRUE
002140         MOVE 0059               TO MSG1-CD-ERRO-TS
               MOVE 'Código da pessoa de contato externo inválido.'
                                       TO MSG1-TX-ERRO-TS
002180         PERFORM 998100-ERRO
002190     END-IF

           IF  SGTS0104-CD-PSS-CTT-EXNO  = ZEROS
002130         SET ERRO-PROGRAMADO     TO TRUE
002140         MOVE 0058               TO MSG1-CD-ERRO-TS
002150         MOVE 'Código da pessoa de contato externo não informado.'
002150                                 TO MSG1-TX-ERRO-TS
002180         PERFORM 998100-ERRO
002190     END-IF
002200
002400     .
002410 110000-SAI.
002420     EXIT.
002430
002440*----------------------------------------------------------------*
002450 200000-PROCESSAR                SECTION.
002460*----------------------------------------------------------------*
002470     MOVE 0003                   TO MSG1-CD-SEQL-ERRO
002480
002530     PERFORM 210000-LISTA-CTT-EXT
002500*
           IF  EXCLUIR-REG  EQUAL  'S'
002530         PERFORM 220000-EXCLUI-CTT-EXT
002500     END-IF
002880     .
002890 200000-SAI.
002900     EXIT.
002910
002920*----------------------------------------------------------------*
002930 210000-LISTA-CTT-EXT    SECTION.
002940*----------------------------------------------------------------*
002950     MOVE 0004                   TO  MSG1-CD-SEQL-ERRO

           EXEC SQL
                 SELECT CD_USU_RSP_ATL
                      , TS_ATL_CTT_EXNO
                   INTO  :CD-USU-RSP-ATL
                       , :TS-ATL-CTT-EXNO
                       FROM DB2SGT.CTT_EXNO_OPR_ETRD
                WHERE CD_CTRA_TIT_VL     = 300000
                  AND CD_OPR_ETRD_MC     = :SGTS0104-CD-OPR-ETRD-MCP
                  AND NR_VRS_OPR_ETRD    = :SGTS0104-NR-VRS-OPR-ETRD
                  AND CD_PSS_CTT_EXNO    = :SGTS0104-CD-PSS-CTT-EXNO
           END-EXEC

           IF  SQLCODE   EQUAL ZEROS
               MOVE 'S'  TO  EXCLUIR-REG
           ELSE
               MOVE 'N'  TO  EXCLUIR-REG
           END-IF
003170     .
003180 210000-SAI.
003190     EXIT.
003200
002920*----------------------------------------------------------------*
002930 220000-EXCLUI-CTT-EXT    SECTION.
002940*----------------------------------------------------------------*
002950     MOVE 0005                       TO  MSG1-CD-SEQL-ERRO
002960
002970     INITIALIZE                      SGTSE035-DADOS
002980
002990*    Movimentar o nome do programa a ser executado
003000     MOVE SGTSE035                   TO  NM-PGM
003010
003020*    Movimentar os campos de entrada
003030
003040     MOVE 300000                     TO  SGTSE035-CD-CTRA-TIT-VL
003040     MOVE SGTS0104-CD-OPR-ETRD-MCP   TO  SGTSE035-CD-OPR-ETRD-MC
003040     MOVE SGTS0104-NR-VRS-OPR-ETRD   TO  SGTSE035-NR-VRS-OPR-ETRD
003040     MOVE SGTS0104-CD-PSS-CTT-EXNO   TO  SGTSE035-CD-PSS-CTT-EXNO
003120*
003130     PERFORM 900000-EXECUTAR-MODULO
003140
003170     .
003180 220000-SAI.
003190     EXIT.
003200
003800*----------------------------------------------------------------*
003810*                                                                *
003820*  SECTION DA FAIXA DE 900000 A 969900 PARA DEMAIS ROTINAS       *
003830*                                                                *
003840*----------------------------------------------------------------*
003850* Executa um modulo implementado com CONTAINER                   *
003860* Nao incluir nessa section os modulos chamados via COMMAREA     *
003870*                                                                *
003880*----------------------------------------------------------------*
003890 900000-EXECUTAR-MODULO          SECTION.
003900*----------------------------------------------------------------*
003910
003920     MOVE 0006                   TO MSG1-CD-SEQL-ERRO
003930*    Grava o container de requisicao do modulo a executar
003940     PERFORM 982000-GRAVAR-CONTAINER-RQSC
003950
003960*    Limpa a resposta e erro antes de executar o modulo
003970     PERFORM 983000-EXCLUIR-RESPOSTA-ERRO
003980
003990*    Não há captura automática de erro CICS por usar RESP e RESP2
004000     EVALUATE NM-PGM
003980
004010         WHEN SGTSE035
004020             EXEC CICS LINK
004030                       PROGRAM ( SGTSE035  )
004040                       CHANNEL ( NM-CNL    )
004050                       RESP    ( MSG1-RESP1)
004060                       RESP2   ( MSG1-RESP2)
004070             END-EXEC
003980
004080         WHEN OTHER
                   SET   ERRO-PROGRAMADO
                                       TO  TRUE
                   MOVE  9006          TO  MSG1-CD-ERRO-TS
004090             STRING
004100               'Não definida a execução do módulo ' NM-PGM
004110                DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
                   END-STRING
004120             PERFORM 998100-ERRO
004130     END-EVALUATE
004140*
004150     EVALUATE TRUE
004160         WHEN MSG1-RESP1        EQUAL DFHRESP(NORMAL)
004170             CONTINUE
004180         WHEN OTHER
004190              SET   ERRO-CICS    TO  TRUE
                    MOVE  3004         TO  MSG1-CD-ERRO-TS
                    STRING
                    'Erro  ao acionar o módulo  ' NM-PGM  ', '
                    'RESP  = '  MSG1-RESP1  ', '
                    'RESP2 = '  MSG1-RESP2
                    DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
004260              PERFORM 998100-ERRO
004270     END-EVALUATE
004280
004290*    Efetua a leitura do Container de Erro
004300     PERFORM 972000-LER-CONTAINER-DE-ERRO
004350     .
004360 900000-SAI.
004370     EXIT.
004380
004870*----------------------------------------------------------------*
005180 969000-MONITORAR-VARIAVEIS      SECTION.
005190*----------------------------------------------------------------*
005200* Monitora as variáveis dos módulos chamados
005210*----------------------------------------------------------------*
005220     MOVE 0007                   TO MSG1-CD-SEQL-ERRO
005230
005240     EVALUATE NM-PGM
005500
005630         WHEN SGTSE035
005640              MOVE 'SGTSE035-CD-CTRA-TIT-VL'
005650                                 TO MSGKW999-NM-VRV-MNT
005660              MOVE SGTSE035-CD-CTRA-TIT-VL
005670                                 TO MSGKW999-VL-VRV-MNT
005680              PERFORM 999003-ADICIONA-VRV-MNT
005690
005570              MOVE 'SGTSE035-CD-OPR-ETRD-MC'
005580                                 TO MSGKW999-NM-VRV-MNT
                    IF  SGTSE035-CD-OPR-ETRD-MC IS NUMERIC
005590                  MOVE SGTSE035-CD-OPR-ETRD-MC
                                       TO CD-OPR-ETRD-MCP
                        MOVE CD-OPR-ETRD-MCP-TX   TO MSGKW999-VL-VRV-MNT
                    ELSE
                        MOVE 'Valor não numérico' TO MSGKW999-VL-VRV-MNT
                    END-IF
005610              PERFORM 999003-ADICIONA-VRV-MNT
005560
005570              MOVE 'SGTSE035-NR-VRS-OPR-ETRD'
005580                                 TO MSGKW999-NM-VRV-MNT
005590              MOVE SGTSE035-NR-VRS-OPR-ETRD
005600                                 TO MSGKW999-VL-VRV-MNT
005610              PERFORM 999003-ADICIONA-VRV-MNT
005560
005570              MOVE 'SGTSE035-CD-PSS-CTT-EXNO'
005580                                 TO MSGKW999-NM-VRV-MNT
005590              MOVE SGTSE035-CD-PSS-CTT-EXNO
005600                                 TO MSGKW999-VL-VRV-MNT
005610              PERFORM 999003-ADICIONA-VRV-MNT
005560
005570              MOVE 'IIBKERRO-TX-ERRO'
005580                                 TO MSGKW999-NM-VRV-MNT
005590              MOVE IIBKERRO-TX-ERRO
005600                                 TO MSGKW999-VL-VRV-MNT
005610              PERFORM 999003-ADICIONA-VRV-MNT
005560
005570              MOVE 'IIBKERRO-CD-ERRO'
005580                                 TO MSGKW999-NM-VRV-MNT
005590              MOVE IIBKERRO-CD-ERRO
005600                                 TO MSGKW999-VL-VRV-MNT
005610              PERFORM 999003-ADICIONA-VRV-MNT
005690
005760         WHEN OTHER
005770              CONTINUE
005780     END-EVALUATE
005790
005800     .
005810 969000-SAI.
005820     EXIT.
005830
005840*----------------------------------------------------------------*
005850*                                                                *
005860* SECTION DA FAIXA DE 970000 A 979900 PARA LEITURA DE DADOS      *
005870*                                                                *
005880*----------------------------------------------------------------*
005890 970000-LER-CANAL                SECTION.
005900*    Obtem o canal corrente
005910*----------------------------------------------------------------*
005920     MOVE 0008                   TO MSG1-CD-SEQL-ERRO
005930
005940*Por causa da captura automática de erro CICS, deve ser previsto
005950*o erro de container
005960     SET  ERRO-CANAL             TO TRUE
005970
005980     EXEC CICS ASSIGN
005990               CHANNEL ( NM-CNL )
006000     END-EXEC
006010
006020*    Ao chegar aqui, há certeza de que não houve erro algum
006030     SET  SEM-ERRO               TO TRUE
006040     .
006050 970000-FIM.
006060     EXIT.
006070
006080*----------------------------------------------------------------*
006090 971000-LER-REQUISICAO           SECTION.
006100*----------------------------------------------------------------*
006110
006120     MOVE 0009                   TO MSG1-CD-SEQL-ERRO
006130
006140*    Monta o nome do container de requisicao
006150     MOVE 'SGTS0104'             TO NM-PGM
006160     MOVE 'SGTS0104-RQSC'        TO NM-CTNR
006170
006180*    Recupera o tamanho do container de requisição
006190     PERFORM 973100-LER-CONTAINER-NODATA
006200
006210* Caso a requisição seja variável (cláusula DEPENDING ON), o
006220* tratamento abaixo não deve ser efetuado, senão, ocasionará erro
006230* -->> Início
006240
006250*    Obtem o tamanho esperado da requisicao
006260     MOVE LENGTH OF SGTS0104-RQSC TO QT-CCT-EPRO
006270
006280*    Valida tamanho esperado da requisicao X tamanho recebido
006290
006300     IF  QT-CCT-LIDO             NOT EQUAL QT-CCT-EPRO
006310         SET ERRO-TAMANHO-AREA   TO TRUE
006320         PERFORM 999000-GRAVA-ERRO-E-ENCERRA
006330     END-IF
006340* -->> Fim
006350
006360     MOVE 0010                   TO MSG1-CD-SEQL-ERRO
006370
006380*Configura dados para leitura do container de requisição
006390     SET  GET-CONTAINER          TO TRUE
006400
006410*Por causa da captura automática de erro CICS, deve ser previsto
006420*o erro de container
006430     SET  ERRO-CONTAINER         TO TRUE
006440
006450*    Efetua a leitura do container
006460     EXEC CICS GET
006470               CONTAINER (NM-CTNR      )
006480               FLENGTH   (QT-CCT-LIDO  )
006490               INTO      (SGTS0104-RQSC)
006500     END-EXEC
006510
006520*Ao chegar aqui, há certeza de que não houve erro algum
006530     SET  SEM-ERRO               TO TRUE
006540
006550     .
006560 971000-SAI.
006570     EXIT.
006580
006590*----------------------------------------------------------------*
006600 972000-LER-CONTAINER-DE-ERRO    SECTION.
006610*----------------------------------------------------------------*
006620*    A presença do container PRM-ERRO indica que houve erro de
006630*    integração, a ausência do container indica sucesso.
006640*----------------------------------------------------------------*
006650
006660     MOVE 0011                   TO MSG1-CD-SEQL-ERRO
006670
006680*    Le o container de erro dos modulos chamados
006690     SET GET-CONTAINER           TO TRUE
006700     MOVE 'PRM-ERRO'             TO NM-CTNR
006710     INITIALIZE IIBKERRO-PRM-ERRO
006720
006730*    Não há captura automática de erro CICS por usar RESP e RESP2
006740     EXEC CICS GET
006750               CONTAINER ( NM-CTNR                     )
006760               INTO      ( IIBKERRO-PRM-ERRO           )
006770               FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
006780               RESP      ( MSG1-RESP1                  )
006790               RESP2     ( MSG1-RESP2                  )
006800     END-EXEC
006810
006820     EVALUATE TRUE
006830*        Caso deseje personalizar ou não interromper o
006840*        processamento, alterar para não dar erro aqui SOMENTE
006850*        PARA O MÓDULO CHAMADO e tratar na section chamadora
006860         WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
006870              CONTINUE
006880         WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
                    SET ERRO-PROGRAMA-ACIONADO TO  TRUE
                    MOVE  NM-PGM               TO  MSG1-NM-PGM-ACND

                    IF  NM-PGM(1:3) EQUAL 'SGT'
                        MOVE  IIBKERRO-CD-ERRO TO  MSG1-CD-RTN-PGM-ACND
                                                   MSG1-CD-ERRO-TS
                        MOVE  IIBKERRO-TX-ERRO TO  MSG1-TX-RTN-PGM-ACND
                                                   MSG1-TX-ERRO-TS
                    ELSE
                       MOVE 3004               TO MSG1-CD-ERRO-TS
                       STRING  'Erro ao acionar o módulo ' NM-PGM '.'
                            DELIMITED BY SIZE
                            INTO           MSG1-TX-ERRO-TS

                       MOVE IIBKERRO-TX-ERRO TO MSG1-TX-RTN-PGM-ACND
                       MOVE IIBKERRO-CD-ERRO TO MSG1-CD-RTN-PGM-ACND
                    END-IF
006890
006980*              Monitorar variáveis do módulo chamado
006990               PERFORM 969000-MONITORAR-VARIAVEIS
007000               PERFORM 998100-ERRO
007010         WHEN OTHER
007020              SET ERRO-CONTAINER  TO TRUE
007030              PERFORM 999002-TRATA-CONDITION-CICS
007040     END-EVALUATE
007050     .
007060 972000-SAI.
007070     EXIT.
007080
007900*----------------------------------------------------------------*
007910 973100-LER-CONTAINER-NODATA     SECTION.
007920*----------------------------------------------------------------*
007930
007940     MOVE 0014                   TO MSG1-CD-SEQL-ERRO
007950
007960*Configura dados para leitura do container de requisição
007970     SET  GET-CONTAINER          TO TRUE
007980
007990*Por causa da captura automática de erro CICS, deve ser previsto
008000*o erro de container
008010     SET  ERRO-CONTAINER         TO TRUE
008020
008030*    Obtem a quantidade de caracteres no container
008040     MOVE ZEROS                  TO QT-CCT-LIDO
008050     EXEC CICS GET
008060               CONTAINER (NM-CTNR)
008070               NODATA
008080               FLENGTH   (QT-CCT-LIDO)
008090     END-EXEC
008100
008110*    Ao chegar aqui, há certeza de que não houve erro algum
008120     SET  SEM-ERRO               TO TRUE
008130     .
008140 973100-SAI.
008150     EXIT.
008410*----------------------------------------------------------------*
008420*                                                                *
008430* SECTION DA FAIXA DE 980000 A 989900 PARA GRAVACAO DE DADOS     *
008440*                                                                *
009130*----------------------------------------------------------------*
009140 982000-GRAVAR-CONTAINER-RQSC    SECTION.
009150*----------------------------------------------------------------*
009160*    Postar container com dados de requisição (entrada)
009170*
009180*                        NOME DO CONTAINER REQUISICAO
009190*                        ----------------------------
009200*
009210*    Modulo CONTAINER  -     NOME_PGM-RQSC
009220*    Modulo IIB (FIXO) -     PRM-DADOS-RQSC
009230*
009240*----------------------------------------------------------------*
009250
009260     MOVE 0015                   TO MSG1-CD-SEQL-ERRO
009270
009280*    Monta o nome dos container de acordo com modulo a executar
009290     MOVE SPACES                 TO NM-CTNR
009300     SET PUT-CONTAINER           TO TRUE
009310
009320*    Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
009330*    nome_modulo -RPST
009400     MOVE NM-PGM                 TO NM-CTNR(1:8)
009410     MOVE '-RQSC'                TO NM-CTNR(9:5)
009430
009440*Por causa da captura automática de erro CICS, deve ser previsto
009450*o erro de container
009460     SET  ERRO-CONTAINER         TO TRUE
009470
009480*    Faz put da requisicao do programa a executar
009490     EVALUATE NM-PGM
004010
004010         WHEN SGTSE035
009570              EXEC CICS PUT
009580                        CONTAINER (NM-CTNR                 )
009590                        FROM      (SGTSE035-RQSC           )
009600                        FLENGTH   (LENGTH OF SGTSE035-RQSC )
004070              END-EXEC
009470
009630         WHEN OTHER
009640              SET   ERRO-PROGRAMADO       TO  TRUE
                    MOVE 9004                   TO  MSG1-CD-ERRO-TS
                    STRING
                     'Erro na gravação da requisição do módulo '
                            NM-PGM '.'
                    DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
009670              PERFORM 998100-ERRO
009680     END-EVALUATE
009690
009700*Ao chegar aqui, há certeza de que não houve erro algum
009710     SET  SEM-ERRO               TO TRUE
009720
009730     .
009740 982000-SAI.
009750     EXIT.
009760
009770*----------------------------------------------------------------*
009780 983000-EXCLUIR-RESPOSTA-ERRO    SECTION.
009790*----------------------------------------------------------------*
009800* Exclui o container de resposta e de erro para evitar problemas *
009810* em modulos executados mais de uma vez dentro da mesma sessao do*
009820* CICS. Equivale ao initialize na area de resposta dos programas *
009830* que utilizam COMMAREA.                                         *
009840*----------------------------------------------------------------*
009850
009860     MOVE 0016                   TO MSG1-CD-SEQL-ERRO
009870
009880*    Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
009890*    nome_modulo -RPST
           MOVE NM-PGM                 TO NM-CTNR(1:8)
009970     MOVE '-RPST'                TO NM-CTNR(9:5)
009990
010000     SET DEL-CONTAINER           TO TRUE
010010
010020*    Exclui o container de resposta
010030*    Não há captura automática de erro CICS por usar RESP e RESP2
010040     EXEC CICS DELETE
010050               CONTAINER (NM-CTNR   )
010060               RESP      (MSG1-RESP1)
010070               RESP2     (MSG1-RESP2)
010080     END-EXEC
010090
010100     EVALUATE TRUE
010110         WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
010120         WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
010130              CONTINUE
010140         WHEN OTHER
010150              SET ERRO-CONTAINER TO TRUE
010160              PERFORM 999002-TRATA-CONDITION-CICS
010170     END-EVALUATE
010180
010190*    Excluir o container de erro
010200     PERFORM 983100-EXCLUIR-CONTAINER-ERRO
010210     .
010220 983000-SAI.
010230     EXIT.
010240*
010250*----------------------------------------------------------------*
010260 983100-EXCLUIR-CONTAINER-ERRO   SECTION.
010270*----------------------------------------------------------------*
010280
010290*    Limpar variaveis container de erro
010300     INITIALIZE IIBKERRO-PRM-ERRO
010310
010320*    Exclui o container de erro
010330     MOVE 0017                   TO MSG1-CD-SEQL-ERRO
010340     MOVE 'PRM-ERRO'             TO NM-CTNR
010350     SET DEL-CONTAINER           TO TRUE
010360
010370*    Não há captura automática de erro CICS por usar RESP e RESP2
010380     EXEC CICS DELETE
010390               CONTAINER (NM-CTNR)
010400               RESP      (MSG1-RESP1)
010410               RESP2     (MSG1-RESP2)
010420     END-EXEC
010430
010440     EVALUATE TRUE
010450         WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
010460         WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
010470              CONTINUE
010480         WHEN OTHER
010490              SET ERRO-CONTAINER TO TRUE
010500              PERFORM 999002-TRATA-CONDITION-CICS
010510     END-EVALUATE
010520     .
010530 983100-SAI.
010540     EXIT.
010550*----------------------------------------------------------------*
010560 984000-GRAVAR-CONTAINER-ERRO    SECTION.
010570*----------------------------------------------------------------*
010600*    Grava o erro gerado no container de erro
010610*    Não há captura automática de erro CICS por usar RESP e RESP2
010620     EXEC CICS PUT
010630               CONTAINER ( 'PRM-ERRO'                  )
010640               FROM      ( IIBKERRO-PRM-ERRO           )
010650               FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
010660               RESP      (MSG1-RESP1                   )
010670               RESP2     (MSG1-RESP2                   )
010680     END-EXEC
010690
010700     .
010710 984000-SAI.
010720     EXIT.
010730

010850*----------------------------------------------------------------*
010860*                                                                *
010870* As sections abaixo iniciadas com 999000 representam os tratamen*
010880* tos de erro utilizando o Sistema MSG (rotina MSGS0001)         *
010890*                                                                *
010900*----------------------------------------------------------------*
010910 998100-ERRO                     SECTION.
010920*----------------------------------------------------------------*
010930
           MOVE 0018                   TO MSG1-CD-SEQL-ERRO

010940     IF MSG1-CD-ERRO-TS EQUAL ZEROS
010950        MOVE 8888                TO MSG1-CD-ERRO-TS
010960     END-IF

010970     IF MSG1-TX-ERRO-TS EQUAL SPACES
010980        MOVE 'Erro não específicado no módulo ' TO MSG1-TX-ERRO-TS
010990     END-IF

011000     PERFORM 999000-GRAVA-ERRO-E-ENCERRA
011010     .
011020 998100-SAI.
011030     EXIT.
011040
011050*----------------------------------------------------------------*
011060*     As sections abaixo representam os tratamentos de erro
011070*    utilizando o sistema MSG de mensageria (rotina MSGS0001)
011080*-----------------------------------------------------------------
011090 999000-GRAVA-ERRO-E-ENCERRA     SECTION.
011100*-----------------------------------------------------------------
011110
           MOVE 0019                   TO MSG1-CD-SEQL-ERRO

011120*    Em caso de erro, desfaz as atualizações em banco de dados
011130
011140     EXEC CICS SYNCPOINT
011150               ROLLBACK
011160     END-EXEC
011170
011180*Verifica se o tipo de erro gerado possui tratamento interno
011190     EVALUATE TRUE
011200         WHEN ERRO-CONTAINER
011210              PERFORM 999004-TRATA-ERRO-CTNR
011220         WHEN ERRO-TAMANHO-AREA
011230              PERFORM 999005-TRATA-ERRO-AREA-DADOS
011240         WHEN ERRO-CANAL
011250              PERFORM 999006-TRATA-ERRO-CANAL
011260         WHEN OTHER
011270              CONTINUE
011280     END-EVALUATE
011290
011300*Adiciona as variáveis de entrada para monitoração
011310*
011320     MOVE 'SGTS0104-CD-OPR-ETRD-MCP'    TO MSGKW999-NM-VRV-MNT
           IF  SGTS0104-CD-OPR-ETRD-MCP       IS NUMERIC
011340         MOVE  SGTS0104-CD-OPR-ETRD-MCP TO CD-OPR-ETRD-MCP
011350         MOVE CD-OPR-ETRD-MCP-TX        TO MSGKW999-VL-VRV-MNT
           ELSE
               MOVE 'Valor não numérico.'     TO MSGKW999-VL-VRV-MNT
           END-IF
011360     PERFORM 999003-ADICIONA-VRV-MNT
011370
011380     MOVE 'SGTS0104-NR-VRS-OPR-ETRD'   TO MSGKW999-NM-VRV-MNT
011400     MOVE SGTS0104-NR-VRS-OPR-ETRD     TO MSGKW999-VL-VRV-MNT
011420     PERFORM 999003-ADICIONA-VRV-MNT
011430
011440     MOVE 'SGTS0104-CD-PSS-CTT-EXNO'   TO MSGKW999-NM-VRV-MNT
011460     MOVE SGTS0104-CD-PSS-CTT-EXNO     TO MSGKW999-VL-VRV-MNT
011480     PERFORM 999003-ADICIONA-VRV-MNT
011560*
011580*    Grava o erro gerado no sistema MSG - Mensageria
011590     PERFORM MSG1-NTF-MSG
011600
011610     MOVE MSG1-CD-ERRO-TS        TO IIBKERRO-CD-ERRO
011620     MOVE MSG1-TX-ERRO-TS        TO IIBKERRO-TX-ERRO
011630
011640*    Grava o erro gerado no container de erro
011650     PERFORM 984000-GRAVAR-CONTAINER-ERRO
011660
011670*    Encerra a transacao
011680     PERFORM 999999-ENCERRAR
011690     .
011700 999000-SAI.
011710     EXIT.
011720* --------------------------------------*
011730* 999001-TRATA-ABEND-CICS        SECTION.
011740* 999002-TRATA-CONDITION-CICS    SECTION.
011750* 999003-ADICIONA-VRV-MNT        SECTION.
      * 999004-ALINHA-NUMERO-ESQUERDA  SECTION.
011760* --------------------------------------*
-INC HLPKP999
011780* --------------------------------------*
011790* 999004-TRATA-ERRO-CTNR         SECTION.
011800* 999005-TRATA-ERRO-AREA-DADOS   SECTION.
011810* 999006-TRATA-ERRO-CANAL        SECTION.
011820*---------------------------------------*
-INC SGTKP999
011840* --------------------------------------*
011850* MSG1-NTF-MSG                   SECTION.
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
011860* --------------------------------------*
-INC MSGKPMG1
      * 999010-TRATA-ERRO-DB2          SECTION.
-INC HLPKP997

010740*----------------------------------------------------------------*
010750 999999-ENCERRAR                 SECTION.
010760*----------------------------------------------------------------*
010770
008790     GOBACK
010810     .
010820 999999-SAI.
010830     EXIT.
010840*
011880*---------------------- FIM SGTS0104 -----------------------------