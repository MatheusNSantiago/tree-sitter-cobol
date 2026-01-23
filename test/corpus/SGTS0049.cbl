000010*-----------------------------------------------------------------
000020* PROGRAMA..: SGTS0049
000030* ANALISTA..: F2054109 - Cintia Ap. Soares Iglesias
000040* AUTOR.....: F6804288 - Marcondes Marques Junior
000050* DATA......: 01/12/2015
000060* OBJETIVO..: Listar versoes de Agências de Classificação Externa
000070*             de Risco e Estados.
      * COMPILACAO: 52 - PSOSE600 - Cobol 4.2 c/otimizacao p/producao
000090*-----------------------------------------------------------------
000100* VRS002 11/03/2019 F9188847 de 2016/145195 - Inclusão monitoração
      *          variáveis para rotina SBDATAF
      *          Acerto mensageria
000100* VRS001 01/12/2015 F6804288 de 2013/120821 - Implantação.
000110*-----------------------------------------------------------------
000120
000130*-----------------------------------------------------------------
000140 IDENTIFICATION DIVISION.
000150*----------------------------------------------------------------*
000160 PROGRAM-ID. SGTS0049.
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
000360 77  CTE-INICIO-SS               PIC X(040) VALUE
000370              '*** S.S. COMECA AQUI ***'.
000380
000390 77  CTE-PROG                    PIC X(018) VALUE
000400              '*** SGTS0049 ***'.
000410 77  CTE-VERS                    PIC X(006) VALUE 'VRS002'.
000420
000430* Módulos chamados
000440 77  SGTSLJ07                    PIC X(008) VALUE 'SGTSLJ07'.
000450 77  SBDATAF                     PIC X(008) VALUE 'SBDATAF'.
000460*----------------------------------------------------------------*
000470 LOCAL-STORAGE                   SECTION.
000480*----------------------------------------------------------------*
000490* Areas de trabalho
000500
000510 01  ACAO                        PIC S9(004) COMP-5 VALUE 0.
000520
000530*--- Indices
000540*--- da lista unica acessada na ação 2 ou 3
000550 01  IC-TAB                      PIC S9(004) COMP-5 VALUE 0.
000560*--- da tabela interna de listas acessadas nas açoes 2 e 3
000570 01  IC-TAB1                     PIC S9(004) COMP-5 VALUE 0.
000580
000590*--- Campos utilizados validacao de datas
000600
000610*--- Valida data Gregoriana.
       01  SBDATAF-FIM-WORKAREA        PIC  X(20) VALUE
                                                 'SBDATAF -WRKAREA-FIM'.
       01  SBDATAF-FUNCAO              PIC  X(0003) VALUE 'F01'.

       01  AREA-WORK                   PIC  X(4095).
       01  AREA-RED                    REDEFINES AREA-WORK.
           03  A-TAM                   PIC  9(0008) COMP.
           03  FILLER                  PIC  X(4071).
           03  A-FIM                   PIC  X(0020).
      *--> Argumento p/ informar a data no formato gregoriano (DDMMAAA)
000620 01  SBDATAF-FUNCAO-01           PIC X(003) VALUE 'F01'.
000630 01  SBDATAF-ARG01-01            PIC 9(008) VALUE 000.
000640 01  SBDATAF-ARG01-01-R          REDEFINES  SBDATAF-ARG01-01.
000650     03  DIA                     PIC 9(002).
000660     03  MES                     PIC 9(002).
000670     03  ANO                     PIC 9(004).
      *--> Argumento da função 01, p/ validação data informada em ARG01.
000680 01  SBDATAF-ARG02-01            PIC 9(001) VALUE 004.
000690
000700 01  DATA-AMD                    PIC X(010) VALUE ' '.
000710 01  DATA-AMD-R                  REDEFINES  DATA-AMD.
000720     03  ANO                     PIC 9(004).
000730     03  HIFEN1                  PIC X(001).
000740     03  MES                     PIC 9(002).
000750     03  HIFEN2                  PIC X(001).
000760     03  DIA                     PIC 9(002).
000770
000780 01  DATA-PSQ-AMD                PIC X(008) VALUE ' '.
000790 01  DATA-PSQ-AMD-R              REDEFINES  DATA-PSQ-AMD.
000800     03  DIA                     PIC 9(002).
000810     03  MES                     PIC 9(002).
000820     03  ANO                     PIC 9(004).
000830
000840 01  DATA-INC-PSQ-AMD            PIC X(008) VALUE ' '.
000850 01  DATA-INC-PSQ-AMD-R          REDEFINES  DATA-INC-PSQ-AMD.
000860     03  ANO                     PIC 9(004).
000870     03  MES                     PIC 9(002).
000880     03  DIA                     PIC 9(002).
000890
000900 01  DATA-ULT-TS-AMD             PIC X(008) VALUE ' '.
000910 01  DATA-ULT-TS-AMD-R           REDEFINES  DATA-ULT-TS-AMD.
000920     03  ANO                     PIC 9(004).
000930     03  MES                     PIC 9(002).
000940     03  DIA                     PIC 9(002).
000950
000960*----------------------------------------------------------------*
000970* Book dos Containers e das subrotinas chamadas
000980*----------------------------------------------------------------*
000990
001000* Book das variáveis auxiliares do tratamento de erro via MSG
=INC SGTKERR1                                                           00001010
001020
001030* Book para captura de abend cics da bloco codigo MSGKP999
=INC HLPKW999
001050
001060* Book para captura de erro via MSG1
=INC MSGKWMG1                                                           00001070
001080
001090* Book do container de requisição e resposta deste modulo
001100 01  SGTS0049-DADOS.
=INC SGTK0049                                                           00001110
001120
001130* Book lista estados de característica da carteira
001140 01  SGTSLJ07-DADOS.
=INC SGTKLJ07                                                           00001150
001160
001170* Book do Container de Erro
=INC IIBKERRO                                                           00001180
001190
001200
001210 01  CTE-FINAL-SS                PIC X(040) VALUE
001220              '*** S.S. TERMINA AQUI ***'.
001230
001240*----------------------------------------------------------------*
001250 PROCEDURE DIVISION.
001260*----------------------------------------------------------------*
001270
001280*----------------------------------------------------------------*
001290 000000-ROTINA-PRINCIPAL         SECTION.
001300*----------------------------------------------------------------*
001310
001320     PERFORM 100000-INICIAR
001330
001340     PERFORM 200000-PROCESSAR
001350
001360     PERFORM 990000-ENCERRAR

           GOBACK
001370     .
001380 000000-SAI.
001390     EXIT.
001400
001410*----------------------------------------------------------------*
001420 100000-INICIAR                  SECTION.
001430*----------------------------------------------------------------*
001440
001450     MOVE 0001                   TO MSG1-CD-SEQL-ERRO
001460
001470* Ativa a captura automatica de abend do CICS.
001480* WHENEVER CICS
=INC HLPKE999
001500
001510     PERFORM 970000-LER-CANAL
001520
001530*--- Define o nome do programa p/ fins manipulacao container
001540     MOVE 'SGTS0049'             TO NM-PGM
001550     MOVE 'SGTS0049-RPST'        TO NM-CTNR
001560
001570*--- Limpa a resposta e erro antes de executar o modulo
001580     PERFORM 983000-EXCLUIR-RESPOSTA-ERRO
001590
001600*--- Recupera a requisição do modulo
001610     PERFORM 971000-LER-REQUISICAO
001620
001630     PERFORM 110000-VALIDAR-REQUISICAO
001640     .
001650 100000-SAI.
001660     EXIT.
001670
001680*----------------------------------------------------------------*
001690 110000-VALIDAR-REQUISICAO       SECTION.
001700*----------------------------------------------------------------*
001710     MOVE 0002                   TO MSG1-CD-SEQL-ERRO
001720
001730     IF  SGTS0049-TS-EST-ULT     = SPACES
001740         MOVE '2900-12-31-01.00.00.000000'
001750                                 TO SGTS0049-TS-EST-ULT
001760     END-IF
001770
001780     IF  SGTS0049-CD-TIP-EST      < ZEROS
001790         SET ERRO-PROGRAMADO     TO TRUE
001800         MOVE 0077               TO MSG1-CD-ERRO-TS
001810         MOVE 'Código do tipo de estado da decisão inválido..'
001820                                 TO MSG1-TX-ERRO-TS
001830
001840         PERFORM 999000-ERRO
001850     END-IF
001860
001870*--- Valida data início da pesquisa

001880     IF  SGTS0049-DT-INC-PSQ  NOT = SPACES

001890         MOVE CORR SGTS0049-DT-INC-PSQ-R
001900                                 TO SBDATAF-ARG01-01-R
001910
001920         PERFORM  910000-CHAMAR-SBDATAF
001930
001940         IF RETURN-CODE          NOT EQUAL  ZEROS
001950            SET ERRO-PROGRAMA-ACIONADO  TO TRUE
001960            MOVE 0761            TO MSG1-CD-ERRO-TS
                  MOVE RETURN-CODE     TO MSG1-CD-RTN-PGM-ACND
001970            MOVE 'Data início da pesquisa inválida.'
001980                                 TO MSG1-TX-ERRO-TS
                                          MSG1-TX-RTN-PGM-ACND
                  MOVE  SBDATAF       TO  MSG1-NM-PGM-ACND
                                          NM-PGM
                  PERFORM 969000-MONITORAR-VARIAVEIS
002000            PERFORM 999000-ERRO
002010         END-IF
002020     END-IF
002030
002040*--- Valida data fim da pesquisa
002050     IF  SGTS0049-DT-FIM-PSQ  NOT = SPACES
002060         MOVE CORR SGTS0049-DT-FIM-PSQ-R
002070                                 TO SBDATAF-ARG01-01-R
002080
002090         PERFORM  910000-CHAMAR-SBDATAF
002100
002110         IF RETURN-CODE          NOT EQUAL  ZEROS
002120            SET ERRO-PROGRAMA-ACIONADO  TO TRUE
002130            MOVE 0762            TO MSG1-CD-ERRO-TS
                  MOVE RETURN-CODE     TO MSG1-CD-RTN-PGM-ACND
002140            MOVE 'Data fim da pesquisa inválida.'
002150                                 TO MSG1-TX-ERRO-TS
                                          MSG1-TX-RTN-PGM-ACND
                  MOVE  SBDATAF       TO  MSG1-NM-PGM-ACND
                                          NM-PGM
                  PERFORM 969000-MONITORAR-VARIAVEIS
002170            PERFORM 999000-ERRO
002180         END-IF
002190     END-IF
002200     .
002210 110000-SAI.
002220     EXIT.
002230
002240*----------------------------------------------------------------*
002250 200000-PROCESSAR                SECTION.
002260*----------------------------------------------------------------*
002270     MOVE 0003                   TO MSG1-CD-SEQL-ERRO
002280
002290*--- Lista versoes de agências clas. de acordo com as datas de
002300*--- início e fim da pesquisa informados
002310     PERFORM 210000-TRATAR-POR-DATA-PSQ
002320
002330*--- Monta resposta ao chamador
002340     PERFORM 899000-MOVIMENTAR-SAIDA
002350     .
002360 200000-SAI.
002370     EXIT.
002380
002390*----------------------------------------------------------------*
002400 210000-TRATAR-POR-DATA-PSQ      SECTION.
002410*----------------------------------------------------------------*
002420     MOVE 0004                   TO MSG1-CD-SEQL-ERRO
002430
002440     IF  SGTS0049-DT-INC-PSQ     = SPACES AND
002450         SGTS0049-DT-FIM-PSQ NOT = SPACES
002460         MOVE '01011900'         TO SGTS0049-DT-INC-PSQ
002470     END-IF
002480
002490     IF  SGTS0049-DT-INC-PSQ NOT = SPACES AND
002500         SGTS0049-DT-FIM-PSQ     = SPACES
002510         MOVE '31122900'       TO SGTS0049-DT-FIM-PSQ
002520     END-IF
002530
002540     IF  SGTS0049-DT-INC-PSQ     = SPACES AND
002550         SGTS0049-DT-FIM-PSQ     = SPACES
002560         PERFORM 210100-LISTAR-SEM-DATA
002570     ELSE
002580         PERFORM 210200-LISTAR-COM-DATA
002590     END-IF
002600     .
002610 210000-SAI.
002620     EXIT.
002630
002640*----------------------------------------------------------------*
002650 210100-LISTAR-SEM-DATA          SECTION.
002660*----------------------------------------------------------------*
002670     MOVE 0005                   TO MSG1-CD-SEQL-ERRO
002680
002690     MOVE SPACES                 TO SGTSLJ07-DADOS
002700     INITIALIZE  SGTSLJ07-DADOS
002710         REPLACING NUMERIC       BY  ZEROS
002720
002730*--- Ação 1 - Listar ultimo Estado da Agência de Classificação
002740*             Externa de Risco
002750     MOVE 1                      TO ACAO
002760
002770     PERFORM 920000-CHAMAR-SGTSLJ07
002780     .
002790 210100-SAI.
002800     EXIT.
002810
002820*----------------------------------------------------------------*
002830 210200-LISTAR-COM-DATA          SECTION.
002840*----------------------------------------------------------------*
002850     MOVE 0006                   TO MSG1-CD-SEQL-ERRO
002860
002870     MOVE '-'                    TO HIFEN1
002880                                    HIFEN2
002890
002900     MOVE CORR SGTS0049-TS-EST-ULT-R
002910                                 TO DATA-ULT-TS-AMD-R
002920     MOVE CORR SGTS0049-DT-INC-PSQ-R
002930                                 TO DATA-INC-PSQ-AMD-R
002940
002950     IF DATA-ULT-TS-AMD          >= DATA-INC-PSQ-AMD
002960*------ Executa Ação 2
002970        PERFORM 210210-TRATAR-LS-DATA-ACAO-02
002980        PERFORM 210220-VERIFICAR-ACAO-02
002990     ELSE
003000*------ Executa Ação 3
003010        PERFORM 210230-TRATAR-LS-DATA-ACAO-03
003020        MOVE SGTSLJ07-IN-CNTO-PSQ
003030                                 TO SGTS0049-IN-CTND-PSQ
003040     END-IF
003050     .
003060 210200-SAI.
003070     EXIT.
003080
003090*----------------------------------------------------------------*
003100 210210-TRATAR-LS-DATA-ACAO-02   SECTION.
003110*----------------------------------------------------------------*
003120     MOVE 0007                   TO MSG1-CD-SEQL-ERRO
003130
003140     MOVE SPACES                 TO SGTSLJ07-DADOS
003150     INITIALIZE  SGTSLJ07-DADOS
003160         REPLACING NUMERIC       BY  ZEROS
003170
003180*--- Ação 2 - Listar Estados de Agência de Classificação Externa
003190*             em um período
003200     MOVE 2                      TO ACAO
003210     MOVE CORR SGTS0049-DT-INC-PSQ-R
003220                                 TO DATA-AMD-R
003230     MOVE DATA-AMD               TO SGTSLJ07-DT-INC-PSQ
003240     MOVE CORR SGTS0049-DT-FIM-PSQ-R
003250                                 TO DATA-AMD-R
003260     MOVE DATA-AMD               TO SGTSLJ07-DT-FIM-PSQ
003270
003280     PERFORM 920000-CHAMAR-SGTSLJ07
003290     .
003300 210210-SAI.
003310     EXIT.
003320
003330*----------------------------------------------------------------*
003340 210220-VERIFICAR-ACAO-02        SECTION.
003350*----------------------------------------------------------------*
003360     MOVE 0008                   TO MSG1-CD-SEQL-ERRO
003370
003380     IF SGTSLJ07-QT-OCR-TAB       > 0
003390
003400*------ Ação 2 contendo mais registros a enviar
003410        IF SGTSLJ07-TEM-MAIS-REGISTRO
003420           MOVE 'S'              TO SGTS0049-IN-CTND-PSQ
003430
003440*------ Ação 2 sem mais registros a enviar, verifica ação 3
003450        ELSE
003460           PERFORM 210225-VERIFICAR-ACAO-03
003470        END-IF
003480
003490*--- Ação 2 sem registros, executa ação 3
003500     ELSE
003510        PERFORM 210230-TRATAR-LS-DATA-ACAO-03
003520        MOVE SGTSLJ07-IN-CNTO-PSQ
003530                                 TO SGTS0049-IN-CTND-PSQ
003540     END-IF
003550     .
003560 210220-SAI.
003570     EXIT.
003580
003590*----------------------------------------------------------------*
003600 210225-VERIFICAR-ACAO-03        SECTION.
003610*----------------------------------------------------------------*
003620     MOVE 0009                   TO MSG1-CD-SEQL-ERRO
003630
003640*--- Ação 2 com 100 ocorrências, executa ação 3 para verificar
003650*--- se há mais registros
003660     IF SGTSLJ07-QT-OCR-TAB      = 100
003670        PERFORM 210230-TRATAR-LS-DATA-ACAO-03
003680        IF SGTSLJ07-QT-OCR-TAB   > 0
003690           MOVE 'S'     TO SGTS0049-IN-CTND-PSQ
003700        ELSE
003710           MOVE 'N'     TO SGTS0049-IN-CTND-PSQ
003720        END-IF
003730
003740*--- Ação 2 com menos ocorrências, executa ação 3 para
003750*--- verificar se há mais registros
003760     ELSE
003770        PERFORM 890000-PREPARAR-SAIDA
003780        PERFORM 210230-TRATAR-LS-DATA-ACAO-03
003790
003800*------ Não há ocorrências na ação 3, encerra a pesquisa
003810        IF SGTSLJ07-QT-OCR-TAB = 0
003820           MOVE 'N'        TO SGTS0049-IN-CTND-PSQ
003830
003840*------ Há ocorrências, prepara tabela interna acrescentando
003850*------ registros da ação 3
003860        ELSE
003870           PERFORM 890000-PREPARAR-SAIDA
003880           IF IC-TAB1      <= 100
003890              MOVE 'N'     TO SGTS0049-IN-CTND-PSQ
003900           ELSE
003910*------- Ajuste do total de ocorrências para enviar ao chamador
003920              MOVE 100     TO IC-TAB1
003930              MOVE 'S'     TO SGTS0049-IN-CTND-PSQ
003940           END-IF
003950        END-IF
003960     END-IF
003970     .
003980 210225-SAI.
003990     EXIT.
004000
004010*----------------------------------------------------------------*
004020 210230-TRATAR-LS-DATA-ACAO-03   SECTION.
004030*----------------------------------------------------------------*
004040     MOVE 0010                   TO MSG1-CD-SEQL-ERRO
004050
004060     MOVE SPACES                 TO SGTSLJ07-DADOS
004070     INITIALIZE  SGTSLJ07-DADOS
004080         REPLACING NUMERIC BY  ZEROS
004090
004100*--- Ação 3 - Listar Agência de Classificação Externa de
004110*             Risco antes de uma data
004120     MOVE 3                      TO ACAO
004130     MOVE CORR SGTS0049-DT-INC-PSQ-R
004140                                 TO DATA-AMD-R
004150     MOVE DATA-AMD               TO SGTSLJ07-DT-PSQ-R
004160
004170     PERFORM 920000-CHAMAR-SGTSLJ07
004180
004190     .
004200 210220-SAI.
004210     EXIT.
004220
004230*----------------------------------------------------------------*
004240 890000-PREPARAR-SAIDA           SECTION.
004250*----------------------------------------------------------------*
004260     MOVE 0011                   TO MSG1-CD-SEQL-ERRO
004270
004280*--- Monta saída a partir das listas acessadas nas Açoes 2 e 3
004290     PERFORM VARYING IC-TAB FROM 1 BY 1
004300             UNTIL IC-TAB        > SGTSLJ07-QT-OCR-TAB
004310
004320       ADD 1                     TO IC-TAB1
004330
004340       IF  IC-TAB1               <= 100
004350         MOVE SGTSLJ07-CD-AG-CLSC-RSCO      (IC-TAB)
004360           TO SGTS0049-CD-AG-CLSC-RSCO      (IC-TAB1)
004370         MOVE SGTSLJ07-NR-ETRD-AG-CLSC      (IC-TAB)
004380           TO SGTS0049-NR-ETRD-AG-CLSC      (IC-TAB1)
004390         MOVE SGTSLJ07-CD-TIP-EST-DCS-RPST  (IC-TAB)
004400           TO SGTS0049-CD-TIP-ULT-EST       (IC-TAB1)
004410         MOVE SGTSLJ07-TS-EST-AG-CLSC-RPST  (IC-TAB)
004420           TO SGTS0049-TS-ULT-EST           (IC-TAB1)
004430         MOVE SGTSLJ07-NM-AG-CLSC-RSCO-RPST (IC-TAB)
004440           TO SGTS0049-NM-AG-CLSC-RSCO      (IC-TAB1)
004450         MOVE SGTSLJ07-IN-AG-CLSC-ATV-RSCO  (IC-TAB)
004460           TO SGTS0049-IN-AG-CLSC-ATV-RSCO  (IC-TAB1)
004470
004480       ELSE
004490*------- Limite de carga da tabela, posiciona índice para sair do
004500*------- procedimento
004510         MOVE 101                TO IC-TAB
004520       END-IF
004530     END-PERFORM
004540     .
004550 890000-SAI.
004560     EXIT.
004570
004580*----------------------------------------------------------------*
004590 899000-MOVIMENTAR-SAIDA         SECTION.
004600*----------------------------------------------------------------*
004610     MOVE 0012                   TO MSG1-CD-SEQL-ERRO
004620
004630*--- Ocorrências acesso acoes 2 e 3 de retorno
004640     IF IC-TAB1                  > 0
004650       MOVE IC-TAB1              TO SGTS0049-QT-OCR-TAB
004660
004670*--- Acessos diretos das acoes 2 ou 3
004680     ELSE
004690       MOVE SGTSLJ07-QT-OCR-TAB  TO SGTS0049-QT-OCR-TAB
004700       MOVE SGTSLJ07-IN-CNTO-PSQ TO SGTS0049-IN-CTND-PSQ
004710
004720       PERFORM VARYING IC-TAB FROM 1 BY 1
004730               UNTIL IC-TAB > SGTSLJ07-QT-OCR-TAB
004740         MOVE SGTSLJ07-CD-AG-CLSC-RSCO      (IC-TAB)
004750           TO SGTS0049-CD-AG-CLSC-RSCO      (IC-TAB)
004760         MOVE SGTSLJ07-NR-ETRD-AG-CLSC      (IC-TAB)
004770           TO SGTS0049-NR-ETRD-AG-CLSC      (IC-TAB)
004780         MOVE SGTSLJ07-CD-TIP-EST-DCS-RPST  (IC-TAB)
004790           TO SGTS0049-CD-TIP-ULT-EST       (IC-TAB)
004800         MOVE SGTSLJ07-TS-EST-AG-CLSC-RPST  (IC-TAB)
004810           TO SGTS0049-TS-ULT-EST           (IC-TAB)
004820         MOVE SGTSLJ07-NM-AG-CLSC-RSCO-RPST (IC-TAB)
004830           TO SGTS0049-NM-AG-CLSC-RSCO      (IC-TAB)
004840         MOVE SGTSLJ07-IN-AG-CLSC-ATV-RSCO  (IC-TAB)
004850           TO SGTS0049-IN-AG-CLSC-ATV-RSCO  (IC-TAB)
004860       END-PERFORM
004870     END-IF
004880
004890*--- Grava o container de resposta da subrotina
004900     PERFORM 980000-GRAVAR-CONTAINER-RPST
004910     .
004920 899000-SAI.
004930     EXIT.
004940
004950*----------------------------------------------------------------*
004960*                                                                *
004970*  SECTION DA FAIXA DE 900000 A 969900 PARA DEMAIS ROTINAS       *
004980*                                                                *
004990*----------------------------------------------------------------*
005000*----------------------------------------------------------------*
005010 910000-CHAMAR-SBDATAF            SECTION.
005020*----------------------------------------------------------------*
005030     MOVE 0013                   TO MSG1-CD-SEQL-ERRO

005040     MOVE 4095                   TO A-TAM
           MOVE SBDATAF-FIM-WORKAREA   TO A-FIM

005050     CALL  SBDATAF                USING SBDATAF-FUNCAO
                                              AREA-WORK
005060                                        SBDATAF-ARG01-01
005070                                        SBDATAF-ARG02-01
005080     END-CALL


005090     .
005100 910000-SAI.
005110     EXIT.
005120
005130*----------------------------------------------------------------*
005140 920000-CHAMAR-SGTSLJ07          SECTION.
005150*----------------------------------------------------------------*
005160     MOVE 0014                   TO MSG1-CD-SEQL-ERRO
005170
005180     MOVE ACAO                   TO SGTSLJ07-CD-PSQ
005190     MOVE SGTS0049-TS-EST-ULT    TO SGTSLJ07-TS-EST-AG-CLSC
005200     MOVE SGTS0049-NM-AG-CLSC-RSCO-E
005210                                 TO SGTSLJ07-NM-AG-CLSC-RSCO
005220     MOVE SGTS0049-CD-TIP-EST    TO SGTSLJ07-CD-TIP-EST-DCS
005230
005240*--- Mover nome do módulo para monitoração de variáveis
005250     MOVE SGTSLJ07               TO NM-PGM

           EXEC CICS LINK
                PROGRAM  (SGTSLJ07)
                COMMAREA (SGTSLJ07-DADOS)
           END-EXEC
005260
005270*    MOVE LENGTH OF SGTSLJ07-DADOS TO EIBCALEN
005280*    CALL SGTSLJ07
005290*         USING DFHEIBLK SGTSLJ07-DADOS
005300*    END-CALL
005310
005320     IF  SGTSLJ07-CD-RTN-PGM     NOT = ZEROS
005330         SET ERRO-PROGRAMA-ACIONADO TO TRUE
005340         MOVE SGTSLJ07-CD-RTN-PGM  TO MSG1-CD-ERRO-TS
                                            MSG1-CD-RTN-PGM-ACND
005350         MOVE SGTSLJ07-MSG-ERRO-PGM
005360                                 TO MSG1-TX-RTN-PGM-ACND
005370                                    MSG1-TX-ERRO-TS
005380         MOVE NM-PGM             TO MSG1-NM-PGM-ACND
005410*------- Monitora variáveis do módulo chamado
005420         PERFORM 969000-MONITORAR-VARIAVEIS
005430         PERFORM 999000-ERRO
005440     END-IF
005450     .
005460 920000-SAI.
005470     EXIT.
005480
005490*----------------------------------------------------------------*
005500 969000-MONITORAR-VARIAVEIS      SECTION.
005510*----------------------------------------------------------------*
005520* Monitora as variáveis dos módulos chamados(Mensageria IIB)
005530*----------------------------------------------------------------*
005560     EVALUATE NM-PGM

               WHEN SBDATAF
                   MOVE 'SBDATAF-FUNCAO'  TO MSGKW999-NM-VRV-MNT
                   MOVE SBDATAF-FUNCAO    TO MSGKW999-VL-VRV-MNT
                   PERFORM 999003-ADICIONA-VRV-MNT

                   MOVE 'SBDATAF-ARG01-01'  TO MSGKW999-NM-VRV-MNT
                   MOVE SBDATAF-ARG01-01    TO MSGKW999-VL-VRV-MNT
                   PERFORM 999003-ADICIONA-VRV-MNT

                   MOVE 'SBDATAF-ARG02-01'  TO MSGKW999-NM-VRV-MNT
                   MOVE SBDATAF-ARG02-01    TO MSGKW999-VL-VRV-MNT
                   PERFORM 999003-ADICIONA-VRV-MNT
005240
005240             MOVE 'A-TAM'        TO MSGKW999-NM-VRV-MNT
005240             MOVE A-TAM          TO MSGKW999-VL-VRV-MNT
005240             PERFORM 999003-ADICIONA-VRV-MNT
005240
005240             MOVE 'A-FIM'        TO MSGKW999-NM-VRV-MNT
005240             MOVE A-FIM          TO MSGKW999-VL-VRV-MNT
005240             PERFORM 999003-ADICIONA-VRV-MNT

005570         WHEN SGTSLJ07
005580             MOVE 'SGTSLJ07-CD-PSQ'
005590                                 TO MSGKW999-NM-VRV-MNT
005600             MOVE SGTSLJ07-CD-PSQ
005610                                 TO MSGKW999-VL-VRV-MNT
005620             PERFORM 999003-ADICIONA-VRV-MNT
005630
005640             MOVE 'SGTSLJ07-TS-EST-AG-CLSC'
005650                                 TO MSGKW999-NM-VRV-MNT
005660             MOVE SGTSLJ07-TS-EST-AG-CLSC
005670                                 TO MSGKW999-VL-VRV-MNT
005680             PERFORM 999003-ADICIONA-VRV-MNT
005690
005700             MOVE 'SGTSLJ07-NM-AG-CLSC-RSCO'
005710                                 TO MSGKW999-NM-VRV-MNT
005720             MOVE SGTSLJ07-NM-AG-CLSC-RSCO
005730                                 TO MSGKW999-VL-VRV-MNT
005740             PERFORM 999003-ADICIONA-VRV-MNT
005750
005760             MOVE 'SGTSLJ07-CD-TIP-EST-DCS'
005770                                 TO MSGKW999-NM-VRV-MNT
005780             MOVE SGTSLJ07-CD-TIP-EST-DCS
005790                                 TO MSGKW999-VL-VRV-MNT
005800             PERFORM 999003-ADICIONA-VRV-MNT
005810
005820             MOVE 'SGTSLJ07-DT-INC-PSQ'
005830                                 TO MSGKW999-NM-VRV-MNT
005840             MOVE SGTSLJ07-DT-INC-PSQ
005850                                 TO MSGKW999-VL-VRV-MNT
005860             PERFORM 999003-ADICIONA-VRV-MNT
005870
005880             MOVE 'SGTSLJ07-DT-FIM-PSQ'
005890                                 TO MSGKW999-NM-VRV-MNT
005900             MOVE SGTSLJ07-DT-FIM-PSQ
005910                                 TO MSGKW999-VL-VRV-MNT
005920             PERFORM 999003-ADICIONA-VRV-MNT
005930
005940             MOVE 'SGTSLJ07-DT-PSQ'
005950                                 TO MSGKW999-NM-VRV-MNT
005960             MOVE SGTSLJ07-DT-PSQ
005970                                 TO MSGKW999-VL-VRV-MNT
005980             PERFORM 999003-ADICIONA-VRV-MNT
005990
006000         WHEN OTHER
006010             CONTINUE
006020     END-EVALUATE
006030     .
006040 969000-SAI.
006050     EXIT.
006060
006070*----------------------------------------------------------------*
006080*                                                                *
006090* SECTION DA FAIXA DE 970000 A 979900 PARA LEITURA DE DADOS      *
006100*                                                                *
006110*----------------------------------------------------------------*
006120 970000-LER-CANAL                SECTION.
006130*----------------------------------------------------------------*
006140* Obtem o canal corrente
006150*----------------------------------------------------------------*
006160     MOVE 0015                   TO MSG1-CD-SEQL-ERRO
006170
006180*--- Por causa da captura automática de erro CICS, deve ser
006190*--- previsto o erro de container
006200     SET  ERRO-CANAL             TO TRUE
006210
006220     EXEC CICS ASSIGN
006230               CHANNEL ( NM-CNL )
006240     END-EXEC
006250
006260*--- Ao chegar aqui, há certeza de que não houve erro algum
006270     SET  SEM-ERRO               TO TRUE
006280     .
006290 970000-SAI.
006300     EXIT.
006310
006320*----------------------------------------------------------------*
006330 971000-LER-REQUISICAO           SECTION.
006340*----------------------------------------------------------------*
006350     MOVE 0016                   TO MSG1-CD-SEQL-ERRO
006360
006370*--- Formata o nome do container de requisicao
006380     MOVE 'SGTS0049'             TO NM-PGM
006390     MOVE 'SGTS0049-RQSC'        TO NM-CTNR
006400
006410*--- Recupera o tamanho do container de requisição
006420     PERFORM 972000-LER-CONTAINER-NODATA
006430
006440*--- Obtem o tamanho esperado da requisicao
006450     MOVE LENGTH OF SGTS0049-RQSC TO QT-CCT-EPRO
006460
006470*--- Valida tamanho esperado da requisicao X tamanho recebido
006480     IF  QT-CCT-LIDO             NOT = QT-CCT-EPRO
006490         SET ERRO-TAMANHO-AREA   TO TRUE
006500         PERFORM 999000-GRAVA-ERRO-E-ENCERRA
006510     END-IF
006520
006530     MOVE 0017                   TO MSG1-CD-SEQL-ERRO
006540
006550*--- Configura dados para leitura do container de requisição
006560     SET  GET-CONTAINER          TO TRUE
006570
006580*--- Por causa da captura automática de erro CICS, deve ser
006590*--- previsto o erro de container
006600     SET  ERRO-CONTAINER         TO TRUE
006610
006620*--- Efetua a leitura do container
006630     EXEC CICS GET
006640               CONTAINER ( NM-CTNR       )
006650               FLENGTH   ( QT-CCT-LIDO   )
006660               INTO      ( SGTS0049-RQSC )
006670     END-EXEC
006680
006690*--- Ao chegar aqui, há certeza de que não houve erro algum
006700     SET  SEM-ERRO               TO TRUE
006710     .
006720 971000-SAI.
006730     EXIT.
006740
006750*----------------------------------------------------------------*
006760 972000-LER-CONTAINER-NODATA     SECTION.
006770*----------------------------------------------------------------*
006790
006800*--- Configura dados para leitura do container de requisição
006810     SET  GET-CONTAINER          TO TRUE
006820
006830*--- Por causa da captura automática de erro CICS, deve ser
006840*--- previsto o erro de container
006850     SET  ERRO-CONTAINER         TO TRUE
006860
006870*--- Obtem a quantidade de caracteres no container
006880     MOVE ZEROS                  TO QT-CCT-LIDO
006890     EXEC CICS GET
006900               CONTAINER ( NM-CTNR     )
006910               NODATA
006920               FLENGTH   ( QT-CCT-LIDO )
006930     END-EXEC
006940
006950*--- Ao chegar aqui, há certeza de que não houve erro algum
006960     SET  SEM-ERRO               TO TRUE
006970     .
006980 972000-SAI.
006990     EXIT.
007000
007010*----------------------------------------------------------------*
007020*                                                                *
007030* SECTION DA FAIXA DE 980000 A 989900 PARA GRAVACAO DE DADOS     *
007040*                                                                *
007050*----------------------------------------------------------------*
007060 980000-GRAVAR-CONTAINER-RPST    SECTION.
007070*----------------------------------------------------------------*
007080
007090*--- Exclui eventual container de erro de outros programas
007100     PERFORM 983100-EXCLUIR-CONTAINER-ERRO
007110
007120     MOVE 0018                   TO MSG1-CD-SEQL-ERRO
007130
007140*--- Monta o nome do container de resposta
007150     MOVE 'SGTS0049-RPST'        TO NM-CTNR
007160     SET PUT-CONTAINER           TO TRUE
007170
007180*--- Por causa da captura automática de erro CICS, deve ser
007190*--- previsto o erro de container
007200     SET  ERRO-CONTAINER         TO TRUE
007210
007220     EXEC CICS PUT
007230               CONTAINER ( NM-CTNR                 )
007240               FROM      ( SGTS0049-RPST           )
007250               FLENGTH   ( LENGTH OF SGTS0049-RPST )
007260     END-EXEC
007270
007280*--- Ao chegar aqui, há certeza de que não houve erro algum
007290     SET  SEM-ERRO               TO TRUE
007300     .
007310 980000-SAI.
007320     EXIT.
007330
007340*---------------------------------------------------------------*
007350 983000-EXCLUIR-RESPOSTA-ERRO    SECTION.
007360*----------------------------------------------------------------*
007370* Exclui o container de resposta e de erro para evitar problemas
007380* em modulos executados mais de uma vez dentro da mesma sessao do
007390* CICS. Equivale ao initialize na area de resposta dos programas
007400* que utilizam COMMAREA.
007410*----------------------------------------------------------------*
007420     MOVE 0019                   TO MSG1-CD-SEQL-ERRO
007430
007440*--- Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
007450*--- (nome_modulo)-RPST
007460     EVALUATE NM-PGM
007470         WHEN 'SGTS0049'
007480             MOVE NM-PGM         TO NM-CTNR(1:8)
007490             MOVE '-RPST'        TO NM-CTNR(9:5)
007500         WHEN OTHER
007510             MOVE 'PRM-DADOS-RPST'
007520                                 TO NM-CTNR
007530     END-EVALUATE
007540
007550     SET DEL-CONTAINER           TO TRUE
007560
007570*--- Exclui o container de resposta
007580*--- Não há captura automática de erro CICS por usar RESP e RESP2
007590     EXEC CICS DELETE
007600               CONTAINER ( NM-CTNR    )
007610               RESP      ( MSG1-RESP1 )
007620               RESP2     ( MSG1-RESP2 )
007630     END-EXEC
007640
007650     EVALUATE TRUE
007660         WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
007670         WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
007680             CONTINUE
007690         WHEN OTHER
007700             SET ERRO-CONTAINER  TO TRUE
007710             PERFORM 999002-TRATA-CONDITION-CICS
007720     END-EVALUATE
007730
007740*--- Exclui o container de erro
007750     PERFORM 983100-EXCLUIR-CONTAINER-ERRO
007760     .
007770 983000-SAI.
007780     EXIT.
007790*
007800*----------------------------------------------------------------*
007810 983100-EXCLUIR-CONTAINER-ERRO   SECTION.
007820*----------------------------------------------------------------*
007830*--- Limpar variaveis container de erro
007840     MOVE SPACES                 TO IIBKERRO-PRM-ERRO
007850     INITIALIZE IIBKERRO-PRM-ERRO
007860         REPLACING NUMERIC       BY  ZEROS
007870
007880*--- Exclui o container de erro
007900     MOVE 'PRM-ERRO'             TO NM-CTNR
007910     SET DEL-CONTAINER           TO TRUE
007920
007930*--- Não há captura automática de erro CICS por usar RESP e RESP2
007940     EXEC CICS DELETE
007950               CONTAINER ( NM-CTNR    )
007960               RESP      ( MSG1-RESP1 )
007970               RESP2     ( MSG1-RESP2 )
007980     END-EXEC
007990
008000     EVALUATE TRUE
008010         WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
008020         WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
008030             CONTINUE
008040         WHEN OTHER
008050             SET ERRO-CONTAINER  TO TRUE
008060             PERFORM 999002-TRATA-CONDITION-CICS
008070     END-EVALUATE
008080     .
008090 983100-SAI.
008100     EXIT.
008110
008120*----------------------------------------------------------------*
008130 984000-GRAVAR-CONTAINER-ERRO    SECTION.
008140*----------------------------------------------------------------*
008160
008170*--- Grava o erro gerado no container de erro
008180*--- Não há captura automática de erro CICS por usar RESP e RESP2
008190     EXEC CICS PUT
008200               CONTAINER ( 'PRM-ERRO'                  )
008210               FROM      ( IIBKERRO-PRM-ERRO           )
008220               FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
008230               RESP      ( MSG1-RESP1                  )
008240               RESP2     ( MSG1-RESP2                  )
008250     END-EXEC
008260
008270     .
008280 984000-SAI.
008290     EXIT.
008300
008310*----------------------------------------------------------------*
008320 990000-ENCERRAR                 SECTION.
008330*----------------------------------------------------------------*
008340
008350     EXEC CICS
008360          RETURN
008370     END-EXEC
008380     .
008390 990000-ENCERRAR-SAI.
008400     EXIT.
008410*
008420*----------------------------------------------------------------*
008430*                                                                *
008440* As sections abaixo iniciadas com 999000 representam os tratamen*
008450* tos de erro utilizando o Sistema MSG (rotina MSGS0001)         *
008460*                                                                *
008470*----------------------------------------------------------------*
008480 999000-ERRO                     SECTION.
008490*----------------------------------------------------------------*
008500
008510     IF  MSG1-CD-ERRO-TS EQUAL ZEROS
008520         MOVE 8888                TO MSG1-CD-ERRO-TS
008530     END-IF
008540     IF  MSG1-TX-ERRO-TS EQUAL SPACES
008550         MOVE 'Erro não especificado no módulo,'
008560                                 TO MSG1-TX-ERRO-TS
008570     END-IF
008580     PERFORM 999000-GRAVA-ERRO-E-ENCERRA
008590     .
008600 999000-ERRO-SAI.
008610     EXIT.
008620
008630*----------------------------------------------------------------*
008640*    As sections abaixo representam os tratamentos de erro
008650*    utilizando o sistema MSG de mensageria (rotina MSGS0001)
008660*-----------------------------------------------------------------
008670 999000-GRAVA-ERRO-E-ENCERRA     SECTION.
008680*-----------------------------------------------------------------
008690
008700*--- Em caso de erro, desfaz as atualizaçoes em banco de dados
008710     EXEC CICS SYNCPOINT
008720               ROLLBACK
008730     END-EXEC
008740
008750*--- Verifica se o tipo de erro gerado possui tratamento interno
008760     EVALUATE TRUE
008770         WHEN ERRO-CONTAINER
008780             PERFORM 999004-TRATA-ERRO-CTNR
008790         WHEN ERRO-TAMANHO-AREA
008800             PERFORM 999005-TRATA-ERRO-AREA-DADOS
008810         WHEN ERRO-CANAL
008820             PERFORM 999006-TRATA-ERRO-CANAL
008830         WHEN OTHER
008840             CONTINUE
008850     END-EVALUATE
008860
008870*--- Adiciona as variáveis de entrada para monitoração

008890         MOVE 'SGTS0049-TS-EST-ULT'
008900                                 TO MSGKW999-NM-VRV-MNT
008910         MOVE SGTS0049-TS-EST-ULT
008920                                 TO MSGKW999-VL-VRV-MNT
008930         PERFORM 999003-ADICIONA-VRV-MNT
008940
008950         MOVE 'SGTS0049-NM-AG-CLSC-RSCO-E'
008960                                 TO MSGKW999-NM-VRV-MNT
008970         MOVE SGTS0049-NM-AG-CLSC-RSCO-E
008980                                 TO MSGKW999-VL-VRV-MNT
008990         PERFORM 999003-ADICIONA-VRV-MNT
009000
009010         MOVE 'SGTS0049-CD-TIP-EST'
009020                                 TO MSGKW999-NM-VRV-MNT
009030         MOVE SGTS0049-CD-TIP-EST
009040                                 TO MSGKW999-VL-VRV-MNT
009050         PERFORM 999003-ADICIONA-VRV-MNT
009060
009070         MOVE 'SGTS0049-DT-INC-PSQ'
009080                                 TO MSGKW999-NM-VRV-MNT
009090         MOVE SGTS0049-DT-INC-PSQ
009100                                 TO MSGKW999-VL-VRV-MNT
009110         PERFORM 999003-ADICIONA-VRV-MNT
009120
009130         MOVE 'SGTS0049-DT-FIM-PSQ'
009140                                 TO MSGKW999-NM-VRV-MNT
009150         MOVE SGTS0049-DT-FIM-PSQ
009160                                 TO MSGKW999-VL-VRV-MNT
009170         PERFORM 999003-ADICIONA-VRV-MNT

009190
009200*--- Grava o erro gerado no sistema MSG - Mensageria
009210     PERFORM MSG1-NTF-MSG
009220
009230*--- Movimenta Mensagem e Código de Erro para o Container de
009240*--- Erro
009250     MOVE MSG1-CD-ERRO-TS        TO IIBKERRO-CD-ERRO
009260     MOVE MSG1-TX-ERRO-TS        TO IIBKERRO-TX-ERRO
009270
009280*--- Grava o erro gerado no container de erro
009290     PERFORM 984000-GRAVAR-CONTAINER-ERRO
009300
009310*--- Encerra a transacao
009320     PERFORM 990000-ENCERRAR
009330     .
009340 999000-SAI.
009350     EXIT.
009360* --------------------------------------*
009370* 999001-TRATA-ABEND-CICS        SECTION.
009380* 999002-TRATA-CONDITION-CICS    SECTION.
009390* 999003-ADICIONA-VRV-MNT        SECTION.
009400* --------------------------------------*
=INC HLPKP999                                                           00009410
009420* --------------------------------------*
009430* 999004-TRATA-ERRO-CTNR         SECTION.
009440* 999005-TRATA-ERRO-AREA-DADOS   SECTION.
009450* 999006-TRATA-ERRO-CANAL        SECTION.
009460*---------------------------------------*
=INC SGTKP999                                                           00009470
009480* --------------------------------------*
009490* MSG1-NTF-MSG                   SECTION.
009500* --------------------------------------*
=INC MSGKPMG1                                                           00009510
009520*---------------------- FIM SGTS0049 -----------------------------