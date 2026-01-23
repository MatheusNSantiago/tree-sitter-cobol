000010CBL ARITH(EXTEND)
000020* ============================================================== *
000030* ============================================================== *
000040* -------------       S I S T E M A     S G T     -------------- *
000050* -------------        Sistema de Gestão TVM      -------------- *
000060* -------------------------------------------------------------- *
000070* PROGRAMA..: SGTSD099                                           *
000080* ANALISTA..: Christina C. G. R. Ribeiro                         *
000090* AUTOR.....: Christina C. G. R. Ribeiro                         *
000100* CHAVE.....: f2020988                                           *
000110* DATA......: 24.09.2018                                         *
000120* OBJETIVO..: Consultar dados do Ativo Financeiro                *
000130* -------------------------------------------------------------- *
000160* VRS001 - 24.09.18 - Christina  - IMPLANTACAO
000180* ============================================================== *
000190*
000200******************************************************************
000210 IDENTIFICATION                  DIVISION.
000220******************************************************************
000230*
000240 PROGRAM-ID.   SGTSD099.
000250*
000260******************************************************************
000270 ENVIRONMENT                     DIVISION.
000280******************************************************************
000290*
000300*----------------------------------------------------------------*
000310 CONFIGURATION                   SECTION.
000320*----------------------------------------------------------------*
000330 SPECIAL-NAMES.
000340     DECIMAL-POINT IS COMMA.
000350*
000360******************************************************************
000370 DATA                            DIVISION.
000380******************************************************************
000390*
000400*----------------------------------------------------------------*
000410 WORKING-STORAGE                 SECTION.
000420*----------------------------------------------------------------*
000430 77  CTE-PROG                    PIC  X(242) VALUE 'SGTSD099'.
000440 77  CTE-VERS                    PIC  X(006) VALUE 'VRS001'.
000450 77  SGTST099                    PIC  X(008) VALUE 'SGTST099'.
000460 77  SGTST101                    PIC  X(008) VALUE 'SGTST101'.
000480 77  SGTST103                    PIC  X(008) VALUE 'SGTST103'.
000490 77  SGTST109                    PIC  X(008) VALUE 'SGTST109'.
000500*
000930* Book do programa de ambiente - CICS5000
000940* Definido na work para preservar as informacoes
=INC HLPKE01B
000960*----------------------------------------------------------------*
000970 LOCAL-STORAGE             SECTION.
000980*----------------------------------------------------------------*
000510 01 WS-RESP-CICS.
000520     05 WS-RESP                  PIC 9(08) VALUE 0.
000530     05 WS-RESP2                 PIC 9(08) VALUE 0.
000540*
000550 01 GD-SQLCODE                   PIC -999.
000560 01 GDA-TIP-CLI                  PIC 9(004) VALUE ZEROS.
000560 01 GDA-TIP-PSS                  PIC 9(004) VALUE ZEROS.
000570 01 GDA-CLIENTE                  PIC 9(009) VALUE ZEROS.
000580 01 GDA-CPF-CGC                  PIC 9(014) VALUE ZEROS.
000590 01 GDA-CNPJ-EMISSOR             PIC 9(014) VALUE ZEROS.
000600 01 GDA-TS-TEMP                  PIC X(026) VALUE SPACES.
000610 01 FILLER REDEFINES GDA-TS-TEMP.
000620    03 GDA-TS-TEMP-FIL           PIC X(012).
000630    03 GDA-TS-TEMP-CNPJ          PIC 9(014).
000640*
000830*----------------------------------------------------------------*
000840*-- AUXILIARES
000850*----------------------------------------------------------------*
000860*
000870 01  I                           PIC  9(004) VALUE ZEROS.
000880 01  WIDX                        PIC  9(09)  VALUE ZEROS.
000890 01  WIND                        PIC  9(09)  VALUE ZEROS.
000900*
000910 01  GDA-SEQL-ERRO               PIC  9(004) VALUE ZEROS.
000920 01  LNK-RESP                    PIC S9(009) COMP.
000930 01  GDA-CHANNEL                 PIC  X(016) VALUE SPACES.
000940 01  IND                         PIC  9(09)  VALUE ZEROS.
000950*
000960*----------------------------------------------------------------*
000970*-- BOOK'S
000980*----------------------------------------------------------------*
000990*
001020*
001520 01 SGTST099-DADOS.
=INC SGTKT099                                                           0000104%
001050*
001520 01 SGTST101-DADOS.
=INC SGTKT101                                                           0000107%
001080*
001520 01 SGTST103-DADOS.
=INC SGTKT103                                                           0000113%
001140*
001520 01 SGTST109-DADOS.
=INC SGTKT109                                                           0000116%
001170*
000780*--> Book definição dados a serem usados na subrotina de erros
=INC HLPKE001                                                           0000079%
000800
000810*------------------- Variaveis do Sistema -----------------------*
000820 01  CTE-SAI-SS                  PIC  X(040) VALUE
000830     '*** Storage Section Termina Aqui ***'.
001400*----------------------------------------------------------------*
001410 LINKAGE                         SECTION.
001420*----------------------------------------------------------------*
001450 01  DFHCOMMAREA.
=INC SGTKD099                                                           0000120%
001210*
001430*
001440******************************************************************
001450 PROCEDURE DIVISION USING DFHCOMMAREA.
001460******************************************************************
001470*
001480*----------------------------------------------------------------*
001490 000000-ROTINA-PRINCIPAL         SECTION.
001500*----------------------------------------------------------------*
001510
001500     IF  EIBCALEN NOT EQUAL LENGTH OF DFHCOMMAREA
001510         MOVE 'Area com tamanho invalido - verifique book/local'
001520                                 TO MSG-ERRO
001530         PERFORM 999001-ERRO-001
001540     END-IF
001550     IF NOT EXECUCAO-ONLINE AND NOT EXECUCAO-BATCH
001560       CALL CICS5000 USING CICS5000-COMMAREA
001570     END-IF
001710     MOVE 0001                   TO PSC-ERRO-PGM
000860
001590* WHENEVER CICS
-INC HLPKE003

001530
001540     PERFORM 200000-VALIDA-ENTRADA
001550     PERFORM 300000-BUSCA-REG-EST-DRIA-ATI
001560     PERFORM 400000-BUSCA-CLASSE-COTAS
001570     PERFORM 500000-BUSCA-CLI-ATIVO
001580     PERFORM 600000-BUSCA-AGRUPAMENTO
001600     PERFORM 990000-ENCERRAR
001610
001620     .
001630 000099-SAI.
001640     EXIT.
001650

001860
001870*----------------------------------------------------------------*
001880 200000-VALIDA-ENTRADA           SECTION.
001890*----------------------------------------------------------------*
001710     MOVE 0002                   TO PSC-ERRO-PGM
001900
001910     IF SGTSD099-DT-EST-ATI          EQUAL SPACES
001930        MOVE 'Informe a data da posição.'
001940          TO MSG-ERRO
001950        PERFORM 999001-ERRO-01
001960     END-IF
001970
001980     IF SGTSD099-NM-ATI-FNCO        EQUAL SPACES
002000        MOVE 'Informe o titulo da posição estruturada.'
002010          TO MSG-ERRO
002020        PERFORM 999001-ERRO-01
002030     END-IF
002040
002050     COMPUTE SGTSD099-NR-SEQL-AQS-ATI =
                   SGTSD099-NR-SEQL-AQS-ATI   * 1
002060       ON SIZE ERROR
002070        MOVE ZEROS               TO SGTSD099-NR-SEQL-AQS-ATI
002080     END-COMPUTE.
002090
002100     IF SGTSD099-NR-SEQL-AQS-ATI     EQUAL ZEROS
002120        MOVE 'Informe a Tranche do título.'
002130          TO MSG-ERRO
002140        PERFORM 999001-ERRO-01
002150     END-IF
002160
002170     COMPUTE SGTSD099-NR-SEQL-REG-EST  =
                   SGTSD099-NR-SEQL-REG-EST  * 1
002180       ON SIZE ERROR
002190        MOVE ZEROS               TO SGTSD099-NR-SEQL-REG-EST
002200     END-COMPUTE.
002210
002220     IF SGTSD099-NR-SEQL-REG-EST    EQUAL ZEROS
002240        MOVE 'Informe o sequencial da posicao diaria do título.'
002250          TO MSG-ERRO
002260        PERFORM 999001-ERRO-01
002270     END-IF
002280
002290     COMPUTE SGTSD099-NR-SEQL-ATI      =
                   SGTSD099-NR-SEQL-ATI      * 1
002300       ON SIZE ERROR
002310        MOVE ZEROS               TO SGTSD099-NR-SEQL-ATI
002320     END-COMPUTE.
002330
002340     IF SGTSD099-NR-SEQL-ATI        EQUAL ZEROS
002360        MOVE 'Informe o codigo da operacao estruturada no SGT.'
002370          TO MSG-ERRO
002380        PERFORM 999001-ERRO-01
002390     END-IF
002400
002520     .
002530 200099-VALIDA-ENTRADA.
002540     EXIT.
002550
002560*----------------------------------------------------------------*
002570 300000-BUSCA-REG-EST-DRIA-ATI   SECTION.
002580*----------------------------------------------------------------*
001710     MOVE 0003                   TO PSC-ERRO-PGM
002590
002600     INITIALIZE SGTST101-DADOS
002610
002620     MOVE 4                              TO SGTST101-ACAO
002630     MOVE SGTSD099-CD-CTRA-SIS-DRIV      TO
                                         SGTST101-CD-CTRA-SIS-DRIV
002640     MOVE SGTSD099-NR-SEQL-ATI           TO
                                         SGTST101-NR-SEQL-ATI
002650     MOVE SGTSD099-DT-EST-ATI            TO
                                         SGTST101-DT-EST-ATI
002650     MOVE SGTSD099-NR-SEQL-REG-EST       TO
                                         SGTST101-NR-SEQL-REG-EST
002660
002670     MOVE LENGTH OF SGTST101-DADOS    TO EIBCALEN
002680     CALL SGTST101 USING DFHEIBLK SGTST101-DADOS
002690
002700     IF SGTST101-MSG-ERRO   NOT EQUAL SPACES
002710        MOVE SGTST101-CD-RTN-PGM TO MSG-ERRO
002720        MOVE SGTST101-MSG-ERRO   TO MSG-ERRO
002730        PERFORM 999001-ERRO-01
002740     END-IF
002750
002820     MOVE SGTST101-CD-PRD            TO SGTSD099-CD-PRD
002830     MOVE SGTST101-CD-MDLD           TO SGTSD099-CD-MDLD
002910     MOVE SGTST101-QT-PAPL-DT-PSC    TO SGTSD099-QT-PAPL-DT-PSC
002960     MOVE SGTST101-DT-AQS-ATI        TO SGTSD099-DT-AQS-ATI
002970     MOVE SGTST101-DT-VNCT-ATI       TO SGTSD099-DT-VNCT-ATI
           MOVE SGTST101-DT-VNCT-ATI       TO SGTSD099-DT-LQDC-OPR-DRIV
002970     MOVE SGTST101-CD-TIP-ATI        TO SGTSD099-CD-TIP-ATI
003030     MOVE SGTST101-IN-ATI-SPRO       TO SGTSD099-IN-ATI-SPRO
003040     MOVE SGTST101-IN-ATI-VRJ        TO SGTSD099-IN-ATI-VRJ
003050     MOVE SGTST101-IN-ATI-DSCP       TO SGTSD099-IN-ATI-DSCP
003060     MOVE SGTST101-IN-ATI-REG-BB     TO SGTSD099-IN-ATI-REG-BB
003070     MOVE SGTST101-IN-ATI-N-NEGO     TO SGTSD099-IN-ATI-N-NEGO
003080     MOVE SGTST101-IN-ATI-EXPO-VCLD  TO SGTSD099-IN-ATI-EXPO-VCLD
003090     MOVE SGTST101-CD-ISIN-ATI       TO SGTSD099-CD-ISIN-ATI
003090     MOVE SGTST101-VL-SDO-TTL-ATZD   TO SGTSD099-VL-SDO-TTL-ATZD
003090     MOVE SGTST101-CD-EST-REG-ATI    TO SGTSD099-CD-EST-REG-ATI
003090     MOVE SGTST101-CD-TIP-ALTO       TO SGTSD099-CD-TIP-ALTO
003090     MOVE SGTST101-VL-CTB-APL-MOEN   TO SGTSD099-VL-CTB-APL-MOEN
003090     MOVE SGTST101-VL-EMS-PPL        TO SGTSD099-VL-EMS-PPL
003090     MOVE SGTST101-VL-JUR-ULT-MM     TO SGTSD099-VL-JUR-ULT-MM
003090     MOVE SGTST101-VL-AMTZ           TO SGTSD099-VL-AMTZ
003090     MOVE SGTST101-VL-PVS-APL-MOEN   TO SGTSD099-VL-PVS-APL-MOEN
003100
003110     .
003120 300099-BUSCA-REG-EST-DRIA-ATI.
003130     EXIT.
003140
003150*----------------------------------------------------------------*
003160 400000-BUSCA-CLASSE-COTAS       SECTION.
003170*----------------------------------------------------------------*
001710     MOVE 0004                   TO PSC-ERRO-PGM
003180
003190     INITIALIZE SGTST099-DADOS
003200
003210     MOVE 4                           TO SGTST099-ACAO
003220     MOVE SGTSD099-CD-CTRA-SIS-DRIV   TO SGTST099-CD-CTRA-SIS-DRIV
003230     MOVE SGTSD099-NR-SEQL-ATI        TO SGTST099-NR-SEQL-ATI
003270
003280     MOVE LENGTH OF SGTST099-DADOS    TO EIBCALEN
003290     CALL SGTST099 USING DFHEIBLK SGTST099-DADOS
003300
003310     IF SGTST099-MSG-ERRO   NOT EQUAL SPACES
003320        MOVE SGTST099-CD-RTN-PGM TO MSG-ERRO
003330        MOVE SGTST099-MSG-ERRO   TO MSG-ERRO
003340        PERFORM 999001-ERRO-01
003350     END-IF
003360
003370     MOVE SGTST099-CD-CLS-COTA-TIT    TO SGTSD099-CD-CLS-COTA-TIT
003530
003540     .
003550 400099-BUSCA-CLASSE-COTAS.
003560     EXIT.
003570
003580*----------------------------------------------------------------*
003590 500000-BUSCA-CLI-ATIVO          SECTION.
003600*----------------------------------------------------------------*
001710     MOVE 0005                   TO PSC-ERRO-PGM
003610
003620     MOVE 1                      TO GDA-TIP-CLI
003630     PERFORM 510000-BUSCA-CLI-ST109
003670     MOVE GDA-CLIENTE            TO SGTSD099-CD-CLI-EMS
003680     MOVE GDA-CPF-CGC            TO SGTSD099-CD-CNPJ-EMS
003730
003740     MOVE 4                      TO GDA-TIP-CLI
003630     PERFORM 510000-BUSCA-CLI-ST109
003670     MOVE GDA-CLIENTE            TO SGTSD099-CD-CLI-AGT-FDCU
003680     MOVE GDA-CPF-CGC            TO SGTSD099-CD-CNPJ-AGT-FDCU
003810
003820     .
003830 500099-BUSCA-CLI-ATIVO.
003840     EXIT.
003850
003860*----------------------------------------------------------------*
003870 510000-BUSCA-CLI-ST109          SECTION.
003880*----------------------------------------------------------------*
001710     MOVE 0006                   TO PSC-ERRO-PGM
003890
003900     INITIALIZE SGTST109-DADOS.
003910
003920     MOVE 4                         TO SGTST109-ACAO
003930     MOVE SGTSD099-CD-CTRA-SIS-DRIV TO SGTST109-CD-CTRA-SIS-DRIV
003940     MOVE SGTSD099-NR-SEQL-ATI      TO SGTST109-NR-SEQL-ATI
003950     MOVE 0                         TO SGTST109-CD-STIT-ATI-FNCO
003960     MOVE GDA-TIP-CLI               TO SGTST109-CD-TIP-FUC-CLI
003970
003980     MOVE LENGTH OF SGTST109-DADOS    TO EIBCALEN
003990     CALL SGTST109 USING DFHEIBLK SGTST109-DADOS
004000
004010     IF NOT ( SGTST109-CD-RTN-PGM EQUAL 0
004020       OR     SGTST109-CD-RTN-PGM EQUAL +100 )
004030        MOVE SGTST109-CD-RTN-PGM TO MSG-ERRO
004040        MOVE SGTST109-MSG-ERRO   TO MSG-ERRO
004050        PERFORM 999001-ERRO-01
004060     END-IF
004070
004080     MOVE SGTST109-CD-CLI            TO GDA-CLIENTE
004090     MOVE SGTST109-NR-IDFR-CLI-EXNO  TO GDA-CPF-CGC
           MOVE SGTST109-CD-TIP-PSS        TO GDA-TIP-PSS
004130
004140     .
004150 510099-BUSCA-CLI-ST109.
004160     EXIT.
004170
004570
004880*----------------------------------------------------------------*
004890 600000-BUSCA-AGRUPAMENTO        SECTION.
004900*----------------------------------------------------------------*
001710     MOVE 0009                       TO PSC-ERRO-PGM
004910
004920     INITIALIZE SGTST103-DADOS
004930
004940     MOVE 5                          TO SGTST103-ACAO
004950     MOVE SGTSD099-CD-CTRA-SIS-DRIV  TO SGTST103-CD-CTRA-SIS-DRIV
004960     MOVE SGTSD099-NR-SEQL-ATI       TO SGTST103-NR-SEQL-ATI
004980
004990     MOVE LENGTH OF SGTST103-DADOS    TO EIBCALEN
005000     CALL SGTST103 USING DFHEIBLK SGTST103-DADOS
005010
005020     IF SGTST103-CD-RTN-PGM NOT EQUAL +100
005030        IF SGTST103-MSG-ERRO   NOT EQUAL SPACES
005040           MOVE SGTST103-CD-RTN-PGM TO MSG-ERRO
005050           MOVE SGTST103-MSG-ERRO   TO MSG-ERRO
005060           PERFORM 999001-ERRO-01
005070        END-IF
005080     END-IF
005090
005100     MOVE SGTST103-CD-AGPT-ATI-FNCO TO SGTSD099-CD-TIP-ALTO
           MOVE SGTST101-CD-TIP-ALTO      TO SGTSD099-CD-TIP-ALTO
005110     .
005120 600099-BUSCA-AGRUPAMENTO.
005130     EXIT.
005140
007490
007250*----------------------------------------------------------------*
007260 999000-ERRO              SECTION.
007250*----------------------------------------------------------------*
007280*-----------------
007290 999001-ERRO-01.
007300*-----------------
-INC HLPKE004
007320     MOVE RTN-PGM                TO SGTSD099-RTN-PGM
003910     PERFORM 990000-ENCERRAR
007340     .
003560 999001-ERRO.
003570     EXIT.
003760
007350*----------------------------
007360* 999003-TRATA-ABEND-CICS
007370* 999004-TRATA-CONDITION-CICS
007380*----------------------------
-INC HLPKE006
007420 999999-SAI.
007430     EXIT.
007660*----------------------------------------------------------------*
007670 990000-ENCERRAR                 SECTION.
007680*----------------------------------------------------------------*
007690
007700     EXEC CICS RETURN END-EXEC
007710
007720     .
007730 990099-ENCERRA.
007740     EXIT.
007750
007760*================================================================*
007440*---------------------- FIM SGTSD099  ---------------------------*