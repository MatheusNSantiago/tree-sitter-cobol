000010CBL ARITH(EXTEND)
000020* ============================================================== *
000030* ============================================================== *
000040* -------------       S I S T E M A     S G T     -------------- *
000050* -------------        Sistema de Gestão TVM      -------------- *
000060* -------------------------------------------------------------- *
000070* PROGRAMA..: SGTSD005                                           *
000080* ANALISTA..: ALESSANDER TAVARES DE PINHO                        *
000090* AUTOR.....: ALESSANDER TAVARES DE PINHO                        *
000100* CHAVE.....: C1077250                                           *
000110* DATA......: 11.07.2015                                         *
000120* OBJETIVO..: Consultar dados da Operacao Estruturada            *
000130* -------------------------------------------------------------- *
000140* SERVICO/VRS/OPERACAO/VRS : XXXXXXXXX / 0001 / 01241558 / 0001  *
000150* -------------------------------------------------------------- *
000270* VRS003 14.02.2019 F8785286 - Somente recompilação por alteração
000270*           no book SGTST006.
      * VRS002 13.03.2018 Christine  - excluir campos da area de retorno
      *                                do módulo SGTST005 e movimentar
      *                                zeros no lugar deles
000160* VRS001 13.08.15 - ALESSANDER - BUSCAR CLIENTE DA OPERACAO      *
000170*        11.07.15 - ALESSANDER - IMPLANTACAO                     *
000180* ============================================================== *
000190*
000200******************************************************************
000210 IDENTIFICATION                  DIVISION.
000220******************************************************************
000230*
000240 PROGRAM-ID.   SGTSD005.
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
000430 77  CTE-PROG                    PIC  X(242) VALUE 'SGTSD005'.
000440 77  CTE-VERS                    PIC  X(006) VALUE 'VRS003'.
000450 77  SGTST004                    PIC  X(008) VALUE 'SGTST004'.
000460 77  SGTST005                    PIC  X(008) VALUE 'SGTST005'.
000470 77  SGTST006                    PIC  X(008) VALUE 'SGTST006'.
000480 77  SGTST013                    PIC  X(008) VALUE 'SGTST013'.
000490 77  SGTST023                    PIC  X(008) VALUE 'SGTST023'.
000500*
000510 01 WS-RESP-CICS.
000520     05 WS-RESP                  PIC 9(08) VALUE 0.
000530     05 WS-RESP2                 PIC 9(08) VALUE 0.
000540*
000550 01 GD-SQLCODE                   PIC -999.
000560 01 GDA-TIP-CLI                  PIC 9(004) VALUE ZEROS.
000570 01 GDA-CLIENTE                  PIC 9(009) VALUE ZEROS.
000580 01 GDA-CPF-CGC                  PIC 9(014) VALUE ZEROS.
000590 01 GDA-CNPJ-EMISSOR             PIC 9(014) VALUE ZEROS.
000600 01 GDA-TS-TEMP                  PIC X(026) VALUE SPACES.
000610 01 FILLER REDEFINES GDA-TS-TEMP.
000620    03 GDA-TS-TEMP-FIL           PIC X(012).
000630    03 GDA-TS-TEMP-CNPJ          PIC 9(014).
000640*
000650 01  SERVICO-IIB-GENERICO.
000660     03  KIIBG-SRVC-ID-SRVC           PIC  9(09).
000670     03  KIIBG-SRVC-VRS-SRVC          PIC  9(04).
000680     03  KIIBG-SRVC-ID-OPR            PIC  9(08).
000690     03  KIIBG-SRVC-VRS-OPR           PIC  9(04).
000700     03  KIIBG-NM-PGM-CHAMAR          PIC  X(08).
000710
000720 01  WS-DADOS-RQSC                    PIC  X(90000)  VALUE SPACES.
000730 01  WS-DADOS-RPST                    PIC  X(90000)  VALUE SPACES.
000740 01  WS-TAM-DADOS-RQSC                PIC S9(09) COMP VALUE ZEROS.
000750 01  WS-TAM-DADOS-RPST                PIC S9(09) COMP VALUE ZEROS.
000760
000770 01  WS-NM-CANAL                      PIC  X(16) VALUE 'CANAL-'.
000780 01  FILLER REDEFINES WS-NM-CANAL.
000790     03  WS-CANAL                     PIC  X(06).
000800     03  WS-NM-PGM                    PIC  X(08).
000810     03  FILLER                       PIC  X(02).
000820*
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
001000 01  MCIS0001-DADOS.
=INC MCIB0001                                                           0000101%
001020*
001030 01  PRM-ST004.
=INC SGTKT004                                                           0000104%
001050*
001060 01  PRM-ST005.
=INC SGTKT005                                                           0000107%
001080*
001090 01  PRM-ST006.
=INC SGTKT006                                                           0000110%
001110*
001120 01  PRM-ST013.
=INC SGTKT013                                                           0000113%
001140*
001150 01  PRM-ST023.
=INC SGTKT023                                                           0000116%
001170*
001180 01  KD005-PRM.
001190    03  KD005-DADOS.
=INC SGTKD005                                                           0000120%
001210*
001220*
001230*--- Book com area de dados da subrotina
001240*--- Book com area de erro p/ retorno ao IIB
=INC SGTKIIBE                                                           0000125%
001260*
001270*--- Book do container de Erro
=INC IIBKERRO                                                           0000128%
001290*
001300*--- Book do container de Requisição de Serviço
=INC IIBKSRVC                                                           0000131%
001320*
=INC IIBKCHV                                                            0000133%
=INC IIBKOPRD                                                           0000134%
001350*
001360*----------------------------------------------------------------*
001370 LOCAL-STORAGE                   SECTION.
001380*----------------------------------------------------------------*
001390*
001400*----------------------------------------------------------------*
001410 LINKAGE                         SECTION.
001420*----------------------------------------------------------------*
001430*
001440******************************************************************
001450 PROCEDURE DIVISION.
001460******************************************************************
001470*
001480*----------------------------------------------------------------*
001490 000000-ROTINA-PRINCIPAL         SECTION.
001500*----------------------------------------------------------------*
001510
001520     PERFORM 100000-GET-MESSAGE
001530
001540     PERFORM 200000-VALIDA-ENTRADA
001550     PERFORM 300000-BUSCA-OPR-ETRD
001560     PERFORM 400000-BUSCA-PSC-SDO-OPR
001570     PERFORM 500000-BUSCA-CLI-OPR-ETRD
001580     PERFORM 600000-BUSCA-AGRUPAMENTO
001590     PERFORM 700000-PUT-RESPOSTA
001600     PERFORM 999000-ENCERRA
001610
001620     .
001630 000099-SAI.
001640     EXIT.
001650
001660*----------------------------------------------------------------*
001670 100000-GET-MESSAGE              SECTION.
001680*----------------------------------------------------------------*
001690
001700     EXEC CICS
001710         GET CONTAINER ( 'SGTSD005-RQSC' )
001720             INTO      (  KD005-ENTD    )
001730     END-EXEC
001740
001750     IF  EIBRESP NOT EQUAL DFHRESP(NORMAL)
001760         MOVE EIBRESP            TO KERRO-CD-ERRO
001770         MOVE 001                TO GDA-SEQL-ERRO
001780         MOVE 'ERRO AO EFETUAR GET NO CONTAINER SGTKD005-RQSC'
001790                                 TO KERRO-TX-ERRO
001800         PERFORM 990000-ERRO
001810     END-IF
001820
001830     .
001840 100099-GET-MESSAGE.
001850     EXIT.
001860
001870*----------------------------------------------------------------*
001880 200000-VALIDA-ENTRADA           SECTION.
001890*----------------------------------------------------------------*
001900
001910     IF KD005-DT-PSC             EQUAL SPACES
001920        MOVE 001                 TO KERRO-CD-ERRO
001930        MOVE 'Informe a data da posição.'
001940          TO KERRO-TX-ERRO
001950        PERFORM 990000-ERRO
001960     END-IF
001970
001980     IF KD005-CD-TIT-OPR-ETRD    EQUAL SPACES
001990        MOVE 002                 TO KERRO-CD-ERRO
002000        MOVE 'Informe o titulo da posição estruturada.'
002010          TO KERRO-TX-ERRO
002020        PERFORM 990000-ERRO
002030     END-IF
002040
002050     COMPUTE KD005-NR-DVS-TIT       = KD005-NR-DVS-TIT       * 1
002060       ON SIZE ERROR
002070        MOVE ZEROS               TO KD005-NR-DVS-TIT
002080     END-COMPUTE.
002090
002100     IF KD005-NR-DVS-TIT         EQUAL ZEROS
002110        MOVE 003                 TO KERRO-CD-ERRO
002120        MOVE 'Informe a Tranche do título.'
002130          TO KERRO-TX-ERRO
002140        PERFORM 990000-ERRO
002150     END-IF
002160
002170     COMPUTE KD005-NR-SEQL-PSC-DRIA = KD005-NR-SEQL-PSC-DRIA * 1
002180       ON SIZE ERROR
002190        MOVE ZEROS               TO KD005-NR-SEQL-PSC-DRIA
002200     END-COMPUTE.
002210
002220     IF KD005-NR-SEQL-PSC-DRIA   EQUAL ZEROS
002230        MOVE 004                 TO KERRO-CD-ERRO
002240        MOVE 'Informe o sequencial da posicao diaria do título.'
002250          TO KERRO-TX-ERRO
002260        PERFORM 990000-ERRO
002270     END-IF
002280
002290     COMPUTE KD005-CD-OPR-ETRD-MCP  = KD005-CD-OPR-ETRD-MCP  * 1
002300       ON SIZE ERROR
002310        MOVE ZEROS               TO KD005-CD-OPR-ETRD-MCP
002320     END-COMPUTE.
002330
002340     IF KD005-CD-OPR-ETRD-MCP    EQUAL ZEROS
002350        MOVE 005                 TO KERRO-CD-ERRO
002360        MOVE 'Informe o codigo da operacao estruturada no SGT.'
002370          TO KERRO-TX-ERRO
002380        PERFORM 990000-ERRO
002390     END-IF
002400
002410     COMPUTE KD005-NR-VRS-OPR-ETRD  = KD005-NR-VRS-OPR-ETRD  * 1
002420       ON SIZE ERROR
002430        MOVE ZEROS               TO KD005-NR-VRS-OPR-ETRD
002440     END-COMPUTE.
002450
002460     IF KD005-NR-VRS-OPR-ETRD    EQUAL ZEROS
002470        MOVE 006                 TO KERRO-CD-ERRO
002480        MOVE 'Informe a versao da operação estruturada no SGT.'
002490          TO KERRO-TX-ERRO
002500        PERFORM 990000-ERRO
002510     END-IF
002520     .
002530 200099-VALIDA-ENTRADA.
002540     EXIT.
002550
002560*----------------------------------------------------------------*
002570 300000-BUSCA-OPR-ETRD           SECTION.
002580*----------------------------------------------------------------*
002590
002600     INITIALIZE PRM-ST005
002610
002620     MOVE 4                        TO SGTST005-ACAO
002630     MOVE KD005-CD-CTRA-TIT-VL     TO SGTST005-CD-CTRA-TIT-VL
002640     MOVE KD005-CD-OPR-ETRD-MCP    TO SGTST005-CD-OPR-ETRD-MCP
002650     MOVE KD005-NR-VRS-OPR-ETRD    TO SGTST005-NR-VRS-OPR-ETRD
002660
002670     MOVE LENGTH OF PRM-ST005    TO EIBCALEN
002680     CALL SGTST005 USING DFHEIBLK PRM-ST005
002690
002700     IF SGTST005-MSG-ERRO   NOT EQUAL SPACES
002710        MOVE SGTST005-CD-RTN-PGM TO KERRO-CD-ERRO
002720        MOVE SGTST005-MSG-ERRO   TO KERRO-TX-ERRO
002730        PERFORM 990000-ERRO
002740     END-IF
002750
      * Campos inibidos pois não existem mais no book SGTKT005 e por não
      * serem usados no Natural que acessa este módulo foi movimentado
      * zeros em seu lugar
002760     MOVE SGTST005-CD-BCO-CRD-VL-OPR  TO KD005-CD-BCO-CRD-VL-OPR
002770     MOVE SGTST005-CD-DEPE-CRD-VL-OPR TO KD005-CD-DEPE-CRD-VL-OPR
002780     MOVE SGTST005-CD-SUB-DEPE-CRD-VL TO KD005-CD-SUB-DEPE-CRD-VL
002790*    MOVE SGTST005-CD-PERC-RMNC       TO KD005-CD-PERC-RMNC
           MOVE ZEROS                       TO KD005-CD-PERC-RMNC
002800*    MOVE SGTST005-CD-IEC-RMNC-OPR    TO KD005-CD-IEC-RMNC-OPR
           MOVE ZEROS                       TO KD005-CD-IEC-RMNC-OPR
002810     MOVE SGTST005-CD-UOR-DTTR-OPR    TO KD005-CD-UOR-DTTR-OPR
002820     MOVE SGTST005-CD-PRD             TO KD005-CD-PRD
002830     MOVE SGTST005-CD-MDLD            TO KD005-CD-MDLD
002840     MOVE SGTST005-CD-UOR-BB-CRD-VL   TO KD005-CD-UOR-BB-CRD-VL
002850     MOVE SGTST005-CD-UOR-BB          TO KD005-CD-UOR-BB
002860     MOVE SGTST005-NR-CC-BB           TO KD005-NR-CC-BB
002870*    MOVE SGTST005-CD-TIP-FMA-APL     TO KD005-CD-TIP-FMA-APL
           MOVE ZEROS                       TO KD005-CD-TIP-FMA-APL
002880     MOVE SGTST005-IN-RMNC-VRV        TO KD005-IN-RMNC-VRV
002890     MOVE SGTST005-NR-CC-OTR-BCO      TO KD005-NR-CC-OTR-BCO
002900     MOVE SGTST005-DV-CC-OTR-BCO      TO KD005-DV-CC-OTR-BCO
002910     MOVE SGTST005-QT-PAPL-TIT-EMS    TO KD005-QT-PAPL-TIT-EMS
002920     MOVE SGTST005-QT-PAPL-TIT-CPR    TO KD005-QT-PAPL-TIT-CPR
002930     MOVE SGTST005-VL-CPR-OPR         TO KD005-VL-CPR-OPR
002940     MOVE SGTST005-VL-PU-EMS-TIT-OPR  TO KD005-VL-PU-EMS-TIT-OPR
002950     MOVE SGTST005-VL-PU-CPR-TIT-OPR  TO KD005-VL-PU-CPR-TIT-OPR
002960     MOVE SGTST005-DT-CPR-OPR         TO KD005-DT-CPR-OPR
002970     MOVE SGTST005-DT-VNCT-OPR        TO KD005-DT-VNCT-OPR
002980     MOVE SGTST005-DT-LQDC-OPR-DRIV   TO KD005-DT-LQDC-OPR-DRIV
002990     MOVE SGTST005-CD-TIT-OPR-ETRD    TO KD005-CD-TIT-OPR-ETRD
003000     MOVE SGTST005-NR-DVS-TIT         TO KD005-NR-DVS-TIT
003010     MOVE SGTST005-NR-UNCO-CTR-OPR    TO KD005-NR-UNCO-CTR-OPR
003020     MOVE SGTST005-CD-TIP-OPR-ETRD    TO KD005-CD-TIP-OPR-ETRD
003030     MOVE SGTST005-IN-OPR-ETRD-SPRO   TO KD005-IN-OPR-ETRD-SPRO
003040     MOVE SGTST005-IN-OPR-ETRD-VRJ    TO KD005-IN-OPR-ETRD-VRJ
003050     MOVE SGTST005-IN-OPR-ETRD-DSCP   TO KD005-IN-OPR-ETRD-DSCP
003060     MOVE SGTST005-IN-REG-OPR-BB      TO KD005-IN-REG-OPR-BB
003070     MOVE SGTST005-IN-OPR-N-NEGO      TO KD005-IN-OPR-N-NEGO
003080     MOVE SGTST005-IN-EXPO-VCLD-GRT   TO KD005-IN-EXPO-VCLD-GRT
003090     MOVE SGTST005-CD-TIP-COTA-TIT    TO KD005-CD-EST-RSTD-ITE
003100
003110     .
003120 300099-BUSCA-OPR-ETRD.
003130     EXIT.
003140
003150*----------------------------------------------------------------*
003160 400000-BUSCA-PSC-SDO-OPR        SECTION.
003170*----------------------------------------------------------------*
003180
003190     INITIALIZE PRM-ST004
003200
003210     MOVE 4                        TO SGTST004-ACAO
003220     MOVE KD005-CD-CTRA-TIT-VL     TO SGTST004-CD-CTRA-TIT-VL
003230     MOVE KD005-DT-PSC             TO SGTST004-DT-PSC
003240     MOVE KD005-CD-TIT-OPR-ETRD    TO SGTST004-CD-TIT-OPR-ETRD
003250     MOVE KD005-NR-DVS-TIT         TO SGTST004-NR-DVS-TIT
003260     MOVE KD005-NR-SEQL-PSC-DRIA   TO SGTST004-NR-SEQL-PSC-DRIA
003270
003280     MOVE LENGTH OF PRM-ST004    TO EIBCALEN
003290     CALL SGTST004 USING DFHEIBLK PRM-ST004
003300
003310     IF SGTST004-MSG-ERRO   NOT EQUAL SPACES
003320        MOVE SGTST004-CD-RTN-PGM TO KERRO-CD-ERRO
003330        MOVE SGTST004-MSG-ERRO   TO KERRO-TX-ERRO
003340        PERFORM 990000-ERRO
003350     END-IF
003360
003370     MOVE SGTST004-TS-ULT-ATL         TO KD005-TS-ULT-ATL
003380     MOVE SGTST004-CD-USU-RSP-ATL     TO KD005-CD-USU-RSP-ATL
003390     MOVE SGTST004-QT-PAPL-TIT-DT-PSC TO KD005-QT-PAPL-TIT-DT-PSC
003400     MOVE SGTST004-VL-PU-TIT-DT-PSC   TO KD005-VL-PU-TIT-DT-PSC
003410     MOVE SGTST004-VL-SDO-TIT-DT-PSC  TO KD005-VL-SDO-TIT-DT-PSC
003420***  MOVE SGTST004-CD-EST-RSTD-ITE    TO KD005-CD-EST-RSTD-ITE
003430     MOVE SGTST004-DT-APL-TIT         TO KD005-DT-APL-TIT
003440     MOVE SGTST004-VL-TAXA-IDX        TO KD005-VL-TAXA-IDX
003450     MOVE SGTST004-CD-IC              TO KD005-CD-IC
003460     MOVE SGTST004-VL-EMS-PPL-TIT     TO KD005-VL-EMS-PPL-TIT
003470     MOVE SGTST004-VL-CTB-APL-MOEN    TO KD005-VL-CTB-APL-MOEN
003480     MOVE SGTST004-VL-JUR-ULT-MM      TO KD005-VL-JUR-ULT-MM
003490     MOVE SGTST004-VL-AMTZ            TO KD005-VL-AMTZ
003500     MOVE SGTST004-VL-PVS-APL-MOEN    TO KD005-VL-PVS-APL-MOEN
003510     MOVE SGTST004-TX-RTN-PRCT-OPR    TO KD005-TX-RTN-PRCT-OPR
003520     MOVE SGTST004-CD-USU-ENVO-PSC    TO KD005-CD-USU-ENVO-PSC
003530
003540     .
003550 400099-BUSCA-PSC-SDO-OPR.
003560     EXIT.
003570
003580*----------------------------------------------------------------*
003590 500000-BUSCA-CLI-OPR-ETRD       SECTION.
003600*----------------------------------------------------------------*
003610
003620     MOVE 1 TO GDA-TIP-CLI
003630     PERFORM 510000-BUSCA-CLI-ST006
003640     IF SGTST006-CD-CLI EQUAL ZEROS
003650        PERFORM 520000-BUSCA-CLI-ST023
003660     END-IF
003670     MOVE GDA-CLIENTE            TO KD005-CD-UOR-BB-CRD-VL
003680     MOVE GDA-CPF-CGC            TO GDA-CNPJ-EMISSOR
003690     MOVE GDA-CNPJ-EMISSOR       TO GDA-TS-TEMP-CNPJ
003700     MOVE '000000000000'         TO GDA-TS-TEMP-FIL
003710     MOVE GDA-TS-TEMP            TO KD005-TS-ENVO-PSC
003720     MOVE ZEROS                  TO KD005-TS-ENVO-PSC
003730
003740     MOVE 4 TO GDA-TIP-CLI
003750     PERFORM 510000-BUSCA-CLI-ST006
003760     IF SGTST006-CD-CLI EQUAL ZEROS
003770        PERFORM 520000-BUSCA-CLI-ST023
003780     END-IF
003790     MOVE GDA-CLIENTE            TO KD005-CD-UOR-BB
003800     MOVE GDA-CPF-CGC            TO KD005-NR-UNCO-CTR-OPR
003810
003820     .
003830 500099-BUSCA-CLI-OPR-ETRD.
003840     EXIT.
003850
003860*----------------------------------------------------------------*
003870 510000-BUSCA-CLI-ST006          SECTION.
003880*----------------------------------------------------------------*
003890
003900     INITIALIZE PRM-ST006
003910
003920     MOVE 5                      TO SGTST006-ACAO
003930     MOVE KD005-CD-CTRA-TIT-VL   TO SGTST006-CD-CTRA-TIT-VL
003940     MOVE KD005-CD-OPR-ETRD-MCP  TO SGTST006-CD-OPR-ETRD-MCP
003950     MOVE KD005-NR-VRS-OPR-ETRD  TO SGTST006-NR-VRS-OPR-ETRD
003960     MOVE GDA-TIP-CLI            TO SGTST006-CD-TIP-FUC-CLI
003970
003980     MOVE LENGTH OF PRM-ST006    TO EIBCALEN
003990     CALL SGTST006 USING DFHEIBLK PRM-ST006
004000
004010     IF NOT ( SGTST006-CD-RTN-PGM EQUAL 0
004020       OR     SGTST006-CD-RTN-PGM EQUAL +100 )
004030        MOVE SGTST006-CD-RTN-PGM TO KERRO-CD-ERRO
004040        MOVE SGTST006-MSG-ERRO   TO KERRO-TX-ERRO
004050        PERFORM 990000-ERRO
004060     END-IF
004070
004080     MOVE SGTST006-CD-CLI        TO GDA-CLIENTE
004090
004100     IF SGTST006-CD-CLI NOT EQUAL ZEROS
004110        PERFORM 511000-BUSCA-CLIENTE-MCI
004120     END-IF
004130
004140     .
004150 510099-BUSCA-CLI-ST006.
004160     EXIT.
004170
004180*----------------------------------------------------------------*
004190 511000-BUSCA-CLIENTE-MCI SECTION.
004200*----------------------------------------------------------------*
004210
004220     INITIALIZE MCIS0001-DADOS
004230
004240     MOVE 19485                    TO KIIBG-SRVC-ID-SRVC
004250     MOVE 1                        TO KIIBG-SRVC-VRS-SRVC
004260     MOVE 207827                   TO KIIBG-SRVC-ID-OPR
004270     MOVE 1                        TO KIIBG-SRVC-VRS-OPR
004280     MOVE 'MCIS0001'               TO KIIBG-NM-PGM-CHAMAR
004290
004300     MOVE GDA-CLIENTE              TO MCIS0001-CLIENTE
004310
004320     MOVE LENGTH OF MCIS0001-DADOS TO WS-TAM-DADOS-RQSC
004330     MOVE MCIS0001-DADOS           TO WS-DADOS-RQSC
004340
004350     PERFORM 600000-RQSC-SRVC-IIB-GENERICO
004360
004370     MOVE WS-DADOS-RPST            TO MCIS0001-DADOS
004380
004390     EVALUATE MCIS0001-RET-CODE
004400        WHEN 0
004410           CONTINUE
004420        WHEN 100
004430           MOVE ZEROS              TO MCIS0001-CPF-CGC
004440        WHEN OTHER
004450           MOVE 005                 TO KERRO-CD-ERRO
004460           MOVE 'Erro ao validar MCI do cliente.'
004470             TO KERRO-TX-ERRO
004480           PERFORM 990000-ERRO
004490     END-EVALUATE
004500
004510     MOVE MCIS0001-CPF-CGC   TO GDA-CPF-CGC
004520
004530
004540     .
004550 510099-BUSCA-CLI-ST006.
004560     EXIT.
004570
004580*----------------------------------------------------------------*
004590 520000-BUSCA-CLI-ST023          SECTION.
004600*----------------------------------------------------------------*
004610
004620     INITIALIZE PRM-ST023
004630
004640     MOVE 4                        TO SGTST023-ACAO
004650     MOVE KD005-CD-CTRA-TIT-VL     TO SGTST023-CD-CTRA-TIT-VL
004660     MOVE KD005-CD-OPR-ETRD-MCP    TO SGTST023-CD-OPR-ETRD-MCP
004670     MOVE KD005-NR-VRS-OPR-ETRD    TO SGTST023-NR-VRS-OPR-ETRD
004680     MOVE 0                        TO SGTST023-CD-STIT-OPR-ETRD
004690     MOVE GDA-TIP-CLI              TO SGTST023-CD-TIP-FUC-CLI
004700
004710     MOVE LENGTH OF PRM-ST023    TO EIBCALEN
004720     CALL SGTST023 USING DFHEIBLK PRM-ST023
004730
004740     IF NOT ( SGTST023-CD-RTN-PGM EQUAL 0
004750       OR     SGTST023-CD-RTN-PGM EQUAL +100 )
004760        MOVE SGTST023-CD-RTN-PGM TO KERRO-CD-ERRO
004770        MOVE SGTST023-MSG-ERRO   TO KERRO-TX-ERRO
004780        PERFORM 990000-ERRO
004790     END-IF
004800
004810     MOVE SGTST023-CD-CLI           TO GDA-CLIENTE
004820     MOVE SGTST023-NR-IDFR-CLI-EXNO TO GDA-CPF-CGC
004830
004840     .
004850 520099-BUSCA-CLI-ST023.
004860     EXIT.
004870
004880*----------------------------------------------------------------*
004890 600000-BUSCA-AGRUPAMENTO        SECTION.
004900*----------------------------------------------------------------*
004910
004920     INITIALIZE PRM-ST013
004930
004940     MOVE 5                        TO SGTST013-ACAO
004950     MOVE KD005-CD-CTRA-TIT-VL     TO SGTST013-CD-CTRA-TIT-VL
004960     MOVE KD005-CD-OPR-ETRD-MCP    TO SGTST013-CD-OPR-ETRD-MCP
004970     MOVE KD005-NR-VRS-OPR-ETRD    TO SGTST013-NR-VRS-OPR-ETRD
004980
004990     MOVE LENGTH OF PRM-ST013    TO EIBCALEN
005000     CALL SGTST013 USING DFHEIBLK PRM-ST013
005010
005020     IF SGTST013-CD-RTN-PGM NOT EQUAL +100
005030        IF SGTST013-MSG-ERRO   NOT EQUAL SPACES
005040           MOVE SGTST013-CD-RTN-PGM TO KERRO-CD-ERRO
005050           MOVE SGTST013-MSG-ERRO   TO KERRO-TX-ERRO
005060           PERFORM 990000-ERRO
005070        END-IF
005080     END-IF
005090
005100     MOVE SGTST013-CD-AGPT-TIT-OPR TO KD005-CD-IC
005110     .
005120 600099-BUSCA-AGRUPAMENTO.
005130     EXIT.
005140
005150*----------------------------------------------------------------*
005160 600000-RQSC-SRVC-IIB-GENERICO     SECTION.
005170*----------------------------------------------------------------*
005180
005190     PERFORM 610000-PUT-CTNR-RQSC-SRVC
005200     PERFORM 620000-PUT-CTNR-DADOS-RQSC
005210     PERFORM 630000-LINK-IIBP2000
005220     PERFORM 640000-GET-CTRN-ERRO
005230
005240     IF  WS-RESP EQUAL DFHRESP(CONTAINERERR)
005250         PERFORM 650000-GET-CTRN-RPST
005260     END-IF
005270     .
005280 600099-RQSC-SRVC-IIB-GENERICO.
005290     EXIT.
005300
005310*----------------------------------------------------------------*
005320 610000-PUT-CTNR-RQSC-SRVC       SECTION.
005330*----------------------------------------------------------------*
005340*    Grava container de requisição de serviço.
005350*    Os dados a serem enviados nesse container devem ter sidos
005360*    previamente cadastrados no sistema IIB.
005370*----------------------------------------------------------------*
005380*
005390     MOVE KIIBG-SRVC-ID-SRVC    TO SRVC-ID-SRVC.
005400     MOVE KIIBG-SRVC-VRS-SRVC   TO SRVC-VRS-SRVC.
005410     MOVE KIIBG-SRVC-ID-OPR     TO SRVC-ID-OPR.
005420     MOVE KIIBG-SRVC-VRS-OPR    TO SRVC-VRS-OPR.
005430     MOVE KIIBG-NM-PGM-CHAMAR   TO WS-NM-PGM
005440*
005450     EXEC CICS PUT
005460               CONTAINER ( 'PRM-RQSC-SRVC'              )
005470               CHANNEL   ( WS-NM-CANAL                  )
005480               FROM      ( SRVC-PRM-RQSC-SRVC           )
005490               FLENGTH   ( LENGTH OF SRVC-PRM-RQSC-SRVC )
005500               RESP      ( WS-RESP                      )
005510               RESP2     ( WS-RESP2                     )
005520     END-EXEC.
005530*
005540     EVALUATE TRUE
005550         WHEN WS-RESP EQUAL DFHRESP(NORMAL)
005560              CONTINUE
005570         WHEN OTHER
005580              MOVE 601              TO IIBKERRO-CD-ERRO
005590              STRING 'SD005-Indisponibilidade de recurso ('
005600                      SRVC-ID-OPR  '-'
005610                      IIBKERRO-CD-ERRO '-'
005620                      WS-RESP          '-'
005630                      WS-RESP2         ')'
005640                          DELIMITED BY SIZE
005650                                  INTO IIBKERRO-TX-ERRO
005660              PERFORM 990000-ERRO
005670     END-EVALUATE.
005680*
005690 610099-PUT-CTNR-RQSC-SRVC.
005700     EXIT.
005710*
005720*----------------------------------------------------------------*
005730 620000-PUT-CTNR-DADOS-RQSC      SECTION.
005740*----------------------------------------------------------------*
005750*    Grava container com parametros do serviço.
005760*----------------------------------------------------------------*
005770*
005780     EXEC CICS PUT
005790               CONTAINER ( 'PRM-DADOS-RQSC' )
005800               CHANNEL   ( WS-NM-CANAL      )
005810               FROM      ( WS-DADOS-RQSC        )
005820               FLENGTH   ( WS-TAM-DADOS-RQSC    )
005830               RESP      ( WS-RESP              )
005840               RESP2     ( WS-RESP2             )
005850     END-EXEC.
005860*
005870     EVALUATE TRUE
005880         WHEN WS-RESP EQUAL DFHRESP(NORMAL)
005890              CONTINUE
005900         WHEN OTHER
005910              MOVE 602              TO IIBKERRO-CD-ERRO
005920              STRING 'SD005-Indisponibilidade de recurso ('
005930                      SRVC-ID-OPR  '-'
005940                      IIBKERRO-CD-ERRO '-'
005950                      WS-RESP          '-'
005960                      WS-RESP2         ')'
005970                          DELIMITED BY SIZE
005980                                  INTO IIBKERRO-TX-ERRO
005990              PERFORM 990000-ERRO
006000     END-EVALUATE.
006010*
006020 620099-PUT-CTNR-DADOS-RQSC.
006030     EXIT.
006040*
006050*----------------------------------------------------------------*
006060 630000-LINK-IIBP2000            SECTION.
006070*----------------------------------------------------------------*
006080*    Link do programa IIBP2000 para requisição do serviço.
006090*----------------------------------------------------------------*
006100*
006110     EXEC CICS LINK
006120               PROGRAM   ( 'IIBP2000'       )
006130               CHANNEL   ( WS-NM-CANAL      )
006140               RESP      ( WS-RESP          )
006150               RESP2     ( WS-RESP2         )
006160     END-EXEC.
006170*
006180     EVALUATE TRUE
006190         WHEN WS-RESP EQUAL DFHRESP(NORMAL)
006200              CONTINUE
006210         WHEN OTHER
006220              MOVE 603              TO IIBKERRO-CD-ERRO
006230              STRING 'SD005-Indisponibilidade de recurso ('
006240                      SRVC-ID-OPR  '-'
006250                      IIBKERRO-CD-ERRO
006260                     '-IIBP2000'
006270                      WS-RESP  ')'
006280                          DELIMITED BY SIZE
006290                                  INTO IIBKERRO-TX-ERRO
006300              PERFORM 990000-ERRO
006310     END-EVALUATE.
006320*
006330 630099-LINK-IIBP2000.
006340     EXIT.
006350
006360*----------------------------------------------------------------*
006370 640000-GET-CTRN-ERRO            SECTION.
006380*----------------------------------------------------------------*
006390*    A presença do container PRM-ERRO indica que houve erro de
006400*    integração, a ausência do container indica sucesso.
006410*----------------------------------------------------------------*
006420*
006430     EXEC CICS GET
006440               CONTAINER ( 'PRM-ERRO'                  )
006450               CHANNEL   ( WS-NM-CANAL                 )
006460               INTO      ( IIBKERRO-PRM-ERRO           )
006470               FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
006480               RESP      ( WS-RESP                     )
006490               RESP2     ( WS-RESP2                    )
006500     END-EXEC.
006510*
006520     EVALUATE TRUE
006530         WHEN WS-RESP EQUAL DFHRESP(CONTAINERERR)
006540              CONTINUE
006550         WHEN WS-RESP EQUAL DFHRESP(NORMAL)
006560              CONTINUE
006570         WHEN OTHER
006580              MOVE 604              TO IIBKERRO-CD-ERRO
006590              STRING 'SD005-Indisponibilidade de recurso ('
006600                      SRVC-ID-OPR  '-'
006610                      IIBKERRO-CD-ERRO '-'
006620                      WS-NM-PGM        '-'
006630                      WS-RESP          '-'
006640                      WS-RESP2         ')'
006650                          DELIMITED BY SIZE
006660                                  INTO IIBKERRO-TX-ERRO
006670              PERFORM 990000-ERRO
006680     END-EVALUATE.
006690*
006700 640099-GET-CTRN-ERRO.
006710     EXIT.
006720
006730*----------------------------------------------------------------*
006740 650000-GET-CTRN-RPST            SECTION.
006750*----------------------------------------------------------------*
006760*    Recupera o container com os dados devolvidos pelo provedor
006770*    do serviço.
006780*----------------------------------------------------------------*
006790*
006800     MOVE 0 TO WS-RESP
006810               WS-RESP2
006820     EXEC CICS GET
006830               CONTAINER ( 'PRM-DADOS-RPST'  )
006840               CHANNEL   ( WS-NM-CANAL       )
006850               NODATA
006860               FLENGTH   ( WS-TAM-DADOS-RPST )
006870               RESP      ( WS-RESP           )
006880               RESP2     ( WS-RESP2          )
006890     END-EXEC.
006900*
006910     EVALUATE TRUE
006920         WHEN WS-RESP EQUAL DFHRESP(NORMAL)
006930              CONTINUE
006940         WHEN OTHER
006950              STRING 'Erro get-1 container PRM-DADOS-RPST, '
006960                      SRVC-ID-OPR  '-'
006970                     'RESP=' WS-RESP ', RESP-2=' WS-RESP2
006980                     ' - ' CTE-PROG
006990                          DELIMITED BY SIZE
007000                                  INTO IIBKERRO-TX-ERRO
007010              PERFORM 990000-ERRO
007020     END-EVALUATE.
007030*
007040     EXEC CICS GET
007050               CONTAINER ( 'PRM-DADOS-RPST'  )
007060               CHANNEL   ( WS-NM-CANAL       )
007070               INTO      ( WS-DADOS-RPST     )
007080               FLENGTH   ( WS-TAM-DADOS-RPST )
007090               RESP      ( WS-RESP           )
007100               RESP2     ( WS-RESP2          )
007110     END-EXEC.
007120*
007130     EVALUATE TRUE
007140         WHEN WS-RESP EQUAL DFHRESP(NORMAL)
007150              CONTINUE
007160         WHEN OTHER
007170              STRING 'Erro get-2 container PRM-DADOS-RPST, '
007180                     'RESP=' WS-RESP ', RESP-2=' WS-RESP2
007190                     ' - ' CTE-PROG
007200                          DELIMITED BY SIZE
007210                                  INTO IIBKERRO-TX-ERRO
007220              PERFORM 990000-ERRO
007230     END-EVALUATE.
007240*
007250 650099-GET-CTRN-RPST.
007260     EXIT.
007270*
007280
007290*----------------------------------------------------------------*
007300 700000-PUT-RESPOSTA             SECTION.
007310*----------------------------------------------------------------*
007320
007330     EXEC CICS
007340         PUT CONTAINER ( 'SGTSD005-RPST' )
007350             FROM      (  KD005-SAID      )
007360     END-EXEC
007370
007380     IF  EIBRESP NOT EQUAL DFHRESP(NORMAL)
007390         MOVE EIBRESP            TO KERRO-CD-ERRO
007400         MOVE 006                TO GDA-SEQL-ERRO
007410         MOVE 'Erro ao gravar saída de dados da operação.'
007420                                 TO KERRO-TX-ERRO
007430         PERFORM 990000-ERRO
007440     END-IF
007450
007460     .
007470 700099-PUT-RESPOSTA.
007480     EXIT.
007490
007500*----------------------------------------------------------------*
007510 990000-ERRO                     SECTION.
007520*----------------------------------------------------------------*
007530
007540     EXEC CICS SYNCPOINT ROLLBACK END-EXEC
007550
007560        EXEC CICS
007570            PUT CONTAINER ( 'PRM-ERRO       ' )
007580                FROM      ( KERRO-IIB-ERRO    )
007590        END-EXEC
007600
007610     PERFORM 999000-ENCERRA
007620     .
007630 990099-ERRO.
007640     EXIT.
007650
007660*----------------------------------------------------------------*
007670 999000-ENCERRA                  SECTION.
007680*----------------------------------------------------------------*
007690
007700     EXEC CICS RETURN END-EXEC
007710
007720     .
007730 999099-ENCERRA.
007740     EXIT.
007750
007760*================================================================*