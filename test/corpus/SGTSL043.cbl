000010*-----------------------------------------------------------------
000020* PROGRAMA..: SGTSL043
000030* ANALISTA..: F2054109 - CINTIA APARECIDA SOARES IGLESIAS
000040* AUTOR.....: F2804421 - ELIANA CRISTINA RAMOS DA SILVA
000050* OBJETIVO..: CONSULTA TABELA DB2SGT.EST_AG_CLSC_EXNO
000060* COMPILACAO: COBOL/CICS/DB2
000070*-----------------------------------------------------------------
000080* VRS001 23.11.2015 - F2804421 - DE 2013/120821 - IMPLANTAÇãO
000090
000100*-----------------------------------------------------------------
000110 IDENTIFICATION DIVISION.
000120*-----------------------------------------------------------------
000130 PROGRAM-ID. SGTSL043.
000140
000150*-----------------------------------------------------------------
000160 ENVIRONMENT DIVISION.
000170*-----------------------------------------------------------------
000180 CONFIGURATION SECTION.
000190 SPECIAL-NAMES.
000200     DECIMAL-POINT IS COMMA.
000210
000220*-----------------------------------------------------------------
000230 DATA DIVISION.
000240*-----------------------------------------------------------------
000250
000260*-----------------------------------------------------------------
000270 WORKING-STORAGE SECTION.
000280*-----------------------------------------------------------------
000290 01  CTE-PROG     PIC  X(17) VALUE '*** SGTSL043 ***'.
000300 01  CTE-VERS     PIC  X(06) VALUE 'VRS001'.
000310
000320*-----------------------------------------------------------------
000330* Book do programa de ambiente - CICS5000
000340* Definido na work para preservar as informacoes
=INC HLPKE01B                                                           00000350
000360 LOCAL-STORAGE SECTION.
000370*-----------------------------------------------------------------
000380* Area de trabalho
000390
000400 01 QT-REG-SELD                         PIC S9(4) COMP-5 VALUE 0.
000410
000420* Campos utilizados para leitura
000430
000440 01  REG-WS.
000450     03 CD-TIP-EST-DCS                 PIC S9(4) COMP-5 VALUE 0.
000460     03 CD-USU-ATL-EST                 PIC X(8) VALUE ' '.
000470     03 TS-EST-AG-CLSC                 PIC X(26) VALUE ' '.
000480     03 TX-OBS-EST-AG-CLSC             PIC X(60) VALUE ' '.
000490
000500* Indicadores de nulidades
000510
000520 01 QT-REG-SELD-N                       PIC S9(4) COMP-5 VALUE 0.
000530 01 TX-OBS-EST-AG-CLSC-N               PIC S9(4) COMP-5 VALUE 0.
000540
000550* Variavel(is) de controle do cursor(es)
000560
000570 01  CTL-CURSOR-PSQ.
000580     03  CD-FIM-CURSOR-PSQ            PIC S9(04) COMP-5 VALUE 0.
000590         88  FIM-CURSOR-PSQ            VALUE +100.
000600     03  IN-CURSOR-PSQ-ABTO           PIC  X(01) VALUE 'N'.
000610         88  CURSOR-PSQ-ABTO          VALUE 'S'.
000620
000630* Subrotinas utilizadas
000640
000650* Books da definicao de dados a serem usados na subrotina de erros
000660
=INC HLPKE001                                                           00000670
000680
000690     EXEC SQL
000700         INCLUDE SQLCA
000710     END-EXEC.
000720
000730*-----------------------------------------------------------------
000740 LINKAGE SECTION.
000750*-----------------------------------------------------------------
000760 01  DFHCOMMAREA.
=INC SGTKL043                                                           00000770
000780     EXEC SQL
000790       DECLARE CURSOR-PSQ CURSOR  FOR
000800       SELECT A.CD_TIP_EST_DCS
000810            , A.CD_USU_ATL_EST
000820            , A.TS_EST_AG_CLSC
000830            , A.TX_OBS_EST_AG_CLSC
000840         FROM DB2SGT.EST_AG_CLSC_EXNO   A
000850        WHERE A.CD_AG_CLSC_RSCO    = :SGTSL043-CD-AG-CLSC-RSCO
000860          AND A.NR_ETRD_AG_CLSC    = :SGTSL043-NR-ETRD-AG-CLSC
000870          AND A.TS_EST_AG_CLSC     > :SGTSL043-ULT-TS-EST-RET
000880     END-EXEC.
000890
000900*-----------------------------------------------------------------
000910 PROCEDURE DIVISION USING DFHCOMMAREA.
000920*-----------------------------------------------------------------
000930     IF  EIBCALEN NOT EQUAL LENGTH OF DFHCOMMAREA
000940         MOVE 'Area com tamanho invalido - verifique book/local'
000950                                 TO MSG-ERRO
000960         PERFORM 999001-ERRO-001
000970     END-IF
000980     IF NOT EXECUCAO-ONLINE AND NOT EXECUCAO-BATCH
000990       CALL CICS5000 USING CICS5000-COMMAREA
001000     END-IF
001010* WHENEVER DB2
=INC HLPKE002                                                           00001020
001030* WHENEVER CICS
=INC HLPKE003                                                           00001040
001050
001060     PERFORM 100000-VALIDAR-PARAMETRO
001070
001080     PERFORM 970101-ABRE-CURSOR-PSQ
001090     PERFORM 970102-LER-CURSOR-PSQ
001100     PERFORM 200000-PROCESSA-PESQUISA UNTIL FIM-CURSOR-PSQ
001110              OR SGTSL043-PREENCHIDO-TAB
001120     IF  NOT FIM-CURSOR-PSQ
001130         MOVE 'S'                TO SGTSL043-IN-CNTO-PSQ
001140     END-IF
001150     PERFORM 970103-FECHA-CURSOR-PSQ
001160
001170     GOBACK
001180     .
001190*-----------------------------------------------------------------
001200 100000-VALIDAR-PARAMETRO SECTION.
001210*-----------------------------------------------------------------
001220     MOVE 0001                   TO PSC-ERRO-PGM
001230     INITIALIZE SGTSL043-AREA-DADOS
001240     MOVE 'N'                    TO SGTSL043-IN-CNTO-PSQ
001250     IF  SGTSL043-CD-AG-CLSC-RSCO EQUAL ZEROS
001260         MOVE 'Codigo agencia classificacao risco Invalido'
001270                                   TO MSG-ERRO
001280         PERFORM 999001-ERRO-001
001290     END-IF
001300
001310     IF  SGTSL043-NR-ETRD-AG-CLSC EQUAL ZEROS
001320         MOVE 'Numero estruturado agencia classificacao Invalido'
001330                                   TO MSG-ERRO
001340         PERFORM 999001-ERRO-001
001350     END-IF
001360
001370***------------------------------------------------------------***
001380***---- Campo SGTSL043-TS-ULT-EST-RET:                         ***
001390***---- For igual a brancos, indica que é 1a leitura no banco  ***
001400***---- For dif  de brancos, indica reg partida da prox.leitura.**
001410***------------------------------------------------------------***
001420
001430     IF  SGTSL043-ULT-TS-EST-RET  EQUAL SPACES
001440         MOVE '1900-01-01-00.00.00.000000'
001450                                   TO  SGTSL043-ULT-TS-EST-RET
001460     END-IF
001470     .
001480
001490 100000-SAI.
001500     EXIT.
001510*-----------------------------------------------------------------
001520 200000-PROCESSA-PESQUISA SECTION.
001530*-----------------------------------------------------------------
001540     MOVE 0002                   TO PSC-ERRO-PGM
001550
001560     ADD  1                 TO SGTSL043-QT-OCR-TAB SGTSL043-IC-TAB
001570     IF  SGTSL043-ESTOURO-TAB
001580         MOVE 'Tabela da linkage estourou'  TO MSG-ERRO
001590         PERFORM 999001-ERRO-001
001600     END-IF
001610     MOVE CD-TIP-EST-DCS               TO
001620               SGTSL043-CD-TIP-EST-DCS(SGTSL043-IC-TAB)
001630     MOVE CD-USU-ATL-EST               TO
001640               SGTSL043-CD-USU-ATL-EST(SGTSL043-IC-TAB)
001650     MOVE TS-EST-AG-CLSC               TO
001660               SGTSL043-TS-EST-AG-CLSC(SGTSL043-IC-TAB)
001670     MOVE TX-OBS-EST-AG-CLSC           TO
001680               SGTSL043-TX-OBS-EST-AG-CLSC(SGTSL043-IC-TAB)
001690     PERFORM 970102-LER-CURSOR-PSQ
001700     .
001710 200000-SAI.
001720     EXIT.
001730*-----------------------------------------------------------------
001740 970101-ABRE-CURSOR-PSQ SECTION.
001750*-----------------------------------------------------------------
001760     MOVE 0003                   TO PSC-ERRO-PGM
001770     EXEC SQL
001780          OPEN CURSOR-PSQ
001790     END-EXEC
001800     MOVE 'S'                    TO IN-CURSOR-PSQ-ABTO
001810     .
001820 970101-SAI.
001830     EXIT.
001840*-----------------------------------------------------------------
001850 970102-LER-CURSOR-PSQ SECTION.
001860*-----------------------------------------------------------------
001870     MOVE 0004                   TO PSC-ERRO-PGM
001880     INITIALIZE REG-WS
001890     EXEC SQL
001900          FETCH CURSOR-PSQ
001910         INTO :CD-TIP-EST-DCS
001920            , :CD-USU-ATL-EST
001930            , :TS-EST-AG-CLSC
001940            , :TX-OBS-EST-AG-CLSC
001950              :TX-OBS-EST-AG-CLSC-N
001960     END-EXEC
001970     MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ
001980     .
001990 970102-SAI.
002000     EXIT.
002010*-----------------------------------------------------------------
002020 970103-FECHA-CURSOR-PSQ SECTION.
002030*-----------------------------------------------------------------
002040     MOVE 0005                   TO PSC-ERRO-PGM
002050     MOVE 'N'                    TO IN-CURSOR-PSQ-ABTO
002060     EXEC SQL
002070          CLOSE CURSOR-PSQ
002080     END-EXEC
002090     .
002100 970103-SAI.
002110     EXIT.
002120*--------------------
002130* 999000-ERRO SECTION
002140*--------------------
002150*-----------------
002160* 999001-ERRO-001.
002170*-----------------
=INC HLPKE004                                                           00002180
=INC HLPKE012                                                           00002190
002200     IF  CURSOR-PSQ-ABTO
002210         PERFORM 970103-FECHA-CURSOR-PSQ
002220     END-IF
002230     MOVE RTN-PGM                      TO SGTSL043-RTN-PGM
002240     GOBACK
002250     .
002260*-----------------
002270* 999002-ERRO-DB2
002280*-----------------
=INC HLPKE005                                                           00002290
002300*-----------------
002310* 999003-TRATA-ABEND-CICS
002320* 999004-TRATA-CONDITION-CICS
002330*-----------------
=INC HLPKE006                                                           00002340