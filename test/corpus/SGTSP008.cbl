000010*-----------------------------------------------------------------
000020* PROGRAMA..: SGTSP008
000030* ANALISTA..: F2054109 - CINTIA APARECIDA SOARES IGLESIAS
000040* AUTOR.....: F2804391 - ELIANA TEMPERANI
000050* OBJETIVO..: LISTAR TIPO DE ATRIBUTO VINCULADO A CARACTERISTICA
000060*             DE OPERACAO
000070* COMPILACAO: COBOL/CICS/DB2
000080*-----------------------------------------------------------------
000090* VRS001 12.11.2015 - F2804391 - DE 2013/120821 - IMPLANTACAO
000100*-----------------------------------------------------------------
000110 IDENTIFICATION DIVISION.
000120*-----------------------------------------------------------------
000130 PROGRAM-ID. SGTSP008.
000140*-----------------------------------------------------------------
000150 ENVIRONMENT DIVISION.
000160*-----------------------------------------------------------------
000170 CONFIGURATION SECTION.
000180 SPECIAL-NAMES.
000190     DECIMAL-POINT IS COMMA.
000200
000210*-----------------------------------------------------------------
000220 DATA DIVISION.
000230*-----------------------------------------------------------------
000240
000250*-----------------------------------------------------------------
000260 WORKING-STORAGE SECTION.
000270*-----------------------------------------------------------------
000280 01  CTE-PROG     PIC  X(17) VALUE '*** SGTSP008 ***'.
000290 01  CTE-VERS     PIC  X(06) VALUE 'VRS001'.
000300
000310*-----------------------------------------------------------------
000320* Book do programa de ambiente - CICS5000
000330* Definido na work para preservar as informacoes
=INC HLPKE01B                                                           00000340
000350 LOCAL-STORAGE SECTION.
000360*-----------------------------------------------------------------
000370*-----------------------------------------------------------------
000380* book de para acionamento do msg1
=INC IIBKERRO                                                           00000390
=INC MSGKWMG1                                                           00000400
=INC MSGKW999                                                           00000410
000420 01  GRP-CONTAINER.
000430     03  CANAL                        PIC X(16) VALUE SPACES.
000440     03  QT-CCT-LIDO                  PIC S9(9) COMP-5 VALUE 0.
000450     03  QT-CCT-EPRO                  PIC S9(9) COMP-5 VALUE 0.
000460     03  VL-EXBR-1                    PIC 9(10)        VALUE 0.
000470     03  VL-EXBR-2                    PIC 9(10)        VALUE 0.
000480* Area de trabalho
000490
000500 01 QT-REG-SELD                         PIC S9(4) COMP-5 VALUE 0.
000510
000520* Campos utilizados para leitura
000530
000540 01  REG-WS.
000550     03  CD-TIP-CRCT-OPR               PIC S9(04) COMP-5
000560         OCCURS 41 TIMES               VALUE 0.
000570     03  TX-TIP-ATB-OPR                PIC X(250)
000580         OCCURS 41 TIMES               VALUE ' '.
000590     03  CD-TIP-ATB-CRCT               PIC S9(04) COMP-5
000600         OCCURS 41 TIMES               VALUE 0.
000610     03  TX-RTL-ATB-OPR                PIC X(60)
000620         OCCURS 41 TIMES               VALUE ' '.
000630     03  TX-CMPT-ATB-OPR-GRP OCCURS 41 TIMES.
000640         49  TX-CMPT-ATB-OPR-LEN
000650                                       PIC S9(04) COMP-5 VALUE 0.
000660         49  TX-CMPT-ATB-OPR           PIC X(450) VALUE ' '.
000670
000680
000690* Indicadores de nulidades
000700
000710 01 QT-REG-SELD-N                       PIC S9(4) COMP-5 VALUE 0.
000720
000730* Variavel(is) de controle do cursor(es)
000740
000750 01  CTL-CURSOR-PSQ.
000760     03  CD-FIM-CURSOR-PSQ            PIC S9(04) COMP-5 VALUE 0.
000770         88  FIM-CURSOR-PSQ            VALUE +100.
000780     03  IN-CURSOR-PSQ-ABTO           PIC  X(01) VALUE 'N'.
000790         88  CURSOR-PSQ-ABTO          VALUE 'S'.
000800
000810* Subrotinas utilizadas
000820
000830
000840     EXEC SQL
000850         INCLUDE SQLCA
000860     END-EXEC.
000870
000880*-----------------------------------------------------------------
=INC SGTKP008                                                           00000890
000900     EXEC SQL
000910       DECLARE CURSOR-PSQ CURSOR
000920               WITH ROWSET POSITIONING FOR
000930     SELECT CD_TIP_CRCT_OPR
000940          , TX_TIP_ATB_OPR
000950          , CD_TIP_ATB_CRCT
000960          , TX_RTL_ATB_OPR
000970          , TX_CMPT_ATB_OPR
000980     FROM DB2SGT.TIP_ATB_VCLD_OPR
000990     WHERE CD_TIP_CRCT_OPR > : SGTSP008-CD-TIP-CRCT-OPR-ULT
001000     ORDER BY
001010     CD_TIP_CRCT_OPR
001020     END-EXEC.
001030
001040*-----------------------------------------------------------------
001050 PROCEDURE DIVISION.
001060*-----------------------------------------------------------------
001070
001080
001090* Habilita tratamento automatico de erros DB2
=INC MSGKE997                                                           00001100
001110* Habilita captura automatica de abend e erros CICS
=INC MSGKE999                                                           00001120
001130
001140     PERFORM 100000-DESCOBRIR-CANAL
001150     PERFORM 110000-EXCLUIR-CTNR-RPST
001160     PERFORM 120000-EXCLUIR-CTNR-ERRO
001170     PERFORM 130000-RECEBER-REQUISICAO
001180     PERFORM 140000-VALIDAR-PARAMETRO
001190
001200     PERFORM 970101-ABRE-CURSOR-PSQ
001210     PERFORM 970102-LER-CURSOR-PSQ
001220     PERFORM 970103-FECHA-CURSOR-PSQ
001230     PERFORM 200000-PROCESSA-PESQUISA
001240              VARYING SGTSP008-IC-TAB FROM 1 BY 1
001250              UNTIL SGTSP008-IC-TAB > SGTSP008-QT-OCR-TAB
001260
001270     PERFORM 300000-POSTA-RESPOSTA
001280     GOBACK
001290     .
001300*-----------------------------------------------------------------
001310 100000-DESCOBRIR-CANAL SECTION.
001320*-----------------------------------------------------------------
001330     MOVE 0001                   TO MSG1-CD-SEQL-ERRO
001340
001350     EXEC CICS ASSIGN
001360        CHANNEL ( CANAL )
001370     END-EXEC
001380     .
001390 100000-SAI.
001400     EXIT.
001410*-----------------------------------------------------------------
001420 110000-EXCLUIR-CTNR-RPST SECTION.
001430*-----------------------------------------------------------------
001440     MOVE 0002                   TO MSG1-CD-SEQL-ERRO
001450
001460     EXEC CICS DELETE
001470        CONTAINER ( 'SGTSP008-RPST' )
001480        CHANNEL   ( CANAL      )
001490        NOHANDLE
001500     END-EXEC
001510     IF  NOT (EIBRESP EQUAL DFHRESP(NORMAL)   OR
001520         EIBRESP EQUAL DFHRESP(CONTAINERERR) )
001530         PERFORM 999002-TRATA-CONDITION-CICS
001540     END-IF
001550     .
001560 110000-SAI.
001570     EXIT.
001580*-----------------------------------------------------------------
001590 120000-EXCLUIR-CTNR-ERRO SECTION.
001600*-----------------------------------------------------------------
001610     MOVE 0003                   TO MSG1-CD-SEQL-ERRO
001620
001630     EXEC CICS DELETE
001640        CONTAINER ( 'PRM-ERRO' )
001650        CHANNEL   ( CANAL      )
001660        NOHANDLE
001670     END-EXEC
001680     IF  NOT (EIBRESP EQUAL DFHRESP(NORMAL)   OR
001690         EIBRESP EQUAL DFHRESP(CONTAINERERR) )
001700         PERFORM 999002-TRATA-CONDITION-CICS
001710     END-IF
001720     .
001730 120000-SAI.
001740     EXIT.
001750*-----------------------------------------------------------------
001760 130000-RECEBER-REQUISICAO SECTION.
001770*-----------------------------------------------------------------
001780     MOVE 0004                   TO MSG1-CD-SEQL-ERRO
001790
001800     MOVE ZEROS                       TO QT-CCT-LIDO
001810     MOVE LENGTH OF SGTSP008-RQSC      TO QT-CCT-EPRO
001820     EXEC CICS GET
001830        CONTAINER ( 'SGTSP008-RQSC' )
001840        CHANNEL   ( CANAL      )
001850        NODATA
001860        FLENGTH ( QT-CCT-LIDO )
001870     END-EXEC
001880
001890     IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
001900         MOVE 3001                    TO MSG1-CD-ERRO-TS
001910         SET  ERRO-PROGRAMADO     TO TRUE
001920         MOVE 0001                TO MSG1-CD-ERRO-TS
001930         MOVE 0005                TO MSG1-CD-SEQL-ERRO
001940         MOVE QT-CCT-EPRO             TO VL-EXBR-1
001950         MOVE QT-CCT-LIDO             TO VL-EXBR-2
001960         STRING
001970             'Container RQSC'
001980             ', TAMANHO ESPERADO: '  VL-EXBR-1
001990             ', TAMANHO RECEBIDO: '  VL-EXBR-2
002000             DELIMITED BY SIZE      INTO MSG1-TX-ERRO-TS
002010         END-STRING
002020         PERFORM 999000-GRAVA-ERRO-E-ENCERRA
002030     END-IF
002040     EXEC CICS GET
002050        CONTAINER ( 'SGTSP008-RQSC' )
002060        CHANNEL   ( CANAL      )
002070        FLENGTH ( LENGTH OF SGTSP008-RQSC )
002080        INTO    ( SGTSP008-RQSC )
002090     END-EXEC
002100     .
002110 130000-SAI.
002120     EXIT.
002130*-----------------------------------------------------------------
002140 140000-VALIDAR-PARAMETRO SECTION.
002150*-----------------------------------------------------------------
002160     MOVE 0006                   TO MSG1-CD-SEQL-ERRO
002170     MOVE 'N'                    TO SGTSP008-IN-CTNC-PSQ
002180     .
002190 140000-SAI.
002200     EXIT.
002210*-----------------------------------------------------------------
002220 200000-PROCESSA-PESQUISA SECTION.
002230*-----------------------------------------------------------------
002240     MOVE 0007                   TO MSG1-CD-SEQL-ERRO
002250
002260     MOVE CD-TIP-CRCT-OPR(SGTSP008-IC-TAB) TO
002270               SGTSP008-CD-TIP-CRCT-OPR(SGTSP008-IC-TAB)
002280     MOVE TX-TIP-ATB-OPR(SGTSP008-IC-TAB) TO
002290               SGTSP008-TX-TIP-ATB-OPR(SGTSP008-IC-TAB)
002300     MOVE CD-TIP-ATB-CRCT(SGTSP008-IC-TAB) TO
002310               SGTSP008-CD-TIP-ATB-CRCT(SGTSP008-IC-TAB)
002320     MOVE TX-RTL-ATB-OPR(SGTSP008-IC-TAB) TO
002330               SGTSP008-TX-RTL-ATB-OPR(SGTSP008-IC-TAB)
002340     MOVE TX-CMPT-ATB-OPR(SGTSP008-IC-TAB) TO
002350               SGTSP008-TX-CMPT-ATB-OPR(SGTSP008-IC-TAB)
002360     .
002370 200000-SAI.
002380     EXIT.
002390*-----------------------------------------------------------------
002400 970101-ABRE-CURSOR-PSQ SECTION.
002410*-----------------------------------------------------------------
002420     MOVE 0008                   TO MSG1-CD-SEQL-ERRO
002430     EXEC SQL
002440          OPEN CURSOR-PSQ
002450     END-EXEC
002460     MOVE 'S'                    TO IN-CURSOR-PSQ-ABTO
002470     .
002480 970101-SAI.
002490     EXIT.
002500*-----------------------------------------------------------------
002510 970102-LER-CURSOR-PSQ SECTION.
002520*-----------------------------------------------------------------
002530     MOVE 0009                   TO MSG1-CD-SEQL-ERRO
002540     INITIALIZE REG-WS
002550     EXEC SQL
002560          FETCH NEXT ROWSET
002570                CURSOR-PSQ FOR 0041 ROWS
002580         INTO :CD-TIP-CRCT-OPR
002590            , :TX-TIP-ATB-OPR
002600            , :CD-TIP-ATB-CRCT
002610            , :TX-RTL-ATB-OPR
002620            , :TX-CMPT-ATB-OPR-GRP
002630     END-EXEC
002640     MOVE SQLERRD(3)             TO QT-REG-SELD
002650     IF  QT-REG-SELD EQUAL 0041
002660         SET SGTSP008-TEM-MAIS-REGISTRO TO TRUE
002670         MOVE 0040                TO QT-REG-SELD
002680     END-IF
002690     MOVE QT-REG-SELD            TO SGTSP008-QT-OCR-TAB
002700     MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ
002710     .
002720 970102-SAI.
002730     EXIT.
002740*-----------------------------------------------------------------
002750 970103-FECHA-CURSOR-PSQ SECTION.
002760*-----------------------------------------------------------------
002770     MOVE 0010                   TO MSG1-CD-SEQL-ERRO
002780     MOVE 'N'                    TO IN-CURSOR-PSQ-ABTO
002790     EXEC SQL
002800          CLOSE CURSOR-PSQ
002810     END-EXEC
002820     .
002830 970103-SAI.
002840     EXIT.
002850*-----------------------------------------------------------------
002860 300000-POSTA-RESPOSTA SECTION.
002870*-----------------------------------------------------------------
002880     MOVE 0011                   TO MSG1-CD-SEQL-ERRO
002890     EXEC CICS PUT
002900        CONTAINER ( 'SGTSP008-RPST' )
002910        CHANNEL   ( CANAL      )
002920        FLENGTH ( LENGTH OF SGTSP008-RPST )
002930        FROM    ( SGTSP008-RPST )
002940     END-EXEC
002950     .
002960 300000-SAI.
002970     EXIT.
002980*-----------------------------------------------------------------
002990 999000-GRAVA-ERRO-E-ENCERRA SECTION.
003000*-----------------------------------------------------------------
003010
003020* --> grava o erro no msg
003030     PERFORM MSG1-NTF-MSG
003040     MOVE MSG1-CD-ERRO-TS             TO IIBKERRO-CD-ERRO
003050     MOVE MSG1-TX-ERRO-TS             TO IIBKERRO-TX-ERRO
003060     EXEC CICS PUT
003070        CONTAINER ( 'PRM-ERRO' )
003080        CHANNEL   ( CANAL      )
003090        FROM      ( IIBKERRO-PRM-ERRO)
003100        FLENGTH ( LENGTH OF IIBKERRO-PRM-ERRO )
003110        NOHANDLE
003120     END-EXEC
003130
003140     PERFORM 999999-ENCERRAR
003150     .
003160 999000-SAI.
003170     EXIT.
003180* --------------------------------------*
003190* 999001-TRATA-ABEND-CICS        SECTION.
003200* 999002-TRATA-CONDITION-CICS    SECTION.
003210* 999003-ADICIONA-VRV-MNT        SECTION.
003220* --------------------------------------*
=INC MSGKP999                                                           00003230
003240* --------------------------------------*
003250* 999010-TRATA-ERRO-DB2          SECTION.
=INC MSGKP997                                                           00003260
003270* --------------------------------------*
003280* MSG1-NTF-MSG                   SECTION.
=INC MSGKPMG1                                                           00003290
003300* --------------------------------------*
003310 999999-ENCERRAR                SECTION.
003320* --------------------------------------*
003330     EXEC CICS
003340          RETURN
003350     END-EXEC.