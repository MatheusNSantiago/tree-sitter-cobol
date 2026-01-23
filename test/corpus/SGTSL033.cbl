000010*-----------------------------------------------------------------
000020* PROGRAMA..: SGTSL033
000030* ANALISTA..: F8608799 - Ricardo Ramos
000040* AUTOR.....: F9188847 - Soila K L Akamatsu
000050* DATA......: 12.11.2015
000060* OBJETIVO..: Listar Permissao Usuario para Operacoes da Unidade
000070* COMPILACAO: COBOL/CICS/DB2
000080*-----------------------------------------------------------------
000090* VRS001 12.11.2015 - F9188847 DE 2015/120821 - IMPLANTAÇAO
000100*-----------------------------------------------------------------
000110
000120*-----------------------------------------------------------------
000130 IDENTIFICATION DIVISION.
000140*-----------------------------------------------------------------
000150 PROGRAM-ID. SGTSL033.
000160
000170*-----------------------------------------------------------------
000180 ENVIRONMENT DIVISION.
000190*-----------------------------------------------------------------
000200 CONFIGURATION SECTION.
000210 SPECIAL-NAMES.
000220     DECIMAL-POINT IS COMMA.
000230
000240*-----------------------------------------------------------------
000250 DATA DIVISION.
000260*-----------------------------------------------------------------
000270
000280*-----------------------------------------------------------------
000290 WORKING-STORAGE SECTION.
000300*-----------------------------------------------------------------
000310 01  CTE-PROG     PIC  X(17) VALUE '*** SGTSL033 ***'.
000320 01  CTE-VERS     PIC  X(06) VALUE 'VRS001'.
000330
000340*-----------------------------------------------------------------
000350* Book do programa de ambiente - CICS5000
000360* Definido na work para preservar as informacoes
=INC HLPKE01B                                                           00000370
000380 LOCAL-STORAGE SECTION.
000390*-----------------------------------------------------------------
000400*-----------------------------------------------------------------
000410* book de para acionamento do msg1
=INC IIBKERRO                                                           00000420
=INC MSGKWMG1                                                           00000430
=INC MSGKW999                                                           00000440
000450 01  GRP-CONTAINER.
000460     03  CANAL                        PIC X(16) VALUE SPACES.
000470     03  QT-CCT-LIDO                  PIC S9(9) COMP-5 VALUE 0.
000480     03  QT-CCT-EPRO                  PIC S9(9) COMP-5 VALUE 0.
000490     03  VL-EXBR-1                    PIC 9(10)        VALUE 0.
000500     03  VL-EXBR-2                    PIC 9(10)        VALUE 0.
000510* Area de trabalho
000520
000530 01 QT-REG-SELD                         PIC S9(4) COMP-5 VALUE 0.
000540
000550* Campos utilizados para leitura
000560
000570 01  REG-WS.
000580     03 CD-UOR-DTTR-OPR                PIC S9(9) COMP-5
000590        OCCURS 101 TIMES               VALUE 0.
000600     03 CD-USU-PMS                     PIC X(8)
000610        OCCURS 101 TIMES               VALUE ' '.
000620     03 CD-USU-RSP-ATL                 PIC X(8)
000630        OCCURS 101 TIMES               VALUE ' '.
000640     03 DT-INC-VGC-PMS                 PIC X(10)
000650        OCCURS 101 TIMES               VALUE ' '.
000660     03 DT-FIM-VGC-PMS                 PIC X(10)
000670        OCCURS 101 TIMES               VALUE ' '.
000680     03 TS-ATL-PMS                     PIC X(26)
000690        OCCURS 101 TIMES               VALUE ' '.
000700
000710
000720* Indicadores de nulidades
000730
000740 01 QT-REG-SELD-N                       PIC S9(4) COMP-5 VALUE 0.
000750
000760* Variavel(is) de controle do cursor(es)
000770
000780 01  CTL-CURSOR-PSQ.
000790     03  CD-FIM-CURSOR-PSQ            PIC S9(04) COMP-5 VALUE 0.
000800         88  FIM-CURSOR-PSQ            VALUE +100.
000810     03  IN-CURSOR-PSQ-ABTO           PIC  X(01) VALUE 'N'.
000820         88  CURSOR-PSQ-ABTO          VALUE 'S'.
000830
000840* Subrotinas utilizadas
000850
000860
000870     EXEC SQL
000880         INCLUDE SQLCA
000890     END-EXEC.
000900
000910*-----------------------------------------------------------------
=INC SGTKL033                                                           00000920
000930     EXEC SQL
000940       DECLARE CURSOR-PSQ CURSOR
000950               WITH ROWSET  POSITIONING FOR
000960       SELECT A.CD_UOR_DTTR_OPR
000970            , A.CD_USU_PMS
000980            , TO_CHAR(A.DT_INC_VGC_PMS,'DDMMYYYY')
000990            , TO_CHAR(A.DT_FIM_VGC_PMS,'DDMMYYYY')
001000            , A.CD_USU_RSP_ATL
001010            , A.TS_ATL_PMS
001020         FROM DB2SGT.PMS_USU_OPR_UOR    A
001030        WHERE
001040            A.CD_UOR_DTTR_OPR    > :SGTSL033-CD-UOR-DTTR-OPR-E
001050         OR  (A.CD_UOR_DTTR_OPR   = :SGTSL033-CD-UOR-DTTR-OPR-E
001060         AND UPPER(A.CD_USU_PMS)  >
001070                                  UPPER(:SGTSL033-CD-USU-PMS-E))
001080         OR  (A.CD_UOR_DTTR_OPR   = :SGTSL033-CD-UOR-DTTR-OPR-E
001090         AND UPPER(A.CD_USU_PMS)  = UPPER(:SGTSL033-CD-USU-PMS-E)
001100         AND A.DT_INC_VGC_PMS     >
001110         TO_CHAR(TO_DATE((:SGTSL033-DT-INC-VGC-PMS-E),'DDMMYYYY'),
001120          'DD.MM.YYYY'))
001130        ORDER BY
001140              A.CD_UOR_DTTR_OPR
001150            , A.CD_USU_PMS
001160            , A.DT_INC_VGC_PMS
001170     END-EXEC.
001180
001190*-----------------------------------------------------------------
001200 PROCEDURE DIVISION.
001210*-----------------------------------------------------------------
001220
001230
001240* Habilita tratamento automatico de erros DB2
=INC MSGKE997                                                           00001250
001260* Habilita captura automatica de abend e erros CICS
=INC MSGKE999                                                           00001270
001280
001290     PERFORM 100000-DESCOBRIR-CANAL
001300     PERFORM 110000-EXCLUIR-CTNR-RPST
001310     PERFORM 120000-EXCLUIR-CTNR-ERRO
001320     PERFORM 130000-RECEBER-REQUISICAO
001330     PERFORM 140000-VALIDAR-PARAMETRO
001340
001350     PERFORM 970101-ABRE-CURSOR-PSQ
001360     PERFORM 970102-LER-CURSOR-PSQ
001370     PERFORM 970103-FECHA-CURSOR-PSQ
001380     PERFORM 200000-PROCESSA-PESQUISA
001390              VARYING SGTSL033-IC-TAB FROM 1 BY 1
001400              UNTIL SGTSL033-IC-TAB > SGTSL033-QT-OCR-TAB
001410
001420     PERFORM 300000-POSTA-RESPOSTA
001430     GOBACK
001440     .
001450*-----------------------------------------------------------------
001460 100000-DESCOBRIR-CANAL SECTION.
001470*-----------------------------------------------------------------
001480     MOVE 0001                   TO MSG1-CD-SEQL-ERRO
001490
001500     EXEC CICS ASSIGN
001510        CHANNEL ( CANAL )
001520     END-EXEC
001530     .
001540 100000-SAI.
001550     EXIT.
001560*-----------------------------------------------------------------
001570 110000-EXCLUIR-CTNR-RPST SECTION.
001580*-----------------------------------------------------------------
001590     MOVE 0002                   TO MSG1-CD-SEQL-ERRO
001600
001610     EXEC CICS DELETE
001620        CONTAINER ( 'SGTSL033-RPST' )
001630        CHANNEL   ( CANAL      )
001640        NOHANDLE
001650     END-EXEC
001660     IF  NOT (EIBRESP EQUAL DFHRESP(NORMAL)   OR
001670         EIBRESP EQUAL DFHRESP(CONTAINERERR) )
001680         PERFORM 999002-TRATA-CONDITION-CICS
001690     END-IF
001700     .
001710 110000-SAI.
001720     EXIT.
001730*-----------------------------------------------------------------
001740 120000-EXCLUIR-CTNR-ERRO SECTION.
001750*-----------------------------------------------------------------
001760     MOVE 0003                   TO MSG1-CD-SEQL-ERRO
001770
001780     EXEC CICS DELETE
001790        CONTAINER ( 'PRM-ERRO' )
001800        CHANNEL   ( CANAL      )
001810        NOHANDLE
001820     END-EXEC
001830     IF  NOT (EIBRESP EQUAL DFHRESP(NORMAL)   OR
001840         EIBRESP EQUAL DFHRESP(CONTAINERERR) )
001850         PERFORM 999002-TRATA-CONDITION-CICS
001860     END-IF
001870     .
001880 120000-SAI.
001890     EXIT.
001900*-----------------------------------------------------------------
001910 130000-RECEBER-REQUISICAO SECTION.
001920*-----------------------------------------------------------------
001930     MOVE 0004                   TO MSG1-CD-SEQL-ERRO
001940
001950     MOVE ZEROS                       TO QT-CCT-LIDO
001960     MOVE LENGTH OF SGTSL033-RQSC      TO QT-CCT-EPRO
001970     EXEC CICS GET
001980        CONTAINER ( 'SGTSL033-RQSC' )
001990        CHANNEL   ( CANAL      )
002000        NODATA
002010        FLENGTH ( QT-CCT-LIDO )
002020     END-EXEC
002030
002040     IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
002050         MOVE 3001                    TO MSG1-CD-ERRO-TS
002060         SET  ERRO-PROGRAMADO     TO TRUE
002070         MOVE 0001                TO MSG1-CD-ERRO-TS
002080         MOVE 0005                TO MSG1-CD-SEQL-ERRO
002090         MOVE QT-CCT-EPRO             TO VL-EXBR-1
002100         MOVE QT-CCT-LIDO             TO VL-EXBR-2
002110         STRING
002120             'Container RQSC'
002130             ', TAMANHO ESPERADO: '  VL-EXBR-1
002140             ', TAMANHO RECEBIDO: '  VL-EXBR-2
002150             DELIMITED BY SIZE      INTO MSG1-TX-ERRO-TS
002160         END-STRING
002170         PERFORM 999000-GRAVA-ERRO-E-ENCERRA
002180     END-IF
002190     EXEC CICS GET
002200        CONTAINER ( 'SGTSL033-RQSC' )
002210        CHANNEL   ( CANAL      )
002220        FLENGTH ( LENGTH OF SGTSL033-RQSC )
002230        INTO    ( SGTSL033-RQSC )
002240     END-EXEC
002250     .
002260 130000-SAI.
002270     EXIT.
002280*-----------------------------------------------------------------
002290 140000-VALIDAR-PARAMETRO SECTION.
002300*-----------------------------------------------------------------
002310     MOVE 0006                   TO MSG1-CD-SEQL-ERRO
002320     MOVE 'N'                    TO SGTSL033-IN-CNTO-PSQ
002330     IF  SGTSL033-CD-UOR-DTTR-OPR-E < ZEROS
002340         SET  ERRO-PROGRAMADO     TO TRUE
002350         MOVE 0002                TO MSG1-CD-ERRO-TS
002360         MOVE 0007                TO MSG1-CD-SEQL-ERRO
002370         MOVE 'Codigo UOR detentora operação inválido.'
002380-                                 TO MSG1-TX-ERRO-TS
002390         PERFORM 999000-GRAVA-ERRO-E-ENCERRA
002400     END-IF
002410     IF  SGTSL033-DT-INC-VGC-PMS-E EQUAL SPACES
002420         MOVE '01010001'        TO SGTSL033-DT-INC-VGC-PMS-E
002430     END-IF
002440
002450     .
002460 140000-SAI.
002470     EXIT.
002480*-----------------------------------------------------------------
002490 200000-PROCESSA-PESQUISA SECTION.
002500*-----------------------------------------------------------------
002510     MOVE 0008                   TO MSG1-CD-SEQL-ERRO
002520
002530     MOVE CD-UOR-DTTR-OPR(SGTSL033-IC-TAB) TO
002540               SGTSL033-CD-UOR-DTTR-OPR(SGTSL033-IC-TAB)
002550     MOVE CD-USU-PMS(SGTSL033-IC-TAB)      TO
002560               SGTSL033-CD-USU-PMS(SGTSL033-IC-TAB)
002570     MOVE CD-USU-RSP-ATL(SGTSL033-IC-TAB)  TO
002580               SGTSL033-CD-USU-RSP-ATL(SGTSL033-IC-TAB)
002590     MOVE DT-FIM-VGC-PMS(SGTSL033-IC-TAB)  TO
002600               SGTSL033-DT-FIM-VGC-PMS(SGTSL033-IC-TAB)
002610     MOVE DT-INC-VGC-PMS(SGTSL033-IC-TAB)  TO
002620               SGTSL033-DT-INC-VGC-PMS(SGTSL033-IC-TAB)
002630     MOVE TS-ATL-PMS(SGTSL033-IC-TAB)      TO
002640               SGTSL033-TS-ATL-PMS(SGTSL033-IC-TAB)
002650     .
002660 200000-SAI.
002670     EXIT.
002680*-----------------------------------------------------------------
002690 970101-ABRE-CURSOR-PSQ SECTION.
002700*-----------------------------------------------------------------
002710     MOVE 0009                   TO MSG1-CD-SEQL-ERRO
002720     EXEC SQL
002730          OPEN CURSOR-PSQ
002740     END-EXEC
002750     MOVE 'S'                    TO IN-CURSOR-PSQ-ABTO
002760     .
002770 970101-SAI.
002780     EXIT.
002790*-----------------------------------------------------------------
002800 970102-LER-CURSOR-PSQ SECTION.
002810*-----------------------------------------------------------------
002820     MOVE 0010                   TO MSG1-CD-SEQL-ERRO
002830     INITIALIZE REG-WS
002840     EXEC SQL
002850          FETCH NEXT ROWSET
002860                CURSOR-PSQ FOR 0101 ROWS
002870         INTO :CD-UOR-DTTR-OPR
002880            , :CD-USU-PMS
002890            , :DT-INC-VGC-PMS
002900            , :DT-FIM-VGC-PMS
002910            , :CD-USU-RSP-ATL
002920            , :TS-ATL-PMS
002930     END-EXEC
002940     MOVE SQLERRD(3)             TO QT-REG-SELD
002950     IF  QT-REG-SELD EQUAL 0101
002960         SET SGTSL033-TEM-MAIS-REGISTRO TO TRUE
002970         MOVE 0100                TO QT-REG-SELD
002980     END-IF
002990     MOVE QT-REG-SELD            TO SGTSL033-QT-OCR-TAB
003000     MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ
003010     .
003020 970102-SAI.
003030     EXIT.
003040*-----------------------------------------------------------------
003050 970103-FECHA-CURSOR-PSQ SECTION.
003060*-----------------------------------------------------------------
003070     MOVE 0011                   TO MSG1-CD-SEQL-ERRO
003080     MOVE 'N'                    TO IN-CURSOR-PSQ-ABTO
003090     EXEC SQL
003100          CLOSE CURSOR-PSQ
003110     END-EXEC
003120     .
003130 970103-SAI.
003140     EXIT.
003150*-----------------------------------------------------------------
003160 300000-POSTA-RESPOSTA SECTION.
003170*-----------------------------------------------------------------
003180     MOVE 0012                   TO MSG1-CD-SEQL-ERRO
003190     EXEC CICS PUT
003200        CONTAINER ( 'SGTSL033-RPST' )
003210        CHANNEL   ( CANAL      )
003220        FLENGTH ( LENGTH OF SGTSL033-RPST )
003230        FROM    ( SGTSL033-RPST )
003240     END-EXEC
003250     .
003260 300000-SAI.
003270     EXIT.
003280*-----------------------------------------------------------------
003290 999000-GRAVA-ERRO-E-ENCERRA SECTION.
003300*-----------------------------------------------------------------
003310
003320     MOVE 'SGTSL033-CD-UOR-DTTR-OPR'   TO MSGKW999-NM-VRV-MNT
003330     MOVE SGTSL033-CD-UOR-DTTR-OPR-E   TO MSGKW999-VL-VRV-MNT
003340     PERFORM 999003-ADICIONA-VRV-MNT
003350     MOVE 'SGTSL033-CD-USU-PMS'        TO MSGKW999-NM-VRV-MNT
003360     MOVE SGTSL033-CD-USU-PMS-E        TO MSGKW999-VL-VRV-MNT
003370     PERFORM 999003-ADICIONA-VRV-MNT
003380     MOVE 'SGTSL033-DT-INC-VGC-PMS'    TO MSGKW999-NM-VRV-MNT
003390     MOVE SGTSL033-DT-INC-VGC-PMS-E    TO MSGKW999-VL-VRV-MNT
003400     PERFORM 999003-ADICIONA-VRV-MNT
003410* --> grava o erro no msg
003420     PERFORM MSG1-NTF-MSG
003430     MOVE MSG1-CD-ERRO-TS             TO IIBKERRO-CD-ERRO
003440     MOVE MSG1-TX-ERRO-TS             TO IIBKERRO-TX-ERRO
003450     EXEC CICS PUT
003460        CONTAINER ( 'PRM-ERRO' )
003470        CHANNEL   ( CANAL      )
003480        FROM      ( IIBKERRO-PRM-ERRO)
003490        FLENGTH ( LENGTH OF IIBKERRO-PRM-ERRO )
003500        NOHANDLE
003510     END-EXEC
003520
003530     PERFORM 999999-ENCERRAR
003540     .
003550 999000-SAI.
003560     EXIT.
003570* --------------------------------------*
003580* 999001-TRATA-ABEND-CICS        SECTION.
003590* 999002-TRATA-CONDITION-CICS    SECTION.
003600* 999003-ADICIONA-VRV-MNT        SECTION.
003610* --------------------------------------*
=INC MSGKP999                                                           00003620
003630* --------------------------------------*
003640* 999010-TRATA-ERRO-DB2          SECTION.
=INC MSGKP997                                                           00003650
003660* --------------------------------------*
003670* MSG1-NTF-MSG                   SECTION.
=INC MSGKPMG1                                                           00003680
003690* --------------------------------------*
003700 999999-ENCERRAR                SECTION.
003710* --------------------------------------*
003720     EXEC CICS
003730          RETURN
003740     END-EXEC.