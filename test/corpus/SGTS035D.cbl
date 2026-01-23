000010*-----------------------------------------------------------------
000020* PROGRAMA..: SGTS035D
000030* ANALISTA..: F8608799 - RICARDO RAMOS
000040* AUTOR.....: F8608799 - RICARDO RAMOS
000050* OBJETIVO..: Consultar permissão de usuário com maior data fim
000060*             de vigência
000070* COMPILACAO: COBOL/CICS/DB2
000080*-----------------------------------------------------------------
000090* VRS001 12/01/2016 F8608799 DE 2013/120821 - Implantação
000100*-----------------------------------------------------------------
000110*-----------------------------------------------------------------
000120 IDENTIFICATION DIVISION.
000130*-----------------------------------------------------------------
000140 PROGRAM-ID. SGTS035D.
000150
000160*-----------------------------------------------------------------
000170 ENVIRONMENT DIVISION.
000180*-----------------------------------------------------------------
000190 CONFIGURATION                   SECTION.
000200 SPECIAL-NAMES.
000210     DECIMAL-POINT IS COMMA.
000220
000230*-----------------------------------------------------------------
000240 DATA DIVISION.
000250*-----------------------------------------------------------------
000260
000270*-----------------------------------------------------------------
000280 WORKING-STORAGE                 SECTION.
000290*-----------------------------------------------------------------
000300 01  CTE-PROG     PIC  X(17) VALUE '*** SGTS035D ***'.
000310 01  CTE-VERS     PIC  X(06) VALUE 'VRS001'.
000320
000330* Book do programa de ambiente - CICS5000
000340* Definido na work para preservar as informacoes
=INC HLPKE01B                                                           00000350
000360*-----------------------------------------------------------------
000370 LOCAL-STORAGE                   SECTION.
000380*-----------------------------------------------------------------
000390* Book de para acionamento do MSG1
=INC IIBKERRO                                                           00000400
=INC MSGKWMG1                                                           00000410
=INC MSGKW999                                                           00000420
000430 01  GRP-CONTAINER.
000440     03  CANAL                        PIC X(16) VALUE SPACES.
000450     03  QT-CCT-LIDO                  PIC S9(9) COMP-5 VALUE 0.
000460     03  QT-CCT-EPRO                  PIC S9(9) COMP-5 VALUE 0.
000470     03  VL-EXBR-1                    PIC 9(10)        VALUE 0.
000480     03  VL-EXBR-2                    PIC 9(10)        VALUE 0.
000490* Area de trabalho
000500
000510 01  QT-REG-SELD                      PIC S9(4) COMP-5 VALUE 0.
000520
000530* Indicadores de nulidades
000540
000550 01  QT-REG-SELD-NULO                 PIC S9(4) COMP-5 VALUE 0.
000560
000570     EXEC SQL
000580         INCLUDE SQLCA
000590     END-EXEC.
000600
=INC SGTK035D                                                           00000610
000620*-----------------------------------------------------------------
000630 PROCEDURE DIVISION.
000640*-----------------------------------------------------------------
000650     IF NOT EXECUCAO-ONLINE AND NOT EXECUCAO-BATCH
000660       CALL CICS5000 USING CICS5000-COMMAREA
000670     END-IF
000680     MOVE 0001                   TO MSG1-CD-SEQL-ERRO
000690* Habilita tratamento automatico de Erros DB2
=INC MSGKE997                                                           00000700
000710* Habilita captura automatica de Abend e Erros CICS
=INC MSGKE999                                                           00000720
000730
000740     PERFORM 100000-DESCOBRIR-CANAL
000750     PERFORM 110000-EXCLUIR-CTNR-RPST
000760     PERFORM 120000-EXCLUIR-CTNR-ERRO
000770     PERFORM 130000-RECEBER-REQUISICAO
000780     PERFORM 140000-VALIDA-PARAMETRO
000790
000800     PERFORM 200000-PROCESSAR
000810     PERFORM 300000-POSTA-RESPOSTA
000820
000830     GOBACK
000840     .
000850*-----------------------------------------------------------------
000860 100000-DESCOBRIR-CANAL          SECTION.
000870*-----------------------------------------------------------------
000880     MOVE 0002                   TO MSG1-CD-SEQL-ERRO
000890
000900     EXEC CICS ASSIGN
000910        CHANNEL ( CANAL )
000920     END-EXEC
000930     .
000940 100000-SAI.
000950     EXIT.
000960*-----------------------------------------------------------------
000970 110000-EXCLUIR-CTNR-RPST        SECTION.
000980*-----------------------------------------------------------------
000990     MOVE 0003                   TO MSG1-CD-SEQL-ERRO
001000
001010     EXEC CICS DELETE
001020        CONTAINER ( 'SGTS035D-RPST' )
001030        CHANNEL   ( CANAL      )
001040        NOHANDLE
001050     END-EXEC
001060     IF  NOT (EIBRESP EQUAL DFHRESP(NORMAL)   OR
001070         EIBRESP EQUAL DFHRESP(CONTAINERERR) )
001080         PERFORM 999002-TRATA-CONDITION-CICS
001090     END-IF
001100     .
001110 110000-SAI.
001120     EXIT.
001130*-----------------------------------------------------------------
001140 120000-EXCLUIR-CTNR-ERRO        SECTION.
001150*-----------------------------------------------------------------
001160     MOVE 0004                   TO MSG1-CD-SEQL-ERRO
001170
001180     EXEC CICS DELETE
001190        CONTAINER ( 'PRM-ERRO' )
001200        CHANNEL   ( CANAL      )
001210        NOHANDLE
001220     END-EXEC
001230     IF  NOT (EIBRESP EQUAL DFHRESP(NORMAL)   OR
001240         EIBRESP EQUAL DFHRESP(CONTAINERERR) )
001250         PERFORM 999002-TRATA-CONDITION-CICS
001260     END-IF
001270     .
001280 120000-SAI.
001290     EXIT.
001300*-----------------------------------------------------------------
001310 130000-RECEBER-REQUISICAO       SECTION.
001320*-----------------------------------------------------------------
001330     MOVE 0005                   TO MSG1-CD-SEQL-ERRO
001340
001350     MOVE ZEROS                  TO QT-CCT-LIDO
001360     MOVE LENGTH OF SGTS035D-RQSC
001370                                 TO QT-CCT-EPRO
001380     EXEC CICS GET
001390        CONTAINER ( 'SGTS035D-RQSC' )
001400        CHANNEL   ( CANAL           )
001410        NODATA
001420        FLENGTH   ( QT-CCT-LIDO     )
001430     END-EXEC
001440
001450     IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
001460         SET  ERRO-PROGRAMADO    TO TRUE
001470         MOVE 3001               TO MSG1-CD-ERRO-TS
001480         MOVE 0006               TO MSG1-CD-SEQL-ERRO
001490         MOVE QT-CCT-EPRO        TO VL-EXBR-1
001500         MOVE QT-CCT-LIDO        TO VL-EXBR-2
001510         STRING
001520             'Container RQSC'
001530             ', TAMANHO ESPERADO: '  VL-EXBR-1
001540             ', TAMANHO RECEBIDO: '  VL-EXBR-2
001550             DELIMITED BY SIZE   INTO MSG1-TX-ERRO-TS
001560         END-STRING
001570         PERFORM 999000-GRAVA-ERRO-E-ENCERRA
001580     END-IF
001590     EXEC CICS GET
001600        CONTAINER ( 'SGTS035D-RQSC' )
001610        CHANNEL   ( CANAL      )
001620        FLENGTH ( LENGTH OF SGTS035D-RQSC )
001630        INTO    ( SGTS035D-RQSC )
001640     END-EXEC
001650     .
001660 130000-SAI.
001670     EXIT.
001680*-----------------------------------------------------------------
001690 140000-VALIDA-PARAMETRO         SECTION.
001700*-----------------------------------------------------------------
001710     MOVE 0007                   TO MSG1-CD-SEQL-ERRO
001720
001730     PERFORM 150000-VALIDA-CAMPOS
001740
001750     .
001760 140000-SAI.
001770     EXIT.
001780*-----------------------------------------------------------------
001790 150000-VALIDA-CAMPOS            SECTION.
001800*-----------------------------------------------------------------
001810     MOVE 0008                   TO MSG1-CD-SEQL-ERRO
001820
001830     IF  SGTS035D-CD-UOR-DTTR-OPR <= ZEROS
001840         SET  ERRO-PROGRAMADO    TO TRUE
001850         MOVE 0001               TO MSG1-CD-ERRO-TS
001860         MOVE 0009               TO MSG1-CD-SEQL-ERRO
001870         MOVE 'Codigo UOR detentora operacao inválido.'
001880                                 TO MSG1-TX-ERRO-TS
001890         PERFORM 999000-GRAVA-ERRO-E-ENCERRA
001900     END-IF
001910
001920     IF  SGTS035D-CD-USU-PMS EQUAL SPACES
001930         SET  ERRO-PROGRAMADO    TO TRUE
001940         MOVE 0002               TO MSG1-CD-ERRO-TS
001950         MOVE 0010               TO MSG1-CD-SEQL-ERRO
001960         MOVE 'Codigo Usuário Permissão inválido.'
001970                                 TO MSG1-TX-ERRO-TS
001980         PERFORM 999000-GRAVA-ERRO-E-ENCERRA
001990     END-IF
002000     .
002010 135000-SAI.
002020     EXIT.
002030*-----------------------------------------------------------------
002040 200000-PROCESSAR                SECTION.
002050*-----------------------------------------------------------------
002060     MOVE 0011                   TO MSG1-CD-SEQL-ERRO
002070
002080     PERFORM 220000-CONSULTAR
002090
002100     .
002110 200000-SAI.
002120     EXIT.
002130*-----------------------------------------------------------------
002140 220000-CONSULTAR                SECTION.
002150*-----------------------------------------------------------------
002160     MOVE 0012                   TO MSG1-CD-SEQL-ERRO
002170
002180     EXEC SQL
002190          SELECT
002200               TO_CHAR (A.DT_INC_VGC_PMS, 'DDMMYYYY')
002210             , TO_CHAR (A.DT_FIM_VGC_PMS, 'DDMMYYYY')
002220           INTO
002230               :SGTS035D-DT-INC-VGC-PMS
002240             , :SGTS035D-DT-FIM-VGC-PMS
002250          FROM
002260              DB2SGT.PMS_USU_OPR_UOR A
002270          WHERE
002280              A.CD_UOR_DTTR_OPR = :SGTS035D-CD-UOR-DTTR-OPR
002290          AND A.CD_USU_PMS      = :SGTS035D-CD-USU-PMS
002300          ORDER BY
002310              A.DT_FIM_VGC_PMS DESC
002320          FETCH FIRST 1 ROW ONLY
002330     END-EXEC
002340
002350     IF  SQLCODE = +100
002360         MOVE SPACES             TO SGTS035D-DT-INC-VGC-PMS
002370                                    SGTS035D-DT-FIM-VGC-PMS
002380     END-IF
002390     .
002400 220000-SAI.
002410     EXIT.
002420*-----------------------------------------------------------------
002430 300000-POSTA-RESPOSTA           SECTION.
002440*-----------------------------------------------------------------
002450     MOVE 0013                   TO MSG1-CD-SEQL-ERRO
002460     EXEC CICS PUT
002470        CONTAINER ( 'SGTS035D-RPST' )
002480        CHANNEL   ( CANAL      )
002490        FLENGTH ( LENGTH OF SGTS035D-RPST )
002500        FROM    ( SGTS035D-RPST )
002510     END-EXEC
002520     .
002530 300000-SAI.
002540     EXIT.
002550*-----------------------------------------------------------------
002560 999000-GRAVA-ERRO-E-ENCERRA     SECTION.
002570*-----------------------------------------------------------------
002580
002590     MOVE 'SGTS035D-CD-UOR-DTTR-OPR'  TO MSGKW999-NM-VRV-MNT
002600     MOVE SGTS035D-CD-UOR-DTTR-OPR    TO MSGKW999-VL-VRV-MNT
002610     PERFORM 999003-ADICIONA-VRV-MNT
002620
002630     MOVE 'SGTS035D-CD-USU-PMS'       TO MSGKW999-NM-VRV-MNT
002640     MOVE SGTS035D-CD-USU-PMS         TO MSGKW999-VL-VRV-MNT
002650     PERFORM 999003-ADICIONA-VRV-MNT
002660
002670     MOVE 'SGTS035D-DT-INC-VGC-PMS'   TO MSGKW999-NM-VRV-MNT
002680     MOVE SGTS035D-DT-INC-VGC-PMS     TO MSGKW999-VL-VRV-MNT
002690     PERFORM 999003-ADICIONA-VRV-MNT
002700
002710     MOVE 'SGTS035D-DT-FIM-VGC-PMS'   TO MSGKW999-NM-VRV-MNT
002720     MOVE SGTS035D-DT-FIM-VGC-PMS     TO MSGKW999-VL-VRV-MNT
002730     PERFORM 999003-ADICIONA-VRV-MNT
002740
002750
002760* --> grava o erro no msg
002770     PERFORM MSG1-NTF-MSG
002780     MOVE MSG1-CD-ERRO-TS        TO IIBKERRO-CD-ERRO
002790     MOVE MSG1-TX-ERRO-TS        TO IIBKERRO-TX-ERRO
002800     EXEC CICS PUT
002810        CONTAINER ( 'PRM-ERRO' )
002820        CHANNEL   ( CANAL      )
002830        FROM      ( IIBKERRO-PRM-ERRO)
002840        FLENGTH ( LENGTH OF IIBKERRO-PRM-ERRO )
002850        NOHANDLE
002860     END-EXEC
002870
002880     PERFORM 999999-ENCERRAR
002890     .
002900 999000-SAI.
002910     EXIT.
002920* --------------------------------------*
002930* 999001-TRATA-ABEND-CICS        SECTION.
002940* 999002-TRATA-CONDITION-CICS    SECTION.
002950* 999003-ADICIONA-VRV-MNT        SECTION.
002960* --------------------------------------*
=INC MSGKP999                                                           00002970
002980* --------------------------------------*
002990* 999010-TRATA-ERRO-DB2          SECTION.
=INC MSGKP997                                                           00003000
003010* --------------------------------------*
003020* MSG1-NTF-MSG                   SECTION.
=INC MSGKPMG1                                                           00003030
003040* --------------------------------------*
003050 999999-ENCERRAR                 SECTION.
003060* --------------------------------------*
003070     EXEC CICS
003080          RETURN
003090     END-EXEC.