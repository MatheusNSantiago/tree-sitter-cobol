000010* ============================================================== *
000020* -------------       S I S T E M A   S G T       -------------- *
000030* -------------       Sistema de Gestão TVM       -------------- *
000040* -------------------------------------------------------------- *
000050* PROGRAMA..: SGTSC024                                           *
000060* ANALISTA..: Alessander Tavares de Pinho                        *
000070* AUTOR.....: Alessander Tavares de Pinho                        *
000080* CHAVE.....: C1077250                                           *
000090* DATA......: 13.07.2015                                         *
000100* OBJETIVO..: Listar PENDENCIAS de Conformidade de Operacoes     *
000110* -------------------------------------------------------------- *
000120* VRS001 13.07.15 - Alessander - Implantacao                     *
000130* ============================================================== *
000140*
000150* Catalogo do iib para execucao do programa sao as seguintes:
000160*
000170*      A. Codigo de servico  - 1001069
000180*      B. Versao do servico  - 0000001
000190*      C. Codigo da operacao - 1234296
000200*      D. Versao da operacao - 0000001
000210*
000220******************************************************************
000230 IDENTIFICATION                  DIVISION.
000240******************************************************************
000250*
000260 PROGRAM-ID.   SGTSC024.
000270*
000280******************************************************************
000290 ENVIRONMENT                     DIVISION.
000300******************************************************************
000310*
000320*----------------------------------------------------------------*
000330 CONFIGURATION                   SECTION.
000340*----------------------------------------------------------------*
000350 SPECIAL-NAMES.
000360     DECIMAL-POINT IS COMMA.
000370*
000380******************************************************************
000390 DATA                            DIVISION.
000400******************************************************************
000410*
000420*----------------------------------------------------------------*
000430 WORKING-STORAGE                 SECTION.
000440*----------------------------------------------------------------*
000450 77  CTE-PROG                    PIC  X(242) VALUE 'SGTSC024'.
000460 77  CTE-VERS                    PIC  X(006) VALUE 'VRS001'.
000470*
000480 01 WS-RESP-CICS.
000490     05 WS-RESP                  PIC 9(08) VALUE 0.
000500     05 WS-RESP2                 PIC 9(08) VALUE 0.
000510*
000520 01 WS-DADOS-RQSC                PIC X(40) VALUE SPACES.
000530 01 WS-TAM-DADOS-RPST            PIC S9(08) COMP VALUE ZEROS.
000540 01 WS-DADOS-RPST                PIC X(2000) VALUE SPACES.
000550*
000560*----------------------------------------------------------------*
000570*-- AUXILIARES
000580*----------------------------------------------------------------*
000590*
000600 01  I                           PIC  9(004) VALUE ZEROS.
000610 01  WIDX                        PIC  9(09)  VALUE ZEROS.
000620 01  WIND                        PIC  9(09)  VALUE ZEROS.
000630*
000640 01  LNK-RESP                    PIC S9(009) COMP.
000650 01  GDA-CHANNEL                 PIC  X(016) VALUE SPACES.
000660 01  IND-FIM-C-C1                PIC  X(01)  VALUE 'N'.
000670 01  IND                         PIC  9(09)  VALUE ZEROS.
000680 01  GDA-CTRA-I                  PIC S9(09) COMP VALUE ZEROS.
000690 01  GDA-CTRA-F                  PIC S9(09) COMP VALUE ZEROS.
000700 01  GDA-CD-STIT-OPR-ETRD        PIC S9(09) COMP VALUE ZEROS.
000710 01  GDA-NR-GRT-TIT              PIC S9(09) COMP VALUE ZEROS.
000720 01  GDA-DT-PNC-CFMD             PIC  X(10)  VALUE '01.01.0001'.
000730 01  GDA-INCLUIDO-3              PIC S9(004) COMP VALUE 3.
000740 01  GDA-ALTERADO-4              PIC S9(004) COMP VALUE 4.
000750 01  GDA-EXCLUIDO-5              PIC S9(004) COMP VALUE 5.
000760 01  GDA-OPERACAO                PIC  X(009) VALUE SPACES.
000770*
000780*----------------------------------------------------------------*
000790*-- BOOK'S
000800*----------------------------------------------------------------*
000810*
000820*--- Book com area de dados da subrotina
=INC SGTKC024                                                           0000083%
000840*
000850*--- Book com area de erro p/ retorno ao IIB
=INC SGTKIIBE                                                           0000086%
000870*
000880*--- Book do container de Erro
=INC IIBKERRO                                                           0000089%
000900*
000910*--- Book do container de Requisição de Serviço
=INC IIBKSRVC                                                           0000092%
000930*
000940*--- Dclgen da tabela DB2SGT.BON_APRC
=INC SGTK024T                                                           0000095%
000960*
000970*----------------------------------------------------------------*
000980*                      DECLARAR CURSORES
000990*----------------------------------------------------------------*
001000
001010     EXEC SQL
001020      DECLARE PENDENCIAS          CURSOR FOR
001030       SELECT A.CD_CTRA_TIT_VL
001040       ,      A.CD_OPR_ETRD_MCP
001050       ,      A.NR_VRS_OPR_ETRD
001060       ,      A.TS_PNC_CFMD
001070       ,      A.NM_PNC_CFMD
001080       ,      A.VL_PNC_CFMD
001090       ,      A.CD_TIP_PNC_CFMD
001100       ,      A.CD_EST_PNC_CFMD
001110       ,      A.CD_USU_CNFC_PNC
001120       ,      A.TX_DCR_PNC_CFMD
001130       FROM   DB2SGT.PNC_CFMD_OPR    A
001140       WHERE  DATE(A.TS_PNC_CFMD) = :GDA-DT-PNC-CFMD
001150         AND  A.CD_CTRA_TIT_VL    BETWEEN :GDA-CTRA-I
001160         AND                              :GDA-CTRA-F
001170         AND  A.CD_EST_PNC_CFMD   IN ( :GDA-INCLUIDO-3
001180                                     , :GDA-ALTERADO-4
001190                                     , :GDA-EXCLUIDO-5 )
001200       ORDER BY A.TS_PNC_CFMD DESC
001210     END-EXEC
001220
001230*----------------------------------------------------------------*
001240 LOCAL-STORAGE                   SECTION.
001250*----------------------------------------------------------------*
001260*
001270     EXEC SQL
001280         INCLUDE SQLCA
001290     END-EXEC.
001300*
001310*----------------------------------------------------------------*
001320 LINKAGE                         SECTION.
001330*----------------------------------------------------------------*
001340*
001350******************************************************************
001360 PROCEDURE DIVISION.
001370******************************************************************
001380*
001390*----------------------------------------------------------------*
001400 000000-ROTINA-PRINCIPAL         SECTION.
001410*----------------------------------------------------------------*
001420
001430     MOVE ZEROS                  TO KC024-QT-REG-DET
001440
001450     PERFORM 100000-GET-MESSAGE
001460     PERFORM 200000-VALIDA-ENTRADA
001470     PERFORM 300000-LISTA-PENDENCIAS
001480     PERFORM 400000-PUT-RESPOSTA
001490     PERFORM 999000-ENCERRA
001500
001510     .
001520 000099-SAI.
001530     EXIT.
001540
001550*----------------------------------------------------------------*
001560 100000-GET-MESSAGE              SECTION.
001570*----------------------------------------------------------------*
001580
001590     EXEC CICS
001600         GET CONTAINER ( 'SGTSC024-RQSC  ' )
001610             INTO      (  KC024-RQSC       )
001620     END-EXEC
001630
001640     IF  EIBRESP NOT EQUAL DFHRESP(NORMAL)
001650         MOVE EIBRESP            TO KERRO-CD-ERRO
001660         MOVE 'Erro ao recuperar dados de entrada da pesquisa.'
001670                                 TO KERRO-TX-ERRO
001680         PERFORM 990000-ERRO
001690     END-IF
001700
001710     MOVE ZEROS                  TO KC024-NR-PSQ-FIM
001720     .
001730 100099-GET-MESSAGE.
001740     EXIT.
001750
001760*----------------------------------------------------------------*
001770 200000-VALIDA-ENTRADA           SECTION.
001780*----------------------------------------------------------------*
001790
001800
001810     COMPUTE KC024-CD-CTRA-TIT-ENTD
001820           = KC024-CD-CTRA-TIT-ENTD * 1
001830       ON SIZE ERROR
001840        MOVE ZEROS               TO KC024-CD-CTRA-TIT-ENTD
001850     END-COMPUTE.
001860
001870     IF KC024-DT-PSQ-PNC-ENTD EQUAL SPACES
001880        MOVE SQLCODE             TO KERRO-CD-ERRO
001890        MOVE 'Informe a data de pesquisa das pendências.'
001900                                 TO KERRO-TX-ERRO
001910        PERFORM 990000-ERRO
001920     END-IF
001930
001940     IF KC024-CD-CTRA-TIT-ENTD EQUAL ZEROS
001950        MOVE ZEROS                  TO GDA-CTRA-I
001960        MOVE 999999999              TO GDA-CTRA-F
001970     ELSE
001980        MOVE KC024-CD-CTRA-TIT-ENTD TO GDA-CTRA-I
001990        MOVE KC024-CD-CTRA-TIT-ENTD TO GDA-CTRA-F
002000     END-IF
002010
002020     .
002030 200099-VALIDA-ENTRADA.
002040     EXIT.
002050
002060*----------------------------------------------------------------*
002070 300000-LISTA-PENDENCIAS            SECTION.
002080*----------------------------------------------------------------*
002090
002100     MOVE 'N' TO KC024-IN-CTND
002110     MOVE 'N' TO IND-FIM-C-C1
002120
002130     PERFORM 310000-OPEN-PNC-CNFD
002140     PERFORM 320000-FETCH-PNC-CNFD UNTIL IND-FIM-C-C1   = 'S'
002150     PERFORM 330000-CLOSE-PNC-CNFD
002160
002170     .
002180 300099-LISTA-PENDENCIAS.
002190     EXIT.
002200
002210*----------------------------------------------------------------*
002220 310000-OPEN-PNC-CNFD             SECTION.
002230*----------------------------------------------------------------*
002240
002250     MOVE KC024-DT-PSQ-PNC-ENTD  TO GDA-DT-PNC-CFMD
002260
002270     IF KC024-CD-EST-GRT-ENTD NOT EQUAL ZEROS
002280        MOVE KC024-CD-EST-GRT-ENTD TO GDA-INCLUIDO-3
002290        MOVE KC024-CD-EST-GRT-ENTD TO GDA-ALTERADO-4
002300        MOVE KC024-CD-EST-GRT-ENTD TO GDA-EXCLUIDO-5
002310     END-IF
002320
002330     EXEC SQL
002340        OPEN  PENDENCIAS
002350     END-EXEC
002360
002370     IF SQLCODE NOT EQUAL ZEROS
002380        MOVE SQLCODE             TO KERRO-CD-ERRO
002390        MOVE 'Erro ao iniciar leitura de pendências.'
002400                                 TO KERRO-TX-ERRO
002410        PERFORM 990000-ERRO
002420     END-IF
002430
002440     .
002450 310099-OPEN-PNC-CNFD.
002460     EXIT.
002470
002480*----------------------------------------------------------------*
002490 320000-FETCH-PNC-CNFD       SECTION.
002500*----------------------------------------------------------------*
002510
002520     EXEC SQL
002530        FETCH PENDENCIAS
002540         INTO  :T024-CD-CTRA-TIT-VL
002550             , :T024-CD-OPR-ETRD-MCP
002560             , :T024-NR-VRS-OPR-ETRD
002570             , :T024-TS-PNC-CFMD
002580             , :T024-NM-PNC-CFMD
002590             , :T024-VL-PNC-CFMD
002600             , :T024-CD-TIP-PNC-CFMD
002610             , :T024-CD-EST-PNC-CFMD
002620             , :T024-CD-USU-CNFC-PNC
002630             , :T024-TX-DCR-PNC-CFMD
002640     END-EXEC
002650
002660     EVALUATE SQLCODE
002670        WHEN ZEROS
002680           ADD 1 TO IND
002690           ADD 1 TO KC024-NR-PSQ-FIM
002700           IF KC024-NR-PSQ-INC-ENTD  LESS IND
002710              IF I >= 100
002720                 COMPUTE IND = IND - 1
002730                 MOVE 'S' TO IND-FIM-C-C1
002740                 MOVE 'S' TO KC024-IN-CTND
002750              ELSE
002760                 PERFORM 304000-MONTA-RESPOSTA
002770              END-IF
002780           END-IF
002790         WHEN 100
002800             MOVE 'S'            TO IND-FIM-C-C1
002810         WHEN OTHER
002820             MOVE SQLCODE        TO KERRO-CD-ERRO
002830             MOVE 'Erro ao processar pendências.'
002840                                 TO KERRO-TX-ERRO
002850             PERFORM 990000-ERRO
002860     END-EVALUATE
002870
002880     .
002890 320099-FETCH-PNC-CNFD.
002900     EXIT.
002910
002920*-------------------------------------
002930 330000-CLOSE-PNC-CNFD         SECTION.
002940*-------------------------------------
002950
002960     EXEC SQL
002970        CLOSE PENDENCIAS
002980     END-EXEC
002990
003000     IF SQLCODE NOT EQUAL ZEROS
003010        MOVE SQLCODE             TO KERRO-CD-ERRO
003020        MOVE 'Erro ao finalizar leitura de pendências.'
003030                                 TO KERRO-TX-ERRO
003040        PERFORM 990000-ERRO
003050     END-IF
003060     .
003070 330099-CLOSE-PNC-CNFD.
003080     EXIT.
003090
003100*----------------------------------------------------------------*
003110 304000-MONTA-RESPOSTA           SECTION.
003120*----------------------------------------------------------------*
003130
003140     ADD 1                         TO I
003150     ADD 1                         TO KC024-QT-REG-DET
003160
003170     MOVE T024-CD-CTRA-TIT-VL      TO KC024-CD-CTRA-TIT-VL  (I)
003180     MOVE T024-CD-OPR-ETRD-MCP     TO KC024-CD-OPR-ETRD-MCP (I)
003190     MOVE T024-NR-VRS-OPR-ETRD     TO KC024-NR-VRS-OPR-ETRD (I)
003200     MOVE T024-TS-PNC-CFMD         TO KC024-TS-PNC-CFMD     (I)
003210
003220     EVALUATE T024-CD-EST-PNC-CFMD
003230        WHEN 3
003240           MOVE 'Inclusão ' TO GDA-OPERACAO
003250        WHEN 4
003260           MOVE 'Alteração' TO GDA-OPERACAO
003270        WHEN 5
003280           MOVE 'Exclusão ' TO GDA-OPERACAO
003290     END-EVALUATE
003300
003310     STRING GDA-OPERACAO  '-' T024-NM-PNC-CFMD
003320        DELIMITED BY SIZE
003330         INTO KC024-NM-PNC-CFMD     (I)
003340
003350     MOVE T024-VL-PNC-CFMD         TO KC024-VL-PNC-CFMD     (I)
003360     MOVE T024-CD-TIP-PNC-CFMD     TO KC024-CD-TIP-PNC-CFMD (I)
003370     MOVE T024-CD-EST-PNC-CFMD     TO KC024-CD-EST-PNC-CFMD (I)
003380     MOVE T024-CD-USU-CNFC-PNC     TO KC024-CD-USU-CNFC-PNC (I)
003390     MOVE T024-TX-DCR-PNC-CFMD     TO KC024-TX-DCR-PNC-CFMD (I)
003400
003410     IF T024-CD-TIP-PNC-CFMD EQUAL 2
003420        EXEC SQL
003430          SELECT A.CD_STIT_OPR_ETRD
003440          INTO   :GDA-CD-STIT-OPR-ETRD
003450          FROM   DB2SGT.PNC_STIT_OPR_ETRD A
003460          WHERE  A.CD_CTRA_TIT_VL  = :T024-CD-CTRA-TIT-VL
003470            AND  A.CD_OPR_ETRD_MCP = :T024-CD-OPR-ETRD-MCP
003480            AND  A.NR_VRS_OPR_ETRD = :T024-NR-VRS-OPR-ETRD
003490            AND  A.TS_PNC_CFMD     = :T024-TS-PNC-CFMD
003500        END-EXEC
003510
003520        EVALUATE SQLCODE
003530           WHEN ZEROS
003540              CONTINUE
003550           WHEN 100
003560              MOVE ZEROS TO GDA-CD-STIT-OPR-ETRD
003570           WHEN OTHER
003580              MOVE SQLCODE        TO KERRO-CD-ERRO
003590              MOVE 'Erro ao processar subtitulo da pendência.'
003600                                  TO KERRO-TX-ERRO
003610              PERFORM 990000-ERRO
003620        END-EVALUATE
003630        MOVE GDA-CD-STIT-OPR-ETRD
003640          TO KC024-CD-STIT-OPR-SAID (I)
003650     END-IF
003660
003670     IF T024-CD-TIP-PNC-CFMD EQUAL 3
003680        EXEC SQL
003690          SELECT A.NR_GRT_TIT
003700          INTO   :GDA-NR-GRT-TIT
003710          FROM   DB2SGT.PNC_GRT_TIT_OPR A
003720          WHERE  A.CD_CTRA_TIT_VL   = :T024-CD-CTRA-TIT-VL
003730            AND  A.CD_OPR_ETRD_MCP  = :T024-CD-OPR-ETRD-MCP
003740            AND  A.NR_VRS_OPR_ETRD  = :T024-NR-VRS-OPR-ETRD
003750            AND  A.TS_PNC_CFMD      = :T024-TS-PNC-CFMD
003760            AND  A.CD_STIT_OPR_ETRD = :GDA-CD-STIT-OPR-ETRD
003770        END-EXEC
003780
003790        EVALUATE SQLCODE
003800           WHEN ZEROS
003810              CONTINUE
003820           WHEN 100
003830              MOVE ZEROS TO KC024-NR-GRT-TIT-SAID  (I)
003840           WHEN OTHER
003850              MOVE SQLCODE        TO KERRO-CD-ERRO
003860              MOVE 'Erro ao processar garantia da pendência.'
003870                                  TO KERRO-TX-ERRO
003880              PERFORM 990000-ERRO
003890        END-EVALUATE
003900        MOVE GDA-NR-GRT-TIT
003910          TO KC024-NR-GRT-TIT-SAID  (I)
003920     END-IF
003930
003940     .
003950 300229-MONTA-RESPOSTA.
003960     EXIT.
003970
003980*----------------------------------------------------------------*
003990 400000-PUT-RESPOSTA             SECTION.
004000*----------------------------------------------------------------*
004010
004020     MOVE 'N'                    TO KC024-IN-CTND
004030
004040     MOVE IND                    TO KC024-NR-PSQ-FIM
004050
004060     EXEC CICS
004070         PUT CONTAINER ( 'SGTSC024-RPST  ' )
004080             FROM      (  KC024-RPST       )
004090     END-EXEC
004100
004110     IF  EIBRESP NOT EQUAL DFHRESP(NORMAL)
004120         MOVE EIBRESP            TO KERRO-CD-ERRO
004130         MOVE 'ERRO AO EFETUAR PUT NO CONTAINER SGTS0242-RPST'
004140                                 TO KERRO-TX-ERRO
004150         PERFORM 990000-ERRO
004160     END-IF
004170
004180     .
004190 400099-PUT-RESPOSTA.
004200     EXIT.
004210
004220*----------------------------------------------------------------*
004230 990000-ERRO                     SECTION.
004240*----------------------------------------------------------------*
004250
004260     EXEC CICS SYNCPOINT ROLLBACK END-EXEC
004270
004280        EXEC CICS
004290            PUT CONTAINER ( 'PRM-ERRO       ' )
004300                FROM      ( KERRO-IIB-ERRO    )
004310        END-EXEC
004320
004330     PERFORM 999000-ENCERRA
004340     .
004350 990099-ERRO.
004360     EXIT.
004370
004380*----------------------------------------------------------------*
004390 999000-ENCERRA                  SECTION.
004400*----------------------------------------------------------------*
004410
004420     EXEC CICS RETURN END-EXEC
004430
004440     .
004450 999099-ENCERRA.
004460     EXIT.
004470
004480*================================================================*