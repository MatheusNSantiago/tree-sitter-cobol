000010*-----------------------------------------------------------------
000020* PROGRAMA..: SGTS0097
000030* ANALISTA..: F0175702 ADRIANO SEIJI NAKAMURA
000040* AUTOR.....: F0175702 ADRIANO SEIJI NAKAMURA
000050* DATA......: 25/11/2016
000060* OBJETIVO..: Consultar Operações encarteiradas e vinculadas a uma
000080*             Nota Tecnica de uma Operação Estruturada
000080* COMPILACAO: COBOL/CICS
000090*-----------------------------------------------------------------
000090* VRS001 28/06/2018 F8785286 DE 2016/145195 - Inclusão de carteira
000090*        08/02/2018 F8785286 DE 2016/145195 - Acerto dos códigos
000090*          das mensagens para mensageria.
000080*        26/07/2017 F8785286  DE 2016/145195 - Recompilado por
000080*                    alteração no book SGTKL067.
000100*        25/11/2016 F0175702 DE 2015/137233 - IMPLANTACAO
000110*-----------------------------------------------------------------
000120
000130*-----------------------------------------------------------------
000140 IDENTIFICATION DIVISION.
000150*----------------------------------------------------------------*
000160 PROGRAM-ID. SGTS0097.
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
000360 77  CTE-INICIO-SS                 PIC  X(040)  VALUE
000370              '*** S.S. COMECA AQUI ***'.
000380
000390 77  CTE-PROG                      PIC  X(018)  VALUE
000400              '*** SGTS0097 ***'.
000410 77  CTE-VERS                      PIC  X(006)  VALUE 'VRS001'.
000420
000430* Modulos acionados
000440 77  IIBP2000                      PIC  X(008) VALUE 'IIBP2000'.
000440 77  SGTSL067                      PIC  X(008) VALUE 'SGTSL067'.
000470*----------------------------------------------------------------*
000480 LOCAL-STORAGE                   SECTION.
000490*----------------------------------------------------------------*
000500
000510* Areas de trabalho
000550 01  CD-UOR-ITVT-NOTA              PIC S9(9)  COMP-5 VALUE 0.
000440 01  IC-TAB                        PIC S9(4)  COMP-5 VALUE 0.
000560*----------------------------------------------------------------*
000570* Book dos Containers e das subrotinas chamadas
000580*----------------------------------------------------------------*
000590
000600* Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1
000620* Book para captura de abend cics da bloco codigo MSGKP999
-INC HLPKW999
000650* Book para captura de erro via MSG1
-INC MSGKWMG1
000800* Book do Container de Erro
-INC IIBKERRO
000820* Book do container de Servicos (para acionamento de servicos)
-INC IIBKSRVC
001400* Book dos dados do usuário
-INC IIBKOPRD
000840
125874* Area de trabalho
125874
125874 01  QT-REG-SELD                  PIC S9(4) COMP-5 VALUE 0.
125874
000000 01  DADOS-LOG-DISPLAY.
000000     03  CD-OPR-ETRD-MCP-LOG      PIC  9(11) VALUE 0.
000000
125874* Campos utilizados para leitura
125874 01  REG-WS.
125874     03 CD-CTRA-TIT-VL           PIC S9(9) COMP-5
125874     OCCURS 101 TIMES            VALUE 0.
125874     03 CD-OPR-ETRD-MCP          PIC S9(11) COMP-3
125874     OCCURS 101 TIMES            VALUE 0.
125874     03 NR-VRS-OPR-ETRD          PIC S9(4) COMP-5
125874     OCCURS 101 TIMES            VALUE 0.
125874     03 NR-NOTA-TCN-OPR          PIC S9(4) COMP-5
125874     OCCURS 101 TIMES            VALUE 0.
125874     03 CD-USU-ULT-ATL           PIC X(08)
125874     OCCURS 101 TIMES            VALUE ' '.
125874     03 TS-INCL-NOTA-TCN         PIC X(26)
125874     OCCURS 101 TIMES            VALUE ' '.
125874     03 DT-EMS-NOTA              PIC X(10)
125874     OCCURS 101 TIMES            VALUE ' '.
125874     03 DT-APVC-NOTA             PIC X(10)
125874     OCCURS 101 TIMES            VALUE ' '.
125874     03 CD-REF-NOTA-TCN          PIC X(12)
125874     OCCURS 101 TIMES            VALUE ' '.
125874     03 TX-TIP-DOC-TCN           PIC X(40)
125874     OCCURS 101 TIMES            VALUE ' '.
125874     03 CD-ARQ-OPR-ETRD          PIC S9(9) COMP-5
125874     OCCURS 101 TIMES            VALUE 0.
125874     03 CD-ARQ-OPR-ETRD-N        PIC S9(4) COMP-5
125874     OCCURS 101 TIMES            VALUE 0.
125874     03 TS-INCL-NOTA-TCN-N       PIC S9(4) COMP-5
125874     OCCURS 101 TIMES            VALUE 0.
125874     03 CD-UOR-DRTA-OGNL         PIC S9(9) COMP-5
125874     OCCURS 101 TIMES            VALUE 0.
001530     03 CD-TIT-OPR-ETRD          PIC  X(12)
125874     OCCURS 101 TIMES            VALUE ' '.
125874     03 CD-TIT-OPR-ETRD-N        PIC S9(4) COMP-5
125874     OCCURS 101 TIMES            VALUE 0.
125874
125874* Indicadores de nulidades
125874
125874* Variavel(is) de controle do cursor(es)
125874 01  CTL-CURSOR-PSQ.
125874     03  CD-FIM-CURSOR-PSQ       PIC S9(04) COMP-5 VALUE 0.
125874         88  FIM-CURSOR-PSQ                 VALUE +100.
125874     03  IN-CURSOR-PSQ-ABTO      PIC  X(01) VALUE 'N'.
125874         88  CURSOR-PSQ-ABTO                VALUE 'S'.
125874
125874* Subrotinas utilizadas
125874
125874     EXEC SQL
125874         INCLUDE SQLCA
125874     END-EXEC.
125874
-INC SGTK0097
125874
125874*--> Caso haja mais que 100 occ, baixa deve pegar ult-occ e chamar
125874     EXEC SQL
125874       DECLARE CURSOR-PSQ-1 CURSOR
125874       WITH ROWSET POSITIONING FOR
125874       SELECT
125874         A.CD_CTRA_TIT_VL
125874       , A.CD_OPR_ETRD_MCP
125874       , A.NR_VRS_OPR_ETRD
125874       , A.CD_USU_ULT_ATL
125874       , A.TS_INCL_NOTA_TCN
125874       , B.CD_TIT_OPR_ETRD
125874       FROM DB2SGT.NOTA_TCN_VCLD_OPR  A
125874       INNER JOIN DB2SGT.OPR_ETRD_MCP B
125874       ON A.CD_CTRA_TIT_VL = B.CD_CTRA_TIT_VL
125874       AND A.CD_OPR_ETRD_MCP = B.CD_OPR_ETRD_MCP
125874       AND A.NR_VRS_OPR_ETRD = B.NR_VRS_OPR_ETRD
125874       WHERE
125874       A.NR_NOTA_TCN_OPR = :SGTS0097-E-NR-NOTA-TCN-OPR
125874*--> Paginação
000340       AND
000341       ((A.CD_CTRA_TIT_VL    > :SGTS0097-E-CD-CTRA-TIT-VL) OR
000342
000341        (A.CD_CTRA_TIT_VL    = :SGTS0097-E-CD-CTRA-TIT-VL  AND
000342          A.CD_OPR_ETRD_MCP  > :SGTS0097-E-CD-OPR-ETRD-MCP) OR
000342
000341        (A.CD_CTRA_TIT_VL    = :SGTS0097-E-CD-CTRA-TIT-VL   AND
000343         A.CD_OPR_ETRD_MCP   = :SGTS0097-E-CD-OPR-ETRD-MCP  AND
000344        A.NR_VRS_OPR_ETRD  > :SGTS0097-E-NR-VRS-OPR-ETRD  ))
125874       ORDER BY  A.CD_CTRA_TIT_VL
125874              ,  A.CD_OPR_ETRD_MCP
125874              ,  A.NR_VRS_OPR_ETRD
000125
125874     END-EXEC.
125874
000850 77  CTE-FINAL-SS                  PIC X(40)      VALUE
000860              '*** S.S. TERMINA AQUI ***'.
000870
000880*----------------------------------------------------------------*
000890 PROCEDURE DIVISION.
000900*----------------------------------------------------------------*
000910
000920*----------------------------------------------------------------*
000930 000000-ROTINA-PRINCIPAL         SECTION.
000940*----------------------------------------------------------------*
000950
000960     PERFORM 100000-INICIAR
000970
000980     PERFORM 200000-PROCESSAR
000990
001000     PERFORM 990000-ENCERRAR
001020     .
001030 000000-SAI.
001040     EXIT.
001060*----------------------------------------------------------------*
001070 100000-INICIAR                  SECTION.
001080*----------------------------------------------------------------*
001100     MOVE 0001                   TO MSG1-CD-SEQL-ERRO
001110
001120*    Ativa a captura automatica de abend do CICS.
001130*    WHENEVER CICS
-INC HLPKE999
001150
001160     PERFORM 970000-LER-CANAL
001170
001180*    Define o nome do programa p/ fins manipulacao container
001190     MOVE 'SGTS0097'             TO NM-PGM
001200
001210*    Limpa a resposta e erro antes de executar o modulo
001220     PERFORM 983000-EXCLUIR-RESPOSTA-ERRO
001230
001240     PERFORM 971000-LER-REQUISICAO
001250
001260     PERFORM 110000-VALIDAR-REQUISICAO
001270     .
001280 100000-SAI.
001290     EXIT.
001310*----------------------------------------------------------------*
001320 110000-VALIDAR-REQUISICAO       SECTION.
001330*----------------------------------------------------------------*
001340     MOVE 0002                   TO MSG1-CD-SEQL-ERRO
001350
001360*    Valida campo a campo da requisicao
001370     IF  SGTS0097-E-NR-NOTA-TCN-OPR   <= ZEROS
000440         SET ERRO-PROGRAMADO TO TRUE
001400         MOVE 0181           TO MSG1-CD-ERRO-TS
001410         MOVE 'Numero nota tecnica operacao invalido.'
001420                                 TO MSG1-TX-ERRO-TS
001430         SET MNT-VRV         TO TRUE
001440         PERFORM 999000-ERRO
000440     END-IF
001470     .
001480 110000-SAI.
001490     EXIT.
001510*----------------------------------------------------------------*
001520 200000-PROCESSAR                SECTION.
001530*----------------------------------------------------------------*
001540     MOVE 0003                   TO MSG1-CD-SEQL-ERRO
001550
125874     PERFORM 970101-ABRE-CURSOR-PSQ
125874     PERFORM 970102-LER-CURSOR-PSQ
125874
125874     PERFORM 210000-PROCESSA-PESQUISA
125874       VARYING IC-TAB FROM 1 BY 1
125874       UNTIL IC-TAB GREATER SGTS0097-QT-OCR-TAB
125874
125874     PERFORM 970103-FECHA-CURSOR-PSQ
000590
000440*    PERFORM 210000-PESQ-NOTA-TECNICA-VCLD
000440
000440     PERFORM 980000-GRAVAR-CONTAINER-RPST
001640     .
001650 200000-SAI.
001660     EXIT.
125874*-----------------------------------------------------------------
125874 210000-PROCESSA-PESQUISA        SECTION.
125874*-----------------------------------------------------------------
125874     MOVE 0004                   TO MSG1-CD-SEQL-ERRO
125874
125874     MOVE CD-CTRA-TIT-VL (IC-TAB)
125874                             TO SGTS0097-CD-CTRA-TIT-VL(IC-TAB)
125874     MOVE CD-OPR-ETRD-MCP(IC-TAB)
125874                             TO SGTS0097-CD-OPR-ETRD-MCP (IC-TAB)
125874     MOVE NR-VRS-OPR-ETRD(IC-TAB)
125874                             TO SGTS0097-NR-VRS-OPR-ETRD (IC-TAB)
125874     MOVE CD-TIT-OPR-ETRD(IC-TAB)
125874                             TO SGTS0097-CD-TIT-OPR-ETRD(IC-TAB)
125874     .
125874 210000-SAI.
125874     EXIT.
002720*----------------------------------------------------------------*
002730*                                                                *
002740*  SECTION DA FAIXA DE 900000 A 969900 PARA DEMAIS ROTINAS       *
002750*                                                                *
002760*----------------------------------------------------------------*
002770* Executa um modulo implementado com CONTAINER                   *
002780* Nao incluir nessa section os modulos chamados via COMMAREA     *
004260*----------------------------------------------------------------*
004270*                                                                *
004280* SECTION DA FAIXA DE 970000 A 979900 PARA LEITURA DE DADOS      *
004290*                                                                *
004300*----------------------------------------------------------------*
004310 970000-LER-CANAL                SECTION.
004320*    Obtem o canal corrente
004330*----------------------------------------------------------------*
004340     MOVE 0005                   TO MSG1-CD-SEQL-ERRO
004350
004360*Por causa da captura automática de erro CICS, deve ser previsto
004370*o erro de container
004380     SET  ERRO-CANAL             TO TRUE
004390
004400     EXEC CICS ASSIGN
004410               CHANNEL ( NM-CNL )
004420     END-EXEC
004430
004440*    Ao chegar aqui, há certeza de que não houve erro algum
004450     SET  SEM-ERRO               TO TRUE
004460     .
004470 970000-FIM.
004480     EXIT.
125874*-----------------------------------------------------------------
125874 970101-ABRE-CURSOR-PSQ          SECTION.
125874*-----------------------------------------------------------------
125874     MOVE 0006                   TO MSG1-CD-SEQL-ERRO
125874
125874     EXEC SQL
125874          OPEN CURSOR-PSQ-1
125874     END-EXEC
125874
125874     MOVE 'S'                    TO IN-CURSOR-PSQ-ABTO
125874     .
125874 970101-SAI.
125874     EXIT.
125874*-----------------------------------------------------------------
125874 970102-LER-CURSOR-PSQ           SECTION.
125874*-----------------------------------------------------------------
125874     MOVE 0007                   TO MSG1-CD-SEQL-ERRO
125874
125874     INITIALIZE REG-WS
125874
125874          EXEC SQL
125874           FETCH NEXT ROWSET
125874               CURSOR-PSQ-1 FOR 101 ROWS
125874              INTO :CD-CTRA-TIT-VL
125874                 , :CD-OPR-ETRD-MCP
125874                 , :NR-VRS-OPR-ETRD
125874                 , :CD-USU-ULT-ATL
125874                 , :TS-INCL-NOTA-TCN
125874                   :TS-INCL-NOTA-TCN-N
125874                 , :CD-TIT-OPR-ETRD
125874                   :CD-TIT-OPR-ETRD-N
125874          END-EXEC
125874
125874     MOVE SQLERRD(3)             TO QT-REG-SELD
125874
125874     IF  QT-REG-SELD EQUAL 0101
125874         SET SGTS0097-TEM-MAIS-REGISTRO TO TRUE
125874         MOVE 0100               TO QT-REG-SELD
125874     END-IF
125874
125874     MOVE QT-REG-SELD            TO SGTS0097-QT-OCR-TAB
125874     MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ
125874     .
125874 970102-SAI.
125874     EXIT.
125874*-----------------------------------------------------------------
125874 970103-FECHA-CURSOR-PSQ         SECTION.
125874*-----------------------------------------------------------------
125874     MOVE 0008                   TO MSG1-CD-SEQL-ERRO
125874
125874     MOVE 'N'                    TO IN-CURSOR-PSQ-ABTO
125874
125874     EXEC SQL
125874          CLOSE CURSOR-PSQ-1
125874     END-EXEC
125874     .
125874 970103-SAI.
125874     EXIT.
004500*----------------------------------------------------------------*
004510 971000-LER-REQUISICAO           SECTION.
004520*----------------------------------------------------------------*
004540     MOVE 0009                   TO MSG1-CD-SEQL-ERRO
004550
004560*    Monta o nome do container de requisicao
004570     MOVE 'SGTS0097'             TO NM-PGM
004580     MOVE 'SGTS0097-RQSC'        TO NM-CTNR
004590
004600*    Recupera o tamanho do container de requisição
004610     PERFORM 973100-LER-CONTAINER-NODATA
004620
004630*    Obtem o tamanho esperado da requisicao
004640     MOVE LENGTH OF SGTS0097-RQSC TO QT-CCT-EPRO
004650
004660*    Valida tamanho esperado da requisicao X tamanho recebido
004680     IF  QT-CCT-LIDO             NOT EQUAL QT-CCT-EPRO
004690         SET ERRO-TAMANHO-AREA   TO TRUE
004700         PERFORM 999000-GRAVA-ERRO-E-ENCERRA
004710     END-IF
004720
004750*Configura dados para leitura do container de requisicao
004760     SET  GET-CONTAINER          TO TRUE
004770
004780*Por causa da captura automática de erro CICS, deve ser previsto
004790*o erro de container
004800     SET  ERRO-CONTAINER         TO TRUE
004810
004820*    Efetua a leitura do container
004830     EXEC CICS GET
004840               CONTAINER (NM-CTNR)
004850               FLENGTH   (QT-CCT-LIDO)
004860               INTO      (SGTS0097-RQSC)
004870     END-EXEC
004880
004890*Ao chegar aqui, há certeza de que não houve erro algum
004900     SET  SEM-ERRO               TO TRUE
004920     .
004930 971000-SAI.
004940     EXIT.
006260*----------------------------------------------------------------*
006270 973100-LER-CONTAINER-NODATA     SECTION.
006280*----------------------------------------------------------------*
006290
006300     MOVE 0010                   TO MSG1-CD-SEQL-ERRO
006310
006320*Configura dados para leitura do container de requisição
006330     SET  GET-CONTAINER          TO TRUE
006340
006350*Por causa da captura automática de erro CICS, deve ser previsto
006360*o erro de container
006370     SET  ERRO-CONTAINER         TO TRUE
006380
006390*    Obtem a quantidade de caracteres no container
006400     MOVE ZEROS                  TO QT-CCT-LIDO
006410     EXEC CICS GET
006420               CONTAINER (NM-CTNR)
006430               NODATA
006440               FLENGTH   (QT-CCT-LIDO)
006450     END-EXEC
006460
006470*    Ao chegar aqui, há certeza de que não houve erro algum
006480     SET  SEM-ERRO               TO TRUE
006490     .
006500 973100-SAI.
006510     EXIT.
006520*----------------------------------------------------------------*
006530*                                                                *
006540* SECTION DA FAIXA DE 980000 A 989900 PARA GRAVACAO DE DADOS     *
006550*                                                                *
006560*----------------------------------------------------------------*
006570 980000-GRAVAR-CONTAINER-RPST    SECTION.
006580*----------------------------------------------------------------*
006590
006600*    Exclui eventual container de erro de outros programas
006610     PERFORM 983100-EXCLUIR-CONTAINER-ERRO
006620
006630     MOVE 0011                   TO MSG1-CD-SEQL-ERRO
006640
006650*    Monta o nome do container de resposta
006660     MOVE 'SGTS0097-RPST'        TO NM-CTNR
006670     SET PUT-CONTAINER           TO TRUE
006680
006690*Por causa da captura automática de erro CICS, deve ser previsto
006700*o erro de container
006710     SET  ERRO-CONTAINER         TO TRUE
006720
006730     EXEC CICS PUT
006740               CONTAINER (NM-CTNR                    )
006750               FROM      (SGTS0097-RPST          )
006760               FLENGTH   (LENGTH OF SGTS0097-RPST)
006770     END-EXEC
006780
006790*Ao chegar aqui, há certeza de que não houve erro algum
006800     SET  SEM-ERRO               TO TRUE
006810     .
006820 980000-SAI.
006830     EXIT.
007790*----------------------------------------------------------------*
007800 983000-EXCLUIR-RESPOSTA-ERRO    SECTION.
007810*----------------------------------------------------------------*
007820* Exclui o container de resposta e de erro para evitar problemas *
007830* em modulos executados mais de uma vez dentro da mesma sessao do*
007840* CICS. Equivale ao initialize na area de resposta dos programas *
007850* que utilizam COMMAREA.                                         *
007860*----------------------------------------------------------------*
007900*    Quando modulos via IIB, fixo nome PRM-DADOS-RPST, senão
007910*    nome_modulo -RPST
007920*     EVALUATE NM-PGM
007930*         WHEN ARHS0882
000440*              MOVE 'PRM-DADOS-RPST' TO NM-CTNR
007960*         WHEN OTHER
007970              MOVE NM-PGM        TO NM-CTNR(1:8)
007980              MOVE '-RPST'       TO NM-CTNR(9:5)
007990*     END-EVALUATE
008000
008010     SET DEL-CONTAINER           TO TRUE
008020
008030*    Exclui o container de resposta
008040*    Não há captura automática de erro CICS por usar RESP e RESP2
008050     EXEC CICS DELETE
008060               CONTAINER (NM-CTNR)
008070               RESP      (MSG1-RESP1)
008080               RESP2     (MSG1-RESP2)
008090     END-EXEC
008100
008110     EVALUATE TRUE
008120         WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
005960
008130         WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
008140              CONTINUE
005960
008150         WHEN OTHER
008160              SET ERRO-CONTAINER TO TRUE
008170              PERFORM 999002-TRATA-CONDITION-CICS
008180     END-EVALUATE
008190
008200*    Excluir o container de erro
008210     PERFORM 983100-EXCLUIR-CONTAINER-ERRO
008220     .
008230 983000-SAI.
008240     EXIT.
008260*----------------------------------------------------------------*
008270 983100-EXCLUIR-CONTAINER-ERRO   SECTION.
008280*----------------------------------------------------------------*
008300*    Limpar variaveis container de erro
008310     INITIALIZE IIBKERRO-PRM-ERRO
008320
008330*    Exclui o container de erro
008350     MOVE 'PRM-ERRO'             TO NM-CTNR
008360     SET DEL-CONTAINER           TO TRUE
008370
008380*    Não há captura automática de erro CICS por usar RESP e RESP2
008390     EXEC CICS DELETE
008400               CONTAINER (NM-CTNR)
008410               RESP      (MSG1-RESP1)
008420               RESP2     (MSG1-RESP2)
008430     END-EXEC
008440
008450     EVALUATE TRUE
008460         WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
005960
008470         WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
008480              CONTINUE
005960
008490         WHEN OTHER
008500              SET ERRO-CONTAINER TO TRUE
008510              PERFORM 999002-TRATA-CONDITION-CICS
008520     END-EVALUATE
008530     .
008540 983100-SAI.
008550     EXIT.
008560*----------------------------------------------------------------*
008570 984000-GRAVAR-CONTAINER-ERRO    SECTION.
008580*----------------------------------------------------------------*
008590     MOVE 0012                   TO MSG1-CD-SEQL-ERRO
008600
008610*    Grava o erro gerado no container de erro
008620*    Não há captura automática de erro CICS por usar RESP e RESP2
008630     EXEC CICS PUT
008640               CONTAINER ( 'PRM-ERRO'                  )
008650               FROM      ( IIBKERRO-PRM-ERRO           )
008660               FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
008670               RESP      (MSG1-RESP1                   )
008680               RESP2     (MSG1-RESP2                   )
008690     END-EXEC
008710     .
008720 984000-SAI.
008730     EXIT.
008750*----------------------------------------------------------------*
008760 990000-ENCERRAR                 SECTION.
008770*----------------------------------------------------------------*
008790     GOBACK
008820     .
008830 990000-ENCERRAR-SAI.
008840     EXIT.
008860*----------------------------------------------------------------*
008870*                                                                *
008880* As sections abaixo iniciadas com 999000 representam os tratamen*
008890* tos de erro utilizando o Sistema MSG (rotina MSGS0001)         *
008900*                                                                *
008910*----------------------------------------------------------------*
008920 999000-ERRO                     SECTION.
008930*----------------------------------------------------------------*
008950     IF MSG1-CD-ERRO-TS EQUAL ZEROS
008960        MOVE 8888                TO MSG1-CD-ERRO-TS
008970     END-IF
008940
008980     IF MSG1-TX-ERRO-TS EQUAL SPACES
008990        MOVE 'ERRO NAO ESPECIFICADO NO MODULO' TO MSG1-TX-ERRO-TS
009000     END-IF
008940
009010     PERFORM 999000-GRAVA-ERRO-E-ENCERRA
009020     .
009030 999000-ERRO-SAI.
009040     EXIT.
009060*----------------------------------------------------------------*
009070*     As sections abaixo representam os tratamentos de erro
009080*    utilizando o sistema MSG de mensageria (rotina MSGS0001)
009090*-----------------------------------------------------------------
009100 999000-GRAVA-ERRO-E-ENCERRA     SECTION.
009110*-----------------------------------------------------------------
009130*    Em caso de erro, desfaz as atualizações em banco de dados
009140
009150     EXEC CICS SYNCPOINT
009160               ROLLBACK
009170     END-EXEC
009180
009190*Verifica se o tipo de erro gerado possui tratamento interno
009200     EVALUATE TRUE
009210         WHEN ERRO-CONTAINER
009220              PERFORM 999004-TRATA-ERRO-CTNR
009230         WHEN ERRO-TAMANHO-AREA
009240              PERFORM 999005-TRATA-ERRO-AREA-DADOS
009250         WHEN ERRO-CANAL
009260              PERFORM 999006-TRATA-ERRO-CANAL
009270         WHEN OTHER
009280              CONTINUE
009290     END-EVALUATE
009300
009310*Adiciona as variáveis de entrada para monitoração
000440         MOVE 'SGTS0097-E-CD-CTRA-TIT-VL'
009340                                 TO MSGKW999-NM-VRV-MNT
009350         MOVE  SGTS0097-E-CD-CTRA-TIT-VL
009360                                 TO MSGKW999-VL-VRV-MNT
009370         PERFORM 999003-ADICIONA-VRV-MNT
009300
000440         MOVE 'SGTS0097-E-NR-NOTA-TCN-OPR'
009340                                 TO MSGKW999-NM-VRV-MNT
009350         MOVE  SGTS0097-E-NR-NOTA-TCN-OPR
009360                                 TO MSGKW999-VL-VRV-MNT
009370         PERFORM 999003-ADICIONA-VRV-MNT
009300
000440         MOVE 'SGTS0097-E-CD-OPR-ETRD-MCP'
009340                                 TO MSGKW999-NM-VRV-MNT
009350         MOVE  SGTS0097-E-CD-OPR-ETRD-MCP
009360                                 TO MSGKW999-VL-VRV-MNT
009370         PERFORM 999003-ADICIONA-VRV-MNT
009300
000440         MOVE 'SGTS0097-E-NR-VRS-OPR-ETRD'
009340                                 TO MSGKW999-NM-VRV-MNT
009350         MOVE  SGTS0097-E-NR-VRS-OPR-ETRD
009360                                 TO MSGKW999-VL-VRV-MNT
009370         PERFORM 999003-ADICIONA-VRV-MNT
009380
009400
009410*    Grava o erro gerado no sistema MSG - Mensageria
009420     PERFORM MSG1-NTF-MSG
009430
009440     MOVE MSG1-CD-ERRO-TS        TO IIBKERRO-CD-ERRO
009450     MOVE MSG1-TX-ERRO-TS        TO IIBKERRO-TX-ERRO
009460
009470*    Grava o erro gerado no container de erro
009480     PERFORM 984000-GRAVAR-CONTAINER-ERRO
009490
009500*    Encerra a transacao
009510     PERFORM 990000-ENCERRAR
009520     .
009530 999000-SAI.
009540     EXIT.
009550* --------------------------------------*
009560* 999001-TRATA-ABEND-CICS        SECTION.
009570* 999002-TRATA-CONDITION-CICS    SECTION.
009580* 999003-ADICIONA-VRV-MNT        SECTION.
009590* --------------------------------------*
-INC HLPKP999
009610* --------------------------------------*
009620* 999004-TRATA-ERRO-CTNR         SECTION.
009630* 999005-TRATA-ERRO-AREA-DADOS   SECTION.
009640* 999006-TRATA-ERRO-CANAL        SECTION.
009650*---------------------------------------*
-INC SGTKP999
009670* --------------------------------------*
009680* MSG1-NTF-MSG                   SECTION.
009690* --------------------------------------*
-INC MSGKPMG1
009710*---------------------- FIM SGTS0097 -----------------------------