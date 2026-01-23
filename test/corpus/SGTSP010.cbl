000010*-----------------------------------------------------------------
000020* Programa..: SGTSP010
000030* Analista..: F8650584 - Roberto Fernandes Lima
000040* Autor.....: F8650584 - Roberto Fernandes Lima
000050* Objetivo..: Consulta Tabela DB2SGT.PSS_CTT_EXNO
000060* Compilação: Cobol/CICS/DB2
000070*-----------------------------------------------------------------
000090* VRS003 18/06/2019 F2020988 DE 2016/145195 - Acerto de mensageria
000080* VRS002 18.11.2016 - F2054109 - DE             - Inclusão de
000080*                     atributo numero de celular
000080* VRS001 11.11.2015 - F8650584 - DE 2013/120821 - Implantação
000090*-----------------------------------------------------------------
000100 IDENTIFICATION DIVISION.
000110*-----------------------------------------------------------------
000120 PROGRAM-ID. SGTSP010.
000130
000140*-----------------------------------------------------------------
000150 ENVIRONMENT DIVISION.
000160*-----------------------------------------------------------------
000170 CONFIGURATION                   SECTION.
000180 SPECIAL-NAMES.
000190     DECIMAL-POINT IS COMMA.
000200
000210*-----------------------------------------------------------------
000220 DATA DIVISION.
000230*-----------------------------------------------------------------
000240
000250*-----------------------------------------------------------------
000260 WORKING-STORAGE                 SECTION.
000270*-----------------------------------------------------------------
000280 77  CTE-PROG                    PIC  X(17)
000290                                 VALUE '*** SGTSP010 ***'.
000300 77  CTE-VERS                    PIC  X(06) VALUE 'VRS003'.
000310
000320 77  TELEFONE-COMERCIAL          PIC S9(04) COMP-5 VALUE 2.
000320 77  TELEFONE-CELULAR            PIC S9(04) COMP-5 VALUE 1.
000330
000340* Book do programa de ambiente - CICS5000
000350* Definido na work para preservar as informacoes
-INC HLPKE01B                                                           00000360
000370
000380*-----------------------------------------------------------------
000390 LOCAL-STORAGE                   SECTION.
000400*-----------------------------------------------------------------
000410
000420*--> Book de para acionamento do MSG1
-INC IIBKERRO                                                           00000430
-INC MSGKWMG1                                                           00000440
-INC HLPKW999                                                           00000450
000460
000470*--> Area de trabalho
000480 01  GRP-CONTAINER.
000490     03  CANAL                   PIC  X(16) VALUE SPACES.
000500*     03  QT-CCT-LIDO             PIC S9(09) COMP-5 VALUE 0.
000510*     03  QT-CCT-EPRO             PIC S9(09) COMP-5 VALUE 0.
000520*     03  VL-EXBR-1               PIC  9(10)        VALUE 0.
000530*     03  VL-EXBR-2               PIC  9(10)        VALUE 0.
000540 01  QT-REG-SELD                 PIC S9(04) COMP-5 VALUE 0.
000550
000560*--> Campos utilizados para leitura
000570 01  REG-WS.
000580     03 CD-PSS-CTT-EXNO          PIC S9(04) COMP-5
000590        OCCURS 101 TIMES         VALUE 0.
000600     03 NM-PSS-CTT-EXNO          PIC  X(60)
000610        OCCURS 101 TIMES         VALUE ' '.
000620     03 NM-CIA-CTT-EXNO          PIC  X(60)
000630        OCCURS 101 TIMES         VALUE ' '.
000640     03 NM-CIA-CTT-EXNO-N        PIC S9(04) COMP-5
000650        OCCURS 101 TIMES         VALUE 0.
000660     03 TX-EMAI-CTT-EXNO         PIC  X(50)
000670        OCCURS 101 TIMES         VALUE ' '.
000680     03 NM-CRG-CTT-EXNO          PIC  X(40)
000690        OCCURS 101 TIMES         VALUE ' '.
000700     03 NM-CRG-CTT-EXNO-N        PIC S9(04) COMP-5
000710        OCCURS 101 TIMES         VALUE 0.
000720     03 CD-SEQL-DDD              PIC S9(04) COMP-5
000730        OCCURS 101 TIMES         VALUE 0.
000740     03 CD-SEQL-DDD-N            PIC S9(04) COMP-5
000750        OCCURS 101 TIMES         VALUE 0.
000760     03 NR-TEL-CTT-EXNO          PIC S9(09) COMP-5
000770        OCCURS 101 TIMES         VALUE 0.
000780     03 NR-TEL-CTT-EXNO-N        PIC S9(04) COMP-5
000790        OCCURS 101 TIMES         VALUE 0.
000800     03 IN-CTT-EXNO-ATI          PIC  X(40)
000810        OCCURS 101 TIMES         VALUE ' '.
000720     03 CD-SEQL-DDD-CELR         PIC S9(04) COMP-5
000730        OCCURS 101 TIMES         VALUE 0.
000740     03 CD-SEQL-DDD-CELR-N        PIC S9(04) COMP-5
000750        OCCURS 101 TIMES         VALUE 0.
000760     03 NR-TEL-CTT-CELR           PIC S9(09) COMP-5
000770        OCCURS 101 TIMES         VALUE 0.
000780     03 NR-TEL-CTT-CELR-N        PIC S9(04) COMP-5
000790        OCCURS 101 TIMES         VALUE 0.
000820
000830*--> Indicadores de nulidades
000840 01  QT-REG-SELD-N                       PIC S9(4) COMP-5 VALUE 0.
000850
000860*--> Variavel(is) de controle do cursor(es)
000870 01  CTL-CURSOR-PSQ.
000880     03  CD-FIM-CURSOR-PSQ            PIC S9(04) COMP-5 VALUE 0.
000890         88  FIM-CURSOR-PSQ            VALUE +100.
000900     03  IN-CURSOR-PSQ-ABTO           PIC  X(01) VALUE 'N'.
000910         88  CURSOR-PSQ-ABTO          VALUE 'S'.
000920
000930*--> SGTSP010 - Area de Requisição e Resposta
-INC SGTKP010                                                           00000940
000950
000960     EXEC SQL
000970         INCLUDE SQLCA
000980     END-EXEC.
000990
      * Book da variaveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1
001000*--> Cursor 01 - Listar todas Pessoas de Contato Externo a partir
001010*                do codigo da Pessoa de Contato Externo informada
001020
001030     EXEC SQL
001040       DECLARE CURSOR-PSQ CURSOR
001050               WITH ROWSET POSITIONING FOR
001060       SELECT
001070             A.CD_PSS_CTT_EXNO
001080           , A.NM_PSS_CTT_EXNO
001090           , A.NM_CIA_CTT_EXNO
001100           , A.TX_EMAI_CTT_EXNO
001110           , A.NM_CRG_CTT_EXNO
001120           , B.CD_SEQL_DDD
001130           , B.NR_TEL_CTT_EXNO
001140           , A.IN_CTT_EXNO_ATI
001120           , E.CD_SEQL_DDD_CELR
001130           , E.NR_TEL_CTT_CELR
001150       FROM
001160             DB2SGT.PSS_CTT_EXNO A
001170       LEFT OUTER JOIN
001180             (
001190       -- RECUPERA TELEFONE COMERCIAL DA PESSOA - INICIO
001200             SELECT
001210                   C.CD_PSS_CTT_EXNO
001220                 , C.CD_SEQL_DDD
001230                 , C.NR_TEL_CTT_EXNO
001240             FROM
001250                   DB2SGT.TEL_PSS_CTT_EXNO C
001260             INNER JOIN
001270             -- RECUPERA SEQUENCIAL PRIMEIRA OCORRÊNCIA DE
001280             -- TELEFONE COMERCIAL POR PESSOA - INICIO
001290                   (
001300                    SELECT
001310                          CD_PSS_CTT_EXNO
001320                        , MIN(NR_SEQL_TEL_EXNO) SEQL
001330                    FROM
001340                          DB2SGT.TEL_PSS_CTT_EXNO
001350                    WHERE
001360                     CD_TIP_TEL   = :TELEFONE-COMERCIAL  AND
001370                  CD_PSS_CTT_EXNO > :SGTSP010-CD-PSS-CTT-EXNO-RQSC
001380                    GROUP BY
001390                          CD_PSS_CTT_EXNO
001400                   ) D
001410             -- RECUPERA PRIMEIRA OCORRÊNCIA DE
001420             -- TELEFONE COMERCIAL POR PESSOA - FIM
001430             ON
001440                   C.CD_PSS_CTT_EXNO  = D.CD_PSS_CTT_EXNO
001450             AND   C.NR_SEQL_TEL_EXNO = D.SEQL
001460       -- RECUPERA TELEFONE COMERCIAL DA PESSOA - FIM
001470             WHERE
001480                C.CD_PSS_CTT_EXNO > :SGTSP010-CD-PSS-CTT-EXNO-RQSC
001490             ORDER BY
001500                    C.CD_PSS_CTT_EXNO
001510             ) B
001520       ON
001530             A.CD_PSS_CTT_EXNO    = B.CD_PSS_CTT_EXNO
001170*
001170*
001170       LEFT OUTER JOIN
001180             (
001190       -- RECUPERA TELEFONE CELULAR DA PESSOA - INICIO
001200             SELECT
001210                   F.CD_PSS_CTT_EXNO
001220                 , F.CD_SEQL_DDD  AS    CD_SEQL_DDD_CELR
001230                 , F.NR_TEL_CTT_EXNO AS NR_TEL_CTT_CELR
001240             FROM
001250                   DB2SGT.TEL_PSS_CTT_EXNO F
001260             INNER JOIN
001270             -- RECUPERA SEQUENCIAL PRIMEIRA OCORRÊNCIA DE
001280             -- TELEFONE CELULAR  POR PESSOA - INICIO
001290                   (
001300                    SELECT
001310                          CD_PSS_CTT_EXNO
001320                        , MIN(NR_SEQL_TEL_EXNO) SEQL
001330                    FROM
001340                          DB2SGT.TEL_PSS_CTT_EXNO
001350                    WHERE
001360                     CD_TIP_TEL   = :TELEFONE-CELULAR    AND
001370                  CD_PSS_CTT_EXNO > :SGTSP010-CD-PSS-CTT-EXNO-RQSC
001380                    GROUP BY
001390                          CD_PSS_CTT_EXNO
001400                   ) G
001410             -- RECUPERA PRIMEIRA OCORRÊNCIA DE
001420             -- TELEFONE CELULAR POR PESSOA - FIM
001430             ON
001440                   F.CD_PSS_CTT_EXNO  = G.CD_PSS_CTT_EXNO
001450             AND   F.NR_SEQL_TEL_EXNO = G.SEQL
001460       -- RECUPERA TELEFONE CELULAR DA PESSOA - FIM
001470             WHERE
001480                F.CD_PSS_CTT_EXNO > :SGTSP010-CD-PSS-CTT-EXNO-RQSC
001510             ) E
001520       ON
001530             A.CD_PSS_CTT_EXNO    = E.CD_PSS_CTT_EXNO
001540       WHERE
001550             A.CD_PSS_CTT_EXNO  > :SGTSP010-CD-PSS-CTT-EXNO-RQSC
001560       ORDER BY
001570             A.CD_PSS_CTT_EXNO
001580     END-EXEC.
001590
001600*-----------------------------------------------------------------
001610 PROCEDURE DIVISION.
001620*-----------------------------------------------------------------
001630
001640*--> Habilita tratamento automatico de erros DB2
-INC MSGKE997                                                           00001650
001660*--> Habilita captura automatica de abend e erros CICS
-INC MSGKE999                                                           00001670
001680
001690     PERFORM 100000-DESCOBRIR-CANAL
001700     PERFORM 110000-EXCLUIR-CTNR-RPST
001710     PERFORM 120000-EXCLUIR-CTNR-ERRO
001720     PERFORM 130000-RECEBER-REQUISICAO
001730     PERFORM 140000-VALIDAR-PARAMETRO
001740
001750     PERFORM 970101-ABRE-CURSOR-PSQ
001760     PERFORM 970102-LER-CURSOR-PSQ
001770     PERFORM 970103-FECHA-CURSOR-PSQ
001780
001790     PERFORM 200000-PROCESSA-PESQUISA
001800              VARYING SGTSP010-IC-TAB FROM 1 BY 1
001810              UNTIL   SGTSP010-IC-TAB > SGTSP010-QT-OCR-TAB
001820
001830     PERFORM 300000-POSTA-RESPOSTA
001840     GOBACK
001850     .
001860
001870*-----------------------------------------------------------------
001880 100000-DESCOBRIR-CANAL          SECTION.
001890*-----------------------------------------------------------------
001900     MOVE  0001                  TO  MSG1-CD-SEQL-ERRO
001910
           SET  ERRO-CANAL             TO  TRUE

001920     EXEC CICS ASSIGN
001930        CHANNEL ( CANAL )
001940     END-EXEC

           SET  SEM-ERRO               TO  TRUE

001950     .
001960 100000-SAI.
001970     EXIT.
001980
001990*-----------------------------------------------------------------
002000 110000-EXCLUIR-CTNR-RPST        SECTION.
002010*-----------------------------------------------------------------
002020     MOVE  0002                  TO  MSG1-CD-SEQL-ERRO
002030
002040     EXEC CICS DELETE
002050        CONTAINER ( 'SGTSP010-RPST' )
002060        CHANNEL   ( CANAL      )
002070        NOHANDLE
002080     END-EXEC
002090     IF  NOT (EIBRESP EQUAL DFHRESP(NORMAL)   OR
002100         EIBRESP EQUAL DFHRESP(CONTAINERERR) )
002110         PERFORM 999002-TRATA-CONDITION-CICS
002120     END-IF
002130     .
002140 110000-SAI.
002150     EXIT.
002160
002170*-----------------------------------------------------------------
002180 120000-EXCLUIR-CTNR-ERRO        SECTION.
002190*-----------------------------------------------------------------
002200     MOVE  0003                  TO  MSG1-CD-SEQL-ERRO
002210
002220     EXEC CICS DELETE
002230        CONTAINER ( 'PRM-ERRO' )
002240        CHANNEL   ( CANAL      )
002250        NOHANDLE
002260     END-EXEC
002270     IF  NOT (EIBRESP EQUAL DFHRESP(NORMAL)   OR
002280         EIBRESP EQUAL DFHRESP(CONTAINERERR) )
002290         PERFORM 999002-TRATA-CONDITION-CICS
002300     END-IF
002310     .
002320 120000-SAI.
002330     EXIT.
002340
002350*-----------------------------------------------------------------
002360 130000-RECEBER-REQUISICAO       SECTION.
002370*-----------------------------------------------------------------
002380     MOVE  0004                  TO  MSG1-CD-SEQL-ERRO
002390
002400     MOVE  ZEROS                 TO  QT-CCT-LIDO
002410     MOVE  LENGTH OF SGTSP010-RQSC   TO  QT-CCT-EPRO

           SET  GET-CONTAINER               TO TRUE
           SET  ERRO-CONTAINER              TO TRUE
002420
002430     EXEC CICS GET
002440        CONTAINER ( 'SGTSP010-RQSC' )
002450        CHANNEL   ( CANAL      )
002460        NODATA
002470        FLENGTH ( QT-CCT-LIDO )
002480     END-EXEC
002490
001820     IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
001830         SET   ERRO-TAMANHO-AREA   TO  TRUE
001930         PERFORM 999000-GRAVA-ERRO-E-ENCERRA
001940     END-IF
002630
002640     EXEC CICS GET
002650        CONTAINER ( 'SGTSP010-RQSC' )
002660        CHANNEL   ( CANAL      )
002670        FLENGTH ( LENGTH OF SGTSP010-RQSC )
002680        INTO    ( SGTSP010-RQSC )
002690     END-EXEC

           SET SEM-ERRO                TO TRUE

002700     .
002710 130000-SAI.
002720     EXIT.
002730
002740*-----------------------------------------------------------------
002750 140000-VALIDAR-PARAMETRO        SECTION.
002760*-----------------------------------------------------------------
002770     MOVE  0006                  TO  MSG1-CD-SEQL-ERRO
002780
002790     MOVE  'N'                   TO  SGTSP010-IN-CTNC-PSQ
002800
002810     IF  SGTSP010-CD-PSS-CTT-EXNO-RQSC  LESS  ZEROS
002820         SET   ERRO-PROGRAMADO   TO  TRUE
002821         MOVE  0059              TO  MSG1-CD-ERRO-TS
002840         MOVE  'Código da Pessoa de Contato Externo Inválido.'
002850                                 TO  MSG1-TX-ERRO-TS
002860         PERFORM 999000-GRAVA-ERRO-E-ENCERRA
002870     END-IF
002880     .
002890 140000-SAI.
002900     EXIT.
002910
002920*-----------------------------------------------------------------
002930 200000-PROCESSA-PESQUISA        SECTION.
002940*-----------------------------------------------------------------
002950     MOVE  0008                  TO  MSG1-CD-SEQL-ERRO
002960
002970     MOVE  CD-PSS-CTT-EXNO                       (SGTSP010-IC-TAB)
002980       TO  SGTSP010-CD-PSS-CTT-EXNO              (SGTSP010-IC-TAB)
002990     MOVE  NM-PSS-CTT-EXNO                       (SGTSP010-IC-TAB)
003000       TO  SGTSP010-NM-PSS-CTT-EXNO              (SGTSP010-IC-TAB)
003010     MOVE  NM-CIA-CTT-EXNO                       (SGTSP010-IC-TAB)
003020       TO  SGTSP010-NM-CIA-CTT-EXNO              (SGTSP010-IC-TAB)
003030     MOVE  TX-EMAI-CTT-EXNO                      (SGTSP010-IC-TAB)
003040       TO  SGTSP010-TX-EMAI-CTT-EXNO             (SGTSP010-IC-TAB)
003050     MOVE  NM-CRG-CTT-EXNO                       (SGTSP010-IC-TAB)
003060       TO  SGTSP010-NM-CRG-CTT-EXNO              (SGTSP010-IC-TAB)
003070     MOVE  CD-SEQL-DDD                           (SGTSP010-IC-TAB)
003080       TO  SGTSP010-CD-SEQL-DDD                  (SGTSP010-IC-TAB)
003090     MOVE  NR-TEL-CTT-EXNO                       (SGTSP010-IC-TAB)
003100       TO  SGTSP010-NR-TEL-CTT-EXNO              (SGTSP010-IC-TAB)
003110     MOVE  IN-CTT-EXNO-ATI                       (SGTSP010-IC-TAB)
003120       TO  SGTSP010-IN-CTT-EXNO-ATI              (SGTSP010-IC-TAB)
003070     MOVE  CD-SEQL-DDD-CELR                      (SGTSP010-IC-TAB)
003080       TO  SGTSP010-CD-SEQL-DDD-CELR             (SGTSP010-IC-TAB)
003090     MOVE  NR-TEL-CTT-CELR                       (SGTSP010-IC-TAB)
003100       TO  SGTSP010-NR-TEL-CTT-CELR              (SGTSP010-IC-TAB)
003130     .
003140 200000-SAI.
003150     EXIT.
003160
003810
003820*-----------------------------------------------------------------
003830 300000-POSTA-RESPOSTA           SECTION.
003840*-----------------------------------------------------------------
003850     MOVE  0012                  TO  MSG1-CD-SEQL-ERRO
003860
003870     EXEC CICS PUT
003880        CONTAINER ( 'SGTSP010-RPST' )
003890        CHANNEL   ( CANAL      )
003900        FLENGTH ( LENGTH OF SGTSP010-RPST )
003910        FROM    ( SGTSP010-RPST )
003920     END-EXEC
003930     .
003940 300000-SAI.
003950     EXIT.
003960
003170*-----------------------------------------------------------------
003180 970101-ABRE-CURSOR-PSQ          SECTION.
003190*-----------------------------------------------------------------
003200     MOVE  0009                  TO  MSG1-CD-SEQL-ERRO
003210
003220     EXEC SQL
003230          OPEN CURSOR-PSQ
003240     END-EXEC
003250
003260     MOVE  'S'                   TO  IN-CURSOR-PSQ-ABTO
003270     .
003280 970101-SAI.
003290     EXIT.
003300
003310*-----------------------------------------------------------------
003320 970102-LER-CURSOR-PSQ           SECTION.
003330*-----------------------------------------------------------------
003340     MOVE  0010                  TO  MSG1-CD-SEQL-ERRO
003350
003360     INITIALIZE REG-WS
003370
003380     EXEC SQL
003390          FETCH NEXT ROWSET
003400                CURSOR-PSQ FOR 0101 ROWS
003410         INTO :CD-PSS-CTT-EXNO
003420            , :NM-PSS-CTT-EXNO
003430            , :NM-CIA-CTT-EXNO
003440              :NM-CIA-CTT-EXNO-N
003450            , :TX-EMAI-CTT-EXNO
003460            , :NM-CRG-CTT-EXNO
003470              :NM-CRG-CTT-EXNO-N
003480            , :CD-SEQL-DDD
003490              :CD-SEQL-DDD-N
003500            , :NR-TEL-CTT-EXNO
003510              :NR-TEL-CTT-EXNO-N
003520            , :IN-CTT-EXNO-ATI
003480            , :CD-SEQL-DDD-CELR
003490              :CD-SEQL-DDD-CELR-N
003500            , :NR-TEL-CTT-CELR
003510              :NR-TEL-CTT-CELR-N
003530     END-EXEC
003540
003550     MOVE  SQLERRD(3)            TO  QT-REG-SELD
003560
003570     IF  QT-REG-SELD EQUAL 0101
003580         SET   SGTSP010-TEM-MAIS-REGISTRO TO TRUE
003590         MOVE  0100              TO  QT-REG-SELD
003600     END-IF
003610
003620     MOVE  QT-REG-SELD           TO  SGTSP010-QT-OCR-TAB
003630     MOVE  SQLCODE               TO  CD-FIM-CURSOR-PSQ
003640     .
003650 970102-SAI.
003660     EXIT.
003670
003680*-----------------------------------------------------------------
003690 970103-FECHA-CURSOR-PSQ         SECTION.
003700*-----------------------------------------------------------------
003710     MOVE  0011                  TO  MSG1-CD-SEQL-ERRO
003720
003730     MOVE  'N'                   TO  IN-CURSOR-PSQ-ABTO
003740
003750     EXEC SQL
003760          CLOSE CURSOR-PSQ
003770     END-EXEC
003780     .
003790 970103-SAI.
003800     EXIT.
003970*-----------------------------------------------------------------
003980 999000-GRAVA-ERRO-E-ENCERRA     SECTION.
003990*-----------------------------------------------------------------
004000
           IF  MSG1-CD-ERRO-TS         EQUAL  ZEROS
              MOVE  8888               TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS         EQUAL  SPACES
               MOVE  'Erro não especificado no módulo.'
                                       TO  MSG1-TX-ERRO-TS
           END-IF

      *--> VERIFICA SE O TIPO DE ERRO GERADO POSSUI TRATAMENTO INTERNO
           EVALUATE TRUE
               WHEN ERRO-CONTAINER
                    PERFORM 999004-TRATA-ERRO-CTNR
               WHEN ERRO-TAMANHO-AREA
                    PERFORM 999005-TRATA-ERRO-AREA-DADOS
               WHEN ERRO-CANAL
                    PERFORM 999006-TRATA-ERRO-CANAL
               WHEN OTHER
                    CONTINUE
           END-EVALUATE

004010     MOVE  'SGTSP010-CD-PSS-CTT-EXNO-RQSC'
004020                                 TO  MSGKW999-NM-VRV-MNT
004030     MOVE  SGTSP010-CD-PSS-CTT-EXNO-RQSC
004040                                 TO  MSGKW999-VL-VRV-MNT
004050     PERFORM 999003-ADICIONA-VRV-MNT
004060
004070*--> grava o erro no msg
004080     PERFORM MSG1-NTF-MSG
004090
004100     MOVE  MSG1-CD-ERRO-TS       TO  IIBKERRO-CD-ERRO
004110     MOVE  MSG1-TX-ERRO-TS       TO  IIBKERRO-TX-ERRO
004120
004130     EXEC CICS PUT
004140        CONTAINER ( 'PRM-ERRO' )
004150        CHANNEL   ( CANAL      )
004160        FROM      ( IIBKERRO-PRM-ERRO)
004170        FLENGTH ( LENGTH OF IIBKERRO-PRM-ERRO )
004180        NOHANDLE
004190     END-EXEC
004200
004210     PERFORM 999999-ENCERRAR
004220     .
004230 999000-SAI.
004240     EXIT.
004250
004260* --------------------------------------*
004270* 999001-TRATA-ABEND-CICS        SECTION.
004280* 999002-TRATA-CONDITION-CICS    SECTION.
004290* 999003-ADICIONA-VRV-MNT        SECTION.
004300* --------------------------------------*
-INC HLPKP999                                                           00004310
004320* --------------------------------------*
004330* 999010-TRATA-ERRO-DB2          SECTION.
-INC HLPKP997                                                           00004340
004350* --------------------------------------*
004360* MSG1-NTF-MSG                   SECTION.
-INC MSGKPMG1
      * --------------------------------------*
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *---------------------------------------*
-INC SGTKP999

004370
004380* --------------------------------------*
004390 999999-ENCERRAR                 SECTION.
004400* --------------------------------------*
004410     EXEC CICS
004420          RETURN
004430     END-EXEC.