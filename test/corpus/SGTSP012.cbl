000010*-----------------------------------------------------------------
000020* PROGRAMA..: SGTSP012
000030* ANALISTA..: F2054109 - Cintia Ap Soares Iglesias
000040* AUTOR.....: F1960679 - Celso Amaral Revoredo
000050* OBJETIVO..: LISTAR TODOS OS ITENS DE MODELOS DE ORDEM DE
000060*             NEGOCIAÇAO DE COMPRA DE UM PRODUTO/MODALIDADE
000070*             DISPONIVEIS PARA V?NCULO.
000080* COMPILACAO: COBOL/CICS/DB2
000090*-----------------------------------------------------------------
000100* VRS002 24/06/2019 F9188847 DE 2016/145195 - Acerto de mensageria
000100* VRS001 24/11/2015 F1960679  DE 2013/120821 - IMPLANTACAO
000110*-----------------------------------------------------------------
000120 IDENTIFICATION DIVISION.
000130*-----------------------------------------------------------------
000140 PROGRAM-ID. SGTSP012.
000150
000160*-----------------------------------------------------------------
000170 ENVIRONMENT DIVISION.
000180*-----------------------------------------------------------------
000190 CONFIGURATION SECTION.
000200 SPECIAL-NAMES.
000210     DECIMAL-POINT IS COMMA.
000220
000230*-----------------------------------------------------------------
000240 DATA DIVISION.
000250*-----------------------------------------------------------------
000260
000270*-----------------------------------------------------------------
000280 WORKING-STORAGE SECTION.
000290*-----------------------------------------------------------------
000300 77  CTE-INICIO-SS            PIC X(35)  VALUE
000310         '*** STORAGE SECTION COMECA AQUI ***'.
000320 01  CTE-PROG                 PIC  X(17) VALUE '*** SGTSP012 ***'.
000330 01  CTE-VERS                 PIC  X(06) VALUE 'VRS002'.

000340*--> Book do programa de ambiente - CICS5000
      *    Definido na work para preservar as informacoes
-INC HLPKE01B
      *-----------------------------------------------------------------
000350 LOCAL-STORAGE SECTION.
000360
000370*-----------------------------------------------------------------
000380* book de para acionamento do msg1
-INC IIBKERRO
      *--- Book de variáveis da mensageria
      *--- Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1
      *--- Book de variáveis para tratamento de erros CICS e SQL
-INC HLPKW999
      *--- Book de variáveis da mensageria
-INC MSGKWMG1

000420*01  GRP-CONTAINER.
000430*    03  CANAL                        PIC X(16) VALUE SPACES.
000440*    03  QT-CCT-LIDO                  PIC S9(9) COMP-5 VALUE 0.
000450*    03  QT-CCT-EPRO                  PIC S9(9) COMP-5 VALUE 0.
000460*    03  VL-EXBR-1                    PIC 9(10)        VALUE 0.
000470*    03  VL-EXBR-2                    PIC 9(10)        VALUE 0.
000480
000490* Area de trabalho
000500
000510 01 QT-REG-SELD                       PIC S9(4) COMP-5 VALUE 0.
000520
000530* Campos utilizados para leitura
000540
000550 01  REG-WS.
000560     03 CD-ITEM-MOD-ORD                PIC S9(4) COMP-5
000570        OCCURS 101 TIMES               VALUE 0.
000580     03 TX-ITEM-MOD-ORD                PIC X(60)
000590        OCCURS 101 TIMES               VALUE ' '.
000600
000610 01  QT-REG-SELD-N                     PIC S9(4) COMP-5 VALUE 0.
000620
000630* Variavel(is) de controle do cursor(es)
000640
000650 01  CTL-CURSOR-PSQ.
000660     03  CD-FIM-CURSOR-PSQ            PIC S9(04) COMP-5 VALUE 0.
000670         88  FIM-CURSOR-PSQ           VALUE +100.
000680     03  IN-CURSOR-PSQ-ABTO           PIC  X(01) VALUE 'N'.
000690         88  CURSOR-PSQ-ABTO          VALUE 'S'.
000700
000710
000720* Subrotinas utilizadas
000730
000740     EXEC SQL
000750         INCLUDE SQLCA
000760     END-EXEC.
000770
000780*-----------------------------------------------------------------
-INC SGTKP012                                                           00000790
000800
000810* Listar todos os ítens de modelos de ordem de
000820* negociação de compra de um produto/modalidade disponíveis
000830* para vínculo.
000840
000850     EXEC SQL
000860       DECLARE CURSOR-PSQ       CURSOR
000870               WITH ROWSET POSITIONING FOR
000880       SELECT  A.CD_ITEM_MOD_ORD, A.TX_ITEM_MOD_ORD
000890         FROM  DB2SGT.ITEM_MOD_ORD_NEGO A
000900        INNER  JOIN DB2SGT.ITEM_PMT_MOD_CPR B
000910        ON
000920          A.CD_ITEM_MOD_ORD = B.CD_ITEM_MOD_ORD
000930        WHERE
000940          A.CD_ITEM_MOD_ORD > :SGTSP012-CD-ITEM-MOD-ORD-ENTD
000950        AND
000960          B.CD_PRD = :SGTSP012-CD-PRD
000970        AND
000980          B.CD_MDLD = :SGTSP012-CD-MDLD
000990        ORDER BY
001000          A.CD_ITEM_MOD_ORD
001010     END-EXEC.
001020
001030*-----------------------------------------------------------------
001040 PROCEDURE DIVISION.
001050*-----------------------------------------------------------------
001060*-----------------------------------------------------------------
001070 000000-PRINCIPAL  SECTION.
001080*-----------------------------------------------------------------
001090* Habilita tratamento automatico de erros DB2
-INC MSGKE997                                                           00001100
001110* Habilita captura automatica de abend e erros CICS
-INC MSGKE999
001130
001140     PERFORM 100000-DESCOBRIR-CANAL
001150     PERFORM 110000-EXCLUIR-CTNR-RPST
001160     PERFORM 120000-EXCLUIR-CTNR-ERRO
001170     PERFORM 130000-RECEBER-REQUISICAO
001190     PERFORM 200000-SOLICITAR-LISTA
001200     PERFORM 300000-PROCESSA-PESQUISA
001210              VARYING SGTSP012-IC-TAB FROM 1 BY 1
001220                UNTIL SGTSP012-IC-TAB > SGTSP012-QT-OCR-TAB
001230
001240     PERFORM 400000-POSTA-RESPOSTA
001250     GOBACK
001260     .
001270 000000-SAI.
001280     EXIT.
001290*-----------------------------------------------------------------
001300 100000-DESCOBRIR-CANAL SECTION.
001310*-----------------------------------------------------------------
001320     MOVE 0001                   TO MSG1-CD-SEQL-ERRO
001330
           SET  ERRO-CANAL             TO  TRUE

001340     EXEC CICS ASSIGN
001350        CHANNEL ( NM-CNL )
001360     END-EXEC

           SET  SEM-ERRO               TO  TRUE
001370     .
001380 100000-SAI.
001390     EXIT.
001400*-----------------------------------------------------------------
001410 110000-EXCLUIR-CTNR-RPST SECTION.
001420*-----------------------------------------------------------------
001430     MOVE 0002                   TO MSG1-CD-SEQL-ERRO
001440
001450     EXEC CICS DELETE
001460        CONTAINER ( 'SGTSP012-RPST' )
001470        CHANNEL   ( NM-CNL      )
001480        NOHANDLE
001490     END-EXEC
001500     IF  NOT (EIBRESP EQUAL DFHRESP(NORMAL)   OR
001510         EIBRESP EQUAL DFHRESP(CONTAINERERR) )
001520         PERFORM 999002-TRATA-CONDITION-CICS
001530     END-IF
001540     .
001550 110000-SAI.
001560     EXIT.
001570*-----------------------------------------------------------------
001580 120000-EXCLUIR-CTNR-ERRO SECTION.
001590*-----------------------------------------------------------------
001600     MOVE 0003                   TO MSG1-CD-SEQL-ERRO
001610
001620     EXEC CICS DELETE
001630        CONTAINER ( 'PRM-ERRO' )
001640        CHANNEL   ( NM-CNL      )
001650        NOHANDLE
001660     END-EXEC
001670     IF  NOT (EIBRESP EQUAL DFHRESP(NORMAL)   OR
001680         EIBRESP EQUAL DFHRESP(CONTAINERERR) )
001690         PERFORM 999002-TRATA-CONDITION-CICS
001700     END-IF
001710     .
001720 120000-SAI.
001730     EXIT.
001740*-----------------------------------------------------------------
001750 130000-RECEBER-REQUISICAO SECTION.
001760*-----------------------------------------------------------------
001770     MOVE 0004                   TO MSG1-CD-SEQL-ERRO
001780
001790     MOVE ZEROS                    TO QT-CCT-LIDO
001800     MOVE LENGTH OF SGTSP012-RQSC  TO QT-CCT-EPRO

           SET  GET-CONTAINER               TO TRUE
           SET  ERRO-CONTAINER              TO TRUE

001810     EXEC CICS GET
001820        CONTAINER ( 'SGTSP012-RQSC' )
001830        CHANNEL   ( NM-CNL      )
001840        NODATA
001850        FLENGTH ( QT-CCT-LIDO )
001860     END-EXEC

           SET  SEM-ERRO               TO TRUE
001870
001880     IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
001890         SET   ERRO-TAMANHO-AREA   TO  TRUE
001990         PERFORM 999000-GRAVA-ERRO-E-ENCERRA
002000     END-IF

012530     SET  GET-CONTAINER          TO TRUE
012530     SET  ERRO-CONTAINER         TO TRUE
002010
002020     EXEC CICS GET
002030        CONTAINER ( 'SGTSP012-RQSC' )
002040        CHANNEL   ( NM-CNL      )
002050        FLENGTH ( LENGTH OF SGTSP012-RQSC )
002060        INTO    ( SGTSP012-RQSC )
002070     END-EXEC
002080
           SET SEM-ERRO                TO TRUE
002090     .
002100 130000-SAI.
002110     EXIT.
002120
002130*-----------------------------------------------------------------
002140 200000-SOLICITAR-LISTA          SECTION.
002150*-----------------------------------------------------------------
002160     MOVE 0005                   TO MSG1-CD-SEQL-ERRO
002170
002180     INITIALIZE REG-WS
002190     MOVE 'N'                    TO SGTSP012-IN-CNTO-PSQ
002200
002210     PERFORM 970101-ABRE-CURSOR-PSQ
002220     PERFORM 970102-LE-CURSOR-PSQ
002230     PERFORM 970103-FECHA-CURSOR-PSQ
002240
002250
002260     .
002270 200000-SAI.
002280     EXIT.
002290*-----------------------------------------------------------------
002300 300000-PROCESSA-PESQUISA SECTION.
002310*-----------------------------------------------------------------
002320     MOVE 0006                   TO MSG1-CD-SEQL-ERRO
002330
002340     MOVE CD-ITEM-MOD-ORD(SGTSP012-IC-TAB) TO
002350               SGTSP012-CD-ITEM-MOD-ORD(SGTSP012-IC-TAB)
002360     MOVE TX-ITEM-MOD-ORD(SGTSP012-IC-TAB) TO
002370               SGTSP012-TX-ITEM-MOD-ORD(SGTSP012-IC-TAB)
002380
002390     .
002400 300000-SAI.
002410     EXIT.
002420*-----------------------------------------------------------------
002430 400000-POSTA-RESPOSTA SECTION.
002440*-----------------------------------------------------------------
002450     MOVE 0007                   TO MSG1-CD-SEQL-ERRO
002460     EXEC CICS PUT
002470        CONTAINER ( 'SGTSP012-RPST' )
002480        CHANNEL   ( NM-CNL      )
002490        FLENGTH   ( LENGTH OF SGTSP012-RPST )
002500        FROM      ( SGTSP012-RPST )
002510     END-EXEC
002520
002530     .
002540 400000-SAI.
002550     EXIT.
002560*-----------------------------------------------------------------
002570 910000-VERIFICA-FIM-LISTA       SECTION.
002580*-----------------------------------------------------------------
002590     MOVE 0008                   TO MSG1-CD-SEQL-ERRO
002600
002610     MOVE SQLERRD (3)            TO QT-REG-SELD
002620
002630     IF  QT-REG-SELD             EQUAL 101
002640         SET SGTSP012-TEM-MAIS-REGISTRO TO TRUE
002650         MOVE 100                TO QT-REG-SELD
002660     END-IF
002670
002680     MOVE QT-REG-SELD            TO SGTSP012-QT-OCR-TAB
002690
002700     .
002710 910000-SAI.
002720     EXIT.
002730*-----------------------------------------------------------------
002740 970101-ABRE-CURSOR-PSQ          SECTION.
002750*-----------------------------------------------------------------
002760     MOVE 0009                   TO MSG1-CD-SEQL-ERRO
002770     EXEC SQL
002780          OPEN CURSOR-PSQ
002790     END-EXEC
002800     MOVE 'S'                    TO IN-CURSOR-PSQ-ABTO
002810     .
002820 970101-SAI.
002830     EXIT.
002840*-----------------------------------------------------------------
002850 970102-LE-CURSOR-PSQ            SECTION.
002860*-----------------------------------------------------------------
002870     MOVE 0010                   TO MSG1-CD-SEQL-ERRO
002880
002890     EXEC SQL
002900          FETCH NEXT ROWSET
002910                CURSOR-PSQ       FOR 0101 ROWS
002920         INTO :CD-ITEM-MOD-ORD
002930            , :TX-ITEM-MOD-ORD
002940     END-EXEC
002950
002960     PERFORM  910000-VERIFICA-FIM-LISTA
002970
002980     MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ
002990     .
003000 970102-SAI.
003010     EXIT.
003020*-----------------------------------------------------------------
003030 970103-FECHA-CURSOR-PSQ         SECTION.
003040*-----------------------------------------------------------------
003050     MOVE 0011                   TO MSG1-CD-SEQL-ERRO
003060     MOVE 'N'                    TO IN-CURSOR-PSQ-ABTO
003070     EXEC SQL
003080          CLOSE CURSOR-PSQ
003090     END-EXEC
003100     .
003110 970103-SAI.
003120     EXIT.
003130*-----------------------------------------------------------------
003140 999000-GRAVA-ERRO-E-ENCERRA SECTION.
003150*-----------------------------------------------------------------
003160* Por precaução verifica se algum cursor ficou aberto.
003170     IF  CURSOR-PSQ-ABTO
003180          PERFORM 970103-FECHA-CURSOR-PSQ
003190     END-IF

           IF  MSG1-CD-ERRO-TS         EQUAL  ZEROS
              MOVE  8888               TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS         EQUAL  SPACES
               MOVE  'Erro não especificado no módulo.'
                                       TO  MSG1-TX-ERRO-TS
           END-IF

      *--> Verifica se o tipo de erro gerado possui tratamento interno
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

           MOVE 'SGTSP012-CD-ITEM-MOD-ORD-ENTD'
                                       TO MSGKW999-NM-VRV-MNT
002770     MOVE  SGTSP012-CD-ITEM-MOD-ORD-ENTD
                                       TO MSGKW999-VL-VRV-MNT
002780     PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSP012-CD-MDLD'
                                       TO MSGKW999-NM-VRV-MNT
002770     MOVE  SGTSP012-CD-MDLD
                                       TO MSGKW999-VL-VRV-MNT
002780     PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSP012-CD-PRD'
                                       TO MSGKW999-NM-VRV-MNT
002770     MOVE  SGTSP012-CD-PRD
                                       TO MSGKW999-VL-VRV-MNT
002780     PERFORM 999003-ADICIONA-VRV-MNT
003200
003210* --> grava o erro no msg
003220     PERFORM MSG1-NTF-MSG
003230     MOVE MSG1-CD-ERRO-TS             TO IIBKERRO-CD-ERRO
003240     MOVE MSG1-TX-ERRO-TS             TO IIBKERRO-TX-ERRO
003250
003260     EXEC CICS PUT
003270        CONTAINER ( 'PRM-ERRO' )
003280        CHANNEL   ( NM-CNL      )
003290        FROM      ( IIBKERRO-PRM-ERRO)
003300        FLENGTH ( LENGTH OF IIBKERRO-PRM-ERRO )
003310        NOHANDLE
003320     END-EXEC
003330
003340     PERFORM 999999-ENCERRAR
003350
003360     .
003370 999000-SAI.
003380     EXIT.
003390* --------------------------------------*
003400* 999001-TRATA-ABEND-CICS        SECTION.
003410* 999002-TRATA-CONDITION-CICS    SECTION.
003420* 999003-ADICIONA-VRV-MNT        SECTION.
003430* --------------------------------------*
-INC HLPKP999
      *---------------------------------------*
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *---------------------------------------*
-INC SGTKP999
003450* --------------------------------------*
003460* 999010-TRATA-ERRO-DB2          SECTION.
-INC HLPKP997                                                           00003470
003480* --------------------------------------*
003490* MSG1-NTF-MSG                   SECTION.
-INC MSGKPMG1                                                           00003500
003510* --------------------------------------*
003520 999999-ENCERRAR                SECTION.
003530* --------------------------------------*
003540     EXEC CICS
003550          RETURN
003560     END-EXEC.