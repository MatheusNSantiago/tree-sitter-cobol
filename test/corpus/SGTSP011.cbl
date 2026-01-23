000010*-----------------------------------------------------------------
000020* PROGRAMA..: SGTSP011
000030* ANALISTA..: F2054109 - Cintia Ap Soares Iglesias
000040* AUTOR.....: F1960679 - Celso Amaral Revoredo
000050* OBJETIVO..: Listar todos itens do Modelo de Ordem de Negociação
000060*             de Venda disponíveis para vinculo.
000070* COMPILACAO: COBOL/CICS/DB2
000080*-----------------------------------------------------------------
000090* VRS002 18/06/2019 F9188847 DE 2016/145195 - Acerto de mensageria
000090* VRS001 24/11/2015 F1960679 DE 2013/120821 - IMPLANTACAO
000100*-----------------------------------------------------------------
000110 IDENTIFICATION DIVISION.
000120*-----------------------------------------------------------------
000130 PROGRAM-ID. SGTSP011.
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
000290 77  CTE-INICIO-SS            PIC X(35)  VALUE
000300         '*** STORAGE SECTION COMECA AQUI ***'.
000310 01  CTE-PROG                 PIC  X(17) VALUE '*** SGTSP011 ***'.
000320 01  CTE-VERS                 PIC  X(06) VALUE 'VRS002'.

000330* Book do programa de ambiente - CICS5000
000360* Definido na work para preservar as informacoes
-INC HLPKE01B

000340*-----------------------------------------------------------------
000390 LOCAL-STORAGE SECTION.
000400*-----------------------------------------------------------------
000370* book de para acionamento do msg1
-INC IIBKERRO                                                           00000380
-INC MSGKWMG1                                                           00000390
-INC HLPKW999                                                           00000400
000410*01  GRP-CONTAINER.
000420*    03  CANAL                        PIC X(16) VALUE SPACES.
000430*    03  QT-CCT-LIDO                  PIC S9(9) COMP-5 VALUE 0.
000440*    03  QT-CCT-EPRO                  PIC S9(9) COMP-5 VALUE 0.
000450*    03  VL-EXBR-1                    PIC 9(10)        VALUE 0.
000460*    03  VL-EXBR-2                    PIC 9(10)        VALUE 0.
000470
000480* Area de trabalho
000490
000500 01 QT-REG-SELD                       PIC S9(4) COMP-5 VALUE 0.
000510
000520* Campos utilizados para leitura
000530
000540 01  REG-WS.
000550     03 CD-ITEM-MOD-ORD                PIC S9(4) COMP-5
000560        OCCURS 101 TIMES               VALUE 0.
000570     03 TX-ITEM-MOD-ORD                PIC X(60)
000580        OCCURS 101 TIMES               VALUE ' '.
000590
000600 01  QT-REG-SELD-N                     PIC S9(4) COMP-5 VALUE 0.
000610
000620* Variavel(is) de controle do cursor(es)
000630
000640 01  CTL-CURSOR-PSQ.
000650     03  CD-FIM-CURSOR-PSQ            PIC S9(04) COMP-5 VALUE 0.
000660         88  FIM-CURSOR-PSQ           VALUE +100.
000670     03  IN-CURSOR-PSQ-ABTO           PIC  X(01) VALUE 'N'.
000680         88  CURSOR-PSQ-ABTO          VALUE 'S'.
000690
000700
000710* Subrotinas utilizadas
000720
000730     EXEC SQL
000740         INCLUDE SQLCA
000750     END-EXEC.
000760
000770*-----------------------------------------------------------------
-INC SGTKP011                                                           00000780
000790
      * Book da variaveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1
000800* Listar todos os ítens de modelos de ordem de
000810* negociação de venda disponíveis para vínculo.
000820
000830     EXEC SQL
000840       DECLARE CURSOR-PSQ       CURSOR
000850               WITH ROWSET POSITIONING FOR
000860       SELECT CD_ITEM_MOD_ORD, TX_ITEM_MOD_ORD
000870          FROM DB2SGT.ITEM_MOD_ORD_NEGO
000880          WHERE
000890              CD_ITEM_MOD_ORD > :SGTSP011-CD-ITEM-MOD-ORD-ENTD
000900          AND
000910              IN_ITEM_PMT_VND = 'S'
000920          ORDER BY
000930              CD_ITEM_MOD_ORD
000940     END-EXEC.
000950
000960
000970*-----------------------------------------------------------------
000980 PROCEDURE DIVISION.
000990*-----------------------------------------------------------------
001000*-----------------------------------------------------------------
001010 000000-PRINCIPAL  SECTION.
001020*-----------------------------------------------------------------
001030* Habilita tratamento automatico de erros DB2
-INC MSGKE997                                                           00001040
001050* Habilita captura automatica de abend e erros CICS
-INC MSGKE999                                                           00001060
001070
001080     PERFORM 100000-DESCOBRIR-CANAL
001090     PERFORM 110000-EXCLUIR-CTNR-RPST
001100     PERFORM 120000-EXCLUIR-CTNR-ERRO
001110     PERFORM 130000-RECEBER-REQUISICAO

001130     PERFORM 200000-SOLICITAR-LISTA
001140     PERFORM 300000-PROCESSA-PESQUISA
001150              VARYING SGTSP011-IC-TAB FROM 1 BY 1
001160                UNTIL SGTSP011-IC-TAB > SGTSP011-QT-OCR-TAB
001170
001180     PERFORM 400000-POSTA-RESPOSTA
001190     GOBACK
001200     .
001210 000000-SAI.
001220     EXIT.
001230*-----------------------------------------------------------------
001240 100000-DESCOBRIR-CANAL SECTION.
001250*-----------------------------------------------------------------
001260     MOVE 0001                   TO MSG1-CD-SEQL-ERRO
001270
           SET  ERRO-CANAL             TO  TRUE

001280     EXEC CICS ASSIGN
001290        CHANNEL ( NM-CNL )
001300     END-EXEC

           SET  SEM-ERRO               TO  TRUE
001310     .
001320 100000-SAI.
001330     EXIT.
001340*-----------------------------------------------------------------
001350 110000-EXCLUIR-CTNR-RPST SECTION.
001360*-----------------------------------------------------------------
001370     MOVE 0002                   TO MSG1-CD-SEQL-ERRO
001380
001390     EXEC CICS DELETE
001400        CONTAINER ( 'SGTSP011-RPST' )
001410        CHANNEL   ( NM-CNL      )
001420        NOHANDLE
001430     END-EXEC
001440     IF  NOT (EIBRESP EQUAL DFHRESP(NORMAL)   OR
001450         EIBRESP EQUAL DFHRESP(CONTAINERERR) )
001460         PERFORM 999002-TRATA-CONDITION-CICS
001470     END-IF
001480     .
001490 110000-SAI.
001500     EXIT.
001510*-----------------------------------------------------------------
001520 120000-EXCLUIR-CTNR-ERRO SECTION.
001530*-----------------------------------------------------------------
001540     MOVE 0003                   TO MSG1-CD-SEQL-ERRO
001550
001560     EXEC CICS DELETE
001570        CONTAINER ( 'PRM-ERRO' )
001580        CHANNEL   ( NM-CNL      )
001590        NOHANDLE
001600     END-EXEC
001610     IF  NOT (EIBRESP EQUAL DFHRESP(NORMAL)   OR
001620         EIBRESP EQUAL DFHRESP(CONTAINERERR) )
001630         PERFORM 999002-TRATA-CONDITION-CICS
001640     END-IF
001650     .
001660 120000-SAI.
001670     EXIT.
001680*-----------------------------------------------------------------
001690 130000-RECEBER-REQUISICAO SECTION.
001700*-----------------------------------------------------------------
001710     MOVE 0004                   TO MSG1-CD-SEQL-ERRO
001720
001730     MOVE ZEROS                    TO QT-CCT-LIDO
001740     MOVE LENGTH OF SGTSP011-RQSC  TO QT-CCT-EPRO

           SET  GET-CONTAINER               TO TRUE
           SET  ERRO-CONTAINER              TO TRUE

001750     EXEC CICS GET
001760        CONTAINER ( 'SGTSP011-RQSC' )
001770        CHANNEL   ( NM-CNL      )
001780        NODATA
001790        FLENGTH ( QT-CCT-LIDO )
001800     END-EXEC

           SET  SEM-ERRO               TO TRUE
001810
001820     IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
001830         SET   ERRO-TAMANHO-AREA   TO  TRUE
001930         PERFORM 999000-GRAVA-ERRO-E-ENCERRA
001940     END-IF
001950
           SET  GET-CONTAINER               TO TRUE
           SET  ERRO-CONTAINER              TO TRUE

001960     EXEC CICS GET
001970        CONTAINER ( 'SGTSP011-RQSC' )
001980        CHANNEL   ( NM-CNL      )
001990        FLENGTH ( LENGTH OF SGTSP011-RQSC )
002000        INTO    ( SGTSP011-RQSC )
002010     END-EXEC

           SET SEM-ERRO                TO TRUE
002020
002030     .
002040 130000-SAI.
002050     EXIT.
002060
002070*-----------------------------------------------------------------
002080 200000-SOLICITAR-LISTA          SECTION.
002090*-----------------------------------------------------------------
002100     MOVE 0005                   TO MSG1-CD-SEQL-ERRO
002110
002120     INITIALIZE REG-WS
002130     MOVE 'N'                    TO SGTSP011-IN-CNTO-PSQ
002140
002150     PERFORM 970101-ABRE-CURSOR-PSQ
002160     PERFORM 970102-LE-CURSOR-PSQ
002170     PERFORM 970103-FECHA-CURSOR-PSQ
002180
002190     .
002200 200000-SAI.
002210     EXIT.
002220*-----------------------------------------------------------------
002230 300000-PROCESSA-PESQUISA SECTION.
002240*-----------------------------------------------------------------
002250     MOVE 0006                   TO MSG1-CD-SEQL-ERRO
002260
002270     MOVE CD-ITEM-MOD-ORD(SGTSP011-IC-TAB) TO
002280               SGTSP011-CD-ITEM-MOD-ORD(SGTSP011-IC-TAB)
002290     MOVE TX-ITEM-MOD-ORD(SGTSP011-IC-TAB) TO
002300               SGTSP011-TX-ITEM-MOD-ORD(SGTSP011-IC-TAB)
002310
002320     .
002330 300000-SAI.
002340     EXIT.
002350*-----------------------------------------------------------------
002360 400000-POSTA-RESPOSTA SECTION.
002370*-----------------------------------------------------------------
002380     MOVE 0007                   TO MSG1-CD-SEQL-ERRO
002390     EXEC CICS PUT
002400        CONTAINER ( 'SGTSP011-RPST' )
002410        CHANNEL   ( NM-CNL      )
002420        FLENGTH   ( LENGTH OF SGTSP011-RPST )
002430        FROM      ( SGTSP011-RPST )
002440     END-EXEC
002450
002460     .
002470 400000-SAI.
002480     EXIT.
002490*-----------------------------------------------------------------
002500 910000-VERIFICA-FIM-LISTA       SECTION.
002510*-----------------------------------------------------------------
002520     MOVE 0008                   TO MSG1-CD-SEQL-ERRO
002530
002540     MOVE SQLERRD (3)            TO QT-REG-SELD
002550
002560     IF  QT-REG-SELD             EQUAL 101
002570         SET SGTSP011-TEM-MAIS-REGISTRO TO TRUE
002580         MOVE 100                TO QT-REG-SELD
002590     END-IF
002600
002610     MOVE QT-REG-SELD            TO SGTSP011-QT-OCR-TAB
002620
002630     .
002640 910000-SAI.
002650     EXIT.
002660*-----------------------------------------------------------------
002670 970101-ABRE-CURSOR-PSQ          SECTION.
002680*-----------------------------------------------------------------
002690     MOVE 0009                   TO MSG1-CD-SEQL-ERRO
002700     EXEC SQL
002710          OPEN CURSOR-PSQ
002720     END-EXEC
002730     MOVE 'S'                    TO IN-CURSOR-PSQ-ABTO
002740     .
002750 970101-SAI.
002760     EXIT.
002770*-----------------------------------------------------------------
002780 970102-LE-CURSOR-PSQ            SECTION.
002790*-----------------------------------------------------------------
002800     MOVE 0010                   TO MSG1-CD-SEQL-ERRO
002810
002820     EXEC SQL
002830          FETCH NEXT ROWSET
002840                CURSOR-PSQ       FOR 0101 ROWS
002850         INTO :CD-ITEM-MOD-ORD
002860            , :TX-ITEM-MOD-ORD
002870     END-EXEC
002880
002890     PERFORM  910000-VERIFICA-FIM-LISTA
002900
002910     MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ
002920     .
002930 970102-SAI.
002940     EXIT.
002950*-----------------------------------------------------------------
002960 970103-FECHA-CURSOR-PSQ         SECTION.
002970*-----------------------------------------------------------------
002980     MOVE 0011                   TO MSG1-CD-SEQL-ERRO
002990     MOVE 'N'                    TO IN-CURSOR-PSQ-ABTO
003000     EXEC SQL
003010          CLOSE CURSOR-PSQ
003020     END-EXEC
003030     .
003040 970103-SAI.
003050     EXIT.
003060*-----------------------------------------------------------------
003070 999000-GRAVA-ERRO-E-ENCERRA SECTION.
003080*-----------------------------------------------------------------
003090* Por precaução verifica se algum cursor ficou aberto.
003100     IF  CURSOR-PSQ-ABTO
003110          PERFORM 970103-FECHA-CURSOR-PSQ
003120     END-IF

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

           MOVE 'SGTSP011-CD-ITEM-MOD-ORD-ENTD'
                                       TO MSGKW999-NM-VRV-MNT
002770     MOVE  SGTSP011-CD-ITEM-MOD-ORD-ENTD
                                       TO MSGKW999-VL-VRV-MNT
002780     PERFORM 999003-ADICIONA-VRV-MNT

003130
003140* --> grava o erro no msg
003150     PERFORM MSG1-NTF-MSG
003160     MOVE MSG1-CD-ERRO-TS             TO IIBKERRO-CD-ERRO
003170     MOVE MSG1-TX-ERRO-TS             TO IIBKERRO-TX-ERRO
003180
003190     EXEC CICS PUT
003200        CONTAINER ( 'PRM-ERRO' )
003210        CHANNEL   ( NM-CNL      )
003220        FROM      ( IIBKERRO-PRM-ERRO)
003230        FLENGTH ( LENGTH OF IIBKERRO-PRM-ERRO )
003240        NOHANDLE
003250     END-EXEC
003260
003270     PERFORM 999999-ENCERRAR
003280
003290     .
003300 999000-SAI.
003310     EXIT.
003320* --------------------------------------*
003330* 999001-TRATA-ABEND-CICS        SECTION.
003340* 999002-TRATA-CONDITION-CICS    SECTION.
003350* 999003-ADICIONA-VRV-MNT        SECTION.
003360* --------------------------------------*
-INC HLPKP999                                                           00003370
003380* --------------------------------------*
003390* 999010-TRATA-ERRO-DB2          SECTION.
-INC HLPKP997                                                           00003400
003410* --------------------------------------*
003420* MSG1-NTF-MSG                   SECTION.
-INC MSGKPMG1
      * --------------------------------------*
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *---------------------------------------*
-INC SGTKP999
003440* --------------------------------------*
003450 999999-ENCERRAR                SECTION.
003460* --------------------------------------*
003470     EXEC CICS
003480          RETURN
003490     END-EXEC.