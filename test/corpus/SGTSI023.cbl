000010*-----------------------------------------------------------------
000020* PROGRAMA..: SGTSP009
000030* ANALISTA..: F2054109 - Cintia Ap Soares Iglesias
000040* AUTOR.....: F1960679 - Celso Amaral Revoredo
000050* OBJETIVO..: LISTAR ITENS PARA VINCULO EM MODELO DE ORDEM DE
000060*             NEGOCIACAO. TABELA DB2SGT.ITEM_MOD_ORD_NEGO
000070* COMPILACAO: COBOL/CICS/DB2
000080*-----------------------------------------------------------------
000090* VRS002 07/0/2019 F9188847 DE 2016/145195 - Acerto de mensageria
000090* VRS001 24/11/2015 F1960679  DE 2013/120821 - IMPLANTACAO
000100*-----------------------------------------------------------------
000110 IDENTIFICATION DIVISION.
000120*-----------------------------------------------------------------
000130 PROGRAM-ID. SGTSP009.
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
000310 01  CTE-PROG                 PIC  X(17) VALUE '*** SGTSP009 ***'.
000320 01  CTE-VERS                 PIC  X(06) VALUE 'VRS002'.
000330
      * Book do programa de ambiente - CICS5000
000360* Definido na work para preservar as informacoes
-INC HLPKE01B
000340 LOCAL-STORAGE SECTION.
000350
000360*-----------------------------------------------------------------
000370* book de para acionamento do msg1
-INC IIBKERRO                                                           0000038%
-INC MSGKWMG1                                                           0000039%
-INC HLPKW999                                                           0000040%
000410 01  GRP-CONTAINER.
000420     03  CANAL                        PIC X(16) VALUE SPACES.
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
000650     03  CD-FIM-CURSOR-PSQ       PIC S9(04) COMP-5 VALUE 0.
000660         88  FIM-CURSOR-PSQ      VALUE +100.
000670     03  IN-CURSOR-PSQ-ABTO      PIC  X(01) VALUE 'N'.
000680         88  CURSOR-PSQ-ABTO     VALUE 'S'.
000690
000700
000710
000720* Subrotinas utilizadas
000730
000740     EXEC SQL
000750         INCLUDE SQLCA
000760     END-EXEC.
000770
000780*-----------------------------------------------------------------
-INC SGTKP009                                                           0000079%
000800
      * BOOK DAS VARI?VEIS AUXILIARES DO TRATAMENTO DE ERRO VIA MSG
-INC SGTKERR1

000810* Listar todos os itens de modelo de ordem de
000820* negociação disponíveis para vínculo.
000830
000840     EXEC SQL
000850       DECLARE CURSOR-PSQ  CURSOR
000860               WITH ROWSET POSITIONING FOR
000870       SELECT CD_ITEM_MOD_ORD, TX_ITEM_MOD_ORD
000880         FROM DB2SGT.ITEM_MOD_ORD_NEGO
000890         WHERE
000900              CD_ITEM_MOD_ORD > :SGTSP009-CD-ITEM-MOD-ORD-ENTD
000910         ORDER BY
000920              CD_ITEM_MOD_ORD
000930     END-EXEC.
000940
000950
000960*-----------------------------------------------------------------
000970 PROCEDURE DIVISION.
000980*-----------------------------------------------------------------
000990*-----------------------------------------------------------------
001000 000000-PRINCIPAL  SECTION.
001010*-----------------------------------------------------------------
001020* Habilita tratamento automatico de erros DB2
-INC MSGKE997                                                           0000103%
001040* Habilita captura automatica de abend e erros CICS
-INC MSGKE999                                                           0000105%
001060
001070     PERFORM 100000-DESCOBRIR-CANAL
001080     PERFORM 110000-EXCLUIR-CTNR-RPST
001090     PERFORM 120000-EXCLUIR-CTNR-ERRO
001100     PERFORM 130000-RECEBER-REQUISICAO
001110     PERFORM 140000-VALIDA-PARAMETRO
001120     PERFORM 200000-SOLICITAR-LISTA
001130     PERFORM 300000-PROCESSA-PESQUISA
001140              VARYING SGTSP009-IC-TAB FROM 1 BY 1
001150                UNTIL SGTSP009-IC-TAB > SGTSP009-QT-OCR-TAB
001160
001170     PERFORM 400000-POSTA-RESPOSTA
001180     GOBACK
001190     .
001200 000000-SAI.
001210     EXIT.
001220*-----------------------------------------------------------------
001230 100000-DESCOBRIR-CANAL SECTION.
001240*-----------------------------------------------------------------
001250     MOVE 0001                   TO MSG1-CD-SEQL-ERRO
001260
           SET  ERRO-CANAL             TO  TRUE

001270     EXEC CICS ASSIGN
001280        CHANNEL ( CANAL )
001290     END-EXEC

           SET  SEM-ERRO               TO  TRUE
001300     .
001310 100000-SAI.
001320     EXIT.
001330*-----------------------------------------------------------------
001340 110000-EXCLUIR-CTNR-RPST SECTION.
001350*-----------------------------------------------------------------
001360     MOVE 0002                   TO MSG1-CD-SEQL-ERRO
001370
001380     EXEC CICS DELETE
001390        CONTAINER ( 'SGTSP009-RPST' )
001400        CHANNEL   ( CANAL      )
001410        NOHANDLE
001420     END-EXEC
001430     IF  NOT (EIBRESP EQUAL DFHRESP(NORMAL)   OR
001440         EIBRESP EQUAL DFHRESP(CONTAINERERR) )
001450         PERFORM 999002-TRATA-CONDITION-CICS
001460     END-IF
001470     .
001480 110000-SAI.
001490     EXIT.
001500*-----------------------------------------------------------------
001510 120000-EXCLUIR-CTNR-ERRO SECTION.
001520*-----------------------------------------------------------------
001530     MOVE 0003                   TO MSG1-CD-SEQL-ERRO
001540
001550     EXEC CICS DELETE
001560        CONTAINER ( 'PRM-ERRO' )
001570        CHANNEL   ( CANAL      )
001580        NOHANDLE
001590     END-EXEC
001600     IF  NOT (EIBRESP EQUAL DFHRESP(NORMAL)   OR
001610         EIBRESP EQUAL DFHRESP(CONTAINERERR) )
001620         PERFORM 999002-TRATA-CONDITION-CICS
001630     END-IF
001640     .
001650 120000-SAI.
001660     EXIT.
001670*-----------------------------------------------------------------
001680 130000-RECEBER-REQUISICAO SECTION.
001690*-----------------------------------------------------------------
001700     MOVE 0004                   TO MSG1-CD-SEQL-ERRO
001710
001720     MOVE ZEROS                    TO QT-CCT-LIDO
001730     MOVE LENGTH OF SGTSP009-RQSC  TO QT-CCT-EPRO

           SET  GET-CONTAINER               TO TRUE
           SET  ERRO-CONTAINER              TO TRUE

001740     EXEC CICS GET
001750        CONTAINER ( 'SGTSP009-RQSC' )
001760        CHANNEL   ( CANAL      )
001770        NODATA
001780        FLENGTH ( QT-CCT-LIDO )
001790     END-EXEC
001800
001810     IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
001820         SET   ERRO-TAMANHO-AREA   TO  TRUE
001920         PERFORM 999000-GRAVA-ERRO-E-ENCERRA
001930     END-IF
001940
001950     EXEC CICS GET
001960        CONTAINER ( 'SGTSP009-RQSC' )
001970        CHANNEL   ( CANAL      )
001980        FLENGTH ( LENGTH OF SGTSP009-RQSC )
001990        INTO    ( SGTSP009-RQSC )
002000     END-EXEC
002010
           SET SEM-ERRO                     TO TRUE
002020     .
002030 130000-SAI.
002040     EXIT.
002050
001710*-----------------------------------------------------------------
001720 140000-VALIDA-PARAMETRO SECTION.
001730*-----------------------------------------------------------------
001740     MOVE 0005                   TO MSG1-CD-SEQL-ERRO
001750
001760     IF  SGTSP009-CD-ITEM-MOD-ORD-ENTD      < ZEROS
001770         SET  ERRO-PROGRAMADO    TO TRUE
001780         MOVE 0983                TO MSG1-CD-ERRO-TS
001800         MOVE 'Código do item o modelo de ordem inválido.'
                                       TO MSG1-TX-ERRO-TS
001820         PERFORM 999000-GRAVA-ERRO-E-ENCERRA
001830     END-IF
001840
002020     .
002030 140000-SAI.
002040     EXIT.
002060*-----------------------------------------------------------------
002070 200000-SOLICITAR-LISTA          SECTION.
002080*-----------------------------------------------------------------
002090     MOVE 0006                   TO MSG1-CD-SEQL-ERRO
002100
002110     INITIALIZE REG-WS
002120     MOVE 'N'                    TO SGTSP009-IN-CNTO-PSQ
002130
002140     PERFORM 970101-ABRE-CURSOR-PSQ
002150     PERFORM 970102-LE-CURSOR-PSQ
002160     PERFORM 970103-FECHA-CURSOR-PSQ
002170
002180
002190     .
002200 200000-SAI.
002210     EXIT.
002220*-----------------------------------------------------------------
002230 300000-PROCESSA-PESQUISA SECTION.
002240*-----------------------------------------------------------------
002250     MOVE 0007                   TO MSG1-CD-SEQL-ERRO
002260
002270     MOVE CD-ITEM-MOD-ORD(SGTSP009-IC-TAB) TO
002280               SGTSP009-CD-ITEM-MOD-ORD(SGTSP009-IC-TAB)
002290     MOVE TX-ITEM-MOD-ORD(SGTSP009-IC-TAB) TO
002300               SGTSP009-TX-ITEM-MOD-ORD(SGTSP009-IC-TAB)
002310
002320     .
002330 300000-SAI.
002340     EXIT.
002350*-----------------------------------------------------------------
002360 400000-POSTA-RESPOSTA SECTION.
002370*-----------------------------------------------------------------
002380     MOVE 0008                   TO MSG1-CD-SEQL-ERRO
002390     EXEC CICS PUT
002400        CONTAINER ( 'SGTSP009-RPST' )
002410        CHANNEL   ( CANAL      )
002420        FLENGTH   ( LENGTH OF SGTSP009-RPST )
002430        FROM      ( SGTSP009-RPST )
002440     END-EXEC
002450
002460     .
002470 400000-SAI.
002480     EXIT.
002490*-----------------------------------------------------------------
002500 910000-VERIFICA-FIM-LISTA       SECTION.
002510*-----------------------------------------------------------------
002520     MOVE 0009                   TO MSG1-CD-SEQL-ERRO
002530
002540     MOVE SQLERRD (3)            TO QT-REG-SELD
002550
002560     IF  QT-REG-SELD             EQUAL 101
002570         SET SGTSP009-TEM-MAIS-REGISTRO TO TRUE
002580         MOVE 100                TO QT-REG-SELD
002590     END-IF
002600
002610     MOVE QT-REG-SELD            TO SGTSP009-QT-OCR-TAB
002620
002630     .
002640 910000-SAI.
002650     EXIT.
002660*-----------------------------------------------------------------
002670 970101-ABRE-CURSOR-PSQ     SECTION.
002680*-----------------------------------------------------------------
002690     MOVE 0010                   TO MSG1-CD-SEQL-ERRO
002700     EXEC SQL
002710          OPEN CURSOR-PSQ
002720     END-EXEC
002730     MOVE 'S'                    TO IN-CURSOR-PSQ-ABTO
002740     .
002750 970101-SAI.
002760     EXIT.
002770*-----------------------------------------------------------------
002780 970102-LE-CURSOR-PSQ       SECTION.
002790*-----------------------------------------------------------------
002800     MOVE 0011                   TO MSG1-CD-SEQL-ERRO
002810
002820     EXEC SQL
002830          FETCH NEXT ROWSET
002840                CURSOR-PSQ  FOR 0101 ROWS
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
002960 970103-FECHA-CURSOR-PSQ    SECTION.
002970*-----------------------------------------------------------------
002980     MOVE 0012                   TO MSG1-CD-SEQL-ERRO
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
003110        PERFORM 970103-FECHA-CURSOR-PSQ
003120     END-IF

003130     IF  MSG1-CD-ERRO-TS         EQUAL  ZEROS
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

           MOVE 'SGTSP009-CD-ITEM-MOD-ORD-ENTD'
                                       TO MSGKW999-NM-VRV-MNT
002770     MOVE  SGTSP009-CD-ITEM-MOD-ORD-ENTD
                                       TO MSGKW999-VL-VRV-MNT
002780     PERFORM 999003-ADICIONA-VRV-MNT

003140* --> grava o erro no msg
003150     PERFORM MSG1-NTF-MSG
003160     MOVE MSG1-CD-ERRO-TS             TO IIBKERRO-CD-ERRO
003170     MOVE MSG1-TX-ERRO-TS             TO IIBKERRO-TX-ERRO
003180
003190     EXEC CICS PUT
003200        CONTAINER ( 'PRM-ERRO' )
003210        CHANNEL   ( CANAL      )
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
-INC HLPKP999                                                          0000337%
003380* --------------------------------------*
003390* 999010-TRATA-ERRO-DB2          SECTION.
-INC HLPKP997                                                           0000340%
003410* --------------------------------------*
003420* MSG1-NTF-MSG                   SECTION.
-INC MSGKPMG1                                                           0000343%
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