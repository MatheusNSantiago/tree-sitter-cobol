000010*-----------------------------------------------------------------
000020* PROGRAMA..: SGTS0098
000030* ANALISTA..: F0175702 ADRIANO SEIJI NAKAMURA
000040* AUTOR.....: F0175702 ADRIANO SEIJI NAKAMURA
000050* DATA......: 28/11/2016
000060* OBJETIVO..: Alterar Nota Tecnica de uma Operao Estruturada
000080* COMPILACAO: COBOL/CICS
000090*-----------------------------------------------------------------
000090* VRS002 02/09/2019 F8785286 DE 2016/145195 - Campo
000090*            SGTS0098-CD-UOR-ITVT-NOTA não é mais obrigatório
000090*            (a pedido gestor).
000090* VRS001 14/02/2018 F8785286 DE 2016/145195 - Acerto dos códigos
000090*          das mensagens para mensageria.
000100* * VRS001 28/11/2016 F0175702 DE 2016/145195 - IMPLANTACAO
000110*-----------------------------------------------------------------
000120
000130*-----------------------------------------------------------------
000140 IDENTIFICATION DIVISION.
000150*----------------------------------------------------------------*
000160 PROGRAM-ID. SGTS0098.
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
000330 WORKING-STORAGE                 SECTION.
000340*----------------------------------------------------------------*
000350
000360 77  CTE-INICIO-SS                 PIC  X(040)  VALUE
000370              '*** S.S. COMECA AQUI ***'.
000380
000390 77  CTE-PROG                      PIC  X(018)  VALUE
000400              '*** SGTS0098 ***'.
000410 77  CTE-VERS                      PIC  X(006)  VALUE 'VRS002'.
000420
000430* Modulos acionados
000440 77  IIBP2000                      PIC  X(008) VALUE 'IIBP2000'.
000440 77  SGTSA055                      PIC  X(008) VALUE 'SGTSA055'.
000440 77  SGTSL056                      PIC  X(008) VALUE 'SGTSL056'.
000440 77  SGTSE056                      PIC  X(008) VALUE 'SGTSE056'.
000440 77  SGTSI056                      PIC  X(008) VALUE 'SGTSI056'.
000440 77  SGTSPE14                      PIC  X(008) VALUE 'SGTSPE14'.
000440 77  SGTSI059                      PIC  X(008) VALUE 'SGTSI059'.
000440 77  SBTIMEDT                      PIC  X(008) VALUE 'SBTIMEDT'.
000470*----------------------------------------------------------------*
000480 LOCAL-STORAGE                   SECTION.
000490*----------------------------------------------------------------*
000500
000510* Areas de trabalho
000500
000440* Auxiliares de Data para validação
000440 01 DT-AUX-DMA                     PIC  X(10) VALUE ' '.
000640 01 DT-AUX-DMA-R                   REDEFINES
000650       DT-AUX-DMA                    .
000660    02 DIA                         PIC  9(2).
000670    02 BARRA-1                     PIC  X(1).
000680    02 MES                         PIC  9(2).
000690    02 BARRA-2                     PIC  X(1).
000700    02 ANO                         PIC  9(4).
000500
000440 01 GDA-DT-APVC-DMA                PIC  X(10) VALUE ' '.
000440 01 GDA-DT-EMS-DMA                 PIC  X(10) VALUE ' '.
000500
000440* Data AnoMesDia Aprov
000440 01 DT-APVC-INFD                   PIC  X(8) VALUE ' '.
000730 01 DT-APVC-INFD-R                 REDEFINES DT-APVC-INFD.
000740    02 ANO                         PIC  X(4).
000750    02 MES                         PIC  X(2).
000760    02 DIA                         PIC  X(2).
000500
000440 01 DT-NOTA-INFD                   PIC  X(8) VALUE ' '.
000730 01 DT-NOTA-INFD-R                 REDEFINES DT-NOTA-INFD.
000740    02 ANO                         PIC  X(4).
000750    02 MES                         PIC  X(2).
000760    02 DIA                         PIC  X(2).
000500
000440 01 DT-AUX-08                      PIC  X(8) VALUE ' '.
000730 01 DT-AUX-08-R                    REDEFINES DT-AUX-08.
000740    02 DIA                         PIC  X(2).
000750    02 MES                         PIC  X(2).
000760    02 ANO                         PIC  X(4).
001920*-----------------------------------------------------------------
001930* AREA PARA RECUPERAR TIMESTAMP E DATA
001940*-----------------------------------------------------------------
001950 01  DADOS-SBTIMEDT.
001960     03  WK-CD-FUC              PIC X(03)      VALUE 'F01'.
001970     03  WK-AREA-TRB.
001980         05  WK-AREA-TRB-TAM    PIC S9(9) COMP VALUE 1024.
001990         05  WK-AREA-TRB-DADOS  PIC  X(1024)   VALUE ' '.
002000*        X'0002' - DD.MM.AAAA-HH:MM:SS.CCCCCCXXXX
002010     03  WK-FMT-TS              PIC S9(4) COMP VALUE 1.
002020*         X'001A'(26)
002030     03  WK-TAM-TS              PIC S9(4) COMP VALUE 30.
002040     03  WK-TS-FMTD.
002050         05  ANO                      PIC  9(4).
002060         05  FILLER                   PIC  X(1).
002070         05  MES                      PIC  9(2).
002080         05  FILLER                   PIC  X(1).
002090         05  DIA                      PIC  9(2).
002100         05  FILLER                   PIC  X(1).
002110         05  HORA                     PIC  9(2).
002120         05  FILLER                   PIC  X(1).
002130         05  MINUTO                   PIC  9(2).
002140         05  FILLER                   PIC  X(1).
002150         05  SEGUNDO                  PIC  9(2).
002160         05  FILLER                   PIC  X(1).
002170         05  MICRO-SEGUNDO            PIC  9(6).
002180         05  FILLER                   PIC  X(4).
000440
000440 01 IC-TAB                            PIC S9(4) COMP-5.
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
000680* Book do container de requisicao e resposta deste modulo
-INC SGTK0098
000720* Alterar Nota Tecnica de Operacao Estruturada
-INC SGTKA055
000560*Excluir da tabela UOR_ITVT_NOTA_TCN
-INC SGTKE056
000560*Listar da tabela UOR_ITVT_NOTA_TCN
-INC SGTKL056
000720* Inclusao da tabela UOR_ITVT_NOTA_TCN
-INC SGTKI056
000560
-INC SGTKI059
000560
000560 01 SGTSPE14-DADOS.
-INC SGTKPE14
000560
000800* Book do Container de Erro
-INC IIBKERRO
000820* Book do container de Servicos (para acionamento de servicos)
-INC IIBKSRVC
001400* Book dos dados do usurio
-INC IIBKOPRD
000840
000850 77  CTE-FINAL-SS                  PIC X(40)      VALUE
000860              '*** S.S. TERMINA AQUI ***'.
000880*----------------------------------------------------------------*
000890 PROCEDURE DIVISION.
000900*----------------------------------------------------------------*
000910
000920*----------------------------------------------------------------*
000930 000000-ROTINA-PRINCIPAL         SECTION.
000940*----------------------------------------------------------------*
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
001190     MOVE 'SGTS0098'             TO NM-PGM
001200
001210*    Limpa a resposta e erro antes de executar o modulo
001220     PERFORM 983000-EXCLUIR-RESPOSTA-ERRO
001230
001240     PERFORM 971000-LER-REQUISICAO
000560     PERFORM 974000-LER-DADOS-OPRD
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
001380     IF  SGTS0098-NR-NOTA-TCN-OPR  <= ZEROS
001390         SET ERRO-PROGRAMADO     TO TRUE
001400         MOVE 0181               TO MSG1-CD-ERRO-TS
001410         MOVE 'Numero Nota tecnica operacao invalido.'
001420                                 TO MSG1-TX-ERRO-TS
001430         SET MNT-VRV             TO TRUE
001440         PERFORM 999000-ERRO
001450     END-IF
000560
000560     IF  SGTS0098-CD-UOR-DRTA-OGNL  <= ZEROS
001390         SET ERRO-PROGRAMADO     TO TRUE
001400         MOVE 0304               TO MSG1-CD-ERRO-TS
001410         MOVE 'Codigo UOR diretoria original inválido.'
001420                                 TO MSG1-TX-ERRO-TS
001430         SET MNT-VRV             TO TRUE
001440         PERFORM 999000-ERRO
001450     END-IF
000500
001450     IF  SGTS0098-CD-REF-NOTA-TCN EQUAL SPACES
001390         SET ERRO-PROGRAMADO     TO TRUE
001400         MOVE 0284               TO MSG1-CD-ERRO-TS
001410         MOVE 'Codigo referencia Nota tecnica inválido.'
001420                                 TO MSG1-TX-ERRO-TS
001430         SET MNT-VRV             TO TRUE
001440         PERFORM 999000-ERRO
001450     END-IF
000500
001450     IF  SGTS0098-DT-EMS-NOTA EQUAL SPACES
001390         SET ERRO-PROGRAMADO     TO TRUE
001400         MOVE 0305               TO MSG1-CD-ERRO-TS
001410         MOVE 'Data emissao Nota inválido.'
001420                                 TO MSG1-TX-ERRO-TS
001430         SET MNT-VRV             TO TRUE
001440         PERFORM 999000-ERRO
001450     ELSE
001450         MOVE SGTS0098-DT-EMS-NOTA TO DT-AUX-08
001450         MOVE CORR DT-AUX-08-R   TO DT-AUX-DMA-R
001450         MOVE '.'                TO BARRA-1 BARRA-2
001450         MOVE DT-AUX-DMA         TO GDA-DT-EMS-DMA
001450     END-IF
000500
001450     IF  SGTS0098-DT-APVC-NOTA EQUAL SPACES
001390         SET ERRO-PROGRAMADO     TO TRUE
001400         MOVE 0306               TO MSG1-CD-ERRO-TS
001410         MOVE 'Data aprovação Nota inválido.'
001420                                 TO MSG1-TX-ERRO-TS
001430         SET MNT-VRV             TO TRUE
001440         PERFORM 999000-ERRO
001450     ELSE
001450         MOVE SGTS0098-DT-APVC-NOTA TO DT-AUX-08
001450         MOVE CORR DT-AUX-08-R   TO DT-AUX-DMA-R
001450         MOVE '.'                TO BARRA-1 BARRA-2
001450         MOVE DT-AUX-DMA         TO GDA-DT-APVC-DMA
001450     END-IF
000500
001450     IF  SGTS0098-CD-ETR-DCS   <= ZEROS
001390         SET ERRO-PROGRAMADO     TO TRUE
001400         MOVE 0307               TO MSG1-CD-ERRO-TS
001410         MOVE 'Codigo estrutura decisoria inválido.'
001420                                 TO MSG1-TX-ERRO-TS
001430         SET MNT-VRV             TO TRUE
001440         PERFORM 999000-ERRO
001450     END-IF
000500
001450     IF  SGTS0098-CD-TIP-DOC-TCN   <= ZEROS
001390         SET ERRO-PROGRAMADO     TO TRUE
001400         MOVE 0308               TO MSG1-CD-ERRO-TS
001410         MOVE 'Codigo tipo documento Nota inválido.'
001420                                 TO MSG1-TX-ERRO-TS
001430         SET MNT-VRV             TO TRUE
001440         PERFORM 999000-ERRO
001450     END-IF
000500
001450     IF  SGTS0098-QT-OCR-TAB   < ZEROS
001390         SET ERRO-PROGRAMADO     TO TRUE
001400         MOVE 0309               TO MSG1-CD-ERRO-TS
001410         MOVE 'Quantidade UOR interveniente inválido.'
001420                                 TO MSG1-TX-ERRO-TS
001430         SET MNT-VRV             TO TRUE
001440         PERFORM 999000-ERRO
001450     END-IF
000500
001450     PERFORM VARYING IC-TAB FROM 1 BY 1
001450       UNTIL IC-TAB GREATER SGTS0098-QT-OCR-TAB
001450       IF  SGTS0098-CD-UOR-ITVT-NOTA(IC-TAB)   <= ZEROS
001390           SET ERRO-PROGRAMADO     TO TRUE
001400           MOVE 0292               TO MSG1-CD-ERRO-TS
001410           MOVE 'Codigo UOR interveniente Nota inválido.'
001420                                 TO MSG1-TX-ERRO-TS
001430           SET MNT-VRV             TO TRUE
001450           MOVE 'IC-TAB' TO MSGKW999-NM-VRV-MNT
009350           MOVE  IC-TAB  TO MSGKW999-VL-VRV-MNT
009370           PERFORM 999003-ADICIONA-VRV-MNT
001440           PERFORM 999000-ERRO
001450       END-IF
001450     END-PERFORM
000500
001450* Validar se a Data Aprovação é maior que Data Nota
001450     MOVE SGTS0098-DT-EMS-NOTA   TO DT-AUX-DMA
001450     MOVE CORR DT-AUX-DMA-R      TO DT-NOTA-INFD-R
001450     MOVE SGTS0098-DT-APVC-NOTA  TO DT-AUX-DMA
001450     MOVE CORR DT-AUX-DMA-R      TO DT-APVC-INFD-R
000500
001450     IF  DT-APVC-INFD >= DT-NOTA-INFD
001450         CONTINUE
001450     ELSE
001450         SET ERRO-PROGRAMADO     TO TRUE
001400         MOVE 0310               TO MSG1-CD-ERRO-TS
001410         MOVE 'Data Aprovação deve ser igual ou maior Data Nota.'
001420                                 TO MSG1-TX-ERRO-TS
001430         SET MNT-VRV             TO TRUE
001440         PERFORM 999000-ERRO
001450     END-IF
000500
001450     PERFORM 140000-BUSCA-TS-DT
001470     .
001480 110000-SAI.
001490     EXIT.
001450*-----------------------------------------------------------------
001450 140000-BUSCA-TS-DT SECTION.
001450*-----------------------------------------------------------------
001450     MOVE 0003                   TO MSG1-CD-SEQL-ERRO
001450
001450     CALL SBTIMEDT         USING WK-CD-FUC
001450         WK-AREA-TRB
012540         WK-FMT-TS
012550         WK-TAM-TS
012560         WK-TS-FMTD
016840
001450     IF  RETURN-CODE             NOT EQUAL ZEROS
001450         SET  ERRO-PROGRAMA-ACIONADO
001450                                 TO  TRUE
001450         MOVE RETURN-CODE        TO  MSG1-CD-RTN-PGM-ACND
001450         MOVE 'SBTIMEDT'         TO  MSG1-NM-PGM-ACND
001450                                     NM-PGM
001450         MOVE 0050               TO  MSG1-CD-ERRO-TS
001450         MOVE 'Erro ao obter data e hora atual.'
001450                                 TO  MSG1-TX-ERRO-TS
001450         PERFORM 969000-MONITORAR-VARIAVEIS
001450         PERFORM 999000-GRAVA-ERRO-E-ENCERRA
001450     END-IF
001450     .
001450 140000-SAI.
001450     EXIT.
001510*----------------------------------------------------------------*
001520 200000-PROCESSAR                SECTION.
001530*----------------------------------------------------------------*
001540     MOVE 0004                   TO MSG1-CD-SEQL-ERRO
001450
001450     PERFORM 210000-VERIFICAR-VCL-NOTA-TCN
001450
001450     IF SGTPE014-QT-NOTA-TCN = ZEROS
001450        MOVE SGTS0098-CD-ARQ-OPR-ETRD  TO
001450                                  SGTS0098-CD-ARQ-OPR-ETRD-S
001450     ELSE
001450        PERFORM 230000-INCLUI-ARQUIVO
001450        MOVE SGTSI059-CD-ARQ-OPR  TO
001450                                  SGTS0098-CD-ARQ-OPR-ETRD-S
001450     END-IF
001450
001450     PERFORM 220000-ALTERAR-NOTA-TECN
001450
001450     PERFORM 980000-GRAVAR-CONTAINER-RPST
001640     .
001650 200000-SAI.
001660     EXIT.
001450*----------------------------------------------------------------*
001450 210000-VERIFICAR-VCL-NOTA-TCN        SECTION.
001450*----------------------------------------------------------------*
001450     MOVE 0005                   TO MSG1-CD-SEQL-ERRO
001450
001450     INITIALIZE         SGTSPE14-DADOS
001450
001450     MOVE SGTSPE14              TO NM-PGM
001450
001450*    Movimentar os campos de entrada
001450     MOVE SGTS0098-NR-NOTA-TCN-OPR   TO SGTSPE14-NR-NOTA-TCN-OPR
001450     MOVE SGTS0098-CD-ARQ-OPR-ETRD   TO SGTSPE14-CD-ARQ-OPR
001450
001450     PERFORM 900000-EXECUTAR-MODULO
001450     .
001450 210000-SAI.
001450     EXIT.
001680*----------------------------------------------------------------*
001450 220000-ALTERAR-NOTA-TECN       SECTION.
001450*----------------------------------------------------------------*
001450* Altera a Nota Tecnica e demais dados no SGT, gravar Tab. Interv.
001450*----------------------------------------------------------------*
001450     MOVE 0006                   TO MSG1-CD-SEQL-ERRO
000500
001730*    Movimentar o nome do programa a ser executado
001450     MOVE SGTSA055               TO NM-PGM
001750
001450     MOVE  SGTS0098-NR-NOTA-TCN-OPR TO SGTSA055-NR-NOTA-TCN-OPR
001450     MOVE  SGTS0098-CD-ARQ-OPR-ETRD-S
001450                                 TO SGTSA055-CD-ARQ-OPR-ETRD
001450     MOVE SGTS0098-CD-UOR-DRTA-OGNL
001450                                 TO SGTSA055-CD-UOR-DRTA-OGNL
001780     MOVE SGTS0098-CD-REF-NOTA-TCN
001450                                 TO SGTSA055-CD-REF-NOTA-TCN
001450     MOVE GDA-DT-EMS-DMA         TO SGTSA055-DT-EMS-NOTA
001450     MOVE GDA-DT-APVC-DMA        TO SGTSA055-DT-APVC-NOTA
001450     MOVE SGTS0098-CD-ETR-DCS    TO SGTSA055-CD-ETR-DCS
001450     MOVE SGTS0098-CD-TIP-DOC-TCN
001450                                 TO SGTSA055-CD-TIP-DOC-TCN
001450     MOVE KOPRD-CD-OPRD          TO SGTSA055-CD-USU-RSP-ULT-ATL
001450     MOVE WK-TS-FMTD             TO SGTSA055-TS-ULT-ATL
001450
001450     PERFORM 900000-EXECUTAR-MODULO
001450
001450     PERFORM 221000-LISTAR-DIRETORIAS-ITVT
001450
001450     PERFORM VARYING IC-TAB FROM 1 BY 1
001450         UNTIL IC-TAB GREATER SGTS0098-QT-OCR-TAB
001450         MOVE SGTSI056           TO NM-PGM
001450         MOVE SGTS0098-NR-NOTA-TCN-OPR
001450                                 TO SGTSI056-NR-NOTA-TCN-OPR
001400         MOVE SGTS0098-CD-UOR-ITVT-NOTA(IC-TAB)
001410                                 TO SGTSI056-CD-UOR-ITVT-NOTA
001430         PERFORM 900000-EXECUTAR-MODULO
001450     END-PERFORM
001450     .
001650 220000-SAI.
001660     EXIT.
001680*----------------------------------------------------------------*
001450 221000-LISTAR-DIRETORIAS-ITVT     SECTION.
001450*----------------------------------------------------------------*
001450* Listar as UORs e excluir, antes de incluir as UORS editadas.
001450*----------------------------------------------------------------*
001450     MOVE 0007                   TO MSG1-CD-SEQL-ERRO
000500
001730*    Movimentar o nome do programa a ser executado
001450     MOVE SGTSL056               TO NM-PGM
001750
001450     MOVE  SGTS0098-NR-NOTA-TCN-OPR TO SGTSL056-E-NR-NOTA-TCN-OPR
001450     MOVE  1                     TO SGTSL056-CD-TIP-SRVC
001450     MOVE ZEROS                  TO SGTSL056-E-CD-UOR-ITVT-NOTA
001450
001450     PERFORM 900000-EXECUTAR-MODULO
001450
001450     PERFORM VARYING SGTSL056-IC-TAB FROM 1 BY 1
001450         UNTIL SGTSL056-IC-TAB GREATER SGTSL056-QT-OCR-TAB
001450*--> Efetua excluso das UORs
001450         MOVE SGTSE056           TO NM-PGM
001450         MOVE SGTS0098-NR-NOTA-TCN-OPR
001450                                 TO SGTSE056-NR-NOTA-TCN-OPR
001400         MOVE SGTSL056-CD-UOR-ITVT-NOTA(SGTSL056-IC-TAB)
001410                                 TO SGTSE056-CD-UOR-ITVT-NOTA
001430         PERFORM 900000-EXECUTAR-MODULO
001450     END-PERFORM
001450     .
001650 221000-SAI.
001660     EXIT.
001660*----------------------------------------------------------------*
001660 230000-INCLUI-ARQUIVO       SECTION.
001660*----------------------------------------------------------------*
001660     MOVE 0008                   TO MSG1-CD-SEQL-ERRO
001660
001660*    Movimentar o nome do programa a ser executado
001660     MOVE SGTSI059                    TO NM-PGM
001660
001660     MOVE ZEROS                       TO SGTSI059-CD-ARQ-OPR-ETRD
001660     MOVE 2000                        TO SGTSI059-CD-TIP-ARQ
001660     MOVE 'CURRENT'                   TO SGTSI059-TS-INCL-ARQ-OPR
001660
001660     PERFORM 900000-EXECUTAR-MODULO
001660     .
001660 230000-SAI.
001660     EXIT.
002720*----------------------------------------------------------------*
002740*  SECTION DA FAIXA DE 900000 A 969900 PARA DEMAIS ROTINAS       *
002760*----------------------------------------------------------------*
002770* Executa um modulo implementado com CONTAINER                   *
002780* Nao incluir nessa section os modulos chamados via COMMAREA     *
002790*                                                                *
002800*----------------------------------------------------------------*
002810 900000-EXECUTAR-MODULO          SECTION.
002820*----------------------------------------------------------------*
002850*    Grava o container de requisicao do modulo a executar
002860     PERFORM 982000-GRAVAR-CONTAINER-RQSC
002870
002880*    Limpa a resposta e erro antes de executar o modulo
002890     PERFORM 983000-EXCLUIR-RESPOSTA-ERRO
002900
002910*    Nao ha captura automatica de erro CICS por usar RESP e RESP2
002920     EVALUATE NM-PGM
001660         WHEN SGTSI059
002940             EXEC CICS LINK
002950                       PROGRAM ( SGTSI059  )
002960                       CHANNEL ( NM-CNL    )
002970                       RESP    ( MSG1-RESP1)
002980                       RESP2   ( MSG1-RESP2)
002990             END-EXEC
001660
001660         WHEN SGTSPE14
002940             EXEC CICS LINK
002950                       PROGRAM ( SGTSPE14  )
002960                       CHANNEL ( NM-CNL    )
002970                       RESP    ( MSG1-RESP1)
002980                       RESP2   ( MSG1-RESP2)
002990             END-EXEC
000500
001660         WHEN SGTSA055
002940             EXEC CICS LINK
002950                       PROGRAM ( SGTSA055  )
002960                       CHANNEL ( NM-CNL    )
002970                       RESP    ( MSG1-RESP1)
002980                       RESP2   ( MSG1-RESP2)
002990             END-EXEC
000500
003000         WHEN SGTSI056
002940             EXEC CICS LINK
002950                       PROGRAM ( SGTSI056  )
002960                       CHANNEL ( NM-CNL    )
002970                       RESP    ( MSG1-RESP1)
002980                       RESP2   ( MSG1-RESP2)
002990             END-EXEC
000500
001660         WHEN SGTSE056
002940             EXEC CICS LINK
002950                       PROGRAM ( SGTSE056  )
002960                       CHANNEL ( NM-CNL    )
002970                       RESP    ( MSG1-RESP1)
002980                       RESP2   ( MSG1-RESP2)
002990             END-EXEC
000500
001660         WHEN SGTSL056
002940             EXEC CICS LINK
002950                       PROGRAM ( SGTSL056  )
002960                       CHANNEL ( NM-CNL    )
002970                       RESP    ( MSG1-RESP1)
002980                       RESP2   ( MSG1-RESP2)
002990             END-EXEC
000500
001660         WHEN OTHER
003010              SET   ERRO-PROGRAMADO
003010                                 TO  TRUE
003010              MOVE  9006          TO  MSG1-CD-ERRO-TS
003010              STRING  'Não definida a execução do módulo'
003010                      NM-PGM '.'
003010                      DELIMITED BY SIZE
003010                      INTO            MSG1-TX-ERRO-TS
003010              PERFORM 999000-ERRO
003050     END-EVALUATE
003060*
003070     EVALUATE TRUE
003080         WHEN MSG1-RESP1        EQUAL DFHRESP(NORMAL)
003090              CONTINUE
000500
003100         WHEN OTHER
003110              SET   ERRO-CICS    TO  TRUE
003110              MOVE  3004         TO  MSG1-CD-ERRO-TS
003110              STRING
003110              'Erro  ao acionar o módulo  ' NM-PGM  ', '
003110              'RESP  = '  MSG1-RESP1  ', '
003110              'RESP2 = '  MSG1-RESP2
003110              DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
003110              PERFORM 999000-ERRO
003190     END-EVALUATE
003200
003210*    Efetua a leitura do Container de Erro
003220     PERFORM 972000-LER-CONTAINER-DE-ERRO
003230
003240*    Efetua a leitura do Container de Resposta
003240     IF  NM-PGM  NOT EQUAL  'SGTSA055'
003250         PERFORM 973000-LER-RESPOSTA
003250     END-IF
003270     .
003280 900000-SAI.
003290     EXIT.
003790*----------------------------------------------------------------*
003800 969000-MONITORAR-VARIAVEIS      SECTION.
003810*----------------------------------------------------------------*
003820* Monitora as variaveis dos modulos chamados
003830*----------------------------------------------------------------*
003860     EVALUATE NM-PGM
001660     WHEN SGTSPE14
001660          MOVE 'SGTSPE14-NR-NOTA-TCN-OPR'  TO MSGKW999-NM-VRV-MNT
001660          MOVE SGTSPE14-NR-NOTA-TCN-OPR    TO MSGKW999-VL-VRV-MNT
001660          PERFORM 999003-ADICIONA-VRV-MNT
001660
001660          MOVE 'SGTSPE14-CD-ARQ-OPR'       TO MSGKW999-NM-VRV-MNT
001660          MOVE SGTSPE14-CD-ARQ-OPR         TO MSGKW999-VL-VRV-MNT
001660          PERFORM 999003-ADICIONA-VRV-MNT
001660
001660     WHEN SGTSI059
001660          MOVE 'SGTSI059-CD-ARQ-OPR-ETRD'  TO MSGKW999-NM-VRV-MNT
001660          MOVE SGTSI059-CD-ARQ-OPR-ETRD    TO MSGKW999-VL-VRV-MNT
001660          PERFORM 999003-ADICIONA-VRV-MNT
001660
001660          MOVE 'SGTSI059-CD-TIP-ARQ'       TO MSGKW999-NM-VRV-MNT
001660          MOVE SGTSI059-CD-TIP-ARQ         TO MSGKW999-VL-VRV-MNT
001660          PERFORM 999003-ADICIONA-VRV-MNT
001660
001660          MOVE 'SGTSI059-TS-INCL-ARQ-OPR'  TO MSGKW999-NM-VRV-MNT
001660          MOVE SGTSI059-TS-INCL-ARQ-OPR    TO MSGKW999-VL-VRV-MNT
001660          PERFORM 999003-ADICIONA-VRV-MNT
001660
001660     WHEN SGTSA055
003880          MOVE 'SGTSA055-NR-NOTA-TCN-OPR'  TO MSGKW999-NM-VRV-MNT
003900          MOVE SGTSA055-NR-NOTA-TCN-OPR    TO MSGKW999-VL-VRV-MNT
003920          PERFORM 999003-ADICIONA-VRV-MNT
001660
001660          MOVE 'SGTSA055-CD-ARQ-OPR-ETRD'  TO MSGKW999-NM-VRV-MNT
003900          MOVE SGTSA055-CD-ARQ-OPR-ETRD    TO MSGKW999-VL-VRV-MNT
003920          PERFORM 999003-ADICIONA-VRV-MNT
001660
001660          MOVE 'SGTSA055-CD-UOR-DRTA-OGNL' TO MSGKW999-NM-VRV-MNT
003900          MOVE SGTSA055-CD-UOR-DRTA-OGNL   TO MSGKW999-VL-VRV-MNT
003920          PERFORM 999003-ADICIONA-VRV-MNT
001660
001660          MOVE 'SGTSA055-CD-REF-NOTA-TCN'  TO MSGKW999-NM-VRV-MNT
003900          MOVE SGTSA055-CD-REF-NOTA-TCN    TO MSGKW999-VL-VRV-MNT
003920          PERFORM 999003-ADICIONA-VRV-MNT
001660
001660          MOVE 'SGTSA055-DT-EMS-NOTA'      TO MSGKW999-NM-VRV-MNT
003900          MOVE SGTSA055-DT-EMS-NOTA        TO MSGKW999-VL-VRV-MNT
003920          PERFORM 999003-ADICIONA-VRV-MNT
001660
001660          MOVE 'SGTSA055-DT-APVC-NOTA'     TO MSGKW999-NM-VRV-MNT
003900          MOVE SGTSA055-DT-APVC-NOTA       TO MSGKW999-VL-VRV-MNT
003920          PERFORM 999003-ADICIONA-VRV-MNT
001660
001660          MOVE 'SGTSA055-CD-ETR-DCS'       TO MSGKW999-NM-VRV-MNT
003900          MOVE SGTSA055-CD-ETR-DCS         TO MSGKW999-VL-VRV-MNT
003920          PERFORM 999003-ADICIONA-VRV-MNT
001660
001660          MOVE 'SGTSA055-CD-TIP-DOC-TCN'   TO MSGKW999-NM-VRV-MNT
003900          MOVE SGTSA055-CD-TIP-DOC-TCN     TO MSGKW999-VL-VRV-MNT
003920          PERFORM 999003-ADICIONA-VRV-MNT
001660
001660          MOVE 'SGTSA055-CD-USU-RSP-ULT-ATL'
003890                                           TO MSGKW999-NM-VRV-MNT
003900          MOVE SGTSA055-CD-USU-RSP-ULT-ATL TO MSGKW999-VL-VRV-MNT
003920          PERFORM 999003-ADICIONA-VRV-MNT
001660
001660          MOVE 'SGTSA055-TS-ULT-ATL'       TO MSGKW999-NM-VRV-MNT
003900          MOVE SGTSA055-TS-ULT-ATL         TO MSGKW999-VL-VRV-MNT
003920          PERFORM 999003-ADICIONA-VRV-MNT
001660
001660     WHEN SGTSI056
003880          MOVE 'SGTSI056-NR-NOTA-TCN-OPR'  TO MSGKW999-NM-VRV-MNT
003900          MOVE SGTSI056-NR-NOTA-TCN-OPR    TO MSGKW999-VL-VRV-MNT
003920          PERFORM 999003-ADICIONA-VRV-MNT
001660
001660          MOVE 'SGTSI056-CD-UOR-ITVT-NOTA' TO MSGKW999-NM-VRV-MNT
003900          MOVE SGTSI056-CD-UOR-ITVT-NOTA   TO MSGKW999-VL-VRV-MNT
003920          PERFORM 999003-ADICIONA-VRV-MNT
001660
001660     WHEN SGTSE056
003880          MOVE 'SGTSE056-NR-NOTA-TCN-OPR'  TO MSGKW999-NM-VRV-MNT
003900          MOVE SGTSE056-NR-NOTA-TCN-OPR    TO MSGKW999-VL-VRV-MNT
003920          PERFORM 999003-ADICIONA-VRV-MNT
001660
001660          MOVE 'SGTSE056-CD-UOR-ITVT-NOTA' TO MSGKW999-NM-VRV-MNT
003900          MOVE SGTSE056-CD-UOR-ITVT-NOTA   TO MSGKW999-VL-VRV-MNT
003920          PERFORM 999003-ADICIONA-VRV-MNT
001660
001660     WHEN SGTSL056
003880          MOVE 'SGTSL056-E-NR-NOTA-TCN-OPR'
003890                                           TO MSGKW999-NM-VRV-MNT
003900          MOVE SGTSL056-E-NR-NOTA-TCN-OPR  TO MSGKW999-VL-VRV-MNT
003920          PERFORM 999003-ADICIONA-VRV-MNT
001660
001660          MOVE 'SGTSL056-CD-TIP-SRVC'      TO MSGKW999-NM-VRV-MNT
003900          MOVE SGTSL056-CD-TIP-SRVC        TO MSGKW999-VL-VRV-MNT
003920          PERFORM 999003-ADICIONA-VRV-MNT
001660
001660          MOVE 'SGTSL056-E-CD-UOR-ITVT-NOTA'
001660                                           TO MSGKW999-NM-VRV-MNT
003900          MOVE SGTSL056-E-CD-UOR-ITVT-NOTA TO MSGKW999-VL-VRV-MNT
003920          PERFORM 999003-ADICIONA-VRV-MNT
001660
003920     WHEN  SBTIMEDT
003920          MOVE  'WK-CD-FUC'                 TO MSGKW999-NM-VRV-MNT
003920          MOVE   WK-CD-FUC                  TO MSGKW999-VL-VRV-MNT
003920          PERFORM  999003-ADICIONA-VRV-MNT
003920
003920          MOVE  'WK-AREA-TRB'               TO MSGKW999-NM-VRV-MNT
003920          MOVE  WK-AREA-TRB                 TO MSGKW999-VL-VRV-MNT
003920          PERFORM  999003-ADICIONA-VRV-MNT
003920
003920          MOVE  'WK-FMT-TS'                 TO MSGKW999-NM-VRV-MNT
003920          MOVE  WK-FMT-TS                   TO MSGKW999-VL-VRV-MNT
003920          PERFORM  999003-ADICIONA-VRV-MNT
003920
003920          MOVE  'WK-TAM-TS'                 TO MSGKW999-NM-VRV-MNT
003920          MOVE  WK-TAM-TS                   TO MSGKW999-VL-VRV-MNT
003920          PERFORM  999003-ADICIONA-VRV-MNT
003920
003920          MOVE  'WK-TS-FMTD'          TO MSGKW999-NM-VRV-MNT
003920          MOVE  WK-TS-FMTD            TO MSGKW999-VL-VRV-MNT
003920          PERFORM  999003-ADICIONA-VRV-MNT
003920
001660     WHEN OTHER
004190          CONTINUE
004200     END-EVALUATE
004220     .
004230 969000-SAI.
004240     EXIT.
004260*----------------------------------------------------------------*
004270*                                                                *
004280* SECTION DA FAIXA DE 970000 A 979900 PARA LEITURA DE DADOS      *
004290*                                                                *
004300*----------------------------------------------------------------*
004310 970000-LER-CANAL                SECTION.
004320*    Obtem o canal corrente
004330*----------------------------------------------------------------*
004340     MOVE 0009                   TO MSG1-CD-SEQL-ERRO
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
004500*----------------------------------------------------------------*
004510 971000-LER-REQUISICAO           SECTION.
004520*----------------------------------------------------------------*
004540     MOVE 0010                   TO MSG1-CD-SEQL-ERRO
004550
004560*    Monta o nome do container de requisicao
004570     MOVE 'SGTS0098'             TO NM-PGM
004580     MOVE 'SGTS0098-RQSC'        TO NM-CTNR
004590
004600*    Recupera o tamanho do container de requisição
004610     PERFORM 973100-LER-CONTAINER-NODATA
004620
004630*    Obtem o tamanho esperado da requisicao
004640     MOVE LENGTH OF SGTS0098-RQSC TO QT-CCT-EPRO
004650
004660*    Valida tamanho esperado da requisicao X tamanho recebido
004670
004680*     IF  QT-CCT-LIDO             NOT EQUAL QT-CCT-EPRO
004690*         SET ERRO-TAMANHO-AREA   TO TRUE
004700*         PERFORM 999000-GRAVA-ERRO-E-ENCERRA
004710*     END-IF
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
004860               INTO      (SGTS0098-RQSC)
004870     END-EXEC
004880
004890*Ao chegar aqui, há certeza de que não houve erro algum
004900     SET  SEM-ERRO               TO TRUE
004920     .
004930 971000-SAI.
004940     EXIT.
004960*----------------------------------------------------------------*
004970 972000-LER-CONTAINER-DE-ERRO    SECTION.
004980*----------------------------------------------------------------*
004990*    A presença do container PRM-ERRO indica que houve erro de
005000*    integração, a ausência do container indica sucesso.
005010*----------------------------------------------------------------*
005050*    Le o container de erro dos modulos chamados
005060     SET GET-CONTAINER           TO TRUE
005070     MOVE 'PRM-ERRO'             TO NM-CTNR
005080     INITIALIZE IIBKERRO-PRM-ERRO
005090
005100*    Não há captura automática de erro CICS por usar RESP e RESP2
005110     EXEC CICS GET
005120               CONTAINER ( NM-CTNR                     )
005130               INTO      ( IIBKERRO-PRM-ERRO           )
005140               FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
005150               RESP      ( MSG1-RESP1                  )
005160               RESP2     ( MSG1-RESP2                  )
005170     END-EXEC
005180
005190     EVALUATE TRUE
005200*        Caso deseje personalizar ou não interromper o
005210*        processamento, alterar para não dar erro aqui SOMENTE
005220*        PARA O MODULO CHAMADO e tratar na section chamadora
005230         WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
005240              CONTINUE
001660
005250         WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
005250              SET   ERRO-PROGRAMA-ACIONADO TO  TRUE
005250              MOVE  NM-PGM                 TO  MSG1-NM-PGM-ACND
005250
005250              IF NM-PGM(1:3) EQUAL 'SGT'
005250                 MOVE  IIBKERRO-CD-ERRO  TO  MSG1-CD-RTN-PGM-ACND
005250                                             MSG1-CD-ERRO-TS
005250                 MOVE  IIBKERRO-TX-ERRO  TO  MSG1-TX-RTN-PGM-ACND
005250                                             MSG1-TX-ERRO-TS
005250              ELSE
005250                 MOVE 3004          TO MSG1-CD-ERRO-TS
005250                 STRING  'Erro ao acionar o módulo ' NM-PGM '.'
005250                      DELIMITED BY SIZE
005250                      INTO           MSG1-TX-ERRO-TS
005250                 MOVE IIBKERRO-TX-ERRO TO MSG1-TX-RTN-PGM-ACND
005250                 MOVE IIBKERRO-CD-ERRO TO MSG1-CD-RTN-PGM-ACND
005250              END-IF
005340
005350*             Monitorar variáveis do módulo chamado
005360              PERFORM 969000-MONITORAR-VARIAVEIS
005370              PERFORM 999000-ERRO
001660
005380         WHEN OTHER
005390              SET ERRO-CONTAINER  TO TRUE
005400              PERFORM 999002-TRATA-CONDITION-CICS
005410     END-EVALUATE
005420     .
005430 972000-SAI.
005440     EXIT.
005460*----------------------------------------------------------------*
005470 973000-LER-RESPOSTA             SECTION.
005480*----------------------------------------------------------------*
005610*    Monta o nome dos container de acordo com modulo a executar
005620     MOVE SPACES                 TO NM-CTNR
005630
005640*    Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
005650*    nome_modulo -RPST
005660*     EVALUATE NM-PGM
005670*         WHEN CAFS0053
005680*              MOVE 'PRM-DADOS-RPST'
005690*                                 TO NM-CTNR
005700*         WHEN OTHER
005710              MOVE NM-PGM        TO NM-CTNR(1:8)
005720              MOVE '-RPST'       TO NM-CTNR(9:5)
005730*     END-EVALUATE
005740
005750*    Recupera o tamanho do container de resposta
005760     PERFORM 973100-LER-CONTAINER-NODATA
005770
005780*Configura dados para leitura do container de resposta
005790     SET  GET-CONTAINER          TO TRUE
005800
005810*Por causa da captura automática de erro CICS, deve ser previsto
005820*o erro de container
005830     SET  ERRO-CONTAINER         TO TRUE
005850
005860*    Le a RESPOSTA de acordo com o programa que a gravou
005890
005900     EVALUATE NM-PGM
005910
005920*        Calcular a quantidade de caracteres esperado e limpar
005930*        a area de dados antes de executar o GET
005940*        Para matrizes com dados variaveis (clausula depending
005950*        on) nao ha como utilizar o comando INITIALIZE
005960
006130         WHEN SGTSI059
005990              MOVE LENGTH OF SGTSI059-RPST TO QT-CCT-EPRO
006000              EXEC CICS GET
006010                        CONTAINER (NM-CTNR          )
006020                        INTO      (SGTSI059-RPST    )
006030                        FLENGTH   (QT-CCT-LIDO      )
006040              END-EXEC
006040
006130         WHEN SGTSPE14
005990              MOVE LENGTH OF SGTSPE14-RPST TO QT-CCT-EPRO
006000              EXEC CICS GET
006010                        CONTAINER (NM-CTNR          )
006020                        INTO      (SGTSPE14-RPST    )
006030                        FLENGTH   (QT-CCT-LIDO      )
006040              END-EXEC
001660
006130         WHEN SGTSI056
005990              MOVE LENGTH OF SGTSI056-RPST TO QT-CCT-EPRO
006000              EXEC CICS GET
006010                        CONTAINER (NM-CTNR          )
006020                        INTO      (SGTSI056-RPST    )
006030                        FLENGTH   (QT-CCT-LIDO      )
006040              END-EXEC
001660
006040         WHEN SGTSE056
005990              MOVE LENGTH OF SGTSE056-RPST TO QT-CCT-EPRO
006000              EXEC CICS GET
006010                        CONTAINER (NM-CTNR          )
006020                        INTO      (SGTSE056-RPST    )
006030                        FLENGTH   (QT-CCT-LIDO      )
006040              END-EXEC
001660
006040         WHEN SGTSL056
005990              MOVE LENGTH OF SGTSL056-RPST TO QT-CCT-EPRO
006000              EXEC CICS GET
006010                        CONTAINER (NM-CTNR          )
006020                        INTO      (SGTSL056-RPST    )
006030                        FLENGTH   (QT-CCT-LIDO      )
006040              END-EXEC
001660
006040*        WHEN SGTSA055
005990*             MOVE LENGTH OF SGTSA055-RPST TO QT-CCT-EPRO
006000*             EXEC CICS GET
006010*                       CONTAINER (NM-CTNR          )
006020*                       INTO      (SGTSA055-RPST    )
006030*                       FLENGTH   (QT-CCT-LIDO      )
006040*             END-EXEC
001660*
006040         WHEN OTHER
006040              SET   ERRO-PROGRAMADO
006040                                 TO  TRUE
006040              MOVE  9003         TO  MSG1-CD-ERRO-TS
006040              STRING  'Erro na leitura da resposta do módulo '
006040                      NM-PGM '.'
006160              DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
006170              PERFORM 999000-ERRO
006180     END-EVALUATE
006190
006200*Ao chegar aqui, há certeza de que não houve erro algum
006210     SET  SEM-ERRO               TO TRUE
006230     .
006240 973000-SAI.
006250     EXIT.
006260*----------------------------------------------------------------*
006270 973100-LER-CONTAINER-NODATA     SECTION.
006280*----------------------------------------------------------------*
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
008160*----------------------------------------------------------------*
008170 974000-LER-DADOS-OPRD           SECTION.
008180*----------------------------------------------------------------*
008190     MOVE 0011                   TO MSG1-CD-SEQL-ERRO
008200
008210*Configura dados para leitura do container de requisio
008220     SET  GET-CONTAINER          TO TRUE
008230     MOVE 'PRM-INF-OPRD'         TO NM-CTNR
008240
008250*Por causa da captura automtica de erro CICS, deve ser previsto
008260*o erro de container
008270     SET  ERRO-CONTAINER         TO TRUE
008280
008290     INITIALIZE PRM-INF-OPRD
008300     EXEC CICS GET
008310          CONTAINER ( NM-CTNR                )
008320          INTO      ( PRM-INF-OPRD           )
008330          FLENGTH   ( LENGTH OF PRM-INF-OPRD )
008340     END-EXEC.
008350
008360*    Ao chegar aqui, h certeza de que no houve erro algum
008370     SET  SEM-ERRO               TO TRUE
008380     .
008390 974000-SAI.
008400     EXIT.
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
006630     MOVE 0012                   TO MSG1-CD-SEQL-ERRO
006640
006650*    Monta o nome do container de resposta
006660     MOVE 'SGTS0098-RPST'        TO NM-CTNR
006670     SET PUT-CONTAINER           TO TRUE
006680
006690*Por causa da captura automática de erro CICS, deve ser previsto
006700*o erro de container
006710     SET  ERRO-CONTAINER         TO TRUE
006720
006730     EXEC CICS PUT
006740               CONTAINER (NM-CTNR                    )
006750               FROM      (SGTS0098-RPST          )
006760               FLENGTH   (LENGTH OF SGTS0098-RPST)
006770     END-EXEC
006780
006790*Ao chegar aqui, há certeza de que não houve erro algum
006800     SET  SEM-ERRO               TO TRUE
006810     .
006820 980000-SAI.
006830     EXIT.
007190*----------------------------------------------------------------*
007200 982000-GRAVAR-CONTAINER-RQSC    SECTION.
007210*----------------------------------------------------------------*
007330
007340*    Monta o nome dos container de acordo com modulo a executar
007350     MOVE SPACES                 TO NM-CTNR
007360     SET PUT-CONTAINER           TO TRUE
007370
007380*    Quando modulos via IIB, fixo nome PRM-DADOS-RPST, senao
007390*    nome_modulo -RPST
007400*    EVALUATE NM-PGM
007410*         WHEN CAFS0053
007420*              MOVE 'PRM-DADOS-RQSC'
007430*                                 TO NM-CTNR
007440*         WHEN OTHER
007450              MOVE NM-PGM        TO NM-CTNR(1:8)
007460              MOVE '-RQSC'       TO NM-CTNR(9:5)
007470*     END-EVALUATE
007480
007490*Por causa da captura automática de erro CICS, deve ser previsto
007500*o erro de container
007510     SET  ERRO-CONTAINER         TO TRUE
007520
007530*    Faz put da requisicao do programa a executar
007540     EVALUATE NM-PGM
007550         WHEN SGTSI059
007560             EXEC CICS PUT
007610                        CONTAINER (NM-CTNR)
007620                        FROM      (SGTSI059-RQSC )
007630                        FLENGTH   (LENGTH OF SGTSI059-RQSC )
007640              END-EXEC
006040
007550         WHEN SGTSPE14
007560             EXEC CICS PUT
007610                        CONTAINER (NM-CTNR)
007620                        FROM      (SGTSPE14-RQSC )
007630                        FLENGTH   (LENGTH OF SGTSPE14-RQSC )
007640              END-EXEC
001660
007550         WHEN SGTSA055
007560             EXEC CICS PUT
007610                        CONTAINER (NM-CTNR)
007620                        FROM      (SGTSA055-RQSC )
007630                        FLENGTH   (LENGTH OF SGTSA055-RQSC )
007640              END-EXEC
001660
007650         WHEN SGTSI056
007560             EXEC CICS PUT
007610                        CONTAINER (NM-CTNR)
007620                        FROM      (SGTSI056-RQSC )
007630                        FLENGTH   (LENGTH OF SGTSI056-RQSC )
007640              END-EXEC
001660
006040         WHEN SGTSE056
007560             EXEC CICS PUT
007610                        CONTAINER (NM-CTNR)
007620                        FROM      (SGTSE056-RQSC )
007630                        FLENGTH   (LENGTH OF SGTSE056-RQSC )
007640              END-EXEC
001660
006040         WHEN SGTSL056
007560             EXEC CICS PUT
007610                        CONTAINER (NM-CTNR)
007620                        FROM      (SGTSL056-RQSC )
007630                        FLENGTH   (LENGTH OF SGTSL056-RQSC )
007640              END-EXEC
001660
006040         WHEN OTHER
006040              SET   ERRO-PROGRAMADO
006040                                 TO  TRUE
006040              MOVE  9004         TO  MSG1-CD-ERRO-TS
006040              STRING  'Erro na gravação da requisição do módulo '
006040                       NM-PGM '.'
007680              DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
007690              PERFORM 999000-ERRO
007700     END-EVALUATE
007710
007720*Ao chegar aqui, há certeza de que não houve erro algum
007730     SET  SEM-ERRO               TO TRUE
007750     .
007760 982000-SAI.
007770     EXIT.
007790*----------------------------------------------------------------*
007800 983000-EXCLUIR-RESPOSTA-ERRO    SECTION.
007810*----------------------------------------------------------------*
007820* Exclui o container de resposta e de erro para evitar problemas *
007830* em modulos executados mais de uma vez dentro da mesma sessao do*
007840* CICS. Equivale ao initialize na area de resposta dos programas *
007850* que utilizam COMMAREA.                                         *
007860*----------------------------------------------------------------*
007900*    Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
007910*    nome_modulo -RPST
007920*     EVALUATE NM-PGM
007930*         WHEN CAFS0053
007940*              MOVE 'PRM-DADOS-RPST'
007950*                                 TO NM-CTNR
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
001660
008130         WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
008140              CONTINUE
001660
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
008470         WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
008480              CONTINUE
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
008940
008950     IF MSG1-CD-ERRO-TS EQUAL ZEROS
008960        MOVE 8888                TO MSG1-CD-ERRO-TS
008970     END-IF
001660
008980     IF MSG1-TX-ERRO-TS EQUAL SPACES
008990        MOVE 'Erro nao especificado no modulo' TO MSG1-TX-ERRO-TS
009000     END-IF
001660
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
009120
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
009320*    IF  MNT-VRV
009320         MOVE 'SGTS0098-NR-NOTA-TCN-OPR'   TO MSGKW999-NM-VRV-MNT
009350         MOVE  SGTS0098-NR-NOTA-TCN-OPR    TO MSGKW999-VL-VRV-MNT
009370         PERFORM 999003-ADICIONA-VRV-MNT
001660
009320         MOVE 'SGTS0098-CD-ARQ-OPR-ETRD'   TO MSGKW999-NM-VRV-MNT
009350         MOVE  SGTS0098-CD-ARQ-OPR-ETRD    TO MSGKW999-VL-VRV-MNT
009370         PERFORM 999003-ADICIONA-VRV-MNT
001660
009320         MOVE 'SGTS0098-CD-UOR-DRTA-OGNL'  TO MSGKW999-NM-VRV-MNT
009350         MOVE  SGTS0098-CD-UOR-DRTA-OGNL   TO MSGKW999-VL-VRV-MNT
009370         PERFORM 999003-ADICIONA-VRV-MNT
001660
009320         MOVE 'SGTS0098-CD-REF-NOTA-TCN'   TO MSGKW999-NM-VRV-MNT
009350         MOVE  SGTS0098-CD-REF-NOTA-TCN    TO MSGKW999-VL-VRV-MNT
009370         PERFORM 999003-ADICIONA-VRV-MNT
001660
009320         MOVE 'SGTS0098-DT-EMS-NOTA'       TO MSGKW999-NM-VRV-MNT
009350         MOVE  SGTS0098-DT-EMS-NOTA        TO MSGKW999-VL-VRV-MNT
009370         PERFORM 999003-ADICIONA-VRV-MNT
001660
009320         MOVE 'SGTS0098-DT-APVC-NOTA'      TO MSGKW999-NM-VRV-MNT
009350         MOVE  SGTS0098-DT-APVC-NOTA       TO MSGKW999-VL-VRV-MNT
009370         PERFORM 999003-ADICIONA-VRV-MNT
001660
009320         MOVE 'SGTS0098-CD-ETR-DCS'        TO MSGKW999-NM-VRV-MNT
009350         MOVE  SGTS0098-CD-ETR-DCS         TO MSGKW999-VL-VRV-MNT
009370         PERFORM 999003-ADICIONA-VRV-MNT
001660
009320         MOVE 'SGTS0098-CD-TIP-DOC-TCN'    TO MSGKW999-NM-VRV-MNT
009350         MOVE  SGTS0098-CD-TIP-DOC-TCN     TO MSGKW999-VL-VRV-MNT
009370         PERFORM 999003-ADICIONA-VRV-MNT
001660
009320         MOVE 'SGTS0098-QT-OCR-TAB'        TO MSGKW999-NM-VRV-MNT
009350         MOVE  SGTS0098-QT-OCR-TAB         TO MSGKW999-VL-VRV-MNT
009370         PERFORM 999003-ADICIONA-VRV-MNT
001660
009320*        PERFORM VARYING IC-TAB FROM 1 BY 1
009320*          UNTIL IC-TAB GREATER SGTS0098-QT-OCR-TAB
009320*          MOVE 'IC-TAB' TO MSGKW999-NM-VRV-MNT
009320*          MOVE IC-TAB   TO MSGKW999-VL-VRV-MNT
009320*          PERFORM 999003-ADICIONA-VRV-MNT
001660*
009320         IF IC-TAB > 0
009320             MOVE 'SGTS0098-CD-UOR-ITVT-NOTA'
009330                                           TO MSGKW999-NM-VRV-MNT
009320             MOVE SGTS0098-CD-UOR-ITVT-NOTA(IC-TAB)
009320                                           TO MSGKW999-VL-VRV-MNT
009320             PERFORM 999003-ADICIONA-VRV-MNT
009320         END-IF
009320*        END-PERFORM
009380
009390*    END-IF
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
009710*---------------------- FIM SGTS0098 -----------------------------