000010*-----------------------------------------------------------------
000020* PROGRAMA..: SGTS0148
000030* ANALISTA..: F2054109 - Cintia A.Iglesias
000040* AUTOR.....: F2054109 - Cintia A.Iglesias
000050* DATA......: 25/05/2016
000060* OBJETIVO..: Consultar Disponibilidade de Atributos por Tipo de
      *             Ativo
      * COMPILACAO: 54 - PSOSE600 - Cobol 6.3 c/otimizacao p/producao
000090*-----------------------------------------------------------------
      *VRS002 07/03/2024 F9158937-Habilitar SGTS0148-IN-HBLT-CD-TIP-COTA
      *                  para "Certificados de Recebíveis".
      *VRS001 25/05/20176 F2054109 DE-2016/145195 - IMPLANTACAO
000110*-----------------------------------------------------------------
000120
000130*-----------------------------------------------------------------
000140 IDENTIFICATION DIVISION.
000150*----------------------------------------------------------------*
000160 PROGRAM-ID. SGTS0148.
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
000360 77  CTE-INICIO-SS               PIC  X(040)  VALUE
000370              '*** S.S. COMEÇA AQUI ***'.
000380
000390 77  CTE-PROG                    PIC  X(018)  VALUE
000400              '*** SGTS0148 ***'.
000410 77  CTE-VERS                    PIC  X(006)  VALUE 'VRS002'.
000420
000480*----------------------------------------------------------------*
000490 LOCAL-STORAGE                   SECTION.
000500*----------------------------------------------------------------*
000510
000520* Areas de trabalho
000540*----------------------------------------------------------------*
000550* Book dos Containers e das subrotinas chamadas
000560*----------------------------------------------------------------*
000570
000580* Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1
000600* Book para captura de abend cics da bloco codigo HLPKP999
-INC HLPKW999
000620
000630* Book para captura de erro via MSG1
-INC MSGKWMG1
000650
000660* Book do container de requisição e resposta deste modulo
-INC SGTK0148
000690
001230* Book do Container de Erro
-INC IIBKERRO
001250* Book do container de Serviços (para acionamento de serviços)

001290
001300 77  CTE-FINAL-SS                PIC X(40)      VALUE
001310              '*** S.S. TERMINA AQUI ***'.
001320
001330*----------------------------------------------------------------*
001340 PROCEDURE DIVISION.
001350*----------------------------------------------------------------*
001360
001370*----------------------------------------------------------------*
001380 000000-ROTINA-PRINCIPAL         SECTION.
001390*----------------------------------------------------------------*
           MOVE 0001                   TO MSG1-CD-SEQL-ERRO
001410     PERFORM 100000-INICIAR
001420
001430     PERFORM 200000-PROCESSAR
001440
001450     PERFORM 999999-ENCERRAR
001460
001470     .
001480 000000-SAI.
001490     EXIT.
001500
001510*----------------------------------------------------------------*
001520 100000-INICIAR                  SECTION.
001530*----------------------------------------------------------------*
001540
001550     MOVE 0002                   TO MSG1-CD-SEQL-ERRO
001560
001570*    Ativa a captura automatica de abend do CICS.
001580*    WHENEVER CICS
-INC MSGKE999
001600
001610     PERFORM 970000-LER-CANAL
001620
001630*    Define o nome do programa p/ fins manipulacao container
001640     MOVE 'SGTS0148'             TO NM-PGM
001650
001660*    Limpa a resposta e erro antes de executar o modulo
001670     PERFORM 983000-EXCLUIR-RESPOSTA-ERRO
001680
001690     PERFORM 971000-LER-REQUISICAO
001720
001730     PERFORM 110000-VALIDAR-REQUISICAO
001740     .
001750 100000-SAI.
001760     EXIT.
001770
001780*----------------------------------------------------------------*
001790 110000-VALIDAR-REQUISICAO       SECTION.
001800*----------------------------------------------------------------*
001810     MOVE 0003                   TO MSG1-CD-SEQL-ERRO
001820
001830*    Valida campo a campo da requisicao
001840
001850     IF  SGTS0148-CD-PRD         < ZEROS
001860         SET ERRO-PROGRAMADO     TO TRUE
001870         MOVE 0010               TO MSG1-CD-ERRO-TS
001880         MOVE 'Código do produto inválido.'
001890                                 TO MSG1-TX-ERRO-TS
001900         SET MNT-VRV             TO TRUE
001910         PERFORM 999000-ERRO
001920     END-IF
001930
001940     IF SGTS0148-CD-MDLD         < ZEROS
001950         SET ERRO-PROGRAMADO     TO TRUE
001960         MOVE 0011               TO MSG1-CD-ERRO-TS
001970         MOVE 'Código da modalidade inválido.'
001980                                 TO MSG1-TX-ERRO-TS
001990         SET MNT-VRV             TO TRUE
002000         PERFORM 999000-ERRO
002090     END-IF
002140     .
002150 110000-SAI.
002160     EXIT.

003280*----------------------------------------------------------------*
003290 200000-PROCESSAR                SECTION.
003300*----------------------------------------------------------------*
003490     MOVE 0004                   TO MSG1-CD-SEQL-ERRO

           PERFORM 230000-TRATAR-INDICADORES

           PERFORM 240000-TRATAR-TIPO-ATIVO

           PERFORM 980000-GRAVAR-CONTAINER-RPST
003420     .
003430 200000-SAI.
003440     EXIT.

      *----------------------------------------------------------------*
003470 230000-TRATAR-INDICADORES       SECTION.
003480*----------------------------------------------------------------*
003490     MOVE 0005                   TO MSG1-CD-SEQL-ERRO

003500     MOVE 'N'                    TO SGTS0148-IN-HBLT-N-CNV-ACAO

           MOVE 'S'                    TO  SGTS0148-IN-HBLT-CD-PRD
                                          SGTS0148-IN-HBLT-CD-MDLD
                                          SGTS0148-IN-HBLT-CD-UOR-DTTR
                                          SGTS0148-IN-HBLT-CD-TIT-OPR
                                          SGTS0148-IN-HBLT-NR-DVS-TIT
                                          SGTS0148-IN-HBLT-CD-ITC-CVM
                                          SGTS0148-IN-HBLT-CD-CTGR-TVM
                                          SGTS0148-IN-HBLT-CD-ESPC-DBTR
                                          SGTS0148-IN-HBLT-CD-TIP-COTA
                                          SGTS0148-IN-HBLT-NR-VRS-COTA
                                          SGTS0148-IN-HBLT-CD-CMRA-OPR
                                          SGTS0148-IN-HBLT-NR-EMS-TIT
                                          SGTS0148-IN-HBLT-NR-SRE-TIT
                                          SGTS0148-IN-HBLT-CD-ISIN-TIT
                                          SGTS0148-IN-HBLT-CD-TIP-MC-CPR
                                          SGTS0148-IN-HBLT-BCO-MC-SCDR
                                          SGTS0148-IN-HBLT-CD-BCO-CRD-VL
                                          SGTS0148-IN-HBLT-CD-DEPE-CRD
                                          SGTS0148-IN-HBLT-NR-CC-CRD-VL
                                          SGTS0148-IN-HBLT-CD-CLI-EMT
                                          SGTS0148-IN-HBLT-CD-CLI-IFLC
                                          SGTS0148-IN-HBLT-CD-CLI-CDT
                                          SGTS0148-IN-HBLT-CD-CLI-ADM
                                          SGTS0148-IN-HBLT-CD-CLI-FDAR
                                          SGTS0148-IN-HBLT-DT-EMS-TIT
                                          SGTS0148-IN-HBLT-QT-PAPL-EMS
                                          SGTS0148-IN-HBLT-VL-EMS-TIT
                                          SGTS0148-IN-HBLT-DT-CPR-TIT
                                          SGTS0148-IN-HBLT-QT-PAPL-CPR
                                          SGTS0148-IN-HBLT-VL-CPR-TIT
                                          SGTS0148-IN-HBLT-DT-VNCT-OPR
                                          SGTS0148-IN-HBLT-CD-RSCO-OPR
                                          SGTS0148-IN-HBLT-VL-PRV-ENCT
                                          SGTS0148-IN-HBLT-VL-APVD-BB
                                          SGTS0148-IN-HBLT-IN-CNG-PBCO
                                          SGTS0148-IN-HBLT-IN-FLZ-EXCO
                                          SGTS0148-IN-HBLT-OPR-ETRD-IETR
                                          SGTS0148-IN-HBLT-IN-SPT-FCTR

003650     .
003660 230000-SAI.
003670     EXIT.

      *----------------------------------------------------------------*
001850 240000-TRATAR-TIPO-ATIVO        SECTION.
001860*----------------------------------------------------------------*
003490     MOVE 0006                   TO MSG1-CD-SEQL-ERRO
           PERFORM 241000-TRATAR-NAO-DEBENTURE
001880
           PERFORM 242000-TRATAR-FIDC

           PERFORM 243000-TRATAR-NAO-CRI-FIDC
           .
002470 240000-SAI.
002480     EXIT.

      *----------------------------------------------------------------*
001850 241000-TRATAR-NAO-DEBENTURE     SECTION.
001860*----------------------------------------------------------------*
001870     MOVE 0007                   TO MSG1-CD-SEQL-ERRO
001880
      * Produto = 427 - Definitiva de Títulos Privados

      * Se tipo de ativo diferente de 'Debentures'
      * Modalidade = 02 - Debêntures Cetipada com Clientes

001890     IF (SGTS0148-CD-PRD         NOT EQUAL 427 OR
               SGTS0148-CD-MDLD        NOT EQUAL 02)
               MOVE 'N'                TO SGTS0148-IN-HBLT-OPR-ETRD-IETR
                                          SGTS0148-IN-HBLT-CD-ESPC-DBTR
           END-IF
           .
002470 241000-SAI.
002480     EXIT.

      *----------------------------------------------------------------*
001850 242000-TRATAR-FIDC              SECTION.
001860*----------------------------------------------------------------*
001870     MOVE 0008                   TO MSG1-CD-SEQL-ERRO
001880
      * Se tipo de ativo diferente de FIDC  e CR
      * Modalidade = 03 - Fundo de Investimento em Direitos Creditórios
      * Modalidade = 04 - Certificados de Recebíveis
      *  - FIDC
      *  - CR

           IF (SGTS0148-CD-PRD         NOT EQUAL 427) OR
                              (SGTS0148-CD-MDLD NOT EQUAL 03 AND 04)
               MOVE 'N'                TO SGTS0148-IN-HBLT-CD-TIP-COTA
           END-IF

      * Se tipo de ativo diferente de FIDC
      * Modalidade = 03 - Fundo de Investimento em Direitos Creditórios
      *  - FIDC

           IF (SGTS0148-CD-PRD         EQUAL 427 AND
                                          SGTS0148-CD-MDLD EQUAL 03)
               MOVE 'N'                TO SGTS0148-IN-HBLT-NR-SRE-TIT
           END-IF

           .
002470 242000-SAI.
002480     EXIT.

      *----------------------------------------------------------------*
001850 243000-TRATAR-NAO-CRI-FIDC      SECTION.
001860*----------------------------------------------------------------*
001870     MOVE 0009                   TO MSG1-CD-SEQL-ERRO
001880
      * Se tipo de ativo diferente de CRI E FIDC
      * Modalidade = 03 - Fundo de Investimento em Direitos Creditórios
      * - FIDC
      * Modalidade = 04 - Certificados de Recebíveis Imobiliários - CRI

           IF (SGTS0148-CD-PRD         NOT EQUAL 427) OR
                                  (SGTS0148-CD-MDLD NOT EQUAL 03 AND 04)
               MOVE 'N'
                                       TO SGTS0148-IN-HBLT-CD-CLI-IFLC
                                          SGTS0148-IN-HBLT-CD-CLI-CDT
                                          SGTS0148-IN-HBLT-CD-CLI-ADM
           END-IF
           .
002470 243000-SAI.
002480     EXIT.

006370*----------------------------------------------------------------*
006380*                                                                *
006390* SECTION DA FAIXA DE 970000 A 979900 PARA LEITURA DE DADOS      *
006400*                                                                *
006410*----------------------------------------------------------------*
006420 970000-LER-CANAL                SECTION.
006430*    Obtem o canal corrente
006440*----------------------------------------------------------------*
006450     MOVE 0010                   TO MSG1-CD-SEQL-ERRO
006460
006470*Por causa da captura automática de erro CICS, deve ser previsto
006480*o erro de container
006490     SET  ERRO-CANAL             TO TRUE
006500
006510     EXEC CICS ASSIGN
006520               CHANNEL ( NM-CNL )
006530     END-EXEC
006540
006550*    Ao chegar aqui, há certeza de que não houve erro algum
006560     SET  SEM-ERRO               TO TRUE
006570     .
006580 970000-FIM.
006590     EXIT.
006600
006610*----------------------------------------------------------------*
006620 971000-LER-REQUISICAO           SECTION.
006630*----------------------------------------------------------------*
006640
006650     MOVE 0011                   TO MSG1-CD-SEQL-ERRO
006660
006670*    Monta o nome do container de requisicao
006680     MOVE 'SGTS0148'             TO NM-PGM
006690     MOVE 'SGTS0148-RQSC'        TO NM-CTNR
006700
006710*    Recupera o tamanho do container de requisição
006720     PERFORM 973100-LER-CONTAINER-NODATA
006730
006740*    Obtem o tamanho esperado da requisicao
006750     MOVE LENGTH OF SGTS0148-RQSC TO QT-CCT-EPRO
006760
006770*    Valida tamanho esperado da requisicao X tamanho recebido
006780
006790     IF  QT-CCT-LIDO             NOT EQUAL QT-CCT-EPRO
006800         SET ERRO-TAMANHO-AREA   TO TRUE
006810         PERFORM 999000-GRAVA-ERRO-E-ENCERRA
006820     END-IF
006830
006840     MOVE 0012                   TO MSG1-CD-SEQL-ERRO
006850
006860*Configura dados para leitura do container de requisição
006870     SET  GET-CONTAINER          TO TRUE
006880
006890*Por causa da captura automática de erro CICS, deve ser previsto
006900*o erro de container
006910     SET  ERRO-CONTAINER         TO TRUE
006920
006930*    Efetua a leitura do container
006940     EXEC CICS GET
006950               CONTAINER (NM-CTNR)
006960               FLENGTH   (QT-CCT-LIDO)
006970               INTO      (SGTS0148-RQSC)
006980     END-EXEC
006990
007000*Ao chegar aqui, há certeza de que não houve erro algum
007010     SET  SEM-ERRO               TO TRUE
007030     .
007040 971000-SAI.
007050     EXIT.
007060
007070*----------------------------------------------------------------*
008240 973100-LER-CONTAINER-NODATA     SECTION.
008250*----------------------------------------------------------------*
008260
008270     MOVE 0013                   TO MSG1-CD-SEQL-ERRO
008280
008290*Configura dados para leitura do container de requisição
008300     SET  GET-CONTAINER          TO TRUE
008310
008320*Por causa da captura automática de erro CICS, deve ser previsto
008330*o erro de container
008340     SET  ERRO-CONTAINER         TO TRUE
008350
008360*    Obtem a quantidade de caracteres no container
008370     MOVE ZEROS                  TO QT-CCT-LIDO
008380     EXEC CICS GET
008390               CONTAINER (NM-CTNR)
008400               NODATA
008410               FLENGTH   (QT-CCT-LIDO)
008420     END-EXEC
008430
008440*    Ao chegar aqui, há certeza de que não houve erro algum
008450     SET  SEM-ERRO               TO TRUE
008460     .
008470 973100-SAI.
008480     EXIT.
008760* SECTION DA FAIXA DE 980000 A 989900 PARA GRAVACAO DE DADOS     *
008770*                                                                *
008780*----------------------------------------------------------------*
008790 980000-GRAVAR-CONTAINER-RPST    SECTION.
008800*----------------------------------------------------------------*
           MOVE 0014                   TO MSG1-CD-SEQL-ERRO
008810
008820*    Exclui eventual container de erro de outros programas
008830     PERFORM 983100-EXCLUIR-CONTAINER-ERRO
008840
008850     MOVE 0015                   TO MSG1-CD-SEQL-ERRO
008860
008870*    Monta o nome do container de resposta
008880     MOVE 'SGTS0148-RPST'        TO NM-CTNR
008890     SET PUT-CONTAINER           TO TRUE
008900
008910*Por causa da captura automática de erro CICS, deve ser previsto
008920*o erro de container
008930     SET  ERRO-CONTAINER         TO TRUE
008940
008950     EXEC CICS PUT
008960               CONTAINER (NM-CTNR                )
008970               FROM      (SGTS0148-RPST          )
008980               FLENGTH   (LENGTH OF SGTS0148-RPST)
008990     END-EXEC
009000
009010*Ao chegar aqui, há certeza de que não houve erro algum
009020     SET  SEM-ERRO               TO TRUE
009030     .
009040 980000-SAI.
009050     EXIT.

009570*----------------------------------------------------------------*
009580 983000-EXCLUIR-RESPOSTA-ERRO    SECTION.
009590*----------------------------------------------------------------*
009600* Exclui o container de resposta e de erro para evitar problemas *
009610* em modulos executados mais de uma vez dentro da mesma sessao do*
009620* CICS. Equivale ao initialize na area de resposta dos programas *
009630* que utilizam COMMAREA.                                         *
009640*----------------------------------------------------------------*
009650
009660     MOVE 0016                   TO MSG1-CD-SEQL-ERRO
009670
009680*    Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
009690*    nome_modulo -RPST
009700     MOVE NM-PGM                 TO NM-CTNR(1:8)
009710     MOVE '-RPST'                TO NM-CTNR(9:5)
009720
009730     SET DEL-CONTAINER           TO TRUE
009740
009750*    Exclui o container de resposta
009760*    Não há captura automática de erro CICS por usar RESP e RESP2
009770     EXEC CICS DELETE
009780               CONTAINER (NM-CTNR)
009790               RESP      (MSG1-RESP1)
009800               RESP2     (MSG1-RESP2)
009810     END-EXEC
009820
009830     EVALUATE TRUE
009840         WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
009850         WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
009860              CONTINUE
009870         WHEN OTHER
009880              SET ERRO-CONTAINER TO TRUE
009890              PERFORM 999002-TRATA-CONDITION-CICS
009900     END-EVALUATE
009910
009920*    Excluir o container de erro
009930     PERFORM 983100-EXCLUIR-CONTAINER-ERRO
009940     .
009950 983000-SAI.
009960     EXIT.
009970*
009980*----------------------------------------------------------------*
009990 983100-EXCLUIR-CONTAINER-ERRO   SECTION.
010000*----------------------------------------------------------------*
           MOVE 0017                   TO MSG1-CD-SEQL-ERRO

010020*    Limpar variaveis container de erro
010030     INITIALIZE IIBKERRO-PRM-ERRO
010040
010050*    Exclui o container de erro
010060     MOVE 0018                   TO MSG1-CD-SEQL-ERRO
010070     MOVE 'PRM-ERRO'             TO NM-CTNR
010080     SET DEL-CONTAINER           TO TRUE
010090
010100*    Não há captura automática de erro CICS por usar RESP e RESP2
010110     EXEC CICS DELETE
010120               CONTAINER (NM-CTNR)
010130               RESP      (MSG1-RESP1)
010140               RESP2     (MSG1-RESP2)
010150     END-EXEC
010160
010170     EVALUATE TRUE
010180         WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
010190         WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
010200              CONTINUE
010210         WHEN OTHER
010220              SET ERRO-CONTAINER TO TRUE
010230              PERFORM 999002-TRATA-CONDITION-CICS
010240     END-EVALUATE
010250     .
010260 983100-SAI.
010270     EXIT.

010280*----------------------------------------------------------------*
010290 984000-GRAVAR-CONTAINER-ERRO    SECTION.
010300*----------------------------------------------------------------*
010310     MOVE 0019                   TO MSG1-CD-SEQL-ERRO
010320
010330*    Grava o erro gerado no container de erro
010340*    Não há captura automática de erro CICS por usar RESP e RESP2
010350     EXEC CICS PUT
010360               CONTAINER ( 'PRM-ERRO'                  )
010370               FROM      ( IIBKERRO-PRM-ERRO           )
010380               FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
010390               RESP      (MSG1-RESP1                   )
010400               RESP2     (MSG1-RESP2                   )
010410     END-EXEC
010430     .
010440 984000-SAI.
010450     EXIT.
010570*
010580*----------------------------------------------------------------*
010590*                                                                *
010600* As sections abaixo iniciadas com 999000 representam os tratamen*
010610* tos de erro utilizando o Sistema MSG (rotina MSGS0001)         *
010620*                                                                *
010630*----------------------------------------------------------------*
010640 999000-ERRO                     SECTION.
010650*----------------------------------------------------------------*
010660
010670     IF  MSG1-CD-ERRO-TS         EQUAL ZEROS
010680         MOVE 8888               TO MSG1-CD-ERRO-TS
010690     END-IF

010700     IF  MSG1-TX-ERRO-TS         EQUAL SPACES
010710         MOVE 'Erro não especificado no módulo.'
                                       TO MSG1-TX-ERRO-TS
010720     END-IF
010730     PERFORM 999000-GRAVA-ERRO-E-ENCERRA
010740     .
010750 999000-ERRO-SAI.
010760     EXIT.
010770
010780*----------------------------------------------------------------*
010790*     As sections abaixo representam os tratamentos de erro
010800*    utilizando o sistema MSG de mensageria (rotina MSGS0001)
010810*-----------------------------------------------------------------
010820 999000-GRAVA-ERRO-E-ENCERRA     SECTION.
010830*-----------------------------------------------------------------
010840
010850*    Em caso de erro, desfaz as atualizações em banco de dados
010860
010870     EXEC CICS SYNCPOINT
010880               ROLLBACK
010890     END-EXEC
010900
010910*Verifica se o tipo de erro gerado possui tratamento interno
010920     EVALUATE TRUE
010930         WHEN ERRO-CONTAINER
010940              PERFORM 999004-TRATA-ERRO-CTNR
010950         WHEN ERRO-TAMANHO-AREA
010960              PERFORM 999005-TRATA-ERRO-AREA-DADOS
010970         WHEN ERRO-CANAL
010980              PERFORM 999006-TRATA-ERRO-CANAL
010990         WHEN OTHER
011000              CONTINUE
011010     END-EVALUATE
011020
011030*Adiciona as variáveis de entrada para monitoração
011040     IF  MNT-VRV
011050         MOVE 'SGTS0148-CD-PRD'
011060                                 TO MSGKW999-NM-VRV-MNT
011070         MOVE  SGTS0148-CD-PRD
011080                                 TO MSGKW999-VL-VRV-MNT
011090         PERFORM 999003-ADICIONA-VRV-MNT
011100
011110         MOVE 'SGTS0148-CD-MDLD'
011120                                 TO MSGKW999-NM-VRV-MNT
011130         MOVE SGTS0148-CD-MDLD
011140                                 TO MSGKW999-VL-VRV-MNT
011150         PERFORM 999003-ADICIONA-VRV-MNT

011290     END-IF
011300
011310*    Grava o erro gerado no sistema MSG - Mensageria
011320     PERFORM MSG1-NTF-MSG
011330
011340     MOVE MSG1-CD-ERRO-TS        TO IIBKERRO-CD-ERRO
011350     MOVE MSG1-TX-ERRO-TS        TO IIBKERRO-TX-ERRO
011360
011370*    Grava o erro gerado no container de erro
011380     PERFORM 984000-GRAVAR-CONTAINER-ERRO
011390
011400*    Encerra a transacao
011410     PERFORM 999999-ENCERRAR
011420     .
011430 999000-SAI.
011440     EXIT.

011450* --------------------------------------*
011460* 999001-TRATA-ABEND-CICS        SECTION.
011470* 999002-TRATA-CONDITION-CICS    SECTION.
011480* 999003-ADICIONA-VRV-MNT        SECTION.
      * 999004-ALINHA-NUMERO-ESQUERDA  SECTION.
011490* --------------------------------------*
-INC HLPKP999
011510* --------------------------------------*
011520* 999004-TRATA-ERRO-CTNR         SECTION.
011530* 999005-TRATA-ERRO-AREA-DADOS   SECTION.
011540* 999006-TRATA-ERRO-CANAL        SECTION.
011550*---------------------------------------*
-INC SGTKP999
011570* --------------------------------------*
011580* MSG1-NTF-MSG                   SECTION.
      * MSG1-PCE-INC                   SECTION.
      * MSG1-PUT-CTNR-PRM              SECTION.
      * MSG1-PUT-CNTRS-OPC             SECTION.
      * MSG1-PUT-CTNR-PGM-ACND         SECTION.
      * MSG1-PUT-CTNR-OPR-ACND         SECTION.
      * MSG1-PUT-CTNR-SQL              SECTION.
      * MSG1-PUT-CTNR-CICS             SECTION.
      * MSG1-PUT-CTNR-VRV              SECTION.
      * MSG1-EXEC-MSG1                 SECTION.
      * MSG1-GET-CTNR-ERRO             SECTION.
011590* --------------------------------------*
-INC MSGKPMG1

010470*----------------------------------------------------------------*
010480 999999-ENCERRAR                 SECTION.
010490*----------------------------------------------------------------*
010500
010510     EXEC CICS
010520          RETURN
010530     END-EXEC
010540     .
010550 999999-SAI.
010560     EXIT.
011610*---------------------- FIM SGTS0148 -----------------------------