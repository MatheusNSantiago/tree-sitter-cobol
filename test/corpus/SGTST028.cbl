000010*-----------------------------------------------------------------
000020* PROGRAMA..: SGTST028
000030* ANALISTA..: F6631109 - MAGALI MARIKO OYAMADA
000040* AUTOR.....: F6631109 - MAGALI MARIKO OYAMADA
000050* DATA......: 15.07.2015
000060* OBJETIVO..: PERSISTENCIA DA TABELA EST_TIP_COTA_TIT
000070* COMPILACAO: COBOL/CICS/DB2
000080*-----------------------------------------------------------------
      * VRS003 22/05/2019 F0950413 DE 2016/145195 - Inclusão na section
      *        999001-ERRO-001, '1 - Erro de db2' e '2 - erro cics', e
      *        adequação das mensagens de erro ao padrão da mensageria.
      * VRS002 19/03/2018 - F9188847 - 2016/145195 - Alteração no
      *              book SGTK014G (Recompilação).
      * VRS001 14/02/2017 F8785286 DE 2016/145195 - Inclusão movimen-
      *              tação campo book para chave log.
000090*        15.07.2015 - F6631109 - IMPLANTACAO
000100*-----------------------------------------------------------------
000110
000120*-----------------------------------------------------------------
000130 IDENTIFICATION DIVISION.
000140*-----------------------------------------------------------------
000150 PROGRAM-ID. SGTST028.
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
000310 01  CTE-PROG     PIC  X(17) VALUE '*** SGTST028 ***'.
       01  CTE-VERS     PIC  X(06) VALUE 'VRS003'.
000330
      *--> Registro da tabela
000360 01  REG-CACHE.
000370     03  CHAVE-CACHE.
000380         05  CD-TIP-COTA-TIT           PIC S9(4) COMP-5 VALUE 0.
000390         05  NR-VRS-TIP-COTA           PIC S9(4) COMP-5 VALUE 0.
000400         05  TS-EST-TIP-COTA           PIC  X(026) VALUE ' '.
000410         05  FILLER                    PIC  X(20) VALUE ' '.
000420     03  DADOS-CACHE.
000430         05  CD-TIP-EST-COTA           PIC S9(4) COMP-5 VALUE 0.
000440         05  CD-USU-ATL-EST            PIC  X(08) VALUE ' '.
000450         05  TX-OBS-EST-TIP            PIC  X(60) VALUE ' '.
000460
000470* Book do programa de ambiente - CICS5000
000480* Definido na work para preservar as informacoes
-INC HLPKE01B                                                           00000490
000500*-----------------------------------------------------------------
000510 LOCAL-STORAGE SECTION.
000520*-----------------------------------------------------------------
000530
      *--> Area de trabalho
       01  IC-TAB                      PIC  9(2)        VALUE 0.
       01  QT-REG-SELD                 PIC S9(4) COMP-5 VALUE 0.
       01  TS-SQL-ATU                  PIC  X(26) VALUE ' '.

      *--> Indicadores de nulidades
000600 01  QT-REG-SELD-NULO                 PIC S9(4) COMP-5 VALUE 0.
000610 01  TX-OBS-EST-TIP-N                 PIC S9(4) COMP-5 VALUE 0.
000620
      *--> Variaveis para gravacao do log
000640 01  DADOS-LOG-DISPLAY.
000650     03  CD-TIP-COTA-TIT-LOG           PIC  9(5) VALUE 0.
000660     03  NR-VRS-TIP-COTA-LOG           PIC  9(5) VALUE 0.
000670     03  TS-EST-TIP-COTA-LOG           PIC  X(26) VALUE ' '.
000680
      *--> Book dos dados do log
       01  DADOS-LOG.
-INC SGTK014G                                                           00000710

      *--> Subrotinas Utilizadas
       01  SGTS014G VALUE 'SGTS014G'         PIC X(8).

      *--> Books definicao de dados a serem usados na subrotina de erros
-INC HLPKE001                                                           00000770

      *--> Máscaras edição de variáveis p/ monitoração mensagens de erro
       01  MCR-S9-15                   PIC      ---------------9.

      *--> Book para captura de erro via MSG1
-INC MSGKWMG1

      *--> HLPKW999
      *    Variaveis Auxiliares para Captura de Erro CICS
      *    e Adicionar Variaveis Monitoradas
       01  MSGKW999-VRV-ERRO-CICS.
           03  MSGKW999-RESP1-X8            PIC  X(08) VALUE SPACES.
           03  MSGKW999-RESP2-X8            PIC  X(08) VALUE SPACES.
       01  MSGKW999-VRV-ALINHA-NUMERO.
           03  MSGKW999-NUMERO-Z17          PIC  ZZZZZZZZZZZZZZZZ9.
           03  MSGKW999-NUMERO-X17          PIC  X(17) VALUE SPACES.
           03  MSGKW999-BRANCOS             PIC  9(08) VALUE ZEROS.
       01  MSGKW999-VRV-AUX.
           03  MSGKW999-NM-VRV-MNT     PIC  X(030) VALUE SPACES.
           03  MSGKW999-VL-VRV-MNT     PIC  X(100) VALUE SPACES.
      *--> Indicador de gravação de variáveis monitoradas
       01  MSGKW999-IN-GRV-VRV         PIC  X(01)  VALUE ' '.
           88 MSGKW999-MNT-VRV                     VALUE 'S'.
           88 MSGKW999-N-MNT-VRV                   VALUE 'N'.
000780
000790     EXEC SQL
000800         INCLUDE SQLCA
000810     END-EXEC.
000820
000830*-----------------------------------------------------------------
000840 LINKAGE SECTION.
000850*-----------------------------------------------------------------
000860 01  DFHCOMMAREA.
-INC SGTKT028                                                           00000870
000880*-----------------------------------------------------------------
000890 PROCEDURE DIVISION USING DFHCOMMAREA.
000900*-----------------------------------------------------------------
           IF  EIBCALEN NOT EQUAL LENGTH OF DFHCOMMAREA
               SET ERRO-CODIFICADO     TO TRUE
               MOVE 0220               TO  CD-RTN-PGM
               MOVE 'Area com tamanho inválido - verifique book/local.'
                                       TO MSG-ERRO
               SET MSGKW999-MNT-VRV    TO TRUE
               PERFORM 999001-ERRO-001
           END-IF
000960     IF NOT EXECUCAO-ONLINE AND NOT EXECUCAO-BATCH
000970       CALL CICS5000 USING CICS5000-COMMAREA
000980     END-IF
000990     MOVE 0001                   TO PSC-ERRO-PGM

      *--> WHENEVER DB2
-INC HLPKE002

      *--> WHENEVER CICS
-INC HLPKE003
001040
001050     PERFORM 100000-VALIDA-PARAMETRO
001060
001070     EVALUATE TRUE
001080        WHEN SGTST028-INCLUIR
001090             PERFORM 400000-INCLUIR
001100        WHEN SGTST028-ALTERAR
001110             PERFORM 500000-ALTERAR
001120        WHEN SGTST028-EXCLUIR
001130             PERFORM 600000-EXCLUIR
001140        WHEN SGTST028-CONSULTAR
001150             IF  CHAVE-CACHE = SGTST028-CHAVE
001160                 MOVE DADOS-CACHE TO SGTST028-AREA-DADOS
001170             ELSE
001180                 PERFORM 700000-CONSULTA
001190             END-IF
001200        WHEN SGTST028-CONSULTAR-EST-ATUAL
001210             PERFORM 800000-CONSULTA-EST-ATUAL
001220        WHEN SGTST028-CONSULTAR-EST-INCL
001230             PERFORM 900000-CONSULTA-EST-INCL
001240        WHEN SGTST028-VRFR-ULT-EST-COTA
001250             PERFORM 910000-VRFR-ULT-EST-COTA
001260     END-EVALUATE
001270
001280     GOBACK
001290     .
001300*-----------------------------------------------------------------
001310 100000-VALIDA-PARAMETRO         SECTION.
001320*-----------------------------------------------------------------
001330     MOVE 0002                   TO PSC-ERRO-PGM
001340     INITIALIZE SGTST028-RTN-PGM
001350
001360     IF  NOT SGTST028-INCLUIR
001370     AND NOT SGTST028-EXCLUIR
001380     AND NOT SGTST028-ALTERAR
001390     AND NOT SGTST028-CONSULTAR
001400     AND NOT SGTST028-CONSULTAR-EST-ATUAL
001410     AND NOT SGTST028-CONSULTAR-EST-INCL
001420     AND NOT SGTST028-VRFR-ULT-EST-COTA
               SET ERRO-CODIFICADO     TO TRUE
               MOVE  0116              TO  CD-RTN-PGM
               MOVE 'Ação inválida.'   TO MSG-ERRO
               SET MSGKW999-MNT-VRV    TO TRUE
               PERFORM 999001-ERRO-001
001450     END-IF
001460
001470     PERFORM 130000-VALIDA-CAMPOS
001480
001490     IF  SGTST028-INCLUIR
001500     OR  SGTST028-ALTERAR
001510         PERFORM 160000-TRATA-NULIDADE
001520         PERFORM 170001-VALIDA-TIP-COTA-TIT-OPR
001530     END-IF
001540     .
001550 100000-SAI.
001560     EXIT.
001570*-----------------------------------------------------------------
001580 130000-VALIDA-CAMPOS            SECTION.
001590*-----------------------------------------------------------------
001600     MOVE 0003                   TO PSC-ERRO-PGM

001610     PERFORM 140000-BUSCA-TS-DT
001620     MOVE HLPKE001-TS-FMTD       TO TS-SQL-ATU

001630     IF  SGTST028-CD-TIP-COTA-TIT EQUAL ZEROS
               SET ERRO-CODIFICADO     TO TRUE
               MOVE  0257              TO  CD-RTN-PGM
               MOVE 'Código do tipo de cota do título da operação não in
      -             'formado.'         TO MSG-ERRO
               SET MSGKW999-MNT-VRV    TO TRUE
001650         PERFORM 999001-ERRO-001
001660     END-IF
001670     IF NOT SGTST028-VRFR-ULT-EST-COTA
001680        IF  SGTST028-NR-VRS-TIP-COTA EQUAL ZEROS
                  SET ERRO-CODIFICADO  TO TRUE
                  MOVE  0258           TO  CD-RTN-PGM
                  MOVE 'N mero da versão do tipo de cota do título da op
      -                'eração não informado.'   TO MSG-ERRO
                  SET MSGKW999-MNT-VRV TO TRUE
                  PERFORM 999001-ERRO-001
001710        END-IF
001720     END-IF
001730     IF NOT SGTST028-CONSULTAR-EST-ATUAL
001740      AND  NOT SGTST028-CONSULTAR-EST-INCL
001750      AND  NOT SGTST028-VRFR-ULT-EST-COTA
001760        IF  SGTST028-TS-EST-TIP-COTA EQUAL SPACES
                  SET ERRO-CODIFICADO  TO TRUE
                  MOVE  1015           TO  CD-RTN-PGM
                  MOVE 'Timestamp do estado do tipo da cota não informad
      -                'o.'            TO MSG-ERRO
                  SET MSGKW999-MNT-VRV TO TRUE
001780            PERFORM 999001-ERRO-001
001790        END-IF
001800     END-IF
001810     IF  SGTST028-INCLUIR
001820     OR  SGTST028-ALTERAR
001830         IF  SGTST028-CD-TIP-EST-COTA EQUAL ZEROS
                   SET ERRO-CODIFICADO      TO TRUE
                   MOVE  1016               TO  CD-RTN-PGM
                   MOVE 'Código do tipo do estado da cota não informado.
      -                 ''                  TO MSG-ERRO
                   SET MSGKW999-MNT-VRV     TO TRUE
                   PERFORM 999001-ERRO-001
001860         END-IF
001870         IF  SGTST028-CD-USU-ATL-EST EQUAL SPACES
                   SET ERRO-CODIFICADO      TO TRUE
                   MOVE  0524               TO  CD-RTN-PGM
                   MOVE 'Código do usuário responsável pela atualização
      -                 'do estado inválido.'    TO MSG-ERRO
                   SET MSGKW999-MNT-VRV     TO TRUE
                   PERFORM 999001-ERRO-001
001910         END-IF
001920     END-IF
001920
001450     IF  SGTST028-INCLUIR
001460     OR  SGTST028-ALTERAR
006320     OR  SGTST028-EXCLUIR
002430         IF  SGTST028-CD-USU-ATL-EST EQUAL SPACES
                   SET ERRO-CODIFICADO      TO TRUE
                   MOVE  0524               TO  CD-RTN-PGM
                   MOVE 'Código do usuário responsável pela atualização
      -                 'do estado inválido.'    TO MSG-ERRO
                   SET MSGKW999-MNT-VRV     TO TRUE
                   PERFORM 999001-ERRO-001
002470         END-IF
006320     END-IF
006320
001930     IF  SGTST028-TS-EST-TIP-COTA EQUAL 'CURRENT'
001940         MOVE TS-SQL-ATU         TO SGTST028-TS-EST-TIP-COTA
001950     END-IF
001960     .
001970 130000-SAI.
001980     EXIT.
001990*-----------------------------------------------------------------
002000 140000-BUSCA-TS-DT              SECTION.
002010*-----------------------------------------------------------------
002020     MOVE 0004                   TO PSC-ERRO-PGM

002030     CALL SBTIMEDT            USING HLPKE001-CD-FUC
002040                                    HLPKE001-AREA-TRB
002050                                    HLPKE001-FMT-TS
002060                                    HLPKE001-TAM-TS
002070                                    HLPKE001-TS-FMTD

           IF  RETURN-CODE      NOT EQUAL ZEROS
               MOVE  0050              TO  CD-RTN-PGM
               MOVE 'Erro ao obter data e hora atual.'
                                       TO MSG-ERRO
               SET   ERRO-DE-PGM       TO TRUE
               MOVE  SBTIMEDT          TO  PGM-ERRO
               SET MSGKW999-MNT-VRV    TO TRUE
               PERFORM 999001-ERRO-001
           END-IF
002130     .
002140 140000-SAI.
002150     EXIT.
002160*-----------------------------------------------------------------
002170 160000-TRATA-NULIDADE           SECTION.
002180*-----------------------------------------------------------------
002190     MOVE 0005                   TO PSC-ERRO-PGM

002200     IF  SGTST028-TX-OBS-EST-TIP EQUAL SPACES
002210         MOVE -1                 TO TX-OBS-EST-TIP-N
002220     END-IF
002240     .
002250 160000-SAI.
002260     EXIT.
002270*-----------------------------------------------------------------
002280 170001-VALIDA-TIP-COTA-TIT-OPR  SECTION.
002290*-----------------------------------------------------------------
002300     MOVE 0006                   TO PSC-ERRO-PGM
002310     MOVE ZEROS                  TO QT-REG-SELD
002320     EXEC SQL
002330       SELECT 1
002340        INTO :QT-REG-SELD
002350         FROM DB2SGT.TIP_COTA_TIT_OPR
002360         WHERE CD_TIP_COTA_TIT    = :SGTST028-CD-TIP-COTA-TIT
002370           AND NR_VRS_TIP_COTA    = :SGTST028-NR-VRS-TIP-COTA
002380     END-EXEC
002390     IF  QT-REG-SELD = 0
               SET ERRO-CODIFICADO     TO TRUE
               MOVE  0265              TO  CD-RTN-PGM
               MOVE 'Tipo de cota do título da operação inexistente.'
                                       TO MSG-ERRO
               SET MSGKW999-MNT-VRV     TO  TRUE
002410         PERFORM 999001-ERRO-001
002420     END-IF
002430     .
002440 170001-SAI.
002450     EXIT.
002460*-----------------------------------------------------------------
002470 400000-INCLUIR                  SECTION.
002480*-----------------------------------------------------------------
002490     MOVE 0007                   TO PSC-ERRO-PGM
002500*
      *--> Desligar erro automatico do DB2
002520*
002530     EXEC SQL WHENEVER SQLERROR CONTINUE END-EXEC
002540*
002550     EXEC SQL
002560          INSERT INTO DB2SGT.EST_TIP_COTA_TIT
002570          ( CD_TIP_COTA_TIT
002580          , NR_VRS_TIP_COTA
002590          , TS_EST_TIP_COTA
002600          , CD_TIP_EST_COTA
002610          , CD_USU_ATL_EST
002620          , TX_OBS_EST_TIP )
002630          VALUES
002640          ( :SGTST028-CD-TIP-COTA-TIT
002650          , :SGTST028-NR-VRS-TIP-COTA
002660          , :SGTST028-TS-EST-TIP-COTA
002670          , :SGTST028-CD-TIP-EST-COTA
002680          , :SGTST028-CD-USU-ATL-EST
002690          , :SGTST028-TX-OBS-EST-TIP
002700            :TX-OBS-EST-TIP-N )
002710     END-EXEC
002720*
002730* Ligar erro automatico do DB2
002740*
002750     EXEC SQL WHENEVER SQLERROR   GO TO 999002-ERRO-DB2 END-EXEC
002760*
002770     EVALUATE SQLCODE
002780         WHEN -803
                   SET ERRO-CODIFICADO      TO TRUE
                   MOVE  0803               TO  CD-RTN-PGM
002790             MOVE 'Inclusão não efetuada, registro existente'
002800                              TO MSG-ERRO
002810             SET MSGKW999-MNT-VRV     TO TRUE
002820             PERFORM 999001-ERRO-001
002830         WHEN 0
002840              CONTINUE
002850         WHEN OTHER
002860* Aciona o tratamento padrao de erro padrao para os demais casos
002870              PERFORM 999002-ERRO-DB2
002880     END-EVALUATE
002890     PERFORM 980101-GUARDA-CACHE
002900     INITIALIZE DADOS-LOG
002910     MOVE 'I'                    TO SGTS014G-CD-TIP-ACAO
002920     PERFORM 980102-GRAVA-LOG
002930     .
002940 400000-SAI.
002950     EXIT.
002960*-----------------------------------------------------------------
002970 500000-ALTERAR                  SECTION.
002980*-----------------------------------------------------------------
002990     MOVE 0008                   TO PSC-ERRO-PGM
003000     EXEC SQL
003010       UPDATE DB2SGT.EST_TIP_COTA_TIT
003020       SET CD_TIP_EST_COTA    = :SGTST028-CD-TIP-EST-COTA
003030         , CD_USU_ATL_EST     = :SGTST028-CD-USU-ATL-EST
003040         , TX_OBS_EST_TIP     = :SGTST028-TX-OBS-EST-TIP
003050                                :TX-OBS-EST-TIP-N
003060          WHERE CD_TIP_COTA_TIT    = :SGTST028-CD-TIP-COTA-TIT
003070            AND NR_VRS_TIP_COTA    = :SGTST028-NR-VRS-TIP-COTA
003080            AND TS_EST_TIP_COTA    = :SGTST028-TS-EST-TIP-COTA
003090     END-EXEC
003100     IF  SQLCODE EQUAL +100
003110         INITIALIZE CHAVE-CACHE
               SET ERRO-CODIFICADO     TO TRUE
               MOVE  0100              TO  CD-RTN-PGM
003120         MOVE 'Alteração não efetuada, registro inexistente'
003130                                 TO MSG-ERRO
003140         SET MSGKW999-MNT-VRV    TO TRUE
003150         PERFORM 999001-ERRO-001
003160     END-IF
003170     PERFORM 980101-GUARDA-CACHE
003180     INITIALIZE DADOS-LOG
003190     MOVE 'A'                    TO SGTS014G-CD-TIP-ACAO
003200     PERFORM 980102-GRAVA-LOG
003210     .
003220 500000-SAI.
003230     EXIT.
003240*-----------------------------------------------------------------
003250 600000-EXCLUIR                  SECTION.
003260*-----------------------------------------------------------------
003270     MOVE 0009                   TO PSC-ERRO-PGM
003280     INITIALIZE DADOS-LOG
003290     MOVE 'E'                    TO SGTS014G-CD-TIP-ACAO
003300     PERFORM 980102-GRAVA-LOG
003310     EXEC SQL
003320          DELETE FROM DB2SGT.EST_TIP_COTA_TIT
003330          WHERE CD_TIP_COTA_TIT    = :SGTST028-CD-TIP-COTA-TIT
003340            AND NR_VRS_TIP_COTA    = :SGTST028-NR-VRS-TIP-COTA
003350            AND TS_EST_TIP_COTA    = :SGTST028-TS-EST-TIP-COTA
003360     END-EXEC
003370     INITIALIZE REG-CACHE
003380     IF  SQLCODE EQUAL +100
               SET ERRO-CODIFICADO     TO TRUE
               MOVE  0100              TO  CD-RTN-PGM
               MOVE 'Exclusão não efetuada, registro inexistente'
                                       TO MSG-ERRO
               SET MSGKW999-MNT-VRV    TO TRUE
               PERFORM 999001-ERRO-001
003430     END-IF
003440     .
003450 600000-SAI.
003460     EXIT.
003470*-----------------------------------------------------------------
003480 700000-CONSULTA                 SECTION.
003490*-----------------------------------------------------------------
003500     MOVE 0010                   TO PSC-ERRO-PGM
003510*    Campos nulos sao inicializados na consulta
003520     INITIALIZE SGTST028-TX-OBS-EST-TIP
003530     EXEC SQL
003540           SELECT CD_TIP_EST_COTA
003550                , CD_USU_ATL_EST
003560                , TX_OBS_EST_TIP
003570             INTO  :SGTST028-CD-TIP-EST-COTA
003580                 , :SGTST028-CD-USU-ATL-EST
003590                 , :SGTST028-TX-OBS-EST-TIP
003600                   :TX-OBS-EST-TIP-N
003610                 FROM DB2SGT.EST_TIP_COTA_TIT
003620          WHERE CD_TIP_COTA_TIT    = :SGTST028-CD-TIP-COTA-TIT
003630            AND NR_VRS_TIP_COTA    = :SGTST028-NR-VRS-TIP-COTA
003640            AND TS_EST_TIP_COTA    = :SGTST028-TS-EST-TIP-COTA
003650     END-EXEC
003660     IF  SQLCODE = +100
003670         INITIALIZE SGTST028-AREA-DADOS
               SET ERRO-CODIFICADO     TO TRUE
               MOVE  SQLCODE           TO  CD-RTN-PGM
               MOVE 'Estado do tipo da cota do título inexistente.'
                                       TO MSG-ERRO
               PERFORM 999001-ERRO-001
003710     END-IF
003720     PERFORM 980101-GUARDA-CACHE
003730     .
003740 700000-SAI.
003750     EXIT.
003760*-----------------------------------------------------------------
003770 800000-CONSULTA-EST-ATUAL       SECTION.
003780*-----------------------------------------------------------------
003790     MOVE 0011                   TO PSC-ERRO-PGM
003800*    Campos nulos sao inicializados na consulta
003810     INITIALIZE SGTST028-TX-OBS-EST-TIP
003820     EXEC SQL
003830           SELECT CD_TIP_EST_COTA
003840                , CD_USU_ATL_EST
003850                , TX_OBS_EST_TIP
003860                , TS_EST_TIP_COTA
003870             INTO  :SGTST028-CD-TIP-EST-COTA
003880                 , :SGTST028-CD-USU-ATL-EST
003890                 , :SGTST028-TX-OBS-EST-TIP
003900                   :TX-OBS-EST-TIP-N
003910                 , :SGTST028-TS-EST-TIP-COTA
003920                 FROM DB2SGT.EST_TIP_COTA_TIT
003930          WHERE CD_TIP_COTA_TIT    = :SGTST028-CD-TIP-COTA-TIT
003940            AND NR_VRS_TIP_COTA    = :SGTST028-NR-VRS-TIP-COTA
003950          ORDER BY
003960                TS_EST_TIP_COTA DESC
003970          FETCH FIRST 1 ROW ONLY
003980     END-EXEC
003990     IF  SQLCODE = +100
               INITIALIZE SGTST028-AREA-DADOS
               SET ERRO-CODIFICADO     TO TRUE
               MOVE  SQLCODE           TO  CD-RTN-PGM
               MOVE 'Estado do tipo da cota do título inexistente'
                                       TO MSG-ERRO
               PERFORM 999001-ERRO-001
004040     END-IF
004050     PERFORM 980101-GUARDA-CACHE
004060     .
004070 800000-SAI.
004080     EXIT.
004090*-----------------------------------------------------------------
004100 900000-CONSULTA-EST-INCL        SECTION.
004110*-----------------------------------------------------------------
004120     MOVE 0012                   TO PSC-ERRO-PGM
004130*    Campos nulos sao inicializados na consulta
004140     INITIALIZE SGTST028-TX-OBS-EST-TIP
004150     EXEC SQL
004160           SELECT CD_TIP_EST_COTA
004170                , CD_USU_ATL_EST
004180                , TX_OBS_EST_TIP
004190                , TS_EST_TIP_COTA
004200             INTO  :SGTST028-CD-TIP-EST-COTA
004210                 , :SGTST028-CD-USU-ATL-EST
004220                 , :SGTST028-TX-OBS-EST-TIP
004230                   :TX-OBS-EST-TIP-N
004240                 , :SGTST028-TS-EST-TIP-COTA
004250                 FROM DB2SGT.EST_TIP_COTA_TIT
004260          WHERE CD_TIP_COTA_TIT    = :SGTST028-CD-TIP-COTA-TIT
004270            AND NR_VRS_TIP_COTA    = :SGTST028-NR-VRS-TIP-COTA
004280          ORDER BY
004290                TS_EST_TIP_COTA
004300          FETCH FIRST 1 ROW ONLY
004310     END-EXEC
004320     IF  SQLCODE = +100
               INITIALIZE SGTST028-AREA-DADOS
               SET ERRO-CODIFICADO     TO TRUE
               MOVE  SQLCODE           TO  CD-RTN-PGM
               MOVE 'Estado do tipo da cota do título inexistente.'
                                       TO MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF
004380     PERFORM 980101-GUARDA-CACHE
004390     .
004400 900000-SAI.
004410     EXIT.
004420*-----------------------------------------------------------------
004430 910000-VRFR-ULT-EST-COTA        SECTION.
004440*-----------------------------------------------------------------
004450     MOVE 0013                   TO PSC-ERRO-PGM
004460*    Campos nulos sao inicializados na consulta
004470     INITIALIZE SGTST028-TX-OBS-EST-TIP
004480     EXEC SQL
004490           SELECT CD_TIP_EST_COTA
004500                , CD_USU_ATL_EST
004510                , TX_OBS_EST_TIP
004520                , TS_EST_TIP_COTA
004530             INTO  :SGTST028-CD-TIP-EST-COTA
004540                 , :SGTST028-CD-USU-ATL-EST
004550                 , :SGTST028-TX-OBS-EST-TIP
004560                   :TX-OBS-EST-TIP-N
004570                 , :SGTST028-TS-EST-TIP-COTA
004580                 FROM DB2SGT.EST_TIP_COTA_TIT
004590          WHERE CD_TIP_COTA_TIT    = :SGTST028-CD-TIP-COTA-TIT
004600          ORDER BY
004610                TS_EST_TIP_COTA DESC
004620          FETCH FIRST 1 ROW ONLY
004630     END-EXEC
004640     IF  SQLCODE = +100
004650         INITIALIZE SGTST028-AREA-DADOS
               SET ERRO-CODIFICADO     TO TRUE
004660         MOVE 'Estado tipo cota título inexistente' TO MSG-ERRO
004670         MOVE SQLCODE                   TO CD-RTN-PGM
004680         PERFORM 999001-ERRO-001
004690     END-IF
004700     PERFORM 980101-GUARDA-CACHE
004710     .
004720 910000-SAI.
004730     EXIT.
004740*-----------------------------------------------------------------
004750*-----------------------------------------------------------------
004760 980101-GUARDA-CACHE             SECTION.
004770*-----------------------------------------------------------------
004780     MOVE 0014                   TO PSC-ERRO-PGM
004790
004800     MOVE SGTST028-CHAVE         TO CHAVE-CACHE
004810     MOVE SGTST028-AREA-DADOS         TO DADOS-CACHE
004820     .
004830 980101-SAI.
004840     EXIT.
004850*-----------------------------------------------------------------
004860 980102-GRAVA-LOG                SECTION.
004870*-----------------------------------------------------------------
004880     MOVE 0015                   TO PSC-ERRO-PGM
004890     MOVE 'EST_TIP_COTA_TIT  '        TO SGTS014G-NM-ENT
004900     IF  EXECUCAO-ONLINE
004900         MOVE SGTST028-CD-USU-ATL-EST TO SGTS014G-CD-CHV-USU
004910*        EXEC CICS
004920*             ASSIGN USERID(SGTS014G-CD-CHV-USU)
004930*        END-EXEC
004940     ELSE
004950         MOVE 'SYSTEM'                TO SGTS014G-CD-CHV-USU
004960     END-IF
004970     MOVE 'SGTST028'                  TO SGTS014G-NM-PGM
004980     MOVE SGTST028-CD-TIP-COTA-TIT       TO CD-TIP-COTA-TIT-LOG
004990     MOVE SGTST028-NR-VRS-TIP-COTA       TO NR-VRS-TIP-COTA-LOG
005000     MOVE SGTST028-TS-EST-TIP-COTA       TO TS-EST-TIP-COTA-LOG
005010     STRING CD-TIP-COTA-TIT-LOG
005020     ';'    NR-VRS-TIP-COTA-LOG
005030     ';'    TS-EST-TIP-COTA-LOG
005040         DELIMITED BY SIZE INTO SGTS014G-CHV-PRMR
005050     MOVE LENGTH OF DADOS-LOG    TO EIBCALEN
005060     CALL SGTS014G USING DFHEIBLK DADOS-LOG

           IF  SGTS014G-CD-RTN-PGM  NOT EQUAL ZEROS
               SET   ERRO-DE-PGM           TO  TRUE
               MOVE  SGTS014G              TO  PGM-ERRO
               MOVE  0112                  TO  CD-RTN-PGM
               MOVE  'Erro na gravação das informações do Log.'
                                           TO  MSG-ERRO
               MOVE SGTS014G               TO  MSG1-NM-PGM-ACND
               MOVE SGTS014G-CD-RTN-PGM    TO  MSG1-CD-RTN-PGM-ACND
               MOVE SGTS014G-MSG-ERRO      TO  MSG1-TX-RTN-PGM-ACND
               SET MSGKW999-MNT-VRV        TO TRUE
               PERFORM 999001-ERRO-001
           END-IF
           .
       980102-SAI.
           EXIT.

005120*--------------------
005130* 999000-ERRO SECTION
005140*--------------------
005150*-----------------
005160* 999001-ERRO-001.
005170*-----------------
-INC HLPKE004                                                           00005180
-INC HLPKE012                                                           00005190

           PERFORM  999999-ENCERRAR
           .
       999001-SAI.
           EXIT.

      *-----------------
      * 999002-ERRO-DB2
      *-----------------
-INC HLPKE005                                                           00005260
      *-----------------
      * 999003-TRATA-ABEND-CICS
      * 999004-TRATA-CONDITION-CICS
      *-----------------
-INC HLPKE006                                                           00005310
      *-----------------
      *-----------------------------------------------------------------
      *    As Sections e Books abaixo fazem o tratamento de erro
      *    através do Sistema MSG (rotina MSGS0001)
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       999000-TRATAR-ERRO-MSG          SECTION.
      *--> Envia a Ocorrência de Erro para o Sistema de Mensageria
      *-----------------------------------------------------------------

      *--> Alterar o MOVE conforme o Book do módulo
           MOVE  SGTST028-TIP-RTN-PGM  TO  TIP-RTN-PGM
      *--> Alterar os Campos de Entrada conforme o Book do módulo
           PERFORM  999100-MONITORAR-ENTRADA

      *--> Movimenta Campos Básicos da Ocorrência de Erro
           MOVE  CD-RTN-PGM            TO  MSG1-CD-ERRO-TS
           MOVE  PSC-ERRO-PGM          TO  MSG1-CD-SEQL-ERRO
           MOVE  MSG-ERRO              TO  MSG1-TX-ERRO-TS

      *--> Realiza a tradução do tipo de erro do COMMAREA
      *    para tipos de erro da Mensageria
           EVALUATE  TRUE
               WHEN  ERRO-DE-DB2
                     PERFORM  999200-MONITORAR-SQL
               WHEN  ERRO-DE-CICS
                     PERFORM  999300-MONITORAR-CICS
               WHEN  ERRO-DE-PGM
                     SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
                     PERFORM  999400-MONITORAR-VARIAVEIS
               WHEN  ERRO-CODIFICADO
                     SET   ERRO-PROGRAMADO
                                       TO TRUE
               WHEN  OTHER
                     CONTINUE
           END-EVALUATE

      *--> Grava o erro no Sistema MSG
           PERFORM  MSG1-NTF-MSG
           .
       999000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999100-MONITORAR-ENTRADA        SECTION.
      *--> Monitora as variáveis de entrada desde módulo
      *    Alterar os Campos conforme o Book
      *-----------------------------------------------------------------

           MOVE 'SGTST028-CD-CHV-USU'            TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST028-CD-CHV-USU             TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTST028-ACAO'                  TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST028-ACAO                   TO  MCR-S9-15
           MOVE  MCR-S9-15                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTST028-CD-TIP-COTA-TIT'    TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST028-CD-TIP-COTA-TIT         TO  MCR-S9-15
           MOVE  MCR-S9-15                     TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTST028-NR-VRS-TIP-COTA'       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST028-NR-VRS-TIP-COTA        TO  MCR-S9-15
           MOVE  MCR-S9-15                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTST028-TS-EST-TIP-COTA'       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST028-TS-EST-TIP-COTA        TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTST028-CD-TIP-EST-COTA'       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST028-CD-TIP-EST-COTA        TO  MCR-S9-15
           MOVE  MCR-S9-15                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTST028-CD-USU-ATL-EST'        TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST028-CD-USU-ATL-EST         TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTST028-TX-OBS-EST-TIP'        TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST028-TX-OBS-EST-TIP         TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT
           .
       999100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999200-MONITORAR-SQL            SECTION.
      *--> Monitora as variáveis do SQLCA (SQL Communication Area)
      *-----------------------------------------------------------------

      *--> Aciona a Monitoração das Variáveis de Erro SQL no MSGS0001
           SET   ERRO-SQL              TO  TRUE

           MOVE  SQLCA                 TO  MSG1-SQLCA

           MOVE 'SQLCODE'              TO  MSGKW999-NM-VRV-MNT
           MOVE  SQLCODE               TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SQLSTATE'             TO  MSGKW999-NM-VRV-MNT
           MOVE  SQLSTATE              TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           PERFORM  VARYING  IC-TAB
                    FROM  0001 BY 0001
                    UNTIL  IC-TAB          GREATER 0012
                    IF  HLPKE001-TAB-ERRO-TX(IC-TAB)
                                           NOT EQUAL SPACES
                        STRING  'DSNTIAR-TX-ERRO(' IC-TAB ')'
                                DELIMITED BY SIZE
                                INTO       MSGKW999-NM-VRV-MNT
                        END-STRING
                        MOVE  HLPKE001-TAB-ERRO-TX(IC-TAB)
                                       TO  MSGKW999-VL-VRV-MNT
                        PERFORM  999500-ADICIONA-VRV-MNT
                    END-IF
           END-PERFORM

           IF  SQLERRML                GREATER  ZEROS
               MOVE 'SQLERRML'         TO  MSGKW999-NM-VRV-MNT
               MOVE  SQLERRML          TO  MSGKW999-VL-VRV-MNT
               PERFORM  999500-ADICIONA-VRV-MNT
           END-IF

           IF  SQLWARN0                EQUAL 'W'
               MOVE 'SQLWARN'          TO  MSGKW999-NM-VRV-MNT
               MOVE  SQLWARN(2:7)      TO  MSGKW999-VL-VRV-MNT
               PERFORM  999500-ADICIONA-VRV-MNT
           END-IF
           .
       999200-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999300-MONITORAR-CICS           SECTION.
      *--> Monitora as variáveis ABEND ou CONDITION do CICS
      *-----------------------------------------------------------------

      *--> Aciona a Monitoração das Variáveis de Erro CICS no MSGS0001
           SET   ERRO-CICS             TO  TRUE

      *--> Tratar e Adicionar EIBRESP 01 e EIBRESP 02
           MOVE  EIBRESP               TO  MSG1-RESP1
           MOVE  EIBRESP2              TO  MSG1-RESP2

           MOVE  MSG1-RESP1            TO  MSGKW999-NUMERO-Z17
           PERFORM  999600-ALINHA-NUMERO-ESQUERDA
           MOVE  MSGKW999-NUMERO-X17   TO  MSGKW999-RESP1-X8

           MOVE  MSG1-RESP2            TO  MSGKW999-NUMERO-Z17
           PERFORM  999600-ALINHA-NUMERO-ESQUERDA
           MOVE  MSGKW999-NUMERO-X17    TO  MSGKW999-RESP2-X8

           MOVE 'EIBRESP 01'           TO  MSGKW999-NM-VRV-MNT
           MOVE  MSGKW999-RESP1-X8     TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'EIBRESP 02'           TO  MSGKW999-NM-VRV-MNT
           MOVE  MSGKW999-RESP2-X8     TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

      *--> Adicionar Outros Campos EIB (Exec Interface Block)
           MOVE 'EIBFN'                TO  MSGKW999-NM-VRV-MNT
           MOVE  EIBFN                 TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'EIBDS'                TO  MSGKW999-NM-VRV-MNT
           MOVE  EIBDS                 TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'EIBRSRCE'             TO  MSGKW999-NM-VRV-MNT
           MOVE  EIBRSRCE              TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'HLPS0201-TX-EIBFN'    TO  MSGKW999-NM-VRV-MNT
           MOVE  HLPS0201-TX-EIBFN     TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'HLPS0201-TX-EIBRESP'  TO  MSGKW999-NM-VRV-MNT
           MOVE  HLPS0201-TX-EIBRESP   TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'Nome CICS'            TO  MSGKW999-NM-VRV-MNT
           MOVE  HLPKE001-NM-CICS      TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'Usuario CICS'         TO  MSGKW999-NM-VRV-MNT
           MOVE  HLPKE001-CHV-USU      TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT
           .
       999300-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999400-MONITORAR-VARIAVEIS       SECTION.
      *--> Monitora as variáveis dos módulos chamados
      *-----------------------------------------------------------------

           EVALUATE  PGM-ERRO
               WHEN  SGTS014G
                     MOVE  'SGTS014G-CD-TIP-ACAO'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS014G-CD-TIP-ACAO
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE  'SGTS014G-CD-CHV-USU'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS014G-CD-CHV-USU
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE  'SGTS014G-NM-PGM'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS014G-NM-PGM
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE  'SGTS014G-CHV-PRMR'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS014G-CHV-PRMR
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTS014G-PSC-ERRO-PGM'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS014G-PSC-ERRO-PGM
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTS014G-CD-RTN-PGM'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS014G-CD-RTN-PGM
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTS014G-MSG-ERRO'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS014G-MSG-ERRO
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTS014G-TIP-RTN-PGM'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS014G-TIP-RTN-PGM
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTS014G-ATB-MSG'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS014G-ATB-MSG
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTS014G-TX-IDFC-ATNT-MSG'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS014G-TX-IDFC-ATNT-MSG
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

               WHEN SBTIMEDT
                    MOVE  'HLPKE001-CD-FUC'  TO  MSGKW999-NM-VRV-MNT
                    MOVE  HLPKE001-CD-FUC    TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999500-ADICIONA-VRV-MNT

                    MOVE  'HLPKE001-AREA-TRB' TO  MSGKW999-NM-VRV-MNT
                    MOVE  HLPKE001-AREA-TRB   TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999500-ADICIONA-VRV-MNT

                    MOVE  'HLPKE001-FMT-TS'   TO  MSGKW999-NM-VRV-MNT
                    MOVE  HLPKE001-FMT-TS     TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999500-ADICIONA-VRV-MNT

                    MOVE  'HLPKE001-TAM-TS'   TO  MSGKW999-NM-VRV-MNT
                    MOVE  HLPKE001-TAM-TS     TO   MSGKW999-VL-VRV-MNT
                    PERFORM  999500-ADICIONA-VRV-MNT

                    MOVE  'HLPKE001-TS-FMTD'  TO  MSGKW999-NM-VRV-MNT
                    MOVE  HLPKE001-TS-FMTD    TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999500-ADICIONA-VRV-MNT

               WHEN  OTHER
                    CONTINUE
           END-EVALUATE
           .
       999400-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999500-ADICIONA-VRV-MNT         SECTION.
      *-----------------------------------------------------------------
           IF  MSGKW999-NM-VRV-MNT         EQUAL SPACES
               MOVE  SPACES            TO  MSGKW999-VL-VRV-MNT
           ELSE
               IF  MSG1-QT-VRV-MNT         LESS 100
                   ADD   0001          TO MSG1-QT-VRV-MNT
                   MOVE  MSGKW999-NM-VRV-MNT
                     TO  MSG1-NM-VRV-MON(MSG1-QT-VRV-MNT)
                   MOVE  MSGKW999-VL-VRV-MNT
                     TO  MSG1-VL-VRV-MON(MSG1-QT-VRV-MNT)
               END-IF
               MOVE SPACES             TO MSGKW999-NM-VRV-MNT
                                          MSGKW999-VL-VRV-MNT
           END-IF
           .
       999500-ADICIONA-VRV-MNT-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999600-ALINHA-NUMERO-ESQUERDA   SECTION.
      *-----------------------------------------------------------------
           MOVE  MSGKW999-NUMERO-Z17   TO  MSGKW999-NUMERO-X17

           UNSTRING  MSGKW999-NUMERO-X17
                     DELIMITED BY ALL ' '
                     INTO MSGKW999-BRANCOS MSGKW999-NUMERO-X17
           END-UNSTRING
           .
       999600-SAI.
           EXIT.

      *----------------------------------------
      * MSG1-NTF-MSG                   SECTION.
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
      *----------------------------------------
-INC MSGKPMG1

      *-----------------------------------------------------------------
      *    Fim do Tratamento através do Sistema MSG (rotina MSGS0001)
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       999999-ENCERRAR                 SECTION.
      *-----------------------------------------------------------------

      * 1 - Erro de db2
           IF TIP-RTN-PGM   EQUAL 1
              IF CD-RTN-PGM NOT EQUAL 100 AND -803 AND +803
                 MOVE 9997    TO   CD-RTN-PGM
              END-IF
           END-IF

      * 2 - erro cics
           IF TIP-RTN-PGM EQUAL 2
              IF CD-RTN-PGM  EQUAL ZEROS OR 333
                 MOVE 9999    TO   CD-RTN-PGM
              END-IF
           END-IF

           MOVE  RTN-PGM               TO  SGTST028-RTN-PGM

           IF  EXECUCAO-ONLINE  AND   MSGKW999-MNT-VRV
               PERFORM  999000-TRATAR-ERRO-MSG
           END-IF

           GOBACK
           .
       999999-SAI.
           EXIT.
005360*---------------------- FIM SGTST028 ----------------------------