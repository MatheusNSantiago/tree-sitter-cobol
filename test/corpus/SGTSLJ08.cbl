000010*-----------------------------------------------------------------
000020* PROGRAMA..: SGTSLJ08
000030* ANALISTA..: F8608799 - RICARDO RAMOS
000040* AUTOR.....: F8658869 - ROBERTO IOSHIMOTO
000050* DATA......: 10.11.2015
000060* OBJETIVO..: CONSULTAR JOIN TAB PSS_CTT_EXNO E TEL_PSS_CTT_EXNO
000070* COMPILACAO: COBOL/CICS/SQL
000080*-----------------------------------------------------------------
      * VRS002 15.03.2018 - F8658869 DE 2016/145195 - Tratamento de men-
      *                                               sageria.
000090* VRS001 10.11.2015 - F8658869 DE 2013/120821 - IMPLANTAÇãO
000100*-----------------------------------------------------------------
000110
000120*-----------------------------------------------------------------
000130 IDENTIFICATION DIVISION.
000140*-----------------------------------------------------------------
000150 PROGRAM-ID. SGTSLJ08.
000160
000170*-----------------------------------------------------------------
000180 ENVIRONMENT DIVISION.
000190*-----------------------------------------------------------------
000200 CONFIGURATION                 SECTION.
000210 SPECIAL-NAMES.
000220     DECIMAL-POINT IS COMMA.
000230
000240*-----------------------------------------------------------------
000250 DATA DIVISION.
000260*-----------------------------------------------------------------
000270
000280*-----------------------------------------------------------------
000290 WORKING-STORAGE               SECTION.
000300*-----------------------------------------------------------------
       77  CTE-INICIO-SS          PIC  X(40)  VALUE
                   '*** S.S. COMECA AQUI ***'.
000310 77  CTE-PROG               PIC  X(17) VALUE '*** SGTSLJ08 ***'.
       77  CTE-VERS               PIC  X(06)  VALUE 'VRS002'.
000320*-----------------------------------------------------------------
000330* Book do programa de ambiente - CICS5000
000340* Definido na work para preservar as informaçoes
000350* -INC HLPKE01B
000360*
000370*-----------------------------------------------------------------
000380 LOCAL-STORAGE                 SECTION.
000390*-----------------------------------------------------------------
000400* Book de para acionamento do MSG1
000410*-----------------------------------------------------------------
-INC IIBKERRO                                                           00000420
-INC MSGKWMG1                                                           00000430
-INC HLPKW999
-INC SGTKERR1

000530* Area de trabalho
000540*
000550* Campos utilizados para leitura
000560*
000570 01  REG-WS.
000580     03  IN-CTT-EXNO-ATI         PIC  X(1)  VALUE ' '.
000590     03  TX-EMAIL-CTT-EXNO       PIC  X(50) VALUE ' '.
000600     03  NM-PSS-CTT-EXNO         PIC  X(60) VALUE ' '.
000610     03  NM-CIA-CTT-EXNO         PIC  X(60) VALUE ' '.
000620     03  CD-CNPJ                 PIC S9(14) COMP-3 VALUE 0.
000630     03  CD-CLI-CIA-SBTC         PIC S9(9) COMP-5 VALUE 0.
000640     03  NM-CRG-CTT-EXNO         PIC X(40)  VALUE ' '.
000650     03  CD-USU-RSP-ATL          PIC X(8)   VALUE ' '.
000660     03  TS-ATL-CTT-EXNO         PIC X(26)  VALUE ' '.
000670     03  NR-SEQL-CML             PIC S9(4) COMP-5 VALUE 0.
000680     03  CD-DDD-CML              PIC S9(4) COMP-5 VALUE 0.
000690     03  NR-TEL-CML              PIC S9(9) COMP-5 VALUE 0.
000700     03  NR-SEQL-CELR            PIC S9(4) COMP-5 VALUE 0.
000710     03  CD-DDD-CELR             PIC S9(4) COMP-5 VALUE 0.
000720     03  NR-TEL-CELR             PIC S9(9) COMP-5 VALUE 0.
000730*
000740* Indicadores de nulidades
000750*
000760 01  NM-CIA-CTT-EXNO-N       PIC S9(4) COMP-5 VALUE 0.
000770 01  CD-CNPJ-N               PIC S9(4) COMP-5 VALUE 0.
000780 01  CD-CLI-CIA-SBTC-N       PIC S9(4) COMP-5 VALUE 0.
000790 01  NM-CRG-CTT-EXNO-N       PIC S9(4) COMP-5 VALUE 0.
000800 01  NR-SEQL-CML-N           PIC S9(4) COMP-5 VALUE 0.
000810 01  CD-DDD-CML-N            PIC S9(4) COMP-5 VALUE 0.
000820 01  NR-TEL-CML-N            PIC S9(4) COMP-5 VALUE 0.
000830 01  NR-SEQL-CELR-N          PIC S9(4) COMP-5 VALUE 0.
000840 01  CD-DDD-CELR-N           PIC S9(4) COMP-5 VALUE 0.
000850 01  NR-TEL-CELR-N           PIC S9(4) COMP-5 VALUE 0.
000860*
000870* Variável(is) de controle do cursor(es)
000880*
000890* Area de comunicação
-INC SGTKLJ08                                                           00000900
000910*
000920* Subrotinas utilizadas
000930*
000940* Books da definição de dados a serem usados na subrotina de erros
000950*
000960*
000970     EXEC SQL
000980         INCLUDE SQLCA
000990     END-EXEC.

       01  CTE-FINAL-SS                PIC  X(40)      VALUE
                    '*** S.S. TERMINA AQUI ***'.
001010*-----------------------------------------------------------------
001020 PROCEDURE DIVISION.
001030*-----------------------------------------------------------------
001040 000000-PRINCIPAL              SECTION.
001050*-----------------------------------------------------------------
001060*
001070* Habilita tratamento automatico de erros DB2
-INC HLPKE997                                                           00001080
001090* Habilita captura automatica de abend e erros CICS
-INC HLPKE999                                                           00001100
001110*
001120     PERFORM 100000-DESCOBRIR-CANAL
001130     PERFORM 110000-EXCLUIR-CTNR-RPST
001140     PERFORM 120000-EXCLUIR-CTNR-ERRO
001150     PERFORM 130000-RECEBER-REQUISICAO
001160     PERFORM 140000-VALIDAR-PARAMETRO
001170*
001180     PERFORM 200000-PROCESSA-PESQUISA
001190*
001200     PERFORM 300000-POSTA-RESPOSTA
001210*
001220     PERFORM 999999-ENCERRAR
001230*
001240     GOBACK
001250     .
001260 000000-SAI.
001270     EXIT.
001280*-----------------------------------------------------------------
001290 100000-DESCOBRIR-CANAL        SECTION.
001300*-----------------------------------------------------------------
001310     MOVE  0001  TO  MSG1-CD-SEQL-ERRO
001320*
001330     EXEC CICS ASSIGN
001340        CHANNEL ( NM-CNL )
001350     END-EXEC
001360     .
001370 100000-SAI.
001380     EXIT.
001390*-----------------------------------------------------------------
001400 110000-EXCLUIR-CTNR-RPST      SECTION.
001410*-----------------------------------------------------------------
001420     MOVE  0002  TO  MSG1-CD-SEQL-ERRO
001430*
001440     EXEC CICS DELETE
001450        CONTAINER ( 'SGTSLJ08-RPST' )
001460        CHANNEL   ( NM-CNL          )
001470        NOHANDLE
001480     END-EXEC
001490*
001500     IF  NOT (EIBRESP EQUAL DFHRESP(NORMAL)   OR
001510         EIBRESP EQUAL DFHRESP(CONTAINERERR) )
001520         PERFORM 999002-TRATA-CONDITION-CICS
001530     END-IF
001540     .
001550 110000-SAI.
001560     EXIT.
001570*-----------------------------------------------------------------
001580 120000-EXCLUIR-CTNR-ERRO      SECTION.
001590*-----------------------------------------------------------------
001600     MOVE  0003  TO  MSG1-CD-SEQL-ERRO
001610*
001620     EXEC CICS DELETE
001630        CONTAINER ( 'PRM-ERRO' )
001640        CHANNEL   ( NM-CNL     )
001650        NOHANDLE
001660     END-EXEC
001670*
001680     IF  NOT (EIBRESP EQUAL DFHRESP(NORMAL)   OR
001690         EIBRESP EQUAL DFHRESP(CONTAINERERR) )
001700         PERFORM 999002-TRATA-CONDITION-CICS
001710     END-IF
001720     .
001730 120000-SAI.
001740     EXIT.
001750*-----------------------------------------------------------------
001760 130000-RECEBER-REQUISICAO     SECTION.
001770*-----------------------------------------------------------------
001780     MOVE  0004  TO  MSG1-CD-SEQL-ERRO
001790*
001800     MOVE  ZEROS TO  QT-CCT-LIDO
001810     MOVE  LENGTH OF SGTSLJ08-RQSC  TO QT-CCT-EPRO
001820*
001830     EXEC CICS GET
001840        CONTAINER ( 'SGTSLJ08-RQSC' )
001850        CHANNEL   ( NM-CNL          )
001860        NODATA
001870        FLENGTH   ( QT-CCT-LIDO     )
001880     END-EXEC
001890*
001900     IF  QT-CCT-LIDO  NOT EQUAL  QT-CCT-EPRO
001920         SET  ERRO-TAMANHO-AREA  TO  TRUE
002020         PERFORM 999000-GRAVA-ERRO-E-ENCERRA
002030     END-IF
002040*
002050     EXEC CICS GET
002060        CONTAINER ( 'SGTSLJ08-RQSC'         )
002070        CHANNEL   ( NM-CNL                  )
002080        FLENGTH   ( LENGTH OF SGTSLJ08-RQSC )
002090        INTO      ( SGTSLJ08-RQSC           )
002100     END-EXEC
002110     .
002120 130000-SAI.
002130     EXIT.
002140*-----------------------------------------------------------------
002150 140000-VALIDAR-PARAMETRO      SECTION.
002160*-----------------------------------------------------------------
002170     MOVE  0005  TO  MSG1-CD-SEQL-ERRO
002180*
002190     IF  SGTSLJ08-CD-PSS-CTT-EXNO  NOT GREATER  ZEROS
002200         SET  ERRO-PROGRAMADO  TO  TRUE
002210         MOVE  0059            TO  MSG1-CD-ERRO-TS
002230         MOVE 'Código da pessoa de contato externo inválido.'
002240                               TO  MSG1-TX-ERRO-TS
002250         PERFORM 999000-GRAVA-ERRO-E-ENCERRA
002260     END-IF
002270     .
002280 140000-SAI.
002290     EXIT.
002300*-----------------------------------------------------------------
002310 200000-PROCESSA-PESQUISA      SECTION.
002320*-----------------------------------------------------------------
002330     MOVE  0006  TO  MSG1-CD-SEQL-ERRO
002340*
002350     PERFORM 970102-LER-CURSOR-PSQ
002360*
002370     MOVE  IN-CTT-EXNO-ATI    TO  SGTSLJ08-IN-CTT-EXNO-ATI
002380     MOVE  TX-EMAIL-CTT-EXNO  TO  SGTSLJ08-TX-EMAIL-CTT-EXNO
002390     MOVE  NM-PSS-CTT-EXNO    TO  SGTSLJ08-NM-PSS-CTT-EXNO
002400     MOVE  NM-CIA-CTT-EXNO    TO  SGTSLJ08-NM-CIA-CTT-EXNO
002410     MOVE  CD-CNPJ            TO  SGTSLJ08-CD-CNPJ
002420     MOVE  CD-CLI-CIA-SBTC    TO  SGTSLJ08-CD-CLI-CIA-SBTC
002430     MOVE  NM-CRG-CTT-EXNO    TO  SGTSLJ08-NM-CRG-CTT-EXNO
002440     MOVE  CD-USU-RSP-ATL     TO  SGTSLJ08-CD-USU-RSP-ATL
002450     MOVE  TS-ATL-CTT-EXNO    TO  SGTSLJ08-TS-ATL-CTT-EXNO
002460     MOVE  NR-SEQL-CML        TO  SGTSLJ08-NR-SEQL-CML
002470     MOVE  CD-DDD-CML         TO  SGTSLJ08-CD-DDD-CML
002480     MOVE  NR-TEL-CML         TO  SGTSLJ08-NR-TEL-CML
002490     MOVE  NR-SEQL-CELR       TO  SGTSLJ08-NR-SEQL-CELR
002500     MOVE  CD-DDD-CELR        TO  SGTSLJ08-CD-DDD-CELR
002510     MOVE  NR-TEL-CELR        TO  SGTSLJ08-NR-TEL-CELR
002520     .
002530 200000-SAI.
002540     EXIT.
002550*-----------------------------------------------------------------
002560 300000-POSTA-RESPOSTA         SECTION.
002570*-----------------------------------------------------------------
002580     MOVE  0007  TO  MSG1-CD-SEQL-ERRO
002590*
002600     EXEC CICS PUT
002610        CONTAINER ( 'SGTSLJ08-RPST'         )
002620        CHANNEL   ( NM-CNL                  )
002630        FLENGTH   ( LENGTH OF SGTSLJ08-RPST )
002640        FROM      ( SGTSLJ08-RPST           )
002650     END-EXEC
002660     .
002670 300000-SAI.
002680     EXIT.
002690*-----------------------------------------------------------------
002700 970102-LER-CURSOR-PSQ         SECTION.
002710*-----------------------------------------------------------------
002720     MOVE  0008  TO  MSG1-CD-SEQL-ERRO
002730*
002740     INITIALIZE REG-WS
002750*
002760     EXEC SQL
002770       SELECT A.IN_CTT_EXNO_ATI
002780            , A.TX_EMAI_CTT_EXNO
002790            , A.NM_PSS_CTT_EXNO
002800            , A.NM_CIA_CTT_EXNO
002810            , A.CD_CNPJ
002820            , A.CD_CLI_CIA_SBTC
002830            , A.NM_CRG_CTT_EXNO
002840            , A.CD_USU_RSP_ATL
002850            , A.TS_ATL_CTT_EXNO
002860            , B.NR_SEQL_TEL_EXNO AS NR_SEQL_CML
002870            , B.CD_SEQL_DDD      AS CD_DDD_CML
002880            , B.NR_TEL_CTT_EXNO  AS NR_TEL_CML
002890            , E.NR_SEQL_TEL_EXNO AS NR_SEQL_CELR
002900            , E.CD_SEQL_DDD      AS CD_DDD_CELR
002910            , E.NR_TEL_CTT_EXNO  AS NR_TEL_CELR
002920         INTO :IN-CTT-EXNO-ATI
002930            , :TX-EMAIL-CTT-EXNO
002940            , :NM-PSS-CTT-EXNO
002950            , :NM-CIA-CTT-EXNO
002960              :NM-CIA-CTT-EXNO-N
002970            , :CD-CNPJ
002980              :CD-CNPJ-N
002990            , :CD-CLI-CIA-SBTC
003000              :CD-CLI-CIA-SBTC-N
003010            , :NM-CRG-CTT-EXNO
003020              :NM-CRG-CTT-EXNO-N
003030            , :CD-USU-RSP-ATL
003040            , :TS-ATL-CTT-EXNO
003050            , :NR-SEQL-CML
003060              :NR-SEQL-CML-N
003070            , :CD-DDD-CML
003080              :CD-DDD-CML-N
003090            , :NR-TEL-CML
003100              :NR-TEL-CML-N
003110            , :NR-SEQL-CELR
003120              :NR-SEQL-CELR-N
003130            , :CD-DDD-CELR
003140              :CD-DDD-CELR-N
003150            , :NR-TEL-CELR
003160              :NR-TEL-CELR-N
003170         FROM DB2SGT.PSS_CTT_EXNO A
003180        LEFT OUTER JOIN
003190*
003200*-- Recupera telefone comercial da pessoa - início
003210*
003220              (SELECT C.CD_PSS_CTT_EXNO
003230                    , C.NR_SEQL_TEL_EXNO
003240                    , C.CD_SEQL_DDD
003250                    , C.NR_TEL_CTT_EXNO
003260               FROM DB2SGT.TEL_PSS_CTT_EXNO  C
003270               INNER JOIN
003280*
003290*              -- Recupera sequencial primeira ocorrência de
003300*              -- telefone comercial por pessoa - início
003310*
003320                     (SELECT CD_PSS_CTT_EXNO
003330                          ,  MIN(NR_SEQL_TEL_EXNO) SEQL
003340                      FROM DB2SGT.TEL_PSS_CTT_EXNO
003350*   CD_TIP_TEL = 2 (Telefone tipo comercial)
003360                      WHERE CD_TIP_TEL = 2
003370                        AND CD_PSS_CTT_EXNO =
003380                            :SGTSLJ08-CD-PSS-CTT-EXNO
003390                      GROUP BY CD_PSS_CTT_EXNO )  D
003400*
003410*              -- Recupera primeira ocorrência de
003420*              -- telefone comercial por pessoa - FIM
003430*
003440               ON C.CD_PSS_CTT_EXNO  = D.CD_PSS_CTT_EXNO
003450              AND C.NR_SEQL_TEL_EXNO = D.SEQL
003460              WHERE C.CD_PSS_CTT_EXNO = :SGTSLJ08-CD-PSS-CTT-EXNO
003470              ORDER BY C.CD_PSS_CTT_EXNO )  B
003480*
003490*-- Recupera telefone comercial da pessoa - FIM
003500*
003510       ON  A.CD_PSS_CTT_EXNO = B.CD_PSS_CTT_EXNO
003520       LEFT OUTER JOIN
003530*
003540*-- Recupera telefone CELULAR da pessoa - INICIO
003550*
003560       (SELECT F.CD_PSS_CTT_EXNO
003570             , F.NR_SEQL_TEL_EXNO
003580             , F.CD_SEQL_DDD
003590             , F.NR_TEL_CTT_EXNO
003600         FROM DB2SGT.TEL_PSS_CTT_EXNO  F
003610         INNER JOIN
003620*
003630*    -- Recupera sequencial primeira ocorrência de
003640*    -- telefone CELULAR por pessoa - INICIO
003650*
003660            (SELECT CD_PSS_CTT_EXNO
003670                  , MIN(NR_SEQL_TEL_EXNO)  SEQL
003680             FROM  DB2SGT.TEL_PSS_CTT_EXNO
003690*  CD_TIP_TEL  = 1 (Telefone tipo celular)
003700             WHERE  CD_TIP_TEL     = 1
003710               AND  CD_PSS_CTT_EXNO = :SGTSLJ08-CD-PSS-CTT-EXNO
003720             GROUP BY CD_PSS_CTT_EXNO )  G
003730*
003740*    -- Recupera primeira ocorrência de
003750*    -- telefone CELULAR por pessoa - FIM
003760*
003770         ON   F.CD_PSS_CTT_EXNO = G.CD_PSS_CTT_EXNO
003780         AND  F.NR_SEQL_TEL_EXNO = G.SEQL
003790         WHERE F.CD_PSS_CTT_EXNO = :SGTSLJ08-CD-PSS-CTT-EXNO
003800         ORDER BY F.CD_PSS_CTT_EXNO)  E
003810*
003820*-- Recupera telefone CELULAR da pessoa - FIM
003830*
003840       ON  A.CD_PSS_CTT_EXNO     =  E.CD_PSS_CTT_EXNO
003850       WHERE A.CD_PSS_CTT_EXNO  = :SGTSLJ08-CD-PSS-CTT-EXNO
003860       ORDER BY A.CD_PSS_CTT_EXNO
003870     END-EXEC
003880*
003890     IF  SQLCODE  EQUAL  ZEROS
003900         CONTINUE
003910     ELSE
003920       IF  SQLCODE  EQUAL  +100
003930           MOVE  545 TO MSG1-CD-ERRO-TS
                 MOVE 'Código da pessoa de contato externo inexistente.'
003960                                    TO  MSG1-TX-ERRO-TS
003970           PERFORM 999000-GRAVA-ERRO-E-ENCERRA
003980       ELSE
003990           MOVE  529            TO  MSG1-CD-ERRO-TS
004010           MOVE  'Erro na consulta das pessoas de contato externo.
      -                ''             TO  MSG1-TX-ERRO-TS
004020           PERFORM 999000-GRAVA-ERRO-E-ENCERRA
004030       END-IF
004040     END-IF
004050     .
004060 970102-SAI.
004070     EXIT.
004080*--------------------
004090 999000-GRAVA-ERRO-E-ENCERRA   SECTION.
004100*--------------------
004110     MOVE  'SGTSLJ08-CD-PSS-CTT-EXNO'  TO  MSGKW999-NM-VRV-MNT
004120     MOVE  SGTSLJ08-CD-PSS-CTT-EXNO    TO  MSGKW999-VL-VRV-MNT
004130     PERFORM 999003-ADICIONA-VRV-MNT
004140*
004150* --> grava o erro no msg
004160*
004170     PERFORM MSG1-NTF-MSG
004180*
004190     PERFORM 999999-ENCERRAR
004200     .
004210 999000-SAI.
004220     EXIT.
004230* --------------------------------------*
004240* 999001-TRATA-ABEND-CICS        SECTION.
004250* 999002-TRATA-CONDITION-CICS    SECTION.
004260* 999003-ADICIONA-VRV-MNT        SECTION.
      * 999004-ALINHA-NUMERO-ESQUERDA  SECTION.
004270* --------------------------------------*
-INC HLPKP999                                                           00004280
004290* --------------------------------------*
004300* 999010-TRATA-ERRO-DB2          SECTION.
-INC HLPKP997
      * --------------------------------------*
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * MSG1-CD-ERRO-TS = 3000
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * MSG1-CD-ERRO-TS = 3001
      * 999006-TRATA-ERRO-CANAL        SECTION.
      * MSG1-CD-ERRO-TS = 3003
      *---------------------------------------*
-INC SGTKP999
004320* --------------------------------------*
004330* MSG1-NTF-MSG                 SECTION.
-INC MSGKPMG1                                                           00004340
004350*--------------------
004360* 999000-ERRO                  SECTION.
004370*--------------------
004380*-----------------
004390* 999001-ERRO-001.
004400*-----------------
004410*
004420     MOVE  MSG1-CD-ERRO-TS             TO  IIBKERRO-CD-ERRO
004430     MOVE  MSG1-TX-ERRO-TS             TO  IIBKERRO-TX-ERRO
004440*
004450     EXEC CICS PUT
004460           CONTAINER ( 'PRM-ERRO'                  )
004470           CHANNEL   ( NM-CNL                      )
004480           FROM      ( IIBKERRO-PRM-ERRO           )
004490           FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
004500           NOHANDLE
004510     END-EXEC
004520*
004530     PERFORM 999999-ENCERRAR
004540*
004550     GOBACK
004560     .

004640* --------------------------------------*
004650 999999-ENCERRAR             SECTION.
004660* --------------------------------------*
004670     EXEC CICS
004680          RETURN
004690     END-EXEC
004700     .
004710 999999-SAI.
004720     EXIT.