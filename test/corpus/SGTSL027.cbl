000010*-----------------------------------------------------------------
000020* PROGRAMA..: SGTSL027
000030* ANALISTA..: F8650584 - ROBERTO FERNANDES LIMA
000040* AUTOR.....: F8650584 - ROBERTO FERNANDES LIMA
000050* OBJETIVO..: CONSULTA TABELA DB2SGT.TIP_COTA_TIT_OPR
000060* COMPILACAO: COBOL/CICS/DB2
000070*-----------------------------------------------------------------
      * VRS002 23.05.2019 - F0950413 DE 2016/145195 -Inclusão na section
      *        999001-ERRO-001, '1 - Erro de db2' e '2 - erro cics', e
      *        adequação das mensagens de erro ao padrão da mensageria.
000080* VRS001 04.08.2015 - F8650584 - IMPLANTACAO
000070*-----------------------------------------------------------------
000090
000100*-----------------------------------------------------------------
000110 IDENTIFICATION DIVISION.
000120*-----------------------------------------------------------------
000130 PROGRAM-ID. SGTSL027.
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
000290 01  CTE-PROG     PIC  X(17) VALUE '*** SGTSL027 ***'.
000300 01  CTE-VERS     PIC  X(06) VALUE 'VRS002'.
000310
000320*-----------------------------------------------------------------
000330* Book do programa de ambiente - CICS5000
000340* Definido na work para preservar as informacoes
-INC HLPKE01B                                                           00000350
000360 LOCAL-STORAGE SECTION.
000370*-----------------------------------------------------------------
      *--> Area de trabalho
       01  IC-TAB                      PIC  9(2)        VALUE 0.
000400 01 QT-REG-SELD                  PIC S9(4) COMP-5 VALUE 0.
000410
000420*--> Campos utilizados para leitura
000430
000440 01  REG-WS.
000450     03 IN-TIP-COTA-ATI                PIC X(1) VALUE ' '.
000460     03 NM-TIP-COTA-TIT                PIC X(40) VALUE ' '.
000470     03 NR-VRS-TIP-COTA                PIC S9(4) COMP-5 VALUE 0.
000480
000490*--> Indicadores de nulidades
000500
000510 01 QT-REG-SELD-N                       PIC S9(4) COMP-5 VALUE 0.
000520
000530*--> Variavel(is) de controle do cursor(es)
000540
000550 01  CTL-CURSOR-PSQ.
000560     03  CD-FIM-CURSOR-PSQ            PIC S9(04) COMP-5 VALUE 0.
000570         88  FIM-CURSOR-PSQ            VALUE +100.
000580     03  IN-CURSOR-PSQ-ABTO           PIC  X(01) VALUE 'N'.
000590         88  CURSOR-PSQ-ABTO          VALUE 'S'.
000600
000630* Books da definicao de dados a serem usados na subrotina de erros
000640
-INC HLPKE001                                                           00000650

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
000660
000670     EXEC SQL
000680         INCLUDE SQLCA
000690     END-EXEC.
000700
000710*-----------------------------------------------------------------
000720 LINKAGE SECTION.
000730*-----------------------------------------------------------------
000740 01  DFHCOMMAREA.
-INC SGTKL027                                                           00000750
000760     EXEC SQL
000770       DECLARE CURSOR-PSQ CURSOR  FOR
000780       SELECT A.IN_TIP_COTA_ATI
000790            , A.NM_TIP_COTA_TIT
000800            , A.NR_VRS_TIP_COTA
000810         FROM DB2SGT.TIP_COTA_TIT_OPR   A
000820        WHERE A.CD_TIP_COTA_TIT    = :SGTSL027-CD-TIP-COTA-TIT
000830        AND    A.NR_VRS_TIP_COTA > :SGTSL027-ULT-NR-VRS-TIP-COTA
000840        ORDER BY A.NR_VRS_TIP_COTA
000850     END-EXEC.
000860
000870*-----------------------------------------------------------------
000880 PROCEDURE DIVISION USING DFHCOMMAREA.
000890*-----------------------------------------------------------------
000900     IF  EIBCALEN NOT EQUAL LENGTH OF DFHCOMMAREA
               SET ERRO-CODIFICADO     TO TRUE
               MOVE 0220               TO  CD-RTN-PGM
               MOVE 'Area com tamanho inválido - verifique book/local.'
                                       TO MSG-ERRO
               SET MSGKW999-MNT-VRV    TO TRUE
               PERFORM 999001-ERRO-001
000940     END-IF
000950     IF NOT EXECUCAO-ONLINE AND NOT EXECUCAO-BATCH
000960       CALL CICS5000 USING CICS5000-COMMAREA
000970     END-IF

      *--> WHENEVER DB2
-INC HLPKE002

      *--> WHENEVER CICS
-INC HLPKE003
001020
001030     PERFORM 100000-VALIDAR-PARAMETRO
001040
001050     PERFORM 970101-ABRE-CURSOR-PSQ
001060     PERFORM 970102-LER-CURSOR-PSQ
001070     PERFORM 200000-PROCESSA-PESQUISA UNTIL FIM-CURSOR-PSQ
001080              OR SGTSL027-PREENCHIDO-TAB
001090     IF  NOT FIM-CURSOR-PSQ
001100         MOVE 'S'                TO SGTSL027-IN-CNTO-PSQ
001110     END-IF
001120     PERFORM 970103-FECHA-CURSOR-PSQ
001130
001140     GOBACK
001150     .
001160*-----------------------------------------------------------------
001170 100000-VALIDAR-PARAMETRO SECTION.
001180*-----------------------------------------------------------------
001190     MOVE 0001                   TO PSC-ERRO-PGM

001200     INITIALIZE SGTSL027-AREA-DADOS
001210     MOVE 'N'                    TO SGTSL027-IN-CNTO-PSQ

001220     IF  SGTSL027-CD-TIP-COTA-TIT EQUAL ZEROS
               SET ERRO-CODIFICADO     TO TRUE
               MOVE  0257              TO  CD-RTN-PGM
               MOVE 'Código do tipo de cota do título da operação não in
      -             'formado.'         TO MSG-ERRO
               SET MSGKW999-MNT-VRV    TO TRUE
001650         PERFORM 999001-ERRO-001
001250     END-IF
001270     .
001280 100000-SAI.
001290     EXIT.
001300*-----------------------------------------------------------------
001310 200000-PROCESSA-PESQUISA SECTION.
001320*-----------------------------------------------------------------
001330     MOVE 0002                   TO PSC-ERRO-PGM
001340
001350     ADD  1                 TO SGTSL027-QT-OCR-TAB SGTSL027-IC-TAB
001360     IF  SGTSL027-ESTOURO-TAB
               SET ERRO-CODIFICADO     TO TRUE
               MOVE  0192              TO  CD-RTN-PGM
               MOVE 'Tabela da linkage estourou' TO MSG-ERRO
               SET MSGKW999-MNT-VRV    TO TRUE
001380         PERFORM 999001-ERRO-001
001390     END-IF

001400     MOVE IN-TIP-COTA-ATI              TO
001410               SGTSL027-IN-TIP-COTA-ATI(SGTSL027-IC-TAB)
001420     MOVE NM-TIP-COTA-TIT              TO
001430               SGTSL027-NM-TIP-COTA-TIT(SGTSL027-IC-TAB)
001440     MOVE NR-VRS-TIP-COTA              TO
001450               SGTSL027-NR-VRS-TIP-COTA(SGTSL027-IC-TAB)
001460     PERFORM 970102-LER-CURSOR-PSQ
001470     .
001480 200000-SAI.
001490     EXIT.
001500*-----------------------------------------------------------------
001510 970101-ABRE-CURSOR-PSQ SECTION.
001520*-----------------------------------------------------------------
001530     MOVE 0003                   TO PSC-ERRO-PGM
001540     EXEC SQL
001550          OPEN CURSOR-PSQ
001560     END-EXEC
001570     MOVE 'S'                    TO IN-CURSOR-PSQ-ABTO
001580     .
001590 970101-SAI.
001600     EXIT.
001610*-----------------------------------------------------------------
001620 970102-LER-CURSOR-PSQ SECTION.
001630*-----------------------------------------------------------------
001640     MOVE 0004                   TO PSC-ERRO-PGM
001650     INITIALIZE REG-WS
001660     EXEC SQL
001670          FETCH CURSOR-PSQ
001680         INTO :IN-TIP-COTA-ATI
001690            , :NM-TIP-COTA-TIT
001700            , :NR-VRS-TIP-COTA
001710     END-EXEC
001720     MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ
001730     .
001740 970102-SAI.
001750     EXIT.
001760*-----------------------------------------------------------------
001770 970103-FECHA-CURSOR-PSQ SECTION.
001780*-----------------------------------------------------------------
001790     MOVE 0005                   TO PSC-ERRO-PGM
001800     MOVE 'N'                    TO IN-CURSOR-PSQ-ABTO
001810     EXEC SQL
001820          CLOSE CURSOR-PSQ
001830     END-EXEC
001840     .
001850 970103-SAI.
001860     EXIT.
001870*--------------------
001880* 999000-ERRO SECTION
001890*--------------------
001900*-----------------
001910* 999001-ERRO-001.
001920*-----------------
-INC HLPKE004                                                           00001930
-INC HLPKE012                                                           00001940

           PERFORM  999999-ENCERRAR
           .
       999001-SAI.
           EXIT.
002000
002010*-----------------
002020* 999002-ERRO-DB2
002030*-----------------
-INC HLPKE005                                                           00002040
002050*-----------------
002060* 999003-TRATA-ABEND-CICS
002070* 999004-TRATA-CONDITION-CICS
002080*-----------------
-INC HLPKE006                                                           00002090
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
           MOVE  SGTSL027-TIP-RTN-PGM  TO  TIP-RTN-PGM
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

           MOVE 'SGTSL027-CD-TIP-COTA-TIT'       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL027-CD-TIP-COTA-TIT        TO  MCR-S9-15
           MOVE  MCR-S9-15                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTSL027-ULT-NR-VRS-TIP-COTA'   TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL027-ULT-NR-VRS-TIP-COTA    TO  MCR-S9-15
           MOVE  MCR-S9-15                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTSL027-QT-OCR-TAB'            TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL027-QT-OCR-TAB             TO  MCR-S9-15
           MOVE  MCR-S9-15                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTSL027-IC-TAB'                TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL027-IC-TAB                 TO  MCR-S9-15
           MOVE  MCR-S9-15                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTSL027-IN-CNTO-PSQ'           TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL027-IN-CNTO-PSQ            TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           PERFORM VARYING SGTSL027-IC-TAB FROM 1 BY 1
                     UNTIL SGTSL027-IC-TAB > SGTSL027-QT-OCR-TAB

              MOVE 'SGTSL027-IC-TAB'             TO  MSGKW999-NM-VRV-MNT
              MOVE  SGTSL027-IC-TAB              TO  MCR-S9-15
              MOVE  MCR-S9-15                    TO  MSGKW999-VL-VRV-MNT
              PERFORM  999500-ADICIONA-VRV-MNT

              MOVE 'SGTSL027-IN-TIP-COTA-ATI'    TO  MSGKW999-NM-VRV-MNT
              MOVE  SGTSL027-IN-TIP-COTA-ATI (SGTSL027-IC-TAB)
                                                 TO  MSGKW999-VL-VRV-MNT
              PERFORM  999500-ADICIONA-VRV-MNT

              MOVE 'SGTSL027-NM-TIP-COTA-TIT'    TO  MSGKW999-NM-VRV-MNT
              MOVE  SGTSL027-NM-TIP-COTA-TIT (SGTSL027-IC-TAB)
                                                 TO  MSGKW999-VL-VRV-MNT
              PERFORM  999500-ADICIONA-VRV-MNT

              MOVE 'SGTSL027-NR-VRS-TIP-COTA'    TO  MSGKW999-NM-VRV-MNT
              MOVE  SGTSL027-NR-VRS-TIP-COTA (SGTSL027-IC-TAB)
                                                 TO  MCR-S9-15
              MOVE  MCR-S9-15                    TO  MSGKW999-VL-VRV-MNT
              PERFORM  999500-ADICIONA-VRV-MNT
           END-PERFORM
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

           IF  CURSOR-PSQ-ABTO
               PERFORM 970103-FECHA-CURSOR-PSQ
           END-IF

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

           MOVE RTN-PGM                TO SGTSL027-RTN-PGM

           IF  EXECUCAO-ONLINE  AND   MSGKW999-MNT-VRV
               PERFORM  999000-TRATAR-ERRO-MSG
           END-IF

           GOBACK
           .
       999999-SAI.
           EXIT.
      *---------------------- FIM SGTSL027 ----------------------------