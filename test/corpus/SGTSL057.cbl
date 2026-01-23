000010*-----------------------------------------------------------------
000020* PROGRAMA..: SGTSL057
000030* ANALISTA..: F2054109 - CINTIA AP SOARES IGLESIAS
000040* AUTOR.....: F2804391 - ELIANA TEMPERANI
000050* OBJETIVO..: LISTAR PRODUTOS MODALIDADES PERMITIDOS PARA ORDEM DE
000060*           : COMPRA
000070* COMPILACAO: COBOL/CICS/DB2
000080*-----------------------------------------------------------------
      * VRS002 29/05/2019 F0950413 DE 2016/145195 - Inclusão na section
      *        999001-ERRO-001, '1 - Erro de db2' e '2 - erro cics', e
      *        adequação das mensagens de erro ao padrão da mensageria.
      * VRS001 25.11.2015 - F2804391 - DE 2013/120821 - IMPLANTACAO
      *-----------------------------------------------------------------
000100
000110*-----------------------------------------------------------------
000120 IDENTIFICATION DIVISION.
000130*-----------------------------------------------------------------
000140 PROGRAM-ID. SGTSL057.
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
000300 01  CTE-PROG     PIC  X(17) VALUE '*** SGTSL057 ***'.
       01  CTE-VERS     PIC  X(06) VALUE 'VRS002'.
000320
000330*-----------------------------------------------------------------
000340* Book do programa de ambiente - CICS5000
000350* Definido na work para preservar as informacoes
-INC HLPKE01B                                                           00000360
000370 LOCAL-STORAGE SECTION.
000380*-----------------------------------------------------------------
000400
      *--> Area de trabalho
       01  IC-TAB                      PIC  9(2)        VALUE 0.
000410 01 QT-REG-SELD                  PIC S9(4) COMP-5 VALUE 0.
000420
000430*--> Campos utilizados para leitura
000440
000450 01  REG-WS.
000460     03 CD-PRD                         PIC S9(4) COMP-5
000470        OCCURS 101 TIMES               VALUE 0.
000480     03 CD-MDLD                        PIC S9(4) COMP-5
000490        OCCURS 101 TIMES               VALUE 0.
000500
000510
000520*--> Indicadores de nulidades
000530
000540 01 QT-REG-SELD-N                       PIC S9(4) COMP-5 VALUE 0.
000550
000560*--> Variavel(is) de controle do cursor(es)
000570
000580 01  CTL-CURSOR-PSQ.
000590     03  CD-FIM-CURSOR-PSQ            PIC S9(04) COMP-5 VALUE 0.
000600         88  FIM-CURSOR-PSQ            VALUE +100.
000610     03  IN-CURSOR-PSQ-ABTO           PIC  X(01) VALUE 'N'.
000620         88  CURSOR-PSQ-ABTO          VALUE 'S'.
000630
      *--> Books definicao de dados a serem usados na subrotina de erros
-INC HLPKE001                                                           00000680

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
000690
000700     EXEC SQL
000710         INCLUDE SQLCA
000720     END-EXEC.
000730
000740*-----------------------------------------------------------------
000750 LINKAGE SECTION.
000760*-----------------------------------------------------------------
000770 01  DFHCOMMAREA.
-INC SGTKL057                                                           00000780
000790     EXEC SQL
000800       DECLARE CURSOR-PSQ CURSOR
000810               WITH ROWSET POSITIONING FOR
000820     SELECT DISTINCT CD_PRD
000830                    ,CD_MDLD
000840     FROM DB2SGT.ITEM_PMT_MOD_CPR
000850     WHERE CD_PRD  > : SGTSL057-CD-PRD-ULT
000860     OR   (CD_PRD  = : SGTSL057-CD-PRD-ULT
000870       AND CD_MDLD > : SGTSL057-CD-MDLD-ULT)
000880     ORDER BY
000890     CD_PRD, CD_MDLD
000900     END-EXEC.
000910
000920*-----------------------------------------------------------------
000930 PROCEDURE DIVISION USING DFHCOMMAREA.
000940*-----------------------------------------------------------------
           IF  EIBCALEN NOT EQUAL LENGTH OF DFHCOMMAREA
               SET   ERRO-CODIFICADO   TO TRUE
               MOVE  0220              TO  CD-RTN-PGM
               MOVE  'Area com tamanho inválido - verifique book/local.'
                                       TO  MSG-ERRO
               SET MSGKW999-MNT-VRV    TO TRUE
               PERFORM 999001-ERRO-001
           END-IF

           IF NOT EXECUCAO-ONLINE AND NOT EXECUCAO-BATCH
             CALL CICS5000 USING CICS5000-COMMAREA
           END-IF

      *--> WHENEVER DB2
-INC HLPKE002                                                           00001040

      *--> WHENEVER CICS
-INC HLPKE003                                                           00001060
001070
001080     PERFORM 100000-VALIDAR-PARAMETRO
001090
001100     PERFORM 970101-ABRE-CURSOR-PSQ
001110     PERFORM 970102-LER-CURSOR-PSQ
001120     PERFORM 970103-FECHA-CURSOR-PSQ
001130     PERFORM 200000-PROCESSA-PESQUISA
001140              VARYING SGTSL057-IC-TAB FROM 1 BY 1
001150              UNTIL SGTSL057-IC-TAB > SGTSL057-QT-OCR-TAB
001160
001170     GOBACK
001180     .
001190*-----------------------------------------------------------------
001200 100000-VALIDAR-PARAMETRO SECTION.
001210*-----------------------------------------------------------------
001220     MOVE 0001                   TO PSC-ERRO-PGM

001230     INITIALIZE SGTSL057-AREA-DADOS
001240     MOVE 'N'                    TO SGTSL057-IN-CNTC-PSQ

001250     IF  SGTSL057-CD-PRD-ULT < ZEROS
               SET   ERRO-CODIFICADO   TO TRUE
               MOVE  0010              TO  CD-RTN-PGM
               MOVE 'Código do produto Inválido.'
                                       TO MSG-ERRO
               SET MSGKW999-MNT-VRV    TO TRUE
001270         PERFORM 999001-ERRO-001
001280     END-IF

001290     IF  SGTSL057-CD-MDLD-ULT < ZEROS
               SET   ERRO-CODIFICADO   TO TRUE
               MOVE  0416              TO  CD-RTN-PGM
               MOVE 'Código da modalidade Inválido.'
                                       TO MSG-ERRO
               SET MSGKW999-MNT-VRV    TO TRUE
001310         PERFORM 999001-ERRO-001
001320     END-IF
001340     .
001350 100000-SAI.
001360     EXIT.
001370*-----------------------------------------------------------------
001380 200000-PROCESSA-PESQUISA SECTION.
001390*-----------------------------------------------------------------
001400     MOVE 0002                   TO PSC-ERRO-PGM
001410
001420     MOVE CD-PRD(SGTSL057-IC-TAB) TO
001430               SGTSL057-CD-PRD(SGTSL057-IC-TAB)
001440     MOVE CD-MDLD(SGTSL057-IC-TAB) TO
001450               SGTSL057-CD-MDLD(SGTSL057-IC-TAB)
001460     .
001470 200000-SAI.
001480     EXIT.
001490*-----------------------------------------------------------------
001500 970101-ABRE-CURSOR-PSQ SECTION.
001510*-----------------------------------------------------------------
001520     MOVE 0003                   TO PSC-ERRO-PGM
001530     EXEC SQL
001540          OPEN CURSOR-PSQ
001550     END-EXEC
001560     MOVE 'S'                    TO IN-CURSOR-PSQ-ABTO
001570     .
001580 970101-SAI.
001590     EXIT.
001600*-----------------------------------------------------------------
001610 970102-LER-CURSOR-PSQ SECTION.
001620*-----------------------------------------------------------------
001630     MOVE 0004                   TO PSC-ERRO-PGM
001640     INITIALIZE REG-WS
001650     EXEC SQL
001660          FETCH NEXT ROWSET
001670                CURSOR-PSQ FOR 0101 ROWS
001680         INTO :CD-PRD
001690            , :CD-MDLD
001700     END-EXEC
001710     MOVE SQLERRD(3)             TO QT-REG-SELD
001720     IF  QT-REG-SELD EQUAL 0101
001730         SET SGTSL057-TEM-MAIS-REGISTRO TO TRUE
001740         MOVE 0100                TO QT-REG-SELD
001750     END-IF
001760     MOVE QT-REG-SELD            TO SGTSL057-QT-OCR-TAB
001770     MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ
001780     .
001790 970102-SAI.
001800     EXIT.
001810*-----------------------------------------------------------------
001820 970103-FECHA-CURSOR-PSQ SECTION.
001830*-----------------------------------------------------------------
001840     MOVE 0005                   TO PSC-ERRO-PGM
001850     MOVE 'N'                    TO IN-CURSOR-PSQ-ABTO
001860     EXEC SQL
001870          CLOSE CURSOR-PSQ
001880     END-EXEC
001890     .
001900 970103-SAI.
001910     EXIT.
001920*--------------------
001930* 999000-ERRO SECTION
001940*--------------------
001950*-----------------
001960* 999001-ERRO-001.
001970*-----------------
-INC HLPKE004                                                           00001980
-INC HLPKE012                                                           00001990

           PERFORM  999999-ENCERRAR
           .
       999001-SAI.
           EXIT.

002060*-----------------
002070* 999002-ERRO-DB2
002080*-----------------
-INC HLPKE005                                                           00002090
002100*-----------------
002110* 999003-TRATA-ABEND-CICS
002120* 999004-TRATA-CONDITION-CICS
002130*-----------------
-INC HLPKE006                                                           00002140
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
           MOVE  SGTSL057-TIP-RTN-PGM  TO  TIP-RTN-PGM
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

           MOVE 'SGTSL057-CD-PRD-ULT'            TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL057-CD-PRD-ULT             TO  MCR-S9-15
           MOVE  MCR-S9-15                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTSL057-CD-MDLD-ULT'           TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL057-CD-MDLD-ULT            TO  MCR-S9-15
           MOVE  MCR-S9-15                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTSL057-QT-OCR-TAB'         TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL057-QT-OCR-TAB              TO  MCR-S9-15
           MOVE  MCR-S9-15                     TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTSL057-IC-TAB'             TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL057-IC-TAB                  TO  MCR-S9-15
           MOVE  MCR-S9-15                     TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTSL057-IN-CNTC-PSQ'           TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL057-IN-CNTC-PSQ            TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           PERFORM  VARYING  SGTSL057-IC-TAB  FROM 1 BY 1
                      UNTIL  SGTSL057-IC-TAB  > SGTSL057-QT-OCR-TAB
              MOVE 'SGTSL057-IC-TAB'            TO  MSGKW999-NM-VRV-MNT
              MOVE  SGTSL057-IC-TAB             TO  MCR-S9-15
              MOVE  MCR-S9-15                   TO  MSGKW999-VL-VRV-MNT
              PERFORM  999500-ADICIONA-VRV-MNT

              MOVE 'SGTSL057-CD-PRD'            TO  MSGKW999-NM-VRV-MNT
              MOVE  SGTSL057-CD-PRD (SGTSL057-IC-TAB)      TO  MCR-S9-15
              MOVE  MCR-S9-15                   TO  MSGKW999-VL-VRV-MNT
              PERFORM  999500-ADICIONA-VRV-MNT

              MOVE 'SGTSL057-CD-MDLD'           TO  MSGKW999-NM-VRV-MNT
              MOVE  SGTSL057-CD-MDLD (SGTSL057-IC-TAB)     TO  MCR-S9-15
              MOVE  MCR-S9-15                   TO  MSGKW999-VL-VRV-MNT
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
      *
           MOVE RTN-PGM                TO SGTSL057-RTN-PGM

           IF  EXECUCAO-ONLINE  AND   MSGKW999-MNT-VRV
               PERFORM  999000-TRATAR-ERRO-MSG
           END-IF

           GOBACK
           .
       999999-SAI.
           EXIT.

      *---------------------- FIM SGTSL057 ----------------------------