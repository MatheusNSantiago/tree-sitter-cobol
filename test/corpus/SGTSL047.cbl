000010*-----------------------------------------------------------------
000020* Programa..: SGTSL047
000030* Analista..: F2054109 - Cintia Ap Soares Iglesias
000040* Autor.....: F2804391 - Eliana Temperani
000050* Objetivo..: Listar Usuário Destinatário de Correspondência
000060* Compilacao: Cobol/CICS/DB2
000070*-----------------------------------------------------------------
      * VRS002 11.04.2019 - F0950413 DE 2016/145195 - Inclusão das vali-
      *             daçoes dos campos SGTSL047-CD-CTRA-TIT-VL,
      *             SGTSL047-CD-TIP-NEGO e SGTSL047-CD-USU-DSTR-ULT,
      *             Inclusão na section 999001-ERRO-001, '1-Erro de db2'
      *             e '2-erro cics', e adequação das mensagens de erro
      *             ao padrão da mensageria.
000070*        04.04.2019 F8785286 DE 2016/145195 - Inclusão do cpo
000070*             SGTSL047-CD-CTRA-TIT-VL  na área da chave.
000080* VRS001 01.12.2015 - F2804391 - DE 2013/120821 - Implantacao
000090*-----------------------------------------------------------------
000120 IDENTIFICATION DIVISION.
000130*-----------------------------------------------------------------
000140 PROGRAM-ID. SGTSL047.
000160*-----------------------------------------------------------------
000170 ENVIRONMENT DIVISION.
000180*-----------------------------------------------------------------
000190 CONFIGURATION                   SECTION.
000200 SPECIAL-NAMES.
000210     DECIMAL-POINT IS COMMA.
000230*-----------------------------------------------------------------
000240 DATA DIVISION.
000250*-----------------------------------------------------------------
000280 WORKING-STORAGE                 SECTION.
000290*-----------------------------------------------------------------
000300 01  CTE-PROG     PIC  X(17) VALUE '*** SGTSL047 ***'.
000310 01  CTE-VERS     PIC  X(06) VALUE 'VRS002'.
000320
000330*--> Book do programa de ambiente - CICS5000
000340*    Definido na work para preservar as informacoes
-INC HLPKE01B
000360
000370*-----------------------------------------------------------------
000380 LOCAL-STORAGE                   SECTION.
000390*-----------------------------------------------------------------
000400*--> Area de trabalho
       01  IC-TAB                      PIC  9(02)        VALUE 0.
000410 01 QT-REG-SELD                  PIC S9(04) COMP-5 VALUE 0.
000420
000430*--> Campos utilizados para leitura
000440 01  REG-WS.
000470     03 CD-TIP-NEGO                    PIC S9(4) COMP-5
000480        OCCURS 101 TIMES               VALUE 0.
000450     03 CD-TIP-DSTR                    PIC S9(4) COMP-5
000460        OCCURS 101 TIMES               VALUE 0.
000490     03 CD-USU-DSTR                    PIC X(8)
000500        OCCURS 101 TIMES               VALUE ' '.
000520
000530*--> Indicadores de nulidades
000540 01 QT-REG-SELD-N                       PIC S9(4) COMP-5 VALUE 0.
000550
000560*--> Variavel(is) de controle do cursor(es)
000570
000580 01  CTL-CURSOR-PSQ.
000590     03  CD-FIM-CURSOR-PSQ            PIC S9(04) COMP-5 VALUE 0.
000600         88  FIM-CURSOR-PSQ            VALUE +100.
000610     03  IN-CURSOR-PSQ-ABTO           PIC  X(01) VALUE 'N'.
000620         88  CURSOR-PSQ-ABTO          VALUE 'S'.

      *--> Máscaras edição de variáveis p/ monitoração mensagens de erro
       01  MCR-S9-15                   PIC      ---------------9.

      *--> Book da definicao de dados a serem usados subrotina de erros
-INC HLPKE001
      *--> Book para captura de erro via MSG1
-INC MSGKWMG1
      *--> Variaveis para tratamento de CONTAINER
-INC SGTKERR1

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
000720 LINKAGE                         SECTION.
000730*-----------------------------------------------------------------
000740 01  DFHCOMMAREA.
-INC SGTKL047
000760
000770     EXEC SQL
000780       DECLARE CURSOR-PSQ CURSOR
000790               WITH ROWSET POSITIONING FOR
000810       SELECT CD_TIP_NEGO
000820            , CD_TIP_DSTR
000800            , CD_USU_DSTR
000830         FROM DB2SGT.USU_DSTR_CRSD
000840         WHERE CD_CTRA_TIT_VL  = :SGTSL047-CD-CTRA-TIT-VL
000840           AND CD_TIP_NEGO     = :SGTSL047-CD-TIP-NEGO
000850           AND CD_USU_DSTR     > :SGTSL047-CD-USU-DSTR-ULT
000860         ORDER BY
000870         CD_USU_DSTR
000880     END-EXEC.
000890
000900*-----------------------------------------------------------------
000910 PROCEDURE DIVISION USING DFHCOMMAREA.
000920*-----------------------------------------------------------------
000930
           IF  EIBCALEN NOT EQUAL LENGTH OF DFHCOMMAREA
               SET ERRO-CODIFICADO     TO TRUE
               MOVE 0220               TO  CD-RTN-PGM
               MOVE 'Area com tamanho inválido - verifique book/local.'
                                       TO MSG-ERRO
               SET MSGKW999-MNT-VRV    TO TRUE
               PERFORM 999001-ERRO-001
           END-IF
000990
001000     IF NOT EXECUCAO-ONLINE AND NOT EXECUCAO-BATCH
001010       CALL CICS5000 USING CICS5000-COMMAREA
001020     END-IF
001030
001040* WHENEVER DB2
-INC HLPKE002
001060* WHENEVER CICS
-INC HLPKE003
001080
001090     PERFORM 100000-VALIDAR-PARAMETRO
001100
001110     PERFORM 970101-ABRE-CURSOR-PSQ
001120     PERFORM 970102-LER-CURSOR-PSQ
001130     PERFORM 970103-FECHA-CURSOR-PSQ
001140     PERFORM 200000-PROCESSA-PESQUISA
001150              VARYING SGTSL047-IC-TAB FROM 1 BY 1
001160              UNTIL SGTSL047-IC-TAB > SGTSL047-QT-OCR-TAB
001170
001180     GOBACK
001190     .
001200*-----------------------------------------------------------------
001210 100000-VALIDAR-PARAMETRO        SECTION.
001220*-----------------------------------------------------------------
           MOVE  0001                  TO  PSC-ERRO-PGM
001240
001250     INITIALIZE SGTSL047-AREA-DADOS
001260
001270     MOVE 'N'                    TO  SGTSL047-IN-CTNC-PSQ

           IF  SGTSL047-CD-CTRA-TIT-VL   EQUAL ZEROS
               SET ERRO-CODIFICADO     TO TRUE
               MOVE  0002              TO  CD-RTN-PGM
               MOVE 'Código da carteira de título e valores mobiliários
      -             ' não informado.' TO MSG-ERRO
               SET MSGKW999-MNT-VRV    TO TRUE
               PERFORM 999001-ERRO-001
           ELSE
            IF SGTSL047-CD-CTRA-TIT-VL  <  0
               SET ERRO-CODIFICADO     TO TRUE
               MOVE  0001              TO  CD-RTN-PGM
               MOVE 'Código da carteira de títulos e valores mobiliários
      -             ' inválido.'       TO MSG-ERRO
               SET MSGKW999-MNT-VRV    TO TRUE
               PERFORM 999001-ERRO-001
            END-IF
           END-IF

           IF  SGTSL047-CD-TIP-NEGO     <  0
               SET ERRO-CODIFICADO      TO TRUE
               MOVE  0714               TO  CD-RTN-PGM
               MOVE 'Código do tipo de negociação inválido.'
                                        TO MSG-ERRO
               SET MSGKW999-MNT-VRV     TO TRUE
               PERFORM 999001-ERRO-001
           ELSE
            IF  SGTSL047-CD-TIP-NEGO EQUAL ZEROS
                SET ERRO-CODIFICADO     TO TRUE
                MOVE  0953              TO  CD-RTN-PGM
                MOVE 'Código do tipo de negociação não informado.'
                                        TO MSG-ERRO
                SET MSGKW999-MNT-VRV    TO TRUE
                PERFORM 999001-ERRO-001
            END-IF
           END-IF
001280     .
001290 100000-SAI.
001300     EXIT.
001320*-----------------------------------------------------------------
001330 200000-PROCESSA-PESQUISA        SECTION.
001340*-----------------------------------------------------------------
001350     MOVE  0002                  TO  PSC-ERRO-PGM
001360
001370     MOVE  CD-TIP-DSTR                           (SGTSL047-IC-TAB)
001380       TO  SGTSL047-CD-TIP-DSTR                  (SGTSL047-IC-TAB)
001390     MOVE  CD-USU-DSTR                           (SGTSL047-IC-TAB)
001400       TO  SGTSL047-CD-USU-DSTR                  (SGTSL047-IC-TAB)
001410     .
001420 200000-SAI.
001430     EXIT.

001450*-----------------------------------------------------------------
001460 970101-ABRE-CURSOR-PSQ          SECTION.
001470*-----------------------------------------------------------------
001480     MOVE  0003                  TO  PSC-ERRO-PGM
001490
001500     EXEC SQL
001510          OPEN CURSOR-PSQ
001520     END-EXEC
001530
001540     MOVE  'S'                   TO  IN-CURSOR-PSQ-ABTO
001550     .
001560 970101-SAI.
001570     EXIT.
001590*-----------------------------------------------------------------
001600 970102-LER-CURSOR-PSQ           SECTION.
001610*-----------------------------------------------------------------
001620     MOVE  0004                  TO  PSC-ERRO-PGM
001630
001640     INITIALIZE REG-WS
001650
001660     EXEC SQL
001670          FETCH NEXT ROWSET
001680                CURSOR-PSQ FOR 0101 ROWS
001700         INTO :CD-TIP-NEGO
001710            , :CD-TIP-DSTR
001690            , :CD-USU-DSTR
001720     END-EXEC
001730
001740     MOVE  SQLERRD(3)            TO  QT-REG-SELD
001750
001760     IF  QT-REG-SELD EQUAL 0101
001770         SET  SGTSL047-TEM-MAIS-REGISTRO
001780                                 TO  TRUE
001790         MOVE  0100              TO  QT-REG-SELD
001800     END-IF
001810     MOVE  QT-REG-SELD           TO  SGTSL047-QT-OCR-TAB
001820     MOVE  SQLCODE               TO  CD-FIM-CURSOR-PSQ
001830     .
001840 970102-SAI.
001850     EXIT.
001870*-----------------------------------------------------------------
001880 970103-FECHA-CURSOR-PSQ         SECTION.
001890*-----------------------------------------------------------------
001900     MOVE  0005                  TO  PSC-ERRO-PGM
001910
001920     MOVE  'N'                   TO  IN-CURSOR-PSQ-ABTO
001930     EXEC SQL
001940          CLOSE CURSOR-PSQ
001950     END-EXEC
001960     .
001970 970103-SAI.
001980     EXIT.
002000*--------------------
002010* 999000-ERRO                    SECTION
002020*--------------------
002030*-----------------
002040* 999001-ERRO-001.
002050*-----------------
-INC HLPKE004
-INC HLPKE012
           PERFORM  999999-ENCERRAR
           .
       999001-SAI.
           EXIT.
002140*-----------------
002150* 999002-ERRO-DB2
002160*-----------------
-INC HLPKE005
002180*-----------------
002190* 999003-TRATA-ABEND-CICS
002200* 999004-TRATA-CONDITION-CICS
002210*-----------------
-INC HLPKE006
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
           MOVE  SGTSL047-TIP-RTN-PGM  TO  TIP-RTN-PGM
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

           MOVE 'SGTSL047-CD-CTRA-TIT-VL'     TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL047-CD-CTRA-TIT-VL          TO  MCR-S9-15
           MOVE  MCR-S9-15                     TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTSL047-CD-TIP-NEGO'           TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL047-CD-TIP-NEGO            TO  MCR-S9-15
           MOVE  MCR-S9-15                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTSL047-CD-USU-DSTR-ULT'       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL047-CD-USU-DSTR-ULT        TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           IF  SGTSL047-QT-OCR-TAB > 0
               MOVE 'SGTSL047-QT-OCR-TAB'        TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTSL047-QT-OCR-TAB         TO  MCR-S9-15
               MOVE  MCR-S9-15                   TO  MSGKW999-VL-VRV-MNT
               PERFORM  999500-ADICIONA-VRV-MNT

               MOVE 'SGTSL047-IN-CTNC-PSQ'       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTSL047-IN-CTNC-PSQ        TO  MSGKW999-VL-VRV-MNT
               PERFORM  999500-ADICIONA-VRV-MNT

               PERFORM VARYING SGTSL047-IC-TAB FROM 1 BY 1
                         UNTIL SGTSL047-IC-TAB > SGTSL047-QT-OCR-TAB
                  MOVE 'SGTSL047-IC-TAB'         TO  MSGKW999-NM-VRV-MNT
                  MOVE  SGTSL047-IC-TAB          TO  MCR-S9-15
                  MOVE  MCR-S9-15                TO  MSGKW999-VL-VRV-MNT
                  PERFORM  999500-ADICIONA-VRV-MNT

                  MOVE 'SGTSL047-CD-TIP-DSTR'    TO  MSGKW999-NM-VRV-MNT
                  MOVE  SGTSL047-CD-TIP-DSTR     (SGTSL047-IC-TAB)
                                                 TO  MCR-S9-15
                  MOVE  MCR-S9-15                TO  MSGKW999-VL-VRV-MNT
                  PERFORM  999500-ADICIONA-VRV-MNT

                  MOVE 'SGTSL047-CD-USU-DSTR'    TO  MSGKW999-NM-VRV-MNT
                  MOVE  SGTSL047-CD-USU-DSTR     (SGTSL047-IC-TAB)
                                                 TO  MSGKW999-VL-VRV-MNT
                  PERFORM  999500-ADICIONA-VRV-MNT
               END-PERFORM
           END-IF
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

      *--> 1 - Erro de db2
           IF TIP-RTN-PGM   EQUAL 1
              IF CD-RTN-PGM NOT EQUAL 100 AND -803 AND +803
                 MOVE 9997    TO   CD-RTN-PGM
              END-IF
           END-IF

      *--> 2 - erro cics
           IF TIP-RTN-PGM EQUAL 2
              IF CD-RTN-PGM  EQUAL ZEROS OR 333
                 MOVE 9999    TO   CD-RTN-PGM
              END-IF
           END-IF

           MOVE RTN-PGM                TO SGTSL047-RTN-PGM

           IF  EXECUCAO-ONLINE  AND   MSGKW999-MNT-VRV
               PERFORM  999000-TRATAR-ERRO-MSG
           END-IF

           GOBACK
           .
       999999-SAI.
           EXIT.
      *---------------------- FIM SGTSL047 ----------------------------