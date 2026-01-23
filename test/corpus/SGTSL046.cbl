000010*-----------------------------------------------------------------
000020* PROGRAMA..: SGTSL046
000030* ANALISTA..: F2054109 - CINTIA AP SOARES IGLESIAS  Z
000040* AUTOR.....: F8779682 - ROSA APARECIDA HENRIQUES FOZ
000050* OBJETIVO..: Listar UORs Destinatários corresp Tab UOR_DSTR_CRSD
000060* COMPILACAO: COBOL/CICS/DB2
000070*-----------------------------------------------------------------
      * VRS002 11.04.2019 - F0950413 DE 2016/145195 - Inclusão da vali-
      *             dação do campo SGTSL046-CD-CTRA-TIT-VL, Inclusão na
      *             section 999001-ERRO-001, '1 - Erro de db2' e
      *             '2 - erro cics', adequação das mensagens de erro ao
      *             padrão da mensageria.
000080*        02.04.2019 - F8785286 DE 2016/145195 - Inclusão do cpo
000080*             SGTSL046-CD-CTRA-TVM no SQL lista.
000080* VRS001 26.11.2015 - F8779682 DE 2013/120821 - IMPLANTACAO
000100*-----------------------------------------------------------------
000110 IDENTIFICATION DIVISION.
000120*-----------------------------------------------------------------
000130 PROGRAM-ID. SGTSL046.
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
000290 01  CTE-PROG     PIC  X(17) VALUE '*** SGTSL046 ***'.
000300 01  CTE-VERS     PIC  X(06) VALUE 'VRS002'.
000310
000320*-----------------------------------------------------------------
000330* Book do programa de ambiente - CICS5000
000340* Definido na work para preservar as informacoes
-INC HLPKE01B
000360 LOCAL-STORAGE SECTION.
000370*-----------------------------------------------------------------
000380* Area de trabalho
000390
       01  IC-TAB                      PIC  9(02)        VALUE 0.
       01  QT-REG-SELD                 PIC S9(04) COMP-5 VALUE 0.
000410
000420*--> Campos utilizados para leitura
000430
000440 01  REG-WS.
000490     03 CD-TIP-NEGO                    PIC S9(4) COMP-5
000500        OCCURS 101 TIMES               VALUE 0.
000470     03 CD-UOR-DSTR                    PIC S9(9) COMP-5
000480        OCCURS 101 TIMES               VALUE 0.
000450     03 CD-TIP-DSTR                    PIC S9(4) COMP-5
000460        OCCURS 101 TIMES               VALUE 0.
000510
000520
000530*--> Indicadores de nulidades
000540
000550 01 QT-REG-SELD-N                       PIC S9(4) COMP-5 VALUE 0.
000560
000570* Variavel(is) de controle do cursor(es)
000580
000590 01  CTL-CURSOR-PSQ.
000600     03  CD-FIM-CURSOR-PSQ            PIC S9(04) COMP-5 VALUE 0.
000610         88  FIM-CURSOR-PSQ           VALUE +100.
000620     03  IN-CURSOR-PSQ-ABTO           PIC  X(01) VALUE 'N'.
000630         88  CURSOR-PSQ-ABTO          VALUE 'S'.

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
000700
000710     EXEC SQL
000720         INCLUDE SQLCA
000730     END-EXEC.
000740
000750*-----------------------------------------------------------------
000760 LINKAGE SECTION.
000770*-----------------------------------------------------------------
000780 01  DFHCOMMAREA.
-INC SGTKL046
      *--> Lista as UOR destinatárias de uma determinada cart/tp negoc
000800     EXEC SQL
000810       DECLARE CURSOR-PSQ CURSOR
000820               WITH ROWSET POSITIONING FOR
000830       SELECT A.CD_TIP_NEGO
000840            , A.CD_UOR_DSTR
000840            , A.CD_TIP_DSTR
000860         FROM DB2SGT.UOR_DSTR_CRSD      A
000870        WHERE A.CD_CTRA_TIT_VL     = :SGTSL046-CD-CTRA-TIT-VL
000870          AND A.CD_TIP_NEGO        = :SGTSL046-CD-TIP-NEGO
000880          AND A.CD_UOR_DSTR        > :SGTSL046-CD-ULT-UOR-DSTR
000890        ORDER BY A.CD_UOR_DSTR
000900     END-EXEC.
000910
000920*-----------------------------------------------------------------
000930 PROCEDURE DIVISION USING DFHCOMMAREA.
000940*-----------------------------------------------------------------
           IF  EIBCALEN NOT EQUAL LENGTH OF DFHCOMMAREA
               SET ERRO-CODIFICADO     TO TRUE
               MOVE 0220               TO  CD-RTN-PGM
               MOVE 'Area com tamanho inválido - verifique book/local.'
                                       TO MSG-ERRO
               SET MSGKW999-MNT-VRV    TO TRUE
               PERFORM 999001-ERRO-001
           END-IF
001770
001000     IF NOT EXECUCAO-ONLINE AND NOT EXECUCAO-BATCH
001010       CALL CICS5000 USING CICS5000-COMMAREA
001020     END-IF
001770
001030* WHENEVER DB2
-INC HLPKE002
001050* WHENEVER CICS
-INC HLPKE003
001070
001080     PERFORM 100000-VALIDAR-PARAMETRO
001090
001100     EVALUATE  SGTSL046-ACAO
001110       WHEN 01
001120         PERFORM 200000-TRATA-ACAO-01
001130     END-EVALUATE
001770
001140     GOBACK
001150     .
001160*-----------------------------------------------------------------
001170 100000-VALIDAR-PARAMETRO SECTION.
001180*-----------------------------------------------------------------
001190     MOVE 0001                   TO PSC-ERRO-PGM
001770
001200     INITIALIZE SGTSL046-AREA-DADOS
001770
001210     MOVE 'N'                    TO SGTSL046-IN-CNTO-PSQ

           IF  SGTSL046-ACAO  NOT  EQUAL  1
               SET ERRO-CODIFICADO     TO TRUE
               MOVE  0116              TO  CD-RTN-PGM
               MOVE 'Ação inválida.'   TO MSG-ERRO
               SET MSGKW999-MNT-VRV    TO TRUE
               PERFORM 999001-ERRO-001
           END-IF

           IF  SGTSL046-ACAO  EQUAL  1
               IF  SGTSL046-CD-CTRA-TIT-VL   EQUAL ZEROS
                   SET ERRO-CODIFICADO           TO TRUE
                   MOVE  0002                    TO  CD-RTN-PGM
                   MOVE 'Código da carteira de título e valores mobiliár
      -                 'ios não informado.'     TO MSG-ERRO
                   SET MSGKW999-MNT-VRV          TO TRUE
                   PERFORM 999001-ERRO-001
               ELSE
                IF SGTSL046-CD-CTRA-TIT-VL  <  0
                   SET ERRO-CODIFICADO      TO TRUE
                   MOVE  0001               TO  CD-RTN-PGM
                   MOVE 'Código da carteira de títulos e valores mobiliá
      -                 'rios inválido.'    TO MSG-ERRO
                   SET MSGKW999-MNT-VRV     TO TRUE
                   PERFORM 999001-ERRO-001
                END-IF
               END-IF

               IF  SGTSL046-CD-TIP-NEGO     <  0
                   SET ERRO-CODIFICADO      TO TRUE
                   MOVE  0714               TO  CD-RTN-PGM
                   MOVE 'Código do tipo de negociação inválido.'
                                            TO MSG-ERRO
                   SET MSGKW999-MNT-VRV     TO TRUE
                   PERFORM 999001-ERRO-001
               ELSE
                IF  SGTSL046-CD-TIP-NEGO EQUAL ZEROS
                    SET ERRO-CODIFICADO     TO TRUE
                    MOVE  0953              TO  CD-RTN-PGM
                    MOVE 'Código do tipo de negociação não informado.'
                                            TO MSG-ERRO
                    SET MSGKW999-MNT-VRV    TO TRUE
                    PERFORM 999001-ERRO-001
                END-IF
               END-IF

               IF  SGTSL046-CD-ULT-UOR-DSTR <  0
                   SET ERRO-CODIFICADO      TO TRUE
                   MOVE  0945               TO  CD-RTN-PGM
                   MOVE 'Código ultima UOR destinatária inválida.'
                                            TO MSG-ERRO
                   SET MSGKW999-MNT-VRV     TO TRUE
                   PERFORM 999001-ERRO-001
               END-IF
           END-IF
           .
       100000-SAI.
           EXIT.
001450
001460*-----------------------------------------------------------------
001470 200000-TRATA-ACAO-01     SECTION.
001480*-----------------------------------------------------------------
001490     MOVE 0002                   TO PSC-ERRO-PGM
001500
001510     PERFORM 970101-ABRE-CURSOR-PSQ
001520     PERFORM 970102-LER-CURSOR-PSQ
001530     PERFORM 970103-FECHA-CURSOR-PSQ
001540
001550     PERFORM 210000-PROCESSA-PESQUISA
001560              VARYING SGTSL046-IC-TAB FROM 1 BY 1
001570              UNTIL SGTSL046-IC-TAB > SGTSL046-QT-OCR-TAB
001580     .
001590 200000-SAI.
001600     EXIT.
001610
001620*-----------------------------------------------------------------
001630 210000-PROCESSA-PESQUISA SECTION.
001640*-----------------------------------------------------------------
001650     MOVE 0003                   TO PSC-ERRO-PGM
001660
001670     MOVE CD-TIP-DSTR(SGTSL046-IC-TAB) TO
001680               SGTSL046-CD-TIP-DSTR(SGTSL046-IC-TAB)
001690     MOVE CD-UOR-DSTR(SGTSL046-IC-TAB) TO
001700               SGTSL046-CD-UOR-DSTR(SGTSL046-IC-TAB)
001710     .
001720 200000-SAI.
001730     EXIT.
001740*-----------------------------------------------------------------
001750 970101-ABRE-CURSOR-PSQ SECTION.
001760*-----------------------------------------------------------------
001770     MOVE 0004                   TO PSC-ERRO-PGM
001770
001780     EXEC SQL
001790          OPEN CURSOR-PSQ
001800     END-EXEC
001770
001810     MOVE 'S'                    TO IN-CURSOR-PSQ-ABTO
001820     .
001830 970101-SAI.
001840     EXIT.
001850*-----------------------------------------------------------------
001860 970102-LER-CURSOR-PSQ SECTION.
001870*-----------------------------------------------------------------
001880     MOVE 0005                   TO PSC-ERRO-PGM
001770
001890     INITIALIZE REG-WS
001770
001900     EXEC SQL
001910          FETCH NEXT ROWSET
001920                CURSOR-PSQ FOR 0101 ROWS
001950         INTO :CD-TIP-NEGO
001940            , :CD-UOR-DSTR
001930            , :CD-TIP-DSTR
001960     END-EXEC
001770
001970     MOVE SQLERRD(3)             TO QT-REG-SELD
001770
001980     IF  QT-REG-SELD EQUAL 0101
001990         SET SGTSL046-TEM-MAIS-REGISTRO TO TRUE
002000         MOVE 0100                TO QT-REG-SELD
002010     END-IF
001770
002020     MOVE QT-REG-SELD            TO SGTSL046-QT-OCR-TAB
002030     MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ
002040     .
002050 970102-SAI.
002060     EXIT.
002070*-----------------------------------------------------------------
002080 970103-FECHA-CURSOR-PSQ SECTION.
002090*-----------------------------------------------------------------
002100     MOVE 0006                   TO PSC-ERRO-PGM
001770
002110     MOVE 'N'                    TO IN-CURSOR-PSQ-ABTO
001770
002120     EXEC SQL
002130          CLOSE CURSOR-PSQ
002140     END-EXEC
002150     .
002160 970103-SAI.
002170     EXIT.
002180*--------------------
002190* 999000-ERRO SECTION
002200*--------------------
002210*-----------------
002220* 999001-ERRO-001.
002230*-----------------
-INC HLPKE004
-INC HLPKE012
           PERFORM  999999-ENCERRAR
           .
       999001-SAI.
           EXIT.
002320*-----------------
002330* 999002-ERRO-DB2
002340*-----------------
-INC HLPKE005
002360*-----------------
002370* 999003-TRATA-ABEND-CICS
002380* 999004-TRATA-CONDITION-CICS
002390*-----------------
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
           MOVE  SGTSL046-TIP-RTN-PGM  TO  TIP-RTN-PGM
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

           MOVE 'SGTSL046-ACAO'                  TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL046-ACAO                   TO  MCR-S9-15
           MOVE  MCR-S9-15                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTSL046-CD-CTRA-TIT-VL'     TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL046-CD-CTRA-TIT-VL          TO  MCR-S9-15
           MOVE  MCR-S9-15                     TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTSL046-CD-TIP-NEGO'           TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL046-CD-TIP-NEGO            TO  MCR-S9-15
           MOVE  MCR-S9-15                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTSL046-CD-ULT-UOR-DSTR'       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL046-CD-ULT-UOR-DSTR        TO  MCR-S9-15
           MOVE  MCR-S9-15                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           IF  SGTSL046-QT-OCR-TAB > 0
               MOVE 'SGTSL046-QT-OCR-TAB'        TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTSL046-QT-OCR-TAB         TO  MCR-S9-15
               MOVE  MCR-S9-15                   TO  MSGKW999-VL-VRV-MNT
               PERFORM  999500-ADICIONA-VRV-MNT

               MOVE 'SGTSL046-IN-CNTO-PSQ'       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTSL046-IN-CNTO-PSQ        TO  MSGKW999-VL-VRV-MNT
               PERFORM  999500-ADICIONA-VRV-MNT

               PERFORM VARYING SGTSL046-IC-TAB FROM 1 BY 1
                         UNTIL SGTSL046-IC-TAB > SGTSL046-QT-OCR-TAB
                  MOVE 'SGTSL046-IC-TAB'         TO  MSGKW999-NM-VRV-MNT
                  MOVE  SGTSL046-IC-TAB          TO  MCR-S9-15
                  MOVE  MCR-S9-15                TO  MSGKW999-VL-VRV-MNT
                  PERFORM  999500-ADICIONA-VRV-MNT

                  MOVE 'SGTSL046-CD-TIP-DSTR'    TO  MSGKW999-NM-VRV-MNT
                  MOVE  SGTSL046-CD-TIP-DSTR     (SGTSL046-IC-TAB)
                                                 TO  MCR-S9-15
                  MOVE  MCR-S9-15                TO  MSGKW999-VL-VRV-MNT
                  PERFORM  999500-ADICIONA-VRV-MNT

                  MOVE 'SGTSL046-CD-UOR-DSTR'    TO  MSGKW999-NM-VRV-MNT
                  MOVE  SGTSL046-CD-UOR-DSTR     (SGTSL046-IC-TAB)
                                                 TO  MCR-S9-15
                  MOVE  MCR-S9-15                TO  MSGKW999-VL-VRV-MNT
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

           MOVE RTN-PGM                TO SGTSL046-RTN-PGM

           IF  EXECUCAO-ONLINE  AND   MSGKW999-MNT-VRV
               PERFORM  999000-TRATAR-ERRO-MSG
           END-IF

           GOBACK
           .
       999999-SAI.
           EXIT.
      *---------------------- FIM SGTSL046 ----------------------------