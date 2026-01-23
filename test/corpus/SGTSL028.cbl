      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSL028
      * ANALISTA..: F6631109 - MAGALI MARIKO OYAMADA
      * AUTOR.....: F6631109 - MAGALI MARIKO OYAMADA
      * OBJETIVO..: CONSULTA TABELA DB2SGT.EST_TIP_COTA_TIT
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS002 24/05/2019 F0950413 DE 2016/145195 - Inclusão na section
      *        999001-ERRO-001, '1 - Erro de db2' e '2 - erro cics', e
      *        adequação das mensagens de erro ao padrão da mensageria.
      * VRS001 20.07.2015 - F6631109 - IMPLANTACAO
      *-----------------------------------------------------------------
      
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSL028.
      
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
      *-----------------------------------------------------------------
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      
      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------
      
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
      *-----------------------------------------------------------------
       01  CTE-PROG     PIC  X(17) VALUE '*** SGTSL028 ***'.
       01  CTE-VERS     PIC  X(06) VALUE 'VRS002'.
      
      *-----------------------------------------------------------------
      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informacoes
       COPY HLPKE01B.                                                           0000

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

      *--> Area de trabalho
       01  IC-TAB                      PIC  9(2)        VALUE 0.
       01 QT-REG-SELD                  PIC S9(4) COMP-5 VALUE 0.
      
      * Campos utilizados para leitura
      
       01  REG-WS.
           03 CD-TIP-EST-COTA          PIC S9(04) COMP-5 VALUE 0.
           03 CD-USU-ATL-EST           PIC  X(08)        VALUE ' '.
           03 TS-EST-TIP-COTA          PIC  X(26)        VALUE ' '.
           03 TX-OBS-EST-TIP           PIC  X(60)        VALUE ' '.
           03 IN-TIP-COTA-ATI-TIT      PIC  X(01)        VALUE ' '.
           03 NM-TIP-COTA-TIT          PIC  X(40)        VALUE ' '.
      
      * Indicadores de nulidades
      
       01 QT-REG-SELD-N                PIC S9(4)  COMP-5 VALUE 0.
       01 TX-OBS-EST-TIP-N             PIC S9(4)  COMP-5 VALUE 0.
      
      * Variavel(is) de controle do cursor(es)
      
       01  CTL-CURSOR-PSQ.
           03  CD-FIM-CURSOR-PSQ            PIC S9(04) COMP-5 VALUE 0.
               88  FIM-CURSOR-PSQ            VALUE +100.
           03  IN-CURSOR-PSQ-ABTO           PIC  X(01) VALUE 'N'.
               88  CURSOR-PSQ-ABTO          VALUE 'S'.

      * Books da definicao de dados a serem usados na subrotina de erros
       COPY HLPKE001.                                                           0000

      *--> Máscaras edição de variáveis p/ monitoração mensagens de erro
       01  MCR-S9-15                   PIC      ---------------9.

      *--> Book para captura de erro via MSG1
       COPY MSGKWMG1.

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
      
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.
      
      *-----------------------------------------------------------------
       LINKAGE SECTION.
      *-----------------------------------------------------------------
       01  DFHCOMMAREA.
       COPY SGTKL028.                                                           0000
           EXEC SQL
             DECLARE CURSOR-PSQ CURSOR  FOR
             SELECT A.CD_TIP_EST_COTA
                  , A.CD_USU_ATL_EST
                  , A.TS_EST_TIP_COTA
                  , A.TX_OBS_EST_TIP
               FROM DB2SGT.EST_TIP_COTA_TIT   A
              WHERE A.CD_TIP_COTA_TIT    = :SGTSL028-CD-TIP-COTA-TIT
              AND   A.NR_VRS_TIP_COTA    = :SGTSL028-NR-VRS-TIP-COTA
              AND   A.TS_EST_TIP_COTA    > :SGTSL028-ULT-TS-EST-TIP-COTA
              ORDER BY  A.TS_EST_TIP_COTA
           END-EXEC.
      
      *-----------------------------------------------------------------
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *-----------------------------------------------------------------
           IF  EIBCALEN NOT EQUAL LENGTH OF DFHCOMMAREA
               SET ERRO-CODIFICADO     TO TRUE
               MOVE 0220               TO  CD-RTN-PGM
               MOVE 'Area com tamanho inválido - verifique book/local.'
                                       TO MSG-ERRO
               SET MSGKW999-MNT-VRV    TO TRUE
               PERFORM 999001-ERRO-001
           END-IF
           IF NOT EXECUCAO-ONLINE AND NOT EXECUCAO-BATCH
             CALL CICS5000 USING CICS5000-COMMAREA
           END-IF

      *--> WHENEVER DB2
       COPY HLPKE002.

      *--> WHENEVER CICS
       COPY HLPKE003.
      
           PERFORM 100000-VALIDAR-PARAMETRO
      
           PERFORM 970101-ABRE-CURSOR-PSQ
           PERFORM 970102-LER-CURSOR-PSQ
           PERFORM 200000-PROCESSA-PESQUISA UNTIL FIM-CURSOR-PSQ
                    OR SGTSL028-PREENCHIDO-TAB
           IF  NOT FIM-CURSOR-PSQ
               MOVE 'S'                TO SGTSL028-IN-CNTO-PSQ
           END-IF
           PERFORM 970103-FECHA-CURSOR-PSQ
      
           GOBACK
           .
      *-----------------------------------------------------------------
       100000-VALIDAR-PARAMETRO SECTION.
      *-----------------------------------------------------------------
           MOVE 0001                   TO PSC-ERRO-PGM

           INITIALIZE SGTSL028-AREA-DADOS
           MOVE 'N'                    TO SGTSL028-IN-CNTO-PSQ

           IF  SGTSL028-CD-TIP-COTA-TIT EQUAL ZEROS
               SET ERRO-CODIFICADO     TO TRUE
               MOVE  0257              TO  CD-RTN-PGM
               MOVE 'Código do tipo de cota do título da operação não in
      -             'formado.'         TO MSG-ERRO
               SET MSGKW999-MNT-VRV    TO TRUE
               PERFORM 999001-ERRO-001
           END-IF

           IF  SGTSL028-ULT-TS-EST-TIP-COTA EQUAL SPACES
               MOVE '1900-01-01-01.01.01.0000001' TO
                                  SGTSL028-ULT-TS-EST-TIP-COTA
           END-IF
      
           PERFORM 151000-VALIDA-TIP-COTA-TIT-OPR
           .
       100000-SAI.
           EXIT.
      
      *-----------------------------------------------------------------
       151000-VALIDA-TIP-COTA-TIT-OPR SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO PSC-ERRO-PGM

           EXEC SQL
             SELECT IN_TIP_COTA_ATI
                  , NM_TIP_COTA_TIT
               INTO :SGTSL028-IN-TIP-COTA-ATI-TIT
                  , :SGTSL028-NM-TIP-COTA-TIT
              FROM  DB2SGT.TIP_COTA_TIT_OPR
              WHERE CD_TIP_COTA_TIT    = :SGTSL028-CD-TIP-COTA-TIT
              AND   NR_VRS_TIP_COTA    = :SGTSL028-NR-VRS-TIP-COTA
           END-EXEC

           IF  SQLCODE NOT EQUAL ZEROS
               SET ERRO-CODIFICADO     TO TRUE
               MOVE  0017              TO  CD-RTN-PGM
               MOVE 'Código do tipo de cota do título da operação inváli
      -             'do.'              TO MSG-ERRO
               SET MSGKW999-MNT-VRV    TO  TRUE
               PERFORM 999001-ERRO-001
           END-IF
           .
       151000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       200000-PROCESSA-PESQUISA SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                   TO PSC-ERRO-PGM
      
           ADD  1                 TO SGTSL028-QT-OCR-TAB SGTSL028-IC-TAB
           IF  SGTSL028-ESTOURO-TAB
               SET ERRO-CODIFICADO     TO TRUE
               MOVE  0192              TO  CD-RTN-PGM
               MOVE 'Tabela da linkage estourou' TO MSG-ERRO
               SET MSGKW999-MNT-VRV    TO TRUE
               PERFORM 999001-ERRO-001
           END-IF

           MOVE CD-TIP-EST-COTA              TO
                     SGTSL028-CD-TIP-EST-COTA(SGTSL028-IC-TAB)
           MOVE CD-USU-ATL-EST               TO
                     SGTSL028-CD-USU-ATL-EST(SGTSL028-IC-TAB)
           MOVE TS-EST-TIP-COTA              TO
                     SGTSL028-TS-EST-TIP-COTA(SGTSL028-IC-TAB)
           MOVE TX-OBS-EST-TIP               TO
                     SGTSL028-TX-OBS-EST-TIP(SGTSL028-IC-TAB)
           PERFORM 970102-LER-CURSOR-PSQ
           .
       200000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970101-ABRE-CURSOR-PSQ SECTION.
      *-----------------------------------------------------------------
           MOVE 0004                   TO PSC-ERRO-PGM
           EXEC SQL
                OPEN CURSOR-PSQ
           END-EXEC
           MOVE 'S'                    TO IN-CURSOR-PSQ-ABTO
           .
       970101-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970102-LER-CURSOR-PSQ SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                   TO PSC-ERRO-PGM
           INITIALIZE REG-WS
           EXEC SQL
                FETCH CURSOR-PSQ
               INTO :CD-TIP-EST-COTA
                  , :CD-USU-ATL-EST
                  , :TS-EST-TIP-COTA
                  , :TX-OBS-EST-TIP
                    :TX-OBS-EST-TIP-N
           END-EXEC
           MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ
           .
       970102-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970103-FECHA-CURSOR-PSQ SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO PSC-ERRO-PGM
           MOVE 'N'                    TO IN-CURSOR-PSQ-ABTO
           EXEC SQL
                CLOSE CURSOR-PSQ
           END-EXEC
           .
       970103-SAI.
           EXIT.
      *--------------------
      * 999000-ERRO SECTION
      *--------------------
      *-----------------
      * 999001-ERRO-001.
      *-----------------
       COPY HLPKE004.                                                           0000
       COPY HLPKE012.                                                           0000

           PERFORM  999999-ENCERRAR
           .
       999001-SAI.
           EXIT.
      
      *-----------------
      * 999002-ERRO-DB2
      *-----------------
       COPY HLPKE005.                                                           0000
      *-----------------
      * 999003-TRATA-ABEND-CICS
      * 999004-TRATA-CONDITION-CICS
      *-----------------
       COPY HLPKE006.                                                           0000
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
           MOVE  SGTSL028-TIP-RTN-PGM  TO  TIP-RTN-PGM
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

           MOVE 'SGTSL028-CD-TIP-COTA-TIT'       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL028-CD-TIP-COTA-TIT        TO  MCR-S9-15
           MOVE  MCR-S9-15                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTSL028-NR-VRS-TIP-COTA'       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL028-NR-VRS-TIP-COTA        TO  MCR-S9-15
           MOVE  MCR-S9-15                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTSL028-ULT-TS-EST-TIP-COTA'   TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL028-ULT-TS-EST-TIP-COTA    TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTSL028-IN-TIP-COTA-ATI-TIT'   TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL028-IN-TIP-COTA-ATI-TIT    TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTSL028-NM-TIP-COTA-TIT'       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL028-NM-TIP-COTA-TIT        TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTSL028-QT-OCR-TAB'            TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL028-QT-OCR-TAB             TO  MCR-S9-15
           MOVE  MCR-S9-15                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTSL028-IC-TAB'                TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL028-IC-TAB                 TO  MCR-S9-15
           MOVE  MCR-S9-15                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTSL028-IN-CNTO-PSQ'           TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL028-IN-CNTO-PSQ            TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           PERFORM VARYING SGTSL028-IC-TAB FROM 1 BY 1
                     UNTIL SGTSL028-IC-TAB > SGTSL028-QT-OCR-TAB

              MOVE 'SGTSL028-IC-TAB'             TO  MSGKW999-NM-VRV-MNT
              MOVE  SGTSL028-IC-TAB              TO  MCR-S9-15
              MOVE  MCR-S9-15                    TO  MSGKW999-VL-VRV-MNT
              PERFORM  999500-ADICIONA-VRV-MNT

              MOVE 'SGTSL028-TS-EST-TIP-COTA'    TO  MSGKW999-NM-VRV-MNT
              MOVE  SGTSL028-TS-EST-TIP-COTA (SGTSL028-IC-TAB)
                                                 TO  MSGKW999-VL-VRV-MNT
              PERFORM  999500-ADICIONA-VRV-MNT

              MOVE 'SGTSL028-CD-TIP-EST-COTA'    TO  MSGKW999-NM-VRV-MNT
              MOVE  SGTSL028-CD-TIP-EST-COTA (SGTSL028-IC-TAB)
                                                 TO  MCR-S9-15
              MOVE  MCR-S9-15                    TO  MSGKW999-VL-VRV-MNT
              PERFORM  999500-ADICIONA-VRV-MNT

              MOVE 'SGTSL028-CD-USU-ATL-EST'     TO  MSGKW999-NM-VRV-MNT
              MOVE  SGTSL028-CD-USU-ATL-EST  (SGTSL028-IC-TAB)
                                                 TO  MSGKW999-VL-VRV-MNT
              PERFORM  999500-ADICIONA-VRV-MNT

              MOVE 'SGTSL028-TX-OBS-EST-TIP'     TO  MSGKW999-NM-VRV-MNT
              MOVE  SGTSL028-TX-OBS-EST-TIP  (SGTSL028-IC-TAB)
                                                 TO  MSGKW999-VL-VRV-MNT
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
       COPY MSGKPMG1.

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

           MOVE RTN-PGM                TO SGTSL028-RTN-PGM

           IF  EXECUCAO-ONLINE  AND   MSGKW999-MNT-VRV
               PERFORM  999000-TRATAR-ERRO-MSG
           END-IF

           GOBACK
           .
       999999-SAI.
           EXIT.
      *---------------------- FIM SGTSL028 ----------------------------
