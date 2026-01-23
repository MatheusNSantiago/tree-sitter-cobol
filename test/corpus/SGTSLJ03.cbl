      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSLJ03
      * ANALISTA..: MAGALI MARIKO OYAMADA.
      * AUTOR.....: MAGALI MARIKO OYAMADA.
      * DATA......: 20/07/2015
      * OBJETIVO..: Recupera Dados das tabelas TIP_COTA_TIT_OPR E
      *             EST_TIP_COTA_TIT
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      *   VRS001 20/07/2015 F6631109  DE 2015/120821 - IMPLANTACAO
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSLJ03.

      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
      *-----------------------------------------------------------------
       CONFIGURATION                   SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       WORKING-STORAGE                 SECTION.
      *-----------------------------------------------------------------
       77  CTE-INICIO-SS                      PIC  X(35) VALUE
               '*** SGTSLJ03 S.S. COMECA AQUI ***'.
       77  CTE-PROG                           PIC  X(18) VALUE
               '*** SGTSLJ03 ***' .
       77  CTE-VERS                           PIC  X(06) VALUE 'VRS001'.

      * CONSTANTES
       77  EMISSOR                            PIC S9(04) COMP-5 VALUE 1.
       77  NAO-ENVIADA                 PIC S9(04) COMP-5  VALUE 1.
       77  PENDENTE-ERRO               PIC S9(04) COMP-5  VALUE 3.
       77  CARTEIRA-300000             PIC S9(09) COMP-5  VALUE 300000.

      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informacoes
-INC HLPKE01B                                                           0000044%
      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------
      * Area de trabalho
       01  IC-TAB                             PIC  9(04)       VALUE 0.

       01 QT-REG-SELD                         PIC S9(04) COMP-5 VALUE 0.
       01 EST-PRTC-CLI-CANCELADO              PIC S9(04) COMP-5 VALUE 2.

      * Campos TS para pesquisa
       01  PAR-TS-INC-PSQ                     PIC X(26) VALUE
                                           '2999-12-31-00.00.00.000000'.
       01  PAR-TS-INC-PSQ-R   REDEFINES PAR-TS-INC-PSQ.
           03  ANO                            PIC 9(04).
           03  FILLER                         PIC X(01).
           03  MES                            PIC 9(02).
           03  FILLER                         PIC X(01).
           03  DIA                            PIC 9(02).
           03  FILLER                         PIC X(01).
           03  HORA                           PIC X(15).

      * Campos data inicio e fim para pesquisa

       01  DT-INC-PER-E        PIC  X(10)  VALUE '    -  -  '.
       01  DT-INC-PER-R     REDEFINES         DT-INC-PER-E.
           03 ANO                       PIC  9(4).
           03 FILLER                    PIC  X(1).
           03 MES                       PIC  9(2).
           03 FILLER                    PIC  X(1).
           03 DIA                       PIC  9(2).

       01  DT-FIM-PER-E        PIC  X(10)  VALUE '    -  -  '.
       01  DT-FIM-PER-R     REDEFINES         DT-FIM-PER-E.
           03 ANO                       PIC  9(4).
           03 FILLER                    PIC  X(1).
           03 MES                       PIC  9(2).
           03 FILLER                    PIC  X(1).
           03 DIA                       PIC  9(2).

      * Campos utilizados para leitura

       01  REG-WS.
           03 CD-TIP-COTA-TIT                   PIC S9(4) COMP-5
              OCCURS 101 TIMES                  VALUE 0.
           03 NR-VRS-TIP-COTA                   PIC S9(4) COMP-5
              OCCURS 101 TIMES                  VALUE 0.
           03 NM-TIP-COTA-TIT                   PIC X(40)
              OCCURS 101 TIMES                  VALUE ' '.
           03 IN-TIP-COTA-ATI                   PIC X(1)
              OCCURS 101 TIMES                  VALUE ' '.
           03 TS-EST-TIP-COTA                   PIC  X(26)
              OCCURS 101 TIMES                  VALUE ' '.
           03 CD-TIP-EST-COTA                   PIC S9(04) COMP-5
              OCCURS 101 TIMES                  VALUE 0.
           03 CD-USU-ATL-EST                    PIC X(8)
              OCCURS 101 TIMES                  VALUE ' '.
           03 TX-OBS-EST-TIP                    PIC X(60)
              OCCURS 101 TIMES                  VALUE ' '.
           03 TX-OBS-EST-TIP-N                  PIC S9(4) COMP-5
              OCCURS 101 TIMES                  VALUE 0.

      * VARIAVEL(IS) DE CONTROLE DO CURSOR(ES)

       01  CTL-CURSOR-PSQ.
           03  CD-FIM-CURSOR-PSQ            PIC S9(04) COMP-5 VALUE 0.
               88  FIM-CURSOR-PSQ           VALUE +100.
           03  IN-CURSOR-PSQ-ABTO           PIC  X(01) VALUE 'N'.
               88  CURSOR-PSQ-ABTO          VALUE 'S'.

       01  PAR-DCR-COTA                     PIC X(40)  VALUE ' '.

       01  PAR-EST-COTA-INI                 PIC S9(04) COMP-5 VALUE 0.
       01  PAR-EST-COTA-FIM                 PIC S9(04) COMP-5 VALUE 0.

       01  PAR-IN-ATI-01                    PIC X(01) VALUE ' '.
       01  PAR-IN-ATI-02                    PIC X(01) VALUE ' '.

       01  PAR-CLI-INI                      PIC S9(09) COMP-5 VALUE 0.
       01  PAR-CLI-FIM                      PIC S9(09) COMP-5 VALUE 0.

       01  PAR-TIT-INI                      PIC  X(12)  VALUE ' '.
       01  PAR-TIT-FIM                      PIC  X(12) VALUE ' '.

       01  QT-CCT-DGTD                 PIC S9(04) COMP  VALUE 0.
       01  QT-CCT-N-DGTD               PIC S9(04) COMP  VALUE 0.
       01  QT-CCT-PSQ                  PIC S9(04) COMP  VALUE 0.
       01  TAM-PAR-TIT-INI             PIC S9(04) COMP VALUE 0.

      * Books da definicao de dados a serem usados na subrotina de erros
-INC HLPKE001                                                           0000133%
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

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

       01  CTE-FINAL-SS                       PIC  X(40) VALUE
               '*** STORAGE SECTION TERMINA AQUI ***'.

      *-----------------------------------------------------------------
       LINKAGE                         SECTION.
      *-----------------------------------------------------------------
       01  DFHCOMMAREA.
-INC SGTKLJ03                                                           0000146%

           EXEC SQL
             DECLARE CURSOR-PSQ-1   CURSOR
                     WITH ROWSET POSITIONING FOR
              SELECT A.CD_TIP_COTA_TIT,
                     A.NR_VRS_TIP_COTA,
                     A.NM_TIP_COTA_TIT,
                     A.IN_TIP_COTA_ATI,
                     B.TS_EST_TIP_COTA,
                     B.CD_TIP_EST_COTA,
                     B.CD_USU_ATL_EST,
                     B.TX_OBS_EST_TIP
               FROM       DB2SGT.TIP_COTA_TIT_OPR A
               INNER JOIN DB2SGT.EST_TIP_COTA_TIT B
                 ON A.CD_TIP_COTA_TIT = B.CD_TIP_COTA_TIT AND
                    A.NR_VRS_TIP_COTA = B.NR_VRS_TIP_COTA
                 INNER JOIN (SELECT C.CD_TIP_COTA_TIT,
                                    C.NR_VRS_TIP_COTA,
                                    MAX(C.TS_EST_TIP_COTA) AS MAX_TS
                              FROM     DB2SGT.EST_TIP_COTA_TIT C
                              GROUP BY C.CD_TIP_COTA_TIT,
                                       C.NR_VRS_TIP_COTA) D
                            ON B.CD_TIP_COTA_TIT = D.CD_TIP_COTA_TIT
                           AND B.NR_VRS_TIP_COTA = D.NR_VRS_TIP_COTA
                           AND B.TS_EST_TIP_COTA = D.MAX_TS
                              WHERE (UPPER(A.NM_TIP_COTA_TIT) LIKE
                                             UPPER(:PAR-DCR-COTA))   AND
                              B.CD_TIP_EST_COTA >= :PAR-EST-COTA-INI AND
                              B.CD_TIP_EST_COTA <= :PAR-EST-COTA-FIM AND
                           DATE (B.TS_EST_TIP_COTA) >= :DT-INC-PER-E AND
                           DATE (B.TS_EST_TIP_COTA) <= :DT-FIM-PER-E AND
                                (A.IN_TIP_COTA_ATI = :PAR-IN-ATI-01  OR
                                 A.IN_TIP_COTA_ATI = :PAR-IN-ATI-02) AND
                      B.TS_EST_TIP_COTA < :SGTSLJ03-TS-ULT-EST-TIP-COTA
                       ORDER BY B.TS_EST_TIP_COTA  DESC
           END-EXEC.

      *-----------------------------------------------------------------
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *-----------------------------------------------------------------

      * teste obrigatorio do tamanho da linkage nao retirar
           IF  EIBCALEN NOT EQUAL LENGTH OF DFHCOMMAREA
               SET  ERRO-CODIFICADO   TO  TRUE
               MOVE 0220               TO  CD-RTN-PGM
               MOVE 'Área com tamanho inválido - verifique book/local.'
                                       TO MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           MOVE 0001                        TO PSC-ERRO-PGM

           IF  NOT EXECUCAO-ONLINE AND NOT EXECUCAO-BATCH
             CALL CICS5000 USING CICS5000-COMMAREA
           END-IF

      * WHENEVER DB2
-INC HLPKE002                                                           0000200%
      * WHENEVER CICS
-INC HLPKE003                                                           0000202%

           PERFORM 100000-VALIDAR-PARAMETROS

           EVALUATE TRUE
           WHEN SGTSLJ03-LST-TIP-COTA-ATUAL
              PERFORM 940101-ABRE-CURSOR-PSQ-1
              PERFORM 950101-LER-CURSOR-PSQ-1
              PERFORM 960101-FECHA-CURSOR-PSQ-1
           END-EVALUATE

           PERFORM 970001-PROCESSA-PESQUISA
                VARYING SGTSLJ03-IC-TAB FROM 1 BY 1
                UNTIL SGTSLJ03-IC-TAB > SGTSLJ03-QT-OCR-TAB

           GOBACK
           .
      *-----------------------------------------------------------------
       100000-VALIDAR-PARAMETROS       SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                        TO PSC-ERRO-PGM

           INITIALIZE SGTSLJ03-RTN-PGM

           MOVE 'N'                         TO SGTSLJ03-IN-CNTO-PSQ

           IF  NOT SGTSLJ03-LST-TIP-COTA-ATUAL
               SET  ERRO-CODIFICADO   TO  TRUE
               MOVE  0116              TO  CD-RTN-PGM
               MOVE 'Ação inválida.'   TO MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           PERFORM 110000-VALIDA-CAMPOS
           .
       100000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       110000-VALIDA-CAMPOS            SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                        TO PSC-ERRO-PGM

           IF SGTSLJ03-LST-TIP-COTA-ATUAL

              IF SGTSLJ03-NM-DCR-TIP-COTA-E  EQUAL SPACES
                 MOVE ALL '%'                     TO  PAR-DCR-COTA
              ELSE
                 PERFORM  970002-FORMATAR-TIT
              END-IF

              IF SGTSLJ03-CD-TIP-EST-COTA-E EQUAL 0
                  MOVE  001                       TO PAR-EST-COTA-INI
                  MOVE  999                       TO PAR-EST-COTA-FIM
              ELSE
                  MOVE  SGTSLJ03-CD-TIP-EST-COTA-E
                                                  TO PAR-EST-COTA-INI
                                                     PAR-EST-COTA-FIM
              END-IF

              IF SGTSLJ03-DT-INC-PER-E  EQUAL SPACES
                  MOVE  '1800-01-01'              TO DT-INC-PER-E
              ELSE
                  MOVE CORR SGTSLJ03-DT-INC-PER-R TO DT-INC-PER-R
              END-IF

              IF SGTSLJ03-DT-FIM-PER-E  EQUAL SPACES
                  MOVE  '9999-12-31'              TO DT-FIM-PER-E
              ELSE
                  MOVE CORR SGTSLJ03-DT-FIM-PER-R TO DT-FIM-PER-R
              END-IF

              IF SGTSLJ03-IN-TIP-COTA-ATI-E EQUAL SPACES
                  MOVE  'S'                       TO PAR-IN-ATI-01
                  MOVE  'N'                       TO PAR-IN-ATI-02
              ELSE
                  MOVE  SGTSLJ03-IN-TIP-COTA-ATI-E
                                                  TO PAR-IN-ATI-01
                                                     PAR-IN-ATI-02
              END-IF

              IF SGTSLJ03-TS-ULT-EST-TIP-COTA EQUAL SPACES
                  MOVE  PAR-TS-INC-PSQ            TO
                                           SGTSLJ03-TS-ULT-EST-TIP-COTA
              END-IF
           END-IF

           .
       110000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       940101-ABRE-CURSOR-PSQ-1       SECTION.
      *-----------------------------------------------------------------
           MOVE 0004                   TO PSC-ERRO-PGM
           EXEC SQL
                OPEN CURSOR-PSQ-1
           END-EXEC
           MOVE 'S'                    TO IN-CURSOR-PSQ-ABTO
           .
       940101-SAI.
           EXIT.

      *-----------------------------------------------------------------
       950101-LER-CURSOR-PSQ-1      SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                   TO PSC-ERRO-PGM
           INITIALIZE REG-WS
           EXEC SQL
                FETCH NEXT ROWSET
                CURSOR-PSQ-1 FOR 0101 ROWS
               INTO   :CD-TIP-COTA-TIT
                    , :NR-VRS-TIP-COTA
                    , :NM-TIP-COTA-TIT
                    , :IN-TIP-COTA-ATI
                    , :TS-EST-TIP-COTA
                    , :CD-TIP-EST-COTA
                    , :CD-USU-ATL-EST
                    , :TX-OBS-EST-TIP
                      :TX-OBS-EST-TIP-N
           END-EXEC
           MOVE SQLERRD(3)             TO QT-REG-SELD
           IF  QT-REG-SELD EQUAL 0101
               SET SGTSLJ03-TEM-MAIS-REGISTRO TO TRUE
               MOVE 0100                TO QT-REG-SELD
           END-IF
           MOVE QT-REG-SELD            TO SGTSLJ03-QT-OCR-TAB
           MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ
           .
       950101-SAI.
           EXIT.
      *-----------------------------------------------------------------
       960101-FECHA-CURSOR-PSQ-1 SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO PSC-ERRO-PGM
           MOVE 'N'                    TO IN-CURSOR-PSQ-ABTO
           EXEC SQL
                CLOSE CURSOR-PSQ-1
           END-EXEC
           .
       960101-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970001-PROCESSA-PESQUISA   SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO PSC-ERRO-PGM

           MOVE CD-TIP-COTA-TIT(SGTSLJ03-IC-TAB)     TO
                         SGTSLJ03-CD-TIP-COTA-TIT(SGTSLJ03-IC-TAB)

           MOVE NR-VRS-TIP-COTA(SGTSLJ03-IC-TAB)          TO
                         SGTSLJ03-NR-VRS-TIP-COTA(SGTSLJ03-IC-TAB)

           MOVE NM-TIP-COTA-TIT(SGTSLJ03-IC-TAB)     TO
                         SGTSLJ03-NM-TIP-COTA-TIT(SGTSLJ03-IC-TAB)

           MOVE IN-TIP-COTA-ATI(SGTSLJ03-IC-TAB)     TO
                         SGTSLJ03-IN-TIP-COTA-ATI(SGTSLJ03-IC-TAB)

           MOVE TS-EST-TIP-COTA(SGTSLJ03-IC-TAB)     TO
                         SGTSLJ03-TS-EST-TIP-COTA(SGTSLJ03-IC-TAB)

           MOVE CD-TIP-EST-COTA(SGTSLJ03-IC-TAB)              TO
                         SGTSLJ03-CD-TIP-EST-COTA(SGTSLJ03-IC-TAB)

           MOVE CD-USU-ATL-EST(SGTSLJ03-IC-TAB)      TO
                         SGTSLJ03-CD-USU-ATL-EST(SGTSLJ03-IC-TAB)

           MOVE TX-OBS-EST-TIP(SGTSLJ03-IC-TAB)      TO
                         SGTSLJ03-TX-OBS-EST-TIP(SGTSLJ03-IC-TAB)
           .
       970001-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970002-FORMATAR-TIT             SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                        TO PSC-ERRO-PGM

           MOVE SPACES                 TO PAR-DCR-COTA
           MOVE ZEROS                  TO QT-CCT-DGTD
                                          QT-CCT-N-DGTD
                                          QT-CCT-PSQ

      *    busca a quantidade de caracteres não digitados, ou seja,
      *    a quantidade de caracteres em brancos à esquerda

           INSPECT FUNCTION REVERSE(SGTSLJ03-NM-DCR-TIP-COTA-E)
               TALLYING QT-CCT-N-DGTD  FOR LEADING SPACES

      *    calcula a quantidade de caracteres digitados

           MOVE LENGTH OF SGTSLJ03-NM-DCR-TIP-COTA-E TO TAM-PAR-TIT-INI
           SUBTRACT QT-CCT-N-DGTD FROM TAM-PAR-TIT-INI
               GIVING QT-CCT-DGTD
               ON SIZE ERROR
                  SET  ERRO-CODIFICADO   TO  TRUE
                  MOVE  0143           TO  CD-RTN-PGM
                  MOVE 'Erro no cálculo do tamanho do texto.'
                                       TO MSG-ERRO
                  PERFORM 999001-ERRO-001
           END-SUBTRACT

      *    calcula a quantidade de caracteres para pesquisa, ou seja,
      *    prevendo a delimitação por 1 carater % no lado direito.

           ADD 1                       TO QT-CCT-DGTD GIVING QT-CCT-PSQ
               ON SIZE ERROR
                  SET  ERRO-CODIFICADO   TO  TRUE
                  MOVE  0143           TO  CD-RTN-PGM
                  MOVE 'Erro no cálculo do tamanho do texto.'
                                       TO MSG-ERRO
                  PERFORM 999001-ERRO-001
           END-ADD

      *    formata campo de pesquisa com delimitador %
      *    e preenche até o final do campo com %

           MOVE SGTSLJ03-NM-DCR-TIP-COTA-E(1:QT-CCT-DGTD)
                                          TO PAR-DCR-COTA(1:QT-CCT-DGTD)
           MOVE ALL '%'       TO PAR-DCR-COTA(QT-CCT-PSQ:QT-CCT-N-DGTD)
           .
       970002-SAI.
           EXIT.

      *--------------------
      * 999000-ERRO                    SECTION
      *--------------------
      * 999001-ERRO-001.
      *--------------------
-INC HLPKE004
-INC HLPKE012

           PERFORM  999999-ENCERRAR
           .
       999001-SAI.
           EXIT.

      *-----------------
      * 999002-ERRO-DB2
      *-----------------
-INC HLPKE005
      *-----------------
      * 999003-TRATA-ABEND-CICS
      * 999004-TRATA-CONDITION-CICS
      *-----------------
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
           MOVE  SGTSLJ03-TIP-RTN-PGM  TO  TIP-RTN-PGM
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

           MOVE 'SGTSLJ03-ACAO'                  TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSLJ03-ACAO                   TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTSLJ03-NM-DCR-TIP-COTA-E'     TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSLJ03-NM-DCR-TIP-COTA-E      TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTSLJ03-CD-TIP-EST-COTA-E'     TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSLJ03-CD-TIP-EST-COTA-E      TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTSLJ03-DT-INC-PER-E'          TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSLJ03-DT-INC-PER-E           TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTSLJ03-DT-FIM-PER-E'          TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSLJ03-DT-FIM-PER-E           TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTSLJ03-IN-TIP-COTA-ATI-E'     TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSLJ03-IN-TIP-COTA-ATI-E      TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTSLJ03-TS-ULT-EST-TIP-COTA'   TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSLJ03-TS-ULT-EST-TIP-COTA    TO  MSGKW999-VL-VRV-MNT
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
      *
           MOVE  RTN-PGM               TO  SGTSLJ03-RTN-PGM

           IF  EXECUCAO-ONLINE
               PERFORM  999000-TRATAR-ERRO-MSG
           END-IF

           GOBACK
           .
       999999-SAI.
           EXIT.
      *---------------------- FIM SGTSLJ03 ----------------------------