190624 CBL ARITH(EXTEND)
190624*-----------------------------------------------------------------
190624* PROGRAMA..: SGTS003X
190624* ANALISTA..: ROSELI AKEMI KAWAHATA
190624* AUTOR.....: ROSELI AKEMI KAWAHATA
190624* OBJETIVO..: Listar os dados das operações que foram importadas
190624*             via CAC/SINQUIA em uma determinada data/hora.
190624* COMPILACAO: COBOL/CICS
190624*-----------------------------------------------------------------
190624* VRS001 19.06.2024 - F8785286 - HU 1210924 - IMPLANTACAO
190624*     Esse módulo é cópia de SGTS003A, com a inclusão de
190624*     SGTS003A-ULT-NR-DVS-TIT na área de entrada , em atendimento
190624*     à RA 4966.
190624*-----------------------------------------------------------------
190624 IDENTIFICATION DIVISION.
190624*-----------------------------------------------------------------
190624 PROGRAM-ID. SGTS003X.
190624
190624*-----------------------------------------------------------------
190624 ENVIRONMENT DIVISION.
190624*-----------------------------------------------------------------
190624 CONFIGURATION SECTION.
190624 SPECIAL-NAMES.
190624     DECIMAL-POINT IS COMMA.
190624
190624*-----------------------------------------------------------------
190624 DATA DIVISION.
190624*-----------------------------------------------------------------
190624
190624*-----------------------------------------------------------------
190624 WORKING-STORAGE SECTION.
190624*-----------------------------------------------------------------
190624 01  CTE-PROG     PIC  X(17) VALUE '*** SGTS003X ***'.
190624 01  CTE-VERS     PIC  X(06) VALUE 'VRS004'.
190624
      *--> Definição dos Nomes dos Programas Acionados
      *--> Módulos acionados / Padrão para Cobolito
       01  TAB-PROGRAMAS-CHAMADOS.
190624     03  SBDATAF                     PIC  X(08) VALUE 'SBDATAF'.
190624     03  MCISI074                    PIC  X(08) VALUE 'MCISI074'.
190624     03  IIBP2000                    PIC  X(08) VALUE 'IIBP2000'.
190624     03  SGTSL037                    PIC  X(08) VALUE 'SGTSL037'.
      *--> Definição dos Variaveis do Cobolito
-INC ALMKCBL1
190624*-----------------------------------------------------------------
190624* Book do programa de ambiente - CICS5000
190624* Definido na work para preservar as informacoes
-INC HLPKE01B
190624 LOCAL-STORAGE SECTION.
190624*-----------------------------------------------------------------
190624*-----------------------------------------------------------------
190624*--> Book dos dados do usuário
-INC IIBKOPRD
190624* book de para acionamento do msg1
-INC IIBKERRO
190624*--> Book do container de Serviços (para acionamento de serviços)
-INC IIBKSRVC
-INC MSGKWMG1
-INC HLPKW999
190624*--> Variaveis para tratamento de CONTAINER
-INC SGTKERR1
190624*-----> BOOK DA ROTINA MCISI074 - CONSULTAR DADOS CLIENTE
190624 01  MCISI074-DADOS.
-INC MCIKI074
190624*--> Book da Subrotina SGTSL037
-INC SGTKL037
190624* ----> book do programa
190624 01  SGTS003X-DADOS.
-INC SGTK003X
190624
190624*--> Indicadores de nulidades
190624 01  NR-UNCO-CTR-OPR-N           PIC S9(4)  COMP-5 VALUE 0.
190624
190624* Area de trabalho
190624
190624 01 QT-REG-SELD                  PIC S9(04) COMP-5 VALUE 0.
190624 01  IC-MCI                      PIC S9(04) COMP-5 VALUE 0.
190624 01  CD-CHV-USU                  PIC  X(08)        VALUE ' '.
190624
190624*--> Guarda da carteira para verificar se responsabilidade
190624*    DIMEC
190624 01  WS-CD-CTRA-ANT              PIC S9(09) COMP-5 VALUE 0.
190624 01  WS-TX-VCLD-CRCT             PIC X(20)         VALUE ' '.
190624 01  IC-CTRA                     PIC S9(04) COMP-5 VALUE 0.
190624
190624* Variavel(is) de controle do cursor(es)
190624
190624 01  CTL-CURSOR-PSQ.
190624     03  CD-FIM-CURSOR-PSQ       PIC S9(04) COMP-5 VALUE 0.
190624         88  FIM-CURSOR-PSQ                        VALUE +100.
190624     03  IN-CURSOR-PSQ-ABTO      PIC  X(01) VALUE 'N'.
190624         88  CURSOR-PSQ-ABTO                VALUE 'S'.
190624
190624 01  REG-WS.
190624     03 CD-CTRA-TIT-VL           PIC S9(09) COMP-5 VALUE ZEROS.
190624     03 CD-TIT-OPR-ETRD          PIC  X(12)        VALUE ' '.
190624     03 CD-OPR-ETRD-MCP          PIC S9(11) COMP-3 VALUE ZEROS.
190624     03 NR-DVS-TIT               PIC S9(04) COMP-5 VALUE ZEROS.
190624     03 NR-DVS-TIT-N             PIC S9(04) COMP-5 VALUE ZEROS.
190624     03 NR-VRS-OPR-ETRD          PIC S9(04) COMP-5 VALUE ZEROS.
190624     03 DT-PSC                   PIC  X(10)        VALUE ' '.
190624     03 VL-SDO-TIT-DT-PSC        PIC S9(15)V99 COMP-3 VALUE ZEROS.
190624     03 CD-EST-RSTD-ITE          PIC S9(04) COMP-5 VALUE ZEROS.
190624     03 CD-CLI                   PIC S9(09) COMP-5 VALUE ZEROS.
190624     03 NR-SEQL-PSC-DRIA         PIC S9(04) COMP-5 VALUE ZEROS.
190624     03 NR-UNCO-CTR-OPR          PIC S9(17) COMP-3 VALUE ZEROS.
190624
190624 01  IN-FIM-PSQ-1                PIC  X(01) VALUE ' '.
190624     88  FIM-LISTA                          VALUE 'S'.
190624
190624*--> Areas de Controle de Programa Chamado (CONTAINER)
190624 01  CTL-PGM-CHMD.
190624     03  NM-PGM-CHMD             PIC  X(08)         VALUE ' '.
190624     03  FILLER                  PIC  X(01)         VALUE ' '.
190624     03  MSG-PGM-CHMD            PIC  X(111)        VALUE ' '.
190624
190624 01  WS-ERRO-CICS.
190624     03  WS-RESP1                PIC  9(008).
190624     03  WS-RESP2                PIC  9(008).
190624
190624*--> Máscaras edição de variáveis p/ monitoração mensagens de erro
190624 01  MCR-S9-15                   PIC      ---------------9.
190624 01  MCR-S9-29          PIC  ---.---.---.---.--9,99999999999999.
190624 01  VL-15V2                     PIC  ---.---.---.---.--9,99.
190624*-----------------------------------------------------------------
190624
190624 01  AUX-DATA                    PIC X(10)  VALUE '0000-00-00'.
190624 01  AUX-DATA-R  REDEFINES AUX-DATA.
190624     03  ANO                     PIC 9(04).
190624     03  FILLER                  PIC X(01).
190624     03  MES                     PIC 9(02).
190624     03  FILLER                  PIC X(01).
190624     03  DIA                     PIC 9(02).
190624
190624 01  AUX-DT                      PIC X(10) VALUE '00.00.0000'.
190624 01  AUX-DT-R  REDEFINES AUX-DT.
190624     03  DIA                     PIC 9(02).
190624     03  FILLER                  PIC X(01).
190624     03  MES                     PIC 9(02).
190624     03  FILLER                  PIC X(01).
190624     03  ANO                     PIC 9(04).
190624
190624 01  AUX-HORARIO                 PIC X(08) VALUE '00:00:00'.
190624 01  AUX-HORARIO-R  REDEFINES AUX-HORARIO.
190624     03  HORA                    PIC 9(02).
190624     03  FILLER                  PIC X(01).
190624     03  MINUTO                  PIC 9(02).
190624     03  FILLER                  PIC X(01).
190624     03  SEGUNDO                 PIC 9(02).
190624
190624 01 AUX-ERRO                     PIC X(30).
190624 01 AUX-ERRO-R  REDEFINES AUX-ERRO.
190624    03 AUX-CD-ERRO               PIC S9(04) COMP.
190624
190624 77  EMISSOR                     PIC S9(04) COMP-5 VALUE 1.
190624
190624*--> Carteira será fixada enquanto não houver a migração completa
190624*    da base de dados
190624 01  CARTEIRA-TVM                PIC S9(09) COMP-5  VALUE 500000.
190624
190624* Area da subrotina SBDATA
190624 01  FUNCAO                      PIC  X(003) VALUE 'F01'.
190624 01  AREA-WORK                   PIC  X(4095).
190624 01  AREA-WORK-RED  REDEFINES    AREA-WORK.
190624     03 A-TAM                    PIC  9(0008) COMP.
190624     03 FILLER                   PIC  X(4071).
190624     03 A-FIM                    PIC  X(0020).
190624 01  ARG01                       PIC  9(008) VALUE 0.
190624 01  ARG01-R REDEFINES ARG01.
190624     03  DIA                     PIC  9(002).
190624     03  MES                     PIC  9(002).
190624     03  ANO                     PIC  9(004).
190624 01  ARG02                       PIC  9(001) VALUE 0.
190624
190624     EXEC SQL
190624         INCLUDE SQLCA
190624     END-EXEC.
190624
190624     EXEC SQL
190624       DECLARE CURSOR-PSQ CURSOR FOR
190624       SELECT
190624           A.CD_CTRA_TIT_VL,
190624           A.CD_TIT_OPR_ETRD,
190624           A.NR_DVS_TIT,
190624           A.DT_PSC,
190624           A.VL_SDO_TIT_DT_PSC,
190624           A.CD_EST_RSTD_ITE,
190624           A.NR_SEQL_PSC_DRIA
190624        FROM
190624           DB2SGT.PSC_DRIA_SDO_OPR A
190624        WHERE
190624              A.DT_IMP_PSC_SDO = :AUX-DATA
190624          AND A.HR_IMP_PSC_SDO = :AUX-HORARIO
190624          AND A.DT_PSC         < CURRENT DATE
190624*---- paginação
190624          AND (A.CD_CTRA_TIT_VL > :SGTS003X-ULT-CD-CTRA-TIT-VL
190624          OR  (A.CD_CTRA_TIT_VL = :SGTS003X-ULT-CD-CTRA-TIT-VL
190624          AND  A.CD_TIT_OPR_ETRD  >
190624                                  :SGTS003X-ULT-CD-TIT-OPR-ETRD)
190624          OR  (A.CD_CTRA_TIT_VL   = :SGTS003X-ULT-CD-CTRA-TIT-VL
190624          AND  A.CD_TIT_OPR_ETRD  =
190624                                  :SGTS003X-ULT-CD-TIT-OPR-ETRD
190624          AND  A.NR_DVS_TIT       >
190624                                  :SGTS003X-ULT-NR-DVS-TIT))
190624        ORDER BY
190624               A.CD_CTRA_TIT_VL,
190624               A.CD_TIT_OPR_ETRD,
190624               A.NR_DVS_TIT
190624     END-EXEC.
190624*-----------------------------------------------------------------
190624 PROCEDURE DIVISION.
190624*-----------------------------------------------------------------
      *--> Habilita Testes no Cobolito e Debug de Operacoes IIB
-INC ALMKCBL2
190624* Habilita tratamento automatico de erros DB2
-INC HLPKE997
190624* Habilita captura automatica de abend e erros CICS
-INC HLPKE999
190624
190624     MOVE 4095                   TO A-TAM
190624     MOVE 'SBDATAF -WRKAREA-FIM' TO A-FIM
190624
190624     PERFORM 100000-DESCOBRIR-CANAL
190624
190624     MOVE 'SGTS003X'             TO NM-PGM
190624
190624     PERFORM 974000-LER-DADOS-OPRD
190624     PERFORM 983000-EXCLUIR-RESPOSTA-ERRO
190624     PERFORM 130000-RECEBER-REQUISICAO
190624     PERFORM 140000-VALIDAR-PARAMETRO
190624     PERFORM 970101-ABRE-CURSOR-PSQ
190624
190624     PERFORM 200000-PROCESSA-PESQUISA
190624              VARYING SGTS003X-IC-TAB FROM 1 BY 1
190624              UNTIL FIM-LISTA
190624
190624     PERFORM 970104-FECHA-CURSOR-PSQ
190624
190624     PERFORM 240000-OBTER-NOME-CLIENTE
190624
190624     PERFORM 300000-POSTA-RESPOSTA
190624     PERFORM 999999-ENCERRAR
190624
190624     GOBACK
190624     .
190624*-----------------------------------------------------------------
190624 100000-DESCOBRIR-CANAL          SECTION.
190624*-----------------------------------------------------------------
190624     MOVE 0001                   TO MSG1-CD-SEQL-ERRO
190624
190624*--> Por causa da captura automática de erro CICS, deve ser
190624*--> previsto      o erro de container
190624     SET  ERRO-CANAL             TO TRUE
190624
190624     EXEC CICS ASSIGN
190624        CHANNEL ( NM-CNL )
190624     END-EXEC
190624
190624*--> Ao chegar aqui, há certeza de que não houve erro algum
190624     SET  SEM-ERRO               TO TRUE
190624     .
190624 100000-SAI.
190624     EXIT.
190624*-----------------------------------------------------------------
190624 130000-RECEBER-REQUISICAO       SECTION.
190624*-----------------------------------------------------------------
190624     MOVE 0002                   TO MSG1-CD-SEQL-ERRO
190624
190624*--> Configura dados para leitura do container de requisição
190624     SET  GET-CONTAINER          TO TRUE
190624
190624*--> Por causa da captura automática de erro CICS, deve ser
190624*--> previsto o erro de container
190624     SET  ERRO-CONTAINER         TO TRUE
190624
190624     MOVE ZEROS                  TO QT-CCT-LIDO
190624     MOVE LENGTH OF SGTS003X-RQSC      TO QT-CCT-EPRO
190624     EXEC CICS GET
190624        CONTAINER ( 'SGTS003X-RQSC' )
190624        CHANNEL   ( NM-CNL          )
190624        NODATA
190624        FLENGTH ( QT-CCT-LIDO )
190624     END-EXEC
190624
190624     IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
190624         SET ERRO-TAMANHO-AREA   TO TRUE
190624         PERFORM 999000-GRAVA-ERRO-E-ENCERRA
190624     END-IF
190624     EXEC CICS GET
190624        CONTAINER ( 'SGTS003X-RQSC' )
190624        CHANNEL   ( NM-CNL      )
190624        FLENGTH ( LENGTH OF SGTS003X-RQSC )
190624        INTO    ( SGTS003X-RQSC )
190624     END-EXEC
190624
190624* --> Ao chegar aqui, há certeza de que não houve erro algum
190624     SET  SEM-ERRO               TO TRUE
190624     .
190624 130000-SAI.
190624     EXIT.
190624*-----------------------------------------------------------------
190624 140000-VALIDAR-PARAMETRO        SECTION.
190624*-----------------------------------------------------------------
190624     MOVE 0003                   TO MSG1-CD-SEQL-ERRO
190624
190624     IF  SGTS003X-DT-IMP-PSC-SDO EQUAL SPACES
190624         SET ERRO-PROGRAMADO     TO TRUE
190624         MOVE 0738               TO MSG1-CD-ERRO-TS
190624         MOVE 'Data importação posição saldo inválida.'
190624                                 TO MSG1-TX-ERRO-TS
190624         SET MNT-VRV             TO TRUE
190624         PERFORM 999000-GRAVA-ERRO-E-ENCERRA
190624     END-IF
190624
190624*    Verifica se a data é válida
190624     MOVE CORR SGTS003X-DT-IMP-PSC-SDO-R
190624                                 TO ARG01-R
190624     PERFORM 940000-VALIDA-DATA
190624
190624*--> Return code = 24 - Data Inválida.
190624*                  22 - Conteudo alfa em argumento numerico
      * MOCK-POINT MCKV03
190624     IF  RETURN-CODE EQUAL 24 OR 22
190624         SET ERRO-PROGRAMADO     TO TRUE
190624         MOVE 0738               TO MSG1-CD-ERRO-TS
190624         MOVE 'Data importação posição saldo inválida.'
190624                                 TO MSG1-TX-ERRO-TS
190624         SET MNT-VRV             TO TRUE
190624         PERFORM 999000-GRAVA-ERRO-E-ENCERRA
190624     END-IF
190624
190624     IF  SGTS003X-HR-IMP-PSC-SDO EQUAL SPACES
190624         SET ERRO-PROGRAMADO     TO TRUE
190624         MOVE 0737               TO MSG1-CD-ERRO-TS
190624         MOVE 'Horário importação posição saldo inválido'
190624                                 TO MSG1-TX-ERRO-TS
190624         SET MNT-VRV             TO TRUE
190624         PERFORM 999000-GRAVA-ERRO-E-ENCERRA
190624     END-IF
190624
190624     MOVE  CORR SGTS003X-DT-IMP-PSC-SDO-R
190624                                 TO AUX-DATA-R
190624     MOVE  CORR SGTS003X-HR-IMP-PSC-SDO-R
190624                                 TO AUX-HORARIO-R
190624     .
190624 140000-SAI.
190624     EXIT.
190624*-----------------------------------------------------------------
190624 200000-PROCESSA-PESQUISA        SECTION.
190624*-----------------------------------------------------------------
190624     MOVE 0004                   TO MSG1-CD-SEQL-ERRO
190624
190624     PERFORM 970102-LER-CURSOR-PSQ
190624
190624     IF  NOT FIM-CURSOR-PSQ
190624         IF  CD-CTRA-TIT-VL      NOT EQUAL  WS-CD-CTRA-ANT
190624             IF  WS-CD-CTRA-ANT  EQUAL  ZEROS
190624                 PERFORM 210000-PSQ-CARACT-CARTEIRA
190624             END-IF
190624             PERFORM VARYING IC-CTRA  FROM 1 BY 1
190624                     UNTIL IC-CTRA > SGTSL037-QT-OCR-TAB
190624                 MOVE CD-CTRA-TIT-VL
190624                                 TO  WS-CD-CTRA-ANT
190624                 IF  CD-CTRA-TIT-VL  EQUAL
190624                                 SGTSL037-CD-CTRA-TVM(IC-CTRA)
190624                     MOVE SGTSL037-TX-VCLD-CRCT(IC-CTRA)
190624                                 TO  WS-TX-VCLD-CRCT
190624                     MOVE 999    TO  IC-CTRA
190624                 ELSE
190624                     MOVE 'N'    TO  WS-TX-VCLD-CRCT
190624                 END-IF
190624             END-PERFORM
190624         END-IF
190624
190624         IF  WS-TX-VCLD-CRCT     EQUAL  'S'
190624             PERFORM 220000-OBTER-OPERACAO
190624             PERFORM 230000-OBTER-CD-CLIENTE
190624         END-IF
190624     END-IF
190624
190624     IF  FIM-CURSOR-PSQ  OR  SGTS003X-QT-OCR-TAB EQUAL 100
190624         IF FIM-CURSOR-PSQ
190624            MOVE 'N'             TO  SGTS003X-IN-CNTO-PSQ
190624         ELSE
190624            MOVE 'S'             TO  SGTS003X-IN-CNTO-PSQ
190624         END-IF
190624         SET FIM-LISTA           TO  TRUE
190624     END-IF
190624     .
190624 200000-SAI.
190624     EXIT.
190624*-----------------------------------------------------------------
190624 210000-PSQ-CARACT-CARTEIRA      SECTION.
190624*-----------------------------------------------------------------
190624     MOVE 0005                   TO MSG1-CD-SEQL-ERRO
190624
190624     MOVE SGTSL037               TO NM-PGM
190624
190624     SET SGTSL037-LISTA-POR-TIP-ATB
190624                                 TO TRUE
190624
190624     MOVE CD-CTRA-TIT-VL         TO SGTSL037-E-CD-CTRA-TVM
190624
190624     MOVE 8                      TO
190624                                 SGTSL037-E-CD-TIP-ATB-CTRA-01
190624
190624     PERFORM 900000-EXECUTAR-MODULO
190624     .
190624 210000-SAI.
190624     EXIT.
190624*-----------------------------------------------------------------
190624 220000-OBTER-OPERACAO           SECTION.
190624*-----------------------------------------------------------------
190624     MOVE 0006                  TO MSG1-CD-SEQL-ERRO
190624
190624     EXEC SQL
190624        SELECT A.CD_OPR_ETRD_MCP,
190624               A.NR_VRS_OPR_ETRD,
190624               B.NR_UNCO_CTR_OPR
190624
190624           INTO :CD-OPR-ETRD-MCP
190624                ,:NR-VRS-OPR-ETRD
190624                ,:NR-UNCO-CTR-OPR
190624                 :NR-UNCO-CTR-OPR-N
190624
190624           FROM DB2SGT.EST_VRS_OPR_ETRD A
190624                INNER JOIN
190624                 DB2SGT.OPR_ETRD_MCP B
190624                 ON
190624                     A.CD_CTRA_TIT_VL  = B.CD_CTRA_TIT_VL
190624                 AND A.CD_OPR_ETRD_MCP = B.CD_OPR_ETRD_MCP
190624                 AND A.NR_VRS_OPR_ETRD = B.NR_VRS_OPR_ETRD
190624
190624            WHERE
190624                 DATE(A.TS_EST_VRS)    <= :AUX-DATA
190624                 AND A.CD_TIP_EST_VRS  = 3
190624                 AND A.CD_CTRA_TIT_VL  = :CD-CTRA-TIT-VL
190624                 AND B.CD_TIT_OPR_ETRD = :CD-TIT-OPR-ETRD
190624                 AND B.NR_DVS_TIT      = :NR-DVS-TIT
190624
190624                 ORDER BY
190624                  A.TS_EST_VRS DESC,
190624                  B.NR_VRS_OPR_ETRD DESC
190624                  FETCH FIRST 1 ROW ONLY
190624        END-EXEC
190624
      * MOCK-POINT MCKV02
190624     EVALUATE SQLCODE
190624        WHEN ZEROS
190624            CONTINUE
190624        WHEN 100
190624            SET   ERRO-PROGRAMADO    TO  TRUE
190624            MOVE  0353               TO  MSG1-CD-ERRO-TS
190624            MOVE 'Título não vinculado a operação estruturada.'
190624                                     TO  MSG1-TX-ERRO-TS
190624            SET MNT-VRV              TO TRUE
190624            PERFORM 999000-GRAVA-ERRO-E-ENCERRA
190624       WHEN OTHER
190624            SET   ERRO-PROGRAMADO    TO  TRUE
190624            MOVE  9997               TO  MSG1-CD-ERRO-TS
190624            MOVE 'Erro de acesso ao banco de dados do sistema.'
190624                                     TO  MSG1-TX-ERRO-TS
190624            SET MNT-VRV              TO TRUE
190624            PERFORM 999000-GRAVA-ERRO-E-ENCERRA
190624     END-EVALUATE
190624
190624     ADD  1                      TO  SGTS003X-QT-OCR-TAB
190624
190624     MOVE CD-CTRA-TIT-VL         TO
190624               SGTS003X-CD-CTRA-TIT-VL(SGTS003X-QT-OCR-TAB)
190624     MOVE CD-TIT-OPR-ETRD        TO
190624               SGTS003X-CD-TIT-OPR-ETRD(SGTS003X-QT-OCR-TAB)
190624     MOVE NR-DVS-TIT             TO
190624               SGTS003X-NR-DVS-TIT (SGTS003X-QT-OCR-TAB)
190624     MOVE VL-SDO-TIT-DT-PSC      TO
190624               SGTS003X-VL-SDO-TIT-DT-PSC(SGTS003X-QT-OCR-TAB)
190624
190624     MOVE DT-PSC                 TO  AUX-DT
190624     MOVE CORR AUX-DT-R          TO  ARG01-R
190624     MOVE ARG01-R                TO
190624               SGTS003X-DT-PSC(SGTS003X-QT-OCR-TAB)
190624
190624     MOVE CD-EST-RSTD-ITE        TO
190624               SGTS003X-CD-EST-RSTD-ITE(SGTS003X-QT-OCR-TAB)
190624     MOVE NR-SEQL-PSC-DRIA       TO
190624               SGTS003X-NR-SEQL-PSC-DRIA(SGTS003X-QT-OCR-TAB)
190624     MOVE CD-OPR-ETRD-MCP        TO
190624               SGTS003X-CD-OPR-ETRD-MCP(SGTS003X-QT-OCR-TAB)
190624     MOVE NR-VRS-OPR-ETRD        TO
190624               SGTS003X-NR-VRS-OPR-ETRD (SGTS003X-QT-OCR-TAB)
190624     MOVE NR-UNCO-CTR-OPR        TO
190624               SGTS003X-NR-UNCO-CTR-OPR(SGTS003X-QT-OCR-TAB)
190624     .
190624 220000-SAI.
190624     EXIT.
190624*-----------------------------------------------------------------
190624 230000-OBTER-CD-CLIENTE         SECTION.
190624*-----------------------------------------------------------------
190624     MOVE 0007                  TO MSG1-CD-SEQL-ERRO
190624
190624     EXEC SQL
190624        SELECT C.CD_CLI
190624           INTO :CD-CLI
190624        FROM DB2SGT.CLI_OPR_ETRD C
190624        WHERE  C.CD_CTRA_TIT_VL  = :CD-CTRA-TIT-VL
190624           AND C.CD_OPR_ETRD_MCP = :CD-OPR-ETRD-MCP
190624           AND C.NR_VRS_OPR_ETRD = :NR-VRS-OPR-ETRD
190624           AND C.CD_TIP_FUC_CLI  = :EMISSOR
190624        FETCH FIRST 1 ROW ONLY
190624     END-EXEC
190624
190624     MOVE CD-CLI                 TO
190624                              SGTS003X-CD-CLI(SGTS003X-QT-OCR-TAB)
190624     .
190624 230000-SAI.
190624     EXIT.
190624*-----------------------------------------------------------------
190624 240000-OBTER-NOME-CLIENTE       SECTION.
190624*-----------------------------------------------------------------
190624     MOVE 0008                   TO MSG1-CD-SEQL-ERRO
190624
190624     MOVE MCISI074               TO  NM-PGM
190624
190624     INITIALIZE MCISI074-DADOS REPLACING  ALPHANUMERIC BY SPACES
190624                                          NUMERIC      BY ZEROS
190624
190624     MOVE CD-CHV-USU             TO  MCISI074-CD-CHV-USU
190624     MOVE SGTS003X-QT-OCR-TAB    TO  MCISI074-QT-REG-ENTD
190624     PERFORM VARYING IC-MCI FROM 1 BY 1
190624             UNTIL IC-MCI > SGTS003X-QT-OCR-TAB
190624
190624         MOVE SGTS003X-CD-CLI(IC-MCI)
190624                              TO  MCISI074-CD-CLI-PF-ENTD(IC-MCI)
190624     END-PERFORM
190624
190624     PERFORM 910000-CHAMAR-SERVICO
190624
190624     IF  MCISI074-CD-RTN             NOT  EQUAL  ZEROES
190624         SET ERRO-PROGRAMA-ACIONADO  TO TRUE
190624         MOVE NM-PGM                 TO MSG1-NM-PGM-ACND
190624         MOVE MCISI074-NR-SEQL-ERRO  TO MSG1-CD-SEQL-PGM-ACND
190624         MOVE MCISI074-TX-MSG-RTN    TO MSG1-TX-RTN-PGM-ACND
190624         MOVE MCISI074-CD-RTN        TO MSG1-CD-RTN-PGM-ACND
190624
190624         MOVE 3004                   TO MSG1-CD-ERRO-TS
190624         STRING  'Erro ao acionar o módulo ' NM-PGM '.'
190624                 DELIMITED BY SIZE
190624                 INTO                MSG1-TX-ERRO-TS
190624*         Monitorar variáveis do módulo chamado
190624          PERFORM 999000-GRAVA-ERRO-E-ENCERRA
190624     END-IF
190624
190624     PERFORM VARYING SGTS003X-IC-TAB FROM 1 BY 1
190624             UNTIL SGTS003X-IC-TAB > MCISI074-QT-REG-SAID
190624
190624         MOVE MCISI074-NM-CLI-PF-SAID(SGTS003X-IC-TAB)  TO
190624                                 SGTS003X-NM-CLI(SGTS003X-IC-TAB)
190624     END-PERFORM
190624     .
190624 240000-SAI.
190624     EXIT.
190624*-----------------------------------------------------------------
190624 300000-POSTA-RESPOSTA           SECTION.
190624*-----------------------------------------------------------------
190624     MOVE 0009                   TO MSG1-CD-SEQL-ERRO
190624
190624     SET PUT-CONTAINER           TO TRUE
190624
190624*--> Por causa da captura automática de erro CICS, deve ser
190624*--> previsto o erro de container
190624     SET  ERRO-CONTAINER         TO TRUE
190624
190624     EXEC CICS PUT
190624        CONTAINER ( 'SGTS003X-RPST' )
190624        CHANNEL   ( NM-CNL      )
190624        FLENGTH ( LENGTH OF SGTS003X-RPST )
190624        FROM    ( SGTS003X-RPST )
190624     END-EXEC
190624
190624     SET SEM-ERRO TO  TRUE
190624     .
190624 300000-SAI.
190624     EXIT.
190624*----------------------------------------------------------------*
190624*                                                                *
190624*  SECTION DA FAIXA DE 900000 A 969900 PARA DEMAIS ROTINAS       *
190624*                                                                *
190624*----------------------------------------------------------------*
190624* Executa um módulo implementado com CONTAINER                   *
190624* Não incluir nessa section os módulos chamados via COMMAREA     *
190624*                                                                *
190624*----------------------------------------------------------------*
190624 900000-EXECUTAR-MODULO          SECTION.
190624*----------------------------------------------------------------*
190624*    Grava o container de requisição do módulo a executar
190624     PERFORM 982000-GRAVAR-CONTAINER-RQSC
190624
190624*    Limpa a resposta e erro antes de executar o módulo
190624     PERFORM 983000-EXCLUIR-RESPOSTA-ERRO
190624
190624*    Não há captura automática de erro CICS por usar RESP e RESP2
190624     EVALUATE NM-PGM
190624
190624         WHEN SGTSL037
190624             EXEC CICS LINK
190624                       PROGRAM ( SGTSL037  )
190624                       CHANNEL ( NM-CNL    )
190624                       RESP    ( MSG1-RESP1)
190624                       RESP2   ( MSG1-RESP2)
190624             END-EXEC
190624
190624         WHEN OTHER
190624             SET   ERRO-PROGRAMADO   TO  TRUE
190624              MOVE  9006             TO  MSG1-CD-ERRO-TS
190624              STRING  'Não definida a execução do módulo'
190624                      NM-PGM '.'
190624                      DELIMITED BY SIZE
190624                      INTO           MSG1-TX-ERRO-TS
190624             PERFORM 999000-GRAVA-ERRO-E-ENCERRA
190624     END-EVALUATE
190624*
190624     EVALUATE TRUE
190624         WHEN MSG1-RESP1         EQUAL DFHRESP(NORMAL)
190624              CONTINUE
190624         WHEN OTHER
190624              SET   ERRO-CICS    TO  TRUE
190624              MOVE  3004         TO  MSG1-CD-ERRO-TS
190624              STRING
190624              'Erro  ao acionar o módulo  ' NM-PGM  ', '
190624              'RESP  = '  MSG1-RESP1  ', '
190624              'RESP2 = '  MSG1-RESP2
190624              DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
190624              PERFORM 999000-GRAVA-ERRO-E-ENCERRA
190624     END-EVALUATE
190624
190624*    Efetua a leitura do Container de Erro
190624     PERFORM 972000-LER-CONTAINER-DE-ERRO
190624
190624*    Efetua a leitura do Container de Resposta
190624     PERFORM 973000-LER-RESPOSTA
190624     .
190624 900000-SAI.
190624     EXIT.
190624*-----------------------------------------------------------------
190624 910000-CHAMAR-SERVICO           SECTION.
190624*--> Acionar programa IIBP2000 para requisição do serviço.
190624*-----------------------------------------------------------------
190624
190624*--> Grava o container com dados do serviço/operação a chamar
190624     PERFORM 981000-GRAVAR-CONTAINER-SRVC
190624
190624*--> Grava o container de requisicao do modulo a executar
190624     PERFORM 982000-GRAVAR-CONTAINER-RQSC
190624
190624*--> Limpa a resposta e erro antes de executar o modulo
190624     PERFORM 983000-EXCLUIR-RESPOSTA-ERRO
190624
190624*--> Não há captura automática de erro CICS por usar RESP e RESP2
190624     EXEC CICS LINK
190624               PROGRAM   ( IIBP2000     )
190624               CHANNEL   ( NM-CNL       )
190624               RESP      ( MSG1-RESP1   )
190624               RESP2     ( MSG1-RESP2   )
190624     END-EXEC
190624
190624     EVALUATE TRUE
190624         WHEN MSG1-RESP1         EQUAL DFHRESP(NORMAL)
190624              CONTINUE
190624         WHEN OTHER
190624              SET   ERRO-CICS    TO  TRUE
190624              MOVE  9000         TO  MSG1-CD-ERRO-TS
190624              STRING  'Erro ao acionar serviço: '
190624                      SRVC-ID-SRVC '.' SRVC-VRS-SRVC '.'
190624                      SRVC-ID-OPR  '.' SRVC-VRS-OPR
190624                      'via IIBP2000, ' 'RESP=' MSG1-RESP1
190624                                     ', RESP-2=' MSG1-RESP2 '.'
190624                      DELIMITED BY SIZE
190624                      INTO  MSG1-TX-ERRO-TS
190624              END-STRING
190624
190624              PERFORM 999000-GRAVA-ERRO-E-ENCERRA
190624     END-EVALUATE
190624
190624*--> Efetua a leitura do Container de Erro
190624     PERFORM 972000-LER-CONTAINER-DE-ERRO
190624
190624*--> Efetua a leitura do Container de Resposta
190624     PERFORM 973000-LER-RESPOSTA
190624     .
190624 910000-SAI.
190624     EXIT.
190624*----------------------------------------------------------------*
190624 940000-VALIDA-DATA              SECTION.
190624*----------------------------------------------------------------*
190624     MOVE 0010                   TO MSG1-CD-SEQL-ERRO
190624
190624     MOVE 4                      TO ARG02
190624     CALL SBDATAF USING FUNCAO
190624                        AREA-WORK
190624                        ARG01
190624                        ARG02
190624
190624*--> Return code = 24 - Data Inválida.
190624*                = 22 - Conteudo alfa em argumento numerico
190624
      * MOCK-POINT MCKV03
190624     IF  NOT (RETURN-CODE        EQUAL ZEROS OR 24 OR 22)
190624         SET ERRO-PROGRAMADO     TO TRUE
190624         MOVE 0051               TO MSG1-CD-ERRO-TS
190624         MOVE RETURN-CODE        TO MSG1-CD-RTN-PGM-ACND
190624         MOVE 'Erro no tratamento de datas.'
190624                                 TO MSG1-TX-ERRO-TS
190624         MOVE SBDATAF            TO MSG1-NM-PGM-ACND
190624         SET MNT-VRV             TO TRUE
190624         PERFORM 999000-GRAVA-ERRO-E-ENCERRA
190624     END-IF
190624
190624     MOVE RETURN-CODE        TO MSG1-CD-RTN-PGM-ACND
190624     .
190624 940000-SAI.
190624     EXIT.
190624*-----------------------------------------------------------------
190624 970101-ABRE-CURSOR-PSQ          SECTION.
190624*-----------------------------------------------------------------
190624     MOVE 0011                   TO MSG1-CD-SEQL-ERRO
190624
190624     EXEC SQL
190624          OPEN CURSOR-PSQ
190624     END-EXEC
190624     MOVE 'S'                    TO IN-CURSOR-PSQ-ABTO
190624     .
190624 970101-SAI.
190624     EXIT.
190624*-----------------------------------------------------------------
190624 970102-LER-CURSOR-PSQ           SECTION.
190624*-----------------------------------------------------------------
190624     MOVE 0012                   TO MSG1-CD-SEQL-ERRO
190624
190624     INITIALIZE REG-WS
190624
190624     EXEC SQL
190624          FETCH CURSOR-PSQ
190624         INTO :CD-CTRA-TIT-VL
190624            , :CD-TIT-OPR-ETRD
190624            , :NR-DVS-TIT
190624            , :DT-PSC
190624            , :VL-SDO-TIT-DT-PSC
190624            , :CD-EST-RSTD-ITE
190624            , :NR-SEQL-PSC-DRIA
190624     END-EXEC
190624
      * MOCK-POINT MCKV01
190624     MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ
190624     .
190624 970102-SAI.
190624     EXIT.
190624*-----------------------------------------------------------------
190624 970104-FECHA-CURSOR-PSQ         SECTION.
190624*-----------------------------------------------------------------
190624     MOVE 0013                   TO MSG1-CD-SEQL-ERRO
190624
190624     MOVE 'N'                    TO IN-CURSOR-PSQ-ABTO
190624     EXEC SQL
190624          CLOSE CURSOR-PSQ
190624     END-EXEC
190624     .
190624 970104-SAI.
190624     EXIT.
190624*-----------------------------------------------------------------
190624 972000-LER-CONTAINER-DE-ERRO    SECTION.
190624*--> A presença do container PRM-ERRO indica que houve erro de
190624*    integração, a ausência do container indica sucesso.
190624*-----------------------------------------------------------------
190624
190624*--> Le o container de erro dos modulos chamados
190624     SET   GET-CONTAINER         TO  TRUE
190624     MOVE  'PRM-ERRO'            TO  NM-CTNR
190624     INITIALIZE                      IIBKERRO-PRM-ERRO
190624
190624*--> Não há captura automática de erro CICS por usar RESP e RESP2
190624     EXEC CICS GET
190624          CONTAINER ( NM-CTNR                     )
190624          INTO      ( IIBKERRO-PRM-ERRO           )
190624          FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
190624          RESP      ( MSG1-RESP1                  )
190624          RESP2     ( MSG1-RESP2                  )
190624     END-EXEC
190624
190624     EVALUATE TRUE
190624*        Caso deseje personalizar ou não interromper o
190624*        processamento, alterar para não dar erro aqui SOMENTE
190624*        PARA O M DULO CHAMADO e tratar na section chamadora
190624         WHEN MSG1-RESP1         EQUAL DFHRESP(CONTAINERERR)
190624              CONTINUE
190624         WHEN MSG1-RESP1         EQUAL DFHRESP(NORMAL)
190624              SET   ERRO-PROGRAMA-ACIONADO TO  TRUE
190624              MOVE  NM-PGM       TO  MSG1-NM-PGM-ACND
190624              IF NM-PGM(1:3)     EQUAL 'SGT'
190624                 MOVE  IIBKERRO-CD-ERRO
190624                                 TO  MSG1-CD-RTN-PGM-ACND
190624                                     MSG1-CD-ERRO-TS
190624                 MOVE  IIBKERRO-TX-ERRO
190624                                 TO  MSG1-TX-RTN-PGM-ACND
190624                                     MSG1-TX-ERRO-TS
190624              ELSE
190624                 MOVE 3004       TO MSG1-CD-ERRO-TS
190624                 STRING  'Erro ao acionar o módulo ' NM-PGM '.'
190624                          DELIMITED BY SIZE
190624                          INTO   MSG1-TX-ERRO-TS
190624
190624                 MOVE IIBKERRO-TX-ERRO
190624                                 TO MSG1-TX-RTN-PGM-ACND
190624                 MOVE IIBKERRO-CD-ERRO
190624                                 TO MSG1-CD-RTN-PGM-ACND
190624
190624              END-IF
190624           PERFORM 999000-GRAVA-ERRO-E-ENCERRA
190624         WHEN OTHER
190624              SET   ERRO-CONTAINER
190624                                 TO  TRUE
190624              PERFORM 999002-TRATA-CONDITION-CICS
190624     END-EVALUATE
190624     .
190624 972000-SAI.
190624     EXIT.
190624*-----------------------------------------------------------------
190624 973000-LER-RESPOSTA             SECTION.
190624*--> Recupera o container com os dados de resposta (saída).
190624*    Modulo CONTAINER  -     NOME_PGM-RPST
190624*    Modulo IIB (FIXO) -     PRM-DADOS-RPST
190624*----------------------------------------------------------------
190624
190624*--> Monta o nome dos container de acordo com modulo a executar
190624     MOVE  SPACES                TO  NM-CTNR
190624
190624*--> Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
190624*    nome_modulo -RPST
190624
190624     EVALUATE NM-PGM
190624         WHEN MCISI074
190624              MOVE  'PRM-DADOS-RPST'
190624                                 TO  NM-CTNR
190624         WHEN OTHER
190624              MOVE  NM-PGM       TO  NM-CTNR(1:8)
190624              MOVE  '-RPST'      TO  NM-CTNR(9:5)
190624     END-EVALUATE
190624
190624*    Recupera o tamanho do container de resposta
190624      PERFORM 973100-LER-CONTAINER-NODATA
190624
190624*==> Observação <=================================================
190624*    Os Programas Acionados que possuem Resposta de Tamanho
190624*    Variável não devem validar o Tamanho Recebido
190624*=================================================================
190624
190624*--> Validar o Tamanho Recebido do Container de Resposta
190624*    com o Tamanho Esperado dos Módulos de Tamanho Fixo
190624
190624     EVALUATE  NM-PGM
190624*------> Incluir Módulos com Resposta de Tamanho Fixo
190624         WHEN  MCISI074
190624               MOVE  LENGTH OF MCISI074-DADOS
190624                                 TO  QT-CCT-EPRO
190624               PERFORM 973200-TAMANHO-ESPERADO
190624         WHEN  OTHER
190624               CONTINUE
190624     END-EVALUATE
190624
190624*--> Configura dados para leitura do container de resposta
190624     SET   GET-CONTAINER         TO  TRUE
190624
190624*--> Por causa da captura automática de erro CICS, deve ser
190624*    previsto o erro de container
190624     SET   ERRO-CONTAINER        TO  TRUE
190624
190624*--> Le a RESPOSTA de acordo com o programa que a gravou
190624
190624     EVALUATE NM-PGM
190624
190624*        Calcular a quantidade de caracteres esperado e limpar
190624*        a area de dados antes de executar o GET
190624*        Para matrizes com dados variaveis (clausula depending
190624*        on) nao ha como utilizar o comando INITIALIZE
190624
190624     WHEN MCISI074
190624          INITIALIZE             MCISI074-DADOS
190624          EXEC CICS GET
190624                    CONTAINER (NM-CTNR          )
190624                    INTO      (MCISI074-DADOS   )
190624                    FLENGTH   (QT-CCT-LIDO      )
190624          END-EXEC
190624
190624     WHEN SGTSL037
190624              MOVE LENGTH OF SGTSL037-RPST TO QT-CCT-EPRO
190624              EXEC CICS GET
190624                        CONTAINER (NM-CTNR          )
190624                        INTO      (SGTSL037-RPST    )
190624                        FLENGTH   (QT-CCT-LIDO      )
190624              END-EXEC
190624     WHEN OTHER
190624          SET   ERRO-PROGRAMADO  TO  TRUE
190624          MOVE  9003             TO  MSG1-CD-ERRO-TS
190624          STRING  'Erro na leitura da resposta do módulo '
190624                  NM-PGM '.'
190624                  DELIMITED BY SIZE
190624                  INTO           MSG1-TX-ERRO-TS
190624          PERFORM 999000-GRAVA-ERRO-E-ENCERRA
190624     END-EVALUATE
190624
190624*--> Ao chegar aqui, há certeza de que não houve erro algum
190624     SET   SEM-ERRO              TO  TRUE
190624     .
190624 973000-SAI.
190624     EXIT.
190624*-----------------------------------------------------------------
190624 973100-LER-CONTAINER-NODATA     SECTION.
190624*-----------------------------------------------------------------
190624
190624*--> Configura dados para leitura do container de requisição
190624     SET   GET-CONTAINER         TO  TRUE
190624
190624*--> Por causa da captura automática de erro CICS, deve ser
190624*    previsto o erro de container
190624     SET   ERRO-CONTAINER        TO  TRUE
190624
190624*--> Obtem a quantidade de caracteres no container
190624     MOVE  ZEROS                 TO  QT-CCT-LIDO
190624     EXEC CICS GET
190624               CONTAINER (NM-CTNR    )
190624               NODATA
190624               FLENGTH   (QT-CCT-LIDO)
190624     END-EXEC
190624*--> Ao chegar aqui, há certeza de que não houve erro algum
190624     SET   SEM-ERRO              TO  TRUE
190624     .
190624 973100-SAI.
190624     EXIT.
190624*-----------------------------------------------------------------
190624 973200-TAMANHO-ESPERADO         SECTION.
190624*-----------------------------------------------------------------
190624     MOVE  0014                  TO  MSG1-CD-SEQL-ERRO
190624
190624     EVALUATE NM-PGM
190624
190624         WHEN MCISI074
190624              MOVE  LENGTH OF MCISI074-DADOS
190624                                 TO  QT-CCT-EPRO
190624         WHEN OTHER
190624              SET   ERRO-PROGRAMADO
190624                                 TO  TRUE
190624              MOVE  9003         TO  MSG1-CD-ERRO-TS
190624              STRING  'Erro na leitura da resposta do módulo '
190624                      NM-PGM '.'
190624                      DELIMITED BY SIZE
190624                      INTO       MSG1-TX-ERRO-TS
190624              END-STRING
190624              PERFORM 999000-GRAVA-ERRO-E-ENCERRA
190624     END-EVALUATE
190624
190624     IF  QT-CCT-LIDO             NOT EQUAL QT-CCT-EPRO
190624         SET   ERRO-PROGRAMADO   TO  TRUE
190624         MOVE  9001              TO  MSG1-CD-ERRO-TS
190624         MOVE  QT-CCT-EPRO       TO  VL-EXBR-1
190624         MOVE  QT-CCT-LIDO       TO  VL-EXBR-2
190624         STRING
190624              'Erro no tamanho de área de requisição '
190624              ', TAMANHO ESPERADO: '  VL-EXBR-1
190624              ', TAMANHO RECEBIDO: '  VL-EXBR-2
190624              DELIMITED BY SIZE   INTO  MSG1-TX-ERRO-TS
190624        END-STRING
190624        PERFORM 999000-GRAVA-ERRO-E-ENCERRA
190624     END-IF
190624     .
190624 973200-SAI.
190624     EXIT.
190624*-----------------------------------------------------------------
190624 974000-LER-DADOS-OPRD           SECTION.
190624*-----------------------------------------------------------------
190624     MOVE  0024                  TO  MSG1-CD-SEQL-ERRO
190624
190624*--> Configura dados para leitura do container de requisição
190624     SET   GET-CONTAINER         TO  TRUE
190624     MOVE  'PRM-INF-OPRD'        TO  NM-CTNR
190624
190624*--> Por causa da captura automática de erro CICS, deve ser
190624*    previsto o erro de container
190624     SET   ERRO-CONTAINER        TO  TRUE
190624
190624     INITIALIZE                  PRM-INF-OPRD
190624     EXEC CICS GET
190624               CONTAINER ( NM-CTNR                )
190624               INTO      ( PRM-INF-OPRD           )
190624               FLENGTH   ( LENGTH OF PRM-INF-OPRD )
190624     END-EXEC.
190624
190624*--> Ao chegar aqui, há certeza de que não houve erro algum
190624     SET   SEM-ERRO              TO  TRUE
190624
190624     MOVE KOPRD-CD-OPRD          TO  CD-CHV-USU
190624     .
190624 974000-SAI.
190624     EXIT.
190624*------------------------------------------------------------
190624 980000-CHAMA-SERVICO            SECTION.
190624*------------------------------------------------------------
190624     MOVE 0015                   TO MSG1-CD-SEQL-ERRO
190624
190624*--> Grava o container com dados do serviço/operação a chamar
190624     PERFORM 981000-GRAVAR-CONTAINER-SRVC
190624
190624*--> Grava o container de requisicao do modulo a executar
190624     PERFORM 982000-GRAVAR-CONTAINER-RQSC
190624
190624*--> Limpa a resposta e erro antes de executar o modulo
190624     PERFORM 983000-EXCLUIR-RESPOSTA-ERRO
190624
190624*--> Não há captura automática de erro CICS por usar RESP e RESP2
190624        EXEC CICS LINK
190624             PROGRAM   ( IIBP2000     )
190624             CHANNEL   ( NM-CNL       )
190624             RESP      ( MSG1-RESP1   )
190624             RESP2     ( MSG1-RESP2   )
190624        END-EXEC
190624
190624        EVALUATE TRUE
190624            WHEN MSG1-RESP1      EQUAL DFHRESP(NORMAL)
190624                 CONTINUE
190624            WHEN OTHER
190624                 SET   ERRO-CICS TO  TRUE
190624                 MOVE  9000      TO  MSG1-CD-ERRO-TS
190624                 STRING  'Erro ao acionar serviço: '
190624                         SRVC-ID-SRVC '.' SRVC-VRS-SRVC '.'
190624                         SRVC-ID-OPR  '.' SRVC-VRS-OPR
190624                         'via IIBP2000, ' 'RESP=' MSG1-RESP1
190624                         ', RESP-2=' MSG1-RESP2 '.'
190624                         DELIMITED BY SIZE
190624                         INTO  MSG1-TX-ERRO-TS
190624                 END-STRING
190624
190624                 PERFORM 999000-GRAVA-ERRO-E-ENCERRA
190624      END-EVALUATE
190624
190624*--> Efetua a leitura do Container de Erro
190624     PERFORM 972000-LER-CONTAINER-DE-ERRO
190624
190624*--> Efetua a leitura do Container de Resposta
190624     PERFORM 973000-LER-RESPOSTA
190624     .
190624 980000-SAI.
190624     EXIT.
190624*-----------------------------------------------------------------
190624 981000-GRAVAR-CONTAINER-SRVC    SECTION.
190624*--> Postar container de requisição de serviço.
190624*-----------------------------------------------------------------
190624       MOVE  0016                TO  MSG1-CD-SEQL-ERRO
190624
190624*--> Monta o nome do container de requisicao do servico
190624       MOVE  'PRM-RQSC-SRVC'     TO  NM-CTNR
190624       SET   PUT-CONTAINER       TO  TRUE
190624
190624      EVALUATE NM-PGM
190624
190624          WHEN MCISI074
190624               MOVE  19485       TO  SRVC-ID-SRVC
190624               MOVE  1           TO  SRVC-VRS-SRVC
190624               MOVE  1574513     TO  SRVC-ID-OPR
190624               MOVE  0001        TO  SRVC-VRS-OPR
190624
190624     END-EVALUATE
190624
190624*--> Por causa da captura automática de erro CICS, deve ser
190624*    previsto o erro de container
190624     SET   ERRO-CONTAINER        TO  TRUE
190624
190624     EXEC CICS PUT
190624          CONTAINER ( NM-CTNR                     )
190624          FROM      ( SRVC-PRM-RQSC-SRVC          )
190624          FLENGTH   ( LENGTH OF SRVC-PRM-RQSC-SRVC)
190624     END-EXEC
190624
190624*--> Ao chegar aqui, há certeza de que não houve erro algum
190624     SET   SEM-ERRO              TO  TRUE
190624     .
190624 981000-SAI.
190624      EXIT.
190624*-----------------------------------------------------------------
190624 982000-GRAVAR-CONTAINER-RQSC    SECTION.
190624*    Postar container com dados de requisição (entrada)
190624*    Modulo CONTAINER  -     NOME_PGM-RQSC
190624*    Modulo IIB (FIXO) -     PRM-DADOS-RQSC
190624*-----------------------------------------------------------------
190624      MOVE  0017                 TO  MSG1-CD-SEQL-ERRO
190624
190624*--> Monta o nome dos container de acordo com modulo a executar
190624     MOVE  SPACES                TO  NM-CTNR
190624     SET   PUT-CONTAINER         TO  TRUE
190624
190624*--> Quando módulos via IIB, fixo nome PRM-DADOS-RQSC, senão
190624*    nome_modulo -RQSC
190624
190624     EVALUATE NM-PGM
190624         WHEN MCISI074
190624              MOVE  'PRM-DADOS-RQSC'   TO  NM-CTNR
190624
190624         WHEN OTHER
190624              MOVE  NM-PGM       TO  NM-CTNR(1:8)
190624              MOVE  '-RQSC'      TO  NM-CTNR(9:5)
190624     END-EVALUATE
190624
190624*--> Por causa da captura automática de erro CICS, deve ser
190624*    previsto o erro de container
190624     SET   ERRO-CONTAINER        TO  TRUE
190624
190624*--> Faz PUT da requisicao do programa a executar
190624      EVALUATE NM-PGM
190624          WHEN MCISI074
190624               EXEC CICS PUT
190624                         CONTAINER (NM-CTNR                 )
190624                         FROM      (MCISI074-DADOS          )
190624                         FLENGTH   (LENGTH OF MCISI074-DADOS)
190624               END-EXEC
190624
190624         WHEN SGTSL037
190624              EXEC CICS PUT
190624                        CONTAINER (NM-CTNR)
190624                        FROM      (SGTSL037-RQSC )
190624                        FLENGTH   (LENGTH OF SGTSL037-RQSC)
190624              END-EXEC
190624
190624          WHEN OTHER
190624               SET   ERRO-PROGRAMADO
190624                                 TO  TRUE
190624               MOVE  9004        TO  MSG1-CD-ERRO-TS
190624               STRING  'Erro na gravação da requisição do módulo '
190624                        NM-PGM '.'
190624                        DELIMITED BY SIZE
190624                        INTO     MSG1-TX-ERRO-TS
190624               END-STRING
190624               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
190624     END-EVALUATE
190624
190624*--> Ao chegar aqui, há certeza de que não houve erro algum
190624      SET   SEM-ERRO             TO  TRUE
190624      .
190624 982000-SAI.
190624     EXIT.
190624*-----------------------------------------------------------------
190624 983000-EXCLUIR-RESPOSTA-ERRO    SECTION.
190624* Exclui o container de resposta e de erro para evitar problemas
190624* em modulos executados mais de uma vez dentro da mesma sessao do
190624* CICS. Equivale ao initialize na area de resposta dos programas
190624* que utilizam COMMAREA.
190624*-----------------------------------------------------------------
190624     MOVE  0018                  TO  MSG1-CD-SEQL-ERRO
190624
190624*--> Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
190624*    nome_modulo -RPST
190624
190624     EVALUATE NM-PGM
190624
190624         WHEN MCISI074
190624              MOVE  'PRM-DADOS-RPST'
190624                                 TO  NM-CTNR
190624         WHEN OTHER
190624              MOVE  NM-PGM       TO  NM-CTNR(1:8)
190624              MOVE  '-RPST'      TO  NM-CTNR(9:5)
190624     END-EVALUATE
190624
190624     SET   DEL-CONTAINER         TO  TRUE
190624
190624*--> Exclui o container de resposta
190624*    Não há captura automática de erro CICS por usar RESP e RESP2
190624     EXEC CICS DELETE
190624          CONTAINER (NM-CTNR   )
190624          RESP      (MSG1-RESP1)
190624          RESP2     (MSG1-RESP2)
190624     END-EXEC
190624
190624     EVALUATE TRUE
190624         WHEN MSG1-RESP1         EQUAL DFHRESP(CONTAINERERR)
190624         WHEN MSG1-RESP1         EQUAL DFHRESP(NORMAL)
190624              CONTINUE
190624         WHEN OTHER
190624              SET   ERRO-CONTAINER
190624                                 TO  TRUE
190624              PERFORM 999002-TRATA-CONDITION-CICS
190624     END-EVALUATE
190624
190624*--> Excluir o container de erro
190624      PERFORM 983100-EXCLUIR-CONTAINER-ERRO
190624      .
190624 983000-SAI.
190624      EXIT.
190624*-----------------------------------------------------------------
190624 983100-EXCLUIR-CONTAINER-ERRO   SECTION.
190624*-----------------------------------------------------------------
190624
190624*--> Limpar variaveis container de erro
190624     INITIALIZE                      IIBKERRO-PRM-ERRO
190624
190624*--> Exclui o container de erro
190624     MOVE  'PRM-ERRO'            TO  NM-CTNR
190624     SET   DEL-CONTAINER         TO  TRUE
190624
190624*--> Não há captura automática de erro CICS por usar RESP e RESP2
190624     EXEC CICS DELETE
190624          CONTAINER (NM-CTNR   )
190624          RESP      (MSG1-RESP1)
190624          RESP2     (MSG1-RESP2)
190624     END-EXEC
190624
190624     EVALUATE TRUE
190624         WHEN MSG1-RESP1         EQUAL DFHRESP(CONTAINERERR)
190624         WHEN MSG1-RESP1         EQUAL DFHRESP(NORMAL)
190624              CONTINUE
190624         WHEN OTHER
190624              SET   ERRO-CONTAINER
190624                                 TO  TRUE
190624              PERFORM 999002-TRATA-CONDITION-CICS
190624     END-EVALUATE
190624     .
190624 983100-SAI.
190624     EXIT.
190624*-----------------------------------------------------------------
190624 984000-GRAVAR-CONTAINER-ERRO    SECTION.
190624*--> Grava o erro gerado no container de erro
190624*-----------------------------------------------------------------
190624
190624*--> Não há captura automática de erro CICS por usar RESP e RESP2
190624     EXEC CICS PUT
190624          CONTAINER ( 'PRM-ERRO'                  )
190624          FROM      ( IIBKERRO-PRM-ERRO           )
190624          FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
190624          RESP      ( MSG1-RESP1                  )
190624          RESP2     ( MSG1-RESP2                  )
190624     END-EXEC
190624     .
190624 984000-SAI.
190624     EXIT.
190624
190624*-----------------------------------------------------------------
190624 999000-GRAVA-ERRO-E-ENCERRA     SECTION.
190624*-----------------------------------------------------------------
190624
190624     IF  ERRO-PROGRAMA-ACIONADO
190624         PERFORM 999100-MONITORAR-VRV-PGM-CHMD
190624     END-IF
190624
190624     PERFORM 999200-MONITORAR-VRV-PGM-E-ENC
190624     .
190624 999000-SAI.
190624     EXIT.
190624*-----------------------------------------------------------------
190624 999100-MONITORAR-VRV-PGM-CHMD   SECTION.
190624*-----------------------------------------------------------------
190624*--> Monitora as variáveis dos módulos chamados
190624
190624     EVALUATE  NM-PGM-CHMD
190624
190624         WHEN SGTSL037
190624              MOVE 'SGTSL037-CD-TIP-SRVC'  TO MSGKW999-NM-VRV-MNT
190624              MOVE SGTSL037-CD-TIP-SRVC    TO MSGKW999-VL-VRV-MNT
190624              PERFORM 999003-ADICIONA-VRV-MNT
190624
190624              MOVE 'SGTSL037-E-CD-TIP-ATB-CTRA-01'
190624                                           TO MSGKW999-NM-VRV-MNT
190624              MOVE SGTSL037-E-CD-TIP-ATB-CTRA-01
190624                                           TO MSGKW999-VL-VRV-MNT
190624              PERFORM 999003-ADICIONA-VRV-MNT
190624
190624         WHEN  OTHER
190624              CONTINUE
190624     END-EVALUATE
190624     .
190624 999100-SAI.
190624     EXIT.
190624*-----------------------------------------------------------------
190624 999200-MONITORAR-VRV-PGM-E-ENC  SECTION.
190624*-----------------------------------------------------------------
190624*--> Monitorar variàveis do pgm, grava e encerra processamento
190624
190624     IF  MSG1-CD-ERRO-TS         EQUAL  ZEROS
190624        MOVE  8888               TO  MSG1-CD-ERRO-TS
190624     END-IF
190624
190624     IF  MSG1-TX-ERRO-TS         EQUAL  SPACES
190624         MOVE  'Erro não especificado no módulo.'
190624                                 TO  MSG1-TX-ERRO-TS
190624     END-IF
190624
190624*--> Em caso de erro, desfaz as atualizaçoes em banco de dados
190624     EXEC CICS SYNCPOINT
190624               ROLLBACK
190624     END-EXEC
190624*--> VERIFICA SE O TIPO DE ERRO GERADO POSSUI TRATAMENTO INTERNO
190624
190624     EVALUATE TRUE
190624         WHEN ERRO-CONTAINER
190624              PERFORM 999004-TRATA-ERRO-CTNR
190624         WHEN ERRO-TAMANHO-AREA
190624              PERFORM 999005-TRATA-ERRO-AREA-DADOS
190624         WHEN ERRO-CANAL
190624              PERFORM 999006-TRATA-ERRO-CANAL
190624         WHEN OTHER
190624              CONTINUE
190624     END-EVALUATE
190624
190624*--> Adiciona as variáveis para monitoração
190624     IF  MNT-VRV
190624         MOVE 'SGTS003X-DT-IMP-PSC-SDO'    TO MSGKW999-NM-VRV-MNT
190624         MOVE SGTS003X-DT-IMP-PSC-SDO      TO MSGKW999-VL-VRV-MNT
190624         PERFORM 999003-ADICIONA-VRV-MNT
190624
190624         MOVE 'SGTS003X-HR-IMP-PSC-SDO'    TO MSGKW999-NM-VRV-MNT
190624         MOVE SGTS003X-HR-IMP-PSC-SDO      TO MSGKW999-VL-VRV-MNT
190624         PERFORM 999003-ADICIONA-VRV-MNT
190624
190624         MOVE  'SGTS003X-ULT-CD-CTRA-TIT-VL'
190624                                           TO  MSGKW999-NM-VRV-MNT
190624         MOVE  SGTS003X-ULT-CD-CTRA-TIT-VL TO    MCR-S9-15
190624         MOVE  MCR-S9-15                   TO MSGKW999-VL-VRV-MNT
190624         PERFORM  999003-ADICIONA-VRV-MNT
190624
190624         MOVE  'SGTS003X-ULT-CD-TIT-OPR-ETRD '
190624                                           TO MSGKW999-NM-VRV-MNT
190624         MOVE  SGTS003X-ULT-CD-TIT-OPR-ETRD
190624                                           TO MSGKW999-VL-VRV-MNT
190624         PERFORM  999003-ADICIONA-VRV-MNT
190624
190624         MOVE 'SBDATA-RETURN-CODE'         TO MSGKW999-NM-VRV-MNT
190624         MOVE  MSG1-CD-RTN-PGM-ACND        TO MSGKW999-VL-VRV-MNT
190624         PERFORM  999003-ADICIONA-VRV-MNT
190624
190624         MOVE  'NR-DVS-TIT'                TO MSGKW999-NM-VRV-MNT
190624         MOVE  NR-DVS-TIT                  TO MCR-S9-15
190624         MOVE  MCR-S9-15                   TO MSGKW999-VL-VRV-MNT
190624         PERFORM  999003-ADICIONA-VRV-MNT
190624
190624         MOVE  'CD-CTRA-TIT-VL'            TO MSGKW999-NM-VRV-MNT
190624         MOVE  CD-CTRA-TIT-VL              TO MCR-S9-15
190624         MOVE  MCR-S9-15                   TO MSGKW999-VL-VRV-MNT
190624         PERFORM  999003-ADICIONA-VRV-MNT
190624
190624         MOVE  'CD-TIT-OPR-ETRD'           TO MSGKW999-NM-VRV-MNT
190624         MOVE  CD-TIT-OPR-ETRD             TO MSGKW999-VL-VRV-MNT
190624         PERFORM  999003-ADICIONA-VRV-MNT
190624
190624     END-IF
190624
190624* --> grava o erro no msg
190624     PERFORM MSG1-NTF-MSG
190624     MOVE MSG1-CD-ERRO-TS                  TO IIBKERRO-CD-ERRO
190624     MOVE MSG1-TX-ERRO-TS                  TO IIBKERRO-TX-ERRO
190624
190624     EXEC CICS PUT
190624        CONTAINER ( 'PRM-ERRO' )
190624        CHANNEL   ( NM-CNL      )
190624        FROM      ( IIBKERRO-PRM-ERRO)
190624        FLENGTH ( LENGTH OF IIBKERRO-PRM-ERRO )
190624        NOHANDLE
190624     END-EXEC
190624
190624     PERFORM 999999-ENCERRAR
190624     .
190624 999200-SAI.
190624     EXIT.
190624* --------------------------------------*
190624* 999001-TRATA-ABEND-CICS        SECTION.
190624* 999002-TRATA-CONDITION-CICS    SECTION.
190624* 999003-ADICIONA-VRV-MNT        SECTION.
190624* --------------------------------------*
-INC HLPKP999
190624*---------------------------------------*
190624* 999004-TRATA-ERRO-CTNR         SECTION.
190624* 999005-TRATA-ERRO-AREA-DADOS   SECTION.
190624* 999006-TRATA-ERRO-CANAL        SECTION.
190624*---------------------------------------*
-INC SGTKP999
190624* --------------------------------------*
190624* 999010-TRATA-ERRO-DB2          SECTION.
-INC HLPKP997
190624
190624* --------------------------------------*
190624* MSG1-NTF-MSG                   SECTION.
-INC MSGKPMG1
190624* --------------------------------------*
190624 999999-ENCERRAR                 SECTION.
190624* --------------------------------------*
190624     EXEC CICS
190624          RETURN
190624     END-EXEC
190624     .
190624 999999-SAI.
190624     EXIT.