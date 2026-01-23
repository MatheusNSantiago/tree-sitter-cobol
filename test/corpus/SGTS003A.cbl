       CBL ARITH(EXTEND)
      *-----------------------------------------------------------------
      * PROGRAMA..: SGTS003A
      * ANALISTA..: EDILENE AKEMI HORITA.
      * AUTOR.....: EDILENE AKEMI HORITA.
      * OBJETIVO..: LISTAR OS DADOS DAS OPERAÇÕES QUE FORAM IMPORTADAS
      *             EM UMA DETERMINADA DATA/HORA
      * COMPILACAO: COBOL/CICS
      *-----------------------------------------------------------------
      * VRS003 09.04.2019 - F2634012 - Inclusão da dt_psc na seleção da
      *                     tabela de saldo cf. email da
      *                     DITEC/UOS/GEINT - E32 - Qualidade de Código
      * VRS002 02.04.2019 - F2634012 - Carregar na tabela somente as
      *                     operações de responsabildade da dimec
      * VRS001 27.08.2018 - F2634012 - DE 2016/145195 - IMPLANTACAO
      *-----------------------------------------------------------------
      *
      * FALAR COM A ANITA PARA FAZER O FILTRO NA BAIXA.
      * CONVERSEI COM A CINTIA EM 04/09.
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTS003A.

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
       01  CTE-PROG     PIC  X(17) VALUE '*** SGTS003A ***'.
       01  CTE-VERS     PIC  X(06) VALUE 'VRS003'.

      * Módulos acionados
       77  SBDATAF                         PIC  X(08) VALUE 'SBDATAF'.
       77  MCISI074                        PIC  X(08) VALUE 'MCISI074'.
       77  IIBP2000                        PIC  X(08) VALUE 'IIBP2000'.
       77  SGTSL037                        PIC  X(08) VALUE 'SGTSL037'.
      *-----------------------------------------------------------------
      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informacoes
-INC HLPKE01B
       LOCAL-STORAGE SECTION.
      *-----------------------------------------------------------------
      *-----------------------------------------------------------------
      *--> Book dos dados do usuário
-INC IIBKOPRD
      * book de para acionamento do msg1
-INC IIBKERRO
      *--> Book do container de Serviços (para acionamento de serviços)
-INC IIBKSRVC
-INC MSGKWMG1
-INC HLPKW999
      *--> Variaveis para tratamento de CONTAINER
-INC SGTKERR1
      *-----> BOOK DA ROTINA MCISI074 - CONSULTAR DADOS CLIENTE
       01  MCISI074-DADOS.
-INC MCIKI074
      *--> Book da Subrotina SGTSL037
-INC SGTKL037
      * ----> book do programa
       01  SGTS003A-DADOS.
-INC SGTK003A

      *--> Indicadores de nulidades
       01  NR-UNCO-CTR-OPR-N          PIC S9(4)  COMP-5 VALUE 0.

      * Area de trabalho

       01 QT-REG-SELD                  PIC S9(04) COMP-5 VALUE 0.
       01  IC-MCI                      PIC S9(04) COMP-5 VALUE 0.
       01  CD-CHV-USU                  PIC  X(08)        VALUE ' '.

      *--> Guarda da carteira para verificar se responsabilidade
      *    DIMEC
       01  WS-CD-CTRA-ANT              PIC S9(09) COMP-5 VALUE 0.
       01  WS-TX-VCLD-CRCT             PIC X(20)         VALUE ' '.
       01  IC-CTRA                     PIC S9(04) COMP-5 VALUE 0.

      * Variavel(is) de controle do cursor(es)

       01  CTL-CURSOR-PSQ.
           03  CD-FIM-CURSOR-PSQ            PIC S9(04) COMP-5 VALUE 0.
               88  FIM-CURSOR-PSQ            VALUE +100.
           03  IN-CURSOR-PSQ-ABTO           PIC  X(01) VALUE 'N'.
               88  CURSOR-PSQ-ABTO          VALUE 'S'.

       01  REG-WS.
           03 CD-CTRA-TIT-VL           PIC S9(09) COMP-5 VALUE ZEROS.
           03 CD-TIT-OPR-ETRD          PIC  X(12)        VALUE ' '.
           03 CD-OPR-ETRD-MCP          PIC S9(11) COMP-3 VALUE ZEROS.
           03 NR-DVS-TIT               PIC S9(04) COMP-5 VALUE ZEROS.
           03 NR-DVS-TIT-N             PIC S9(04) COMP-5 VALUE ZEROS.
           03 NR-VRS-OPR-ETRD          PIC S9(04) COMP-5 VALUE ZEROS.
           03 DT-PSC                   PIC  X(10)        VALUE ' '.
           03 VL-SDO-TIT-DT-PSC        PIC S9(15)V99 COMP-3 VALUE ZEROS.
           03 CD-EST-RSTD-ITE          PIC S9(04) COMP-5 VALUE ZEROS.
           03 CD-CLI                   PIC S9(09) COMP-5 VALUE ZEROS.
           03 NR-SEQL-PSC-DRIA         PIC S9(04) COMP-5 VALUE ZEROS.
           03 NR-UNCO-CTR-OPR          PIC S9(17) COMP-3 VALUE ZEROS.

       01  IN-FIM-PSQ-1                PIC  X(01) VALUE ' '.
           88  FIM-LISTA                          VALUE 'S'.

      *--> Areas de Controle de Programa Chamado (CONTAINER)
       01  CTL-PGM-CHMD.
           03  NM-PGM-CHMD             PIC  X(08)         VALUE ' '.
           03  FILLER                  PIC  X(01)         VALUE ' '.
           03  MSG-PGM-CHMD            PIC  X(111)        VALUE ' '.

       01  WS-ERRO-CICS.
           03  WS-RESP1                PIC  9(008).
           03  WS-RESP2                PIC  9(008).

      *--> Máscaras edição de variáveis p/ monitoração mensagens de erro
       01  MCR-S9-15                   PIC      ---------------9.
       01  MCR-S9-29          PIC  ---.---.---.---.--9,99999999999999.
       01  VL-15V2                     PIC  ---.---.---.---.--9,99.
      *-----------------------------------------------------------------

       01  AUX-DATA                         PIC X(10)
                                            VALUE '0000-00-00'.
       01  AUX-DATA-R  REDEFINES AUX-DATA.
           03  ANO                          PIC 9(04).
           03  FILLER                       PIC X(01).
           03  MES                          PIC 9(02).
           03  FILLER                       PIC X(01).
           03  DIA                          PIC 9(02).

       01  AUX-DT                           PIC X(10)
                                            VALUE '00.00.0000'.
       01  AUX-DT-R  REDEFINES AUX-DT.
           03  DIA                          PIC 9(02).
           03  FILLER                       PIC X(01).
           03  MES                          PIC 9(02).
           03  FILLER                       PIC X(01).
           03  ANO                          PIC 9(04).

       01  AUX-HORARIO                      PIC X(08)
                                            VALUE '00:00:00'.
       01  AUX-HORARIO-R  REDEFINES AUX-HORARIO.
           03  HORA                         PIC 9(02).
           03  FILLER                       PIC X(01).
           03  MINUTO                       PIC 9(02).
           03  FILLER                       PIC X(01).
           03  SEGUNDO                      PIC 9(02).

       01 AUX-ERRO                     PIC X(30).
       01 AUX-ERRO-R  REDEFINES AUX-ERRO.
          03 AUX-CD-ERRO               PIC S9(04) COMP.

       77  EMISSOR                     PIC S9(04) COMP-5 VALUE 1.

      *--> Carteira será fixada enquanto não houver a migração completa
      *    da base de dados
       01  CARTEIRA-TVM                PIC S9(09) COMP-5  VALUE 500000.

      * Area da subrotina SBDATA
       01  FUNCAO                      PIC  X(003) VALUE 'F01'.
       01  AREA-WORK                   PIC  X(4095).
       01  AREA-WORK-RED  REDEFINES  AREA-WORK.
           03 A-TAM                    PIC  9(0008) COMP.
           03 FILLER                   PIC  X(4071).
           03 A-FIM                    PIC  X(0020).
       01  ARG01                       PIC  9(008) VALUE 0.
       01  ARG01-R REDEFINES ARG01.
           03  DIA                     PIC  9(002).
           03  MES                     PIC  9(002).
           03  ANO                     PIC  9(004).
       01  ARG02                       PIC  9(001) VALUE 0.

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.


           EXEC SQL
             DECLARE CURSOR-PSQ CURSOR FOR
             SELECT
                 A.CD_CTRA_TIT_VL,
                 A.CD_TIT_OPR_ETRD,
                 A.NR_DVS_TIT,
                 A.DT_PSC,
                 A.VL_SDO_TIT_DT_PSC,
                 A.CD_EST_RSTD_ITE,
                 A.NR_SEQL_PSC_DRIA
              FROM
                 DB2SGT.PSC_DRIA_SDO_OPR A
              WHERE
                    A.DT_IMP_PSC_SDO = :AUX-DATA
                AND A.HR_IMP_PSC_SDO = :AUX-HORARIO
                AND A.DT_PSC         < CURRENT DATE
      *---- paginação
                AND (A.CD_CTRA_TIT_VL > :SGTS003A-ULT-CD-CTRA-TIT-VL
                OR  (A.CD_CTRA_TIT_VL = :SGTS003A-ULT-CD-CTRA-TIT-VL
                AND  A.CD_TIT_OPR_ETRD  >
                                        :SGTS003A-ULT-CD-TIT-OPR-ETRD))
              ORDER BY
                     A.CD_CTRA_TIT_VL,
                     A.CD_TIT_OPR_ETRD
           END-EXEC.

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------

      * Habilita tratamento automatico de erros DB2
-INC HLPKE997
      * Habilita captura automatica de abend e erros CICS
-INC HLPKE999

           MOVE 4095                   TO A-TAM
           MOVE 'SBDATAF -WRKAREA-FIM' TO A-FIM

           PERFORM 100000-DESCOBRIR-CANAL
           MOVE 'SGTS003A' TO NM-PGM
           PERFORM 974000-LER-DADOS-OPRD
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO
           PERFORM 130000-RECEBER-REQUISICAO
           PERFORM 140000-VALIDAR-PARAMETRO
           PERFORM 970101-ABRE-CURSOR-PSQ
           PERFORM 200000-PROCESSA-PESQUISA
                    VARYING SGTS003A-IC-TAB FROM 1 BY 1
                    UNTIL FIM-LISTA
           PERFORM 970104-FECHA-CURSOR-PSQ

           PERFORM 240000-OBTER-NOME-CLIENTE

           PERFORM 300000-POSTA-RESPOSTA
           PERFORM 999999-ENCERRAR
           GOBACK
           .
      *-----------------------------------------------------------------
       100000-DESCOBRIR-CANAL SECTION.
      *-----------------------------------------------------------------
           MOVE 0001                   TO MSG1-CD-SEQL-ERRO

      *--> Por causa da captura automática de erro CICS, deve ser
      *--> previsto      o erro de container
           SET  ERRO-CANAL             TO TRUE

           EXEC CICS ASSIGN
              CHANNEL ( NM-CNL )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE

           .
       100000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       130000-RECEBER-REQUISICAO SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                  TO MSG1-CD-SEQL-ERRO

      *--> Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *--> previsto o erro de container
           SET  ERRO-CONTAINER         TO TRUE

           MOVE ZEROS                  TO QT-CCT-LIDO
           MOVE LENGTH OF SGTS003A-RQSC      TO QT-CCT-EPRO
           EXEC CICS GET
              CONTAINER ( 'SGTS003A-RQSC' )
              CHANNEL   ( NM-CNL          )
              NODATA
              FLENGTH ( QT-CCT-LIDO )
           END-EXEC

           IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
               SET ERRO-TAMANHO-AREA   TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           EXEC CICS GET
              CONTAINER ( 'SGTS003A-RQSC' )
              CHANNEL   ( NM-CNL      )
              FLENGTH ( LENGTH OF SGTS003A-RQSC )
              INTO    ( SGTS003A-RQSC )
           END-EXEC

      * --> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE

           .
       130000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       140000-VALIDAR-PARAMETRO SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                  TO MSG1-CD-SEQL-ERRO

           IF  SGTS003A-DT-IMP-PSC-SDO EQUAL SPACES
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0738               TO MSG1-CD-ERRO-TS
               MOVE 'Data importação posição saldo inválida.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *    Verifica se a data é válida
           MOVE CORR SGTS003A-DT-IMP-PSC-SDO-R
                                      TO ARG01-R
           PERFORM 940000-VALIDA-DATA

      *--> Return code = 24 - Data Inválida.
      *                  22 - Conteudo alfa em argumento numerico
           IF  RETURN-CODE EQUAL 24 OR 22
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0738               TO MSG1-CD-ERRO-TS
               MOVE 'Data importação posição saldo inválida.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS003A-HR-IMP-PSC-SDO  EQUAL SPACES
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0737               TO MSG1-CD-ERRO-TS
               MOVE 'Horário importação posição saldo inválido'
                                            TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           MOVE  CORR SGTS003A-DT-IMP-PSC-SDO-R
                                       TO AUX-DATA-R
           MOVE  CORR SGTS003A-HR-IMP-PSC-SDO-R
                                       TO AUX-HORARIO-R

           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-PROCESSA-PESQUISA SECTION.
      *-----------------------------------------------------------------
           MOVE 0004                  TO MSG1-CD-SEQL-ERRO

           PERFORM 970102-LER-CURSOR-PSQ

           IF  NOT FIM-CURSOR-PSQ
               IF  CD-CTRA-TIT-VL      NOT EQUAL  WS-CD-CTRA-ANT
                   IF  WS-CD-CTRA-ANT  EQUAL  ZEROS
                       PERFORM 210000-PSQ-CARACT-CARTEIRA
                   END-IF
                   PERFORM VARYING IC-CTRA  FROM 1 BY 1
                           UNTIL IC-CTRA > SGTSL037-QT-OCR-TAB
                       MOVE CD-CTRA-TIT-VL
                                       TO  WS-CD-CTRA-ANT
                       IF  CD-CTRA-TIT-VL  EQUAL
                                       SGTSL037-CD-CTRA-TVM(IC-CTRA)
                           MOVE SGTSL037-TX-VCLD-CRCT(IC-CTRA)
                                       TO  WS-TX-VCLD-CRCT
                           MOVE 999    TO  IC-CTRA
                       ELSE
                           MOVE 'N'    TO  WS-TX-VCLD-CRCT
                       END-IF
                   END-PERFORM
               END-IF

               IF  WS-TX-VCLD-CRCT     EQUAL  'S'
                   PERFORM 220000-OBTER-OPERACAO
                   PERFORM 230000-OBTER-CD-CLIENTE
               END-IF
           END-IF

           IF  FIM-CURSOR-PSQ  OR  SGTS003A-QT-OCR-TAB EQUAL 100
               IF FIM-CURSOR-PSQ
                  MOVE 'N'    TO  SGTS003A-IN-CNTO-PSQ
               ELSE
                  MOVE 'S'    TO  SGTS003A-IN-CNTO-PSQ
               END-IF
               SET FIM-LISTA  TO  TRUE
           END-IF

           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       210000-PSQ-CARACT-CARTEIRA      SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

           MOVE SGTSL037               TO NM-PGM

           SET SGTSL037-LISTA-POR-TIP-ATB
                                       TO TRUE

           MOVE CD-CTRA-TIT-VL         TO SGTSL037-E-CD-CTRA-TVM

           MOVE 8                      TO
                                       SGTSL037-E-CD-TIP-ATB-CTRA-01

           PERFORM 900000-EXECUTAR-MODULO
           .
       210000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       220000-OBTER-OPERACAO   SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                  TO MSG1-CD-SEQL-ERRO

           EXEC SQL
              SELECT A.CD_OPR_ETRD_MCP,
                     A.NR_VRS_OPR_ETRD,
                     B.NR_UNCO_CTR_OPR

                 INTO :CD-OPR-ETRD-MCP
                      ,:NR-VRS-OPR-ETRD
                      ,:NR-UNCO-CTR-OPR
                       :NR-UNCO-CTR-OPR-N

                 FROM DB2SGT.EST_VRS_OPR_ETRD A
                      INNER JOIN
                       DB2SGT.OPR_ETRD_MCP B
                       ON
                           A.CD_CTRA_TIT_VL  = B.CD_CTRA_TIT_VL
                       AND A.CD_OPR_ETRD_MCP = B.CD_OPR_ETRD_MCP
                       AND A.NR_VRS_OPR_ETRD = B.NR_VRS_OPR_ETRD

                  WHERE
                       DATE(A.TS_EST_VRS)    <= :AUX-DATA
                       AND A.CD_TIP_EST_VRS  = 3
                       AND A.CD_CTRA_TIT_VL  = :CD-CTRA-TIT-VL
                       AND B.CD_TIT_OPR_ETRD = :CD-TIT-OPR-ETRD
                       AND B.NR_DVS_TIT      = :NR-DVS-TIT

                       ORDER BY
                        A.TS_EST_VRS DESC,
                        B.NR_VRS_OPR_ETRD DESC
                        FETCH FIRST 1 ROW ONLY
              END-EXEC

           EVALUATE SQLCODE
              WHEN ZEROS
                  CONTINUE
              WHEN 100
                  SET   ERRO-PROGRAMADO   TO  TRUE
                  MOVE  0353              TO  MSG1-CD-ERRO-TS
                  MOVE 'Título não vinculado a operação estruturada.'
                                          TO  MSG1-TX-ERRO-TS
                  SET MNT-VRV             TO TRUE
                  PERFORM 999000-GRAVA-ERRO-E-ENCERRA
             WHEN OTHER
                  SET   ERRO-PROGRAMADO   TO  TRUE
                  MOVE  9997              TO  MSG1-CD-ERRO-TS
                  MOVE 'Erro de acesso ao banco de dados do sistema.'
                                          TO  MSG1-TX-ERRO-TS
                  SET MNT-VRV             TO TRUE
                  PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

           ADD  1                      TO  SGTS003A-QT-OCR-TAB

           MOVE CD-CTRA-TIT-VL         TO
                     SGTS003A-CD-CTRA-TIT-VL(SGTS003A-QT-OCR-TAB)
           MOVE CD-TIT-OPR-ETRD        TO
                    SGTS003A-CD-TIT-OPR-ETRD(SGTS003A-QT-OCR-TAB)
           MOVE NR-DVS-TIT             TO
                     SGTS003A-NR-DVS-TIT (SGTS003A-QT-OCR-TAB)
           MOVE VL-SDO-TIT-DT-PSC      TO
                SGTS003A-VL-SDO-TIT-DT-PSC(SGTS003A-QT-OCR-TAB)
           MOVE DT-PSC                 TO  AUX-DT
           MOVE CORR AUX-DT-R          TO   ARG01-R
           MOVE ARG01-R                TO
                SGTS003A-DT-PSC(SGTS003A-QT-OCR-TAB)
           MOVE CD-EST-RSTD-ITE        TO
                SGTS003A-CD-EST-RSTD-ITE(SGTS003A-QT-OCR-TAB)
           MOVE NR-SEQL-PSC-DRIA       TO
                SGTS003A-NR-SEQL-PSC-DRIA(SGTS003A-QT-OCR-TAB)
           MOVE CD-OPR-ETRD-MCP        TO
                     SGTS003A-CD-OPR-ETRD-MCP(SGTS003A-QT-OCR-TAB)
           MOVE NR-VRS-OPR-ETRD        TO
                     SGTS003A-NR-VRS-OPR-ETRD (SGTS003A-QT-OCR-TAB)
           MOVE NR-UNCO-CTR-OPR        TO
                SGTS003A-NR-UNCO-CTR-OPR(SGTS003A-QT-OCR-TAB)

           .
       220000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       230000-OBTER-CD-CLIENTE   SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                  TO MSG1-CD-SEQL-ERRO

           EXEC SQL
              SELECT C.CD_CLI
                 INTO :CD-CLI
              FROM DB2SGT.CLI_OPR_ETRD C
              WHERE  C.CD_CTRA_TIT_VL  = :CD-CTRA-TIT-VL
                 AND C.CD_OPR_ETRD_MCP = :CD-OPR-ETRD-MCP
                 AND C.NR_VRS_OPR_ETRD = :NR-VRS-OPR-ETRD
                 AND C.CD_TIP_FUC_CLI  = :EMISSOR
              FETCH FIRST 1 ROW ONLY
           END-EXEC

           MOVE CD-CLI              TO
                                    SGTS003A-CD-CLI(SGTS003A-QT-OCR-TAB)
           .
       230000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       240000-OBTER-NOME-CLIENTE   SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                  TO MSG1-CD-SEQL-ERRO

           MOVE MCISI074               TO  NM-PGM

           INITIALIZE MCISI074-DADOS REPLACING  ALPHANUMERIC BY SPACES
                                                NUMERIC      BY ZEROS

           MOVE CD-CHV-USU             TO  MCISI074-CD-CHV-USU
           MOVE SGTS003A-QT-OCR-TAB    TO  MCISI074-QT-REG-ENTD
           PERFORM VARYING IC-MCI FROM 1 BY 1
                   UNTIL IC-MCI > SGTS003A-QT-OCR-TAB

               MOVE SGTS003A-CD-CLI(IC-MCI)
                                    TO  MCISI074-CD-CLI-PF-ENTD(IC-MCI)
           END-PERFORM

           PERFORM 910000-CHAMAR-SERVICO

           IF  MCISI074-CD-RTN         NOT  EQUAL  ZEROES
               SET ERRO-PROGRAMA-ACIONADO  TO TRUE
               MOVE NM-PGM                 TO MSG1-NM-PGM-ACND
               MOVE MCISI074-NR-SEQL-ERRO  TO MSG1-CD-SEQL-PGM-ACND
               MOVE MCISI074-TX-MSG-RTN    TO MSG1-TX-RTN-PGM-ACND
               MOVE MCISI074-CD-RTN        TO MSG1-CD-RTN-PGM-ACND

               MOVE 3004                   TO MSG1-CD-ERRO-TS
               STRING  'Erro ao acionar o módulo ' NM-PGM '.'
                       DELIMITED BY SIZE
                       INTO                MSG1-TX-ERRO-TS
      *         Monitorar variáveis do módulo chamado
                PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

            PERFORM VARYING SGTS003A-IC-TAB FROM 1 BY 1
                   UNTIL SGTS003A-IC-TAB > MCISI074-QT-REG-SAID

               MOVE MCISI074-NM-CLI-PF-SAID(SGTS003A-IC-TAB)  TO
                                       SGTS003A-NM-CLI(SGTS003A-IC-TAB)
           END-PERFORM

           .
       240000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-POSTA-RESPOSTA SECTION.
      *-----------------------------------------------------------------
           MOVE 0009                 TO MSG1-CD-SEQL-ERRO

           SET PUT-CONTAINER           TO TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *--> previsto o erro de container
           SET  ERRO-CONTAINER         TO TRUE

           EXEC CICS PUT
              CONTAINER ( 'SGTS003A-RPST' )
              CHANNEL   ( NM-CNL      )
              FLENGTH ( LENGTH OF SGTS003A-RPST )
              FROM    ( SGTS003A-RPST )
           END-EXEC

           SET SEM-ERRO TO  TRUE
           .
       300000-SAI.
           EXIT.

      *----------------------------------------------------------------*
      *                                                                *
      *  SECTION DA FAIXA DE 900000 A 969900 PARA DEMAIS ROTINAS       *
      *                                                                *
      *----------------------------------------------------------------*
      * Executa um módulo implementado com CONTAINER                   *
      * Não incluir nessa section os módulos chamados via COMMAREA     *
      *                                                                *
      *----------------------------------------------------------------*
       900000-EXECUTAR-MODULO          SECTION.
      *----------------------------------------------------------------*
      *    Grava o container de requisição do módulo a executar
           PERFORM 982000-GRAVAR-CONTAINER-RQSC

      *    Limpa a resposta e erro antes de executar o módulo
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO

      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EVALUATE NM-PGM

               WHEN SGTSL037
                   EXEC CICS LINK
                             PROGRAM ( SGTSL037  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC

               WHEN OTHER
                   SET   ERRO-PROGRAMADO TO  TRUE
                    MOVE  9006           TO  MSG1-CD-ERRO-TS
                    STRING  'Não definida a execução do módulo'
                            NM-PGM '.'
                            DELIMITED BY SIZE
                            INTO            MSG1-TX-ERRO-TS
                   PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE
      *
           EVALUATE TRUE
               WHEN MSG1-RESP1        EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    SET   ERRO-CICS    TO  TRUE
                    MOVE  3004         TO  MSG1-CD-ERRO-TS
                    STRING
                    'Erro  ao acionar o módulo  ' NM-PGM  ', '
                    'RESP  = '  MSG1-RESP1  ', '
                    'RESP2 = '  MSG1-RESP2
                    DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *    Efetua a leitura do Container de Erro
           PERFORM 972000-LER-CONTAINER-DE-ERRO

      *    Efetua a leitura do Container de Resposta
           PERFORM 973000-LER-RESPOSTA
           .
       900000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       910000-CHAMAR-SERVICO           SECTION.
      *--> Acionar programa IIBP2000 para requisição do serviço.
      *-----------------------------------------------------------------

      *--> Grava o container com dados do serviço/operação a chamar
           PERFORM 981000-GRAVAR-CONTAINER-SRVC

      *--> Grava o container de requisicao do modulo a executar
           PERFORM 982000-GRAVAR-CONTAINER-RQSC

      *--> Limpa a resposta e erro antes de executar o modulo
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS LINK
                     PROGRAM   ( IIBP2000     )
                     CHANNEL   ( NM-CNL       )
                     RESP      ( MSG1-RESP1   )
                     RESP2     ( MSG1-RESP2   )
           END-EXEC

           EVALUATE TRUE
               WHEN MSG1-RESP1             EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    SET   ERRO-CICS    TO  TRUE
                    MOVE  9000         TO  MSG1-CD-ERRO-TS
                    STRING  'Erro ao acionar serviço: '
                            SRVC-ID-SRVC '.' SRVC-VRS-SRVC '.'
                            SRVC-ID-OPR  '.' SRVC-VRS-OPR
                            'via IIBP2000, ' 'RESP=' MSG1-RESP1
                                           ', RESP-2=' MSG1-RESP2 '.'
                            DELIMITED BY SIZE
                            INTO  MSG1-TX-ERRO-TS
                    END-STRING

                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *--> Efetua a leitura do Container de Erro
           PERFORM 972000-LER-CONTAINER-DE-ERRO

      *--> Efetua a leitura do Container de Resposta
           PERFORM 973000-LER-RESPOSTA
           .
       910000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       940000-VALIDA-DATA              SECTION.
      *----------------------------------------------------------------*
           MOVE 0010                 TO MSG1-CD-SEQL-ERRO
           MOVE 4                      TO ARG02
           CALL SBDATAF USING FUNCAO
                              AREA-WORK
                              ARG01
                              ARG02

      *--> Return code = 24 - Data Inválida.
      *                = 22 - Conteudo alfa em argumento numerico

           IF  NOT (RETURN-CODE EQUAL ZEROS OR 24 OR 22)
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0051               TO MSG1-CD-ERRO-TS
               MOVE RETURN-CODE        TO MSG1-CD-RTN-PGM-ACND
               MOVE 'Erro no tratamento de datas.'
                                       TO MSG1-TX-ERRO-TS
               MOVE SBDATAF            TO MSG1-NM-PGM-ACND
               SET MNT-VRV             TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
               MOVE RETURN-CODE        TO MSG1-CD-RTN-PGM-ACND
           .
       940000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970101-ABRE-CURSOR-PSQ                SECTION.
      *-----------------------------------------------------------------
           MOVE 0011                  TO MSG1-CD-SEQL-ERRO
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
           MOVE 0012                  TO MSG1-CD-SEQL-ERRO

           INITIALIZE REG-WS

           EXEC SQL
                FETCH CURSOR-PSQ
               INTO :CD-CTRA-TIT-VL
                  , :CD-TIT-OPR-ETRD
                  , :NR-DVS-TIT
                  , :DT-PSC
                  , :VL-SDO-TIT-DT-PSC
                  , :CD-EST-RSTD-ITE
                  , :NR-SEQL-PSC-DRIA
           END-EXEC

           MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ

           .
       970102-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970104-FECHA-CURSOR-PSQ SECTION.
      *-----------------------------------------------------------------
           MOVE 0013                  TO MSG1-CD-SEQL-ERRO
           MOVE 'N'                    TO IN-CURSOR-PSQ-ABTO
           EXEC SQL
                CLOSE CURSOR-PSQ
           END-EXEC
           .
       970104-SAI.
           EXIT.

      *-----------------------------------------------------------------
       972000-LER-CONTAINER-DE-ERRO    SECTION.
      *--> A presença do container PRM-ERRO indica que houve erro de
      *    integração, a ausência do container indica sucesso.
      *-----------------------------------------------------------------

      *--> Le o container de erro dos modulos chamados
           SET   GET-CONTAINER         TO  TRUE
           MOVE  'PRM-ERRO'            TO  NM-CTNR
           INITIALIZE                      IIBKERRO-PRM-ERRO

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS GET
                CONTAINER ( NM-CTNR                     )
                INTO      ( IIBKERRO-PRM-ERRO           )
                FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                RESP      ( MSG1-RESP1                  )
                RESP2     ( MSG1-RESP2                  )
           END-EXEC

           EVALUATE TRUE
      *        Caso deseje personalizar ou não interromper o
      *        processamento, alterar para não dar erro aqui SOMENTE
      *        PARA O M DULO CHAMADO e tratar na section chamadora
               WHEN MSG1-RESP1             EQUAL DFHRESP(CONTAINERERR)
                    CONTINUE
               WHEN MSG1-RESP1             EQUAL DFHRESP(NORMAL)
                    SET   ERRO-PROGRAMA-ACIONADO TO  TRUE
                    MOVE  NM-PGM    TO  MSG1-NM-PGM-ACND
                    IF NM-PGM(1:3) EQUAL 'SGT'
                       MOVE  IIBKERRO-CD-ERRO
                                    TO  MSG1-CD-RTN-PGM-ACND
                                        MSG1-CD-ERRO-TS
                       MOVE  IIBKERRO-TX-ERRO
                                      TO  MSG1-TX-RTN-PGM-ACND
                                          MSG1-TX-ERRO-TS
                    ELSE
                       MOVE 3004          TO MSG1-CD-ERRO-TS
                       STRING  'Erro ao acionar o módulo ' NM-PGM '.'
                                DELIMITED BY SIZE
                                INTO           MSG1-TX-ERRO-TS

                       MOVE IIBKERRO-TX-ERRO
                                   TO MSG1-TX-RTN-PGM-ACND
                       MOVE IIBKERRO-CD-ERRO
                                   TO MSG1-CD-RTN-PGM-ACND

                    END-IF
                 PERFORM 999000-GRAVA-ERRO-E-ENCERRA
               WHEN OTHER
                    SET   ERRO-CONTAINER
                                  TO  TRUE
                    PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       972000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       973000-LER-RESPOSTA             SECTION.
      *--> Recupera o container com os dados de resposta (saída).
      *    Modulo CONTAINER  -     NOME_PGM-RPST
      *    Modulo IIB (FIXO) -     PRM-DADOS-RPST
      *----------------------------------------------------------------

      *--> Monta o nome dos container de acordo com modulo a executar
           MOVE  SPACES                TO  NM-CTNR

      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *    nome_modulo -RPST

           EVALUATE NM-PGM
               WHEN MCISI074
                    MOVE  'PRM-DADOS-RPST'
                                       TO  NM-CTNR
               WHEN OTHER
                    MOVE  NM-PGM       TO  NM-CTNR(1:8)
                    MOVE  '-RPST'      TO  NM-CTNR(9:5)
           END-EVALUATE

      *    Recupera o tamanho do container de resposta
            PERFORM 973100-LER-CONTAINER-NODATA

      *==> Observação <=================================================
      *    Os Programas Acionados que possuem Resposta de Tamanho
      *    Variável não devem validar o Tamanho Recebido
      *=================================================================

      *--> Validar o Tamanho Recebido do Container de Resposta
      *    com o Tamanho Esperado dos Módulos de Tamanho Fixo

           EVALUATE  NM-PGM
      *------> Incluir Módulos com Resposta de Tamanho Fixo
               WHEN  MCISI074
                     MOVE  LENGTH OF MCISI074-DADOS
                                       TO  QT-CCT-EPRO
                     PERFORM 973200-TAMANHO-ESPERADO
               WHEN  OTHER
                     CONTINUE
           END-EVALUATE

      *--> Configura dados para leitura do container de resposta
           SET   GET-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *--> Le a RESPOSTA de acordo com o programa que a gravou

           EVALUATE NM-PGM

      *        Calcular a quantidade de caracteres esperado e limpar
      *        a area de dados antes de executar o GET
      *        Para matrizes com dados variaveis (clausula depending
      *        on) nao ha como utilizar o comando INITIALIZE

           WHEN MCISI074
                INITIALIZE             MCISI074-DADOS
                EXEC CICS GET
                          CONTAINER (NM-CTNR          )
                          INTO      (MCISI074-DADOS   )
                          FLENGTH   (QT-CCT-LIDO      )
                END-EXEC
               WHEN SGTSL037
                    MOVE LENGTH OF SGTSL037-RPST TO QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSL037-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC
           WHEN OTHER
                SET   ERRO-PROGRAMADO
                                    TO  TRUE
                MOVE  9003          TO  MSG1-CD-ERRO-TS
                STRING  'Erro na leitura da resposta do módulo '
                        NM-PGM '.'
                        DELIMITED BY SIZE
                        INTO           MSG1-TX-ERRO-TS
                PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       973000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       973100-LER-CONTAINER-NODATA     SECTION.
      *-----------------------------------------------------------------

      *--> Configura dados para leitura do container de requisição
           SET   GET-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *--> Obtem a quantidade de caracteres no container
           MOVE  ZEROS                 TO  QT-CCT-LIDO
           EXEC CICS GET
                     CONTAINER (NM-CTNR    )
                     NODATA
                     FLENGTH   (QT-CCT-LIDO)
           END-EXEC
      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       973100-SAI.
           EXIT.
      *-----------------------------------------------------------------
       973200-TAMANHO-ESPERADO         SECTION.
      *-----------------------------------------------------------------
           MOVE  0014                TO  MSG1-CD-SEQL-ERRO

           EVALUATE NM-PGM

               WHEN MCISI074
                    MOVE  LENGTH OF MCISI074-DADOS
                                         TO  QT-CCT-EPRO
               WHEN OTHER
                    SET   ERRO-PROGRAMADO
                                         TO  TRUE
                    MOVE  9003           TO  MSG1-CD-ERRO-TS
                    STRING  'Erro na leitura da resposta do módulo '
                            NM-PGM '.'
                            DELIMITED BY SIZE
                            INTO           MSG1-TX-ERRO-TS
                    END-STRING
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE
           IF  QT-CCT-LIDO                 NOT EQUAL QT-CCT-EPRO
               SET   ERRO-PROGRAMADO
                                           TO  TRUE
               MOVE  9001              TO  MSG1-CD-ERRO-TS
               MOVE  QT-CCT-EPRO       TO  VL-EXBR-1
               MOVE  QT-CCT-LIDO       TO  VL-EXBR-2
               STRING
                    'Erro no tamanho de área de requisição '
                    ', TAMANHO ESPERADO: '  VL-EXBR-1
                    ', TAMANHO RECEBIDO: '  VL-EXBR-2
                    DELIMITED BY SIZE   INTO  MSG1-TX-ERRO-TS
              END-STRING
              PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       973200-SAI.
           EXIT.

      *-----------------------------------------------------------------
       974000-LER-DADOS-OPRD           SECTION.
      *-----------------------------------------------------------------
           MOVE  0024                  TO  MSG1-CD-SEQL-ERRO

      *--> Configura dados para leitura do container de requisição
           SET   GET-CONTAINER         TO  TRUE
           MOVE  'PRM-INF-OPRD'        TO  NM-CTNR

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

           INITIALIZE                  PRM-INF-OPRD
           EXEC CICS GET
                     CONTAINER ( NM-CTNR                )
                     INTO      ( PRM-INF-OPRD           )
                     FLENGTH   ( LENGTH OF PRM-INF-OPRD )
           END-EXEC.

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE

           MOVE KOPRD-CD-OPRD          TO  CD-CHV-USU
           .
       974000-SAI.
           EXIT.
      *------------------------------------------------------------
       980000-CHAMA-SERVICO             SECTION.
      *------------------------------------------------------------
           MOVE 0015                 TO MSG1-CD-SEQL-ERRO

      *--> Grava o container com dados do serviço/operação a chamar
           PERFORM 981000-GRAVAR-CONTAINER-SRVC

      *--> Grava o container de requisicao do modulo a executar
           PERFORM 982000-GRAVAR-CONTAINER-RQSC

      *--> Limpa a resposta e erro antes de executar o modulo
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
              EXEC CICS LINK
                   PROGRAM   ( IIBP2000     )
                   CHANNEL   ( NM-CNL       )
                   RESP      ( MSG1-RESP1   )
                   RESP2     ( MSG1-RESP2   )
              END-EXEC
              EVALUATE TRUE
                  WHEN MSG1-RESP1             EQUAL DFHRESP(NORMAL)
                       CONTINUE
                  WHEN OTHER
                       SET   ERRO-CICS    TO  TRUE
                       MOVE  9000         TO  MSG1-CD-ERRO-TS
                       STRING  'Erro ao acionar serviço: '
                               SRVC-ID-SRVC '.' SRVC-VRS-SRVC '.'
                               SRVC-ID-OPR  '.' SRVC-VRS-OPR
                               'via IIBP2000, ' 'RESP=' MSG1-RESP1
                               ', RESP-2=' MSG1-RESP2 '.'
                               DELIMITED BY SIZE
                               INTO  MSG1-TX-ERRO-TS
                       END-STRING

                       PERFORM 999000-GRAVA-ERRO-E-ENCERRA
            END-EVALUATE

      *--> Efetua a leitura do Container de Erro
           PERFORM 972000-LER-CONTAINER-DE-ERRO

      *--> Efetua a leitura do Container de Resposta
           PERFORM 973000-LER-RESPOSTA

           .
       980000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       981000-GRAVAR-CONTAINER-SRVC    SECTION.
      *--> Postar container de requisição de serviço.
      *-----------------------------------------------------------------
             MOVE  0016                TO  MSG1-CD-SEQL-ERRO

      *--> Monta o nome do container de requisicao do servico
             MOVE  'PRM-RQSC-SRVC'       TO  NM-CTNR
             SET   PUT-CONTAINER         TO  TRUE

            EVALUATE NM-PGM

                WHEN MCISI074
                     MOVE  19485        TO  SRVC-ID-SRVC
                     MOVE  1            TO  SRVC-VRS-SRVC
                     MOVE  1574513      TO  SRVC-ID-OPR
                     MOVE  0001         TO  SRVC-VRS-OPR

           END-EVALUATE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

           EXEC CICS PUT
                CONTAINER ( NM-CTNR                     )
                FROM      ( SRVC-PRM-RQSC-SRVC          )
                FLENGTH   ( LENGTH OF SRVC-PRM-RQSC-SRVC)
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       981000-SAI.
            EXIT.
      *-----------------------------------------------------------------
       982000-GRAVAR-CONTAINER-RQSC    SECTION.
      *    Postar container com dados de requisição (entrada)
      *    Modulo CONTAINER  -     NOME_PGM-RQSC
      *    Modulo IIB (FIXO) -     PRM-DADOS-RQSC
      *-----------------------------------------------------------------
            MOVE  0017                TO  MSG1-CD-SEQL-ERRO

      *--> Monta o nome dos container de acordo com modulo a executar
           MOVE  SPACES                TO  NM-CTNR
           SET   PUT-CONTAINER         TO  TRUE

      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RQSC, senão
      *    nome_modulo -RQSC

           EVALUATE NM-PGM
               WHEN MCISI074
                    MOVE  'PRM-DADOS-RQSC'   TO  NM-CTNR

               WHEN OTHER
                    MOVE  NM-PGM       TO  NM-CTNR(1:8)
                    MOVE  '-RQSC'      TO  NM-CTNR(9:5)
           END-EVALUATE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *--> Faz PUT da requisicao do programa a executar
            EVALUATE NM-PGM
                WHEN MCISI074
                     EXEC CICS PUT
                               CONTAINER (NM-CTNR                 )
                               FROM      (MCISI074-DADOS          )
                               FLENGTH   (LENGTH OF MCISI074-DADOS)
                     END-EXEC

               WHEN SGTSL037
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (SGTSL037-RQSC )
                              FLENGTH   (LENGTH OF SGTSL037-RQSC)
                    END-EXEC

                WHEN OTHER
                     SET   ERRO-PROGRAMADO
                                        TO  TRUE
                     MOVE  9004         TO  MSG1-CD-ERRO-TS
                     STRING  'Erro na gravação da requisição do módulo '
                              NM-PGM '.'
                              DELIMITED BY SIZE
                              INTO           MSG1-TX-ERRO-TS
                     END-STRING
                     PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *--> Ao chegar aqui, há certeza de que não houve erro algum
            SET   SEM-ERRO              TO  TRUE
            .
       982000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       983000-EXCLUIR-RESPOSTA-ERRO    SECTION.
      * Exclui o container de resposta e de erro para evitar problemas
      * em modulos executados mais de uma vez dentro da mesma sessao do
      * CICS. Equivale ao initialize na area de resposta dos programas
      * que utilizam COMMAREA.
      *-----------------------------------------------------------------
           MOVE  0018                TO  MSG1-CD-SEQL-ERRO

      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *    nome_modulo -RPST

           EVALUATE NM-PGM

               WHEN MCISI074
                    MOVE  'PRM-DADOS-RPST'
                                       TO  NM-CTNR
               WHEN OTHER
                    MOVE  NM-PGM       TO  NM-CTNR(1:8)
                    MOVE  '-RPST'      TO  NM-CTNR(9:5)
           END-EVALUATE

           SET   DEL-CONTAINER         TO  TRUE

      *--> Exclui o container de resposta
      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS DELETE
                CONTAINER (NM-CTNR   )
                RESP      (MSG1-RESP1)
                RESP2     (MSG1-RESP2)
           END-EXEC

           EVALUATE TRUE
               WHEN MSG1-RESP1             EQUAL DFHRESP(CONTAINERERR)
               WHEN MSG1-RESP1             EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    SET   ERRO-CONTAINER
                                   TO  TRUE
                    PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE

      *--> Excluir o container de erro
            PERFORM 983100-EXCLUIR-CONTAINER-ERRO
            .
       983000-SAI.
            EXIT.
      *-----------------------------------------------------------------
       983100-EXCLUIR-CONTAINER-ERRO   SECTION.
      *-----------------------------------------------------------------

      *--> Limpar variaveis container de erro
           INITIALIZE                      IIBKERRO-PRM-ERRO

      *--> Exclui o container de erro
           MOVE  'PRM-ERRO'            TO  NM-CTNR
           SET   DEL-CONTAINER         TO  TRUE

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS DELETE
                CONTAINER (NM-CTNR   )
                RESP      (MSG1-RESP1)
                RESP2     (MSG1-RESP2)
           END-EXEC

           EVALUATE TRUE
               WHEN MSG1-RESP1             EQUAL DFHRESP(CONTAINERERR)
               WHEN MSG1-RESP1             EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    SET   ERRO-CONTAINER
                                     TO  TRUE
                    PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       983100-SAI.
           EXIT.
      *-----------------------------------------------------------------
       984000-GRAVAR-CONTAINER-ERRO    SECTION.
      *--> Grava o erro gerado no container de erro
      *-----------------------------------------------------------------

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS PUT
                CONTAINER ( 'PRM-ERRO'                  )
                FROM      ( IIBKERRO-PRM-ERRO           )
                FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                RESP      ( MSG1-RESP1                  )
                RESP2     ( MSG1-RESP2                  )
           END-EXEC
           .
       984000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------

           IF  ERRO-PROGRAMA-ACIONADO
               PERFORM 999100-MONITORAR-VRV-PGM-CHMD
           END-IF

           PERFORM 999200-MONITORAR-VRV-PGM-E-ENC
           .
       999000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       999100-MONITORAR-VRV-PGM-CHMD   SECTION.
      *-----------------------------------------------------------------
      *--> Monitora as variáveis dos módulos chamados

           EVALUATE  NM-PGM-CHMD

               WHEN SGTSL037
                    MOVE 'SGTSL037-CD-TIP-SRVC'  TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL037-CD-TIP-SRVC    TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL037-E-CD-TIP-ATB-CTRA-01'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL037-E-CD-TIP-ATB-CTRA-01
                                                 TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN  OTHER
                    CONTINUE
           END-EVALUATE
           .
       999100-SAI.
           EXIT.
      *-----------------------------------------------------------------
       999200-MONITORAR-VRV-PGM-E-ENC  SECTION.
      *-----------------------------------------------------------------
      *--> Monitorar variàveis do pgm, grava e encerra processamento

           IF  MSG1-CD-ERRO-TS             EQUAL  ZEROS
              MOVE  8888               TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS             EQUAL  SPACES
               MOVE  'Erro não especificado no módulo.'
                                       TO  MSG1-TX-ERRO-TS
           END-IF

      *--> Em caso de erro, desfaz as atualizaçoes em banco de dados
           EXEC CICS SYNCPOINT
                     ROLLBACK
           END-EXEC
      *--> VERIFICA SE O TIPO DE ERRO GERADO POSSUI TRATAMENTO INTERNO

           EVALUATE TRUE
               WHEN ERRO-CONTAINER
                    PERFORM 999004-TRATA-ERRO-CTNR
               WHEN ERRO-TAMANHO-AREA
                    PERFORM 999005-TRATA-ERRO-AREA-DADOS
               WHEN ERRO-CANAL
                    PERFORM 999006-TRATA-ERRO-CANAL
               WHEN OTHER
                    CONTINUE
           END-EVALUATE
      *--> Adiciona as variáveis para monitoração
           IF  MNT-VRV
               MOVE 'SGTS003A-DT-IMP-PSC-SDO'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE SGTS003A-DT-IMP-PSC-SDO
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS003A-HR-IMP-PSC-SDO'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE SGTS003A-HR-IMP-PSC-SDO
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE  'SGTS003A-ULT-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTS003A-ULT-CD-CTRA-TIT-VL
                                       TO    MCR-S9-15
               MOVE  MCR-S9-15         TO    MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE  'SGTS003A-ULT-CD-TIT-OPR-ETRD '
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTS003A-ULT-CD-TIT-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

              MOVE 'SBDATA-RETURN-CODE'   TO  MSGKW999-NM-VRV-MNT
              MOVE  MSG1-CD-RTN-PGM-ACND  TO  MSGKW999-VL-VRV-MNT
              PERFORM  999003-ADICIONA-VRV-MNT

               MOVE  'NR-DVS-TIT'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  NR-DVS-TIT        TO    MCR-S9-15
               MOVE  MCR-S9-15         TO    MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE  'CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  CD-CTRA-TIT-VL
                                       TO    MCR-S9-15
               MOVE  MCR-S9-15         TO    MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE  'CD-TIT-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  CD-TIT-OPR-ETRD   TO    MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

           END-IF
      * --> grava o erro no msg
           PERFORM MSG1-NTF-MSG
           MOVE MSG1-CD-ERRO-TS        TO IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS        TO IIBKERRO-TX-ERRO
           EXEC CICS PUT
              CONTAINER ( 'PRM-ERRO' )
              CHANNEL   ( NM-CNL      )
              FROM      ( IIBKERRO-PRM-ERRO)
              FLENGTH ( LENGTH OF IIBKERRO-PRM-ERRO )
              NOHANDLE
           END-EXEC

           PERFORM 999999-ENCERRAR
           .
       999200-SAI.
           EXIT.

      * --------------------------------------*
      * 999001-TRATA-ABEND-CICS        SECTION.
      * 999002-TRATA-CONDITION-CICS    SECTION.
      * 999003-ADICIONA-VRV-MNT        SECTION.
      * --------------------------------------*
-INC HLPKP999
      *---------------------------------------*
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *---------------------------------------*
-INC SGTKP999
      * --------------------------------------*
      * 999010-TRATA-ERRO-DB2          SECTION.
-INC HLPKP997

      * --------------------------------------*
      * MSG1-NTF-MSG                   SECTION.
-INC MSGKPMG1
      * --------------------------------------*
       999999-ENCERRAR                SECTION.
      * --------------------------------------*
           EXEC CICS
                RETURN
           END-EXEC
           .
       999999-SAI.
           EXIT.