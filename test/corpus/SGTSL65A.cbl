      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSL65A
      * ANALISTA..: F8608799 - RICARDO RAMOS
      * AUTOR.....: F8608799 - RICARDO RAMOS
      * OBJETIVO..: CONSULTA LISTA ANALITICA DA TABELA IMP_MVTC_FNCR_TIT
      *             DE UMA DATA E HORA IMPORTACAO
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS004 19.11.2024 - F2054109 - Ajustes SQL
      * VRS003 30.09.2024 - F7464259 - Adição de filtros para
      *                     CD_EST_IMP
      * VRS002 05.04.2018 - F2634012 - DE 2016/145195 - EXCLUSÃO DO CAM-
      *        PO IN_IMP_MVTC_SCS E INCLUSÃO DO CAMPO CD_EST_IMP
      * VRS001 11.07.2016 - F8608799 - DE 2015/137233 - IMPLANTACAO
      *-----------------------------------------------------------------
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSL65A.

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
       01  CTE-PROG     PIC  X(17) VALUE '*** SGTSL65A ***'.
       01  CTE-VERS     PIC  X(06) VALUE 'VRS004'.

      * Módulos acionados
       77  SBDATA                           PIC  X(08) VALUE 'SBDATA'.
      *-----------------------------------------------------------------
      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informacoes
-INC HLPKE01B
       LOCAL-STORAGE SECTION.
      *-----------------------------------------------------------------
      *-----------------------------------------------------------------
      * book de para acionamento do msg1
-INC IIBKERRO
-INC MSGKWMG1
-INC HLPKW999
      *--> Variaveis para tratamento de CONTAINER
-INC SGTKERR1
      * Area de trabalho

       01 QT-REG-SELD                         PIC S9(4) COMP-5 VALUE 0.

      * Campos utilizados para leitura

       01  REG-WS.
           03 CD-TIT-OPR-ETRD                PIC  X(12)
              OCCURS 101 TIMES               VALUE ' '.
           03 CD-TIT-OPR-ETRD-N              PIC  S9(4) COMP-5
              OCCURS 101 TIMES               VALUE 0.

           03 NR-DVS-TIT                     PIC  S9(4) COMP-5
              OCCURS 101 TIMES               VALUE 0.
           03 NR-DVS-TIT-N                   PIC  S9(4) COMP-5
              OCCURS 101 TIMES               VALUE 0.

           03 DT-EFT-MVTC                    PIC X(08)
              OCCURS 101 TIMES               VALUE ' '.
           03 DT-EFT-MVTC-N                  PIC S9(4) COMP-5
              OCCURS 101 TIMES               VALUE 0.

           03 CD-TIP-MVTC                    PIC S9(4) COMP-5
              OCCURS 101 TIMES               VALUE 0.
           03 CD-TIP-MVTC-N                  PIC S9(4) COMP-5
              OCCURS 101 TIMES               VALUE 0.

           03 NM-TIP-MVTC                    PIC  X(50)
              OCCURS 101 TIMES               VALUE ' '.
           03 NM-TIP-MVTC-N                  PIC S9(4) COMP-5
              OCCURS 101 TIMES               VALUE 0.

           03 CD-EST-IMP                     PIC S9(4) COMP-5
              OCCURS 101 TIMES               VALUE 0.

           03 TX-DCR-ERRO-IMP                PIC X(200)
              OCCURS 101 TIMES               VALUE ' '.
           03 TX-DCR-ERRO-IMP-N              PIC S9(4) COMP-5
              OCCURS 101 TIMES               VALUE 0.

           03 NR-IMP-MVTC-FNCR                 PIC S9(4) COMP-5
              OCCURS 101 TIMES               VALUE 0.
           03 NR-IMP-MVTC-FNCR-N               PIC S9(4) COMP-5
              OCCURS 101 TIMES               VALUE 0.

           03 CD-CTRA-TIT-VL                   PIC S9(9) COMP-5
              OCCURS 101 TIMES               VALUE 0.
           03 CD-CTRA-TIT-VL-N                 PIC S9(4) COMP-5
              OCCURS 101 TIMES               VALUE 0.

           03 CD-TIP-OGM-INF                 PIC S9(4) COMP-5
              OCCURS 101 TIMES               VALUE 0.



      * Indicadores de nulidades

       01 QT-REG-SELD-N                       PIC S9(4) COMP-5 VALUE 0.

      * Variavel(is) de controle do cursor(es)

       01  CTL-CURSOR-PSQ.
           03  CD-FIM-CURSOR-PSQ            PIC S9(04) COMP-5 VALUE 0.
               88  FIM-CURSOR-PSQ            VALUE +100.
           03  IN-CURSOR-PSQ-ABTO           PIC  X(01) VALUE 'N'.
               88  CURSOR-PSQ-ABTO          VALUE 'S'.

      * Variáveis utilizadas na query
       01  AUX-CTRA-1                       PIC S9(09) COMP-5 VALUE 0.
       01  AUX-CTRA-2                       PIC S9(09) COMP-5 VALUE 0.
       01  AUX-IN-1                         PIC S9(4)  COMP-5 VALUE 0.
       01  AUX-IN-2                         PIC S9(4)  COMP-5 VALUE 0.
       01  AUX-TIT                          PIC X(12) VALUE ' '.

       01 GDA-CD-TIT-OPR-ETRD            PIC  X(12) VALUE ' '.
       01 GDA-NR-DVS-TIT                 PIC  S9(4) COMP-5 VALUE 0.
       01 GDA-CD-CTRA-TIT-VL             PIC  S9(9) COMP-5 VALUE 0.
       01 GDA-CD-OPR-ETRD-MCP-1          PIC S9(11) COMP-3 VALUE 0.

       01 GDA-CD-OPR-ETRD-MCP            PIC S9(11) COMP-3 VALUE 0.
       01 GDA-NR-VRS-OPR-ETRD            PIC S9(04) COMP-5 VALUE 0.
       01 GDA-NR-SEQL-RCBT-PRVT          PIC  S9(4) COMP-5 VALUE 0.

       01 GDA-VL-PND-VCLC-DMTV-CX       PIC S9(10)V9(8) COMP-3  VALUE 0.
       01 GDA-NR-IMP-MVTC-FNCR          PIC S9(4) COMP-5 VALUE 0.

       01  GDA-SQLCODE                 PIC -9999.


      * Para cálculo de diferença entre datas (após validação SBDATA)
       01  AUX-DT-INC                       PIC 9(08) VALUE 0.
       01  AUX-DT-INC-AAAAMMDD REDEFINES AUX-DT-INC.
           03  ANO                          PIC 9(04).
           03  MES                          PIC 9(02).
           03  DIA                          PIC 9(02).
       01  AUX-DT-FIM                       PIC 9(08) VALUE 0.
       01  AUX-DT-FIM-AAAAMMDD REDEFINES AUX-DT-FIM.
           03  ANO                          PIC 9(04).
           03  MES                          PIC 9(02).
           03  DIA                          PIC 9(02).
      *
       01  AUX-DATA                         PIC X(10)
                                            VALUE '00.00.0000'.
       01  AUX-DATA-R  REDEFINES AUX-DATA.
           03  DIA                          PIC 9(02).
           03  FILLER                       PIC X(01).
           03  MES                          PIC 9(02).
           03  FILLER                       PIC X(01).
           03  ANO                          PIC 9(04).

       01  AUX-DT-IMP-MVTC                  PIC X(10).
       01  AUX-DT-REF-INI                   PIC X(10).
       01  AUX-DT-REF-FIM                   PIC X(10).

      * Subrotinas utilizadas
      * Validar data
       01  AREA-SBDATA-F01.
           03  SBDATA-FUNCAO-F01       PIC  X(3) VALUE 'F01'.
           03  ARG01-F01               PIC  9(8) VALUE 0.
           03  ARG01-F01-R REDEFINES ARG01-F01.
               05  DIA                 PIC  9(2).
               05  MES                 PIC  9(2).
               05  ANO                 PIC  9(4).
           03  ARG02-F01               PIC  9(1) VALUE 0.

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

      *-----------------------------------------------------------------
-INC SGTKL65A
           EXEC SQL
             DECLARE CURSOR-PSQ CURSOR
                     WITH ROWSET POSITIONING FOR
             SELECT
                 A.CD_TIT_OPR_ETRD
               , A.NR_DVS_TIT
               , CHAR(TO_CHAR(A.DT_EFT_MVTC,'DDMMYYYY') , 8)
               , A.CD_TIP_MVTC
               , B.NM_TIP_MVTC_TIT
               , A.CD_EST_IMP
               , A.TX_DCR_ERRO_IMP
               , A.NR_IMP_MVTC_FNCR
               , A.CD_CTRA_TIT_VL
               , A.CD_TIP_OGM_INF
             FROM
             	DB2SGT.IMP_MVTC_FNCR_TIT A
             LEFT OUTER JOIN
                 DB2SGT.TIP_MVTC_TIT B
             ON  A.CD_TIP_MVTC     = B.CD_TIP_MVTC_TIT
             WHERE
             -- DADOS A REMESSA A LISTAR
                 A.DT_IMP_MVTC_FNCR  = :AUX-DT-IMP-MVTC
             AND A.HR_IMP_MVTC_FNCR  = :SGTSL65A-HR-IMP-MVTC-FNCR
             -- FILTROS INFORMADOS EM TELA
             AND(UPPER (A.CD_TIT_OPR_ETRD) LIKE
                 UPPER ('%'||:SGTSL65A-CD-TIT-OPR-ETRD)
             OR  VALUE(A.CD_TIT_OPR_ETRD , '') = :AUX-TIT)
             AND (A.CD_CTRA_TIT_VL BETWEEN :AUX-CTRA-1 AND :AUX-CTRA-2
             OR   VALUE(A.CD_CTRA_TIT_VL , 0) = :AUX-CTRA-1)
             AND(A.DT_EFT_MVTC    BETWEEN :AUX-DT-REF-INI
                                  AND     :AUX-DT-REF-FIM
             OR A.DT_EFT_MVTC      IS NULL)
             AND (A.CD_EST_IMP     BETWEEN :AUX-IN-1 AND :AUX-IN-2)
             -- CAMPOS PARA PAGINAÇÃO
             AND A.NR_IMP_MVTC_FNCR > :SGTSL65A-NR-IMP-MVTC-FNCR-PAG
             ORDER BY
                 A.NR_IMP_MVTC_FNCR ASC
           END-EXEC.

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------


      * Habilita tratamento automatico de erros DB2
-INC HLPKE997
      * Habilita captura automatica de abend e erros CICS
-INC HLPKE999

           PERFORM 100000-DESCOBRIR-CANAL
           PERFORM 110000-EXCLUIR-CTNR-RPST
           PERFORM 120000-EXCLUIR-CTNR-ERRO
           PERFORM 130000-RECEBER-REQUISICAO
           PERFORM 140000-VALIDAR-PARAMETRO

           PERFORM 970101-ABRE-CURSOR-PSQ
           PERFORM 970102-LER-CURSOR-PSQ
           PERFORM 970103-FECHA-CURSOR-PSQ
           PERFORM 200000-PROCESSA-PESQUISA
                    VARYING SGTSL65A-IC-TAB FROM 1 BY 1
                    UNTIL SGTSL65A-IC-TAB > SGTSL65A-QT-OCR-TAB

           PERFORM 300000-POSTA-RESPOSTA
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
       110000-EXCLUIR-CTNR-RPST SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO

           SET DEL-CONTAINER           TO TRUE

      *--> Exclui o container de resposta
      *--> Não há captura automática de erro CICS por usar RESP e RESP2

           EXEC CICS DELETE
              CONTAINER ( 'SGTSL65A-RPST' )
              RESP      (MSG1-RESP1)
              RESP2     (MSG1-RESP2)
           END-EXEC

           EVALUATE TRUE
               WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
               WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    SET ERRO-CONTAINER TO TRUE
                   PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE

           .
       110000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       120000-EXCLUIR-CTNR-ERRO SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                   TO MSG1-CD-SEQL-ERRO

      *--> Limpar variaveis container de erro
           INITIALIZE IIBKERRO-PRM-ERRO
           SET DEL-CONTAINER           TO TRUE

      *--> Não há captura automática de erro CICS por usar RESP e RESP2

           EXEC CICS DELETE
              CONTAINER ( 'PRM-ERRO' )
              RESP      (MSG1-RESP1)
              RESP2     (MSG1-RESP2)
           END-EXEC

           EVALUATE TRUE
               WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
               WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    SET ERRO-CONTAINER TO TRUE
                    PERFORM 999002-TRATA-CONDITION-CICS
            END-EVALUATE

           .
       120000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       130000-RECEBER-REQUISICAO SECTION.
      *-----------------------------------------------------------------
           MOVE 0004                   TO MSG1-CD-SEQL-ERRO

      *--> Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *--> previsto o erro de container
           SET  ERRO-CONTAINER         TO TRUE

           MOVE ZEROS                  TO QT-CCT-LIDO
           MOVE LENGTH OF SGTSL65A-RQSC      TO QT-CCT-EPRO
           EXEC CICS GET
              CONTAINER ( 'SGTSL65A-RQSC' )
              NODATA
              FLENGTH ( QT-CCT-LIDO )
           END-EXEC

      * --> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE

           IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
               SET ERRO-TAMANHO-AREA   TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           EXEC CICS GET
              CONTAINER ( 'SGTSL65A-RQSC' )
              CHANNEL   ( NM-CNL      )
              FLENGTH ( LENGTH OF SGTSL65A-RQSC )
              INTO    ( SGTSL65A-RQSC )
           END-EXEC
           .
       130000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       140000-VALIDAR-PARAMETRO SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO
           MOVE 'N'                    TO SGTSL65A-IN-CNTO-PSQ
           IF  SGTSL65A-DT-IMP-MVTC-FNCR EQUAL SPACES
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0368               TO MSG1-CD-ERRO-TS
               MOVE 'Data importacao movimentacao financeira nao informa
      -              'da.'             TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
      *    Verifica se a data é válida                                    VRS001
           MOVE CORR SGTSL65A-DT-IMP-MVTC-FNCR-R
                                       TO ARG01-F01-R                     VRS001
           MOVE 4                      TO ARG02-F01                       VRS001
           PERFORM 940000-VALIDA-DATA                                     VRS001
                                                                          VRS001
      *--> Return code = 24 - Data Inválida.
      *                  22 - Conteudo alfa em argumento numerico
           IF  RETURN-CODE EQUAL 24 OR 22                                 VRS001
               SET ERRO-PROGRAMADO     TO TRUE                            VRS001
               MOVE 0371               TO MSG1-CD-ERRO-TS                 VRS001
               MOVE 'Data importacao movimentacao financeira inválida.'   VRS00
                                       TO MSG1-TX-ERRO-TS                 VRS001
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL65A-HR-IMP-MVTC-FNCR EQUAL SPACES
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0369               TO MSG1-CD-ERRO-TS
               MOVE 'Horario importacao movimentacao financeira invalido
      -             '.'                TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *    Filtro pelo situação.

      *    cd-est-imp  = 20 ---> todas as situações
VRS003     IF SGTSL65A-CD-EST-IMP      EQUAL  20
              MOVE 0                   TO     AUX-IN-1
VRS003        MOVE 20                  TO     AUX-IN-2
           ELSE
              MOVE SGTSL65A-CD-EST-IMP TO     AUX-IN-1 AUX-IN-2
           END-IF

      *    Filtro pelo titulo.
      *    Move para auxiliar para qdo for brancos, recuperar valores
      *    nulos.
      *    Depois, substitui brancos por % para pesquisa
           MOVE SGTSL65A-CD-TIT-OPR-ETRD
                                       TO AUX-TIT
           INSPECT SGTSL65A-CD-TIT-OPR-ETRD
                   REPLACING ALL SPACES BY '%'


      *    Filtro carteira, se vier só considera a informada, senão
      *    todas. 2147483647-valor máximo para INTEGER (S9(09) COMP-5)
           EVALUATE SGTSL65A-CD-CTRA-TIT-VL
               WHEN 0
                    MOVE 0             TO AUX-CTRA-1
                    MOVE 2147483647    TO AUX-CTRA-2
               WHEN OTHER
                    MOVE SGTSL65A-CD-CTRA-TIT-VL
                                       TO AUX-CTRA-1
                                          AUX-CTRA-2
           END-EVALUATE

      *    Filtro por período de data efetiva. se não informado, move
      *    minimo e máximo
           IF  SGTSL65A-DT-EFT-MVTC-INC EQUAL SPACES
               MOVE '01010001'         TO SGTSL65A-DT-EFT-MVTC-INC
           ELSE
      *    Verifica se a data é válida
               MOVE CORR SGTSL65A-DT-EFT-MVTC-INC-R
                                       TO ARG01-F01-R
               MOVE 4                  TO ARG02-F01
               PERFORM 940000-VALIDA-DATA

      *--> Return code = 24 - Data Inválida.
      *                  22 - Conteudo alfa em argumento numerico
               IF  RETURN-CODE EQUAL 24 OR 22
                   SET ERRO-PROGRAMADO TO TRUE
                   MOVE 0394           TO MSG1-CD-ERRO-TS
                   MOVE 'Data de início do período informada inválida.'
                                       TO MSG1-TX-ERRO-TS
                   PERFORM 999000-GRAVA-ERRO-E-ENCERRA
               END-IF
           END-IF

           IF  SGTSL65A-DT-EFT-MVTC-FIM EQUAL SPACES
               MOVE '31129999'         TO SGTSL65A-DT-EFT-MVTC-FIM
           ELSE
      *    Verifica se a data é válida
               MOVE CORR SGTSL65A-DT-EFT-MVTC-FIM-R
                                       TO ARG01-F01-R
               MOVE 4                  TO ARG02-F01
               PERFORM 940000-VALIDA-DATA

      *--> Return code = 24 - Data Inválida.
      *                  22 - Conteudo alfa em argumento numerico
               IF  RETURN-CODE EQUAL 24 OR 22
                   SET ERRO-PROGRAMADO TO TRUE
                   MOVE 0396           TO MSG1-CD-ERRO-TS
                   MOVE 'Data final do período informada inválida'
                                       TO MSG1-TX-ERRO-TS
                   PERFORM 999000-GRAVA-ERRO-E-ENCERRA
               END-IF
           END-IF

      * Verificar se data início é menor ou igual a data fim
           MOVE CORR SGTSL65A-DT-EFT-MVTC-INC-R
                                       TO AUX-DT-INC-AAAAMMDD
           MOVE CORR SGTSL65A-DT-EFT-MVTC-FIM-R
                                       TO AUX-DT-FIM-AAAAMMDD

           IF  AUX-DT-INC-AAAAMMDD > AUX-DT-FIM-AAAAMMDD
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0410              TO MSG1-CD-ERRO-TS
               MOVE 'Data início da pesquisa não pode ser maior que data
      -             ' fim.'            TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           MOVE CORR  SGTSL65A-DT-EFT-MVTC-INC-R
                                       TO AUX-DATA-R
           MOVE AUX-DATA               TO AUX-DT-REF-INI

           MOVE CORR  SGTSL65A-DT-EFT-MVTC-FIM-R
                                       TO AUX-DATA-R
           MOVE AUX-DATA               TO AUX-DT-REF-FIM

           MOVE CORR  SGTSL65A-DT-IMP-MVTC-FNCR-R
                                       TO AUX-DATA-R
           MOVE AUX-DATA               TO AUX-DT-IMP-MVTC

           .
       140000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       200000-PROCESSA-PESQUISA SECTION.
      *-----------------------------------------------------------------
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO

           MOVE CD-TIT-OPR-ETRD(SGTSL65A-IC-TAB)  TO
                     SGTSL65A-CD-TIT-OPR-ETRD-LS(SGTSL65A-IC-TAB)
                     GDA-CD-TIT-OPR-ETRD
           MOVE NR-DVS-TIT(SGTSL65A-IC-TAB)       TO
                     SGTSL65A-NR-DVS-TIT-LS(SGTSL65A-IC-TAB)
                     GDA-NR-DVS-TIT
           MOVE DT-EFT-MVTC(SGTSL65A-IC-TAB)      TO
                     SGTSL65A-DT-EFT-MVTC-LS(SGTSL65A-IC-TAB)
           MOVE CD-TIP-MVTC(SGTSL65A-IC-TAB)      TO
                     SGTSL65A-CD-TIP-MVTC-LS(SGTSL65A-IC-TAB)
           MOVE NM-TIP-MVTC(SGTSL65A-IC-TAB)      TO
                     SGTSL65A-NM-TIP-MVTC-LS(SGTSL65A-IC-TAB)
           MOVE CD-EST-IMP(SGTSL65A-IC-TAB)       TO
                     SGTSL65A-CD-EST-IMP-LS(SGTSL65A-IC-TAB)
           MOVE TX-DCR-ERRO-IMP(SGTSL65A-IC-TAB)  TO
                     SGTSL65A-TX-DCR-ERRO-IMP-LS(SGTSL65A-IC-TAB)
           MOVE NR-IMP-MVTC-FNCR(SGTSL65A-IC-TAB) TO
                     SGTSL65A-NR-IMP-MVTC-FNCR-LS(SGTSL65A-IC-TAB)
                     GDA-NR-IMP-MVTC-FNCR
           MOVE CD-CTRA-TIT-VL(SGTSL65A-IC-TAB)   TO
                     SGTSL65A-CD-CTRA-TIT-VL-LS(SGTSL65A-IC-TAB)
                     GDA-CD-CTRA-TIT-VL

           MOVE ZEROS TO  SGTSL65A-VL-PND-VCLC-DMTV-CX(SGTSL65A-IC-TAB)

      *Calcula o valor Pendente na demonstrativo de caixa apenas
      *se o estado da importação for ZERO E A ORIGEM DO MOVIMENTO
      *for CAC
           IF CD-EST-IMP(SGTSL65A-IC-TAB)  EQUAL ZEROS
           AND CD-TIP-OGM-INF(SGTSL65A-IC-TAB) EQUAL 2
      * EM AJUSTE DBIQ
              PERFORM 210000-CONS-VL-PND-VCLC
              MOVE GDA-VL-PND-VCLC-DMTV-CX TO
                  SGTSL65A-VL-PND-VCLC-DMTV-CX(SGTSL65A-IC-TAB)
      *         MOVE ZEROS TO
      *             SGTSL65A-VL-PND-VCLC-DMTV-CX(SGTSL65A-IC-TAB)
           END-IF

           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       210000-CONS-VL-PND-VCLC       SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

           MOVE ZEROS TO GDA-VL-PND-VCLC-DMTV-CX

           EXEC SQL
                SELECT DISTINCT
                CD_OPR_ETRD_MCP
                INTO
                  :GDA-CD-OPR-ETRD-MCP-1
                FROM DB2SGT.RCBT_RLZD_OPR
                WHERE
                   CD_CTRA_TIT_VL = :GDA-CD-CTRA-TIT-VL
                   AND CD_TIT_OPR_ETRD = :GDA-CD-TIT-OPR-ETRD
                   AND NR_DVS_TIT = :GDA-NR-DVS-TIT
                   FETCH FIRST 1 ROW ONLY
           END-EXEC

           IF  SQLCODE NOT EQUAL ZEROS  AND 100
               SET  ERRO-SQL        TO TRUE
               MOVE 0002              TO MSG1-CD-ERRO-TS
               MOVE SQLCODE            TO GDA-SQLCODE
               STRING 'Erro consulta RCBT_RLZD_OPR - 1;'
                     'SQLCODE=' GDA-SQLCODE
                     DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
               END-STRING
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC SQL
                WITH
                      MAIOR_TS_VERSAO (CD_CTRA_TIT_VL, CD_OPR_ETRD_MCP,
                       NR_VRS_OPR_ETRD, ULT_TS_VRS)
                   AS (
                       SELECT C.CD_CTRA_TIT_VL, C.CD_OPR_ETRD_MCP,
                       C.NR_VRS_OPR_ETRD, MAX(C.TS_EST_VRS)
                       FROM DB2SGT.EST_VRS_OPR_ETRD C
                       WHERE
                       C.CD_CTRA_TIT_VL = :GDA-CD-CTRA-TIT-VL
                       AND C.CD_OPR_ETRD_MCP = :GDA-CD-OPR-ETRD-MCP-1
                       GROUP BY C.CD_CTRA_TIT_VL, C.CD_OPR_ETRD_MCP,
                       C.NR_VRS_OPR_ETRD)
                       SELECT B.CD_OPR_ETRD_MCP
                       , MAX(B.NR_VRS_OPR_ETRD)
                       INTO
                          :GDA-CD-OPR-ETRD-MCP
                          , :GDA-NR-VRS-OPR-ETRD
                        FROM MAIOR_TS_VERSAO B
                        INNER  JOIN
                        DB2SGT.EST_VRS_OPR_ETRD C
                        ON
                            B.CD_CTRA_TIT_VL = C.CD_CTRA_TIT_VL
                            AND B.CD_OPR_ETRD_MCP = C.CD_OPR_ETRD_MCP
                            AND B.NR_VRS_OPR_ETRD = C.NR_VRS_OPR_ETRD
                            AND B.ULT_TS_VRS = C.TS_EST_VRS
                        WHERE
                            C.CD_CTRA_TIT_VL = :GDA-CD-CTRA-TIT-VL
                        AND C.CD_OPR_ETRD_MCP = :GDA-CD-OPR-ETRD-MCP-1
                            AND C.CD_TIP_EST_VRS <> 4
                            AND C.CD_TIP_EST_VRS <> 5
                        GROUP BY B.CD_OPR_ETRD_MCP
                        ORDER BY B.CD_OPR_ETRD_MCP
                        DESC
                        FETCH FIRST 1 ROW ONLY
           END-EXEC

           IF  SQLCODE NOT EQUAL ZEROS  AND 100
               SET  ERRO-SQL        TO TRUE
               MOVE 0002              TO MSG1-CD-ERRO-TS
               MOVE SQLCODE            TO GDA-SQLCODE
               STRING 'Erro consulta RCBT_RLZD_OPR - 2;'
                     'SQLCODE=' GDA-SQLCODE
                     DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
               END-STRING
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SQLCODE EQUAL ZEROS
               PERFORM 210002-CALC-VL-PND
           ELSE
               PERFORM 210001-CONS-VL-DMTV-CX
           END-IF

           .
       210000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       210001-CONS-VL-DMTV-CX       SECTION.
      *-----------------------------------------------------------------

           EXEC SQL
                SELECT
                 COALESCE(VL_EFT_MVTC, 0)
                INTO :GDA-VL-PND-VCLC-DMTV-CX
                FROM
                DB2SGT.MVTC_VCLD_DMTV_CX A
                INNER JOIN
                DB2SGT.DMTV_CX_TIT B
                ON
                A.CD_TIT_OPR_ETRD = B.CD_TIT_OPR_ETRD
                AND A.NR_DVS_TIT = B.NR_DVS_TIT
                AND A.TS_REG_PGTO = B.TS_REG_PGTO
                WHERE
                A.DT_IMP_MVTC_FNCR = :AUX-DT-IMP-MVTC
                AND A.HR_IMP_MVTC_FNCR = :SGTSL65A-HR-IMP-MVTC-FNCR
                AND A.NR_IMP_MVTC_FNCR = :GDA-NR-IMP-MVTC-FNCR
                AND B.CD_CTRA_TIT_VL = :GDA-CD-CTRA-TIT-VL
                FETCH FIRST 1 ROW ONLY
           END-EXEC

           IF  SQLCODE NOT EQUAL ZEROS AND 100
               SET  ERRO-SQL        TO TRUE
               MOVE 0002              TO MSG1-CD-ERRO-TS
               MOVE SQLCODE            TO GDA-SQLCODE
               STRING 'Erro consulta DMTV_CX_TIT - 1;'
                     'SQLCODE=' GDA-SQLCODE
                     DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
               END-STRING
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF SQLCODE EQUAL 100
              MOVE ZEROS TO GDA-VL-PND-VCLC-DMTV-CX
           END-IF

           .
        210001-SAI.
           EXIT.
      *-----------------------------------------------------------------
       210002-CALC-VL-PND       SECTION.
      *-----------------------------------------------------------------
           EXEC SQL
                SELECT COALESCE(MAX(NR_SEQL_RCBT_PRVT), 0)
                INTO :GDA-NR-SEQL-RCBT-PRVT
                FROM
                DB2SGT.RCBT_PRVT_OPR_ETRD
                WHERE
                CD_CTRA_TIT_VL = :GDA-CD-CTRA-TIT-VL
                AND CD_OPR_ETRD_MCP = :GDA-CD-OPR-ETRD-MCP
                AND NR_VRS_OPR_ETRD = :GDA-NR-VRS-OPR-ETRD
           END-EXEC

           IF  SQLCODE NOT EQUAL ZEROS  AND 100
               SET  ERRO-SQL        TO TRUE
               MOVE 0002              TO MSG1-CD-ERRO-TS
               MOVE SQLCODE            TO GDA-SQLCODE
               STRING 'Erro consulta RCBT_RLZD_OPR - 3;'
                     'SQLCODE=' GDA-SQLCODE
                     DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
               END-STRING
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF SQLCODE EQUAL 100 OR GDA-NR-SEQL-RCBT-PRVT  EQUAL ZEROS
              MOVE ZEROS TO GDA-VL-PND-VCLC-DMTV-CX
                            GDA-NR-SEQL-RCBT-PRVT
           END-IF


           EXEC SQL
             SELECT COALESCE(MAX(VL_EFT_MVTC) -
             SUM(COALESCE(VL_RLZD_VCLD, 0)), 0)
             INTO :GDA-VL-PND-VCLC-DMTV-CX
                FROM
                (
                SELECT
                A.CD_TIT_OPR_ETRD
                , A.NR_DVS_TIT
                , A.TS_REG_PGTO
                , COALESCE(B.VL_EFT_MVTC, 0) AS VL_EFT_MVTC
                FROM
                DB2SGT.MVTC_VCLD_DMTV_CX A
                INNER JOIN
                DB2SGT.DMTV_CX_TIT B
                ON
                A.CD_TIT_OPR_ETRD = B.CD_TIT_OPR_ETRD
                AND A.NR_DVS_TIT = B.NR_DVS_TIT
                AND A.TS_REG_PGTO = B.TS_REG_PGTO
                WHERE
                A.DT_IMP_MVTC_FNCR = :AUX-DT-IMP-MVTC
                AND A.HR_IMP_MVTC_FNCR = :SGTSL65A-HR-IMP-MVTC-FNCR
                AND A.NR_IMP_MVTC_FNCR = :GDA-NR-IMP-MVTC-FNCR
                AND B.CD_CTRA_TIT_VL = :GDA-CD-CTRA-TIT-VL
                ) X
                LEFT OUTER JOIN
                DB2SGT.RCBT_RLZD_OPR C
                ON
                X.CD_TIT_OPR_ETRD = C.CD_TIT_OPR_ETRD
                AND X.NR_DVS_TIT = C.NR_DVS_TIT
                AND X.TS_REG_PGTO = C.TS_REG_PGTO
                AND CD_OPR_ETRD_MCP = :GDA-CD-OPR-ETRD-MCP
                AND NR_VRS_OPR_ETRD = :GDA-NR-VRS-OPR-ETRD
                AND NR_SEQL_RCBT_PRVT = :GDA-NR-SEQL-RCBT-PRVT
           END-EXEC

           IF  SQLCODE NOT EQUAL ZEROS   AND 100
               SET  ERRO-SQL        TO TRUE
               MOVE 0002              TO MSG1-CD-ERRO-TS
               MOVE SQLCODE            TO GDA-SQLCODE
               STRING 'Erro consulta DMTV_CX_TIT - 1;'
                    'SQLCODE=' GDA-SQLCODE
                     DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
               END-STRING
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF SQLCODE EQUAL 100
              MOVE ZEROS TO GDA-VL-PND-VCLC-DMTV-CX
           END-IF
           .
       210002-SAI.
           EXIT.

      *----------------------------------------------------------------*
       940000-VALIDA-DATA              SECTION.
      *----------------------------------------------------------------*
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO

           CALL SBDATA USING SBDATA-FUNCAO-F01
                             ARG01-F01
                             ARG02-F01

      *--> Return code = 24 - Data Inválida.
      *                = 22 - Conteudo alfa em argumento numerico

           IF  NOT (RETURN-CODE EQUAL ZEROS OR 24 OR 22)
               SET ERRO-PROGRAMA-ACIONADO TO TRUE
               MOVE 0051               TO MSG1-CD-ERRO-TS
               MOVE RETURN-CODE        TO MSG1-CD-RTN-PGM-ACND
               MOVE 'Erro no tratamento de datas.'
                                       TO MSG1-TX-RTN-PGM-ACND
                                          MSG1-TX-ERRO-TS
               MOVE SBDATA             TO MSG1-NM-PGM-ACND
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       940000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970101-ABRE-CURSOR-PSQ SECTION.
      *-----------------------------------------------------------------
           MOVE 0011                   TO MSG1-CD-SEQL-ERRO
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
           MOVE 0012                   TO MSG1-CD-SEQL-ERRO
           INITIALIZE REG-WS
           EXEC SQL
                FETCH NEXT ROWSET
                      CURSOR-PSQ FOR 0101 ROWS
               INTO :CD-TIT-OPR-ETRD
                    :CD-TIT-OPR-ETRD-N
                  , :NR-DVS-TIT
                    :NR-DVS-TIT-N
                  , :DT-EFT-MVTC
                    :DT-EFT-MVTC-N
                  , :CD-TIP-MVTC
                    :CD-TIP-MVTC-N
                  , :NM-TIP-MVTC
                    :NM-TIP-MVTC-N
                  , :CD-EST-IMP
                  , :TX-DCR-ERRO-IMP
                    :TX-DCR-ERRO-IMP-N
                  , :NR-IMP-MVTC-FNCR
                    :NR-IMP-MVTC-FNCR-N
                  , :CD-CTRA-TIT-VL
                    :CD-CTRA-TIT-VL-N
                  , :CD-TIP-OGM-INF
           END-EXEC
           MOVE SQLERRD(3)             TO QT-REG-SELD
           IF  QT-REG-SELD EQUAL 0101
               SET SGTSL65A-TEM-MAIS-REGISTRO TO TRUE
               MOVE 0100               TO QT-REG-SELD
           END-IF
           MOVE QT-REG-SELD            TO SGTSL65A-QT-OCR-TAB
           MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ
           .
       970102-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970103-FECHA-CURSOR-PSQ SECTION.
      *-----------------------------------------------------------------
           MOVE 0013                   TO MSG1-CD-SEQL-ERRO
           MOVE 'N'                    TO IN-CURSOR-PSQ-ABTO
           EXEC SQL
                CLOSE CURSOR-PSQ
           END-EXEC
           .
       970103-SAI.
           EXIT.
      *-----------------------------------------------------------------
       300000-POSTA-RESPOSTA SECTION.
      *-----------------------------------------------------------------
           MOVE 0014                   TO MSG1-CD-SEQL-ERRO

           SET PUT-CONTAINER           TO TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *--> previsto o erro de container
           SET  ERRO-CONTAINER         TO TRUE

           EXEC CICS PUT
              CONTAINER ( 'SGTSL65A-RPST' )
              CHANNEL   ( NM-CNL      )
              FLENGTH ( LENGTH OF SGTSL65A-RPST )
              FROM    ( SGTSL65A-RPST )
           END-EXEC
           .
       300000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA SECTION.
      *-----------------------------------------------------------------

           IF  MSG1-CD-ERRO-TS EQUAL ZEROS
              MOVE 8888                TO MSG1-CD-ERRO-TS
           END-IF

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

           MOVE 'SGTSL65A-DT-IMP-MVTC-FNCR'
                                       TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL65A-DT-IMP-MVTC-FNCR
                                       TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL65A-HR-IMP-MVTC-FNCR'
                                       TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL65A-HR-IMP-MVTC-FNCR
                                       TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL65A-CD-TIT-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL65A-CD-TIT-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL65A-NR-DVS-TIT'  TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL65A-NR-DVS-TIT    TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL65A-DT-EFT-MVTC-INC'
                                       TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL65A-DT-EFT-MVTC-INC
                                       TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL65A-DT-EFT-MVTC-FIM'
                                       TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL65A-DT-EFT-MVTC-FIM
                                       TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL65A-CD-EST-IMP'
                                       TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL65A-CD-EST-IMP
                                       TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL65A-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL65A-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL65A-NR-IMP-MVTC-FNCR-PAG'
                                       TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL65A-NR-IMP-MVTC-FNCR-PAG
                                       TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

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
       999000-SAI.
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
           END-EXEC.