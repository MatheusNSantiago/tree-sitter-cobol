      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSL503
      * ANALISTA..: F2054109 Cintia Ap Soares Iglesias
      * AUTOR.....: F0950413 António Francisco Mendonça Silva
      * DATA......: 19/04/2017
      * OBJETIVO..: Listar Recebimentos Previstos da Operação Estrut.
      * COMPILACAO: 54 - PSOSE600 - Cobol 6.3 c/otimizacao p/producao
      *-----------------------------------------------------------------
      * VRS004 20.06.2024 F9458161 Valida parm entrada TIT e DVS
      *                   F2054109 Tratamento CD_TIP_OGM_INF = 2
      * VRS003 04.12.2019 F2804421 DE 2016/145195 - Incluir condição pa-
      *                   CD_TIP_LCTO_CTB = 2 (crédito).
      * VRS002 24.04.2019 - F2634012 - DE 2016/145195 - Selecionar
      *        apenas os registros da tabela dmtv_cx_tit que tenham
      *        o campo in_mvtc_atv = 's'
      * VRS001 19.04.2017 - F0950413 - 2015/137233 - Implantação
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTSL503.

      *-----------------------------------------------------------------
       ENVIRONMENT                     DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       CONFIGURATION                   SECTION.
      *-----------------------------------------------------------------
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *-----------------------------------------------------------------
       DATA                            DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       WORKING-STORAGE                 SECTION.
      *-----------------------------------------------------------------
       77  CTE-INICIO-SS               PIC  X(40) VALUE
           '*** Storage Section Comeca Aqui ***'.
       77  CTE-PROG                    PIC  X(17)
                                       VALUE '*** SGTSL503 ***'.
       77  CTE-VERS                    PIC  X(06)
                                       VALUE 'VRS004'.

      *-----------------------------------------------------------------
      *--> Book do programa de ambiente - CICS5000
      *    Definido na work para preservar as informacoes
-INC HLPKE01B
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

      *--> Area de trabalho
       01  IC-TAB                      PIC S9(04) COMP-5 VALUE ZEROS.
       01  QT-REG-SELD                 PIC S9(04)      COMP-5 VALUE 0.

      *--> Auxiliares para movimentação na monitoração de variáveis.
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).

      *--> Campos utilizados para leitura
       01  REG-WS.
           03 CD-TIP-RCBT-PRVT         PIC S9(4) COMP-5
           OCCURS 101 TIMES            VALUE 0.
           03 DT-RCBT-PRVT             PIC X(8)
           OCCURS 101 TIMES            VALUE ' '.
           03 PC-RCBT-PRVT             PIC S9(04)V9(4) COMP-3
           OCCURS 101 TIMES            VALUE ZEROS.
           03 PC-RCBT-PRVT-N           PIC S9(4) COMP-5
           OCCURS 101 TIMES            VALUE ZEROS.
           03 VL-RCBT-PRVT             PIC S9(15)V9(2) COMP-3
           OCCURS 101 TIMES            VALUE ZEROS.
           03 VL-RCBT-PRVT-N           PIC S9(4) COMP-5
           OCCURS 101 TIMES            VALUE ZEROS.
           03 DT-EFTC-MVTC             PIC X(8)
           OCCURS 101 TIMES            VALUE ' '.
           03 DT-EFTC-MVTC-N           PIC S9(4) COMP-5
           OCCURS 101 TIMES            VALUE ZEROS.
           03 VL-RLZD-VCLD             PIC S9(15)V9(2) COMP-3
           OCCURS 101 TIMES            VALUE ZEROS.
           03 VL-RLZD-VCLD-N           PIC S9(4) COMP-5
           OCCURS 101 TIMES            VALUE ZEROS.

      *--> Indicadores de nulidades
       01 QT-REG-SELD-N                PIC S9(4) COMP-5 VALUE 0.

      *--> Variavel(is) de controle do cursor(es)
       01  CTL-CURSOR-PSQ.
           03  CD-FIM-CURSOR-PSQ       PIC S9(04) COMP-5 VALUE 0.
               88  FIM-CURSOR-PSQ                        VALUE +100.
           03  IN-CURSOR-PSQ-ABTO      PIC  X(01) VALUE 'N'.
               88  CURSOR-PSQ-ABTO                VALUE 'S'.

      *--> Variável para forçar zero no CD_TIP_RCBT_PRVT do where do
      *--> subselect.
       01  K-CD-TIP-RCBT-PRVT          PIC S9(04) COMP-5 VALUE 0.
       01  K-TS-REG-PGTO               PIC X(26)
                                     VALUE '0000-00-00-00.00.00.000000'.

      *-----------------------------------------------------------------
      *    Books Utilizados
      *-----------------------------------------------------------------
      *--> Variáveis para tratamento de CONTAINER
-INC SGTKERR1

      *--> Book do Container de Erro
-INC IIBKERRO


      *--> Book para captura de erro via MSG1
-INC MSGKWMG1

      *--> Book para captura de abend cics da bloco codigo MSGKP999
-INC HLPKW999

      *--> Book do container de requisicao e resposta deste modulo
-INC SGTKL503

      *-----------------------------------------------------------------
      *    Cursores de Pesquisa
      *-----------------------------------------------------------------

      *--> Cursor de Pesquisa
           EXEC SQL
             DECLARE CURSOR-PSQ CURSOR
                     WITH ROWSET POSITIONING FOR

             SELECT A.CD_TIP_RCBT_PRVT
                  , CHAR(TO_CHAR(A.DT_RCBT_PRVT, 'DDMMYYYY'), 8)
                  , A.PC_RCBT_PRVT
                  , A.VL_RCBT_PRVT
                  , DECIMAL(SUM(VALUE(D.VL_RLZD_VCLD, 0)), 17, 2)
                                   AS SOMA_REALIZACAO
                  , TO_CHAR(MAX(VALUE(D.DT_EFT_MVTC, '0001-01-01')),
                      'DDMMYYYY')  AS DATA_REALIZACAO
             FROM
                 DB2SGT.RCBT_PRVT_OPR_ETRD  A
               LEFT OUTER JOIN
                 (SELECT
                   B.CD_CTRA_TIT_VL
                   , B.CD_OPR_ETRD_MCP
                   , B.NR_VRS_OPR_ETRD
                   , B.NR_SEQL_RCBT_PRVT
                   , B.CD_TIP_RCBT_PRVT
                   , B.DT_RCBT_PRVT
                   , B.VL_RLZD_VCLD
                   , C.DT_EFT_MVTC
                  FROM
                                DB2SGT.RCBT_RLZD_OPR  B
                    INNER JOIN  DB2SGT.DMTV_CX_TIT    C
                    ON
                      B.CD_TIT_OPR_ETRD  = C.CD_TIT_OPR_ETRD  AND
                      B.NR_DVS_TIT   = C.NR_DVS_TIT           AND
                      B.TS_REG_PGTO  = C.TS_REG_PGTO
                  WHERE
                   B.CD_CTRA_TIT_VL    = :SGTSL503-CD-CTRA-TIT-VL    AND
                   B.CD_OPR_ETRD_MCP   = :SGTSL503-CD-OPR-ETRD-MCP   AND
                   B.NR_VRS_OPR_ETRD   = :SGTSL503-NR-VRS-OPR-ETRD   AND
                   B.NR_SEQL_RCBT_PRVT = :SGTSL503-NR-SEQL-RCBT-PRVT AND
                   B.CD_TIT_OPR_ETRD   = :SGTSL503-CD-TIT-OPR-ETRD   AND
                   B.NR_DVS_TIT        = :SGTSL503-NR-DVS-TIT        AND
                   B.CD_TIP_RCBT_PRVT  > :K-CD-TIP-RCBT-PRVT         AND
                   C.TS_REG_PGTO       > :K-TS-REG-PGTO              AND
                   C.IN_MVTC_ATV       = 'S'                         AND
                   (C.CD_TIP_LCTO_CTB =   2
                     OR C.CD_TIP_OGM_INF = 2)
                   ) D
               ON
                   A.CD_CTRA_TIT_VL    = D.CD_CTRA_TIT_VL     AND
                   A.CD_OPR_ETRD_MCP   = D.CD_OPR_ETRD_MCP    AND
                   A.NR_VRS_OPR_ETRD   = D.NR_VRS_OPR_ETRD    AND
                   A.NR_SEQL_RCBT_PRVT = D.NR_SEQL_RCBT_PRVT  AND
                   A.CD_TIP_RCBT_PRVT  = D.CD_TIP_RCBT_PRVT   AND
                   A.DT_RCBT_PRVT      = D.DT_RCBT_PRVT
             WHERE
                   A.CD_CTRA_TIT_VL    = :SGTSL503-CD-CTRA-TIT-VL    AND
                   A.CD_OPR_ETRD_MCP   = :SGTSL503-CD-OPR-ETRD-MCP   AND
                   A.NR_VRS_OPR_ETRD   = :SGTSL503-NR-VRS-OPR-ETRD   AND
                   A.NR_SEQL_RCBT_PRVT = :SGTSL503-NR-SEQL-RCBT-PRVT AND
                   (A.DT_RCBT_PRVT
                         > TO_CHAR(TO_DATE((:SGTSL503-DT-RCBT-PRVT-ULT),
                         'DDMMYYYY'), 'DD.MM.YYYY')        OR
                     ( A.DT_RCBT_PRVT
                         = TO_CHAR(TO_DATE((:SGTSL503-DT-RCBT-PRVT-ULT),
                         'DDMMYYYY'), 'DD.MM.YYYY')     AND
                       A.CD_TIP_RCBT_PRVT
                         >  :SGTSL503-CD-TIP-RCBT-PRVT-ULT))
             GROUP BY
                   A.CD_CTRA_TIT_VL
                 , A.CD_OPR_ETRD_MCP
                 , A.NR_VRS_OPR_ETRD
                 , A.NR_SEQL_RCBT_PRVT
                 , A.DT_RCBT_PRVT
                 , A.CD_TIP_RCBT_PRVT
                 , A.PC_RCBT_PRVT
                 , A.VL_RCBT_PRVT
             ORDER BY
                   A.DT_RCBT_PRVT
                 , A.CD_TIP_RCBT_PRVT

           END-EXEC.

      *--> Parâmetros para a utilizacao do DB2
           EXEC SQL
                     INCLUDE SQLCA
           END-EXEC.

       01  CTE-FINAL-SS                PIC  X(40) VALUE
           '*** Storage Section Termina Aqui ***'.

      *-----------------------------------------------------------------
       PROCEDURE                       DIVISION.
      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------
           MOVE  0001                  TO  MSG1-CD-SEQL-ERRO

      *--> Habilita tratamento automatico de erros DB2
-INC HLPKE997
      *--> Habilita captura automatica de abend e erros CICS
-INC HLPKE999

           PERFORM  100000-DESCOBRIR-CANAL
           PERFORM  110000-EXCLUIR-CTNR-RPST
           PERFORM  120000-EXCLUIR-CTNR-ERRO
           PERFORM  130000-RECEBER-REQUISICAO

           PERFORM  140000-VALIDAR-PARAMETRO

           PERFORM  970101-ABRE-CURSOR-PSQ
           PERFORM  970102-LER-CURSOR-PSQ

           PERFORM  200000-PROCESSA-PESQUISA
                    VARYING                IC-TAB
                                           FROM 0001 BY 0001
                    UNTIL                  IC-TAB
                                           GREATER SGTSL503-QT-OCR-TAB

           PERFORM  970103-FECHA-CURSOR-PSQ

           PERFORM  300000-POSTA-RESPOSTA
           PERFORM  999999-ENCERRAR

           GOBACK
           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-DESCOBRIR-CANAL          SECTION.
      *-----------------------------------------------------------------
           MOVE  0002                  TO  MSG1-CD-SEQL-ERRO

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CANAL            TO  TRUE

           EXEC CICS  ASSIGN
                      CHANNEL ( NM-CNL )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       100000-SAI.
           EXIT.

      *---------------------------------------------------------------
       110000-EXCLUIR-CTNR-RPST        SECTION.
      *---------------------------------------------------------------
           MOVE  0003                  TO  MSG1-CD-SEQL-ERRO

           EXEC CICS  DELETE
                      CONTAINER ( 'SGTSL503-RPST' )
                      CHANNEL   ( NM-CNL          )
                      NOHANDLE
           END-EXEC

           IF  NOT (EIBRESP                EQUAL  DFHRESP(NORMAL)  OR
                    EIBRESP                EQUAL  DFHRESP(CONTAINERERR))
               PERFORM  999002-TRATA-CONDITION-CICS
           END-IF
           .
       110000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       120000-EXCLUIR-CTNR-ERRO        SECTION.
      *-----------------------------------------------------------------
           MOVE  0004                  TO  MSG1-CD-SEQL-ERRO

           EXEC CICS  DELETE
                      CONTAINER ( 'PRM-ERRO' )
                      CHANNEL   ( NM-CNL     )
                      NOHANDLE
           END-EXEC
           IF  NOT (EIBRESP                EQUAL  DFHRESP(NORMAL)   OR
                    EIBRESP                EQUAL  DFHRESP(CONTAINERERR))
               PERFORM  999002-TRATA-CONDITION-CICS
           END-IF

           .
       120000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       130000-RECEBER-REQUISICAO       SECTION.
      *-----------------------------------------------------------------
           MOVE  0005                  TO  MSG1-CD-SEQL-ERRO

           MOVE  ZEROS                     TO  QT-CCT-LIDO
           MOVE  LENGTH OF  SGTSL503-RQSC  TO  QT-CCT-EPRO
           EXEC CICS  GET
                      CONTAINER ( 'SGTSL503-RQSC' )
                      CHANNEL   ( NM-CNL          )
                      NODATA
                      FLENGTH   ( QT-CCT-LIDO     )
           END-EXEC

           IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
               SET   ERRO-TAMANHO-AREA   TO  TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS  GET
                      CONTAINER ( 'SGTSL503-RQSC'         )
                      CHANNEL   ( NM-CNL                  )
                      FLENGTH   ( LENGTH OF SGTSL503-RQSC )
                      INTO      ( SGTSL503-RQSC           )
           END-EXEC
           .
       130000-SAI.
           EXIT.

      *--------------------------------------------------------------
       140000-VALIDAR-PARAMETRO        SECTION.
      *--------------------------------------------------------------
           MOVE  0006                  TO  MSG1-CD-SEQL-ERRO

           MOVE  'N'                   TO  SGTSL503-IN-CNTC-PSQ

           IF  SGTSL503-CD-CTRA-TIT-VL     LESS ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0001              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' inválido.'      TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL503-CD-CTRA-TIT-VL     EQUAL ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0002              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' não informado.' TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL503-CD-OPR-ETRD-MCP    NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0003              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada mercado de capitais nã
      -              'o numérico.'     TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL503-CD-OPR-ETRD-MCP    LESS  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0004              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada mercado de capitais in
      -              'válido.'         TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL503-CD-OPR-ETRD-MCP    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0005              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada mercado de capitais nã
      -              'o informado.'    TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL503-NR-VRS-OPR-ETRD    LESS  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0006              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação estruturada inválido.
      -              ''                TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL503-NR-VRS-OPR-ETRD    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0007              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL503-NR-SEQL-RCBT-PRVT  < ZEROS
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE  0108              TO  MSG1-CD-ERRO-TS
               MOVE  'Número sequencial do recebimento previsto inválido
      -              '.'               TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL503-DT-RCBT-PRVT-ULT   EQUAL SPACES  OR
                                                 '00000000'
               MOVE  '01010001'        TO  SGTSL503-DT-RCBT-PRVT-ULT
           END-IF

           MOVE  0                     TO  K-CD-TIP-RCBT-PRVT
           MOVE  '1900-01-01-01.00.00.000000'
                                       TO  K-TS-REG-PGTO

           IF  SGTSL503-CD-TIT-OPR-ETRD    EQUAL SPACES
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0231              TO  MSG1-CD-ERRO-TS
               MOVE  'Código do título da op. estr. não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL503-NR-DVS-TIT         <= 0
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0013              TO  MSG1-CD-ERRO-TS
               MOVE  'Número divisão do título inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-PROCESSA-PESQUISA        SECTION.
      *-----------------------------------------------------------------
           MOVE  0007                  TO  MSG1-CD-SEQL-ERRO

           MOVE  DT-RCBT-PRVT                                   (IC-TAB)
             TO  SGTSL503-DT-RCBT-PRVT                          (IC-TAB)
           MOVE  CD-TIP-RCBT-PRVT                               (IC-TAB)
             TO  SGTSL503-CD-TIP-RCBT-PRVT                      (IC-TAB)
           MOVE  PC-RCBT-PRVT                                   (IC-TAB)
             TO  SGTSL503-PC-RCBT-PRVT                          (IC-TAB)
           MOVE  VL-RCBT-PRVT                                   (IC-TAB)
             TO  SGTSL503-VL-RCBT-PRVT                          (IC-TAB)

           IF DT-EFTC-MVTC(IC-TAB) EQUAL '01010001'
              MOVE SPACES          TO    DT-EFTC-MVTC(IC-TAB)
           END-IF

           MOVE  DT-EFTC-MVTC                                   (IC-TAB)
             TO  SGTSL503-DT-EFTC-MVTC                          (IC-TAB)
           MOVE  VL-RLZD-VCLD                                   (IC-TAB)
             TO  SGTSL503-VL-RLZD-VCLD                          (IC-TAB)
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-POSTA-RESPOSTA           SECTION.
      *-----------------------------------------------------------------
           MOVE  0008                  TO  MSG1-CD-SEQL-ERRO

           EXEC CICS  PUT
                      CONTAINER ( 'SGTSL503-RPST'         )
                      CHANNEL   ( NM-CNL                  )
                      FLENGTH   ( LENGTH OF SGTSL503-RPST )
                      FROM      ( SGTSL503-RPST           )
           END-EXEC
           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970101-ABRE-CURSOR-PSQ          SECTION.
      *-----------------------------------------------------------------
           MOVE  0009                  TO  MSG1-CD-SEQL-ERRO

           EXEC SQL
                OPEN CURSOR-PSQ
           END-EXEC

           MOVE  'S'                   TO  IN-CURSOR-PSQ-ABTO
           .
       970101-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970102-LER-CURSOR-PSQ           SECTION.
      *-----------------------------------------------------------------
           MOVE  0010                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE REG-WS

           EXEC SQL
                FETCH NEXT ROWSET
                     CURSOR-PSQ   FOR 101 ROWS
                INTO :CD-TIP-RCBT-PRVT
                   , :DT-RCBT-PRVT
                   , :PC-RCBT-PRVT
                     :PC-RCBT-PRVT-N
                   , :VL-RCBT-PRVT
                     :VL-RCBT-PRVT-N
                   , :VL-RLZD-VCLD
                     :VL-RLZD-VCLD-N
                   , :DT-EFTC-MVTC
                     :DT-EFTC-MVTC-N
           END-EXEC

           MOVE  SQLERRD(3)            TO  QT-REG-SELD
           IF  QT-REG-SELD                 EQUAL  0101
               SET   SGTSL503-TEM-MAIS-REGISTRO
                                       TO  TRUE
               MOVE  0100              TO  QT-REG-SELD
           END-IF

           MOVE  QT-REG-SELD           TO  SGTSL503-QT-OCR-TAB
           MOVE  SQLCODE               TO  CD-FIM-CURSOR-PSQ
           .
       970102-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970103-FECHA-CURSOR-PSQ         SECTION.
      *-----------------------------------------------------------------
           MOVE  0011                  TO  MSG1-CD-SEQL-ERRO

           MOVE  'N'                   TO  IN-CURSOR-PSQ-ABTO

           EXEC SQL
                CLOSE CURSOR-PSQ
           END-EXEC
           .
       970103-SAI.
           EXIT.

      *--------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *--------------------------------------------------------------
           IF  IN-CURSOR-PSQ-ABTO  EQUAL  'S'
               PERFORM 970103-FECHA-CURSOR-PSQ
           END-IF

           IF  MSG1-CD-ERRO-TS             EQUAL ZEROS
               MOVE  8888              TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS             EQUAL SPACES
               MOVE  'Erro não especificado no módulo.'
                                       TO  MSG1-TX-ERRO-TS
           END-IF

      *--> Verifica se o tipo de erro gerado possui tratamento interno
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

           MOVE  'SGTSL503-CD-CTRA-TIT-VL'     TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL503-CD-CTRA-TIT-VL       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSL503-CD-OPR-ETRD-MCP'    TO  MSGKW999-NM-VRV-MNT
      *------> Verifica se o conteudo e numerico antes de adicionar
           IF  SGTSL503-CD-OPR-ETRD-MCP    IS NUMERIC
               MOVE  SGTSL503-CD-OPR-ETRD-MCP  TO  CD-OPR-ETRD-MCP
               MOVE  CD-OPR-ETRD-MCP-TX        TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico.'     TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSL503-NR-VRS-OPR-ETRD'    TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL503-NR-VRS-OPR-ETRD      TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSL503-NR-SEQL-RCBT-PRVT'  TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL503-NR-SEQL-RCBT-PRVT    TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSL503-CD-TIP-RCBT-PRVT'   TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL503-CD-TIP-RCBT-PRVT-ULT TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSL503-DT-RCBT-PRVT'       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL503-DT-RCBT-PRVT-ULT     TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

      *--> Grava o erro no msg
           PERFORM  MSG1-NTF-MSG

           MOVE  MSG1-CD-ERRO-TS       TO  IIBKERRO-CD-ERRO
           MOVE  MSG1-TX-ERRO-TS       TO  IIBKERRO-TX-ERRO

           EXEC CICS  PUT
                      CONTAINER ( 'PRM-ERRO'                  )
                      CHANNEL   ( NM-CNL                      )
                      FROM      ( IIBKERRO-PRM-ERRO           )
                      FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                      NOHANDLE
           END-EXEC

           PERFORM 999999-ENCERRAR
           .
       999000-SAI.
           EXIT.

      * ---------------------------------------
      * 999001-TRATA-ABEND-CICS        SECTION.
      * 999002-TRATA-CONDITION-CICS    SECTION.
      * 999003-ADICIONA-VRV-MNT        SECTION.
      * 999004-ALINHA-NUMERO-ESQUERDA  SECTION.
      * ---------------------------------------
-INC HLPKP999
      *----------------------------------------
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *----------------------------------------
-INC SGTKP999
      * ---------------------------------------
      * 999010-TRATA-ERRO-DB2          SECTION.
      * ---------------------------------------
-INC HLPKP997
      * ---------------------------------------
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
      * ---------------------------------------
-INC MSGKPMG1
      * ---------------------------------------
       999999-ENCERRAR                 SECTION.
      * ---------------------------------------
           EXEC CICS
                RETURN
           END-EXEC
           .
       999999-SAI.
           EXIT.
      *-----------------------------------------------------------------