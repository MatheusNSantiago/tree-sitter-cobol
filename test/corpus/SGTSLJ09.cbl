      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSLJ09
      * ANALISTA..: F8608799 - Ricardo Ramos
      * AUTOR.....: F8608799 - Ricardo Ramos
      * OBJETIVO..: CONSULTA TABELA DB2SGT.CLI_OPR_ETRD E CLI_GRTR
      * COMPILACAO: COBOL/CICS/DB2
      *----------------------------------------------------------------
      * VRS001 31.01.2019 - F8608799 2016/145195 - Inclusão dos campos
      *           SGTSLJ09-CD-OPR-ETRD-MCP  e  SGTSLJ09-NR-VRS-OPR-ETRD
      *           no SQL.
      *        22.05.2018 - F2804421 DE 2016/145195
      *                   - Aceitar receber carteira com zeros para
      *                    listar todas as carteiras.
      *        11.05.2017 - F8608799 2016/145195 - IMPLANTACAO
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSLJ09.

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
       77  CTE-INICIO-SS               PIC  X(40)  VALUE
                   '*** S.S. COMEÇA AQUI ***'.
       77  CTE-PROG     PIC  X(17) VALUE '*** SGTSLJ09 ***'.
       77  CTE-VERS     PIC  X(06) VALUE 'VRS001'.

      *-----------------------------------------------------------------
      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informacoes
-INC HLPKE01B
      *-----------------------------------------------------------------
       LOCAL-STORAGE SECTION.
      *-----------------------------------------------------------------
      * book de para acionamento do msg1
-INC IIBKERRO
-INC MSGKWMG1
-INC HLPKW999

      *--> Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1

      * Area de trabalho

       01 QT-REG-SELD                       PIC S9(4) COMP-5 VALUE 0.
       01 CD-TIP-FUC-CLI-INI                PIC S9(4) COMP-5 VALUE 0.
       01 CD-TIP-FUC-CLI-FIM                PIC S9(4) COMP-5 VALUE 0.
       01 CD-CTRA-TIT-VL-INI                PIC S9(9) COMP-5 VALUE 0.
       01 CD-CTRA-TIT-VL-FIM                PIC S9(9) COMP-5 VALUE 0.
       01 CD-OPR-ETRD-MCP-INI               PIC S9(11) COMP-3 VALUE 0.
       01 CD-OPR-ETRD-MCP-FIM               PIC S9(11) COMP-3 VALUE 0.
       01 NR-VRS-OPR-ETRD-INI               PIC S9(4) COMP-5 VALUE 0.
       01 NR-VRS-OPR-ETRD-FIM               PIC S9(4) COMP-5 VALUE 0.
       01 AUX-CTRA-RCPO-CLI-OPR             PIC S9(9) COMP-5 VALUE 0.
       01 AUX-CTRA-RCPO-CLI-GRTR            PIC S9(9) COMP-5 VALUE 0.

      * Campos utilizados para leitura

       01  REG-WS.
           03 CD-CLI                         PIC S9(9) COMP-5
              OCCURS 101 TIMES               VALUE 0.

      * Indicadores de nulidades

      * Variavel(is) de controle do cursor(es)

       01  CTL-CURSOR-PSQ.
           03  CD-FIM-CURSOR-PSQ            PIC S9(04) COMP-5 VALUE 0.
               88  FIM-CURSOR-PSQ            VALUE +100.
           03  IN-CURSOR-PSQ-ABTO           PIC  X(01) VALUE 'N'.
               88  CURSOR-PSQ-ABTO          VALUE 'S'.

      * Subrotinas utilizadas

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

      *-----------------------------------------------------------------
-INC SGTKLJ09
           EXEC SQL
             DECLARE CURSOR-PSQ CURSOR
                     WITH ROWSET POSITIONING FOR
             WITH
                 CLIENTES (COD_CLI)
             AS (
                 SELECT
                     DISTINCT(A.CD_CLI)
                 FROM
                     DB2SGT.CLI_OPR_ETRD A
                 WHERE
                     A.CD_CTRA_TIT_VL >= :CD-CTRA-TIT-VL-INI
                 AND A.CD_CTRA_TIT_VL <= :CD-CTRA-TIT-VL-FIM
                 AND A.CD_CTRA_TIT_VL > :AUX-CTRA-RCPO-CLI-OPR -- SE
                                         -- 0, CONSIDERA ESSA QUERY
                                         -- 2147483647 PARA IGNORA-LA
                 AND A.CD_OPR_ETRD_MCP >= :CD-OPR-ETRD-MCP-INI
                 AND A.CD_OPR_ETRD_MCP <= :CD-OPR-ETRD-MCP-FIM
                 AND A.NR_VRS_OPR_ETRD >= :NR-VRS-OPR-ETRD-INI
                 AND A.NR_VRS_OPR_ETRD <= :NR-VRS-OPR-ETRD-FIM
                 AND A.CD_TIP_FUC_CLI  >= :CD-TIP-FUC-CLI-INI
                 AND A.CD_TIP_FUC_CLI  <= :CD-TIP-FUC-CLI-FIM
                 -- PARA PAGINAÇÃO
                 AND A.CD_CLI           > :SGTSLJ09-CD-CLI-ULT
                 ORDER BY
                     A.CD_CLI ASC
                 ),
                 CLIENTE_GARANTIDOR (COD_CLI)
             AS  (
                 SELECT
                     DISTINCT(A.CD_CLI)
                 FROM
                     DB2SGT.CLI_GRTR A
                 WHERE
                     A.CD_CTRA_TIT_VL >= :CD-CTRA-TIT-VL-INI
                 AND A.CD_CTRA_TIT_VL <= :CD-CTRA-TIT-VL-FIM
                 AND A.CD_CTRA_TIT_VL > :AUX-CTRA-RCPO-CLI-GRTR --SE
                                         -- 0, CONSIDERA ESSA QUERY
                                         -- 2147483647 PARA IGNORA-LA
                 -- PARA PAGINAÇÃO
                 AND A.CD_OPR_ETRD_MCP >= :CD-OPR-ETRD-MCP-INI
                 AND A.CD_OPR_ETRD_MCP <= :CD-OPR-ETRD-MCP-FIM
                 AND A.NR_VRS_OPR_ETRD >= :NR-VRS-OPR-ETRD-INI
                 AND A.NR_VRS_OPR_ETRD <= :NR-VRS-OPR-ETRD-FIM
                 AND A.CD_CLI           > :SGTSLJ09-CD-CLI-ULT
                 ORDER BY
                 A.CD_CLI ASC
             )
             SELECT
                 VALUE(A.COD_CLI, B.COD_CLI) AS CLIENTE
             FROM
                 CLIENTES A
             FULL OUTER JOIN
                 CLIENTE_GARANTIDOR B
             ON
                 A.COD_CLI = B.COD_CLI
             WHERE
             -- CAMPOS PARA PAGINAÇÃO
                 A.COD_CLI > :SGTSLJ09-CD-CLI-ULT
             OR  B.COD_CLI > :SGTSLJ09-CD-CLI-ULT
             ORDER BY
                 CLIENTE ASC
           END-EXEC.

       01  CTE-FINAL-SS                PIC  X(40)      VALUE
                    '*** S.S. TERMINA AQUI ***'.
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
                    VARYING SGTSLJ09-IC-TAB FROM 1 BY 1
                    UNTIL SGTSLJ09-IC-TAB > SGTSLJ09-QT-OCR-TAB

           PERFORM 300000-POSTA-RESPOSTA
           GOBACK
           .

      *-----------------------------------------------------------------
       100000-DESCOBRIR-CANAL SECTION.
      *-----------------------------------------------------------------
           MOVE 0001                   TO MSG1-CD-SEQL-ERRO

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CANAL            TO  TRUE

           EXEC CICS ASSIGN
              CHANNEL ( NM-CNL )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-EXCLUIR-CTNR-RPST SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS DELETE
              CONTAINER ( 'SGTSLJ09-RPST' )
              CHANNEL   ( NM-CNL          )
              NOHANDLE
           END-EXEC

           IF  NOT (EIBRESP EQUAL DFHRESP(NORMAL)   OR
               EIBRESP EQUAL DFHRESP(CONTAINERERR) )
               PERFORM 999002-TRATA-CONDITION-CICS
           END-IF
           .
       110000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       120000-EXCLUIR-CTNR-ERRO SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS DELETE
              CONTAINER ( 'PRM-ERRO' )
              CHANNEL   ( NM-CNL     )
              NOHANDLE
           END-EXEC

           IF  NOT (EIBRESP EQUAL DFHRESP(NORMAL)   OR
               EIBRESP EQUAL DFHRESP(CONTAINERERR) )
               PERFORM 999002-TRATA-CONDITION-CICS
           END-IF
           .
       120000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       130000-RECEBER-REQUISICAO SECTION.
      *-----------------------------------------------------------------
           MOVE 0004                   TO MSG1-CD-SEQL-ERRO

           SET  GET-CONTAINER          TO  TRUE
           SET  ERRO-CONTAINER         TO  TRUE
           MOVE 'SGTSLJ09-RQSC'        TO  NM-CTNR

           MOVE ZEROS                        TO QT-CCT-LIDO
           MOVE LENGTH OF SGTSLJ09-RQSC      TO QT-CCT-EPRO

           EXEC CICS GET
              CONTAINER ( NM-CTNR     )
              CHANNEL   ( NM-CNL      )
              NODATA
              FLENGTH   ( QT-CCT-LIDO )
           END-EXEC

           IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
               SET  ERRO-TAMANHO-AREA    TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS GET
              CONTAINER ( NM-CTNR       )
              CHANNEL   ( NM-CNL        )
              FLENGTH   ( QT-CCT-LIDO   )
              INTO      ( SGTSLJ09-RQSC )
           END-EXEC

           SET  SEM-ERRO               TO  TRUE
           .
       130000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       140000-VALIDAR-PARAMETRO SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

           MOVE 'N'                    TO SGTSLJ09-IN-CNTO-PSQ

           IF  SGTSLJ09-CD-CTRA-TIT-VL < ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0001               TO MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de título e valores mobiliários
      -             'inválido.'        TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSLJ09-CD-CTRA-TIT-VL EQUAL ZEROS
      ***    Igual a zeros traz lista com todas as carteiras
      ***    move 2147483647 (valor máximo para Integer)
               MOVE ZEROS          TO  CD-CTRA-TIT-VL-INI
               MOVE 2147483647     TO  CD-CTRA-TIT-VL-FIM
           ELSE
               MOVE SGTSLJ09-CD-CTRA-TIT-VL
                                   TO  CD-CTRA-TIT-VL-INI
                                       CD-CTRA-TIT-VL-FIM
           END-IF

           IF  SGTSLJ09-CD-OPR-ETRD-MCP < ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0004               TO MSG1-CD-ERRO-TS
               MOVE 'Código da operação estruturada mercado de capitais
      -             'inválido.'        TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSLJ09-CD-OPR-ETRD-MCP EQUAL ZEROS
               MOVE ZEROS          TO  CD-OPR-ETRD-MCP-INI
               MOVE 99999999999    TO  CD-OPR-ETRD-MCP-FIM
           ELSE
               MOVE SGTSLJ09-CD-OPR-ETRD-MCP
                                   TO  CD-OPR-ETRD-MCP-INI
                                       CD-OPR-ETRD-MCP-FIM
           END-IF

           IF  SGTSLJ09-NR-VRS-OPR-ETRD < ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0006               TO MSG1-CD-ERRO-TS
               MOVE 'Número da versão da operação estruturada inválido.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSLJ09-NR-VRS-OPR-ETRD EQUAL ZEROS
               MOVE ZEROS          TO  NR-VRS-OPR-ETRD-INI
               MOVE 32767          TO  NR-VRS-OPR-ETRD-FIM
           ELSE
               MOVE SGTSLJ09-NR-VRS-OPR-ETRD
                                   TO  NR-VRS-OPR-ETRD-INI
                                       NR-VRS-OPR-ETRD-FIM
           END-IF

           IF  SGTSLJ09-CD-TIP-FUC-CLI < ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0692               TO MSG1-CD-ERRO-TS
               MOVE 'Código do tipo de função do cliente inválido.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           ELSE
      *        Se 0, recupera todos estados.
      *        Movendo range de 0 a 32767 (máximo valor para smallint)
               IF  SGTSLJ09-CD-TIP-FUC-CLI EQUAL ZEROS
                   MOVE 0              TO CD-TIP-FUC-CLI-INI
                   MOVE 32767          TO CD-TIP-FUC-CLI-FIM
               ELSE
                   MOVE SGTSLJ09-CD-TIP-FUC-CLI
                                       TO CD-TIP-FUC-CLI-INI
                                          CD-TIP-FUC-CLI-FIM
               END-IF
           END-IF

           IF  FUNCTION UPPER-CASE (SGTSLJ09-IN-RCPO-CLI-OPR)
               NOT EQUAL 'S'
           AND FUNCTION UPPER-CASE (SGTSLJ09-IN-RCPO-CLI-OPR)
               NOT EQUAL 'N'
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0693               TO MSG1-CD-ERRO-TS
               MOVE 'Indicador de recuperação de clientes da operação in
      -             'válido.'          TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           ELSE
      *        Se 'S', move 0 para considerar a query de cliente
      *        da operação.
               IF  FUNCTION UPPER-CASE (SGTSLJ09-IN-RCPO-CLI-OPR)
                   EQUAL 'S'
                   MOVE 0              TO AUX-CTRA-RCPO-CLI-OPR
      *        Se 'N', move 2147483647 (valor máximo para Integer), para
      *        ignorar a query de cliente da operação.
               ELSE
                   MOVE 2147483647     TO AUX-CTRA-RCPO-CLI-OPR
               END-IF
           END-IF

           IF  FUNCTION UPPER-CASE (SGTSLJ09-IN-RCPO-CLI-GRTR)
               NOT EQUAL 'S'
           AND FUNCTION UPPER-CASE (SGTSLJ09-IN-RCPO-CLI-GRTR)
               NOT EQUAL 'N'
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0694               TO MSG1-CD-ERRO-TS
               MOVE 'Indicador de recuperação de clientes garantidores i
      -             'nválido.'         TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           ELSE
      *        Se 'S', move 0 para considerar a query de cliente
      *        garantidor
               IF  FUNCTION UPPER-CASE (SGTSLJ09-IN-RCPO-CLI-GRTR)
                   EQUAL 'S'
                   MOVE 0              TO AUX-CTRA-RCPO-CLI-GRTR
      *        Se 'N', move 2147483647 (valor máximo para Integer), para
      *        ignorar a query de cliente garantidor
               ELSE
                   MOVE 2147483647     TO AUX-CTRA-RCPO-CLI-GRTR
               END-IF
           END-IF

           IF  SGTSLJ09-CD-CLI-ULT < ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0695               TO MSG1-CD-ERRO-TS
               MOVE 'Código do último cliente pesquisado inválido.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-PROCESSA-PESQUISA SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

           MOVE CD-CLI(SGTSLJ09-IC-TAB) TO
                     SGTSLJ09-CD-CLI(SGTSLJ09-IC-TAB)
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-POSTA-RESPOSTA SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS PUT
              CONTAINER ( 'SGTSLJ09-RPST'         )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSLJ09-RPST )
              FROM      ( SGTSLJ09-RPST           )
           END-EXEC
           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970101-ABRE-CURSOR-PSQ SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

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
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE REG-WS

           EXEC SQL
                FETCH NEXT ROWSET
                      CURSOR-PSQ FOR 0101 ROWS
               INTO :CD-CLI
           END-EXEC

           MOVE SQLERRD(3)             TO QT-REG-SELD
           IF  QT-REG-SELD EQUAL 0101
               SET SGTSLJ09-TEM-MAIS-REGISTRO TO TRUE
               MOVE 0100               TO QT-REG-SELD
           END-IF
           MOVE QT-REG-SELD            TO SGTSLJ09-QT-OCR-TAB
           MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ
           .
       970102-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970103-FECHA-CURSOR-PSQ SECTION.
      *-----------------------------------------------------------------
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO

           MOVE 'N'                    TO IN-CURSOR-PSQ-ABTO
           EXEC SQL
                CLOSE CURSOR-PSQ
           END-EXEC
           .
       970103-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA SECTION.
      *-----------------------------------------------------------------
           IF  IN-CURSOR-PSQ-ABTO  EQUAL  'S'
               PERFORM 970103-FECHA-CURSOR-PSQ
           END-IF

      *--> Verifica se o tipo de erro gerado possui tratamento interno
           EVALUATE  TRUE
               WHEN  ERRO-CONTAINER
                     PERFORM  999004-TRATA-ERRO-CTNR
               WHEN  ERRO-TAMANHO-AREA
                     PERFORM  999005-TRATA-ERRO-AREA-DADOS
               WHEN  ERRO-CANAL
                     PERFORM  999006-TRATA-ERRO-CANAL
               WHEN  OTHER
                     CONTINUE
           END-EVALUATE

           IF  MSG1-CD-ERRO-TS             EQUAL  ZEROS
               MOVE  8888                  TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS             EQUAL  SPACES
               MOVE  'Erro não especificado no módulo.'
                                           TO  MSG1-TX-ERRO-TS
           END-IF

           MOVE 'SGTSLJ09-CD-CTRA-TIT-VL'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSLJ09-CD-CTRA-TIT-VL     TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSLJ09-CD-TIP-FUC-CLI'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSLJ09-CD-TIP-FUC-CLI     TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSLJ09-IN-RCPO-CLI-OPR'  TO MSGKW999-NM-VRV-MNT
           MOVE SGTSLJ09-IN-RCPO-CLI-OPR    TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSLJ09-IN-RCPO-CLI-GRTR' TO MSGKW999-NM-VRV-MNT
           MOVE SGTSLJ09-IN-RCPO-CLI-GRTR   TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSLJ09-CD-CLI-ULT'       TO MSGKW999-NM-VRV-MNT
           MOVE SGTSLJ09-CD-CLI-ULT         TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

      * --> grava o erro no msg
           PERFORM MSG1-NTF-MSG
           MOVE MSG1-CD-ERRO-TS             TO IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS             TO IIBKERRO-TX-ERRO

           EXEC CICS PUT
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

      * --------------------------------------*
      * 999001-TRATA-ABEND-CICS        SECTION.
      * 999002-TRATA-CONDITION-CICS    SECTION.
      * 999003-ADICIONA-VRV-MNT        SECTION.
      * 999004-ALINHA-NUMERO-ESQUERDA  SECTION.
      * --------------------------------------*
-INC HLPKP999

      * --------------------------------------*
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * MSG1-CD-ERRO-TS = 3000
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * MSG1-CD-ERRO-TS = 3001
      * 999006-TRATA-ERRO-CANAL        SECTION.
      * MSG1-CD-ERRO-TS = 3003
      *---------------------------------------*
-INC SGTKP999
      * --------------------------------------*
      * 999010-TRATA-ERRO-DB2          SECTION.
-INC HLPKP997
      * --------------------------------------*
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