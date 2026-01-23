      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSL077
      * ANALISTA..: F8658869 - Roberto Ioshimoto
      * AUTOR.....: F8658869 - Roberto Ioshimoto
      * OBJETIVO..: Listar a Tabela DB2SGT.AUTZ_NEGO_OPR_ETRD
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 29.06.2017 - F8658869 - 2016/D145195 - Implantação

      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSL077.

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
       01  CTE-PROG     PIC  X(17) VALUE '*** SGTSL077 ***'.
       01  CTE-VERS     PIC  X(06) VALUE 'VRS001'.

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
-INC SGTKERR1

      * Area de trabalho

       01  QT-REG-SELD                      PIC S9(4) COMP-5 VALUE 0.

      * Campos utilizados para leitura

       01  REG-WS.
           03 CD-USU-RSP-AUTZ                PIC X(8)
              OCCURS 101 TIMES               VALUE ' '.
           03 IN-FLZ-EXCO-OPR                PIC X(1)
              OCCURS 101 TIMES               VALUE ' '.
           03 NR-SEQL-ORD-NEGO               PIC S9(9) COMP-5
              OCCURS 101 TIMES               VALUE 0.
           03 TS-AUTZ-NEGO                   PIC X(26)
              OCCURS 101 TIMES               VALUE ' '.
           03 TX-JST-FLZ-EXCO                PIC X(200)
              OCCURS 101 TIMES               VALUE ' '.
           03 TX-JST-FLZ-EXCO-N              PIC S9(4) COMP-5
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
       01  SGTSL077-DADOS.
-INC SGTKL077

           EXEC SQL
             DECLARE CURSOR-PSQ CURSOR
                     WITH ROWSET POSITIONING FOR
             SELECT A.CD_USU_RSP_AUTZ
                  , A.IN_FLZ_EXCO_OPR
                  , A.NR_SEQL_ORD_NEGO
                  , A.TS_AUTZ_NEGO
                  , A.TX_JST_FLZ_EXCO
               FROM DB2SGT.AUTZ_NEGO_OPR_ETRD A
              WHERE A.CD_CTRA_TIT_VL     = :SGTSL077-CD-CTRA-TIT-VL
                AND A.CD_OPR_ETRD_MCP    = :SGTSL077-CD-OPR-ETRD-MCP
                AND A.NR_VRS_OPR_ETRD    = :SGTSL077-NR-VRS-OPR-ETRD
                AND (A.NR_SEQL_ORD_NEGO  > :SGTSL077-NR-SEQL-ULT OR
                    (A.NR_SEQL_ORD_NEGO  = :SGTSL077-NR-SEQL-ULT AND
                     A.TS_AUTZ_NEGO      > :SGTSL077-TS-AUTZ-ULT))
             ORDER BY A.NR_SEQL_ORD_NEGO, A.TS_AUTZ_NEGO
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
                    VARYING SGTSL077-IC-TAB FROM 1 BY 1
                    UNTIL SGTSL077-IC-TAB > SGTSL077-QT-OCR-TAB

           PERFORM 300000-POSTA-RESPOSTA
           GOBACK
           .
      *-----------------------------------------------------------------
       100000-DESCOBRIR-CANAL SECTION.
      *-----------------------------------------------------------------
           MOVE 0001                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS ASSIGN
              CHANNEL ( NM-CNL )
           END-EXEC
           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-EXCLUIR-CTNR-RPST SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS DELETE
              CONTAINER ( 'SGTSL077-RPST' )
              CHANNEL   ( NM-CNL      )
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
              CHANNEL   ( NM-CNL      )
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

      *    Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE
           SET  ERRO-CONTAINER         TO TRUE

           MOVE ZEROS                  TO QT-CCT-LIDO
           MOVE LENGTH OF SGTSL077-RQSC
                                       TO QT-CCT-EPRO
           EXEC CICS GET
              CONTAINER ( 'SGTSL077-RQSC' )
              CHANNEL   ( NM-CNL           )
              NODATA
              FLENGTH   ( QT-CCT-LIDO     )
           END-EXEC

           SET  SEM-ERRO               TO TRUE

      *    Valida tamanho esperado da requisição X tamanho recebido

           IF  QT-CCT-LIDO             NOT EQUAL QT-CCT-EPRO
               SET ERRO-TAMANHO-AREA   TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

      *    Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE
           SET  ERRO-CONTAINER         TO TRUE

           EXEC CICS GET
              CONTAINER ( 'SGTSL077-RQSC'         )
              CHANNEL   ( NM-CNL                   )
              FLENGTH   ( LENGTH OF SGTSL077-RQSC )
              INTO      ( SGTSL077-RQSC           )
           END-EXEC

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       130000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       140000-VALIDAR-PARAMETRO SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

           MOVE 'N'                    TO SGTSL077-IN-CNTO-PSQ
           IF  SGTSL077-CD-CTRA-TIT-VL EQUAL ZEROS
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 0002                TO MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' não informado.'  TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL077-CD-OPR-ETRD-MCP NOT NUMERIC
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 0003                TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -        ' numérico.'             TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL077-CD-OPR-ETRD-MCP EQUAL ZEROS
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 0005                TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -        ' informado.'            TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL077-NR-VRS-OPR-ETRD EQUAL ZEROS
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 0007                TO MSG1-CD-ERRO-TS
               MOVE 'Número da versão da operação não informado.'
                                        TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-PROCESSA-PESQUISA SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

           MOVE CD-USU-RSP-AUTZ(SGTSL077-IC-TAB) TO
                     SGTSL077-CD-USU-RSP-AUTZ(SGTSL077-IC-TAB)
           MOVE IN-FLZ-EXCO-OPR(SGTSL077-IC-TAB) TO
                     SGTSL077-IN-FLZ-EXCO-OPR(SGTSL077-IC-TAB)
           MOVE NR-SEQL-ORD-NEGO(SGTSL077-IC-TAB) TO
                     SGTSL077-NR-SEQL-ORD-NEGO(SGTSL077-IC-TAB)
           MOVE TS-AUTZ-NEGO(SGTSL077-IC-TAB) TO
                     SGTSL077-TS-AUTZ-NEGO(SGTSL077-IC-TAB)
           MOVE TX-JST-FLZ-EXCO(SGTSL077-IC-TAB) TO
                     SGTSL077-TX-JST-FLZ-EXCO(SGTSL077-IC-TAB)
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-POSTA-RESPOSTA SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS PUT
              CONTAINER ( 'SGTSL077-RPST'         )
              CHANNEL   ( NM-CNL                   )
              FLENGTH   ( LENGTH OF SGTSL077-RPST )
              FROM      ( SGTSL077-RPST           )
           END-EXEC
           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970101-ABRE-CURSOR-PSQ SECTION.
      *-----------------------------------------------------------------
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO

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
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE REG-WS
           EXEC SQL
                FETCH NEXT ROWSET
                      CURSOR-PSQ FOR 0101 ROWS
               INTO :CD-USU-RSP-AUTZ
                  , :IN-FLZ-EXCO-OPR
                  , :NR-SEQL-ORD-NEGO
                  , :TS-AUTZ-NEGO
                  , :TX-JST-FLZ-EXCO
                    :TX-JST-FLZ-EXCO-N
           END-EXEC

           MOVE SQLERRD(3)             TO QT-REG-SELD
           IF  QT-REG-SELD EQUAL 0101
               SET SGTSL077-TEM-MAIS-REGISTRO TO TRUE
               MOVE 0100                TO QT-REG-SELD
           END-IF
           MOVE QT-REG-SELD            TO SGTSL077-QT-OCR-TAB
           MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ
           .
       970102-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970103-FECHA-CURSOR-PSQ SECTION.
      *-----------------------------------------------------------------
           MOVE 0011                   TO MSG1-CD-SEQL-ERRO

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

           IF  IN-CURSOR-PSQ-ABTO  EQUAL 'S'
               PERFORM 970103-FECHA-CURSOR-PSQ
           END-IF

           IF  MSG1-CD-ERRO-TS EQUAL ZEROS
               MOVE 8888                TO MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS EQUAL SPACES
               MOVE 'Erro não especificado no módulo.'
                                        TO MSG1-TX-ERRO-TS
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

           MOVE 'SGTSL077-CD-CTRA-TIT-VL'    TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL077-CD-CTRA-TIT-VL      TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL077-CD-OPR-ETRD-MCP'   TO MSGKW999-NM-VRV-MNT
           IF  SGTSL077-CD-OPR-ETRD-MCP    NUMERIC
               MOVE  SGTSL077-CD-OPR-ETRD-MCP
                                       TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL077-NR-VRS-OPR-ETRD'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL077-NR-VRS-OPR-ETRD     TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL077-NR-SEQL-ULT'       TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL077-NR-SEQL-ULT         TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL077-TS-AUTZ-ULT'       TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL077-TS-AUTZ-ULT         TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

      * --> grava o erro no msg
           PERFORM MSG1-NTF-MSG
           MOVE MSG1-CD-ERRO-TS             TO IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS             TO IIBKERRO-TX-ERRO

           EXEC CICS PUT
              CONTAINER ( 'PRM-ERRO' )
              CHANNEL   ( NM-CNL      )
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
-INC HLPKP999
      * --------------------------------------*

      *---------------------------------------*
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
-INC SGTKP999
      * --------------------------------------*
      * 999010-TRATA-ERRO-DB2          SECTION.
-INC HLPKP997
      * --------------------------------------*

      * --------------------------------------*
      * MSG1-NTF-MSG                   SECTION.
-INC MSGKPMG1
      * --------------------------------------*

      * --------------------------------------*
       999999-ENCERRAR                SECTION.
      * --------------------------------------*
           EXEC CICS
                RETURN
           END-EXEC
           .
       999000-SAI.
           EXIT.
      * --------------------------------------*