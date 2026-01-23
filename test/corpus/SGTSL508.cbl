      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSL508
      * ANALISTA..: CINTIA IGLESIAS
      * AUTOR.....: CINTIA IGLESIAS
      * OBJETIVO..: LISTAR PRECIFICAÇÕES VIGENTES EM UM PERIODO
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001  08/09/2017 - F2054109 -  DE 2016/145195
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSL508.

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
       77  CTE-INICIO-SS                 PIC  X(035)  VALUE
                                  '*** SGTSL508 S.S. COMECA AQUI ***'.
       77  CTE-PROG     PIC  X(18) VALUE '*** SGTSL508 ***'.
       77  CTE-VERS     PIC  X(06) VALUE 'VRS001'.

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

       01  QT-REG-SELD                       PIC S9(4) COMP-5 VALUE 0.
       01  CD-OPR-ETRD-MCP-DISPLAY           PIC  9(11) VALUE 0.

       01  TIP-TAXA-INI                      PIC S9(4) COMP-5 VALUE 0.
       01  TIP-TAXA-FIM                      PIC S9(4) COMP-5 VALUE 99.

      * Campos utilizados para leitura

       01  REG-WS.
           03  DT-FIM-CND-TAXA                PIC X(10)
               OCCURS 101 TIMES               VALUE ' '.
           03  DT-INC-CND-TAXA                PIC X(10)
               OCCURS 101 TIMES               VALUE ' '.
           03  PC-MIN-TAXA                    PIC S9(4)V9(10) COMP-3
               OCCURS 101 TIMES               VALUE 0.
           03  PC-MIN-TAXA-N                  PIC S9(4) COMP-5
               OCCURS 101 TIMES               VALUE 0.
           03  CD-TIP-APL-PREF                PIC S9(4) COMP-5
               OCCURS 101 TIMES               VALUE 0.
           03  CD-PERC-APL-PREF               PIC S9(4) COMP-5
               OCCURS 101 TIMES               VALUE 0.
           03  CD-IEC-PREF                    PIC S9(4) COMP-5
               OCCURS 101 TIMES               VALUE 0.

      * Indicadores de nulidades

      * Variavel(is) de controle do cursor(es)

       01  CTL-CURSOR-PSQ.
           03  CD-FIM-CURSOR-PSQ            PIC S9(04) COMP-5 VALUE 0.
               88  FIM-CURSOR-PSQ            VALUE +100.
           03  IN-CURSOR-PSQ-ABTO           PIC  X(01) VALUE 'N'.
               88  CURSOR-PSQ-ABTO          VALUE 'S'.

      * Variavel(is) auxiliares
       01 DT-FMTD-INC                       PIC X(10) VALUE '  .  . '.
       01 DT-FMTD-INC-R   REDEFINES  DT-FMTD-INC.
            03  DIA                         PIC  9(2).
            03  FILLER                      PIC  X(1).
            03  MES                         PIC  9(2).
            03  FILLER                      PIC  X(1).
            03  ANO                         PIC  9(4).

       01 DT-FMTD-FIM                       PIC X(10) VALUE '  .  . '.
       01 DT-FMTD-FIM-R   REDEFINES  DT-FMTD-FIM.
            03  DIA                         PIC  9(2).
            03  FILLER                      PIC  X(1).
            03  MES                         PIC  9(2).
            03  FILLER                      PIC  X(1).
            03  ANO                         PIC  9(4).

       01 DT-FMTD-ULT                       PIC X(10) VALUE '  .  . '.
       01 DT-FMTD-ULT-R   REDEFINES  DT-FMTD-ULT.
            03  DIA                         PIC  9(2).
            03  FILLER                      PIC  X(1).
            03  MES                         PIC  9(2).
            03  FILLER                      PIC  X(1).
            03  ANO                         PIC  9(4).

      * Subrotinas utilizadas

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

      *-----------------------------------------------------------------
-INC SGTKL508
           EXEC SQL
             DECLARE CURSOR-PSQ CURSOR
                     WITH ROWSET POSITIONING FOR
             SELECT
                    A.DT_FIM_CND_TAXA
                  , A.DT_INC_CND_TAXA
                  , A.PC_MIN_TAXA
                  , A.CD_TIP_APL_PREF
                  , A.CD_PERC_APL_PREF
                  , A.CD_IEC_PREF
               FROM DB2SGT.PREF_TAXA_OPR   A
              WHERE A.CD_CTRA_TIT_VL     = :SGTSL508-CD-CTRA-TIT-VL
                AND A.CD_OPR_ETRD_MCP    = :SGTSL508-CD-OPR-ETRD-MCP
                AND A.NR_VRS_OPR_ETRD    = :SGTSL508-NR-VRS-OPR-ETRD
                AND A.CD_TIP_TAXA        = :SGTSL508-CD-TIP-TAXA

                AND ((A.DT_INC_CND_TAXA   <= :DT-FMTD-INC
                AND A.DT_FIM_CND_TAXA   >= :DT-FMTD-INC)
                OR
                    (A.DT_INC_CND_TAXA   >= :DT-FMTD-INC
                AND A.DT_INC_CND_TAXA   <=  :DT-FMTD-FIM))

                AND A.DT_INC_CND_TAXA   > :DT-FMTD-ULT
              ORDER BY
                A.DT_INC_CND_TAXA
           END-EXEC.

       01  CTE-FINAL-SS                PIC  X(40)      VALUE
                    '*** S.S. TERMINA AQUI ***'.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
      *-----------------------------------------------------------------
       000000-PRINCIPAL                SECTION.
      *-----------------------------------------------------------------

           MOVE 0001                   TO MSG1-CD-SEQL-ERRO

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
                    VARYING SGTSL508-IC-TAB FROM 1 BY 1
                    UNTIL SGTSL508-IC-TAB > SGTSL508-QT-OCR-TAB

           PERFORM 300000-POSTA-RESPOSTA
           GOBACK
           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-DESCOBRIR-CANAL SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CANAL             TO TRUE

           EXEC CICS ASSIGN
              CHANNEL ( NM-CNL )
           END-EXEC

      *--- Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE

           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-EXCLUIR-CTNR-RPST SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                   TO MSG1-CD-SEQL-ERRO

           SET DEL-CONTAINER           TO TRUE

      *    Exclui o container de resposta
      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS DELETE
                     CONTAINER ('SGTSL508-RPST')
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
           MOVE 0004                   TO MSG1-CD-SEQL-ERRO

      *    Limpar variáveis container de erro
           INITIALIZE IIBKERRO-PRM-ERRO

      *    Exclui o container de erro
           MOVE 'PRM-ERRO'             TO NM-CTNR
           SET DEL-CONTAINER           TO TRUE

      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS DELETE
                     CONTAINER (NM-CTNR   )
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
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE


           MOVE ZEROS                        TO QT-CCT-LIDO
           MOVE LENGTH OF SGTSL508-RQSC      TO QT-CCT-EPRO

           EXEC CICS GET
              CONTAINER ( 'SGTSL508-RQSC' )
              CHANNEL   ( NM-CNL          )
              NODATA
              FLENGTH   ( QT-CCT-LIDO     )
           END-EXEC

      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE

      *    Valida tamanho esperado da requisição X tamanho recebido

           IF  QT-CCT-LIDO             NOT EQUAL QT-CCT-EPRO
               SET ERRO-TAMANHO-AREA   TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           MOVE 0013                   TO MSG1-CD-SEQL-ERRO

      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *--- Efetua a leitura do container

           EXEC CICS GET
              CONTAINER ( 'SGTSL508-RQSC'         )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSL508-RQSC )
              INTO      ( SGTSL508-RQSC           )
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

           MOVE 'N'                     TO SGTSL508-IN-CNTO-PSQ
           IF  SGTSL508-CD-CTRA-TIT-VL EQUAL ZEROS
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 0002                TO MSG1-CD-ERRO-TS
               MOVE 'Código carteira título valor não informado.'
                                        TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL508-CD-OPR-ETRD-MCP NOT NUMERIC
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 0003                TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturado mercado de capitais não
      -        ' numérico.'             TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL508-CD-OPR-ETRD-MCP   EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE 0004               TO  MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturado mercado de capitais inv
      -        'álido.'                TO  MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL508-NR-VRS-OPR-ETRD EQUAL ZEROS
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 0007                TO MSG1-CD-ERRO-TS
               MOVE 'Número versão operação estruturado não informado.'
                                        TO MSG1-TX-ERRO-TS
              SET MNT-VRV             TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL508-CD-TIP-TAXA  EQUAL ZEROS
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 0166                TO MSG1-CD-ERRO-TS
               MOVE 'Código do tipo da taxa não informado.'
                                        TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL508-DT-INC-PSQ  EQUAL ZEROS
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 0437                TO MSG1-CD-ERRO-TS
               MOVE 'Data início de pesquisa não informado.'
                                        TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL508-DT-FIM-PSQ  EQUAL ZEROS
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 0438                TO MSG1-CD-ERRO-TS
               MOVE 'Data fim de pesquisa não informado.'
                                        TO MSG1-TX-ERRO-TS
              SET MNT-VRV             TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF


           IF SGTSL508-DT-ULT-INC EQUAL SPACES
              MOVE '01011900'   TO  SGTSL508-DT-ULT-INC
           END-IF

           MOVE CORR SGTSL508-DT-INC-PSQ-R  TO  DT-FMTD-INC-R
           MOVE CORR SGTSL508-DT-FIM-PSQ-R  TO  DT-FMTD-FIM-R
           MOVE CORR SGTSL508-DT-ULT-INC-R  TO  DT-FMTD-ULT-R
           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-PROCESSA-PESQUISA SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

           MOVE DT-FIM-CND-TAXA(SGTSL508-IC-TAB) TO  DT-FMTD-INC
           MOVE CORR DT-FMTD-INC-R         TO
                             SGTSL508-DT-FIM-CND-TAXA-R(SGTSL508-IC-TAB)
           MOVE DT-INC-CND-TAXA(SGTSL508-IC-TAB) TO  DT-FMTD-INC
           MOVE CORR DT-FMTD-INC-R         TO
                             SGTSL508-DT-INC-CND-TAXA-R(SGTSL508-IC-TAB)
           MOVE PC-MIN-TAXA(SGTSL508-IC-TAB)  TO
                                   SGTSL508-PC-MIN-TAXA(SGTSL508-IC-TAB)
           MOVE CD-TIP-APL-PREF(SGTSL508-IC-TAB) TO
                               SGTSL508-CD-TIP-APL-PREF(SGTSL508-IC-TAB)
           MOVE CD-PERC-APL-PREF(SGTSL508-IC-TAB) TO
                              SGTSL508-CD-PERC-APL-PREF(SGTSL508-IC-TAB)
           MOVE CD-IEC-PREF(SGTSL508-IC-TAB) TO
                                   SGTSL508-CD-IEC-PREF(SGTSL508-IC-TAB)
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-POSTA-RESPOSTA SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

           SET PUT-CONTAINER           TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container

           SET  ERRO-CONTAINER         TO TRUE



           EXEC CICS PUT
              CONTAINER ( 'SGTSL508-RPST'         )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSL508-RPST )
              FROM      ( SGTSL508-RPST           )
           END-EXEC

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE

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
               INTO
                    :DT-FIM-CND-TAXA
                  , :DT-INC-CND-TAXA
                  , :PC-MIN-TAXA
                    :PC-MIN-TAXA-N
                  , :CD-TIP-APL-PREF
                  , :CD-PERC-APL-PREF
                  , :CD-IEC-PREF
           END-EXEC

           MOVE SQLERRD(3)             TO QT-REG-SELD

           IF  QT-REG-SELD EQUAL 0101
               SET SGTSL508-TEM-MAIS-REGISTRO TO TRUE
               MOVE 0100               TO QT-REG-SELD
           END-IF
           MOVE QT-REG-SELD            TO SGTSL508-QT-OCR-TAB
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
           MOVE 0012                   TO  MSG1-CD-SEQL-ERRO

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

      *Verifica se o tipo de erro gerado possui tratamento interno
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

           MOVE 'SGTSL508-CD-CTRA-TIT-VL'    TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL508-CD-CTRA-TIT-VL      TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL508-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
           IF SGTSL508-CD-OPR-ETRD-MCP IS NUMERIC
               MOVE SGTSL508-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP-DISPLAY
               MOVE CD-OPR-ETRD-MCP-DISPLAY
                                       TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE 'Código não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL508-NR-VRS-OPR-ETRD'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL508-NR-VRS-OPR-ETRD     TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL508-CD-TIP-TAXA'       TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL508-CD-TIP-TAXA         TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT


           MOVE 'SGTSL508-DT-INC-PSQ'        TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL508-DT-INC-PSQ          TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL508-DT-FIM-PSQ'        TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL508-DT-FIM-PSQ          TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL508-DT-ULT-INC'        TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL508-DT-ULT-INC          TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

      * --> grava o erro no msg
           PERFORM MSG1-NTF-MSG
           MOVE MSG1-CD-ERRO-TS              TO IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS              TO IIBKERRO-TX-ERRO

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
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
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