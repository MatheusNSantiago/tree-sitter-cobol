      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSL068
      * ANALISTA..: CINTIA IGLESIAS
      * AUTOR.....: CINTIA IGLESIAS
      * OBJETIVO..: LISTA DA TABELA PREF_TAXA_OPR
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001  22.08.2017 - F9188847 -  DE 2016/145195
      *               Alteração nome Tabela:
      *                                 De: CND_MIN_TAXA_OPR
      *                                 Para: PREF_TAXA_OPR
      *               Inclusão campos:  CD_TIP_APL_PREF
      *                               , CD_PERC_APL_PREF
      *                               , CD_IEC_PREF
      *          13.12.2016 - F2054109 DE 2015/137233 - IMPLANTACAO
      *
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSL068.

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
       77  CTE-INICIO-SS                 PIC  X(040)  VALUE
                   '*** S.S. COMEÇA AQUI ***'.
       77  CTE-PROG     PIC  X(17) VALUE '*** SGTSL068 ***'.
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

      *--> Auxiliares Movimentacao Monitoração
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).

       01  TIP-TAXA-INI                      PIC S9(4) COMP-5 VALUE 0.
       01  TIP-TAXA-FIM                      PIC S9(4) COMP-5 VALUE 99.

      * Campos utilizados para leitura

       01  REG-WS.
           03  CD-TIP-TAXA                    PIC S9(4) COMP-5
               OCCURS 101 TIMES               VALUE 0.
           03  CD-USU-ULT-ATL                 PIC X(8)
               OCCURS 101 TIMES               VALUE ' '.
           03  DT-FIM-CND-TAXA                PIC X(10)
               OCCURS 101 TIMES               VALUE ' '.
           03  DT-INC-CND-TAXA                PIC X(10)
               OCCURS 101 TIMES               VALUE ' '.
           03  PC-MIN-TAXA                    PIC S9(4)V9(10) COMP-3
               OCCURS 101 TIMES               VALUE 0.
           03  PC-MIN-TAXA-N                  PIC S9(4) COMP-5
               OCCURS 101 TIMES               VALUE 0.
           03  TS-ULT-ATL                     PIC X(26)
               OCCURS 101 TIMES               VALUE ' '.
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
       01 DT-FMTD                           PIC X(10) VALUE ' '.
       01 DT-FMTD-R   REDEFINES  DT-FMTD.
            03  DIA                         PIC  9(2).
            03  PTO1                        PIC  X(1).
            03  MES                         PIC  9(2).
            03  PTO2                        PIC  X(1).
            03  ANO                         PIC  9(4).

      * Subrotinas utilizadas

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

      *-----------------------------------------------------------------
-INC SGTKL068
           EXEC SQL
             DECLARE CURSOR-PSQ CURSOR
                     WITH ROWSET POSITIONING FOR
             SELECT A.CD_TIP_TAXA
                  , A.CD_USU_ULT_ATL
                  , A.DT_FIM_CND_TAXA
                  , A.DT_INC_CND_TAXA
                  , A.PC_MIN_TAXA
                  , A.TS_ULT_ATL
                  , A.CD_TIP_APL_PREF
                  , A.CD_PERC_APL_PREF
                  , A.CD_IEC_PREF
               FROM DB2SGT.PREF_TAXA_OPR   A
              WHERE A.CD_CTRA_TIT_VL     = :SGTSL068-CD-CTRA-TIT-VL
                AND A.CD_OPR_ETRD_MCP    = :SGTSL068-CD-OPR-ETRD-MCP
                AND A.NR_VRS_OPR_ETRD    = :SGTSL068-NR-VRS-OPR-ETRD
                AND (A.CD_TIP_TAXA      >= :TIP-TAXA-INI
                AND  A.CD_TIP_TAXA      <= :TIP-TAXA-FIM)
                AND (A.CD_TIP_TAXA       > :SGTSL068-CD-ULT-TIP-TAXA
                OR  (A.CD_TIP_TAXA       = :SGTSL068-CD-ULT-TIP-TAXA
                AND A.DT_INC_CND_TAXA    > :DT-FMTD))
              ORDER BY
                A.CD_TIP_TAXA, A.DT_INC_CND_TAXA
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
                    VARYING SGTSL068-IC-TAB FROM 1 BY 1
                    UNTIL SGTSL068-IC-TAB > SGTSL068-QT-OCR-TAB

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
              CONTAINER ( 'SGTSL068-RPST' )
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

      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE
           SET  ERRO-CONTAINER         TO TRUE
           MOVE 'SGTSL068-RQSC'        TO NM-CTNR

           MOVE ZEROS                  TO QT-CCT-LIDO
           MOVE LENGTH OF SGTSL068-RQSC
                                       TO QT-CCT-EPRO
           EXEC CICS GET
              CONTAINER  (NM-CTNR         )
              CHANNEL   ( NM-CNL          )
              NODATA
              FLENGTH   ( QT-CCT-LIDO     )
           END-EXEC

      *    Valida tamanho esperado da requisição X tamanho recebido

           IF  QT-CCT-LIDO             NOT EQUAL QT-CCT-EPRO
               SET ERRO-TAMANHO-AREA   TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS GET
              CONTAINER ( NM-CTNR                 )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSL068-RQSC )
              INTO      ( SGTSL068-RQSC           )
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

           MOVE 'N'                     TO SGTSL068-IN-CNTO-PSQ
           IF  SGTSL068-CD-CTRA-TIT-VL EQUAL ZEROS
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 0002                TO MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de título e  valores mobiliários
      -             ' não informado.'   TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL068-CD-OPR-ETRD-MCP NOT NUMERIC
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 0003                TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -        ' numérico.'             TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL068-CD-OPR-ETRD-MCP EQUAL ZEROS
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 0005                TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -        ' informado.'            TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL068-NR-VRS-OPR-ETRD EQUAL ZEROS
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 0007                TO MSG1-CD-ERRO-TS
               MOVE 'Número da versão da operação não informado.'
                                        TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL068-CD-TIP-TAXA  > ZEROS
               MOVE SGTSL068-CD-TIP-TAXA   TO  TIP-TAXA-INI
                                               TIP-TAXA-FIM
                                               SGTSL068-CD-ULT-TIP-TAXA
           END-IF

           IF SGTSL068-DT-ULT-INC-VLD EQUAL SPACES
              MOVE '01011900'   TO  SGTSL068-DT-ULT-INC-VLD
           END-IF

           MOVE CORR SGTSL068-DT-ULT-INC-VLD-R  TO  DT-FMTD-R
           MOVE '.'                             TO  PTO1 PTO2
           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-PROCESSA-PESQUISA SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

           MOVE CD-TIP-TAXA(SGTSL068-IC-TAB)    TO
                                 SGTSL068-CD-TIP-TAXA-S(SGTSL068-IC-TAB)
           MOVE DT-FIM-CND-TAXA(SGTSL068-IC-TAB) TO  DT-FMTD
           MOVE CORR DT-FMTD-R         TO
                             SGTSL068-DT-FIM-CND-TAXA-R(SGTSL068-IC-TAB)
           MOVE DT-INC-CND-TAXA(SGTSL068-IC-TAB) TO  DT-FMTD
           MOVE CORR DT-FMTD-R         TO
                             SGTSL068-DT-INC-CND-TAXA-R(SGTSL068-IC-TAB)
           MOVE PC-MIN-TAXA(SGTSL068-IC-TAB)  TO
                                   SGTSL068-PC-MIN-TAXA(SGTSL068-IC-TAB)
           MOVE CD-TIP-APL-PREF(SGTSL068-IC-TAB) TO
                               SGTSL068-CD-TIP-APL-PREF(SGTSL068-IC-TAB)
           MOVE CD-PERC-APL-PREF(SGTSL068-IC-TAB) TO
                              SGTSL068-CD-PERC-APL-PREF(SGTSL068-IC-TAB)
           MOVE CD-IEC-PREF(SGTSL068-IC-TAB) TO
                                   SGTSL068-CD-IEC-PREF(SGTSL068-IC-TAB)
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-POSTA-RESPOSTA SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

           SET  PUT-CONTAINER          TO TRUE
           SET  ERRO-CONTAINER         TO TRUE
           MOVE 'SGTSL068-RPST'        TO NM-CTNR

           EXEC CICS PUT
              CONTAINER ( NM-CTNR                 )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSL068-RPST )
              FROM      ( SGTSL068-RPST           )
           END-EXEC

           SET SEM-ERRO                TO TRUE
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
               INTO :CD-TIP-TAXA
                  , :CD-USU-ULT-ATL
                  , :DT-FIM-CND-TAXA
                  , :DT-INC-CND-TAXA
                  , :PC-MIN-TAXA
                    :PC-MIN-TAXA-N
                  , :TS-ULT-ATL
                  , :CD-TIP-APL-PREF
                  , :CD-PERC-APL-PREF
                  , :CD-IEC-PREF
           END-EXEC

           MOVE SQLERRD(3)             TO QT-REG-SELD

           IF  QT-REG-SELD EQUAL 0101
               SET SGTSL068-TEM-MAIS-REGISTRO TO TRUE
               MOVE 0100               TO QT-REG-SELD
           END-IF
           MOVE QT-REG-SELD            TO SGTSL068-QT-OCR-TAB
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

           MOVE 'SGTSL068-CD-CTRA-TIT-VL'    TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL068-CD-CTRA-TIT-VL      TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL068-CD-OPR-ETRD-MCP'   TO MSGKW999-NM-VRV-MNT
           IF   SGTSL068-CD-OPR-ETRD-MCP     IS NUMERIC
                MOVE SGTSL068-CD-OPR-ETRD-MCP     TO CD-OPR-ETRD-MCP
                MOVE CD-OPR-ETRD-MCP              TO MSGKW999-VL-VRV-MNT
           ELSE
                MOVE 'Código de operação estruturada de mercado de capit
      -              'ais não numérico.'          TO MSGKW999-VL-VRV-MNT

           END-IF
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL068-NR-VRS-OPR-ETRD'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL068-NR-VRS-OPR-ETRD     TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL068-CD-TIP-TAXA'       TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL068-CD-TIP-TAXA         TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL068-CD-ULT-TIP-TAXA'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL068-CD-ULT-TIP-TAXA     TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL068-DT-ULT-INC-VLD'    TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL068-DT-ULT-INC-VLD      TO MSGKW999-VL-VRV-MNT
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