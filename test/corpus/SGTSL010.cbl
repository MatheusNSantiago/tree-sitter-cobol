      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSL010
      * ANALISTA..: F8785286 Roseli Akemi Kawahata
      * AUTOR.....: F8785286 Roseli Akemi Kawahata
      * OBJETIVO..: Consulta Tabela MVTC_FNCR_OPR_ETRD
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 03/07/2018 - F8785286 DE 2016/145195 - IMPLANTACAO
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSL010.
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
      *-----------------------------------------------------------------
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
      *-----------------------------------------------------------------
       01  CTE-PROG     PIC  X(17) VALUE '*** SGTSL010 ***'.
       01  CTE-VERS     PIC  X(06) VALUE 'VRS001'.
      *-----------------------------------------------------------------
      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informacoes
-INC HLPKE01B
       LOCAL-STORAGE SECTION.
      *-----------------------------------------------------------------
      * book de para acionamento do msg1
-INC IIBKERRO
-INC MSGKWMG1
-INC HLPKW999
-INC SGTKERR1

       01  GRP-CONTAINER.
           03  CANAL                   PIC X(16) VALUE SPACES.

      * Area de trabalho
       01 QT-REG-SELD                  PIC S9(4) COMP-5 VALUE 0.

      * Campos utilizados para leitura
       01  REG-WS.
           03 CD-TIP-MVTC-TIT          PIC S9(4) COMP-5
              OCCURS 101 TIMES         VALUE 0.
           03 TS-REG-PGTO              PIC  X(26)
              OCCURS 101 TIMES         VALUE ' '.
           03 DT-PRV-MVT               PIC  X(10)
              OCCURS 101 TIMES         VALUE ' '.
           03 VL-PRVT-MVTC             PIC S9(15)V9(2) COMP-3
              OCCURS 101 TIMES         VALUE 0.
           03 DT-MVTC-TIT              PIC  X(10)
              OCCURS 101 TIMES         VALUE ' '.
           03 VL-MVTC-TIT              PIC S9(15)V9(2) COMP-3
              OCCURS 101 TIMES         VALUE 0.
           03 DT-PRV-MVT-N             PIC S9(4) COMP-5
              OCCURS 101 TIMES         VALUE 0.
           03 VL-PRVT-MVTC-N           PIC S9(4) COMP-5
              OCCURS 101 TIMES         VALUE 0.
           03 DT-MVTC-TIT-N            PIC S9(4) COMP-5
              OCCURS 101 TIMES         VALUE 0.
           03 VL-MVTC-TIT-N            PIC S9(4) COMP-5
              OCCURS 101 TIMES         VALUE 0.

      * Indicadores de nulidades
       01 QT-REG-SELD-N                PIC S9(4) COMP-5 VALUE 0.

      * Variáveis auxiliares
       01 AUX-CD-TIP-EST-DCS           PIC S9(4) COMP-5 VALUE 2.

      * Variavel(is) de controle do cursor(es)
       01  CTL-CURSOR-PSQ.
           03  CD-FIM-CURSOR-PSQ       PIC S9(04) COMP-5 VALUE 0.
               88  FIM-CURSOR-PSQ      VALUE +100.
           03  IN-CURSOR-PSQ-ABTO      PIC  X(01) VALUE 'N'.
               88  CURSOR-PSQ-ABTO     VALUE 'S'.

       01  DADOS-LOG-DISPLAY.
           03  CD-OPR-ETRD-MCP-DISP    PIC  9(11) VALUE 0.

      * Subrotinas utilizadas
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

      *-----------------------------------------------------------------
      *--> Pesquisa Registros de Movimentação Financeira
      *-----------------------------------------------------------------
-INC SGTKL010
           EXEC SQL
             DECLARE CURSOR-PSQ-1 CURSOR
                     WITH ROWSET POSITIONING FOR
             SELECT A.CD_TIP_MVTC_TIT
                  , A.TS_REG_PGTO
                  , A.DT_PRV_MVT
                  , A.VL_PRVT_MVTC
                  , A.DT_MVTC_TIT
                  , A.VL_MVTC_TIT

             FROM DB2SGT.MVTC_FNCR_OPR_ETRD   A

             WHERE A.CD_CTRA_TIT_VL      = :SGTSL010-CD-CTRA-TIT-VL
               AND A.CD_OPR_ETRD_MCP     = :SGTSL010-CD-OPR-ETRD-MCP
               AND A.NR_VRS_OPR_ETRD     = :SGTSL010-NR-VRS-OPR-ETRD
               AND A.DT_MVTC_TIT         >= :SGTSL010-DT-MVTC-TIT-ULT
             ORDER BY  A.CD_TIP_MVTC_TIT

           END-EXEC.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
       000000-ROTINA-PRINCIPAL         SECTION.
      *----------------------------------------------------------------*

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
                    VARYING SGTSL010-IC-TAB FROM 1 BY 1
                    UNTIL SGTSL010-IC-TAB > SGTSL010-QT-OCR-TAB

           PERFORM 300000-POSTA-RESPOSTA

           GOBACK
           .
       000000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       100000-DESCOBRIR-CANAL          SECTION.
      *-----------------------------------------------------------------
           MOVE 0001                   TO  MSG1-CD-SEQL-ERRO

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CANAL             TO  TRUE

           EXEC CICS ASSIGN
              CHANNEL ( CANAL )
           END-EXEC

      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO  TRUE
           .
       100000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       110000-EXCLUIR-CTNR-RPST        SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO  MSG1-CD-SEQL-ERRO

           EXEC CICS DELETE
              CONTAINER ( 'SGTSL010-RPST' )
              CHANNEL   ( CANAL      )
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
       120000-EXCLUIR-CTNR-ERRO        SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS DELETE
              CONTAINER ( 'PRM-ERRO' )
              CHANNEL   ( CANAL      )
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
       130000-RECEBER-REQUISICAO       SECTION.
      *-----------------------------------------------------------------
           MOVE 0004                   TO  MSG1-CD-SEQL-ERRO

           MOVE ZEROS                  TO  QT-CCT-LIDO
           MOVE LENGTH OF SGTSL010-RQSC    TO  QT-CCT-EPRO

           SET GET-CONTAINER           TO TRUE
           SET ERRO-CONTAINER          TO TRUE
           MOVE 'SGTSL010-RQSC'        TO NM-CTNR

           EXEC CICS GET
              CONTAINER ( NM-CTNR    )
              CHANNEL   ( CANAL      )
              NODATA
              FLENGTH ( QT-CCT-LIDO )
           END-EXEC

           IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
               SET   ERRO-TAMANHO-AREA   TO  TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS GET
              CONTAINER ( NM-CTNR    )
              CHANNEL   ( CANAL      )
              FLENGTH ( LENGTH OF SGTSL010-RQSC )
              INTO    ( SGTSL010-RQSC )
           END-EXEC

           SET SEM-ERRO                TO TRUE
           .
       130000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       140000-VALIDAR-PARAMETRO        SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO  MSG1-CD-SEQL-ERRO

           MOVE 'N'                    TO  SGTSL010-IN-CTNC-PSQ

           IF  SGTSL010-CD-CTRA-TIT-VL     EQUAL ZEROS
               SET  ERRO-PROGRAMADO        TO  TRUE
               MOVE 002                    TO  MSG1-CD-ERRO-TS
               MOVE 'Codigo Carteira TVM deve ser informado.'
                                           TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL010-CD-OPR-ETRD-MCP    NOT NUMERIC
               SET  ERRO-PROGRAMADO        TO  TRUE
               MOVE 003                    TO  MSG1-CD-ERRO-TS
               MOVE 'Codigo da Operação Estruturada inválido.'
                                           TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           ELSE
               IF  SGTSL010-CD-OPR-ETRD-MCP    EQUAL ZEROS
                   SET  ERRO-PROGRAMADO        TO  TRUE
                   MOVE 005                    TO  MSG1-CD-ERRO-TS
                   MOVE 'Codigo da Operação Estruturada deve ser informa
      -                 'do.'                  TO  MSG1-TX-ERRO-TS
                   PERFORM 999000-GRAVA-ERRO-E-ENCERRA
               END-IF
           END-IF

           IF  SGTSL010-NR-VRS-OPR-ETRD    EQUAL ZEROS
               SET  ERRO-PROGRAMADO        TO  TRUE
               MOVE 007                    TO  MSG1-CD-ERRO-TS
               MOVE 'Número da Versão da Operação deve ser informado.'
                                           TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       140000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       200000-PROCESSA-PESQUISA        SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO  MSG1-CD-SEQL-ERRO

           MOVE CD-TIP-MVTC-TIT  (SGTSL010-IC-TAB) TO
                             SGTSL010-CD-TIP-MVTC-TIT  (SGTSL010-IC-TAB)

           MOVE TS-REG-PGTO(SGTSL010-IC-TAB)   TO
                             SGTSL010-TS-REG-PGTO(SGTSL010-IC-TAB)

           MOVE DT-PRV-MVT(SGTSL010-IC-TAB)  TO
                             SGTSL010-DT-PRV-MVT(SGTSL010-IC-TAB)

           MOVE VL-PRVT-MVTC(SGTSL010-IC-TAB)    TO
                             SGTSL010-VL-PRVT-MVTC(SGTSL010-IC-TAB)

           MOVE DT-MVTC-TIT(SGTSL010-IC-TAB)    TO
                             SGTSL010-DT-MVTC-TIT(SGTSL010-IC-TAB)
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-POSTA-RESPOSTA           SECTION.
      *-----------------------------------------------------------------
           MOVE 0011                   TO  MSG1-CD-SEQL-ERRO

           SET PUT-CONTAINER           TO TRUE
           SET ERRO-CONTAINER          TO TRUE
           MOVE 'SGTSL010-RPST'        TO NM-CTNR

           EXEC CICS PUT
              CONTAINER ( NM-CTNR    )
              CHANNEL   ( CANAL      )
              FLENGTH ( LENGTH OF SGTSL010-RPST )
              FROM    ( SGTSL010-RPST )
           END-EXEC

           SET SEM-ERRO                TO TRUE
           .
       300000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970101-ABRE-CURSOR-PSQ          SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO  MSG1-CD-SEQL-ERRO

           EXEC SQL
                OPEN CURSOR-PSQ-1
           END-EXEC

           MOVE 'S'                    TO  IN-CURSOR-PSQ-ABTO
           .
       970101-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970102-LER-CURSOR-PSQ           SECTION.
      *-----------------------------------------------------------------
           MOVE 0009                   TO  MSG1-CD-SEQL-ERRO

           INITIALIZE REG-WS

           EXEC SQL
                FETCH NEXT ROWSET CURSOR-PSQ-1
                         FOR 0101 ROWS
                INTO  :CD-TIP-MVTC-TIT
                    , :TS-REG-PGTO
                    , :DT-PRV-MVT
                      :DT-PRV-MVT-N
                    , :VL-PRVT-MVTC
                      :VL-PRVT-MVTC-N
                    , :DT-MVTC-TIT
                      :DT-MVTC-TIT-N
                    , :VL-MVTC-TIT
                      :VL-MVTC-TIT-N
           END-EXEC

           MOVE SQLERRD(3)             TO  QT-REG-SELD

           IF  QT-REG-SELD EQUAL 0101
               SET SGTSL010-TEM-MAIS-REGISTRO TO TRUE
               MOVE 0100               TO  QT-REG-SELD
           END-IF

           MOVE QT-REG-SELD            TO  SGTSL010-QT-OCR-TAB
           MOVE SQLCODE                TO  CD-FIM-CURSOR-PSQ
           .
       970102-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970103-FECHA-CURSOR-PSQ         SECTION.
      *-----------------------------------------------------------------
           MOVE 0010                   TO  MSG1-CD-SEQL-ERRO

           MOVE 'N'                    TO  IN-CURSOR-PSQ-ABTO

           EXEC SQL
                CLOSE  CURSOR-PSQ-1
           END-EXEC
           .
       970103-SAI.
           EXIT.
      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------
           IF MSG1-CD-ERRO-TS EQUAL ZEROS
              MOVE 8888                TO MSG1-CD-ERRO-TS
           END-IF

           IF MSG1-TX-ERRO-TS EQUAL SPACES
              MOVE 'Erro não especificado no módulo.' TO MSG1-TX-ERRO-TS
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

           MOVE 'SGTSL010-CD-CTRA-TIT-VL'      TO  MSGKW999-NM-VRV-MNT
           MOVE SGTSL010-CD-CTRA-TIT-VL        TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           IF  SGTSL010-CD-OPR-ETRD-MCP        IS  NUMERIC
               MOVE 'SGTSL010-CD-OPR-ETRD-MCP' TO MSGKW999-NM-VRV-MNT
               MOVE  SGTSL010-CD-OPR-ETRD-MCP  TO CD-OPR-ETRD-MCP-DISP
               MOVE  CD-OPR-ETRD-MCP-DISP      TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT
           END-IF

           MOVE 'SGTSL010-NR-VRS-OPR-ETRD'     TO  MSGKW999-NM-VRV-MNT
           MOVE SGTSL010-NR-VRS-OPR-ETRD       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL010-CD-TIP-MVTC-TIT-ULT' TO  MSGKW999-NM-VRV-MNT
           MOVE SGTSL010-CD-TIP-MVTC-TIT-ULT   TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL010-DT-MVTC-TIT-ULT'     TO  MSGKW999-NM-VRV-MNT
           MOVE SGTSL010-DT-MVTC-TIT-ULT       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

      * --> grava o erro no msg
           PERFORM MSG1-NTF-MSG
           MOVE MSG1-CD-ERRO-TS                TO  IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS                TO  IIBKERRO-TX-ERRO

           EXEC CICS PUT
              CONTAINER ( 'PRM-ERRO' )
              CHANNEL   ( CANAL      )
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
-INC MSGKPMG1
      * --------------------------------------*
       999999-ENCERRAR                SECTION.
      * --------------------------------------*
           EXEC CICS
                RETURN
           END-EXEC.