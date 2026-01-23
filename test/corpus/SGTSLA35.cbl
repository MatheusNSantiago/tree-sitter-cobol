      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSLA35
      * ANALISTA..: IVONE DE MELO COSTA RIBEIRO
      * AUTOR.....: IVONE DE MELO COSTA RIBEIRO
      * OBJETIVO..: LISTA TABELA  DB2SGT.CTT_EXNO_OPR_ETRD
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 25/10/2016 F4535118 2016/145195 - Implantação
      *-----------------------------------------------------------------

       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSLA35.
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
       77  CTE-INICIO-SS               PIC  X(40) VALUE
               '*** Storage Section começa aqui ***'.
       77  CTE-PROG     PIC  X(17) VALUE '*** SGTSLA35 ***'.
       77  CTE-VERS     PIC  X(06) VALUE 'VRS001'.

      *---------------------------------------------------------------
      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informacoes
-INC HLPKE01B
       LOCAL-STORAGE SECTION.
      *---------------------------------------------------------------
      *---------------------------------------------------------------
      * book de para acionamento do msg1
-INC IIBKERRO
-INC MSGKWMG1
-INC HLPKW999
       01  GRP-CONTAINER.
           03  CANAL                        PIC X(16) VALUE SPACES.
           03  QT-CCT-LIDO                  PIC S9(9) COMP-5 VALUE 0.
           03  QT-CCT-EPRO                  PIC S9(9) COMP-5 VALUE 0.
           03  VL-EXBR-1                    PIC 9(10)        VALUE 0.
           03  VL-EXBR-2                    PIC 9(10)        VALUE 0.
      * Area de trabalho

       01 QT-REG-SELD                         PIC S9(4) COMP-5 VALUE 0.

      *--> Auxiliares Movimentacao Monitoração
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).

      * Campos utilizados para leitura

       01  REG-WS.
           03 CD-PSS-CTT-EXNO                PIC S9(4) COMP-5 VALUE 0.
           03 CD-USU-RSP-ATL                 PIC X(8) VALUE ' '.
           03 TS-ATL-CTT-EXNO                PIC X(26) VALUE ' '.

      * Indicadores de nulidades

       01 QT-REG-SELD-N                       PIC S9(4) COMP-5 VALUE 0.

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
-INC SGTKLA35
           EXEC SQL
             DECLARE CURSOR-PSQ CURSOR  FOR
             SELECT A.CD_PSS_CTT_EXNO
                  , A.CD_USU_RSP_ATL
                  , A.TS_ATL_CTT_EXNO
               FROM DB2SGT.CTT_EXNO_OPR_ETRD  A
              WHERE A.CD_CTRA_TIT_VL     = :SGTSLA35-CD-CTRA-TIT-VL
                AND A.CD_OPR_ETRD_MC     = :SGTSLA35-CD-OPR-ETRD-MC
                AND A.NR_VRS_OPR_ETRD    = :SGTSLA35-NR-VRS-OPR-ETRD
                AND A.CD_PSS_CTT_EXNO    > :SGTSLA35-CD-PSS-CTT-ULT
              ORDER BY
                    A.CD_PSS_CTT_EXNO
           END-EXEC.

       01  CTE-FINAL-SS                PIC X(40) VALUE
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
           PERFORM 200000-PROCESSA-PESQUISA UNTIL FIM-CURSOR-PSQ
                    OR SGTSLA35-PREENCHIDO-TAB
           IF  NOT FIM-CURSOR-PSQ
               MOVE 'S'                TO SGTSLA35-IN-CNTO-PSQ
           END-IF
           PERFORM 970103-FECHA-CURSOR-PSQ

           PERFORM 300000-POSTA-RESPOSTA
           GOBACK
           .
      *-----------------------------------------------------------------
       100000-DESCOBRIR-CANAL SECTION.
      *-----------------------------------------------------------------
           MOVE 0001                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS ASSIGN
              CHANNEL ( CANAL )
           END-EXEC
           .
       100000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       110000-EXCLUIR-CTNR-RPST SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS DELETE
              CONTAINER ( 'SGTSLA35-RPST' )
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
      *---------------------------------------------------------------
       120000-EXCLUIR-CTNR-ERRO SECTION.
      *---------------------------------------------------------------
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
       130000-RECEBER-REQUISICAO SECTION.
      *-----------------------------------------------------------------
           MOVE 0004                   TO MSG1-CD-SEQL-ERRO

           MOVE ZEROS                       TO QT-CCT-LIDO
           MOVE LENGTH OF SGTSLA35-RQSC      TO QT-CCT-EPRO
           EXEC CICS GET
              CONTAINER ( 'SGTSLA35-RQSC' )
              CHANNEL   ( CANAL      )
              NODATA
              FLENGTH ( QT-CCT-LIDO )
           END-EXEC

           IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
               MOVE 9001                    TO MSG1-CD-ERRO-TS
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE QT-CCT-EPRO             TO VL-EXBR-1
               MOVE QT-CCT-LIDO             TO VL-EXBR-2
               STRING 'Erro no tamanho de área de requisição '
                   ', TAMANHO ESPERADO: '  VL-EXBR-1
                   ', TAMANHO RECEBIDO: '  VL-EXBR-2
                   DELIMITED BY SIZE      INTO MSG1-TX-ERRO-TS
               END-STRING
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           EXEC CICS GET
              CONTAINER ( 'SGTSLA35-RQSC' )
              CHANNEL   ( CANAL      )
              FLENGTH ( LENGTH OF SGTSLA35-RQSC )
              INTO    ( SGTSLA35-RQSC )
           END-EXEC
           .
       130000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       140000-VALIDAR-PARAMETRO SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO
           MOVE 'N'                    TO SGTSLA35-IN-CNTO-PSQ
           IF  SGTSLA35-CD-CTRA-TIT-VL EQUAL ZEROS
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 0001                TO MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de título e valores mobiliários
      -             'inválido.'
                                         TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSLA35-CD-OPR-ETRD-MC NOT NUMERIC
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 0003                TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -             ' numérico.'
                                         TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           IF  SGTSLA35-CD-OPR-ETRD-MC EQUAL ZEROS
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 0004                TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais inv
      -             'álido.'
                                         TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           IF  SGTSLA35-NR-VRS-OPR-ETRD EQUAL ZEROS
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 0006                TO MSG1-CD-ERRO-TS
               MOVE 'Número da versão da operação estruturada inválido.'
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

           ADD  1                      TO SGTSLA35-QT-OCR-TAB
                                          SGTSLA35-IC-TAB
           IF  SGTSLA35-ESTOURO-TAB
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 0192                TO MSG1-CD-ERRO-TS
               MOVE 'Tabela da linkage estourou.'  TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           MOVE CD-PSS-CTT-EXNO              TO
                     SGTSLA35-CD-PSS-CTT-EXNO(SGTSLA35-IC-TAB)

           MOVE SGTSLA35-CD-PSS-CTT-EXNO(SGTSLA35-IC-TAB)  TO
                     SGTSLA35-CD-PSS-CTT-ULT

           MOVE CD-USU-RSP-ATL               TO
                     SGTSLA35-CD-USU-RSP-ATL(SGTSLA35-IC-TAB)
           MOVE TS-ATL-CTT-EXNO              TO
                     SGTSLA35-TS-ATL-CTT-EXNO(SGTSLA35-IC-TAB)
           PERFORM 970102-LER-CURSOR-PSQ
           .
       200000-SAI.
           EXIT.
      *----------------------------------------------------------------
       970101-ABRE-CURSOR-PSQ SECTION.
      *----------------------------------------------------------------
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO
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
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO
           INITIALIZE REG-WS
           EXEC SQL
                FETCH CURSOR-PSQ
               INTO :CD-PSS-CTT-EXNO
                  , :CD-USU-RSP-ATL
                  , :TS-ATL-CTT-EXNO
           END-EXEC
           MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ
           .
       970102-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970103-FECHA-CURSOR-PSQ SECTION.
      *-----------------------------------------------------------------
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO
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
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO
           EXEC CICS PUT
              CONTAINER ( 'SGTSLA35-RPST' )
              CHANNEL   ( CANAL      )
              FLENGTH ( LENGTH OF SGTSLA35-RPST )
              FROM    ( SGTSLA35-RPST )
           END-EXEC
           .
       300000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA SECTION.
      *-----------------------------------------------------------------

           MOVE 'SGTSLA35-CD-CTRA-TIT-VL'    TO MSGKW999-NM-VRV-MNT
           MOVE SGTSLA35-CD-CTRA-TIT-VL      TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT
           MOVE 'SGTSLA35-CD-OPR-ETRD-MC'    TO MSGKW999-NM-VRV-MNT
           IF   SGTSLA35-CD-OPR-ETRD-MC      IS NUMERIC
                MOVE SGTSLA35-CD-OPR-ETRD-MC      TO CD-OPR-ETRD-MCP
                MOVE CD-OPR-ETRD-MCP              TO MSGKW999-VL-VRV-MNT
           ELSE
                MOVE 'Código de operação de mercado de capitais não numé
      -              'rico.'                      TO MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM 999003-ADICIONA-VRV-MNT
           MOVE 'SGTSLA35-NR-VRS-OPR-ETRD'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSLA35-NR-VRS-OPR-ETRD     TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT
           MOVE 'SGTSLA35-CD-PSS-CTT-ULT'    TO MSGKW999-NM-VRV-MNT
           MOVE SGTSLA35-CD-PSS-CTT-ULT      TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT
      * --> grava o erro no msg
           PERFORM MSG1-NTF-MSG
           MOVE MSG1-CD-ERRO-TS             TO IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS             TO IIBKERRO-TX-ERRO
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
       999999-SAI.
            EXIT.
      *---------------------- FIM SGTSLA35 -----------------------------