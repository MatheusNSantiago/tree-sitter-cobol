      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSL058
      * ANALISTA..: F0175702 ADRIANO SEIJI NAKAMURA
      * AUTOR.....: F0175702 ADRIANO SEIJI NAKAMURA
      * OBJETIVO..: Listar Tipos de Arquivo da Operação Estruturada
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 08/11/2016 F0175702 DE 2015/137233 - IMPLANTACAO
      *----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSL058.
      *-----------------------------------------------------------------
       ENVIRONMENT                     DIVISION.
      *-----------------------------------------------------------------
       CONFIGURATION                   SECTION.
      *-----------------------------------------------------------------
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *-----------------------------------------------------------------
       DATA                            DIVISION.
      *-----------------------------------------------------------------
       WORKING-STORAGE                 SECTION.
      *-----------------------------------------------------------------
      *-----------------------------------------------------------------
       01  CTE-PROG     PIC  X(17)     VALUE '*** SGTSL058 ***'.
       01  CTE-VERS     PIC  X(06)     VALUE 'VRS001'.

      *-----------------------------------------------------------------
      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informacoes
       COPY HLPKE01B.
       LOCAL-STORAGE SECTION.
      *-----------------------------------------------------------------
      *-----------------------------------------------------------------
      * book de para acionamento do msg1
       COPY IIBKERRO.
       COPY MSGKWMG1.
       COPY HLPKW999.
       01  GRP-CONTAINER.
           03  CANAL                   PIC X(16) VALUE SPACES.
           03  QT-CCT-LIDO             PIC S9(9) COMP-5 VALUE 0.
           03  QT-CCT-EPRO             PIC S9(9) COMP-5 VALUE 0.
           03  VL-EXBR-1               PIC 9(10)        VALUE 0.
           03  VL-EXBR-2               PIC 9(10)        VALUE 0.
      * Area de trabalho

       01 QT-REG-SELD                  PIC S9(4) COMP-5 VALUE 0.

      * Campos utilizados para leitura

       01  REG-WS.
           03 CD-TIP-ARQ               PIC S9(9) COMP-5
           OCCURS 101 TIMES             VALUE 0.
           03 TX-DCR-TIP-ARQ           PIC X(100)
           OCCURS 101 TIMES             VALUE ' '.

      * Indicadores de nulidades

       01 QT-REG-SELD-N                PIC S9(4) COMP-5 VALUE 0.

      * Variavel(is) de controle do cursor(es)

       01  CTL-CURSOR-PSQ.
           03  CD-FIM-CURSOR-PSQ       PIC S9(04) COMP-5 VALUE 0.
               88  FIM-CURSOR-PSQ                        VALUE +100.
           03  IN-CURSOR-PSQ-ABTO      PIC  X(01) VALUE 'N'.
               88  CURSOR-PSQ-ABTO                VALUE 'S'.

      * Subrotinas utilizadas


           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

      *-----------------------------------------------------------------
       COPY SGTKL058.
           EXEC SQL
             DECLARE CURSOR-PSQ CURSOR
             WITH ROWSET POSITIONING FOR
             SELECT
               A.CD_TIP_ARQ
             , A.TX_DCR_TIP_ARQ
             FROM
             DB2SGT.TIP_ARQ_OPR A
             WHERE
             A.CD_TIP_ARQ > :SGTSL058-CD-TIP-ARQ-ENTD
             AND A.CD_TIP_ARQ < 100
      * Os tipos de arquivo maiores que 100 foram criados devido
      * a integridade referencial com ORD_NEGO_OPR, NOTA_TCN_OPR_PRD,
      * ISLC_OPR_ETRD, NOTA_CLSC_PCT_OPR e NOTA_CLSC_PCT_OPR
      * Estes tipos não devem ser exibidos no combobox da tela de
      * manutenção de anexos
             ORDER BY
             A.CD_TIP_ARQ
           END-EXEC.

      *-----------------------------------------------------------------
       PROCEDURE                       DIVISION.
      *-----------------------------------------------------------------


      * Habilita tratamento automatico de erros DB2
       COPY HLPKE997.
      * Habilita captura automatica de abend e erros CICS
       COPY HLPKE999.

           PERFORM 100000-DESCOBRIR-CANAL
           PERFORM 110000-EXCLUIR-CTNR-RPST
           PERFORM 120000-EXCLUIR-CTNR-ERRO
           PERFORM 130000-RECEBER-REQUISICAO
           PERFORM 140000-VALIDAR-PARAMETRO
           PERFORM 140000-VALIDAR-PARAMETRO

           PERFORM 970101-ABRE-CURSOR-PSQ
           PERFORM 970102-LER-CURSOR-PSQ
           PERFORM 200000-PROCESSA-PESQUISA
             VARYING SGTSL058-IC-TAB FROM 1 BY 1
             UNTIL SGTSL058-IC-TAB GREATER SGTSL058-QT-OCR-TAB
           PERFORM 970103-FECHA-CURSOR-PSQ

           PERFORM 300000-POSTA-RESPOSTA
           GOBACK
           .
      *-----------------------------------------------------------------
       100000-DESCOBRIR-CANAL          SECTION.
      *-----------------------------------------------------------------
           MOVE 0001                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS ASSIGN
              CHANNEL ( CANAL )
           END-EXEC
           .
       100000-SAI.
           EXIT.
      *---------------------------------------------------------------
       110000-EXCLUIR-CTNR-RPST        SECTION.
      *---------------------------------------------------------------
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS DELETE
              CONTAINER ( 'SGTSL058-RPST' )
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
           MOVE 0004                   TO MSG1-CD-SEQL-ERRO

           MOVE ZEROS                  TO QT-CCT-LIDO
           MOVE LENGTH OF SGTSL058-RQSC      TO QT-CCT-EPRO
           EXEC CICS GET
              CONTAINER ( 'SGTSL058-RQSC' )
              CHANNEL   ( CANAL      )
              NODATA
              FLENGTH ( QT-CCT-LIDO )
           END-EXEC

           IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
               MOVE 3001                    TO MSG1-CD-ERRO-TS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0001               TO MSG1-CD-ERRO-TS
               MOVE 0005               TO MSG1-CD-SEQL-ERRO
               MOVE QT-CCT-EPRO        TO VL-EXBR-1
               MOVE QT-CCT-LIDO        TO VL-EXBR-2
               STRING
                   'Container RQSC'
                   ', TAMANHO ESPERADO: '  VL-EXBR-1
                   ', TAMANHO RECEBIDO: '  VL-EXBR-2
                   DELIMITED BY SIZE      INTO MSG1-TX-ERRO-TS
               END-STRING
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           EXEC CICS GET
              CONTAINER ( 'SGTSL058-RQSC' )
              CHANNEL   ( CANAL      )
              FLENGTH ( LENGTH OF SGTSL058-RQSC )
              INTO    ( SGTSL058-RQSC )
           END-EXEC
           .
       130000-SAI.
           EXIT.
      *--------------------------------------------------------------
       140000-VALIDAR-PARAMETRO        SECTION.
      *--------------------------------------------------------------
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO
           MOVE 'N'                    TO SGTSL058-IN-CTNC-PSQ
           IF  SGTSL058-CD-TIP-ARQ-ENTD EQUAL ZEROS
               CONTINUE
           END-IF

           .
       140000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       200000-PROCESSA-PESQUISA        SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                    TO MSG1-CD-SEQL-ERRO

           MOVE CD-TIP-ARQ    (SGTSL058-IC-TAB)  TO
                     SGTSL058-CD-TIP-ARQ(SGTSL058-IC-TAB)
           MOVE TX-DCR-TIP-ARQ(SGTSL058-IC-TAB)       TO
                     SGTSL058-TX-DCR-TIP-ARQ(SGTSL058-IC-TAB)
           .
       200000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970101-ABRE-CURSOR-PSQ          SECTION.
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
       970102-LER-CURSOR-PSQ           SECTION.
      *-----------------------------------------------------------------
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO
           INITIALIZE REG-WS
           EXEC SQL
                FETCH NEXT ROWSET
                CURSOR-PSQ FOR 101 ROWS
               INTO :CD-TIP-ARQ
                  , :TX-DCR-TIP-ARQ
           END-EXEC
           MOVE SQLERRD(3)             TO QT-REG-SELD

           IF  QT-REG-SELD EQUAL 0101
               SET SGTSL058-TEM-MAIS-REGISTRO TO TRUE
               MOVE 0100               TO QT-REG-SELD
           END-IF
           MOVE QT-REG-SELD            TO SGTSL058-QT-OCR-TAB
           MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ

           .
       970102-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970103-FECHA-CURSOR-PSQ         SECTION.
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
       300000-POSTA-RESPOSTA           SECTION.
      *-----------------------------------------------------------------
           MOVE 0011                   TO MSG1-CD-SEQL-ERRO
           EXEC CICS PUT
              CONTAINER ( 'SGTSL058-RPST' )
              CHANNEL   ( CANAL      )
              FLENGTH ( LENGTH OF SGTSL058-RPST )
              FROM    ( SGTSL058-RPST )
           END-EXEC
           .
       300000-SAI.
           EXIT.
      *--------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *--------------------------------------------------------------

           MOVE 'SGTSL058-CD-TIP-ARQ-ENTD'  TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL058-CD-TIP-ARQ-ENTD    TO MSGKW999-VL-VRV-MNT
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
       COPY HLPKP999.
      * --------------------------------------*
      * 999010-TRATA-ERRO-DB2          SECTION.
       COPY HLPKP997.
      * --------------------------------------*
      * MSG1-NTF-MSG                   SECTION.
       COPY MSGKPMG1.
      * --------------------------------------*
       999999-ENCERRAR                 SECTION.
      * --------------------------------------*
           GOBACK.
      *---------------------- FIM SGTSL058 -----------------------------
