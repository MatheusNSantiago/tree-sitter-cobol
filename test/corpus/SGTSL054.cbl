      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSL054
      * ANALISTA..: F0175702 ADRIANO SEIJI NAKAMURA
      * AUTOR.....: F0175702 ADRIANO SEIJI NAKAMURA
      * OBJETIVO..: Listar Tipo de Documento da Nota Técnica
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 17/09/2018 F8090555 - Acerto CURSOR-PSQ-1, para WHERE
      *          pegar Tiop Doc >= entrada.
      *        21/02/2018 F8785286 DE 2016/145195 - Acerto dos códigos
      *          das mensagens para mensageria.
      *        16/11/2016 F0175702 DE 2016/145195 - IMPLANTACAO
      *----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSL054.
      *-----------------------------------------------------------------
       ENVIRONMENT                     DIVISION.
      *-----------------------------------------------------------------
       CONFIGURATION                   SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *-----------------------------------------------------------------
       DATA                            DIVISION.
      *-----------------------------------------------------------------
       WORKING-STORAGE                 SECTION.
      *-----------------------------------------------------------------
       01  CTE-PROG     PIC  X(17)     VALUE '*** SGTSL054 ***'.
       01  CTE-VERS     PIC  X(06)     VALUE 'VRS001'.

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

       01  GRP-CONTAINER.
           03  CANAL                   PIC X(16) VALUE SPACES.
      *    03  QT-CCT-LIDO             PIC S9(9) COMP-5 VALUE 0.
      *    03  QT-CCT-EPRO             PIC S9(9) COMP-5 VALUE 0.
      *    03  VL-EXBR-1               PIC 9(10)        VALUE 0.
      *    03  VL-EXBR-2               PIC 9(10)        VALUE 0.

      * Area de trabalho
       01 QT-REG-SELD                  PIC S9(4) COMP-5 VALUE 0.

      * Campos utilizados para leitura
       01  REG-WS.
           03 CD-TIP-DOC-TCN           PIC S9(4) COMP-5
           OCCURS 101 TIMES            VALUE 0.
           03 TX-TIP-DOC-TCN           PIC X(40)
           OCCURS 101 TIMES            VALUE ' '.

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

      * Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1
      *-----------------------------------------------------------------
-INC SGTKL054
      *--> Caso haja mais que 100 occ, baixa deve pegar ult-occ e chamar
           EXEC SQL
             DECLARE CURSOR-PSQ-1 CURSOR
             WITH ROWSET POSITIONING FOR
             SELECT
               A.CD_TIP_DOC_TCN
             , A.TX_TIP_DOC_TCN
             FROM DB2SGT.TIP_DOC_NOTA_TCN  A
             WHERE
             A.CD_TIP_DOC_TCN >= :SGTSL054-E-CD-TIP-DOC-TCN
           END-EXEC.

      *-----------------------------------------------------------------
       PROCEDURE                       DIVISION.
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

           PERFORM 200000-PROCESSA-PESQUISA
             VARYING SGTSL054-IC-TAB FROM 1 BY 1
             UNTIL SGTSL054-IC-TAB GREATER SGTSL054-QT-OCR-TAB

           PERFORM 970103-FECHA-CURSOR-PSQ

           PERFORM 300000-POSTA-RESPOSTA

           GOBACK
           .
       000000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       100000-DESCOBRIR-CANAL          SECTION.
      *-----------------------------------------------------------------
           MOVE 0001                   TO MSG1-CD-SEQL-ERRO

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
      *---------------------------------------------------------------
       110000-EXCLUIR-CTNR-RPST        SECTION.
      *---------------------------------------------------------------
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS DELETE
              CONTAINER ( 'SGTSL054-RPST' )
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
           MOVE LENGTH OF SGTSL054-RQSC      TO QT-CCT-EPRO

           EXEC CICS GET
              CONTAINER ( 'SGTSL054-RQSC' )
              CHANNEL   ( CANAL      )
              NODATA
              FLENGTH ( QT-CCT-LIDO )
           END-EXEC

           IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
               SET   ERRO-TAMANHO-AREA     TO  TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS GET
              CONTAINER ( 'SGTSL054-RQSC' )
              CHANNEL   ( CANAL      )
              FLENGTH ( LENGTH OF SGTSL054-RQSC )
              INTO    ( SGTSL054-RQSC )
           END-EXEC
           .
       130000-SAI.
           EXIT.
      *--------------------------------------------------------------
       140000-VALIDAR-PARAMETRO        SECTION.
      *--------------------------------------------------------------
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

           MOVE 'N'                    TO SGTSL054-IN-CTNC-PSQ

           EVALUATE SGTSL054-CD-TIP-SRVC
           WHEN 1
               IF  SGTSL054-E-CD-TIP-DOC-TCN EQUAL ZEROS
                   SET  ERRO-PROGRAMADO     TO TRUE
                   MOVE 0308                TO MSG1-CD-ERRO-TS
                   MOVE 'Codigo tipo documento invalido'
                                        TO MSG1-TX-ERRO-TS
                   PERFORM 999000-GRAVA-ERRO-E-ENCERRA
               END-IF
           END-EVALUATE
           .
       140000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       200000-PROCESSA-PESQUISA        SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

           MOVE CD-TIP-DOC-TCN         (SGTSL054-IC-TAB)
                             TO SGTSL054-CD-TIP-DOC-TCN(SGTSL054-IC-TAB)
           MOVE TX-TIP-DOC-TCN         (SGTSL054-IC-TAB)
                             TO SGTSL054-TX-TIP-DOC-TCN(SGTSL054-IC-TAB)
           .
       200000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970101-ABRE-CURSOR-PSQ          SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

           EVALUATE SGTSL054-CD-TIP-SRVC
           WHEN 1
               EXEC SQL
                   OPEN CURSOR-PSQ-1
               END-EXEC
           END-EVALUATE

           MOVE 'S'                    TO IN-CURSOR-PSQ-ABTO
           .
       970101-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970102-LER-CURSOR-PSQ           SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE REG-WS

           EVALUATE SGTSL054-CD-TIP-SRVC
           WHEN 1
               EXEC SQL
                   FETCH NEXT ROWSET
                   CURSOR-PSQ-1 FOR 101 ROWS
                   INTO :CD-TIP-DOC-TCN
                      , :TX-TIP-DOC-TCN
               END-EXEC
           END-EVALUATE

           MOVE SQLERRD(3)             TO QT-REG-SELD

           IF  QT-REG-SELD EQUAL 0101
               SET SGTSL054-TEM-MAIS-REGISTRO TO TRUE
               MOVE 0100               TO QT-REG-SELD
           END-IF

           MOVE QT-REG-SELD            TO SGTSL054-QT-OCR-TAB
           MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ
           .
       970102-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970103-FECHA-CURSOR-PSQ         SECTION.
      *-----------------------------------------------------------------
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO

           MOVE 'N'                    TO IN-CURSOR-PSQ-ABTO

           EVALUATE SGTSL054-CD-TIP-SRVC
           WHEN 1
               EXEC SQL
                   CLOSE CURSOR-PSQ-1
               END-EXEC
           END-EVALUATE
           .
       970103-SAI.
           EXIT.
      *-----------------------------------------------------------------
       300000-POSTA-RESPOSTA           SECTION.
      *-----------------------------------------------------------------
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS PUT
              CONTAINER ( 'SGTSL054-RPST' )
              CHANNEL   ( CANAL      )
              FLENGTH ( LENGTH OF SGTSL054-RPST )
              FROM    ( SGTSL054-RPST )
           END-EXEC
           .
       300000-SAI.
           EXIT.
      *--------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *--------------------------------------------------------------

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

           MOVE 'SGTSL054-E-CD-TIP-DOC-TCN'  TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL054-E-CD-TIP-DOC-TCN    TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL054-CD-TIP-SRVC'       TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL054-CD-TIP-SRVC         TO MSGKW999-VL-VRV-MNT
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
       999999-ENCERRAR                 SECTION.
      * --------------------------------------*
           EXEC CICS
                RETURN
           END-EXEC
           .
       999999-SAI.
           EXIT.
      *---------------------- FIM SGTSL054 -----------------------------