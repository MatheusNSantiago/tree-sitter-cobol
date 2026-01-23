      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSL042
      * ANALISTA..: F0175702 ADRIANO SEIJI NAKAMURA
      * AUTOR.....: F0175702 ADRIANO SEIJI NAKAMURA
      * OBJETIVO..: Listar as Agências de Classificação de Risco.
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 24/01/2017 F0175702 DE 2016/145195 - IMPLANTACAO
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSL042.

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
       77  CTE-PROG                    PIC  X(17) VALUE
                   '*** SGTSL042 ***'.
       77  CTE-VERS                    PIC  X(06) VALUE 'VRS001'.

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
       01  GRP-CONTAINER.
           03  CANAL                        PIC X(16) VALUE SPACES.

      * Area de trabalho

       01 QT-REG-SELD                       PIC S9(4) COMP-5 VALUE 0.
       01 CD-TIP-EST-DCS                    PIC S9(4) COMP-5 VALUE 2.
       01 AX-IN-AG-CLSC-ATV                 PIC X(1)         VALUE 'S'.

      * Campos utilizados para leitura

       01  REG-WS.
           03 CD-AG-CLSC-RSCO                PIC S9(4) COMP-5
              OCCURS 101 TIMES               VALUE 0.
           03 NR-ETRD-AG-CLSC                PIC S9(4) COMP-5
              OCCURS 101 TIMES               VALUE 0.
           03 IN-AG-CLSC-ATV                 PIC  X(1)
              OCCURS 101 TIMES               VALUE SPACES.
           03 NM-AG-CLSC-RSCO                PIC  X(60)
              OCCURS 101 TIMES               VALUE SPACES.


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

      * Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1
      *-----------------------------------------------------------------
-INC SGTKL042
           EXEC SQL
             DECLARE CURSOR-PSQ-1 CURSOR
                     WITH ROWSET POSITIONING FOR
             SELECT A.NR_ETRD_AG_CLSC
                  , A.CD_AG_CLSC_RSCO
                  , A.NM_AG_CLSC_RSCO
               FROM DB2SGT.AG_CLSC_EXNO_RSCO  A
               INNER JOIN DB2SGT.EST_AG_CLSC_EXNO  B
               ON  A.CD_AG_CLSC_RSCO = B.CD_AG_CLSC_RSCO
               AND A.NR_ETRD_AG_CLSC = B.NR_ETRD_AG_CLSC
               INNER JOIN (
                 SELECT C.CD_AG_CLSC_RSCO
                      , C.NR_ETRD_AG_CLSC
                      , MAX(C.TS_EST_AG_CLSC) AS TS_ATL_AG_CLSC
002360           FROM DB2SGT.EST_AG_CLSC_EXNO  C
                 WHERE C.CD_AG_CLSC_RSCO > :SGTSL042-E-CD-AG-CLSC-RSCO
                 GROUP BY C.CD_AG_CLSC_RSCO
                        , C.NR_ETRD_AG_CLSC
                 ORDER BY C.CD_AG_CLSC_RSCO
                        , C.NR_ETRD_AG_CLSC) Z
               ON  A.CD_AG_CLSC_RSCO = Z.CD_AG_CLSC_RSCO
               AND A.NR_ETRD_AG_CLSC = Z.NR_ETRD_AG_CLSC
               WHERE A.IN_AG_CLSC_ATV = :AX-IN-AG-CLSC-ATV
               AND   B.CD_TIP_EST_DCS = :CD-TIP-EST-DCS
               AND   B.TS_EST_AG_CLSC = Z.TS_ATL_AG_CLSC
      *--> Paginacao
               AND   ((A.CD_AG_CLSC_RSCO > :SGTSL042-E-CD-AG-CLSC-RSCO)
               OR    (A.CD_AG_CLSC_RSCO = :SGTSL042-E-CD-AG-CLSC-RSCO
               AND   A.NR_ETRD_AG_CLSC >= :SGTSL042-E-NR-ETRD-AG-CLSC))
             ORDER BY A.CD_AG_CLSC_RSCO
                    , A.NR_ETRD_AG_CLSC
           END-EXEC.

       77  CTE-FINAL-SS                  PIC X(40)      VALUE
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
                    VARYING SGTSL042-IC-TAB FROM 1 BY 1
                    UNTIL SGTSL042-IC-TAB > SGTSL042-QT-OCR-TAB

           PERFORM 300000-POSTA-RESPOSTA
           GOBACK
           .
      *-----------------------------------------------------------------
       100000-DESCOBRIR-CANAL SECTION.
      *-----------------------------------------------------------------
           MOVE 0001                   TO MSG1-CD-SEQL-ERRO

      *  Por causa da captura automática de erro CICS, deve ser previsto
004370*o erro de container

004380     SET  ERRO-CANAL             TO  TRUE

           EXEC CICS ASSIGN
              CHANNEL ( CANAL )
           END-EXEC

      *    Ao chegar aqui, há certeza de que não houve erro algum
004450     SET  SEM-ERRO               TO  TRUE
           .
       100000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       110000-EXCLUIR-CTNR-RPST SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS DELETE
              CONTAINER ( 'SGTSL042-RPST' )
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
       120000-EXCLUIR-CTNR-ERRO SECTION.
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
       130000-RECEBER-REQUISICAO SECTION.
      *-----------------------------------------------------------------
           MOVE 0004                   TO MSG1-CD-SEQL-ERRO

           MOVE ZEROS                       TO QT-CCT-LIDO
           MOVE LENGTH OF SGTSL042-RQSC      TO QT-CCT-EPRO
           EXEC CICS GET
              CONTAINER ( 'SGTSL042-RQSC' )
              CHANNEL   ( CANAL      )
              NODATA
              FLENGTH ( QT-CCT-LIDO )
           END-EXEC

           IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
               SET   ERRO-TAMANHO-AREA   TO  TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           EXEC CICS GET
              CONTAINER ( 'SGTSL042-RQSC' )
              CHANNEL   ( CANAL      )
              FLENGTH ( LENGTH OF SGTSL042-RQSC )
              INTO    ( SGTSL042-RQSC )
           END-EXEC
           .
       130000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       140000-VALIDAR-PARAMETRO SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO
           MOVE 'N'                    TO SGTSL042-IN-CTNC-PSQ

           IF SGTSL042-E-CD-AG-CLSC-RSCO < ZEROS
              SET  ERRO-PROGRAMADO     TO TRUE
              MOVE 0269                TO MSG1-CD-ERRO-TS
              MOVE 'Código da agência de classificação de risco inválido
      -            '.'
                                       TO MSG1-TX-ERRO-TS
              PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF SGTSL042-E-NR-ETRD-AG-CLSC < ZEROS
              SET  ERRO-PROGRAMADO     TO TRUE
              MOVE 0271                TO MSG1-CD-ERRO-TS
              MOVE 'Número estruturado da agência de classificação de ri
      -            'sco inválido.'
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

           MOVE CD-AG-CLSC-RSCO(SGTSL042-IC-TAB) TO
                     SGTSL042-CD-AG-CLSC-RSCO(SGTSL042-IC-TAB)
           MOVE NR-ETRD-AG-CLSC(SGTSL042-IC-TAB) TO
                     SGTSL042-NR-ETRD-AG-CLSC(SGTSL042-IC-TAB)
           MOVE NM-AG-CLSC-RSCO(SGTSL042-IC-TAB) TO
                     SGTSL042-NM-AG-CLSC-RSCO(SGTSL042-IC-TAB)
           .
       200000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970101-ABRE-CURSOR-PSQ SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

           EXEC SQL
                OPEN CURSOR-PSQ-1
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
                FETCH NEXT ROWSET
                           CURSOR-PSQ-1 FOR 0101 ROWS
                INTO :NR-ETRD-AG-CLSC
                   , :CD-AG-CLSC-RSCO
                   , :NM-AG-CLSC-RSCO
           END-EXEC

           MOVE SQLERRD(3)             TO QT-REG-SELD
           IF  QT-REG-SELD EQUAL 0101
               SET SGTSL042-TEM-MAIS-REGISTRO TO TRUE
               MOVE 0100                TO QT-REG-SELD
           END-IF
           MOVE QT-REG-SELD            TO SGTSL042-QT-OCR-TAB
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
                CLOSE CURSOR-PSQ-1
           END-EXEC
           .
       970103-SAI.
           EXIT.
      *-----------------------------------------------------------------
       300000-POSTA-RESPOSTA SECTION.
      *-----------------------------------------------------------------
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO
           EXEC CICS PUT
              CONTAINER ( 'SGTSL042-RPST' )
              CHANNEL   ( CANAL      )
              FLENGTH ( LENGTH OF SGTSL042-RPST )
              FROM    ( SGTSL042-RPST )
           END-EXEC
           .
       300000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA SECTION.
      *-----------------------------------------------------------------
      *--> Monitora variàveis do pgm, grava e encerra

           IF  MSG1-CD-ERRO-TS             EQUAL  ZEROS
              MOVE  8888               TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS             EQUAL  SPACES
               MOVE  'Erro não especificado no módulo.'
                                       TO  MSG1-TX-ERRO-TS
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
      *==> Observação <=================================================
      *    Normalmente se Monitora apenas as Variáveis da Requisição
      *    para Monitoração da Mensageria.
      *    O Analista que especificou o módulo e o Analista Programador
      *    tem liberdade para Monitorar outras Variáveis que forem
      *    relevantes ao Suporte do Sistema.
      *=================================================================

      *--> Adiciona as variáveis para monitoração

             MOVE 'SGTSL042-E-CD-AG-CLSC-RSCO' TO MSGKW999-NM-VRV-MNT
             MOVE SGTSL042-E-CD-AG-CLSC-RSCO   TO MSGKW999-VL-VRV-MNT
             PERFORM 999003-ADICIONA-VRV-MNT

             MOVE 'SGTSL042-E-NR-ETRD-AG-CLSC' TO MSGKW999-NM-VRV-MNT
             MOVE SGTSL042-E-NR-ETRD-AG-CLSC   TO MSGKW999-VL-VRV-MNT
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
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *---------------------------------------*
-INC SGTKP999
      * --------------------------------------*
       999999-ENCERRAR                SECTION.
      * --------------------------------------*
           EXEC CICS
                RETURN
           END-EXEC.