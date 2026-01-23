      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSL044
      * ANALISTA..: F0175702 ADRIANO SEIJI NAKAMURA
      * AUTOR.....: F0175702 ADRIANO SEIJI NAKAMURA
      * OBJETIVO..: Listar as Notas das Agências de Classificação de
      *             Risco
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 26/01/2017 F0175702 DE 2016/145195 - IMPLANTACAO
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSL044.

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

       77  CTE-PROG     PIC  X(17) VALUE '*** SGTSL044 ***'.

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
           03 NR-SEQL-NOTA-AG                PIC S9(4) COMP-5
              OCCURS 101 TIMES               VALUE 0.
           03 CD-TIP-NOTA-AGPR               PIC S9(4) COMP-5
              OCCURS 101 TIMES               VALUE 0.
           03 SG-NOTA-AG-CLSC                PIC  X(10)
              OCCURS 101 TIMES               VALUE SPACES.
           03 TX-NOTA-AG-CLSC                PIC  X(60)
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
-INC SGTKL044
      *--> Pesquisa as Notas de classificacao de Agencias Ativas
           EXEC SQL
             DECLARE CURSOR-PSQ-1 CURSOR
                     WITH ROWSET POSITIONING FOR
             SELECT A.NR_ETRD_AG_CLSC
                  , A.CD_AG_CLSC_RSCO
                  , A.NR_SEQL_NOTA_AG
                  , A.CD_TIP_NOTA_AGPR
                  , A.SG_NOTA_AG_CLSC
                  , A.TX_NOTA_AG_CLSC
               FROM DB2SGT.NOTA_AG_CLSC_EXNO  A
               INNER JOIN DB2SGT.AG_CLSC_EXNO_RSCO  D
               ON  A.CD_AG_CLSC_RSCO = D.CD_AG_CLSC_RSCO
               AND A.NR_ETRD_AG_CLSC = D.NR_ETRD_AG_CLSC
               INNER JOIN DB2SGT.EST_AG_CLSC_EXNO  B
               ON  A.CD_AG_CLSC_RSCO = B.CD_AG_CLSC_RSCO
               AND A.NR_ETRD_AG_CLSC = B.NR_ETRD_AG_CLSC
               INNER JOIN (
                 SELECT C.CD_AG_CLSC_RSCO
                      , C.NR_ETRD_AG_CLSC
                      , MAX(C.TS_EST_AG_CLSC) AS TS_ATL_AG_CLSC
002360           FROM DB2SGT.EST_AG_CLSC_EXNO  C
                 WHERE C.CD_AG_CLSC_RSCO = :SGTSL044-E-CD-AG-CLSC-RSCO
                 AND   C.NR_ETRD_AG_CLSC = :SGTSL044-E-NR-ETRD-AG-CLSC
                 GROUP BY C.CD_AG_CLSC_RSCO
                        , C.NR_ETRD_AG_CLSC
                 ORDER BY C.CD_AG_CLSC_RSCO
                        , C.NR_ETRD_AG_CLSC) Z
               ON  A.CD_AG_CLSC_RSCO = Z.CD_AG_CLSC_RSCO
               AND A.NR_ETRD_AG_CLSC = Z.NR_ETRD_AG_CLSC
               WHERE D.IN_AG_CLSC_ATV = :AX-IN-AG-CLSC-ATV
               AND   B.CD_TIP_EST_DCS = :CD-TIP-EST-DCS
               AND   B.TS_EST_AG_CLSC = Z.TS_ATL_AG_CLSC
               AND   A.CD_AG_CLSC_RSCO = :SGTSL044-E-CD-AG-CLSC-RSCO
               AND   A.NR_ETRD_AG_CLSC = :SGTSL044-E-NR-ETRD-AG-CLSC
      *--> Paginao
               AND   A.NR_SEQL_NOTA_AG > :SGTSL044-E-NR-SEQL-NOTA-AG
             ORDER BY A.CD_AG_CLSC_RSCO
                    , A.NR_ETRD_AG_CLSC
                    , A.NR_SEQL_NOTA_AG
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
                    VARYING SGTSL044-IC-TAB FROM 1 BY 1
                    UNTIL SGTSL044-IC-TAB > SGTSL044-QT-OCR-TAB

           PERFORM 300000-POSTA-RESPOSTA
           GOBACK
           .
      *-----------------------------------------------------------------
       100000-DESCOBRIR-CANAL SECTION.
      *-----------------------------------------------------------------
           MOVE 0001                   TO MSG1-CD-SEQL-ERRO

      *  Por causa da captura automática de erro CICS, deve ser previsto
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
       110000-EXCLUIR-CTNR-RPST SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS DELETE
              CONTAINER ( 'SGTSL044-RPST' )
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
           MOVE LENGTH OF SGTSL044-RQSC      TO QT-CCT-EPRO
           EXEC CICS GET
              CONTAINER ( 'SGTSL044-RQSC' )
              CHANNEL   ( CANAL      )
              NODATA
              FLENGTH ( QT-CCT-LIDO )
           END-EXEC

           IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
               SET   ERRO-TAMANHO-AREA   TO  TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS GET
              CONTAINER ( 'SGTSL044-RQSC' )
              CHANNEL   ( CANAL      )
              FLENGTH ( LENGTH OF SGTSL044-RQSC )
              INTO    ( SGTSL044-RQSC )
           END-EXEC
           .
       130000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       140000-VALIDAR-PARAMETRO SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO
           MOVE 'N'                    TO SGTSL044-IN-CTNC-PSQ

           IF  SGTSL044-E-CD-AG-CLSC-RSCO EQUAL ZEROS
               SET  ERRO-PROGRAMADO TO TRUE
               MOVE 270                TO  MSG1-CD-ERRO-TS
               MOVE 'Código da agência de classificação de risco não inf
      -             'ormado.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
            END-IF
            IF  SGTSL044-E-NR-ETRD-AG-CLSC EQUAL ZEROS
                SET  ERRO-PROGRAMADO    TO TRUE
                MOVE 272                TO  MSG1-CD-ERRO-TS
                MOVE 'Número estruturado da agência de classificação de
      -              'risco não informado.'
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

           MOVE CD-AG-CLSC-RSCO(SGTSL044-IC-TAB) TO
                     SGTSL044-CD-AG-CLSC-RSCO(SGTSL044-IC-TAB)
           MOVE NR-ETRD-AG-CLSC(SGTSL044-IC-TAB) TO
                     SGTSL044-NR-ETRD-AG-CLSC(SGTSL044-IC-TAB)
           MOVE NR-SEQL-NOTA-AG(SGTSL044-IC-TAB) TO
                     SGTSL044-NR-SEQL-NOTA-AG(SGTSL044-IC-TAB)
           MOVE CD-TIP-NOTA-AGPR(SGTSL044-IC-TAB) TO
                     SGTSL044-CD-TIP-NOTA-AGPR(SGTSL044-IC-TAB)
           MOVE SG-NOTA-AG-CLSC(SGTSL044-IC-TAB) TO
                     SGTSL044-SG-NOTA-AG-CLSC(SGTSL044-IC-TAB)
           MOVE TX-NOTA-AG-CLSC(SGTSL044-IC-TAB) TO
                     SGTSL044-TX-NOTA-AG-CLSC(SGTSL044-IC-TAB)
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
                       , :NR-SEQL-NOTA-AG
                       , :CD-TIP-NOTA-AGPR
                       , :SG-NOTA-AG-CLSC
                       , :TX-NOTA-AG-CLSC
           END-EXEC

           MOVE SQLERRD(3)             TO QT-REG-SELD
           IF  QT-REG-SELD EQUAL 0101
               SET SGTSL044-TEM-MAIS-REGISTRO TO TRUE
               MOVE 0100                TO QT-REG-SELD
           END-IF
           MOVE QT-REG-SELD            TO SGTSL044-QT-OCR-TAB
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
              CONTAINER ( 'SGTSL044-RPST' )
              CHANNEL   ( CANAL      )
              FLENGTH ( LENGTH OF SGTSL044-RPST )
              FROM    ( SGTSL044-RPST )
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

           MOVE 'SGTSL044-E-CD-AG-CLSC-RSCO' TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL044-E-CD-AG-CLSC-RSCO   TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL044-E-NR-ETRD-AG-CLSC' TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL044-E-NR-ETRD-AG-CLSC   TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL044-E-NR-SEQL-NOTA-AG' TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL044-E-NR-SEQL-NOTA-AG   TO MSGKW999-VL-VRV-MNT
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