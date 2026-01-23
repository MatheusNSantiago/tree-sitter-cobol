       CBL ARITH(EXTEND)
      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSL094
      * ANALISTA..: F8785286 - ROSELI AKEMI KAWAHATA
      * AUTOR.....: F8785286 - ROSELI AKEMI KAWAHATA
      * OBJETIVO..: Consulta da Tabela IFLC_MNL_LIM_EXCO
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 13.11.2018 - F8785286 - DE 2015/137233 - IMPLANTACAO
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSL094.
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
       77  CTE-INICIO-SS               PIC  X(40)  VALUE
                   '*** S.S. COMECA AQUI ***'.
       01  CTE-PROG     PIC  X(17) VALUE '*** SGTSL094 ***'.
       01  CTE-VERS     PIC  X(06) VALUE 'VRS001'.

      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informacoes
-INC HLPKE01B
      *-----------------------------------------------------------------
       LOCAL-STORAGE SECTION.
      *-----------------------------------------------------------------
      * Book de para acionamento do MSG1
-INC IIBKERRO
-INC MSGKWMG1
-INC HLPKW999

      *--> Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1

      * Area de trabalho
       01  QT-REG-SELD                      PIC S9(4) COMP-5 VALUE 0.

      * Indicadores de nulidades
       01  QT-REG-SELD-NULO                 PIC S9(4) COMP-5 VALUE 0.

      * Campos utilizados para leitura
       01  REG-WS.
           03 CD-CTRA-TIT-VL               PIC S9(9) COMP-5
              OCCURS 101 TIMES             VALUE 0.
           03 CD-TIT-OPR-ETRD              PIC X(12)
              OCCURS 101 TIMES             VALUE ' '.
           03 NR-DVS-TIT                   PIC S9(4) COMP-5
              OCCURS 101 TIMES             VALUE 0.
           03 TS-ATL-IFLC-MNL              PIC  X(26)
              OCCURS 101 TIMES             VALUE ' '.
           03 IN-IFLC-MNL-EXCO             PIC X(1)
              OCCURS 101 TIMES             VALUE ' '.
           03 CD-USU-RSP-ULT-ATL           PIC X(8)
              OCCURS 101 TIMES             VALUE ' '.

      * Variavel(is) de controle do cursor(es)

       01  CTL-CURSOR-PSQ.
           03  CD-FIM-CURSOR-PSQ            PIC S9(04) COMP-5 VALUE 0.
               88  FIM-CURSOR-PSQ            VALUE +100.
           03  IN-CURSOR-PSQ-ABTO           PIC  X(01) VALUE 'N'.
               88  CURSOR-PSQ-ABTO          VALUE 'S'.

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

-INC SGTKL094
      *-----------------------------------------------------------------
      * Consulta registros de impacto manual para o título
      *-----------------------------------------------------------------
           EXEC SQL
             DECLARE CURSOR-PSQ-1 CURSOR
                     WITH ROWSET POSITIONING FOR
             SELECT A.CD_CTRA_TIT_VL
                  , A.CD_TIT_OPR_ETRD
                  , A.NR_DVS_TIT
                  , A.TS_ATL_IFLC_MNL
                  , A.IN_IFLC_MNL_EXCO
                  , A.CD_USU_RSP_ULT_ATL
               FROM DB2SGT.IFLC_MNL_LIM_EXCO   A
              WHERE A.CD_CTRA_TIT_VL    = :SGTSL094-E-CD-CTRA-TIT-VL
                AND A.CD_TIT_OPR_ETRD   = :SGTSL094-E-CD-TIT-OPR-ETRD
                AND A.NR_DVS_TIT        = :SGTSL094-E-NR-DVS-TIT
                AND A.TS_ATL_IFLC_MNL  >= :SGTSL094-E-TS-ATL-IFLC-MNL
                AND A.IN_IFLC_MNL_EXCO  = 'S'
                AND A.TS_ATL_IFLC_MNL  = (SELECT MAX (B.TS_ATL_IFLC_MNL)
                   FROM  DB2SGT.IFLC_MNL_LIM_EXCO   B
                   WHERE B.CD_CTRA_TIT_VL   =  A.CD_CTRA_TIT_VL
                   AND   B.CD_TIT_OPR_ETRD  =  A.CD_TIT_OPR_ETRD
                   AND   B.NR_DVS_TIT       =  A.NR_DVS_TIT)
             ORDER BY A.TS_ATL_IFLC_MNL  DESC
           END-EXEC.

        01  CTE-FINAL-SS                PIC  X(40)      VALUE
                    '*** S.S. TERMINA AQUI ***'.

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL          SECTION.

           IF NOT EXECUCAO-ONLINE AND NOT EXECUCAO-BATCH
             CALL CICS5000 USING CICS5000-COMMAREA
           END-IF

           MOVE 0001                   TO MSG1-CD-SEQL-ERRO

      * Habilita tratamento automatico de Erros DB2
-INC HLPKE997
      * Habilita captura automatica de Abend e Erros CICS
-INC HLPKE999

           PERFORM 100000-DESCOBRIR-CANAL
           PERFORM 110000-EXCLUIR-CTNR-RPST
           PERFORM 120000-EXCLUIR-CTNR-ERRO
           PERFORM 130000-RECEBER-REQUISICAO
           PERFORM 140000-VALIDA-PARAMETRO

           PERFORM 970101-ABRE-CURSOR-PSQ
           PERFORM 970102-LER-CURSOR-PSQ
           PERFORM 970103-FECHA-CURSOR-PSQ
           PERFORM 200000-PROCESSA-PESQUISA
                    VARYING SGTSL094-IC-TAB FROM 1 BY 1
                    UNTIL SGTSL094-IC-TAB > SGTSL094-QT-OCR-TAB

           PERFORM 300000-POSTA-RESPOSTA

           GOBACK
           .
       000000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       100000-DESCOBRIR-CANAL SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO

           SET  ERRO-CANAL             TO TRUE

           EXEC CICS ASSIGN
                CHANNEL ( NM-CNL )
           END-EXEC

           SET  SEM-ERRO               TO TRUE
           .
       100000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       110000-EXCLUIR-CTNR-RPST SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS DELETE
              CONTAINER ( 'SGTSL094-RPST' )
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
           MOVE 0004                   TO MSG1-CD-SEQL-ERRO

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
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

           MOVE ZEROS                       TO QT-CCT-LIDO
           MOVE LENGTH OF SGTSL094-RQSC      TO QT-CCT-EPRO

           SET GET-CONTAINER           TO TRUE
           SET ERRO-CONTAINER          TO TRUE
           MOVE 'SGTSL094-RQSC'        TO NM-CTNR

           EXEC CICS GET
              CONTAINER ( NM-CTNR         )
              CHANNEL   ( NM-CNL          )
              NODATA
              FLENGTH   ( QT-CCT-LIDO     )
           END-EXEC

           IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
               SET  ERRO-TAMANHO-AREA     TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS GET
              CONTAINER ( NM-CTNR                 )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSL094-RQSC )
              INTO      ( SGTSL094-RQSC           )
           END-EXEC

      ** Este ponto indica que não houve erro de cics.
           SET SEM-ERRO        TO TRUE

           .
       130000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       140000-VALIDA-PARAMETRO SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

           MOVE 'N'                    TO SGTSL094-IN-CTNC-PSQ

           IF  SGTSL094-E-CD-TIP-SRVC  EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 107                TO  MSG1-CD-ERRO-TS
               MOVE 'Código tipo serviço inválido.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL094-E-CD-CTRA-TIT-VL EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 001                TO  MSG1-CD-ERRO-TS
               MOVE 'Código carteira título valor inválido.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL094-E-CD-TIT-OPR-ETRD EQUAL SPACES
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0231               TO MSG1-CD-ERRO-TS
               MOVE 'Código do título da operação estruturada não inform
      -         'ado.'                 TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL094-E-NR-DVS-TIT   EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0013               TO  MSG1-CD-ERRO-TS
               MOVE ' Número divisão do título inválido.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL094-E-TS-ATL-IFLC-MNL  EQUAL 'CURRENT'
               MOVE   '1900-01-01-00.00.00.000000'
                                       TO  SGTSL094-E-TS-ATL-IFLC-MNL
           END-IF
           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-PROCESSA-PESQUISA SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

           MOVE CD-CTRA-TIT-VL (SGTSL094-IC-TAB) TO
                     SGTSL094-CD-CTRA-TIT-VL (SGTSL094-IC-TAB)
           MOVE CD-TIT-OPR-ETRD (SGTSL094-IC-TAB) TO
                     SGTSL094-CD-TIT-OPR-ETRD (SGTSL094-IC-TAB)
           MOVE NR-DVS-TIT (SGTSL094-IC-TAB)       TO
                     SGTSL094-NR-DVS-TIT (SGTSL094-IC-TAB)
           MOVE TS-ATL-IFLC-MNL(SGTSL094-IC-TAB)   TO
                     SGTSL094-TS-ATL-IFLC-MNL(SGTSL094-IC-TAB)
           MOVE IN-IFLC-MNL-EXCO(SGTSL094-IC-TAB)        TO
                     SGTSL094-IN-IFLC-MNL-EXCO(SGTSL094-IC-TAB)
           MOVE CD-USU-RSP-ULT-ATL(SGTSL094-IC-TAB) TO
                     SGTSL094-CD-USU-RSP-ULT-ATL(SGTSL094-IC-TAB)
           .
       200000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       300000-POSTA-RESPOSTA SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS PUT
              CONTAINER ( 'SGTSL094-RPST' )
              CHANNEL   ( NM-CNL      )
              FLENGTH ( LENGTH OF SGTSL094-RPST )
              FROM    ( SGTSL094-RPST )
           END-EXEC
           .
       300000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970101-ABRE-CURSOR-PSQ SECTION.
      *-----------------------------------------------------------------
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO

           EVALUATE SGTSL094-E-CD-TIP-SRVC
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
       970102-LER-CURSOR-PSQ SECTION.
      *-----------------------------------------------------------------
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE REG-WS

           EVALUATE SGTSL094-E-CD-TIP-SRVC
           WHEN 1
               EXEC SQL
                    FETCH NEXT ROWSET
                          CURSOR-PSQ-1 FOR 0101 ROWS
                    INTO :CD-CTRA-TIT-VL
                       , :CD-TIT-OPR-ETRD
                       , :NR-DVS-TIT
                       , :TS-ATL-IFLC-MNL
                       , :IN-IFLC-MNL-EXCO
                       , :CD-USU-RSP-ULT-ATL
               END-EXEC

           END-EVALUATE

           MOVE SQLERRD(3)             TO QT-REG-SELD

           IF  QT-REG-SELD EQUAL 0101
               SET SGTSL094-TEM-MAIS-REGISTRO TO TRUE
               MOVE 0100                TO QT-REG-SELD
           END-IF

           MOVE QT-REG-SELD            TO SGTSL094-QT-OCR-TAB
           MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ
           .
       970102-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970103-FECHA-CURSOR-PSQ SECTION.
      *-----------------------------------------------------------------
           MOVE 0011                   TO MSG1-CD-SEQL-ERRO

           MOVE 'N'                    TO IN-CURSOR-PSQ-ABTO
           EVALUATE SGTSL094-E-CD-TIP-SRVC
           WHEN 1
               EXEC SQL
                   CLOSE CURSOR-PSQ-1
               END-EXEC
           END-EVALUATE
           .
       970103-SAI.
           EXIT.
      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA SECTION.
      *-----------------------------------------------------------------
           IF  IN-CURSOR-PSQ-ABTO  EQUAL  'S'
               PERFORM 970103-FECHA-CURSOR-PSQ
           END-IF

           IF  MSG1-CD-ERRO-TS             EQUAL  ZEROS
               MOVE  8888                    TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS             EQUAL  SPACES
               MOVE  'Erro não especificado no módulo.'
                                             TO  MSG1-TX-ERRO-TS
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

           MOVE 'SGTSL094-E-CD-TIP-SRVC'     TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL094-E-CD-TIP-SRVC       TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL094-E-CD-CTRA-TIT-VL'  TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL094-E-CD-CTRA-TIT-VL    TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL094-E-CD-TIT-OPR-ETRD' TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL094-E-CD-TIT-OPR-ETRD   TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL094-E-NR-DVS-TIT'      TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL094-E-NR-DVS-TIT        TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL094-E-TS-ATL-IFLC-MNL' TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL094-E-TS-ATL-IFLC-MNL   TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

      * --> grava o erro no msg
           PERFORM MSG1-NTF-MSG
           MOVE MSG1-CD-ERRO-TS              TO IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS              TO IIBKERRO-TX-ERRO
           EXEC CICS PUT
              CONTAINER ( 'PRM-ERRO' )
              CHANNEL   ( NM-CNL      )
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