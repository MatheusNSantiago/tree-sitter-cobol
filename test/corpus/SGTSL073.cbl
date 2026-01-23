000760*-----------------------------------------------------------------
000760* PROGRAMA..: SGTSL073
000760* ANALISTA..: F0175702 ADRIANO SEIJI NAKAMURA
000760* AUTOR.....: F0175702 ADRIANO SEIJI NAKAMURA
000760* OBJETIVO..: Listar Tabela NOTA_CLSC_PCT_OPR
000760* COMPILACAO: COBOL/CICS/DB2
000760*-----------------------------------------------------------------
001254* VRS001 17.08.2018 - F8785286 DE 2016/145195 - Inclusão do cpo
001254*        IN_NOTA_PCT_CNCD no serviço 1 IN-CNCT-NOTA-CLSC.
001254*        13.07.2018 - F2804421 DE 2016/145195 - Acerto mensageria
000760*        13.09.2017 - F8785286 DE 2016/145195 -Inclusão de consis-
000730*         tência do campo IN_NOTA_PCT_CNCD = N em serv. 2
000760*        12.06.2017 - F8785286 DE 2016/145195 - Exclusão do campo
000760*        DT-ATBC-NOTA-CLSC e inclusão dos campos
000760*        SGTSL073-DT-INC-ATBC-NOTA  e  SGTSL073-DT-FIM-ATBC-NOTA
000760*        03.03.2017 - F0175702 DE 2016/145195 - IMPLANTACAO
000760*-----------------------------------------------------------------
000760 IDENTIFICATION DIVISION.
000760*-----------------------------------------------------------------
000760 PROGRAM-ID. SGTSL073.
000760*-----------------------------------------------------------------
000760 ENVIRONMENT DIVISION.
000760*-----------------------------------------------------------------
000760 CONFIGURATION SECTION.
000760 SPECIAL-NAMES.
000760     DECIMAL-POINT IS COMMA.
000760*-----------------------------------------------------------------
000760 DATA DIVISION.
000760*-----------------------------------------------------------------
000760 WORKING-STORAGE SECTION.
000760*-----------------------------------------------------------------
000760 01  CTE-PROG     PIC  X(17) VALUE '*** SGTSL073 ***'.
000760 01  CTE-VERS     PIC  X(06) VALUE 'VRS001'.
000760*-----------------------------------------------------------------
000760* Book do programa de ambiente - CICS5000
000760* Definido na work para preservar as informacoes
-INC HLPKE01B
000760 LOCAL-STORAGE SECTION.
000760*-----------------------------------------------------------------
000760* book de para acionamento do msg1
-INC IIBKERRO
-INC MSGKWMG1
-INC HLPKW999
-INC SGTKERR1
000760
000760 01 QT-REG-SELD                  PIC S9(4) COMP-5 VALUE 0.

000760* Campos utilizados para leitura
000760 01  REG-WS.
000760     03 CD-AG-CLSC-RSCO          PIC S9(4) COMP-5
000760        OCCURS 101 TIMES         VALUE 0.
000760     03 CD-ARQ-OPR-ETRD          PIC S9(9) COMP-5
000760        OCCURS 101 TIMES         VALUE 0.
000760     03 CD-CLI                   PIC S9(9) COMP-5
000760        OCCURS 101 TIMES         VALUE 0.
000760     03 CD-ARQ-OPR-ETRD-N        PIC S9(4) COMP-5
000760        OCCURS 101 TIMES         VALUE 0.
000760     03 CD-USU-ULT-ATL           PIC X(8)
000760        OCCURS 101 TIMES         VALUE ' '.
000760     03 NR-ETRD-AG-CLSC          PIC S9(4) COMP-5
000760        OCCURS 101 TIMES         VALUE 0.
000760     03 NR-SEQL-NOTA-PCT         PIC S9(4) COMP-5
000760        OCCURS 101 TIMES         VALUE 0.
000760     03 TS-ULT-ATL               PIC X(26)
000760        OCCURS 101 TIMES         VALUE ' '.
000760     03 CD-TIP-NOTA-AGPR         PIC S9(4) COMP-5
000760        OCCURS 101 TIMES         VALUE ZEROS.
000760     03 SG-NOTA-AG-CLSC          PIC X(10)
000760        OCCURS 101 TIMES         VALUE ' '.
000760     03 DT-INC-ATBC-NOTA         PIC X(8)
000760        OCCURS 101 TIMES         VALUE ' '.
000760     03 DT-FIM-ATBC-NOTA         PIC X(8)
000760        OCCURS 101 TIMES         VALUE ' '.
000760     03 DT-FIM-ATBC-NOTA-N       PIC S9(4) COMP-5
000760        OCCURS 101 TIMES         VALUE 0.
000760     03 NM-AG-CLSC-RSCO          PIC X(60)
000760        OCCURS 101 TIMES         VALUE ' '.
000760     03 TX-NOTA-AG-CLSC          PIC X(60)
000760        OCCURS 101 TIMES         VALUE ' '.
000760     03 NR-SEQL-NOTA-AG          PIC S9(4) COMP-5
000760        OCCURS 101 TIMES         VALUE 0.
000760     03 IN-CNCT-NOTA-CLSC        PIC  X(1)
000760        OCCURS 101 TIMES         VALUE ' '.
000760
000760* Variáveis auxiliares
000760 01 WS-CD-CLI-INC                PIC S9(9) COMP-5 VALUE 0.
000760 01 WS-CD-CLI-FIM                PIC S9(9) COMP-5
000760                                 VALUE 999999999.
000760 01 WS-CD-AG-CLSC-INC            PIC S9(4) COMP-5 VALUE 0.
000760 01 WS-CD-AG-CLSC-FIM            PIC S9(4) COMP-5 VALUE 9999.
000760 01 WS-NR-ETRD-AG-INC            PIC S9(4) COMP-5 VALUE 0.
000760 01 WS-NR-ETRD-AG-FIM            PIC S9(4) COMP-5 VALUE 9999.
000760 01 WS-SG-NOTA-AG-INC            PIC  X(10) VALUE 'A'.
000760 01 WS-SG-NOTA-AG-FIM            PIC  X(10)
000760                                 VALUE 'ZZZZZZZZZZ'.
000760 01 WS-DT-INC-ATBC               PIC  X(8)
000760                                 VALUE '01010001'.
000760 01 WS-DT-FIM-ATBC               PIC  X(8)
000760                                 VALUE '31129999'.
000760 01 WS-NR-SEQL-NOTA-INC          PIC S9(4) COMP-5 VALUE 0.
000760 01 WS-NR-SEQL-NOTA-FIM          PIC S9(4) COMP-5
000760                                 VALUE 9999.
000760
000760* Variavel(is) de controle do cursor(es)
000760 01  CTL-CURSOR-PSQ.
000760     03  CD-FIM-CURSOR-PSQ       PIC S9(04) COMP-5 VALUE 0.
000760         88  FIM-CURSOR-PSQ      VALUE +100.
000760     03  IN-CURSOR-PSQ-ABTO      PIC  X(01) VALUE 'N'.
000760         88  CURSOR-PSQ-ABTO     VALUE 'S'.
000760
000760* Subrotinas utilizadas
000760     EXEC SQL
000760         INCLUDE SQLCA
000760     END-EXEC.
000760
000760*-----------------------------------------------------------------
000410*--> Pesquisa as Notas de classificacao Cadastradas por Cliente
000410*-----------------------------------------------------------------
-INC SGTKL073
000760     EXEC SQL
000760       DECLARE CURSOR-PSQ-1 CURSOR
000760               WITH ROWSET POSITIONING FOR
000760       SELECT A.CD_AG_CLSC_RSCO
000760            , A.CD_ARQ_OPR_ETRD
000760            , A.CD_USU_ULT_ATL
000760            , A.NR_ETRD_AG_CLSC
000760            , A.NR_SEQL_NOTA_PCT
000760            , A.TS_ULT_ATL
000760            , A.CD_CLI
000760            , A.IN_NOTA_PCT_CNCD
000410            , CHAR(TO_CHAR(A.DT_INC_ATBC_NOTA,'DDMMYYYY') , 8 )
000410            , VALUE(CHAR(TO_CHAR
000410             (A.DT_FIM_ATBC_NOTA,'DDMMYYYY') , 8 ),'')
000760            , A.NR_SEQL_NOTA_AG
000760            , B.CD_TIP_NOTA_AGPR
000760            , B.SG_NOTA_AG_CLSC
000760            , B.TX_NOTA_AG_CLSC
000760            , C.NM_AG_CLSC_RSCO
000760         FROM DB2SGT.NOTA_CLSC_PCT_OPR  A
000760
000760         INNER JOIN DB2SGT.NOTA_AG_CLSC_EXNO  B
000760         ON  A.CD_AG_CLSC_RSCO = B.CD_AG_CLSC_RSCO
000760         AND A.NR_ETRD_AG_CLSC = B.NR_ETRD_AG_CLSC
000760         AND A.NR_SEQL_NOTA_AG = B.NR_SEQL_NOTA_AG
000760
000760         INNER JOIN DB2SGT.AG_CLSC_EXNO_RSCO  C
000760         ON  A.CD_AG_CLSC_RSCO = C.CD_AG_CLSC_RSCO
000760         AND A.NR_ETRD_AG_CLSC = C.NR_ETRD_AG_CLSC
000760
000760         WHERE A.CD_CLI           >= :WS-CD-CLI-INC
000760         AND   A.CD_CLI           <= :WS-CD-CLI-FIM
000760         AND   A.NR_ETRD_AG_CLSC  >= :WS-NR-ETRD-AG-INC
000760         AND   A.NR_ETRD_AG_CLSC  <= :WS-NR-ETRD-AG-FIM
000760         AND   A.CD_AG_CLSC_RSCO  >= :WS-CD-AG-CLSC-INC
000760         AND   A.CD_AG_CLSC_RSCO  <= :WS-CD-AG-CLSC-FIM
000760         AND   UPPER (B.SG_NOTA_AG_CLSC)  >=
000760               UPPER (:WS-SG-NOTA-AG-INC)
000760         AND   UPPER (B.SG_NOTA_AG_CLSC)  <=
000760               UPPER (:WS-SG-NOTA-AG-FIM)
000760
000041         AND   A.DT_INC_ATBC_NOTA  >=
000410             TO_CHAR(TO_DATE(:WS-DT-INC-ATBC,'DDMMYYYY'),
000410                             'DD.MM.YYYY')
000410
000410         AND   (A.DT_FIM_ATBC_NOTA <=
000410             TO_CHAR(TO_DATE(:WS-DT-FIM-ATBC,'DDMMYYYY'),
000410                             'DD.MM.YYYY')
000410             OR A.DT_FIM_ATBC_NOTA IS NULL)
000410
000760*--> Paginacao
000760         AND   A.CD_CLI > :SGTSL073-E-CD-CLI-PAG
000760         OR   (A.CD_CLI = :SGTSL073-E-CD-CLI-PAG
000760        AND   A.NR_SEQL_NOTA_PCT > :SGTSL073-E-NR-SEQL-NOTA-PAG)
000760       ORDER BY A.CD_CLI  ,  A.NR_SEQL_NOTA_PCT
000760     END-EXEC.
000760
000410*-----------------------------------------------------------------
000410*--> Pesquisa Notas de classificacao Cadastradas
000410*-----------------------------------------------------------------
000410     EXEC SQL
000410       DECLARE CURSOR-PSQ-2 CURSOR
000410               WITH ROWSET POSITIONING FOR
000410       SELECT A.CD_AG_CLSC_RSCO
000410            , A.CD_ARQ_OPR_ETRD
000410            , A.CD_USU_ULT_ATL
000410            , A.NR_ETRD_AG_CLSC
000410            , A.NR_SEQL_NOTA_PCT
000410            , A.TS_ULT_ATL
000410            , A.CD_CLI
000410            , CHAR(TO_CHAR(A.DT_INC_ATBC_NOTA,'DDMMYYYY') , 8 )
000410            , VALUE(CHAR(TO_CHAR
000410             (A.DT_FIM_ATBC_NOTA,'DDMMYYYY') , 8 ),'')
000410            , A.NR_SEQL_NOTA_AG
000090
000760         FROM DB2SGT.NOTA_CLSC_PCT_OPR  A
000090
000760         WHERE A.CD_CLI            = :SGTSL073-E-CD-CLI
000760         AND   A.NR_SEQL_NOTA_PCT >= :WS-NR-SEQL-NOTA-INC
000760         AND   A.NR_SEQL_NOTA_PCT <= :WS-NR-SEQL-NOTA-FIM
000760         AND   A.CD_AG_CLSC_RSCO  >= :WS-CD-AG-CLSC-INC
000760         AND   A.CD_AG_CLSC_RSCO  <= :WS-CD-AG-CLSC-FIM
000410
000410         AND   A.IN_NOTA_PCT_CNCD  =   'N'
000410
000410         ORDER BY A.DT_INC_ATBC_NOTA
000410     END-EXEC.
000410
000760*-----------------------------------------------------------------
000760 PROCEDURE DIVISION.
000760*-----------------------------------------------------------------
000760
000760* Habilita tratamento automatico de erros DB2
-INC HLPKE997
000760* Habilita captura automatica de abend e erros CICS
-INC HLPKE999
000760
000760     PERFORM 100000-DESCOBRIR-CANAL
000760     PERFORM 110000-EXCLUIR-CTNR-RPST
000760     PERFORM 120000-EXCLUIR-CTNR-ERRO
000760     PERFORM 130000-RECEBER-REQUISICAO
000760     PERFORM 140000-VALIDAR-PARAMETRO
000760
000760     PERFORM 970101-ABRE-CURSOR-PSQ
000760     PERFORM 970102-LER-CURSOR-PSQ
000760     PERFORM 970103-FECHA-CURSOR-PSQ
000760
000760     PERFORM 200000-PROCESSA-PESQUISA
000760              VARYING SGTSL073-IC-TAB FROM 1 BY 1
000760              UNTIL SGTSL073-IC-TAB > SGTSL073-QT-OCR-TAB
000760
000760     PERFORM 300000-POSTA-RESPOSTA
000760
000760     GOBACK
000760     .
000760*-----------------------------------------------------------------
000760 100000-DESCOBRIR-CANAL          SECTION.
000760*-----------------------------------------------------------------
000760     MOVE 0001                   TO  MSG1-CD-SEQL-ERRO

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET  ERRO-CANAL             TO TRUE
001254
001254     EXEC CICS ASSIGN
001254        CHANNEL ( NM-CNL )
001254     END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
000760     .
000760 100000-SAI.
000760     EXIT.
000760*-----------------------------------------------------------------
000760 110000-EXCLUIR-CTNR-RPST        SECTION.
000760*-----------------------------------------------------------------
000760     MOVE 0002                   TO  MSG1-CD-SEQL-ERRO
000760
000760     EXEC CICS DELETE
000760        CONTAINER ( 'SGTSL073-RPST' )
000760        CHANNEL   ( NM-CNL      )
000760        NOHANDLE
000760     END-EXEC
000760
000760     IF  NOT (EIBRESP EQUAL DFHRESP(NORMAL)   OR
000760         EIBRESP EQUAL DFHRESP(CONTAINERERR) )
000760         PERFORM 999002-TRATA-CONDITION-CICS
000760     END-IF
000760     .
000760 110000-SAI.
000760     EXIT.
000760*-----------------------------------------------------------------
000760 120000-EXCLUIR-CTNR-ERRO        SECTION.
000760*-----------------------------------------------------------------
000760     MOVE 0003                   TO MSG1-CD-SEQL-ERRO
000760
000760     EXEC CICS DELETE
000760        CONTAINER ( 'PRM-ERRO' )
000760        CHANNEL   ( NM-CNL      )
000760        NOHANDLE
000760     END-EXEC
000760
000760     IF  NOT (EIBRESP EQUAL DFHRESP(NORMAL)   OR
000760         EIBRESP EQUAL DFHRESP(CONTAINERERR) )
000760         PERFORM 999002-TRATA-CONDITION-CICS
000760     END-IF
000760     .
000760 120000-SAI.
000760     EXIT.
000760*-----------------------------------------------------------------
000760 130000-RECEBER-REQUISICAO       SECTION.
000760*-----------------------------------------------------------------
000760     MOVE 0004                   TO  MSG1-CD-SEQL-ERRO
000760
000760     MOVE ZEROS                  TO  QT-CCT-LIDO
000760
000760     MOVE LENGTH OF SGTSL073-RQSC    TO  QT-CCT-EPRO

           MOVE 'SGTSL073-RQSC'        TO  NM-CTNR
           SET  GET-CONTAINER          TO  TRUE
           SET  ERRO-CONTAINER         TO  TRUE

000760     EXEC CICS GET
000760        CONTAINER ( NM-CTNR )
000760        CHANNEL   ( NM-CNL      )
000760        NODATA
000760        FLENGTH ( QT-CCT-LIDO )
000760     END-EXEC
000760
      * 3001
           IF  QT-CCT-LIDO   NOT   EQUAL   QT-CCT-EPRO
               SET  ERRO-TAMANHO-AREA  TO  TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
000760
000760     EXEC CICS GET
000760        CONTAINER ( NM-CTNR )
000760        CHANNEL   ( NM-CNL      )
000760        FLENGTH ( LENGTH OF SGTSL073-RQSC )
000760        INTO    ( SGTSL073-RQSC )
000760     END-EXEC

           SET  SEM-ERRO               TO  TRUE
000760     .
000760 130000-SAI.
000760     EXIT.
000760*-----------------------------------------------------------------
000760 140000-VALIDAR-PARAMETRO        SECTION.
000760*-----------------------------------------------------------------
000760     MOVE 0006                   TO  MSG1-CD-SEQL-ERRO
000760
000760     MOVE 'N'                    TO  SGTSL073-IN-CTNC-PSQ
000760
000760     EVALUATE SGTSL073-CD-TIP-SRVC
000760     WHEN 1
000760         IF  SGTSL073-E-CD-CLI EQUAL ZEROS
000760             CONTINUE
000760         ELSE
000760             MOVE SGTSL073-E-CD-CLI      TO  WS-CD-CLI-INC
000760                                             WS-CD-CLI-FIM
000760         END-IF
000760
000760         IF  SGTSL073-E-NR-SEQL-NOTA-PAG   EQUAL ZEROS
000760             CONTINUE
000760         ELSE
000760             MOVE SGTSL073-E-NR-SEQL-NOTA-PAG
000760                                         TO  WS-NR-SEQL-NOTA-INC
000760                                             WS-NR-SEQL-NOTA-FIM
000760         END-IF
000760
000760         IF  SGTSL073-E-CD-AG-CLSC-RSCO  EQUAL ZEROS
000760             CONTINUE
000760         ELSE
000760             MOVE SGTSL073-E-CD-AG-CLSC-RSCO TO  WS-CD-AG-CLSC-INC
000760                                                 WS-CD-AG-CLSC-FIM
000760         END-IF
000760
000760         IF  SGTSL073-E-NR-ETRD-AG-CLSC  EQUAL ZEROS
000760             CONTINUE
000760         ELSE
000760             MOVE SGTSL073-E-NR-ETRD-AG-CLSC TO  WS-NR-ETRD-AG-INC
000760                                                 WS-NR-ETRD-AG-FIM
000760         END-IF
000760
000760         IF  SGTSL073-E-SG-NOTA-AG-CLSC  EQUAL SPACES
000760             CONTINUE
000760         ELSE
000760             MOVE SGTSL073-E-SG-NOTA-AG-CLSC TO  WS-SG-NOTA-AG-INC
000760                                                 WS-SG-NOTA-AG-FIM
000760         END-IF
000760
000410         IF  SGTSL073-E-DT-INC-PER-ATBC  EQUAL SPACES
000410             CONTINUE
000410         ELSE
000410             MOVE SGTSL073-E-DT-INC-PER-ATBC   TO  WS-DT-INC-ATBC
000410         END-IF
000090
000410         IF  SGTSL073-E-DT-FIM-PER-ATBC  EQUAL SPACES
000410             CONTINUE
000410         ELSE
000410             MOVE SGTSL073-E-DT-FIM-PER-ATBC   TO  WS-DT-FIM-ATBC
000410         END-IF
000090
000760     WHEN 2
000760         IF  SGTSL073-E-CD-CLI EQUAL ZEROS
000760             CONTINUE
000760         ELSE
000760             MOVE SGTSL073-E-CD-CLI      TO  WS-CD-CLI-INC
000760                                             WS-CD-CLI-FIM
000760         END-IF
000760
000760         IF  SGTSL073-E-NR-SEQL-NOTA-PAG   EQUAL ZEROS
000760             CONTINUE
000760         ELSE
000760             MOVE SGTSL073-E-NR-SEQL-NOTA-PAG
000760                                         TO  WS-NR-SEQL-NOTA-INC
000760                                             WS-NR-SEQL-NOTA-FIM
000760         END-IF
000760
000760         IF  SGTSL073-E-CD-AG-CLSC-RSCO  EQUAL ZEROS
000760             CONTINUE
000760         ELSE
000760             MOVE SGTSL073-E-CD-AG-CLSC-RSCO TO  WS-CD-AG-CLSC-INC
000760                                                 WS-CD-AG-CLSC-FIM
000760         END-IF
000760
000760     END-EVALUATE
000760
000760     .
000760 140000-SAI.
000760     EXIT.
000760*-----------------------------------------------------------------
000760 200000-PROCESSA-PESQUISA        SECTION.
000760*-----------------------------------------------------------------
000760     MOVE 0007                   TO  MSG1-CD-SEQL-ERRO
000760
000760     MOVE CD-AG-CLSC-RSCO(SGTSL073-IC-TAB)     TO
000760                       SGTSL073-CD-AG-CLSC-RSCO(SGTSL073-IC-TAB)
000760
000760     MOVE CD-ARQ-OPR-ETRD(SGTSL073-IC-TAB)     TO
000760                       SGTSL073-CD-ARQ-OPR-ETRD(SGTSL073-IC-TAB)
000760
000760     MOVE CD-USU-ULT-ATL(SGTSL073-IC-TAB)      TO
000760                       SGTSL073-CD-USU-ULT-ATL(SGTSL073-IC-TAB)
000760
000760     MOVE NR-ETRD-AG-CLSC(SGTSL073-IC-TAB)     TO
000760                       SGTSL073-NR-ETRD-AG-CLSC(SGTSL073-IC-TAB)
000760
000760     MOVE NR-SEQL-NOTA-PCT(SGTSL073-IC-TAB)    TO
000760                       SGTSL073-NR-SEQL-NOTA-PCT(SGTSL073-IC-TAB)
000760
000760     MOVE TS-ULT-ATL(SGTSL073-IC-TAB)          TO
000760                       SGTSL073-TS-ULT-ATL(SGTSL073-IC-TAB)
000760
000760     MOVE CD-TIP-NOTA-AGPR(SGTSL073-IC-TAB)    TO
000760                       SGTSL073-CD-TIP-NOTA-AGPR(SGTSL073-IC-TAB)
000760
000760     MOVE SG-NOTA-AG-CLSC(SGTSL073-IC-TAB)     TO
000760                       SGTSL073-SG-NOTA-AG-CLSC(SGTSL073-IC-TAB)
000760
000760     MOVE NR-SEQL-NOTA-AG(SGTSL073-IC-TAB)    TO
000760                       SGTSL073-NR-SEQL-NOTA-AG(SGTSL073-IC-TAB)
000760
000760     MOVE DT-INC-ATBC-NOTA(SGTSL073-IC-TAB)    TO
000760                       SGTSL073-DT-INC-ATBC-NOTA(SGTSL073-IC-TAB)
000760
000760     MOVE DT-FIM-ATBC-NOTA(SGTSL073-IC-TAB)  TO
000760                       SGTSL073-DT-FIM-ATBC-NOTA(SGTSL073-IC-TAB)
000760
000760     MOVE NM-AG-CLSC-RSCO(SGTSL073-IC-TAB)       TO
000760                       SGTSL073-NM-AG-CLSC-RSCO(SGTSL073-IC-TAB)
000760
000760     MOVE TX-NOTA-AG-CLSC(SGTSL073-IC-TAB)       TO
000760                       SGTSL073-TX-NOTA-AG-CLSC (SGTSL073-IC-TAB)
000760
000760     MOVE CD-CLI(SGTSL073-IC-TAB)                TO
000760                       SGTSL073-CD-CLI-PCT(SGTSL073-IC-TAB)
000760
000760     MOVE IN-CNCT-NOTA-CLSC(SGTSL073-IC-TAB)                TO
000760                       SGTSL073-IN-CNCT-NOTA-CLSC(SGTSL073-IC-TAB)
000760     .
000760 200000-SAI.
000760     EXIT.
000760
000760*-----------------------------------------------------------------
000760 970101-ABRE-CURSOR-PSQ          SECTION.
000760*-----------------------------------------------------------------
000760     MOVE 0008                   TO  MSG1-CD-SEQL-ERRO
000760
000760     EVALUATE SGTSL073-CD-TIP-SRVC
000760
000760     WHEN 1
000760          EXEC SQL
000760               OPEN CURSOR-PSQ-1
000760          END-EXEC
000760
000760     WHEN 2
000760          EXEC SQL
000760               OPEN CURSOR-PSQ-2
000760          END-EXEC
000760     END-EVALUATE
000760
000760     MOVE 'S'                    TO  IN-CURSOR-PSQ-ABTO
000760     .
000760 970101-SAI.
000760     EXIT.
000760*-----------------------------------------------------------------
000760 970102-LER-CURSOR-PSQ           SECTION.
000760*-----------------------------------------------------------------
000760     MOVE 0009                   TO  MSG1-CD-SEQL-ERRO
000760
000760     INITIALIZE REG-WS
000760
000760     EVALUATE SGTSL073-CD-TIP-SRVC
000760     WHEN 1
000760          EXEC SQL
000760               FETCH NEXT ROWSET
000760                     CURSOR-PSQ-1 FOR 0101 ROWS
000760              INTO :CD-AG-CLSC-RSCO
000760                 , :CD-ARQ-OPR-ETRD
000760                   :CD-ARQ-OPR-ETRD-N
000760                 , :CD-USU-ULT-ATL
000760                 , :NR-ETRD-AG-CLSC
000760                 , :NR-SEQL-NOTA-PCT
000760                 , :TS-ULT-ATL
000760                 , :CD-CLI
000760                 , :IN-CNCT-NOTA-CLSC
000760                 , :DT-INC-ATBC-NOTA
000760                 , :DT-FIM-ATBC-NOTA
000760                   :DT-FIM-ATBC-NOTA-N
000760                 , :NR-SEQL-NOTA-AG
000760                 , :CD-TIP-NOTA-AGPR
000760                 , :SG-NOTA-AG-CLSC
000760                 , :TX-NOTA-AG-CLSC
000760                 , :NM-AG-CLSC-RSCO
000760          END-EXEC
000760
000760     WHEN 2
000760          EXEC SQL
000760               FETCH NEXT ROWSET
000760                     CURSOR-PSQ-2 FOR 0101 ROWS
000760              INTO :CD-AG-CLSC-RSCO
000760                 , :CD-ARQ-OPR-ETRD
000760                   :CD-ARQ-OPR-ETRD-N
000760                 , :CD-USU-ULT-ATL
000760                 , :NR-ETRD-AG-CLSC
000760                 , :NR-SEQL-NOTA-PCT
000760                 , :TS-ULT-ATL
000760                 , :CD-CLI
000760                 , :DT-INC-ATBC-NOTA
000760                 , :DT-FIM-ATBC-NOTA
000760                   :DT-FIM-ATBC-NOTA-N
000760                 , :NR-SEQL-NOTA-AG
000760          END-EXEC
000760     END-EVALUATE
000760
000760     MOVE SQLERRD(3)             TO  QT-REG-SELD
000760
000760     IF  QT-REG-SELD EQUAL 0101
000760         SET SGTSL073-TEM-MAIS-REGISTRO TO TRUE
000760         MOVE 0100               TO  QT-REG-SELD
000760     END-IF
000760
000760     MOVE QT-REG-SELD            TO  SGTSL073-QT-OCR-TAB
000760     MOVE SQLCODE                TO  CD-FIM-CURSOR-PSQ
000760     .
000760 970102-SAI.
000760     EXIT.
000760*-----------------------------------------------------------------
000760 970103-FECHA-CURSOR-PSQ         SECTION.
000760*-----------------------------------------------------------------
000760     MOVE 0010                   TO  MSG1-CD-SEQL-ERRO
000760
000760     MOVE 'N'                    TO  IN-CURSOR-PSQ-ABTO
000760
000760     EVALUATE SGTSL073-CD-TIP-SRVC
000760     WHEN 1
000760          EXEC SQL
000760               CLOSE CURSOR-PSQ-1
000760          END-EXEC
000760
000760     WHEN 2
000760          EXEC SQL
000760               CLOSE CURSOR-PSQ-2
000760          END-EXEC
000760     END-EVALUATE
000760     .
000760 970103-SAI.
000760     EXIT.
000760*-----------------------------------------------------------------
000760 300000-POSTA-RESPOSTA           SECTION.
000760*-----------------------------------------------------------------
000760     MOVE 0011                   TO  MSG1-CD-SEQL-ERRO

           MOVE 'SGTSL073-RPST'        TO  NM-CTNR
           SET  PUT-CONTAINER          TO  TRUE
           SET  ERRO-CONTAINER         TO  TRUE
000760
000760     EXEC CICS PUT
000760        CONTAINER ( 'SGTSL073-RPST' )
000760        CHANNEL   ( NM-CNL      )
000760        FLENGTH ( LENGTH OF SGTSL073-RPST )
000760        FROM    ( SGTSL073-RPST )
000760     END-EXEC

           SET  SEM-ERRO               TO  TRUE
000760     .
000760 300000-SAI.
000760     EXIT.
000760*-----------------------------------------------------------------
000760 999000-GRAVA-ERRO-E-ENCERRA     SECTION.
000760*-----------------------------------------------------------------
      *--> Monitora variàveis do pgm, grava e encerra

           IF  MSG1-CD-ERRO-TS             EQUAL  ZEROS
              MOVE  8888               TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS             EQUAL  SPACES
               MOVE  'Erro não especificado no módulo.'
                                       TO  MSG1-TX-ERRO-TS
           END-IF

      *--> Verifica se o tipo de erro gerado possui tratamento interno.
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
      *    O Analista que especificou o módulo e o Analista Programador
      *    tem liberdade para Monitorar outras Variáveis que forem
      *    relevantes ao Suporte do Sistema.
      *=================================================================
000760
000760     MOVE 'SGTSL073-CD-TIP-SRVC'         TO  MSGKW999-NM-VRV-MNT
000760     MOVE SGTSL073-CD-TIP-SRVC           TO  MSGKW999-VL-VRV-MNT
000760     PERFORM 999003-ADICIONA-VRV-MNT
000760
000760     MOVE 'SGTSL073-E-CD-CLI'            TO  MSGKW999-NM-VRV-MNT
000760     MOVE SGTSL073-E-CD-CLI              TO  MSGKW999-VL-VRV-MNT
000760     PERFORM 999003-ADICIONA-VRV-MNT
000760
000760     MOVE 'SGTSL073-E-CD-AG-CLSC-RSCO'   TO  MSGKW999-NM-VRV-MNT
000760     MOVE SGTSL073-E-CD-AG-CLSC-RSCO     TO  MSGKW999-VL-VRV-MNT
000760     PERFORM 999003-ADICIONA-VRV-MNT
000760
000760     MOVE 'SGTSL073-E-NR-ETRD-AG-CLSC'   TO  MSGKW999-NM-VRV-MNT
000760     MOVE SGTSL073-E-NR-ETRD-AG-CLSC     TO  MSGKW999-VL-VRV-MNT
000760     PERFORM 999003-ADICIONA-VRV-MNT
000760
000760     MOVE 'SGTSL073-E-SG-NOTA-AG-CLSC'   TO  MSGKW999-NM-VRV-MNT
000760     MOVE SGTSL073-E-SG-NOTA-AG-CLSC     TO  MSGKW999-VL-VRV-MNT
000760     PERFORM 999003-ADICIONA-VRV-MNT
000760
000760     MOVE 'SGTSL073-E-DT-INC-PER-ATBC'   TO  MSGKW999-NM-VRV-MNT
000760     MOVE SGTSL073-E-DT-INC-PER-ATBC     TO  MSGKW999-VL-VRV-MNT
000760     PERFORM 999003-ADICIONA-VRV-MNT
000760
000760     MOVE 'SGTSL073-E-DT-FIM-PER-ATBC'   TO  MSGKW999-NM-VRV-MNT
000760     MOVE SGTSL073-E-DT-FIM-PER-ATBC     TO  MSGKW999-VL-VRV-MNT
000760     PERFORM 999003-ADICIONA-VRV-MNT
000760
000760     MOVE 'SGTSL073-E-CD-CLI-PAG'        TO  MSGKW999-NM-VRV-MNT
000760     MOVE SGTSL073-E-CD-CLI-PAG          TO  MSGKW999-VL-VRV-MNT
000760     PERFORM 999003-ADICIONA-VRV-MNT
000760
000760     MOVE 'SGTSL073-E-NR-SEQL-NOTA-PAG'  TO  MSGKW999-NM-VRV-MNT
000760     MOVE SGTSL073-E-NR-SEQL-NOTA-PAG    TO  MSGKW999-VL-VRV-MNT
000760     PERFORM 999003-ADICIONA-VRV-MNT
000760
000760* --> grava o erro no msg
000760     PERFORM MSG1-NTF-MSG
000760     MOVE MSG1-CD-ERRO-TS                TO  IIBKERRO-CD-ERRO
000760     MOVE MSG1-TX-ERRO-TS                TO  IIBKERRO-TX-ERRO
000760
000760     EXEC CICS PUT
000760        CONTAINER ( 'PRM-ERRO' )
000760        CHANNEL   ( NM-CNL      )
000760        FROM      ( IIBKERRO-PRM-ERRO)
000760        FLENGTH ( LENGTH OF IIBKERRO-PRM-ERRO )
000760        NOHANDLE
000760     END-EXEC
000760
000760     PERFORM 999999-ENCERRAR
000760     .
000760 999000-SAI.
000760     EXIT.
000760* --------------------------------------*
000760* 999001-TRATA-ABEND-CICS        SECTION.
000760* 999002-TRATA-CONDITION-CICS    SECTION.
000760* 999003-ADICIONA-VRV-MNT        SECTION.
000760* --------------------------------------*
-INC HLPKP999
000760* --------------------------------------*
000760* 999010-TRATA-ERRO-DB2          SECTION.
-INC HLPKP997
000760* --------------------------------------*
000760* MSG1-NTF-MSG                   SECTION.
-INC MSGKPMG1
-INC SGTKP999
000760* --------------------------------------*
000760 999999-ENCERRAR                SECTION.
000760* --------------------------------------*
000760     EXEC CICS
000760          RETURN
000760     END-EXEC.