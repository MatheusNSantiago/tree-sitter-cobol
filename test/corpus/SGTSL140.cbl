      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSL140
      * ANALISTA..: F9188847 - Soila Akamatsu
      * AUTOR.....: F9188847 - Soila Akamatsu
      * OBJETIVO..: Listar TABELA DB2SGT.PRCT_CRNG_TIT_EXCD
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      *  VRS001 22.04.2025 - F9188847 - Historia 1852713 - Implantação
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSL140.
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
       77  CTE-PROG     PIC  X(17) VALUE '*** SGTSL140 ***'.
       77  CTE-VERS     PIC  X(06) VALUE 'VRS001'.


      *--> Definição dos Variaveis do Cobolito
-INC ALMKCBL1

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

      * Book para trataro container
-INC SGTKERR1

      * Area de trabalho

       01  QT-REG-SELD                      PIC S9(4) COMP-5 VALUE 0.

      * Campos utilizados para leitura

       01  REG-WS.
           03  CD-EST-PRCT-CRNG             PIC S9(4) COMP-5
               OCCURS 101 TIMES             VALUE 0.
           03  CD-USU-RSP-ULT-ATL           PIC X(8)
               OCCURS 101 TIMES             VALUE ' '.
           03  TS-EST-ENVO-CRNG             PIC X(26)
               OCCURS 101 TIMES             VALUE ' '.
           03  TX-PRCT-EVT                  PIC X(50)
               OCCURS 101 TIMES             VALUE ' '.
           03  TS-PRCT-RTN-EVT              PIC X(26)
               OCCURS 101 TIMES             VALUE ' '.


           03  TX-PRCT-EVT-N                PIC S9(4) COMP-5
               OCCURS 101 TIMES             VALUE 0.
           03  TS-PRCT-RTN-EVT-N            PIC S9(4) COMP-5
               OCCURS 101 TIMES             VALUE 0.




      * Variavel(is) de controle do cursor(es)

       01  CTL-CURSOR-PSQ.
           03  CD-FIM-CURSOR-PSQ            PIC S9(04) COMP-5 VALUE 0.
               88  FIM-CURSOR-PSQ           VALUE +100.
           03  IN-CURSOR-PSQ-ABTO           PIC  X(01) VALUE 'N'.
               88  CURSOR-PSQ-ABTO          VALUE 'S'.

      * Subrotinas utilizadas

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

      *-----------------------------------------------------------------
       01  SGTKL140-DADOS.
-INC SGTKL140

           EXEC SQL
             DECLARE CURSOR-PSQ CURSOR
                     WITH ROWSET POSITIONING FOR
             SELECT A.CD_EST_PRCT_CRNG
                  , A.CD_USU_RSP_ULT_ATL
                  , A.TS_EST_ENVO_CRNG
                  , A.TX_PRCT_EVT
                  , A.TS_PRCT_RTN_EVT

               FROM DB2SGT.PRCT_CRNG_TIT_EXCD      A
              WHERE A.CD_ITMT_FNCO        = :SGTSL140-CD-ITMT-FNCO
                AND A.CD_GR_EVT_FDAR      = :SGTSL140-CD-GR-EVT-FDAR
                AND A.NR_SEQL_EVT_EXCD    = :SGTSL140-NR-SEQL-EVT-EXCD
                AND A.TS_EST_ENVO_CRNG  > :SGTSL140-TS-EST-ENVO-CRNG-ULT
              ORDER BY  A.TS_EST_ENVO_CRNG DESC
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
           PERFORM 200000-PROCESSA-PESQUISA
                    VARYING SGTSL140-IC-TAB FROM 1 BY 1
                    UNTIL SGTSL140-IC-TAB > SGTSL140-QT-OCR-TAB
           PERFORM 970103-FECHA-CURSOR-PSQ
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
              CONTAINER ( 'SGTSL140-RPST' )
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

           MOVE ZEROS                       TO QT-CCT-LIDO
           MOVE LENGTH OF SGTSL140-RQSC      TO QT-CCT-EPRO
           EXEC CICS GET
              CONTAINER ( 'SGTSL140-RQSC' )
              CHANNEL   ( NM-CNL          )
              NODATA
              FLENGTH   ( QT-CCT-LIDO     )
           END-EXEC

           IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
               SET  ERRO-TAMANHO-AREA     TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS GET
              CONTAINER ( 'SGTSL140-RQSC'         )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSL140-RQSC )
              INTO      ( SGTSL140-RQSC           )
           END-EXEC

      ** Este ponto indica que não houve erro de cics.
           SET SEM-ERRO        TO TRUE
           .
       130000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       140000-VALIDAR-PARAMETRO SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

           MOVE 'N'                    TO SGTSL140-IN-CNTO-PSQ

           IF  SGTSL140-E-CD-TIP-SRVC   > 1
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 0107                TO MSG1-CD-ERRO-TS
               MOVE 'Código do tipo de serviço inválido.'
                                        TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

            IF  SGTSL140-CD-ITMT-FNCO EQUAL SPACES
               SET   ERRO-PROGRAMADO   TO  TRUE
                MOVE  1160             TO  MSG1-CD-ERRO-TS
                MOVE  'Código do Instrumento Financeiro não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL140-CD-GR-EVT-FDAR < ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  1161              TO  MSG1-CD-ERRO-TS
               MOVE  'Código do Grupo do Evento Fiduciário inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL140-CD-GR-EVT-FDAR EQUAL ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  1149              TO  MSG1-CD-ERRO-TS
               MOVE  'Código do Grupo do Evento Fiduciário não informado
      -               '.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL140-NR-SEQL-EVT-EXCD EQUAL ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  1179              TO  MSG1-CD-ERRO-TS
               MOVE 'Numero do Evento SGT  não informado.'
                                         TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL140-NR-SEQL-EVT-EXCD < ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  1180              TO  MSG1-CD-ERRO-TS
               MOVE 'Numero do Evento SGT  inválido.'
                                         TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *--> Se TS não informado, formatar padrão DB2
           IF SGTSL140-TS-EST-ENVO-CRNG-ULT = ' '
              MOVE '0001-01-01-00.00.00.000000'
                                       TO SGTSL140-TS-EST-ENVO-CRNG-ULT
           END-IF

           .
       140000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       200000-PROCESSA-PESQUISA SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

           MOVE CD-EST-PRCT-CRNG(SGTSL140-IC-TAB) TO
                     SGTSL140-CD-EST-PRCT-CRNG(SGTSL140-IC-TAB)
           MOVE CD-USU-RSP-ULT-ATL(SGTSL140-IC-TAB) TO
                     SGTSL140-CD-USU-RSP-ULT-ATL(SGTSL140-IC-TAB)
           MOVE TS-EST-ENVO-CRNG(SGTSL140-IC-TAB) TO
                     SGTSL140-TS-EST-ENVO-CRNG(SGTSL140-IC-TAB)
           MOVE TX-PRCT-EVT(SGTSL140-IC-TAB) TO
                     SGTSL140-TX-PRCT-EVT(SGTSL140-IC-TAB)
           MOVE TS-PRCT-RTN-EVT(SGTSL140-IC-TAB) TO
                     SGTSL140-TS-PRCT-RTN-EVT(SGTSL140-IC-TAB)

           .
       200000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970101-ABRE-CURSOR-PSQ SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

           EVALUATE  TRUE
               WHEN  SGTSL140-LISTA-POR-PRCT
                     EXEC SQL
                          OPEN CURSOR-PSQ
                     END-EXEC

           END-EVALUATE

           MOVE 'S'                    TO IN-CURSOR-PSQ-ABTO
           .
       970101-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970102-LER-CURSOR-PSQ SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE REG-WS
           EVALUATE  TRUE
               WHEN  SGTSL140-LISTA-POR-PRCT
               EXEC SQL
                   FETCH NEXT ROWSET
                      CURSOR-PSQ FOR 0101 ROWS
                   INTO :CD-EST-PRCT-CRNG
                      , :CD-USU-RSP-ULT-ATL
                      , :TS-EST-ENVO-CRNG
                      , :TX-PRCT-EVT
                        :TX-PRCT-EVT-N
                      , :TS-PRCT-RTN-EVT
                        :TS-PRCT-RTN-EVT-N
               END-EXEC

           END-EVALUATE


           MOVE SQLERRD(3)             TO QT-REG-SELD

           IF  QT-REG-SELD EQUAL 0101
               SET SGTSL140-TEM-MAIS-REGISTRO TO TRUE
               MOVE 0100               TO QT-REG-SELD
           END-IF

           MOVE QT-REG-SELD            TO SGTSL140-QT-OCR-TAB
           MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ
           .
       970102-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970103-FECHA-CURSOR-PSQ SECTION.
      *-----------------------------------------------------------------
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO

           MOVE 'N'                    TO IN-CURSOR-PSQ-ABTO

           EVALUATE  TRUE
               WHEN  SGTSL140-LISTA-POR-PRCT
                     EXEC SQL
                          CLOSE CURSOR-PSQ
                     END-EXEC
           END-EVALUATE
           .
       970103-SAI.
           EXIT.
      *-----------------------------------------------------------------
       300000-POSTA-RESPOSTA SECTION.
      *-----------------------------------------------------------------
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO

           SET PUT-CONTAINER           TO TRUE
           SET ERRO-CONTAINER          TO TRUE

           EXEC CICS PUT
              CONTAINER ( 'SGTSL140-RPST'         )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSL140-RPST )
              FROM      ( SGTSL140-RPST           )
           END-EXEC

           SET  SEM-ERRO               TO TRUE
           .
       300000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA SECTION.
      *-----------------------------------------------------------------
           IF  IN-CURSOR-PSQ-ABTO  EQUAL  'S'
               PERFORM 970103-FECHA-CURSOR-PSQ
               MOVE 'N'                      TO IN-CURSOR-PSQ-ABTO
           END-IF

           IF  MSG1-CD-ERRO-TS               EQUAL  ZEROS
               MOVE  8888                    TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS               EQUAL  SPACES
               MOVE  'Erro não especificado no módulo.'
                                             TO  MSG1-TX-ERRO-TS
           END-IF

           MOVE 'SGTSL140-E-CD-TIP-SRVC'     TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL140-E-CD-TIP-SRVC       TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL140-CD-ITMT-FNCO'    TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL140-CD-ITMT-FNCO      TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL140-CD-GR-EVT-FDAR'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL140-CD-GR-EVT-FDAR       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL140-NR-SEQL-EVT-EXCD'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL140-NR-SEQL-EVT-EXCD     TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL140-TS-EST-ENVO-CRNG-ULT'
                                              TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL140-TS-EST-ENVO-CRNG-ULT TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

      * --> grava o erro no msg
           PERFORM MSG1-NTF-MSG

           MOVE MSG1-CD-ERRO-TS             TO IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS             TO IIBKERRO-TX-ERRO

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
      * 999006-TRATA-ERRO-CANAL        SECTION.
      * --------------------------------------*
-INC HLPKP999
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