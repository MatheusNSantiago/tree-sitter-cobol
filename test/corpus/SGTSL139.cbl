      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSL139
      * ANALISTA..: F9188847 - Soila Akamatsu
      * AUTOR.....: F9188847 - Soila Akamatsu
      * OBJETIVO..: Listar TABELA EST_EVT_TIT_EXCD
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 22.04.2025 - F9188847 - Historia 1852713 - Implantação
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSL139.
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
       77  CTE-PROG     PIC  X(17) VALUE '*** SGTSL139 ***'.
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
           03  CD-EST-EVT-EXCD              PIC S9(4) COMP-5
               OCCURS 101 TIMES             VALUE 0.
           03  CD-USU-ULT-ATL               PIC X(8)
               OCCURS 101 TIMES             VALUE ' '.
           03  TS-EST-EVT-EXCD              PIC X(26)
               OCCURS 101 TIMES             VALUE ' '.
           03  TX-JST-EVT-EXCD              PIC X(200)
               OCCURS 101 TIMES             VALUE ' '.

      *--> Indicadores de nulidades
           03  TX-JST-EVT-EXCD-N            PIC S9(4) COMP-5
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
       01  SGTKL139-DADOS.
-INC SGTKL139

           EXEC SQL
             DECLARE CURSOR-PSQ CURSOR
                     WITH ROWSET POSITIONING FOR
             SELECT A.CD_EST_EVT_EXCD
                  , A.CD_USU_ULT_ATL
                  , A.TS_EST_EVT_EXCD
                  , A.TX_JST_EVT_EXCD
               FROM DB2SGT.EST_EVT_TIT_EXCD       A
              WHERE A.CD_ITMT_FNCO        = :SGTSL139-CD-ITMT-FNCO
                AND A.CD_GR_EVT_FDAR      = :SGTSL139-CD-GR-EVT-FDAR
                AND A.NR_SEQL_EVT_EXCD    = :SGTSL139-NR-SEQL-EVT-EXCD
                AND A.TS_EST_EVT_EXCD    > :SGTSL139-TS-EST-EVT-EXCD-ULT
              ORDER BY  A.TS_EST_EVT_EXCD DESC
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
                    VARYING SGTSL139-IC-TAB FROM 1 BY 1
                    UNTIL SGTSL139-IC-TAB > SGTSL139-QT-OCR-TAB

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
              CONTAINER ( 'SGTSL139-RPST' )
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
           MOVE LENGTH OF SGTSL139-RQSC      TO QT-CCT-EPRO

           SET GET-CONTAINER           TO TRUE
           SET ERRO-CONTAINER          TO TRUE

           EXEC CICS GET
              CONTAINER ( 'SGTSL139-RQSC' )
              CHANNEL   ( NM-CNL          )
              NODATA
              FLENGTH   ( QT-CCT-LIDO     )
           END-EXEC

           IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
               SET  ERRO-TAMANHO-AREA     TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS GET
              CONTAINER ( 'SGTSL139-RQSC'         )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSL139-RQSC )
              INTO      ( SGTSL139-RQSC           )
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

           MOVE 'N'                    TO SGTSL139-IN-CNTO-PSQ

           IF  SGTSL139-E-CD-TIP-SRVC   > 1
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 0107                TO MSG1-CD-ERRO-TS
               MOVE 'Código do tipo de serviço inválido.'
                                        TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

            IF  SGTSL139-CD-ITMT-FNCO EQUAL SPACES
               SET   ERRO-PROGRAMADO   TO  TRUE
                MOVE  1160             TO  MSG1-CD-ERRO-TS
                MOVE  'Código do Instrumento Financeiro não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL139-CD-GR-EVT-FDAR < ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  1161              TO  MSG1-CD-ERRO-TS
               MOVE  'Código do Grupo do Evento Fiduciário inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL139-CD-GR-EVT-FDAR EQUAL ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  1149              TO  MSG1-CD-ERRO-TS
               MOVE  'Código do Grupo do Evento Fiduciário não informado
      -               '.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL139-NR-SEQL-EVT-EXCD EQUAL ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  1179              TO  MSG1-CD-ERRO-TS
               MOVE 'Numero do Evento SGT  não informado.'
                                         TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF.

           IF  SGTSL139-NR-SEQL-EVT-EXCD < ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  1180              TO  MSG1-CD-ERRO-TS
               MOVE 'Numero do Evento SGT  inválido.'
                                         TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *--> Se TS não informado, formatar padrão DB2
           IF SGTSL139-TS-EST-EVT-EXCD-ULT = ' '
              MOVE '0001-01-01-00.00.00.000000'
                                       TO SGTSL139-TS-EST-EVT-EXCD-ULT
           END-IF

           .
       140000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       200000-PROCESSA-PESQUISA SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

           MOVE CD-EST-EVT-EXCD(SGTSL139-IC-TAB) TO
                     SGTSL139-CD-EST-EVT-EXCD(SGTSL139-IC-TAB)
           MOVE CD-USU-ULT-ATL(SGTSL139-IC-TAB) TO
                     SGTSL139-CD-USU-ULT-ATL(SGTSL139-IC-TAB)
           MOVE TS-EST-EVT-EXCD(SGTSL139-IC-TAB) TO
                     SGTSL139-TS-EST-EVT-EXCD(SGTSL139-IC-TAB)
           MOVE TX-JST-EVT-EXCD(SGTSL139-IC-TAB) TO
                     SGTSL139-TX-JST-EVT-EXCD(SGTSL139-IC-TAB)
           .
       200000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970101-ABRE-CURSOR-PSQ SECTION.
      *-----------------------------------------------------------------
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
                   FETCH NEXT ROWSET
                      CURSOR-PSQ FOR 0101 ROWS
                   INTO :CD-EST-EVT-EXCD
                      , :CD-USU-ULT-ATL
                      , :TS-EST-EVT-EXCD
                      , :TX-JST-EVT-EXCD
                        :TX-JST-EVT-EXCD-N
               END-EXEC

           MOVE SQLERRD(3)             TO QT-REG-SELD

           IF  QT-REG-SELD EQUAL 0101
               SET SGTSL139-TEM-MAIS-REGISTRO TO TRUE
               MOVE 0100               TO QT-REG-SELD
           END-IF

           MOVE QT-REG-SELD            TO SGTSL139-QT-OCR-TAB
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

           SET PUT-CONTAINER           TO TRUE
           SET ERRO-CONTAINER          TO TRUE

           EXEC CICS PUT
              CONTAINER ( 'SGTSL139-RPST'         )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSL139-RPST )
              FROM      ( SGTSL139-RPST           )
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



           MOVE 'SGTSL139-E-CD-TIP-SRVC'     TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL139-E-CD-TIP-SRVC       TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL139-CD-ITMT-FNCO'    TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL139-CD-ITMT-FNCO      TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL139-CD-GR-EVT-FDAR'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL139-CD-GR-EVT-FDAR       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL139-NR-SEQL-EVT-EXCD'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL139-NR-SEQL-EVT-EXCD     TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL139-TS-EST-EVT-EXCD-ULT'
                                             TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL139-TS-EST-EVT-EXCD-ULT TO MSGKW999-VL-VRV-MNT
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