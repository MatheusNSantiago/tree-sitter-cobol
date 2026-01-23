      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSL009
      * ANALISTA..: F2054109 - Cintia Ap Soares Iglesias
      * AUTOR.....: F8658869 - Roberto Ioshimoto
      * OBJETIVO..: Listar TABELA DB2SGT.EST_OPR_ETRD
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 13.11.2018 - F8785286 - D145195/P13773 - Incl serv-2
      *        09.06.2017 - F86592438 - D145195/P13773 - Implantação
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSL009.
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
       77  CTE-PROG     PIC  X(17) VALUE '*** SGTSL009 ***'.
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

      * Book para trataro container
-INC SGTKERR1

      * Area de trabalho
      *--> Auxiliares para movimentação na monitoração de variáveis.
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).


       01  QT-REG-SELD                      PIC S9(4) COMP-5 VALUE 0.

      * Campos utilizados para leitura

       01  REG-WS.
           03  CD-TIP-EST-OPR               PIC S9(9) COMP-5
               OCCURS 101 TIMES             VALUE 0.
           03  CD-TIP-EST-OPR-N             PIC S9(4) COMP-5
               OCCURS 101 TIMES             VALUE 0.
           03  CD-USU-RSP-EST               PIC X(8)
               OCCURS 101 TIMES             VALUE ' '.
           03  TS-EST-OPR-ETRD              PIC X(26)
               OCCURS 101 TIMES             VALUE ' '.
           03  TX-JST-EST                   PIC X(100)
               OCCURS 101 TIMES             VALUE ' '.

           03 CD-TIP-EST-VRS           PIC S9(4) COMP-5
              OCCURS 101 TIMES         VALUE 0.
           03 TS-EST-VRS               PIC X(26)
              OCCURS 101 TIMES         VALUE ' '.

           03  TX-JST-EST-N                 PIC S9(4) COMP-5
               OCCURS 101 TIMES             VALUE 0.
           03  CD-MTV-ALT                   PIC S9(4) COMP-5
               OCCURS 101 TIMES             VALUE 0.
           03  CD-MTV-ALT-N                 PIC S9(4) COMP-5
               OCCURS 101 TIMES             VALUE 0.

      * Indicadores de nulidades

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
       01  SGTKL009-DADOS.
-INC SGTKL009

           EXEC SQL
             DECLARE CURSOR-PSQ CURSOR
                     WITH ROWSET POSITIONING FOR
             SELECT A.CD_TIP_EST_OPR
                  , A.CD_USU_RSP_EST
                  , A.TS_EST_OPR_ETRD
                  , A.TX_JST_EST
                  , A.CD_MTV_ALT
               FROM DB2SGT.EST_OPR_ETRD       A
              WHERE A.CD_CTRA_TIT_VL     = :SGTSL009-CD-CTRA-TIT-VL
                AND A.CD_OPR_ETRD_MCP    = :SGTSL009-CD-OPR-ETRD-MCP
                AND A.NR_VRS_OPR_ETRD    = :SGTSL009-NR-VRS-OPR-ETRD
                AND A.TS_EST_OPR_ETRD    > :SGTSL009-TS-EST-OPR-ETRD-ULT
              ORDER BY  A.TS_EST_OPR_ETRD  DESC
           END-EXEC.

      *-----------------------------------------------------------------
      *-   Consulta estado da operação e estado da versão
      *-      Classificação: timestamp estado operação decrescente
      *-----------------------------------------------------------------
           EXEC SQL
             DECLARE CURSOR-PSQ-2 CURSOR
                     WITH ROWSET POSITIONING FOR
                SELECT A.CD_USU_RSP_EST
                     , A.CD_TIP_EST_OPR
                     , A.TX_JST_EST
                     , A.CD_MTV_ALT
                     , A.TS_EST_OPR_ETRD
                     , B.CD_TIP_EST_VRS

                FROM DB2SGT.EST_OPR_ETRD  A
                JOIN DB2SGT.EST_VRS_OPR_ETRD  B
                  ON  B.CD_CTRA_TIT_VL  =  A.CD_CTRA_TIT_VL
                 AND  B.CD_OPR_ETRD_MCP =  A.CD_OPR_ETRD_MCP
                 AND  B.NR_VRS_OPR_ETRD =  A.NR_VRS_OPR_ETRD

                WHERE A.CD_CTRA_TIT_VL    = :SGTSL009-CD-CTRA-TIT-VL
                  AND A.CD_OPR_ETRD_MCP   = :SGTSL009-CD-OPR-ETRD-MCP
                  AND A.NR_VRS_OPR_ETRD   = :SGTSL009-NR-VRS-OPR-ETRD
                  AND A.TS_EST_OPR_ETRD   =
                                     (SELECT MAX (C.TS_EST_OPR_ETRD)
                      FROM DB2SGT.EST_OPR_ETRD C
                      WHERE C.CD_CTRA_TIT_VL  =  A.CD_CTRA_TIT_VL
                        AND C.CD_OPR_ETRD_MCP =  A.CD_OPR_ETRD_MCP
                        AND C.NR_VRS_OPR_ETRD =  A.NR_VRS_OPR_ETRD)

                  AND B.TS_EST_VRS  =  (SELECT MAX (D.TS_EST_VRS)
                      FROM DB2SGT.EST_VRS_OPR_ETRD D
                      WHERE D.CD_CTRA_TIT_VL  = B.CD_CTRA_TIT_VL
                        AND D.CD_OPR_ETRD_MCP = B.CD_OPR_ETRD_MCP
                        AND D.NR_VRS_OPR_ETRD = B.NR_VRS_OPR_ETRD)

              ORDER BY  A.TS_EST_OPR_ETRD  DESC
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
           PERFORM 970103-FECHA-CURSOR-PSQ
           PERFORM 200000-PROCESSA-PESQUISA
                    VARYING SGTSL009-IC-TAB FROM 1 BY 1
                    UNTIL SGTSL009-IC-TAB > SGTSL009-QT-OCR-TAB

           PERFORM 300000-POSTA-RESPOSTA
           GOBACK
           .
      *-----------------------------------------------------------------
       100000-DESCOBRIR-CANAL SECTION.
      *-----------------------------------------------------------------
           MOVE 0001                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS ASSIGN
              CHANNEL ( NM-CNL )
           END-EXEC
           .
       100000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       110000-EXCLUIR-CTNR-RPST SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS DELETE
              CONTAINER ( 'SGTSL009-RPST' )
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
           MOVE LENGTH OF SGTSL009-RQSC      TO QT-CCT-EPRO
           EXEC CICS GET
              CONTAINER ( 'SGTSL009-RQSC' )
              CHANNEL   ( NM-CNL          )
              NODATA
              FLENGTH   ( QT-CCT-LIDO     )
           END-EXEC

           IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
               SET  ERRO-TAMANHO-AREA     TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS GET
              CONTAINER ( 'SGTSL009-RQSC'         )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSL009-RQSC )
              INTO      ( SGTSL009-RQSC           )
           END-EXEC
           .
       130000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       140000-VALIDAR-PARAMETRO SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

           MOVE 'N'                    TO SGTSL009-IN-CNTO-PSQ

           IF  SGTSL009-E-CD-TIP-SRVC   > 2
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 0107                TO MSG1-CD-ERRO-TS
               MOVE 'Código do tipo de serviço inválido.'
                                        TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL009-CD-CTRA-TIT-VL EQUAL ZEROS
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 0002                TO MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de título e valores mobiliários
      -             'não informado.'    TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL009-CD-OPR-ETRD-MCP NOT NUMERIC
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 0003                TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -             ' numérico.'        TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL009-CD-OPR-ETRD-MCP EQUAL ZEROS
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 0005                TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -             ' informado.'       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL009-NR-VRS-OPR-ETRD EQUAL ZEROS
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 0007                TO MSG1-CD-ERRO-TS
               MOVE 'Número da versão da operação não informado.'
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

           MOVE CD-TIP-EST-OPR(SGTSL009-IC-TAB) TO
                     SGTSL009-CD-TIP-EST-OPR(SGTSL009-IC-TAB)
           MOVE CD-USU-RSP-EST(SGTSL009-IC-TAB) TO
                     SGTSL009-CD-USU-RSP-EST(SGTSL009-IC-TAB)
           MOVE TS-EST-OPR-ETRD(SGTSL009-IC-TAB) TO
                     SGTSL009-TS-EST-OPR-ETRD(SGTSL009-IC-TAB)
           MOVE TX-JST-EST(SGTSL009-IC-TAB) TO
                     SGTSL009-TX-JST-EST(SGTSL009-IC-TAB)
           MOVE CD-MTV-ALT(SGTSL009-IC-TAB) TO
                     SGTSL009-CD-MTV-ALT(SGTSL009-IC-TAB)
           MOVE CD-TIP-EST-VRS(SGTSL009-IC-TAB) TO
                     SGTSL009-CD-TIP-EST-VRS(SGTSL009-IC-TAB)
           .
       200000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970101-ABRE-CURSOR-PSQ SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

000760     EVALUATE  TRUE
000760         WHEN  SGTSL009-LISTA-POR-OPR
                     EXEC SQL
                          OPEN CURSOR-PSQ
                     END-EXEC
000760
000760         WHEN  SGTSL009-LISTA-EST-OPR
000760               EXEC SQL
000760                    OPEN CURSOR-PSQ-2
000760               END-EXEC
000760     END-EVALUATE

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
               WHEN  SGTSL009-LISTA-POR-OPR
               EXEC SQL
                   FETCH NEXT ROWSET
                      CURSOR-PSQ FOR 0101 ROWS
                   INTO :CD-TIP-EST-OPR
                        :CD-TIP-EST-OPR-N
                      , :CD-USU-RSP-EST
                      , :TS-EST-OPR-ETRD
                      , :TX-JST-EST
                        :TX-JST-EST-N
                      , :CD-MTV-ALT
                        :CD-MTV-ALT-N
               END-EXEC

               WHEN  SGTSL009-LISTA-EST-OPR
               EXEC SQL
                   FETCH NEXT ROWSET
                      CURSOR-PSQ-2 FOR 0101 ROWS
                   INTO :CD-USU-RSP-EST
                      , :CD-TIP-EST-OPR
                      , :TX-JST-EST
                        :TX-JST-EST-N
                      , :CD-MTV-ALT
                        :CD-MTV-ALT-N
                      , :TS-EST-OPR-ETRD
                      , :CD-TIP-EST-VRS

               END-EXEC

           END-EVALUATE


           MOVE SQLERRD(3)             TO QT-REG-SELD

           IF  QT-REG-SELD EQUAL 0101
               SET SGTSL009-TEM-MAIS-REGISTRO TO TRUE
               MOVE 0100               TO QT-REG-SELD
           END-IF

           MOVE QT-REG-SELD            TO SGTSL009-QT-OCR-TAB
           MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ
           .
       970102-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970103-FECHA-CURSOR-PSQ SECTION.
      *-----------------------------------------------------------------
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO

           MOVE 'N'                    TO IN-CURSOR-PSQ-ABTO

000760     EVALUATE  TRUE
000760         WHEN  SGTSL009-LISTA-POR-OPR
                     EXEC SQL
                          CLOSE CURSOR-PSQ
                     END-EXEC

000760         WHEN  SGTSL009-LISTA-EST-OPR
                     EXEC SQL
                          CLOSE CURSOR-PSQ-2
                     END-EXEC
           END-EVALUATE
           .
       970103-SAI.
           EXIT.
      *-----------------------------------------------------------------
       300000-POSTA-RESPOSTA SECTION.
      *-----------------------------------------------------------------
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS PUT
              CONTAINER ( 'SGTSL009-RPST'         )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSL009-RPST )
              FROM      ( SGTSL009-RPST           )
           END-EXEC
           .
       300000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA SECTION.
      *-----------------------------------------------------------------
           IF  MSG1-CD-ERRO-TS               EQUAL  ZEROS
               MOVE  8888                    TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS               EQUAL  SPACES
               MOVE  'Erro não especificado no módulo.'
                                             TO  MSG1-TX-ERRO-TS
           END-IF

           IF  IN-CURSOR-PSQ-ABTO  EQUAL  'S'
               PERFORM 970103-FECHA-CURSOR-PSQ
               MOVE 'N'                      TO IN-CURSOR-PSQ-ABTO
           END-IF

           MOVE 'SGTSL009-E-CD-TIP-SRVC'     TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL009-E-CD-TIP-SRVC       TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL009-CD-CTRA-TIT-VL'    TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL009-CD-CTRA-TIT-VL      TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL009-CD-OPR-ETRD-MCP'   TO MSGKW999-NM-VRV-MNT
      *------> Verifica se o conteudo e numerico antes de adicionar
           IF  SGTSL009-CD-OPR-ETRD-MCP    IS NUMERIC
               MOVE SGTSL009-CD-OPR-ETRD-MCP TO  CD-OPR-ETRD-MCP
               MOVE CD-OPR-ETRD-MCP-TX       TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico'    TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL009-NR-VRS-OPR-ETRD'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL009-NR-VRS-OPR-ETRD     TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL009-TS-EST-OPR-ETRD-ULT'
                                             TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL009-TS-EST-OPR-ETRD-ULT TO MSGKW999-VL-VRV-MNT
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