      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSL069
      * ANALISTA..: F2054109 - Cintia Ap Soares Iglesias
      * AUTOR.....: F2804421 - Eliana Cristina Ramos da Silva
      * OBJETIVO..: Lista container da tabela EST_VRS_OPR_ETRD
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS002 17.07.2019 - F2804421 DE-2016/145195 - Inclusão do servi-
      *        ço 3 consulta de estado aprovado de versão (tipo=3).
      *
      * VRS001 02.10.2018 - F8785286 DE-2016/145195 - Inclusão do srv2 e
      *        dos campos CD-OPR-ETRD-MCP, NR-VRS-OPR-ETRD,
      *        CD-TIP EST-OPR e TS-EST OPR-ETRD.
      *        05.09.2017 - F2804421 DE-2016/145195 - Implantação
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSL069.

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
                   '*** S.S. COMECA AQUI ***'.
       77  CTE-PROG     PIC  X(17) VALUE '*** SGTSL069 ***'.
       77  CTE-VERS     PIC  X(06) VALUE 'VRS002'.

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

      *--> Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1

      * Area de trabalho

      *--> Auxiliares para movimentação na monitoração de variáveis.
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).

       01 QT-REG-SELD                  PIC S9(4) COMP-5 VALUE 0.

      *--> Auxiliares para pesquisa estado versão = 3 - aprovada.
       01 CD-EST-APVD                 PIC S9(4) COMP-5 VALUE 3.

      * Campos utilizados para leitura

       01  REG-WS.
000760     03 CD-OPR-ETRD-MCP-S        PIC S9(11) COMP-3
000760        OCCURS 101 TIMES         VALUE 0.
000760     03 NR-VRS-OPR-ETRD          PIC S9(4) COMP-5
000760        OCCURS 101 TIMES         VALUE 0.
           03 CD-TIP-EST-VRS           PIC S9(4) COMP-5
              OCCURS 101 TIMES         VALUE 0.
           03 CD-USU-ULT-ATL           PIC X(8)
              OCCURS 101 TIMES         VALUE ' '.
           03 TS-EST-OPR-ETRD          PIC X(26)
              OCCURS 101 TIMES         VALUE ' '.
           03 TS-EST-VRS               PIC X(26)
              OCCURS 101 TIMES         VALUE ' '.
           03 TX-OBS-EST-VRS           PIC X(200)
              OCCURS 101 TIMES         VALUE ' '.
000760     03 CD-TIP-EST-OPR           PIC S9(9) COMP-5
000760        OCCURS 101 TIMES         VALUE 0.
           03 TX-OBS-EST-VRS-N         PIC S9(4) COMP-5
              OCCURS 101 TIMES         VALUE 0.

      * Indicadores de nulidades

      * Variavel(is) de controle do cursor(es)

       01  CTL-CURSOR-PSQ.
           03  CD-FIM-CURSOR-PSQ       PIC S9(04) COMP-5 VALUE 0.
               88  FIM-CURSOR-PSQ      VALUE +100.
           03  IN-CURSOR-PSQ-ABTO      PIC  X(01) VALUE 'N'.
               88  CURSOR-PSQ-ABTO     VALUE 'S'.

      * Subrotinas utilizadas

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

      *-----------------------------------------------------------------
-INC SGTKL069
           EXEC SQL
             DECLARE CURSOR-PSQ-01 CURSOR
                     WITH ROWSET POSITIONING FOR
             SELECT A.CD_TIP_EST_VRS
                  , A.CD_USU_ULT_ATL
                  , A.TS_EST_VRS
                  , A.TX_OBS_EST_VRS
               FROM DB2SGT.EST_VRS_OPR_ETRD   A
              WHERE A.CD_CTRA_TIT_VL    = :SGTSL069-CD-CTRA-TIT-VL
                AND A.CD_OPR_ETRD_MCP   = :SGTSL069-CD-OPR-ETRD-MCP
                AND A.NR_VRS_OPR_ETRD   = :SGTSL069-NR-VRS-OPR-ETRD
                AND A.TS_EST_VRS        > :SGTSL069-TS-EST-VRS-PAG
           END-EXEC.

      *-----------------------------------------------------------------
      *   Lista estado da versão por operação, independente da versão
      *     da operação.
      *-----------------------------------------------------------------
           EXEC SQL
             DECLARE CURSOR-PSQ-02 CURSOR
                     WITH ROWSET POSITIONING FOR
             SELECT A.CD_OPR_ETRD_MCP
                  , A.NR_VRS_OPR_ETRD
                  , B.CD_TIP_EST_OPR
                  , A.CD_TIP_EST_VRS
                  , B.TS_EST_OPR_ETRD
                  , A.TS_EST_VRS
                  , A.CD_USU_ULT_ATL
                  , A.TX_OBS_EST_VRS
             FROM DB2SGT.EST_VRS_OPR_ETRD   A

             INNER JOIN DB2SGT.EST_OPR_ETRD B
                 ON A.CD_CTRA_TIT_VL  = B.CD_CTRA_TIT_VL
                AND A.CD_OPR_ETRD_MCP = B.CD_OPR_ETRD_MCP
                AND A.NR_VRS_OPR_ETRD = B.NR_VRS_OPR_ETRD

             WHERE  A.CD_CTRA_TIT_VL    = :SGTSL069-CD-CTRA-TIT-VL
                AND A.CD_OPR_ETRD_MCP   = :SGTSL069-CD-OPR-ETRD-MCP
                AND A.TS_EST_VRS  =  (SELECT MAX (C.TS_EST_VRS)
                   FROM DB2SGT.EST_VRS_OPR_ETRD C
                   WHERE C.CD_CTRA_TIT_VL   =  A.CD_CTRA_TIT_VL
                   AND   C.CD_OPR_ETRD_MCP  =  A.CD_OPR_ETRD_MCP
                   AND   C.NR_VRS_OPR_ETRD  =  A.NR_VRS_OPR_ETRD)
                AND B.TS_EST_OPR_ETRD  =
                    (SELECT MAX (D.TS_EST_OPR_ETRD)
                    FROM DB2SGT.EST_OPR_ETRD  D
                    WHERE D.CD_CTRA_TIT_VL   = B.CD_CTRA_TIT_VL
                    AND   D.CD_OPR_ETRD_MCP  = B.CD_OPR_ETRD_MCP
                    AND   D.NR_VRS_OPR_ETRD  = B.NR_VRS_OPR_ETRD)

             ORDER BY A.TS_EST_VRS DESC
           END-EXEC.

      *-----------------------------------------------------------------
      *   Lista estado aprovado (3=vigente) da versão da operação.
      *-----------------------------------------------------------------
           EXEC SQL
             DECLARE CURSOR-PSQ-03 CURSOR
                     WITH ROWSET POSITIONING FOR
             SELECT A.CD_TIP_EST_VRS
                  , A.CD_USU_ULT_ATL
                  , A.TS_EST_VRS
                  , A.TX_OBS_EST_VRS
               FROM DB2SGT.EST_VRS_OPR_ETRD   A
             WHERE  A.CD_CTRA_TIT_VL    = :SGTSL069-CD-CTRA-TIT-VL
                AND A.CD_OPR_ETRD_MCP   = :SGTSL069-CD-OPR-ETRD-MCP
                AND A.NR_VRS_OPR_ETRD   = :SGTSL069-NR-VRS-OPR-ETRD
                AND A.CD_TIP_EST_VRS    = :CD-EST-APVD

             ORDER BY A.TS_EST_VRS DESC
           END-EXEC.

        01  CTE-FINAL-SS                PIC  X(40)      VALUE
                    '*** S.S. TERMINA AQUI ***'.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL          SECTION.

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
                    VARYING SGTSL069-IC-TAB FROM 1 BY 1
                    UNTIL SGTSL069-IC-TAB > SGTSL069-QT-OCR-TAB

           PERFORM 300000-POSTA-RESPOSTA
           GOBACK
           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-DESCOBRIR-CANAL SECTION.
      *-----------------------------------------------------------------
           MOVE 0001                   TO MSG1-CD-SEQL-ERRO

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
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS DELETE
              CONTAINER ( 'SGTSL069-RPST' )
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

           MOVE ZEROS                  TO QT-CCT-LIDO
           MOVE LENGTH OF SGTSL069-RQSC
                                       TO QT-CCT-EPRO

           SET GET-CONTAINER           TO TRUE
           SET ERRO-CONTAINER          TO TRUE
           MOVE 'SGTSL069-RQSC'        TO NM-CTNR

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
              FLENGTH   ( LENGTH OF SGTSL069-RQSC )
              INTO      ( SGTSL069-RQSC           )
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

           MOVE 'N'                    TO SGTSL069-IN-CNTO-PSQ

000760     IF  SGTSL069-CD-TIP-SRVC        EQUAL ZEROS   OR
000760         SGTSL069-CD-TIP-SRVC        GREATER 3
000760         SET  ERRO-PROGRAMADO        TO  TRUE
000760         MOVE 107                    TO  MSG1-CD-ERRO-TS
000760         MOVE 'Código do tipo de serviço inválido.'
000760                                     TO  MSG1-TX-ERRO-TS
000760         PERFORM 999000-GRAVA-ERRO-E-ENCERRA
000760     END-IF
000760
           IF  SGTSL069-CD-CTRA-TIT-VL EQUAL ZEROS
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 0002                TO MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de título e valores mobiliários
      -             'não informado.'    TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL069-CD-OPR-ETRD-MCP NOT NUMERIC
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 0003                TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -             'numérico.'         TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL069-CD-OPR-ETRD-MCP EQUAL ZEROS
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 0005                TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -             ' informado.'       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

000760     IF  SGTSL069-LISTA-TODOS
               IF  SGTSL069-NR-VRS-OPR-ETRD EQUAL ZEROS
                   SET  ERRO-PROGRAMADO     TO TRUE
                   MOVE 0007                TO MSG1-CD-ERRO-TS
                   MOVE 'Número da versão da operação não informado.'
                                            TO MSG1-TX-ERRO-TS
                   PERFORM 999000-GRAVA-ERRO-E-ENCERRA
               END-IF
           END-IF

           IF  SGTSL069-TS-EST-VRS-PAG  EQUAL SPACES
               MOVE '1901-01-01-00.00.00.00000'
                                        TO SGTSL069-TS-EST-VRS-PAG
           END-IF

           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-PROCESSA-PESQUISA SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

000760     EVALUATE  TRUE
000760         WHEN  SGTSL069-LISTA-TODOS
                     MOVE CD-TIP-EST-VRS(SGTSL069-IC-TAB) TO
                          SGTSL069-CD-TIP-EST-VRS(SGTSL069-IC-TAB)
                     MOVE CD-USU-ULT-ATL(SGTSL069-IC-TAB) TO
                          SGTSL069-CD-USU-ULT-ATL(SGTSL069-IC-TAB)
                     MOVE TS-EST-VRS(SGTSL069-IC-TAB) TO
                          SGTSL069-TS-EST-VRS(SGTSL069-IC-TAB)
                     MOVE TX-OBS-EST-VRS(SGTSL069-IC-TAB) TO
                          SGTSL069-TX-OBS-EST-VRS(SGTSL069-IC-TAB)

000760         WHEN  SGTSL069-LISTA-OPR
                     MOVE CD-OPR-ETRD-MCP-S(SGTSL069-IC-TAB)   TO
                          SGTSL069-CD-OPR-ETRD-MCP-S(SGTSL069-IC-TAB)
                     MOVE NR-VRS-OPR-ETRD(SGTSL069-IC-TAB)     TO
                          SGTSL069-NR-VRS-OPR-ETRD-S(SGTSL069-IC-TAB)
                     MOVE CD-TIP-EST-OPR(SGTSL069-IC-TAB)      TO
                          SGTSL069-CD-TIP-EST-OPR(SGTSL069-IC-TAB)
                     MOVE CD-TIP-EST-VRS(SGTSL069-IC-TAB)      TO
                          SGTSL069-CD-TIP-EST-VRS(SGTSL069-IC-TAB)
                     MOVE TS-EST-OPR-ETRD(SGTSL069-IC-TAB)     TO
                          SGTSL069-TS-EST-OPR-ETRD(SGTSL069-IC-TAB)
                     MOVE TS-EST-VRS(SGTSL069-IC-TAB)          TO
                          SGTSL069-TS-EST-VRS(SGTSL069-IC-TAB)
                     MOVE CD-USU-ULT-ATL(SGTSL069-IC-TAB)      TO
                          SGTSL069-CD-USU-ULT-ATL(SGTSL069-IC-TAB)
                     MOVE TX-OBS-EST-VRS(SGTSL069-IC-TAB)      TO
                          SGTSL069-TX-OBS-EST-VRS(SGTSL069-IC-TAB)

               WHEN  SGTSL069-LISTA-APVD
                     MOVE CD-TIP-EST-VRS(SGTSL069-IC-TAB)      TO
                          SGTSL069-CD-TIP-EST-VRS(SGTSL069-IC-TAB)
                     MOVE CD-USU-ULT-ATL(SGTSL069-IC-TAB)      TO
                          SGTSL069-CD-USU-ULT-ATL(SGTSL069-IC-TAB)
                     MOVE TS-EST-VRS(SGTSL069-IC-TAB)          TO
                          SGTSL069-TS-EST-VRS(SGTSL069-IC-TAB)
                     MOVE TX-OBS-EST-VRS(SGTSL069-IC-TAB)      TO
                          SGTSL069-TX-OBS-EST-VRS(SGTSL069-IC-TAB)
           END-EVALUATE
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970101-ABRE-CURSOR-PSQ SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

000760     EVALUATE  TRUE
000760         WHEN  SGTSL069-LISTA-TODOS
                     EXEC SQL
                          OPEN CURSOR-PSQ-01
                     END-EXEC
000760
000760         WHEN  SGTSL069-LISTA-OPR
000760               EXEC SQL
000760                    OPEN CURSOR-PSQ-02
000760               END-EXEC

000760         WHEN  SGTSL069-LISTA-APVD
                     EXEC SQL
                          OPEN CURSOR-PSQ-03
                     END-EXEC

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

000760     EVALUATE  TRUE
000760         WHEN  SGTSL069-LISTA-TODOS
               EXEC SQL
                   FETCH NEXT ROWSET
                       CURSOR-PSQ-01 FOR 0101 ROWS
                   INTO  :CD-TIP-EST-VRS
                       , :CD-USU-ULT-ATL
                       , :TS-EST-VRS
                       , :TX-OBS-EST-VRS
                         :TX-OBS-EST-VRS-N
               END-EXEC

000760         WHEN  SGTSL069-LISTA-OPR
               EXEC SQL
                   FETCH NEXT ROWSET
                       CURSOR-PSQ-02 FOR 0101 ROWS
                   INTO  :CD-OPR-ETRD-MCP-S
                       , :NR-VRS-OPR-ETRD
                       , :CD-TIP-EST-OPR
                       , :CD-TIP-EST-VRS
                       , :TS-EST-OPR-ETRD
                       , :TS-EST-VRS
                       , :CD-USU-ULT-ATL
                       , :TX-OBS-EST-VRS
                         :TX-OBS-EST-VRS-N
               END-EXEC

               WHEN  SGTSL069-LISTA-APVD
               EXEC SQL
                   FETCH NEXT ROWSET
                       CURSOR-PSQ-03 FOR 0101 ROWS
                   INTO  :CD-TIP-EST-VRS
                       , :CD-USU-ULT-ATL
                       , :TS-EST-VRS
                       , :TX-OBS-EST-VRS
                         :TX-OBS-EST-VRS-N

               END-EXEC
           END-EVALUATE

           MOVE SQLERRD(3)             TO QT-REG-SELD

           IF  QT-REG-SELD EQUAL 0101
               SET SGTSL069-TEM-MAIS-REGISTRO TO TRUE
               MOVE 0100                TO QT-REG-SELD
           END-IF

           MOVE QT-REG-SELD            TO SGTSL069-QT-OCR-TAB
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
000760         WHEN  SGTSL069-LISTA-TODOS
                     EXEC SQL
                          CLOSE CURSOR-PSQ-01
                     END-EXEC

000760         WHEN  SGTSL069-LISTA-OPR
                     EXEC SQL
                          CLOSE CURSOR-PSQ-02
                     END-EXEC

000760         WHEN  SGTSL069-LISTA-APVD
                     EXEC SQL
                          CLOSE CURSOR-PSQ-03
                     END-EXEC
           END-EVALUATE
           .
       970103-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-POSTA-RESPOSTA SECTION.
      *-----------------------------------------------------------------
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO

           SET  PUT-CONTAINER          TO TRUE
           SET  ERRO-CONTAINER         TO TRUE
           MOVE 'SGTSL069-RPST'        TO NM-CTNR

           EXEC CICS PUT
              CONTAINER ( NM-CTNR                 )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSL069-RPST )
              FROM      ( SGTSL069-RPST           )
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

           MOVE 'SGTSL069-CD-CTRA-TIT-VL'    TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL069-CD-CTRA-TIT-VL      TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL069-CD-OPR-ETRD-MCP'   TO MSGKW999-NM-VRV-MNT
      *------> Verifica se o conteudo e numerico antes de adicionar
           IF  SGTSL069-CD-OPR-ETRD-MCP    IS NUMERIC
               MOVE SGTSL069-CD-OPR-ETRD-MCP TO  CD-OPR-ETRD-MCP
               MOVE CD-OPR-ETRD-MCP-TX       TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE 'Valor não numérico'     TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL069-NR-VRS-OPR-ETRD'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL069-NR-VRS-OPR-ETRD     TO MSGKW999-VL-VRV-MNT
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
      * 999004-ALINHA-NUMERO-ESQUERDA  SECTION.
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
           END-EXEC
           .
       999999-SAI.
           EXIT.