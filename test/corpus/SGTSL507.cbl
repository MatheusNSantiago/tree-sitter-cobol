      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSL507
      * ANALISTA..: F2054109 - Cintia Ap Soares Iglesias
      * AUTOR.....: F9188847 - Soila Akamatsu
      * OBJETIVO..: Lista Operações que são pagas pela Operação Informa-
      *             da. tabela DB2SGT.OPR_PG_OTR_OPR.
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 04/05/2017 F9188847 DE 2016/145195 - IMPLANTACAO

      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSL507.

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
       77  CTE-INICIO-SS                 PIC  X(040)  VALUE
                   '*** S.S. COMEÇA AQUI ***'.
       77  CTE-PROG     PIC  X(17) VALUE '*** SGTSL507 ***'.
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

      * Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1

      * Area de trabalho
      *--> Auxiliares para movimentação na monitoração de variáveis.
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).

       01 QT-REG-SELD                         PIC S9(4) COMP-5 VALUE 0.

      * Campos utilizados para leitura
       01  REG-WS.
           03 CD-CTRA-OPR-VCLD         PIC S9(09) COMP-5
                                       OCCURS 0101 TIMES  VALUE ZEROS.
           03 CD-OPR-ETRD-VCLD         PIC S9(11) COMP-3
                                       OCCURS 0101 TIMES  VALUE ZEROS.
           03 NR-VRS-OPR-VCLD          PIC S9(04) COMP-5
                                       OCCURS 0101 TIMES  VALUE ZEROS.
           03 VL-PGTO                  PIC S9(15)V9(2) COMP-3
                                       OCCURS 0101 TIMES  VALUE ZEROS.

      * Indicadores de nulidades

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

      *-----------------------------------------------------------------
-INC SGTKL507

           EXEC SQL
             DECLARE CURSOR-PSQ CURSOR
                     WITH ROWSET POSITIONING FOR
             SELECT A.CD_CTRA_OPR_VCLD
                  , A.CD_OPR_ETRD_VCLD
                  , A.NR_VRS_OPR_VCLD
                  , A.VL_PGTO
               FROM DB2SGT.OPR_PG_OTR_OPR     A
              WHERE A.CD_CTRA_OPR_PGDR    = :SGTSL507-CD-CTRA-OPR-PGDR
                AND A.CD_OPR_PGDR         = :SGTSL507-CD-OPR-PGDR
                AND A.NR_VRS_OPR_PGDR     = :SGTSL507-NR-VRS-OPR-PGDR
                AND A.CD_CTRA_OPR_VCLD    = :SGTSL507-CD-CTRA-OPR-VCLD-E
                AND ( A.CD_OPR_ETRD_VCLD  > :SGTSL507-CD-OPR-ETRD-VCLD-E
                 OR (A.CD_OPR_ETRD_VCLD   = :SGTSL507-CD-OPR-ETRD-VCLD-E
                 AND  A.NR_VRS_OPR_VCLD  > :SGTSL507-NR-VRS-OPR-VCLD-E))
              ORDER BY A.CD_OPR_ETRD_VCLD,  A.NR_VRS_OPR_VCLD
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
                    VARYING SGTSL507-IC-TAB FROM 1 BY 1
                    UNTIL SGTSL507-IC-TAB > SGTSL507-QT-OCR-TAB

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
              CONTAINER ( 'SGTSL507-RPST' )
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
           MOVE LENGTH OF SGTSL507-RQSC     TO QT-CCT-EPRO

           SET GET-CONTAINER           TO TRUE
           SET ERRO-CONTAINER          TO TRUE
           MOVE 'SGTSL507-RQSC'        TO NM-CTNR

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
              FLENGTH   ( LENGTH OF SGTSL507-RQSC )
              INTO      ( SGTSL507-RQSC           )
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

           MOVE 'N'                    TO SGTSL507-IN-CTNC-PSQ

           IF  SGTSL507-CD-CTRA-OPR-PGDR LESS ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0497              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira da operação pagadora inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL507-CD-OPR-PGDR     NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0498              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação pagadora não numérico.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL507-NR-VRS-OPR-PGDR   LESS ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0502              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação pagadora inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

            IF  SGTSL507-CD-CTRA-OPR-VCLD-E  LESS ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0504              TO  MSG1-CD-ERRO-TS
               MOVE 'Código da carteira da operação vinculada inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL507-CD-OPR-ETRD-VCLD-E   LESS ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0507              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturda vinculada inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL507-NR-VRS-OPR-VCLD-E   LESS ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0509              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação vinculada inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-PROCESSA-PESQUISA SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

           MOVE CD-CTRA-OPR-VCLD(SGTSL507-IC-TAB)
                           TO SGTSL507-CD-CTRA-OPR-VCLD(SGTSL507-IC-TAB)
           MOVE CD-OPR-ETRD-VCLD(SGTSL507-IC-TAB)
                           TO SGTSL507-CD-OPR-ETRD-VCLD(SGTSL507-IC-TAB)
           MOVE NR-VRS-OPR-VCLD(SGTSL507-IC-TAB)
                           TO SGTSL507-NR-VRS-OPR-VCLD(SGTSL507-IC-TAB)
           MOVE VL-PGTO(SGTSL507-IC-TAB)
                           TO SGTSL507-VL-PGTO(SGTSL507-IC-TAB)
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
               INTO :CD-CTRA-OPR-VCLD
                  , :CD-OPR-ETRD-VCLD
                  , :NR-VRS-OPR-VCLD
                  , :VL-PGTO
           END-EXEC

           MOVE SQLERRD(3)             TO QT-REG-SELD

           IF  QT-REG-SELD EQUAL 0101
               SET SGTSL507-TEM-MAIS-REGISTRO TO TRUE
               MOVE 0100                TO QT-REG-SELD
           END-IF

           MOVE QT-REG-SELD            TO SGTSL507-QT-OCR-TAB
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
           MOVE 'SGTSL507-RPST'        TO NM-CTNR

           EXEC CICS PUT
              CONTAINER ( NM-CTNR        )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSL507-RPST )
              FROM      ( SGTSL507-RPST           )
           END-EXEC

           SET SEM-ERRO                TO TRUE
           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA SECTION.
      *-----------------------------------------------------------------
           IF  IN-CURSOR-PSQ-ABTO  EQUAL  'S'
               PERFORM 970103-FECHA-CURSOR-PSQ
           END-IF

           IF  MSG1-CD-ERRO-TS EQUAL ZEROS
               MOVE 8888                     TO MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS EQUAL SPACES
               MOVE 'Erro não especificado no módulo.'
                                             TO MSG1-TX-ERRO-TS
           END-IF

      *--> Verifica se o tipo de erro gerado possui tratamento interno
           EVALUATE  TRUE
               WHEN  ERRO-CONTAINER
                     PERFORM  999004-TRATA-ERRO-CTNR
               WHEN  ERRO-TAMANHO-AREA
                     PERFORM  999005-TRATA-ERRO-AREA-DADOS
               WHEN  ERRO-CANAL
                     PERFORM  999006-TRATA-ERRO-CANAL
               WHEN  OTHER
                     CONTINUE
           END-EVALUATE

           MOVE 'SGTSL507-CD-CTRA-OPR-PGDR'    TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL507-CD-CTRA-OPR-PGDR      TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL507-CD-OPR-PGDR'         TO MSGKW999-NM-VRV-MNT
      *------> Verifica se o conteudo e numerico antes de adicionar
            IF  SGTSL507-CD-OPR-PGDR    IS NUMERIC
                MOVE SGTSL507-CD-OPR-PGDR       TO  CD-OPR-ETRD-MCP
                MOVE CD-OPR-ETRD-MCP-TX         TO  MSGKW999-VL-VRV-MNT
            ELSE
                MOVE 'Valor não numérico'       TO  MSGKW999-VL-VRV-MNT
            END-IF

           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL507-NR-VRS-OPR-PGDR'     TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL507-NR-VRS-OPR-PGDR       TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL507-CD-CTRA-OPR-VCLD-E'  TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL507-CD-CTRA-OPR-VCLD-E    TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL507-CD-OPR-ETRD-VCLD-E'  TO MSGKW999-NM-VRV-MNT
      *------> Verifica se o conteudo e numerico antes de adicionar
           IF  SGTSL507-CD-OPR-ETRD-VCLD-E    IS NUMERIC
               MOVE SGTSL507-CD-OPR-ETRD-VCLD-E  TO  CD-OPR-ETRD-MCP
               MOVE CD-OPR-ETRD-MCP-TX         TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE 'Valor não numérico'       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL507-NR-VRS-OPR-VCLD-E'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL507-NR-VRS-OPR-VCLD-E     TO MSGKW999-VL-VRV-MNT
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
      *----------------------------------------
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *----------------------------------------
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