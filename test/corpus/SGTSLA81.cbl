      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSLA81
      * ANALISTA..: F8785286 - Roseli A. Kawahata
      * AUTOR.....: F8785286 - Roseli A. Kawahata
      * OBJETIVO..: Lista container da tabela  PRV_CMPS_GRT join com
      *              DB2SGT.CLI_GRTR para devolver clientes com
      *              CD_NTZ_GRT = recebido como parâmetro
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 31.10.2017 - F8785286 Trocar book de MSGKW999 p/ HLPKW999
      *        09.10.2017 - F8785286 2016/145195 - IMPLANTACAO
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSLA81.
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
       77  CTE-INICIO-SS                 PIC  X(040)  VALUE
                   '*** S.S. COMEÇA AQUI ***'.
       77  CTE-PROG     PIC  X(17) VALUE '*** SGTSLA81 ***'.
       77  CTE-VERS     PIC  X(06) VALUE 'VRS001'.

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
-INC SGTKERR1

      * Area de trabalho
      *--> Auxiliares para movimentação na monitoração de variáveis.
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).

       01 AUX-CD-OPR                   PIC S9(11) VALUE 0.
       01 AUX-CD-NTZ-GRT-INI           PIC S9(4) COMP-5 VALUE 1.
       01 AUX-CD-NTZ-GRT-FIM           PIC S9(4) COMP-5 VALUE 99.
       01 QT-REG-SELD                  PIC S9(4) COMP-5 VALUE 0.

      * Campos utilizados para leitura
       01  REG-WS.
           03 CD-FMA-VCLC              PIC S9(4) COMP-5
              OCCURS 101 TIMES         VALUE 0.
           03 CD-NTZ-GRT               PIC S9(4) COMP-5
              OCCURS 101 TIMES         VALUE 0.
           03 CD-USU-ULT-ATL           PIC X(8)
              OCCURS 101 TIMES         VALUE ' '.
           03 DT-LIM-GRT               PIC X(10)
              OCCURS 101 TIMES         VALUE ' '.
           03 NR-PRV-CMPS-GRT          PIC S9(9) COMP-5
              OCCURS 101 TIMES         VALUE 0.
           03 PC-GRT-CMPS-PRVT         PIC S9(4)V9(10) COMP-3
              OCCURS 101 TIMES         VALUE 0.
           03 PC-GRT-CMPS-PRVT-N       PIC S9(4) COMP-5
              OCCURS 101 TIMES         VALUE 0.
           03 TS-ULT-ATL               PIC X(26)
              OCCURS 101 TIMES         VALUE ' '.
           03 VL-GRT-CMPS-PRVT         PIC S9(15)V9(2) COMP-3
              OCCURS 101 TIMES         VALUE 0.
           03 VL-GRT-CMPS-PRVT-N       PIC S9(4) COMP-5
              OCCURS 101 TIMES         VALUE 0.
           03 CD-CLI                   PIC S9(9) COMP-5
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
-INC SGTKLA81
           EXEC SQL
             DECLARE CURSOR-PSQ CURSOR
                     WITH ROWSET POSITIONING FOR
               SELECT A.CD_FMA_VCLC,
                      A.CD_NTZ_GRT,
                      A.CD_USU_ULT_ATL,
                      A.DT_LIM_GRT,
                      A.NR_PRV_CMPS_GRT,
                      A.PC_GRT_CMPS_PRVT,
                      A.TS_ULT_ATL,
                      A.VL_GRT_CMPS_PRVT,
                      B.CD_CLI
               FROM DB2SGT.PRV_CMPS_GRT A

               INNER JOIN  DB2SGT.CLI_GRTR  B
               ON A.CD_CTRA_TIT_VL   =  B.CD_CTRA_TIT_VL   AND
                  A.CD_OPR_ETRD_MCP  =  B.CD_OPR_ETRD_MCP  AND
                  A.NR_VRS_OPR_ETRD  =  B.NR_VRS_OPR_ETRD  AND
                  A.NR_PRV_CMPS_GRT  =  B.NR_PRV_CMPS_GRT

               WHERE
                  A.CD_CTRA_TIT_VL   =  :SGTSLA81-CD-CTRA-TIT-VL   AND
                  A.CD_OPR_ETRD_MCP  =  :SGTSLA81-CD-OPR-ETRD-MCP  AND
                  A.NR_VRS_OPR_ETRD  =  :SGTSLA81-NR-VRS-OPR-ETRD  AND
                  A.CD_NTZ_GRT       >= :AUX-CD-NTZ-GRT-INI        AND
                  A.CD_NTZ_GRT       <= :AUX-CD-NTZ-GRT-FIM        AND
                  A.NR_PRV_CMPS_GRT  >  :SGTSLA81-NR-ULT-PRV-CMPS-GRT

               ORDER BY A.NR_PRV_CMPS_GRT
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
                    VARYING SGTSLA81-IC-TAB FROM 1 BY 1
                    UNTIL SGTSLA81-IC-TAB > SGTSLA81-QT-OCR-TAB

           PERFORM 300000-POSTA-RESPOSTA
           GOBACK
           .
      *-----------------------------------------------------------------
       100000-DESCOBRIR-CANAL          SECTION.
      *-----------------------------------------------------------------
           MOVE 0001                   TO MSG1-CD-SEQL-ERRO

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CANAL             TO  TRUE

           EXEC CICS ASSIGN
              CHANNEL ( NM-CNL )
           END-EXEC

      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO  TRUE
           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-EXCLUIR-CTNR-RPST        SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS DELETE
              CONTAINER ( 'SGTSLA81-RPST' )
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
       120000-EXCLUIR-CTNR-ERRO        SECTION.
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
       130000-RECEBER-REQUISICAO       SECTION.
      *-----------------------------------------------------------------
           MOVE 0004                   TO MSG1-CD-SEQL-ERRO

           MOVE ZEROS                        TO QT-CCT-LIDO
           MOVE LENGTH OF SGTSLA81-RQSC      TO QT-CCT-EPRO

           EXEC CICS GET
              CONTAINER ( 'SGTSLA81-RQSC' )
              CHANNEL   ( NM-CNL          )
              NODATA
              FLENGTH   ( QT-CCT-LIDO     )
           END-EXEC

           IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
               SET   ERRO-TAMANHO-AREA   TO  TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS GET
              CONTAINER ( 'SGTSLA81-RQSC'         )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSLA81-RQSC )
              INTO      ( SGTSLA81-RQSC           )
           END-EXEC
           .
       130000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       140000-VALIDAR-PARAMETRO        SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                   TO  MSG1-CD-SEQL-ERRO

           MOVE 'N'                      TO  SGTSLA81-IN-CTNC-PSQ

            IF  SGTSLA81-CD-OPR-ETRD-MCP NOT NUMERIC
               SET  ERRO-PROGRAMADO      TO  TRUE
               MOVE 0003                 TO  MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturado mercado de capitais não
      -        ' numérico.'              TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSLA81-CD-OPR-ETRD-MCP  EQUAL ZEROS
               SET  ERRO-PROGRAMADO      TO  TRUE
               MOVE 0005                 TO  MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturado mercado de capitais não
      -        ' informado.'             TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSLA81-NR-VRS-OPR-ETRD  EQUAL ZEROS
               SET  ERRO-PROGRAMADO      TO  TRUE
               MOVE 0007                 TO  MSG1-CD-ERRO-TS
               MOVE 'Número versão operação estruturado não informado.'
                                         TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSLA81-NR-ULT-PRV-CMPS-GRT  LESS THAN ZEROS
               SET  ERRO-PROGRAMADO      TO  TRUE
               MOVE 0117                 TO  MSG1-CD-ERRO-TS
               MOVE 'Número da Previsão de Constituição de Garantia invá
      -        'lido'                    TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSLA81-CD-NTZ-GRT-E     NOT EQUAL ZEROES
               MOVE SGTSLA81-CD-NTZ-GRT-E    TO  AUX-CD-NTZ-GRT-INI
                                                 AUX-CD-NTZ-GRT-FIM
           END-IF
           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-PROCESSA-PESQUISA        SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

           MOVE CD-FMA-VCLC(SGTSLA81-IC-TAB)       TO
                             SGTSLA81-CD-FMA-VCLC(SGTSLA81-IC-TAB)
           MOVE CD-NTZ-GRT(SGTSLA81-IC-TAB)        TO
                             SGTSLA81-CD-NTZ-GRT(SGTSLA81-IC-TAB)
           MOVE CD-USU-ULT-ATL(SGTSLA81-IC-TAB)    TO
                             SGTSLA81-CD-USU-ULT-ATL(SGTSLA81-IC-TAB)
           MOVE DT-LIM-GRT(SGTSLA81-IC-TAB)        TO
                             SGTSLA81-DT-LIM-GRT(SGTSLA81-IC-TAB)
           MOVE NR-PRV-CMPS-GRT(SGTSLA81-IC-TAB)   TO
                             SGTSLA81-NR-PRV-CMPS-GRT(SGTSLA81-IC-TAB)
           MOVE PC-GRT-CMPS-PRVT(SGTSLA81-IC-TAB)  TO
                             SGTSLA81-PC-GRT-CMPS-PRVT(SGTSLA81-IC-TAB)
           MOVE TS-ULT-ATL(SGTSLA81-IC-TAB)        TO
                             SGTSLA81-TS-ULT-ATL(SGTSLA81-IC-TAB)
           MOVE VL-GRT-CMPS-PRVT(SGTSLA81-IC-TAB)  TO
                             SGTSLA81-VL-GRT-CMPS-PRVT(SGTSLA81-IC-TAB)
           MOVE CD-CLI(SGTSLA81-IC-TAB)            TO
                             SGTSLA81-CD-CLI(SGTSLA81-IC-TAB)
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-POSTA-RESPOSTA           SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS PUT
              CONTAINER ( 'SGTSLA81-RPST'         )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSLA81-RPST )
              FROM      ( SGTSLA81-RPST           )
           END-EXEC
           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970101-ABRE-CURSOR-PSQ          SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

           EXEC SQL
                OPEN CURSOR-PSQ
           END-EXEC

           MOVE 'S'                    TO IN-CURSOR-PSQ-ABTO
           .
       970101-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970102-LER-CURSOR-PSQ           SECTION.
      *-----------------------------------------------------------------
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE REG-WS

           EXEC SQL
                FETCH NEXT ROWSET
                      CURSOR-PSQ FOR 0101 ROWS
               INTO :CD-FMA-VCLC
                  , :CD-NTZ-GRT
                  , :CD-USU-ULT-ATL
                  , :DT-LIM-GRT
                  , :NR-PRV-CMPS-GRT
                  , :PC-GRT-CMPS-PRVT
                    :PC-GRT-CMPS-PRVT-N
                  , :TS-ULT-ATL
                  , :VL-GRT-CMPS-PRVT
                    :VL-GRT-CMPS-PRVT-N
                  , :CD-CLI
           END-EXEC

           MOVE SQLERRD(3)             TO QT-REG-SELD

           IF  QT-REG-SELD EQUAL 0101
               SET SGTSLA81-TEM-MAIS-REGISTRO TO TRUE
               MOVE 0100               TO QT-REG-SELD
           END-IF

           MOVE QT-REG-SELD            TO SGTSLA81-QT-OCR-TAB
           MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ
           .
       970102-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970103-FECHA-CURSOR-PSQ         SECTION.
      *-----------------------------------------------------------------
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO

           MOVE 'N'                    TO IN-CURSOR-PSQ-ABTO
           EXEC SQL
                CLOSE CURSOR-PSQ
           END-EXEC
           .
       970103-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
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

           MOVE 'SGTSLA81-CD-OPR-ETRD-MCP' TO MSGKW999-NM-VRV-MNT
           IF  SGTSLA81-CD-OPR-ETRD-MCP        NUMERIC
               MOVE SGTSLA81-CD-OPR-ETRD-MCP   TO  CD-OPR-ETRD-MCP
               MOVE CD-OPR-ETRD-MCP-TX         TO MSGKW999-VL-VRV-MNT
           ELSE
               MOVE 'Valor não numérico.'      TO MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSLA81-NR-VRS-OPR-ETRD'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSLA81-NR-VRS-OPR-ETRD     TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSLA81-CD-NTZ-GRT-E'      TO MSGKW999-NM-VRV-MNT
           MOVE SGTSLA81-CD-NTZ-GRT-E        TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSLA81-NR-ULT-PRV-CMPS-GRT'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSLA81-NR-ULT-PRV-CMPS-GRT     TO MSGKW999-VL-VRV-MNT
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
      * --------------------------------------*
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