      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSL081
      * ANALISTA..: F9188847 - Soila K. L. Akamatsu
      * AUTOR.....: F9188847 - Soila K. L. Akamatsu
      * OBJETIVO..: LISTA CONTAINER DA TABELA PRV_CMPS_GRT
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 25.07.2018 - F2804421 2016/145195 - Acerto na mensageria,
      *        carregar nome do container na variável 'nm-cntr'.
      *        18.01.2017 - F9188847 2016/145195 - IMPLANTACAO

      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSL081.

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
       77  CTE-PROG     PIC  X(17) VALUE '*** SGTSL081 ***'.
       77  CTE-VERS     PIC  X(06) VALUE 'VRS001'.

      *-----------------------------------------------------------------
      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informacoes
-INC HLPKE01B

       LOCAL-STORAGE SECTION.

      *-----------------------------------------------------------------
      * book de para acionamento do msg1
-INC IIBKERRO
-INC MSGKWMG1
-INC HLPKW999
-INC SGTKERR1

      * Area de trabalho

       01 QT-REG-SELD                         PIC S9(4) COMP-5 VALUE 0.

      *--> Auxiliares Movimentacao Monitoração
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).

      * Campos utilizados para leitura

       01  REG-WS.
           03 CD-FMA-VCLC                    PIC S9(4) COMP-5
              OCCURS 101 TIMES               VALUE 0.
           03 CD-NTZ-GRT                     PIC S9(4) COMP-5
              OCCURS 101 TIMES               VALUE 0.
           03 CD-USU-ULT-ATL                 PIC X(8)
              OCCURS 101 TIMES               VALUE ' '.
           03 DT-LIM-GRT                     PIC X(10)
              OCCURS 101 TIMES               VALUE ' '.
           03 NR-PRV-CMPS-GRT                PIC S9(9) COMP-5
              OCCURS 101 TIMES               VALUE 0.
           03 PC-GRT-CMPS-PRVT               PIC S9(4)V9(10) COMP-3
              OCCURS 101 TIMES               VALUE 0.
           03 PC-GRT-CMPS-PRVT-N             PIC S9(4) COMP-5
              OCCURS 101 TIMES               VALUE 0.
           03 TS-ULT-ATL                     PIC X(26)
              OCCURS 101 TIMES               VALUE ' '.
           03 VL-GRT-CMPS-PRVT               PIC S9(15)V9(2) COMP-3
              OCCURS 101 TIMES               VALUE 0.
           03 VL-GRT-CMPS-PRVT-N             PIC S9(4) COMP-5
              OCCURS 101 TIMES               VALUE 0.

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
-INC SGTKL081
           EXEC SQL
             DECLARE CURSOR-PSQ CURSOR
                     WITH ROWSET POSITIONING FOR
             SELECT A.CD_FMA_VCLC
                  , A.CD_NTZ_GRT
                  , A.CD_USU_ULT_ATL
                  , A.DT_LIM_GRT
                  , A.NR_PRV_CMPS_GRT
                  , A.PC_GRT_CMPS_PRVT
                  , A.TS_ULT_ATL
                  , A.VL_GRT_CMPS_PRVT
               FROM DB2SGT.PRV_CMPS_GRT       A
              WHERE A.CD_CTRA_TIT_VL     = :SGTSL081-CD-CTRA-TIT-VL
                AND A.CD_OPR_ETRD_MCP    = :SGTSL081-CD-OPR-ETRD-MCP
                AND A.NR_VRS_OPR_ETRD    = :SGTSL081-NR-VRS-OPR-ETRD
                AND A.NR_PRV_CMPS_GRT    > :SGTSL081-NR-ULT-PRV-CMPS-GRT
                ORDER BY NR_PRV_CMPS_GRT
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
                    VARYING SGTSL081-IC-TAB FROM 1 BY 1
                    UNTIL SGTSL081-IC-TAB > SGTSL081-QT-OCR-TAB

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
              CONTAINER ( 'SGTSL081-RPST' )
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

           MOVE ZEROS                        TO QT-CCT-LIDO
           MOVE LENGTH OF SGTSL081-RQSC      TO QT-CCT-EPRO
      *
           SET  GET-CONTAINER          TO TRUE
           SET  ERRO-CONTAINER         TO TRUE
           MOVE 'SGTSL081-RQSC'        TO NM-CTNR

           EXEC CICS GET
              CONTAINER ( NM-CTNR         )
              CHANNEL   ( NM-CNL          )
              NODATA
              FLENGTH   ( QT-CCT-LIDO     )
           END-EXEC

           IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
               SET  ERRO-TAMANHO-AREA  TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS GET
              CONTAINER ( NM-CTNR                 )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSL081-RQSC )
              INTO      ( SGTSL081-RQSC           )
           END-EXEC

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE

           .
       130000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       140000-VALIDAR-PARAMETRO SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                    TO MSG1-CD-SEQL-ERRO

           MOVE 'N'                     TO SGTSL081-IN-CTNC-PSQ

           IF  SGTSL081-CD-CTRA-TIT-VL  LESS ZEROS
               SET ERRO-PROGRAMADO TO TRUE
               MOVE 0001           TO MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de título e valores mobiliários
      -             'inválido.'    TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL081-CD-CTRA-TIT-VL  EQUAL ZEROS
               SET ERRO-PROGRAMADO TO TRUE
               MOVE 0002           TO MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de título e valores mobiliários
      -             'não informado.'
                                   TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

            IF  SGTSL081-CD-OPR-ETRD-MCP NOT NUMERIC
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 0003                TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -        ' numérico.'             TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL081-CD-OPR-ETRD-MCP EQUAL ZEROS
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 0005                TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -        ' informado.'            TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL081-NR-VRS-OPR-ETRD  EQUAL ZEROS
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 0007                TO MSG1-CD-ERRO-TS
               MOVE 'Número da versão da operação não informado.'
                                         TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

            IF  SGTSL081-NR-ULT-PRV-CMPS-GRT  LESS THAN ZEROS
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 0117                TO MSG1-CD-ERRO-TS
               MOVE 'Número da previsão de constituição de garantia invá
      -        'lido.' TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-PROCESSA-PESQUISA SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

           MOVE CD-FMA-VCLC(SGTSL081-IC-TAB) TO
                     SGTSL081-CD-FMA-VCLC(SGTSL081-IC-TAB)
           MOVE CD-NTZ-GRT(SGTSL081-IC-TAB) TO
                     SGTSL081-CD-NTZ-GRT(SGTSL081-IC-TAB)
           MOVE CD-USU-ULT-ATL(SGTSL081-IC-TAB) TO
                     SGTSL081-CD-USU-ULT-ATL(SGTSL081-IC-TAB)
           MOVE DT-LIM-GRT(SGTSL081-IC-TAB) TO
                     SGTSL081-DT-LIM-GRT(SGTSL081-IC-TAB)
           MOVE NR-PRV-CMPS-GRT(SGTSL081-IC-TAB) TO
                     SGTSL081-NR-PRV-CMPS-GRT(SGTSL081-IC-TAB)
           MOVE PC-GRT-CMPS-PRVT(SGTSL081-IC-TAB) TO
                     SGTSL081-PC-GRT-CMPS-PRVT(SGTSL081-IC-TAB)
           MOVE TS-ULT-ATL(SGTSL081-IC-TAB) TO
                     SGTSL081-TS-ULT-ATL(SGTSL081-IC-TAB)
           MOVE VL-GRT-CMPS-PRVT(SGTSL081-IC-TAB) TO
                     SGTSL081-VL-GRT-CMPS-PRVT(SGTSL081-IC-TAB)
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-POSTA-RESPOSTA SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO
           SET  PUT-CONTAINER          TO TRUE
           SET  ERRO-CONTAINER         TO TRUE
           MOVE 'SGTSL081-RPST'        TO NM-CTNR

           EXEC CICS PUT
              CONTAINER ( NM-CTNR                 )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSL081-RPST )
              FROM      ( SGTSL081-RPST           )
           END-EXEC

           SET  SEM-ERRO               TO TRUE
           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970101-ABRE-CURSOR-PSQ SECTION.
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
       970102-LER-CURSOR-PSQ SECTION.
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
           END-EXEC

           MOVE SQLERRD(3)             TO QT-REG-SELD
           IF  QT-REG-SELD EQUAL 0101
               SET SGTSL081-TEM-MAIS-REGISTRO TO TRUE
               MOVE 0100               TO QT-REG-SELD
           END-IF
           MOVE QT-REG-SELD            TO SGTSL081-QT-OCR-TAB
           MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ
           .
       970102-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970103-FECHA-CURSOR-PSQ SECTION.
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

           MOVE 'SGTSL081-CD-CTRA-TIT-VL'    TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL081-CD-CTRA-TIT-VL      TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL081-CD-OPR-ETRD-MCP'   TO MSGKW999-NM-VRV-MNT
           IF   SGTSL081-CD-OPR-ETRD-MCP     IS NUMERIC
               MOVE SGTSL081-CD-OPR-ETRD-MCP TO CD-OPR-ETRD-MCP
               MOVE CD-OPR-ETRD-MCP          TO MSGKW999-VL-VRV-MNT
           ELSE
               MOVE 'Valor não numérico.'
                                             TO MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL081-NR-VRS-OPR-ETRD'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL081-NR-VRS-OPR-ETRD     TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL081-NR-ULT-PRV-CMPS-GRT'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL081-NR-ULT-PRV-CMPS-GRT     TO MSGKW999-VL-VRV-MNT
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
       999999-ENCERRAR                SECTION.
      * --------------------------------------*
           EXEC CICS
                RETURN
           END-EXEC
           .
       999999-SAI.
           EXIT.