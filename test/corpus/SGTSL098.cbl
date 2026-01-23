      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSL098
      * ANALISTA..: F2054109 - Cintia Iglesias
      * AUTOR.....: F8785286 - Roseli Akemi Kawahata
      * OBJETIVO..: Listar carteiras associdadas à regra de validação da
      *       caracteristica da operação.
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 12.06.2018 F8785286 DE 2016/145195 - Implantação
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSL098.
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
                   '*** S.S. COMEÇA AQUI ***'.
       77  CTE-PROG     PIC  X(17) VALUE '*** SGTSL098 ***'.
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

      * Campos utilizados para leitura
       01  REG-WS.
           03 CD-RGR-VLDC                    PIC S9(9) COMP-5
              OCCURS 101 TIMES               VALUE 0.
           03 NR-VRS-RGR-VLDC                PIC S9(9) COMP-5
              OCCURS 101 TIMES               VALUE 0.
           03 CD-CTRA-TIT-VL                 PIC S9(9) COMP-5
              OCCURS 101 TIMES               VALUE 0.

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

      *-----------------------------------------------------------------
-INC SGTKL098

      * Cursor 01
      * Lista carteiras associdadas à regra de validação da caracteris-
      *       tica da operação.

           EXEC SQL
             DECLARE CURSOR-PSQ-01 CURSOR
                     WITH ROWSET POSITIONING FOR
             SELECT A.CD_RGR_VLDC
                  , A.NR_VRS_RGR_VLDC
                  , A.CD_CTRA_TIT_VL
              FROM  DB2SGT.CTRA_RGR_VLDC  A
              WHERE A.CD_RGR_VLDC     >= :SGTSL098-CD-RGR-VLDC-INI
                AND A.CD_RGR_VLDC     <= :SGTSL098-CD-RGR-VLDC-FIM
                AND A.NR_VRS_RGR_VLDC >= :SGTSL098-NR-VRS-RGR-VLDC-INI
                AND A.NR_VRS_RGR_VLDC <= :SGTSL098-NR-VRS-RGR-VLDC-FIM
                AND A.CD_CTRA_TIT_VL  >= :SGTSL098-CD-CTRA-TIT-VL-INI
                AND A.CD_CTRA_TIT_VL  <= :SGTSL098-CD-CTRA-TIT-VL-FIM
             ORDER BY A.CD_RGR_VLDC
                    , A.NR_VRS_RGR_VLDC
                    , A.CD_CTRA_TIT_VL
           END-EXEC.
      *

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

           PERFORM 200000-PROCESSA-PESQUISA

           PERFORM 300000-POSTA-RESPOSTA
           GOBACK
           .
      *-----------------------------------------------------------------
       100000-DESCOBRIR-CANAL SECTION.
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
       110000-EXCLUIR-CTNR-RPST SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS DELETE
              CONTAINER ( 'SGTSL098-RPST' )
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
           MOVE LENGTH OF SGTSL098-RQSC     TO QT-CCT-EPRO
           EXEC CICS GET
              CONTAINER ( 'SGTSL098-RQSC' )
              CHANNEL   ( NM-CNL          )
              NODATA
              FLENGTH   ( QT-CCT-LIDO     )
           END-EXEC

           IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
               SET  ERRO-TAMANHO-AREA         TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

           EXEC CICS GET
              CONTAINER ( 'SGTSL098-RQSC'         )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSL098-RQSC )
              INTO      ( SGTSL098-RQSC           )
           END-EXEC

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       130000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       140000-VALIDAR-PARAMETRO SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

           IF  NOT  SGTSL098-LISTA-POR-OPR
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0107              TO  MSG1-CD-ERRO-TS
               MOVE  'Código tipo serviço inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL098-CD-RGR-VLDC-FIM  EQUAL  ZEROS
               MOVE  99999999          TO  SGTSL098-CD-RGR-VLDC-FIM
           END-IF

           IF  SGTSL098-NR-VRS-RGR-VLDC-FIM   EQUAL  ZEROS
               MOVE  99999999          TO  SGTSL098-NR-VRS-RGR-VLDC-FIM
           END-IF

           IF  SGTSL098-CD-CTRA-TIT-VL-FIM   EQUAL  ZEROS
               MOVE  99999999          TO  SGTSL098-CD-CTRA-TIT-VL-FIM
           END-IF

           .
       140000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       200000-PROCESSA-PESQUISA SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

           MOVE 'N'                    TO SGTSL098-IN-CTNC-PSQ

           PERFORM 970101-ABRE-CURSOR-PSQ
           PERFORM 970102-LER-CURSOR-PSQ
           PERFORM 970103-FECHA-CURSOR-PSQ

           PERFORM  210000-MONTAR-TABELA-RESPOSTA
                     VARYING SGTSL098-IC-TAB FROM 1 BY 1
                    UNTIL SGTSL098-IC-TAB > SGTSL098-QT-OCR-TAB
           .
       200000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       210000-MONTAR-TABELA-RESPOSTA SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

           EVALUATE  TRUE
            WHEN  SGTSL098-LISTA-POR-OPR
                 MOVE  CD-RGR-VLDC (SGTSL098-IC-TAB)           TO
                           SGTSL098-CD-RGR-VLDC(SGTSL098-IC-TAB)
                 MOVE  NR-VRS-RGR-VLDC (SGTSL098-IC-TAB)       TO
                           SGTSL098-NR-VRS-RGR-VLDC(SGTSL098-IC-TAB)
                 MOVE  CD-CTRA-TIT-VL (SGTSL098-IC-TAB)       TO
                           SGTSL098-CD-CTRA-TIT-VL(SGTSL098-IC-TAB)
           END-EVALUATE
           .
       200000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       300000-POSTA-RESPOSTA SECTION.
      *-----------------------------------------------------------------
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS PUT
              CONTAINER ( 'SGTSL098-RPST'         )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSL098-RPST )
              FROM      ( SGTSL098-RPST           )
           END-EXEC
           .
       300000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970101-ABRE-CURSOR-PSQ SECTION.
      *-----------------------------------------------------------------
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO

           EVALUATE  TRUE
               WHEN  SGTSL098-LISTA-POR-OPR
                     EXEC SQL
                          OPEN CURSOR-PSQ-01
                     END-EXEC
           END-EVALUATE

           MOVE 'S'                    TO IN-CURSOR-PSQ-ABTO
           .
       970101-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970102-LER-CURSOR-PSQ SECTION.
      *-----------------------------------------------------------------
           MOVE 0011                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE REG-WS

           EVALUATE  TRUE

               WHEN  SGTSL098-LISTA-POR-OPR
                     EXEC SQL
                          FETCH NEXT ROWSET CURSOR-PSQ-01
                                FOR 101 ROWS
                           INTO
                                :CD-RGR-VLDC
                              , :NR-VRS-RGR-VLDC
                              , :CD-CTRA-TIT-VL
                     END-EXEC

           END-EVALUATE

           MOVE SQLERRD(3)             TO QT-REG-SELD

           IF  QT-REG-SELD EQUAL 0101
               SET SGTSL098-TEM-MAIS-REGISTRO TO TRUE
               MOVE 0100               TO QT-REG-SELD
           END-IF

           MOVE QT-REG-SELD            TO SGTSL098-QT-OCR-TAB
           MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ
           .
       970102-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970103-FECHA-CURSOR-PSQ SECTION.
      *-----------------------------------------------------------------
           MOVE 0012                   TO MSG1-CD-SEQL-ERRO

           MOVE 'N'                    TO IN-CURSOR-PSQ-ABTO

           EVALUATE  TRUE
               WHEN  SGTSL098-LISTA-POR-OPR
                     EXEC SQL
                          CLOSE  CURSOR-PSQ-01
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

           IF  MSG1-CD-ERRO-TS EQUAL ZEROS
               MOVE 8888                TO MSG1-CD-ERRO-TS
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

           MOVE 'SGTSL098-CD-TIP-SRVC'         TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL098-CD-TIP-SRVC           TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL098-CD-RGR-VLDC-INI'     TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL098-CD-RGR-VLDC-INI       TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL098-CD-RGR-VLDC-FIM'     TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL098-CD-RGR-VLDC-FIM       TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL098-NR-VRS-RGR-VLDC-INI' TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL098-NR-VRS-RGR-VLDC-INI   TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL098-NR-VRS-RGR-VLDC-FIM' TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL098-NR-VRS-RGR-VLDC-FIM   TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL098-CD-CTRA-TIT-VL-INI'  TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL098-CD-CTRA-TIT-VL-INI    TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL098-CD-CTRA-TIT-VL-FIM'  TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL098-CD-CTRA-TIT-VL-FIM    TO MSGKW999-VL-VRV-MNT
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