      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSL117
      * ANALISTA..: F8785286 - Roseli Akemi Kawahata
      * AUTOR.....: F0950413 - António Francisco Mendonça Silva
      * OBJETIVO..: Persist.- Lista das tabelas de Justificativas
      *             JST-VLDC-OPR-ETRD, TIP-JST-VLDC-OPR.
      * COMPILACAO: COBOL/CICS/DB2 (On-line)
      *-----------------------------------------------------------------
      * VRS001 10.10.2018 - F0950413 - 2016/145195 - Implantação
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSL117.

      *-----------------------------------------------------------------
       ENVIRONMENT                     DIVISION.
      *-----------------------------------------------------------------
       CONFIGURATION                   SECTION.
      *-----------------------------------------------------------------
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *-----------------------------------------------------------------
       DATA                            DIVISION.
      *-----------------------------------------------------------------
       WORKING-STORAGE                 SECTION.
      *-----------------------------------------------------------------
       77  CTE-INICIO-WS               PIC X(30) VALUE
                                       '*** WSS COMECA AQUI ***'.
       77  CTE-PROG                    PIC X(18) VALUE
                                       '*** SGTSL117 ***'.
       77  CTE-VERS                    PIC X(06) VALUE 'VRS001'.

      *-----------------------------------------------------------------
      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informacoes
-INC HLPKE01B

       01  CTE-FIM-WS                  PIC  X(30)  VALUE
                                       '*** WSS TERMINA AQUI ***'.
      *-----------------------------------------------------------------
       LOCAL-STORAGE SECTION.
      *-----------------------------------------------------------------
       01  CTE-INICIO-LS               PIC X(30)  VALUE
                                       '*** LS COMECA AQUI   ***'.
      *--> Areas de trabalho
       01  IC-TAB                      PIC S9(04)     COMP-5 VALUE 0.
       01  QT-REG-SELD                 PIC S9(04)     COMP-5 VALUE 0.

      *--> Book do container de erro
-INC IIBKERRO
      *--> Book para captura de erro via MSG1
-INC MSGKWMG1
      *--> Variaveis para tratamento de Abend e Condition
-INC HLPKW999
      *--> Variaveis para tratamento de CONTAINER
-INC SGTKERR1

      *--> Container de erro
       01  CTNR-PRM-ERRO               PIC X(16) VALUE 'PRM-ERRO'.

      *--> Máscaras edição de variáveis p/ monitoração mensagens de erro
       01  MCR-S9-15                   PIC      -999999999999999.


      *--> Variaveis utilizados para leitura
       01  REG-WS.
           03 CD-CTRA-TIT-VL           PIC S9(09)     COMP-5
              OCCURS 101 TIMES                               VALUE 0.
           03 CD-OPR-ETRD-MCP          PIC S9(11)     COMP-3
              OCCURS 101 TIMES                               VALUE 0.
           03 NR-VRS-OPR-ETRD          PIC S9(04)     COMP-5
              OCCURS 101 TIMES                               VALUE 0.
           03 CD-TIP-VLDC-OPR          PIC S9(04)     COMP-5
              OCCURS 101 TIMES                               VALUE 0.
           03 CD-USU-ULT-ATL           PIC  X(08)
              OCCURS 101 TIMES                               VALUE ' '.
           03 TX-JST-VLDC-OPR          PIC  X(200)
              OCCURS 101 TIMES                               VALUE ' '.
           03 TS-ULT-ATL               PIC  X(26)
              OCCURS 101 TIMES                               VALUE ' '.
           03 TX-TIP-VLDC-OPR          PIC  X(50)
              OCCURS 101 TIMES                               VALUE ' '.

      *--> Variavel(is) de controle do cursor(es)
       01  CTL-CURSOR-PSQ-1.
           03  CD-FIM-CURSOR-PSQ-1     PIC S9(04)     COMP-5 VALUE 0.
               88  FIM-CURSOR-PSQ-1                          VALUE +100.
           03  IN-CURSOR-PSQ-ABTO-1    PIC  X(01)            VALUE 'N'.
               88  CURSOR-PSQ-ABTO-1   VALUE 'S'.

      *--> Book do programa
       01  SGTSL117-DADOS.
-INC SGTKL117

      *--> Area de trabalho SQL/ DB2.
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

      *--> Declaração do cursor para pesquisa
           EXEC SQL
             DECLARE CURSOR-PSQ-1 CURSOR
                      WITH ROWSET POSITIONING FOR
             SELECT A.CD_CTRA_TIT_VL
                  , A.CD_OPR_ETRD_MCP
                  , A.NR_VRS_OPR_ETRD
                  , A.CD_TIP_VLDC_OPR
                  , A.CD_USU_ULT_ATL
                  , A.TX_JST_VLDC_OPR
                  , A.TS_ULT_ATL
                  , B.TX_TIP_VLDC_OPR
             FROM  DB2SGT.JST_VLDC_OPR_ETRD      A
             INNER JOIN DB2SGT.TIP_JST_VLDC_OPR  B
                ON   B.CD_TIP_VLDC_OPR  =  A.CD_TIP_VLDC_OPR
             WHERE A.CD_CTRA_TIT_VL  = :SGTSL117-CD-CTRA-TIT-VL
               AND A.CD_OPR_ETRD_MCP = :SGTSL117-CD-OPR-ETRD-MCP
               AND A.NR_VRS_OPR_ETRD = :SGTSL117-NR-VRS-OPR-ETRD
               AND A.CD_TIP_VLDC_OPR > :SGTSL117-ULT-CD-TIP-VLDC-OPR
             ORDER BY A.CD_TIP_VLDC_OPR
           END-EXEC.

       01  CTE-FIM-LS                  PIC X(30)  VALUE
                                       '*** LS TERMINA AQUI  ***'.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------
           MOVE 0001                   TO MSG1-CD-SEQL-ERRO

      *--> Habilita tratamento automatico de Erros DB2
-INC HLPKE997
      *--> Habilita captura automatica de abend e erros CICS
-INC HLPKE999
           MOVE CTE-PROG(5:8)          TO NM-PGM

           PERFORM 100000-OBTER-NOME-DO-CANAL
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO
           PERFORM 970000-LER-REQUISICAO-RECEBIDA

           PERFORM 140000-VALIDAR-PARAMETROS

           PERFORM 970101-ABRE-CURSOR-PSQ-1
           PERFORM 970102-LER-CURSOR-PSQ-1

           PERFORM 200000-PROCESSA-PESQUISA
                   VARYING IC-TAB FROM 1 BY 1
                     UNTIL IC-TAB > SGTSL117-QT-OCR-TAB

           PERFORM 970103-FECHA-CURSOR-PSQ-1

           PERFORM 980000-GRAVAR-CTNR-RESPOSTA
           PERFORM 999999-ENCERRAR

           GOBACK
           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-OBTER-NOME-DO-CANAL      SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO

      *--> Por causa da captura automática de erro CICS, deve ser
      *--> previsto o erro de container
           SET  ERRO-CANAL             TO TRUE

           EXEC CICS ASSIGN
              CHANNEL ( NM-CNL  )

              END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       140000-VALIDAR-PARAMETROS       SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                   TO MSG1-CD-SEQL-ERRO

           MOVE 'N'                    TO SGTSL117-IN-CTNC-PSQ
           IF  NOT SGTSL117-LS-JST-VLDC-OPR-ETRD
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0107               TO MSG1-CD-ERRO-TS
               MOVE 'Código do tipo de serviço inválido.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL117-CD-CTRA-TIT-VL  EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0001               TO MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de títulos e valores mobiliários
      -             ' inválido.'       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL117-CD-OPR-ETRD-MCP NOT NUMERIC
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0003               TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -             ' numérico.'       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           ELSE
            IF SGTSL117-CD-OPR-ETRD-MCP = 0
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0005               TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -             ' informado.'      TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
            END-IF
           END-IF

           IF  SGTSL117-NR-VRS-OPR-ETRD EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0006               TO MSG1-CD-ERRO-TS
               MOVE 'Número da versão da operação estruturada inválido.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-PROCESSA-PESQUISA        SECTION.
      *-----------------------------------------------------------------
           MOVE 0004                   TO MSG1-CD-SEQL-ERRO

           MOVE CD-TIP-VLDC-OPR  (IC-TAB)
                                  TO SGTSL117-CD-TIP-VLDC-OPR  (IC-TAB)
           MOVE TX-JST-VLDC-OPR  (IC-TAB)
                                  TO SGTSL117-TX-JST-VLDC-OPR  (IC-TAB)
           MOVE TX-TIP-VLDC-OPR  (IC-TAB)
                                  TO SGTSL117-TX-TIP-VLDC-OPR  (IC-TAB)
           MOVE CD-USU-ULT-ATL   (IC-TAB)
                                  TO SGTSL117-CD-USU-ULT-ATL   (IC-TAB)
           MOVE TS-ULT-ATL       (IC-TAB)
                                  TO SGTSL117-TS-ULT-ATL       (IC-TAB)
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970000-LER-REQUISICAO-RECEBIDA  SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

           MOVE ZEROS                       TO QT-CCT-LIDO
           MOVE LENGTH OF SGTSL117-RQSC     TO QT-CCT-EPRO

           EXEC CICS
                GET  CONTAINER ( 'SGTSL117-RQSC' )
                     CHANNEL   ( NM-CNL          )
                     NODATA
                     FLENGTH   ( QT-CCT-LIDO     )
           END-EXEC

           IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
               SET  ERRO-TAMANHO-AREA  TO  TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS
                GET  CONTAINER ( 'SGTSL117-RQSC'         )
                     CHANNEL   ( NM-CNL                  )
                     FLENGTH   ( LENGTH OF SGTSL117-RQSC )
                     INTO      ( SGTSL117-RQSC           )
           END-EXEC
           .
       970000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970101-ABRE-CURSOR-PSQ-1          SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

           EXEC SQL
                OPEN CURSOR-PSQ-1
           END-EXEC
           MOVE 'S'                    TO IN-CURSOR-PSQ-ABTO-1
           .
       970101-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970102-LER-CURSOR-PSQ-1         SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE REG-WS

           EXEC SQL
                FETCH NEXT ROWSET
                      CURSOR-PSQ-1 FOR 0101 ROWS
                INTO :CD-CTRA-TIT-VL
                   , :CD-OPR-ETRD-MCP
                   , :NR-VRS-OPR-ETRD
                   , :CD-TIP-VLDC-OPR
                   , :CD-USU-ULT-ATL
                   , :TX-JST-VLDC-OPR
                   , :TS-ULT-ATL
                   , :TX-TIP-VLDC-OPR
           END-EXEC

           MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ-1
           MOVE SQLERRD(3)             TO QT-REG-SELD

           IF  QT-REG-SELD EQUAL 0101
               SET SGTSL117-TEM-MAIS-REGISTRO TO TRUE
               MOVE 0100               TO QT-REG-SELD
           END-IF

           MOVE QT-REG-SELD            TO SGTSL117-QT-OCR-TAB
           MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ-1
           .
       970102-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970103-FECHA-CURSOR-PSQ-1       SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

           MOVE 'N'                    TO IN-CURSOR-PSQ-ABTO-1
           EXEC SQL
                CLOSE CURSOR-PSQ-1
           END-EXEC
           .
       970103-SAI.
           EXIT.

      *-----------------------------------------------------------------
       980000-GRAVAR-CTNR-RESPOSTA     SECTION.
      *-----------------------------------------------------------------
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO

      *--> Exclui eventual container de erro de outros programas
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO

      *--> Monta o nome do container de resposta
           MOVE 'SGTSL117-RPST'        TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *--> previsto o erro de container
           SET  ERRO-CONTAINER         TO TRUE

           EXEC CICS
                PUT  CONTAINER ( NM-CTNR                 )
                     CHANNEL   ( NM-CNL                  )
                     FLENGTH   ( LENGTH OF SGTSL117-RPST )
                     FROM      ( SGTSL117-RPST           )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       980000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       983000-EXCLUIR-RESPOSTA-ERRO    SECTION.
      *----------------------------------------------------------------*
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO
      *----------------------------------------------------------------*
      * Exclui o container de resposta e de erro para evitar problemas *
      * em modulos executados mais de uma vez dentro da mesma sessao do*
      * CICS. Equivale ao initialize na area de resposta dos programas *
      * que utilizam COMMAREA.                                         *
      *----------------------------------------------------------------*
      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *--> nome_modulo -RPST

           MOVE NM-PGM                 TO NM-CTNR(1:8)
           MOVE '-RPST'                TO NM-CTNR(9:5)

           SET DEL-CONTAINER           TO TRUE

      *--> Exclui o container de resposta
      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS
                DELETE CONTAINER (NM-CTNR   )
                       RESP      (MSG1-RESP1)
                       RESP2     (MSG1-RESP2)
           END-EXEC

           EVALUATE TRUE
               WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
               WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    SET ERRO-CONTAINER TO TRUE
                    PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE

      *--> Excluir o container de erro
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO
           .
       983000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       983100-EXCLUIR-CONTAINER-ERRO   SECTION.
      *----------------------------------------------------------------*
           MOVE 0011                   TO MSG1-CD-SEQL-ERRO

      *--> Limpar variaveis container de erro
           INITIALIZE IIBKERRO-PRM-ERRO

      *--> Exclui o container de erro
           MOVE CTNR-PRM-ERRO          TO NM-CTNR
           SET DEL-CONTAINER           TO TRUE

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS
                DELETE CONTAINER (NM-CTNR   )
                       RESP      (MSG1-RESP1)
                       RESP2     (MSG1-RESP2)
           END-EXEC

           EVALUATE TRUE
               WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
               WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    SET ERRO-CONTAINER TO TRUE
                    PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       983100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------
      *--> Monitora variàveis do pgm, grava e encerra processamento

           IF  MSG1-CD-ERRO-TS             EQUAL  ZEROS
              MOVE  8888               TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS             EQUAL  SPACES
               MOVE  'Erro não especificado no módulo.'
                                       TO  MSG1-TX-ERRO-TS
           END-IF

      *--> Verifica se o tipo de erro gerado possui tratamento interno
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

      *--> Adiciona as variáveis para monitoração
           MOVE 'SGTSL117-CD-TIP-SRVC'       TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL117-CD-TIP-SRVC         TO MCR-S9-15
           MOVE MCR-S9-15                    TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL117-CD-CTRA-TIT-VL'    TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL117-CD-CTRA-TIT-VL      TO MCR-S9-15
           MOVE MCR-S9-15                    TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE  'SGTSL117-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
      *--> Verifica se o conteudo e numerico antes de adicionar
           IF  SGTSL117-CD-OPR-ETRD-MCP    IS NUMERIC
               MOVE  SGTSL117-CD-OPR-ETRD-MCP
                                       TO  MCR-S9-15
               MOVE  MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL117-NR-VRS-OPR-ETRD'  TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL117-NR-VRS-OPR-ETRD    TO MCR-S9-15
           MOVE MCR-S9-15                   TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL117-ULT-CD-TIP-VLDC-OPR'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL117-ULT-CD-TIP-VLDC-OPR     TO MCR-S9-15
           MOVE MCR-S9-15                   TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

      *--> grava o erro no msg
           PERFORM MSG1-NTF-MSG
           MOVE MSG1-CD-ERRO-TS        TO IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS        TO IIBKERRO-TX-ERRO

           EXEC CICS PUT
              CONTAINER ( CTNR-PRM-ERRO               )
              CHANNEL   ( NM-CNL                      )
              FROM      ( IIBKERRO-PRM-ERRO           )
              FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
              NOHANDLE
           END-EXEC

           PERFORM 999999-ENCERRAR
           .
       999000-SAI.
           EXIT.

      *---------------------------------------*
      * 999001-TRATA-ABEND-CICS        SECTION.
      * 999002-TRATA-CONDITION-CICS    SECTION.
      * 999003-ADICIONA-VRV-MNT        SECTION.
      *---------------------------------------*
-INC HLPKP999

      *---------------------------------------*
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *---------------------------------------*
-INC SGTKP999

      *---------------------------------------*
      * 999010-TRATA-ERRO-DB2          SECTION.
      *---------------------------------------*
-INC HLPKP997

      *---------------------------------------*
      * MSG1-NTF-MSG                   SECTION.
      *---------------------------------------*
-INC MSGKPMG1

      *---------------------------------------*
       999999-ENCERRAR                 SECTION.
      *---------------------------------------*

           IF  IN-CURSOR-PSQ-ABTO-1  EQUAL  'S'
               PERFORM 970103-FECHA-CURSOR-PSQ-1
           END-IF

           EXEC CICS
                RETURN
           END-EXEC
           .
       999999-SAI.
           EXIT.
      *---------------------- FIM SGTSL117 ----------------------------