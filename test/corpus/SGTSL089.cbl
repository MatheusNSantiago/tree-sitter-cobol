       CBL ARITH(EXTEND)
      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSL089
      * ANALISTA..: F2054109 Cintia Ap Soares Iglesias
      * AUTOR.....: F0950413 António Francisco Mendonça Silva
      * DATA......: 04/09/2017
      * OBJETIVO..: Listar todas as instruçoes de uma regra e versão ,
      *             tabela DB2SGT.ITC_RGR_VLDC_CRCT
      * COMPILACAO: COBOL/CICS/DB2 (On-line)
      *-----------------------------------------------------------------
      * VRS001 11.04.2018 - F0950413 - 2015/137233 - Alteração de tam
      *                     do campo TX-ITC-RGR-VLDC de X(01) p/ X(20).
      *        04.09.2017 - F0950413 - 2015/137233 - Implantação
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSL089.

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
       77  CTE-INICIO-SS               PIC X(40) VALUE
           '*** STORAGE SECTION COMECA AQUI ***'.
       77  CTE-PROG                    PIC X(18) VALUE
           '*** SGTSL089 ***'.
       77  CTE-VERS                    PIC X(06) VALUE 'VRS001'.

      *-----------------------------------------------------------------
      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informacoes
-INC HLPKE01B
      *-----------------------------------------------------------------
       LOCAL-STORAGE SECTION.
      *-----------------------------------------------------------------
       01  IC-TAB                      PIC S9(04)     COMP-5 VALUE 0.

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
       01  MCR-S9-04                   PIC                 -9999.
       01  MCR-S9-09                   PIC            -999999999.

      *--> Area de trabalho
       01  QT-REG-SELD                 PIC S9(04)     COMP-5 VALUE 0.

      *--> Indicadores de nulidades
       01  QT-REG-SELD-N               PIC S9(04)     COMP-5 VALUE 0.

      *--> Variaveis utilizados para leitura
       01  REG-WS.
           03 NR-SEQL-ITC              PIC S9(04)     COMP-5
              OCCURS 101 TIMES                               VALUE 0.
           03 CD-TIP-CMDO-VLDC         PIC S9(04)     COMP-5
              OCCURS 101 TIMES                               VALUE 0.
           03 NR-GR-CND-ITC            PIC S9(04)     COMP-5
              OCCURS 101 TIMES                               VALUE 0.
           03 CD-ATB-VLDC              PIC S9(04)     COMP-5
              OCCURS 101 TIMES                               VALUE 0.
           03 CD-ATB-VLDC-N            PIC S9(04)     COMP-5
              OCCURS 101 TIMES                               VALUE 0.
           03 CD-TIP-CFT-LGC           PIC S9(04)     COMP-5
              OCCURS 101 TIMES                               VALUE 0.
           03 CD-TIP-CFT-LGC-N         PIC S9(04)     COMP-5
              OCCURS 101 TIMES                               VALUE 0.
           03 CD-TIP-CFT-ITEM          PIC S9(04)     COMP-5
              OCCURS 101 TIMES                               VALUE 0.
           03 CD-TIP-CFT-ITEM-N        PIC S9(04)     COMP-5
              OCCURS 101 TIMES                               VALUE 0.
           03 CD-RGR-VLDC-EXAR         PIC S9(09)     COMP-5
              OCCURS 101 TIMES                               VALUE 0.
           03 CD-RGR-VLDC-EXAR-N       PIC S9(04)     COMP-5
              OCCURS 101 TIMES                               VALUE 0.
           03 NR-VRS-RGR-EXAR          PIC S9(09)     COMP-5
              OCCURS 101 TIMES                               VALUE 0.
           03 NR-VRS-RGR-EXAR-N        PIC S9(04)     COMP-5
              OCCURS 101 TIMES                               VALUE 0.
           03 VL-ITC-RGR-VLDC          PIC S9(15)V9(4) COMP-3
              OCCURS 101 TIMES                               VALUE 0.
           03 VL-ITC-RGR-VLDC-N        PIC S9(04)     COMP-5
              OCCURS 101 TIMES                               VALUE 0.
           03 NR-ITC-RGR-VLDC          PIC S9(09)     COMP-5
              OCCURS 101 TIMES                               VALUE 0.
           03 NR-ITC-RGR-VLDC-N        PIC S9(04)     COMP-5
              OCCURS 101 TIMES                               VALUE 0.
           03 TX-ITC-RGR-VLDC          PIC  X(20)
              OCCURS 101 TIMES                               VALUE ' '.
           03 TX-ITC-RGR-VLDC-N        PIC S9(04)     COMP-5
              OCCURS 101 TIMES                               VALUE 0.
           03 TX-DCR-ITC               PIC  X(400)
              OCCURS 101 TIMES                               VALUE ' '.
           03 TX-DCR-ITC-N             PIC S9(04)     COMP-5
              OCCURS 101 TIMES                               VALUE 0.
           03 CD-TIP-ATB-VLDC          PIC S9(04)     COMP-5
              OCCURS 101 TIMES                               VALUE 0.
           03 CD-TIP-ATB-VLDC-N        PIC S9(04)     COMP-5
              OCCURS 101 TIMES                               VALUE 0.

      *--> Variavel(is) de controle do cursor(es)
       01  CTL-CURSOR-PSQ.
           03  CD-FIM-CURSOR-PSQ       PIC S9(04)     COMP-5 VALUE 0.
               88  FIM-CURSOR-PSQ                            VALUE +100.
           03  IN-CURSOR-PSQ-ABTO      PIC  X(01)            VALUE 'N'.
               88  CURSOR-PSQ-ABTO     VALUE 'S'.

      *--> Book do programa
       01  SGTSL089-DADOS.
-INC SGTKL089

      *--> Area de trabalho SQL/ DB2.
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

      *--> Declaração do cursor para pesquisa
           EXEC SQL
             DECLARE CURSOR-PSQ CURSOR
                      WITH ROWSET POSITIONING FOR
             SELECT A.NR_SEQL_ITC
                  , A.CD_TIP_CMDO_VLDC
                  , A.NR_GR_CND_ITC
                  , A.CD_ATB_VLDC
                  , A.CD_TIP_CFT_LGC
                  , A.CD_TIP_CFT_ITEM
                  , A.CD_RGR_VLDC_EXAR
                  , A.NR_VRS_RGR_EXAR
                  , A.VL_ITC_RGR_VLDC
                  , A.NR_ITC_RGR_VLDC
                  , A.TX_ITC_RGR_VLDC
                  , A.TX_DCR_ITC
                  , B.CD_TIP_ATB_VLDC
             FROM   DB2SGT.ITC_RGR_VLDC_CRCT  A
             LEFT OUTER JOIN DB2SGT.ATB_VLDC B
             ON A.CD_ATB_VLDC = B.CD_ATB_VLDC
             WHERE  A.CD_RGR_VLDC      = :SGTSL089-CD-RGR-VLDC
               AND  A.NR_VRS_RGR_VLDC  = :SGTSL089-NR-VRS-RGR-VLDC
               AND  A.NR_SEQL_ITC      > :SGTSL089-NR-SEQL-ITC-ULT
               ORDER BY  A.CD_RGR_VLDC, A.NR_VRS_RGR_VLDC, A.NR_SEQL_ITC
           END-EXEC.

       01  CTE-FINAL-SS                PIC X(40) VALUE
           '*** STORAGE SECTION TERMINA AQUI ***'.
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

           PERFORM 970101-ABRE-CURSOR-PSQ
           PERFORM 970102-LER-CURSOR-PSQ

           PERFORM 200000-PROCESSA-PESQUISA
                   VARYING IC-TAB FROM 1 BY 1
                     UNTIL IC-TAB > SGTSL089-QT-OCR-TAB

           PERFORM 970103-FECHA-CURSOR-PSQ

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
      *    previsto o erro de container
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

           MOVE 'N'                    TO SGTSL089-IN-CTNC-PSQ
           IF  SGTSL089-CD-RGR-VLDC EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0076               TO MSG1-CD-ERRO-TS
               MOVE 'Código da regra de validação inválido.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL089-NR-VRS-RGR-VLDC EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0083               TO MSG1-CD-ERRO-TS
               MOVE 'N mero da versão da regra de validação inválido.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL089-NR-SEQL-ITC-ULT < 0
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0413               TO MSG1-CD-ERRO-TS
               MOVE 'N mero sequencial instrução inválido.'
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

           MOVE NR-SEQL-ITC      (IC-TAB)
                                  TO SGTSL089-NR-SEQL-ITC      (IC-TAB)
           MOVE CD-TIP-CMDO-VLDC (IC-TAB)
                                  TO SGTSL089-CD-TIP-CMDO-VLDC (IC-TAB)
           MOVE NR-GR-CND-ITC    (IC-TAB)
                                  TO SGTSL089-NR-GR-CND-ITC    (IC-TAB)
           MOVE CD-ATB-VLDC      (IC-TAB)
                                  TO SGTSL089-CD-ATB-VLDC      (IC-TAB)
           MOVE CD-TIP-CFT-LGC   (IC-TAB)
                                  TO SGTSL089-CD-TIP-CFT-LGC   (IC-TAB)
           MOVE CD-TIP-CFT-ITEM  (IC-TAB)
                                  TO SGTSL089-CD-TIP-CFT-ITEM  (IC-TAB)
           MOVE CD-RGR-VLDC-EXAR (IC-TAB)
                                  TO SGTSL089-CD-RGR-VLDC-EXAR (IC-TAB)
           MOVE NR-VRS-RGR-EXAR  (IC-TAB)
                                  TO SGTSL089-NR-VRS-RGR-EXAR  (IC-TAB)
           MOVE VL-ITC-RGR-VLDC  (IC-TAB)
                                  TO SGTSL089-VL-ITC-RGR-VLDC  (IC-TAB)
           MOVE NR-ITC-RGR-VLDC  (IC-TAB)
                                  TO SGTSL089-NR-ITC-RGR-VLDC  (IC-TAB)
           MOVE TX-ITC-RGR-VLDC  (IC-TAB)
                                  TO SGTSL089-TX-ITC-RGR-VLDC  (IC-TAB)
           MOVE TX-DCR-ITC       (IC-TAB)
                                  TO SGTSL089-TX-DCR-ITC       (IC-TAB)
           MOVE CD-TIP-ATB-VLDC  (IC-TAB)
                                  TO SGTSL089-CD-TIP-ATB-VLDC  (IC-TAB)
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970000-LER-REQUISICAO-RECEBIDA  SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

           MOVE ZEROS                       TO QT-CCT-LIDO
           MOVE LENGTH OF SGTSL089-RQSC     TO QT-CCT-EPRO

           EXEC CICS
                GET  CONTAINER ( 'SGTSL089-RQSC' )
                     CHANNEL   ( NM-CNL          )
                     NODATA
                     FLENGTH   ( QT-CCT-LIDO     )
           END-EXEC

           IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
               SET  ERRO-TAMANHO-AREA  TO  TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS
                GET  CONTAINER ( 'SGTSL089-RQSC'         )
                     CHANNEL   ( NM-CNL                  )
                     FLENGTH   ( LENGTH OF SGTSL089-RQSC )
                     INTO      ( SGTSL089-RQSC           )
           END-EXEC
           .
       970000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970101-ABRE-CURSOR-PSQ          SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

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
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE REG-WS

           EXEC SQL
                FETCH NEXT ROWSET
                      CURSOR-PSQ FOR 0101 ROWS
                INTO :NR-SEQL-ITC
                   , :CD-TIP-CMDO-VLDC
                   , :NR-GR-CND-ITC
                   , :CD-ATB-VLDC
                     :CD-ATB-VLDC-N
                   , :CD-TIP-CFT-LGC
                     :CD-TIP-CFT-LGC-N
                   , :CD-TIP-CFT-ITEM
                     :CD-TIP-CFT-ITEM-N
                   , :CD-RGR-VLDC-EXAR
                     :CD-RGR-VLDC-EXAR-N
                   , :NR-VRS-RGR-EXAR
                     :NR-VRS-RGR-EXAR-N
                   , :VL-ITC-RGR-VLDC
                     :VL-ITC-RGR-VLDC-N
                   , :NR-ITC-RGR-VLDC
                     :NR-ITC-RGR-VLDC-N
                   , :TX-ITC-RGR-VLDC
                     :TX-ITC-RGR-VLDC-N
                   , :TX-DCR-ITC
                     :TX-DCR-ITC-N
                   , :CD-TIP-ATB-VLDC
                     :CD-TIP-ATB-VLDC-N
           END-EXEC

           MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ
           MOVE SQLERRD(3)             TO QT-REG-SELD

           IF  QT-REG-SELD EQUAL 0101
               SET SGTSL089-TEM-MAIS-REGISTRO TO TRUE
               MOVE 0100               TO QT-REG-SELD
           END-IF

           MOVE QT-REG-SELD            TO SGTSL089-QT-OCR-TAB
           MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ
           .
       970102-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970103-FECHA-CURSOR-PSQ         SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

           MOVE 'N'                    TO IN-CURSOR-PSQ-ABTO
           EXEC SQL
                CLOSE CURSOR-PSQ
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
           MOVE 'SGTSL089-RPST'        TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *--> previsto o erro de container
           SET  ERRO-CONTAINER         TO TRUE

           EXEC CICS
                PUT  CONTAINER ( NM-CTNR                 )
                     CHANNEL   ( NM-CNL                  )
                     FLENGTH   ( LENGTH OF SGTSL089-RPST )
                     FROM      ( SGTSL089-RPST           )
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
           MOVE 'SGTSL089-CD-RGR-VLDC'       TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL089-CD-RGR-VLDC         TO MCR-S9-09
           MOVE MCR-S9-09                    TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL089-NR-VRS-RGR-VLDC'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL089-NR-VRS-RGR-VLDC     TO MCR-S9-09
           MOVE MCR-S9-09                    TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL089-NR-SEQL-ITC-ULT'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL089-NR-SEQL-ITC-ULT     TO MCR-S9-04
           MOVE MCR-S9-04                    TO MSGKW999-VL-VRV-MNT
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
           EXEC CICS
                RETURN
           END-EXEC
           .
       999999-SAI.
           EXIT.