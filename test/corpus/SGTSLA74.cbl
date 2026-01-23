000410*-----------------------------------------------------------------
000410* PROGRAMA..: SGTSLA74
000410* ANALISTA..: F8785286 Roseli Akemi Kawahata
000410* AUTOR.....: F8785286 Roseli Akemi Kawahata
000410* OBJETIVO..: Consulta tabela DB2SGT.NOTA_CLSC_OPR por Período
000410* COMPILACAO: COBOL/CICS/DB2
000410*-----------------------------------------------------------------
000090* VRS001 17/05/2017 F8785286 DE 2016/145195 - IMPLANTACAO
000410*-----------------------------------------------------------------
000410 IDENTIFICATION DIVISION.
000410*-----------------------------------------------------------------
000410 PROGRAM-ID. SGTSLA74.
000410*-----------------------------------------------------------------
000410 ENVIRONMENT DIVISION.
000410*-----------------------------------------------------------------
000410 CONFIGURATION SECTION.
000410 SPECIAL-NAMES.
000410     DECIMAL-POINT IS COMMA.
000410*-----------------------------------------------------------------
000410 DATA DIVISION.
000410*-----------------------------------------------------------------
000410 WORKING-STORAGE SECTION.
000410*-----------------------------------------------------------------
       77  CTE-INICIO-SS               PIC  X(40)  VALUE
                   '*** S.S. COMEÇA AQUI ***'.
000410 77  CTE-PROG     PIC  X(17) VALUE '*** SGTSLA74 ***'.
000410 77  CTE-VERS     PIC  X(06) VALUE 'VRS001'.
000410*-----------------------------------------------------------------
000410* Book do programa de ambiente - CICS5000
000410* Definido na work para preservar as informacoes
-INC HLPKE01B
000410 LOCAL-STORAGE SECTION.
000410*-----------------------------------------------------------------
000410* book de para acionamento do msg1
-INC IIBKERRO
-INC MSGKWMG1
-INC HLPKW999

000410* Area de trabalho
000090
000410 01 QT-REG-SELD                  PIC S9(4) COMP-5 VALUE 0.

      *--> Auxiliares para movimentação na monitoração de variáveis.
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).
000090
000410* Campos utilizados para leitura
000090
000410 01  REG-WS.
000410     03 NR-SEQL-NOTA-OPR         PIC S9(9) COMP-5
000410        OCCURS 101 TIMES         VALUE 0.
000410     03 CD-AG-CLSC-RSCO          PIC S9(4) COMP-5
000410        OCCURS 101 TIMES         VALUE 0.
000410     03 NR-ETRD-AG-CLSC          PIC S9(4) COMP-5
000410        OCCURS 101 TIMES         VALUE 0.
000410     03 NR-SEQL-NOTA-AG          PIC S9(4) COMP-5
000410        OCCURS 101 TIMES         VALUE 0.
000410     03 CD-ARQ-OPR-ETRD          PIC S9(9) COMP-5
000410        OCCURS 101 TIMES         VALUE 0.
000410     03 CD-ARQ-OPR-ETRD-N        PIC S9(4) COMP-5
000410        OCCURS 101 TIMES         VALUE 0.
000410     03 DT-INC-ATBC-NOTA         PIC  X(8)
000410        OCCURS 101 TIMES         VALUE SPACES.
000410     03 DT-FIM-ATBC-NOTA         PIC  X(8)
000410        OCCURS 101 TIMES         VALUE SPACES.
000410     03 DT-FIM-ATBC-NOTA-N       PIC S9(4) COMP-5
000410        OCCURS 101 TIMES         VALUE 0.
000410
000410* Indicadores de nulidades
000090
000410 01 QT-REG-SELD-N                PIC S9(4) COMP-5 VALUE 0.
000410 01 WS-CD-AG-CLSC-INC            PIC S9(4) COMP-5 VALUE 0.
000410 01 WS-CD-AG-CLSC-FIM            PIC S9(4) COMP-5 VALUE 9999.
000410 01 WS-NR-ETRD-AG-INC            PIC S9(4) COMP-5 VALUE 0.
000410 01 WS-NR-ETRD-AG-FIM            PIC S9(4) COMP-5 VALUE 9999.
000410 01 WS-DT-INC-ATBC               PIC  X(8) VALUE '01010001'.
000410 01 WS-DT-FIM-ATBC               PIC  X(8) VALUE '31129999'.
000410 01 WS-NR-SEQL-NOTA-INC          PIC S9(9) COMP-5 VALUE 0.
000410 01 WS-NR-SEQL-NOTA-FIM          PIC S9(9) COMP-5 VALUE 999999999.
000090
000410* Variavel(is) de controle do cursor(es)
000090
000410 01  CTL-CURSOR-PSQ.
000410     03  CD-FIM-CURSOR-PSQ            PIC S9(04) COMP-5 VALUE 0.
000410         88  FIM-CURSOR-PSQ            VALUE +100.
000410     03  IN-CURSOR-PSQ-ABTO           PIC  X(01) VALUE 'N'.
000410         88  CURSOR-PSQ-ABTO          VALUE 'S'.
000090
000410* Subrotinas utilizadas
000410
000410     EXEC SQL
000410         INCLUDE SQLCA
000410     END-EXEC.
000090* Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1
000410*-----------------------------------------------------------------
-INC SGTKLA74
000410*-----------------------------------------------------------------
000410*--> Pesq Nota de classificacao Cadastrada no período indicado
000410*-----------------------------------------------------------------
000410     EXEC SQL
000410       DECLARE CURSOR-PSQ-1 CURSOR
000410               WITH ROWSET POSITIONING FOR
000410       SELECT A.NR_SEQL_NOTA_OPR
000410            , A.CD_AG_CLSC_RSCO
000410            , A.NR_ETRD_AG_CLSC
000410            , A.NR_SEQL_NOTA_AG
000410            , A.CD_ARQ_OPR_ETRD
000410            , CHAR(TO_CHAR(A.DT_INC_ATBC_NOTA,'DDMMYYYY') , 8 )
000410            , VALUE(CHAR(TO_CHAR
000410             (A.DT_FIM_ATBC_NOTA,'DDMMYYYY') , 8 ),'')
000090*
000410         FROM DB2SGT.NOTA_CLSC_OPR  A
000090
000410         WHERE A.CD_CTRA_TIT_VL    = :SGTSLA74-E-CD-CTRA-TIT-VL
000410         AND   A.CD_OPR_ETRD_MCP   = :SGTSLA74-E-CD-OPR-ETRD-MCP
000410         AND   A.CD_AG_CLSC_RSCO  >= :WS-CD-AG-CLSC-INC
000410         AND   A.CD_AG_CLSC_RSCO  <= :WS-CD-AG-CLSC-FIM
000040
000041         AND   A.DT_INC_ATBC_NOTA <=
000410             TO_CHAR(TO_DATE(:WS-DT-INC-ATBC,'DDMMYYYY'),
000410                             'DD.MM.YYYY')
000410
000410         AND   (A.DT_FIM_ATBC_NOTA >=
000410             TO_CHAR(TO_DATE(:WS-DT-FIM-ATBC,'DDMMYYYY'),
000410                             'DD.MM.YYYY')
000410             OR A.DT_FIM_ATBC_NOTA IS NULL)
000410
000410         AND   A.IN_NOTA_OPR_CNCD  =   'N'
000090*
000410         ORDER BY A.DT_FIM_ATBC_NOTA
000410     END-EXEC.
000410
       77  CTE-FINAL-SS                  PIC X(40)      VALUE
000860              '*** S.S. TERMINA AQUI ***'.
000410*-----------------------------------------------------------------
000410 PROCEDURE DIVISION.
000410*-----------------------------------------------------------------
000090
000410* Habilita tratamento automatico de erros DB2
-INC HLPKE997
000410* Habilita captura automatica de abend e erros CICS
-INC HLPKE999
000090
000410     PERFORM 100000-DESCOBRIR-CANAL
000410     PERFORM 110000-EXCLUIR-CTNR-RPST
000410     PERFORM 120000-EXCLUIR-CTNR-ERRO
000410     PERFORM 130000-RECEBER-REQUISICAO
000410     PERFORM 140000-VALIDAR-PARAMETRO
000090
000410     PERFORM 970101-ABRE-CURSOR-PSQ
000410     PERFORM 970102-LER-CURSOR-PSQ
000410     PERFORM 970103-FECHA-CURSOR-PSQ
000090
000410     PERFORM 200000-PROCESSA-PESQUISA
000410              VARYING SGTSLA74-IC-TAB FROM 1 BY 1
000410              UNTIL SGTSLA74-IC-TAB > SGTSLA74-QT-OCR-TAB
000090
000410     PERFORM 300000-POSTA-RESPOSTA
000090
000410     GOBACK
000410     .

000410*-----------------------------------------------------------------
000410 100000-DESCOBRIR-CANAL          SECTION.
000410*-----------------------------------------------------------------
000410     MOVE 0001                   TO  MSG1-CD-SEQL-ERRO
000090
      *Por causa da captura automática de erro CICS, deve ser previsto
004370*o erro de container

004380     SET  ERRO-CANAL             TO  TRUE

000410     EXEC CICS ASSIGN
000410        CHANNEL ( NM-CNL )
000410     END-EXEC

      *    Ao chegar aqui, há certeza de que não houve erro algum
004450     SET  SEM-ERRO               TO  TRUE
000410     .
000410 100000-SAI.
000410     EXIT.

000410*-----------------------------------------------------------------
000410 110000-EXCLUIR-CTNR-RPST        SECTION.
000410*-----------------------------------------------------------------
000410     MOVE 0002                   TO  MSG1-CD-SEQL-ERRO
000090
000410     EXEC CICS DELETE
000410        CONTAINER ( 'SGTSLA74-RPST' )
000410        CHANNEL   ( NM-CNL          )
000410        NOHANDLE
000410     END-EXEC
000090
000410     IF  NOT (EIBRESP EQUAL DFHRESP(NORMAL)    OR
000410         EIBRESP EQUAL DFHRESP(CONTAINERERR) )
000410         PERFORM 999002-TRATA-CONDITION-CICS
000410     END-IF
000410     .
000410 110000-SAI.
000410     EXIT.

000410*-----------------------------------------------------------------
000410 120000-EXCLUIR-CTNR-ERRO        SECTION.
000410*-----------------------------------------------------------------
000410     MOVE 0003                   TO  MSG1-CD-SEQL-ERRO
000090
000410     EXEC CICS DELETE
000410        CONTAINER ( 'PRM-ERRO' )
000410        CHANNEL   ( NM-CNL     )
000410        NOHANDLE
000410     END-EXEC
000090
000410     IF  NOT (EIBRESP EQUAL DFHRESP(NORMAL)    OR
000410         EIBRESP EQUAL DFHRESP(CONTAINERERR) )
000410         PERFORM 999002-TRATA-CONDITION-CICS
000410     END-IF
000410     .
000410 120000-SAI.
000410     EXIT.

000410*-----------------------------------------------------------------
000410 130000-RECEBER-REQUISICAO       SECTION.
000410*-----------------------------------------------------------------
000410     MOVE 0004                   TO  MSG1-CD-SEQL-ERRO
000090
000410     MOVE ZEROS                      TO  QT-CCT-LIDO
000410     MOVE LENGTH OF SGTSLA74-RQSC    TO  QT-CCT-EPRO
000090
000410     EXEC CICS GET
000410        CONTAINER ( 'SGTSLA74-RQSC' )
000410        CHANNEL   ( NM-CNL          )
000410        NODATA
000410        FLENGTH   ( QT-CCT-LIDO     )
000410     END-EXEC
000090
000410     IF  QT-CCT-LIDO   NOT   EQUAL QT-CCT-EPRO
000410         SET   ERRO-TAMANHO-AREA   TO  TRUE
000410         PERFORM 999000-GRAVA-ERRO-E-ENCERRA
000410     END-IF
000090
000410     EXEC CICS GET
000410        CONTAINER ( 'SGTSLA74-RQSC'         )
000410        CHANNEL   ( NM-CNL                  )
000410        FLENGTH   ( LENGTH OF SGTSLA74-RQSC )
000410        INTO      ( SGTSLA74-RQSC           )
000410     END-EXEC
000410     .
000410 130000-SAI.
000410     EXIT.

000410*-----------------------------------------------------------------
000410 140000-VALIDAR-PARAMETRO        SECTION.
000410*-----------------------------------------------------------------
000410     MOVE 0005                   TO  MSG1-CD-SEQL-ERRO
000090
000410     MOVE 'N'                    TO  SGTSLA74-IN-CTNC-PSQ

           IF  SGTSLA74-E-CD-CTRA-TIT-VL < ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0001               TO MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de título e valores mobiliários
      -             'inválido.'        TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSLA74-E-CD-CTRA-TIT-VL EQUAL ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0002               TO MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de título e valores mobiliários
      -             'não informado.'   TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
000090
000410     IF  SGTSLA74-E-CD-OPR-ETRD-MCP  IS NOT NUMERIC
000410         SET  ERRO-PROGRAMADO    TO  TRUE
000410         MOVE 0003               TO MSG1-CD-ERRO-TS
                   MOVE 'Código operação estruturada mercado de capitais
      -                 ' não numérico.'
000410                                 TO  MSG1-TX-ERRO-TS
000410         PERFORM 999000-GRAVA-ERRO-E-ENCERRA
000410     END-IF

           IF  SGTSLA74-E-CD-OPR-ETRD-MCP  EQUAL ZEROS
000410         SET  ERRO-PROGRAMADO    TO  TRUE
000410         MOVE 0005               TO  MSG1-CD-ERRO-TS
001410         MOVE 'Código operação estruturada mercado de capitais não
      -             ' informado.'      TO  MSG1-TX-ERRO-TS
000410         PERFORM 999000-GRAVA-ERRO-E-ENCERRA
000410     END-IF
000090
000410     IF  SGTSLA74-E-NR-VRS-OPR-ETRD  EQUAL ZEROS
000410         SET  ERRO-PROGRAMADO    TO  TRUE
000410         MOVE 0007               TO MSG1-CD-ERRO-TS
                   MOVE 'Número da versão da operação não informado.'
000410                                 TO  MSG1-TX-ERRO-TS
000410         PERFORM 999000-GRAVA-ERRO-E-ENCERRA
000410     END-IF
000090
000410     IF  SGTSLA74-E-CD-AG-CLSC-RSCO  EQUAL ZEROS
000410         CONTINUE
000410     ELSE
000410         MOVE SGTSLA74-E-CD-AG-CLSC-RSCO   TO  WS-CD-AG-CLSC-INC
000410                                               WS-CD-AG-CLSC-FIM
000410     END-IF
000090
000410     IF  SGTSLA74-E-NR-SEQL-NOTA-OPR   EQUAL   ZEROS
000410         CONTINUE
000410     ELSE
000410         MOVE SGTSLA74-E-NR-SEQL-NOTA-OPR  TO  WS-NR-SEQL-NOTA-INC
000410                                               WS-NR-SEQL-NOTA-FIM
000410     END-IF
000090
000410     IF  SGTSLA74-E-DT-INC-ATBC-NOTA EQUAL SPACES
000410         CONTINUE
000410     ELSE
000410         MOVE SGTSLA74-E-DT-INC-ATBC-NOTA  TO  WS-DT-INC-ATBC
000410     END-IF
000090
000410     IF  SGTSLA74-E-DT-FIM-ATBC-NOTA EQUAL SPACES
000410         CONTINUE
000410     ELSE
000410         MOVE SGTSLA74-E-DT-FIM-ATBC-NOTA  TO  WS-DT-FIM-ATBC
000410     END-IF
000410     .
000410 140000-SAI.
000410     EXIT.

000410*-----------------------------------------------------------------
000410 200000-PROCESSA-PESQUISA        SECTION.
000410*-----------------------------------------------------------------
000410     MOVE 0006                   TO  MSG1-CD-SEQL-ERRO
000410
000410     MOVE NR-SEQL-NOTA-OPR(SGTSLA74-IC-TAB)  TO
000410                       SGTSLA74-NR-SEQL-NOTA-OPR(SGTSLA74-IC-TAB)
000090
000410     MOVE CD-AG-CLSC-RSCO(SGTSLA74-IC-TAB)   TO
000410                       SGTSLA74-CD-AG-CLSC-RSCO(SGTSLA74-IC-TAB)
000090
000410     MOVE NR-ETRD-AG-CLSC(SGTSLA74-IC-TAB)   TO
000410                       SGTSLA74-NR-ETRD-AG-CLSC(SGTSLA74-IC-TAB)
000090
000410     MOVE NR-SEQL-NOTA-AG(SGTSLA74-IC-TAB)   TO
000410                       SGTSLA74-NR-SEQL-NOTA-AG(SGTSLA74-IC-TAB)
000090
000410     MOVE CD-ARQ-OPR-ETRD(SGTSLA74-IC-TAB)   TO
000410                       SGTSLA74-CD-ARQ-OPR-ETRD(SGTSLA74-IC-TAB)
000090
000410     MOVE DT-INC-ATBC-NOTA(SGTSLA74-IC-TAB)  TO
000410                       SGTSLA74-DT-INC-ATBC-NOTA(SGTSLA74-IC-TAB)
000090
000410     MOVE DT-FIM-ATBC-NOTA(SGTSLA74-IC-TAB)  TO
000410                       SGTSLA74-DT-FIM-ATBC-NOTA(SGTSLA74-IC-TAB)
000410     .
000410 200000-SAI.
000410     EXIT.

000410*-----------------------------------------------------------------
000410 300000-POSTA-RESPOSTA           SECTION.
000410*-----------------------------------------------------------------
000410     MOVE 0007                   TO  MSG1-CD-SEQL-ERRO
000090
000410     EXEC CICS PUT
000410        CONTAINER ( 'SGTSLA74-RPST'         )
000410        CHANNEL   ( NM-CNL                  )
000410        FLENGTH   ( LENGTH OF SGTSLA74-RPST )
000410        FROM      ( SGTSLA74-RPST           )
000410     END-EXEC
000410     .
000410 300000-SAI.
000410     EXIT.

000410*-----------------------------------------------------------------
000410 970101-ABRE-CURSOR-PSQ          SECTION.
000410*-----------------------------------------------------------------
000410     MOVE 0008                   TO  MSG1-CD-SEQL-ERRO
000090
000410     EXEC SQL
000410          OPEN CURSOR-PSQ-1
000410     END-EXEC
000090
000410     MOVE 'S'                    TO  IN-CURSOR-PSQ-ABTO
000410     .
000410 970101-SAI.
000410     EXIT.

000410*-----------------------------------------------------------------
000410 970102-LER-CURSOR-PSQ           SECTION.
000410*-----------------------------------------------------------------
000410     MOVE 0009                   TO  MSG1-CD-SEQL-ERRO
000090
000410     INITIALIZE REG-WS
000090
000410      EXEC SQL
000410          FETCH NEXT ROWSET
000410                 CURSOR-PSQ-1 FOR 0101 ROWS
000410          INTO :NR-SEQL-NOTA-OPR
000410             , :CD-AG-CLSC-RSCO
000410             , :NR-ETRD-AG-CLSC
000410             , :NR-SEQL-NOTA-AG
000410             , :CD-ARQ-OPR-ETRD
000410               :CD-ARQ-OPR-ETRD-N
000410             , :DT-INC-ATBC-NOTA
000410             , :DT-FIM-ATBC-NOTA
000410               :DT-FIM-ATBC-NOTA-N
000410          END-EXEC
000090
000410     MOVE SQLERRD(3)             TO  QT-REG-SELD
000090
000410     IF  QT-REG-SELD EQUAL 0101
000410         SET SGTSLA74-TEM-MAIS-REGISTRO  TO  TRUE
000410         MOVE 0100               TO  QT-REG-SELD
000410     END-IF
000090
000410     MOVE QT-REG-SELD            TO  SGTSLA74-QT-OCR-TAB
000410     MOVE SQLCODE                TO  CD-FIM-CURSOR-PSQ
000410     .
000410 970102-SAI.
000410     EXIT.

000410*-----------------------------------------------------------------
000410 970103-FECHA-CURSOR-PSQ         SECTION.
000410*-----------------------------------------------------------------
000410     MOVE 0010                   TO  MSG1-CD-SEQL-ERRO
000090
000410     MOVE 'N'                    TO  IN-CURSOR-PSQ-ABTO
000090
000410      EXEC SQL
000410           CLOSE CURSOR-PSQ-1
000410      END-EXEC
000090
000410     .
000410 970103-SAI.
000410     EXIT.

000410*-----------------------------------------------------------------
000410 999000-GRAVA-ERRO-E-ENCERRA     SECTION.
000410*-----------------------------------------------------------------
           IF  IN-CURSOR-PSQ-ABTO  EQUAL  'S'
               PERFORM  970103-FECHA-CURSOR-PSQ
           END-IF

000410
000410*--> Monitora variàveis do pgm, grava e encerra

           IF  MSG1-CD-ERRO-TS             EQUAL  ZEROS
              MOVE  8888               TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS             EQUAL  SPACES
               MOVE  'Erro não especificado no módulo.'
                                       TO  MSG1-TX-ERRO-TS
           END-IF

      *--> VERIFICA SE O TIPO DE ERRO GERADO POSSUI TRATAMENTO INTERNO
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
      *==> Observação <=================================================
      *    Normalmente se Monitora apenas as Variáveis da Requisição
      *    para Monitoração da Mensageria.
      *    O Analista que especificou o módulo e o Analista Programador
      *    tem liberdade para Monitorar outras Variáveis que forem
      *    relevantes ao Suporte do Sistema.
      *=================================================================

      *--> Adiciona as variáveis para monitoração
000410     MOVE 'SGTSLA74-E-CD-CTRA-TIT-VL'    TO  MSGKW999-NM-VRV-MNT
000410     MOVE SGTSLA74-E-CD-CTRA-TIT-VL      TO  MSGKW999-VL-VRV-MNT
000410     PERFORM 999003-ADICIONA-VRV-MNT
000090
000410     MOVE 'SGTSLA74-E-CD-OPR-ETRD-MCP'   TO  MSGKW999-NM-VRV-MNT
           IF  SGTSLA74-E-CD-OPR-ETRD-MCP  IS NUMERIC
               MOVE SGTSLA74-E-CD-OPR-ETRD-MCP TO  CD-OPR-ETRD-MCP
               MOVE CD-OPR-ETRD-MCP-TX         TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE 'Código não numérico.'     TO  MSGKW999-VL-VRV-MNT
003880     END-IF
000410     PERFORM 999003-ADICIONA-VRV-MNT
000090
000410     MOVE 'SGTSLA74-E-NR-VRS-OPR-ETRD'    TO  MSGKW999-NM-VRV-MNT
000410     MOVE SGTSLA74-E-NR-VRS-OPR-ETRD      TO  MSGKW999-VL-VRV-MNT
000410     PERFORM 999003-ADICIONA-VRV-MNT
000090
000410     MOVE 'SGTSLA74-E-CD-AG-CLSC-RSCO'    TO  MSGKW999-NM-VRV-MNT
000410     MOVE SGTSLA74-E-CD-AG-CLSC-RSCO      TO  MSGKW999-VL-VRV-MNT
000410     PERFORM 999003-ADICIONA-VRV-MNT
000090
000410     MOVE 'SGTSLA74-E-DT-INC-ATBC-NOTA'   TO  MSGKW999-NM-VRV-MNT
000410     MOVE SGTSLA74-E-DT-INC-ATBC-NOTA     TO  MSGKW999-VL-VRV-MNT
000410     PERFORM 999003-ADICIONA-VRV-MNT
000090
000410     MOVE 'SGTSLA74-E-DT-FIM-ATBC-NOTA'   TO  MSGKW999-NM-VRV-MNT
000410     MOVE SGTSLA74-E-DT-FIM-ATBC-NOTA     TO  MSGKW999-VL-VRV-MNT
000410     PERFORM 999003-ADICIONA-VRV-MNT
000090
000410     MOVE 'SGTSLA74-E-NR-SEQL-NOTA-OPR'   TO  MSGKW999-NM-VRV-MNT
000410     MOVE SGTSLA74-E-NR-SEQL-NOTA-OPR     TO  MSGKW999-VL-VRV-MNT
000410     PERFORM 999003-ADICIONA-VRV-MNT

000410* --> grava o erro no msg
000410     PERFORM MSG1-NTF-MSG
000410     MOVE MSG1-CD-ERRO-TS                TO  IIBKERRO-CD-ERRO
000410     MOVE MSG1-TX-ERRO-TS                TO  IIBKERRO-TX-ERRO
000090
000410     EXEC CICS PUT
000410        CONTAINER ( 'PRM-ERRO'                  )
000410        CHANNEL   ( NM-CNL                      )
000410        FROM      ( IIBKERRO-PRM-ERRO           )
000410        FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
000410        NOHANDLE
000410     END-EXEC
000090
000410     PERFORM 999999-ENCERRAR
000410     .
000410 999000-SAI.
000410     EXIT.

000410* --------------------------------------*
000410* 999001-TRATA-ABEND-CICS        SECTION.
000410* 999002-TRATA-CONDITION-CICS    SECTION.
000410* 999003-ADICIONA-VRV-MNT        SECTION.
      * 999004-ALINHA-NUMERO-ESQUERDA  SECTION.
000410* --------------------------------------*
-INC HLPKP999
000410* --------------------------------------*
000410* 999010-TRATA-ERRO-DB2          SECTION.
-INC HLPKP997
000410* --------------------------------------*
000410* MSG1-NTF-MSG                   SECTION.
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
000410* --------------------------------------*
000410 999999-ENCERRAR                 SECTION.
000410* --------------------------------------*
000410     EXEC CICS
000410          RETURN
000410     END-EXEC
           .
       999999-SAI.
           EXIT.