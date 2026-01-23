000760*-----------------------------------------------------------------
000760* PROGRAMA..: SGTSLA64
000760* ANALISTA..: F8785286 Roseli Akemi Kawahata
000760* AUTOR.....: F8785286 Roseli Akemi Kawahata
000760* OBJETIVO..: Consulta Tabela  DB2SGT.BCO_PCT_OPR
000760* COMPILACAO: COBOL/CICS/DB2
000760*-----------------------------------------------------------------
000760* VRS001 07.08.2017 - F8785286 DE 2016/145195 - IMPLANTACAO
000760*-----------------------------------------------------------------
000760 IDENTIFICATION DIVISION.
000760*-----------------------------------------------------------------
000760 PROGRAM-ID. SGTSLA64.
000760*-----------------------------------------------------------------
000760 ENVIRONMENT DIVISION.
000760*-----------------------------------------------------------------
000760 CONFIGURATION SECTION.
000760 SPECIAL-NAMES.
000760     DECIMAL-POINT IS COMMA.
000760*-----------------------------------------------------------------
000760 DATA DIVISION.
000760*-----------------------------------------------------------------
000760 WORKING-STORAGE SECTION.
000760*-----------------------------------------------------------------
       77  CTE-INICIO-SS               PIC  X(40)  VALUE
                   '*** S.S. COMEÇA AQUI ***'.
000760 77  CTE-PROG     PIC  X(17) VALUE '*** SGTSLA64 ***'.
000760 77  CTE-VERS     PIC  X(06) VALUE 'VRS001'.
000760*-----------------------------------------------------------------
000760* Book do programa de ambiente - CICS5000
000760* Definido na work para preservar as informacoes
-INC HLPKE01B
000760 LOCAL-STORAGE SECTION.
000760*-----------------------------------------------------------------
000760* book de para acionamento do msg1
-INC IIBKERRO
-INC MSGKWMG1
-INC HLPKW999

000760* Area de trabalho
      *--> Auxiliares para movimentação na monitoração de variáveis.
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).

000760 01 QT-REG-SELD                  PIC S9(4) COMP-5 VALUE 0.
000760
000760 01 CARTEIRA-300000              PIC S9(9)  COMP-5 VALUE 300000.
000760
000760* Campos utilizados para leitura
000760 01  REG-WS.
000760     03 CD-BCO-PCT               PIC S9(4) COMP-5
000760        OCCURS 101 TIMES         VALUE 0.
000760     03 CD-TIP-PRTC              PIC S9(9) COMP-5
000760        OCCURS 101 TIMES         VALUE 0.
000760     03 IN-BCO-LDR               PIC  X(1)
000760        OCCURS 101 TIMES         VALUE SPACES.
000760     03 PC-PRTC-BCO              PIC S9(4)V9999  COMP-3
000760        OCCURS 101 TIMES         VALUE 0.
000760     03 TS-ULT-ATL               PIC  X(26)
000760        OCCURS 101 TIMES         VALUE ' '.
000760     03 CD-USU-ULT-ATL           PIC  X(8)
000760        OCCURS 101 TIMES         VALUE ' '.
000760
000760* Indicadores de nulidades
000760
000760* Variáveis auxiliares
000760 01 AUX-CD-TIP-EST-DCS           PIC S9(4) COMP-5 VALUE 2.
000760
000760* Variavel(is) de controle do cursor(es)
000760 01  CTL-CURSOR-PSQ.
000760     03  CD-FIM-CURSOR-PSQ       PIC S9(04) COMP-5 VALUE 0.
000760         88  FIM-CURSOR-PSQ      VALUE +100.
000760     03  IN-CURSOR-PSQ-ABTO      PIC  X(01) VALUE 'N'.
000760         88  CURSOR-PSQ-ABTO     VALUE 'S'.
000760
000760* Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1
000760
000760* Subrotinas utilizadas
000760     EXEC SQL
000760         INCLUDE SQLCA
000760     END-EXEC.
000760*-----------------------------------------------------------------
000410*--> Pesquisa Característica da Carteira mais recente
000410*-----------------------------------------------------------------
-INC SGTKLA64
000760     EXEC SQL
000760       DECLARE CURSOR-PSQ-1 CURSOR
000760               WITH ROWSET POSITIONING FOR
000760       SELECT A.CD_BCO_PCT
000760            , A.CD_TIP_PRTC
000760            , A.IN_BCO_LDR
000760            , A.PC_PRTC_BCO
000760            , A.CD_USU_ULT_ATL
000760            , A.TS_ULT_ATL
000760       FROM DB2SGT.BCO_PCT_OPR  A
000760       WHERE A.CD_CTRA_TIT_VL      = :SGTSLA64-CD-CTRA-TIT-VL
000760         AND A.CD_OPR_ETRD_MCP     = :SGTSLA64-CD-OPR-ETRD-MCP
000760         AND A.NR_VRS_OPR_ETRD     = :SGTSLA64-NR-VRS-OPR-ETRD
000760         AND A.CD_TIP_PRTC         = :SGTSLA64-E-CD-TIP-PRTC
000760         AND A.IN_BCO_LDR          = :SGTSLA64-E-IN-BCO-LDR
000760
007600      ORDER BY  A.CD_BCO_PCT
000760     END-EXEC.
000760
       01  CTE-FINAL-SS                PIC  X(40)      VALUE
                    '*** S.S. TERMINA AQUI ***'.
000760*-----------------------------------------------------------------
000760 PROCEDURE DIVISION.
000760*-----------------------------------------------------------------
000760
000760* Habilita tratamento automatico de erros DB2
-INC HLPKE997
000760* Habilita captura automatica de abend e erros CICS
-INC HLPKE999
000760
000760     PERFORM 100000-DESCOBRIR-CANAL
000760     PERFORM 110000-EXCLUIR-CTNR-RPST
000760     PERFORM 120000-EXCLUIR-CTNR-ERRO
000760     PERFORM 130000-RECEBER-REQUISICAO
000760     PERFORM 140000-VALIDAR-PARAMETRO
000760
000760     PERFORM 970101-ABRE-CURSOR-PSQ
000760     PERFORM 970102-LER-CURSOR-PSQ
000760     PERFORM 970103-FECHA-CURSOR-PSQ
000760
000760     PERFORM 200000-PROCESSA-PESQUISA
000760              VARYING SGTSLA64-IC-TAB FROM 1 BY 1
000760              UNTIL SGTSLA64-IC-TAB > SGTSLA64-QT-OCR-TAB
000760
000760     PERFORM 300000-POSTA-RESPOSTA
000760
000760     GOBACK
000760     .
000760*-----------------------------------------------------------------
000760 100000-DESCOBRIR-CANAL          SECTION.
000760*-----------------------------------------------------------------
000760     MOVE 0001                   TO  MSG1-CD-SEQL-ERRO
000760
000760*Por causa da captura automática de erro CICS, deve ser previsto
000760*o erro de container
000760     SET  ERRO-CANAL             TO  TRUE
000760
000760     EXEC CICS ASSIGN
000760        CHANNEL ( NM-CNL )
000760     END-EXEC
000760
000760*    Ao chegar aqui, há certeza de que não houve erro algum
000760     SET  SEM-ERRO               TO  TRUE
000760     .
000760 100000-SAI.
000760     EXIT.

000760*-----------------------------------------------------------------
000760 110000-EXCLUIR-CTNR-RPST        SECTION.
000760*-----------------------------------------------------------------
000760     MOVE 0002                   TO  MSG1-CD-SEQL-ERRO
000760
000760     EXEC CICS DELETE
000760        CONTAINER ( 'SGTSLA64-RPST' )
000760        CHANNEL   ( NM-CNL          )
000760        NOHANDLE
000760     END-EXEC
000760
000760     IF  NOT (EIBRESP EQUAL DFHRESP(NORMAL)   OR
000760         EIBRESP EQUAL DFHRESP(CONTAINERERR) )
000760         PERFORM 999002-TRATA-CONDITION-CICS
000760     END-IF
000760     .
000760 110000-SAI.
000760     EXIT.

000760*-----------------------------------------------------------------
000760 120000-EXCLUIR-CTNR-ERRO        SECTION.
000760*-----------------------------------------------------------------
000760     MOVE 0003                   TO  MSG1-CD-SEQL-ERRO
000760
000760     EXEC CICS DELETE
000760        CONTAINER ( 'PRM-ERRO' )
000760        CHANNEL   ( NM-CNL      )
000760        NOHANDLE
000760     END-EXEC
000760
000760     IF  NOT (EIBRESP EQUAL DFHRESP(NORMAL)   OR
000760         EIBRESP EQUAL DFHRESP(CONTAINERERR) )
000760         PERFORM 999002-TRATA-CONDITION-CICS
000760     END-IF
000760     .
000760 120000-SAI.
000760     EXIT.

000760*-----------------------------------------------------------------
000760 130000-RECEBER-REQUISICAO       SECTION.
000760*-----------------------------------------------------------------
000760     MOVE 0004                   TO  MSG1-CD-SEQL-ERRO
000760
000760     MOVE ZEROS                      TO  QT-CCT-LIDO
000760     MOVE LENGTH OF SGTSLA64-RQSC    TO  QT-CCT-EPRO
000760
000760     EXEC CICS GET
000760        CONTAINER ( 'SGTSLA64-RQSC' )
000760        CHANNEL   ( NM-CNL          )
000760        NODATA
000760        FLENGTH   ( QT-CCT-LIDO     )
000760     END-EXEC
000760
000760     IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
000760         SET   ERRO-TAMANHO-AREA   TO  TRUE
000760         PERFORM 999000-GRAVA-ERRO-E-ENCERRA
000760     END-IF
000760
000760     EXEC CICS GET
000760        CONTAINER ( 'SGTSLA64-RQSC'         )
000760        CHANNEL   ( NM-CNL                  )
000760        FLENGTH   ( LENGTH OF SGTSLA64-RQSC )
000760        INTO      ( SGTSLA64-RQSC           )
000760     END-EXEC
000760     .
000760 130000-SAI.
000760     EXIT.

000760*-----------------------------------------------------------------
000760 140000-VALIDAR-PARAMETRO        SECTION.
000760*-----------------------------------------------------------------
000760     MOVE 0005                   TO  MSG1-CD-SEQL-ERRO
000760
000760     MOVE 'N'                    TO  SGTSLA64-IN-CTNC-PSQ
000760
000760     IF  SGTSLA64-CD-CTRA-TIT-VL     EQUAL ZEROS
000760         SET  ERRO-PROGRAMADO        TO  TRUE
000760         MOVE 002                    TO  MSG1-CD-ERRO-TS
000760         MOVE 'Código da carteira de título e valores mobiliários
      -             'não informado.'       TO  MSG1-TX-ERRO-TS
000760         PERFORM 999000-GRAVA-ERRO-E-ENCERRA
000760     END-IF

           IF  SGTSLA64-CD-OPR-ETRD-MCP NOT NUMERIC
               SET  ERRO-PROGRAMADO    TO TRUE
000760         MOVE 0003               TO  MSG1-CD-ERRO-TS
000760         MOVE 'Código operação estruturada mercado de capitais não
      -             ' numérico.'       TO MSG1-TX-ERRO-TS
000760         PERFORM 999000-GRAVA-ERRO-E-ENCERRA
000760     END-IF
000760
000760     IF  SGTSLA64-CD-OPR-ETRD-MCP NOT GREATER 0
000760         SET  ERRO-PROGRAMADO    TO TRUE
000760         MOVE 0004               TO  MSG1-CD-ERRO-TS
000760         MOVE 'Código operação estruturada mercado de capitais inv
      -             'álido.'           TO MSG1-TX-ERRO-TS
000760         PERFORM 999000-GRAVA-ERRO-E-ENCERRA
000760     END-IF
000760
000760     IF  SGTSLA64-NR-VRS-OPR-ETRD   EQUAL ZEROS
000760         SET  ERRO-PROGRAMADO        TO  TRUE
000760         MOVE 007                    TO  MSG1-CD-ERRO-TS
000760         MOVE 'Número da versão da operação não informado.'
000760                                     TO  MSG1-TX-ERRO-TS
000760         PERFORM 999000-GRAVA-ERRO-E-ENCERRA
000760     END-IF
000760
000760     IF  SGTSLA64-E-CD-TIP-PRTC      EQUAL ZEROS
000760         SET  ERRO-PROGRAMADO        TO  TRUE
000760         MOVE 008                    TO  MSG1-CD-ERRO-TS
000760         MOVE 'Código do tipo da participação inválido.'
000760                                     TO  MSG1-TX-ERRO-TS
000760         PERFORM 999000-GRAVA-ERRO-E-ENCERRA
000760     END-IF
000760
000760     IF  SGTSLA64-E-IN-BCO-LDR       EQUAL SPACES
000760         SET  ERRO-PROGRAMADO        TO  TRUE
000760         MOVE 601                    TO  MSG1-CD-ERRO-TS
000760         MOVE 'Indicador de Banco Lider deve ser informado.'
000760                                     TO  MSG1-TX-ERRO-TS
000760         PERFORM 999000-GRAVA-ERRO-E-ENCERRA
000760     END-IF
000760     .
000760 140000-SAI.
000760     EXIT.

000760*-----------------------------------------------------------------
000760 200000-PROCESSA-PESQUISA        SECTION.
000760*-----------------------------------------------------------------
000760     MOVE 0006                   TO  MSG1-CD-SEQL-ERRO
000760
000760     MOVE CD-BCO-PCT(SGTSLA64-IC-TAB)      TO
000760                       SGTSLA64-CD-BCO-PCT(SGTSLA64-IC-TAB)
000760
000760     MOVE CD-TIP-PRTC(SGTSLA64-IC-TAB)     TO
000760                       SGTSLA64-CD-TIP-PRTC(SGTSLA64-IC-TAB)
000760
000760     MOVE IN-BCO-LDR(SGTSLA64-IC-TAB)      TO
000760                       SGTSLA64-IN-BCO-LDR(SGTSLA64-IC-TAB)
000760
000760     MOVE PC-PRTC-BCO(SGTSLA64-IC-TAB)     TO
000760                       SGTSLA64-PC-PRTC-BCO(SGTSLA64-IC-TAB)
000760
000760     MOVE CD-USU-ULT-ATL(SGTSLA64-IC-TAB)  TO
000760                       SGTSLA64-CD-USU-ULT-ATL(SGTSLA64-IC-TAB)
000760
000760     MOVE TS-ULT-ATL(SGTSLA64-IC-TAB)      TO
000760                       SGTSLA64-TS-ULT-ATL(SGTSLA64-IC-TAB)
000760     .
000760 200000-SAI.
000760     EXIT.

000760*-----------------------------------------------------------------
000760 300000-POSTA-RESPOSTA           SECTION.
000760*-----------------------------------------------------------------
000760     MOVE 0007                   TO  MSG1-CD-SEQL-ERRO
000760
000760     EXEC CICS PUT
000760        CONTAINER ( 'SGTSLA64-RPST'         )
000760        CHANNEL   ( NM-CNL                  )
000760        FLENGTH   ( LENGTH OF SGTSLA64-RPST )
000760        FROM      ( SGTSLA64-RPST           )
000760     END-EXEC
000760     .
000760 300000-SAI.
000760     EXIT.

000760*-----------------------------------------------------------------
000760 970101-ABRE-CURSOR-PSQ          SECTION.
000760*-----------------------------------------------------------------
000760     MOVE 0008                   TO  MSG1-CD-SEQL-ERRO
000760
000760     EXEC SQL
000760          OPEN CURSOR-PSQ-1
000760     END-EXEC
000760
000760     MOVE 'S'                    TO  IN-CURSOR-PSQ-ABTO
000760     .
000760 970101-SAI.
000760     EXIT.

000760*-----------------------------------------------------------------
000760 970102-LER-CURSOR-PSQ           SECTION.
000760*-----------------------------------------------------------------
000760     MOVE 0009                   TO  MSG1-CD-SEQL-ERRO
000760
000760     INITIALIZE REG-WS
000760
000760     EXEC SQL
000760          FETCH NEXT ROWSET
000760                CURSOR-PSQ-1 FOR 0101 ROWS
000760          INTO :CD-BCO-PCT
000760             , :CD-TIP-PRTC
000760             , :IN-BCO-LDR
000760             , :PC-PRTC-BCO
000760             , :CD-USU-ULT-ATL
000760             , :TS-ULT-ATL
000760     END-EXEC
000760
000760
000760     MOVE SQLERRD(3)             TO  QT-REG-SELD
000760
000760     IF  QT-REG-SELD EQUAL 0101
000760         SET SGTSLA64-TEM-MAIS-REGISTRO TO TRUE
000760         MOVE 0100               TO  QT-REG-SELD
000760     END-IF
000760
000760     MOVE QT-REG-SELD            TO  SGTSLA64-QT-OCR-TAB
000760     MOVE SQLCODE                TO  CD-FIM-CURSOR-PSQ
000760     .
000760 970102-SAI.
000760     EXIT.

000760*-----------------------------------------------------------------
000760 970103-FECHA-CURSOR-PSQ         SECTION.
000760*-----------------------------------------------------------------
000760     MOVE 0010                   TO  MSG1-CD-SEQL-ERRO
000760
000760     MOVE 'N'                    TO  IN-CURSOR-PSQ-ABTO
000760
000760     EXEC SQL
000760          CLOSE CURSOR-PSQ-1
000760     END-EXEC
000760     .
000760 970103-SAI.
000760     EXIT.

000760*-----------------------------------------------------------------
000760 999000-GRAVA-ERRO-E-ENCERRA     SECTION.
000760*-----------------------------------------------------------------
000760
000760*--> Monitora variàveis do pgm, grava e encerra
000760
000760     IF  MSG1-CD-ERRO-TS             EQUAL  ZEROS
000760        MOVE  8888               TO  MSG1-CD-ERRO-TS
000760     END-IF
000760
000760     IF  MSG1-TX-ERRO-TS             EQUAL  SPACES
000760         MOVE  'Erro não especificado no módulo.'
000760                                 TO  MSG1-TX-ERRO-TS
000760     END-IF
000760
000760*--> VERIFICA SE O TIPO DE ERRO GERADO POSSUI TRATAMENTO INTERNO
000760     EVALUATE TRUE
000760         WHEN ERRO-CONTAINER
000760              PERFORM 999004-TRATA-ERRO-CTNR
000760         WHEN ERRO-TAMANHO-AREA
000760              PERFORM 999005-TRATA-ERRO-AREA-DADOS
000760         WHEN ERRO-CANAL
000760              PERFORM 999006-TRATA-ERRO-CANAL
000760         WHEN OTHER
000760              CONTINUE
000760     END-EVALUATE
000760
000760     MOVE 'SGTSLA64-CD-CTRA-TIT-VL'      TO  MSGKW999-NM-VRV-MNT
000760     MOVE SGTSLA64-CD-CTRA-TIT-VL        TO  MSGKW999-VL-VRV-MNT
000760     PERFORM 999003-ADICIONA-VRV-MNT
000760
           MOVE 'SGTSLA64-CD-OPR-ETRD-MCP'     TO  MSGKW999-NM-VRV-MNT
000760     IF  SGTSLA64-CD-OPR-ETRD-MCP        IS  NUMERIC
000760         MOVE SGTSLA64-CD-OPR-ETRD-MCP   TO  CD-OPR-ETRD-MCP
000760         MOVE CD-OPR-ETRD-MCP-TX         TO  MSGKW999-VL-VRV-MNT
000760     ELSE
               MOVE 'Valor não numérico.'      TO  MSGKW999-VL-VRV-MNT
000760     END-IF
           PERFORM 999003-ADICIONA-VRV-MNT
000760
000760     MOVE 'SGTSLA64-NR-VRS-OPR-ETRD'     TO  MSGKW999-NM-VRV-MNT
000760     MOVE SGTSLA64-NR-VRS-OPR-ETRD       TO  MSGKW999-VL-VRV-MNT
000760     PERFORM 999003-ADICIONA-VRV-MNT
000760
000760     MOVE 'SGTSLA64-E-CD-TIP-PRTC'       TO  MSGKW999-NM-VRV-MNT
000760     MOVE SGTSLA64-E-CD-TIP-PRTC         TO  MSGKW999-VL-VRV-MNT
000760     PERFORM 999003-ADICIONA-VRV-MNT
000760
000760     MOVE 'SGTSLA64-E-IN-BCO-LDR'        TO  MSGKW999-NM-VRV-MNT
000760     MOVE SGTSLA64-E-IN-BCO-LDR          TO  MSGKW999-VL-VRV-MNT
000760     PERFORM 999003-ADICIONA-VRV-MNT
000760
000760* --> grava o erro no msg
000760     PERFORM MSG1-NTF-MSG
000760     MOVE MSG1-CD-ERRO-TS                TO  IIBKERRO-CD-ERRO
000760     MOVE MSG1-TX-ERRO-TS                TO  IIBKERRO-TX-ERRO
000760
000760     EXEC CICS PUT
000760        CONTAINER ( 'PRM-ERRO'                  )
000760        CHANNEL   ( NM-CNL                      )
000760        FROM      ( IIBKERRO-PRM-ERRO           )
000760        FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
000760        NOHANDLE
000760     END-EXEC
000760
000760     PERFORM 999999-ENCERRAR
000760     .
000760 999000-SAI.
000760     EXIT.

000760* --------------------------------------*
000760* 999001-TRATA-ABEND-CICS        SECTION.
000760* 999002-TRATA-CONDITION-CICS    SECTION.
000760* 999003-ADICIONA-VRV-MNT        SECTION.
      * 999004-ALINHA-NUMERO-ESQUERDA  SECTION.
000760* --------------------------------------*
-INC HLPKP999
000760* --------------------------------------*
000760* 999010-TRATA-ERRO-DB2          SECTION.
-INC HLPKP997
000760* --------------------------------------*
000760* MSG1-NTF-MSG                   SECTION.
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
000760* --------------------------------------*
000760* 999004-TRATA-ERRO-CTNR         SECTION.
000760* 999005-TRATA-ERRO-AREA-DADOS   SECTION.
000760* 999006-TRATA-ERRO-CANAL        SECTION.
000760*---------------------------------------*
-INC SGTKP999

000760* --------------------------------------*
000760 999999-ENCERRAR                SECTION.
000760* --------------------------------------*
000760     EXEC CICS
000760          RETURN
000760     END-EXEC
           .
       999999-SAI.
           EXIT.