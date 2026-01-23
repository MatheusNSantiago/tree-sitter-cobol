      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSL061
      * ANALISTA..: IVONE DE MELO COSTA RIBEIRO
      * AUTOR.....: IVONE DE MELO COSTA RIBEIRO
      * OBJETIVO..: LISTA CONTAINER DA TABELA  RPAC_PGMD_OPR
      * COMPILACAO: COBOL/CICS/SQL
      *-----------------------------------------------------------------
      * VRS002 04/02/2019 F8658869 DE 2015/137233 - Incluir monitoração
      *                   de variáveis para a rotina SBDATAF.
      *        02/12/2016 F0175702 DE 2015/137233 - Feito ajuste para
      *                   Cursor ROWSET a pedido do RT.
      *VRS001  31/10/2016 F4535118 DE 2015/137233 - IMPLANTACAO
      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSL061.

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

       77  CTE-INICIO-SS               PIC X(35) VALUE
               '*** STORAGE SECTION COMEÇA AQUI ***'.
       77  CTE-PROG                    PIC X(18) VALUE
               '*** SGTSL061 ***'.
       77  CTE-VERS                    PIC X(06) VALUE 'VRS002'.

      *    Book do programa de ambiente - CICS5000
      *    Definido na working para preservar as informacoes
-INC HLPKE01B

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------
      *    Campos de trabalho
-INC IIBKERRO
-INC MSGKWMG1
-INC HLPKW999


       01  QT-REG-SELD                 PIC S9(4) COMP-5 VALUE 0.

      *--> Auxiliares Movimentacao Monitoração
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).

      *    Chamada à subrotina SBDATAF
001020*
       01  WKSBDATA        PIC X(4099).
       01  AREA-RED REDEFINES WKSBDATA.
           03  AREA-TAM    PIC 9(0008) COMP.
           03  FILLER      PIC X(4071).
           03  AREA-FIM    PIC X(0020).

001030 01  SBDATAF                     PIC  X(08) VALUE 'SBDATAF'.
001040 01  FUNCAO                      PIC  X(03) VALUE 'F01'.
001050 01  ARG01                       PIC  9(08).
001060 01  ARG01-R  REDEFINES  ARG01.
001070   03  DIA                       PIC  9(02).
001080   03  MES                       PIC  9(02).
001090   03  ANO                       PIC  9(04).
      *     Valida no formato Gregoriano.
001100 01  ARG02                       PIC  9(01) VALUE 4.

      *    Campos utilizados para leitura de container
-INC SGTKERR1

       01  REG-WS.
           03  DT-RPAC-PGMD            PIC X(10)
           OCCURS 101 TIMES.
           03  CD-USU-ULT-ATL          PIC X(8)
           OCCURS 101 TIMES            VALUE ' '.
           03  TS-ULT-ATL              PIC X(26)
           OCCURS 101 TIMES            VALUE ' '.
           03  TX-MTV-RPAC-PGMD        PIC X(100)
           OCCURS 101 TIMES            VALUE ' '.
           03  TX-MTV-RPAC-PGMD-N      PIC S9(4) COMP-5
           OCCURS 101 TIMES            VALUE 0.


       01  ULT-DT-RPAC            PIC X(10)  VALUE '  .  .    '.
       01  ULT-DT-RPAC-R   REDEFINES ULT-DT-RPAC.
            05  DIA                 PIC 9(2).
            05  FILLER              PIC X(1).
            05  MES                 PIC 9(2).
            05  FILLER              PIC X(1).
            05  ANO                 PIC 9(4).

      *    Indicadores de nulidade

      *    Variaveis de controle dos cursores

       01  CTL-CURSOR-PSQ.
           03  CD-FIM-CURSOR-PSQ       PIC S9(4) COMP-5 VALUE 0.
               88  FIM-CURSOR-PSQ      VALUE +100.
           03  IN-CURSOR-PSQ-ABTO      PIC X(01) VALUE ' '.
               88  CURSOR-PSQ-ABTO     VALUE 'S'.

      *    Subrotinas utilizadas

           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

      *    Book do programa corrente
-INC SGTKL061

      *    Book de para acionamento do msg1

           EXEC SQL
             DECLARE CURSOR-PSQ CURSOR
             WITH ROWSET POSITIONING FOR
               SELECT A.DT_RPAC_PGMD
                    , A.CD_USU_ULT_ATL
                    , A.TS_ULT_ATL
                    , A.TX_MTV_RPAC_PGMD
               FROM DB2SGT.RPAC_PGMD_OPR A
               WHERE  A.CD_CTRA_TIT_VL  = :SGTSL061-CD-CTRA-TIT-VL
                 AND  A.CD_OPR_ETRD_MCP = :SGTSL061-CD-OPR-ETRD-MCP
                 AND  A.NR_VRS_OPR_ETRD = :SGTSL061-NR-VRS-OPR-ETRD
                 AND  A.DT_RPAC_PGMD    > :ULT-DT-RPAC
               ORDER BY A.DT_RPAC_PGMD
           END-EXEC.

       01  CTE-FINAL-SS                PIC X(40) VALUE
               '*** STORAGE SECTION TERMINA AQUI ***'.

      *-----------------------------------------------------------------
       PROCEDURE                       DIVISION.
      *-----------------------------------------------------------------
           MOVE 0001                   TO MSG1-CD-SEQL-ERRO

      *    WHENEVER DB2
-INC HLPKE997
      *    WHENEVER CICS
-INC HLPKE999

           PERFORM 100000-DESCOBRIR-CANAL
           PERFORM 110000-EXCLUIR-CTNR-RPST
           PERFORM 120000-EXCLUIR-CTNR-ERRO
           PERFORM 130000-RECEBER-REQUISICAO
           PERFORM 140000-VALIDAR-PARAMETRO

           PERFORM 970101-ABRE-CURSOR-PSQ
           PERFORM 970102-LER-CURSOR-PSQ
           PERFORM 200000-PROCESSA-PESQUISA
             VARYING SGTSL061-IC-TAB FROM 1 BY 1
             UNTIL SGTSL061-IC-TAB GREATER SGTSL061-QT-OCR-TAB
           PERFORM 970103-FECHA-CURSOR-PSQ

           PERFORM 300000-POSTA-RESPOSTA
           GOBACK
           .
      *-----------------------------------------------------------------
       100000-DESCOBRIR-CANAL SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO

           SET   ERRO-CANAL            TO  TRUE

           EXEC CICS ASSIGN
              CHANNEL ( NM-CNL )
           END-EXEC

           SET   SEM-ERRO              TO  TRUE
           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-EXCLUIR-CTNR-RPST SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS DELETE
              CONTAINER ( 'SGTSL061-RPST' )
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
           MOVE 0004                   TO MSG1-CD-SEQL-ERRO

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
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

           MOVE ZEROS                       TO QT-CCT-LIDO
           MOVE LENGTH OF SGTSL061-RQSC     TO QT-CCT-EPRO

           EXEC CICS GET
              CONTAINER ( 'SGTSL061-RQSC' )
              CHANNEL   ( NM-CNL          )
              NODATA
              FLENGTH   ( QT-CCT-LIDO     )
           END-EXEC

           IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
               SET  ERRO-TAMANHO-AREA       TO TRUE
               MOVE 'QT-CCT-LIDO'  TO MSGKW999-NM-VRV-MNT
               MOVE  QT-CCT-LIDO   TO MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE 'QT-CCT-EPRO'  TO MSGKW999-NM-VRV-MNT
               MOVE  QT-CCT-EPRO   TO MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS GET
              CONTAINER ( 'SGTSL061-RQSC'         )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSL061-RQSC )
              INTO      ( SGTSL061-RQSC           )
           END-EXEC
           .
       130000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       140000-VALIDAR-PARAMETRO SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

           MOVE 'N'                    TO SGTSL061-IN-CNTO-PSQ

           IF  SGTSL061-CD-CTRA-TIT-VL EQUAL ZEROS
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 0002                TO MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de título e valores mobiliários
      -             'não informado.'     TO MSG1-TX-ERRO-TS
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL061-CD-CTRA-TIT-VL     NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0001              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' inválido.'      TO  MSG1-TX-ERRO-TS
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL061-CD-OPR-ETRD-MCP NOT NUMERIC
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 0003                TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -        ' numérico.'             TO MSG1-TX-ERRO-TS
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL061-CD-OPR-ETRD-MCP EQUAL ZEROS
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 0005                TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -        ' informado.'            TO MSG1-TX-ERRO-TS
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL061-CD-OPR-ETRD-MCP    NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0004              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada do mercado de capit
      -              'ais inválido.'   TO  MSG1-TX-ERRO-TS
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL061-NR-VRS-OPR-ETRD EQUAL ZEROS
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 0007                TO MSG1-CD-ERRO-TS
               MOVE 'Número da versão da operação não informado.'
                                         TO MSG1-TX-ERRO-TS
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL061-NR-VRS-OPR-ETRD    NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0006              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação estruturada inválido.
      -              ''                TO  MSG1-TX-ERRO-TS
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           MOVE CORR SGTSL061-DT-RPAC-ULT-R TO ULT-DT-RPAC-R
           IF  DIA OF SGTSL061-DT-RPAC-ULT-R EQUAL SPACES
               OR MES OF SGTSL061-DT-RPAC-ULT-R EQUAL SPACES
               OR ANO OF SGTSL061-DT-RPAC-ULT-R EQUAL SPACES
               MOVE '01.01.1900'  TO ULT-DT-RPAC
           ELSE
               PERFORM 141000-VALIDAR-DATA
           END-IF
           .
       140000-SAI.
           EXIT.

002440*----------------------------------------------------------------*  VRS001
002450 141000-VALIDAR-DATA                SECTION.                        VRS001
002460*----------------------------------------------------------------*  VRS001
001960     MOVE 0007                   TO MSG1-CD-SEQL-ERRO               VRS001

           MOVE 4095 TO AREA-TAM.
           MOVE 'SBDATAF -WRKAREA-FIM' TO AREA-FIM.
005400
           MOVE CORR ULT-DT-RPAC-R  TO ARG01-R
005410     CALL SBDATAF     USING FUNCAO WKSBDATA  ARG01 ARG02
005440
005450     IF  RETURN-CODE             NOT EQUAL ZEROS AND 24 AND 22
                                                       AND 46
005460         SET ERRO-PROGRAMA-ACIONADO TO TRUE
005470         MOVE 0067               TO MSG1-CD-ERRO-TS
005480         MOVE RETURN-CODE        TO MSG1-CD-RTN-PGM-ACND
005490         MOVE 'Erro na rotina que valida data.'
005500                                 TO MSG1-TX-RTN-PGM-ACND
005510                                    MSG1-TX-ERRO-TS
005520         MOVE SBDATAF            TO MSG1-NM-PGM-ACND
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
005560
005570     END-IF
005580
005590     IF  RETURN-CODE               EQUAL 24 OR 22 OR 46
005600         SET ERRO-PROGRAMADO     TO TRUE
005610         MOVE 0049               TO MSG1-CD-ERRO-TS
005620         MOVE 'Data informada inválida'
005630                                 TO MSG1-TX-ERRO-TS
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
005660     END-IF
002400     .                                                              VRS001
002410 141000-SAI.                                                        VRS001
002420     EXIT.

      *-----------------------------------------------------------------
       200000-PROCESSA-PESQUISA SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

           MOVE DT-RPAC-PGMD(SGTSL061-IC-TAB)            TO
                                           ULT-DT-RPAC
           MOVE CORR ULT-DT-RPAC-R                       TO
                     SGTSL061-DT-RPAC-PGMD-R(SGTSL061-IC-TAB)
           MOVE CD-USU-ULT-ATL(SGTSL061-IC-TAB)          TO
                     SGTSL061-CD-USU-ULT-ATL(SGTSL061-IC-TAB)
           MOVE TS-ULT-ATL(SGTSL061-IC-TAB)              TO
                     SGTSL061-TS-ULT-ATL(SGTSL061-IC-TAB)
           MOVE TX-MTV-RPAC-PGMD(SGTSL061-IC-TAB)        TO
                     SGTSL061-TX-MTV-RPAC-PGMD(SGTSL061-IC-TAB)
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-POSTA-RESPOSTA SECTION.
      *-----------------------------------------------------------------
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS PUT
              CONTAINER ( 'SGTSL061-RPST'         )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSL061-RPST )
              FROM      ( SGTSL061-RPST           )
           END-EXEC
           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       969000-MONITORAR-VARIAVEIS      SECTION.
      *--> Monitora as variáveis do módulo
      *-----------------------------------------------------------------
           MOVE  0010                  TO  MSG1-CD-SEQL-ERRO

           MOVE 'SGTSL061-CD-CTRA-TIT-VL'   TO MSGKW999-NM-VRV-MNT
           MOVE  SGTSL061-CD-CTRA-TIT-VL    TO MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL061-CD-OPR-ETRD-MCP'  TO MSGKW999-NM-VRV-MNT
      *-----------> Verifica se o conteudo e numerico antes de adicionar
           IF  SGTSL061-CD-OPR-ETRD-MCP IS NUMERIC
               MOVE SGTSL061-CD-OPR-ETRD-MCP TO CD-OPR-ETRD-MCP
               MOVE  CD-OPR-ETRD-MCP-TX     TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico'   TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL061-NR-VRS-OPR-ETRD'  TO MSGKW999-NM-VRV-MNT
           MOVE  SGTSL061-NR-VRS-OPR-ETRD   TO MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL061-DT-RPAC-ULT'      TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL061-DT-RPAC-ULT        TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           IF  NM-PGM  EQUAL  SBDATAF
               MOVE  'RETURN-CODE'          TO MSGKW999-NM-VRV-MNT
               MOVE   RETURN-CODE           TO MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE  'FUNCAO'               TO MSGKW999-NM-VRV-MNT
               MOVE   FUNCAO                TO MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE  'ARG01'                TO MSGKW999-NM-VRV-MNT
               MOVE   ARG01                 TO MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE  'ARG02'                TO MSGKW999-NM-VRV-MNT
               MOVE   ARG02                 TO MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT
           END-IF
           .
       969000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970101-ABRE-CURSOR-PSQ SECTION.
      *-----------------------------------------------------------------
           MOVE 0011                   TO MSG1-CD-SEQL-ERRO

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
           MOVE 0012                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE REG-WS
           EXEC SQL
                FETCH NEXT ROWSET
                CURSOR-PSQ FOR 101 ROWS
               INTO :DT-RPAC-PGMD
                  , :CD-USU-ULT-ATL
                  , :TS-ULT-ATL
                  , :TX-MTV-RPAC-PGMD
                    :TX-MTV-RPAC-PGMD-N
           END-EXEC

           MOVE SQLERRD(3)             TO QT-REG-SELD
           IF  QT-REG-SELD EQUAL 0101
               SET SGTSL061-TEM-MAIS-REGISTRO TO TRUE
               MOVE 0100               TO QT-REG-SELD
           END-IF
           MOVE QT-REG-SELD            TO SGTSL061-QT-OCR-TAB
           MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ
           .
       970102-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970103-FECHA-CURSOR-PSQ SECTION.
      *-----------------------------------------------------------------
           MOVE 0013                   TO MSG1-CD-SEQL-ERRO

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
               MOVE 8888                TO MSG1-CD-ERRO-TS
           END-IF

           IF   MSG1-TX-ERRO-TS EQUAL SPACES
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
           GOBACK
           .
       999999-SAI.
           EXIT.
      *---------------------- FIM SGTSL061 -----------------------------