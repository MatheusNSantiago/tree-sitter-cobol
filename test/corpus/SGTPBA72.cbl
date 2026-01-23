       CBL ARITH(EXTEND)
      *-----------------------------------------------------------------
      * PROGRAMA..: SGTPB075
      * ANALISTA..: F9458161 - Vanessa Tsukamoto
      * AUTOR.....: F4957150 - João P.S. Tomaz
      * DATA......: 11/10/2024
      * COMPILACAO: 54 - PSOSE600 - Cobol 6.3 c/otimizacao p/producao
      * OBJETIVO..: Carteira 200.000 atualização da posição no OPR
      *-----------------------------------------------------------------
      * VRS004 14/12/2024 F9458161 - Ajuste operacao vencida
      * VRS003 12/12/2024 F9458161 - Ajustes na atualizacao da tranche
      * VRS002 11/12/2024 F6800292 - Ajustes de registro aceito
      * VRS001 15/10/2024 F4957150 - IMPLANTAÇÃO
      *-----------------------------------------------------------------

      *----------------------------------------------------------------*
       IDENTIFICATION DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID. SGTPB075.

      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
      *----------------------------------------------------------------*
       CONFIGURATION SECTION.

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT                SECTION.
      *
       FILE-CONTROL.
           SELECT SGTF075E  ASSIGN  TO  SGTF075E.
           SELECT SGTF136E  ASSIGN  TO  SGTF136E.

      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------
       FILE                        SECTION.
      *
      *
       FD  SGTF075E
           RECORDING F
           RECORD 063
           BLOCK 0.
       01  REG-FD-SGTF075E        PIC X(063).
      *
       FD  SGTF136E
           RECORDING F
           RECORD 349
           BLOCK 0.
       01  REG-FD-SGTF136E        PIC X(349).

      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.

      *--------------------> Variaveis do Sistema <--------------------*
       77  CTE-INICIO-SS               PIC  X(35) VALUE
           '*** STORAGE SECTION COMECA AQUI ***'.

       77  CTE-PROG                    PIC  X(17) VALUE
           '*** SGTPB075 ***'.

       77  CTE-VERS                    PIC  X(06) VALUE 'VRS004'.

      * CHAMADOS + MOCK
       01  TAB-PROGRAMAS-CHAMADOS.
           03  SBABEND       PIC  X(08) VALUE 'SBABEND '.
           03  SBVERSAO      PIC  X(08) VALUE 'SBVERSAO'.
           03  SGTST004      PIC  X(08) VALUE 'SGTST004'.
           03  SGTST009      PIC  X(08) VALUE 'SGTST009'.
           03  SGTSX005      PIC  X(08) VALUE 'SGTSX005'.
           03  SGTS001B      PIC  X(08) VALUE 'SGTS001B'.
           03  DB2S0001      PIC  X(08) VALUE 'DB2S0001'.
      *
       COPY DB2K0001.
      *
       COPY ALMKCBL1.

      *----------------------------------------------------------------*
      *           Book do programa de ambiente - CICS5000              *
      *       Definido na work, para preservar as informacoes          *
      *----------------------------------------------------------------*
       LOCAL-STORAGE SECTION.
       COPY HLPKE001.

      *--> HLPKDFHE - Definicao da area DFHEIBLK
       COPY HLPKDFHE.

       COPY HLPKE01B.

       01 CNT-LIDOS                    PIC S9(009)    COMP-5 VALUE 0.
       01 CNT-GRAVADOS                 PIC S9(009)    COMP-5 VALUE 0.
       01 CNT-ERRO                     PIC S9(009)    COMP-5 VALUE 0.
      *
       01  DIS-CODE                    PIC --------9.
       01  DIS-SMALL                   PIC ----9.
      *
       01  CTL-ERRO                    PIC  X(01) VALUE SPACES.
           88 ERRO-REGISTRO                  VALUE 'S'.

       01  GDA-CAMPO-ERRO              PIC  X(16) VALUE SPACES.
      *
       01  AUX-CD-OGM-ERRO             PIC S9(4) COMP-5 VALUE 0.
           88 ERRO-CAC                       VALUE 1.
           88 ERRO-GSF                       VALUE 2.
           88 ERRO-PRD                       VALUE 3.
           88 ERRO-SGT                       VALUE 4.
           88 ERRO-RSM                       VALUE 5.
           88 ERRO-MST                       VALUE 6.
      *
      * ------- PROGRAMAS CHAMADOS -------------------------------------
       01  SGTSX005-DADOS.
       COPY SGTKX005.
      *
       01  SGTST009-DADOS.
       COPY SGTKT009.
      *
       01  SGTS001B-DADOS.
       COPY SGTK001B.
      *
       01  SGTST004-DADOS.
       COPY SGTKT004.
      *
      *------- LAYOUT ARQUIVO DE ERROS
       01  SGTF136E-DADOS.
       COPY SGTK136E.
      *
      *------- LAYOUT ARQUIVO DE ENTRADA
       01  SGTPB75E-DADOS.
      *
      *
       COPY SGTKB75E.
      *
      * Controle de leitura do arquivo de entrada
       01  CTL-LET-ARQ.
           03  IN-FIM-ARQ       PIC X(01)          VALUE SPACES.
               88  FIM-ARQ                         VALUE 'S'.
      *
      *
           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.
      *
      *------------------- Variaveis do Sistema -----------------------*
       01  CTE-SAI-SS                  PIC  X(40) VALUE
           '*** Storage Section Termina Aqui ***'.

       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
       000000-PRINCIPAL                SECTION.
      *----------------------------------------------------------------*
      * BOOK MOCK BATCH
       COPY ALMKCBL2.

           MOVE  0001                  TO  PSC-ERRO-PGM

           CALL  SBVERSAO  USING  CTE-PROG  CTE-VERS


           DISPLAY '------------------------------------------'
           DISPLAY '-- INICIO DE PROCESSAMENTO - SGTPB075     '
           DISPLAY '------------------------------------------'


           OPEN INPUT  SGTF075E
           OPEN OUTPUT SGTF136E

           PERFORM UNTIL FIM-ARQ
               INITIALIZE SGTPB75E-DADOS CTL-ERRO
               READ SGTF075E INTO SGTPB75E-DADOS
                  AT END
                     SET FIM-ARQ       TO TRUE
                  NOT AT END
                  ADD +1            TO CNT-LIDOS
                     ON SIZE ERROR
                     MOVE 'ERRO EM OPERACAO ARITMETICA 1'
                                    TO MSG-ERRO
                     PERFORM 999000-ERRO
                  END-ADD
                  PERFORM 100000-VALIDA-ENTRADA
                  PERFORM 400000-ATUALIZA-OPR
                  PERFORM 300000-ATUALIZA-TRANCHE
                  PERFORM 500000-ATUALIZA-ENVIO
                  PERFORM 200000-ATUALIZA-ESTADO
                  PERFORM 980000-CONTROLA-COMMIT
                  ADD +1            TO CNT-GRAVADOS
                     ON SIZE ERROR
                     MOVE 'ERRO EM OPERACAO ARITMETICA 2'
                                       TO MSG-ERRO
                     PERFORM 999000-ERRO
                  END-ADD
               END-READ

           END-PERFORM

           PERFORM 800000-FIM
           .
      *
       000000-SAI.
           GOBACK.
      *
      *----------------------------------------------------------------*
       100000-VALIDA-ENTRADA           SECTION.
      *----------------------------------------------------------------*
           MOVE  0002                  TO  PSC-ERRO-PGM
      *
           IF SGTPB75E-CD-CTRA-TIT-VL  EQUAL ZEROS
              SET  ERRO-REGISTRO       TO TRUE
              MOVE 0014                TO PSC-ERRO-PGM
              MOVE 'CD-CTRA-TIT-VL' TO GDA-CAMPO-ERRO
           END-IF
           IF SGTPB75E-DT-PSC          EQUAL SPACES
              SET  ERRO-REGISTRO       TO TRUE
              MOVE 0015                TO PSC-ERRO-PGM
              MOVE 'DT-PSC'            TO GDA-CAMPO-ERRO
           END-IF
           IF SGTPB75E-CD-TIT-OPR-ETRD EQUAL SPACES
              SET  ERRO-REGISTRO       TO TRUE
              MOVE 0016                TO PSC-ERRO-PGM
              MOVE 'CD-TIT-OPR-ETRD'   TO GDA-CAMPO-ERRO
           END-IF
           IF SGTPB75E-NR-DVS-TIT      EQUAL ZEROS
              SET  ERRO-REGISTRO       TO TRUE
              MOVE 0017                TO PSC-ERRO-PGM
              MOVE 'NR-DVS-TIT'        TO GDA-CAMPO-ERRO
           END-IF
           IF SGTPB75E-NR-SEQL-PSC-DRIA EQUAL ZEROS
              SET  ERRO-REGISTRO       TO TRUE
              MOVE 0018                TO PSC-ERRO-PGM
              MOVE 'NR-SEQL-PSC-DRIA'  TO GDA-CAMPO-ERRO
           END-IF
           IF SGTPB75E-CD-OPR-ETRD-MCP EQUAL ZEROS
              SET  ERRO-REGISTRO       TO TRUE
              MOVE 0019                TO PSC-ERRO-PGM
              MOVE 'CD-OPR-ETRD-MCP'   TO GDA-CAMPO-ERRO
           END-IF
           IF SGTPB75E-NR-VRS-OPR-ETRD EQUAL ZEROS
              SET  ERRO-REGISTRO       TO TRUE
              MOVE 0020                TO PSC-ERRO-PGM
              MOVE 'NR-VRS-OPR-ETRD'   TO GDA-CAMPO-ERRO
           END-IF
            IF  SGTPB75E-VL-PU-TIT-DT-PSC EQUAL ZEROS
            AND SGTPB75E-QT-PAPL-TIT-DT-PSC  >  ZEROS
              SET  ERRO-REGISTRO       TO TRUE
              MOVE 0021                TO PSC-ERRO-PGM
              STRING 'Valor do PU da posicao diaria zerado'
                     '; PSC=' PSC-ERRO-PGM
                     DELIMITED BY SIZE INTO MSG-ERRO
              END-STRING
           END-IF
      *
      * MOCK-POINT MCKV1
           IF ERRO-REGISTRO
              SET  ERRO-SGT            TO TRUE
              MOVE 888                 TO CD-RTN-PGM
              MOVE 'SGTPB075'          TO PGM-ERRO
              IF  MSG-ERRO EQUAL SPACES
                  STRING PSC-ERRO-PGM '-'
                         'Erro na validacao de campo de entrada: '
                         GDA-CAMPO-ERRO
                         DELIMITED BY SIZE INTO MSG-ERRO
                  END-STRING
              END-IF
              PERFORM 600000-GRAVA-ERRO
           END-IF

           IF SGTPB75E-QT-PAPL-TIT-DT-PSC EQUAL ZEROS
              SET VENCIDO              TO TRUE
           END-IF
           .
      *
       100000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       200000-ATUALIZA-ESTADO          SECTION.
      *----------------------------------------------------------------*
           MOVE  0003                  TO  PSC-ERRO-PGM

           IF  NOT ERRO-REGISTRO
              INITIALIZE SGTST009-DADOS
              SET  SGTST009-INCLUIR         TO TRUE
              MOVE SGTPB75E-CD-CTRA-TIT-VL  TO SGTST009-CD-CTRA-TIT-VL
              MOVE SGTPB75E-CD-OPR-ETRD-MCP TO SGTST009-CD-OPR-ETRD-MCP
              MOVE SGTPB75E-NR-VRS-OPR-ETRD TO SGTST009-NR-VRS-OPR-ETRD
              IF  VENCIDO
                  MOVE 6                    TO SGTST009-CD-TIP-EST-OPR
              ELSE
                  MOVE 5                    TO SGTST009-CD-TIP-EST-OPR
              END-IF
              MOVE 'CURRENT'                TO SGTST009-TS-EST-OPR-ETRD
              MOVE 'SGTPB075'               TO SGTST009-CD-USU-RSP-EST
                                               SGTST009-CD-USU-RSP-ATL
              PERFORM 910000-CHAMAR-SGTST009
           END-IF
           .
      *
       200000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       300000-ATUALIZA-TRANCHE         SECTION.
      *----------------------------------------------------------------*
           MOVE  0004                  TO  PSC-ERRO-PGM

           IF  NOT ERRO-REGISTRO
      *--      consulta operacao no sgt
               INITIALIZE SGTSX005-DADOS
               SET  SGTSX005-CONSULTAR TO TRUE
               MOVE SGTPB75E-CD-CTRA-TIT-VL
                                       TO SGTSX005-CD-CTRA-TIT-VL
               MOVE SGTPB75E-CD-OPR-ETRD-MCP
                                       TO SGTSX005-CD-OPR-ETRD-MCP
               MOVE SGTPB75E-NR-VRS-OPR-ETRD
                                       TO SGTSX005-NR-VRS-OPR-ETRD
               MOVE 'SGTPB075'         TO SGTSX005-CD-CHV-USU
               PERFORM 900000-CHAMAR-SGTSX005

      *        IF  VENCIDO AND SGTPB75E-DT-EVT NOT EQUAL SPACES
      *            INITIALIZE SGTSX005-RTN-PGM
      *            SET  SGTSX005-ALTERAR
      *                                TO TRUE
      *            MOVE SGTPB75E-DT-EVT
      *                                TO SGTSX005-DT-LQDC-OPR-DRIV
      *            MOVE 'SGTPB075'
      *                                TO SGTSX005-CD-USU-ULT-ATL
      *            MOVE 'CURRENT'      TO SGTSX005-TS-ULT-ATL
      *            PERFORM 900000-CHAMAR-SGTSX005
      *        END-IF
           END-IF
           .
      *
       300000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       400000-ATUALIZA-OPR               SECTION.
      *----------------------------------------------------------------*
           MOVE  0005                  TO  PSC-ERRO-PGM

           IF  NOT ERRO-REGISTRO
               INITIALIZE SGTS001B-DADOS
               MOVE SGTPB75E-CD-CTRA-TIT-VL
                                       TO SGTS001B-CD-CTRA-TIT-VL
               MOVE SGTPB75E-CD-OPR-ETRD-MCP
                                       TO SGTS001B-CD-OPR-ETRD-MCP
               MOVE SGTPB75E-NR-VRS-OPR-ETRD
                                       TO SGTS001B-NR-VRS-OPR-ETRD
               MOVE SGTPB75E-DT-PSC
                                       TO SGTS001B-DT-PSC
               MOVE SGTPB75E-QT-PAPL-TIT-DT-PSC
                                       TO SGTS001B-QT-PAPL-TIT-DT-PSC
               MOVE SGTPB75E-VL-PU-TIT-DT-PSC
                                       TO SGTS001B-VL-PU-TIT-DT-PSC
               PERFORM 920000-CHAMAR-SGTS001B
           END-IF
           .
      *
       400000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       500000-ATUALIZA-ENVIO           SECTION.
      *----------------------------------------------------------------*
           MOVE  0006                  TO  PSC-ERRO-PGM

           IF  NOT ERRO-REGISTRO
               INITIALIZE SGTST004-DADOS
               SET  SGTST004-CONSULTAR     TO TRUE
               MOVE 'SGTPB075'             TO SGTST004-CD-CHV-USU
               MOVE SGTPB75E-CD-CTRA-TIT-VL
                                           TO SGTST004-CD-CTRA-TIT-VL
               MOVE SGTPB75E-DT-PSC        TO SGTST004-DT-PSC
               MOVE SGTPB75E-CD-TIT-OPR-ETRD
                                           TO SGTST004-CD-TIT-OPR-ETRD
               MOVE SGTPB75E-NR-DVS-TIT    TO SGTST004-NR-DVS-TIT
               MOVE SGTPB75E-NR-SEQL-PSC-DRIA
                                           TO SGTST004-NR-SEQL-PSC-DRIA
               PERFORM 930000-CHAMAR-SGTST004
      *
               INITIALIZE SGTST004-RTN-PGM
               SET  SGTST004-ALTERAR       TO TRUE
               MOVE 'CURRENT'              TO SGTST004-TS-ULT-ATL
                                              SGTST004-TS-ENVO-PSC
               MOVE 'SGTPB075'             TO SGTST004-CD-USU-ENVO-PSC
                                              SGTST004-CD-USU-RSP-ATL
               MOVE 2                      TO SGTST004-CD-EST-RSTD-ITE
               MOVE SGTS001B-LST-ERR-OPR   TO SGTST004-TX-RTN-PRCT-OPR


               PERFORM 930000-CHAMAR-SGTST004
           END-IF
           .
      *
       500000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       600000-GRAVA-ERRO            SECTION.
      *----------------------------------------------------------------*
           MOVE  0007                  TO  PSC-ERRO-PGM

           SET  ERRO-REGISTRO          TO TRUE
      *
           PERFORM 999003-ROLLBACK
      *
           MOVE SGTSX005-DT-CPR-OPR
                                       TO SGTPB136-DT-CPR
           MOVE SGTPB75E-CD-TIT-OPR-ETRD
                                       TO SGTPB136-CD-ITMT-FNCO
           MOVE SGTSX005-VL-PU-CPR-TIT-OPR
                                       TO SGTPB136-VL-PU-CPR
           MOVE 1                      TO SGTPB136-CD-TIP-PRCT
           MOVE 2                      TO SGTPB136-CD-EST-PRCT
           MOVE 'CURRENT'              TO SGTPB136-TS-PRCT
                                          SGTPB136-DT-PRCT
           MOVE 'SGTPB075'             TO SGTPB136-CD-PGM
           MOVE SGTPB75E-CD-CTRA-TIT-VL
                                       TO SGTPB136-CD-CTRA-TIT-VL
           MOVE SGTPB75E-CD-OPR-ETRD-MCP
                                       TO SGTPB136-CD-OPR-ETRD-MCP
           MOVE SGTPB75E-NR-VRS-OPR-ETRD
                                       TO SGTPB136-NR-VRS-OPR-ETRD
           MOVE 'SGTPB075'             TO SGTPB136-CD-USU
           MOVE AUX-CD-OGM-ERRO        TO SGTPB136-CD-OGM-ERRO
           MOVE MSG-ERRO               TO SGTPB136-TX-ERRO-PRCT
      *
           WRITE REG-FD-SGTF136E FROM SGTF136E-DADOS
      *
           ADD   +1       TO CNT-ERRO
                           ON SIZE ERROR
                           MOVE 'ERRO EM OPERACAO ARITMETICA 3'
                                       TO MSG-ERRO
                           PERFORM   999000-ERRO
           END-ADD
           .
      *
       600000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       800000-FIM                      SECTION.
      *----------------------------------------------------------------*
           MOVE  0008                  TO  PSC-ERRO-PGM

           CLOSE SGTF075E
                 SGTF136E
      *
           PERFORM 980000-CONTROLA-COMMIT
      *
           DISPLAY '*-------------------------------------------------*'
           DISPLAY '*- FIM DO PROCESSAMENTO                          -*'
           DISPLAY '*-------------------------------------------------*'
           DISPLAY ' '
           MOVE CNT-LIDOS              TO DIS-CODE
           DISPLAY ' QT REGISTROS LIDOS               : ' DIS-CODE
           MOVE CNT-GRAVADOS           TO DIS-CODE
           DISPLAY ' QT DE REGISTROS GRAVADOS         : ' DIS-CODE
           MOVE CNT-ERRO               TO DIS-CODE
           DISPLAY ' QT DE REGISTROS COM ERRO         : ' DIS-CODE
           DISPLAY '888 ' CTE-PROG ' Código           : ' RETURN-CODE
           DISPLAY ' '
           DISPLAY '***************************************************'
           .
      *
       800000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       900000-CHAMAR-SGTSX005          SECTION.
      *-----------------------------------------------------------------
           MOVE 0009                   TO PSC-ERRO-PGM

           MOVE LENGTH OF SGTSX005-DADOS
                                       TO EIBCALEN
           CALL SGTSX005 USING DFHEIBLK SGTSX005-DADOS
      * MOCK-POINT MCKV2
           IF SGTSX005-CD-RTN-PGM NOT EQUAL ZEROS
              SET  ERRO-SGT            TO TRUE
              MOVE SGTSX005-RTN-PGM    TO RTN-PGM
              MOVE SGTSX005-CD-RTN-PGM TO DIS-SMALL
              STRING 'Erro pgm ' SGTSX005
                     '; RC='  DIS-SMALL
                     '; PSC=' SGTSX005-PSC-ERRO-PGM
                     '; '     SGTSX005-MSG-ERRO
                     DELIMITED BY SIZE INTO MSG-ERRO
              END-STRING
              PERFORM 600000-GRAVA-ERRO
           END-IF
           .
      *
       900000-SAI.
           EXIT.
      *
      *-----------------------------------------------------------------
       910000-CHAMAR-SGTST009          SECTION.
      *-----------------------------------------------------------------
           MOVE 0010                   TO PSC-ERRO-PGM

      *
           MOVE LENGTH OF SGTST009-DADOS
                                       TO EIBCALEN
           CALL SGTST009 USING DFHEIBLK SGTST009-DADOS
      * MOCK-POINT MCKV3
           IF SGTST009-CD-RTN-PGM NOT EQUAL ZEROS
              SET ERRO-SGT             TO TRUE
              MOVE SGTST009-RTN-PGM    TO RTN-PGM
              MOVE SGTST009-CD-RTN-PGM TO DIS-SMALL
              STRING 'Erro pgm ' SGTST009
                     '; RC='  DIS-SMALL
                     '; PSC=' SGTST009-PSC-ERRO-PGM
                     '; '     SGTST009-MSG-ERRO
                     DELIMITED BY SIZE INTO MSG-ERRO
              END-STRING
              PERFORM 600000-GRAVA-ERRO
           END-IF
           .
      *
       910000-SAI.
           EXIT.
      *
      *-----------------------------------------------------------------
       920000-CHAMAR-SGTS001B          SECTION.
      *-----------------------------------------------------------------
           MOVE 0011                   TO PSC-ERRO-PGM

           MOVE LENGTH OF SGTS001B-DADOS
                                       TO EIBCALEN
           CALL SGTS001B USING DFHEIBLK SGTS001B-DADOS
      * MOCK-POINT MCKV4
           IF SGTS001B-CD-RTN-PGM NOT EQUAL ZEROS
              SET  ERRO-SGT            TO TRUE
              MOVE SGTS001B-RTN-PGM    TO RTN-PGM
              MOVE SGTS001B-CD-RTN-PGM TO DIS-SMALL
              STRING 'Erro SGTS001B'
                     '; RC='  DIS-SMALL
                     '; '     SGTS001B-MSG-ERRO
                     DELIMITED BY SIZE INTO MSG-ERRO
              END-STRING
              PERFORM 600000-GRAVA-ERRO
           END-IF
           .
      *
       920000-SAI.
           EXIT.
      *
      *-----------------------------------------------------------------
       930000-CHAMAR-SGTST004          SECTION.
      *-----------------------------------------------------------------
           MOVE 0012                   TO PSC-ERRO-PGM
      *
           MOVE LENGTH OF SGTST004-DADOS
                                       TO EIBCALEN
           CALL SGTST004 USING DFHEIBLK SGTST004-DADOS
      * MOCK-POINT MCKV5
           IF SGTST004-CD-RTN-PGM NOT EQUAL ZEROS
              SET  ERRO-SGT            TO TRUE
              MOVE SGTST004-RTN-PGM    TO RTN-PGM
              MOVE SGTST004-CD-RTN-PGM TO DIS-SMALL
              STRING 'Erro pgm ' SGTST004
                     '; RC='  DIS-SMALL
                     '; PSC=' SGTST004-PSC-ERRO-PGM
                     '; '     SGTST004-MSG-ERRO
                     DELIMITED BY SIZE INTO MSG-ERRO
              END-STRING
              PERFORM 600000-GRAVA-ERRO
           END-IF
           .
      *
       930000-SAI.
           EXIT.
      *
      *---------------------------------------*
       980000-CONTROLA-COMMIT          SECTION.
      *---------------------------------------*
           MOVE 0013                   TO PSC-ERRO-PGM
      *
           CALL DB2S0001 USING DB2S0001-LINK.
           IF  NOT DB2S0001-FIM-NORMAL
               PERFORM 980001-ERRO-COMMIT
           END-IF
           .
       980000-FIM.
           EXIT.
      *----------------------------------------------------------------*
       980001-ERRO-COMMIT     SECTION.
      *----------------------------------------------------------------*
      *

           DISPLAY '888 ' CTE-PROG ' 006 - ERRO CHAMADA DB2S0001 --- '
                                            UPON SYSOUT.
           DISPLAY '888 ' CTE-PROG ' 006 - ERRO DB2S0001             '
                                            UPON SYSOUT.
           DISPLAY '888 ' CTE-PROG ' 006 - DB2S0001-RETORNO        = '
                   DB2S0001-RETORNO         UPON SYSOUT.
           DISPLAY '888 ' CTE-PROG ' 006 - DB2S0001-SQLCODE        = '
                   DB2S0001-SQLCODE         UPON SYSOUT.
           DISPLAY '888 ' CTE-PROG ' 006 - DB2S0001-MENSAGEM       = '
                   DB2S0001-MENSAGEM        UPON SYSOUT.

           CALL SBABEND

           .
       9800001-SAI.
           EXIT.
      *
      *--------------------
       999000-ERRO                     SECTION.
      *--------------------
           EXEC SQL
                ROLLBACK
           END-EXEC
      *--------------------
           PERFORM 999001-ERRO-001
           .
       999000-ERRO-SAI.
           EXIT.
      *-----------------
      * 999001-ERRO-001.
      *-----------------
       COPY HLPKE004.

           IF  ERRO-CODIFICADO IN TIP-RTN-PGM
               PERFORM 999082-RECUPERA-MSG-CODIFICADA
           END-IF

           DISPLAY  '888'  CTE-PROG 'CANCELADO POR'
           DISPLAY  '888'  CTE-PROG  MSG-ERRO-PGM
           DISPLAY  '888'  CTE-PROG 'PROGRAMA:' PGM-ERRO
           DISPLAY HLPS0200-TX-MSG-ITNO
           DISPLAY  '888'  CTE-PROG 'CD-RTN :'  CD-RTN-PGM
           DISPLAY  '888'  CTE-PROG 'TIP-RTN :' TIP-RTN-PGM
           DISPLAY  '888'  CTE-PROG 'PSC-ERRO:' PSC-ERRO-PGM
           MOVE CNT-LIDOS              TO DIS-CODE
           DISPLAY ' QT REGISTROS LIDOS               : ' DIS-CODE
           MOVE CNT-GRAVADOS           TO DIS-CODE
           DISPLAY ' QT DE REGISTROS GRAVADOS         : ' DIS-CODE
           MOVE CNT-ERRO               TO DIS-CODE
           DISPLAY ' QT DE REGISTROS COM ERRO         : ' DIS-CODE
           DISPLAY  '888'  CTE-PROG 'MAIS INFORMACOES VIDE DUMP FORMATA
      -        'DO.'

           CALL SBABEND

           .
      *-----------------
      * 999002-ERRO-DB2
      *-----------------
      *=INC HLPKE005
      *     .
       999009-SAI.
           EXIT.

      *----------------------------------------------------------------*
       999003-ROLLBACK                SECTION.
      *----------------------------------------------------------------*
           EXEC SQL
                ROLLBACK
           END-EXEC
           .
       999003-SAI.
           EXIT.
      *------------------------ FIM SGTPB075 --------------------------
