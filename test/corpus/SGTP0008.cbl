       CBL ARITH(EXTEND)
      *-----------------------------------------------------------------
      * PROGRAMA..: SGTPB071
      * ANALISTA..: F9458161 - Vanessa Tsukamoto
      * AUTOR.....: F4957150 - João P.S. Tomaz
      * DATA......: 04/10/2024
      * COMPILACAO: 54 - PSOSE600 - Cobol 6.3 c/otimizacao p/producao
      * OBJETIVO..: Carteira 200.000
      *             Gera Internalização do Cronograma e Conciliação
      *             dos pagamentos por título
      *-----------------------------------------------------------------
      * VRS003 16/12/2024 F4957150 - Ajusta busca para VNA/VNE
      * VRS003 11/12/2024 F9458161 - Altera gravacao de erros
      * VRS002 06/12/2024 F6800292 - Ajuste no envio ao GSF p/ CD-IEC
      * VRS001 04/10/2024 F4957150 - IMPLANTAÇÃO
      *-----------------------------------------------------------------

      *----------------------------------------------------------------*
       IDENTIFICATION DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID. SGTPB071.

      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
      *----------------------------------------------------------------*
       CONFIGURATION SECTION.

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT                SECTION.
      *
       FILE-CONTROL.
           SELECT SGTF071E  ASSIGN  TO  SGTF071E.
           SELECT SGTF071S  ASSIGN  TO  SGTF071S.
           SELECT SGTF136E  ASSIGN  TO  SGTF136E.

      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------
       FILE                        SECTION.
      *
      * Arquivo GSF com CRNG_EVT_ITMT x ITMT_FNCO_FDAR
       FD  SGTF071E
           RECORDING F
           RECORD 79
           BLOCK 0.
       01  REG-FD-SGTF071E        PIC X(79).
      * Arquivo BASE DB2SGT.CRNG_EVT_TIT
       FD  SGTF071S
           RECORDING F
           RECORD 114
           BLOCK 0.
       01  REG-FD-SGTF071S        PIC X(114).
      * Arquivo BASE DB2SGT.CRNG_EVT_TIT
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
           '*** SGTPB071 ***'.

       77  CTE-VERS                    PIC  X(06) VALUE 'VRS003'.

      * CHAMADOS + MOCK
       01  TAB-PROGRAMAS-CHAMADOS.
           03  SBABEND       PIC  X(08) VALUE 'SBABEND '.
           03  SBVERSAO      PIC  X(08) VALUE 'SBVERSAO'.
           03  GSFS038P      PIC  X(08) VALUE 'GSFS038P'.
           03  SGTSX005      PIC  X(08) VALUE 'SGTSX005'.
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

       01 GDA-MSG-ERRO                 PIC  X(200)    VALUE SPACES.
       01 CNT-LIDOS                    PIC S9(005)    COMP-5 VALUE 0.
       01 CNT-GRAVADOS                 PIC S9(005)    COMP-5 VALUE 0.
       01 CNT-ERRO                     PIC S9(005)    COMP-5 VALUE 0.
       01 CNT-CHAVE                    PIC S9(005)    COMP-5 VALUE 0.

       01  DIS-CODE                PIC  --------9.
       01  DIS-SINAL-CODE          PIC X(1).
       01  DIS-VALOR               PIC 9999999999,9999999.
       01  DIS-SMALL               PIC ----9.
       01  DIS-SMALL2              PIC ----9.
       01  DIS-INTEGER             PIC ---------9.

       01  CHAMADA-GSF                  PIC  X(01) VALUE SPACES.
           88 ERRO-GSF                  VALUE 'S'.

       01  SITUACAO-ITMT                PIC  X(01) VALUE SPACES.
           88 ITMT-AUSENTE              VALUE 'S'.

       01  REG-AUX.
           03  AUX-CD-ITMT-FNCO-FDAR    PIC  9(009) COMP-5.
           03  AUX-CD-GR-EVT-FDAR       PIC  9(004) COMP-5.
           03  AUX-DT-EVT-ITMT-FDAR     PIC   X(010).
           03  AUX-PC-TAXA-PGTO-EVT     PIC  S9(10)V9(7) COMP-3.
           03  AUX-CD-ITMT-FNCO         PIC   X(050).
           03  AUX-CD-TIP-PREC          PIC  9(004) COMP-5.
           03  AUX-CD-IEC               PIC  S9(004) COMP-5.
               88  AUX-VNA              VALUE 915.
           03  AUX-VL-PREC-ITMT         PIC S9(10)V9(8) COMP-3.
           03  AUX-VL-SBR-ITMT          PIC S9(10)V9(8) COMP-3.
           03  AUX-VL-EVT-ITMT          PIC S9(10)V9(8) COMP-3.


      * ------- PROGRAMAS CHAMADOS -------------------------------------
       01  GSFS038P-DADOS.
       COPY GSFK038P.
      *
       01  SGTF136E-DADOS.
       COPY SGTK136E.
*
       01  SGTSX005-DADOS.
       COPY SGTKX005.

      *------- LAYOUT ARQUIVO DE ENTRADA
       01  SGTPB71E-DADOS.
       COPY SGTKB71E.

      *------- LAYOUT ARQUIVO DE SAIDA
       01  SGTPB71S-DADOS.
       COPY SGTKB71S.


      * Controle de leitura do arquivo de entrada
       01  CTL-LET-ARQ.
           03  IN-FIM-ARQ       PIC X(01)          VALUE SPACES.
               88  FIM-ARQ                         VALUE 'S'.

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
           DISPLAY '-- INICIO DE PROCESSAMENTO - SGTPB071     '
           DISPLAY '-- Internalização do Cronograma 200.000   '
           DISPLAY '------------------------------------------'


           OPEN INPUT  SGTF071E
           OPEN OUTPUT SGTF071S
           OPEN OUTPUT SGTF136E

           PERFORM UNTIL FIM-ARQ
               INITIALIZE SGTPB71E-DADOS
               READ SGTF071E INTO SGTPB71E-DADOS
                  AT END
                     SET FIM-ARQ       TO TRUE
                  NOT AT END
      * MOCK-POINT MCKV1
                     ADD +1            TO CNT-LIDOS
                     ON SIZE ERROR
                        MOVE 'ERRO EM OPERACAO ARITMETICA 1'
                                       TO MSG-ERRO
                        PERFORM 999000-ERRO
                     END-ADD
                  IF SGTPB71E-TIP-REG EQUAL 01
      * MOCK-POINT MCKV6
                     IF SGTPB71E-CD-ITMT-FNCO-FDAR NOT EQUAL
                        AUX-CD-ITMT-FNCO-FDAR
                        PERFORM 100000-QUEBRA-CHAVE
                     END-IF
                     IF NOT ERRO-GSF
                        IF SGTPB71E-CD-GR-EVT-FDAR EQUAL 1 OR 15
                           PERFORM 200000-CALCULA-VALORES
                           PERFORM 300000-MONTA-SAIDA
                        ELSE
                           PERFORM 301000-MONTA-SAIDA-JUROS
                        END-IF
                        WRITE  REG-FD-SGTF071S   FROM SGTPB71S-DADOS
      * MOCK-POINT MCKV2
                        ADD   +1       TO CNT-GRAVADOS
                           ON SIZE ERROR
                           MOVE 'ERRO EM OPERACAO ARITMETICA 2'
                                       TO MSG-ERRO
                           PERFORM   999000-ERRO
                        END-ADD
      *              ELSE
      *                 PERFORM 400000-GRAVA-ERRO
                     END-IF
                  END-IF
               END-READ

           END-PERFORM

           PERFORM 800000-FIM
           .
       000000-SAI.
           GOBACK.
      *
      *----------------------------------------------------------------*
       100000-QUEBRA-CHAVE        SECTION.
      *----------------------------------------------------------------*
           MOVE  0002                  TO  PSC-ERRO-PGM
           INITIALIZE GSFS038P-DADOS REG-AUX CHAMADA-GSF
      * MOCK-POINT MCKV3
           ADD   +1       TO CNT-CHAVE
                           ON SIZE ERROR
                           MOVE 'ERRO EM OPERACAO ARITMETICA 3'
                                       TO MSG-ERRO
                           PERFORM   999000-ERRO
           END-ADD
      *--------- BUSCAR VNE OU VNA  -------------------------
           SET GSFK038P-CONSULTA-TIP   TO TRUE
           MOVE SGTPB71E-CD-ITMT-FNCO-FDAR
                                       TO GSFK038P-CD-ITMT-FNCO-FDAR
                                          AUX-CD-ITMT-FNCO-FDAR
           MOVE SGTPB71E-CD-IEC        TO AUX-CD-IEC
           IF AUX-VNA
              MOVE 14                  TO GSFK038P-CD-TIP-PREC-C
                                          AUX-CD-TIP-PREC
              MOVE 'S'                 TO SGTPB71S-IN-VNA
           ELSE
              MOVE 5                   TO GSFK038P-CD-TIP-PREC-C
                                          AUX-CD-TIP-PREC
              MOVE 'N'                 TO SGTPB71S-IN-VNA
           END-IF
           PERFORM 900000-CHAMAR-GSFS038P
      *-----------------------------------------------------------------
      ** Quando não encontrar o VNA OU VNE o sistema passa a consultar
      **  o P.U. da Curva (CD-TIP-PREC = 1)
           IF GSFK038P-SQLCODE EQUAL +100
              INITIALIZE GSFS038P-DADOS
              SET GSFK038P-CONSULTA-TIP
                                       TO TRUE
              MOVE SGTPB71E-CD-ITMT-FNCO-FDAR
                                       TO GSFK038P-CD-ITMT-FNCO-FDAR
              MOVE 1                   TO GSFK038P-CD-TIP-PREC-C
              PERFORM 900000-CHAMAR-GSFS038P
           END-IF
      *-----------------------------------------------------------------
           IF NOT ERRO-GSF
              MOVE GSFK038P-VL-PREC-ITMT
                                       TO AUX-VL-PREC-ITMT
                                          AUX-VL-SBR-ITMT
           END-IF
      *
           .
       100000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       200000-CALCULA-VALORES        SECTION.
      *----------------------------------------------------------------*
           MOVE  0003                  TO  PSC-ERRO-PGM
      *
           MOVE SGTPB71E-CD-GR-EVT-FDAR
                                       TO AUX-CD-GR-EVT-FDAR
           MOVE SGTPB71E-CD-ITMT-FNCO  TO AUX-CD-ITMT-FNCO
           MOVE SGTPB71E-DT-EVT-ITMT-FDAR
                                       TO AUX-DT-EVT-ITMT-FDAR
           MOVE SGTPB71E-PC-TAXA-PGTO-EVT
                                       TO AUX-PC-TAXA-PGTO-EVT
      *
      * ---- CALCULO DO VALOR DA PARCELA
      *
      * MOCK-POINT MCKV4
           COMPUTE AUX-VL-EVT-ITMT  =
                  (AUX-PC-TAXA-PGTO-EVT * AUX-VL-SBR-ITMT) / 100
               ON SIZE ERROR
               MOVE 'ERRO EM OPERACAO ARITMETICA 4'
                                       TO MSG-ERRO
               PERFORM   999000-ERRO
           END-COMPUTE
      *
      * ---- CALCULO DO SALDO AMORTIZAÇÃO CARECA
      *
      * MOCK-POINT MCKV5
           SUBTRACT AUX-VL-EVT-ITMT FROM AUX-VL-SBR-ITMT
           ON SIZE ERROR
               MOVE 'ERRO EM OPERACAO ARITMETICA 5'
                                       TO MSG-ERRO
               PERFORM   999000-ERRO
           END-SUBTRACT
           .
       200000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       300000-MONTA-SAIDA            SECTION.
      *----------------------------------------------------------------*
           MOVE  0004                  TO  PSC-ERRO-PGM

           MOVE AUX-CD-ITMT-FNCO-FDAR  TO  SGTPB71S-CD-ITMT-FNCO-FDAR
           MOVE AUX-VL-EVT-ITMT        TO  SGTPB71S-VL-PRVT-EVT
           MOVE 1                      TO  SGTPB71S-CD-EST-EVT
           MOVE 0                      TO  SGTPB71S-NR-SEQL-PGTO-EVT
           MOVE AUX-CD-ITMT-FNCO       TO  SGTPB71S-CD-ITMT-FNCO
           MOVE AUX-CD-GR-EVT-FDAR     TO  SGTPB71S-CD-GR-EVT-FDAR
           MOVE AUX-DT-EVT-ITMT-FDAR   TO  SGTPB71S-DT-EVT
           MOVE AUX-PC-TAXA-PGTO-EVT   TO  SGTPB71S-PC-TAXA-PGTO-EVT
           MOVE AUX-VL-PREC-ITMT       TO  SGTPB71S-VL-PU-REF
           MOVE AUX-VL-SBR-ITMT        TO  SGTPB71S-VL-SDO-AMTZ
           .
       300000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       301000-MONTA-SAIDA-JUROS        SECTION.
      *----------------------------------------------------------------*
           MOVE  0005                  TO  PSC-ERRO-PGM

           MOVE AUX-CD-ITMT-FNCO-FDAR  TO  SGTPB71S-CD-ITMT-FNCO-FDAR
           MOVE SGTPB71E-CD-ITMT-FNCO  TO  SGTPB71S-CD-ITMT-FNCO
           MOVE SGTPB71E-CD-GR-EVT-FDAR
                                       TO  SGTPB71S-CD-GR-EVT-FDAR
           MOVE SGTPB71E-DT-EVT-ITMT-FDAR
                                       TO  SGTPB71S-DT-EVT
           MOVE SGTPB71E-PC-TAXA-PGTO-EVT
                                       TO  SGTPB71S-PC-TAXA-PGTO-EVT
           MOVE 1                      TO  SGTPB71S-CD-EST-EVT
           MOVE 0                      TO  SGTPB71S-NR-SEQL-PGTO-EVT
                                           SGTPB71S-VL-PU-REF
                                           SGTPB71S-VL-SDO-AMTZ
                                           SGTPB71S-VL-PRVT-EVT
           .
       301000-SAI.
           EXIT.

      *
      *----------------------------------------------------------------*
       400000-GRAVA-ERRO            SECTION.
      *----------------------------------------------------------------*
           MOVE  0006                  TO  PSC-ERRO-PGM

           PERFORM 900001-CHAMAR-SGTSX005
      * MOCK-POINT MCKV9
           IF NOT ITMT-AUSENTE
              INITIALIZE SGTF136E-DADOS

              MOVE '01.01.0001'        TO SGTPB136-DT-CPR
              MOVE SGTPB71E-CD-ITMT-FNCO
                                       TO SGTPB136-CD-ITMT-FNCO
              MOVE 0                   TO SGTPB136-VL-PU-CPR
              MOVE 1                   TO SGTPB136-CD-TIP-PRCT
              MOVE 2                   TO SGTPB136-CD-EST-PRCT
              MOVE 'CURRENT'           TO SGTPB136-TS-PRCT
                                          SGTPB136-DT-PRCT
              MOVE 'SGTPB071'          TO SGTPB136-CD-PGM
              MOVE GSFS038P            TO SGTPB136-CD-USU
              MOVE 2                   TO SGTPB136-CD-OGM-ERRO
      *       MOVE GSFK038P-RTN-PGM    TO SGTPB136-TX-ERRO-PRCT
              MOVE GDA-MSG-ERRO        TO SGTPB136-TX-ERRO-PRCT

              WRITE  REG-FD-SGTF136E      FROM SGTF136E-DADOS
              ADD   +1                 TO CNT-ERRO
                              ON SIZE ERROR
                              MOVE 'ERRO EM OPERACAO ARITMETICA 6'
                                       TO MSG-ERRO
                              PERFORM   999000-ERRO
              END-ADD

           ELSE
               DISPLAY CTE-PROG ' ********** Erro GSFS038P ********* '
               MOVE    GSFK038P-CD-RTN-PGM     TO DIS-CODE
               DISPLAY CTE-PROG ' CD-RTN...: ' DIS-CODE
               DISPLAY CTE-PROG ' MSG-ERRO.: ' GDA-MSG-ERRO
               DISPLAY CTE-PROG ' CD-TIT...: ' SGTPB71E-CD-ITMT-FNCO
               MOVE    GSFK038P-CD-TIP-PREC-C  TO DIS-SMALL
               DISPLAY CTE-PROG ' TIP-PREC.: ' DIS-SMALL
           END-IF
           .
       400000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       800000-FIM                      SECTION.
      *----------------------------------------------------------------*
           MOVE  0007                  TO  PSC-ERRO-PGM

           CLOSE SGTF071E
                 SGTF071S
                 SGTF136E

           DISPLAY '*-------------------------------------------------*'
           DISPLAY '*- FIM DO PROCESSAMENTO                          -*'
           DISPLAY '*-------------------------------------------------*'
           DISPLAY ' '
           MOVE CNT-LIDOS              TO DIS-CODE
           DISPLAY ' QT REGISTROS LIDOS               : ' DIS-CODE
           MOVE CNT-CHAVE              TO DIS-CODE
           DISPLAY ' QT DE CD-ITMT-FNCO-FDAR LIDOS    : ' DIS-CODE
           MOVE CNT-GRAVADOS           TO DIS-CODE
           DISPLAY ' QT DE REGISTROS GRAVADOS         : ' DIS-CODE
           MOVE CNT-ERRO               TO DIS-CODE
      *    DISPLAY ' QT DE REGISTROS COM ERRO         : ' DIS-CODE
           DISPLAY ' QT DE CD-ITMT-FNCO-FDAR ERRO-GSF : ' DIS-CODE
           DISPLAY '888 ' CTE-PROG ' Código           : ' RETURN-CODE
           DISPLAY ' '
           DISPLAY '***************************************************'
           .
       800000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       900000-CHAMAR-GSFS038P              SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO PSC-ERRO-PGM

           MOVE LENGTH OF GSFS038P-DADOS
                                       TO EIBCALEN
           CALL GSFS038P USING DFHEIBLK GSFS038P-DADOS
      * MOCK-POINT MCKV7
           IF GSFK038P-CD-RTN-PGM NOT EQUAL ZEROS
              SET ERRO-GSF             TO TRUE
              MOVE GSFK038P-CD-ITMT-FNCO-FDAR TO DIS-INTEGER
              MOVE GSFK038P-CD-TIP-PREC-C     TO DIS-SMALL
              MOVE AUX-CD-IEC                 TO DIS-SMALL2
              EVALUATE TRUE
                  WHEN GSFK038P-SQLCODE EQUAL +100
                   AND GSFK038P-CD-TIP-PREC-C EQUAL
                       AUX-CD-TIP-PREC
                       INITIALIZE CHAMADA-GSF
                  WHEN GSFK038P-SQLCODE EQUAL +100 AND AUX-VNA
                       AND GSFK038P-CD-TIP-PREC-C EQUAL 1
                       STRING 'VNA nao encontrado no GSF'
                              '; CD-ITMT-FNCO-FDAR=' DIS-INTEGER
                              '; CD-IEC='            DIS-SMALL2
                              '; CD-TIP-PREC='       DIS-SMALL
                              '; ' GSFS038P ' PSC '
                                   GSFK038P-PSC-ERRO-PGM
                              DELIMITED BY SIZE INTO GDA-MSG-ERRO
                       END-STRING
                  WHEN GSFK038P-SQLCODE EQUAL +100 AND NOT AUX-VNA
                       AND GSFK038P-CD-TIP-PREC-C EQUAL 1
                       STRING 'VNE nao encontrado no GSF'
                              '; CD-ITMT-FNCO-FDAR=' DIS-INTEGER
                              '; CD-IEC='            DIS-SMALL2
                              '; CD-TIP-PREC='       DIS-SMALL
                              '; ' GSFS038P ' PSC '
                                   GSFK038P-PSC-ERRO-PGM
                              DELIMITED BY SIZE INTO GDA-MSG-ERRO
                       END-STRING
                  WHEN OTHER
                       MOVE GSFK038P-SQLCODE TO DIS-CODE
                       STRING 'Erro: '   GSFK038P-MSG-ERRO-PGM
                              '; RC='    GSFK038P-CD-RTN-PGM
                              '; PSC='    GSFK038P-PSC-ERRO-PGM
                              '; CD-ITMT-FNCO-FDAR=' DIS-INTEGER
                              '; CD-TIP-PREC='       DIS-SMALL
                              '; SQLCODE='           DIS-CODE
                              DELIMITED BY SIZE INTO GDA-MSG-ERRO
                       END-STRING
              END-EVALUATE
              IF ERRO-GSF
                 PERFORM 400000-GRAVA-ERRO
              END-IF
           END-IF
           .

       900000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       900001-CHAMAR-SGTSX005          SECTION.
      *-----------------------------------------------------------------
           MOVE  0008                  TO PSC-ERRO-PGM

           INITIALIZE SGTSX005-DADOS SITUACAO-ITMT
               REPLACING NUMERIC BY ZEROS ALPHANUMERIC BY SPACES

           SET  SGTSX005-CONSULTAR-OPR-POR-TIT TO TRUE
           MOVE 200000                 TO SGTSX005-CD-CTRA-TIT-VL
           MOVE SGTPB71E-CD-ITMT-FNCO  TO SGTSX005-CD-TIT-OPR-ETRD
           MOVE 1                      TO SGTSX005-NR-DVS-TIT

           MOVE LENGTH OF SGTSX005-DADOS TO EIBCALEN
           CALL SGTSX005 USING DFHEIBLK SGTSX005-DADOS
      * MOCK-POINT MCKV8
           IF SGTSX005-CD-RTN-PGM EQUAL +100
              SET ITMT-AUSENTE              TO TRUE
           END-IF
           .
      *
       900001-SAI.
           EXIT.
      *--------------------
       999000-ERRO                     SECTION.
      *--------------------
           PERFORM 999001-ERRO-001
           .
       999000-ERRO-SAI.
           EXIT.
      *-----------------
      * 999001-ERRO-001.
      *-----------------
       COPY HLPKE004.

           IF  ERRO-CODIFICADO
               PERFORM 999082-RECUPERA-MSG-CODIFICADA
           END-IF

           DISPLAY  '888'  CTE-PROG 'CANCELADO POR'
           DISPLAY  '888'  CTE-PROG  MSG-ERRO-PGM
           DISPLAY  '888'  CTE-PROG 'PROGRAMA:' PGM-ERRO
           DISPLAY HLPS0200-TX-MSG-ITNO
           DISPLAY  '888'  CTE-PROG 'CD-RTN :'  CD-RTN-PGM
           DISPLAY  '888'  CTE-PROG 'TIP-RTN :' TIP-RTN-PGM
           DISPLAY  '888'  CTE-PROG 'PSC-ERRO:' PSC-ERRO-PGM
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


      *------------------------ FIM SGTPB071 --------------------------
