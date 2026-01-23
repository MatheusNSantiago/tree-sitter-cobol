       CBL ARITH(EXTEND)
      *-----------------------------------------------------------------
      * PROGRAMA..: SGTPB099
      * ANALISTA..: F9458161 - Vanessa Tsukamoto
      * AUTOR.....: F9458161 - Vanessa Tsukamoto
      * DATA......: 12.10.2024
      * COMPILACAO: 54 - PSOSE600 - Cobol 6.3 c/otimizacao p/producao
      * OBJETIVO..: Selecionar dados de uma remessa
      *-----------------------------------------------------------------
      * VRS001 12.10.2024 F9458161 - IMPLANTACAO
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID. SGTPB099.
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
      *----------------------------------------------------------------*
       CONFIGURATION SECTION.

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT                SECTION.
      *
       FILE-CONTROL.
           SELECT SGTF099H  ASSIGN  TO  SGTF099H.
           SELECT SGTF099E  ASSIGN  TO  SGTF099E.
           SELECT SGTF099S  ASSIGN  TO  SGTF099S.

      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------
       FILE                        SECTION.
      *
       FD  SGTF099H
           RECORDING F
           RECORD 500
           BLOCK 0.
       01  REG-FD-SGTF099H        PIC X(500).

      * Arquivo GSF com CRNG_EVT_ITMT x ITMT_FNCO_FDAR
       FD  SGTF099E
           RECORDING F
           RECORD 500
           BLOCK 0.
       01  REG-FD-SGTF099E        PIC X(500).

      * Arquivo BASE DB2SGT.CRNG_EVT_TIT
       FD  SGTF099S
           RECORDING F
           RECORD 500
           BLOCK 0.
       01  REG-FD-SGTF099S        PIC X(500).

      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
       77  CTE-INICIO-SS               PIC  X(35) VALUE
           '*** STORAGE SECTION COMECA AQUI ***'.

       77  CTE-PROG                    PIC  X(17) VALUE
           '*** SGTPB099 ***'.

       77  CTE-VERS                    PIC  X(06) VALUE 'VRS001'.

       01  TAB-PROGRAMAS-CHAMADOS.
           03  SBABEND       PIC  X(08) VALUE 'SBABEND '.
           03  SBVERSAO      PIC  X(08) VALUE 'SBVERSAO'.

-INC ALMKCBL1

      *----------------------------------------------------------------*
       LOCAL-STORAGE SECTION.
      *----------------------------------------------------------------*
-INC HLPKE001

      *--> HLPKDFHE - Definicao da area DFHEIBLK
-INC HLPKDFHE

-INC HLPKE01B

       01  CNT-LIDOS-099H                   PIC S9(5) COMP-5 VALUE 0.
       01  CNT-LIDOS-099E                   PIC S9(5) COMP-5 VALUE 0.
       01  CNT-GRAVADOS                     PIC S9(5) COMP-5 VALUE 0.

       01  DIS-CODE                         PIC  --------9.

       01  IN-RMS-VLDA                      PIC X(01) VALUE SPACES.
           88 RMS-VLDA                                VALUE 'S'.

       01  GDA-HDR                          PIC  X(500) VALUE SPACES.
       01  GDA-HDR-R REDEFINES GDA-HDR.
           03 GDA-TIP-REG-HDR               PIC  9(002).
           03 GDA-NM-ARQ                    PIC  X(008).
           03 GDA-TS-ARQ                    PIC  X(026).
           03 FILLER                        PIC  X(464).

      *------- LAYOUT ARQUIVO DE ENTRADA SGTF099H
       01  F099H-REG-GERAL                  PIC  X(500) VALUE SPACES.
       01  F099H-HEADER REDEFINES F099H-REG-GERAL.
           03 F099H-TIP-REG-HDR             PIC  9(002).
           03 F099H-NM-ARQ                  PIC  X(008).
           03 F099H-TS-ARQ                  PIC  X(026).
           03 FILLER                        PIC  X(464).

      *------- LAYOUT ARQUIVO DE ENTRADA SGTF099E
       01  F099E-REG-GERAL                  PIC  X(500) VALUE SPACES.
       01  F099E-REG-GERAL-R REDEFINES F099E-REG-GERAL.
           03 F099E-TIP-REG                 PIC  9(002).
              88 F099E-TIP-REG-HDR          VALUE 0.
              88 F099E-TIP-REG-DET          VALUE 1.
              88 F099E-TIP-REG-TRL          VALUE 2.
           03 F099E-DADOS                   PIC  X(498).

      *------- LAYOUT ARQUIVO DE SAIDA
       01  F099S-REG-GERAL                  PIC  X(500) VALUE SPACES.



      * Controle de leitura dos arquivos de entrada
       01  CTL-LET-ARQ-099H.
           03  IN-FIM-ARQ-099H              PIC X(01) VALUE SPACES.
               88  FIM-ARQ-099H                       VALUE 'S'.

       01  CTL-LET-ARQ-099E.
           03  IN-FIM-ARQ-099E              PIC X(01) VALUE SPACES.
               88  FIM-ARQ-099E                       VALUE 'S'.

      *------------------- Variaveis do Sistema -----------------------*
       01  CTE-SAI-SS                  PIC  X(40) VALUE
           '*** Storage Section Termina Aqui ***'.

      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
       000000-PRINCIPAL SECTION.
      *----------------------------------------------------------------*
      * BOOK MOCK
-INC ALMKCBL2

           MOVE 0001                   TO PSC-ERRO-PGM

           CALL  SBVERSAO  USING  CTE-PROG  CTE-VERS

           DISPLAY '------------------------------------------'
           DISPLAY '-- INICIO DE PROCESSAMENTO - SGTPB099     '
           DISPLAY '------------------------------------------'

           PERFORM 100000-INICIA
           PERFORM 200000-PROCESSA UNTIL FIM-ARQ-099E
           PERFORM 800000-FIM
           .
       000000-SAI.
           GOBACK.
      *
      *----------------------------------------------------------------*
       100000-INICIA SECTION.
      *----------------------------------------------------------------*
           MOVE 0002                   TO PSC-ERRO-PGM

           OPEN INPUT  SGTF099H
                       SGTF099E
           OPEN OUTPUT SGTF099S

           PERFORM 971000-LE-SGTF099H

           IF  FIM-ARQ-099H
               PERFORM 999001-ERRO-ARQ-099H-VAZIO
           END-IF

           IF  F099H-TIP-REG-HDR NOT EQUAL ZEROS
               MOVE 'Erro TIP-REG HEADER invalido' TO MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           PERFORM 970000-LE-SGTF099E

           IF  FIM-ARQ-099E
               PERFORM 999001-ERRO-ARQ-099E-VAZIO
           END-IF
           .
       100000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       200000-PROCESSA SECTION.
      *----------------------------------------------------------------*
           MOVE 0003                   TO PSC-ERRO-PGM

           EVALUATE F099E-TIP-REG
               WHEN 0
                    PERFORM 200001-TRATA-HEADER

               WHEN 1
                    IF  RMS-VLDA
                        PERFORM 300000-MONTA-SAIDA
                    END-IF

               WHEN 2
                    IF  RMS-VLDA
                        SET FIM-ARQ-099E TO TRUE
                    END-IF

               WHEN OTHER
                    MOVE 'Erro F099E-TIP-REG invalido' TO MSG-ERRO
                    PERFORM 999001-ERRO-001
           END-EVALUATE

           IF  NOT FIM-ARQ-099E
               PERFORM 970000-LE-SGTF099E
           END-IF
           .
       200000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       200001-TRATA-HEADER SECTION.
      *----------------------------------------------------------------*
           MOVE 0004                   TO PSC-ERRO-PGM

           INITIALIZE GDA-HDR
           MOVE F099E-REG-GERAL        TO GDA-HDR

           IF  GDA-TS-ARQ EQUAL F099H-TS-ARQ
               SET RMS-VLDA            TO TRUE
           END-IF
           .
       200001-SAI.
           EXIT.
      *----------------------------------------------------------------*
       300000-MONTA-SAIDA SECTION.
      *----------------------------------------------------------------*
           MOVE 0005                   TO PSC-ERRO-PGM

           INITIALIZE F099S-REG-GERAL
           MOVE F099E-REG-GERAL        TO F099S-REG-GERAL

           WRITE REG-FD-SGTF099S       FROM F099S-REG-GERAL
           ADD   +1                    TO CNT-GRAVADOS
                 ON SIZE ERROR
                    MOVE 'ERRO EM OPERACAO ARITMETICA 1' TO MSG-ERRO
                    PERFORM 999001-ERRO-001
           END-ADD
           .
       300000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       800000-FIM SECTION.
      *----------------------------------------------------------------*
           MOVE 0006                   TO PSC-ERRO-PGM

           CLOSE SGTF099H
                 SGTF099E
                 SGTF099S

           DISPLAY '*-------------------------------------------------*'
           DISPLAY '*- FIM DO PROCESSAMENTO                          -*'
           DISPLAY '*-------------------------------------------------*'
           DISPLAY ' '
           MOVE CNT-LIDOS-099H          TO DIS-CODE
           DISPLAY ' QT REGISTROS LIDOS  F099H        : ' DIS-CODE
           MOVE CNT-LIDOS-099E          TO DIS-CODE
           DISPLAY ' QT REGISTROS LIDOS  F099E        : ' DIS-CODE
           MOVE CNT-GRAVADOS           TO DIS-CODE
           DISPLAY ' QT DE REGISTROS GRAVADOS         : ' DIS-CODE
           DISPLAY '888 ' CTE-PROG ' Código           : ' RETURN-CODE
           DISPLAY ' '
           DISPLAY '***************************************************'
           .
       800000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970000-LE-SGTF099E SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO PSC-ERRO-PGM

           INITIALIZE F099E-REG-GERAL

           READ SGTF099E INTO F099E-REG-GERAL
                AT END
                    SET FIM-ARQ-099E    TO TRUE
                NOT AT END
                    ADD 1              TO CNT-LIDOS-099E
           END-READ
           .
       970000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       971000-LE-SGTF099H SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO PSC-ERRO-PGM

           INITIALIZE F099H-REG-GERAL

           READ SGTF099H INTO F099H-REG-GERAL
                AT END
                    SET FIM-ARQ-099H    TO TRUE
                NOT AT END
                    ADD 1              TO CNT-LIDOS-099H
           END-READ
           .
       971000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       999001-ERRO-ARQ-099H-VAZIO SECTION.
      *-----------------------------------------------------------------
           DISPLAY '888 ' CTE-PROG ' 888 - **********************'
           DISPLAY '888 ' CTE-PROG ' 888 - ARQUIVO SGTF099H VAZIO'
           DISPLAY '888 ' CTE-PROG ' 888 - **********************'

           MOVE 2 TO RETURN-CODE
           STOP RUN
           .
      *-----------------------------------------------------------------
       999001-ERRO-ARQ-099E-VAZIO SECTION.
      *-----------------------------------------------------------------
           DISPLAY '888 ' CTE-PROG ' 888 - **********************'
           DISPLAY '888 ' CTE-PROG ' 888 - ARQUIVO SGTF099E VAZIO'
           DISPLAY '888 ' CTE-PROG ' 888 - **********************'

           MOVE 4 TO RETURN-CODE
           STOP RUN
           .
      *-----------------------------------------------------------------
       999001-ERRO-001 SECTION.
      *-----------------------------------------------------------------
           DISPLAY  '888'  CTE-PROG '************* ERRO **************'
           DISPLAY  '888'  CTE-PROG 'CANCELADO POR'
           DISPLAY  '888'  CTE-PROG  MSG-ERRO
           DISPLAY  '888'  CTE-PROG 'PSC-ERRO:' PSC-ERRO-PGM
           MOVE   CNT-LIDOS-099E     TO DIS-CODE
           DISPLAY  '888'  CTE-PROG 'QT REG LIDOS F099E: ' DIS-CODE
           DISPLAY  '888'  CTE-PROG '*********************************'

           CALL SBABEND
           .
      *------------------------ FIM SGTPB099 --------------------------
