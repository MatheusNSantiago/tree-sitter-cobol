      *================================================================*
      * PROGRAMA: foo
      * AUTOR:    F7023235 Matheus Santiago.
      * COMPILACAO: 54 - PSOSE600 - Cobol 6.3 c/otimizacao p/producao
      * OBJETIVO..: .
      *================================================================*
      *
      * VRS001 06/01/2024 F7023235 Implantação - TAREFA
      *
      *================================================================*
      *************************
       IDENTIFICATION DIVISION.
        PROGRAM-ID. foo.
        AUTHOR. F7023236.
      *************************
      *
      **********************
       ENVIRONMENT DIVISION.
      **********************
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *
       INPUT-OUTPUT SECTION.
      *
       FILE-CONTROL.
      *
           SELECT ACH510EE ASSIGN TO UT-S-ACH510EE.
      *
      ***************
       DATA DIVISION.
      ***************
      *
      *-----------------------------------------------------------------
       FILE SECTION.
      *-----------------------------------------------------------------
      *
       FD  ACH510EE
           BLOCK  0
           RECORD 1200
           RECORDING F.
      *
       01  REGISTRO-ACH510EE            PIC X(1200).

      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
      *-----------------------------------------------------------------
      *
      *----------------------- P R O G R A M A S ----------------------*
      *
       77  SBVERSAO                     PIC  X(008) VALUE 'SBVERSAO'.
       77  SBABEND                      PIC  X(007) VALUE 'SBABEND'.
       77  SBCPU                        PIC  X(005) VALUE 'SBCPU'.
      *
      *--------------------- C O N S T A N T E S ----------------------*
      *
       77  CTE-PROG                     PIC  X(016)
                                        VALUE '*** foo ***'.
       77  CTE-VERS                     PIC  X(006) VALUE 'VRS001'.
      *
      *-------------------- I N D I C A D O R E S ---------------------*
      *

      *
      *--------------------- C O N T A D O R E S ----------------------*
      *

      *
      *-----------------------  G U A R D A S -------------------------*
      *

      * +--------------------------------------------------------------+
      * |          Variaveis utilizadas na chamada da SBCPU.           |
      * +--------------------------------------------------------------+
       01  SBCPU-TX-IDFR-AMB            PIC  X(004).
       01  FILLER REDEFINES SBCPU-TX-IDFR-AMB.
           03 SBCPU-NIVEL88-IDFR-AMB    PIC  X(003).
              88 SBCPU-N88-DES          VALUE 'DES'.
              88 SBCPU-N88-HMH          VALUE 'HOM'.
              88 SBCPU-N88-PRD          VALUE 'BSB' 'BS2'.
           03 FILLER                    PIC  X(001).
      *
      ********************
       PROCEDURE DIVISION.
      ********************
      *
      *----------------------------------------------------------------*
       000000-ROTINA-PRINCIPAL                                  SECTION.
      *----------------------------------------------------------------*
      *
           PERFORM 100000-INICIA.
      *
           STOP RUN.
      *
      *----------------------------------------------------------------*
       100000-INICIA                                            SECTION.
      *----------------------------------------------------------------*
      *
           CALL SBVERSAO USING CTE-PROG CTE-VERS.
      *
           PERFORM 110000-ABRE-ARQUIVOS.
           PERFORM 120000-VERIFICA-AMBIENTE.
      *
       100099-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       120000-VERIFICA-AMBIENTE                                 SECTION.
      *----------------------------------------------------------------*
      *
           DISPLAY 'foo (120000) - CALL SBCPU...'.
      *
           CALL SBCPU USING SBCPU-TX-IDFR-AMB.
      *
           DISPLAY 'foo (120000) - SBCPU RC: ' RETURN-CODE.
      *
           IF RETURN-CODE NOT EQUAL ZEROES
              PERFORM 990001-ERRO-01
           END-IF.
      *
           EVALUATE TRUE
               WHEN SBCPU-N88-DES
                    DISPLAY 'SBCPU - AMBIENTE DESENVOLVIMENTO'
      *
               WHEN SBCPU-N88-HMH
                    DISPLAY 'SBCPU - AMBIENTE HOMOLOGACAO    '
      *
               WHEN SBCPU-N88-PRD
                    DISPLAY 'SBCPU - AMBIENTE PRODUCAO       '
      *
               WHEN OTHER
                    DISPLAY 'SBCPU - AMBIENTE DESCONHECIDO   '
      *
           END-EVALUATE.
      *
       120099-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       130000-ABRE-ARQUIVOS                                     SECTION.
      *----------------------------------------------------------------*
      *
           OPEN INPUT  ACH510EE.

      *
       130099-SAI.
           EXIT.
