      *================================================================*
      * PROGRAMA: ACHPCT98
      *
      * AUTOR:    F7023235 Matheus Santiago.
      *
      * COMPILACAO: 54 - PSOSE600 - Cobol 6.3 c/otimizacao p/producao
      * OBJETIVO..: as.
      *
      *================================================================*
      *
      * VRS001 26/11/2023 F7023235 Implantação - TAREFA
      *
      *================================================================*
      *************************
       IDENTIFICATION DIVISION.
        PROGRAM-ID. ACHPCT98.
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
      ***************
       DATA DIVISION.
      ***************
      *
      *-----------------------------------------------------------------
       FILE SECTION.
      *-----------------------------------------------------------------
      *

      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
      *-----------------------------------------------------------------
      *
      *----------------------- P R O G R A M A S ----------------------*
      *
       77 SBVERSAO                     PIC  X(008) VALUE 'SBVERSAO'.
       77 SBABEND                      PIC  X(008) VALUE 'SBABEND'.

      *
      *--------------------- C O N S T A N T E S ----------------------*
      *
       77 CTE-PROG                     PIC  X(016)
                                       VALUE '*** ACHPCT98 ***'.
       77 CTE-VERS                     PIC  X(006) VALUE 'VRS001'.

      *
      *-------------------- I N D I C A D O R E S ---------------------*
      *

      *
      *--------------------- C O N T A D O R E S ----------------------*
      *

      *
      *-------------------------- B O O K S ---------------------------*
      *

      *
      *-----------------------  G U A R D A S -------------------------*
      *

      * ┌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┐
      * ╎          Variaveis utilizadas na chamada da SBCPU.           ╎
      * └╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┘
       01  SBCPU-TX-IDFR-AMB           PIC  X(004).
       01  FILLER REDEFINES SBCPU-TX-IDFR-AMB.
           03 SBCPU-NIVEL88-IDFR-AMB   PIC  X(003).
              88 SBCPU-N88-DES         VALUE 'DES'.
              88 SBCPU-N88-HMH         VALUE 'HOM'.
              88 SBCPU-N88-PRD         VALUE 'BSB' 'BS2'.
           03 FILLER                   PIC  X(001).
      *
      ********************
       PROCEDURE DIVISION.
      ********************
      *
      *---------------------------------------*
       000000-ROTINA-PRINCIPAL         SECTION.
      *---------------------------------------*
      *
           PERFORM 100000-INICIA.
      *
           PERFORM 200000-PROCESSA
      *
           PERFORM 300000-FINALIZA.
      *
           STOP RUN.
      *
      *---------------------------------------*
       100000-INICIA                  SECTION.
      *---------------------------------------*
      *
      *    CALL SBVERSAO USING CTE-PROG CTE-VERS.
      *
           PERFORM 110000-INICIALIZA-VARIAVEIS.
           PERFORM 120000-VERIFICA-AMBIENTE.

      *
       100099-SAI.
           EXIT.
      *
      *----------------------------------------
       110000-INICIALIZA-VARIAVEIS     SECTION.
      *----------------------------------------
      *


      *
       110099-SAI.
           EXIT.
      *
      *----------------------------------------
       120000-VERIFICA-AMBIENTE        SECTION.
      *----------------------------------------
      *
           DISPLAY 'ACHPCT98 (120000) - CALL SBCPU...'.
      *
      *    CALL SBCPU USING SBCPU-TX-IDFR-AMB.
      *
           DISPLAY 'ACHPCT98 (120000) - SBCPU RC: ' RETURN-CODE.
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
      *---------------------------------------*
       200000-PROCESSA                 SECTION.
      *---------------------------------------*
      *


      *
       200099-SAI.
           EXIT.
      *
      *--------------------------------------*
       300000-FINALIZA                SECTION.
      *--------------------------------------*
      *


       300099-SAI.
           EXIT.
      *
      *--------------------------------------*
       990000-ERROS                   SECTION.
      *--------------------------------------*
      *
       990001-ERRO-01.
           DISPLAY '================================================='.
           DISPLAY '888 ' CTE-PROG ' 001 - ERR-SBCPU - '.
           DISPLAY 'ERRO DURANTE A CHAMADA DA SUB-ROTINA. '.
           DISPLAY '================================================='.
           DISPLAY '888 ' CTE-PROG ' 001 - SBCPU-TX-IDFR-AMB ......: '
                                     SBCPU-TX-IDFR-AMB.

           DISPLAY '888 ' CTE-PROG ' 001 - RETURN-CODE ............: '
                                     RETURN-CODE.
           PERFORM 999000-ABENDA .
      *
      *--------------------------------------*
       999000-ABENDA                  SECTION.
      *--------------------------------------*
           DISPLAY '888 ' CTE-PROG ' 888 CANCELADO'.
      *    CALL SBABEND.
           EXIT.
      *====================== FIM ACHPCT98 ============================*
