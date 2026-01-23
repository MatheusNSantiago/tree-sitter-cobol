       CBL ARITH(EXTEND)
      *-----------------------------------------------------------------
      * PROGRAMA..: SGTPB136
      * ANALISTA..: F9458161 - Vanessa Chinen
      * AUTOR.....: F4957150 - João P.S. Tomaz
      * DATA......: 09/10/2024
      * COMPILACAO: 54 - PSOSE600 - Cobol 6.3 c/otimizacao p/producao
      * OBJETIVO..: GRAVAR REGISTROS NA TABELA PRCT_CTRA_TSR
      *-----------------------------------------------------------------
      * VRS001 04/10/2024 F4957150 - IMPLANTAÇÃO
      *-----------------------------------------------------------------

      *----------------------------------------------------------------*
       IDENTIFICATION DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID. SGTPB136.

      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
      *----------------------------------------------------------------*
       CONFIGURATION SECTION.

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT                SECTION.
      *
       FILE-CONTROL.
           SELECT SGTF136E  ASSIGN  TO  SGTF136E.

      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------
       FILE                        SECTION.
      *
      * Arquivo GSF com CRNG_EVT_ITMT x ITMT_FNCO_FDAR
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
           '*** SGTPB136 ***'.

       77  CTE-VERS                    PIC  X(06) VALUE 'VRS001'.

      * CHAMADOS + MOCK
       01  TAB-PROGRAMAS-CHAMADOS.
           03  SBABEND       PIC  X(08) VALUE 'SBABEND '.
           03  SBVERSAO      PIC  X(08) VALUE 'SBVERSAO'.
           03  SGTST136      PIC  X(08) VALUE 'SGTST136'.
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

        01 IA-1                        PIC S9(004)    COMP-5  VALUE 0.
        01 IA-2                        PIC S9(004)    COMP-5  VALUE 0.


       01 GDA-MSG-ERRO                 PIC  X(50) VALUE SPACES.
       01 CNT-LIDOS                    PIC S9(005)    COMP-5 VALUE 0.
       01 CNT-GRAVADOS                 PIC S9(005)    COMP-5 VALUE 0.
       01 CNT-COMMIT                   PIC S9(005)    COMP-5 VALUE 0.

       01  DIS-CODE                PIC  --------9.
       01  DIS-SINAL-CODE          PIC X(1).


      * ------- PROGRAMAS CHAMADOS -------------------------------------
       01  SGTST136-DADOS.
       COPY SGTKT136.

      *------- LAYOUT ARQUIVO DE ENTRADA
       01  SGTP136E-DADOS.
       COPY SGTK136E.

      * Controle de leitura do arquivo de entrada
       01  CTL-LET-ARQ.
           03  IN-FIM-ARQ       PIC X(01)          VALUE SPACES.
               88  FIM-ARQ                         VALUE 'S'.
      *
           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.
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
           DISPLAY '-- INICIO DE PROCESSAMENTO - SGTPB136     '
           DISPLAY '------------------------------------------'


           OPEN INPUT  SGTF136E

           PERFORM UNTIL FIM-ARQ
               INITIALIZE SGTP136E-DADOS
               READ SGTF136E INTO SGTP136E-DADOS
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
                  PERFORM 400000-GRAVA-REGISTRO
               END-READ

           END-PERFORM

           PERFORM 800000-FIM
           .
       000000-SAI.
           GOBACK.
      *
      *
      *----------------------------------------------------------------*
       400000-GRAVA-REGISTRO           SECTION.
      *----------------------------------------------------------------*
           MOVE  0002                  TO  PSC-ERRO-PGM
           SET  SGTST136-INCLUIR       TO TRUE
           MOVE SGTPB136-DT-PRCT       TO SGTST136-DT-PRCT
           MOVE SGTPB136-NR-SEQL-PRCT  TO SGTST136-NR-SEQL-PRCT
           MOVE SGTPB136-DT-CPR        TO SGTST136-DT-CPR
           MOVE SGTPB136-CD-ITMT-FNCO  TO SGTST136-CD-ITMT-FNCO
           MOVE SGTPB136-VL-PU-CPR     TO SGTST136-VL-PU-CPR
           MOVE SGTPB136-CD-TIP-PRCT   TO SGTST136-CD-TIP-PRCT
           MOVE SGTPB136-CD-EST-PRCT   TO SGTST136-CD-EST-PRCT
           MOVE 'CURRENT'              TO SGTST136-TS-PRCT
           MOVE SGTPB136-CD-PGM        TO SGTST136-CD-PGM
           MOVE SGTPB136-CD-CTRA-TIT-VL
                                       TO SGTST136-CD-CTRA-TIT-VL
           MOVE SGTPB136-CD-OPR-ETRD-MCP
                                       TO SGTST136-CD-OPR-ETRD-MCP
           MOVE SGTPB136-NR-VRS-OPR-ETRD
                                       TO SGTST136-NR-VRS-OPR-ETRD
           MOVE SGTPB136-CD-USU        TO SGTST136-CD-USU
           MOVE SGTPB136-CD-OGM-ERRO   TO SGTST136-CD-OGM-ERRO
           MOVE SGTPB136-TX-ERRO-PRCT  TO SGTST136-TX-ERRO-PRCT
      *
           PERFORM 901000-CHAMAR-SGTST136
      *
      * MOCK-POINT MCKV2
           ADD   +1       TO CNT-COMMIT CNT-GRAVADOS
                           ON SIZE ERROR
                           MOVE 'ERRO EM OPERACAO ARITMETICA 2'
                                       TO MSG-ERRO
                           PERFORM   999000-ERRO
           END-ADD
           IF CNT-COMMIT = 1000
               PERFORM 900900-CONTROLA-COMMIT
               MOVE 0                  TO CNT-COMMIT
           END-IF
           .
       400000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       800000-FIM                      SECTION.
      *----------------------------------------------------------------*
           MOVE  0003                  TO  PSC-ERRO-PGM

           CLOSE SGTF136E

           PERFORM 900900-CONTROLA-COMMIT

           DISPLAY '*-------------------------------------------------*'
           DISPLAY '*- FIM DO PROCESSAMENTO                          -*'
           DISPLAY '*-------------------------------------------------*'
           DISPLAY ' '
           MOVE CNT-LIDOS              TO DIS-CODE
           DISPLAY ' QT REGISTROS LIDOS               : ' DIS-CODE
           MOVE CNT-GRAVADOS           TO DIS-CODE
           DISPLAY ' QT DE REGISTROS GRAVADOS         : ' DIS-CODE
           DISPLAY '888 ' CTE-PROG ' Código           : ' RETURN-CODE
           DISPLAY ' '
           DISPLAY '***************************************************'
           .
       800000-SAI.
           EXIT.
      *
      *-----------------------------------------------------------------
       901000-CHAMAR-SGTST136              SECTION.
      *-----------------------------------------------------------------
           MOVE 0004                   TO PSC-ERRO-PGM
      *
           MOVE LENGTH OF SGTST136-DADOS
                                       TO EIBCALEN
           CALL SGTST136 USING DFHEIBLK SGTST136-DADOS
      * MOCK-POINT MCKV3
           IF SGTST136-CD-RTN-PGM NOT EQUAL ZEROS
           DISPLAY 'ERRO ' SGTST136-CD-RTN-PGM ' - '
                           SGTST136-MSG-ERRO-PGM
              MOVE SGTST136-RTN-PGM    TO RTN-PGM
              MOVE CNT-GRAVADOS        TO DIS-CODE
              STRING 'ERRO NA GRAVAÇÃO DO LOG NO REGISTRO ' DIS-CODE
                                      DELIMITED BY SIZE
                                     INTO TX-IDFC-ATNT-MSG-E
              PERFORM 999000-ERRO
           END-IF
           .

       901000-SAI.
           EXIT.
      *
      *---------------------------------------*
       900900-CONTROLA-COMMIT          SECTION.
      *---------------------------------------*

           CALL DB2S0001 USING DB2S0001-LINK.
      * MOCK-POINT MCKV4
           IF  NOT DB2S0001-FIM-NORMAL
               PERFORM 900901-ERRO-COMMIT
           END-IF
           .
       900900-FIM.
           EXIT.

      *----------------------------------------------------------------*
       900901-ERRO-COMMIT     SECTION.
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
       900901-SAI.
           EXIT.

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
           MOVE CNT-LIDOS              TO DIS-CODE
           DISPLAY ' QT REGISTROS LIDOS               : ' DIS-CODE
           MOVE CNT-GRAVADOS           TO DIS-CODE
           DISPLAY ' QT DE REGISTROS GRAVADOS         : ' DIS-CODE
           DISPLAY '888 ' CTE-PROG ' Código           : ' RETURN-CODE

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


      *------------------------ FIM SGTPB136 --------------------------
