       CBL ARITH(EXTEND)
      *-----------------------------------------------------------------
      * Programa..: SGTPB101
      * Analista..: F8658331 - Roberto Hiroshi Suto
      * Autor.....: F8658331 - Roberto Hiroshi Suto
      * Data......: 23/07/2020
      * Objetivo..: A partir do arquivo gerado pelo programa SGTP0100
      *             exclui registro de carteira com  erro.
      * COMPILACAO: 52 - PSOSE600 - Cobol 4.2 c/otimizacao p/producao
      *-----------------------------------------------------------------
      * VRS002      21/12/2020 F9158937 - Não considerar registros da
      *                                   Carteira 333 por enquanto.
      * VRS001      23/07/2019 F8658331 DE 2020/241714 - IMPLANTACAO
      *-----------------------------------------------------------------

      *----------------------------------------------------------------*
       IDENTIFICATION DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID. SGTPB101.

      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
      *----------------------------------------------------------------*
       CONFIGURATION SECTION.

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT                SECTION.
      *
       FILE-CONTROL.
           SELECT SGTF110E  ASSIGN  TO  SGTF110E.

           SELECT SGTF110S  ASSIGN  TO  SGTF110S.

      *----------------------------------------------------------------*
       DATA DIVISION.
      *----------------------------------------------------------------*

       FILE                        SECTION.
      *
       FD  SGTF110E
           RECORDING F
           RECORD 150
           BLOCK 0.

       01  SGTF100T-REG-FD             PIC X(150).


       FD  SGTF110S
           RECORDING F
           RECORD 150
           BLOCK 0.

       01  SGTF100T-REG-FD-S           PIC X(150).




      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *--------------------> Variaveis do Sistema <--------------------*
       77  CTE-INICIO-SS               PIC  X(035) VALUE
           '*** STORAGE SECTION COMECA AQUI ***'.
       77  CTE-PROG                    PIC  X(017)
           VALUE '*** SGTPB101 ***'.

       77  CTE-VERS                    PIC  X(006) VALUE 'VRS002'.
       77  SBABEND  VALUE 'SBABEND'    PIC  X(008).
       77  SBVERSAO VALUE 'SBVERSAO'   PIC  X(008).


      *----------------------------------------------------------------*
      *           Book do programa de ambiente - CICS5000              *
      *       Definido na work, para preservar as informacoes          *
      *----------------------------------------------------------------*
       LOCAL-STORAGE SECTION.
       COPY HLPKE001.

      * HLPKDFHE - Definicao da area DFHEIBLK
       COPY HLPKDFHE.

       COPY HLPKE01B.

       01 CNT-LIDOS                     PIC S9(004) COMP-5 VALUE 0.
       01 CNT-GRAVA                     PIC S9(004) COMP-5 VALUE 0.
       01 AX-IN-ERR-CTRA                PIC  9(002)        VALUE 0.
       01 AX-CARTEIRA-ANT               PIC S9(009) COMP-5 VALUE 0.

      * Arquivo da TIR/Duration das Operações

       01  SGTF110T-DADOS.
       COPY SGTK100T.

      * Controle de leitura do arquivo de entrada
       01  CTL-LET-ARQ.
           03  IN-FIM-ARQ               PIC X(001)  VALUE SPACES.
               88  FIM-ARQ                          VALUE 'N'.


      *------------------- Variaveis do Sistema -----------------------*
       01  CTE-SAI-SS                  PIC  X(40) VALUE
           '*** Storage Section Termina Aqui ***'.

      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*

       000000-PRINCIPAL                SECTION.

           MOVE 0001       TO     PSC-ERRO-PGM

           CALL  SBVERSAO  USING  CTE-PROG  CTE-VERS

           OPEN            INPUT   SGTF110E
                           OUTPUT  SGTF110S

           MOVE ZEROS                  TO RETURN-CODE.

           DISPLAY '------------------------------------------'
           DISPLAY '-- INICIO DE PROCESSAMENTO - SGTPB101     '
           DISPLAY '-- Exclui os registros de uma carteira    '
           DISPLAY '-- cateira caso tenha pelo menos uma      '
           DISPLAY '-- operação com  erro.                    '
           DISPLAY '------------------------------------------'

           PERFORM 700000-LER-REGISTROS

           PERFORM 200000-TRATA-REGISTRO UNTIL
                                IN-FIM-ARQ = 'S'

           PERFORM 800000-FIM

           .
       000000-SAI.
           GOBACK.

      *----------------------------------------------------------------*
       200000-TRATA-REGISTRO        SECTION.
      *----------------------------------------------------------------*
           MOVE  0002                  TO    PSC-ERRO-PGM

           MOVE  SGTF100T-CD-CTRA-TVM  TO    AX-CARTEIRA-ANT
           MOVE  SGTF100T-IN-ERR-CTRA  TO    AX-IN-ERR-CTRA

           DISPLAY 'CARTEIRA ==> ' AX-CARTEIRA-ANT ' - ' AX-IN-ERR-CTRA

           PERFORM    UNTIL SGTF100T-CD-CTRA-TVM NOT EQUAL
                            AX-CARTEIRA-ANT      OR
                            IN-FIM-ARQ           EQUAL 'S'

              IF SGTF100T-CD-CTRA-TVM NOT EQUAL 333
      *       Não considerar Carteira 333
                 IF  AX-IN-ERR-CTRA        EQUAL  ZEROS

                     WRITE SGTF100T-REG-FD-S   FROM SGTF100T-SAIDA

                     ADD +1 TO CNT-GRAVA

                       ON SIZE ERROR
                          MOVE  0706               TO  CD-RTN-PGM
                          MOVE  'Erro em operação aritmética.'
                                                   TO  MSG-ERRO
                          PERFORM 999000-ERRO
                     END-ADD

                 END-IF
              END-IF

              PERFORM 700000-LER-REGISTROS

           END-PERFORM

           .
       200000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       700000-LER-REGISTROS        SECTION.
      *----------------------------------------------------------------*

           MOVE  0003                  TO    PSC-ERRO-PGM

           READ SGTF110E INTO SGTF110T-DADOS
                AT END
                   MOVE 'S' TO IN-FIM-ARQ

                NOT AT END
                    ADD +1 TO CNT-LIDOS
                     ON SIZE ERROR
                        MOVE  0706               TO  CD-RTN-PGM
                        MOVE  'Erro em operação aritmética.'
                                                 TO  MSG-ERRO
                        PERFORM 999000-ERRO
                    END-ADD
           END-READ
           .
       700000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       800000-FIM                      SECTION.

           MOVE  0004                  TO  PSC-ERRO-PGM

           CLOSE       SGTF110E
                       SGTF110S

           IF CNT-GRAVA = 0
              MOVE  04                 TO  RETURN-CODE
           END-IF

           DISPLAY '*-------------------------------------------------*'
           DISPLAY '*- FIM DO PROCESSAMENTO                          -*'
           DISPLAY '*-------------------------------------------------*'
           DISPLAY ' '
           DISPLAY ' QT-REG lidos    SGTF110E: ' CNT-LIDOS
           DISPLAY ' QT-REG gravados SGTF110S: ' CNT-GRAVA
           DISPLAY '888 ' CTE-PROG ' Código      : ' RETURN-CODE
           DISPLAY ' '
           DISPLAY '***************************************************'

           .
       800000-SAI.
           EXIT.

      *---------------

       999000-ERRO SECTION.

           PERFORM 999001-ERRO-001
           .
       999000-ERRO-SAI.
           EXIT.
      *--------------------
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

       999009-SAI.
           EXIT.
      *---------------- FIM SGTPB101      ----------------------------
