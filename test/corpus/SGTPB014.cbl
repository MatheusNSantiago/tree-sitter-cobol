       CBL ARITH(EXTEND)
      *-----------------------------------------------------------------
      * PROGRAMA..: SGTPB014
      * ANALISTA..: EDILENE AKEMI
      * AUTOR.....: EDILENE AKEMI
      * DATA......: 05/01/2018
      * OBJETIVO..: Programa batch para reformatar arquivo de moviment-
      *             ação financeira de titulo oriundo do sistema Drive
      * COMPILACAO: COBOL
      *-----------------------------------------------------------------
      * VRS001 05/01/2018 - F2634012 - DE 2016/145195 - IMPLANTACAO
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------

       PROGRAM-ID. SGTPB014.

      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
      *-----------------------------------------------------------------

       CONFIGURATION SECTION.


       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT                SECTION.
      *
       FILE-CONTROL.
           SELECT SGTFDRV1  ASSIGN  TO  SGTFDRV1.
           SELECT SGTF014S  ASSIGN  TO  SGTF014S.


      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------
       FILE                        SECTION.
      *
       FD  SGTFDRV1
           RECORDING F
           RECORD 1000
           BLOCK 0.
       01  REG-FD-ARQ1        PIC X(1000).
      *
       FD  SGTF014S
           RECORDING F
           RECORD 300
           BLOCK 0.
       01  SGTF014S-REG-FD    PIC X(300).
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
      *-----------------------------------------------------------------

       77  CTE-INICIO-SS    PIC  X(40) VALUE
           '*** STORAGE SECTION COMECA AQUI ***'.
       77  CTE-PROG     PIC  X(17) VALUE '*** SGTPB014 ***'.
       77  CTE-VERS     PIC  X(6)  VALUE 'VRS001'.

      *
      * Subrotinas Utilizadas

       77  SBABEND  VALUE 'SBABEND'    PIC  X(008).
       77  SBVERSAO VALUE 'SBVERSAO'   PIC  X(008).
       77  SBDATA   VALUE 'SBDATA'     PIC  X(008).

      *----------------------------------------------------------------
       LOCAL-STORAGE SECTION.
      *----------------------------------------------------------------


      * Books da definicao de dados a serem usados na subrotina de erros
-INC HLPKE001

      * HLPKDFHE - Definicao da area DFHEIBLK
-INC HLPKDFHE

       01 CNT-LIDOS                       PIC S9(004) COMP-5 VALUE 0.
       01 CNT-REJEITADOS                  PIC S9(004) COMP-5 VALUE 0.
       01 CNT-GRAVADOS                    PIC S9(004) COMP-5 VALUE 0.
       01 CNT-DESPREZADOS                 PIC S9(004) COMP-5 VALUE 0.


      * os contadores abaixo servem para fornecer o tamanho dos campos
      * do arquivo de entrada. Caso algum deles vierem fora do tama-
      * nho previsto o pgm irá abendar.
      *
       01 CNT-DT-MOVTO                    PIC S9(004) COMP-5 VALUE 0.
       01 CNT-TP-REG                      PIC S9(004) COMP-5 VALUE 0.
       01 CNT-TIT                         PIC S9(004) COMP-5 VALUE 0.
       01 CNT-INT-PAPL                    PIC S9(004) COMP-5 VALUE 0.
       01 CNT-DCML-PAPL                   PIC S9(004) COMP-5 VALUE 0.
       01 CNT-TP-LACTO                    PIC S9(004) COMP-5 VALUE 0.
       01 CNT-INT-VALOR                   PIC S9(004) COMP-5 VALUE 0.
       01 CNT-DCML-VALOR                  PIC S9(004) COMP-5 VALUE 0.
       01 CNT-CAPTURA                     PIC S9(004) COMP-5 VALUE 0.
       01 CNT-NRREG                       PIC S9(004) COMP-5 VALUE 0.
       01 CNT-TEXTO                       PIC S9(004) COMP-5 VALUE 0.
       01 CNT-CARTEIRA                    PIC S9(004) COMP-5 VALUE 0.

       01 CH-ABEND                        PIC X(001)  VALUE 'N'.
      *

       01 AUX-ERRO                       PIC  9(003)  VALUE ZEROS.

       01  IN-ERRO-VALIDACAO             PIC X(01)  VALUE SPACES.
               88  ERRO-VALIDACAO                  VALUE 'S'.
      *

       01 WS-REG-A.
           05 WS-REG-B           PIC X(1) OCCURS 1000 TIMES.
       01 WS-NRREG-A.
           05 WS-NRREG-B         PIC X(1) OCCURS 09 TIMES.
       01 WS-NRREG-A-ALFA.
          05 WS-NRREG-B-ALFA     PIC X(1) OCCURS 09 TIMES.
       01 WS-TIT-A.
           05 WS-TIT-B           PIC X(1) OCCURS 12 TIMES.
       01 WS-VL-A-INT15.
           05 WS-VL-B-INT15      PIC X(1) OCCURS 15 TIMES.
       01 WS-VL-A-INT13.
           05 WS-VL-B-INT13      PIC X(1) OCCURS 13 TIMES.
       01 WS-VL-A-DCML14.
           05 WS-VL-B-DCML14     PIC X(1) OCCURS 14 TIMES.
       01 WS-VL-A-DCML2.
           05 WS-VL-B-DCML2      PIC X(1) OCCURS 2 TIMES.
       01 WS-TP-REG.
          05  WS-TP-REG-R        PIC X(1) OCCURS 3 TIMES.
       01 WS-TP-REG-NUM.
          05  WS-TP-REG-N        PIC X(1) OCCURS 3 TIMES.
       01 WS-TP-LACTO            PIC X(1).
       01 WS-CARTEIRA.
          05  WS-CARTEIRA-A      PIC X(1) OCCURS 9 TIMES.
       01 WS-CARTEIRA-NUM.
          05  WS-CARTEIRA-N      PIC X(1) OCCURS 9 TIMES.
      *
       01 WS-VALOR15V14          PIC X(29).
       01 WS-VALOR15V14-R        REDEFINES WS-VALOR15V14.
          03  WS-VL1-INT15       PIC X(15).
          03  WS-VL1-DCML14      PIC X(14).
       01 FILLER                 REDEFINES WS-VALOR15V14-R.
           05 WS-VL1-INT15-R     PIC X(1) OCCURS 15 TIMES.
           05 WS-VL1-DCML14-R    PIC X(1) OCCURS 14 TIMES.
      *
       01 WS-VALOR13V2           PIC X(15).
       01 WS-VALOR13V2-R         REDEFINES WS-VALOR13V2.
          03  WS-VL2-INT13       PIC X(13).
          03  WS-VL2-DCML2       PIC X(02).
       01 FILLER                 REDEFINES WS-VALOR13V2-R.
           05 WS-VL2-INT13-R     PIC X(1) OCCURS 13 TIMES.
           05 WS-VL2-DCML2-R     PIC X(1) OCCURS 2 TIMES.

       01 WS-DATA-A.
           05 WS-DATA-B          PIC X(1) OCCURS 08 TIMES.
       01 WS-TXDCR-A.
           05 WS-TXDCR-B         PIC X(1) OCCURS 40 TIMES.
       01 WS-VALOR-TIT-A-INT.
           05 WS-VALOR-TIT-B-INT PIC X(1) OCCURS 15 TIMES.
       01 WS-VALOR-TIT-A-DCML.
           05 WS-VALOR-TIT-B-DCML PIC X(1) OCCURS 2 TIMES.
      *

      * àreas usadas para retirar caracter a caracter da entrada
       01 WS-CNT-PNT-VRG      PIC 9(04) COMP-5.
       01 WS-CNT-CMP          PIC 9(04) COMP-5.
       01 WS-CCT              PIC 9(04) COMP-5.

      * Ìndices
       01 IT-INT              PIC 9(04) COMP-5.
       01 IT-DCML             PIC 9(04) COMP-5.

      * Àreas para ajuste de centavos (trocar direita pela esquerda)
       01 IC-1           PIC 9(03).
       01 IC-2           PIC 9(03).

      * Separa inteiros e decimais
       01 AUX-FLAG-VRG       PIC X(01) VALUE SPACES.
       01 AUX-FLAG-SNL-NGT   PIC X(01) VALUE SPACES.
       01 CT-STOP            PIC 9(4).

      * indicadores de negativo para os campos de valor
      *
        01  AUX-SNL-NGT-PAPL          PIC X(01) VALUE SPACES.
        01  AUX-SNL-NGT-VALOR         PIC X(01) VALUE SPACES.
      *
        01  AUX-REG-V                 PIC X(01).

        01  SGTPB14E-DADOS.
-INC SGTKB14E
        01  SGTPB14S-DADOS.
-INC SGTKB14S


      * Controle de leitura do arquivo de entrada
       01  CTL-LET-ARQ.
           03  IN-FIM-ARQ       PIC X(01)          VALUE SPACES.
               88  FIM-ARQ                         VALUE 'S'.


      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL SECTION.
      *-----------------------------------------------------------------

           MOVE 0001                   TO PSC-ERRO-PGM

           CALL  SBVERSAO  USING  CTE-PROG  CTE-VERS

           OPEN INPUT   SGTFDRV1
                OUTPUT  SGTF014S

           PERFORM UNTIL FIM-ARQ

                    INITIALIZE SGTPB14E-DADOS SGTPB14S-DADOS

               READ SGTFDRV1
                    AT END
                       MOVE 'S' TO IN-FIM-ARQ

                    NOT AT END
                    ADD  +1     TO CNT-LIDOS
                         ON SIZE ERROR
                         MOVE 1 TO AUX-ERRO
                         PERFORM   999000-ERRO
                    END-ADD
                    INITIALIZE  AUX-SNL-NGT-PAPL
                                AUX-SNL-NGT-VALOR
                                AUX-ERRO

                    MOVE ZEROS TO WS-CNT-PNT-VRG

                    MOVE REG-FD-ARQ1 TO WS-REG-A

                    MOVE ZEROS TO WS-CCT

                    PERFORM   140000-TRATA-REGISTRO
                             UNTIL WS-CCT   GREATER 999
                    PERFORM 100000-VERIFICA-DADOS

               END-READ


           END-PERFORM

           PERFORM 900100-EXIBIR-RESUMO-PROCESS

           STOP RUN
           .

       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       140000-TRATA-REGISTRO          SECTION.
      *-----------------------------------------------------------------

           MOVE 0002                 TO   PSC-ERRO-PGM

           ADD    +1                 TO   WS-CCT
                  ON SIZE ERROR
                  MOVE     2         TO   AUX-ERRO
                  PERFORM   999000-ERRO
           END-ADD

           IF     WS-CCT             EQUAL   1
                  PERFORM 150000-TRATA-NRREG
           END-IF

           IF     WS-REG-B(WS-CCT)   EQUAL   ';'
                  ADD   +1           TO      WS-CNT-PNT-VRG
                        ON SIZE ERROR
                        MOVE     3   TO      AUX-ERRO
                        PERFORM   999000-ERRO
                  END-ADD
      *
      *  CAMPOS TRATADOS NO EVALUATE
      *  = 2 --->  verifica se o registro contem o cabeçalho das colu-
      *            nas que deverá ser desprezado
      *  = 3 --->  código da carteira
      *  = 4 --->  data (origem) - DT-MOVTO (destino)
      *  = 5 --->  TipoRegistro( origem ) - TP-REG ( destino )
      *  = 11 ---> Quantidade - QT-PAPL (destino)
      *  = 12 ---> Valor (origem) - VALOR (destino)
      *  = 13 ---> Dc (origem)  -  TP-LACTO (destino)
      *  = 14 ---> TituloCP (origem)  -  TITULO (destino)
      *  = 16 ---> TipoMov  (origem)  -  CAPTURA (destino)
      *  = 17 ---> Historico (origem) - TEXTO ( destino )
            EVALUATE WS-CNT-PNT-VRG
             WHEN 2
                    PERFORM 146000-VERIFICA-REGISTRO
             WHEN 3
                    PERFORM 149000-TRATA-CARTEIRA
             WHEN 4
                    PERFORM 142000-TRATA-DATA
             WHEN 5
                    PERFORM 147000-TRATA-TP-REG
             WHEN 14
                    PERFORM 141000-TRATA-TIT
             WHEN 11
                    PERFORM 143000-TRATA-VALOR1
             WHEN 12
                    PERFORM 144000-TRATA-VALOR2
             WHEN 13
                    PERFORM 148000-TRATA-LACTO
             WHEN 16
                    PERFORM 145000-TRATA-TXDCR
             WHEN 17
                    PERFORM 145000-TRATA-TXDCR
            END-EVALUATE
           END-IF

           .

       140000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       141000-TRATA-TIT          SECTION.
      *-----------------------------------------------------------------

           MOVE 0003             TO   PSC-ERRO-PGM

           MOVE    ZEROS         TO    IT-INT
           MOVE    SPACES        TO    WS-TIT-A
           MOVE    WS-CCT        TO    WS-CNT-CMP
           ADD     +1            TO    WS-CNT-CMP
                   ON SIZE ERROR
                   MOVE      4   TO    AUX-ERRO
                   PERFORM   999000-ERRO
           END-ADD
      *
           PERFORM UNTIL WS-REG-B(WS-CNT-CMP) EQUAL    ';' OR
                         WS-CNT-CMP           GREATER  999
             ADD     +1                       TO    IT-INT
                     ON SIZE ERROR
                      MOVE      5   TO      AUX-ERRO
                      PERFORM   999000-ERRO
             END-ADD
             IF    IT-INT                     NOT GREATER  12
                   MOVE  WS-REG-B(WS-CNT-CMP) TO    WS-TIT-B(IT-INT)
             END-IF

             ADD     +1                       TO   WS-CNT-CMP
                     ON SIZE ERROR
                      MOVE     6   TO      AUX-ERRO
                      PERFORM   999000-ERRO
             END-ADD
           END-PERFORM
           MOVE IT-INT     TO CNT-TIT
           MOVE WS-TIT-A TO SGTPB14S-TITULO
           .

       141000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       142000-TRATA-DATA         SECTION.
      *-----------------------------------------------------------------

           MOVE 0004            TO    PSC-ERRO-PGM
           MOVE    ZEROS        TO    IT-INT
           MOVE    '00000000'   TO    WS-DATA-A
           MOVE    WS-CCT       TO    WS-CNT-CMP
           ADD     1            TO    WS-CNT-CMP
                   ON SIZE ERROR
                   MOVE     7   TO      AUX-ERRO
                   PERFORM   999000-ERRO
           END-ADD
      *
           PERFORM UNTIL WS-REG-B(WS-CNT-CMP) EQUAL   ';' OR
                         WS-CNT-CMP           GREATER  999
             IF    WS-REG-B(WS-CNT-CMP) NOT EQUAL '/' AND
                   WS-REG-B(WS-CNT-CMP) NOT EQUAL ' '
                   ADD      +1  TO  IT-INT
                        ON SIZE ERROR
                        MOVE      8   TO      AUX-ERRO
                        PERFORM   999000-ERRO
                  END-ADD
               IF  IT-INT              NOT GREATER     8
                   MOVE    WS-REG-B(WS-CNT-CMP) TO WS-DATA-B(IT-INT)
               END-IF
             END-IF
             ADD +1   TO WS-CNT-CMP
                      ON SIZE ERROR
                        MOVE      9   TO      AUX-ERRO
                        PERFORM   999000-ERRO
             END-ADD
           END-PERFORM
           MOVE    WS-DATA-A          TO      SGTPB14S-DT-MOVTO
           MOVE    IT-INT             TO      CNT-DT-MOVTO
           .

       142000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       143000-TRATA-VALOR1          SECTION.
      *-----------------------------------------------------------------

      * Formata os campos de valores que possuem 15 casas inteiras e
      * 14 decimais - quantidade de papeis

           MOVE    0005    TO    PSC-ERRO-PGM
           MOVE    SPACES  TO    AUX-FLAG-VRG
           MOVE    SPACES  TO    AUX-FLAG-SNL-NGT
           MOVE    SPACES  TO    WS-VL-A-INT15 WS-VL-A-DCML14
           MOVE    ZEROS   TO    CNT-INT-PAPL CNT-DCML-PAPL
           MOVE    ZEROS   TO    IT-INT IT-DCML
           MOVE    WS-CCT  TO    WS-CNT-CMP
           ADD     1       TO    WS-CNT-CMP
                   ON SIZE ERROR
                      MOVE      10   TO      AUX-ERRO
                      PERFORM   999000-ERRO
           END-ADD

           PERFORM UNTIL WS-REG-B(WS-CNT-CMP)EQUAL     ';'  OR
                         WS-CNT-CMP           GREATER  999
             IF      WS-REG-B(WS-CNT-CMP)    NOT EQUAL ','
              IF     WS-REG-B(WS-CNT-CMP)    EQUAL     '-'
                     MOVE 'S'                TO   AUX-FLAG-SNL-NGT
              ELSE
               IF    AUX-FLAG-VRG            EQUAL SPACES
                     ADD  1                  TO   IT-INT
                      ON SIZE ERROR
                        MOVE     11   TO      AUX-ERRO
                        PERFORM   999000-ERRO
                     END-ADD
                     IF   IT-INT            NOT GREATER 15
                          MOVE WS-REG-B(WS-CNT-CMP) TO
                          WS-VL-B-INT15(IT-INT)
                     END-IF
               ELSE
                     ADD  1                    TO IT-DCML
                      ON SIZE ERROR
                        MOVE      12   TO      AUX-ERRO
                        PERFORM   999000-ERRO
                     END-ADD

                 IF   IT-DCML         NOT GREATER  14
                      MOVE WS-REG-B(WS-CNT-CMP) TO
                      WS-VL-B-DCML14(IT-DCML)
                 END-IF
               END-IF
              END-IF
             ELSE
                     MOVE 'X'                 TO AUX-FLAG-VRG
             END-IF
             ADD     1       TO      WS-CNT-CMP
                     ON SIZE ERROR
                        MOVE      13   TO      AUX-ERRO
                        PERFORM   999000-ERRO
             END-ADD
           END-PERFORM
      *
      * inverte a posição das casas inteiras
      *
           MOVE          15             TO        IC-1
           MOVE          15             TO        IC-2
           MOVE          SPACES         TO        WS-VALOR15V14
           PERFORM UNTIL  IC-1          EQUAL     0
            IF WS-VL-B-INT15(IC-1)      NOT EQUAL ' '
               MOVE WS-VL-B-INT15(IC-1) TO WS-VL1-INT15-R(IC-2)
               SUBTRACT 1               FROM      IC-2
                     ON SIZE ERROR
                        MOVE      14    TO      AUX-ERRO
                        PERFORM   999000-ERRO
               END-SUBTRACT
            END-IF
            SUBTRACT 1                  FROM      IC-1
                     ON SIZE ERROR
                        MOVE      15    TO      AUX-ERRO
                        PERFORM   999000-ERRO
            END-SUBTRACT
           END-PERFORM

      * completa com zeros a esquerda as casas inteiras
      *
           INSPECT WS-VL1-INT15 REPLACING ALL ' ' BY '0'
      *
      * completa com zeros a esquerda as casas decimais
      *
           MOVE    WS-VL-A-DCML14 TO        WS-VL1-DCML14
           INSPECT WS-VL1-DCML14  REPLACING ALL ' ' BY '0'
      *
           ADD   IT-INT                 TO       CNT-INT-PAPL
                 ON SIZE ERROR
                    MOVE      16        TO       AUX-ERRO
                    PERFORM   999000-ERRO
           END-ADD
           ADD   IT-DCML                TO       CNT-DCML-PAPL
                 ON SIZE ERROR
                    MOVE      17        TO       AUX-ERRO
                    PERFORM   999000-ERRO
           END-ADD

           MOVE WS-VALOR15V14           TO       SGTPB14S-QT-PAPL
           MOVE AUX-FLAG-SNL-NGT        TO       AUX-SNL-NGT-PAPL

           .

       143000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       144000-TRATA-VALOR2          SECTION.
      *-----------------------------------------------------------------

      * Formata os campos de valores que possuem 13 casas inteiras e
      * 2 decimais - valor

           MOVE 0006       TO    PSC-ERRO-PGM
           MOVE    SPACES  TO    AUX-FLAG-VRG
           MOVE    SPACES  TO    AUX-FLAG-SNL-NGT
           MOVE    SPACES  TO    WS-VL-A-INT13 WS-VL-A-DCML2
           MOVE    ZEROS   TO    CNT-INT-VALOR CNT-DCML-VALOR
           MOVE    ZEROS   TO    IT-INT IT-DCML
           MOVE    WS-CCT  TO    WS-CNT-CMP
           ADD     1       TO    WS-CNT-CMP
                   ON SIZE ERROR
                      MOVE      18     TO      AUX-ERRO
                      PERFORM   999000-ERRO
           END-ADD

           PERFORM UNTIL WS-REG-B(WS-CNT-CMP)EQUAL     ';'  OR
                         WS-CNT-CMP           GREATER  999
             IF      WS-REG-B(WS-CNT-CMP)    NOT EQUAL ','
              IF     WS-REG-B(WS-CNT-CMP)    EQUAL     '-'
                     MOVE 'S'                TO   AUX-FLAG-SNL-NGT
              ELSE
               IF    AUX-FLAG-VRG            EQUAL SPACES
                     ADD  1                  TO   IT-INT
                      ON SIZE ERROR
                        MOVE      19     TO   AUX-ERRO
                        PERFORM   999000-ERRO
                     END-ADD

                     IF   IT-INT            NOT GREATER 13
                          MOVE WS-REG-B(WS-CNT-CMP) TO
                          WS-VL-B-INT13(IT-INT)
                    END-IF
               ELSE
                    ADD  1                    TO IT-DCML
                      ON SIZE ERROR
                        MOVE      20       TO  AUX-ERRO
                        PERFORM   999000-ERRO
                    END-ADD

                    IF   IT-DCML         NOT GREATER  2
                         MOVE WS-REG-B(WS-CNT-CMP) TO
                         WS-VL-B-DCML2(IT-DCML)
                    END-IF
                END-IF
              END-IF
             ELSE
                     MOVE 'X'                 TO AUX-FLAG-VRG
             END-IF
             ADD     1       TO      WS-CNT-CMP
                     ON SIZE ERROR
                        MOVE      21     TO      AUX-ERRO
                        PERFORM   999000-ERRO
             END-ADD
           END-PERFORM
      *
      * inverte a posição das casas inteiras
      *
           MOVE          13             TO        IC-1
           MOVE          13             TO        IC-2
           MOVE          SPACES         TO        WS-VALOR13V2

           PERFORM UNTIL  IC-1          EQUAL     0
            IF WS-VL-B-INT13(IC-1)      NOT EQUAL ' '
               MOVE WS-VL-B-INT13(IC-1) TO WS-VL2-INT13-R(IC-2)
               SUBTRACT 1               FROM      IC-2
                     ON SIZE ERROR
                        MOVE      22    TO      AUX-ERRO
                        PERFORM   999000-ERRO
               END-SUBTRACT
            END-IF
            SUBTRACT 1                  FROM      IC-1
                     ON SIZE ERROR
                        MOVE      23    TO      AUX-ERRO
                        PERFORM   999000-ERRO
            END-SUBTRACT
           END-PERFORM

      * completa com zeros a esquerda as casas inteiras
      *
           INSPECT WS-VL2-INT13 REPLACING ALL ' ' BY '0'
      * completa com zeros a esquerda as casas decimais
      *
           MOVE    WS-VL-A-DCML2 TO        WS-VL2-DCML2
           INSPECT WS-VL2-DCML2 REPLACING ALL ' ' BY '0'
      *
           ADD   IT-INT                 TO       CNT-INT-VALOR
                 ON SIZE ERROR
                        MOVE      24    TO        AUX-ERRO
                        PERFORM   999000-ERRO
           END-ADD
           ADD   IT-DCML                TO       CNT-DCML-VALOR
                 ON SIZE ERROR
                        MOVE      25    TO        AUX-ERRO
                        PERFORM   999000-ERRO
           END-ADD

           MOVE WS-VALOR13V2            TO       SGTPB14S-VALOR
           MOVE AUX-FLAG-SNL-NGT        TO       AUX-SNL-NGT-VALOR

           .

       144000-SAI.
           EXIT.



      *-----------------------------------------------------------------
       145000-TRATA-TXDCR          SECTION.
      *-----------------------------------------------------------------

           MOVE    0007             TO    PSC-ERRO-PGM
           MOVE    ZERO             TO    IT-INT
           MOVE    SPACES           TO    WS-TXDCR-A
           MOVE    WS-CCT           TO    WS-CNT-CMP
           ADD    +1                TO    WS-CNT-CMP
                  ON SIZE ERROR
                        MOVE  26    TO    AUX-ERRO
                        PERFORM   999000-ERRO
           END-ADD

      *
           PERFORM UNTIL WS-REG-B(WS-CNT-CMP) EQUAL   ';'  OR
                         WS-CNT-CMP           GREATER  999
               ADD +1          TO         IT-INT
                   ON SIZE ERROR
                        MOVE      27     TO        AUX-ERRO
                        PERFORM   999000-ERRO
               END-ADD
               IF  IT-INT      NOT GREATER  40
                   MOVE   WS-REG-B(WS-CNT-CMP) TO WS-TXDCR-B(IT-INT)
               END-IF
               ADD +1          TO            WS-CNT-CMP
                   ON SIZE ERROR
                        MOVE      28     TO        AUX-ERRO
                        PERFORM   999000-ERRO
               END-ADD
           END-PERFORM

           EVALUATE WS-CNT-PNT-VRG
           WHEN 16
                 MOVE  WS-TXDCR-A        TO  SGTPB14S-TP-CAPTURA
                 MOVE  IT-INT            TO  CNT-CAPTURA
           WHEN 17
                 MOVE  WS-TXDCR-A        TO  SGTPB14S-TEXTO
                 MOVE  IT-INT            TO  CNT-TEXTO
      *
           END-EVALUATE
      *
           .

       145000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       146000-VERIFICA-REGISTRO          SECTION.
      *-----------------------------------------------------------------

           MOVE    0008             TO    PSC-ERRO-PGM
           MOVE    1                TO    IT-INT
           MOVE    SPACES           TO    AUX-REG-V
           MOVE    WS-CCT           TO    WS-CNT-CMP
           ADD    +1                TO    WS-CNT-CMP
                  ON SIZE ERROR
                        MOVE     29 TO    AUX-ERRO
                        PERFORM   999000-ERRO
           END-ADD
           PERFORM UNTIL WS-REG-B(WS-CNT-CMP) EQUAL  ';' OR
                         WS-CNT-CMP           GREATER    999
               IF  WS-REG-B(WS-CNT-CMP)       EQUAL  'V'
                   MOVE    'V'                TO     AUX-REG-V
                   MOVE     9000              TO     WS-CNT-CMP
               END-IF
               ADD         +1                TO     WS-CNT-CMP
                            ON SIZE ERROR
                               MOVE      30  TO     AUX-ERRO
                               PERFORM   999000-ERRO
               END-ADD
           END-PERFORM
           IF       AUX-REG-V           NOT EQUAL     'V'
                    MOVE  9900               TO     WS-CCT
           END-IF

           .

       146000-SAI.
           EXIT.
      *
      *-----------------------------------------------------------------
       147000-TRATA-TP-REG          SECTION.
      *-----------------------------------------------------------------

           MOVE 0009             TO   PSC-ERRO-PGM

           MOVE    ZEROS         TO    IT-INT
           MOVE    SPACES        TO    WS-TP-REG WS-TP-REG-NUM
           MOVE    WS-CCT        TO    WS-CNT-CMP
           ADD     +1            TO    WS-CNT-CMP
                   ON SIZE ERROR
                   MOVE      31   TO    AUX-ERRO
                   PERFORM   999000-ERRO
           END-ADD
      *
           PERFORM UNTIL WS-REG-B(WS-CNT-CMP) EQUAL    ';' OR
                         WS-CNT-CMP           GREATER  999
             ADD     +1                       TO    IT-INT
                     ON SIZE ERROR
                      MOVE      32  TO      AUX-ERRO
                      PERFORM   999000-ERRO
             END-ADD
             IF    IT-INT                     NOT GREATER  3
                   MOVE  WS-REG-B(WS-CNT-CMP) TO    WS-TP-REG-R(IT-INT)
             END-IF

             ADD     +1                       TO   WS-CNT-CMP
                     ON SIZE ERROR
                      MOVE     33   TO      AUX-ERRO
                      PERFORM   999000-ERRO
             END-ADD
           END-PERFORM
      *
              MOVE      3               TO IC-1
              MOVE      3               TO IC-2
              PERFORM   UNTIL  IC-1     EQUAL 0
               IF WS-TP-REG-R(IC-1)     NOT EQUAL  ' '
                  MOVE WS-TP-REG-R(IC-1) TO WS-TP-REG-N(IC-2)
                  SUBTRACT 1            FROM IC-2
                           ON SIZE ERROR
                              MOVE      34     TO        AUX-ERRO
                              PERFORM   999000-ERRO
                  END-SUBTRACT
               END-IF
               SUBTRACT 1 FROM IC-1
                        ON SIZE ERROR
                        MOVE      35     TO        AUX-ERRO
                        PERFORM   999000-ERRO
               END-SUBTRACT
               END-PERFORM
                INSPECT WS-TP-REG-NUM   REPLACING ALL ' ' BY '0'
                MOVE    WS-TP-REG-NUM   TO    SGTPB14S-TP-REG

      *
           MOVE IT-INT     TO CNT-TP-REG
           MOVE WS-TP-REG-NUM  TO SGTPB14S-TP-REG

           .

       147000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       148000-TRATA-LACTO         SECTION.
      *-----------------------------------------------------------------

           MOVE 0010             TO   PSC-ERRO-PGM

           MOVE    ZEROS         TO    IT-INT
           MOVE    SPACES        TO    WS-TP-LACTO
           MOVE    WS-CCT        TO    WS-CNT-CMP
           ADD     +1            TO    WS-CNT-CMP
                   ON SIZE ERROR
                   MOVE      36  TO    AUX-ERRO
                   PERFORM   999000-ERRO
           END-ADD
      *
           PERFORM UNTIL WS-REG-B(WS-CNT-CMP) EQUAL    ';' OR
                         WS-CNT-CMP           GREATER  999
             ADD     +1                       TO    IT-INT
                     ON SIZE ERROR
                      MOVE      37   TO      AUX-ERRO
                      PERFORM   999000-ERRO
             END-ADD
             IF    IT-INT                     NOT GREATER  1
                   MOVE  WS-REG-B(WS-CNT-CMP) TO    WS-TP-LACTO
             END-IF

             ADD     +1                       TO   WS-CNT-CMP
                     ON SIZE ERROR
                      MOVE     38   TO      AUX-ERRO
                      PERFORM   999000-ERRO
             END-ADD
           END-PERFORM
           MOVE IT-INT        TO CNT-TP-LACTO
           MOVE WS-TP-LACTO   TO SGTPB14S-TP-LACTO

           .

       148000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       149000-TRATA-CARTEIRA          SECTION.
      *-----------------------------------------------------------------

           MOVE 0011             TO   PSC-ERRO-PGM

           MOVE    ZEROS         TO    IT-INT
           MOVE    SPACES        TO    WS-CARTEIRA WS-CARTEIRA-NUM
           MOVE    WS-CCT        TO    WS-CNT-CMP
           ADD     +1            TO    WS-CNT-CMP
                   ON SIZE ERROR
                   MOVE      39   TO    AUX-ERRO
                   PERFORM   999000-ERRO
           END-ADD
      *
           PERFORM UNTIL WS-REG-B(WS-CNT-CMP) EQUAL    ';' OR
                         WS-CNT-CMP           GREATER  999
             ADD     +1                       TO    IT-INT
                     ON SIZE ERROR
                      MOVE      40  TO      AUX-ERRO
                      PERFORM   999000-ERRO
             END-ADD
             IF    IT-INT                     NOT GREATER  9
                   MOVE  WS-REG-B(WS-CNT-CMP) TO   WS-CARTEIRA-A(IT-INT)
             END-IF

             ADD     +1                       TO   WS-CNT-CMP
                     ON SIZE ERROR
                      MOVE     41   TO      AUX-ERRO
                      PERFORM   999000-ERRO
             END-ADD
           END-PERFORM
      *
              MOVE      9               TO IC-1
              MOVE      9               TO IC-2
              PERFORM   UNTIL  IC-1     EQUAL 0
               IF WS-CARTEIRA-A(IC-1)   NOT EQUAL  ' '
                  MOVE WS-CARTEIRA-A(IC-1) TO WS-CARTEIRA-N(IC-2)
                  SUBTRACT 1            FROM IC-2
                           ON SIZE ERROR
                              MOVE      42     TO        AUX-ERRO
                              PERFORM   999000-ERRO
                  END-SUBTRACT
               END-IF
               SUBTRACT 1 FROM IC-1
                        ON SIZE ERROR
                        MOVE      43     TO        AUX-ERRO
                        PERFORM   999000-ERRO
               END-SUBTRACT
               END-PERFORM
                INSPECT WS-CARTEIRA-NUM   REPLACING ALL ' ' BY '0'
      *
           MOVE IT-INT     TO CNT-CARTEIRA
           MOVE WS-CARTEIRA-NUM  TO SGTPB14S-CARTEIRA

           .

       149000-SAI.
           EXIT.



      *-----------------------------------------------------------------
       150000-TRATA-NRREG          SECTION.
      *-----------------------------------------------------------------

           MOVE 0012          TO   PSC-ERRO-PGM

           IF WS-REG-B(1) NOT EQUAL SPACES
              MOVE    ZEROS   TO    IT-INT
              MOVE    SPACES  TO    WS-NRREG-A WS-NRREG-A-ALFA
              MOVE    WS-CCT  TO    WS-CNT-CMP
              PERFORM UNTIL WS-REG-B(WS-CNT-CMP) EQUAL   ';'  OR
                         WS-CNT-CMP           GREATER  999
               ADD    +1                   TO IT-INT
                      ON SIZE ERROR
                        MOVE      44       TO AUX-ERRO
                        PERFORM   999000-ERRO
               END-ADD

               IF IT-INT            NOT GREATER  9
                  MOVE   WS-REG-B(WS-CNT-CMP) TO WS-NRREG-B(IT-INT)
               END-IF
               ADD     +1                  TO WS-CNT-CMP
                       ON SIZE ERROR
                        MOVE      45       TO AUX-ERRO
                        PERFORM   999000-ERRO
               END-ADD
              END-PERFORM
              MOVE     IT-INT           TO CNT-NRREG

      * inverte a posição
              MOVE      9               TO IC-1
              MOVE      9               TO IC-2
              PERFORM   UNTIL  IC-1     EQUAL 0
               IF WS-NRREG-B(IC-1)      NOT EQUAL  ' '
                  MOVE WS-NRREG-B(IC-1) TO WS-NRREG-B-ALFA(IC-2)
                  SUBTRACT 1            FROM IC-2
                           ON SIZE ERROR
                              MOVE      46     TO        AUX-ERRO
                              PERFORM   999000-ERRO
                  END-SUBTRACT
               END-IF
               SUBTRACT 1 FROM IC-1
                        ON SIZE ERROR
                        MOVE      47     TO        AUX-ERRO
                        PERFORM   999000-ERRO
               END-SUBTRACT
               END-PERFORM
                INSPECT WS-NRREG-A-ALFA   REPLACING ALL ' ' BY '0'
                MOVE    WS-NRREG-A-ALFA   TO    SGTPB14S-NR-REG

            END-IF

           .

       150000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-VERIFICA-DADOS          SECTION.
      *-----------------------------------------------------------------

           MOVE 0013                    TO     PSC-ERRO-PGM
           IF       (SGTPB14S-TP-REG    NOT EQUAL '002') OR
                    (AUX-REG-V          NOT EQUAL  'V')  OR
                    (SGTPB14S-TITULO    EQUAL      'RESGA') OR
                    (SGTPB14S-TITULO    EQUAL      'EMITI')
                     ADD +1             TO     CNT-DESPREZADOS
                         ON SIZE ERROR
                          MOVE      48  TO     AUX-ERRO
                          PERFORM   999000-ERRO
                     END-ADD
           ELSE
                     PERFORM 110000-CONSISTE
           END-IF

           .

       100000-SAI.
           EXIT.


      *-----------------------------------------------------------------
       110000-CONSISTE               SECTION.
      *-----------------------------------------------------------------

           MOVE 0014                     TO     PSC-ERRO-PGM
           MOVE SPACES                   TO     IN-ERRO-VALIDACAO

      *
      *             verifica nr. registro
      *
           IF       SGTPB14S-NR-REG       IS NOT      NUMERIC
                    DISPLAY '----------------------------------'
                    DISPLAY CTE-PROG ' NR.REG NAO NUMERICO - '
                    SGTPB14S-NR-REG
                    DISPLAY CTE-PROG 'LINHA DO REGISTRO INCONSISTENTE'
                    ' - '  CNT-LIDOS
                    DISPLAY CTE-PROG 'PSC-ERRO:' PSC-ERRO-PGM
                    MOVE   'S'            TO          CH-ABEND
                    SET ERRO-VALIDACAO    TO          TRUE
           END-IF
           IF       CNT-NRREG             GREATER     9
                    DISPLAY '----------------------------------'
                    DISPLAY CTE-PROG ' TAMANHO DO NR.REG MAIOR QUE O'
                    ' ESPERADO - ' CNT-NRREG
                    DISPLAY CTE-PROG 'LINHA DO REGISTRO INCONSISTENTE'
                    ' - '  CNT-LIDOS
                    DISPLAY CTE-PROG 'PSC-ERRO:' PSC-ERRO-PGM
                    MOVE   'S'            TO          CH-ABEND
                    SET ERRO-VALIDACAO    TO          TRUE
           END-IF
      *             verifica data de emissão
      *
           IF       SGTPB14S-DT-MOVTO     IS NOT      NUMERIC
                    DISPLAY '----------------------------------'
                    DISPLAY CTE-PROG ' DT.MOVTO NAO NUMERICA - '
                    SGTPB14S-DT-MOVTO
                    DISPLAY CTE-PROG 'LINHA DO REGISTRO INCONSISTENTE'
                    ' - '  CNT-LIDOS
                    DISPLAY CTE-PROG 'PSC-ERRO:' PSC-ERRO-PGM
                    MOVE   'S'            TO          CH-ABEND
                    SET ERRO-VALIDACAO    TO          TRUE
           END-IF
           IF       CNT-DT-MOVTO           GREATER     8
                    DISPLAY '----------------------------------'
                    DISPLAY CTE-PROG ' TAMANHO DA DT.MOVTO MAIOR QUE'
                    ' O ESPERADO - ' CNT-DT-MOVTO
                    DISPLAY CTE-PROG 'LINHA DO REGISTRO INCONSISTENTE'
                    ' - '  CNT-LIDOS
                    DISPLAY CTE-PROG 'PSC-ERRO:' PSC-ERRO-PGM
                    MOVE   'S'            TO          CH-ABEND
                    SET ERRO-VALIDACAO    TO          TRUE
           END-IF
      *             verifica titulo
      *
           IF       CNT-TIT               GREATER     12
                    DISPLAY '----------------------------------'
                    DISPLAY CTE-PROG ' TAMANHO DO TITULO MAIOR QUE'
                    ' O ESPERADO - ' CNT-TIT
                    DISPLAY CTE-PROG 'LINHA DO REGISTRO INCONSISTENTE'
                    ' - '  CNT-LIDOS
                    DISPLAY CTE-PROG 'PSC-ERRO:' PSC-ERRO-PGM
                    MOVE   'S'            TO          CH-ABEND
                    SET ERRO-VALIDACAO    TO          TRUE
           END-IF
      *             verifica quantidade de papeis
      *
           IF       SGTPB14S-QT-PAPL      IS NOT      NUMERIC
                    DISPLAY '----------------------------------'
                    DISPLAY CTE-PROG ' QTD. PAPEIS NAO NUMERICO - '
                    SGTPB14S-QT-PAPL
                    DISPLAY CTE-PROG 'LINHA DO REGISTRO INCONSISTENTE'
                    ' - '  CNT-LIDOS
                    DISPLAY CTE-PROG 'PSC-ERRO:' PSC-ERRO-PGM
                    MOVE   'S'            TO          CH-ABEND
                    SET ERRO-VALIDACAO    TO          TRUE
           END-IF
           IF       CNT-INT-PAPL              GREATER     15 OR
                    CNT-DCML-PAPL             GREATER     14
                    DISPLAY '----------------------------------'
                    DISPLAY CTE-PROG ' TAMANHO DA QTD. PAPEIS MAIOR '
                    ' QUE O ESPERADO '
                    DISPLAY CTE-PROG ' INTEIRAS - ' CNT-INT-PAPL ' '
                    DISPLAY CTE-PROG ' DECIMAIS - ' CNT-DCML-PAPL
                    DISPLAY CTE-PROG 'LINHA DO REGISTRO INCONSISTENTE'
                    ' - '  CNT-LIDOS
                    DISPLAY CTE-PROG 'PSC-ERRO:' PSC-ERRO-PGM
                    MOVE   'S'            TO          CH-ABEND
                    SET ERRO-VALIDACAO    TO          TRUE
           END-IF
      *  verifica tamanho do tipo de lançamento
           IF       CNT-TP-LACTO          GREATER     1
                    DISPLAY '----------------------------------'
                    DISPLAY CTE-PROG ' TAMANHO DO TP.LANÇAMENTO MAIOR '
                    ' QUE O ESPERADO ' CNT-TP-LACTO
                    DISPLAY CTE-PROG ' DECIMAIS - ' CNT-TP-LACTO
                    DISPLAY CTE-PROG 'LINHA DO REGISTRO INCONSISTENTE'
                    ' - '  CNT-LIDOS
                    DISPLAY CTE-PROG 'PSC-ERRO:' PSC-ERRO-PGM
                    MOVE   'S'            TO          CH-ABEND
                    SET ERRO-VALIDACAO    TO          TRUE
           END-IF

      *             verifica valor
      *
           IF       SGTPB14S-VALOR        IS NOT     NUMERIC
                    DISPLAY '----------------------------------'
                    DISPLAY CTE-PROG ' VALOR NAO NUMERICO - '
                    SGTPB14S-VALOR
                    DISPLAY CTE-PROG 'LINHA DO REGISTRO INCONSISTENTE'
                    ' - '  CNT-LIDOS
                    DISPLAY CTE-PROG 'PSC-ERRO:' PSC-ERRO-PGM
                    MOVE   'S'            TO          CH-ABEND
                    SET ERRO-VALIDACAO    TO          TRUE
           END-IF
           IF       CNT-INT-VALOR         GREATER    13 OR
                    CNT-DCML-VALOR        GREATER    2
                    DISPLAY '----------------------------------'
                    DISPLAY CTE-PROG ' TAMANHO DO VALOR MAIOR '
                    ' QUE O ESPERADO  '
                    DISPLAY CTE-PROG ' INTEIRAS - ' CNT-INT-VALOR ' '
                    DISPLAY CTE-PROG ' DECIMAIS - ' CNT-DCML-VALOR
                    DISPLAY CTE-PROG 'LINHA DO REGISTRO INCONSISTENTE'
                    ' - '  CNT-LIDOS
                    DISPLAY CTE-PROG 'PSC-ERRO:' PSC-ERRO-PGM
                    MOVE   'S'            TO          CH-ABEND
                    SET ERRO-VALIDACAO    TO          TRUE
           END-IF
      *             verifica tipo captura
      *
           IF       CNT-CAPTURA            GREATER    40
                    DISPLAY '----------------------------------'
                    DISPLAY CTE-PROG ' TAMANHO DO TIPO CAPTURA MAIOR '
                    ' QUE O ESPERADO - ' CNT-CAPTURA
                     DISPLAY CTE-PROG 'LINHA DO REGISTRO INCONSISTENTE'
                    ' - '  CNT-LIDOS
                    DISPLAY CTE-PROG 'PSC-ERRO:' PSC-ERRO-PGM
                    MOVE   'S'            TO          CH-ABEND
                    SET ERRO-VALIDACAO    TO          TRUE
           END-IF
      *             verifica data de aplicação
      *
           IF       CNT-TEXTO              GREATER    40
                    DISPLAY '----------------------------------'
                    DISPLAY CTE-PROG ' TAMANHO DA DESCRICAO MAIOR'
                    ' QUE O ESPERADO - ' CNT-TEXTO
                    DISPLAY CTE-PROG 'LINHA DO REGISTRO INCONSISTENTE'
                    ' - '  CNT-LIDOS
                    DISPLAY CTE-PROG 'PSC-ERRO:' PSC-ERRO-PGM
                    MOVE   'S'            TO          CH-ABEND
                    SET ERRO-VALIDACAO    TO          TRUE
           END-IF
      * verifica carteira
      *
           IF       SGTPB14S-CARTEIRA     IS NOT      NUMERIC
                    DISPLAY '----------------------------------'
                    DISPLAY CTE-PROG ' COD. CARTEIRA NÃO NUMÉRICO - '
                    SGTPB14S-CARTEIRA
                    DISPLAY CTE-PROG 'LINHA DO REGISTRO INCONSISTENTE'
                    ' - '  CNT-LIDOS
                    DISPLAY CTE-PROG 'PSC-ERRO:' PSC-ERRO-PGM
                    MOVE   'S'            TO          CH-ABEND
                    SET ERRO-VALIDACAO    TO          TRUE
           END-IF


           IF       CNT-CARTEIRA           GREATER    9
                    DISPLAY '----------------------------------'
                    DISPLAY CTE-PROG ' TAMANHO DA CARTEIRA MAIOR'
                    ' QUE O ESPERADO - ' CNT-CARTEIRA
                    DISPLAY CTE-PROG 'LINHA DO REGISTRO INCONSISTENTE'
                    ' - '  CNT-LIDOS
                    DISPLAY CTE-PROG 'PSC-ERRO:' PSC-ERRO-PGM
                    MOVE   'S'            TO          CH-ABEND
                    SET ERRO-VALIDACAO    TO          TRUE
           END-IF

      *
            IF       ERRO-VALIDACAO
                     ADD +1               TO        CNT-REJEITADOS
                         ON SIZE ERROR
                         MOVE      49     TO        AUX-ERRO
                         PERFORM   999000-ERRO
                     END-ADD
            ELSE
             IF      (SGTPB14S-NR-REG-R     NOT EQUAL 0)          OR
                     (SGTPB14S-DT-MOVTO     NOT EQUAL '00000000') OR
                     (SGTPB14S-TITULO       NOT EQUAL SPACES)     OR
                     (SGTPB14S-QT-PAPL-R    NOT EQUAL ZEROS)      OR
                     (SGTPB14S-TP-REG       NOT EQUAL SPACES)     OR
                     (SGTPB14S-TP-LACTO     NOT EQUAL SPACES)     OR
                     (SGTPB14S-VALOR-R      NOT EQUAL ZEROS)      OR
                     (SGTPB14S-TP-CAPTURA   NOT EQUAL SPACES)     OR
                     (SGTPB14S-TEXTO        NOT EQUAL SPACES)     OR
                     (SGTPB14S-CARTEIRA     NOT EQUAL '000000')
                     PERFORM  120000-GRAVA-REGISTRO
             ELSE
                      ADD +1             TO     CNT-DESPREZADOS
                         ON SIZE ERROR
                            MOVE      50 TO        AUX-ERRO
                            PERFORM   999000-ERRO
                     END-ADD
             END-IF
            END-IF
           .

       110000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       120000-GRAVA-REGISTRO                SECTION.
      *-----------------------------------------------------------------
      *
           MOVE   0015                            TO PSC-ERRO-PGM
           IF     AUX-SNL-NGT-PAPL       EQUAL    'S'
                  MULTIPLY SGTPB14S-QT-PAPL-R      BY  -1
                            GIVING                 SGTPB14S-QT-PAPL-R
                            ON SIZE ERROR
                               MOVE      51     TO        AUX-ERRO
                               PERFORM   999000-ERRO
                  END-MULTIPLY
           END-IF
      *
           IF     AUX-SNL-NGT-VALOR       EQUAL    'S'
                  MULTIPLY SGTPB14S-VALOR-R      BY  -1
                            GIVING                 SGTPB14S-VALOR-R
                            ON SIZE ERROR
                               MOVE      52     TO        AUX-ERRO
                               PERFORM   999000-ERRO
                  END-MULTIPLY
           END-IF
      *

      *
           WRITE  SGTF014S-REG-FD               FROM SGTPB14S-DADOS
      *
           ADD    +1             TO     CNT-GRAVADOS
                  ON SIZE ERROR
                     MOVE      53     TO        AUX-ERRO
                     PERFORM   999000-ERRO
           END-ADD
           .

       120000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       900100-EXIBIR-RESUMO-PROCESS        SECTION.
      *-----------------------------------------------------------------

           MOVE 0016                     TO PSC-ERRO-PGM

           CLOSE     SGTFDRV1
                     SGTF014S

           IF CNT-LIDOS    EQUAL       ZEROS OR
              CNT-GRAVADOS EQUAL       ZEROS
              DISPLAY '001 - NÃO HÁ REGISTROS PARA INTERNALIZAR'
              DISPLAY '001 - ' CTE-PROG
              DISPLAY '001 - QTDE. REG. LIDOS ......: ' CNT-LIDOS
              DISPLAY '001 - QTDE. REG. DESPREZADOS.: ' CNT-DESPREZADOS
              DISPLAY '001 - QTDE. REG. REJEITADOS..: ' CNT-REJEITADOS
              DISPLAY '001 - QTDE. REG. GRAVADOS....: ' CNT-GRAVADOS
              DISPLAY '------------------------------------------------'
              MOVE    004 TO          RETURN-CODE
              DISPLAY '999 ' CTE-PROG '999 - FIM-NORMAL'
           ELSE
            IF (AUX-ERRO   NOT EQUAL   ZEROS) OR
               (CH-ABEND   EQUAL       'S')
              DISPLAY '------------------------------------------------'
              DISPLAY '888 - ' CTE-PROG
              DISPLAY '888 - QTDE. REG. LIDOS ......: ' CNT-LIDOS
              DISPLAY '888 - QTDE. REG. DESPREZADOS.: ' CNT-DESPREZADOS
              DISPLAY '888 - QTDE. REG. REJEITADOS..: ' CNT-REJEITADOS
              DISPLAY '888 - QTDE. REG. GRAVADOS....: ' CNT-GRAVADOS
              DISPLAY '------------------------------------------------'
              CALL SBABEND
            ELSE
              DISPLAY '------------------------------------------------'
              DISPLAY '002 - ' CTE-PROG
              DISPLAY '002 - QTDE. REG. LIDOS ......: ' CNT-LIDOS
              DISPLAY '002 - QTDE. REG. DESPREZADOS.: ' CNT-DESPREZADOS
              DISPLAY '002 - QTDE. REG. REJEITADOS..: ' CNT-REJEITADOS
              DISPLAY '002 - QTDE. REG. GRAVADOS....: ' CNT-GRAVADOS
              DISPLAY '------------------------------------------------'
              DISPLAY '999 ' CTE-PROG '999 - FIM-NORMAL'
              MOVE ZEROS  TO          RETURN-CODE
            END-IF
           END-IF

           .

       900100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999000-ERRO SECTION.
      *-----------------------------------------------------------------
      *
           DISPLAY '888 - ' CTE-PROG
           DISPLAY '888 - Erro em operação aritmética'
           DISPLAY '888 - Ocorreu o erro ' AUX-ERRO ' no processamento'
           DISPLAY '888 - Resumo até a ocorrência: '
           PERFORM 900100-EXIBIR-RESUMO-PROCESS

           .
       999000-ERRO-SAI.
           EXIT.
      *--------------------
      *---------------- FIM SGTPB014 ----------------------------