      *-----------------------------------------------------------------
      * PROGRAMA..: SGTPB026
      * ANALISTA..: PEDRO ARGOLO LACERDA
      * AUTOR.....: PEDRO ARGOLO LACERDA
      * DATA......: 15/08/2019
      * OBJETIVO..: Programa batch para reformatar arquivo de posição
      *             mensal do rating referencial fornecido pela DIRIS.
      * COMPILACAO: COBOL
      *-----------------------------------------------------------------
      * VRS001 16/08/2017 - F8214779 - DE 2019/145195 - IMPLANTACAO
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTPB026.
      *-----------------------------------------------------------------
        ENVIRONMENT DIVISION.
      *-----------------------------------------------------------------

       CONFIGURATION SECTION.
        SPECIAL-NAMES.
            DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT                SECTION.
      *
       FILE-CONTROL.
           SELECT SGTFDIR1  ASSIGN  TO  SGTFDIR1.
           SELECT SGTF999S  ASSIGN  TO  SGTF999S.
      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------
        FILE                        SECTION.
      *
       FD  SGTFDIR1
           RECORDING F
           RECORD 1000
           BLOCK  0.
       01  REG-FD-ARQ1        PIC X(1000).
      *
       FD  SGTF999S
           RECORDING F
           RECORD 200
           BLOCK 0.
       01  SGTF999S-REG-FD    PIC X(200).
      *
       WORKING-STORAGE SECTION.
       77  CTE-INICIO-SS    PIC  X(40) VALUE 'inicio'.
       77  CTE-PROG         PIC  X(17) VALUE '*** SGTPB026 ***'.
       77  CTE-VERS         PIC  X(6)  VALUE 'VRS001'.
      *
      * SUBROTINAS UTILIZADAS
      *
       77  SBABEND  VALUE 'SBABEND'    PIC  X(008).
       77  SBVERSAO VALUE 'SBVERSAO'   PIC  X(008).
       77  SBDATA   VALUE 'SBDATA'     PIC  X(008).
      *----------------------------------------------------------------
         LOCAL-STORAGE SECTION.
      *----------------------------------------------------------------

      * BOOKS DA DEFINICAO DE DADOS A SEREM USADOS NA SUBROTINA DE ERRos
       COPY HLPKE001.
      * HLPKDFHE - DEFINICAO DA AREA DFHEIBLK
       COPY HLPKDFHE.

       01 CNT-LIDOS                      PIC S9(004) COMP-5 VALUE 0.
       01 CNT-REJEITADOS                 PIC S9(004) COMP-5 VALUE 0.
       01 CNT-GRAVADOS                   PIC S9(004) COMP-5 VALUE 0.
       01 CNT-DESPREZADOS                PIC S9(004) COMP-5 VALUE 0.

      * OS CONTADORES ABAIXO SERVEM PARA FORNECER O TAMANHO DOS CAMPOS
      * DO ARQUIVO DE ENTRADA. CASO ALGUM DELES VIEREM
      * PREVISTO O PGM IR? ABENDAR.
      *
      * Ìndices
       01 IT-INT                         PIC  9(04) COMP-5.
       01 CH-ABEND                       PIC  X(001) VALUE 'N'.
       01 AUX-ERRO                       PIC  9(003) VALUE ZEROS.
       01  IN-ERRO-VALIDACAO             PIC  X(01)  VALUE SPACES.
                 88  ERRO-VALIDACAO                    VALUE 'S'.
      *
       01 WS-REG-A.
             05 WS-REG-B                   PIC  X(1) OCCURS 1000 TIMES.
      *
       01 WS-ATI-A.
            05 WS-ATI-B         PIC X(1) OCCURS 12 TIMES.

       01 WS-ATI-A-ALFA.
          05 WS-ATI-B-ALFA      PIC X(1) OCCURS 12 TIMES.
      *
        01 WS-MCI-A.
            05 WS-MCI-B         PIC X(1) OCCURS 9 TIMES.
      *
        01 WS-MCI-A-ALFA.
            05 WS-MCI-B-ALFA    PIC X(1) OCCURS 9 TIMES.
      *
        01 WS-EMISSOR-A.
            05 WS-EMISSOR-B         PIC X(1) OCCURS 60 TIMES.
      *
        01 WS-EMISSOR-A-ALFA.
            05 WS-EMISSOR-B-ALFA    PIC X(1) OCCURS 60 TIMES.
      *
        01 WS-TIP-ATI-A.
            05 WS-TIP-ATI-B         PIC X(1) OCCURS 12 TIMES.
      *
        01 WS-TIP-ATI-A-ALFA.
            05 WS-TIP-ATI-B-ALFA    PIC X(1) OCCURS 50 TIMES.
      *
        01 WS-RATING1A-A.
            05 WS-RATING1A-B        PIC X(1) OCCURS  3 TIMES.
      *
        01 WS-RATING1A-A-ALFA.
            05 WS-RATING1A-B-ALFA   PIC X(1) OCCURS  3 TIMES.
      *
        01 WS-RATING23-A.
            05 WS-RATING23-B        PIC X(1) OCCURS  3 TIMES.
      *
        01 WS-RATING23-A-ALFA.
            05 WS-RATING23-B-ALFA   PIC X(1) OCCURS  3 TIMES.
      *
        01 WS-DT-VNCT-A.
            05 WS-DT-VNCT-B         PIC X(1) OCCURS  10 TIMES.
      *
        01 WS-DT-VNCT-A-ALFA.
            05 WS-DT-VNCT-B-ALFA    PIC X(1) OCCURS  10 TIMES.
      *
        01 WS-RSCO-CLCD-EMISSOR-A.
            05 WS-RSCO-CLCD-EMISSOR-B      PIC X(1) OCCURS   3 TIMES.
      *
        01 WS-RSCO-CLCD-EMISSOR-A-ALFA.
            05 WS-RSCO-CLCD-EMISSOR-B-ALFA PIC X(1) OCCURS   3 TIMES.
      *
        01 WS-RSCO-ATBD-EMISSOR-A.
            05 WS-RSCO-ATBD-EMISSOR-B      PIC X(1) OCCURS   3 TIMES.
      *
        01 WS-RSCO-ATBD-EMISSOR-A-ALFA.
            05 WS-RSCO-ATBD-EMISSOR-B-ALFA PIC X(1) OCCURS   3 TIMES.
      *
        01 WS-CD-GR-ECNC-A.
            05 WS-CD-GR-ECNC-B             PIC X(1) OCCURS   6 TIMES.
      *
        01 WS-CD-GR-ECNC-A-ALFA.
            05 WS-CD-GR-ECNC-B-ALFA        PIC X(1) OCCURS   6 TIMES.
      *
        01 WS-RSCO-ATBD-GRUPO-A.
            05 WS-RSCO-ATBD-GRUPO-B        PIC X(1) OCCURS   3 TIMES.
      *
        01 WS-RSCO-ATBD-GRUPO-A-ALFA.
            05 WS-RSCO-ATBD-GRUPO-B-ALFA   PIC X(1) OCCURS   3 TIMES.
      *
      * ÀREAS USADAS PARA RETIRAR CARACTER A CARACTER DA ENTRADA
       01 WS-CNT-PNT-VRG      PIC 9(04) COMP-5.
       01 WS-CNT-CMP          PIC 9(04) COMP-5.
       01 WS-CCT              PIC 9(04) COMP-5.
      * ELIMINA OS BRANCOS E ENCOSTA TUDO À DIREITA
       01 IC-1           PIC 9(03).
       01 IC-2           PIC 9(03).


       01  SGTPB26E-DADOS.
       COPY SGTKB26E.

       01  SGTPB26S-DADOS.
       COPY SGTKB26S.

      * CONTROLE DE LEITURA DO ARQUIVO DE ENTRADA
       01  CTL-LET-ARQ.
            03  IN-FIM-ARQ       PIC X(01)          VALUE SPACES.
                88  FIM-ARQ                         VALUE 'S'.


      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL SECTION.
      *-----------------------------------------------------------------

           MOVE 0001                   TO PSC-ERRO-PGM

           CALL  SBVERSAO  USING  CTE-PROG  CTE-VERS

           OPEN INPUT   SGTFDIR1
                OUTPUT  SGTF999S.

           PERFORM UNTIL FIM-ARQ

           INITIALIZE SGTPB26E-DADOS SGTPB26S-DADOS

           READ SGTFDIR1
               AT END
               MOVE 'S' TO IN-FIM-ARQ

               NOT AT END
               ADD  +1     TO CNT-LIDOS
                    ON SIZE ERROR
               MOVE 1 TO AUX-ERRO
                    PERFORM   999000-ERRO
               END-ADD
               IF   CNT-LIDOS  NOT EQUAL 1
               INITIALIZE  AUX-ERRO

               MOVE REG-FD-ARQ1 TO WS-REG-A

               MOVE ZEROS TO WS-CCT WS-CNT-PNT-VRG

               PERFORM 140000-TRATA-REGISTRO
                              UNTIL WS-CCT   GREATER 999

               PERFORM 100000-VERIFICA-DADOS
               END-IF
           END-READ

           END-PERFORM

           PERFORM 900100-EXIBIR-RESUMO-PROCESS

           STOP RUN
           .

       000000-SAI.
            EXIT.
      *-----------------------------------------------------------------
       100000-VERIFICA-DADOS          SECTION.
      *-----------------------------------------------------------------

           MOVE 0002                  TO     PSC-ERRO-PGM

           IF        SGTPB26S-ATI     NOT EQUAL  ' '
                     PERFORM  110000-GRAVA-REGISTRO
           ELSE
                     ADD +1             TO     CNT-DESPREZADOS
                         ON SIZE ERROR
                          MOVE       2  TO     AUX-ERRO
                          PERFORM   999000-ERRO
                     END-ADD
           END-IF
      *
           .
       100000-SAI.
            EXIT.
      *-----------------------------------------------------------------
       110000-GRAVA-REGISTRO                SECTION.
      *-----------------------------------------------------------------
      *

            MOVE   0003                 TO PSC-ERRO-PGM

            ADD     +1                  TO CNT-GRAVADOS
                   ON SIZE ERROR
                   MOVE       3       TO AUX-ERRO
                   PERFORM   999000-ERRO
            END-ADD

            WRITE  SGTF999S-REG-FD               FROM SGTPB26S-DADOS

            .

       110000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       140000-TRATA-REGISTRO          SECTION.
      *-----------------------------------------------------------------

            MOVE 0004                 TO   PSC-ERRO-PGM

            ADD    +1                 TO   WS-CCT
                   ON SIZE ERROR
                   MOVE     4         TO   AUX-ERRO
                   PERFORM   999000-ERRO
            END-ADD

            IF     WS-CCT             EQUAL   1
                   PERFORM 150000-TRATA-ATI
            END-IF

            IF     WS-REG-B(WS-CCT)   EQUAL   ';'
                   ADD   +1           TO      WS-CNT-PNT-VRG
                         ON SIZE ERROR
                         MOVE     5   TO      AUX-ERRO
                         PERFORM   999000-ERRO
                   END-ADD
             PERFORM 141000-DECIDE-TRATA-CAMPO
            END-IF
            .


       140000-SAI.
            EXIT.
      *-----------------------------------------------------------------
       141000-DECIDE-TRATA-CAMPO      SECTION.
      *-----------------------------------------------------------------

            MOVE 0005                     TO PSC-ERRO-PGM

            EVALUATE WS-CNT-PNT-VRG
              WHEN 01
                     PERFORM 151000-TRATA-MCI
              WHEN 02
                     PERFORM 152000-TRATA-EMISSOR
              WHEN 03
                     PERFORM 153000-TRATA-TIP-ATI
              WHEN 04
                     PERFORM 154000-RATING1A
              WHEN 05
                     PERFORM 155000-RATING-23
              WHEN 06
                     PERFORM 156000-DT-VNCT
              WHEN 07
                     PERFORM 157000-RSCO-CLCD-EMISSOR
              WHEN 08
                     PERFORM 158000-RSCO-ATBD-EMISSOR
              WHEN 09
                     PERFORM 159000-CD-GR-ECNC
              WHEN 10
                     PERFORM 160000-RSCO-ATBD-GRUPO
              END-EVALUATE
            .
       141000-SAI.
            EXIT.
      *-----------------------------------------------------------------
       150000-TRATA-ATI          SECTION.
      *-----------------------------------------------------------------

           MOVE 0006          TO   PSC-ERRO-PGM

           MOVE    ZEROS   TO    IT-INT
           MOVE    SPACES  TO    WS-ATI-A
           MOVE    WS-CCT  TO    WS-CNT-CMP
           PERFORM UNTIL WS-REG-B(WS-CNT-CMP) EQUAL   ';'  OR
                         WS-CNT-CMP           GREATER  1000
              ADD  +1        TO    IT-INT
                   ON SIZE ERROR
                   MOVE       6    TO    AUX-ERRO
                   PERFORM   999000-ERRO
              END-ADD

              IF    IT-INT    NOT GREATER    12
                   MOVE    WS-REG-B(WS-CNT-CMP)   TO    WS-ATI-B(IT-INT)
              END-IF
              ADD  +1        TO    WS-CNT-CMP
                   ON SIZE ERROR
                   MOVE       7    TO    AUX-ERRO
                   PERFORM   999000-ERRO
              END-ADD
           END-PERFORM

      * Alinha à esquerda
           MOVE      SPACES         TO WS-ATI-A-ALFA
           MOVE      1              TO IC-1
           MOVE      1              TO IC-2
           PERFORM   UNTIL  IC-1     > 12
             IF    WS-ATI-B(IC-1)      NOT EQUAL  ' '
                   MOVE    WS-ATI-B(IC-1)    TO    WS-ATI-B-ALFA(IC-2)
                   ADD  +1    TO    IC-2
                        ON SIZE ERROR
                        MOVE       8     TO        AUX-ERRO
                        PERFORM   999000-ERRO
                   END-ADD
             END-IF
             ADD  +1    TO    IC-1
                   ON SIZE ERROR
                   MOVE       9     TO        AUX-ERRO
                   PERFORM   999000-ERRO
             END-ADD
             END-PERFORM
             MOVE    WS-ATI-A-ALFA   TO    SGTPB26S-ATI

           .
       150000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       151000-TRATA-MCI         SECTION.
      *-----------------------------------------------------------------

           MOVE 0007          TO   PSC-ERRO-PGM

           MOVE    ZEROS   TO    IT-INT
           MOVE    SPACES  TO    WS-MCI-A
           MOVE    WS-CCT  TO    WS-CNT-CMP
           ADD    +1       TO    WS-CNT-CMP
               ON SIZE ERROR
               MOVE     10       TO    AUX-ERRO
               PERFORM   999000-ERRO
           END-ADD

           PERFORM UNTIL WS-REG-B(WS-CNT-CMP) EQUAL   ';'  OR
                         WS-CNT-CMP           GREATER  1000
           ADD    +1       TO     IT-INT
                  ON SIZE ERROR
                  MOVE     11     TO    AUX-ERRO
                  PERFORM   999000-ERRO
           END-ADD

           IF IT-INT       NOT GREATER    9
              MOVE   WS-REG-B(WS-CNT-CMP)    TO    WS-MCI-B(IT-INT)
           END-IF

           ADD    +1       TO     WS-CNT-CMP
                  ON SIZE ERROR
                  MOVE     12     TO    AUX-ERRO
                  PERFORM   999000-ERRO
           END-ADD
           END-PERFORM
      * Alinha à esquerda
           MOVE      SPACES         TO WS-MCI-A-ALFA
           MOVE      1              TO IC-1
           MOVE      1              TO IC-2
           PERFORM   UNTIL  IC-1     > 9
           IF    WS-MCI-B(IC-1)      NOT EQUAL  ' '
                 MOVE    WS-MCI-B(IC-1)    TO    WS-MCI-B-ALFA(IC-2)
                 ADD  +1    TO    IC-2
                       ON SIZE ERROR
                       MOVE    13     TO    AUX-ERRO
                       PERFORM 999000-ERRO
                 END-ADD
           END-IF
           ADD  +1    TO    IC-1
                 ON SIZE ERROR
                 MOVE    14    TO     AUX-ERRO
                 PERFORM   999000-ERRO
           END-ADD
           END-PERFORM
           MOVE    WS-MCI-A-ALFA    TO    SGTPB26S-MCI

           .
       151000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       152000-TRATA-EMISSOR     SECTION.
      *-----------------------------------------------------------------

           MOVE 0008          TO   PSC-ERRO-PGM

           MOVE    ZEROS   TO    IT-INT
           MOVE    SPACES  TO    WS-EMISSOR-A
           MOVE    WS-CCT  TO    WS-CNT-CMP
           ADD    +1       TO    WS-CNT-CMP

           PERFORM UNTIL WS-REG-B(WS-CNT-CMP) EQUAL   ';'  OR
                         WS-CNT-CMP           GREATER  1000
           ADD    +1                   TO IT-INT
                  ON SIZE ERROR
                  MOVE      15       TO AUX-ERRO
                  PERFORM   999000-ERRO
           END-ADD

           IF     IT-INT    NOT GREATER    60
              MOVE   WS-REG-B(WS-CNT-CMP)    TO    WS-EMISSOR-B(IT-INT)
           END-IF

           ADD     +1                  TO WS-CNT-CMP
                   ON SIZE ERROR
                   MOVE      16       TO AUX-ERRO
                   PERFORM   999000-ERRO
           END-ADD
           END-PERFORM

      * Alinha à esquerda
           MOVE      SPACES         TO WS-EMISSOR-A-ALFA
           MOVE      1              TO IC-1
           MOVE      1              TO IC-2
           PERFORM   UNTIL  IC-1     > 60
      *     IF    WS-EMISSOR-B(IC-1)      NOT EQUAL  ' '
                 MOVE WS-EMISSOR-B(IC-1) TO WS-EMISSOR-B-ALFA(IC-2)
                 ADD  +1    TO    IC-2
                      ON SIZE ERROR
                      MOVE      17     TO        AUX-ERRO
                      PERFORM   999000-ERRO
                 END-ADD
      *     END-IF
           ADD  +1    TO    IC-1
                 ON SIZE ERROR
                 MOVE      18     TO        AUX-ERRO
                 PERFORM   999000-ERRO
           END-ADD
           END-PERFORM
           MOVE    WS-EMISSOR-A-ALFA   TO   SGTPB26S-EMISSOR

           .
       152000-SAI.
           EXIT.



      *-----------------------------------------------------------------
       153000-TRATA-TIP-ATI     SECTION.
      *-----------------------------------------------------------------

           MOVE 0009          TO   PSC-ERRO-PGM

           MOVE    ZEROS   TO    IT-INT
           MOVE    SPACES  TO    WS-TIP-ATI-A
           MOVE    WS-CCT  TO    WS-CNT-CMP
           ADD    +1       TO    WS-CNT-CMP
           PERFORM UNTIL WS-REG-B(WS-CNT-CMP) EQUAL   ';'  OR
                         WS-CNT-CMP           GREATER  1000
           ADD    +1                   TO IT-INT
                  ON SIZE ERROR
                  MOVE      19       TO AUX-ERRO
                  PERFORM   999000-ERRO
           END-ADD
           IF    IT-INT    NOT GREATER    50
              MOVE    WS-REG-B(WS-CNT-CMP)    TO    WS-TIP-ATI-B(IT-INT)
           END-IF
           ADD     +1                  TO WS-CNT-CMP
                   ON SIZE ERROR
                   MOVE      20       TO AUX-ERRO
                   PERFORM   999000-ERRO
           END-ADD
           END-PERFORM
      * Alinha à esquerda
           MOVE      SPACES         TO WS-TIP-ATI-A-ALFA
           MOVE      1              TO IC-1
           MOVE      1              TO IC-2
           PERFORM   UNTIL  IC-1    > 50
      *     IF    WS-TIP-ATI-B(IC-1)      NOT EQUAL  ' '
                 MOVE WS-TIP-ATI-B (IC-1)   TO   WS-TIP-ATI-B-ALFA(IC-2)
                 ADD  +1    TO    IC-2
                      ON SIZE ERROR
                      MOVE      21     TO        AUX-ERRO
                      PERFORM   999000-ERRO
                 END-ADD
      *     END-IF
           ADD  +1    TO    IC-1
                 ON SIZE ERROR
                 MOVE      22     TO        AUX-ERRO
                 PERFORM   999000-ERRO
           END-ADD
           END-PERFORM
           MOVE    WS-TIP-ATI-A-ALFA   TO   SGTPB26S-TIP-ATI

           .
       153000-SAI.
           EXIT.


      *-----------------------------------------------------------------
       154000-RATING1A             SECTION.
      *-----------------------------------------------------------------

           MOVE 0010    TO   PSC-ERRO-PGM
           MOVE ZEROS   TO   IT-INT
           MOVE SPACES  TO   WS-RATING1A-A
           MOVE WS-CCT  TO   WS-CNT-CMP
           ADD  +1      TO   WS-CNT-CMP
                  ON SIZE ERROR
                  MOVE      23       TO AUX-ERRO
                  PERFORM   999000-ERRO
           END-ADD

           PERFORM UNTIL WS-REG-B(WS-CNT-CMP) EQUAL   ';'  OR
                         WS-CNT-CMP           GREATER  1000
           ADD    +1    TO    IT-INT
                  ON SIZE ERROR
                  MOVE      24       TO    AUX-ERRO
                  PERFORM   999000-ERRO
           END-ADD
           IF    IT-INT     NOT GREATER    04
              MOVE   WS-REG-B(WS-CNT-CMP)    TO    WS-RATING1A-B(IT-INT)
           END-IF
           ADD    +1    TO    WS-CNT-CMP
                  ON SIZE ERROR
                  MOVE      25       TO AUX-ERRO
                  PERFORM   999000-ERRO
           END-ADD
           END-PERFORM
      * Alinha à esquerda
           MOVE    SPACES     TO    WS-RATING1A-A-ALFA
           MOVE    1    TO    IC-1
           MOVE    1    TO    IC-2
           PERFORM   UNTIL  IC-1     > 4
           IF    WS-RATING1A-B(IC-1)      NOT EQUAL  ' '
                 MOVE  WS-RATING1A-B (IC-1) TO  WS-RATING1A-B-ALFA(IC-2)
                 ADD  +1    TO    IC-2
                      ON SIZE ERROR
                      MOVE      26     TO        AUX-ERRO
                      PERFORM   999000-ERRO
                 END-ADD
           END-IF
           ADD  +1    TO    IC-1
                 ON SIZE ERROR
                 MOVE      27     TO        AUX-ERRO
                 PERFORM   999000-ERRO
           END-ADD
           END-PERFORM
           MOVE    WS-RATING1A-A-ALFA    TO    SGTPB26S-RATING1A

           .
       154000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       155000-RATING-23          SECTION.
      *-----------------------------------------------------------------

           MOVE 0011          TO   PSC-ERRO-PGM

           MOVE ZEROS     TO    IT-INT
           MOVE SPACES    TO    WS-RATING23-A
           MOVE WS-CCT    TO    WS-CNT-CMP
           ADD  +1        TO    WS-CNT-CMP
                  ON SIZE ERROR
                  MOVE      28       TO AUX-ERRO
                  PERFORM   999000-ERRO
           END-ADD

           PERFORM UNTIL WS-REG-B(WS-CNT-CMP) EQUAL   ';'  OR
                            WS-CNT-CMP           GREATER  1000
           ADD    +1                   TO IT-INT
                  ON SIZE ERROR
                  MOVE      29       TO AUX-ERRO
                  PERFORM   999000-ERRO
           END-ADD
           IF    IT-INT     NOT GREATER    04
              MOVE   WS-REG-B(WS-CNT-CMP)    TO    WS-RATING23-B(IT-INT)
           END-IF
           ADD     +1                  TO WS-CNT-CMP
                   ON SIZE ERROR
                   MOVE      30       TO AUX-ERRO
                   PERFORM   999000-ERRO
           END-ADD
           END-PERFORM
      * Alinha à esquerda
           MOVE    SPACES     TO    WS-RATING23-A-ALFA
           MOVE    1    TO    IC-1
           MOVE    1    TO    IC-2
           PERFORM   UNTIL  IC-1     > 4
           IF    WS-RATING23-B(IC-1)      NOT EQUAL  ' '
                 MOVE WS-RATING23-B  (IC-1) TO WS-RATING23-B-ALFA(IC-2)
                 ADD  +1    TO    IC-2
                      ON SIZE ERROR
                      MOVE      31     TO        AUX-ERRO
                      PERFORM   999000-ERRO
                 END-ADD
           END-IF
           ADD  +1    TO    IC-1
                 ON SIZE ERROR
                 MOVE      32     TO        AUX-ERRO
                 PERFORM   999000-ERRO
           END-ADD
           END-PERFORM
           MOVE    WS-RATING23-A-ALFA    TO    SGTPB26S-RATING23

           .
       155000-SAI.
           EXIT.



      *-----------------------------------------------------------------
       156000-DT-VNCT            SECTION.
      *-----------------------------------------------------------------

           MOVE 0012          TO   PSC-ERRO-PGM
           MOVE ZEROS   TO    IT-INT
           MOVE SPACES  TO    WS-DT-VNCT-A
           MOVE WS-CCT  TO    WS-CNT-CMP
           ADD  +1      TO    WS-CNT-CMP

           PERFORM UNTIL WS-REG-B(WS-CNT-CMP) EQUAL   ';'  OR
                         WS-CNT-CMP           GREATER  1000
           ADD    +1                   TO IT-INT
                  ON SIZE ERROR
                  MOVE      33       TO AUX-ERRO
                  PERFORM   999000-ERRO
           END-ADD
           IF IT-INT            NOT GREATER  10
              MOVE   WS-REG-B(WS-CNT-CMP)    TO    WS-DT-VNCT-B(IT-INT)
           END-IF
           ADD     +1                  TO WS-CNT-CMP
                   ON SIZE ERROR
                   MOVE      34       TO AUX-ERRO
                   PERFORM   999000-ERRO
           END-ADD
           END-PERFORM
           MOVE WS-DT-VNCT-A   TO SGTPB26S-DT-VNCT
      * Alinha à esquerda
           MOVE    SPACES     TO    WS-DT-VNCT-A-ALFA
           MOVE    1    TO    IC-1
           MOVE    1    TO    IC-2
           PERFORM   UNTIL  IC-1     > 10
           IF    WS-DT-VNCT-B(IC-1)      NOT EQUAL  ' '
                 MOVE WS-DT-VNCT-B   (IC-1) TO WS-DT-VNCT-B-ALFA(IC-2)
                 ADD  +1    TO    IC-2
                      ON SIZE ERROR
                      MOVE      35     TO        AUX-ERRO
                      PERFORM   999000-ERRO
                 END-ADD
           END-IF
           ADD  +1    TO    IC-1
                 ON SIZE ERROR
                 MOVE      36     TO        AUX-ERRO
                 PERFORM   999000-ERRO
           END-ADD
           END-PERFORM
           MOVE    WS-DT-VNCT-A-ALFA    TO    SGTPB26S-DT-VNCT

           .
       156000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       157000-RSCO-CLCD-EMISSOR  SECTION.
      *-----------------------------------------------------------------

           MOVE 0013    TO    PSC-ERRO-PGM
           MOVE ZEROS   TO    IT-INT
           MOVE SPACES  TO    WS-RSCO-CLCD-EMISSOR-A
           MOVE WS-CCT  TO    WS-CNT-CMP
           ADD  +1      TO    WS-CNT-CMP
           PERFORM UNTIL WS-REG-B(WS-CNT-CMP) EQUAL   ';'  OR
                            WS-CNT-CMP           GREATER  1000
           ADD    +1                   TO IT-INT
                  ON SIZE ERROR
                  MOVE      37       TO AUX-ERRO
                  PERFORM   999000-ERRO
           END-ADD
           IF    IT-INT     NOT GREATER    03
            MOVE WS-REG-B(WS-CNT-CMP) TO WS-RSCO-CLCD-EMISSOR-B (IT-INT)
           END-IF
           ADD     +1                  TO WS-CNT-CMP
                   ON SIZE ERROR
                   MOVE      38       TO AUX-ERRO
                   PERFORM   999000-ERRO
           END-ADD
           END-PERFORM
      * Alinha à esquerda
           MOVE    SPACES     TO    WS-RSCO-CLCD-EMISSOR-A-ALFA
           MOVE    1    TO    IC-1
           MOVE    1    TO    IC-2
           PERFORM   UNTIL  IC-1     > 03
           IF   WS-RSCO-CLCD-EMISSOR-B(IC-1)      NOT EQUAL  ' '
                 MOVE    WS-RSCO-CLCD-EMISSOR-B    (IC-1)
                            TO    WS-RSCO-CLCD-EMISSOR-B-ALFA     (IC-2)
                 ADD  +1    TO    IC-2
                      ON SIZE ERROR
                      MOVE      39     TO        AUX-ERRO
                      PERFORM   999000-ERRO
                 END-ADD
           END-IF
           ADD  +1    TO    IC-1
                 ON SIZE ERROR
                 MOVE      40     TO        AUX-ERRO
                 PERFORM   999000-ERRO
           END-ADD
           END-PERFORM
           MOVE  WS-RSCO-CLCD-EMISSOR-A-ALFA
                                  TO    SGTPB26S-RSCO-CLCD-EMISSOR

           .
       157000-SAI.
           EXIT.



      *-----------------------------------------------------------------
       158000-RSCO-ATBD-EMISSOR  SECTION.
      *-----------------------------------------------------------------

           MOVE 0014    TO   PSC-ERRO-PGM
           MOVE ZEROS   TO   IT-INT
           MOVE SPACES  TO   WS-RSCO-ATBD-EMISSOR-A
           MOVE WS-CCT  TO   WS-CNT-CMP
           ADD  +1      TO   WS-CNT-CMP
                     ON SIZE ERROR
           MOVE      41       TO AUX-ERRO
           PERFORM   999000-ERRO
           END-ADD
           PERFORM UNTIL WS-REG-B(WS-CNT-CMP) EQUAL   ';'  OR
                         WS-CNT-CMP           GREATER  1000
           ADD    +1                   TO IT-INT
                  ON SIZE ERROR
                  MOVE      42       TO AUX-ERRO
                  PERFORM   999000-ERRO
           END-ADD
           IF IT-INT            NOT GREATER  03
            MOVE WS-REG-B(WS-CNT-CMP) TO WS-RSCO-ATBD-EMISSOR-B (IT-INT)
           END-IF
           ADD     +1                  TO WS-CNT-CMP
                   ON SIZE ERROR
                   MOVE      43       TO AUX-ERRO
                   PERFORM   999000-ERRO
           END-ADD
           END-PERFORM
      * Alinha à esquerda
           MOVE    SPACES     TO     WS-RSCO-ATBD-EMISSOR-A-ALFA
           MOVE    1    TO    IC-1
           MOVE    1    TO    IC-2
           PERFORM   UNTIL    IC-1     >    03
           IF    WS-RSCO-ATBD-EMISSOR-B(IC-1)      NOT EQUAL  ' '
                 MOVE    WS-RSCO-ATBD-EMISSOR-B    (IC-1)
                            TO    WS-RSCO-ATBD-EMISSOR-B-ALFA     (IC-2)
                 ADD  +1    TO    IC-2
                      ON SIZE ERROR
                      MOVE      44     TO        AUX-ERRO
                      PERFORM   999000-ERRO
                 END-ADD
           END-IF
           ADD  +1    TO    IC-1
                 ON SIZE ERROR
                 MOVE      45     TO        AUX-ERRO
                 PERFORM   999000-ERRO
           END-ADD
           END-PERFORM
           MOVE WS-RSCO-ATBD-EMISSOR-A-ALFA
                                   TO SGTPB26S-RSCO-ATBD-EMISSOR

           .
       158000-SAI.
           EXIT.


      *-----------------------------------------------------------------
       159000-CD-GR-ECNC         SECTION.
      *-----------------------------------------------------------------

           MOVE 0015    TO    PSC-ERRO-PGM
           MOVE ZEROS   TO    IT-INT
           MOVE SPACES  TO    WS-CD-GR-ECNC-A
           MOVE WS-CCT  TO    WS-CNT-CMP
           ADD  +1      TO    WS-CNT-CMP
           PERFORM UNTIL WS-REG-B(WS-CNT-CMP) EQUAL   ';'  OR
                            WS-CNT-CMP           GREATER  1000

           ADD    +1                   TO IT-INT
                  ON SIZE ERROR
                  MOVE      46       TO AUX-ERRO
                  PERFORM   999000-ERRO
           END-ADD
           IF    IT-INT            NOT GREATER    06
            MOVE   WS-REG-B(WS-CNT-CMP)   TO    WS-CD-GR-ECNC-B (IT-INT)
           END-IF
           ADD     +1     TO    WS-CNT-CMP
                   ON SIZE ERROR
                   MOVE      47       TO    AUX-ERRO
                   PERFORM   999000-ERRO
           END-ADD
           END-PERFORM

      * Alinha à esquerda
           MOVE SPACES  TO    WS-CD-GR-ECNC-A-ALFA
           MOVE    1    TO    IC-1
           MOVE    1    TO    IC-2
           PERFORM   UNTIL    IC-1     >    06
           IF    WS-CD-GR-ECNC-B(IC-1)      NOT EQUAL  ' '
                 MOVE    WS-CD-GR-ECNC-B           (IC-1)
                            TO    WS-CD-GR-ECNC-B-ALFA     (IC-2)
                 ADD  +1    TO    IC-2
                      ON SIZE ERROR
                      MOVE      48     TO        AUX-ERRO
                      PERFORM   999000-ERRO
                 END-ADD
           END-IF
           ADD  +1    TO    IC-1
                 ON SIZE ERROR
                 MOVE      49     TO        AUX-ERRO
                 PERFORM   999000-ERRO
           END-ADD
           END-PERFORM
           MOVE WS-CD-GR-ECNC-A-ALFA    TO    SGTPB26S-CD-GR-ECNC
           .
       159000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       160000-RSCO-ATBD-GRUPO    SECTION.
      *-----------------------------------------------------------------

           MOVE 0016    TO    PSC-ERRO-PGM
           MOVE ZEROS   TO    IT-INT
           MOVE SPACES  TO    WS-RSCO-ATBD-GRUPO-A
           MOVE WS-CCT  TO    WS-CNT-CMP
           ADD  +1      TO    WS-CNT-CMP
                  ON SIZE ERROR
                  MOVE      50       TO AUX-ERRO
                  PERFORM   999000-ERRO
           END-ADD
           PERFORM UNTIL WS-REG-B(WS-CNT-CMP) EQUAL   ';'  OR
                            WS-CNT-CMP    GREATER  1000
           ADD    +1     TO IT-INT
                  ON SIZE ERROR
                  MOVE      51       TO AUX-ERRO
                  PERFORM   999000-ERRO
           END-ADD
           IF IT-INT            NOT GREATER  03
            MOVE WS-REG-B(WS-CNT-CMP) TO WS-RSCO-ATBD-GRUPO-B (IT-INT)
           END-IF
           ADD     +1    TO WS-CNT-CMP
                   ON SIZE ERROR
                   MOVE      52       TO AUX-ERRO
                   PERFORM   999000-ERRO
           END-ADD
           END-PERFORM
      * Alinha à esquerda
           MOVE    SPACES  TO    WS-RSCO-ATBD-GRUPO-A-ALFA
           MOVE    1    TO    IC-1
           MOVE    1    TO    IC-2
           PERFORM   UNTIL    IC-1     >    03
           IF    WS-RSCO-ATBD-GRUPO-B(IC-1)      NOT EQUAL  ' '
                 MOVE    WS-RSCO-ATBD-GRUPO-B      (IC-1)
                            TO    WS-RSCO-ATBD-GRUPO-B-ALFA      (IC-2)
                 ADD  +1    TO    IC-2
                      ON SIZE ERROR
                      MOVE      53     TO        AUX-ERRO
                      PERFORM   999000-ERRO
                 END-ADD
           END-IF
           ADD  +1    TO    IC-1
                 ON SIZE ERROR
                 MOVE      54     TO        AUX-ERRO
                 PERFORM   999000-ERRO
           END-ADD
           END-PERFORM
           MOVE  WS-RSCO-ATBD-GRUPO-A-ALFA
                                       TO   SGTPB26S-RSCO-ATBD-GRUPO
           .
       160000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       900100-EXIBIR-RESUMO-PROCESS        SECTION.
      *-----------------------------------------------------------------
            MOVE 0017                     TO PSC-ERRO-PGM

            CLOSE     SGTFDIR1
                      SGTF999S.

            IF CNT-LIDOS   EQUAL       ZEROS
               DISPLAY '001 - ARQUIVO VAZIO.'
               MOVE    004 TO          RETURN-CODE
               DISPLAY '999 ' CTE-PROG '999 - FIM-NORMAL'
            ELSE
             IF (AUX-ERRO   NOT EQUAL   ZEROS) OR
                (CH-ABEND   EQUAL       'S')
               DISPLAY '-----------------------------------------------'
               DISPLAY '888 - ' CTE-PROG
               DISPLAY '888 - QTDE. REG. LIDOS ......: ' CNT-LIDOS
               DISPLAY '888 - QTDE. REG. DESPREZADOS.: ' CNT-DESPREZADOS
               DISPLAY '888 - QTDE. REG. REJEITADOS..: ' CNT-REJEITADOS
               DISPLAY '888 - QTDE. REG. GRAVADOS....: ' CNT-GRAVADOS
               DISPLAY '-----------------------------------------------'
               CALL SBABEND
             ELSE
              DISPLAY '-----------------------------------------------'
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

            MOVE 0018                     TO PSC-ERRO-PGM

            DISPLAY '888 - ' CTE-PROG
            DISPLAY '888 - Erro em operação aritmética'
            DISPLAY '888 - Ocorreu o erro ' AUX-ERRO ' no processamento'
            DISPLAY '888 - Resumo até a ocorrência: '
            PERFORM 900100-EXIBIR-RESUMO-PROCESS

            .
       999000-ERRO-SAI.
           EXIT.
      *---------------- FIM SGTPB026 ----------------------------            *--
