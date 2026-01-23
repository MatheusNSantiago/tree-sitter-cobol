      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSC038
      * ANALISTA..: F9458161 - Vanessa Tsukamoto
      * AUTOR.....: F9458161 - Vanessa Tsukamoto
      * COMPILACAO: 54 - PSOSE600 - Cobol 6.3 c/otimizacao p/producao
      * OBJETIVO..: Consulta tabela DB2SGT.EST_CRCT_CTRA_TVM
      *-----------------------------------------------------------------
      * VRS001 23.07.2024 - F9458161 - Implantacao
      *-----------------------------------------------------------------
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSC038.
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
      *-----------------------------------------------------------------
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
      *-----------------------------------------------------------------
       01  CTE-PROG     PIC  X(17) VALUE '*** SGTSC038 ***'.
       01  CTE-VERS     PIC  X(06) VALUE 'VRS001'.

       01  PSC-PGM                     PIC  9(03) VALUE 0.
           88 PSC-INICIO               VALUE 1.
           88 PSC-VLDR-ENTRADA         VALUE 2.
           88 PSC-CONSULTA             VALUE 3.
           88 PSC-CONSULTA-APVD        VALUE 4.
           88 PSC-GUARDA-CACHE         VALUE 5.

      * Registro da tabela
       01  REG-CACHE.
           03  CHAVE-CACHE.
               05  CD-CTRA-TVM               PIC S9(9) COMP-5 VALUE 0.
               05  CD-TIP-ATB-CTRA           PIC S9(4) COMP-5 VALUE 0.
               05  NR-ETRD-CRCT-CTRA         PIC S9(4) COMP-5 VALUE 0.
               05  TS-EST-CRCT-CTRA          PIC  X(026) VALUE ' '.
               05  FILLER                    PIC  X(20) VALUE ' '.
           03  DADOS-CACHE.
               05  CD-TIP-EST-DCS            PIC S9(4) COMP-5 VALUE 0.
               05  CD-USU-ATL-EST            PIC  X(8) VALUE ' '.
               05  TX-OBS-EST-CRCT           PIC  X(60) VALUE ' '.

-INC ALMKCBL1

      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informacoes
-INC HLPKE01B

      *-----------------------------------------------------------------
       LOCAL-STORAGE SECTION.
      *-----------------------------------------------------------------
      * Area de trabalho
       01  NM-CMD-DB2                  PIC  X(20) VALUE ' '.
       01  NM-TAB                      PIC  X(30) VALUE ' '.

       01  GDA-MSG-ERRO                PIC  X(120) VALUE ' '.
       01  GDA-DISPLAY-NUMERICO        PIC -ZZZZZZZZ9.

      * Indicadores de nulidades
       01  TX-OBS-EST-CRCT-N           PIC S9(04) COMP-5 VALUE 0.

      * Book definição dados a serem usados na subrotina de erros
-INC HLPKE001

      * Book Mensageria
-INC MSGKWMG1

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

      *-----------------------------------------------------------------
       LINKAGE SECTION.
      *-----------------------------------------------------------------
       01  DFHCOMMAREA.
-INC SGTKC038
      *-----------------------------------------------------------------
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *-----------------------------------------------------------------
           IF  EIBCALEN NOT EQUAL LENGTH OF DFHCOMMAREA
               SET  ERRO-CODIFICADO    TO TRUE
               MOVE 0220               TO  CD-RTN-PGM
               MOVE 'Area com tamanho inválido - verifique book/local.'
                                       TO GDA-MSG-ERRO
               PERFORM 999000-ERRO
           END-IF

           IF  NOT EXECUCAO-ONLINE AND NOT EXECUCAO-BATCH
               CALL CICS5000 USING CICS5000-COMMAREA
           END-IF

      * WHENEVER DB2
-INC HLPKE002
      * WHENEVER CICS
-INC HLPKE003

           SET  PSC-INICIO             TO TRUE

           PERFORM 100000-VALIDA-PARAMETRO

           EVALUATE TRUE
              WHEN SGTSC038-CONSULTAR
                   IF  CHAVE-CACHE = SGTSC038-CHAVE
                       MOVE DADOS-CACHE TO SGTSC038-AREA-DADOS
                   ELSE
                       PERFORM 200000-CONSULTA
                   END-IF
              WHEN SGTSC038-CONSULTAR-APVD
                   PERFORM 200001-CONSULTA-APVD
           END-EVALUATE

           GOBACK
           .
      *-----------------------------------------------------------------
       100000-VALIDA-PARAMETRO SECTION.
      *-----------------------------------------------------------------
           SET  PSC-VLDR-ENTRADA       TO TRUE

           INITIALIZE  RTN-PGM  SGTSC038-RTN-PGM

           IF  NOT SGTSC038-CONSULTAR
           AND NOT SGTSC038-CONSULTAR-APVD
               SET  ERRO-CODIFICADO    TO TRUE
               MOVE 0116               TO CD-RTN-PGM
               MOVE 'Acao invalida'    TO GDA-MSG-ERRO
               PERFORM 999000-ERRO
           END-IF

           IF  SGTSC038-CD-CTRA-TVM    <= ZEROS
               SET ERRO-CODIFICADO     TO TRUE
               MOVE 0001               TO CD-RTN-PGM
               MOVE 'Codigo da carteira de titulo e valores imobiliarios
      -             ' invalido.'       TO GDA-MSG-ERRO
               PERFORM 999000-ERRO
           END-IF

           IF  SGTSC038-CD-TIP-ATB-CTRA <= ZEROS
               SET ERRO-CODIFICADO     TO TRUE
               MOVE 0602               TO CD-RTN-PGM
               MOVE 'Codigo do tipo de atributo da carteira invalido'
                                       TO GDA-MSG-ERRO
               PERFORM 999000-ERRO
           END-IF

           IF  SGTSC038-CONSULTAR
               IF  SGTSC038-NR-ETRD-CRCT-CTRA <= ZEROS
                   SET ERRO-CODIFICADO TO TRUE
                   MOVE 0753           TO CD-RTN-PGM
                   MOVE 'Numero estruturado caracteristica carteira inva
      -                'lido.'         TO GDA-MSG-ERRO
                   PERFORM 999000-ERRO
               END-IF
               IF  SGTSC038-TS-EST-CRCT-CTRA EQUAL SPACES
                   SET ERRO-CODIFICADO TO TRUE
                   MOVE 0895           TO CD-RTN-PGM
                   MOVE 'Timestamp estado caracteristica carteira nao in
      -                 'formado.'     TO GDA-MSG-ERRO
                   PERFORM 999000-ERRO
               END-IF
           END-IF
           .
       100000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       200000-CONSULTA SECTION.
      *-----------------------------------------------------------------
           SET  PSC-CONSULTA           TO TRUE

           INITIALIZE SGTSC038-AREA-DADOS

           EXEC SQL
                SELECT CD_TIP_EST_DCS
                     , CD_USU_ATL_EST
                     , TX_OBS_EST_CRCT
                  INTO :SGTSC038-CD-TIP-EST-DCS
                     , :SGTSC038-CD-USU-ATL-EST
                     , :SGTSC038-TX-OBS-EST-CRCT  :TX-OBS-EST-CRCT-N
                  FROM DB2SGT.EST_CRCT_CTRA_TVM
                 WHERE CD_CTRA_TVM       = :SGTSC038-CD-CTRA-TVM
                   AND CD_TIP_ATB_CTRA   = :SGTSC038-CD-TIP-ATB-CTRA
                   AND NR_ETRD_CRCT_CTRA = :SGTSC038-NR-ETRD-CRCT-CTRA
                   AND TS_EST_CRCT_CTRA  = :SGTSC038-TS-EST-CRCT-CTRA
           END-EXEC

      * MOCK-POINT MCKV1
           EVALUATE SQLCODE
               WHEN 0
                    CONTINUE

               WHEN +100
                    SET  ERRO-CODIFICADO TO TRUE
                    MOVE 0911            TO CD-RTN-PGM
                    MOVE 'Estado característica carteira inexistente.'
                                         TO GDA-MSG-ERRO
                    PERFORM 999000-ERRO

               WHEN OTHER
                    SET  ERRO-DE-DB2         TO TRUE
                    MOVE 9007                TO CD-RTN-PGM
                    MOVE 'EST_CRCT_CTRA_TVM' TO NM-TAB
                    MOVE SQLCODE             TO GDA-DISPLAY-NUMERICO
                    STRING 'Erro select ' NM-TAB ';'
                           'SQLCODE=' GDA-DISPLAY-NUMERICO
                            DELIMITED BY SIZE INTO GDA-MSG-ERRO
                    END-STRING
                    PERFORM 999000-ERRO
           END-EVALUATE

           PERFORM 980101-GUARDA-CACHE
           .
       200000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       200001-CONSULTA-APVD SECTION.
      *-----------------------------------------------------------------
           SET  PSC-CONSULTA-APVD      TO TRUE

           INITIALIZE  SGTSC038-NR-ETRD-CRCT-CTRA
                       SGTSC038-TS-EST-CRCT-CTRA
                       SGTSC038-CD-USU-ATL-EST
                       SGTSC038-TX-OBS-EST-CRCT

      *    Estado 2 - Aprovado
           MOVE 2                      TO SGTSC038-CD-TIP-EST-DCS

           EXEC SQL
                SELECT A.NR_ETRD_CRCT_CTRA
                     , MAX(A.TS_EST_CRCT_CTRA)
                  INTO :SGTSC038-NR-ETRD-CRCT-CTRA
                     , :SGTSC038-TS-EST-CRCT-CTRA
                  FROM DB2SGT.EST_CRCT_CTRA_TVM A
                 WHERE A.CD_CTRA_TVM     = :SGTSC038-CD-CTRA-TVM
                   AND A.CD_TIP_ATB_CTRA = :SGTSC038-CD-TIP-ATB-CTRA
                   AND A.CD_TIP_EST_DCS  = :SGTSC038-CD-TIP-EST-DCS
              GROUP BY A.NR_ETRD_CRCT_CTRA
              ORDER BY A.NR_ETRD_CRCT_CTRA DESC
                 FETCH FIRST 1 ROW ONLY
           END-EXEC

      * MOCK-POINT MCKV2
           EVALUATE SQLCODE
               WHEN 0
                    CONTINUE

               WHEN +100
                    SET  ERRO-CODIFICADO    TO TRUE
                    MOVE 0914               TO CD-RTN-PGM
                    MOVE 'Não existe estado aprovado para característica
      -                  ' da carteira TVM' TO GDA-MSG-ERRO
                    PERFORM 999000-ERRO

               WHEN OTHER
                    SET  ERRO-DE-DB2         TO TRUE
                    MOVE 9007                TO CD-RTN-PGM
                    MOVE 'EST_CRCT_CTRA_TVM' TO NM-TAB
                    MOVE SQLCODE             TO GDA-DISPLAY-NUMERICO
                    STRING 'Erro select ' NM-TAB ';'
                           'SQLCODE=' GDA-DISPLAY-NUMERICO
                            DELIMITED BY SIZE INTO GDA-MSG-ERRO
                    END-STRING
                    PERFORM 999000-ERRO
           END-EVALUATE
           .
       200001-SAI.
           EXIT.
      *-----------------------------------------------------------------
       980101-GUARDA-CACHE SECTION.
      *-----------------------------------------------------------------
           SET  PSC-GUARDA-CACHE       TO TRUE

           MOVE SGTSC038-CHAVE         TO CHAVE-CACHE
           MOVE SGTSC038-AREA-DADOS    TO DADOS-CACHE
           .
       980101-SAI.
           EXIT.
      *-----------------------------------------------------------------
       999000-ERRO SECTION.
      *-----------------------------------------------------------------
           MOVE PSC-PGM                TO PSC-ERRO-PGM
           MOVE GDA-MSG-ERRO           TO MSG-ERRO

           IF  ERRO-DE-DB2
               PERFORM 999002-ERRO-DB2
           ELSE
               PERFORM 998000-GRAVA-APPL-ERRO
               PERFORM 999001-ERRO-001
           END-IF
           .
      *-----------------
      * 999001-ERRO-001.
      *-----------------
-INC HLPKE004
-INC HLPKE012

           IF  EXECUCAO-ONLINE
               PERFORM 999901-GRAVA-ERRO-E-ENCERRA
           END-IF

           PERFORM  999999-ENCERRAR.

      *-----------------
      * 999002-ERRO-DB2
      *-----------------
-INC HLPKE005
      *-----------------
      * 999003-TRATA-ABEND-CICS
      * 999004-TRATA-CONDITION-CICS
      *-----------------
-INC HLPKE006

      *-----------------------------------------------------------------
        999901-GRAVA-ERRO-E-ENCERRA SECTION.
      *-----------------------------------------------------------------
      *--  Mensageria
           MOVE PSC-PGM                TO MSG1-CD-SEQL-ERRO
           MOVE CD-RTN-PGM             TO MSG1-CD-ERRO-TS
           MOVE GDA-MSG-ERRO           TO MSG1-TX-ERRO-TS

           EVALUATE TRUE
               WHEN ERRO-DE-DB2
                    SET  ERRO-SQL      TO TRUE
                    MOVE SQLCODE       TO GDA-DISPLAY-NUMERICO
                    MOVE 'SELECT'      TO NM-CMD-DB2

                    ADD  1        TO MSG1-QT-VRV-MNT
                    MOVE 'SQLCODE'
                                  TO MSG1-NM-VRV-MON(MSG1-QT-VRV-MNT)
                    MOVE GDA-DISPLAY-NUMERICO
                                  TO MSG1-VL-VRV-MON(MSG1-QT-VRV-MNT)

                    ADD  1
                                  TO MSG1-QT-VRV-MNT
                    MOVE 'COMANDO DB2'
                                  TO MSG1-NM-VRV-MON(MSG1-QT-VRV-MNT)
                    MOVE NM-CMD-DB2
                                  TO MSG1-VL-VRV-MON(MSG1-QT-VRV-MNT)

                    ADD  1
                                  TO MSG1-QT-VRV-MNT
                    MOVE 'Nome da Tabela'
                                  TO MSG1-NM-VRV-MON(MSG1-QT-VRV-MNT)
                    MOVE NM-TAB
                                  TO MSG1-VL-VRV-MON(MSG1-QT-VRV-MNT)

               WHEN ERRO-CODIFICADO
                    SET ERRO-PROGRAMADO TO TRUE

           END-EVALUATE

           ADD  1                 TO MSG1-QT-VRV-MNT
           MOVE ' SGTSC038-CD-CHV-USU '
                                  TO MSG1-NM-VRV-MON(MSG1-QT-VRV-MNT)
           MOVE   SGTSC038-CD-CHV-USU
                                  TO MSG1-VL-VRV-MON(MSG1-QT-VRV-MNT)

           ADD  1                 TO MSG1-QT-VRV-MNT
           MOVE ' SGTSC038-ACAO '
                                  TO MSG1-NM-VRV-MON(MSG1-QT-VRV-MNT)
           MOVE   SGTSC038-ACAO
                                  TO GDA-DISPLAY-NUMERICO
           MOVE   GDA-DISPLAY-NUMERICO
                                  TO MSG1-VL-VRV-MON(MSG1-QT-VRV-MNT)

           ADD  1                 TO MSG1-QT-VRV-MNT
           MOVE ' SGTSC038-CD-CTRA-TVM '
                                  TO MSG1-NM-VRV-MON(MSG1-QT-VRV-MNT)
           MOVE   SGTSC038-CD-CTRA-TVM
                                  TO GDA-DISPLAY-NUMERICO
           MOVE   GDA-DISPLAY-NUMERICO
                                  TO MSG1-VL-VRV-MON(MSG1-QT-VRV-MNT)

           ADD  1                 TO MSG1-QT-VRV-MNT
           MOVE ' SGTSC038-CD-TIP-ATB-CTRA '
                                  TO MSG1-NM-VRV-MON(MSG1-QT-VRV-MNT)
           MOVE   SGTSC038-CD-TIP-ATB-CTRA
                                  TO GDA-DISPLAY-NUMERICO
           MOVE   GDA-DISPLAY-NUMERICO
                                  TO MSG1-VL-VRV-MON(MSG1-QT-VRV-MNT)

           ADD  1                 TO MSG1-QT-VRV-MNT
           MOVE ' SGTSC038-NR-ETRD-CRCT-CTRA '
                                  TO MSG1-NM-VRV-MON(MSG1-QT-VRV-MNT)
           MOVE   SGTSC038-NR-ETRD-CRCT-CTRA
                                  TO GDA-DISPLAY-NUMERICO
           MOVE   GDA-DISPLAY-NUMERICO
                                  TO MSG1-VL-VRV-MON(MSG1-QT-VRV-MNT)

           ADD  1                 TO MSG1-QT-VRV-MNT
           MOVE ' SGTSC038-TS-EST-CRCT-CTRA '
                                  TO MSG1-NM-VRV-MON(MSG1-QT-VRV-MNT)
           MOVE   SGTSC038-TS-EST-CRCT-CTRA
                                  TO MSG1-VL-VRV-MON(MSG1-QT-VRV-MNT)

           ADD  1                 TO MSG1-QT-VRV-MNT
           MOVE ' SGTSC038-CD-TIP-EST-DCS '
                                  TO MSG1-NM-VRV-MON(MSG1-QT-VRV-MNT)
           MOVE   SGTSC038-CD-TIP-EST-DCS
                                  TO GDA-DISPLAY-NUMERICO
           MOVE   GDA-DISPLAY-NUMERICO
                                  TO MSG1-VL-VRV-MON(MSG1-QT-VRV-MNT)

           ADD  1                 TO MSG1-QT-VRV-MNT
           MOVE ' SGTSC038-CD-USU-ATL-EST '
                                  TO MSG1-NM-VRV-MON(MSG1-QT-VRV-MNT)
           MOVE   SGTSC038-CD-USU-ATL-EST
                                  TO MSG1-VL-VRV-MON(MSG1-QT-VRV-MNT)

           ADD  1                 TO MSG1-QT-VRV-MNT
           MOVE ' SGTSC038-TX-OBS-EST-CRCT '
                                  TO MSG1-NM-VRV-MON(MSG1-QT-VRV-MNT)
           MOVE   SGTSC038-TX-OBS-EST-CRCT
                                  TO MSG1-VL-VRV-MON(MSG1-QT-VRV-MNT)

           PERFORM MSG1-NTF-MSG
           .
       999901-SAI.
           EXIT.
      *--------------------------------------*
      * Mensageria - Padrao da SIGLA e do MSG.
      *--------------------------------------*
-INC MSGKPMG1

      *-----------------------------------------------------------------
       999999-ENCERRAR SECTION.
      *-----------------------------------------------------------------
           MOVE RTN-PGM                TO SGTSC038-RTN-PGM
           GOBACK.
      *-----------------------------------------------------------------
