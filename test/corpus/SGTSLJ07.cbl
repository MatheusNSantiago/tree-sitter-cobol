      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSLJ07
      * ANALISTA..: F2054109 - CINTIA APARECIDA SOARES IGLESIAS
      * AUTOR.....: F9855659 - WILSON ARANDA DA SILVA
      * OBJETIVO..: LISTAR VERSOES DE AGÊNCIA DE CLASSIFICAÇÃO EXTERNA
      *               DE RISCO.
      *               TABELAS DB2 EST_AG_CLSC_EXNO E AG_CLSC_EXNO_RSCO
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS002 11.04.2019 - F9188847 de 2016/145195 - Acerto mensageria
      * VRS001 18.11.2015 - F9855659 - 2013/120821 - IMPLANTACAO
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSLJ07.

      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
      *-----------------------------------------------------------------
       CONFIGURATION          SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       WORKING-STORAGE                 SECTION.
      *-----------------------------------------------------------------
       01  CTE-PROG     PIC  X(17) VALUE '*** SGTSLJ07 ***'.
       01  CTE-VERS     PIC  X(06) VALUE 'VRS002'.

      *-----------------------------------------------------------------
      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informacoes
-INC HLPKE01B                                                           00000360
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------
      * Area de trabalho
       01  IC-TAB                            PIC  9(04)       VALUE 0.

       01  PARAMETROS.
           03 CD-TIP-EST-DCS-INC       PIC S9(4)  COMP-5 VALUE 0.
           03 CD-TIP-EST-DCS-FIM       PIC S9(4)  COMP-5 VALUE 0.
           03 DT-INC-PSQ               PIC  X(10) VALUE ' '.
           03 DT-FIM-PSQ               PIC  X(10) VALUE ' '.
           03 DT-PSQ                   PIC  X(10) VALUE ' '.

       01 QT-REG-SELD                  PIC S9(4) COMP-5 VALUE 0.

      * Campos utilizados para tratar campo nome para pesquisa.
       01  NM-PSQD                     PIC X(60) VALUE SPACES.
       01  TAM-PSQD                    PIC S9(4) COMP-5 VALUE 0.
       01  QT-CCT-DGTD                 PIC S9(4) COMP-5 VALUE 0.
       01  QT-CCT-N-DGTD               PIC S9(4) COMP-5 VALUE 0.
       01  QT-CCT-PSQ                  PIC S9(4) COMP-5 VALUE 0.

      * Campos utilizados para leitura

       01  REG-WS.
           03 CD-AG-CLSC-RSCO          PIC S9(4) COMP-5
              OCCURS 101 TIMES         VALUE 0.
           03 NR-ETRD-AG-CLSC          PIC S9(4) COMP-5
              OCCURS 101 TIMES         VALUE 0.
           03 TS-EST-AG-CLSC           PIC X(26)
              OCCURS 101 TIMES         VALUE ' '.
           03 NM-AG-CLSC-RSCO          PIC X(60)
              OCCURS 101 TIMES         VALUE ' '.
           03 IN-AG-CLSC-ATV           PIC X(1)
              OCCURS 101 TIMES         VALUE ' '.
           03 CD-TIP-EST-DCS           PIC S9(4) COMP-5
              OCCURS 101 TIMES         VALUE 0.
           03 CD-USU-ATL-EST           PIC X(8)
              OCCURS 101 TIMES         VALUE ' '.

      * Indicadores de nulidades

       01 QT-REG-SELD-N                PIC S9(4) COMP-5 VALUE 0.

      * Variavel(is) de controle do cursor(es)

       01  CTL-CURSOR-PSQ.
           03  CD-FIM-CURSOR-PSQ       PIC S9(04) COMP-5 VALUE 0.
               88  FIM-CURSOR-PSQ      VALUE +100.
           03  IN-CURSOR-PSQ-ABTO      PIC  X(01) VALUE 'N'.
               88  CURSOR-PSQ-ABTO     VALUE 'S'.

      * Subrotinas utilizadas

      * Books da definicao de dados a serem usados na subrotina de erros

-INC HLPKE001                                                           00000900
      *--> Book para captura de erro via MSG1
-INC MSGKWMG1
      *--> Variaveis para tratamento de CONTAINER
-INC SGTKERR1

      *--> HLPKW999
      *    Variaveis Auxiliares para Captura de Erro CICS
      *    e Adicionar Variaveis Monitoradas
       01  MSGKW999-VRV-ERRO-CICS.
           03  MSGKW999-RESP1-X8            PIC  X(08) VALUE SPACES.
           03  MSGKW999-RESP2-X8            PIC  X(08) VALUE SPACES.
       01  MSGKW999-VRV-ALINHA-NUMERO.
           03  MSGKW999-NUMERO-Z17          PIC  ZZZZZZZZZZZZZZZZ9.
           03  MSGKW999-NUMERO-X17          PIC  X(17) VALUE SPACES.
           03  MSGKW999-BRANCOS             PIC  9(08) VALUE ZEROS.
       01  MSGKW999-VRV-AUX.
           03  MSGKW999-NM-VRV-MNT     PIC  X(030) VALUE SPACES.
           03  MSGKW999-VL-VRV-MNT     PIC  X(100) VALUE SPACES.

      *--> Máscaras edição de variáveis p/ monitoração mensagens de erro
       01  MCR-S9-15                   PIC      ---------------9.

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

      *-----------------------------------------------------------------
       LINKAGE                         SECTION.
      *-----------------------------------------------------------------
       01  DFHCOMMAREA.
-INC SGTKLJ07                                                           00001000

      * PSQ1
      * Listar Ultimo Estado da Agência de Classificação Externa de Risc
           EXEC SQL
             DECLARE CURSOR-PSQ1 CURSOR
                     WITH ROWSET POSITIONING FOR
           SELECT
                 C.CD_AG_CLSC_RSCO,
                 C.NR_ETRD_AG_CLSC,
                 C.TS_ESTADO,
                 C.NM_AG_CLSC_RSCO,
                 C.IN_AG_CLSC_ATV,
                 D.CD_TIP_EST_DCS,
                 D.CD_USU_ATL_EST
           FROM
                (SELECT A.CD_AG_CLSC_RSCO,
                      A.NR_ETRD_AG_CLSC,
                      MAX(A.TS_EST_AG_CLSC) AS TS_ESTADO,
                      B.NM_AG_CLSC_RSCO,
                      B.IN_AG_CLSC_ATV
                FROM
                      DB2SGT.EST_AG_CLSC_EXNO A
                INNER JOIN
                       DB2SGT.AG_CLSC_EXNO_RSCO B
                ON
                       A.CD_AG_CLSC_RSCO = B.CD_AG_CLSC_RSCO
                       AND A.NR_ETRD_AG_CLSC = B.NR_ETRD_AG_CLSC
                WHERE

                    UPPER(B.NM_AG_CLSC_RSCO)
                  LIKE UPPER(:NM-PSQD)
                GROUP BY
                       A.CD_AG_CLSC_RSCO,
                       A.NR_ETRD_AG_CLSC,
                       B.NM_AG_CLSC_RSCO,
                       B.IN_AG_CLSC_ATV)C
           INNER JOIN
                 DB2SGT.EST_AG_CLSC_EXNO D
           ON
             C.CD_AG_CLSC_RSCO = D.CD_AG_CLSC_RSCO
             AND C.NR_ETRD_AG_CLSC = D.NR_ETRD_AG_CLSC
             AND C.TS_ESTADO = D.TS_EST_AG_CLSC
           WHERE
                   D.CD_TIP_EST_DCS >= :CD-TIP-EST-DCS-INC
               AND D.CD_TIP_EST_DCS <= :CD-TIP-EST-DCS-FIM
               AND C.TS_ESTADO      <  :SGTSLJ07-TS-EST-AG-CLSC
           ORDER BY
               C.TS_ESTADO DESC
           END-EXEC.

      *PSQ2
      *Listar Estados da Agência de Classificação Externa em um período.
           EXEC SQL
             DECLARE CURSOR-PSQ2 CURSOR
               WITH ROWSET POSITIONING FOR
           SELECT
                 A.CD_AG_CLSC_RSCO,
                 A.NR_ETRD_AG_CLSC,
                 A.TS_EST_AG_CLSC,
                 B.NM_AG_CLSC_RSCO,
                 B.IN_AG_CLSC_ATV,
                 A.CD_TIP_EST_DCS,
                 A.CD_USU_ATL_EST
           FROM
                 DB2SGT.EST_AG_CLSC_EXNO A
           INNER JOIN
                 DB2SGT.AG_CLSC_EXNO_RSCO B
           ON
                 A.CD_AG_CLSC_RSCO     = B.CD_AG_CLSC_RSCO
                 AND A.NR_ETRD_AG_CLSC = B.NR_ETRD_AG_CLSC
             WHERE
                    UPPER(B.NM_AG_CLSC_RSCO)
                LIKE UPPER (:NM-PSQD)
                AND (A.CD_TIP_EST_DCS       >= :CD-TIP-EST-DCS-INC
                AND A.CD_TIP_EST_DCS        <= :CD-TIP-EST-DCS-FIM)
                AND (DATE(A.TS_EST_AG_CLSC) >= :DT-INC-PSQ
                AND DATE(A.TS_EST_AG_CLSC)  <= :DT-FIM-PSQ)
                AND A.TS_EST_AG_CLSC         < :SGTSLJ07-TS-EST-AG-CLSC
           ORDER BY
              A.TS_EST_AG_CLSC DESC
           END-EXEC.

      * PSQ3
      *Listar Agência de Classificação Externade Risco Antes de uma data
           EXEC SQL
             DECLARE CURSOR-PSQ3 CURSOR
               WITH ROWSET POSITIONING FOR
           SELECT
              C.CD_AG_CLSC_RSCO,
              C.NR_ETRD_AG_CLSC,
              C.TS_ESTADO,
              C.NM_AG_CLSC_RSCO,
              C.IN_AG_CLSC_ATV,
              D.CD_TIP_EST_DCS,
              D.CD_USU_ATL_EST
           FROM
              (SELECT A.CD_AG_CLSC_RSCO,
               A.NR_ETRD_AG_CLSC,
               MAX(A.TS_EST_AG_CLSC) AS TS_ESTADO,
               B.NM_AG_CLSC_RSCO,
               B.IN_AG_CLSC_ATV
           FROM
               DB2SGT.EST_AG_CLSC_EXNO A
           INNER JOIN
               DB2SGT.AG_CLSC_EXNO_RSCO B
           ON
               A.CD_AG_CLSC_RSCO = B.CD_AG_CLSC_RSCO
               AND A.NR_ETRD_AG_CLSC = B.NR_ETRD_AG_CLSC
           WHERE
                   UPPER(B.NM_AG_CLSC_RSCO)
                LIKE UPPER (:NM-PSQD)
               AND DATE(A.TS_EST_AG_CLSC) < :DT-PSQ
           GROUP BY
               A.CD_AG_CLSC_RSCO,
               A.NR_ETRD_AG_CLSC,
               B.NM_AG_CLSC_RSCO,
               B.IN_AG_CLSC_ATV) C
           INNER JOIN
               DB2SGT.EST_AG_CLSC_EXNO D
           ON
               C.CD_AG_CLSC_RSCO = D.CD_AG_CLSC_RSCO
               AND C.NR_ETRD_AG_CLSC = D.NR_ETRD_AG_CLSC
               AND C.TS_ESTADO = D.TS_EST_AG_CLSC
           WHERE
               D.CD_TIP_EST_DCS     >= :CD-TIP-EST-DCS-INC
               AND D.CD_TIP_EST_DCS <= :CD-TIP-EST-DCS-FIM
               AND C.TS_ESTADO       < :SGTSLJ07-TS-EST-AG-CLSC
           ORDER BY
               C.TS_ESTADO DESC
           END-EXEC.

      *-----------------------------------------------------------------
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *-----------------------------------------------------------------

      *--> teste obrigatorio do tamanho da linkage nao retirar
           IF  EIBCALEN NOT EQUAL LENGTH OF DFHCOMMAREA
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE 0220               TO  CD-RTN-PGM
               MOVE 'Área com tamanho inválido - verifique book/local.'
                                       TO MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           MOVE 0001                        TO PSC-ERRO-PGM

           IF  NOT EXECUCAO-ONLINE AND NOT EXECUCAO-BATCH
             CALL CICS5000 USING CICS5000-COMMAREA
           END-IF

      * WHENEVER DB2
-INC HLPKE002                                                           00002460
      * WHENEVER CICS
-INC HLPKE003                                                           00002480

           PERFORM 100000-VALIDAR-PARAMETRO

           EVALUATE TRUE
           WHEN SGTSLJ07-LISR-ULT-EST-AG-CLSC
                PERFORM 970101-ABRE-CURSOR-PSQ1
                PERFORM 970102-LER-CURSOR-PSQ1
                PERFORM 970103-FECHA-CURSOR-PSQ1
                PERFORM 200000-PROCESSA-PSQ1
                    VARYING SGTSLJ07-IC-TAB FROM 1 BY 1
                    UNTIL SGTSLJ07-IC-TAB > SGTSLJ07-QT-OCR-TAB

           WHEN SGTSLJ07-LISR-EST-AG-CLSC
                PERFORM 970104-ABRE-CURSOR-PSQ2
                PERFORM 970105-LER-CURSOR-PSQ2
                PERFORM 970106-FECHA-CURSOR-PSQ2
                PERFORM 300000-PROCESSA-PSQ2
                    VARYING SGTSLJ07-IC-TAB FROM 1 BY 1
                    UNTIL SGTSLJ07-IC-TAB > SGTSLJ07-QT-OCR-TAB

           WHEN SGTSLJ07-LISR-AG-CLSC
                PERFORM 970107-ABRE-CURSOR-PSQ3
                PERFORM 970108-LER-CURSOR-PSQ3
                PERFORM 970109-FECHA-CURSOR-PSQ3
                PERFORM 400000-PROCESSA-PSQ3
                    VARYING SGTSLJ07-IC-TAB FROM 1 BY 1
                    UNTIL SGTSLJ07-IC-TAB > SGTSLJ07-QT-OCR-TAB
           END-EVALUATE

           GOBACK
           .
      *-----------------------------------------------------------------
       100000-VALIDAR-PARAMETRO        SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO PSC-ERRO-PGM

      *    INITIALIZE SGTSLJ07-RQSC.
           MOVE 'N'                    TO SGTSLJ07-IN-CNTO-PSQ

           IF  SGTSLJ07-TS-EST-AG-CLSC  EQUAL SPACES
               MOVE '9999-12-31-00.00.00.000000'
                                       TO SGTSLJ07-TS-EST-AG-CLSC
           END-IF

           IF  SGTSLJ07-CD-PSQ  EQUAL 1
               PERFORM 100100-VALIDAR-CD-TIP-EST-DCS
           ELSE
               IF  SGTSLJ07-CD-PSQ  EQUAL 2
                   PERFORM 100100-VALIDAR-CD-TIP-EST-DCS
                   PERFORM 100200-VALIDAR-DT-INC-FIM
               ELSE
                   IF  SGTSLJ07-CD-PSQ  EQUAL 3
                       PERFORM 100100-VALIDAR-CD-TIP-EST-DCS
                       PERFORM 100300-VALIDAR-DT-PSQ
                   ELSE
                       SET  ERRO-PROGRAMADO    TO  TRUE
                       MOVE  0116              TO  CD-RTN-PGM
                       MOVE 'Ação inválida.'   TO MSG-ERRO
                       PERFORM 999001-ERRO-001
                   END-IF
               END-IF
           END-IF

           PERFORM 100400-TRATAR-NOME
           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100100-VALIDAR-CD-TIP-EST-DCS   SECTION.
      * Verificar Código do Tipo de Estado Descrição.
      *-----------------------------------------------------------------
           MOVE 0003                   TO PSC-ERRO-PGM

           IF SGTSLJ07-CD-TIP-EST-DCS  >  ZEROS
              MOVE SGTSLJ07-CD-TIP-EST-DCS    TO CD-TIP-EST-DCS-INC
                                                 CD-TIP-EST-DCS-FIM
           ELSE
              MOVE 0000                TO CD-TIP-EST-DCS-INC
              MOVE 9999                TO CD-TIP-EST-DCS-FIM
           END-IF
           .
       100100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100200-VALIDAR-DT-INC-FIM       SECTION.
      * Verificar Data Inicio e Data Fim da Pesquisa.
      *-----------------------------------------------------------------
           MOVE 0004                   TO PSC-ERRO-PGM

           IF SGTSLJ07-DT-INC-PSQ      EQUAL SPACES
              MOVE '1900-01-01'        TO DT-INC-PSQ
           ELSE
              MOVE SGTSLJ07-DT-INC-PSQ   TO DT-INC-PSQ
           END-IF

           IF  SGTSLJ07-DT-FIM-PSQ    EQUAL SPACES
              MOVE '2900-12-31'        TO DT-FIM-PSQ
           ELSE
              MOVE SGTSLJ07-DT-FIM-PSQ   TO DT-FIM-PSQ
           END-IF
           .
       100200-SAI.
           EXIT.
      *-----------------------------------------------------------------
       100300-VALIDAR-DT-PSQ           SECTION.
      * Verifica a Data da Pesquisa Infomada.
      *-----------------------------------------------------------------
           MOVE 0005                   TO PSC-ERRO-PGM

           IF  SGTSLJ07-DT-PSQ         EQUAL SPACES
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE  0049              TO  CD-RTN-PGM
               MOVE 'Data informada inválida.'
                                       TO MSG-ERRO
               PERFORM 999001-ERRO-001
           ELSE
              MOVE SGTSLJ07-DT-PSQ     TO DT-PSQ
           END-IF
           .
       100300-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100400-TRATAR-NOME              SECTION.
      *--> Trocar brancos do final da string por %
      *-----------------------------------------------------------------
           MOVE 0006                   TO PSC-ERRO-PGM

           MOVE SPACES                 TO NM-PSQD
           MOVE ZEROS                  TO QT-CCT-DGTD
                                          QT-CCT-N-DGTD
                                          QT-CCT-PSQ

      ***  Busca a quantidade de caracteres não digitados, ou seja,
      ***  a quantidade de caracteres em brancos à esquerda.

           INSPECT FUNCTION REVERSE(SGTSLJ07-NM-AG-CLSC-RSCO)
               TALLYING QT-CCT-N-DGTD  FOR LEADING SPACES

      ***  Calcula a quantidade de caracteres digitados

           MOVE   LENGTH OF SGTSLJ07-NM-AG-CLSC-RSCO  TO TAM-PSQD
           SUBTRACT QT-CCT-N-DGTD FROM TAM-PSQD
               GIVING QT-CCT-DGTD
               ON SIZE ERROR
                  SET  ERRO-PROGRAMADO    TO  TRUE
                  MOVE  0143           TO  CD-RTN-PGM
                  MOVE 'Erro no cálculo do tamanho do texto.'
                                       TO MSG-ERRO
                  PERFORM 999001-ERRO-001
           END-SUBTRACT

      ***  Calcula a quantidade de caracteres para pesquisa, ou seja,
      ***  prevendo a delimitação por 1 caracter % no lado direito.

           ADD 1                       TO QT-CCT-DGTD GIVING QT-CCT-PSQ
               ON SIZE ERROR
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE  0143              TO  CD-RTN-PGM
               MOVE 'Erro no cálculo do tamanho do texto para pesquisa'
                                       TO MSG-ERRO
               PERFORM 999001-ERRO-001
           END-ADD

      ***  Formata campo de pesquisa com delimitador %
      ***  e preenche até o final do campo com %

           MOVE ALL '%'                TO NM-PSQD

           IF   QT-CCT-DGTD  GREATER ZEROS
           MOVE SGTSLJ07-NM-AG-CLSC-RSCO     (1:QT-CCT-DGTD)
                                       TO NM-PSQD (2:QT-CCT-DGTD)
           END-IF
           .
       100400-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-PROCESSA-PSQ1            SECTION.
      *Lista Ultimo Estado da Agência de Classificação Externa de Risco.
      *-----------------------------------------------------------------
           MOVE 0007                   TO PSC-ERRO-PGM

           MOVE CD-AG-CLSC-RSCO(SGTSLJ07-IC-TAB) TO
                     SGTSLJ07-CD-AG-CLSC-RSCO(SGTSLJ07-IC-TAB)
           MOVE NR-ETRD-AG-CLSC(SGTSLJ07-IC-TAB) TO
                     SGTSLJ07-NR-ETRD-AG-CLSC(SGTSLJ07-IC-TAB)
           MOVE TS-EST-AG-CLSC(SGTSLJ07-IC-TAB) TO
                     SGTSLJ07-TS-EST-AG-CLSC-RPST(SGTSLJ07-IC-TAB)
           MOVE NM-AG-CLSC-RSCO(SGTSLJ07-IC-TAB) TO
                     SGTSLJ07-NM-AG-CLSC-RSCO-RPST(SGTSLJ07-IC-TAB)
           MOVE IN-AG-CLSC-ATV(SGTSLJ07-IC-TAB) TO
                     SGTSLJ07-IN-AG-CLSC-ATV-RSCO(SGTSLJ07-IC-TAB)
           MOVE CD-TIP-EST-DCS(SGTSLJ07-IC-TAB) TO
                     SGTSLJ07-CD-TIP-EST-DCS-RPST(SGTSLJ07-IC-TAB)
           MOVE CD-USU-ATL-EST(SGTSLJ07-IC-TAB) TO
                     SGTSLJ07-CD-USU-ATL-EST(SGTSLJ07-IC-TAB)
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-PROCESSA-PSQ2            SECTION.
      * Lista Estado de Agência de Classificação Externa em um período.
      *-----------------------------------------------------------------
           MOVE 0008                   TO PSC-ERRO-PGM

           MOVE CD-AG-CLSC-RSCO(SGTSLJ07-IC-TAB) TO
                     SGTSLJ07-CD-AG-CLSC-RSCO(SGTSLJ07-IC-TAB)
           MOVE NR-ETRD-AG-CLSC(SGTSLJ07-IC-TAB) TO
                     SGTSLJ07-NR-ETRD-AG-CLSC(SGTSLJ07-IC-TAB)
           MOVE TS-EST-AG-CLSC(SGTSLJ07-IC-TAB) TO
                     SGTSLJ07-TS-EST-AG-CLSC-RPST(SGTSLJ07-IC-TAB)
           MOVE NM-AG-CLSC-RSCO(SGTSLJ07-IC-TAB) TO
                     SGTSLJ07-NM-AG-CLSC-RSCO-RPST(SGTSLJ07-IC-TAB)
           MOVE IN-AG-CLSC-ATV(SGTSLJ07-IC-TAB) TO
                     SGTSLJ07-IN-AG-CLSC-ATV-RSCO(SGTSLJ07-IC-TAB)
           MOVE CD-TIP-EST-DCS(SGTSLJ07-IC-TAB) TO
                     SGTSLJ07-CD-TIP-EST-DCS-RPST(SGTSLJ07-IC-TAB)
           MOVE CD-USU-ATL-EST(SGTSLJ07-IC-TAB) TO
                     SGTSLJ07-CD-USU-ATL-EST(SGTSLJ07-IC-TAB)
           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       400000-PROCESSA-PSQ3            SECTION.
      * Lista Agência Classificação Externa de Risco Antes de uma Data.
      *-----------------------------------------------------------------
           MOVE 0009                   TO PSC-ERRO-PGM

           MOVE CD-AG-CLSC-RSCO(SGTSLJ07-IC-TAB) TO
                     SGTSLJ07-CD-AG-CLSC-RSCO(SGTSLJ07-IC-TAB)
           MOVE NR-ETRD-AG-CLSC(SGTSLJ07-IC-TAB) TO
                     SGTSLJ07-NR-ETRD-AG-CLSC(SGTSLJ07-IC-TAB)
           MOVE TS-EST-AG-CLSC(SGTSLJ07-IC-TAB) TO
                     SGTSLJ07-TS-EST-AG-CLSC-RPST(SGTSLJ07-IC-TAB)
           MOVE NM-AG-CLSC-RSCO(SGTSLJ07-IC-TAB) TO
                     SGTSLJ07-NM-AG-CLSC-RSCO-RPST(SGTSLJ07-IC-TAB)
           MOVE IN-AG-CLSC-ATV(SGTSLJ07-IC-TAB) TO
                     SGTSLJ07-IN-AG-CLSC-ATV-RSCO(SGTSLJ07-IC-TAB)
           MOVE CD-TIP-EST-DCS(SGTSLJ07-IC-TAB) TO
                     SGTSLJ07-CD-TIP-EST-DCS-RPST(SGTSLJ07-IC-TAB)
           MOVE CD-USU-ATL-EST(SGTSLJ07-IC-TAB) TO
                     SGTSLJ07-CD-USU-ATL-EST(SGTSLJ07-IC-TAB)
           .
       400000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970101-ABRE-CURSOR-PSQ1         SECTION.
      *-----------------------------------------------------------------
           MOVE 0010                   TO PSC-ERRO-PGM

           EXEC SQL
                OPEN CURSOR-PSQ1
           END-EXEC
           MOVE 'S'                    TO IN-CURSOR-PSQ-ABTO
           .
       970101-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970102-LER-CURSOR-PSQ1          SECTION.
      *-----------------------------------------------------------------
           MOVE 0011                   TO PSC-ERRO-PGM

           INITIALIZE REG-WS
           EXEC SQL
                FETCH NEXT ROWSET
                      CURSOR-PSQ1 FOR 0101 ROWS

               INTO :CD-AG-CLSC-RSCO
                  , :NR-ETRD-AG-CLSC
                  , :TS-EST-AG-CLSC
                  , :NM-AG-CLSC-RSCO
                  , :IN-AG-CLSC-ATV
                  , :CD-TIP-EST-DCS
                  , :CD-USU-ATL-EST
           END-EXEC

           MOVE SQLERRD(3)             TO QT-REG-SELD
           IF  QT-REG-SELD EQUAL 0101
               SET SGTSLJ07-TEM-MAIS-REGISTRO TO TRUE
               MOVE 0100                TO QT-REG-SELD
           END-IF
           MOVE QT-REG-SELD            TO SGTSLJ07-QT-OCR-TAB
           MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ
           .
       970102-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970103-FECHA-CURSOR-PSQ1        SECTION.
      *-----------------------------------------------------------------
           MOVE 0012                   TO PSC-ERRO-PGM

           MOVE 'N'                    TO IN-CURSOR-PSQ-ABTO
           EXEC SQL
                CLOSE CURSOR-PSQ1
           END-EXEC
           .
       970103-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970104-ABRE-CURSOR-PSQ2         SECTION.
      *-----------------------------------------------------------------
           MOVE 0013                   TO PSC-ERRO-PGM

           EXEC SQL
                OPEN CURSOR-PSQ2
           END-EXEC
           MOVE 'S'                    TO IN-CURSOR-PSQ-ABTO
           .
       970104-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970105-LER-CURSOR-PSQ2          SECTION.
      *-----------------------------------------------------------------
           MOVE 0014                   TO PSC-ERRO-PGM

           INITIALIZE REG-WS
           EXEC SQL
                FETCH NEXT ROWSET
                      CURSOR-PSQ2 FOR 0101 ROWS
               INTO :CD-AG-CLSC-RSCO
                  , :NR-ETRD-AG-CLSC
                  , :TS-EST-AG-CLSC
                  , :NM-AG-CLSC-RSCO
                  , :IN-AG-CLSC-ATV
                  , :CD-TIP-EST-DCS
                  , :CD-USU-ATL-EST
           END-EXEC

           MOVE SQLERRD(3)             TO QT-REG-SELD
           IF  QT-REG-SELD EQUAL 0101
               SET SGTSLJ07-TEM-MAIS-REGISTRO TO TRUE
               MOVE 0100                TO QT-REG-SELD
           END-IF
           MOVE QT-REG-SELD            TO SGTSLJ07-QT-OCR-TAB
           MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ
           .
       970105-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970106-FECHA-CURSOR-PSQ2        SECTION.
      *-----------------------------------------------------------------
           MOVE 0015                   TO PSC-ERRO-PGM

           MOVE 'N'                    TO IN-CURSOR-PSQ-ABTO
           EXEC SQL
                CLOSE CURSOR-PSQ2
           END-EXEC
           .
       970106-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970107-ABRE-CURSOR-PSQ3         SECTION.
      *-----------------------------------------------------------------
           MOVE 0016                   TO PSC-ERRO-PGM

           EXEC SQL
                OPEN CURSOR-PSQ3
           END-EXEC
           MOVE 'S'                    TO IN-CURSOR-PSQ-ABTO
           .
       970107-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970108-LER-CURSOR-PSQ3          SECTION.
      *-----------------------------------------------------------------
           MOVE 0017                   TO PSC-ERRO-PGM

           INITIALIZE REG-WS
           EXEC SQL
                FETCH NEXT ROWSET
                      CURSOR-PSQ3 FOR 0101 ROWS
               INTO :CD-AG-CLSC-RSCO
                  , :NR-ETRD-AG-CLSC
                  , :TS-EST-AG-CLSC
                  , :NM-AG-CLSC-RSCO
                  , :IN-AG-CLSC-ATV
                  , :CD-TIP-EST-DCS
                  , :CD-USU-ATL-EST
           END-EXEC

           MOVE SQLERRD(3)             TO QT-REG-SELD
           IF  QT-REG-SELD EQUAL 0101
               SET SGTSLJ07-TEM-MAIS-REGISTRO TO TRUE
               MOVE 0100                TO QT-REG-SELD
           END-IF
           MOVE QT-REG-SELD            TO SGTSLJ07-QT-OCR-TAB
           MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ
           .
       970108-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970109-FECHA-CURSOR-PSQ3        SECTION.
      *-----------------------------------------------------------------
           MOVE 0018                   TO PSC-ERRO-PGM

           MOVE 'N'                    TO IN-CURSOR-PSQ-ABTO
           EXEC SQL
                CLOSE CURSOR-PSQ3
           END-EXEC
           .
        970109-SAI.
           EXIT.

      *--------------------
      * 999000-ERRO                    SECTION
      *--------------------
      * 999001-ERRO-001.
      *--------------------
-INC HLPKE004
-INC HLPKE012

           PERFORM  999999-ENCERRAR
           .
       999001-SAI.
           EXIT.

      *-----------------
      * 999002-ERRO-DB2
      *-----------------
-INC HLPKE005
      *-----------------
      * 999003-TRATA-ABEND-CICS
      * 999004-TRATA-CONDITION-CICS
      *-----------------
-INC HLPKE006
      *-----------------
      *-----------------------------------------------------------------
      *    As Sections e Books abaixo fazem o tratamento de erro
      *    através do Sistema MSG (rotina MSGS0001)
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       999000-TRATAR-ERRO-MSG          SECTION.
      *--> Envia a Ocorrência de Erro para o Sistema de Mensageria
      *-----------------------------------------------------------------

      *--> Alterar o MOVE conforme o Book do módulo
           MOVE  SGTSLJ07-TIP-RTN-PGM  TO  TIP-RTN-PGM
      *--> Alterar os Campos de Entrada conforme o Book do módulo
           PERFORM  999100-MONITORAR-ENTRADA

      *--> Movimenta Campos Básicos da Ocorrência de Erro
           MOVE  CD-RTN-PGM            TO  MSG1-CD-ERRO-TS
           MOVE  PSC-ERRO-PGM          TO  MSG1-CD-SEQL-ERRO
           MOVE  MSG-ERRO              TO  MSG1-TX-ERRO-TS

      *--> Realiza a tradução do tipo de erro do COMMAREA
      *    para tipos de erro da Mensageria
           EVALUATE  TRUE
               WHEN  ERRO-DE-DB2
                     PERFORM  999200-MONITORAR-SQL
               WHEN  ERRO-DE-CICS
                     PERFORM  999300-MONITORAR-CICS
               WHEN  ERRO-DE-PGM
                     SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               WHEN  ERRO-CODIFICADO
                     SET   ERRO-PROGRAMADO
                                       TO TRUE
               WHEN  OTHER
                     CONTINUE
           END-EVALUATE

      *--> Grava o erro no Sistema MSG
           PERFORM  MSG1-NTF-MSG
           .
       999000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999100-MONITORAR-ENTRADA        SECTION.
      *--> Monitora as variáveis de entrada desde módulo
      *    Alterar os Campos conforme o Book
      *-----------------------------------------------------------------

           MOVE 'SGTSLJ07-CD-PSQ'                TO  MSGKW999-NM-VRV-MNT
           IF SGTSLJ07-CD-PSQ IS NUMERIC
              MOVE SGTSLJ07-CD-PSQ               TO  MCR-S9-15
              MOVE MCR-S9-15                     TO  MSGKW999-VL-VRV-MNT
           ELSE
              MOVE 'Valor não numérico'          TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTSLJ07-TS-EST-AG-CLSC'        TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSLJ07-TS-EST-AG-CLSC         TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTSLJ07-CD-TIP-EST-DCS'        TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSLJ07-CD-TIP-EST-DCS         TO  MCR-S9-15
           MOVE  MCR-S9-15                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTSLJ07-DT-INC-PSQ'            TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSLJ07-DT-INC-PSQ             TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTSLJ07-DT-FIM-PSQ'            TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSLJ07-DT-FIM-PSQ             TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTSLJ07-DT-PSQ'                TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSLJ07-DT-PSQ                 TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT
           .
       999100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999200-MONITORAR-SQL            SECTION.
      *--> Monitora as variáveis do SQLCA (SQL Communication Area)
      *-----------------------------------------------------------------

      *--> Aciona a Monitoração das Variáveis de Erro SQL no MSGS0001
           SET   ERRO-SQL              TO  TRUE

           MOVE  SQLCA                 TO  MSG1-SQLCA

           MOVE 'SQLCODE'              TO  MSGKW999-NM-VRV-MNT
           MOVE  SQLCODE               TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SQLSTATE'             TO  MSGKW999-NM-VRV-MNT
           MOVE  SQLSTATE              TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           PERFORM  VARYING  IC-TAB
                    FROM  0001 BY 0001
                    UNTIL  IC-TAB          GREATER 0012
                    IF  HLPKE001-TAB-ERRO-TX(IC-TAB)
                                           NOT EQUAL SPACES
                        STRING  'DSNTIAR-TX-ERRO(' IC-TAB ')'
                                DELIMITED BY SIZE
                                INTO       MSGKW999-NM-VRV-MNT
                        END-STRING
                        MOVE  HLPKE001-TAB-ERRO-TX(IC-TAB)
                                       TO  MSGKW999-VL-VRV-MNT
                        PERFORM  999500-ADICIONA-VRV-MNT
                    END-IF
           END-PERFORM

           IF  SQLERRML                GREATER  ZEROS
               MOVE 'SQLERRML'         TO  MSGKW999-NM-VRV-MNT
               MOVE  SQLERRML          TO  MSGKW999-VL-VRV-MNT
               PERFORM  999500-ADICIONA-VRV-MNT
           END-IF

           IF  SQLWARN0                EQUAL 'W'
               MOVE 'SQLWARN'          TO  MSGKW999-NM-VRV-MNT
               MOVE  SQLWARN(2:7)      TO  MSGKW999-VL-VRV-MNT
               PERFORM  999500-ADICIONA-VRV-MNT
           END-IF
           .
       999200-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999300-MONITORAR-CICS           SECTION.
      *--> Monitora as variáveis ABEND ou CONDITION do CICS
      *-----------------------------------------------------------------

      *--> Aciona a Monitoração das Variáveis de Erro CICS no MSGS0001
           SET   ERRO-CICS             TO  TRUE

      *--> Tratar e Adicionar EIBRESP 01 e EIBRESP 02
           MOVE  EIBRESP               TO  MSG1-RESP1
           MOVE  EIBRESP2              TO  MSG1-RESP2

           MOVE  MSG1-RESP1            TO  MSGKW999-NUMERO-Z17
           PERFORM  999600-ALINHA-NUMERO-ESQUERDA
           MOVE  MSGKW999-NUMERO-X17   TO  MSGKW999-RESP1-X8

           MOVE  MSG1-RESP2            TO  MSGKW999-NUMERO-Z17
           PERFORM  999600-ALINHA-NUMERO-ESQUERDA
           MOVE  MSGKW999-NUMERO-X17    TO  MSGKW999-RESP2-X8

           MOVE 'EIBRESP 01'           TO  MSGKW999-NM-VRV-MNT
           MOVE  MSGKW999-RESP1-X8     TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'EIBRESP 02'           TO  MSGKW999-NM-VRV-MNT
           MOVE  MSGKW999-RESP2-X8     TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

      *--> Adicionar Outros Campos EIB (Exec Interface Block)
           MOVE 'EIBFN'                TO  MSGKW999-NM-VRV-MNT
           MOVE  EIBFN                 TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'EIBDS'                TO  MSGKW999-NM-VRV-MNT
           MOVE  EIBDS                 TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'EIBRSRCE'             TO  MSGKW999-NM-VRV-MNT
           MOVE  EIBRSRCE              TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'HLPS0201-TX-EIBFN'    TO  MSGKW999-NM-VRV-MNT
           MOVE  HLPS0201-TX-EIBFN     TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'HLPS0201-TX-EIBRESP'  TO  MSGKW999-NM-VRV-MNT
           MOVE  HLPS0201-TX-EIBRESP   TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'Nome CICS'            TO  MSGKW999-NM-VRV-MNT
           MOVE  HLPKE001-NM-CICS      TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'Usuario CICS'         TO  MSGKW999-NM-VRV-MNT
           MOVE  HLPKE001-CHV-USU      TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT
           .
       999300-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999500-ADICIONA-VRV-MNT         SECTION.
      *-----------------------------------------------------------------
           IF  MSGKW999-NM-VRV-MNT         EQUAL SPACES
               MOVE  SPACES            TO  MSGKW999-VL-VRV-MNT
           ELSE
               IF  MSG1-QT-VRV-MNT         LESS 100
                   ADD   0001          TO MSG1-QT-VRV-MNT
                   MOVE  MSGKW999-NM-VRV-MNT
                     TO  MSG1-NM-VRV-MON(MSG1-QT-VRV-MNT)
                   MOVE  MSGKW999-VL-VRV-MNT
                     TO  MSG1-VL-VRV-MON(MSG1-QT-VRV-MNT)
               END-IF
               MOVE SPACES             TO MSGKW999-NM-VRV-MNT
                                          MSGKW999-VL-VRV-MNT
           END-IF
           .
       999500-ADICIONA-VRV-MNT-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999600-ALINHA-NUMERO-ESQUERDA   SECTION.
      *-----------------------------------------------------------------
           MOVE  MSGKW999-NUMERO-Z17   TO  MSGKW999-NUMERO-X17

           UNSTRING  MSGKW999-NUMERO-X17
                     DELIMITED BY ALL ' '
                     INTO MSGKW999-BRANCOS MSGKW999-NUMERO-X17
           END-UNSTRING
           .
       999600-SAI.
           EXIT.

      *----------------------------------------
      * MSG1-NTF-MSG                   SECTION.
      * MSG1-PCE-INC                   SECTION.
      * MSG1-PUT-CTNR-PRM              SECTION.
      * MSG1-PUT-CNTRS-OPC             SECTION.
      * MSG1-PUT-CTNR-PGM-ACND         SECTION.
      * MSG1-PUT-CTNR-OPR-ACND         SECTION.
      * MSG1-PUT-CTNR-SQL              SECTION.
      * MSG1-PUT-CTNR-CICS             SECTION.
      * MSG1-PUT-CTNR-VRV              SECTION.
      * MSG1-EXEC-MSG1                 SECTION.
      * MSG1-GET-CTNR-ERRO             SECTION.
      *----------------------------------------
-INC MSGKPMG1

      *-----------------------------------------------------------------
      *    Fim do Tratamento através do Sistema MSG (rotina MSGS0001)
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       999999-ENCERRAR                 SECTION.
      *-----------------------------------------------------------------

      * 1 - Erro de db2
           IF TIP-RTN-PGM   EQUAL 1
              IF CD-RTN-PGM NOT EQUAL 100 AND -803 AND +803
                 MOVE 9997    TO   CD-RTN-PGM
              END-IF
           END-IF

      * 2 - erro cics
           IF TIP-RTN-PGM EQUAL 2
              IF CD-RTN-PGM  EQUAL ZEROS OR 333
                 MOVE 9999    TO   CD-RTN-PGM
              END-IF
           END-IF
      *
           MOVE  RTN-PGM               TO  SGTSLJ07-RTN-PGM

           IF  EXECUCAO-ONLINE
               PERFORM  999000-TRATAR-ERRO-MSG
           END-IF

           GOBACK
           .
       999999-SAI.
           EXIT.
      *---------------------- FIM SGTSLJ07 ----------------------------