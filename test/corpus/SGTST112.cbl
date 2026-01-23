      *-----------------------------------------------------------------
      * PROGRAMA..: SGTST112
      * ANALISTA..: F8658869 - Roberto Ioshimoto
      * AUTOR.....: F8658869 - Roberto Ioshimoto
      * DATA......: 12.09.2018
      * OBJETIVO..: PERSISTENCIA DA TABELA HST_GRT_ATI_FNCO
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 12.09.2018 - F8658869 DE 2016/145195 - Implantação
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTST112.

      *-----------------------------------------------------------------
       ENVIRONMENT                     DIVISION.
      *-----------------------------------------------------------------
       CONFIGURATION                   SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *-----------------------------------------------------------------
       DATA                            DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       WORKING-STORAGE                 SECTION.
      *-----------------------------------------------------------------
       77  CTE-INICIO-SS               PIC  X(40)  VALUE
                   '*** S.S. COMEÇA AQUI ***'.
       77  CTE-PROG     PIC  X(17) VALUE '*** SGTST112 ***'.
       77  CTE-VERS     PIC  X(06) VALUE 'VRS001'.

      *--> Registro da tabela
       01  REG-CACHE.
           03  CHAVE-CACHE.
               05  CD-CTRA-SIS-DRIV          PIC S9(9) COMP-5 VALUE 0.
               05  NR-SEQL-ATI               PIC S9(9) COMP-5 VALUE 0.
               05  CD-STIT-ATI-FNCO          PIC S9(9) COMP-5 VALUE 0.
               05  NR-GRT-ATI                PIC S9(9) COMP-5 VALUE 0.
               05  TS-HST-GRT-ATI            PIC  X(026) VALUE ' '.
               05  FILLER                    PIC  X(20) VALUE ' '.
           03  DADOS-CACHE.
               05  CD-ESPC-GRT               PIC S9(4) COMP-5 VALUE 0.
               05  CD-FMA-VCLC               PIC S9(4) COMP-5 VALUE 0.
               05  VL-ATZD-GRT-VCLD          PIC S9(13)V9(2) COMP-3
                                             VALUE 0.
               05  VL-OGNL-GRT-VCLD          PIC S9(13)V9(2) COMP-3
                                             VALUE 0.
               05  DT-INC-GRT-ATI            PIC  X(10) VALUE ' '.
               05  DT-INC-GRT-ATI-R REDEFINES
                   DT-INC-GRT-ATI.
                   07 DIA                    PIC  9(2).
                   07 FILLER                 PIC  X(1).
                   07 MES                    PIC  9(2).
                   07 FILLER                 PIC  X(1).
                   07 ANO                    PIC  9(4).
               05  DT-FIM-GRT-ATI            PIC  X(10) VALUE ' '.
               05  DT-FIM-GRT-ATI-R REDEFINES
                   DT-FIM-GRT-ATI.
                   07 DIA                    PIC  9(2).
                   07 FILLER                 PIC  X(1).
                   07 MES                    PIC  9(2).
                   07 FILLER                 PIC  X(1).
                   07 ANO                    PIC  9(4).
               05  CD-EST-GRT-ATI            PIC S9(4) COMP-5 VALUE 0.
               05  CD-USU-MNTC-GRT           PIC  X(8) VALUE ' '.
               05  CD-USU-CNFC-GRT           PIC  X(8) VALUE ' '.
               05  CD-PGM-GRT-ATI            PIC  X(8) VALUE ' '.

      *--> Definição dos Nomes dos Programas Acionados
       01  TAB-PROGRAMAS-CHAMADOS.
           03  IIBP2000                PIC  X(08) VALUE 'IIBP2000'.

      *--> Definição dos Variaveis do Cobolito
-INC ALMKCBL1

      *--> Book do programa de ambiente - CICS5000
      *    Definido na work para preservar as informacoes
-INC HLPKE01B

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------
      * Area de trabalho

-INC SGTK112T


       01  DT-SQL-ATU                       PIC  X(10)
                                                 VALUE '  .  .    '.
       01  DT-SQL-ATU-R REDEFINES DT-SQL-ATU.
           03 DIA                           PIC  9(02).
           03 FILLER                        PIC  X(01).
           03 MES                           PIC  9(02).
           03 FILLER                        PIC  X(01).
           03 ANO                           PIC  9(04).

       01  TS-SQL-ATU                       PIC  X(26) VALUE ' '.

      * Indicadores de nulidades

       01  IND                              PIC S9(4) COMP VALUE 0  .
       01  IND-FIM-C-C1                     PIC  X(1) VALUE 'N'.
       01  CD-USU-CNFC-GRT-N                PIC S9(4) COMP-5 VALUE 0.

      * Subrotinas Utilizadas

      * Books da definicao de dados a serem usados na subrotina de erros
-INC HLPKE001

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

      *-----------------------------------------------------------------
       LINKAGE                         SECTION.
      *-----------------------------------------------------------------
       01  DFHCOMMAREA.
-INC SGTKT112

       01  CTE-FINAL-SS                PIC  X(40)      VALUE
                    '*** S.S. TERMINA AQUI ***'.
      *-----------------------------------------------------------------
       PROCEDURE                       DIVISION USING DFHCOMMAREA.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       000000-INICIO                   SECTION.
      *-----------------------------------------------------------------

           IF  EIBCALEN  NOT EQUAL LENGTH OF  DFHCOMMAREA
               SET   ERRO-CODIFICADO   TO  TRUE
               MOVE  0001              TO  PSC-ERRO-PGM
               MOVE  0220              TO  CD-RTN-PGM
               MOVE '?rea com tamanho inválido - verifique book/local.'
                                       TO  MSG-ERRO
               PERFORM  999001-ERRO-001
           END-IF

           MOVE  0001                  TO  PSC-ERRO-PGM

           IF  NOT EXECUCAO-ONLINE  AND
               NOT EXECUCAO-BATCH
               CALL  CICS5000       USING  CICS5000-COMMAREA
           END-IF

      *--> Habilita Testes no Cobolito e Debug de Operacoes IIB
-INC ALMKCBL2

      *--> WHENEVER DB2
-INC HLPKE002

      *--> WHENEVER CICS
-INC HLPKE003

           PERFORM  100000-VALIDA-PARAMETRO

           EVALUATE TRUE
              WHEN SGTST112-INCLUIR
                   PERFORM  400000-INCLUIR
              WHEN SGTST112-CONSULTAR
                   PERFORM  500000-CONSULTAR
           END-EVALUATE

           GOBACK
           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-VALIDA-PARAMETRO         SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO PSC-ERRO-PGM

           INITIALIZE                      SGTST112-RTN-PGM

           IF  NOT SGTST112-INCLUIR  AND
               NOT SGTST112-CONSULTAR
               SET   ERRO-CODIFICADO   TO  TRUE
               MOVE  0116              TO  CD-RTN-PGM
               MOVE  'Ação inválida.'  TO  MSG-ERRO
               PERFORM  999001-ERRO-001
           END-IF

           PERFORM  130000-VALIDA-CAMPOS

           IF  SGTST112-INCLUIR
               IF  SGTST112-CD-USU-CNFC-GRT EQUAL  SPACES
                   MOVE -1                 TO CD-USU-CNFC-GRT-N
               END-IF
           END-IF
           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       130000-VALIDA-CAMPOS            SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                   TO PSC-ERRO-PGM

           PERFORM  140000-BUSCA-TS-DT
           MOVE CORR HLPKE001-TS-FMTD  TO DT-SQL-ATU-R
           MOVE HLPKE001-TS-FMTD       TO TS-SQL-ATU

           IF  SGTST112-CD-CTRA-SIS-DRIV   NEGATIVE
               SET   ERRO-CODIFICADO   TO  TRUE
               MOVE  0001              TO  CD-RTN-PGM
               MOVE  'Código da carteira sistema drive inválido.'
                                       TO  MSG-ERRO
               PERFORM  999001-ERRO-001
           END-IF

           IF  SGTST112-CD-CTRA-SIS-DRIV   EQUAL  ZEROS
               SET   ERRO-CODIFICADO   TO  TRUE
               MOVE  0002              TO  CD-RTN-PGM
               MOVE  'Código da carteira sistema drive não informado.'
                                       TO  MSG-ERRO
               PERFORM  999001-ERRO-001
           END-IF

           IF  SGTST112-NR-SEQL-ATI        EQUAL  ZEROS
               SET   ERRO-CODIFICADO   TO  TRUE
               MOVE  0003              TO  CD-RTN-PGM
               MOVE  'Número sequencial ativo não informado.'
                                       TO  MSG-ERRO
               PERFORM  999001-ERRO-001
           END-IF

      *     IF  SGTST112-CD-STIT-ATI-FNCO EQUAL  ZEROS
      *         MOVE 'Código subtítulo ativo financeiro não informado.'
      *                                   TO MSG-ERRO
      *         PERFORM  999001-ERRO-001
      *     END-IF

      *     IF  SGTST112-NR-GRT-ATI         EQUAL ZEROS
      *         SET   ERRO-CODIFICADO   TO  TRUE
      *         MOVE  0004              TO  CD-RTN-PGM
      *         MOVE  'Número garantia ativo não informado.'
      *                                 TO  MSG-ERRO
      *         PERFORM  999001-ERRO-001
      *     END-IF

           IF  SGTST112-TS-HST-GRT-ATI     EQUAL SPACES
               SET   ERRO-CODIFICADO   TO  TRUE
               MOVE  0005              TO  CD-RTN-PGM
               MOVE  'Data e hora atual historico garantia ativo não inf
      -              'ormada.'         TO  MSG-ERRO
               PERFORM  999001-ERRO-001
           END-IF

           IF  SGTST112-INCLUIR
               IF  SGTST112-CD-ESPC-GRT    EQUAL ZEROS
                   SET   ERRO-CODIFICADO
                                       TO  TRUE
                   MOVE  0006          TO  CD-RTN-PGM
                   MOVE  'Código espécie garantia não informado.'
                                       TO MSG-ERRO
                   PERFORM  999001-ERRO-001
               END-IF

               IF  SGTST112-CD-FMA-VCLC    EQUAL ZEROS
                   SET   ERRO-CODIFICADO
                                       TO  TRUE
                   MOVE  0007          TO  CD-RTN-PGM
                   MOVE 'Código forma vinculação não informado.'
                                       TO  MSG-ERRO
                   PERFORM  999001-ERRO-001
               END-IF

               IF  SGTST112-VL-ATZD-GRT-VCLD
                                           EQUAL ZEROS
                   SET   ERRO-CODIFICADO
                                       TO  TRUE
                   MOVE  0008          TO  CD-RTN-PGM
                   MOVE  'Valor atualizado garantia vinculado não inform
      -                  'ado.'        TO  MSG-ERRO
                   PERFORM  999001-ERRO-001
               END-IF

               IF  SGTST112-VL-OGNL-GRT-VCLD
                                           EQUAL ZEROS
                   SET   ERRO-CODIFICADO
                                       TO  TRUE
                   MOVE  0009          TO  CD-RTN-PGM
                   MOVE  'Valor original garantia vinculado não informad
      -                  'o.'          TO  MSG-ERRO
                   PERFORM  999001-ERRO-001
               END-IF

               IF  SGTST112-DT-INC-GRT-ATI EQUAL SPACES
                   SET   ERRO-CODIFICADO
                                       TO  TRUE
                   MOVE  0010          TO  CD-RTN-PGM
                   MOVE  'Data inicio garantia ativo não informada.'
                                       TO  MSG-ERRO
                   PERFORM  999001-ERRO-001
               END-IF

               IF  SGTST112-DT-FIM-GRT-ATI
                                           EQUAL SPACES
                   SET   ERRO-CODIFICADO
                                       TO  TRUE
                   MOVE  0011          TO  CD-RTN-PGM
                   MOVE  'Data fim garantia ativo não informada.'
                                       TO MSG-ERRO
                   PERFORM  999001-ERRO-001
               END-IF

               IF  SGTST112-CD-EST-GRT-ATI
                                           EQUAL ZEROS
                   SET   ERRO-CODIFICADO
                                       TO  TRUE
                   MOVE  0012          TO  CD-RTN-PGM
                   MOVE  'Código estado garantia ativo não informado.'
                                       TO  MSG-ERRO
                   PERFORM  999001-ERRO-001
               END-IF

               IF  SGTST112-CD-USU-MNTC-GRT
                                           EQUAL SPACES
                   SET   ERRO-CODIFICADO
                                       TO  TRUE
                   MOVE  0013          TO  CD-RTN-PGM
                   MOVE 'Código usuário manutenção garantia não informad
      -                 'o.'           TO  MSG-ERRO
                   PERFORM  999001-ERRO-001
               END-IF

      *         IF  SGTST112-CD-PGM-GRT-ATI EQUAL  SPACES
      *             MOVE 'Código programa garantia ativo não informado.'
      *                                   TO MSG-ERRO
      *             PERFORM  999001-ERRO-001
      *         END-IF
           END-IF

           IF  SGTST112-TS-HST-GRT-ATI     EQUAL  'CURRENT'
               MOVE TS-SQL-ATU         TO SGTST112-TS-HST-GRT-ATI
           END-IF

           IF  SGTST112-DT-INC-GRT-ATI     EQUAL  'CURRENT'
               MOVE DT-SQL-ATU         TO SGTST112-DT-INC-GRT-ATI
           END-IF

           IF  SGTST112-DT-FIM-GRT-ATI     EQUAL  'CURRENT'
               MOVE DT-SQL-ATU         TO SGTST112-DT-FIM-GRT-ATI
           END-IF
           .
       130000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       140000-BUSCA-TS-DT              SECTION.
      *-----------------------------------------------------------------
           MOVE 0004                   TO PSC-ERRO-PGM

           CALL  SBTIMEDT           USING  HLPKE001-CD-FUC
                                           HLPKE001-AREA-TRB
                                           HLPKE001-FMT-TS
                                           HLPKE001-TAM-TS
                                           HLPKE001-TS-FMTD

           IF  RETURN-CODE                 NOT EQUAL  ZEROS
               SET   ERRO-DE-PGM       TO  TRUE
               MOVE  0050              TO  CD-RTN-PGM
               MOVE  'SBTIMEDT'        TO  PGM-ERRO
               MOVE  'Erro ao obter data e hora atual.'
                                       TO  MSG-ERRO
              PERFORM  999001-ERRO-001
           END-IF
           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       400000-INCLUIR                  SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                      TO PSC-ERRO-PGM

           MOVE SGTST112-CD-CTRA-SIS-DRIV TO T112-CD-CTRA-SIS-DRIV
           MOVE SGTST112-NR-SEQL-ATI      TO T112-NR-SEQL-ATI
           MOVE SGTST112-CD-STIT-ATI-FNCO TO T112-CD-STIT-ATI-FNCO
           MOVE SGTST112-NR-GRT-ATI       TO T112-NR-GRT-ATI
           MOVE SGTST112-TS-HST-GRT-ATI   TO T112-TS-HST-GRT-ATI
           MOVE SGTST112-CD-ESPC-GRT      TO T112-CD-ESPC-GRT
           MOVE SGTST112-CD-FMA-VCLC      TO T112-CD-FMA-VCLC
           MOVE SGTST112-VL-ATZD-GRT-VCLD TO T112-VL-ATZD-GRT-VCLD
           MOVE SGTST112-VL-OGNL-GRT-VCLD TO T112-VL-OGNL-GRT-VCLD
           MOVE SGTST112-DT-INC-GRT-ATI   TO T112-DT-INC-GRT-ATI
           MOVE SGTST112-DT-FIM-GRT-ATI   TO T112-DT-FIM-GRT-ATI
           MOVE SGTST112-CD-EST-GRT-ATI   TO T112-CD-EST-GRT-ATI
           MOVE SGTST112-CD-USU-MNTC-GRT  TO T112-CD-USU-MNTC-GRT
           MOVE SGTST112-CD-USU-CNFC-GRT  TO T112-CD-USU-CNFC-GRT
           MOVE 'SGTST112'                TO T112-CD-PGM-GRT-ATI
      *
      * Desligar erro automatico do DB2
      *
           EXEC SQL WHENEVER SQLERROR CONTINUE END-EXEC
      *
           EXEC SQL
                INSERT INTO DB2SGT.HST_GRT_ATI_FNCO
                ( CD_CTRA_SIS_DRIV
                , NR_SEQL_ATI
                , CD_STIT_ATI_FNCO
                , NR_GRT_ATI
                , TS_HST_GRT_ATI
                , CD_ESPC_GRT
                , CD_FMA_VCLC
                , VL_ATZD_GRT_VCLD
                , VL_OGNL_GRT_VCLD
                , DT_INC_GRT_ATI
                , DT_FIM_GRT_ATI
                , CD_EST_GRT_ATI
                , CD_USU_MNTC_GRT
                , CD_USU_CNFC_GRT
                , CD_PGM_GRT_ATI )
                VALUES
                ( :T112-CD-CTRA-SIS-DRIV
                , :T112-NR-SEQL-ATI
                , :T112-CD-STIT-ATI-FNCO
                , :T112-NR-GRT-ATI
                , :T112-TS-HST-GRT-ATI
                , :T112-CD-ESPC-GRT
                , :T112-CD-FMA-VCLC
                , :T112-VL-ATZD-GRT-VCLD
                , :T112-VL-OGNL-GRT-VCLD
                , :T112-DT-INC-GRT-ATI
                , :T112-DT-FIM-GRT-ATI
                , :T112-CD-EST-GRT-ATI
                , :T112-CD-USU-MNTC-GRT
                , :T112-CD-USU-CNFC-GRT
                , :T112-CD-PGM-GRT-ATI )
           END-EXEC
      *
      * Ligar erro automatico do DB2
      *
           EXEC SQL WHENEVER SQLERROR   GO TO 999002-ERRO-DB2 END-EXEC
      *
           EVALUATE  SQLCODE
               WHEN  -803
                     MOVE  'Inclusão não efetuada, registro existente.'
                                       TO  MSG-ERRO
                     MOVE  SQLCODE     TO  CD-RTN-PGM
                     PERFORM  999001-ERRO-001
               WHEN  ZEROS
                     CONTINUE
               WHEN  OTHER
      * Aciona o tratamento padrao de erro padrao para os demais casos
                     PERFORM  999002-ERRO-DB2
           END-EVALUATE

           PERFORM  980101-GUARDA-CACHE
           .
       400000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       500000-CONSULTAR                SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                      TO PSC-ERRO-PGM

           EXEC SQL
                 DECLARE CURSOR-PR1  CURSOR FOR
                 SELECT CD_ESPC_GRT
                      , CD_FMA_VCLC
                      , VL_ATZD_GRT_VCLD
                      , VL_OGNL_GRT_VCLD
                      , DT_INC_GRT_ATI
                      , DT_FIM_GRT_ATI
                      , CD_EST_GRT_ATI
                      , CD_USU_MNTC_GRT
                      , CD_USU_CNFC_GRT
                      , CD_PGM_GRT_ATI
                       FROM DB2SGT.HST_GRT_ATI_FNCO
                WHERE CD_CTRA_SIS_DRIV   = :SGTST112-CD-CTRA-SIS-DRIV
                  AND NR_SEQL_ATI        = :SGTST112-NR-SEQL-ATI
                  AND CD_STIT_ATI_FNCO   = :SGTST112-CD-STIT-ATI-FNCO
                  AND NR_GRT_ATI         = :SGTST112-NR-GRT-ATI
                ORDER BY TS_HST_GRT_ATI DESC
                FETCH FIRST 2 ROWS ONLY
           END-EXEC

           EXEC SQL
              OPEN  CURSOR-PR1
           END-EXEC

           IF  SQLCODE                     NOT EQUAL  ZEROS
               MOVE  'Erro ao iniciar histórico da garantia.'
                                       TO  MSG-ERRO
               MOVE  SQLCODE           TO  CD-RTN-PGM
               PERFORM  999001-ERRO-001
           END-IF

           MOVE 'N' TO IND-FIM-C-C1
           PERFORM  510000-FETCH-CTRA-HST
                    UNTIL IND-FIM-C-C1     EQUAL 'S'

           EXEC SQL
              CLOSE  CURSOR-PR1
           END-EXEC

           IF  SQLCODE                     NOT EQUAL  ZEROS
               MOVE 'Erro ao finalizar histórico da garantia.'
                                       TO MSG-ERRO
               MOVE SQLCODE            TO CD-RTN-PGM
               PERFORM  999001-ERRO-001
           END-IF
           .
       500000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       510000-FETCH-CTRA-HST           SECTION.
      *-----------------------------------------------------------------
           EXEC SQL
              FETCH CURSOR-PR1
                   INTO  :T112-CD-ESPC-GRT
                       , :T112-CD-FMA-VCLC
                       , :T112-VL-ATZD-GRT-VCLD
                       , :T112-VL-OGNL-GRT-VCLD
                       , :T112-DT-INC-GRT-ATI
                       , :T112-DT-FIM-GRT-ATI
                       , :T112-CD-EST-GRT-ATI
                       , :T112-CD-USU-MNTC-GRT
                       , :T112-CD-USU-CNFC-GRT
                         :CD-USU-CNFC-GRT-N
                       , :T112-CD-PGM-GRT-ATI
           END-EXEC

           EVALUATE SQLCODE
           WHEN 0
             ADD 01 TO IND
             IF  IND                       EQUAL  0002
                 MOVE T112-CD-ESPC-GRT      TO SGTST112-CD-ESPC-GRT
                 MOVE T112-CD-FMA-VCLC      TO SGTST112-CD-FMA-VCLC
                 MOVE T112-VL-ATZD-GRT-VCLD TO SGTST112-VL-ATZD-GRT-VCLD
                 MOVE T112-VL-OGNL-GRT-VCLD TO SGTST112-VL-OGNL-GRT-VCLD
                 MOVE T112-DT-INC-GRT-ATI   TO SGTST112-DT-INC-GRT-ATI
                 MOVE T112-DT-FIM-GRT-ATI   TO SGTST112-DT-FIM-GRT-ATI
                 MOVE T112-CD-EST-GRT-ATI   TO SGTST112-CD-EST-GRT-ATI
                 MOVE T112-CD-USU-MNTC-GRT  TO SGTST112-CD-USU-MNTC-GRT
                 MOVE T112-CD-USU-CNFC-GRT  TO SGTST112-CD-USU-CNFC-GRT
                 MOVE T112-CD-PGM-GRT-ATI   TO SGTST112-CD-PGM-GRT-ATI
             END-IF
           WHEN +100
             MOVE 'S' TO IND-FIM-C-C1
           WHEN OTHER
             MOVE 'Erro na leitura histórico garantia.'
                                       TO MSG-ERRO
             MOVE SQLCODE              TO CD-RTN-PGM
             PERFORM  999001-ERRO-001
           END-EVALUATE

           IF  IND                         EQUAL  ZEROS
               MOVE  'Registro inexistente.'  TO  MSG-ERRO
               PERFORM  999001-ERRO-001
           END-IF

           PERFORM  980101-GUARDA-CACHE
           .
       510000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       980101-GUARDA-CACHE             SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO PSC-ERRO-PGM

           MOVE SGTST112-CHAVE         TO CHAVE-CACHE
           MOVE SGTST112-AREA-DADOS    TO DADOS-CACHE
           .
       980101-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999000-ERROS                    SECTION.
      *-----------------------------------------------------------------
      *----------------------------
      * 999001-ERRO-001.
      *----------------------------
-INC HLPKE004
-INC HLPKE012
           MOVE  RTN-PGM               TO  SGTST112-RTN-PGM

           PERFORM  999999-ENCERRAR

           .
       999000-SAI.
           EXIT.

      *----------------------------
      * 999002-ERRO-DB2
      *----------------------------
-INC HLPKE005

      *----------------------------
      * 999003-TRATA-ABEND-CICS
      * 999004-TRATA-CONDITION-CICS
      *----------------------------
-INC HLPKE006

      *-----------------------------------------------------------------
       999999-ENCERRAR                 SECTION.
      *-----------------------------------------------------------------

            GOBACK
           .
       999999-SAI.
           EXIT.
      *---------------------- FIM SGTST112 ----------------------------