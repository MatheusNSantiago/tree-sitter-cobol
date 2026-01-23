      *-----------------------------------------------------------------
      * Programa..: SGTS0513
      * Analista..: F4961858 João Pedro Soares Ferreira
      * Autor.....: F4961858 João Pedro Soares Ferreira
      * Data......: 22.08.2024
      * Objetivo..: Tratar vínculos das movimentações da tabela
      *             DB2SGT.DMTV_CX_TIT na tabela DB2SGT.RCBT_RLZD_OPR
      * Compilação: Cobol/CICS
      *-----------------------------------------------------------------
      * VRS001 22.08.2024 F4961858 - IMPLANTAÇÃO
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTS0513.

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

       77  CTE-INICIO-SS               PIC  X(40) VALUE
                                       '*** S.S. Comeca Aqui ***'.
       77  CTE-PROG                    PIC  X(18) VALUE
                                       '*** SGTS0513 ***'.
       77  CTE-VERS                    PIC  X(06) VALUE 'VRS001'.

      *--> Subrotinas utilizadas neste programa
       01  TAB-PROGRAMAS-CHAMADOS.
           03  SGTST087                PIC  X(08) VALUE 'SGTST087'.
           03  SGTSL087                PIC  X(08) VALUE 'SGTSL087'.
           03  SBABEND                 PIC  X(08) VALUE 'SBABEND'.

       77  SBSORT                      PIC  X(08) VALUE 'SBSORT  '.
       77  CURRENT                     PIC  X(07) VALUE 'CURRENT'.

       77  IN-DVCL                     PIC  9(01)  VALUE 0.
           88 DESVINCULACAO                        VALUE 1.
           88 DESVINCULACAO-2                      VALUE 2.

       77  IND-FIM-RCBT-PRVT           PIC  9(01)  VALUE 0.
           88 CND-FIM-RCBT-PRVT                    VALUE 1.

       77  IND-FIM-RCBT-RLZD           PIC  9(01)  VALUE 0.
           88 CND-FIM-RCBT-RLZD                    VALUE 1.

       77  IND-FIM-RCBT-RLZD-VCL       PIC  9(01)  VALUE 0.
           88 CND-FIM-RCBT-RLZD-VCL                VALUE 1.

      *--> Book do programa de ambiente - CICS5000
      *    Definido na work para preservar as informacoes
-INC HLPKE01B

      *----------------------------------------------------------------*
      *                     Containers utilizados
      *----------------------------------------------------------------*

      * Container de erro
       77  CTNR-PRM-ERRO               PIC  X(16) VALUE 'PRM-ERRO'.

      * Container do usuário logado
       77  CTNR-PRM-INF-OPRD           PIC  X(16) VALUE 'PRM-INF-OPRD'.

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------
      *--> Areas de trabalho
       01  IC-TAB                      PIC S9(04) VALUE ZEROS.

       01  VCL-RCBT-RLZD-OPR.
           05 VCL-CD-CTRA-TIT-VL       PIC S9(9) USAGE COMP-5.
           05 VCL-CD-OPR-ETRD-MCP      PIC S9(11)V USAGE COMP-3.
           05 VCL-NR-VRS-OPR-ETRD      PIC S9(4) USAGE COMP-5.
           05 VCL-NR-SEQL-RCBT-PRVT    PIC S9(4) USAGE COMP-5.
           05 VCL-CD-TIP-RCBT-PRVT     PIC S9(4) USAGE COMP-5.
           05 VCL-DT-RCBT-PRVT         PIC X(10).
           05 VCL-CD-TIT-OPR-ETRD      PIC X(12).
           05 VCL-NR-DVS-TIT           PIC S9(4) USAGE COMP-5.
           05 VCL-TS-REG-PGTO          PIC X(26).
           05 VCL-IN-VCL-MNL           PIC X(1).
           05 VCL-CD-USU-VCL           PIC X(8).
           05 VCL-TS-ULT-ATL           PIC X(26).
           05 VCL-VL-RLZD-VCLD         PIC S9(15)V9(2) USAGE COMP-3.

       01 VL-RLZD-VCLD                  PIC S9(15)V9(2) COMP-3 VALUE 0.
       01 VL-VCLD-OPR                   PIC S9(15)V9(2) COMP-3 VALUE 0.
       01 VL-DVCL-OPR                   PIC S9(15)V9(2) COMP-3 VALUE 0.

       01 DT-EFT-MVTC-X                 PIC X(010).
       01 DT-EFT-MVTC REDEFINES DT-EFT-MVTC-X.
          03 DIA-EFT-MVTC               PIC 9(02).
          03 FILLER                     PIC X(001).
          03 MES-EFT-MVTC               PIC 9(02).
          03 FILLER                     PIC X(001).
          03 ANO-EFT-MVTC               PIC 9(04).

       01 DT-EFT-MVTC-PRVR              PIC 9(08).
       01 DT-EFT-MVTC-DFNT              PIC 9(08).

       01 DT-RCBT-PRVT-X                PIC X(010).
       01 DT-RCBT-PRVT REDEFINES DT-RCBT-PRVT-X.
          03 DIA-RCBT-PRVT              PIC 9(02).
          03 FILLER                     PIC X(001).
          03 MES-RCBT-PRVT              PIC 9(02).
          03 FILLER                     PIC X(001).
          03 ANO-RCBT-PRVT              PIC 9(04).

       01 DT-RCBT-PRVT-PRVR             PIC 9(08).
       01 DT-RCBT-PRVT-DFNT             PIC 9(08).

       01 GDA-VL-RCBT-PRVT         PIC S9(15)V9(2) USAGE COMP-3.

      *-----------------------------------------------------------------
      *    Books Utilizados
      *-----------------------------------------------------------------
      * Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1
      *--> Book para captura de erro via MSG1
-INC MSGKWMG1
      * Books da definicao de dados a serem usados na subrotina de erros
-INC HLPKE001
      *--> Book do Container de Erro
-INC IIBKERRO

      *--> Persistência da tabela RCBT_RLZD_OPR
       01  AREA-SGTST087.
-INC SGTKT087

      *-----------------------------------------------------------------

      * DB2SGT.RCBT_PRVT_OPR_ETRD
-INC SGTK070D

      * DB2SGT.RCBT_RLZD_OPR
-INC SGTK087D

      * DB2SGT.HST_RCBT_RLZD_OPR
-INC SGTK129D


       01  CTE-FINAL-SS                PIC  X(40) VALUE
                                       '*** S.S. Termina Aqui ***'.
      *-----------------------------------------------------------------
      *                    CURSOR
      *-----------------------------------------------------------------

           EXEC SQL
            DECLARE DESVINCULADOS CURSOR FOR
               SELECT CD_CTRA_TIT_VL
                    , CD_OPR_ETRD_MCP
               					, NR_VRS_OPR_ETRD
                    , NR_SEQL_RCBT_PRVT
                    , CD_TIP_RCBT_PRVT
                    , DT_RCBT_PRVT
                    , PC_RCBT_PRVT
                    , VL_RCBT_PRVT
               FROM DB2SGT.RCBT_PRVT_OPR_ETRD
               WHERE
               (CD_CTRA_TIT_VL, CD_OPR_ETRD_MCP, NR_VRS_OPR_ETRD,
                 NR_SEQL_RCBT_PRVT, CD_TIP_RCBT_PRVT, DT_RCBT_PRVT)
               NOT IN
               (
               SELECT
               CD_CTRA_TIT_VL, CD_OPR_ETRD_MCP, NR_VRS_OPR_ETRD,
               NR_SEQL_RCBT_PRVT, CD_TIP_RCBT_PRVT, DT_RCBT_PRVT
               FROM DB2SGT.RCBT_RLZD_OPR
               WHERE
               CD_CTRA_TIT_VL =        :SGTS0513-CD-CTRA-TIT-VL
               AND CD_OPR_ETRD_MCP =   :SGTS0513-CD-OPR-ETRD-MCP
               AND NR_VRS_OPR_ETRD =   :SGTS0513-NR-VRS-OPR-ETRD
               AND NR_SEQL_RCBT_PRVT = :SGTS0513-NR-SEQL-RCBT-PRVT
               AND CD_TIP_RCBT_PRVT =  :SGTS0513-CD-TIP-MVTC
               )
               AND CD_CTRA_TIT_VL =    :SGTS0513-CD-CTRA-TIT-VL
               AND CD_OPR_ETRD_MCP =   :SGTS0513-CD-OPR-ETRD-MCP
               AND NR_VRS_OPR_ETRD =   :SGTS0513-NR-VRS-OPR-ETRD
               AND NR_SEQL_RCBT_PRVT = :SGTS0513-NR-SEQL-RCBT-PRVT
               AND CD_TIP_RCBT_PRVT =  :SGTS0513-CD-TIP-MVTC
               ORDER BY DT_RCBT_PRVT
           END-EXEC

           EXEC SQL
            DECLARE VINCULADOS CURSOR FOR
                SELECT
                  CD_CTRA_TIT_VL
                , CD_OPR_ETRD_MCP
                , NR_VRS_OPR_ETRD
                , NR_SEQL_RCBT_PRVT
                , CD_TIP_RCBT_PRVT
                , DT_RCBT_PRVT
                , CD_TIT_OPR_ETRD
                , NR_DVS_TIT
                , TS_REG_PGTO
                , VL_RLZD_VCLD
                FROM DB2SGT.RCBT_RLZD_OPR
                WHERE
                CD_CTRA_TIT_VL =        :SGTS0513-CD-CTRA-TIT-VL
                AND CD_OPR_ETRD_MCP =   :SGTS0513-CD-OPR-ETRD-MCP
                AND NR_VRS_OPR_ETRD =   :SGTS0513-NR-VRS-OPR-ETRD
                AND NR_SEQL_RCBT_PRVT = :SGTS0513-NR-SEQL-RCBT-PRVT
                AND CD_TIP_RCBT_PRVT =  :SGTS0513-CD-TIP-MVTC
                AND CD_TIT_OPR_ETRD =   :SGTS0513-CD-TIT-OPR-ETRD
                AND NR_DVS_TIT =        :SGTS0513-NR-DVS-TIT
                AND TS_REG_PGTO =       :SGTS0513-TS-REG-PGTO
                ORDER BY DT_RCBT_PRVT DESC
           END-EXEC

           EXEC SQL
            DECLARE VINCULADOS-PRVT CURSOR FOR
                SELECT
                  CD_CTRA_TIT_VL
                , CD_OPR_ETRD_MCP
                , NR_VRS_OPR_ETRD
                , NR_SEQL_RCBT_PRVT
                , CD_TIP_RCBT_PRVT
                , DT_RCBT_PRVT
                , CD_TIT_OPR_ETRD
                , NR_DVS_TIT
                , TS_REG_PGTO
                , VL_RLZD_VCLD
                FROM DB2SGT.RCBT_RLZD_OPR
                WHERE
                    CD_CTRA_TIT_VL =    :087-CD-CTRA-TIT-VL
                AND CD_OPR_ETRD_MCP =   :087-CD-OPR-ETRD-MCP
                AND NR_VRS_OPR_ETRD =   :087-NR-VRS-OPR-ETRD
                AND NR_SEQL_RCBT_PRVT = :087-NR-SEQL-RCBT-PRVT
                AND CD_TIP_RCBT_PRVT =  :087-CD-TIP-RCBT-PRVT
                AND DT_RCBT_PRVT =      :087-DT-RCBT-PRVT
           END-EXEC


           EXEC SQL INCLUDE SQLCA END-EXEC.

      *-----------------------------------------------------------------
       LINKAGE SECTION.
      *-----------------------------------------------------------------
       01  DFHCOMMAREA.
-INC SGTK0513

      *-----------------------------------------------------------------
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *-----------------------------------------------------------------
      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------

           IF  EIBCALEN NOT EQUAL LENGTH OF DFHCOMMAREA
               MOVE  0001              TO  CD-RTN-PGM
               MOVE  'Área com tamanho inválido.'
                                       TO  MSG-ERRO
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           IF NOT EXECUCAO-ONLINE AND NOT EXECUCAO-BATCH
             CALL CICS5000 USING CICS5000-COMMAREA
           END-IF

      * WHENEVER DB2
-INC HLPKE002
      * WHENEVER CICS
-INC HLPKE003
           MOVE 'SGT'  TO SGTS0513-SG-SIS-MSG
           MOVE ZEROES TO SGTS0513-CD-RTN-PGM

           PERFORM 100000-VALIDAR-PARAMETRO
      *
           PERFORM 200000-TRATA-DEMONSTRATIVO

           PERFORM 999999-ENCERRAR
           .

       000000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       100000-VALIDAR-PARAMETRO        SECTION.
      *----------------------------------------------------------------*

           MOVE  0001                  TO  PSC-ERRO-PGM

           IF SGTS0513-CD-CTRA-TIT-VL  = 0
              MOVE 0002                TO CD-RTN-PGM
              MOVE 'Código da carteira de título e valores mobiliários n
      -       'ão informado.' TO  MSG-ERRO
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF.

           IF SGTS0513-CD-TIT-OPR-ETRD  = ' '
              MOVE  0231               TO  CD-RTN-PGM
              MOVE 'Código do título da operação estruturada não informa
      -       'do.'            TO  MSG-ERRO
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF.

           IF SGTS0513-NR-DVS-TIT = 0
              MOVE  0232               TO  CD-RTN-PGM
              MOVE 'Número da divisão do título da operação não informad
      -        'o.'           TO  MSG-ERRO
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF.

           IF SGTS0513-TS-REG-PGTO = ' '
              MOVE  0316               TO  CD-RTN-PGM
              MOVE  'Timestamp do registro pagamento não informado.'
                                       TO  MSG-ERRO
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF.

           IF SGTS0513-VL-EFT-MVTC = 0
              MOVE 0341                TO  CD-RTN-PGM
              MOVE 'Valor efetivo movimentação não numérico.'
                                       TO  MSG-ERRO
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF.

           IF SGTS0513-CD-TIP-MVTC = 0
              MOVE 0337                TO  CD-RTN-PGM
              MOVE 'Código tipo movimentação título inválido.'
                                       TO  MSG-ERRO
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF.

           IF SGTS0513-DT-EFT-MVTC = ' '
              MOVE  0336               TO  CD-RTN-PGM
              MOVE 'Data efetiva da movimentação não informada.'
                                       TO  MSG-ERRO
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF.

           IF SGTS0513-CD-OPR-ETRD-MCP = 0
              MOVE  0003               TO  CD-RTN-PGM
              MOVE 'Código operação estruturada mercado de capitais não
      -              'numérico.'     TO  MSG-ERRO
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF.

           IF SGTS0513-NR-VRS-OPR-ETRD = 0
              MOVE  0006               TO CD-RTN-PGM
              MOVE 'Número da versão da operação estruturada inválido.'
                                       TO  MSG-ERRO
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF.

           IF SGTS0513-NR-SEQL-RCBT-PRVT = 0
              MOVE  0315               TO CD-RTN-PGM
              MOVE 'Número sequencial do recebimento previsto não inform
      -            'ado.'    TO  MSG-ERRO
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF.

       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-BUSCA-TS-DT SECTION.
      *-----------------------------------------------------------------
           MOVE 0011                   TO PSC-ERRO-PGM

           CALL SBTIMEDT            USING HLPKE001-CD-FUC
                                          HLPKE001-AREA-TRB
                                          HLPKE001-FMT-TS
                                          HLPKE001-TAM-TS
                                          HLPKE001-TS-FMTD
           IF  RETURN-CODE      NOT EQUAL ZEROS
               MOVE RETURN-CODE        TO CD-RTN-PGM
               MOVE 'ERRO NA CHAMADA DA SBTIMEDT' TO MSG-ERRO
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       110000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       200000-TRATA-DEMONSTRATIVO     SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                        TO PSC-ERRO-PGM

           MOVE ZEROES TO IND-FIM-RCBT-PRVT
                          IND-FIM-RCBT-RLZD
                          VL-RLZD-VCLD

           MOVE SGTS0513-DT-EFT-MVTC        TO DT-EFT-MVTC-X

           COMPUTE DT-EFT-MVTC-PRVR = (ANO-EFT-MVTC * 10000) +
                   (MES-EFT-MVTC * 100) + DIA-EFT-MVTC
           ON SIZE ERROR
                   SET   ERRO-PROGRAMADO TO  TRUE

                   MOVE  0096              TO  CD-RTN-PGM
                   MOVE  'Erro no formato da variavel DT-EFT-MVTC.'
                                           TO  MSG-ERRO
                   PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-COMPUTE

           COMPUTE DT-EFT-MVTC-DFNT =
                   FUNCTION INTEGER-OF-DATE (DT-EFT-MVTC-PRVR).

      * Inicia tratamento dos registros do demonstrativo de caixa

           PERFORM 310000-BUSCAR-SOMA-VL-RLZD

           IF SGTS0513-CD-TIP-MVTC < 5
              IF SGTS0513-VL-EFT-MVTC >= VL-RLZD-VCLD
                 COMPUTE VL-VCLD-OPR = SGTS0513-VL-EFT-MVTC
                                       - VL-RLZD-VCLD
                 ON SIZE ERROR
                     SET   ERRO-PROGRAMADO TO  TRUE

                     MOVE  0002              TO  CD-RTN-PGM
                     MOVE  'Erro ao calcular valor a ser vinculado.'
                                             TO  MSG-ERRO
                     PERFORM 999000-GRAVA-ERRO-E-ENCERRA
                 END-COMPUTE

                 IF VL-VCLD-OPR > 0
                    PERFORM 320001-ABRE-CUR-DESVINCULADOS

                    PERFORM 320000-LER-DESVINCULADOS
                    PERFORM 210000-TRATA-VINCULACAO
                    UNTIL CND-FIM-RCBT-PRVT

                    MOVE 1 TO SGTS0513-CD-EST-EVT
                 END-IF
              ELSE
                 COMPUTE VL-DVCL-OPR = VL-RLZD-VCLD
                                       - SGTS0513-VL-EFT-MVTC
                 ON SIZE ERROR
      *
                    SET   ERRO-PROGRAMADO TO  TRUE

                    MOVE  0003              TO  CD-RTN-PGM
                    MOVE  'Erro ao calcular valor a ser desvinculado.'
                                             TO  MSG-ERRO
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
                 END-COMPUTE

                 PERFORM 330001-ABRE-CUR-VINCULADOS

                 PERFORM 330000-LER-VINCULADOS
                 PERFORM 220000-TRATA-DESVINCULACAO
                 UNTIL CND-FIM-RCBT-RLZD

                 MOVE 2 TO SGTS0513-CD-EST-EVT
              END-IF
           END-IF
           .

       200999-SAI.
           EXIT.
      *-----------------------------------------------------------------
       210000-TRATA-VINCULACAO     SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                   TO PSC-ERRO-PGM

           MOVE 070-DT-RCBT-PRVT       TO DT-RCBT-PRVT-X

           COMPUTE DT-RCBT-PRVT-PRVR = (ANO-RCBT-PRVT * 10000) +
                   (MES-RCBT-PRVT * 100) + DIA-RCBT-PRVT.

           COMPUTE DT-RCBT-PRVT-DFNT =
                   FUNCTION INTEGER-OF-DATE (DT-RCBT-PRVT-PRVR).

      *Caso seja lançamentos de juros, vincular o valor inteiro em
      * apenas 1 registro do recebimento previsto
           IF SGTS0513-CD-TIP-MVTC EQUAL 1 OR 3
               MOVE VL-VCLD-OPR       TO  GDA-VL-RCBT-PRVT
           ELSE
               MOVE 070-VL-RCBT-PRVT  TO  GDA-VL-RCBT-PRVT
           END-IF
      *
           IF ((GDA-VL-RCBT-PRVT <= VL-VCLD-OPR) AND
              (DT-RCBT-PRVT-DFNT >= DT-EFT-MVTC-DFNT))
      *
              PERFORM 500000-INCLUIR-VINCULO
      *
              COMPUTE VL-VCLD-OPR = VL-VCLD-OPR - GDA-VL-RCBT-PRVT
              ON SIZE ERROR
                    SET   ERRO-PROGRAMADO TO  TRUE

                     MOVE  0002              TO  CD-RTN-PGM
                     MOVE  'Erro ao calcular valor a ser vinculado.'
                                             TO  MSG-ERRO
                     PERFORM 999000-GRAVA-ERRO-E-ENCERRA
              END-COMPUTE
           END-IF

           PERFORM 320000-LER-DESVINCULADOS

      *Caso seja lançamentos de juros vincula apenas no 1o registro
      *encontrado
           IF SGTS0513-CD-TIP-MVTC EQUAL 1 OR 3
              MOVE 1 TO IND-FIM-RCBT-PRVT
              PERFORM 320002-FECHA-CUR-DESVINCULADOS
           END-IF
           .

       210999-SAI.
           EXIT.

      *-----------------------------------------------------------------
       220000-TRATA-DESVINCULACAO     SECTION.
      *-----------------------------------------------------------------
           MOVE 0004                   TO PSC-ERRO-PGM

           MOVE ZEROES TO IND-FIM-RCBT-RLZD
           MOVE 1      TO IC-TAB IN-DVCL

           PERFORM 600000-INCLUIR-HST-RLZD
           PERFORM 510000-EXCLUIR-VINCULO

           COMPUTE VL-DVCL-OPR = VL-DVCL-OPR - 087-VL-RLZD-VCLD
           ON SIZE ERROR
                 SET   ERRO-PROGRAMADO TO  TRUE
                 MOVE  0003              TO  CD-RTN-PGM
                 MOVE  'Erro ao calcular valor a ser desvinculado.'
                                          TO  MSG-ERRO
                 PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-COMPUTE

           EXEC SQL
             OPEN VINCULADOS-PRVT
           END-EXEC
      *
           PERFORM 340000-LER-VINCULADOS-PRVT

           PERFORM 230000-TRATA-DESVINCULACAO-VCL
           UNTIL CND-FIM-RCBT-RLZD-VCL

           IF VL-DVCL-OPR > 0
              CONTINUE
           ELSE
              MOVE 1 TO IND-FIM-RCBT-RLZD
           END-IF

           PERFORM 330000-LER-VINCULADOS
           .

       220999-SAI.
           EXIT.
      *-----------------------------------------------------------------
       230000-TRATA-DESVINCULACAO-VCL     SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                   TO PSC-ERRO-PGM
           MOVE 2                      TO IN-DVCL

           PERFORM 600000-INCLUIR-HST-RLZD
           PERFORM 510000-EXCLUIR-VINCULO

           IF  (087-CD-TIT-OPR-ETRD EQUAL VCL-CD-TIT-OPR-ETRD)
           AND (087-NR-DVS-TIT      EQUAL VCL-NR-DVS-TIT)
           AND (087-TS-REG-PGTO     EQUAL VCL-TS-REG-PGTO)
      *
              COMPUTE VL-DVCL-OPR =
                      VL-DVCL-OPR - VCL-VL-RLZD-VCLD
              ON SIZE ERROR
                    SET   ERRO-PROGRAMADO TO  TRUE
                    MOVE  0003              TO  CD-RTN-PGM
                    MOVE  'Erro ao calcular valor a ser desvinculado.'
                                             TO  MSG-ERRO
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
              END-COMPUTE
           ELSE
              PERFORM 340000-LER-VINCULADOS-PRVT
           END-IF
           .

       230999-SAI.
           EXIT.
      *-----------------------------------------------------------------
       320000-LER-DESVINCULADOS     SECTION.
      *-----------------------------------------------------------------
           EXEC SQL
             FETCH DESVINCULADOS
             INTO :070-CD-CTRA-TIT-VL ,
                  :070-CD-OPR-ETRD-MCP ,
                  :070-NR-VRS-OPR-ETRD ,
                  :070-NR-SEQL-RCBT-PRVT ,
                  :070-CD-TIP-RCBT-PRVT ,
                  :070-DT-RCBT-PRVT ,
                  :070-PC-RCBT-PRVT ,
                  :070-VL-RCBT-PRVT
           END-EXEC

           EVALUATE SQLCODE
               WHEN 0
                 CONTINUE
               WHEN +100
                 PERFORM 320002-FECHA-CUR-DESVINCULADOS

                 MOVE 1 TO IND-FIM-RCBT-PRVT
               WHEN OTHER
                 PERFORM 320002-FECHA-CUR-DESVINCULADOS

                 SET SGTS0513-ERRO-DE-DB2 TO TRUE

                 MOVE  SQLCODE            TO  CD-RTN-PGM
                 MOVE  'Erro no FETCH do cursor DESVINCULADOS.'
                                          TO  MSG-ERRO
                 PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE
           .

       320999-SAI.
           EXIT.
      *-----------------------------------------------------------------
       320001-ABRE-CUR-DESVINCULADOS           SECTION.
      *-----------------------------------------------------------------
           EXEC SQL
              OPEN DESVINCULADOS
           END-EXEC
           EVALUATE SQLCODE
               WHEN 0
                 CONTINUE
               WHEN OTHER
                 SET SGTS0513-ERRO-DE-DB2 TO TRUE

                 MOVE  SQLCODE            TO  CD-RTN-PGM
                 MOVE  'Erro no OPEN do cursor DESVINCULADOS.'
                                          TO  MSG-ERRO
                 PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

            .

       320999-SAI.
           EXIT.

      *-----------------------------------------------------------------
       320002-FECHA-CUR-DESVINCULADOS           SECTION.
      *-----------------------------------------------------------------
           EXEC SQL
              CLOSE DESVINCULADOS
           END-EXEC
           EVALUATE SQLCODE
               WHEN 0
                 CONTINUE
               WHEN OTHER
                 SET SGTS0513-ERRO-DE-DB2 TO TRUE

                 MOVE  SQLCODE            TO  CD-RTN-PGM
                 MOVE  'Erro no CLOSE do cursor DESVINCULADOS.'
                                          TO  MSG-ERRO
                 PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

            .

       320999-SAI.
           EXIT.

      *-----------------------------------------------------------------
       330000-LER-VINCULADOS           SECTION.
      *-----------------------------------------------------------------
           EXEC SQL
             FETCH VINCULADOS
             INTO :087-CD-CTRA-TIT-VL ,
                  :087-CD-OPR-ETRD-MCP ,
                  :087-NR-VRS-OPR-ETRD ,
                  :087-NR-SEQL-RCBT-PRVT ,
                  :087-CD-TIP-RCBT-PRVT ,
                  :087-DT-RCBT-PRVT ,
                  :087-CD-TIT-OPR-ETRD ,
                  :087-NR-DVS-TIT ,
                  :087-TS-REG-PGTO ,
                  :087-VL-RLZD-VCLD
           END-EXEC

           EVALUATE SQLCODE
               WHEN 0
                 CONTINUE
               WHEN +100
                 PERFORM 330002-FECHA-CUR-VINCULADOS

                 MOVE 1 TO IND-FIM-RCBT-RLZD
               WHEN OTHER
                 PERFORM 330002-FECHA-CUR-VINCULADOS

                 SET SGTS0513-ERRO-DE-DB2 TO TRUE

                 MOVE  SQLCODE            TO  CD-RTN-PGM
                 MOVE  'Erro no FETCH do cursor VINCULADOS.'
                                          TO  MSG-ERRO
                 PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE
           .

       330999-SAI.
           EXIT.

      *-----------------------------------------------------------------
       330001-ABRE-CUR-VINCULADOS           SECTION.
      *-----------------------------------------------------------------
           EXEC SQL
              OPEN VINCULADOS
           END-EXEC
           EVALUATE SQLCODE
               WHEN 0
                 CONTINUE
               WHEN OTHER
                 SET SGTS0513-ERRO-DE-DB2 TO TRUE

                 MOVE  SQLCODE            TO  CD-RTN-PGM
                 MOVE  'Erro no OPEN do cursor VINCULADOS.'
                                          TO  MSG-ERRO
                 PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

            .

       330999-SAI.
           EXIT.

      *-----------------------------------------------------------------
       330002-FECHA-CUR-VINCULADOS           SECTION.
      *-----------------------------------------------------------------
           EXEC SQL
              CLOSE VINCULADOS
           END-EXEC
           EVALUATE SQLCODE
               WHEN 0
                 CONTINUE
               WHEN OTHER
                 SET SGTS0513-ERRO-DE-DB2 TO TRUE

                 MOVE  SQLCODE            TO  CD-RTN-PGM
                 MOVE  'Erro no CLOSE do cursor VINCULADOS.'
                                          TO  MSG-ERRO
                 PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

            .

       330999-SAI.
           EXIT.


      *-----------------------------------------------------------------
       340000-LER-VINCULADOS-PRVT           SECTION.
      *-----------------------------------------------------------------
           EXEC SQL
             FETCH VINCULADOS-PRVT
             INTO :VCL-CD-CTRA-TIT-VL ,
                  :VCL-CD-OPR-ETRD-MCP ,
                  :VCL-NR-VRS-OPR-ETRD ,
                  :VCL-NR-SEQL-RCBT-PRVT ,
                  :VCL-CD-TIP-RCBT-PRVT ,
                  :VCL-DT-RCBT-PRVT ,
                  :VCL-CD-TIT-OPR-ETRD ,
                  :VCL-NR-DVS-TIT ,
                  :VCL-TS-REG-PGTO ,
                  :VCL-VL-RLZD-VCLD
           END-EXEC

           EVALUATE SQLCODE
               WHEN 0
                 CONTINUE
               WHEN +100
                 EXEC SQL
                   CLOSE VINCULADOS-PRVT
                 END-EXEC

                 MOVE 1 TO IND-FIM-RCBT-RLZD-VCL
               WHEN OTHER
                 EXEC SQL
                   CLOSE VINCULADOS-PRVT
                 END-EXEC

                 SET SGTS0513-ERRO-DE-DB2 TO TRUE

                 MOVE  SQLCODE            TO  CD-RTN-PGM
                 MOVE  'Erro no FETCH do cursor VINCULADOS-PRVT.'
                                          TO  MSG-ERRO
                 PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE
           .

       340999-SAI.
           EXIT.
      *-----------------------------------------------------------------
       310000-BUSCAR-SOMA-VL-RLZD     SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO PSC-ERRO-PGM

           EXEC SQL
               SELECT COALESCE(SUM(VL_RLZD_VCLD), 0)
               INTO :VL-RLZD-VCLD
               FROM DB2SGT.RCBT_RLZD_OPR
               WHERE
                   CD_CTRA_TIT_VL =    :SGTS0513-CD-CTRA-TIT-VL
               AND CD_OPR_ETRD_MCP =   :SGTS0513-CD-OPR-ETRD-MCP
               AND NR_VRS_OPR_ETRD =   :SGTS0513-NR-VRS-OPR-ETRD
               AND NR_SEQL_RCBT_PRVT = :SGTS0513-NR-SEQL-RCBT-PRVT
               AND CD_TIP_RCBT_PRVT =  :SGTS0513-CD-TIP-MVTC
               AND CD_TIT_OPR_ETRD =   :SGTS0513-CD-TIT-OPR-ETRD
               AND NR_DVS_TIT =        :SGTS0513-NR-DVS-TIT
               AND TS_REG_PGTO =       :SGTS0513-TS-REG-PGTO
           END-EXEC

           EVALUATE SQLCODE
               WHEN 0
                 CONTINUE
               WHEN OTHER
                 SET SGTS0513-ERRO-DE-DB2 TO TRUE

                 MOVE  SQLCODE            TO  CD-RTN-PGM
                 STRING  'Não existem recebimentos previstos para'
                         'o movimento.' DELIMITED BY SIZE
                                          INTO  MSG-ERRO
                 PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE
           .

       310999-SAI.
           EXIT.
      *-----------------------------------------------------------------
						 500000-INCLUIR-VINCULO          SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO  PSC-ERRO-PGM

           INITIALIZE                      SGTST087-RTN-PGM
                                           SGTST087-AREA-DADOS
                                           REPLACING NUMERIC
                                           BY  ZEROS

           MOVE  SPACES                TO  SGTST087-RTN-PGM
                                           SGTST087-AREA-DADOS

      *--> Incluir
           MOVE 1 TO SGTST087-ACAO

      *--> Movimenta Operação Estruturada
           MOVE  070-CD-CTRA-TIT-VL
                                       TO  SGTST087-CD-CTRA-TIT-VL
           MOVE  070-CD-OPR-ETRD-MCP
                                       TO  SGTST087-CD-OPR-ETRD-MCP
           MOVE  070-NR-VRS-OPR-ETRD
                                       TO  SGTST087-NR-VRS-OPR-ETRD

      *--> Movimenta Chave do Recebimento Previsto
           MOVE  070-NR-SEQL-RCBT-PRVT
                                       TO  SGTST087-NR-SEQL-RCBT-PRVT

           MOVE  070-CD-TIP-RCBT-PRVT
                                       TO  SGTST087-CD-TIP-RCBT-PRVT
           MOVE  070-DT-RCBT-PRVT
                                       TO  SGTST087-DT-RCBT-PRVT

      *--> Movimenta Chave do Demonstrativo de Caixa
           MOVE  SGTS0513-CD-TIT-OPR-ETRD
                                       TO  SGTST087-CD-TIT-OPR-ETRD
           MOVE  SGTS0513-NR-DVS-TIT
                                       TO  SGTST087-NR-DVS-TIT
           MOVE  SGTS0513-TS-REG-PGTO
                                       TO  SGTST087-TS-REG-PGTO

      *--> Movimenta Recebimento Realizado
           MOVE  'N'
                                       TO  SGTST087-IN-VCL-MNL
           MOVE  SGTS0513-CD-USU-IMP
                                       TO  SGTST087-CD-USU-VCL
           MOVE  'CURRENT'
                                       TO  SGTST087-TS-ULT-ATL(1:7)
           MOVE  GDA-VL-RCBT-PRVT
                                       TO  SGTST087-VL-RLZD-VCLD

      *
           IF EXECUCAO-ONLINE
              EXEC CICS LINK
                        PROGRAM ( SGTST087       )
                        COMMAREA( AREA-SGTST087  )
                        RESP    ( MSG1-RESP1     )
                        RESP2   ( MSG1-RESP2     )
              END-EXEC
              IF  MSG1-RESP1 NOT EQUAL  DFHRESP(NORMAL)
                  SET SGTS0513-ERRO-DE-CICS TO TRUE
                  MOVE  026            TO  CD-RTN-PGM
                  MOVE  'Erro ao acionar o módulo SGTST087.'
                                       TO  MSG-ERRO
                  PERFORM 999000-GRAVA-ERRO-E-ENCERRA
              END-IF
           ELSE
              MOVE LENGTH OF               AREA-SGTST087 TO EIBCALEN
              CALL SGTST087 USING DFHEIBLK AREA-SGTST087
           END-IF

           IF SGTST087-CD-RTN-PGM NOT = 0
               MOVE SGTST087-RTN-PGM   TO RTN-PGM
               MOVE SGTST087-MSG-ERRO-PGM TO SGTS0513-MSG-ERRO-PGM
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .

       500999-SAI.
           EXIT.
      *-----------------------------------------------------------------
       510000-EXCLUIR-VINCULO          SECTION.
      *--> Acionar o SGTSE087 para excluir vínculo financeiro de um
      *    Recebimento Previsto
      *-----------------------------------------------------------------
           MOVE 0008                   TO  PSC-ERRO-PGM

           INITIALIZE                      SGTST087-RTN-PGM
                                           SGTST087-AREA-DADOS
                                           REPLACING NUMERIC
                                           BY  ZEROS

           MOVE  SPACES                TO  SGTST087-RTN-PGM
                                           SGTST087-AREA-DADOS

      *--> Excluir
           MOVE 3 TO SGTST087-ACAO

           MOVE  087-CD-CTRA-TIT-VL
                                       TO  SGTST087-CD-CTRA-TIT-VL
           MOVE  087-CD-OPR-ETRD-MCP
                                       TO  SGTST087-CD-OPR-ETRD-MCP
           MOVE  087-NR-VRS-OPR-ETRD
                                       TO  SGTST087-NR-VRS-OPR-ETRD

           EVALUATE IN-DVCL
               WHEN 1
                 MOVE  087-NR-SEQL-RCBT-PRVT
                                       TO  SGTST087-NR-SEQL-RCBT-PRVT
                 MOVE  087-CD-TIP-RCBT-PRVT
                                       TO  SGTST087-CD-TIP-RCBT-PRVT
                 MOVE  087-DT-RCBT-PRVT
                                       TO  SGTST087-DT-RCBT-PRVT
                 MOVE  087-CD-TIT-OPR-ETRD
                                       TO  SGTST087-CD-TIT-OPR-ETRD
                 MOVE  087-NR-DVS-TIT
                                       TO  SGTST087-NR-DVS-TIT
                 MOVE  087-TS-REG-PGTO
                                       TO  SGTST087-TS-REG-PGTO
               WHEN 2
                 MOVE  VCL-NR-SEQL-RCBT-PRVT
                                       TO  SGTST087-NR-SEQL-RCBT-PRVT
                 MOVE  VCL-CD-TIP-RCBT-PRVT
                                       TO  SGTST087-CD-TIP-RCBT-PRVT
                 MOVE  VCL-DT-RCBT-PRVT
                                       TO  SGTST087-DT-RCBT-PRVT
                 MOVE  VCL-CD-TIT-OPR-ETRD
                                       TO  SGTST087-CD-TIT-OPR-ETRD
                 MOVE  VCL-NR-DVS-TIT
                                       TO  SGTST087-NR-DVS-TIT
                 MOVE  VCL-TS-REG-PGTO
                                       TO  SGTST087-TS-REG-PGTO
           END-EVALUATE

      *
           IF EXECUCAO-ONLINE
              EXEC CICS LINK
                        PROGRAM ( SGTST087       )
                        COMMAREA( AREA-SGTST087  )
                        RESP    ( MSG1-RESP1     )
                        RESP2   ( MSG1-RESP2     )
              END-EXEC
              IF  MSG1-RESP1 NOT EQUAL  DFHRESP(NORMAL)
                  SET SGTS0513-ERRO-DE-CICS TO TRUE
                  MOVE  026            TO  CD-RTN-PGM
                  MOVE  'Erro ao acionar o módulo SGTST087.'
                                       TO  MSG-ERRO
                  PERFORM 999000-GRAVA-ERRO-E-ENCERRA
              END-IF
           ELSE
              MOVE LENGTH OF               AREA-SGTST087 TO EIBCALEN
              CALL SGTST087 USING DFHEIBLK AREA-SGTST087
           END-IF

           IF SGTST087-CD-RTN-PGM NOT = 0
               MOVE SGTST087-RTN-PGM   TO RTN-PGM
               MOVE SGTST087-MSG-ERRO-PGM TO SGTS0513-MSG-ERRO-PGM
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .

       510000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       600000-INCLUIR-HST-RLZD         SECTION.
      *-----------------------------------------------------------------
           MOVE 0009                   TO  PSC-ERRO-PGM
           PERFORM 110000-BUSCA-TS-DT
      *
           MOVE HLPKE001-TS-FMTD            TO 129-TS-INCL-HST
           MOVE 1                           TO 129-CD-TIP-ATL
           MOVE 'N'                         TO 129-IN-VCL-MNL
           MOVE SGTS0513-CD-USU-IMP         TO 129-CD-USU-VCL
           MOVE HLPKE001-TS-FMTD            TO 129-TS-ULT-ATL
           MOVE 087-CD-CTRA-TIT-VL          TO 129-CD-CTRA-TIT-VL
           MOVE 087-CD-OPR-ETRD-MCP         TO 129-CD-OPR-ETRD-MCP
           MOVE 087-NR-VRS-OPR-ETRD         TO 129-NR-VRS-OPR-ETRD

           EVALUATE IN-DVCL
               WHEN 1
                 MOVE 087-NR-SEQL-RCBT-PRVT TO 129-NR-SEQL-RCBT-PRVT
                 MOVE 087-CD-TIP-RCBT-PRVT  TO 129-CD-TIP-RCBT-PRVT
                 MOVE 087-DT-RCBT-PRVT      TO 129-DT-RCBT-PRVT
                 MOVE 087-CD-TIT-OPR-ETRD   TO 129-CD-TIT-OPR-ETRD
                 MOVE 087-NR-DVS-TIT        TO 129-NR-DVS-TIT
                 MOVE 087-TS-REG-PGTO       TO 129-TS-REG-PGTO
                 MOVE 087-VL-RLZD-VCLD      TO 129-VL-RLZD-VCLD
               WHEN 2
                 MOVE VCL-NR-SEQL-RCBT-PRVT TO 129-NR-SEQL-RCBT-PRVT
                 MOVE VCL-CD-TIP-RCBT-PRVT  TO 129-CD-TIP-RCBT-PRVT
                 MOVE VCL-DT-RCBT-PRVT      TO 129-DT-RCBT-PRVT
                 MOVE VCL-TS-REG-PGTO       TO 129-TS-REG-PGTO
                 MOVE VCL-VL-RLZD-VCLD      TO 129-VL-RLZD-VCLD
                 MOVE VCL-CD-TIT-OPR-ETRD   TO 129-CD-TIT-OPR-ETRD
                 MOVE VCL-NR-DVS-TIT        TO 129-NR-DVS-TIT
           END-EVALUATE

           EXEC SQL
                INSERT INTO DB2SGT.HST_RCBT_RLZD_OPR
                ( CD_CTRA_TIT_VL
                , CD_OPR_ETRD_MCP
                , NR_VRS_OPR_ETRD
                , NR_SEQL_RCBT_PRVT
                , CD_TIP_RCBT_PRVT
                , DT_RCBT_PRVT
                , CD_TIT_OPR_ETRD
                , NR_DVS_TIT
                , TS_REG_PGTO
                , TS_INCL_HST
                , CD_TIP_ATL
                , IN_VCL_MNL
                , CD_USU_VCL
                , TS_ULT_ATL
                , VL_RLZD_VCLD
                )
                VALUES(
                    :129-CD-CTRA-TIT-VL
                  , :129-CD-OPR-ETRD-MCP
                  , :129-NR-VRS-OPR-ETRD
                  , :129-NR-SEQL-RCBT-PRVT
                  , :129-CD-TIP-RCBT-PRVT
                  , :129-DT-RCBT-PRVT
                  , :129-CD-TIT-OPR-ETRD
                  , :129-NR-DVS-TIT
                  , :129-TS-REG-PGTO
                  , :129-TS-INCL-HST
                  , :129-CD-TIP-ATL
                  , :129-IN-VCL-MNL
                  , :129-CD-USU-VCL
                  , :129-TS-ULT-ATL
                  , :129-VL-RLZD-VCLD
                  )
           END-EXEC

      * Ligar erro automatico do DB2
      *
           EXEC SQL
               WHENEVER SQLERROR   GO TO 999002-ERRO-DB2
           END-EXEC

           EVALUATE SQLCODE
               WHEN 0
                 CONTINUE
               WHEN -803
                 SET SGTS0513-ERRO-DE-DB2 TO TRUE
                 MOVE  SQLCODE            TO  CD-RTN-PGM
                 STRING  'Registro já existente na tabela '
                         'HST_RCBT_RLZD_OPR.'
                 DELIMITED BY SIZE INTO MSG-ERRO
                 PERFORM 999000-GRAVA-ERRO-E-ENCERRA
               WHEN OTHER
                 SET SGTS0513-ERRO-DE-DB2 TO TRUE
                 MOVE  SQLCODE            TO  CD-RTN-PGM
                 STRING  'Erro ao incluir registro na tabela '
                         'HST_RCBT_RLZD_OPR.'
                 DELIMITED BY SIZE INTO MSG-ERRO
                 PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE
           .
       600000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA                     SECTION.
      *-----------------------------------------------------------------
           MOVE CD-RTN-PGM    TO SGTS0513-CD-RTN-PGM

           IF SGTS0513-MSG-ERRO EQUAL SPACES
              MOVE MSG-ERRO      TO SGTS0513-MSG-ERRO
           END-IF

           PERFORM 999001-ERRO-001
           .
       999000-SAI.
           EXIT.
      *--------------------
      * 999000-ERRO SECTION
      *--------------------
      *-----------------
      * 999001-ERRO-001.
      *-----------------
-INC HLPKE004
-INC HLPKE012

           GOBACK
           .
      *-----------------
      * 999002-ERRO-DB2
      *-----------------
-INC HLPKE005
      *-----------------
      * 999003-TRATA-ABEND-CICS
      * 999004-TRATA-CONDITION-CICS
      *-----------------
-INC HLPKE006
      *----------------------------------------
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *----------------------------------------
      *-INC SGTKP999
      * ---------------------------------------
      * MSG1-NTF-MSG                   SECTION.
      * ---------------------------------------
-INC MSGKPMG1
      * ---------------------------------------
       999999-ENCERRAR                 SECTION.
      * ---------------------------------------

           GOBACK.
      *------------------------- Fim SGTS0513 -------------------------*