       CBL ARITH(EXTEND)
      *-----------------------------------------------------------------
      * PROGRAMA..: SGTS001B
      * ANALISTA..: F9458161 - Vanessa Tsukamoto
      * AUTOR.....: F4957150 - João P.S. Tomaz
      * DATA......: 11/10/2024
      * COMPILACAO: 54 - PSOSE600 - Cobol 6.3 c/otimizacao p/producao
      * OBJETIVO..: Carteira 200 mil - Atualização OPR
      *-----------------------------------------------------------------
      * VRS006 12/02/2025 F4961858 - Envio da data de Posição ao módulo
      *                              CACS00IC
      * VRS005 17/12/2024 F9458161 - Ajusta W9603-DT-EST-VNCT-SCTR
      * VRS004 16/12/2024 F9458161 - Ajusta CD-EST-VNCT-SCTR
      * VRS003 14/12/2024 F9458161 - Ajusta atualizacao no OPR
      * VRS002 12/12/2024 F9458161 - Ajusta preenchimento do valor do
      *                   subcontrato
      * VRS001 15/10/2024 F4957150 - IMPLANTAÇÃO
      * VRS01B 08/11/2024 F9812118 - Inclusão de MOCKS, ajuste para in-
      *                   formar a data correta ao OPR, e ajuste para
      *                   passar o valor para a variavel correta no
      *                   slq3 (de AUX-VL-SDO-CPTL para AUX-VL-SDO-JUR).
      *                   Recompilação, ajuste select
      *
      *-----------------------------------------------------------------
      *
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTS001B.
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
       01  CTE-PROG                PIC  X(17) VALUE '*** SGTS001B ***'.
       01  CTE-VERS                    PIC  X(06) VALUE 'VRS005'.

      *
       77 AUX-ZERO                     PIC S9(04) COMP-5 VALUE 0.
       77 AUX-UM                       PIC S9(04) COMP-3 VALUE 1.
       77 AUX-CINCO                    PIC S9(04) COMP-3 VALUE 5.
       77 AUX-QUINZE                   PIC S9(04) COMP-3 VALUE 15.

       77 AUX-ZERO-SQL                 PIC S9(04) COMP-5 VALUE 0.
       77 AUX-UM-SQL                   PIC S9(04) COMP-5 VALUE 1.
       77 AUX-CINCO-SQL                PIC S9(04) COMP-5 VALUE 5.
       77 AUX-QUINZE-SQL               PIC S9(04) COMP-5 VALUE 15.

      * Subrotinas Utilizadas
       01  TAB-PROGRAMAS-CHAMADOS.
           03 SGTSLJ11 VALUE 'SGTSLJ11'          PIC X(8).
           03 CACS00IC VALUE 'CACS00IC'          PIC X(8).
           03 OPRS5900 VALUE 'OPRS5900'          PIC X(8).
           03 OPRS9601 VALUE 'OPRS9601'          PIC X(8).
           03 OPRS9602 VALUE 'OPRS9602'          PIC X(8).
           03 OPRS9603 VALUE 'OPRS9603'          PIC X(8).
           03 OPRS9604 VALUE 'OPRS9604'          PIC X(8).
           03 OPRS9624 VALUE 'OPRS9624'          PIC X(8).
           03 MSTS8002 VALUE 'MSTS8002'          PIC X(8).
      *

      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informacoes
-INC HLPKE01B
      *-----------------------------------------------------------------
       LOCAL-STORAGE SECTION.
      *-----------------------------------------------------------------
      * Area de trabalho

       01  DT-SQL-ATU                  PIC  X(10)
                                            VALUE '  .  .    '.
       01  DT-SQL-ATU-R REDEFINES DT-SQL-ATU.
           03 DIA                      PIC  9(02).
           03 FILLER                   PIC  X(01).
           03 MES                      PIC  9(02).
           03 FILLER                   PIC  X(01).
           03 ANO                      PIC  9(04).

       01 GDA-CD-EST-VNCT-SCTR         PIC S9(04)    COMP-3 VALUE 0.    Rose
       01 CD-OPR-ETRD-MCP              PIC S9(11)    COMP-3 VALUE 0.    Rose
       01 NR-VRS-OPR-ETRD              PIC S9(04)    COMP-5 VALUE 0.    Rose
       01 CD-UOR-DTTR-OPR              PIC S9(09)    COMP-5 VALUE 0.    Rose
       01 CD-UOR-BB                    PIC S9(09)    COMP-5 VALUE 0.    Rose
       01 CD-PRD                       PIC S9(09)    COMP-5 VALUE 0.    Rose
       01 CD-MDLD                      PIC S9(09)    COMP-5 VALUE 0.    Rose
       01 CD-TIT-OPR-ETRD              PIC  X(12)           VALUE ' '.
       01 GDA-CD-ITMT                  PIC  X(50)           VALUE ' '.
001440 01 GDA-CLI-EMISSOR              PIC S9(09)    COMP-5 VALUE 0.
001450 01 GDA-ULT-CLI-EMISSOR          PIC S9(09)    COMP-5 VALUE 0.
001460 01 GDA-CLI-IMPACTO-LC           PIC S9(09)    COMP-5 VALUE 0.
       01 CD-ERR-OPR                   PIC  9(04)           VALUE 0.    Rose
       01 LST-ERR-OPR                  PIC  X(180).                     Rose
000900 01 IC-ERRO                      PIC S9(04)    COMP-5 VALUE 0.    Rose
       01 VL-CPR-OPR                   PIC S9(15)V99 COMP-3 VALUE 0.
       01 AUX-VL-SDO-TIT-DT-PSC        PIC S9(15)V99 COMP-3 VALUE 0.
       01 AUX-VL-EM-ATRASO             PIC S9(15)V99 COMP-3 VALUE 0.
       01 AUX-VL-SDO-CPTL              PIC S9(15)V99 COMP-3 VALUE 0.
       01 AUX-VL-SDO-JUR               PIC S9(15)V99 COMP-3 VALUE 0.

      *-- indicador de nulidade
       01  NR-UNCO-CTR-OPR-N           PIC S9(04) COMP-5 VALUE 0.
       01  DT-CPR-OPR-N                PIC S9(04) COMP-5 VALUE 0.

       01  GDA-DT-A10                  PIC  X(10) VALUE ' '.
       01  GDA-DT-A10-R REDEFINES GDA-DT-A10.
           03 DIA                      PIC  9(02).
           03 FILLER                   PIC  X(01).
           03 MES                      PIC  9(02).
           03 FILLER                   PIC  X(01).
           03 ANO                      PIC  9(04).

       01  DT-OPR                      PIC  9(08).
       01  DT-OPR-R      REDEFINES DT-OPR.
           03 ANO                      PIC  9(4).
           03 MES                      PIC  9(2).
           03 DIA                      PIC  9(2).

       01  DT-SQL-DMA                  PIC  9(08).
       01  DT-SQL-DMA-R      REDEFINES DT-SQL-DMA.
           03 DIA                      PIC  9(2).
           03 MES                      PIC  9(2).
           03 ANO                      PIC  9(4).

       01  DT-SQL-AMD                  PIC  9(08).
       01  DT-SQL-AMD-R      REDEFINES DT-SQL-AMD.
           03 ANO                      PIC  9(4).
           03 MES                      PIC  9(2).
           03 DIA                      PIC  9(2).

       01  DT-FRMZ                     PIC  9(08).
       01  DT-FRMZ-R     REDEFINES DT-FRMZ.
           03 ANO                      PIC  9(4).
           03 MES                      PIC  9(2).
           03 DIA                      PIC  9(2).

       01 AUX-DT-PRMO-CRNG             PIC  9(08)  VALUE 0.             VRS01B
       01 AUX-DT-PRMO-CRNG-R REDEFINES AUX-DT-PRMO-CRNG.
          03 ANO                       PIC  9(04).
          03 MES                       PIC  9(02).
          03 DIA                       PIC  9(02).

001940 01  DT-PSC-INVERTIDA            PIC  9(08).
001950 01  DT-PSC-INVERTIDA-R  REDEFINES DT-PSC-INVERTIDA.
001960     07 ANO                       PIC  9(4).
001970     07 MES                       PIC  9(2).
001980     07 DIA                       PIC  9(2).
001990

002060 01  DT-CPR-OPR                  PIC  X(10).
002070 01  DT-CPR-OPR-R      REDEFINES DT-CPR-OPR.
002100     03 DIA                      PIC  9(2).
002100     03 FILLER                   PIC  X(01).
002090     03 MES                      PIC  9(2).
002100     03 FILLER                   PIC  X(01).
002080     03 ANO                      PIC  9(4).

002060 01  DT-CPR-OPR-AMD              PIC  9(8).
002070 01  DT-CPR-OPR-AMD-R      REDEFINES DT-CPR-OPR-AMD.
002080     03 ANO                      PIC  9(4).
002090     03 MES                      PIC  9(2).
002100     03 DIA                      PIC  9(2).

001010*    Chamada à subrotina SBDATA
001020*
001030 01  SBDATA                      PIC  X(08) VALUE 'SBDATA'.
001040 01  FUNCAO                      PIC  X(03) VALUE 'F03'.
001050 01  ARG01                       PIC  9(08).
001060 01  ARG01-R  REDEFINES  ARG01.
001070   03  DIA                       PIC  9(02).
001080   03  MES                       PIC  9(02).
001090   03  ANO                       PIC  9(04).
001100 01  ARG02                       PIC  9(08).
001110 01  ARG02-R  REDEFINES  ARG02.
001120   03  DIA                       PIC  9(02).
001130   03  MES                       PIC  9(02).
001140   03  ANO                       PIC  9(04).
001150 01  ARG03                       PIC S9(05) VALUE ZEROS.
001151 01  ARG03-ABS                   PIC  9(05) VALUE ZEROS.
001160*
      * Books da definicao de dados a serem usados na subrotina de erros
-INC HLPKE001

002670*-----> BOOK DA SUBROTINA SGTSLJ11
002680 01  SGTSLJ11-DADOS.
-INC SGTKLJ11                                                           0000269%
002700
      *-----> CACS00IC  -  Consulta Indicador Ativo Problematico
       01  CACS00IC-DADOS.
-INC CACK00IC

      * BOOKS OPR
       01 DADOS-OPRS9601.
-INC OPRK9601
       01 DADOS-OPRS9602.
-INC OPRK9602
       01 DADOS-OPRS9603.
-INC OPRK9603
       01 DADOS-OPRS9604.
-INC OPRK9604
       01 DADOS-OPRS9624.
-INC OPRK9624
       01 DADOS-OPRS5900.
-INC OPRK5900

002750*-----> Buscar prefixo da UOR
002760 01  MSTS8002-DADOS.
-INC MSTK8002
002780
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

      *-----------------------------------------------------------------
       LINKAGE SECTION.
      *-----------------------------------------------------------------
       01  DFHCOMMAREA.
-INC SGTK001B
      *-----------------------------------------------------------------
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *-----------------------------------------------------------------
111024D    DISPLAY CTE-PROG ' Procedure Division '

           IF  EIBCALEN NOT EQUAL LENGTH OF DFHCOMMAREA
               MOVE 'Area com tamanho invalido - verifique book/local'
                                       TO MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           MOVE 0001                   TO PSC-ERRO-PGM
      * WHENEVER DB2
-INC HLPKE002
      * WHENEVER CICS
-INC HLPKE003

           PERFORM 970400-BUSCA-TS-DT
           PERFORM 100000-VALIDA-ENTRADA
           PERFORM 300000-ACIONA-OPR
           PERFORM 970300-CHAMA-OPRS5900

      *--  Se operacao ativa no SGT e nao ativa no OPR, atualiza OPR
           IF  SGTS001B-EST-OPR            EQUAL 1
           AND W5900-CD-EST-CDU-S03(1) NOT EQUAL 1
               PERFORM 400000-ALTERA-CONTRATO
               PERFORM 401000-ALTERA-PARTICIPACAO
               PERFORM 402000-ALTERA-EMPREENDIMENTO
           END-IF

           IF VENCIDO
              PERFORM 400000-ALTERA-CONTRATO
              PERFORM 401000-ALTERA-PARTICIPACAO
              PERFORM 402000-ALTERA-EMPREENDIMENTO
           ELSE
              PERFORM 200000-CALCULA-SALDO
           END-IF

           PERFORM 500000-ALTERA-SUBCONTRATO
           PERFORM 501000-ALTERA-VALOR-SUB-CTR
      *
           GOBACK
           .
      *
      *-----------------------------------------------------------------
       100000-VALIDA-ENTRADA           SECTION.
      *-----------------------------------------------------------------
111024D    DISPLAY CTE-PROG ' 100000-VALIDA-ENTRADA '

           MOVE 0002                   TO PSC-ERRO-PGM
           INITIALIZE SGTS001B-RTN-PGM

           IF  SGTS001B-CD-CTRA-TIT-VL EQUAL ZEROS
               SET   ERRO-CODIFICADO   TO TRUE
               MOVE 'Codigo da carteira nao informado' TO MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           IF  SGTS001B-CD-OPR-ETRD-MCP EQUAL ZEROS
               SET   ERRO-CODIFICADO   TO TRUE
               MOVE 'Codigo da operacao nao informado' TO MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           IF  SGTS001B-NR-VRS-OPR-ETRD EQUAL ZEROS
               SET   ERRO-CODIFICADO   TO TRUE
               MOVE 'Numero da versao nao informado' TO MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           IF  SGTS001B-DT-PSC EQUAL SPACES
               SET   ERRO-CODIFICADO   TO TRUE
               MOVE 'Data de posicao nao informada' TO MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           IF  SGTS001B-QT-PAPL-TIT-DT-PSC < 0
               SET   ERRO-CODIFICADO   TO TRUE
               MOVE 'Quantidade de titulos invalida' TO MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           IF  SGTS001B-QT-PAPL-TIT-DT-PSC EQUAL ZEROS
               SET  VENCIDO            TO TRUE
           ELSE
               MOVE 1                  TO SGTS001B-EST-OPR
           END-IF

           MOVE SGTS001B-DT-PSC        TO GDA-DT-A10
           MOVE ANO OF GDA-DT-A10-R    TO ANO OF DT-OPR-R
           MOVE MES OF GDA-DT-A10-R    TO MES OF DT-OPR-R
           MOVE DIA OF GDA-DT-A10-R    TO DIA OF DT-OPR-R

           MOVE ZEROS                  TO AUX-VL-SDO-TIT-DT-PSC
                                          AUX-VL-EM-ATRASO
                                          AUX-VL-SDO-CPTL
                                          AUX-VL-SDO-JUR
                                          AUX-DT-PRMO-CRNG
           .
      *
       100000-SAI.
           EXIT.
      *
      *-----------------------------------------------------------------
       200000-CALCULA-SALDO            SECTION.
      *-----------------------------------------------------------------
111024D    DISPLAY CTE-PROG ' 200000-CALCULA-SALDO '

           MOVE 0003                   TO PSC-ERRO-PGM
      *--  Calcula o saldo da operacao na data de posicao
           MULTIPLY SGTS001B-QT-PAPL-TIT-DT-PSC
                    BY SGTS001B-VL-PU-TIT-DT-PSC
                    GIVING AUX-VL-SDO-TIT-DT-PSC
               ON SIZE ERROR
                    SET   ERRO-CODIFICADO                TO TRUE
                    MOVE 'ERRO EM OPERACAO ARITMETICA 1' TO MSG-ERRO
                    PERFORM 999001-ERRO-001
           END-MULTIPLY
      *
      *--  Calcula o saldo em atraso (valor das parcelas vencidas e
      *--  nao pagas incluindo juros)
           EXEC SQL
                SELECT COALESCE (SUM(A.VL_PRVT_EVT_OPR), 0)
                  INTO :AUX-VL-EM-ATRASO
                FROM       DB2SGT.CRNG_EVT_OPR A
                INNER JOIN DB2SGT.CRNG_EVT_TIT B
                ON  A.CD_ITMT_FNCO       = B.CD_ITMT_FNCO
                AND A.CD_GR_EVT_FDAR     = B.CD_GR_EVT_FDAR
                AND A.DT_EVT             = B.DT_EVT
                INNER JOIN DB2SGT.OPR_ETRD_MCP C
                ON  A.CD_CTRA_TIT_VL     = C.CD_CTRA_TIT_VL
                AND A.CD_OPR_ETRD_MCP    = C.CD_OPR_ETRD_MCP
                AND A.NR_VRS_OPR_ETRD    = C.NR_VRS_OPR_ETRD
                WHERE A.CD_CTRA_TIT_VL   = :SGTS001B-CD-CTRA-TIT-VL
                AND   A.CD_OPR_ETRD_MCP  = :SGTS001B-CD-OPR-ETRD-MCP
                AND   A.NR_VRS_OPR_ETRD  = :SGTS001B-NR-VRS-OPR-ETRD
                AND   B.NR_SEQL_PGTO_EVT = :AUX-ZERO
                AND   B.DT_EVT           < :SGTS001B-DT-PSC
                AND   B.DT_EVT           >= C.DT_CPR_OPR
                AND   B.CD_GR_EVT_FDAR   > :AUX-ZERO                    VRS01B
                AND   B.CD_ITMT_FNCO     = :GDA-CD-ITMT
           END-EXEC
      *
      *--  Calcula o saldo de capital (careca) que falta pagar
           EXEC SQL
                SELECT COALESCE (SUM(A.VL_PRVT_EVT_OPR), 0)
                  INTO :AUX-VL-SDO-CPTL
                FROM       DB2SGT.CRNG_EVT_OPR A
                INNER JOIN DB2SGT.CRNG_EVT_TIT B
                ON  A.CD_ITMT_FNCO       = B.CD_ITMT_FNCO
                AND A.CD_GR_EVT_FDAR     = B.CD_GR_EVT_FDAR
                AND A.DT_EVT             = B.DT_EVT
                INNER JOIN DB2SGT.OPR_ETRD_MCP C
                ON  A.CD_CTRA_TIT_VL     = C.CD_CTRA_TIT_VL
                AND A.CD_OPR_ETRD_MCP    = C.CD_OPR_ETRD_MCP
                AND A.NR_VRS_OPR_ETRD    = C.NR_VRS_OPR_ETRD
                WHERE A.CD_CTRA_TIT_VL   = :SGTS001B-CD-CTRA-TIT-VL
                AND   A.CD_OPR_ETRD_MCP  = :SGTS001B-CD-OPR-ETRD-MCP
                AND   A.NR_VRS_OPR_ETRD  = :SGTS001B-NR-VRS-OPR-ETRD
                AND   B.NR_SEQL_PGTO_EVT = :AUX-ZERO
                AND   B.DT_EVT          >= C.DT_CPR_OPR
                AND  (B.CD_GR_EVT_FDAR   = :AUX-UM-SQL  OR
                      B.CD_GR_EVT_FDAR   = :AUX-QUINZE-SQL)
                AND   B.CD_ITMT_FNCO     = :GDA-CD-ITMT
           END-EXEC
      *
      *--  Calcula o saldo de juros que falta pagar
           EXEC SQL
                SELECT COALESCE (SUM(A.VL_PRVT_EVT_OPR), 0)
                  INTO :AUX-VL-SDO-JUR
                FROM       DB2SGT.CRNG_EVT_OPR A
                INNER JOIN DB2SGT.CRNG_EVT_TIT B
                ON  A.CD_ITMT_FNCO       = B.CD_ITMT_FNCO
                AND A.CD_GR_EVT_FDAR     = B.CD_GR_EVT_FDAR
                AND A.DT_EVT             = B.DT_EVT
                INNER JOIN DB2SGT.OPR_ETRD_MCP C
                ON  A.CD_CTRA_TIT_VL     = C.CD_CTRA_TIT_VL
                AND A.CD_OPR_ETRD_MCP    = C.CD_OPR_ETRD_MCP
                AND A.NR_VRS_OPR_ETRD    = C.NR_VRS_OPR_ETRD
                WHERE A.CD_CTRA_TIT_VL   = :SGTS001B-CD-CTRA-TIT-VL
                AND   A.CD_OPR_ETRD_MCP  = :SGTS001B-CD-OPR-ETRD-MCP
                AND   A.NR_VRS_OPR_ETRD  = :SGTS001B-NR-VRS-OPR-ETRD
                AND   B.NR_SEQL_PGTO_EVT = :AUX-ZERO-SQL
                AND   B.DT_EVT           >= C.DT_CPR_OPR
                AND   B.CD_GR_EVT_FDAR   = :AUX-CINCO-SQL
                AND   B.CD_ITMT_FNCO     = :GDA-CD-ITMT
           END-EXEC
      *
      *--  Consulta a data do primeiro cronograma em aberto
           EXEC SQL
                SELECT A.DT_EVT
                  INTO :DT-SQL-ATU
                FROM       DB2SGT.CRNG_EVT_OPR A
                INNER JOIN DB2SGT.CRNG_EVT_TIT B
                ON  A.CD_ITMT_FNCO       = B.CD_ITMT_FNCO
                AND A.CD_GR_EVT_FDAR     = B.CD_GR_EVT_FDAR
                AND A.DT_EVT             = B.DT_EVT
                INNER JOIN DB2SGT.OPR_ETRD_MCP C
                ON  A.CD_CTRA_TIT_VL     = C.CD_CTRA_TIT_VL
                AND A.CD_OPR_ETRD_MCP    = C.CD_OPR_ETRD_MCP
                AND A.NR_VRS_OPR_ETRD    = C.NR_VRS_OPR_ETRD
                WHERE A.CD_CTRA_TIT_VL   = :SGTS001B-CD-CTRA-TIT-VL
                AND   A.CD_OPR_ETRD_MCP  = :SGTS001B-CD-OPR-ETRD-MCP
                AND   A.NR_VRS_OPR_ETRD  = :SGTS001B-NR-VRS-OPR-ETRD
                AND   B.CD_EST_EVT       = :AUX-UM-SQL
                AND   B.DT_EVT          >= C.DT_CPR_OPR
                AND   B.CD_ITMT_FNCO     = :GDA-CD-ITMT
                ORDER BY A.DT_EVT
                FETCH FIRST 1 ROW ONLY
           END-EXEC
      *
           IF  SQLCODE EQUAL ZEROS
               MOVE ANO OF DT-SQL-ATU-R TO ANO OF AUX-DT-PRMO-CRNG-R
               MOVE MES OF DT-SQL-ATU-R TO MES OF AUX-DT-PRMO-CRNG-R
               MOVE DIA OF DT-SQL-ATU-R TO DIA OF AUX-DT-PRMO-CRNG-R
           ELSE
               MOVE SPACES              TO DT-SQL-ATU
           END-IF
           .
      *
       200000-SAI.
           EXIT.
      *
      *-----------------------------------------------------------------
       300000-ACIONA-OPR               SECTION.
      *-----------------------------------------------------------------
111024D    DISPLAY CTE-PROG ' 300000-ACIONA-OPR '

           MOVE 0004                   TO PSC-ERRO-PGM

           MOVE ZEROS                  TO SGTS001B-NR-UNCO-CTR-OPR

           EXEC SQL
                SELECT NR_UNCO_CTR_OPR
                     , CD_OPR_ETRD_MCP
                     , NR_VRS_OPR_ETRD
                     , CD_UOR_DTTR_OPR
                     , CD_UOR_BB
                     , DT_CPR_OPR
                     , VL_CPR_OPR
                     , CD_PRD
                     , CD_MDLD
                     , CD_TIT_OPR_ETRD
                  INTO :SGTS001B-NR-UNCO-CTR-OPR
                       :NR-UNCO-CTR-OPR-N
                     , :CD-OPR-ETRD-MCP
                     , :NR-VRS-OPR-ETRD
                     , :CD-UOR-DTTR-OPR
                     , :CD-UOR-BB
                     , :DT-CPR-OPR
                       :DT-CPR-OPR-N
                     , :VL-CPR-OPR
                     , :CD-PRD
                     , :CD-MDLD
                     , :CD-TIT-OPR-ETRD
                  FROM DB2SGT.OPR_ETRD_MCP
                 WHERE CD_CTRA_TIT_VL   = :SGTS001B-CD-CTRA-TIT-VL
                   AND CD_OPR_ETRD_MCP  = :SGTS001B-CD-OPR-ETRD-MCP
                   AND NR_VRS_OPR_ETRD  = :SGTS001B-NR-VRS-OPR-ETRD
           END-EXEC

           IF  SQLCODE = +100
           OR  SGTS001B-NR-UNCO-CTR-OPR EQUAL ZEROS
               SET  ERRO-CODIFICADO    TO TRUE
               MOVE 'Numero Unico OPR inexistente'
                                       TO MSG-ERRO
               MOVE SQLCODE            TO CD-RTN-PGM
               PERFORM 999001-ERRO-001
           END-IF

           MOVE CD-TIT-OPR-ETRD        TO GDA-CD-ITMT
           .
      *
       300000-SAI.
           EXIT.
      *
      *-----------------------------------------------------------------
       400000-ALTERA-CONTRATO          SECTION.
      *-----------------------------------------------------------------
111024D    DISPLAY CTE-PROG ' 400000-ALTERA-CONTRATO '

           MOVE 0005                   TO PSC-ERRO-PGM
      *
           INITIALIZE DADOS-OPRS9601
               REPLACING NUMERIC BY ZEROS ALPHANUMERIC BY SPACES
           MOVE 1                      TO W9601-FUNCAO
           MOVE 'SGTS001B'             TO W9601-PRG-CHAMADA
           MOVE 'B'                    TO W9601-AMB-CHAMADA
           MOVE 'SGT'                  TO W9601-SG-SIS-RSP-I
           MOVE SGTS001B-NR-UNCO-CTR-OPR
                                       TO W9601-NR-UNCO-CTR-OPR-I
      *
           MOVE SGTS001B-EST-OPR       TO W9601-CD-EST-CDU
           MOVE DT-OPR                 TO W9601-DT-EST-CDU
      *
      *    Inclusão Roseli Início
           MOVE CD-OPR-ETRD-MCP        TO W9601-NR-CTR-OPR

014080     IF CD-UOR-DTTR-OPR          > ZEROS
014090* Busca dependência
014100        INITIALIZE   MSTS8002-DADOS
014110        MOVE  CD-UOR-DTTR-OPR    TO MSTS8002-CD-UOR-E
014120        PERFORM 970100-CHAMA-MSTS8002
014130        MOVE MSTS8002-CD-DEPE-UOR-S
014130                                 TO W9601-CD-PRF-DEPE-NEGR
014130                                    W9601-CD-PRF-DEPE-CDU
014140     END-IF
008441                                                                    VRS002
008450     IF  CD-UOR-BB               >   ZEROS
014090* Busca dependência
008470        INITIALIZE   MSTS8002-DADOS
008480        MOVE  CD-UOR-BB          TO MSTS8002-CD-UOR-E
008490        PERFORM 970100-CHAMA-MSTS8002
008500        MOVE MSTS8002-CD-DEPE-UOR-S
014170                                 TO  W9601-CD-PRF-DEPE-EXEC
008500*                                TO  W9601-CD-PRF-DEPE-CDU
008510     END-IF
014150
014220     MOVE CD-PRD                      TO W9601-CD-PRD
014230     MOVE CD-MDLD                     TO W9601-CD-MDLD
014250     MOVE CORR DT-CPR-OPR-R           TO DT-FRMZ-R
014260     MOVE DT-FRMZ                     TO W9601-DT-FRMZ
014300     MOVE 103                         TO W9601-CD-IEC-MOE

           MOVE 1                           TO  W9601-CD-IOR-OGM
           MOVE 1                           TO  W9601-CD-TIP-OBJ
           MOVE 'CNV'                       TO  W9601-SG-SIS-OGM-CVN
           MOVE CD-UOR-DTTR-OPR             TO  W9601-CD-UOR-DEPE-NEGR
                                                W9601-CD-UOR-DEPE-CDTR
           MOVE 00010101                    TO  W9601-DT-PRTB-BCRO
           MOVE 1                           TO  W9601-CD-PAIS-NEGO

      *    Inclusão Roseli Fim
      *
           MOVE LENGTH OF DADOS-OPRS9601
                                       TO EIBCALEN
           CALL OPRS9601 USING DFHEIBLK   DADOS-OPRS9601
      *
      * MOCK-POINT 400000.1 Teste de mock de variável                   VRS01B
      *

           IF W9601-COD-RETORNO NOT EQUAL ZEROS
019140        IF W9601-QTD-ERROS  >  ZEROS
019160           MOVE 'OPRS9601(1) '   TO LST-ERR-OPR
019160           PERFORM VARYING IC-ERRO FROM 1 BY 1
019160                   UNTIL IC-ERRO > W9601-QTD-ERROS
019170              MOVE W9601-COD-ERRO(IC-ERRO) TO CD-ERR-OPR
019180              STRING LST-ERR-OPR  '*' CD-ERR-OPR
019200                     DELIMITED BY SPACES
019210                     INTO  LST-ERR-OPR
019230           END-PERFORM
019230           MOVE LST-ERR-OPR      TO SGTS001B-LST-ERR-OPR
019240        END-IF
019240
019250*   001 Alteracao efetuada com pendencia (*)
019250*   002 Nao ha alteracao de dados
019250        IF W9601-COD-RETORNO   NOT EQUAL 1 AND 2
                 STRING PSC-ERRO-PGM '-' OPRS9601 W9601-MSG-RETORNO
                    DELIMITED BY SIZE INTO MSG-ERRO
                 END-STRING
                 PERFORM 999001-ERRO-001
019300        END-IF
           END-IF
           .
      *
       400000-SAI.
           EXIT.
      *
      *-----------------------------------------------------------------
       401000-ALTERA-PARTICIPACAO      SECTION.
      *-----------------------------------------------------------------
111024D    DISPLAY CTE-PROG ' 401000-ALTERA-PARTICIPACAO '

           MOVE 0006                   TO PSC-ERRO-PGM
      *
           INITIALIZE DADOS-OPRS9604
               REPLACING NUMERIC BY ZEROS ALPHANUMERIC BY SPACES

           MOVE AUX-UM                 TO W9604-FUNCAO
           MOVE 'SGTS001B'             TO W9604-PRG-CHAMADA
           MOVE 'B'                    TO W9604-AMB-CHAMADA
           MOVE 'SGT'                  TO W9604-SG-SIS-RSP-I
           MOVE SGTS001B-NR-UNCO-CTR-OPR
                                       TO W9604-NR-UNCO-CTR-OPR-I
      *    MCI                         TO W9604-CD-PSS-CTR-OPR-I
           MOVE AUX-UM                 TO W9604-NR-SEQL-PRTC-I
                                          W9604-CD-TIP-PRTC-I
           MOVE SGTS001B-EST-OPR       TO W9604-CD-EST-PRTC
           MOVE DT-OPR                 TO W9604-DT-FIM-PRTC

      *    Inclusões Roseli início
014720     PERFORM 401100-CONSULTA-PRTC-CLI
014720     MOVE GDA-CLI-EMISSOR             TO W9604-CD-PSS-CTR-OPR
           IF   GDA-CLI-EMISSOR    NOT EQUAL  GDA-CLI-IMPACTO-LC   AND
                GDA-CLI-IMPACTO-LC NOT EQUAL  ZEROS
                MOVE GDA-CLI-IMPACTO-LC     TO W9604-CD-PSS-CTR-OPR
           END-IF

014250     MOVE CORR DT-CPR-OPR-R      TO DT-CPR-OPR-AMD-R
           MOVE DT-CPR-OPR-AMD         TO W9604-DT-INC-PRTC
           MOVE 1                      TO W9604-CD-TIP-PRTC
                                          W9604-NR-SEQL-PRTC


      *    Inclusões Roseli fim
      *
           MOVE LENGTH OF DADOS-OPRS9604
                                       TO EIBCALEN
           CALL OPRS9604 USING DFHEIBLK  DADOS-OPRS9604
      *
      * MOCK-POINT 401000.1 Teste de mock de variável                   VRS01B
      *

           IF W9604-COD-RETORNO NOT EQUAL ZEROS
019140        IF W9604-QTD-ERROS  >  ZEROS
019160*          MOVE 'OPRS9624(1) '   TO LST-ERR-OPR
019180           STRING LST-ERR-OPR  '*'
019190                  'OPRS9604(1)'  DELIMITED BY SPACES
019210           INTO  LST-ERR-OPR
019160           PERFORM VARYING IC-ERRO FROM 1 BY 1
019160                   UNTIL IC-ERRO > W9604-QTD-ERROS
019170              MOVE W9604-COD-ERRO(IC-ERRO) TO CD-ERR-OPR
019180              STRING LST-ERR-OPR  '*'
019190                     CD-ERR-OPR  DELIMITED BY SPACES
019200                     ',*' DELIMITED BY SIZE
019210              INTO  LST-ERR-OPR
019230           END-PERFORM
019230           MOVE LST-ERR-OPR      TO SGTS001B-LST-ERR-OPR
019240        END-IF
019240
019250*   001 Alteracao efetuada com pendencia (*)
019250*   002 Nao ha alteracao de dados
019250        IF W9604-COD-RETORNO   NOT EQUAL 1 AND 2
                 STRING PSC-ERRO-PGM '-' OPRS9604 W9604-MSG-RETORNO
                     DELIMITED BY SIZE INTO MSG-ERRO
                 END-STRING
                 PERFORM 999001-ERRO-001
019300        END-IF
           END-IF
           .
      *
       401000-SAI.
           EXIT.
005900*----------------------------------------------------------------*
005910 401100-CONSULTA-PRTC-CLI        SECTION.
005920*----------------------------------------------------------------*
111024D    DISPLAY CTE-PROG ' 401100-CONSULTA-PRTC-CLI '
007771
           MOVE 0079                   TO PSC-ERRO-PGM
005941
005950     INITIALIZE         SGTSLJ11-DADOS
                              REPLACING NUMERIC BY  ZEROS
005960
005970     SET    SGTSLJ11-CONS-CLI-TP-PRTC TO  TRUE
           MOVE   200000                 TO  SGTSLJ11-CD-CTRA-TIT-VL
005980     MOVE   CD-OPR-ETRD-MCP        TO  SGTSLJ11-CD-OPR-ETRD-MCP
006000     MOVE   NR-VRS-OPR-ETRD        TO  SGTSLJ11-NR-VRS-OPR-ETRD
006020     MOVE   AUX-UM                 TO  SGTSLJ11-CD-TIP-FUC-CLI
006030     PERFORM   970220-CHAMA-SGTSLJ11
006040     MOVE   SGTSLJ11-CD-CLI        TO  GDA-CLI-EMISSOR
006050
006060     INITIALIZE         SGTSLJ11-DADOS
006070
006080     SET    SGTSLJ11-CONS-CLI-TP-PRTC TO  TRUE
           MOVE   200000                 TO  SGTSLJ11-CD-CTRA-TIT-VL
005980     MOVE   CD-OPR-ETRD-MCP        TO  SGTSLJ11-CD-OPR-ETRD-MCP
006000     MOVE   NR-VRS-OPR-ETRD        TO  SGTSLJ11-NR-VRS-OPR-ETRD
006130     MOVE   2                      TO  SGTSLJ11-CD-TIP-FUC-CLI
006140     PERFORM   970220-CHAMA-SGTSLJ11
006150     MOVE   SGTSLJ11-CD-CLI        TO  GDA-CLI-IMPACTO-LC
006160     .
006170 401100-SAI.
006180     EXIT.
006190
      *-----------------------------------------------------------------
       402000-ALTERA-EMPREENDIMENTO    SECTION.
      *-----------------------------------------------------------------
111024D    DISPLAY CTE-PROG ' 402000-ALTERA-EMPREENDIMENTO '

           MOVE 0007                   TO PSC-ERRO-PGM
      *
           INITIALIZE DADOS-OPRS9602
               REPLACING NUMERIC BY ZEROS ALPHANUMERIC BY SPACES
           MOVE 2                      TO W9602-FUNCAO
           MOVE 'SGTS001B'             TO W9602-PRG-CHAMADA
           MOVE 'B'                    TO W9602-AMB-CHAMADA
           MOVE 'SGT'                  TO W9602-SG-SIS-RSP
           MOVE SGTS001B-NR-UNCO-CTR-OPR
                                       TO W9602-NR-UNCO-CTR-OPR
           MOVE AUX-UM                 TO W9602-NR-EPRD-FNCD
           MOVE SGTS001B-EST-OPR       TO W9602-CD-EST-CDU-EPRD
           MOVE DT-OPR                 TO W9602-DT-EST-CDU-EPRD
015160     MOVE VL-CPR-OPR             TO W9602-VL-EPRD-FNCD


           MOVE LENGTH OF DADOS-OPRS9602
                                       TO EIBCALEN
           CALL OPRS9602 USING DFHEIBLK   DADOS-OPRS9602
      *
      * MOCK-POINT 402000.1 Teste de mock de variável                   VRS01B
      *

           IF W9602-COD-RETORNO NOT EQUAL ZEROS
019140        IF W9602-QTD-ERROS  >  ZEROS
019180           STRING LST-ERR-OPR  '*'
019190                  'OPRS9602(1)'  DELIMITED BY SPACES
019210           INTO  LST-ERR-OPR
019160           PERFORM VARYING IC-ERRO FROM 1 BY 1
019160                   UNTIL IC-ERRO > W9602-QTD-ERROS
019170              MOVE W9602-COD-ERRO(IC-ERRO) TO CD-ERR-OPR
019180              STRING LST-ERR-OPR  '*' CD-ERR-OPR
019200                     DELIMITED BY SPACES
019210                     INTO  LST-ERR-OPR
019230           END-PERFORM
019230           MOVE LST-ERR-OPR      TO SGTS001B-LST-ERR-OPR
019240        END-IF
019240
019250*   001 Alteracao efetuada com pendencia (*)
019250*   002 Nao ha alteracao de dados
019250        IF W9602-COD-RETORNO   NOT EQUAL 1 AND 2 AND 988
                 STRING PSC-ERRO-PGM '-' OPRS9602 W9602-MSG-RETORNO
                    DELIMITED BY SIZE INTO MSG-ERRO
                 END-STRING
                 PERFORM 999001-ERRO-001
019300        END-IF
           END-IF
           .
       402000-SAI.
           EXIT.
      *
      *-----------------------------------------------------------------
       500000-ALTERA-SUBCONTRATO       SECTION.
      *-----------------------------------------------------------------
111024D    DISPLAY CTE-PROG ' 500000-ALTERA-SUBCONTRATO '

           MOVE 0009                   TO PSC-ERRO-PGM
      *
           INITIALIZE DADOS-OPRS9603
               REPLACING NUMERIC BY ZEROS ALPHANUMERIC BY SPACES

           MOVE AUX-UM                 TO W9603-FUNCAO
           MOVE 'SGTS001B'             TO W9603-PRG-CHAMADA
           MOVE 'B'                    TO W9603-AMB-CHAMADA
           MOVE 'SGT'                  TO W9603-SG-SIS-RSP-CDU
           MOVE SGTS001B-NR-UNCO-CTR-OPR
                                       TO W9603-NR-UNCO-CTR-OPR
           MOVE AUX-UM                 TO W9603-NR-EPRD-FNCD
           MOVE AUX-UM                 TO W9603-NR-SCTR-OPR
           MOVE SGTS001B-EST-OPR       TO W9603-CD-EST-CDU-SCTR

           MOVE DT-OPR                 TO W9603-DT-EST-CDU-SCTR
014250     MOVE CORR DT-CPR-OPR-R      TO DT-CPR-OPR-AMD-R
           MOVE DT-CPR-OPR-AMD         TO W9603-DT-EST-CTB
           MOVE AUX-UM                 TO W9603-CD-RSCO-OPRL
           MOVE AUX-UM                 TO W9603-CD-EST-CTB
           MOVE VL-CPR-OPR             TO W9603-VL-SCTR-OPR

      *    Inclusão Roseli Início
           PERFORM 970200-CALCULAR-EST-VNCT-SCTR
015380
015430     MOVE 30                     TO W9603-CD-DTNC-CRD
015430     MOVE 3                      TO W9603-CD-FNLD-CRD
015440*
015760     MOVE CD-PRD                      TO W9603-CD-PRD
015770     MOVE CD-MDLD                     TO W9603-CD-MDLD
015910     MOVE 99                          TO W9603-CD-TIP-BLQ-INDP
           MOVE 1                           TO W9603-CD-TIP-OBJ
           MOVE 9                           TO W9603-CD-FMA-CM-NMLD
           MOVE 9                           TO W9603-CD-FMA-CM-INDP
           MOVE 99                          TO W9603-CD-TIP-MIC-CRD

013700     MOVE GDA-CD-EST-VNCT-SCTR        TO W9603-CD-EST-VNCT-SCTR

016450     MOVE CORR DT-SQL-ATU-R           TO DT-SQL-AMD-R
           IF  DT-SQL-AMD EQUAL ZEROS
               MOVE CORR HLPKE001-TS-FMTD   TO DT-SQL-AMD-R
           END-IF
016450     MOVE DT-SQL-AMD                  TO W9603-DT-EST-VNCT-SCTR

016450     MOVE ZEROES                      TO W9603-CD-PGM-GOV

016450*    Por causa da atualização com ressalvas
016450     MOVE 1                           TO W9603-CD-NTZ-RCS
016450     MOVE CD-MDLD                     TO W9603-CD-MDLD-CRD
016450*    Por causa da atualização com ressalvas  FIM
      *
      ***** OS CAMPOS ABAIXOS DEVERÃO SER INFORMADOS NA IMPLEMENTAÇÃO
      * DO SISTEMA CORPORATIVO
           MOVE ZEROES                      TO W9603-QT-MED-DD-PZ-SCTR
                                               W9603-QT-PCL-VCLD-OPR
                                               W9603-PC-CBT-GRT-FMAL
                                               W9603-PC-CBT-GRT-IFMA
      *
      *    Inclusão Roseli Fim

           MOVE LENGTH OF DADOS-OPRS9603
                                       TO EIBCALEN
           CALL OPRS9603 USING DFHEIBLK   DADOS-OPRS9603
      *
      * MOCK-POINT 500000.1 Teste de mock de variável                   VRS01B
      *

           IF W9603-COD-RETORNO NOT EQUAL ZEROS
019140        IF W9603-QTD-ERROS  >  ZEROS
019160           MOVE 'OPRS9603(1) '   TO LST-ERR-OPR
019160           PERFORM VARYING IC-ERRO FROM 1 BY 1
019160                   UNTIL IC-ERRO > W9603-QTD-ERROS
019170              MOVE W9603-COD-ERRO(IC-ERRO) TO CD-ERR-OPR
019180              STRING LST-ERR-OPR  '*' CD-ERR-OPR
019200                     DELIMITED BY SPACES
019210                     INTO  LST-ERR-OPR
019230           END-PERFORM
019230           MOVE LST-ERR-OPR      TO SGTS001B-LST-ERR-OPR
019240        END-IF
019240
019250*   001 Alteracao efetuada com pendencia (*)
019250*   002 Nao ha alteracao de dados
019250        IF W9603-COD-RETORNO   NOT EQUAL 1 AND 2
                 STRING PSC-ERRO-PGM '-' OPRS9603 W9603-MSG-RETORNO
                    DELIMITED BY SIZE INTO MSG-ERRO
                 END-STRING
                 PERFORM 999001-ERRO-001
019300        END-IF
           END-IF
           .
      *
       500000-SAI.
           EXIT.
      *
      *-----------------------------------------------------------------
       501000-ALTERA-VALOR-SUB-CTR     SECTION.
      *-----------------------------------------------------------------
      * VL-SDO-SCTR: Valor do Saldo de Capital do Subcontrato - Deve
      * conter o valor atualizado do Saldo de Capital (somente o
      * principal) do subcontrato em uma determinada data. Isto é: o
      * Saldo de Capital devedor tende a ser decrescente ao longo da
      * operação para as operações adimplentes. Para operações
      * inadimplentes, pode ocorrer a integralização das Rendas a
      * Apropriar, se a regra negocial assim o permitir.
      *
      * VL-ATU-SCTR: Valor Atual do Subcontrato - Deve conter o valor
      * atualizado de todas as verbas financiadas (todo o saldo devedor)
      * do subcontrato em uma determinada data. Isto é: A soma do
      * Saldo do Capital + Saldo devedor de Acessórios + Valor dos
      * Encargos a Capitalizar + Valor das Rendas a Apropriar.
      *
      *
111024D    DISPLAY CTE-PROG ' 501000-ALTERA-VALOR-SUB-CTR '

           MOVE 0010                   TO PSC-ERRO-PGM
      *
      *    MOVE SPACES                 TO W9624-BOOK
           INITIALIZE DADOS-OPRS9624
               REPLACING NUMERIC BY ZEROS ALPHANUMERIC BY SPACES
           MOVE AUX-UM                 TO W9624-FUNCAO
           MOVE 'SGTS001B'             TO W9624-PRG-CHAMADA
           MOVE 'B'                    TO W9624-AMB-CHAMADA
           MOVE 'SGT'                  TO W9624-SG-SIS-RSP-CDU
           MOVE SGTS001B-NR-UNCO-CTR-OPR
                                       TO W9624-NR-UNCO-CTR-OPR
           MOVE AUX-UM                 TO W9624-NR-EPRD-FNCD
                                          W9624-NR-SCTR-OPR
      *
           MOVE AUX-VL-SDO-TIT-DT-PSC  TO W9624-VL-SDO-SCTR
                                          W9624-VL-ATU-SCTR
      *
           MOVE AUX-VL-EM-ATRASO       TO W9624-VL-ATR-SCTR
           MOVE AUX-VL-SDO-CPTL        TO W9624-VL-SDO-CPTL
           MOVE AUX-VL-SDO-JUR         TO W9624-VL-SDO-JUR

           MOVE DT-OPR                 TO W9624-DT-SDO-SCTR
                                          W9624-DT-VL-ATU-SCTR
                                          W9624-DT-EST-VNCT-SCTR
      *
      *  Inclusão Roseli Início
           MOVE GDA-CD-EST-VNCT-SCTR   TO W9624-CD-EST-VNCT-SCTR
      *  Inclusão Roseli Fim
      *
      *    Aqui movemos o AUX-DT-PRMO-CRNG-R, pois ja contem a data     VRS01B
      *    no formato certo para ser usado na variavel                  VRS01B
      *    W9624-DT-PRMO-CRNG-ABTO                                      VRS01B
      *
           MOVE AUX-DT-PRMO-CRNG       TO W9624-DT-PRMO-CRNG-ABTO       VRS01B
      *    MOVE AUX-ZERO               TO W9624-VL-ENCG-CPTZ-SCTR
           MOVE ZEROS                  TO W9624-VL-ENCG-CPTZ-SCTR
                                          W9624-VL-REN-APPR-SCTR
                                          W9624-VL-TRND-PRJZ
                                          W9624-VL-CMB-MOEN
                                          W9624-VL-CMB-MOEE
      *
            MOVE ZEROS                 TO
                                          W9624-VL-SDO-MOEE
                                          W9624-VL-SDO-CRC
                                          W9624-VL-SDO-ACSR
                                          W9624-VL-JUR-CPTZ-SCTR
                                          W9624-VL-CRC-CPTZ-SCTR
      *
            MOVE ZEROS                 TO
                                          W9624-VL-JUR-APPR-SCTR
                                          W9624-VL-CRC-APPR-SCTR
                                          W9624-VL-ACSR-APPR-SCTR
                                          W9624-DT-TRNS-PRJZ
      *
            MOVE ZEROS                 TO
                                          W9624-VL-SDO-SCTR-USD
                                          W9624-VL-REN-APPR-USD
                                          W9624-CD-TIP-SDO
      *
           MOVE LENGTH OF DADOS-OPRS9624
                                       TO EIBCALEN
           CALL OPRS9624 USING DFHEIBLK   DADOS-OPRS9624
      *
      * MOCK-POINT 501000.1 Teste de mock de variável                   VRS01B
      *

           IF W9624-COD-RETORNO NOT EQUAL ZEROS
019140        IF W9624-QTD-ERROS  >  ZEROS
019160*          MOVE 'OPRS9624(1) '   TO LST-ERR-OPR
019180           STRING LST-ERR-OPR  '*'
019190                  'OPRS9624(1)'  DELIMITED BY SPACES
019210           INTO  LST-ERR-OPR
019160           PERFORM VARYING IC-ERRO FROM 1 BY 1
019160                   UNTIL IC-ERRO > W9624-QTD-ERROS
019170              MOVE W9624-COD-ERRO(IC-ERRO) TO CD-ERR-OPR
019180              STRING LST-ERR-OPR  '*'
019190                     CD-ERR-OPR  DELIMITED BY SPACES
019200                     ',*' DELIMITED BY SIZE
019210              INTO  LST-ERR-OPR
019230           END-PERFORM
019230           MOVE LST-ERR-OPR      TO SGTS001B-LST-ERR-OPR
019240        END-IF
019240
019250*   001 Alteracao efetuada com pendencia (*)
019250*   002 Nao ha alteracao de dados
019250        IF W9624-COD-RETORNO   NOT EQUAL 1 AND 2
                 STRING PSC-ERRO-PGM '-' OPRS9624 W9624-MSG-RETORNO
                     DELIMITED BY SIZE INTO MSG-ERRO
                 END-STRING
                 PERFORM 999001-ERRO-001
019300        END-IF
           END-IF
           .
      *
       501000-SAI.
           EXIT.
      *
020450*-----------------------------------------------------------------
020460 970100-CHAMA-MSTS8002           SECTION.
020470*-----------------------------------------------------------------
111024D    DISPLAY CTE-PROG ' 970100-CHAMA-MSTS8002 '
020490
020480     MOVE 0011                   TO PSC-ERRO-PGM
020491                                                                    VRS002
020500     MOVE 'S'                    TO MSTS8002-IN-DADOS-BASICOS-UOR
020510     MOVE 1                      TO MSTS8002-CD-FUNCAO
020520     MOVE LENGTH OF MSTS8002-DADOS    TO EIBCALEN
020530
020540     CALL MSTS8002 USING DFHEIBLK MSTS8002-DADOS
020550*
      * MOCK-POINT 970100.1 Teste de mock de variável
      *
020560     IF  MSTS8002-CD-RTN         NOT EQUAL ZEROS
020570         MOVE 'MST'              TO SG-SIS-MSG
020580         MOVE MSTS8002-CD-RTN    TO CD-RTN-PGM
020590         MOVE MSTS8002-PGM-ERRO  TO PGM-ERRO
020600         MOVE MSTS8002-TX-RTN    TO MSG-ERRO
020610         PERFORM 999001-ERRO-001
020620     END-IF
020630     .
020640 970100-SAI.
020650     EXIT.
020660
007710*----------------------------------------------------------------*
007720 970200-CALCULAR-EST-VNCT-SCTR   SECTION.
007730*----------------------------------------------------------------*
007740* ARG01 = data do primeiro cronograma (GDA-DT-PRMO-CRNG)
007750* ARG02 = data posição (DT-PSC)
007760* ARG03 = retorna a quantidade de dias da diferença entre as datas
007770*----------------------------------------------------------------*
      * Dominios OPR - CD-EST-VNCT-SCTR:
      *     20-A vencer há (+ 90 dias)
      *     21-A vencer (entre 61 e 90 dias)
      *     22-A vencer entre 30 e 60 dias
      *     23-A vencer em até 30 dias
      *     24-Vencida ha até 30 dias
      *     25-Vencida entre 31 e 60 dias
      *     26-Vencida (entre 61 e 90 dias)
      *     27-Vencida ha (+ 90 dias)
      *     28-Em CL vencida ate 60 dias
      *     29-Em CL vencida (+ 60 dias)
      *     30-Em prejuízo/perdas
      *     31-Indefinido
007770*----------------------------------------------------------------*
111024D    DISPLAY CTE-PROG ' 970200-CALCULAR-EST-VNCT-SCTR '
007771
           MOVE         0012              TO PSC-ERRO-PGM

           PERFORM 970210-ACESSA-CACS00IC

           IF (CACK00IC-IN-ATPB EQUAL '9'  OR
               CACK00IC-IN-ATPB EQUAL 'P'     )
      *       É Ativo Problemático
              MOVE 30 TO GDA-CD-EST-VNCT-SCTR

           ELSE
      *       Não é  Ativo Problemático então verifica se é operação
      *       Vincenda ou Vencida
007771       IF  AUX-DT-PRMO-CRNG NOT EQUAL ZEROS
007780           MOVE CORR AUX-DT-PRMO-CRNG-R  TO  ARG01-R
                 MOVE SGTS001B-DT-PSC        TO GDA-DT-A10
                 MOVE ANO OF GDA-DT-A10-R    TO ANO OF ARG02-R
                 MOVE MES OF GDA-DT-A10-R    TO MES OF ARG02-R
                 MOVE DIA OF GDA-DT-A10-R    TO DIA OF ARG02-R

007820           MOVE CORR GDA-DT-A10-R     TO DT-PSC-INVERTIDA-R
007840
007850           CALL 'SBDATA' USING FUNCAO ARG01 ARG02 ARG03
007860
007861           MOVE ARG03    TO   ARG03-ABS
007862
007870           IF  AUX-DT-PRMO-CRNG-R > DT-PSC-INVERTIDA
007880*    Vincenda
007890*
007891               IF  ARG03-ABS > 60
007910                   MOVE 21 TO GDA-CD-EST-VNCT-SCTR
007930               END-IF
007920
007931               IF  ARG03-ABS > 30 AND ARG03-ABS < 61
007950                   MOVE 22 TO GDA-CD-EST-VNCT-SCTR
007970               END-IF
007920
007971               IF  ARG03-ABS < 31
007990                   MOVE 23 TO GDA-CD-EST-VNCT-SCTR
008010               END-IF
008030           ELSE
008040*    Vencida
008050*
008051               IF  ARG03-ABS < 31
008070                   MOVE 24 TO GDA-CD-EST-VNCT-SCTR
008090               END-IF
007920
008091               IF  ARG03-ABS > 30 AND ARG03-ABS < 61
008110                   MOVE 25 TO GDA-CD-EST-VNCT-SCTR
008130               END-IF
007920
008131               IF  ARG03-ABS > 60
008150                   MOVE 26 TO GDA-CD-EST-VNCT-SCTR
008170               END-IF
008180           END-IF
             ELSE
                 MOVE 31               TO GDA-CD-EST-VNCT-SCTR
             END-IF
008180     END-IF
008200     .
008210 970200-SAI.
008220     EXIT.
008230
      *----------------------------------------------------------------*
       970210-ACESSA-CACS00IC          SECTION.
      * Verifica se é ativo problemático no CACS00IC
      *----------------------------------------------------------------*
111024D    DISPLAY CTE-PROG ' 970210-ACESSA-CACS00IC '
007771
           MOVE         0013              TO PSC-ERRO-PGM

           INITIALIZE         CACS00IC-DADOS
                              REPLACING NUMERIC BY  ZEROS
           MOVE SGTS001B-NR-UNCO-CTR-OPR   TO  CACK00IC-NR-UNCO-CTR-OPR
           MOVE SGTS001B-DT-PSC            TO  CACK00IC-DT-ANL-RSCO-ATPB
           MOVE      'B'                   TO  CACK00IC-CD-AMB
           MOVE   'SGTS001B'               TO  CACK00IC-NM-PGM

      *---> CHAMA  CACS00IC
020520     MOVE LENGTH OF CACS00IC-DADOS   TO EIBCALEN
020530
020540     CALL CACS00IC USING DFHEIBLK CACS00IC-DADOS
      *
020550* MOCK-POINT 970210.1 Teste de mock de variável
      *
020560     IF  CACK00IC-CD-ERRO            NOT EQUAL ZEROS
              IF CACK00IC-CD-SQLCODE       EQUAL +100
                 MOVE  '0'                 TO  CACK00IC-IN-ATPB
020570        ELSE
020570           MOVE 'CAC'                TO SG-SIS-MSG
020580           MOVE CACK00IC-CD-ERRO     TO CD-RTN-PGM
020590           MOVE CACK00IC-NM-PGM      TO PGM-ERRO
020600           MOVE CACK00IC-TX-SQLERRM  TO MSG-ERRO
020610           PERFORM 999001-ERRO-001
              END-IF
           END-IF
           .
005870 970210-SAI.
005880     EXIT.
020130*-----------------------------------------------------------------
020140 970220-CHAMA-SGTSLJ11           SECTION.
020150*-----------------------------------------------------------------
111024D    DISPLAY CTE-PROG ' 970220-CHAMA-SGTSLJ11 '
007771
           MOVE         0014              TO PSC-ERRO-PGM
020200
020520     MOVE LENGTH OF SGTSLJ11-DADOS   TO EIBCALEN
020530
020540     CALL SGTSLJ11 USING DFHEIBLK SGTSLJ11-DADOS
      *
020550* MOCK-POINT 970220.1 Teste de mock de variável
      *
020560     IF  SGTSLJ11-CD-RTN-PGM         NOT EQUAL ZEROS
020570         MOVE 'SGT'                TO SG-SIS-MSG
020580         MOVE SGTSLJ11-CD-RTN-PGM  TO CD-RTN-PGM
020590         MOVE SGTSLJ11             TO PGM-ERRO
020600         MOVE SGTSLJ11-MSG-ERRO    TO MSG-ERRO
020610         PERFORM 999001-ERRO-001
           END-IF
020250     .
020260 970220-SAI.
020270     EXIT.
      *-----------------------------------------------------------------
       970300-CHAMA-OPRS5900           SECTION.
      *-----------------------------------------------------------------
      *--  Consulta operacao no OPR
      D    DISPLAY CTE-PROG ' 970300-CHAMA-OPRS5900 '

           MOVE 0015                   TO PSC-ERRO-PGM

           MOVE SPACES                 TO W5900-DADOS-CHAMADA

           MOVE 3                      TO W5900-FUNCAO
           MOVE 'SGTS001B'             TO W5900-PRG-CHAMADA
           MOVE 'B'                    TO W5900-AMB-CHAMADA
           MOVE 0                      TO W5900-NR-OCORR-CHAMADA-E

           MOVE SGTS001B-NR-UNCO-CTR-OPR TO W5900-NR-UNCO-CTR-OPR-E

           MOVE 'S'                    TO W5900-CD-EST-CDU-IND
           MOVE 'S'                    TO W5900-CD-EST-PRTC-IND

           MOVE LENGTH OF DADOS-OPRS5900 TO EIBCALEN
           CALL OPRS5900 USING DFHEIBLK  DADOS-OPRS5900
      *
      * MOCK-POINT 970300.1 Teste de mock de variável
      *
           IF  W5900-COD-RETORNO NOT EQUAL ZEROS
               STRING PSC-ERRO-PGM '-' OPRS9602 W5900-MSG-RETORNO
                      DELIMITED BY SIZE INTO MSG-ERRO
               END-STRING
               PERFORM 999001-ERRO-001
           END-IF
           .
      *
       970300-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970400-BUSCA-TS-DT SECTION.
      *-----------------------------------------------------------------
      D    DISPLAY CTE-PROG ' 970400-BUSCA-TS-DT '
           MOVE 0016                   TO PSC-ERRO-PGM

           CALL SBTIMEDT            USING HLPKE001-CD-FUC
                                          HLPKE001-AREA-TRB
                                          HLPKE001-FMT-TS
                                          HLPKE001-TAM-TS
                                          HLPKE001-TS-FMTD
      *
      * MOCK-POINT 970400.1 Teste de mock de variável
           IF  RETURN-CODE      NOT EQUAL ZEROS
               SET  ERRO-DE-PGM        TO TRUE
               MOVE SBTIMEDT           TO PGM-ERRO
               MOVE RETURN-CODE        TO CD-RTN-PGM
               MOVE 'Erro ao obter data e hora atual.' TO MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF
           .
       970400-SAI.
           EXIT.
      *--------------------
      * 999000-ERRO SECTION
      *--------------------
      *-----------------
      * 999001-ERRO-001.
      *-----------------
-INC HLPKE004
-INC HLPKE012
      *
111024D    DISPLAY CTE-PROG ' 999001-ERRO-001. '

           EXEC SQL
                ROLLBACK
           END-EXEC
      *
           MOVE PSC-ERRO-PGM                 TO PSC-ERRO-PGM
           MOVE RTN-PGM                      TO SGTS001B-RTN-PGM
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
      *---------------------- FIM SGTS001B ----------------------------