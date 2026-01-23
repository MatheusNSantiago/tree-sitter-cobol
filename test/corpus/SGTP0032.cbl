      CBL ARITH(EXTEND)
      *---------------------------------------------------------------*
       IDENTIFICATION DIVISION.
      *---------------------------------------------------------------*
      *                                                               *
      *---------------------------------------------------------------*
       PROGRAM-ID.   SGTP0032.
      *---------------------------------------------------------------*
      * Formatar informacao de receita dos papeis do SGT para informe *
      * aos sistemas REN/ARI.                                         *
      *---------------------------------------------------------------*
      * VRS006 18/06/25 F6769534 - AJUSTE NA DATA DA ULTIMA POSICAO   *
      *                            DO MES ANTERIOR, PARA ATIVOS COM   *
      *                            DATA DE COMPRA NO MES ATUAL, NA    *
      *                            OBTENCAO DA MOVIMENTACAO DE JUROS, *
      *                            AMORTIZACOES E VENDAS DO MES       *
      * VRS005 17/06/25 F6769534 - AJUSTE NO SALDO DO MES ANTERIOR,   *
      *                            PARA ATIVOS COM DATA DE COMPRA NO  *
      *                            MES ATUAL                          *
      * VRS004 09/06/25 F6769534 - CORRECAO NA GRAVACAO DO SINAL DO   *
      *                            VALOR DA RECEITA                   *
      * VRS003 06/06/25 F6769534 - CORRECAO PARA O CASO DE NAO EXIS-  *
      *                            TIR NENHUMA POSICAO DE SALDO ANTE- *
      *                            RIOR AA DATA PESQUISADA            *
      * VRS002 02/06/25 F6769534 - INCLUIR ARQ ATIVOS PROBLEMATICOS   *
      *                          - TRATAR SQLCODE +100 NA BUSCA PELO  *
      *                            SALDO DO TITULO EM UMA DATA,       *
      *                            TROCANDO OPERADOR '=' POR '<='     *
      * VRS001 28/04/25 F6769534 - IMPLANTACAO                        *
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
       ENVIRONMENT DIVISION.
      *---------------------------------------------------------------*
      *
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
            DECIMAL-POINT IS COMMA.
      *
       INPUT-OUTPUT SECTION.

       FILE-CONTROL.
           SELECT SGTF032E ASSIGN TO UT-S-SGTF032E
                           FILE STATUS IS FS-032E.
           SELECT SGTF032S ASSIGN TO UT-S-SGTF032S
                           FILE STATUS IS FS-032S.
           SELECT SGTF032P ASSIGN TO UT-S-SGTF032P
                           FILE STATUS IS FS-032P.

      *
      *---------------------------------------------------------------*
       DATA DIVISION.
      *---------------------------------------------------------------*
      *
       FILE SECTION.
      *
       FD  SGTF032E
           RECORDING F
           RECORD 230
           BLOCK 0.
      *
       01  SGTF032E-FD                  PIC X(230).
      *
       FD  SGTF032S
           RECORDING F
           RECORD 230
           BLOCK 0.
      *
       01  SGTF032S-FD                  PIC X(230).
      *
       FD  SGTF032P
           RECORDING F
           RECORD 060
           BLOCK 0.
      *
       01  SGTF032P-FD                  PIC X(060).

      *---------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *---------------------------------------------------------------*
      *
       77  CTE-INICIO                   PIC X(29) VALUE
           'SGTP0032 - W.S.S. COMECA AQUI'.
       77  CTE-PROG                     PIC X(16) VALUE
           '*** SGTP0032 ***'.
       77  CTE-VERS                     PIC  X(06) VALUE 'VRS006'.

       01  TAB-PROGRAMAS-CHAMADOS.
         03  CACS00IC                   PIC  X(008)  VALUE 'CACS00IC'.
         03  SBABEND                    PIC  X(008)  VALUE 'SBABEND'.
         03  SBDATAF                    PIC  X(008)  VALUE 'SBDATAF'.
         03  SBVERSAO                   PIC  X(008)  VALUE 'SBVERSAO'.

      *    Book para mock
-INC ALMKCBL1
      *
      *---------------------------------------------------------------*
       LOCAL-STORAGE                 SECTION.
      *---------------------------------------------------------------*

      *    Area para subrotina SBDATAF
       01  FUNCAO                      PIC  X(03) VALUE 'F16'.

       01  AREA-WORK                   PIC  X(4095).
       01  AREA-WORK-RED  REDEFINES  AREA-WORK.
           03 A-TAM                      PIC  9(0008) COMP.
           03 FILLER                     PIC  X(4071).
           03 A-FIM                      PIC  X(0020).
      *
       01  ARG01                       PIC  9(08).
       01  ARG01-R  REDEFINES  ARG01.
         03  DIA                       PIC  9(02).
         03  MES                       PIC  9(02).
         03  ANO                       PIC  9(04).
      *
       01  ARG02                       PIC  9(08).
       01  ARG02-R  REDEFINES  ARG02.
         03  DIA                       PIC  9(02).
         03  MES                       PIC  9(02).
         03  ANO                       PIC  9(04).
      *
       01  ARG03                       PIC  9(08).
       01  ARG03-R  REDEFINES  ARG03.
         03  DIA                       PIC  9(02).
         03  MES                       PIC  9(02).
         03  ANO                       PIC  9(04).

      *    Variaveis para controle de fluxo e contadores
       01  GDA-FIM-032E                  PIC 9(01) VALUE ZEROS.
       01  GDA-CNT-032E                  PIC 9(09) VALUE ZEROS.
       01  GDA-GRV-032S                  PIC 9(09) VALUE ZEROS.
       01  GDA-GRV-032P                  PIC 9(09) VALUE ZEROS.

      *    Variaveis auxiliares de trabalho
       01  AX-VL-SDO-TIT-FIM-MM          PIC S9(15)V9(2) COMP-3.
       01  AX-VL-SDO-TIT-FIM-MM-ANT      PIC S9(15)V9(2) COMP-3.
       01  AX-VL-SDO-TIT-FIM-MM-ATU      PIC S9(15)V9(2) COMP-3.
       01  AX-VL-AMTZ-JUR-VND-TIT-MM-ATU PIC S9(15)V9(2) COMP-3.
       01  AX-VL-RC-TIT                  PIC S9(15)V9(2) COMP-3.
       01  AX-QT-PGTO-ATIV-PROBLEMATICO  PIC S9(4) USAGE COMP-5.
       01  AX-GRV-SAID                   PIC X.
       01  AX-MES-DB2                    PIC S9(4) USAGE COMP-5.
       01  AX-TMP-SEC-ANO                PIC 9(4).
       01  AX-ANO-DB2                    PIC S9(4) USAGE COMP-5.

      *    Variaveis "display" para exibicao nas mensagens de erro
       01  AX-DISPLAY-TAM-AREA           PIC  9(04) VALUE ZEROS.
       01  AX-DISPLAY-SQLCODE            PIC S9(04) VALUE ZEROS.
       01  AX-DISPLAY-NR-DVS-TIT         PIC S9(04) VALUE ZEROS.
       01  AX-DISPLAY-CD-CTRA-TIT-VL     PIC S9(09) VALUE ZEROS.
       01  AX-DISPLAY-NR-UNCO-CTR-OPR    PIC S9(17)V     VALUE ZEROS.
       01  AX-DISPLAY-VL-MNTR            PIC S9(15)V9(2) VALUE ZEROS.

      *    Variaveis para as datas de pesquisa
       01  DT-ULT-UTIL-MES-ANTERIOR-PSQ  PIC X(10) VALUE '  .  .    '.
       01  DT-MES-ANTERIOR-RED REDEFINES DT-ULT-UTIL-MES-ANTERIOR-PSQ.
           03  DIA-ULT-UTIL-MES-ANTERIOR-PSQ PIC 9(02).
           03  FILLER                        PIC X(01).
           03  MES-ULT-UTIL-MES-ANTERIOR-PSQ PIC 9(02).
           03  FILLER                        PIC X(01).
           03  SEC-ULT-UTIL-MES-ANTERIOR-PSQ PIC 9(02).
           03  ANO-ULT-UTIL-MES-ANTERIOR-PSQ PIC 9(02).

       01  DT-ULT-UTIL-MES-ATUAL-PSQ    PIC X(10) VALUE '  .  .    '.
       01  DT-MES-ATUAL-RED REDEFINES DT-ULT-UTIL-MES-ATUAL-PSQ.
           03  DIA-ULT-UTIL-MES-ATUAL-PSQ PIC 9(02).
           03  FILLER                     PIC X(01).
           03  MES-ULT-UTIL-MES-ATUAL-PSQ PIC 9(02).
           03  FILLER                     PIC X(01).
           03  SEC-ULT-UTIL-MES-ATUAL-PSQ PIC 9(02).
           03  ANO-ULT-UTIL-MES-ATUAL-PSQ PIC 9(02).

       01  DT-ULT-DD-MES-ATUAL-PSQ      PIC X(10) VALUE '  .  .    '.
       01  DT-ULT-DD-MES-ATUAL-RED REDEFINES DT-ULT-DD-MES-ATUAL-PSQ.
           03  DIA-ULT-DD-MES-ATUAL-PSQ PIC 9(02).
           03  FILLER                   PIC X(01).
           03  MES-ULT-DD-MES-ATUAL-PSQ PIC 9(02).
           03  FILLER                   PIC X(01).
           03  SEC-ULT-DD-MES-ATUAL-PSQ PIC 9(02).
           03  ANO-ULT-DD-MES-ATUAL-PSQ PIC 9(02).

       01  DT-PRMO-DD-MES-ATUAL-PSQ      PIC X(10) VALUE '  .  .    '.
       01  DT-PRMO-DD-MES-ATUAL-RED REDEFINES DT-PRMO-DD-MES-ATUAL-PSQ.
           03  DIA-PRMO-DD-MES-ATUAL-PSQ PIC 9(02).
           03  FILLER                    PIC X(01).
           03  MES-PRMO-DD-MES-ATUAL-PSQ PIC 9(02).
           03  FILLER                    PIC X(01).
           03  SEC-PRMO-DD-MES-ATUAL-PSQ PIC 9(02).
           03  ANO-PRMO-DD-MES-ATUAL-PSQ PIC 9(02).

       01  DT-BASE-MM-ANT                PIC X(10) VALUE '  .  .    '.

       01  DT-TRB-DDMMAAAA.
           03  DIA-TRB-DDMMAAAA         PIC 9(02).
           03  MES-TRB-DDMMAAAA         PIC 9(02).
           03  SEC-ANO-TRB-DDMMAAAA.
             05  SEC-TRB-DDMMAAAA       PIC 9(02).
             05  ANO-TRB-DDMMAAAA       PIC 9(02).

       01  AX-DT-TRB-DB2                PIC X(10)  VALUE '  .  .    '.
       01  AX-DT-TRB-DB2-RED REDEFINES AX-DT-TRB-DB2.
           03  DIA-TRB-DB2              PIC 9(02).
           03  FILLER                   PIC X.
           03  MES-TRB-DB2              PIC 9(02).
           03  FILLER                   PIC X.
           03  SEC-ANO-TRB-DB2.
             05  SEC-TRB-DB2            PIC 9(02).
             05  ANO-TRB-DB2            PIC 9(02).

      *    Controle de file status dos arquivos
       01  FS-032E                 PIC 9(02)  VALUE  ZEROS.
       01  FS-032S                 PIC 9(02)  VALUE  ZEROS.
       01  FS-032P                 PIC 9(02)  VALUE  ZEROS.

      * Book dos arquivos, de entrada (MSD) e de saida (Receita)
-INC RSTK502D

      * Book do arquivo de Ativos Problematicos
-INC SGTK032P

      * Books de DCLGEN das tabelas consultadas:
      * DB2SGT.PSC_DRIA_SDO_OPR
-INC SGTK004T

      * DB2SGT.OPR_ETRD_MCP
-INC SGTK005T

      * DB2SGT.DMTV_CX_TIT
-INC SGTK086D

      *--> HLPKDFHE - Definicao da area DFHEIBLK
-INC HLPKDFHE

      *    Area para chamada ao programa CACS00IC
      *    (Consulta Indicador Ativo Problematico)
       01  CACS00IC-DADOS.
-INC CACK00IC

      *------------------------------*
      * Area SQL - SQLCA.
      *------------------------------*
      *
           EXEC SQL INCLUDE SQLCA END-EXEC.

      *
      *---------------------------------------------------------------*
       LINKAGE SECTION.
      *---------------------------------------------------------------*
       01  LK-PARM.
           03  LK-SIZE                   PIC S9(04) COMP.
           03  LK-DATA-PRCT-AMD          PIC  9(06).
           03  LK-DATA-PRCT REDEFINES LK-DATA-PRCT-AMD.
               05  ANO-PRCT-LK           PIC  9(02).
               05  MES-PRCT-LK           PIC  9(02).
               05  DIA-PRCT-LK           PIC  9(02).
           03  LK-DATA-PSC-AMD           PIC  9(06).
           03  LK-DATA-PSC REDEFINES LK-DATA-PSC-AMD.
               05  ANO-PSC-LK           PIC  9(02).
               05  MES-PSC-LK           PIC  9(02).
               05  DIA-PSC-LK           PIC  9(02).

      *
      *---------------------------------------------------------------*
       PROCEDURE DIVISION USING LK-PARM.
      *---------------------------------------------------------------*

      * Book para mock
-INC ALMKCBL2

       000000-ROTINA-PRINCIPAL         SECTION.
      *---------------------------------------------------------------*
      *
           CALL SBVERSAO USING CTE-PROG CTE-VERS
      *
           PERFORM 100000-INICIO
           PERFORM 200000-PROCESSAMENTO UNTIL GDA-FIM-032E = 1
           PERFORM 301000-FINAL
      *
           STOP RUN
           .
      *
      *---------------------------------------------------------------*
       100000-INICIO                   SECTION.
      *---------------------------------------------------------------*
      *
           IF  LK-SIZE NOT EQUAL 0012
               PERFORM 999000-ERRO-01
               PERFORM 990000-ABENDA
           END-IF

           IF LK-DATA-PRCT-AMD NOT NUMERIC OR
              LK-DATA-PSC-AMD  NOT NUMERIC
               PERFORM 999000-ERRO-02
               PERFORM 990000-ABENDA
           END-IF

           OPEN  INPUT SGTF032E
                OUTPUT SGTF032S SGTF032P

           IF  FS-032E NOT EQUAL ZEROS OR
               FS-032S NOT EQUAL ZEROS OR
               FS-032P NOT EQUAL ZEROS
               PERFORM 999000-ERRO-03
               PERFORM 990000-ABENDA
           END-IF

      *
           MOVE ZEROS                  TO  GDA-FIM-032E
           MOVE ZEROS                  TO  GDA-CNT-032E
           MOVE ZEROS                  TO  GDA-GRV-032S
           MOVE ZEROS                  TO  GDA-GRV-032P
      *
      *    Atraves de SBDATAF, descobrir qual foi o ultimo dia util
      *    do mes base do calculo, ou seja, o mesmo mes que estah
      *    especificado no parametro LK-DATA-PRCT-AMD.
      *
           MOVE DIA-PRCT-LK            TO  DIA-TRB-DDMMAAAA
           MOVE MES-PRCT-LK            TO  MES-TRB-DDMMAAAA
      *    Se o ano for zero, presumir 2100
           IF ANO-PRCT-LK NOT EQUAL ZEROS
                MOVE '20'                   TO  SEC-TRB-DDMMAAAA
           ELSE
                MOVE '21'                   TO  SEC-TRB-DDMMAAAA
           END-IF
           MOVE ANO-PRCT-LK            TO  ANO-TRB-DDMMAAAA

           MOVE 4095                   TO A-TAM
           MOVE 'SBDATAF -WRKAREA-FIM' TO A-FIM

           MOVE DT-TRB-DDMMAAAA        TO ARG01
           MOVE ZEROS                  TO ARG02 ARG03

           CALL SBDATAF USING FUNCAO AREA-WORK ARG01 ARG02 ARG03

           IF RETURN-CODE NOT EQUAL ZEROS
               PERFORM 999000-ERRO-04
               PERFORM 990000-ABENDA
           ELSE
      *
      *    Ultimo dia util do mes base de calculo encontrado com su-
      *    cesso; agora eh soh atualizar a respectiva data de pesquisa.
      *    Tambem precisaremos do ultimo dia CORRIDO do mes base.
      *
      *         Ultimo dia util, para encontrar a posicao de saldo.
                MOVE            ARG02 TO DT-TRB-DDMMAAAA
                MOVE DIA-TRB-DDMMAAAA TO DIA-ULT-UTIL-MES-ATUAL-PSQ
                MOVE MES-TRB-DDMMAAAA TO MES-ULT-UTIL-MES-ATUAL-PSQ
                MOVE SEC-TRB-DDMMAAAA TO SEC-ULT-UTIL-MES-ATUAL-PSQ
                MOVE ANO-TRB-DDMMAAAA TO ANO-ULT-UTIL-MES-ATUAL-PSQ
      *         Ultimo dia corrido, para encontrar amortizacoes/juros e
      *         vendas do papel.
                MOVE            ARG03 TO DT-TRB-DDMMAAAA
                MOVE DIA-TRB-DDMMAAAA TO DIA-ULT-DD-MES-ATUAL-PSQ
                MOVE MES-TRB-DDMMAAAA TO MES-ULT-DD-MES-ATUAL-PSQ
                MOVE SEC-TRB-DDMMAAAA TO SEC-ULT-DD-MES-ATUAL-PSQ
                MOVE ANO-TRB-DDMMAAAA TO ANO-ULT-DD-MES-ATUAL-PSQ
      *         Primeiro dia corrido, para encontrar amortizacoes/juros
      *         e vendas do papel.
                MOVE 1                TO DIA-TRB-DDMMAAAA
                MOVE DIA-TRB-DDMMAAAA TO DIA-PRMO-DD-MES-ATUAL-PSQ
                MOVE MES-TRB-DDMMAAAA TO MES-PRMO-DD-MES-ATUAL-PSQ
                MOVE SEC-TRB-DDMMAAAA TO SEC-PRMO-DD-MES-ATUAL-PSQ
                MOVE ANO-TRB-DDMMAAAA TO ANO-PRMO-DD-MES-ATUAL-PSQ
           END-IF
      *
      *    Agora precisamos encontrar o ultimo dia util do mes ANTERIOR
      *    ao da base de calculo. Atraves de SBDATAF, descobrir qual
      *    foi o ultimo dia util do mes anterior ao que estah
      *    especificado no parametro LK-DATA-PRCT-AMD.
      *    Antes de mais nada, montar data de pesquisa no mes anterior.
      *
           MOVE 1 TO DIA-TRB-DDMMAAAA
           SUBTRACT 1 FROM MES-TRB-DDMMAAAA
           IF MES-TRB-DDMMAAAA EQUAL ZEROS
      *    Ajuste para o ano passado, se o mes base atual eh janeiro.
               MOVE     12   TO MES-TRB-DDMMAAAA
               IF SEC-ANO-TRB-DDMMAAAA NOT EQUAL '2100'
      *        A nao ser que o mes atual seja janeiro de 2100, o unico
      *        ajuste eh diminuir em 1 o ano; se for janeiro de 2100,
      *        temos que acertar tambem o seculo.
                   SUBTRACT  1 FROM ANO-TRB-DDMMAAAA
               ELSE
                   MOVE 20 TO SEC-TRB-DDMMAAAA
                   MOVE 99 TO ANO-TRB-DDMMAAAA
               END-IF
           END-IF
      *
      *
           MOVE 4095                   TO A-TAM
           MOVE 'SBDATAF -WRKAREA-FIM' TO A-FIM

           MOVE DT-TRB-DDMMAAAA        TO ARG01
           MOVE ZEROS                  TO ARG02 ARG03

           CALL SBDATAF USING FUNCAO AREA-WORK ARG01 ARG02 ARG03

      * MOCK-POINT MCKV1

           IF RETURN-CODE NOT EQUAL ZEROS
               PERFORM 999000-ERRO-05
               PERFORM 990000-ABENDA
           ELSE
      *
      *    Ultimo dia util do mes anterior ao da base de calculo
      *    encontrado com sucesso; agora eh soh atualizar a respectiva
      *    data de pesquisa.
      *
                MOVE            ARG02 TO DT-TRB-DDMMAAAA
                MOVE DIA-TRB-DDMMAAAA TO DIA-ULT-UTIL-MES-ANTERIOR-PSQ
                MOVE MES-TRB-DDMMAAAA TO MES-ULT-UTIL-MES-ANTERIOR-PSQ
                MOVE SEC-TRB-DDMMAAAA TO SEC-ULT-UTIL-MES-ANTERIOR-PSQ
                MOVE ANO-TRB-DDMMAAAA TO ANO-ULT-UTIL-MES-ANTERIOR-PSQ

                MOVE DT-ULT-UTIL-MES-ANTERIOR-PSQ TO DT-BASE-MM-ANT
                DISPLAY '999 ' CTE-PROG ' DATA BASE PADRAO PARA O ULTIMO
      -                 ' DIA DO MES ANTERIOR: DT-BASE-MM-ANT = '
                        DT-BASE-MM-ANT

           END-IF

      *

           PERFORM 970000-LEITURA-032E
           .
      *
       100099-SAI-INICIO.
           EXIT.
      *
      *---------------------------------------------------------------*
       970000-LEITURA-032E             SECTION.
      *---------------------------------------------------------------*
      *
           INITIALIZE REG-RSTF502
      *
           READ SGTF032E INTO REG-RSTF502
                AT END
                   MOVE 1                      TO GDA-FIM-032E
                NOT AT  END
                   ADD  +1                     TO GDA-CNT-032E
           END-READ
           .
      *
       970099-SAI-LEITURA-380E.
           EXIT.
      *
      *---------------------------------------------------------------*
       200000-PROCESSAMENTO            SECTION.
      *---------------------------------------------------------------*

           MOVE 'S' TO AX-GRV-SAID
           MOVE ZEROS TO AX-VL-SDO-TIT-FIM-MM
                         AX-VL-SDO-TIT-FIM-MM-ANT
                         AX-VL-SDO-TIT-FIM-MM-ATU
                         AX-VL-AMTZ-JUR-VND-TIT-MM-ATU
                         AX-VL-RC-TIT
                         AX-QT-PGTO-ATIV-PROBLEMATICO
           MOVE DT-ULT-UTIL-MES-ANTERIOR-PSQ TO DT-BASE-MM-ANT

      *    Primeiro passo: Saldo do papel no ultimo dia util do mes
      *    anterior ao mes base do calculo (mês x) -
      *    Tabela DB2SGT.PSC_DRIA_SDO_OPR (sequencial maior).

           MOVE FD502D-NR-UNCO-OPR  TO T005-NR-UNCO-CTR-OPR
           PERFORM 210000-PSQ-PAPL-NR-UNCO

           MOVE DT-ULT-UTIL-MES-ANTERIOR-PSQ TO AX-DT-TRB-DB2
           PERFORM 220000-PSQ-SDO-PAPL-FIM-MM
      *    Saldo encontrado, hora de guardah-lo.
           MOVE AX-VL-SDO-TIT-FIM-MM TO AX-VL-SDO-TIT-FIM-MM-ANT

      *    Segundo passo: Saldo do papel no ultimo dia util do mes
      *    base do calculo (mês y) - Tabela
      *    DB2SGT.PSC_DRIA_SDO_OPR (sequencial maior).

           MOVE FD502D-NR-UNCO-OPR  TO T005-NR-UNCO-CTR-OPR
           MOVE DT-ULT-UTIL-MES-ATUAL-PSQ TO AX-DT-TRB-DB2

           PERFORM 220000-PSQ-SDO-PAPL-FIM-MM
      *    Saldo encontrado, hora de guardah-lo.
           MOVE AX-VL-SDO-TIT-FIM-MM TO AX-VL-SDO-TIT-FIM-MM-ATU

      *    Terceiro passo: caso seja necessario, ajustar o saldo de
      *    posicao do mes anterior. Esse ajuste consiste no seguinte:
      *    Se a Data de Compra da Operacao eh do mesmo mes e ano do
      *    do mes base, significa, pela logica, que a Operacao nao
      *    tinha saldo no mes anterior. Neste caso, o saldo do mes
      *    anterior nao pode ser zerado; serah entao ajustado para o
      *    saldo da primeira Data de Posicao encontrada.
           PERFORM 230000-AJST-PSQ-SDO-PAPL-ANT

      *    Quarto passo: Amortizacoes/Juros e Vendas do Papel -
      *    Tabela DB2SGT.DMTV_CX_TIT, com IN_MVTC_ATV = 'S' e
      *    CD_TIP_LCTO_CTB IN (2, 0), no mes base.

           MOVE T005-CD-TIT-OPR-ETRD TO 086-CD-TIT-OPR-ETRD
           MOVE T005-NR-DVS-TIT      TO 086-NR-DVS-TIT
           MOVE T005-CD-CTRA-TIT-VL  TO 086-CD-CTRA-TIT-VL

      *    Atraves do Papel e Tranche jah obtidos, buscar a
      *    soma de valores desse Papel e Tranche na tabela de
      *    Demonstrativo de Caixa.

      *    Observacao: ainda nao sabemos se esse papel eh um Ativo
      *    Problematico. Estamos apenas antecipando a verfificacao da
      *    quantidade de movimentacoes desse Papel porque trata-se
      *    exatamente da mesma pesquisa (para somar o valor das
      *    movimentacoes e para saber se hah alguma movimentacao).
      *    A verificacao de Ativo Problematico serah feira depois,
      *    e a variavel AX-QT-PGTO-ATIV-PROBLEMATICO soh serah
      *    realmente usada se esse papel estiver marcado como
      *    Ativo Problematico.

           EXEC SQL
               SELECT VALUE(SUM(VL_EFT_MVTC), 0), COUNT(*)
               INTO :AX-VL-AMTZ-JUR-VND-TIT-MM-ATU,
                    :AX-QT-PGTO-ATIV-PROBLEMATICO
               FROM DB2SGT.DMTV_CX_TIT
               WHERE CD_TIT_OPR_ETRD  = :086-CD-TIT-OPR-ETRD
                 AND NR_DVS_TIT       = :086-NR-DVS-TIT
                 AND CD_CTRA_TIT_VL   = :086-CD-CTRA-TIT-VL
                 AND IN_MVTC_ATV = 'S'
                 AND CD_TIP_LCTO_CTB IN (2, 0)
                 AND DT_EFT_MVTC BETWEEN :DT-PRMO-DD-MES-ATUAL-PSQ
                                     AND :DT-ULT-DD-MES-ATUAL-PSQ
                 AND DT_EFT_MVTC      > :DT-BASE-MM-ANT
           END-EXEC
      * VRS006: Para ativos 'padrao', que jah estavam ativos no mes
      *         base e tambem no mes anterior (situacao que serah detec-
      *         tada atraves de leituras na tabela de Posicao de Saldo
      *         (Datas de posicao) e na tabela de Operacoes Estruturadas
      *         (Data de Compra) o conteudo de DT-BASE-MM-ANT serah o
      *         padrao (ultimo dia util do mes anterior). Para ativos
      *         cuja Data de Compra seja no mesmo mes base, o conteudo
      *         de DT-BASE-MM-ANT serah o da primeira Data de Posicao
      *         naquele mes - e, portanto, neste caso de excecao,
      *         qualquer movimentacao desta data ou de data anterior
      *         serah desconsiderada.

      * MOCK-POINT MCKV2
           IF SQLCODE NOT EQUAL ZEROS
               PERFORM 999000-ERRO-06
               PERFORM 990000-ABENDA
           END-IF

      *    Com os valores dos tres passos prontos, efetuar o calculo
      *    final.
           COMPUTE AX-VL-RC-TIT =
                   AX-VL-SDO-TIT-FIM-MM-ATU - AX-VL-SDO-TIT-FIM-MM-ANT
                 + AX-VL-AMTZ-JUR-VND-TIT-MM-ATU
           ON SIZE ERROR
               PERFORM 999000-ERRO-07
           END-COMPUTE

      *    Agora precisamos verificar o caso de excecao: ativo problema-
      *    tico. Se a Operacao que estah sendo processada, que jah teve
      *    sua receita calculada, estiver marcado como Ativo Problema-
      *    tico, essa receita soh serah enviada ao REN se tiver ocorri-
      *    do algum pagamento para ela no mes base.
           INITIALIZE CACS00IC-DADOS
               REPLACING ALPHANUMERIC BY SPACES NUMERIC BY ZEROS

           MOVE T005-NR-UNCO-CTR-OPR TO CACK00IC-NR-UNCO-CTR-OPR
           MOVE DIA-PSC-LK           TO DIA-TRB-DB2
           MOVE MES-PSC-LK           TO MES-TRB-DB2
      *    Se o ano for zero, presumir 2100
           IF ANO-PSC-LK NOT EQUAL ZEROS
               MOVE 20                   TO SEC-TRB-DB2
           ELSE
               MOVE 21                   TO SEC-TRB-DB2
           END-IF
           MOVE ANO-PSC-LK           TO ANO-TRB-DB2
           MOVE AX-DT-TRB-DB2           TO CACK00IC-DT-ANL-RSCO-ATPB

           MOVE LENGTH OF CACS00IC-DADOS TO EIBCALEN
           CALL CACS00IC USING DFHEIBLK CACS00IC-DADOS

           IF CACK00IC-CD-ERRO NOT EQUAL ZEROS
               IF CACK00IC-CD-SQLCODE EQUAL +100
                   MOVE  '0'   TO  CACK00IC-IN-ATPB
               ELSE
                   PERFORM 999000-ERRO-10
                   PERFORM 990000-ABENDA
               END-IF
           END-IF

      *    Avaliando o retorno:
      *    CACK00IC-IN-ATPB = '3', OU '9', OU 'P' significa tratar-se
      *    de um Ativo Problematico. Neste caso, ou seja, se for um
      *    Ativo Problematico, a Receita e o MSD dele soh serao
      *    enviados ao REN se ele teve movimentacao no mes base do
      *    calculo.
           IF  (CACK00IC-IN-ATPB EQUAL '3' OR '9' OR 'P')
               MOVE CACK00IC-NR-UNCO-CTR-OPR
                 TO AX-DISPLAY-NR-UNCO-CTR-OPR
               DISPLAY 'A OPERACAO ' AX-DISPLAY-NR-UNCO-CTR-OPR
                       ' ESTAH ASSINALADA COMO ATIVO PROBLEMATICO.'
      *        Guardar esse Ativo Problematico no arquivo de saida
               INITIALIZE SGTF032P-REG-DETALHE
               MOVE T005-CD-TIT-OPR-ETRD      TO F032P-CD-TIT-OPR-ETRD
               MOVE FD502D-NR-UNCO-OPR        TO F032P-NR-UNCO-CTR-OPR
               MOVE CACK00IC-DT-ANL-RSCO-ATPB TO F032P-DT-ANL-RSCO-ATPB
               MOVE CACK00IC-IN-ATPB          TO F032P-IN-ATPB
               WRITE SGTF032P-FD FROM SGTF032P-REG-DETALHE
               ADD 1 TO GDA-GRV-032P

               IF (AX-QT-PGTO-ATIV-PROBLEMATICO EQUAL ZEROS)
                   DISPLAY 'O ATIVO PROBLEMATICO '
                           AX-DISPLAY-NR-UNCO-CTR-OPR
                           ' NAO TEVE MOVIMENTACAO NESTE MES BASE E, POR
      -                    ' ISSO, NAO TERAH MSD NEM RECEITA ENVIADOS.'
                   MOVE 'N' TO AX-GRV-SAID
               END-IF
           END-IF

           IF DT-ULT-UTIL-MES-ANTERIOR-PSQ NOT EQUAL DT-BASE-MM-ANT
      *    Avisar sobre os ativos cuja data base para a consulta de
      *    movimentacao seja diferente do padrao.
               MOVE FD502D-NR-UNCO-OPR TO AX-DISPLAY-NR-UNCO-CTR-OPR
               DISPLAY 'PARA O ATIVO ' AX-DISPLAY-NR-UNCO-CTR-OPR
                       ', A DATA BASE PARA A CONSULTA DE MOVIMENTACAO DE
      -                ' AMORTIZACOES, JUROS E VENDAS FOI DIFERENTE DO P
      -                'ADRAO. PARA ESTE ATIVO, ESSA DATA FOI '
                       DT-BASE-MM-ANT ', E NAO O ULTIMO DIA UTIL DO MES
      -                'ANTERIOR.'
           END-IF

           IF AX-GRV-SAID EQUAL 'S'
               PERFORM 980000-GRAVA-SAIDA
           END-IF

      *
           PERFORM 970000-LEITURA-032E
           .
      *
       200099-SAI-PROCESSAMENTO.
           EXIT.

      *
      *---------------------------------------------------------------*
       210000-PSQ-PAPL-NR-UNCO         SECTION.
      *---------------------------------------------------------------*

      *    Atraves do Numero Unico da Operacao, buscar dados da
      *    Carteira, Papel e Tranche na tabela de Operacoes.

           EXEC SQL
               SELECT CD_CTRA_TIT_VL, CD_TIT_OPR_ETRD,
                      NR_DVS_TIT, DT_CPR_OPR
               INTO :T005-CD-CTRA-TIT-VL, :T005-CD-TIT-OPR-ETRD,
                    :T005-NR-DVS-TIT, :T005-DT-CPR-OPR
               FROM DB2SGT.OPR_ETRD_MCP
               WHERE NR_UNCO_CTR_OPR = :T005-NR-UNCO-CTR-OPR
               ORDER BY NR_VRS_OPR_ETRD DESC
               FETCH FIRST 1 ROWS ONLY
           END-EXEC
           IF SQLCODE NOT EQUAL ZEROS
               PERFORM 999000-ERRO-08
               PERFORM 990000-ABENDA
           END-IF

           .
      *
       210099-SAI-PSQ-PAPL-NR-UNCO.
           EXIT.

      *
      *---------------------------------------------------------------*
       220000-PSQ-SDO-PAPL-FIM-MM      SECTION.
      *---------------------------------------------------------------*

      *    Com os dados de Carteira, Papel e Tranche obtidos, buscar
      *    o Saldo do Papel no ultimo dia util do mes.

           MOVE T005-CD-CTRA-TIT-VL  TO 004T-CD-CTRA-TIT-VL
           MOVE AX-DT-TRB-DB2        TO 004T-DT-PSC
           MOVE T005-CD-TIT-OPR-ETRD TO 004T-CD-TIT-OPR-ETRD
           MOVE T005-NR-DVS-TIT      TO 004T-NR-DVS-TIT

           EXEC SQL
               SELECT VL_SDO_TIT_DT_PSC
               INTO :AX-VL-SDO-TIT-FIM-MM
               FROM DB2SGT.PSC_DRIA_SDO_OPR
               WHERE CD_CTRA_TIT_VL  = :004T-CD-CTRA-TIT-VL
                 AND DT_PSC         <= :004T-DT-PSC
                 AND CD_TIT_OPR_ETRD = :004T-CD-TIT-OPR-ETRD
                 AND NR_DVS_TIT      = :004T-NR-DVS-TIT
               ORDER BY DT_PSC DESC, NR_SEQL_PSC_DRIA DESC
               FETCH FIRST 1 ROWS ONLY
           END-EXEC
      * MOCK-POINT MCKV3
           IF SQLCODE NOT EQUAL ZEROS AND +100
               PERFORM 999000-ERRO-09
               PERFORM 990000-ABENDA
           ELSE
               IF SQLCODE EQUAL +100
      *        VRS003: Se procuramos uma Posicao de Saldo desse titulo,
      *                em uma Data de Posicao MENOR OU IGUAL aa do
      *                ultimo dia util, seja do Mes Base ou do Mes
      *                Anterior, e nao encontramos nada, significa que
      *                nao hah NENHUMA Posicao de Saldo desse Titulo/
      *                Operacao para ser informada - assumiremos que
      *                ainda nao existia essa Operacao e o saldo serah
      *                zero neste caso.
                   MOVE ZEROS TO AX-VL-SDO-TIT-FIM-MM
               END-IF
           END-IF

           .
      *
       220099-SAI-PSQ-SDO-PAPL-FIM-MM.
           EXIT.

      *
      *---------------------------------------------------------------*
       230000-AJST-PSQ-SDO-PAPL-ANT    SECTION.
      *---------------------------------------------------------------*

      *    Esse ajuste serah realizado SOMENTE SE a Data de Compra
      *    da Operacao for do mesmo mes e ano referente ao mes base
      *    que estah sendo processado.

           MOVE T005-DT-CPR-OPR TO AX-DT-TRB-DB2

           IF MES-TRB-DB2 EQUAL MES-ULT-UTIL-MES-ATUAL-PSQ AND
              SEC-TRB-DB2 EQUAL SEC-ULT-UTIL-MES-ATUAL-PSQ AND
              ANO-TRB-DB2 EQUAL ANO-ULT-UTIL-MES-ATUAL-PSQ

      *        Com os dados de Carteira, Papel e Tranche obtidos, buscar
      *        o primeiro Saldo desse Papel cuja Data de Compra eh do
      *        atual mes base. Esse primeiro Saldo do atual mes base,
      *        neste caso de excecao, serah considerado como se fosse
      *        o Saldo do final do mes anterior.

               MOVE T005-CD-CTRA-TIT-VL  TO 004T-CD-CTRA-TIT-VL
               MOVE AX-DT-TRB-DB2        TO 004T-DT-PSC
               MOVE T005-CD-TIT-OPR-ETRD TO 004T-CD-TIT-OPR-ETRD
               MOVE T005-NR-DVS-TIT      TO 004T-NR-DVS-TIT
               MOVE MES-TRB-DB2          TO AX-MES-DB2
               MOVE SEC-ANO-TRB-DB2      TO AX-TMP-SEC-ANO
               MOVE AX-TMP-SEC-ANO       TO AX-ANO-DB2

               EXEC SQL
                   SELECT VL_SDO_TIT_DT_PSC, DT_PSC
                   INTO :AX-VL-SDO-TIT-FIM-MM, :DT-BASE-MM-ANT
                   FROM DB2SGT.PSC_DRIA_SDO_OPR
                   WHERE CD_CTRA_TIT_VL  = :004T-CD-CTRA-TIT-VL
                     AND DT_PSC         >= :004T-DT-PSC
                     AND CD_TIT_OPR_ETRD = :004T-CD-TIT-OPR-ETRD
                     AND NR_DVS_TIT      = :004T-NR-DVS-TIT
                     AND MONTH(DT_PSC)   = :AX-MES-DB2
                     AND YEAR(DT_PSC)    = :AX-ANO-DB2
                   ORDER BY DT_PSC, NR_SEQL_PSC_DRIA DESC
                   FETCH FIRST 1 ROWS ONLY
               END-EXEC
      * MOCK-POINT MCKV4
               IF SQLCODE NOT EQUAL ZEROS AND +100
                   PERFORM 999000-ERRO-11
                   PERFORM 990000-ABENDA
               ELSE
                   IF SQLCODE EQUAL +100
      *                A unica explicacao logica para nao ser encontrada
      *                uma Data de Posicao neste caso eh que realmente
      *                nao hah nenhuma cadastrada na tabela neste mo-
      *                mento para este titulo, com data deste mes base,
      *                mesmo com a Operacao ativa - ou tendo estado
      *                ativa - durante o mes base. Nesse caso, nao tem
      *                jeito: o saldo serah zero mesmo.
                       MOVE ZEROS TO AX-VL-SDO-TIT-FIM-MM-ANT
                       MOVE DT-ULT-UTIL-MES-ANTERIOR-PSQ
                         TO DT-BASE-MM-ANT
                   ELSE
      *                Saldo encontrado, na primeira data de posicao
      *                do mes da compra do ativo
                       MOVE AX-VL-SDO-TIT-FIM-MM
                         TO AX-VL-SDO-TIT-FIM-MM-ANT
                   END-IF
               END-IF
           END-IF

           .
      *
       230099-SAI-AJST-PSQ-SDO-ANT.
           EXIT.


      *
      *---------------------------------------------------------------*
       980000-GRAVA-SAIDA              SECTION.
      *---------------------------------------------------------------*
      *

      *    Vamos considerar que o arquivo de entrada contem todas as
      *    informacoes referentes ao MSD, que sao calculadas no pro-
      *    grama SGTP0031. Mas essas informacoes soh podem ser envia-
      *    das ao REN caso o Papel nao seja um Ativo Problematico ou,
      *    caso seja um Ativo Problematico, tenha tido movimentacao no
      *    no mes base de calculo (a verificacao eh feita aqui no
      *    programa SGTP0032). Por isso, aqui serah feita a gravacao
      *    do registro de saida igualzinho ao de entrada (MSD) e,
      *    depois, a adaptacao do tipo de registro e do valor apurado
      *    aqui no SGTP0032 e geracao do novo registro de saida
      *    (Receita).

      *    Registro de MSD do Papel (por Operacao)
           WRITE SGTF032S-FD              FROM RSTF502-REG-DETALHE
           ADD +1                         TO GDA-GRV-032S

      *    Registro de Receita do Papel (por Operacao)
           MOVE 193          TO FD502D-CD-TIP-OCOR
           IF AX-VL-RC-TIT LESS ZERO
               MOVE '-' TO FD502D-SNL-VL-OGM
           ELSE
               MOVE '+' TO FD502D-SNL-VL-OGM
           END-IF
           MOVE AX-VL-RC-TIT TO FD502D-VL-OGM
           WRITE SGTF032S-FD              FROM RSTF502-REG-DETALHE
           ADD +1                         TO GDA-GRV-032S
           .
      *
       980000-SAI-GRAVA-SAIDA.
           EXIT.
      *---------------------------------------------------------------*
       301000-FINAL                    SECTION.
      *---------------------------------------------------------------*
      *
           DISPLAY '999 ' CTE-PROG ' FIM NORMAL     '
           DISPLAY '999 ' CTE-PROG ' LK-DATA-PRCT-AMD = '
                                     LK-DATA-PRCT-AMD
           DISPLAY '999 ' CTE-PROG ' LK-DATA-PSC-AMD = '
                                     LK-DATA-PSC-AMD
           DISPLAY '999 ' CTE-PROG ' DT-ULT-UTIL-MES-ANTERIOR-PSQ = '
                                     DT-ULT-UTIL-MES-ANTERIOR-PSQ
           DISPLAY '999 ' CTE-PROG ' DT-ULT-UTIL-MES-ATUAL-PSQ = '
                                     DT-ULT-UTIL-MES-ATUAL-PSQ
           DISPLAY '999 ' CTE-PROG ' DT-ULT-DD-MES-ATUAL-PSQ = '
                                     DT-ULT-DD-MES-ATUAL-PSQ
           DISPLAY '999 ' CTE-PROG ' DT-PRMO-DD-MES-ATUAL-PSQ = '
                                     DT-PRMO-DD-MES-ATUAL-PSQ

           IF GDA-CNT-032E EQUAL ZEROS
               DISPLAY '999 ' CTE-PROG ' ATENCAO: ARQUIVO SGTF032E VAZIO
      -                                '.'
           END-IF
           DISPLAY '999 ' CTE-PROG ' GDA-CNT-032E = ' GDA-CNT-032E
           DISPLAY '999 ' CTE-PROG ' GDA-GRV-032S = ' GDA-GRV-032S
           DISPLAY '999 ' CTE-PROG ' GDA-GRV-032P = ' GDA-GRV-032P
      *
           CLOSE SGTF032E SGTF032S SGTF032P
           .
      *
       300099-SAI-FINAL.
           EXIT.

      *-----------------------------------------------------------------
       999000-ERROS                    SECTION.
      *-----------------------------------------------------------------
       999000-ERRO-01.
           MOVE LK-SIZE TO AX-DISPLAY-TAM-AREA
           DISPLAY '888 ' CTE-PROG ' 01 - TAMANHO DO PARAMETRO INCORRETO
      -                            '.'
           DISPLAY '888 ' CTE-PROG ' 01 - LK-SIZE = '
                                     AX-DISPLAY-TAM-AREA
           .
       999000-ERRO-02.
           DISPLAY '888 ' CTE-PROG ' 02 - A DATA DE PROCESSAMENTO E/OU A
      -                            '  DE POSICAO NAO EH NUMERICA.'
           .

       999000-ERRO-03.
           DISPLAY '888 ' CTE-PROG ' 03 - ERRO DE ABERTURA DE ARQUIVO.'
           IF FS-032E NOT EQUAL ZEROS
               DISPLAY '888 ' CTE-PROG ' 03 - FILE STATUS SGTF032E = '
                                         FS-032E
           END-IF
           IF FS-032S NOT EQUAL ZEROS
               DISPLAY '888 ' CTE-PROG ' 03 - FILE STATUS SGTF032S = '
                                         FS-032S
           END-IF
           IF FS-032P NOT EQUAL ZEROS
               DISPLAY '888 ' CTE-PROG ' 03 - FILE STATUS SGTF032P = '
                                         FS-032P
           END-IF
           .

       999000-ERRO-04.
           DISPLAY '888 ' CTE-PROG ' 04 - ERRO NA CHAMADA A SBDATAF FUNC
      -                            'AO 16 (MES DA DATA BASE).'
           DISPLAY '888 ' CTE-PROG ' 04 - RETURN CODE = ' RETURN-CODE
           DISPLAY '888 ' CTE-PROG ' 04 - ARG01 = ' ARG01
           DISPLAY '888 ' CTE-PROG ' 04 - ARG02 = ' ARG02
           DISPLAY '888 ' CTE-PROG ' 04 - ARG03 = ' ARG03
           .

       999000-ERRO-05.
           DISPLAY '888 ' CTE-PROG ' 05 - ERRO NA CHAMADA A SBDATAF FUNC
      -                            'AO 16 (MES ANTERIOR AA DATA BASE).'
           DISPLAY '888 ' CTE-PROG ' 05 - RETURN CODE = ' RETURN-CODE
           DISPLAY '888 ' CTE-PROG ' 05 - ARG01 = ' ARG01
           DISPLAY '888 ' CTE-PROG ' 05 - ARG02 = ' ARG02
           DISPLAY '888 ' CTE-PROG ' 05 - ARG03 = ' ARG03
           .

       999000-ERRO-06.
           MOVE SQLCODE            TO AX-DISPLAY-SQLCODE
           MOVE 086-NR-DVS-TIT     TO AX-DISPLAY-NR-DVS-TIT
           MOVE 086-CD-CTRA-TIT-VL TO AX-DISPLAY-CD-CTRA-TIT-VL
           DISPLAY '888 ' CTE-PROG ' 06 - ERRO NA PESQUISA DE AMORTIZACO
      -                            'ES/JUROS E VENDAS DO PAPEL.'
           DISPLAY '888 ' CTE-PROG ' 06 - SQLCODE = ' AX-DISPLAY-SQLCODE
           DISPLAY '888 ' CTE-PROG ' 06 - 086-CD-TIT-OPR-ETRD = '
                                          086-CD-TIT-OPR-ETRD
           DISPLAY '888 ' CTE-PROG ' 06 - 086-NR-DVS-TIT = '
                                          AX-DISPLAY-NR-DVS-TIT
           DISPLAY '888 ' CTE-PROG ' 06 - 086-CD-CTRA-TIT-VL = '
                                          AX-DISPLAY-CD-CTRA-TIT-VL
           DISPLAY '888 ' CTE-PROG ' 06 - DT-PRMO-DD-MES-ATUAL-PSQ = '
                                          DT-PRMO-DD-MES-ATUAL-PSQ
           DISPLAY '888 ' CTE-PROG ' 06 - DT-ULT-DD-MES-ATUAL-PSQ = '
                                          DT-ULT-DD-MES-ATUAL-PSQ

           .

       999000-ERRO-07.
           DISPLAY '888 ' CTE-PROG ' 07 - ERRO NO CALCULO DA RECEITA (SI
      -                            'ZE ERROR).'
           MOVE AX-VL-SDO-TIT-FIM-MM-ATU TO AX-DISPLAY-VL-MNTR
           DISPLAY '888 ' CTE-PROG ' 07 - AX-VL-SDO-TIT-FIM-MM-ATU = '
                                          AX-DISPLAY-VL-MNTR
           MOVE AX-VL-SDO-TIT-FIM-MM-ANT TO AX-DISPLAY-VL-MNTR
           DISPLAY '888 ' CTE-PROG ' 07 - AX-VL-SDO-TIT-FIM-MM-ANT = '
                                          AX-DISPLAY-VL-MNTR
           MOVE AX-VL-AMTZ-JUR-VND-TIT-MM-ATU TO AX-DISPLAY-VL-MNTR
           DISPLAY '888 ' CTE-PROG
                   ' 07 - AX-VL-AMTZ-JUR-VND-TIT-MM-ATU = '
                   AX-DISPLAY-VL-MNTR
           IF AX-VL-RC-TIT IS NUMERIC
               MOVE AX-VL-RC-TIT TO AX-DISPLAY-VL-MNTR
               DISPLAY '888 ' CTE-PROG ' 07 - AX-VL-RC-TIT = '
                                              AX-DISPLAY-VL-MNTR
           END-IF
      *    Aqui, e apenas aqui, eh feito tambem o desvio para o SBABEND,
      *    porque nao dah para colocah-lo dentro da clausula
      *    ON SIZE ERROR do comando COMPUTE.
           PERFORM 990000-ABENDA

           .
       999000-ERRO-08.
           MOVE SQLCODE                  TO AX-DISPLAY-SQLCODE
           MOVE T005-NR-UNCO-CTR-OPR     TO AX-DISPLAY-NR-UNCO-CTR-OPR
           DISPLAY '888 ' CTE-PROG ' 08 - ERRO NA PESQUISA DE PAPEL E TR
      -                     'ANCHE ATRAVES DO NUMERO UNICO DA OPERACAO.'
           DISPLAY '888 ' CTE-PROG ' 08 - SQLCODE = ' AX-DISPLAY-SQLCODE
           DISPLAY '888 ' CTE-PROG ' 08 - T005-NR-UNCO-CTR-OPR = '
                                          AX-DISPLAY-NR-UNCO-CTR-OPR

           .
       999000-ERRO-09.
           MOVE SQLCODE             TO AX-DISPLAY-SQLCODE
           MOVE 004T-CD-CTRA-TIT-VL TO AX-DISPLAY-CD-CTRA-TIT-VL
           MOVE 004T-NR-DVS-TIT     TO AX-DISPLAY-NR-DVS-TIT
           DISPLAY '888 ' CTE-PROG ' 09 - ERRO EM UMA DAS PESQUISAS DE S
      -                            'ALDO DO PAPEL EM UMA DATA DE POSICAO
      -                            '.'
           DISPLAY '888 ' CTE-PROG ' 09 - SQLCODE = ' AX-DISPLAY-SQLCODE
           DISPLAY '888 ' CTE-PROG ' 09 - 004T-CD-CTRA-TIT-VL = '
                                          AX-DISPLAY-CD-CTRA-TIT-VL
           DISPLAY '888 ' CTE-PROG ' 09 - 004T-DT-PSC = '
                                          004T-DT-PSC
           DISPLAY '888 ' CTE-PROG ' 09 - 004T-CD-TIT-OPR-ETRD = '
                                          004T-CD-TIT-OPR-ETRD
           DISPLAY '888 ' CTE-PROG ' 09 - 004T-NR-DVS-TIT = '
                                          AX-DISPLAY-NR-DVS-TIT

           .
       999000-ERRO-10.
           MOVE CACK00IC-NR-UNCO-CTR-OPR TO AX-DISPLAY-NR-UNCO-CTR-OPR
           MOVE CACK00IC-CD-SQLCODE      TO AX-DISPLAY-SQLCODE
           DISPLAY '888 ' CTE-PROG ' 10 - ERRO NA CHAMADA AO PROGRAMA CA
      -                 'CS00IC.'

           DISPLAY '888 ' CTE-PROG ' 10 - CACK00IC-NR-UNCO-CTR-OPR = '
                                          AX-DISPLAY-NR-UNCO-CTR-OPR
           DISPLAY '888 ' CTE-PROG ' 10 - CACK00IC-DT-ANL-RSCO-ATPB = '
                                          CACK00IC-DT-ANL-RSCO-ATPB
           DISPLAY '888 ' CTE-PROG ' 10 - CACK00IC-CD-ERRO = '
                                          CACK00IC-CD-ERRO
           DISPLAY '888 ' CTE-PROG ' 10 - CACK00IC-CD-SQLCODE = '
                                          AX-DISPLAY-SQLCODE

           .

       999000-ERRO-11.
           MOVE SQLCODE             TO AX-DISPLAY-SQLCODE
           MOVE 004T-CD-CTRA-TIT-VL TO AX-DISPLAY-CD-CTRA-TIT-VL
           MOVE 004T-NR-DVS-TIT     TO AX-DISPLAY-NR-DVS-TIT
           DISPLAY '888 ' CTE-PROG ' 11 - ERRO NA PESQUISA DE AJUSTE DO
      -                            'SALDO DO PAPEL EM UMA DATA DE POSICA
      -                            'O.'
           DISPLAY '888 ' CTE-PROG ' 11 - SQLCODE = ' AX-DISPLAY-SQLCODE
           DISPLAY '888 ' CTE-PROG ' 11 - 004T-CD-CTRA-TIT-VL = '
                                          AX-DISPLAY-CD-CTRA-TIT-VL
           DISPLAY '888 ' CTE-PROG ' 11 - 004T-DT-PSC = '
                                          004T-DT-PSC
           DISPLAY '888 ' CTE-PROG ' 11 - 004T-CD-TIT-OPR-ETRD = '
                                          004T-CD-TIT-OPR-ETRD
           DISPLAY '888 ' CTE-PROG ' 11 - 004T-NR-DVS-TIT = '
                                          AX-DISPLAY-NR-DVS-TIT

           .

      *---------------------------------------------------------------*
       990000-ABENDA SECTION.
      *---------------------------------------------------------------*
      *
           DISPLAY '888 ' CTE-PROG ' 888-CANCELADO'
           DISPLAY '888 ' CTE-PROG ' GDA-CNT-032E = ' GDA-CNT-032E
           DISPLAY '888 ' CTE-PROG ' GDA-GRV-032S = ' GDA-GRV-032S
           DISPLAY '888 ' CTE-PROG ' GDA-GRV-032P = ' GDA-GRV-032P
           CLOSE SGTF032E SGTF032S SGTF032P
           CALL SBABEND
           .
      *
      *---------------------------------------------------------------*
      *        S G T P 0 0 3 2  -  F I M  D O  P R O G R A M A        *
      *---------------------------------------------------------------*