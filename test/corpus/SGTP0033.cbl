      CBL ARITH(EXTEND)
      *---------------------------------------------------------------*
       IDENTIFICATION DIVISION.
      *---------------------------------------------------------------*
      *                                                               *
      *---------------------------------------------------------------*
       PROGRAM-ID.   SGTP0033.
      *---------------------------------------------------------------*
      * Formatar informacao de receita dos papeis do SGT para informe *
      * aos sistemas REN/ARI.                                         *
      *---------------------------------------------------------------*
      * VRS002 02/07/25 F6769534 - AJUSTE NO CALCULO. MUDAR ORIGEM DO *
      *                            P.U. BUSCAR NA TABELA DE IMPORTA-  *
      *                            CAO, NA QUAL HAH MAIOR PRECISAO.   *
      * VRS001 20/06/25 F6769534 - IMPLANTACAO                        *
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
           SELECT SGTF033E ASSIGN TO UT-S-SGTF033E
                           FILE STATUS IS FS-033E.
           SELECT SGTF033S ASSIGN TO UT-S-SGTF033S
                           FILE STATUS IS FS-033S.
           SELECT SGTF033P ASSIGN TO UT-S-SGTF033P
                           FILE STATUS IS FS-033P.
      *
      *---------------------------------------------------------------*
       DATA DIVISION.
      *---------------------------------------------------------------*
      *
       FILE SECTION.
      *
       FD  SGTF033E
           RECORDING F
           RECORD 230
           BLOCK 0.
      *
       01  SGTF033E-FD                  PIC X(230).
      *
       FD  SGTF033S
           RECORDING F
           RECORD 230
           BLOCK 0.
      *
       01  SGTF033S-FD                  PIC X(230).

       FD  SGTF033P
           RECORDING F
           RECORD 100
           BLOCK 0.
      *
       01  SGTF033P-FD                  PIC X(100).

      *---------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *---------------------------------------------------------------*
      *
       77  CTE-INICIO                   PIC X(29) VALUE
           'SGTP0033 - W.S.S. COMECA AQUI'.
       77  CTE-PROG                     PIC X(16) VALUE
           '*** SGTP0033 ***'.
       77  CTE-VERS                     PIC  X(06) VALUE 'VRS002'.

       01  TAB-PROGRAMAS-CHAMADOS.
         03  CACS0132                   PIC  X(008)  VALUE 'CACS0132'.
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
       01  GDA-FIM-033E                  PIC 9(01) VALUE ZEROS.
       01  GDA-CNT-033E-01               PIC 9(09) VALUE ZEROS.
       01  GDA-CNT-033E-02               PIC 9(09) VALUE ZEROS.
       01  GDA-GRV-033S                  PIC 9(09) VALUE ZEROS.
       01  GDA-GRV-033P                  PIC 9(09) VALUE ZEROS.

      *    Registro header do arquivo de saida, no formato RSTK502D
       01  AX-REG-HEADER-RSTK502D.
         03  IND-TIP-REG-H               PIC  9(001) VALUE ZEROS.
         03  SG-SIS-H                    PIC  X(003) VALUE 'SGT'.
         03  DT-DD-SIS-H                 PIC  9(002) VALUE ZEROS.
         03  DT-MM-SIS-H                 PIC  9(002) VALUE ZEROS.
         03  DT-SEC-SIS-H                PIC  9(002) VALUE ZEROS.
         03  DT-AA-SIS-H                 PIC  9(002) VALUE ZEROS.
         03  FILLER                      PIC  X(218) VALUE SPACES.

      *    Registro trailler do arquivo de saida, no formato RSTK502D
       01  AX-REG-TRAILLER-RSTK502D.
         03  IND-TIP-REG-T               PIC  9(001) VALUE 9.
         03  FILLER                      PIC  X(001) VALUE SPACES.
         03  QTDE-REG-SIS                PIC  9(016) VALUE ZEROS.
         03  FILLER                      PIC  X(212) VALUE ZEROS.

      *    Variaveis auxiliares de trabalho
       01  AX-VL-ACM-RC-PRD-715          PIC S9(15)V9(2) COMP-3.
       01  AX-VL-RC-PRD-715              PIC S9(15)V9(2) COMP-3.
       01  AX-ITEM                       PIC S9(4) COMP.
       01  AX-QT-TTL-OCR                 PIC S9(4) COMP.
       01  AX-TMP-SEC-ANO                PIC 9(4).
       01  AX-PSC-EXTT                   PIC X.


      *    Variaveis "display" para exibicao nas mensagens de erro
       01  AX-DISPLAY-TAM-AREA           PIC  9(04) VALUE ZEROS.
       01  AX-DISPLAY-SQLCODE            PIC S9(04) VALUE ZEROS.
       01  AX-DISPLAY-NR-UNCO-CTR-OPR    PIC S9(17)V     VALUE ZEROS.
       01  AX-DISPLAY-CD-CTRA-TIT-VL     PIC S9(09) VALUE ZEROS.
       01  AX-DISPLAY-NR-DVS-TIT         PIC S9(04) VALUE ZEROS.
       01  AX-DISPLAY-VL-MNTR            PIC S9(15)V9(2) VALUE ZEROS.
       01  AX-DISPLAY-QT-MNTR            PIC S9(9) VALUE ZEROS.
       01  AX-DISPLAY-QT-FRC-MNTR        PIC S9(10)V9(8) VALUE ZEROS.
       01  AX-DISPLAY-OPR-ITMT-FNCO-ULT  PIC S9(9) VALUE ZEROS.
       01  AX-CACS0132-CD-RTN-PGM        PIC S9(4) VALUE ZEROS.

      *    Variaveis para as datas de pesquisa
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

       01  AX-DT2-TRB-DB2               PIC X(10)  VALUE '  .  .    '.
       01  AX-DT2-TRB-DB2-RED REDEFINES AX-DT2-TRB-DB2.
           03  SEC2-ANO-TRB-DB2.
             05  SEC2-TRB-DB2           PIC 9(02).
             05  ANO2-TRB-DB2           PIC 9(02).
           03  FILLER                   PIC X.
           03  MES2-TRB-DB2             PIC 9(02).
           03  FILLER                   PIC X.
           03  DIA2-TRB-DB2             PIC 9(02).

      *    Controle de file status dos arquivos
       01  FS-033E                 PIC 9(02)  VALUE  ZEROS.
       01  FS-033S                 PIC 9(02)  VALUE  ZEROS.
       01  FS-033P                 PIC 9(02)  VALUE  ZEROS.

      * Book dos arquivos, de entrada (MSD) e de saida (Produto 715)
-INC RSTK502D

      * Book do arquivo de Erros de Processamento do Produto 715
-INC SGTK033P

      * Books de DCLGEN das tabelas consultadas:
      * DB2SGT.IMP_PSC_DRIA_SDO
-INC SGTK066T

      * DB2SGT.OPR_ETRD_MCP
-INC SGTK005T

      *--> HLPKDFHE - Definicao da area DFHEIBLK
-INC HLPKDFHE

      *    Area para chamada ao programa CACS0132
      *    (Consulta para apurar as Vendas de um Titulo das Carteiras
      *     TVM, dentro de um determinado periodo)
       01  CACS0132-DADOS.
-INC CACK0132

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
           PERFORM 200000-PROCESSAMENTO UNTIL GDA-FIM-033E = 1
           PERFORM 301000-FINAL
      *
           STOP RUN
           .
      *
      *---------------------------------------------------------------*
       100000-INICIO                   SECTION.
      *---------------------------------------------------------------*
      *
           IF  LK-SIZE NOT EQUAL 0006
               PERFORM 999000-ERRO-01
               PERFORM 990000-ABENDA
           END-IF

           IF LK-DATA-PRCT-AMD NOT NUMERIC
               PERFORM 999000-ERRO-02
               PERFORM 990000-ABENDA
           END-IF

           OPEN  INPUT SGTF033E
                OUTPUT SGTF033S
                OUTPUT SGTF033P

           IF  FS-033E NOT EQUAL ZEROS OR
               FS-033S NOT EQUAL ZEROS OR
               FS-033P NOT EQUAL ZEROS
               PERFORM 999000-ERRO-03
               PERFORM 990000-ABENDA
           END-IF

      *
           MOVE ZEROS                  TO  GDA-FIM-033E
           MOVE ZEROS                  TO  GDA-CNT-033E-01
           MOVE ZEROS                  TO  GDA-CNT-033E-02
           MOVE ZEROS                  TO  GDA-GRV-033S
           MOVE ZEROS                  TO  GDA-GRV-033P
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
      *         Ultimo dia util do mes base
                MOVE            ARG02 TO DT-TRB-DDMMAAAA
                MOVE DIA-TRB-DDMMAAAA TO DIA-ULT-UTIL-MES-ATUAL-PSQ
                MOVE MES-TRB-DDMMAAAA TO MES-ULT-UTIL-MES-ATUAL-PSQ
                MOVE SEC-TRB-DDMMAAAA TO SEC-ULT-UTIL-MES-ATUAL-PSQ
                MOVE ANO-TRB-DDMMAAAA TO ANO-ULT-UTIL-MES-ATUAL-PSQ
      *         Ultimo dia corrido, para pesquisar vendas do papel.
                MOVE            ARG03 TO DT-TRB-DDMMAAAA
                MOVE DIA-TRB-DDMMAAAA TO DIA-ULT-DD-MES-ATUAL-PSQ
                MOVE MES-TRB-DDMMAAAA TO MES-ULT-DD-MES-ATUAL-PSQ
                MOVE SEC-TRB-DDMMAAAA TO SEC-ULT-DD-MES-ATUAL-PSQ
                MOVE ANO-TRB-DDMMAAAA TO ANO-ULT-DD-MES-ATUAL-PSQ
      *         Primeiro dia corrido, para pesquisar e vendas do papel.
                MOVE 1                TO DIA-TRB-DDMMAAAA
                MOVE DIA-TRB-DDMMAAAA TO DIA-PRMO-DD-MES-ATUAL-PSQ
                MOVE MES-TRB-DDMMAAAA TO MES-PRMO-DD-MES-ATUAL-PSQ
                MOVE SEC-TRB-DDMMAAAA TO SEC-PRMO-DD-MES-ATUAL-PSQ
                MOVE ANO-TRB-DDMMAAAA TO ANO-PRMO-DD-MES-ATUAL-PSQ
           END-IF

      *    Primeiramente serah feita a gravacao do registro header no
      *    arquivo de saida, porque este arquivo eh o que irah para o
      *    Sistema REN. Serah feita a leitura do arquivo de entrada
      *    duas vezes. Na primeira, serao lidos integralmente e
      *    gravados integralmente no arquivo de saida todos os
      *    registros do arquivo de entrada, que correspondem aos
      *    calculos de MSD e Receita. Depois de gravados esses regis-
      *    tros (registro header, mais todos os registros do arquivo
      *    de entrada), passaremos ao processamento referente ao Pro-
      *    duto 715.

      *    Montar data do mes base no registro header
           MOVE DIA-ULT-UTIL-MES-ATUAL-PSQ TO DT-DD-SIS-H
           MOVE MES-ULT-UTIL-MES-ATUAL-PSQ TO DT-MM-SIS-H
           MOVE SEC-ULT-UTIL-MES-ATUAL-PSQ TO DT-SEC-SIS-H
           MOVE ANO-ULT-UTIL-MES-ATUAL-PSQ TO DT-AA-SIS-H

           WRITE SGTF033S-FD FROM AX-REG-HEADER-RSTK502D

           PERFORM 970000-PRMO-LEITURA-033E
      *
           PERFORM 110000-GRV-MSD-RECEITA UNTIL GDA-FIM-033E = 1

      *
      *    Concluida a primeira leitura, com gravacao de todos os re-
      *    gistros do arquivo de entrada (MSD e Receita jah calculados)
      *    para o arquivo de saida, reiniciar o processo de leitura do
      *    arquivo de entrada.
      *

           CLOSE SGTF033E
           OPEN  INPUT SGTF033E

      * MOCK-POINT MCKV1
           IF  FS-033E NOT EQUAL ZEROS
               PERFORM 999000-ERRO-03
               PERFORM 990000-ABENDA
           END-IF

           MOVE ZEROS TO GDA-FIM-033E
           PERFORM 970000-SGDO-LEITURA-033E
           .
      *
       100099-SAI-INICIO.
           EXIT.

      *---------------------------------------------------------------*
       110000-GRV-MSD-RECEITA          SECTION.
      *---------------------------------------------------------------*
      *
      *    Registro de MSD ou Receita do Papel (por Operacao)
      *    Jah veio pronto no arquivo de entrada, eh soh gravar tambem
      *    no arquivo de saida.
      *
           WRITE SGTF033S-FD              FROM RSTF502-REG-DETALHE
           ADD +1                         TO GDA-GRV-033S

           PERFORM 970000-PRMO-LEITURA-033E

           .
      *
       110099-SAI-GRV-MSD-RECEITA.
           EXIT.

      *---------------------------------------------------------------*
       970000-PRMO-LEITURA-033E        SECTION.
      *---------------------------------------------------------------*
      *
           INITIALIZE REG-RSTF502
      *
           READ SGTF033E INTO REG-RSTF502
                AT END
                   MOVE 1                      TO GDA-FIM-033E
                NOT AT  END
                   ADD  +1                     TO GDA-CNT-033E-01
           END-READ
           .
      *
       970099-SAI-PRMO-LEITURA-033E.
           EXIT.

      *
      *---------------------------------------------------------------*
       970000-SGDO-LEITURA-033E        SECTION.
      *---------------------------------------------------------------*
      *
           INITIALIZE REG-RSTF502
      *
           READ SGTF033E INTO REG-RSTF502
                AT END
                   MOVE 1                      TO GDA-FIM-033E
                NOT AT  END
                   ADD  +1                     TO GDA-CNT-033E-02
           END-READ
           .
      *
       970099-SAI-SGDO-LEITURA-033E.
           EXIT.
      *
      *---------------------------------------------------------------*
       200000-PROCESSAMENTO            SECTION.
      *---------------------------------------------------------------*

      *    Para evitar que se faca o processamento mais de uma vez
      *    para o mesmo Numero Unico, filtrar pelo Tipo de Ocorrencia.
      *    O arquivo de entrada tem dois registros, um de cada Tipo de
      *    Ocorrencia, para o mesmo Numero Unico.

           IF FD502D-CD-TIP-OCOR EQUAL 183

      *        1) Atraves do Numero Unico, trazer dados da Carteira,
      *        Titulo e Tranche.
               MOVE FD502D-NR-UNCO-OPR  TO T005-NR-UNCO-CTR-OPR
               PERFORM 210000-PSQ-PAPL-NR-UNCO

      *        2) Buscar no Sistema CAC as informacoes de vendas do
      *           Ativo. Se houver vendas, serah gerado o registro do
      *           Produto 715; senao, serao gerados apenas MSD e
      *           Receita que jah foram calculdos antes - e jah estao
      *           no arquivo de saida.

               INITIALIZE CACS0132-DADOS
                   REPLACING ALPHANUMERIC BY SPACES NUMERIC BY ZEROS

               MOVE ZEROS TO AX-VL-ACM-RC-PRD-715 AX-QT-TTL-OCR
               MOVE '0001-01-01' TO CACS0132-DT-INC-PSQ
                                    CACS0132-DT-FIM-PSQ
               MOVE '0001-01-01' TO CACS0132-DT-OPR-ITMT-FNCO-ULT
               MOVE ZEROS        TO CACS0132-CD-OPR-ITMT-FNCO-ULT
               SET CACS0132-TEM-MAIS-REGISTRO TO TRUE
               MOVE FD502D-NR-UNCO-OPR TO CACS0132-NR-UNCO-CTR-OPR

               MOVE DT-PRMO-DD-MES-ATUAL-PSQ TO AX-DT-TRB-DB2
               MOVE DIA-PRMO-DD-MES-ATUAL-PSQ TO
                    DIA OF CACS0132-DT-INC-PSQ-R
               MOVE MES-PRMO-DD-MES-ATUAL-PSQ TO
                    MES OF CACS0132-DT-INC-PSQ-R
               MOVE SEC-ANO-TRB-DB2 TO AX-TMP-SEC-ANO
               MOVE AX-TMP-SEC-ANO  TO ANO OF CACS0132-DT-INC-PSQ-R

               MOVE DT-ULT-DD-MES-ATUAL-PSQ TO AX-DT-TRB-DB2
               MOVE DIA-ULT-DD-MES-ATUAL-PSQ
                 TO DIA OF CACS0132-DT-FIM-PSQ-R
               MOVE MES-ULT-DD-MES-ATUAL-PSQ
                 TO MES OF CACS0132-DT-FIM-PSQ-R
               MOVE SEC-ANO-TRB-DB2 TO AX-TMP-SEC-ANO
               MOVE AX-TMP-SEC-ANO  TO ANO OF CACS0132-DT-FIM-PSQ-R

               PERFORM UNTIL NOT CACS0132-TEM-MAIS-REGISTRO
                   MOVE LENGTH OF CACS0132-DADOS TO EIBCALEN
                   CALL CACS0132 USING DFHEIBLK CACS0132-DADOS

                   IF CACS0132-CD-RTN-PGM NOT EQUAL ZEROS AND +100
                       PERFORM 999000-ERRO-08
                       PERFORM 990000-ABENDA
                   END-IF

                   ADD CACS0132-QT-OCR-TAB TO AX-QT-TTL-OCR

                   IF CACS0132-QT-OCR-TAB GREATER ZEROS
      *            Existe movimento de Vendas, efetuar o calculo e
      *            acumular
                       MOVE 1 TO AX-ITEM
                       MOVE '01.01.0001' TO 066T-DT-PSC
                       MOVE '0001-01-01' TO AX-DT2-TRB-DB2
                       PERFORM UNTIL AX-ITEM GREATER CACS0132-QT-OCR-TAB
                               OR AX-ITEM GREATER 10
                           MOVE DIA
                             OF CACS0132-DT-OPR-ITMT-FNCO-R(AX-ITEM)
                             TO DIA-TRB-DB2
                           MOVE MES
                             OF CACS0132-DT-OPR-ITMT-FNCO-R(AX-ITEM)
                             TO MES-TRB-DB2
                           MOVE ANO
                             OF CACS0132-DT-OPR-ITMT-FNCO-R(AX-ITEM)
                             TO AX-TMP-SEC-ANO
                           MOVE AX-TMP-SEC-ANO
                             TO SEC-ANO-TRB-DB2
      *                    Buscar o P.U. na Data de Posicao (se a data
      *                    estah repetida, nao eh necessario buscar
      *                    novamente)
                           IF AX-DT2-TRB-DB2 NOT EQUAL
                               CACS0132-DT-OPR-ITMT-FNCO(AX-ITEM)
                               PERFORM 220000-PSQ-PU-PSC-PAPL
                           END-IF

      *                    Calculo do Produto 715 para esta venda
      *                    (P.U. da venda - P. U. na data de posicao) *
      *                    Quantidade vendida
                           IF AX-PSC-EXTT EQUAL 'S'
                               COMPUTE AX-VL-RC-PRD-715 =
                                 ( CACS0132-VL-PU-OPR-ITMT(AX-ITEM) -
                                   066T-VL-PU-TIT-DT-PSC )
                                 * CACS0132-QT-OPR-ITMT-FNCO(AX-ITEM)
                               ON SIZE ERROR
                                   PERFORM 999000-ERRO-05
                               END-COMPUTE
                           ELSE
                               MOVE ZEROS TO AX-VL-RC-PRD-715
                           END-IF

                           ADD AX-VL-RC-PRD-715 TO AX-VL-ACM-RC-PRD-715
                           ADD 1 TO AX-ITEM
                       END-PERFORM
                   END-IF

      *            Atualizando dados para a rechamada
                   IF CACS0132-TEM-MAIS-REGISTRO
                       MOVE CACS0132-DT-OPR-ITMT-FNCO(10)
                         TO CACS0132-DT-OPR-ITMT-FNCO-ULT
                       MOVE CACS0132-CD-OPR-ITMT-FNCO(10)
                         TO CACS0132-CD-OPR-ITMT-FNCO-ULT
                   END-IF

               END-PERFORM

      *        Se hah vendas, vamos guardar os dados do Produto 715
               IF AX-QT-TTL-OCR GREATER ZEROS
                   PERFORM 980000-GRAVA-SAIDA-715
               END-IF

           END-IF
      *
           PERFORM 970000-SGDO-LEITURA-033E
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
                      NR_DVS_TIT
               INTO :T005-CD-CTRA-TIT-VL, :T005-CD-TIT-OPR-ETRD,
                    :T005-NR-DVS-TIT
               FROM DB2SGT.OPR_ETRD_MCP
               WHERE NR_UNCO_CTR_OPR = :T005-NR-UNCO-CTR-OPR
               ORDER BY NR_VRS_OPR_ETRD DESC
               FETCH FIRST 1 ROWS ONLY
           END-EXEC
           IF SQLCODE NOT EQUAL ZEROS
               PERFORM 999000-ERRO-06
               PERFORM 990000-ABENDA
           END-IF

           .
      *
       210099-SAI-PSQ-PAPL-NR-UNCO.
           EXIT.

      *
      *---------------------------------------------------------------*
       220000-PSQ-PU-PSC-PAPL          SECTION.
      *---------------------------------------------------------------*

      *    Com os dados de Carteira, Papel e Tranche obtidos, buscar
      *    o valor do PU do Ativo em uma determinada Data de Posicao.

           MOVE 'S'                  TO AX-PSC-EXTT
           MOVE T005-CD-CTRA-TIT-VL  TO 066T-CD-CTRA-TIT-VL
           MOVE AX-DT-TRB-DB2        TO 066T-DT-PSC
           MOVE T005-CD-TIT-OPR-ETRD TO 066T-CD-TIT-OPR-ETRD
           MOVE T005-NR-DVS-TIT      TO 066T-NR-DVS-TIT

           EXEC SQL
               SELECT VL_PU_TIT_DT_PSC, DT_PSC
               INTO :066T-VL-PU-TIT-DT-PSC, :066T-DT-PSC
               FROM DB2SGT.IMP_PSC_DRIA_SDO
               WHERE CD_CTRA_TIT_VL  = :066T-CD-CTRA-TIT-VL
                 AND DT_PSC          = :066T-DT-PSC
                 AND CD_TIT_OPR_ETRD = :066T-CD-TIT-OPR-ETRD
                 AND NR_DVS_TIT      = :066T-NR-DVS-TIT
               ORDER BY NR_IMP_PSC_SDO DESC
               FETCH FIRST 1 ROWS ONLY
           END-EXEC
      * MOCK-POINT MCKV2
           IF SQLCODE NOT EQUAL ZEROS AND +100
               PERFORM 999000-ERRO-07
               PERFORM 990000-ABENDA
           ELSE
               IF SQLCODE EQUAL +100
                   MOVE 'N'                  TO AX-PSC-EXTT
      *        Se nao foi encontrada uma Data de Posicao que existe
      *        na lista de Vendas trazida pelo CAC, isso eh uma
      *        inconsistencia que precisa ser apontada pelo Sistema.
                   MOVE FD502D-NR-UNCO-OPR   TO F033P-NR-UNCO-CTR-OPR
                   MOVE 066T-CD-TIT-OPR-ETRD TO F033P-CD-TIT-OPR-ETRD
                   MOVE 066T-NR-DVS-TIT      TO F033P-NR-DVS-TIT
                   STRING 'Nao foi encontrada posicao de saldo de '
                          066T-DT-PSC
                         DELIMITED BY SIZE INTO F033P-TX-OCR-ERRO
                   END-STRING
                   WRITE SGTF033P-FD FROM SGTF033P-REG-DETALHE
                   ADD +1 TO GDA-GRV-033P
               END-IF
           END-IF

      *    Atualizar Data de Posicao pesquisada na variavel auxiliar
      *    de controle. Em uma eventual proxima leitura de Data de
      *    Venda, caso esta data se repita nao eh necessario consultar
      *    novamente a tabela.
           MOVE DIA-TRB-DB2     TO DIA2-TRB-DB2
           MOVE MES-TRB-DB2     TO MES2-TRB-DB2
           MOVE SEC-ANO-TRB-DB2 TO AX-TMP-SEC-ANO
           MOVE AX-TMP-SEC-ANO  TO SEC2-ANO-TRB-DB2
           .
      *
       220099-SAI-PSQ-PU-PSC-PAPL.
           EXIT.

      *
      *---------------------------------------------------------------*
       980000-GRAVA-SAIDA-715          SECTION.
      *---------------------------------------------------------------*
      *
      *    Registro do Produto 715 (por Operacao)
           MOVE 715          TO FD502D-CD-OBJ
           MOVE 193          TO FD502D-CD-TIP-OCOR
           IF AX-VL-ACM-RC-PRD-715 LESS ZERO
               MOVE '-' TO FD502D-SNL-VL-OGM
           ELSE
               MOVE '+' TO FD502D-SNL-VL-OGM
           END-IF
           MOVE AX-VL-ACM-RC-PRD-715 TO FD502D-VL-OGM
           WRITE SGTF033S-FD              FROM RSTF502-REG-DETALHE
           ADD +1                         TO GDA-GRV-033S
           .
      *
       980000-SAI-GRAVA-SAIDA-715.
           EXIT.
      *---------------------------------------------------------------*
       301000-FINAL                    SECTION.
      *---------------------------------------------------------------*
      *

      *    O processamento chegou ao final; precisamos gravar o
      *    registro trailler no arquivo de saida.

      *    Montar quantidade de registros detalhe no registro trailler
           MOVE GDA-GRV-033S TO QTDE-REG-SIS
           WRITE SGTF033S-FD FROM AX-REG-TRAILLER-RSTK502D

           DISPLAY '999 ' CTE-PROG ' FIM NORMAL     '
           DISPLAY '999 ' CTE-PROG ' LK-DATA-PRCT-AMD = '
                                     LK-DATA-PRCT-AMD
           DISPLAY '999 ' CTE-PROG ' DT-ULT-UTIL-MES-ATUAL-PSQ = '
                                     DT-ULT-UTIL-MES-ATUAL-PSQ
           DISPLAY '999 ' CTE-PROG ' DT-ULT-DD-MES-ATUAL-PSQ = '
                                     DT-ULT-DD-MES-ATUAL-PSQ
           DISPLAY '999 ' CTE-PROG ' DT-PRMO-DD-MES-ATUAL-PSQ = '
                                     DT-PRMO-DD-MES-ATUAL-PSQ

           IF GDA-CNT-033E-02 EQUAL ZEROS
               DISPLAY '999 ' CTE-PROG ' ATENCAO: ARQUIVO SGTF033E VAZIO
      -                                '.'
           END-IF
           DISPLAY '999 ' CTE-PROG ' GDA-CNT-033E-01 = ' GDA-CNT-033E-01
           DISPLAY '999 ' CTE-PROG ' GDA-CNT-033E-02 = ' GDA-CNT-033E-02
           DISPLAY '999 ' CTE-PROG ' GDA-GRV-033S = ' GDA-GRV-033S
           DISPLAY '999 ' CTE-PROG ' GDA-GRV-033P = ' GDA-GRV-033P
      *
           CLOSE SGTF033E SGTF033S SGTF033P
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
           DISPLAY '888 ' CTE-PROG ' 02 - A DATA DE PROCESSAMENTO NAO EH
      -                            ' NUMERICA.'
           .

       999000-ERRO-03.
           DISPLAY '888 ' CTE-PROG ' 03 - ERRO DE ABERTURA DE ARQUIVO.'
           IF FS-033E NOT EQUAL ZEROS
               DISPLAY '888 ' CTE-PROG ' 03 - FILE STATUS SGTF033E = '
                                         FS-033E
           END-IF
           IF FS-033S NOT EQUAL ZEROS
               DISPLAY '888 ' CTE-PROG ' 03 - FILE STATUS SGTF033S = '
                                         FS-033S
           END-IF
           IF FS-033P NOT EQUAL ZEROS
               DISPLAY '888 ' CTE-PROG ' 03 - FILE STATUS SGTF033P = '
                                         FS-033P
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
           DISPLAY '888 ' CTE-PROG ' 05 - ERRO NO CALCULO DA RECEITA PRD
      -                            ' 715 (SIZE ERROR).'
           MOVE AX-ITEM TO AX-DISPLAY-QT-MNTR
           DISPLAY '888 ' CTE-PROG
                        ' AX-ITEM '
                          AX-DISPLAY-QT-MNTR
           MOVE CACS0132-VL-PU-OPR-ITMT(AX-ITEM) TO AX-DISPLAY-VL-MNTR
           DISPLAY '888 ' CTE-PROG
                        ' 05 - CACS0132-VL-PU-OPR-ITMT(AX-ITEM) '
                          AX-DISPLAY-VL-MNTR
           MOVE 066T-VL-PU-TIT-DT-PSC TO AX-DISPLAY-VL-MNTR
           DISPLAY '888 ' CTE-PROG ' 05 - 066T-VL-PU-TIT-DT-PSC = '
                                          AX-DISPLAY-VL-MNTR
           MOVE CACS0132-QT-OPR-ITMT-FNCO(AX-ITEM)
             TO AX-DISPLAY-QT-FRC-MNTR
           DISPLAY '888 ' CTE-PROG
                   ' 05 - CACS0132-QT-OPR-ITMT-FNCO(AX-ITEM) = '
                   AX-DISPLAY-QT-FRC-MNTR
           IF AX-VL-RC-PRD-715 IS NUMERIC
               MOVE AX-VL-RC-PRD-715 TO AX-DISPLAY-VL-MNTR
               DISPLAY '888 ' CTE-PROG ' 05 - AX-VL-RC-PRD-715 = '
                                              AX-DISPLAY-VL-MNTR
           END-IF
      *    Aqui, e apenas aqui, eh feito tambem o desvio para o SBABEND,
      *    porque nao dah para colocah-lo dentro da clausula
      *    ON SIZE ERROR do comando COMPUTE.
           PERFORM 990000-ABENDA

           .
       999000-ERRO-06.
           MOVE SQLCODE                  TO AX-DISPLAY-SQLCODE
           MOVE T005-NR-UNCO-CTR-OPR     TO AX-DISPLAY-NR-UNCO-CTR-OPR
           DISPLAY '888 ' CTE-PROG ' 06 - ERRO NA PESQUISA DE PAPEL E TR
      -                     'ANCHE ATRAVES DO NUMERO UNICO DA OPERACAO.'
           DISPLAY '888 ' CTE-PROG ' 06 - SQLCODE = ' AX-DISPLAY-SQLCODE
           DISPLAY '888 ' CTE-PROG ' 06 - T005-NR-UNCO-CTR-OPR = '
                                          AX-DISPLAY-NR-UNCO-CTR-OPR

           .
       999000-ERRO-07.
           MOVE SQLCODE             TO AX-DISPLAY-SQLCODE
           MOVE 066T-CD-CTRA-TIT-VL TO AX-DISPLAY-CD-CTRA-TIT-VL
           MOVE 066T-NR-DVS-TIT     TO AX-DISPLAY-NR-DVS-TIT
           DISPLAY '888 ' CTE-PROG ' 07 - ERRO EM UMA DAS PESQUISAS DE S
      -                            'ALDO DO PAPEL EM UMA DATA DE POSICAO
      -                            '.'
           DISPLAY '888 ' CTE-PROG ' 07 - SQLCODE = ' AX-DISPLAY-SQLCODE
           DISPLAY '888 ' CTE-PROG ' 07 - 066T-CD-CTRA-TIT-VL = '
                                          AX-DISPLAY-CD-CTRA-TIT-VL
           DISPLAY '888 ' CTE-PROG ' 07 - 066T-DT-PSC = '
                                          066T-DT-PSC
           DISPLAY '888 ' CTE-PROG ' 07 - 066T-CD-TIT-OPR-ETRD = '
                                          066T-CD-TIT-OPR-ETRD
           DISPLAY '888 ' CTE-PROG ' 07 - 066T-NR-DVS-TIT = '
                                          AX-DISPLAY-NR-DVS-TIT

           .
       999000-ERRO-08.
           MOVE CACS0132-NR-UNCO-CTR-OPR
             TO AX-DISPLAY-NR-UNCO-CTR-OPR
           MOVE CACS0132-CD-RTN-PGM           TO AX-CACS0132-CD-RTN-PGM
           MOVE CACS0132-CD-OPR-ITMT-FNCO-ULT
             TO AX-DISPLAY-OPR-ITMT-FNCO-ULT

           DISPLAY '888 ' CTE-PROG ' 08 - ERRO NA CHAMADA AO PROGRAMA CA
      -                 'CS0132.'

           DISPLAY '888 ' CTE-PROG ' 08 - CACS0132-NR-UNCO-CTR-OPR = '
                                          AX-DISPLAY-NR-UNCO-CTR-OPR
           DISPLAY '888 ' CTE-PROG ' 08 - CACS0132-DT-INC-PSQ = '
                                          CACS0132-DT-INC-PSQ
           DISPLAY '888 ' CTE-PROG ' 08 - CACS0132-DT-FIM-PSQ = '
                                          CACS0132-DT-FIM-PSQ
           DISPLAY '888 ' CTE-PROG ' 08 - CACS0132-DT-OPR-ITMT-FNCO-ULT
      -                                   '= '
                                          CACS0132-DT-OPR-ITMT-FNCO-ULT
           DISPLAY '888 ' CTE-PROG ' 08 - CACS0132-CD-OPR-ITMT-FNCO = '
                                          AX-DISPLAY-OPR-ITMT-FNCO-ULT
           DISPLAY '888 ' CTE-PROG ' 08 - CACS0132-CD-RTN-PGM = '
                                          AX-CACS0132-CD-RTN-PGM
           DISPLAY '888 ' CTE-PROG ' 08 - CACS0132-PSC-ERRO-PGM = '
                                          CACS0132-PSC-ERRO-PGM
           DISPLAY '888 ' CTE-PROG ' 08 - CACS0132-MSG-ERRO-PGM = '
                                          CACS0132-MSG-ERRO-PGM

           .

      *---------------------------------------------------------------*
       990000-ABENDA SECTION.
      *---------------------------------------------------------------*
      *
           DISPLAY '888 ' CTE-PROG ' 888-CANCELADO'
           DISPLAY '888 ' CTE-PROG ' GDA-CNT-033E-01 = ' GDA-CNT-033E-01
           DISPLAY '888 ' CTE-PROG ' GDA-CNT-033E-02 = ' GDA-CNT-033E-02
           DISPLAY '888 ' CTE-PROG ' GDA-GRV-033S = ' GDA-GRV-033S
           DISPLAY '888 ' CTE-PROG ' GDA-GRV-033P = ' GDA-GRV-033P
           CLOSE SGTF033E SGTF033S SGTF033P
           CALL SBABEND
           .
      *
      *---------------------------------------------------------------*
      *        S G T P 0 0 3 2  -  F I M  D O  P R O G R A M A        *
      *---------------------------------------------------------------*