       CBL ARITH(EXTEND)
      *----------------------------------------------------------------*
      * Programa..: SGTS009A                                           *
      * Analista..: Edilene Akemi Horita.                              *
      * Autor.....: Edilene Akemi Horita.                              *
      * Data......: 22.03.2019                                         *
      * Objetivo..: Unifica a hora de uma data de importação na tabela *
      *             de saldo da operação das operações encerradas.     *
      * Compilacao: COBOL/CICS                                         *
      *----------------------------------------------------------------*
      * VRS003 16/01/2025 F9158937 Acerto no Select principal para sele-
      *        cionar qq número sequencial e Inclusão de book IIB.
      * VRS002 30/01/2020 F2634012  DE 2016/145195 - Inclusao do campo *
      *                   codigo da carteira
      * VRS001 22/03/2019 F2634012  DE 2016/145195 - Implantação       *
      *----------------------------------------------------------------*
       IDENTIFICATION                  DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID. SGTS009A.

      *----------------------------------------------------------------*
       ENVIRONMENT                     DIVISION.
      *----------------------------------------------------------------*
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *----------------------------------------------------------------*
       DATA                            DIVISION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *--------------------> Variaveis do Sistema <--------------------*
       77  CTE-INICIO-SS               PIC  X(35)  VALUE
           '*** Storage Section Comeca Aqui ***'.
       77  CTE-PROG                    PIC  X(17)  VALUE
           '*** SGTS009A ***'.
       77  CTE-VERS                    PIC  X(06)  VALUE 'VRS003'.

       77  SGTST004                    PIC  X(08)  VALUE 'SGTST004'.

      *--> Book do programa de ambiente - CICS5000
      *    Definido na work, para preservar as informacoes
-INC HLPKE01B

      *----------------------------------------------------------------*
       LOCAL-STORAGE SECTION.
      *----------------------------------------------------------------*

      *--> Book SGTST004 - CRUD  PSC_DRIA_SDO_OPR
       01  SGTST004-DADOS.
-INC SGTKT004

      *--> Book definição dados a serem usados na subrotina de erros
-INC HLPKE001

      * Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1

      *--> Book do container de Erro (IIB)
-INC IIBKERRO

      *------------------- Variaveis do Sistema -----------------------*

       01  WS-CICS.
           03  WS-RESP1                PIC  9(008).
           03  WS-RESP2                PIC  9(008).

-INC MSGKWMG1

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
           03  MSGKW999-NM-VRV-MNT          PIC  X(030) VALUE ' '.
           03  MSGKW999-VL-VRV-MNT          PIC  X(100) VALUE ' '.

       01 AUX-DT-IMP-PSC-SDO                PIC X(10) VALUE
                                                      '00.00.0000'.
       01 AUX-DT-IMP-PSC-SDO-R REDEFINES AUX-DT-IMP-PSC-SDO.
          03 DIA               PIC 9(02).
          03 FILLER            PIC X(01).
          03 MES               PIC 9(02).
          03 FILLER            PIC X(01).
          03 ANO               PIC 9(04).

       01 AUX-HR-IMP-PSC-SDO                 PIC X(08) VALUE '00:00:00'.
       01 AUX-HR-IMP-PSC-SDO-R REDEFINES AUX-HR-IMP-PSC-SDO.
          03 HORA              PIC 9(02).
          03 FILLER            PIC X(01).
          03 MINUTO            PIC 9(02).
          03 FILLER            PIC X(01).
          03 SEGUNDO           PIC 9(02).

       01 IA-1                 PIC S9(4) COMP-5 VALUE 0.
       01 QTD-REG-SEL          PIC S9(4) COMP-5 VALUE 0.
       01 AUX-SALDO            PIC S9(15)V9(02) COMP-3 VALUE 0.
       01 MCR-S9-15-V9-02      PIC  ----.---.---.---.--9,99.

       01  CTL-CURSOR-PSQ.
           03  CD-FIM-CURSOR-PSQ            PIC S9(04) COMP-5 VALUE 0.
               88  FIM-CURSOR-PSQ           VALUE +100.
           03  IN-CURSOR-PSQ-ABTO           PIC  X(01) VALUE 'N'.
               88  CURSOR-PSQ-ABTO          VALUE 'S'.

000550 01  REG-WS.
000560     03 CD-TIT-OPR-ETRD                   PIC  X(12)
000570        OCCURS 101 TIMES                  VALUE ' '.
000580     03 CD-TIT-OPR-ETRD-N                 PIC S9(4) COMP-5
000590        OCCURS 101 TIMES                  VALUE 0.
000600     03 NR-DVS-TIT                        PIC S9(04) COMP-5
000610        OCCURS 101 TIMES                  VALUE 0.
000620     03 NR-DVS-TIT-N                      PIC S9(4) COMP-5
000630        OCCURS 101 TIMES                  VALUE 0.
000620     03 CD-CTRA-TIT-VL                    PIC S9(9) COMP-5
000630        OCCURS 101 TIMES                  VALUE 0.
000720     03 DT-PSC                            PIC  X(10)
000730        OCCURS 101 TIMES                  VALUE ' '.
000760     03 NR-SEQL-PSC-DRIA                  PIC S9(04) COMP-5
000770        OCCURS 101 TIMES                  VALUE 0.
000780     03 NR-SEQL-PSC-DRIA-N                PIC S9(04) COMP-5
000790        OCCURS 101 TIMES                  VALUE 0.

001180     EXEC SQL
001190         INCLUDE SQLCA
001200     END-EXEC.

       01  CTE-SAI-SS                  PIC  X(040) VALUE
           '*** Storage Section Termina Aqui ***'.

      *-----------------------------------------------------------------
       LINKAGE SECTION.
      *-----------------------------------------------------------------
       01  DFHCOMMAREA.
-INC SGTK009A

003020      EXEC SQL
003030        DECLARE CURSOR-PSQ-1 CURSOR
003040            WITH ROWSET POSITIONING FOR
              SELECT
                   A.CD_CTRA_TIT_VL,
                   A.DT_PSC,
                   A.CD_TIT_OPR_ETRD,
                   A.NR_DVS_TIT,
                   A.NR_SEQL_PSC_DRIA

              FROM DB2SGT.PSC_DRIA_SDO_OPR A
              INNER JOIN
                   (SELECT CD_CTRA_TIT_VL, DT_PSC, CD_TIT_OPR_ETRD,
                   NR_DVS_TIT, MAX(NR_SEQL_PSC_DRIA) AS MAX_NR_SEQ
                   FROM DB2SGT.PSC_DRIA_SDO_OPR
               GROUP BY
                 CD_CTRA_TIT_VL, DT_PSC, CD_TIT_OPR_ETRD, NR_DVS_TIT) B

               ON
                   A.CD_CTRA_TIT_VL   = B.CD_CTRA_TIT_VL
               AND A.DT_PSC           = B.DT_PSC
               AND A.CD_TIT_OPR_ETRD  = B.CD_TIT_OPR_ETRD
               AND A.NR_DVS_TIT       = B.NR_DVS_TIT
      *        AND A.NR_SEQL_PSC_DRIA = B.MAX_NR_SEQ

               WHERE
                    A.CD_CTRA_TIT_VL    = :SGTS009A-CD-CTRA-TIT-VL
               AND  A.DT_PSC            < CURRENT DATE
               AND  A.VL_SDO_TIT_DT_PSC = :AUX-SALDO
               AND  A.DT_IMP_PSC_SDO    = :AUX-DT-IMP-PSC-SDO

003150      END-EXEC.

      *----------------------------------------------------------------*
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *----------------------------------------------------------------*

       000000-PRINCIPAL                SECTION.

      * teste obrigatorio do tamanho da linkage nao retirar
           IF  EIBCALEN NOT EQUAL LENGTH OF DFHCOMMAREA
               MOVE  0220              TO  CD-RTN-PGM
               MOVE  'Área com tamanho inválido.'
                                       TO  MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           IF  NOT EXECUCAO-ONLINE     AND NOT EXECUCAO-BATCH
               CALL  CICS5000 USING CICS5000-COMMAREA
           END-IF

           MOVE  0001                  TO  PSC-ERRO-PGM


      * WHENEVER CICS   **********
-INC HLPKE003


           INITIALIZE  SGTS009A-RTN-PGM

           PERFORM 100000-VALIDAR-PARAMETRO

           PERFORM 200000-PROCESSAR

           .
       000000-SAI.
           GOBACK.

      *----------------------------------------------------------------*
       100000-VALIDAR-PARAMETRO        SECTION.
      *--> Valida os campos de entrada do programa.
      *----------------------------------------------------------------*
           MOVE  0002                  TO  PSC-ERRO-PGM

           IF  SGTS009A-CD-USU-RSP-ALT  EQUAL SPACES
               MOVE 0278                TO  CD-RTN-PGM
               MOVE 'Código do usuário responsável pela atualização não
      -             'informado.'        TO  MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           IF  SGTS009A-DT-IMP-PSC-SDO  EQUAL SPACES
               MOVE 0738                TO  CD-RTN-PGM
               MOVE 'Data da importacao invalida' TO MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           IF  SGTS009A-HR-IMP-PSC-SDO   EQUAL SPACES
               MOVE 0737                TO  CD-RTN-PGM
               MOVE 'Hora da importacao invalida' TO MSG-ERRO
002700         PERFORM 999001-ERRO-001
           END-IF

           MOVE CORR SGTS009A-DT-IMP-PSC-SDO-R
                                        TO  AUX-DT-IMP-PSC-SDO-R
           MOVE CORR SGTS009A-HR-IMP-PSC-SDO-R
                                        TO  AUX-HR-IMP-PSC-SDO-R
           IF  SGTS009A-CD-CTRA-TIT-VL  EQUAL ZEROES
               MOVE 0002                TO CD-RTN-PGM
               MOVE 'Código da carteira de título e valores mobiliários
      -        'não informado.'         TO MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           .

       100000-SAI.
           EXIT.

      *----------------------------------- ----------------------------*
       200000-PROCESSAR                SECTION.
      *----------------------------------------------------------------*
           MOVE  0003                  TO  PSC-ERRO-PGM

           PERFORM 210000-SEL-ENCERRADAS
           IF      QTD-REG-SEL    GREATER  100
                   MOVE 0800      TO       CD-RTN-PGM
                   MOVE 'Indexador de ocorrências da tabela é maior que
      -            'o limite.'    TO       MSG-ERRO
                   PERFORM 999001-ERRO-001
           END-IF
           IF      QTD-REG-SEL     GREATER  ZEROS
                   PERFORM 220000-ALTERAR
                           VARYING IA-1 FROM 1 BY 1
                           UNTIL IA-1 > QTD-REG-SEL
           END-IF

           .

       200000-SAI.
           EXIT.

      *----------------------------------- ----------------------------*
       210000-SEL-ENCERRADAS            SECTION.
      *----------------------------------------------------------------*
           MOVE  0004                  TO  PSC-ERRO-PGM

           PERFORM 940101-ABRE-CURSOR-PSQ-1
           PERFORM 950101-LER-CURSOR-PSQ-1
           PERFORM 960101-FECHA-CURSOR-PSQ-1
           .

       210000-SAI.
           EXIT.

      *----------------------------------- ----------------------------*
       220000-ALTERAR                 SECTION.
      *----------------------------------------------------------------*
           MOVE  0005                   TO  PSC-ERRO-PGM

           SET   SGTST004-CONSULTAR     TO  TRUE
           MOVE  SGTS009A-CD-USU-RSP-ALT
                                        TO  SGTST004-CD-CHV-USU
           MOVE  CD-CTRA-TIT-VL(IA-1)   TO  SGTST004-CD-CTRA-TIT-VL
           MOVE  DT-PSC(IA-1)           TO  SGTST004-DT-PSC
           MOVE  CD-TIT-OPR-ETRD(IA-1)  TO  SGTST004-CD-TIT-OPR-ETRD
           MOVE  NR-DVS-TIT(IA-1)       TO  SGTST004-NR-DVS-TIT
           MOVE  NR-SEQL-PSC-DRIA(IA-1) TO  SGTST004-NR-SEQL-PSC-DRIA

           PERFORM 910001-CHAMAR-SGTST004

           SET   SGTST004-ALTERAR       TO  TRUE
           MOVE  AUX-HR-IMP-PSC-SDO     TO  SGTST004-HR-IMP-PSC-SDO
           MOVE  SGTS009A-CD-USU-RSP-ALT
                                        TO  SGTST004-CD-USU-RSP-ATL

           PERFORM 910001-CHAMAR-SGTST004
           .

       220000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       910001-CHAMAR-SGTST004          SECTION.
      *----------------------------------------------------------------*
           MOVE  0006                  TO  PSC-ERRO-PGM

           IF EXECUCAO-ONLINE
              EXEC CICS LINK
                        PROGRAM ( SGTST004       )
                        COMMAREA( SGTST004-DADOS )
                        RESP    ( WS-RESP1)
                        RESP2   ( WS-RESP2)
              END-EXEC
              IF  WS-RESP1 NOT EQUAL  DFHRESP(NORMAL)
                  MOVE  3004           TO  CD-RTN-PGM
                  MOVE  'Erro ao acionar o módulo.'
                                       TO  MSG-ERRO
                  PERFORM 999001-ERRO-001
              END-IF
           ELSE
              MOVE LENGTH OF SGTST004-DADOS
                                       TO EIBCALEN
              CALL SGTST004 USING DFHEIBLK SGTST004-DADOS
           END-IF

           IF SGTST004-CD-RTN-PGM      NOT EQUAL ZEROS
               MOVE SGTST004-RTN-PGM   TO RTN-PGM
               PERFORM 999001-ERRO-001
           END-IF
           .

       910001-SAI.
           EXIT.

      *-----------------------------------------------------------------
       940101-ABRE-CURSOR-PSQ-1       SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO PSC-ERRO-PGM
           EXEC SQL
                OPEN CURSOR-PSQ-1
           END-EXEC
           MOVE 'S'                    TO IN-CURSOR-PSQ-ABTO
           .
       940101-SAI.
           EXIT.
      *-----------------------------------------------------------------
       950101-LER-CURSOR-PSQ-1      SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO PSC-ERRO-PGM
           INITIALIZE REG-WS
           EXEC SQL
                FETCH NEXT ROWSET
                CURSOR-PSQ-1 FOR 0101 ROWS
               INTO   :CD-CTRA-TIT-VL
                    , :DT-PSC
                    , :CD-TIT-OPR-ETRD
                      :CD-TIT-OPR-ETRD-N
                    , :NR-DVS-TIT
                      :NR-DVS-TIT-N
                    , :NR-SEQL-PSC-DRIA
           END-EXEC
           MOVE SQLERRD(3)             TO  QTD-REG-SEL
      *     IF  QTD-REG-SEL            EQUAL 0101
      *         SET SGTSLJ02-TEM-MAIS-REGISTRO TO TRUE
      *         MOVE 0100                TO QT-REG-SELD
      *     END-IF
           MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ
           .
       950101-SAI.
           EXIT.
      *-----------------------------------------------------------------
       960101-FECHA-CURSOR-PSQ-1 SECTION.
      *-----------------------------------------------------------------
           MOVE 0009                   TO PSC-ERRO-PGM
           MOVE 'N'                    TO IN-CURSOR-PSQ-ABTO
           EXEC SQL
                CLOSE CURSOR-PSQ-1
           END-EXEC
           .
       960101-SAI.
           EXIT.

      *----------------------------------------------------------------*
      * 998000-GRAVA-APPL-ERRO         SECTION.                        *
      * 999082-RECUPERA-MSG-CODIFICADA SECTION.                        *
      * 999001-ERRO-001                SECTION.                        *
      *----------------------------------------------------------------*
-INC HLPKE004
           PERFORM  999999-ENCERRAR
           .
       999001-SAI.
           EXIT.

      *----------------------------------------------------------------*
      * 999003-TRATA-ABEND-CICS        SECTION.                        *
      * 999004-TRATA-CONDITION-CICS    SECTION.                        *
      * 999005-CMPT-ERRO-CICS          SECTION.                        *
      *----------------------------------------------------------------*
-INC HLPKE006

      *-----------------------------------------------------------------
      *    As Sections e Books abaixo fazem o tratamento de erro
      *    através do Sistema MSG (rotina MSGS0001)
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       999000-TRATAR-ERRO-MSG          SECTION.
      *--> Envia a Ocorrência de Erro para o Sistema de Mensageria
      *-----------------------------------------------------------------

      *--> Alterar o MOVE conforme o Book do módulo
           MOVE  SGTS009A-TIP-RTN-PGM  TO  TIP-RTN-PGM
      *--> Alterar os Campos de Entrada conforme o Book do módulo
           PERFORM  999100-MONITORAR-ENTRADA

      *--> Movimenta Campos Básicos da Ocorrência de Erro
           MOVE  CD-RTN-PGM            TO  MSG1-CD-ERRO-TS
           MOVE  PSC-ERRO-PGM          TO  MSG1-CD-SEQL-ERRO
           MOVE  MSG-ERRO              TO  MSG1-TX-ERRO-TS

      *--> Realiza a tradução do tipo de erro do COMMAREA
      *    para tipos de erro da Mensageria
           EVALUATE  TRUE
               WHEN  ERRO-DE-CICS
                     PERFORM  999300-MONITORAR-CICS
               WHEN  ERRO-DE-PGM
                     SET  ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
                     MOVE CD-RTN-PGM   TO  MSG1-CD-RTN-PGM-ACND
                     MOVE MSG-ERRO     TO  MSG1-TX-RTN-PGM-ACND
                     MOVE PGM-ERRO     TO  MSG1-NM-PGM-ACND
                     MOVE PSC-ERRO-PGM TO  MSG1-CD-SEQL-PGM-ACND
                     PERFORM  999400-MONITORAR-VARIAVEIS
               WHEN  ERRO-CODIFICADO
                     SET  ERRO-PROGRAMADO
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

           MOVE 'SGTS009A-CD-USU-RSP-ALT '
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS009A-CD-USU-RSP-ALT
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTS009A-DT-IMP-PSC-SDO'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS009A-DT-IMP-PSC-SDO
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTS009A-HR-IMP-PSC-SDO'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS009A-HR-IMP-PSC-SDO
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTS009A-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS009A-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           .
       999100-SAI.
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
       999400-MONITORAR-VARIAVEIS       SECTION.
      *--> Monitora as variáveis dos módulos chamados
      *-----------------------------------------------------------------

           EVALUATE  PGM-ERRO
               WHEN  SGTST004
                    MOVE 'SGTST004-CD-CHV-USU'   TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST004-CD-CHV-USU     TO MSGKW999-VL-VRV-MNT
                    PERFORM 999500-ADICIONA-VRV-MNT

                    MOVE 'SGTST004-ACAO'         TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST004-ACAO           TO MSGKW999-VL-VRV-MNT
                    PERFORM 999500-ADICIONA-VRV-MNT

000450              MOVE 'SGTST004-CD-CTRA-TIT-VL'
000450                                           TO MSGKW999-NM-VRV-MNT
000450              MOVE SGTST004-CD-CTRA-TIT-VL TO MSGKW999-VL-VRV-MNT
003920              PERFORM 999500-ADICIONA-VRV-MNT

                    MOVE 'SGTST004-DT-PSC'       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST004-DT-PSC         TO MSGKW999-VL-VRV-MNT
                    PERFORM 999500-ADICIONA-VRV-MNT

000450              MOVE 'SGTST004-CD-TIT-OPR-ETRD'
003890                                           TO MSGKW999-NM-VRV-MNT
000450              MOVE SGTST004-CD-TIT-OPR-ETRD
000450                                           TO MSGKW999-VL-VRV-MNT
003920              PERFORM 999500-ADICIONA-VRV-MNT
000450
000450              MOVE 'SGTST004-NR-DVS-TIT'   TO MSGKW999-NM-VRV-MNT
000450              MOVE SGTST004-NR-DVS-TIT     TO MSGKW999-VL-VRV-MNT
003920              PERFORM 999500-ADICIONA-VRV-MNT
000450
000450              MOVE 'SGTST004-NR-SEQL-PSC-DRIA'
000450                                           TO MSGKW999-NM-VRV-MNT
000450              MOVE SGTST004-NR-SEQL-PSC-DRIA
000450                                           TO MSGKW999-VL-VRV-MNT
003920              PERFORM 999500-ADICIONA-VRV-MNT
000450
000450              MOVE 'SGTST004-TS-ULT-ATL'   TO MSGKW999-NM-VRV-MNT
000450              MOVE SGTST004-TS-ULT-ATL     TO MSGKW999-VL-VRV-MNT
003920              PERFORM 999500-ADICIONA-VRV-MNT
000450
000450              MOVE 'SGTST004-CD-USU-RSP-ATL'
000450                                           TO MSGKW999-NM-VRV-MNT
000450              MOVE SGTST004-CD-USU-RSP-ATL TO MSGKW999-VL-VRV-MNT
003920              PERFORM 999500-ADICIONA-VRV-MNT
000450
000450              MOVE 'SGTST004-QT-PAPL-TIT-DT-PSC'
003890                                           TO MSGKW999-NM-VRV-MNT
000450              MOVE SGTST004-QT-PAPL-TIT-DT-PSC
000450                                           TO MSGKW999-VL-VRV-MNT
003920              PERFORM 999500-ADICIONA-VRV-MNT
000450
000450              MOVE 'SGTST004-VL-PU-TIT-DT-PSC'
                                                 TO MSGKW999-NM-VRV-MNT
000450              MOVE SGTST004-VL-PU-TIT-DT-PSC
                                                 TO MCR-S9-15-V9-02
                    MOVE MCR-S9-15-V9-02         TO MSGKW999-VL-VRV-MNT
003920              PERFORM 999500-ADICIONA-VRV-MNT
000450
000450              MOVE 'SGTST004-VL-SDO-TIT-DT-PSC'
003890                                           TO MSGKW999-NM-VRV-MNT
000450              MOVE SGTST004-VL-SDO-TIT-DT-PSC
                                                 TO MCR-S9-15-V9-02
003910              MOVE MCR-S9-15-V9-02         TO MSGKW999-VL-VRV-MNT
003920              PERFORM 999500-ADICIONA-VRV-MNT
000450
000450              MOVE 'SGTST004-CD-EST-RSTD-ITE'
                                                 TO MSGKW999-NM-VRV-MNT
000450              MOVE SGTST004-CD-EST-RSTD-ITE
                                                 TO MSGKW999-VL-VRV-MNT
003920              PERFORM 999500-ADICIONA-VRV-MNT
000450
000450              MOVE 'SGTST004-DT-APL-TIT'   TO MSGKW999-NM-VRV-MNT
000450              MOVE SGTST004-DT-APL-TIT     TO MSGKW999-VL-VRV-MNT
003920              PERFORM 999500-ADICIONA-VRV-MNT
000450
000450              MOVE 'SGTST004-DT-IMP-PSC-SDO'
003890                                           TO MSGKW999-NM-VRV-MNT
000450              MOVE SGTST004-DT-IMP-PSC-SDO
003910                                           TO MSGKW999-VL-VRV-MNT
003920              PERFORM 999500-ADICIONA-VRV-MNT
000450
000450              MOVE 'SGTST004-HR-IMP-PSC-SDO '
000450                                           TO MSGKW999-NM-VRV-MNT
000450              MOVE SGTST004-HR-IMP-PSC-SDO
000450                                           TO MSGKW999-VL-VRV-MNT
003920              PERFORM 999500-ADICIONA-VRV-MNT
000450
000450              MOVE 'SGTST004-NR-IMP-PSC-SDO'
003890                                           TO MSGKW999-NM-VRV-MNT
000450              MOVE SGTST004-NR-IMP-PSC-SDO
003910                                           TO MSGKW999-VL-VRV-MNT
003920              PERFORM 999500-ADICIONA-VRV-MNT

               WHEN  OTHER
                    CONTINUE
           END-EVALUATE
           .
       999400-SAI.
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
       999999-ENCERRAR                 SECTION.
      *-----------------------------------------------------------------
           MOVE  RTN-PGM               TO  SGTS009A-RTN-PGM

           IF  EXECUCAO-ONLINE
               PERFORM  999000-TRATAR-ERRO-MSG
           END-IF

           GOBACK
           .
       999999-SAI.
           EXIT.
      *------------------------> FIM SGTS009A <------------------------*