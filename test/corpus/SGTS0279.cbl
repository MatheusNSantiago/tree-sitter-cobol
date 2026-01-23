      *-----------------------------------------------------------------
      * Programa..: SGTS0279
      * Analista..: F8785286 - Roseli Akemi Kawahata
      * Autor.....: F8785286 - Roseli Akemi Kawahata
      * Data......: 26/09/2019
      * Objetivo..: A partir dos dados de entrada efetua a alteração do
      *             vínculo do Modelo de Ordem de Venda à Oper Estrut.
      * Compilacao: Cobol/CICS
      *-----------------------------------------------------------------
      * VRS001 06/12/2019 - F0950413 - DE 2016/145195 - Alterar a forma-
      *        tação da mensagem 1069 e monitorar variável TX-DTQ-MSG
      *        26/09/2019 - F8785286 - DE 2016/145195 - Implantação
      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTS0279.
      *-----------------------------------------------------------------
       ENVIRONMENT                     DIVISION.
      *-----------------------------------------------------------------
       CONFIGURATION                   SECTION.
      *-----------------------------------------------------------------
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
                                       '*** SGTS0279 ***'.
       77  CTE-VERS                    PIC  X(06) VALUE 'VRS002'.

      *--> Definição dos Nomes dos Programas Acionados
       77  IIBP2000                    PIC  X(08) VALUE 'IIBP2000'.
       77  SGTSA075                    PIC  X(08) VALUE 'SGTSA075'.
       77  SGTSI078                    PIC  X(08) VALUE 'SGTSI078'.
       77  SGTSL069                    PIC  X(08) VALUE 'SGTSL069'.
       77  SGTSL078                    PIC  X(08) VALUE 'SGTSL078'.
       77  SGTSLA75                    PIC  X(08) VALUE 'SGTSLA75'.
       77  SGTST005                    PIC  X(08) VALUE 'SGTST005'.
       77  SGTST009                    PIC  X(08) VALUE 'SGTST009'.

       77  SBDATAF                     PIC  X(08) VALUE 'SBDATAF'.

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

      *--> Area de trabalho

      *--> Auxiliares para movimentação na monitoração de variáveis.
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP-DISP    PIC  9(11).

       01  VL-PU-PAPL-VND-TX.
           03  VL-PU-PAPL-VND-DISP     PIC  -ZZ.ZZZ.ZZZ.ZZZ.ZZ9,99.

       01  VL-TAXA-NEGO-VND-TX.
           03  VL-TAXA-NEGO-VND-DISP   PIC  -ZZ.ZZZ.ZZZ.ZZZ.ZZ9,99.

       01  AUX-EST-ATI                 PIC  X(01).
       01  AUX-ACHOU-MOD               PIC  X(01).
       01  TX-DTQ-MSG                  PIC  X(09).

      *--> Auxiliares para leitura da tabela MOD_ORD_NEGO com
      *-->   EST_MOD_ORD_NEGO
       01  DADOS-ATL.
           03 NR-VRS-OPR-ETRD          PIC S9(4) COMP-5
                                       OCCURS 0101 TIMES  VALUE ZEROS.
           03 NR-SEQL-ORD-NEGO         PIC S9(9) COMP-5
                                       OCCURS 0101 TIMES  VALUE ZEROS.
           03 CD-USU-ULT-ATL           PIC X(8)
                                       OCCURS 0101 TIMES  VALUE SPACES.
           03 TS-ULT-ATL               PIC X(26)
                                       OCCURS 0101 TIMES  VALUE SPACES.
           03 CD-ARQ-OPR-ETRD          PIC S9(9) COMP-5
                                       OCCURS 0101 TIMES  VALUE ZEROS.
           03 DT-EMS-ORD-NEGO          PIC X(10)
                                       OCCURS 0101 TIMES  VALUE SPACES.
           03 CD-MOD-ORD-NEGO          PIC S9(9) COMP-5
                                       OCCURS 0101 TIMES  VALUE ZEROS.
           03 CD-TIP-NEGO              PIC S9(4) COMP-5
                                       OCCURS 0101 TIMES  VALUE ZEROS.
           03 CD-PRD                   PIC S9(4) COMP-5
                                       OCCURS 0101 TIMES  VALUE ZEROS.
           03 CD-MDLD                  PIC S9(4) COMP-5
                                       OCCURS 0101 TIMES  VALUE ZEROS.
           03 IN-MOD-ORD-ATI           PIC X(1)
                                       OCCURS 0101 TIMES  VALUE SPACES.
           03 CD-TIP-EST-NEGO          PIC S9(9) COMP-5
                                       OCCURS 0101 TIMES  VALUE ZEROS.
           03 CD-TIP-EST-DCS           PIC S9(4) COMP-5
                                       OCCURS 0101 TIMES  VALUE ZEROS.
           03 TS-EST-ORD-NEGO          PIC X(26)
                                       OCCURS 0101 TIMES  VALUE SPACES.
           03 TS-EST-MOD-ORD           PIC X(26)
                                       OCCURS 0101 TIMES  VALUE SPACES.

      *--> Indicadores de nulidades
           03  CD-PRD-N                PIC S9(04)     COMP-5
                                       OCCURS 0101 TIMES  VALUE ZEROS.
           03  CD-MDLD-N               PIC S9(04)     COMP-5
                                       OCCURS 0101 TIMES  VALUE ZEROS.
           03  CD-ARQ-OPR-ETRD-N       PIC S9(04)     COMP-5
                                       OCCURS 0101 TIMES  VALUE ZEROS.
           03  DT-EMS-ORD-NEGO-N       PIC S9(04)     COMP-5
                                       OCCURS 0101 TIMES  VALUE ZEROS.

      *----------------------------------------------------------------*
      *                 AREA DA SUBROTINA SBDATAF
      *----------------------------------------------------------------*
      *    Campos utilizados para validação de data Gregoriana.
       01  SBDATAF-FUNCAO-01           PIC  X(003) VALUE 'F01'.
       01  SBDATAF-ARG01-01            PIC  9(008) VALUE 000.
       01  SBDATAF-ARG01-01-R          REDEFINES  SBDATAF-ARG01-01.
           03  DD                      PIC  9(002).
           03  MM                      PIC  9(002).
           03  AA                      PIC  9(004).
       01  SBDATAF-ARG02-01            PIC  9(001) VALUE 004.

       01  AREAWORK                    PIC  X(4095) VALUE SPACES.
       01  AREA-RED REDEFINES AREAWORK.
           03  A-TAM    PIC 9(0008) COMP.
           03  FILLER   PIC X(4071).
           03  A-FIM    PIC X(0020).

      * Variáveis para gravação do log
       01  DADOS-LOG-DISPLAY.
           03  CD-CTRA-TIT-VL-LOG      PIC S9(09)       VALUE 0.
           03  CD-OPR-ETRD-MCP-LOG     PIC S9(11)       VALUE 0.
           03  NR-VRS-OPR-ETRD-LOG     PIC S9(04)       VALUE 0.
           03  NR-SEQL-ORD-NEGO-LOG    PIC S9(09)       VALUE 0.
      *-----------------------------------------------------------------
      *    Books Utilizados
      *-----------------------------------------------------------------
      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informacoes
-INC HLPKE01B

      *--> Book dos dados do usuário
-INC IIBKOPRD

      *--> Book do Container de Erro
-INC IIBKERRO

      *--> Book para captura de abend cics da bloco codigo HLPKW999
-INC HLPKW999

      *--> Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1

      *--> Book para captura de erro via MSG1
-INC MSGKWMG1

      *--> Book do container de requisição e resposta deste modulo
       01 SGTS0279-DADOS.
-INC SGTK0279

      *-----------------------------------------------------------------
      * Book dos Containers e das subrotinas chamadas
      *-----------------------------------------------------------------

      * Inclusão na Tabela: DB2SGT.EST_ORD_NEGO_OPR
       01  SGTSI078-DADOS.
-INC SGTKI078

      * Alteração da tabela: DB2SGT.ORD_NEGO_OPR
-INC SGTKA075

      * Lista versão aprovada - DB2SGT.EST_VRS_OPR_ETRD
       01  SGTSL069-DADOS.
-INC SGTKL069

      * Lista estado da Ordem de Negociação - DB2SGT.EST_ORD_NEGO_OPR
       01  SGTSL078-DADOS.
-INC SGTKL078

      * Lista da tabela DB2SGT.ORD_NEGO_OPR
-INC SGTKLA75

      * Book SGT -  Lista da tabela DB2SGT.OPR_ETRD_MCP
       01  SGTST005-DADOS.
-INC SGTKT005

      * Verificar em qual estado está a operação -  SGTST009
       01 SGTST009-DADOS.
-INC SGTKT009

      *--> Area de trabalho SQL/ DB2.
           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

       77  CTE-FINAL-SS                PIC X(40) VALUE
                                       '*** S.S. Termina Aqui ***'.
      *-----------------------------------------------------------------
       PROCEDURE                       DIVISION.
      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------

      *--> Habilita tratamento automatico de Erros DB2
-INC HLPKE997
      *--> Habilita captura automatica de Abend e Erros CICS
-INC HLPKE999

           PERFORM  100000-INICIAR

           PERFORM  974000-LER-DADOS-OPRD

           PERFORM  200000-PROCESSAR

           PERFORM  999999-ENCERRAR

           GOBACK
           .
       000000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       100000-INICIAR                  SECTION.
      *-----------------------------------------------------------------
           MOVE  0001                  TO  MSG1-CD-SEQL-ERRO

           PERFORM  970000-LER-CANAL

      *--> Define o nome do programa p/ fins manipulacao container
           MOVE  'SGTS0279'            TO  NM-PGM

      *--> Limpa a resposta e erro antes de executar o modulo
           PERFORM  983000-EXCLUIR-RESPOSTA-ERRO

           PERFORM  971000-LER-REQUISICAO

           PERFORM  974000-LER-DADOS-OPRD

           PERFORM  110000-VALIDAR-REQUISICAO
           .
       100000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       110000-VALIDAR-REQUISICAO       SECTION.
      *--> Valida campo a campo da requisicao
      *-----------------------------------------------------------------
           MOVE  0002                  TO  MSG1-CD-SEQL-ERRO

      *--> Validar Chave da Operação Estruturada
           IF  SGTS0279-CD-CTRA-TVM        LESS  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0001              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' inválido.'      TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0279-CD-CTRA-TVM        EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0002              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0279-CD-OPR-ETRD-MCP    NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0003              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada do mercado de capit
      -              'ais não numérico.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0279-CD-OPR-ETRD-MCP    LESS  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0004              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada do mercado de capit
      -              'ais inválido.'   TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0279-CD-OPR-ETRD-MCP    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0005              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada do mercado de capit
      -              'ais não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0279-NR-VRS-OPR-ETRD    LESS  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0006              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação estruturada inválido.
      -              ''                TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0279-NR-VRS-OPR-ETRD    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0007              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação estruturada não infor
      -              'mado.'           TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0279-NR-SEQL-ORD-NEGO   EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0542              TO  MSG1-CD-ERRO-TS
               MOVE  'Número sequencial da ordem da negociação não infor
      -              'mado.'           TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0279-CD-MOD-ORD-NEGO    LESS ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0641              TO  MSG1-CD-ERRO-TS
               MOVE  'Código do modelo de ordem de negociação não inform
      -              'ado.'            TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       110000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       200000-PROCESSAR                SECTION.
      *-----------------------------------------------------------------
           MOVE  0003                  TO  MSG1-CD-SEQL-ERRO

           PERFORM  210000-CONSULTAR-OPR-ETRD
           PERFORM  220000-VERIFICAR-EST-OPR
           MOVE 'N'                    TO  AUX-ACHOU-MOD
           PERFORM  230000-VERIFICAR-ORD-NEGO
           MOVE 'N'                    TO  AUX-EST-ATI
           PERFORM  250000-ATUALIZAR-ORD-NEGO
           .
       200000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       210000-CONSULTAR-OPR-ETRD       SECTION.
      *    Consulta dados da operação estruturada.
      *-----------------------------------------------------------------
           MOVE  0004                  TO  MSG1-CD-SEQL-ERRO

           MOVE SGTST005                   TO  NM-PGM
           SET  SGTST005-CONSULTAR         TO  TRUE
           MOVE SGTS0279-CD-CTRA-TVM       TO  SGTST005-CD-CTRA-TIT-VL
           MOVE SGTS0279-CD-OPR-ETRD-MCP   TO  SGTST005-CD-OPR-ETRD-MCP
           MOVE SGTS0279-NR-VRS-OPR-ETRD   TO  SGTST005-NR-VRS-OPR-ETRD

           PERFORM 920000-CHAMAR-COMMAREA

           IF  SGTST005-CD-RTN-PGM  NOT EQUAL  ZEROES
               SET ERRO-PROGRAMA-ACIONADO TO TRUE
               MOVE NM-PGM             TO MSG1-NM-PGM-ACND
               MOVE SGTST005-MSG-ERRO-PGM
                                       TO MSG1-TX-RTN-PGM-ACND
                                          MSG1-TX-ERRO-TS
               MOVE SGTST005-CD-RTN-PGM
                                       TO MSG1-CD-RTN-PGM-ACND
                                          MSG1-CD-ERRO-TS

      *        Monitorar variáveis do mdulo chamado
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-ERRO
           END-IF
           .
       210000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       220000-VERIFICAR-EST-OPR        SECTION.
      *    Consulta estado atual da operação estruturada.
      *    Caso o estado não seja encarteirado/aprovado, não pode emitir
      *    ordem de venda para a operação estruturada.
      *-----------------------------------------------------------------
           MOVE  0005                  TO  MSG1-CD-SEQL-ERRO

           MOVE SGTSL069               TO NM-PGM
           SET  SGTSL069-LISTA-OPR     TO TRUE
           MOVE SGTS0279-CD-CTRA-TVM
                                       TO SGTSL069-CD-CTRA-TIT-VL
           MOVE SGTS0279-CD-OPR-ETRD-MCP
                                       TO SGTSL069-CD-OPR-ETRD-MCP

           PERFORM 900000-EXECUTAR-MODULO

           IF SGTSL069-CD-TIP-EST-OPR(1) NOT EQUAL 5
              OR
              SGTSL069-CD-TIP-EST-VRS(1) NOT EQUAL 3
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0920              TO  MSG1-CD-ERRO-TS
               MOVE  'Código do estado da operação não permite vinculaçã
      -              'o de modelo na Operação Estruturada.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  969000-MONITORAR-VARIAVEIS
               PERFORM  999000-ERRO
           END-IF

           IF SGTSL069-NR-VRS-OPR-ETRD-S(1) NOT EQUAL
              SGTS0279-NR-VRS-OPR-ETRD
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0514              TO  MSG1-CD-ERRO-TS
               MOVE  'Foi encontrada versão mais atualizada da operação
      -              'vinculada.'      TO  MSG1-TX-ERRO-TS
               PERFORM  969000-MONITORAR-VARIAVEIS
               PERFORM  999000-ERRO
           END-IF
           .
       220000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       230000-VERIFICAR-ORD-NEGO       SECTION.
      *    Consulta se o modelo de ordem de venda informado existe,
      *    se está ativo e se o modelo é do tipo 2-"Ordem de venda"
      *    e se o estado da última ordem está ativo
      *-----------------------------------------------------------------
           MOVE  0006                  TO  MSG1-CD-SEQL-ERRO

           PERFORM  231000-VERIFICAR-MOD-ORD

           MOVE SGTSLA75               TO  NM-PGM

           SET SGTSLA75-LISTA-ORD-SEQL TO TRUE
           MOVE SGTS0279-CD-CTRA-TVM   TO  SGTSLA75-E-CD-CTRA-TIT-VL
           MOVE SGTS0279-CD-OPR-ETRD-MCP
                                       TO  SGTSLA75-E-CD-OPR-ETRD-MCP
           MOVE SGTS0279-NR-VRS-OPR-ETRD
                                       TO  SGTSLA75-E-NR-VRS-OPR-ETRD
           MOVE SGTS0279-NR-SEQL-ORD-NEGO
                                       TO  SGTSLA75-E-NR-SEQL-ORD-NEGO
           PERFORM 900000-EXECUTAR-MODULO

           IF  SGTSLA75-QT-OCR-TAB     >   ZEROS
              MOVE 'S'                 TO  AUX-ACHOU-MOD
           ELSE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  1090              TO  MSG1-CD-ERRO-TS
               MOVE  'Sequencial da Ordem de Negociação Venda não locali
      -              'zado.'           TO  MSG1-TX-ERRO-TS
               PERFORM  969000-MONITORAR-VARIAVEIS
               PERFORM  999000-ERRO
           END-IF

           IF SGTSLA75-CD-TIP-EST-NEGO(1)   NOT EQUAL 1
              SET   ERRO-PROGRAMADO    TO  TRUE
              MOVE  1069               TO  MSG1-CD-ERRO-TS
              MOVE 'Alteração'         TO  TX-DTQ-MSG

              STRING 'Código do estado da operação não permite '
                      DELIMITED BY SIZE
                      TX-DTQ-MSG
                      DELIMITED BY SPACES
                      ' de cadastramento de ordem de venda.'
                      DELIMITED BY SIZE
                      INTO                MSG1-TX-ERRO-TS
              END-STRING

              PERFORM 969000-MONITORAR-VARIAVEIS
              PERFORM  999000-ERRO
           END-IF
           .
       230000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       231000-VERIFICAR-MOD-ORD        SECTION.
      *    Consulta se o modelo de ordem de venda informado existe,
      *    se está ativo e se o modelo é do tipo 2-"Ordem de venda".
      *    Executado sempre, para validar o modelo que está sendo infor-
      *    mado para vinculação à Operação Estruturada.
      *-----------------------------------------------------------------
           MOVE  0007                  TO  MSG1-CD-SEQL-ERRO

           EXEC SQL
              SELECT  A.CD_PRD
                   ,  A.CD_MDLD
                   ,  A.IN_MOD_ORD_ATI
                   ,  A.CD_TIP_NEGO
                   ,  B.CD_TIP_EST_DCS
                   ,  B.TS_EST_MOD_ORD

              INTO   :CD-PRD
                     :CD-PRD-N
                   , :CD-MDLD
                     :CD-MDLD-N
                   , :IN-MOD-ORD-ATI
                   , :CD-TIP-NEGO
                   , :CD-TIP-EST-DCS
                   , :TS-EST-MOD-ORD

              FROM  DB2SGT.MOD_ORD_NEGO  A

              INNER  JOIN DB2SGT.EST_MOD_ORD_NEGO B
                  ON  A.CD_MOD_ORD_NEGO    = B.CD_MOD_ORD_NEGO

              WHERE   A.CD_MOD_ORD_NEGO    = :SGTS0279-CD-MOD-ORD-NEGO
                 AND  B.TS_EST_MOD_ORD     =
                      (SELECT MAX (C.TS_EST_MOD_ORD)  FROM
                        DB2SGT.EST_MOD_ORD_NEGO  C
                       WHERE
                          B.CD_MOD_ORD_NEGO    = C.CD_MOD_ORD_NEGO)

           END-EXEC

           IF  SQLCODE                     NOT EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  9997              TO  MSG1-CD-ERRO-TS
               MOVE  SQLCODE           TO  MSG1-CD-RTN-PGM-ACND
               MOVE  'Erro no acesso à tabela modelo ordem negociação.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM  999000-ERRO
           END-IF

      *    CD-TIP-EST-DCS  = 2 (Aprovado)
           IF  CD-TIP-EST-DCS(1)    NOT EQUAL 2
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0916              TO  MSG1-CD-ERRO-TS
               MOVE  'Modelo informado deve estar no estado aprovado.'
                                      TO  MSG1-TX-ERRO-TS
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM  999000-ERRO
           END-IF

           IF  IN-MOD-ORD-ATI(1)    NOT EQUAL 'S'
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0917              TO  MSG1-CD-ERRO-TS
               MOVE  'Modelo de ordem de venda deve estar ativo.'
                                    TO  MSG1-TX-ERRO-TS
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM  999000-ERRO
           END-IF

      *    CD-TIP-NEGO = 2 (Tipo de Negociação = Negociação de venda)
           IF  CD-TIP-NEGO(1)      NOT EQUAL 2
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  1074              TO  MSG1-CD-ERRO-TS
               MOVE 'Tipo de negociação do modelo não é negociação de ve
      -             'nda.'             TO  MSG1-TX-ERRO-TS
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM  999000-ERRO
           END-IF
           .
       231000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       250000-ATUALIZAR-ORD-NEGO       SECTION.
      *  Efetua a alteração da ordem negociação venda, com os dados
      *    informados na entrada e campos que não podem ser alterados
      *    nessa operação, permanecem inalterados.
      *-----------------------------------------------------------------
           MOVE  0008                  TO  MSG1-CD-SEQL-ERRO

           MOVE SGTSA075               TO  NM-PGM

           MOVE SGTS0279-CD-CTRA-TVM
                                       TO  SGTSA075-CD-CTRA-TIT-VL
           MOVE SGTS0279-CD-OPR-ETRD-MCP
                                       TO  SGTSA075-CD-OPR-ETRD-MCP
           MOVE SGTS0279-NR-VRS-OPR-ETRD
                                       TO  SGTSA075-NR-VRS-OPR-ETRD
           MOVE SGTS0279-NR-SEQL-ORD-NEGO
                                       TO  SGTSA075-NR-SEQL-ORD-NEGO
           MOVE 'CURRENT'              TO  SGTSA075-TS-ULT-ATL
           MOVE SGTS0279-CD-MOD-ORD-NEGO
                                       TO  SGTSA075-CD-MOD-ORD-NEGO
           MOVE SGTSLA75-CD-ARQ-OPR-ETRD(1)
                                       TO  SGTSA075-CD-ARQ-OPR-ETRD
           MOVE SGTSLA75-CD-MTV-NEGO-VND(1)
                                       TO  SGTSA075-CD-MTV-NEGO-VND
           MOVE SGTSLA75-CD-TIP-APL-TAXA(1)
                                       TO  SGTSA075-CD-TIP-APL-TAXA
           MOVE SGTSLA75-CD-IEC-TAXA-VND (1)
                                       TO  SGTSA075-CD-IEC-TAXA-VND
           MOVE SGTSLA75-DT-EMS-ORD-NEGO (1)
                                       TO  SGTSA075-DT-EMS-ORD-NEGO
           MOVE SGTSLA75-DT-ESTV-TAXA-VND (1)
                                       TO  SGTSA075-DT-ESTV-TAXA-VND
           MOVE SGTSLA75-VL-PU-PAPL-VND (1)
                                       TO  SGTSA075-VL-PU-PAPL-VND
           MOVE SGTSLA75-VL-TAXA-NEGO-VND (1)
                                       TO  SGTSA075-VL-TAXA-NEGO-VND
           MOVE SGTSLA75-TX-JST-VND-OPR (1)
                                       TO  SGTSA075-TX-JST-VND-OPR
           MOVE SGTSLA75-TX-OBS-ORD-NEGO (1)
                                       TO  SGTSA075-TX-OBS-ORD-NEGO
           MOVE SGTSLA75-DT-VND(1)     TO  SGTSA075-DT-VND
           MOVE SGTSLA75-QT-TTL-PAPL(1)
                                       TO  SGTSA075-QT-TTL-PAPL
           MOVE SGTSLA75-QT-PAPL-CMPD (1)
                                       TO  SGTSA075-QT-PAPL-CMPD
           MOVE SGTSLA75-QT-PAPL-LVRE (1)
                                       TO  SGTSA075-QT-PAPL-LVRE
           MOVE SGTSLA75-TX-MSG-ORD-NEGO (1)
                                       TO  SGTSA075-TX-MSG-ORD-NEGO

           PERFORM 900000-EXECUTAR-MODULO

           PERFORM 252000-INCLUIR-EST-ORD-NEGO
           .
       250000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       252000-INCLUIR-EST-ORD-NEGO     SECTION.
      *  Inclusão de estado ativo para a ordem negociação incluída.
      *  SGTSI078-CD-TIP-EST-NEGO = 1 (ativo)
      *-----------------------------------------------------------------
           MOVE  0009                  TO  MSG1-CD-SEQL-ERRO

           MOVE SGTSI078               TO  NM-PGM

           MOVE SGTS0279-CD-CTRA-TVM   TO  SGTSI078-CD-CTRA-TIT-VL
           MOVE SGTS0279-CD-OPR-ETRD-MCP
                                       TO  SGTSI078-CD-OPR-ETRD-MCP
           MOVE SGTS0279-NR-VRS-OPR-ETRD
                                       TO  SGTSI078-NR-VRS-OPR-ETRD
           MOVE SGTS0279-NR-SEQL-ORD-NEGO
                                       TO  SGTSI078-NR-SEQL-ORD-NEGO
           MOVE 'CURRENT'              TO  SGTSI078-TS-EST-ORD-NEGO
           MOVE KOPRD-CD-OPRD          TO  SGTSI078-CD-USU-ULT-ATL
           MOVE 1                      TO  SGTSI078-CD-TIP-EST-NEGO

           PERFORM 900000-EXECUTAR-MODULO
           .
       252000-SAI.
           EXIT.
      *-----------------------------------------------------------------
      *   Section da Faixa de 900000 a 969900 para Demais Rotinas
      *-----------------------------------------------------------------
      * Executa módulos implementados com CONTAINER                   *
      *-----------------------------------------------------------------
       900000-EXECUTAR-MODULO          SECTION.
      *-----------------------------------------------------------------

      *    Grava o container de requisição do módulo a executar
           PERFORM 982000-GRAVAR-CONTAINER-RQSC

      *    Limpa a resposta e erro antes de executar o módulo
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO

      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EVALUATE NM-PGM

               WHEN SGTSL078
                    EXEC CICS LINK
                              PROGRAM ( SGTSL078   )
                              CHANNEL ( NM-CNL     )
                              RESP    ( MSG1-RESP1 )
                              RESP2   ( MSG1-RESP2 )
                    END-EXEC

               WHEN SGTSA075
                    EXEC CICS LINK
                              PROGRAM ( SGTSA075   )
                              CHANNEL ( NM-CNL     )
                              RESP    ( MSG1-RESP1 )
                              RESP2   ( MSG1-RESP2 )
                    END-EXEC

               WHEN SGTSL069
                    EXEC CICS LINK
                              PROGRAM ( SGTSL069   )
                              CHANNEL ( NM-CNL     )
                              RESP    ( MSG1-RESP1 )
                              RESP2   ( MSG1-RESP2 )
                    END-EXEC

               WHEN SGTSLA75
                    EXEC CICS LINK
                              PROGRAM ( SGTSLA75   )
                              CHANNEL ( NM-CNL     )
                              RESP    ( MSG1-RESP1 )
                              RESP2   ( MSG1-RESP2 )
                    END-EXEC

               WHEN SGTSI078
                    EXEC CICS LINK
                              PROGRAM ( SGTSI078   )
                              CHANNEL ( NM-CNL     )
                              RESP    ( MSG1-RESP1 )
                              RESP2   ( MSG1-RESP2 )
                    END-EXEC

               WHEN OTHER
                    MOVE 9006          TO MSG1-CD-ERRO-TS
                    MOVE 'Não definida a execução do módulo.'
                                       TO MSG1-TX-ERRO-TS
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE
      *
           EVALUATE TRUE
               WHEN MSG1-RESP1        EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    MOVE 3004          TO MSG1-CD-ERRO-TS
                    MOVE 'Erro ao acionar o módulo.'
                                       TO MSG1-TX-ERRO-TS
                    SET ERRO-CICS      TO TRUE
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *    Efetua a leitura do Container de Erro
           PERFORM 972000-LER-CONTAINER-DE-ERRO

      *    Efetua a leitura do Container de Resposta
           PERFORM 973000-LER-RESPOSTA
           .
       900000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       920000-CHAMAR-COMMAREA          SECTION.
      *-----------------------------------------------------------------
           MOVE  0010                  TO  MSG1-CD-SEQL-ERRO
      *
      *--> Movimento nome do módulo para monitoração de variáveis

           EVALUATE  NM-PGM
           WHEN SGTST005
                EXEC CICS LINK
                     PROGRAM  (SGTST005)
                     COMMAREA (SGTST005-DADOS)
                     FLENGTH  (LENGTH OF SGTST005-DADOS)
                END-EXEC

           WHEN SGTST009
                EXEC CICS LINK
                     PROGRAM  (SGTST009)
                     COMMAREA (SGTST009-DADOS)
                     FLENGTH  (LENGTH OF SGTST009-DADOS)
                END-EXEC

           WHEN OTHER
                CONTINUE
           END-EVALUATE
           .
       920000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       969000-MONITORAR-VARIAVEIS      SECTION.
      *--> Monitora as variáveis dos módulos chamados
      *-----------------------------------------------------------------

           EVALUATE  NM-PGM

               WHEN  SGTSA075
                     MOVE 'SGTSA075-CD-CTRA-TIT-VL'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSA075-CD-CTRA-TIT-VL
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSA075-CD-OPR-ETRD-MCP'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSA075-CD-OPR-ETRD-MCP
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSA075-NR-VRS-OPR-ETRD'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSA075-NR-VRS-OPR-ETRD
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSA075-NR-SEQL-ORD-NEGO'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSA075-NR-SEQL-ORD-NEGO
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSA075-TS-ULT-ATL'  TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSA075-TS-ULT-ATL   TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSA075-CD-MOD-ORD-NEGO'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSA075-CD-MOD-ORD-NEGO
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSA075-CD-ARQ-OPR-ETRD'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSA075-CD-ARQ-OPR-ETRD
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSA075-CD-MTV-NEGO-VND'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSA075-CD-MTV-NEGO-VND
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSA075-CD-TIP-APL-TAXA'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSA075-CD-TIP-APL-TAXA
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSA075-CD-IEC-TAXA-VND'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSA075-CD-IEC-TAXA-VND
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSA075-DT-EMS-ORD-NEGO'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSA075-DT-EMS-ORD-NEGO
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSA075-DT-ESTV-TAXA-VND'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSA075-DT-ESTV-TAXA-VND
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSA075-VL-PU-PAPL-VND'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSA075-VL-PU-PAPL-VND
                                                 TO  VL-PU-PAPL-VND-DISP
                     MOVE  VL-PU-PAPL-VND-TX     TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSA075-VL-TAXA-NEGO-VND'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSA075-VL-TAXA-NEGO-VND
                                                TO VL-TAXA-NEGO-VND-DISP
                     MOVE  VL-TAXA-NEGO-VND-TX   TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSA075-TX-JST-VND-OPR'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSA075-TX-JST-VND-OPR
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSA075-TX-OBS-ORD-NEGO'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSA075-TX-OBS-ORD-NEGO
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSA075-TX-MSG-ORD-NEGO'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSA075-TX-MSG-ORD-NEGO
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSA075-DT-VND'      TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSA075-DT-VND       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSA075-QT-TTL-PAPL' TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSA075-QT-TTL-PAPL  TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSA075-QT-PAPL-CMPD'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSA075-QT-PAPL-CMPD TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSA075-QT-PAPL-LVRE'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSA075-QT-PAPL-LVRE TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  SGTSL069
                     MOVE 'SGTSL069-CD-TIP-SRVC' TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL069-CD-TIP-SRVC  TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSL069-CD-CTRA-TIT-VL'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL069-CD-CTRA-TIT-VL
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSL069-CD-OPR-ETRD-MCP'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL069-CD-OPR-ETRD-MCP
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSL069-NR-VRS-OPR-ETRD'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL069-NR-VRS-OPR-ETRD
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSL069-TS-EST-VRS-PAG'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL069-TS-EST-VRS-PAG
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  SGTSL078
                     MOVE 'SGTSL078-CD-TIP-SRVC' TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL078-CD-TIP-SRVC  TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSL078-CD-CTRA-TIT-VL'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL078-CD-CTRA-TIT-VL
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSL078-CD-OPR-ETRD-MCP'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL078-CD-OPR-ETRD-MCP
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSL078-NR-VRS-OPR-ETRD'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL078-NR-VRS-OPR-ETRD
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSL078-NR-SEQL-ORD-ULT'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL078-NR-SEQL-ORD-ULT
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSL078-TS-EST-ULT'  TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL078-TS-EST-ULT   TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  SGTSLA75
                     MOVE 'SGTSLA75-E-CD-TIP-SRVC'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSLA75-E-CD-TIP-SRVC
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSLA75-E-CD-CTRA-TIT-VL'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSLA75-E-CD-CTRA-TIT-VL
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSLA75-E-CD-OPR-ETRD-MCP'
                                                 TO  MSGKW999-NM-VRV-MNT
                     IF  SGTSLA75-E-CD-OPR-ETRD-MCP    NUMERIC
                         MOVE  SGTSLA75-E-CD-OPR-ETRD-MCP
                                                 TO CD-OPR-ETRD-MCP-DISP
                         MOVE  CD-OPR-ETRD-MCP-TX
                                                 TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE 'Valor não numérico'
                                                 TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSLA75-E-NR-VRS-OPR-ETRD'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSLA75-E-NR-VRS-OPR-ETRD
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  SGTSI078
                     MOVE 'SGTSI078-CD-CTRA-TIT-VL'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI078-CD-CTRA-TIT-VL
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSI078-CD-OPR-ETRD-MCP'
                                                 TO  MSGKW999-NM-VRV-MNT
                     IF  SGTSI078-CD-OPR-ETRD-MCP    NUMERIC
                         MOVE  SGTSI078-CD-OPR-ETRD-MCP
                                                 TO CD-OPR-ETRD-MCP-DISP
                         MOVE  CD-OPR-ETRD-MCP-TX
                                                 TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE 'Valor não numérico'
                                                 TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSI078-NR-VRS-OPR-ETRD'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI078-NR-VRS-OPR-ETRD
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSI078-NR-SEQL-ORD-NEGO'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI078-NR-SEQL-ORD-NEGO
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSI078-TS-EST-ORD-NEGO'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI078-TS-EST-ORD-NEGO
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSI078-CD-USU-ULT-ATL'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI078-CD-USU-ULT-ATL
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSI078-CD-TIP-EST-NEGO'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI078-CD-TIP-EST-NEGO
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  SGTST005
                     MOVE 'SGTST005-ACAO'        TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST005-ACAO         TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTST005-CD-CTRA-TIT-VL'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST005-CD-CTRA-TIT-VL
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTST005-CD-OPR-ETRD-MCP'
                                                 TO  MSGKW999-NM-VRV-MNT
                     IF  SGTST005-CD-OPR-ETRD-MCP    NUMERIC
                         MOVE  SGTST005-CD-OPR-ETRD-MCP
                                                 TO CD-OPR-ETRD-MCP-DISP
                         MOVE  CD-OPR-ETRD-MCP-TX
                                                 TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE 'Valor não numérico'
                                                 TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTST005-NR-VRS-OPR-ETRD'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST005-NR-VRS-OPR-ETRD
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  SGTST009
                     MOVE 'SGTST009-ACAO'        TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST009-ACAO         TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTST009-CD-CTRA-TIT-VL'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST009-CD-CTRA-TIT-VL
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTST009-CD-OPR-ETRD-MCP'
                                                 TO  MSGKW999-NM-VRV-MNT
                     IF  SGTST009-CD-OPR-ETRD-MCP    NUMERIC
                         MOVE  SGTST009-CD-OPR-ETRD-MCP
                                                 TO CD-OPR-ETRD-MCP-DISP
                         MOVE  CD-OPR-ETRD-MCP-TX
                                                 TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE 'Valor não numérico'
                                                 TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTST009-NR-VRS-OPR-ETRD'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST009-NR-VRS-OPR-ETRD
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTST009-TS-EST-OPR-ETRD'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST009-TS-EST-OPR-ETRD
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTST009-CD-USU-RSP-ATL'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST009-CD-USU-RSP-ATL
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  OTHER
                     CONTINUE
           END-EVALUATE
           .
       969000-SAI.
           EXIT.
      *-----------------------------------------------------------------
      *    Section da Faixa de 970000 a 979900 para Leitura de Dados
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       970000-LER-CANAL                SECTION.
      *--> Obtem o canal corrente
      *-----------------------------------------------------------------
           MOVE  0011                  TO  MSG1-CD-SEQL-ERRO

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CANAL            TO  TRUE

           EXEC CICS ASSIGN
                     CHANNEL ( NM-CNL )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       970000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       971000-LER-REQUISICAO           SECTION.
      *-----------------------------------------------------------------

      *--> Monta o nome do container de requisicao
           MOVE  'SGTS0279'            TO  NM-PGM
           MOVE  'SGTS0279-RQSC'       TO  NM-CTNR

      *--> Recupera o tamanho do container de requisição
           PERFORM  973100-LER-CONTAINER-NODATA

           MOVE  0012                  TO  MSG1-CD-SEQL-ERRO

      *--> Obtem o tamanho esperado da requisicao
           MOVE  LENGTH OF SGTS0279-RQSC
                                       TO  QT-CCT-EPRO

      *--> Valida tamanho esperado da requisicao X tamanho recebido

           IF  QT-CCT-LIDO                 NOT EQUAL QT-CCT-EPRO
               SET   ERRO-TAMANHO-AREA
                                       TO  TRUE
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *--> Configura dados para leitura do container de requisição
           SET   GET-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *--> Efetua a leitura do container
           EXEC CICS GET
                     CONTAINER (NM-CTNR)
                     FLENGTH   (QT-CCT-LIDO)
                     INTO      (SGTS0279-RQSC)
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       971000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       972000-LER-CONTAINER-DE-ERRO    SECTION.
      *--> A presença do container PRM-ERRO indica que houve erro de
      *    integração, a ausência do container indica sucesso.
      *-----------------------------------------------------------------
           MOVE  0013                  TO  MSG1-CD-SEQL-ERRO

      *--> Le o container de erro dos modulos chamados
           SET   GET-CONTAINER         TO  TRUE
           MOVE  'PRM-ERRO'            TO  NM-CTNR
           INITIALIZE                  IIBKERRO-PRM-ERRO

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS  GET
                      CONTAINER ( NM-CTNR                     )
                      INTO      ( IIBKERRO-PRM-ERRO           )
                      FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                      RESP      ( MSG1-RESP1                  )
                      RESP2     ( MSG1-RESP2                  )
           END-EXEC

           EVALUATE  TRUE
      *        Caso deseje personalizar ou não interromper o
      *        processamento, alterar para não dar erro aqui SOMENTE
      *        PARA O MÓDULO CHAMADO e tratar na section chamadora
               WHEN  MSG1-RESP1            EQUAL DFHRESP(CONTAINERERR)
                     CONTINUE
               WHEN  MSG1-RESP1            EQUAL DFHRESP(NORMAL)
                     SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
                     MOVE  NM-PGM      TO  MSG1-NM-PGM-ACND
                     IF  NM-PGM(1:3)       EQUAL 'SGT'
                         MOVE  IIBKERRO-CD-ERRO
                                       TO  MSG1-CD-RTN-PGM-ACND
                                           MSG1-CD-ERRO-TS
                         MOVE  IIBKERRO-TX-ERRO
                                       TO  MSG1-TX-RTN-PGM-ACND
                                           MSG1-TX-ERRO-TS
                     ELSE
                         MOVE  3004    TO  MSG1-CD-ERRO-TS
                         STRING  'Erro ao acionar o módulo ' NM-PGM '.'
                                 DELIMITED BY SIZE
                                 INTO      MSG1-TX-ERRO-TS
                         MOVE  IIBKERRO-TX-ERRO
                                       TO  MSG1-TX-RTN-PGM-ACND
                         MOVE  IIBKERRO-CD-ERRO
                                       TO  MSG1-CD-RTN-PGM-ACND
                     END-IF
      *              Monitorar variáveis do módulo chamado
                     PERFORM  969000-MONITORAR-VARIAVEIS
                     PERFORM  999000-ERRO
               WHEN  OTHER
                     SET   ERRO-CONTAINER
                                       TO  TRUE
                     PERFORM  999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       972000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       973000-LER-RESPOSTA             SECTION.
      *-----------------------------------------------------------------
      *    Recupera o container com os dados de resposta (saída).
      *-----------------------------------------------------------------

      *    Monta o nome dos container de acordo com modulo a executar
           MOVE SPACES                 TO NM-CTNR

      *    Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *    nome_modulo -RPST

           MOVE NM-PGM                 TO NM-CTNR(1:8)
           MOVE '-RPST'                TO NM-CTNR(9:5)

      *    Recupera o tamanho do container de resposta
           PERFORM 973100-LER-CONTAINER-NODATA

      *Configura dados para leitura do container de resposta
           SET  GET-CONTAINER          TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Lê a RESPOSTA de acordo com o programa que a gravou

           EVALUATE NM-PGM

      *    Calcula a quantidade de caracteres esperado e limpa a área de
      *    dados antes de executar o GET (somentepara matrizes com dados
      *    não variáveis)

               WHEN SGTSA075
                    EXEC CICS GET
                              CONTAINER ( NM-CTNR       )
                              INTO      ( SGTSA075-RPST )
                              FLENGTH   ( QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSL069
                    EXEC CICS GET
                              CONTAINER ( NM-CTNR       )
                              INTO      ( SGTSL069-RPST )
                              FLENGTH   ( QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSL078
                    EXEC CICS GET
                              CONTAINER ( NM-CTNR       )
                              INTO      ( SGTSL078-RPST )
                              FLENGTH   ( QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSLA75
                    EXEC CICS GET
                              CONTAINER ( NM-CTNR       )
                              INTO      ( SGTSLA75-RPST )
                              FLENGTH   ( QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSI078
                    EXEC CICS GET
                              CONTAINER ( NM-CTNR       )
                              INTO      ( SGTSI078-RPST )
                              FLENGTH   ( QT-CCT-LIDO   )
                    END-EXEC

               WHEN OTHER
                    MOVE 9003          TO MSG1-CD-ERRO-TS
                    MOVE 'Erro na leitura de resposta do módulo.'
                                       TO MSG1-TX-ERRO-TS
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       973000-SAI.
           EXIT.
      *-----------------------------------------------------------------
      *-----------------------------------------------------------------
       973100-LER-CONTAINER-NODATA     SECTION.
      *-----------------------------------------------------------------
           MOVE  0014                  TO  MSG1-CD-SEQL-ERRO

      *--> Configura dados para leitura do container de requisição
           SET   GET-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *--> Obtem a quantidade de caracteres no container
           MOVE  ZEROS                 TO  QT-CCT-LIDO

           EXEC CICS  GET
                      CONTAINER ( NM-CTNR     )
                      NODATA
                      FLENGTH   ( QT-CCT-LIDO )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que no houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       973100-SAI.
           EXIT.
      *-----------------------------------------------------------------
       974000-LER-DADOS-OPRD           SECTION.
      *-----------------------------------------------------------------
           MOVE  0015                  TO  MSG1-CD-SEQL-ERRO

      *--> Configura dados para leitura do container de requisição
           SET   GET-CONTAINER         TO  TRUE
           MOVE  'PRM-INF-OPRD'        TO  NM-CTNR

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

           INITIALIZE                      PRM-INF-OPRD

           EXEC CICS GET
                     CONTAINER ( NM-CTNR                )
                     INTO      ( PRM-INF-OPRD           )
                     FLENGTH   ( LENGTH OF PRM-INF-OPRD )
           END-EXEC.

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       974000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       975000-CHAMA-SBDATAF-01         SECTION.
      *-----------------------------------------------------------------
           MOVE  0016                  TO  MSG1-CD-SEQL-ERRO

           MOVE 4095                   TO A-TAM
           MOVE 'SBDATAF -WRKAREA-FIM' TO A-FIM

           CALL  SBDATAF               USING  SBDATAF-FUNCAO-01
                                              AREAWORK
                                              SBDATAF-ARG01-01
                                              SBDATAF-ARG02-01

           IF  RETURN-CODE             NOT EQUAL ZEROS
               SET  ERRO-PROGRAMA-ACIONADO TO  TRUE
               MOVE 0051                   TO  MSG1-CD-ERRO-TS
               MOVE 'Erro na rotina que valida data.'
                                           TO  MSG1-TX-ERRO-TS
               MOVE SBDATAF                TO  MSG1-NM-PGM-ACND
               MOVE RETURN-CODE            TO  MSG1-CD-RTN-PGM-ACND
               MOVE 'Data emissão da ordem de negociação inválida.'
                                           TO  MSG1-TX-RTN-PGM-ACND

               MOVE 'SBDATAF-FUNCAO-01'  TO  MSGKW999-NM-VRV-MNT
               MOVE  SBDATAF-FUNCAO-01   TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SBDATAF-ARG01-01'   TO  MSGKW999-NM-VRV-MNT
               MOVE  SBDATAF-ARG01-01    TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SBDATAF-ARG02-01'   TO  MSGKW999-NM-VRV-MNT
               MOVE  SBDATAF-ARG02-01    TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       975000-SAI.
           EXIT.
      *-----------------------------------------------------------------
      *   Section da Faixa de 980000 a 989900 para Gravação de Dados
      *-----------------------------------------------------------------

      *----------------------------------------------------------------*
       982000-GRAVAR-CONTAINER-RQSC    SECTION.
      *-----------------------------------------------------------------
      *    Postar container com dados de requisição (entrada)
      *-----------------------------------------------------------------

      *    Monta o nome dos container de acordo com modulo a executar
           MOVE SPACES                 TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE

      *    Quando módulos via IIB, fixo nome PRM-DADOS-RQSC, senão
      *    nome_modulo -RQSC

           MOVE NM-PGM        TO NM-CTNR(1:8)
           MOVE '-RQSC'       TO NM-CTNR(9:5)

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Faz put da requisição do programa a executar
           EVALUATE NM-PGM
               WHEN SGTSA075
                    EXEC CICS PUT
                              CONTAINER ( NM-CTNR                 )
                              FROM      ( SGTSA075-RQSC           )
                              FLENGTH   ( LENGTH OF SGTSA075-RQSC )
                    END-EXEC

               WHEN SGTSL069
                    EXEC CICS PUT
                              CONTAINER ( NM-CTNR                 )
                              FROM      ( SGTSL069-RQSC           )
                              FLENGTH   ( LENGTH OF SGTSL069-RQSC )
                    END-EXEC

               WHEN SGTSL078
                    EXEC CICS PUT
                              CONTAINER ( NM-CTNR                 )
                              FROM      ( SGTSL078-RQSC           )
                              FLENGTH   ( LENGTH OF SGTSL078-RQSC )
                    END-EXEC

               WHEN SGTSLA75
                    EXEC CICS PUT
                              CONTAINER ( NM-CTNR                 )
                              FROM      ( SGTSLA75-RQSC           )
                              FLENGTH   ( LENGTH OF SGTSLA75-RQSC )
                    END-EXEC

               WHEN SGTSI078
                    EXEC CICS PUT
                              CONTAINER ( NM-CTNR                 )
                              FROM      ( SGTSI078-RQSC           )
                              FLENGTH   ( LENGTH OF SGTSI078-RQSC )
                    END-EXEC

               WHEN OTHER
                    MOVE 9004          TO MSG1-CD-ERRO-TS
                    MOVE 'Erro na gravação da requisição do módulo.'
                                       TO MSG1-TX-ERRO-TS
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       982000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       983000-EXCLUIR-RESPOSTA-ERRO    SECTION.
      *--> Exclui o container de resposta e de erro para evitar
      *    problemas em módulos executados mais de uma vez dentro
      *    da mesma sessão do CICS.
      *    Equivale ao initialize na área de resposta dos
      *    programas que utilizam COMMAREA.
      *-----------------------------------------------------------------
           MOVE  0017                  TO  MSG1-CD-SEQL-ERRO

      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *    nome_modulo -RPST
           MOVE  NM-PGM                TO  NM-CTNR(1:8)
           MOVE  '-RPST'               TO  NM-CTNR(9:5)

           SET   DEL-CONTAINER         TO  TRUE

      *--> Exclui o container de resposta
      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS  DELETE
                      CONTAINER ( NM-CTNR    )
                      RESP      ( MSG1-RESP1 )
                      RESP2     ( MSG1-RESP2 )
           END-EXEC

           EVALUATE  TRUE
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(CONTAINERERR)
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(NORMAL)
                     CONTINUE
               WHEN  OTHER
                     SET   ERRO-CONTAINER
                                       TO  TRUE
                     PERFORM  999002-TRATA-CONDITION-CICS
           END-EVALUATE

      *--> Excluir o container de erro
           PERFORM  983100-EXCLUIR-CONTAINER-ERRO
           .
       983000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       983100-EXCLUIR-CONTAINER-ERRO   SECTION.
      *-----------------------------------------------------------------
           MOVE  0018                  TO  MSG1-CD-SEQL-ERRO

      *--> Limpar variaveis container de erro
           INITIALIZE                  IIBKERRO-PRM-ERRO

      *--> Exclui o container de erro
           MOVE  'PRM-ERRO'            TO  NM-CTNR
           SET   DEL-CONTAINER         TO  TRUE

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS  DELETE
                      CONTAINER ( NM-CTNR    )
                      RESP      ( MSG1-RESP1 )
                      RESP2     ( MSG1-RESP2 )
           END-EXEC

           EVALUATE  TRUE
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(CONTAINERERR)
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(NORMAL)
                     CONTINUE
               WHEN  OTHER
                     SET   ERRO-CONTAINER
                                       TO  TRUE
                     PERFORM  999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       983100-SAI.
           EXIT.
      *-----------------------------------------------------------------
       984000-GRAVAR-CONTAINER-ERRO    SECTION.
      *--> Grava o erro gerado no container de erro
      *-----------------------------------------------------------------

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS  PUT
                      CONTAINER ( 'PRM-ERRO'                  )
                      FROM      ( IIBKERRO-PRM-ERRO           )
                      FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                      RESP      ( MSG1-RESP1                  )
                      RESP2     ( MSG1-RESP2                  )
           END-EXEC
           .
       984000-SAI.
           EXIT.
      *-----------------------------------------------------------------
      *    As sections abaixo iniciadas com 999000 representam os
      *    tratamentos de erro através do Sistema MSG (rotina MSGS0001)
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       999000-ERRO                     SECTION.
      *-----------------------------------------------------------------

           IF  MSG1-CD-ERRO-TS             EQUAL ZEROS
              MOVE  8888               TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS             EQUAL SPACES
               MOVE  'Erro não específicado no módulo.'
                                       TO  MSG1-TX-ERRO-TS
           END-IF

           PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           .
       999000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------

      *--> Em caso de erro, desfaz as atualizações em banco de dados
           EXEC CICS SYNCPOINT
                     ROLLBACK
           END-EXEC

      *--> Verifica se o tipo de erro gerado possui tratamento interno
           EVALUATE  TRUE
               WHEN  ERRO-CONTAINER
                     PERFORM  999004-TRATA-ERRO-CTNR
               WHEN  ERRO-TAMANHO-AREA
                     PERFORM  999005-TRATA-ERRO-AREA-DADOS
               WHEN  ERRO-CANAL
                     PERFORM  999006-TRATA-ERRO-CANAL
               WHEN  OTHER
                     CONTINUE
           END-EVALUATE

      *--> Adiciona as variáveis de entrada para monitoração
           MOVE 'SGTS0279-CD-CTRA-TVM'         TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0279-CD-CTRA-TVM          TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0279-CD-OPR-ETRD-MCP'     TO  MSGKW999-NM-VRV-MNT
      *------> Verifica se o conteudo e numerico antes de adicionar
           IF  SGTS0279-CD-OPR-ETRD-MCP    NUMERIC
               MOVE  SGTS0279-CD-OPR-ETRD-MCP  TO  CD-OPR-ETRD-MCP-DISP
               MOVE  CD-OPR-ETRD-MCP-TX        TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico'      TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0279-NR-VRS-OPR-ETRD'     TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0279-NR-VRS-OPR-ETRD      TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0279-CD-MOD-ORD-NEGO'     TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0279-CD-MOD-ORD-NEGO      TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0279-NR-SEQL-ORD-NEGO'    TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0279-NR-SEQL-ORD-NEGO     TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'TX-DTQ-MSG'                   TO  MSGKW999-NM-VRV-MNT
           MOVE  TX-DTQ-MSG                    TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

      *--> Grava o erro gerado no sistema MSG - Mensageria
           PERFORM  MSG1-NTF-MSG

           MOVE  MSG1-CD-ERRO-TS               TO  IIBKERRO-CD-ERRO
           MOVE  MSG1-TX-ERRO-TS               TO  IIBKERRO-TX-ERRO

      *--> Grava o erro gerado no container de erro
           PERFORM  984000-GRAVAR-CONTAINER-ERRO

      *--> Encerra a transacao
           PERFORM  999999-ENCERRAR
           .
       999000-SAI.
           EXIT.
      *---------------------------------------
      * 999001-TRATA-ABEND-CICS        SECTION.
      * 999002-TRATA-CONDITION-CICS    SECTION.
      * 999003-ADICIONA-VRV-MNT        SECTION.
      * 999004-ALINHA-NUMERO-ESQUERDA  SECTION.
      *----------------------------------------
-INC HLPKP999
      *----------------------------------------
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *----------------------------------------
-INC SGTKP999
      *----------------------------------------
      * 999010-TRATA-ERRO-DB2          SECTION.
      *----------------------------------------
-INC HLPKP997
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

           EXEC CICS
                RETURN
           END-EXEC
           .
       999999-SAI.
           EXIT.
      *---------------------- FIM SGTS0279 -----------------------------