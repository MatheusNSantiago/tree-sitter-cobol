      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSLE78
      * ANALISTA..: F2634012 Edilene Akemi Horita
      * AUTOR.....: F2634012 Edilene Akemi Horita
      * DATA......: 12/09/2019
      * OBJETIVO..: CONSULTA TABELA DB2EST_ORD_NEGO_OPR
      * COMPILACAO: 52 - PSOSE600 - Cobol 4.2 c/otimizacao p/producao
      *-----------------------------------------------------------------
      * VRS003 - 27.07.2021 - F0950413 - DE 2021/157480 - Alteração da
      *          Quantidade de ocorrências de 100 para 400 e alteração
      *          do SQL do cursor CURSOR-PSQ-01.
      * VRS002 28/10/2020 F9507141 2016/145195 - Alteraçao nos critérios
      *                                          de pesquisa do SQL.
      * VRS001 12/09/2019 F2634012 2016/145195 - Implantação.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTSLE78.

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
           '*** Storage Section Começa Aqui ***'.
       77  CTE-PROG                    PIC  X(17)
                                       VALUE '*** SGTSLE78 ***'.
       77  CTE-VERS                    PIC  X(06) VALUE 'VRS001'.

      * Subrotinas Utilizadas

      *--> Definição dos Nomes dos Programas Acionados
       01  TAB-PROGRAMAS-CHAMADOS.
           03  SBDATAF  VALUE 'SBDATAF'     PIC  X(008).

      *--> Definição dos Variaveis do Cobolito
-INC ALMKCBL1

      * Area da subrotina SBDATA
       01  FUNCAO                        PIC  X(3) VALUE 'F01'.
       01  AREA-WORK                     PIC  X(4095).
       01  AREA-WORK-RED  REDEFINES  AREA-WORK.
           03 A-TAM                      PIC  9(0008) COMP.
           03 FILLER                     PIC  X(4071).
           03 A-FIM                      PIC  X(0020).
       01  ARG01                         PIC  9(8) VALUE ZEROS.
       01  ARG01-R REDEFINES ARG01.
           03  DIA                       PIC  9(2).
           03  MES                       PIC  9(2).
           03  ANO                       PIC  9(4).
       01  ARG02                         PIC  9(1) VALUE 4.

       01 DATA-INICIO                    PIC  9(08) VALUE ZEROS.
       01 DATA-INICIO-R REDEFINES DATA-INICIO.
          03 ANO                         PIC  9(04).
          03 MES                         PIC  9(02).
          03 DIA                         PIC  9(02).

       01 DATA-FIM                       PIC  9(08) VALUE ZEROS.
       01 DATA-FIM-R REDEFINES DATA-FIM.
          03 ANO                         PIC  9(04).
          03 MES                         PIC  9(02).
          03 DIA                         PIC  9(02).

       01 MSG-SBDATA                     PIC X(30).
       01 MSG-SBDATA-R  REDEFINES MSG-SBDATA.
          03 RTN-SBDATA                  PIC S9(04) COMP.

       01  IC-TAB-MNT                    PIC  9(02) VALUE ZEROS.

      *--> Book do programa de ambiente - CICS5000
      *    Definido na work para preservar as informacoes
-INC HLPKE01B

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

      *--> Area de trabalho
       01  QT-REG-SELD                 PIC S9(4)      COMP-5 VALUE 0.
       01  IC-TAB                      PIC S9(04)     COMP-5 VALUE 0.

      *--> Campos utilizados para leitura da Tabela DMTV_CX_TIT

       01  EST-ORD-NEGO-OPR.

           03 CD-CTRA-TIT-VL           PIC S9(009) COMP-5
                                       OCCURS 0401 TIMES  VALUE ZEROS.
           03 CD-OPR-ETRD-MCP          PIC S9(11) COMP-3
                                       OCCURS 0401 TIMES  VALUE ZEROS.
           03 NR-VRS-OPR-ETRD          PIC S9(4) COMP-5
                                       OCCURS 0401 TIMES  VALUE ZEROS.
           03 NR-SEQL-ORD-NEGO         PIC S9(009) COMP-5
                                       OCCURS 0401 TIMES  VALUE ZEROS.
           03 TS-EST-ORD-NEGO          PIC X(026)
                                       OCCURS 0401 TIMES  VALUE ZEROS.
           03 CD-USU-ULT-ATL           PIC X(08)
                                       OCCURS 0401 TIMES  VALUE SPACES.
           03 CD-TIP-EST-NEGO          PIC S9(009) COMP-5
                                       OCCURS 0401 TIMES  VALUE ZEROS.
           03 DT-VND                   PIC X(10)
                                       OCCURS 0401 TIMES  VALUE SPACES.
           03 DT-VND-N                 PIC S9(004) COMP-5
                                       OCCURS 0401 TIMES  VALUE ZEROS.
           03 QT-TTL-PAPL              PIC S9(009) COMP-5
                                       OCCURS 0401 TIMES  VALUE ZEROS.
           03 QT-TTL-PAPL-N            PIC S9(004) COMP-5
                                       OCCURS 0401 TIMES  VALUE ZEROS.

      *--> Indicadores de nulidades
       01  QT-REG-SELD-N                     PIC S9(4) COMP-5 VALUE 0.

      *--> Variavel(is) de controle do cursor(es)
       01  CTL-CURSOR-PSQ.
           03  CD-FIM-CURSOR-PSQ            PIC S9(04) COMP-5 VALUE 0.
               88  FIM-CURSOR-PSQ            VALUE +100.
           03  IN-CURSOR-PSQ-ABTO           PIC  X(01) VALUE 'N'.
               88  CURSOR-PSQ-ABTO          VALUE 'S'.

      * Books da definicao de dados a serem usados na subrotina de erros
-INC HLPKE001
      *--> Book para captura de erro via MSG1
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
           03  MSGKW999-NM-VRV-MNT     PIC  X(030) VALUE SPACES.
           03  MSGKW999-VL-VRV-MNT     PIC  X(100) VALUE SPACES.
      *--> Indicador de gravação de variáveis monitoradas
       01  MSGKW999-IN-GRV-VRV         PIC  X(01)  VALUE ' '.
           88 MSGKW999-MNT-VRV                     VALUE 'S'.
           88 MSGKW999-N-MNT-VRV                   VALUE 'N'.

      *--> Máscaras edição de variáveis p/ monitoração mensagens de erro
       01  MCR-S9-15                   PIC      ---------------9.
       01  MCR-S9-15-V9-02             PIC  ----.---.---.---.--9,99.
       01  MCR-S9-04-V9-04             PIC                --.--9,9999.

           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

       01  CTE-FINAL-SS                PIC X(40) VALUE
           '*** Storage Section Termina Aqui ***'.

       01  AX-EIBCALEN-ALFA.
           03 AX-EIBCALEN-EDITADO    PIC ZZZZ9.

      * ----------------------------------------------------------------
       LINKAGE                  SECTION.
      * ----------------------------------------------------------------

       01  DFHCOMMAREA.
-INC SGTKLE78

      *-----------------------------------------------------------------
      *    Cursores de Pesquisa
      *-----------------------------------------------------------------

      *--> Cursor de Pesquisa 01
      *    Listar, em uma intervalo de data, de um determinado tipo de
      *     estado o estado da ordem de negociação
      *
           EXEC SQL
             DECLARE CURSOR-PSQ-01 CURSOR
                     WITH ROWSET POSITIONING FOR
             WITH
      *     Tabela com maior Versao aprovada de cada Carteira_Operacao.
             TAB_MAI_VER_APR
                  (        CD_CTRA_TIT_VL
                         , CD_OPR_ETRD_MCP
                         , MAI_VER_APR
                         , CD_TIP_EST_VRS
                         , MAIOR_TS        )
               AS ( SELECT W.CD_CTRA_TIT_VL
                         , W.CD_OPR_ETRD_MCP
                         , W.NR_VRS_OPR_ETRD
                         , W.CD_TIP_EST_VRS
                         , W.TS_EST_VRS
             FROM  DB2SGT.EST_VRS_OPR_ETRD W
             WHERE
               W.CD_CTRA_TIT_VL  > 0 AND
               W.CD_OPR_ETRD_MCP > 0 AND
               W.NR_VRS_OPR_ETRD > 0 AND
               W.TS_EST_VRS = (
               SELECT MAX(X.TS_EST_VRS)
               FROM DB2SGT.EST_VRS_OPR_ETRD X
               WHERE X.CD_CTRA_TIT_VL  =  W.CD_CTRA_TIT_VL
                 AND X.CD_OPR_ETRD_MCP =  W.CD_OPR_ETRD_MCP
                 AND X.NR_VRS_OPR_ETRD = (
                     SELECT MAX(Y.NR_VRS_OPR_ETRD)
                     FROM DB2SGT.EST_VRS_OPR_ETRD Y
                     WHERE Y.CD_CTRA_TIT_VL  =  X.CD_CTRA_TIT_VL
                       AND Y.CD_OPR_ETRD_MCP =  X.CD_OPR_ETRD_MCP
                       AND Y.CD_TIP_EST_VRS  =  3
                       AND Y.TS_EST_VRS      = (
                           SELECT MAX(Z.TS_EST_VRS)
                           FROM DB2SGT.EST_VRS_OPR_ETRD Z
                           WHERE Z.CD_CTRA_TIT_VL  =  Y.CD_CTRA_TIT_VL
                             AND Z.CD_OPR_ETRD_MCP =  Y.CD_OPR_ETRD_MCP
                             AND Z.NR_VRS_OPR_ETRD =  Y.NR_VRS_OPR_ETRD
                                               )
                                         )
                              )
                  )
             SELECT A.CD_CTRA_TIT_VL
                  , A.CD_OPR_ETRD_MCP
                  , A.NR_VRS_OPR_ETRD
                  , A.NR_SEQL_ORD_NEGO  MAXSEQ
                  , C.MAXTS
                  , D.CD_USU_ULT_ATL
                  , D.CD_TIP_EST_NEGO
                  , IFNULL (A.DT_VND, '01.01.1900')
                  , IFNULL (A.QT_TTL_PAPL, 0)
             FROM DB2SGT.ORD_NEGO_OPR A
             JOIN TAB_MAI_VER_APR B
                  ON  A.CD_CTRA_TIT_VL  = B.CD_CTRA_TIT_VL
                  AND A.CD_OPR_ETRD_MCP = B.CD_OPR_ETRD_MCP
                  AND A.NR_VRS_OPR_ETRD = B.MAI_VER_APR
             JOIN ( SELECT CD_CTRA_TIT_VL
                         , CD_OPR_ETRD_MCP
                         , NR_VRS_OPR_ETRD
                         , NR_SEQL_ORD_NEGO
                         , MAX(TS_EST_ORD_NEGO) AS MAXTS
                    FROM DB2SGT.EST_ORD_NEGO_OPR
                    GROUP BY CD_CTRA_TIT_VL
                           , CD_OPR_ETRD_MCP
                           , NR_VRS_OPR_ETRD
                           , NR_SEQL_ORD_NEGO            ) C
                  ON  A.CD_CTRA_TIT_VL   = C.CD_CTRA_TIT_VL
                  AND A.CD_OPR_ETRD_MCP  = C.CD_OPR_ETRD_MCP
                  AND A.NR_VRS_OPR_ETRD  = C.NR_VRS_OPR_ETRD
                  AND A.NR_SEQL_ORD_NEGO = C.NR_SEQL_ORD_NEGO
             JOIN DB2SGT.EST_ORD_NEGO_OPR D
                  ON  D.CD_CTRA_TIT_VL   = C.CD_CTRA_TIT_VL
                  AND D.CD_OPR_ETRD_MCP  = C.CD_OPR_ETRD_MCP
                  AND D.NR_VRS_OPR_ETRD  = C.NR_VRS_OPR_ETRD
                  AND D.NR_SEQL_ORD_NEGO = C.NR_SEQL_ORD_NEGO
                  AND D.TS_EST_ORD_NEGO  = C.MAXTS
                  AND D.CD_TIP_EST_NEGO  = :SGTSLE78-E-CD-TIP-EST-NEGO
             JOIN DB2SGT.MOD_ORD_NEGO J
                  ON  J.CD_MOD_ORD_NEGO = A.CD_MOD_ORD_NEGO
                  AND J.CD_TIP_NEGO     = 2
             WHERE A.DT_VND = (
                   SELECT MAX (H.DT_VND)
                   FROM DB2SGT.ORD_NEGO_OPR H
                   WHERE H.CD_CTRA_TIT_VL  = A.CD_CTRA_TIT_VL
                     AND H.CD_OPR_ETRD_MCP = A.CD_OPR_ETRD_MCP
                     AND H.DT_VND         >= :SGTSLE78-E-DT-INI
                     AND H.DT_VND         <= :SGTSLE78-E-DT-FIM
                     AND H.NR_VRS_OPR_ETRD = (
                         SELECT I.MAI_VER_APR
                         FROM TAB_MAI_VER_APR I
                         WHERE I.CD_CTRA_TIT_VL  = H.CD_CTRA_TIT_VL
                           AND I.CD_OPR_ETRD_MCP = H.CD_OPR_ETRD_MCP
                                             )
                              )
             ORDER BY A.CD_CTRA_TIT_VL
                    , A.CD_OPR_ETRD_MCP
           END-EXEC.

      *-----------------------------------------------------------------
       PROCEDURE DIVISION  USING  DFHCOMMAREA.
      *-----------------------------------------------------------------

      *--> Habilita Testes no Cobolito e Debug de Operacoes IIB
-INC ALMKCBL2

           IF  EIBCALEN NOT EQUAL LENGTH OF DFHCOMMAREA
               MOVE  0220              TO  CD-RTN-PGM
               MOVE  'Area com tamanho inválido.'
                                       TO  MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF.
      *-----------------------------------------------------------------
        000000-ROTINA-PRINCIPAL                   SECTION.
      *-----------------------------------------------------------------

           MOVE  0001                       TO  PSC-ERRO-PGM

           IF  NOT EXECUCAO-ONLINE  AND
               NOT EXECUCAO-BATCH
               CALL CICS5000 USING CICS5000-COMMAREA
           END-IF

      * WHENEVER DB2
-INC HLPKE002
      * WHENEVER CICS
-INC HLPKE003

           MOVE ZEROS  TO   SGTSLE78-QT-OCR-TAB
           PERFORM  100000-VALIDAR-PARAMETRO

           PERFORM  970101-ABRE-CURSOR-PSQ

           PERFORM  970102-LER-CURSOR-PSQ

           PERFORM  970103-FECHA-CURSOR-PSQ

           PERFORM  200000-PROCESSA-PESQUISA
                    VARYING                IC-TAB
                                           FROM 0001 BY 0001
                    UNTIL                  IC-TAB
                                           GREATER  SGTSLE78-QT-OCR-TAB

           GOBACK
           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-VALIDAR-PARAMETRO        SECTION.
      *-----------------------------------------------------------------
           MOVE  0002                  TO  PSC-ERRO-PGM

           IF  SGTSLE78-CD-FUNCAO      NOT = 1
               MOVE  0110              TO  CD-RTN-PGM
               SET ERRO-CODIFICADO     TO TRUE
               SET MSGKW999-MNT-VRV    TO TRUE
               MOVE 'Tipo de função inválido.'
                                       TO  MSG-ERRO
               PERFORM  999001-ERRO-001
           END-IF

           IF  SGTSLE78-LST-ORD-APV
               IF  SGTSLE78-E-CD-TIP-EST-NEGO =  ZEROS
                   MOVE  0652                TO  CD-RTN-PGM
                   SET ERRO-CODIFICADO       TO TRUE
                   SET MSGKW999-MNT-VRV      TO TRUE
                   MOVE  'Código do tipo de estado da negociação não inf
      -                  'ormado.'           TO  MSG-ERRO
                   PERFORM 999001-ERRO-001
               END-IF
               MOVE 4095                     TO    A-TAM
               MOVE 'SBDATAF -WRKAREA-FIM'   TO    A-FIM
               IF SGTSLE78-E-DT-INI          =     SPACES
                  MOVE  0393                 TO    CD-RTN-PGM
                   SET ERRO-CODIFICADO       TO TRUE
                   SET MSGKW999-MNT-VRV      TO TRUE
                  MOVE 'Data de início do período não informada.'
                                             TO    MSG-ERRO
                  PERFORM  999001-ERRO-001
               ELSE
                  MOVE  CORR SGTSLE78-E-DT-INI-R
                                                  TO ARG01-R
                  PERFORM 101000-VALIDAR-DATA
                  IF  RETURN-CODE            EQUAL 022 OR 0024 OR 0046
                      MOVE 0394              TO    CD-RTN-PGM
                      SET ERRO-CODIFICADO    TO TRUE
                      SET MSGKW999-MNT-VRV   TO TRUE
                      MOVE 'Data de início do período informada inválida
      -                    '.'               TO    MSG-ERRO
                      PERFORM 999001-ERRO-001
                  END-IF
               END-IF

               IF SGTSLE78-E-DT-FIM          =      SPACES
                  MOVE  0395                 TO     CD-RTN-PGM
                   SET ERRO-CODIFICADO       TO TRUE
                   SET MSGKW999-MNT-VRV      TO TRUE
                  MOVE 'Data final do período não informada.'
                                             TO     MSG-ERRO
                  PERFORM  999001-ERRO-001
               ELSE
                  MOVE  CORR SGTSLE78-E-DT-FIM-R
                                              TO    ARG01-R
                  PERFORM 101000-VALIDAR-DATA
                  IF  RETURN-CODE             EQUAL 022 OR 0024 OR 0046
                      MOVE 0396               TO    CD-RTN-PGM
                      SET ERRO-CODIFICADO     TO TRUE
                     SET MSGKW999-MNT-VRV     TO TRUE
                      MOVE 'Data final do período informada inválida.'
                                              TO    MSG-ERRO
                      PERFORM 999001-ERRO-001
                  END-IF
               END-IF
           END-IF

           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       101000-VALIDAR-DATA             SECTION.
      *--> Acionar a Funcao 01 da Rotina SBDATAF e verificar se
      *    a Data efetiva da movimentação é válida
      *-----------------------------------------------------------------
           MOVE  0003                  TO  PSC-ERRO-PGM

           CALL SBDATAF                USING FUNCAO AREA-WORK ARG01
                                             ARG02
           IF  RETURN-CODE             NOT EQUAL  ZEROS  AND
                                       0022  AND  0024   AND 0046
               MOVE 0051               TO CD-RTN-PGM
               MOVE RETURN-CODE        TO RTN-SBDATA
               SET ERRO-CODIFICADO     TO TRUE
               SET MSGKW999-MNT-VRV    TO TRUE
               MOVE 'Erro no tratamento de datas.' TO MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           MOVE RETURN-CODE            TO RTN-SBDATA

           .
       101000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-PROCESSA-PESQUISA        SECTION.
      *-----------------------------------------------------------------
           MOVE  0004                  TO  PSC-ERRO-PGM

           MOVE  CD-CTRA-TIT-VL                           (IC-TAB)
             TO  SGTSLE78-CD-CTRA-TIT-VL                  (IC-TAB)
           MOVE  CD-OPR-ETRD-MCP                          (IC-TAB)
             TO  SGTSLE78-CD-OPR-ETRD-MCP                 (IC-TAB)
           MOVE  NR-VRS-OPR-ETRD                          (IC-TAB)
             TO  SGTSLE78-NR-VRS-OPR-ETRD                 (IC-TAB)
           MOVE  NR-SEQL-ORD-NEGO                         (IC-TAB)
             TO  SGTSLE78-NR-SEQL-ORD-NEGO                (IC-TAB)
           MOVE  TS-EST-ORD-NEGO                          (IC-TAB)
             TO  SGTSLE78-TS-EST-ORD-NEGO                 (IC-TAB)
           MOVE  CD-USU-ULT-ATL                           (IC-TAB)
             TO  SGTSLE78-CD-USU-ULT-ATL                  (IC-TAB)
           MOVE  CD-TIP-EST-NEGO                          (IC-TAB)
             TO  SGTSLE78-CD-TIP-EST-NEGO                 (IC-TAB)
           MOVE  DT-VND                                   (IC-TAB)
             TO  SGTSLE78-DT-VND                          (IC-TAB)
           MOVE  QT-TTL-PAPL                              (IC-TAB)
             TO  SGTSLE78-QT-TTL-PAPL                     (IC-TAB)

           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970101-ABRE-CURSOR-PSQ          SECTION.
      *-----------------------------------------------------------------
           MOVE  0005                  TO  PSC-ERRO-PGM

           MOVE  'N'                   TO  SGTSLE78-IN-CNTO-PSQ
           EVALUATE  TRUE
              WHEN  SGTSLE78-LST-ORD-APV
                    EXEC SQL
                         OPEN CURSOR-PSQ-01
                    END-EXEC
           END-EVALUATE
           MOVE  'S'                   TO  IN-CURSOR-PSQ-ABTO
           .
       970101-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970102-LER-CURSOR-PSQ           SECTION.
      *-----------------------------------------------------------------
           MOVE  0006                  TO  PSC-ERRO-PGM

           INITIALIZE                      EST-ORD-NEGO-OPR

           EVALUATE  TRUE
              WHEN  SGTSLE78-LST-ORD-APV
                    EXEC SQL
                         FETCH NEXT ROWSET
                          CURSOR-PSQ-01 FOR 0401 ROWS
                          INTO   :CD-CTRA-TIT-VL
                             ,   :CD-OPR-ETRD-MCP
                             ,   :NR-VRS-OPR-ETRD
                             ,   :NR-SEQL-ORD-NEGO
                             ,   :TS-EST-ORD-NEGO
                             ,   :CD-USU-ULT-ATL
                             ,   :CD-TIP-EST-NEGO
                             ,   :DT-VND
                                 :DT-VND-N
                             ,   :QT-TTL-PAPL
                                 :QT-TTL-PAPL-N
                    END-EXEC

           END-EVALUATE

           MOVE  SQLERRD(3)            TO  QT-REG-SELD

           IF  QT-REG-SELD             EQUAL 0401
               SET   SGTSLE78-TEM-MAIS-REGISTRO
                                       TO  TRUE
               MOVE  0400              TO  QT-REG-SELD
           END-IF

           MOVE  QT-REG-SELD           TO  SGTSLE78-QT-OCR-TAB
           MOVE  SQLCODE               TO  CD-FIM-CURSOR-PSQ
           .
       970102-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970103-FECHA-CURSOR-PSQ         SECTION.
      *-----------------------------------------------------------------
           MOVE  0007                  TO  PSC-ERRO-PGM

           MOVE  'N'                   TO  IN-CURSOR-PSQ-ABTO

           EVALUATE  TRUE
              WHEN  SGTSLE78-LST-ORD-APV
                    EXEC SQL
                         CLOSE CURSOR-PSQ-01
                    END-EXEC
           END-EVALUATE

           .
       970103-SAI.
           EXIT.

      *--------------------
      * 999000-ERRO                    SECTION
      *--------------------
      *-----------------
      * 999001-ERRO-001.
      *-----------------
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
           MOVE  SGTSLE78-TIP-RTN-PGM  TO  TIP-RTN-PGM
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
                     PERFORM  999400-MONITORAR-VARIAVEIS
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

           MOVE 'LE78-001-CD-FUNCAO'   TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSLE78-CD-FUNCAO    TO  MCR-S9-15
           MOVE  MCR-S9-15             TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'LE78-002-E-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSLE78-E-CD-CTRA-TIT-VL
                                       TO  MCR-S9-15
           MOVE  MCR-S9-15             TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE  'LE78-003-E-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
      *------> Verifica se o conteudo e numerico antes de adicionar
           IF  SGTSLE78-E-CD-OPR-ETRD-MCP  IS NUMERIC
               MOVE  SGTSLE78-E-CD-OPR-ETRD-MCP
                                       TO  MCR-S9-15
               MOVE  MCR-S9-15         TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'LE78-004-E-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSLE78-E-NR-VRS-OPR-ETRD
                                       TO  MCR-S9-15
           MOVE  MCR-S9-15             TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'LE78-005-E-NR-SEQL-ORD-NEGO'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSLE78-E-NR-SEQL-ORD-NEGO
                                       TO  MCR-S9-15
           MOVE  MCR-S9-15             TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'LE78-006-E-TS-EST-ORD-NEGO'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSLE78-E-TS-EST-ORD-NEGO
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'LE78-007-E-CD-TIP-EST-NEGO'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSLE78-E-CD-TIP-EST-NEGO
                                       TO  MCR-S9-15
           MOVE  MCR-S9-15             TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'LE78-008-E-DT-INI '   TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSLE78-E-DT-INI     TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'LE78-009-E-DT-FIM '   TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSLE78-E-DT-FIM     TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'LE78-010-IC-TAB'      TO  MSGKW999-NM-VRV-MNT
           MOVE  IC-TAB                TO  MCR-S9-15
           MOVE  MCR-S9-15             TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'LE78-011-QT-OCR-TAB'  TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSLE78-QT-OCR-TAB   TO  MCR-S9-15
           MOVE  MCR-S9-15                    TO  MSGKW999-VL-VRV-MNT
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

           PERFORM  VARYING  IC-TAB-MNT
                    FROM  0001 BY 0001
                    UNTIL  IC-TAB-MNT      GREATER 0012
                    IF  HLPKE001-TAB-ERRO-TX(IC-TAB-MNT)
                                           NOT EQUAL SPACES
                        STRING  'DSNTIAR-TX-ERRO(' IC-TAB-MNT ')'
                                DELIMITED BY SIZE
                                INTO       MSGKW999-NM-VRV-MNT
                        END-STRING
                        MOVE  HLPKE001-TAB-ERRO-TX(IC-TAB-MNT)
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
       999400-MONITORAR-VARIAVEIS       SECTION.
      *--> Monitora as variáveis dos módulos chamados
      *-----------------------------------------------------------------

           EVALUATE  PGM-ERRO
               WHEN  SBDATAF
                    MOVE  'FUNCAO'     TO  MSGKW999-NM-VRV-MNT
                    MOVE  FUNCAO       TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999500-ADICIONA-VRV-MNT

                    MOVE  'A-TAM'      TO  MSGKW999-NM-VRV-MNT
                    MOVE   A-TAM       TO  MCR-S9-15
                    MOVE  MCR-S9-15    TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999500-ADICIONA-VRV-MNT

                    MOVE  'A-FIM'      TO  MSGKW999-NM-VRV-MNT
                    MOVE  A-FIM        TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999500-ADICIONA-VRV-MNT

                    MOVE 'ARG01'       TO  MSGKW999-NM-VRV-MNT
                    MOVE  ARG01        TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999500-ADICIONA-VRV-MNT

                    MOVE 'ARG02'       TO  MSGKW999-NM-VRV-MNT
                    MOVE  ARG02        TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999500-ADICIONA-VRV-MNT

                    MOVE 'RETURN-CODE' TO  MSGKW999-NM-VRV-MNT
                    MOVE  RTN-SBDATA   TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999500-ADICIONA-VRV-MNT

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
      *    Fim do Tratamento através do Sistema MSG (rotina MSGS0001)
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       999999-ENCERRAR                 SECTION.
      *-----------------------------------------------------------------

           IF  CURSOR-PSQ-ABTO
               PERFORM 970103-FECHA-CURSOR-PSQ
           END-IF

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

           MOVE RTN-PGM                TO SGTSLE78-RTN-PGM

           IF  EXECUCAO-ONLINE  AND   MSGKW999-MNT-VRV
               PERFORM  999000-TRATAR-ERRO-MSG
           END-IF

           IF  EXECUCAO-BATCH
               IF   RTN-SBDATA      GREATER    ZEROS
                    MOVE RTN-SBDATA TO         CD-RTN-PGM
               END-IF
           END-IF

           GOBACK
           .
       999999-SAI.
           EXIT.
      *---------------------- FIM SGTSLE78 ----------------------------