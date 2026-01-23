      *-----------------------------------------------------------------
      * Programa..: SGTSL029
      * Analista..: F2746899 - Eduardo Pio Ribeiro Junior
      * Autor.....: F2746899 - Eduardo Pio Ribeiro Junior
      * Objetivo..: Consulta Tabela DB2SGT.CTGR_BC_TVM
      * Compilacao: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS002 02/05/2019 - F8650584 - DE 2016/145195
      *                     Inclusão de Mensageria e atualização no
      *                     Tratamento de Ocorrências de Erros
      * VRS001 15/07/2015 - F2746899 - DE 2013/120821 - Implantação
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTSL029.

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
       77  CTE-PROG                    PIC  X(17) VALUE
                                       '*** SGTSL029 ***'.
       77  CTE-VERS                    PIC  X(06) VALUE 'VRS002'.

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

      *--> Area de trabalho
       01  QT-REG-SELD                 PIC S9(04) COMP-5 VALUE 0.
       01  AUX-NM-CTGR-TVM-E           PIC  X(40) VALUE ' '.
       01  PSC-FIM                     PIC S9(04) COMP-5  VALUE 0.
       01  K-ZERO                      PIC S9(04) COMP-5  VALUE 0.

      *--> Campos utilizados para leitura
       01  CTGR-BC-TVM.
           03  CD-CTGR-TVM             PIC S9(04) COMP-5
                                       OCCURS 101 TIMES  VALUE ZEROS.
           03  NM-CTGR-TVM             PIC  X(40)
                                       OCCURS 101 TIMES  VALUE SPACES.
           03  IN-CTGR-TVM             PIC X(01)
                                       OCCURS 101 TIMES  VALUE SPACES.
           03  TS-ATL                  PIC X(26)
                                       OCCURS 101 TIMES  VALUE SPACES.
           03  CD-USU-ATL              PIC X(08)
                                       OCCURS 101 TIMES  VALUE SPACES.

      *--> Indicadores de nulidades
       01  QT-REG-SELD-N               PIC S9(4) COMP-5 VALUE 0.

      *--> Variavel(is) de controle do cursor(es)
       01  CTL-CURSOR-PSQ.
           03  CD-FIM-CURSOR-PSQ       PIC S9(04) COMP-5 VALUE 0.
               88  FIM-CURSOR-PSQ                        VALUE +100.
           03  IN-CURSOR-PSQ-ABTO      PIC  X(01) VALUE 'N'.
               88  CURSOR-PSQ-ABTO                VALUE 'S'.

      *--> Book das variáveis auxiliares do tratamento de erro
-INC HLPKE001

      *-----------------------------------------------------------------
      *    Areas e Books Utilizados no Tratamento da Mensageria
      *-----------------------------------------------------------------
       01  IC-TAB                      PIC  9(02) VALUE ZEROS.

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

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

      *-----------------------------------------------------------------
       LINKAGE                         SECTION.
      *-----------------------------------------------------------------
       01  DFHCOMMAREA.
-INC SGTKL029

      *---> Cursor 01
      *    Listar todas Categorias TVM do BACEN
      *    e a rechamada feita pelo ultimo Codigo da Categoria
           EXEC SQL
             DECLARE CURSOR-PSQ-1 CURSOR
                     WITH ROWSET POSITIONING FOR
             SELECT  A.CD_CTGR_TVM
                  ,  A.NM_CTGR_TVM
                  ,  A.IN_CTGR_TVM
                  ,  A.TS_ATL
                  ,  A.CD_USU_ATL
               FROM  DB2SGT.CTGR_BC_TVM    A
              WHERE  A.CD_CTGR_TVM >  :SGTSL029-CD-CTGR-TVM-E
            ORDER BY A.CD_CTGR_TVM     ASC
           END-EXEC.

      *---> Cursor 02
      *    Listar as Categorias TVM do BACEN a partir
      *    de um Timestamp de Entrada
           EXEC SQL
             DECLARE CURSOR-PSQ-2 CURSOR
                     WITH ROWSET POSITIONING FOR
             SELECT  A.CD_CTGR_TVM
                  ,  A.NM_CTGR_TVM
                  ,  A.IN_CTGR_TVM
                  ,  A.TS_ATL
                  ,  A.CD_USU_ATL
               FROM  DB2SGT.CTGR_BC_TVM    A
              WHERE
                     A.TS_ATL      >  :SGTSL029-TS-ATL-E
            ORDER BY A.CD_CTGR_TVM     ASC
           END-EXEC.

      *---> Cursor 03
      *    Listar as Categorias TVM do BACEN a partir de parte de um
      *    Nome da Categoria de entrada
           EXEC SQL
             DECLARE CURSOR-PSQ-3 CURSOR
                     WITH ROWSET POSITIONING FOR
             SELECT  A.CD_CTGR_TVM
                  ,  A.NM_CTGR_TVM
                  ,  A.IN_CTGR_TVM
                  ,  A.TS_ATL
                  ,  A.CD_USU_ATL
               FROM  DB2SGT.CTGR_BC_TVM    A
              WHERE  A.CD_CTGR_TVM  >  :SGTSL029-CD-CTGR-TVM-E
               AND   A.NM_CTGR_TVM  LIKE  :SGTSL029-NM-CTGR-TVM-E || '%'
           ORDER BY  A.CD_CTGR_TVM  DESC
           END-EXEC.

      *-----------------------------------------------------------------
       PROCEDURE                       DIVISION USING DFHCOMMAREA.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------

           IF  EIBCALEN NOT EQUAL LENGTH OF DFHCOMMAREA
               MOVE  0001              TO  PSC-ERRO-PGM
               MOVE  0220              TO  CD-RTN-PGM
               MOVE  'Área com tamanho inválido - verifique book/local.'
                                       TO  MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           MOVE  0001                  TO  PSC-ERRO-PGM

           IF  NOT EXECUCAO-ONLINE  AND
               NOT EXECUCAO-BATCH
               CALL CICS5000 USING CICS5000-COMMAREA
           END-IF

      *--> Habilita Testes no Cobolito
-INC ALMKCBL2

      *--> WHENEVER DB2
-INC HLPKE002

      *--> WHENEVER CICS
-INC HLPKE003

           PERFORM  100000-VALIDAR-PARAMETRO

           PERFORM  970101-ABRE-CURSOR-PSQ
           PERFORM  970102-LER-CURSOR-PSQ
           PERFORM  970103-FECHA-CURSOR-PSQ

           PERFORM  200000-PROCESSA-PESQUISA
                    VARYING SGTSL029-IC-TAB
                    FROM 0001 BY 0001
                    UNTIL SGTSL029-IC-TAB > SGTSL029-QT-OCR-TAB

           GOBACK
           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-VALIDAR-PARAMETRO        SECTION.
      *-----------------------------------------------------------------
           MOVE  0001                  TO  PSC-ERRO-PGM

           IF  SGTSL029-TS-ATL-E       EQUAL  SPACES
               MOVE  '9999-12-31-23.59.59.000000'
                                       TO  SGTSL029-TS-ATL-E
           END-IF

           IF  SGTSL029-NM-CTGR-TVM-E  NOT EQUAL  SPACES
               PERFORM 110000-RETIRA-ESPAC-INI-DESCR
           END-IF

           MOVE  'N'                   TO  SGTSL029-IN-CNTO-PSQ
           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-RETIRA-ESPAC-INI-DESCR   SECTION.
      *-----------------------------------------------------------------
           MOVE  0002                  TO  PSC-ERRO-PGM

           INSPECT  SGTSL029-NM-CTGR-TVM-E
                    TALLYING PSC-FIM
                    FOR LEADING SPACES

           MOVE  SGTSL029-NM-CTGR-TVM-E(PSC-FIM + 1:40 - PSC-FIM)
                                       TO  AUX-NM-CTGR-TVM-E

           MOVE  SPACES                TO  SGTSL029-NM-CTGR-TVM-E

           MOVE  AUX-NM-CTGR-TVM-E     TO  SGTSL029-NM-CTGR-TVM-E

           .
       110000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-PROCESSA-PESQUISA        SECTION.
      *-----------------------------------------------------------------
           MOVE  0003                  TO  PSC-ERRO-PGM

           MOVE  CD-CTGR-TVM                           (SGTSL029-IC-TAB)
             TO  SGTSL029-CD-CTGR-TVM                  (SGTSL029-IC-TAB)

           MOVE  NM-CTGR-TVM                           (SGTSL029-IC-TAB)
             TO  SGTSL029-NM-CTGR-TVM                  (SGTSL029-IC-TAB)

           MOVE  IN-CTGR-TVM                           (SGTSL029-IC-TAB)
             TO  SGTSL029-IN-CTGR-TVM                  (SGTSL029-IC-TAB)

           MOVE  TS-ATL                                (SGTSL029-IC-TAB)
             TO  SGTSL029-TS-ATL                       (SGTSL029-IC-TAB)

           MOVE  CD-USU-ATL                            (SGTSL029-IC-TAB)
             TO  SGTSL029-CD-USU-ATL                   (SGTSL029-IC-TAB)
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970101-ABRE-CURSOR-PSQ          SECTION.
      *-----------------------------------------------------------------
           MOVE  0004                  TO  PSC-ERRO-PGM

           INITIALIZE                  SGTSL029-AREA-DADOS

           EVALUATE  TRUE
               WHEN  SGTSL029-LISTA-TOTAL-CD
                     EXEC SQL
                         OPEN CURSOR-PSQ-1
                     END-EXEC
               WHEN  SGTSL029-LISTA-TOTAL-TS
                     EXEC SQL
                         OPEN CURSOR-PSQ-2
                     END-EXEC
               WHEN  SGTSL029-LISTA-NOME
                     EXEC SQL
                         OPEN CURSOR-PSQ-3
                     END-EXEC
           END-EVALUATE

           MOVE  'S'                   TO  IN-CURSOR-PSQ-ABTO
           .
       970101-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970102-LER-CURSOR-PSQ           SECTION.
      *-----------------------------------------------------------------
           MOVE  0005                  TO  PSC-ERRO-PGM

           INITIALIZE                      CTGR-BC-TVM
           MOVE  'N'                   TO  SGTSL029-IN-CNTO-PSQ

           IF  SGTSL029-LISTA-TOTAL-CD
               EXEC SQL
                  FETCH NEXT ROWSET
                       CURSOR-PSQ-1 FOR 0101 ROWS
                  INTO :CD-CTGR-TVM
                     , :NM-CTGR-TVM
                     , :IN-CTGR-TVM
                     , :TS-ATL
                     , :CD-USU-ATL
               END-EXEC
           END-IF

           IF  SGTSL029-LISTA-TOTAL-TS
               EXEC SQL
                  FETCH NEXT ROWSET
                       CURSOR-PSQ-2 FOR 0101 ROWS
                  INTO :CD-CTGR-TVM
                     , :NM-CTGR-TVM
                     , :IN-CTGR-TVM
                     , :TS-ATL
                     , :CD-USU-ATL
               END-EXEC
           END-IF

           IF  SGTSL029-LISTA-NOME
               EXEC SQL
                  FETCH NEXT ROWSET
                       CURSOR-PSQ-3 FOR 0101 ROWS
                  INTO :CD-CTGR-TVM
                     , :NM-CTGR-TVM
                     , :IN-CTGR-TVM
                     , :TS-ATL
                     , :CD-USU-ATL
               END-EXEC
           END-IF

           MOVE  SQLERRD(3)            TO  QT-REG-SELD

           IF  QT-REG-SELD                 EQUAL  0101
               SET   SGTSL029-TEM-MAIS-REGISTRO
                                       TO  TRUE
               MOVE  0100              TO  QT-REG-SELD
           END-IF

           MOVE  QT-REG-SELD           TO  SGTSL029-QT-OCR-TAB
           MOVE  SQLCODE               TO  CD-FIM-CURSOR-PSQ
           .
       970102-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970103-FECHA-CURSOR-PSQ         SECTION.
      *-----------------------------------------------------------------
           MOVE  0006                  TO  PSC-ERRO-PGM

           MOVE  'N'                   TO  IN-CURSOR-PSQ-ABTO

           EVALUATE  TRUE
               WHEN  SGTSL029-LISTA-TOTAL-CD
                     EXEC SQL
                         CLOSE CURSOR-PSQ-1
                     END-EXEC
               WHEN  SGTSL029-LISTA-TOTAL-TS
                     EXEC SQL
                         CLOSE CURSOR-PSQ-2
                     END-EXEC
               WHEN  SGTSL029-LISTA-NOME
                     EXEC SQL
                         CLOSE CURSOR-PSQ-3
                     END-EXEC
           END-EVALUATE
           .
       970103-SAI.
           EXIT.

      *-----------------------------------------------------------------
      * 999000-ERRO SECTION
      *-----------------------------------------------------------------
      *----------------------------
      * 999001-ERRO-001.
      *----------------------------
-INC HLPKE004
-INC HLPKE012

           IF  CURSOR-PSQ-ABTO
               PERFORM 970103-FECHA-CURSOR-PSQ
           END-IF

      *--> 0001 - Erro de db2
           IF  TIP-RTN-PGM                 EQUAL  0001
               IF  CD-RTN-PGM              NOT EQUAL  +100  AND  -803
                   MOVE  9997          TO  CD-RTN-PGM
               END-IF
           END-IF

      *--> 0002 - Erro cics
           IF  TIP-RTN-PGM                 EQUAL  0002
               IF  CD-RTN-PGM              EQUAL  ZEROS  OR  0333
                   MOVE  9999          TO  CD-RTN-PGM
               END-IF
           END-IF

           PERFORM  999999-ENCERRAR
           .
       999001-SAI.
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
      *    As Sections e Books abaixo fazem o tratamento de erro
      *    através do Sistema MSG (rotina MSGS0001)
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       999000-TRATAR-ERRO-MSG          SECTION.
      *--> Envia a Ocorrência de Erro para o Sistema de Mensageria
      *-----------------------------------------------------------------

      *--> Alterar o MOVE conforme o Book do módulo
           MOVE  SGTSL029-TIP-RTN-PGM  TO  TIP-RTN-PGM
      *--> Alterar os Campos de Entrada conforme o Book do módulo
           PERFORM  999100-MONITORAR-ENTRADA

      *--> Movimenta Campos Básicos da Ocorrência de Erro
           MOVE  CD-RTN-PGM            TO  MSG1-CD-ERRO-TS
           MOVE  PSC-ERRO-PGM          TO  MSG1-CD-SEQL-ERRO
           MOVE  MSG-ERRO              TO  MSG1-TX-ERRO-TS

      *--> Realiza a tradução do tipo de erro do COMMAREA
      *     para tipos de erro da Mensageria
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
           MOVE 'L029-001-ACAO'        TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL029-ACAO         TO  MCR-S9-15
           MOVE  MCR-S9-15             TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'L029-002-CD-CTGR-TVM-E'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL029-CD-CTGR-TVM-E
                                       TO MCR-S9-15
           MOVE  MCR-S9-15             TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'L029-003-TS-ATL-E'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL029-TS-ATL-E
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'L029-004-NM-CTGR-TVM-E'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL029-NM-CTGR-TVM-E
                                       TO  MSGKW999-VL-VRV-MNT
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
       999400-MONITORAR-VARIAVEIS       SECTION.
      *--> Monitora as variáveis dos módulos chamados
      *-----------------------------------------------------------------

           EVALUATE  PGM-ERRO
               WHEN  SBTIMEDT
                     MOVE  'HLPKE001-CD-FUC'     TO  MSGKW999-NM-VRV-MNT
                     MOVE  HLPKE001-CD-FUC       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE  'HLPKE001-AREA-TRB'   TO  MSGKW999-NM-VRV-MNT
                     MOVE  HLPKE001-AREA-TRB     TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE  'HLPKE001-FMT-TS'     TO  MSGKW999-NM-VRV-MNT
                     MOVE  HLPKE001-FMT-TS       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE  'HLPKE001-TAM-TS'     TO  MSGKW999-NM-VRV-MNT
                     MOVE  HLPKE001-TAM-TS       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE  'HLPKE001-TS-FMTD'    TO  MSGKW999-NM-VRV-MNT
                    MOVE  HLPKE001-TS-FMTD       TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999500-ADICIONA-VRV-MNT

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

      *----------------------------------------
       999999-ENCERRAR                 SECTION.
      *----------------------------------------

           MOVE  RTN-PGM               TO  SGTSL029-RTN-PGM

           IF  EXECUCAO-ONLINE
               PERFORM  999000-TRATAR-ERRO-MSG
           END-IF

           GOBACK
           .
       999999-SAI.
           EXIT.

      *------------------------- FIM SGTSL029 --------------------------