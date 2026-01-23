      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSL045
      * ANALISTA..: F2054109 - CINTIA AP SOARES IGLESIAS
      * AUTOR.....: F8779682 - ROSA APARECIDA HENRIQUES FOZ
      * DATA......: 09.11.2015
      * OBJETIVO..: LISTAR TIPO DE NEGOCIACAO - TAB DB2SGT.TIP_NEGO
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS002 15.07.2019 - F8785286 - DE 2016/145195 - Inclusão SERV-2.
      *        02.04.2018 - F9188847 - DE 2016/145195 - Inclusão campos:
      *         - SGTSL045-E-CD-CTRA-TIT-VL, SGTSL045-CD-CTRA-TIT-VL
      *         - Adequação para Listar todas Carteiras ou específica
      * VRS001 05.11.2015 - F8779682 DE 2013/120821 - IMPLANTACAO
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSL045.

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
       01  CTE-PROG     PIC  X(17) VALUE '*** SGTSL045 ***'.
       01  CTE-VERS     PIC  X(06) VALUE 'VRS002'.

      *-----------------------------------------------------------------
      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informacoes
       COPY HLPKE01B.
       LOCAL-STORAGE SECTION.
      *-----------------------------------------------------------------
      * Area de trabalho

       01  IC-TAB                      PIC  9(04)       VALUE 0.
       01 QT-REG-SELD                  PIC S9(4) COMP-5 VALUE 0.

      * Campos utilizados para leitura

       01  REG-WS.
           03 CD-CTRA-TIT-VL                 PIC S9(9) COMP-5
              OCCURS 101 TIMES               VALUE 0.
           03 CD-TIP-NEGO                    PIC S9(4) COMP-5
              OCCURS 101 TIMES               VALUE 0.
           03 CD-UOR-RMTE                    PIC S9(9) COMP-5
              OCCURS 101 TIMES               VALUE 0.
           03 CD-UOR-RMTE-N                  PIC S9(4) COMP-5
              OCCURS 101 TIMES               VALUE 0.
           03 NM-TIP-NEGO                    PIC X(40)
              OCCURS 101 TIMES               VALUE ' '.


      * Indicadores de nulidades

       01 QT-REG-SELD-N                       PIC S9(4) COMP-5 VALUE 0.

      * Variavel(is) de controle do cursor(es)

       01  CTL-CURSOR-PSQ.
           03  CD-FIM-CURSOR-PSQ            PIC S9(04) COMP-5 VALUE 0.
               88  FIM-CURSOR-PSQ            VALUE +100.
           03  IN-CURSOR-PSQ-ABTO           PIC  X(01) VALUE 'N'.
               88  CURSOR-PSQ-ABTO          VALUE 'S'.

       01  CTL-PSQ-TAB.
           03  WS-CD-CTRA-TIT-VL-INI        PIC S9(09) COMP-5 VALUE 0.
           03  WS-CD-CTRA-TIT-VL-FIM        PIC S9(09) COMP-5 VALUE 0.
           03  WS-CD-TIP-TIP-NEGO-INI       PIC S9(04) COMP-5 VALUE 0.
           03  WS-CD-TIP-TIP-NEGO-FIM       PIC S9(04) COMP-5 VALUE 0.

      * Subrotinas utilizadas
      * Books da definicao de dados a serem usados na subrotina de erros
       COPY HLPKE001.
      *--> Book para captura de erro via MSG1
       COPY MSGKWMG1.
      *--> Variaveis para tratamento de CONTAINER
       COPY SGTKERR1.

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

      *--> Máscaras edição de variáveis p/ monitoração mensagens de
       01  MCR-S9-15                   PIC      ---------------9.

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

      *-----------------------------------------------------------------
       LINKAGE SECTION.
      *-----------------------------------------------------------------
       01  DFHCOMMAREA.
       COPY SGTKL045.
           EXEC SQL
             DECLARE CURSOR-PSQ CURSOR
                     WITH ROWSET POSITIONING FOR
             SELECT A.CD_CTRA_TIT_VL
                  , A.CD_TIP_NEGO
                  , A.CD_UOR_RMTE
                  , A.NM_TIP_NEGO
               FROM DB2SGT.TIP_NEGO    A
              WHERE A.CD_CTRA_TIT_VL >= :WS-CD-CTRA-TIT-VL-INI
                AND A.CD_CTRA_TIT_VL <= :WS-CD-CTRA-TIT-VL-FIM
                AND A.CD_TIP_NEGO    >= :WS-CD-TIP-TIP-NEGO-INI
                AND A.CD_TIP_NEGO    <= :WS-CD-TIP-TIP-NEGO-FIM
               ORDER BY A.CD_CTRA_TIT_VL, A.CD_TIP_NEGO
           END-EXEC.

      *-----------------------------------------------------------------
      * Lista Ordem de Negociação Ativa
      *-----------------------------------------------------------------
           EXEC SQL
             DECLARE CURSOR-PSQ-2 CURSOR
                     WITH ROWSET POSITIONING FOR
             SELECT A.CD_CTRA_TIT_VL
                  , A.CD_TIP_NEGO
                  , A.CD_UOR_RMTE
                  , A.NM_TIP_NEGO
               FROM DB2SGT.TIP_NEGO    A
               INNER JOIN DB2SGT.EST_TIP_NEGO  B
               ON  B.CD_CTRA_TIT_VL  =  A.CD_CTRA_TIT_VL
               AND B.CD_TIP_NEGO      =  A.CD_TIP_NEGO

              WHERE A.CD_CTRA_TIT_VL >= :WS-CD-CTRA-TIT-VL-INI
                AND A.CD_CTRA_TIT_VL <= :WS-CD-CTRA-TIT-VL-FIM
                AND A.CD_TIP_NEGO    >= :WS-CD-TIP-TIP-NEGO-INI
                AND A.CD_TIP_NEGO    <= :WS-CD-TIP-TIP-NEGO-FIM
                AND B.TS_EST_TIP_NEGO = (SELECT MAX (C.TS_EST_TIP_NEGO)
                   FROM  DB2SGT.EST_TIP_NEGO C
                       WHERE C.CD_CTRA_TIT_VL     = A.CD_CTRA_TIT_VL
                        AND  C.CD_TIP_NEGO    = A.CD_TIP_NEGO
                        AND  C.CD_TIP_EST_DCS = 2 )

               ORDER BY A.CD_CTRA_TIT_VL, A.CD_TIP_NEGO
           END-EXEC.

      *-----------------------------------------------------------------
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *-----------------------------------------------------------------

      *--> teste obrigatorio do tamanho da linkage nao retirar
           IF  EIBCALEN NOT EQUAL LENGTH OF DFHCOMMAREA
               SET ERRO-CODIFICADO     TO TRUE
               MOVE 0220               TO  CD-RTN-PGM
               MOVE '?rea com tamanho inválido - verifique book/local.'
                                       TO MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           MOVE 0001                   TO PSC-ERRO-PGM

           IF  NOT EXECUCAO-ONLINE AND NOT EXECUCAO-BATCH
             CALL CICS5000 USING CICS5000-COMMAREA
           END-IF

      * WHENEVER DB2
       COPY HLPKE002.
      * WHENEVER CICS
       COPY HLPKE003.

           PERFORM 100000-VALIDAR-PARAMETRO

           EVALUATE TRUE
               WHEN SGTSL045-LISTA-TIP
               WHEN SGTSL045-LISTA-TIP-ATV
                    PERFORM 970101-ABRE-CURSOR-PSQ
                    PERFORM 970102-LER-CURSOR-PSQ
                    PERFORM 970103-FECHA-CURSOR-PSQ
           END-EVALUATE

           PERFORM 200000-PROCESSA-PESQUISA
                    VARYING SGTSL045-IC-TAB FROM 1 BY 1
                    UNTIL SGTSL045-IC-TAB > SGTSL045-QT-OCR-TAB

           GOBACK
           .
      *-----------------------------------------------------------------
       100000-VALIDAR-PARAMETRO SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO PSC-ERRO-PGM
           INITIALIZE SGTSL045-AREA-DADOS
           MOVE 'N'                    TO SGTSL045-IN-CNTO-PSQ

           IF  NOT  SGTSL045-LISTA-TIP AND
               NOT  SGTSL045-LISTA-TIP-ATV
               SET ERRO-CODIFICADO     TO TRUE
               MOVE  0116              TO  CD-RTN-PGM
               MOVE 'Ação inválida.'   TO  MSG-ERRO-PGM
               PERFORM 999001-ERRO-001
           END-IF

           IF  SGTSL045-LISTA-TIP      OR  SGTSL045-LISTA-TIP-ATV
             IF  SGTSL045-E-CD-CTRA-TIT-VL EQUAL ZEROS
               MOVE ZEROS              TO  WS-CD-CTRA-TIT-VL-INI
               MOVE  999999999         TO  WS-CD-CTRA-TIT-VL-FIM
             ELSE
               MOVE SGTSL045-E-CD-CTRA-TIT-VL
                                       TO  WS-CD-CTRA-TIT-VL-INI
                                           WS-CD-CTRA-TIT-VL-FIM
             END-IF

             IF  SGTSL045-E-CD-CTRA-TIT-VL     < ZEROS
               SET ERRO-CODIFICADO     TO TRUE
               MOVE  0001              TO  CD-RTN-PGM
               MOVE  'Código da carteira de título e valores mobiliári
      -              ' inválido.'      TO  MSG-ERRO
               PERFORM 999001-ERRO-001
             END-IF

             IF  SGTSL045-CD-ULT-TIP-NEGO  EQUAL ZEROS
               MOVE ZEROS              TO  WS-CD-TIP-TIP-NEGO-INI
               MOVE  9999              TO  WS-CD-TIP-TIP-NEGO-FIM
             ELSE
               MOVE SGTSL045-CD-ULT-TIP-NEGO
                                       TO  WS-CD-TIP-TIP-NEGO-INI
                                           WS-CD-TIP-TIP-NEGO-FIM
             END-IF

             IF  SGTSL045-CD-ULT-TIP-NEGO  <  ZEROS
               SET ERRO-CODIFICADO     TO TRUE
               MOVE  0716              TO  CD-RTN-PGM
               MOVE  'Código do tipo de negociação inválido'
                                       TO MSG-ERRO
               PERFORM 999001-ERRO-001
             END-IF
           END-IF
           .
       100000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       200000-PROCESSA-PESQUISA SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                   TO PSC-ERRO-PGM

           MOVE CD-CTRA-TIT-VL(SGTSL045-IC-TAB) TO
                     SGTSL045-CD-CTRA-TIT-VL(SGTSL045-IC-TAB)
           MOVE CD-TIP-NEGO(SGTSL045-IC-TAB) TO
                     SGTSL045-CD-TIP-NEGO(SGTSL045-IC-TAB)
           MOVE CD-UOR-RMTE(SGTSL045-IC-TAB) TO
                     SGTSL045-CD-UOR-RMTE(SGTSL045-IC-TAB)
           MOVE NM-TIP-NEGO(SGTSL045-IC-TAB) TO
                     SGTSL045-NM-TIP-NEGO(SGTSL045-IC-TAB)
           .
       200000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970101-ABRE-CURSOR-PSQ SECTION.
      *-----------------------------------------------------------------
           MOVE 0004                   TO PSC-ERRO-PGM

           EVALUATE SGTSL045-ACAO
           WHEN 1
               EXEC SQL
                   OPEN CURSOR-PSQ
               END-EXEC

           WHEN 2
               EXEC SQL
                   OPEN CURSOR-PSQ-2
               END-EXEC
           END-EVALUATE

           MOVE 'S'                    TO IN-CURSOR-PSQ-ABTO
           .
       970101-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970102-LER-CURSOR-PSQ SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                   TO PSC-ERRO-PGM

           INITIALIZE REG-WS

           EVALUATE SGTSL045-ACAO
           WHEN 1
           EXEC SQL
                FETCH NEXT ROWSET
                      CURSOR-PSQ FOR 0101 ROWS
               INTO :CD-CTRA-TIT-VL
                  , :CD-TIP-NEGO
                  , :CD-UOR-RMTE
                    :CD-UOR-RMTE-N
                  , :NM-TIP-NEGO
           END-EXEC

           WHEN 2
           EXEC SQL
                FETCH NEXT ROWSET
                      CURSOR-PSQ-2 FOR 0101 ROWS
               INTO :CD-CTRA-TIT-VL
                  , :CD-TIP-NEGO
                  , :CD-UOR-RMTE
                    :CD-UOR-RMTE-N
                  , :NM-TIP-NEGO
           END-EXEC
           END-EVALUATE

           MOVE SQLERRD(3)             TO QT-REG-SELD

           IF  QT-REG-SELD EQUAL 0101
               SET SGTSL045-TEM-MAIS-REGISTRO TO TRUE
               MOVE 0075                TO QT-REG-SELD
           END-IF

           MOVE QT-REG-SELD            TO SGTSL045-QT-OCR-TAB
           MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ
           .
       970102-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970103-FECHA-CURSOR-PSQ SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO PSC-ERRO-PGM
           MOVE 'N'                    TO IN-CURSOR-PSQ-ABTO
           EVALUATE SGTSL045-ACAO
           WHEN 1
               EXEC SQL
                   CLOSE CURSOR-PSQ
               END-EXEC
           WHEN 2
               EXEC SQL
                   CLOSE CURSOR-PSQ-2
               END-EXEC
           END-EVALUATE
           .
       970103-SAI.
           EXIT.

      *--------------------
      * 999000-ERRO                    SECTION
      *--------------------
      * 999001-ERRO-001.
      *--------------------
       COPY HLPKE004.
       COPY HLPKE012.

           PERFORM  999999-ENCERRAR
           .
       999001-SAI.
           EXIT.

      *-----------------
      * 999002-ERRO-DB2
      *-----------------
       COPY HLPKE005.
      *-----------------
      * 999003-TRATA-ABEND-CICS
      * 999004-TRATA-CONDITION-CICS
      *-----------------
       COPY HLPKE006.
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
           MOVE  SGTSL045-TIP-RTN-PGM  TO  TIP-RTN-PGM
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

           MOVE 'SGTSL045-ACAO'                  TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL045-ACAO                   TO  MCR-S9-15
           MOVE  MCR-S9-15                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTSL045-E-CD-CTRA-TIT-VL'      TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL045-E-CD-CTRA-TIT-VL       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTSL045-CD-ULT-TIP-NEGO'       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL045-CD-ULT-TIP-NEGO        TO  MCR-S9-15
           MOVE  MCR-S9-15                       TO  MSGKW999-VL-VRV-MNT
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
       COPY MSGKPMG1.

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
           MOVE  RTN-PGM               TO  SGTSL045-RTN-PGM

           IF  EXECUCAO-ONLINE
               PERFORM  999000-TRATAR-ERRO-MSG
           END-IF

           GOBACK
           .
       999999-SAI.
           EXIT.
      *---------------------- FIM SGTSL045 ----------------------------
