      *-----------------------------------------------------------------
      * PROGRAMA..: SGTST036
      * ANALISTA..: F2054109 - CINTIA AP SOARES IGLESIAS
      * AUTOR.....: F2804391 - ELIANA TEMPERANI
      * DATA......: 12.11.2015
      * OBJETIVO..: PERSISTENCIA DA TABELA TIP_ATB_VCLD_CTRA
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS003 21/02/2019 F9188847 DE 2016/145195 - Inclusão na section
      *        999001-ERRO-001, '1 - Erro de db2' e '2 - erro cics' E
      *        adequação das mensagens de erro ao padrão da mensageria
      * VRS002 14/02/2017 F8785286 DE 2016/145195 - Inclusão movimen-
      *              tação campo book para chave log.
      * VRS001 12.11.2015 - F2804391 - DE 2013/120821 - IMPLANTACAO
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTST036.

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
       01  CTE-PROG     PIC  X(17) VALUE '*** SGTST036 ***'.
       01  CTE-VERS     PIC  X(06) VALUE 'VRS003'.

      * Registro da tabela

       01  REG-CACHE.
           03  CHAVE-CACHE.
               05  CD-TIP-ATB-CTRA           PIC S9(4) COMP-5 VALUE 0.
               05  FILLER                    PIC  X(20) VALUE ' '.
           03  DADOS-CACHE.
               05  CD-TIP-ATB-CRCT           PIC S9(4) COMP-5 VALUE 0.
               05  TX-TIP-ATB-CTRA           PIC  X(100) VALUE ' '.
               05  TX-RTL-ATB-CTRA           PIC  X(60) VALUE ' '.
               05  TX-CMPT-ATB-CTRA-GRP.
                   49 TX-CMPT-ATB-CTRA-LEN   PIC S9(04) COMP-5 VALUE 0.
                   49 TX-CMPT-ATB-CTRA       PIC  X(450) VALUE ' '.
               05  IN-CRCT-GLB               PIC  X.

      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informacoes
-INC HLPKE01B                                                           00000510
      *-----------------------------------------------------------------
       LOCAL-STORAGE SECTION.
      *-----------------------------------------------------------------
      * Area de trabalho
       01  IC-TAB                      PIC  9(04)       VALUE 0.

       01  QT-REG-SELD                 PIC S9(4) COMP-5 VALUE 0.

      * Indicadores de nulidades

       01  QT-REG-SELD-NULO            PIC S9(4) COMP-5 VALUE 0.

      * Variaveis para gravacao do log
       01  DADOS-LOG-DISPLAY.
           03  CD-TIP-ATB-CTRA-LOG     PIC  9(5) VALUE 0.

      * Book dos dados do log
       01  DADOS-LOG.
-INC SGTK014G                                                           00000700

      * Subrotinas Utilizadas

       01  SGTS014G VALUE 'SGTS014G'         PIC X(8).
      * Books da definicao de dados a serem usados na subrotina de erros
-INC HLPKE001                                                           00000760
      *--> Book para captura de erro via MSG1
-INC MSGKWMG1
      *--> Variaveis para tratamento de CONTAINER
-INC SGTKERR1

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

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

      *-----------------------------------------------------------------
       LINKAGE SECTION.
      *-----------------------------------------------------------------
       01  DFHCOMMAREA.
-INC SGTKT036                                                           00000860
      *-----------------------------------------------------------------
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *-----------------------------------------------------------------

           IF  EIBCALEN NOT EQUAL LENGTH OF DFHCOMMAREA
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE 0220               TO  CD-RTN-PGM
               MOVE '?rea com tamanho inválido - verifique book/local.'
                                       TO MSG-ERRO
               SET MSGKW999-MNT-VRV    TO TRUE
               PERFORM 999001-ERRO-001
           END-IF

           MOVE 0001                   TO PSC-ERRO-PGM

           IF NOT EXECUCAO-ONLINE AND NOT EXECUCAO-BATCH
             CALL CICS5000 USING CICS5000-COMMAREA
           END-IF
      * WHENEVER DB2
-INC HLPKE002                                                           00001000
      * WHENEVER CICS
-INC HLPKE003                                                           00001020

           PERFORM 100000-VALIDA-PARAMETRO

           EVALUATE TRUE
              WHEN SGTST036-INCLUIR
                   PERFORM 400000-INCLUIR
              WHEN SGTST036-ALTERAR
                   PERFORM 500000-ALTERAR
              WHEN SGTST036-EXCLUIR
                   PERFORM 600000-EXCLUIR
              WHEN SGTST036-CONSULTAR
                   IF  CHAVE-CACHE = SGTST036-CHAVE
                       MOVE DADOS-CACHE TO SGTST036-AREA-DADOS
                   ELSE
                       PERFORM 700000-CONSULTA
                   END-IF
           END-EVALUATE

           GOBACK
           .
      *-----------------------------------------------------------------
       100000-VALIDA-PARAMETRO         SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO PSC-ERRO-PGM
           INITIALIZE SGTST036-RTN-PGM

           IF  NOT SGTST036-INCLUIR
           AND NOT SGTST036-EXCLUIR
           AND NOT SGTST036-ALTERAR
           AND NOT SGTST036-CONSULTAR
               SET ERRO-CODIFICADO     TO TRUE
               MOVE  0116              TO  CD-RTN-PGM
               MOVE 'Ação inválida.'   TO MSG-ERRO
               SET MSGKW999-MNT-VRV    TO TRUE
               PERFORM 999001-ERRO-001
           END-IF

           PERFORM 130000-VALIDA-CAMPOS

           IF  SGTST036-EXCLUIR
               PERFORM 170001-VALIDA-CRCT-CTRA-TVM
           END-IF
           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       130000-VALIDA-CAMPOS            SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                   TO PSC-ERRO-PGM

           IF  SGTST036-CD-TIP-ATB-CTRA EQUAL ZEROS
               SET ERRO-CODIFICADO     TO TRUE
               MOVE  0602              TO  CD-RTN-PGM
               MOVE 'Código do tipo de atributo da carteira deve ser inf
      -              'ormado.'         TO MSG-ERRO
               SET MSGKW999-MNT-VRV    TO TRUE
               PERFORM 999001-ERRO-001
           END-IF

           IF  SGTST036-INCLUIR
           OR  SGTST036-ALTERAR
               IF  SGTST036-CD-TIP-ATB-CRCT EQUAL ZEROS
                   SET ERRO-CODIFICADO     TO TRUE
                   MOVE  0708          TO  CD-RTN-PGM
                   MOVE 'Código do tipo de atributo da característica in
      -                 'válido.'        TO MSG-ERRO
                   SET MSGKW999-MNT-VRV    TO TRUE
                   PERFORM 999001-ERRO-001
               END-IF
               IF  SGTST036-TX-TIP-ATB-CTRA EQUAL SPACES
                   SET ERRO-CODIFICADO     TO TRUE
                   MOVE  0709          TO  CD-RTN-PGM
                   MOVE 'Texto do tipo de atributo da carteira inválido.
      -                 ''               TO MSG-ERRO
                   SET MSGKW999-MNT-VRV    TO TRUE
                   PERFORM 999001-ERRO-001
               END-IF
               IF  SGTST036-TX-RTL-ATB-CTRA EQUAL SPACES
                   SET ERRO-CODIFICADO     TO TRUE
                   MOVE  0710          TO  CD-RTN-PGM
                   MOVE 'Texto do rótulo do atributo da carteira inválid
      -                 'o.'             TO MSG-ERRO
                   SET MSGKW999-MNT-VRV    TO TRUE
                   PERFORM 999001-ERRO-001
               END-IF
               IF  SGTST036-TX-CMPT-ATB-CTRA EQUAL SPACES
                   SET ERRO-CODIFICADO     TO TRUE
                   MOVE  0711          TO  CD-RTN-PGM
                   MOVE 'Texto do complemento do atributo da carteira in
      -            'válido.'           TO MSG-ERRO
                   SET MSGKW999-MNT-VRV    TO TRUE
                   PERFORM 999001-ERRO-001
               END-IF
               IF  SGTST036-CD-USU-RSP-ATL EQUAL SPACES
                   SET ERRO-CODIFICADO     TO TRUE
                   MOVE  0226          TO  CD-RTN-PGM
                   MOVE 'Código do usuário responsável pela última atual
      -            'ização inválido.'  TO  MSG-ERRO
                   SET MSGKW999-MNT-VRV    TO TRUE
                   PERFORM 999001-ERRO-001
               END-IF
               IF  SGTST036-IN-CRCT-GLB    NOT EQUAL 'S' OR 'N'
                   SET ERRO-CODIFICADO     TO TRUE
                   MOVE  0712          TO  CD-RTN-PGM
                   MOVE 'Indicador de característica global inválido.'
                                       TO  MSG-ERRO
                   SET MSGKW999-MNT-VRV    TO TRUE
                   PERFORM 999001-ERRO-001
               END-IF
      * CALCULA O TAMANHO DO CAMPO VARCHAR
               MOVE 0          TO SGTST036-TX-CMPT-ATB-CTRA-LEN
               INSPECT FUNCTION REVERSE(SGTST036-TX-CMPT-ATB-CTRA)
               TALLYING SGTST036-TX-CMPT-ATB-CTRA-LEN FOR LEADING SPACES
               COMPUTE SGTST036-TX-CMPT-ATB-CTRA-LEN =
                        LENGTH OF SGTST036-TX-CMPT-ATB-CTRA -
                        SGTST036-TX-CMPT-ATB-CTRA-LEN
                  ON SIZE ERROR
                  SET ERRO-CODIFICADO     TO TRUE
                  MOVE  0143           TO  CD-RTN-PGM
                  MOVE 'Erro no cálculo do tamanho do texto.'
                                       TO MSG-ERRO
                  SET MSGKW999-MNT-VRV    TO TRUE
                  PERFORM 999001-ERRO-001
               END-COMPUTE
           END-IF

           IF  SGTST036-EXCLUIR
               IF  SGTST036-CD-USU-RSP-ATL EQUAL SPACES
                   SET ERRO-CODIFICADO     TO TRUE
                   MOVE  0278           TO  CD-RTN-PGM
                   MOVE 'Código do usuário responsável pela atualização
      -            'não informado.'    TO  MSG-ERRO
                   SET MSGKW999-MNT-VRV    TO TRUE
                   PERFORM 999001-ERRO-001
               END-IF
           END-IF
           .
       130000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       170001-VALIDA-CRCT-CTRA-TVM     SECTION.
      *-----------------------------------------------------------------
           MOVE 0004                   TO PSC-ERRO-PGM
           MOVE ZEROS                  TO QT-REG-SELD
           EXEC SQL
             SELECT 1
              INTO :QT-REG-SELD
               FROM DB2SGT.CRCT_CTRA_TVM
               WHERE CD_TIP_ATB_CTRA    = :SGTST036-CD-TIP-ATB-CTRA
             FETCH FIRST 1 ROW ONLY
           END-EXEC
           IF  QT-REG-SELD NOT EQUAL ZEROS
               SET ERRO-CODIFICADO     TO TRUE
               MOVE  0713              TO  CD-RTN-PGM
               MOVE 'Característica da carteira de títulos e valores mob
      -             'iliários vinculada.'        TO MSG-ERRO
               SET MSGKW999-MNT-VRV    TO TRUE
               PERFORM 999001-ERRO-001
           END-IF
           .
       170001-SAI.
           EXIT.
      *-----------------------------------------------------------------
       400000-INCLUIR                  SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO PSC-ERRO-PGM
      *
      * Desligar erro automatico do DB2
      *
           EXEC SQL WHENEVER SQLERROR CONTINUE END-EXEC
      *
           EXEC SQL
                INSERT INTO DB2SGT.TIP_ATB_VCLD_CTRA
                ( CD_TIP_ATB_CTRA
                , CD_TIP_ATB_CRCT
                , TX_TIP_ATB_CTRA
                , TX_RTL_ATB_CTRA
                , TX_CMPT_ATB_CTRA
                , IN_CRCT_GLB )
                VALUES
                ( :SGTST036-CD-TIP-ATB-CTRA
                , :SGTST036-CD-TIP-ATB-CRCT
                , :SGTST036-TX-TIP-ATB-CTRA
                , :SGTST036-TX-RTL-ATB-CTRA
                , :SGTST036-TX-CMPT-ATB-CTRA-GRP
                , :SGTST036-IN-CRCT-GLB )
           END-EXEC
      *
      * Ligar erro automatico do DB2
      *
           EXEC SQL WHENEVER SQLERROR   GO TO 999002-ERRO-DB2 END-EXEC
      *
           EVALUATE SQLCODE
               WHEN -803
                   SET ERRO-CODIFICADO     TO TRUE
                   MOVE  0803              TO  CD-RTN-PGM
                   MOVE 'Inclusão não efetuada, registro existente.'
                                    TO MSG-ERRO
                   SET MSGKW999-MNT-VRV    TO TRUE
                   PERFORM 999001-ERRO-001
               WHEN 0
                    CONTINUE
               WHEN OTHER
      * Aciona o tratamento padrao de erro padrao para os demais casos
                    PERFORM 999002-ERRO-DB2
           END-EVALUATE
           PERFORM 980101-GUARDA-CACHE
           INITIALIZE DADOS-LOG
           MOVE 'I'                    TO SGTS014G-CD-TIP-ACAO
           PERFORM 980102-GRAVA-LOG
           .
       400000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       500000-ALTERAR                  SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO PSC-ERRO-PGM
           EXEC SQL
             UPDATE DB2SGT.TIP_ATB_VCLD_CTRA
             SET CD_TIP_ATB_CRCT    = :SGTST036-CD-TIP-ATB-CRCT
               , TX_TIP_ATB_CTRA    = :SGTST036-TX-TIP-ATB-CTRA
               , TX_RTL_ATB_CTRA    = :SGTST036-TX-RTL-ATB-CTRA
               , TX_CMPT_ATB_CTRA   = :SGTST036-TX-CMPT-ATB-CTRA-GRP
               , IN_CRCT_GLB        = :SGTST036-IN-CRCT-GLB
                WHERE CD_TIP_ATB_CTRA    = :SGTST036-CD-TIP-ATB-CTRA
           END-EXEC
           IF  SQLCODE EQUAL +100
               INITIALIZE CHAVE-CACHE
               SET ERRO-CODIFICADO     TO TRUE
               MOVE  0100              TO  CD-RTN-PGM
               MOVE 'Alteração não efetuada, registro inexistente.'
                                             TO MSG-ERRO
               SET MSGKW999-MNT-VRV    TO TRUE
               PERFORM 999001-ERRO-001
           END-IF
           PERFORM 980101-GUARDA-CACHE
           INITIALIZE DADOS-LOG
           MOVE 'A'                    TO SGTS014G-CD-TIP-ACAO
           PERFORM 980102-GRAVA-LOG
           .
       500000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       600000-EXCLUIR                  SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO PSC-ERRO-PGM
           INITIALIZE DADOS-LOG
           MOVE 'E'                    TO SGTS014G-CD-TIP-ACAO
           PERFORM 980102-GRAVA-LOG
           EXEC SQL
                DELETE FROM DB2SGT.TIP_ATB_VCLD_CTRA
                WHERE CD_TIP_ATB_CTRA    = :SGTST036-CD-TIP-ATB-CTRA
           END-EXEC
           INITIALIZE REG-CACHE
           IF  SQLCODE EQUAL +100
               SET ERRO-CODIFICADO     TO TRUE
               MOVE  0100              TO  CD-RTN-PGM
               MOVE 'Exclusão não efetuada, registro inexistente.'
                                       TO MSG-ERRO
               SET MSGKW999-MNT-VRV    TO TRUE
               PERFORM 999001-ERRO-001
           END-IF
           .
       600000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       700000-CONSULTA                 SECTION.
      *-----------------------------------------------------------------
           MOVE 0009                   TO PSC-ERRO-PGM
           EXEC SQL
                 SELECT CD_TIP_ATB_CRCT
                      , TX_TIP_ATB_CTRA
                      , TX_RTL_ATB_CTRA
                      , TX_CMPT_ATB_CTRA
                      , IN_CRCT_GLB
                   INTO  :SGTST036-CD-TIP-ATB-CRCT
                       , :SGTST036-TX-TIP-ATB-CTRA
                       , :SGTST036-TX-RTL-ATB-CTRA
                       , :SGTST036-TX-CMPT-ATB-CTRA-GRP
                       , :SGTST036-IN-CRCT-GLB
                       FROM DB2SGT.TIP_ATB_VCLD_CTRA
                WHERE CD_TIP_ATB_CTRA    = :SGTST036-CD-TIP-ATB-CTRA
           END-EXEC
           IF  SQLCODE = +100
               INITIALIZE SGTST036-AREA-DADOS
               SET ERRO-CODIFICADO     TO TRUE
               MOVE  SQLCODE           TO  CD-RTN-PGM
               MOVE 'Tipo de atributo vinculado a carteira inexistente.'
                                       TO MSG-ERRO

               PERFORM 999001-ERRO-001
           END-IF
           PERFORM 980101-GUARDA-CACHE
           .
       700000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       980101-GUARDA-CACHE             SECTION.
      *-----------------------------------------------------------------
           MOVE 0010                   TO PSC-ERRO-PGM

           MOVE SGTST036-CHAVE         TO CHAVE-CACHE
           MOVE SGTST036-AREA-DADOS    TO DADOS-CACHE
           .
       980101-SAI.
           EXIT.
      *-----------------------------------------------------------------
       980102-GRAVA-LOG                SECTION.
      *-----------------------------------------------------------------
           MOVE 0011                   TO PSC-ERRO-PGM

           MOVE 'TIP_ATB_VCLD_CTRA '         TO SGTS014G-NM-ENT

           IF  EXECUCAO-ONLINE
               MOVE SGTST036-CD-USU-RSP-ATL  TO SGTS014G-CD-CHV-USU
           ELSE
               MOVE 'SYSTEM'                 TO SGTS014G-CD-CHV-USU
           END-IF

           MOVE 'SGTST036'                   TO SGTS014G-NM-PGM
           MOVE SGTST036-CD-TIP-ATB-CTRA     TO CD-TIP-ATB-CTRA-LOG
           MOVE CD-TIP-ATB-CTRA-LOG          TO SGTS014G-CHV-PRMR

           MOVE LENGTH OF DADOS-LOG    TO EIBCALEN
           CALL SGTS014G USING DFHEIBLK DADOS-LOG

           IF  SGTS014G-CD-RTN-PGM  NOT EQUAL ZEROS
               SET   ERRO-DE-PGM           TO  TRUE
               MOVE  0112                  TO  CD-RTN-PGM
               MOVE  'Erro na gravação das informações do Log.'
                                           TO  MSG-ERRO
               MOVE SGTS014G               TO  MSG1-NM-PGM-ACND
               MOVE SGTS014G-CD-RTN-PGM    TO  MSG1-CD-RTN-PGM-ACND
               MOVE SGTS014G-MSG-ERRO      TO  MSG1-TX-RTN-PGM-ACND
               SET MSGKW999-MNT-VRV    TO TRUE
               PERFORM 999001-ERRO-001
           END-IF
           .
       980102-SAI.
           EXIT.

      *--------------------
      * 999000-ERRO                    SECTION
      *--------------------
      * 999001-ERRO-001.
      *--------------------
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
           MOVE  SGTST036-TIP-RTN-PGM  TO  TIP-RTN-PGM
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

           MOVE 'T036-001-CD-CHV-USU'            TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST036-CD-CHV-USU             TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'T036-002-ACAO'                  TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST036-ACAO                   TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'T036-003-CD-TIP-ATB-CTRA'       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST036-CD-TIP-ATB-CTRA        TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'T036-004-CD-TIP-ATB-CRCT'       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST036-CD-TIP-ATB-CRCT        TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'T036-005-TX-TIP-ATB-CTRA'       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST036-TX-TIP-ATB-CTRA        TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'T036-006-TX-RTL-ATB-CTRA'       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST036-TX-RTL-ATB-CTRA        TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'T036-007-TX-CMPT-ATB-CTRA-GRP'  TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST036-TX-CMPT-ATB-CTRA-GRP   TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'T036-008-CD-USU-RSP-ATL'        TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST036-CD-USU-RSP-ATL         TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'T036-009-IN-CRCT-GLB'           TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST036-IN-CRCT-GLB            TO  MSGKW999-VL-VRV-MNT
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

      *    EVALUATE  PGM-ERRO
           EVALUATE  MSG1-NM-PGM-ACND
               WHEN  SGTS014G
                     MOVE  'SGTS014G-CD-TIP-ACAO'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS014G-CD-TIP-ACAO
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE  'SGTS014G-CD-CHV-USU'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS014G-CD-CHV-USU
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE  'SGTS014G-NM-PGM'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS014G-NM-PGM
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE  'SGTS014G-CHV-PRMR'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS014G-CHV-PRMR
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTS014G-PSC-ERRO-PGM'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS014G-PSC-ERRO-PGM
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTS014G-CD-RTN-PGM'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS014G-CD-RTN-PGM
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTS014G-MSG-ERRO'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS014G-MSG-ERRO
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTS014G-TIP-RTN-PGM'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS014G-TIP-RTN-PGM
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTS014G-ATB-MSG'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS014G-ATB-MSG
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTS014G-TX-IDFC-ATNT-MSG'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS014G-TX-IDFC-ATNT-MSG
                                       TO  MSGKW999-VL-VRV-MNT
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
           MOVE  RTN-PGM               TO  SGTST036-RTN-PGM

           IF  EXECUCAO-ONLINE AND   MSGKW999-MNT-VRV
               PERFORM  999000-TRATAR-ERRO-MSG
           END-IF

           GOBACK
           .
       999999-SAI.
           EXIT.
      *---------------------- FIM SGTST036 ----------------------------