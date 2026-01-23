      *-----------------------------------------------------------------
      * PROGRAMA..: SGTST048
      * ANALISTA..: F2054109 - CINTIA APARECIDA SOARES IGLESIAS
      * AUTOR.....: F8658869 - ROBERTO IOSHIMOTO
      * DATA......: 06.11.2015
      * OBJETIVO..: PERSISTENCIA DA TABELA 'EST_TIP_NEGO'
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS002 28/03/2019 F9188847 DE 2016/145195 - Inclusão dos campos:
      *              SGTST048-E-CD-CTRA-TIT-VL e SGTST048-CD-CTRA-TIT-VL
      *        01/03/2019 F0950413 DE 2016/145195 - Inclusão na section
      *        999001-ERRO-001, '1 - Erro de db2' e '2 - erro cics' E
      *        adequação das mensagens de erro ao padrão da mensageria
      *        20/03/2018 F9188847 DE 2016/145195 - Alteração no
      *              book SGTK014G (Recompilação).
      *        16/02/2017 F8785286 DE 2016/145195 - Inclusão movimen-
      *              tação campo book para chave log.
      * VRS001 06.11.2015 - F8658869 - IMPLANTACAO
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTST048.

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
       01  CTE-PROG     PIC  X(17) VALUE '*** SGTST048 ***'.
       01  CTE-VERS     PIC  X(06) VALUE 'VRS002'.

      * Registro da tabela

       01  REG-CACHE.
           03  CHAVE-CACHE.
               05  CD-CTRA-TIT-VL            PIC S9(9) COMP-5 VALUE 0.
               05  CD-TIP-NEGO               PIC S9(4) COMP-5 VALUE 0.
               05  TS-EST-TIP-NEGO           PIC  X(026) VALUE ' '.
               05  FILLER                    PIC  X(20) VALUE ' '.
           03  DADOS-CACHE.
               05  CD-TIP-EST-DCS            PIC S9(4) COMP-5 VALUE 0.
               05  CD-USU-ATL-EST            PIC  X(8) VALUE ' '.
               05  TX-OBS-EST-TIP            PIC  X(60) VALUE ' '.

      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informaçoes
-INC HLPKE01B                                                           00000480
      *-----------------------------------------------------------------
       LOCAL-STORAGE SECTION.
      *-----------------------------------------------------------------
      * Area de trabalho
       01  IC-TAB                      PIC  9(04)       VALUE 0.

       01  QT-REG-SELD                 PIC S9(4) COMP-5 VALUE 0.
       01  TS-SQL-ATU                  PIC  X(26) VALUE ' '.

      * Indicadores de nulidades

       01  QT-REG-SELD-NULO            PIC S9(4) COMP-5 VALUE 0.
       01  TX-OBS-EST-TIP-N            PIC S9(4) COMP-5 VALUE 0.

      * Variáveis para gravação do log
       01  DADOS-LOG-DISPLAY.
           03  CD-CTRA-TIT-VL-LOG             PIC  9(10) VALUE 0.
           03  CD-TIP-NEGO-LOG               PIC  9(5) VALUE 0.
           03  TS-EST-TIP-NEGO-LOG           PIC  X(26) VALUE ' '.

      * Book dos dados do log
       01  DADOS-LOG.
-INC SGTK014G                                                           00000690

      * Subrotinas Utilizadas

       01  SGTS014G VALUE 'SGTS014G'         PIC X(8).

      *--> Máscaras edição de variáveis p/ monitoração mensagens de erro
       01  MCR-S9-15                   PIC      ---------------9.

      * Books da definição de dados a serem usados na subrotina de erros
-INC HLPKE001                                                           00000750
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
-INC SGTKT048                                                           00000850
      *-----------------------------------------------------------------
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *-----------------------------------------------------------------

           IF  EIBCALEN NOT EQUAL LENGTH OF DFHCOMMAREA
               SET ERRO-CODIFICADO     TO TRUE
               MOVE 0220               TO  CD-RTN-PGM
               MOVE 'Área com tamanho inválido - verifique book/local.'
                                       TO MSG-ERRO
               SET MSGKW999-MNT-VRV    TO TRUE
               PERFORM 999001-ERRO-001
           END-IF

           IF NOT EXECUCAO-ONLINE AND NOT EXECUCAO-BATCH
             CALL CICS5000 USING CICS5000-COMMAREA
           END-IF

           MOVE 0001                   TO PSC-ERRO-PGM
      * WHENEVER DB2
-INC HLPKE002                                                           00000990
      * WHENEVER CICS
-INC HLPKE003                                                           00001010

           PERFORM 100000-VALIDA-PARAMETRO

           EVALUATE TRUE
              WHEN SGTST048-INCLUIR
                   PERFORM 400000-INCLUIR
              WHEN SGTST048-ALTERAR
                   PERFORM 500000-ALTERAR
              WHEN SGTST048-EXCLUIR
                   PERFORM 600000-EXCLUIR
              WHEN SGTST048-CONSULTAR
                   IF  CHAVE-CACHE = SGTST048-CHAVE
                       MOVE DADOS-CACHE TO SGTST048-AREA-DADOS
                   ELSE
                       PERFORM 700000-CONSULTA
                   END-IF
              WHEN SGTST048-CONS-EST-ATU
                   PERFORM 800000-CONS-EST-ATU
              WHEN SGTST048-CONS-EST-INCL
                   PERFORM 900000-CONS-EST-INCL
           END-EVALUATE

           GOBACK
           .
      *-----------------------------------------------------------------
       100000-VALIDA-PARAMETRO         SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO PSC-ERRO-PGM
           INITIALIZE SGTST048-RTN-PGM

           IF  NOT SGTST048-INCLUIR
           AND NOT SGTST048-EXCLUIR
           AND NOT SGTST048-ALTERAR
           AND NOT SGTST048-CONSULTAR
           AND NOT SGTST048-CONS-EST-ATU
           AND NOT SGTST048-CONS-EST-INCL
               SET ERRO-CODIFICADO     TO TRUE
               MOVE  0116              TO  CD-RTN-PGM
               MOVE 'Ação inválida.'   TO MSG-ERRO
               SET MSGKW999-MNT-VRV    TO TRUE
               PERFORM 999001-ERRO-001
           END-IF

           PERFORM 130000-VALIDA-CAMPOS

           IF  SGTST048-INCLUIR
           OR  SGTST048-ALTERAR
               PERFORM 160000-TRATA-NULIDADE
               PERFORM 170001-VALIDA-TIP-NEGO
           END-IF
           .
       100000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       130000-VALIDA-CAMPOS            SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                   TO PSC-ERRO-PGM
           PERFORM 140000-BUSCA-TS-DT
           MOVE HLPKE001-TS-FMTD       TO TS-SQL-ATU

           IF  SGTST048-E-CD-CTRA-TIT-VL     NOT POSITIVE
               SET ERRO-CODIFICADO     TO TRUE
               MOVE  0001              TO  CD-RTN-PGM
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' inválido.'      TO  MSG-ERRO
               SET MSGKW999-MNT-VRV    TO TRUE
               PERFORM 999001-ERRO-001
           END-IF

           IF  SGTST048-E-CD-CTRA-TIT-VL EQUAL ZEROS
               SET ERRO-CODIFICADO     TO TRUE
               MOVE  0002              TO  CD-RTN-PGM
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' não informado.'
                                       TO MSG-ERRO
               SET MSGKW999-MNT-VRV    TO TRUE
001670         PERFORM 999001-ERRO-001
001680     END-IF

           IF  SGTST048-CD-TIP-NEGO EQUAL ZEROS
               SET ERRO-CODIFICADO     TO TRUE
               MOVE  0714              TO  CD-RTN-PGM
               MOVE 'Código do tipo de negociação inválido.'
                                       TO MSG-ERRO
               SET MSGKW999-MNT-VRV    TO TRUE
               PERFORM 999001-ERRO-001
           END-IF

           IF  SGTST048-TS-EST-TIP-NEGO EQUAL SPACES
               IF  SGTST048-INCLUIR
                   MOVE TS-SQL-ATU TO SGTST048-TS-EST-TIP-NEGO
               ELSE
                   IF  NOT  SGTST048-CONS-EST-ATU  AND
                       NOT  SGTST048-CONS-EST-INCL
                       SET ERRO-CODIFICADO  TO TRUE
                       MOVE  0715      TO  CD-RTN-PGM
                       MOVE 'Timestamp do estado do tipo de negociação i
      -                     'nválido.' TO MSG-ERRO
                       SET MSGKW999-MNT-VRV TO TRUE
                       PERFORM 999001-ERRO-001
                   END-IF
               END-IF
           ELSE
               IF  SGTST048-INCLUIR
                   MOVE TS-SQL-ATU TO SGTST048-TS-EST-TIP-NEGO
               END-IF
           END-IF

           IF  SGTST048-INCLUIR
           OR  SGTST048-ALTERAR
               IF  SGTST048-CD-TIP-EST-DCS EQUAL ZEROS
                   SET ERRO-CODIFICADO TO TRUE
                   MOVE  0077          TO  CD-RTN-PGM
                   MOVE 'Código do tipo de estado da decisão inválido.'
                                            TO MSG-ERRO
                   SET MSGKW999-MNT-VRV     TO TRUE
                   PERFORM 999001-ERRO-001
               END-IF
               IF  SGTST048-CD-USU-ATL-EST EQUAL SPACES OR
                   SGTST048-CD-USU-ATL-EST EQUAL 0
                   SET ERRO-CODIFICADO TO TRUE
                   MOVE  0524          TO  CD-RTN-PGM
                   MOVE 'Código do usuário responsável pela atualização
      -                 'do estado inválido.'    TO MSG-ERRO
                   SET MSGKW999-MNT-VRV          TO TRUE
                   PERFORM 999001-ERRO-001
               END-IF
           END-IF

           IF  SGTST048-EXCLUIR
               IF  SGTST048-CD-USU-ATL-EST EQUAL SPACES OR
                   SGTST048-CD-USU-ATL-EST EQUAL 0
                   SET ERRO-CODIFICADO TO TRUE
                   MOVE  0524          TO  CD-RTN-PGM
                   MOVE 'Código do usuário responsável pela atualização
      -                 'do estado inválido.'    TO MSG-ERRO
                   SET MSGKW999-MNT-VRV          TO TRUE
                   PERFORM 999001-ERRO-001
               END-IF
           END-IF

           IF  SGTST048-TS-EST-TIP-NEGO EQUAL 'CURRENT'
               MOVE TS-SQL-ATU         TO SGTST048-TS-EST-TIP-NEGO
           END-IF
           .
       130000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       140000-BUSCA-TS-DT              SECTION.
      *-----------------------------------------------------------------
           MOVE 0004                   TO PSC-ERRO-PGM
           CALL SBTIMEDT            USING HLPKE001-CD-FUC
                                          HLPKE001-AREA-TRB
                                          HLPKE001-FMT-TS
                                          HLPKE001-TAM-TS
                                          HLPKE001-TS-FMTD
           IF  RETURN-CODE      NOT EQUAL ZEROS
               MOVE  0050              TO  CD-RTN-PGM
               MOVE 'Erro ao obter data e hora atual.'
                                       TO MSG-ERRO
               SET   ERRO-DE-PGM       TO TRUE
               MOVE  SBTIMEDT          TO  PGM-ERRO
               SET MSGKW999-MNT-VRV    TO TRUE
               PERFORM 999001-ERRO-001
           END-IF
           .
       140000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       160000-TRATA-NULIDADE           SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                   TO PSC-ERRO-PGM
           IF  SGTST048-TX-OBS-EST-TIP EQUAL SPACES
               MOVE -1                 TO TX-OBS-EST-TIP-N
           END-IF

           .
       160000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       170001-VALIDA-TIP-NEGO         SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO PSC-ERRO-PGM
           MOVE ZEROS                  TO QT-REG-SELD
           EXEC SQL
             SELECT 1
              INTO :QT-REG-SELD
               FROM DB2SGT.TIP_NEGO
               WHERE CD_CTRA_TIT_VL     = : SGTST048-E-CD-CTRA-TIT-VL
                 AND CD_TIP_NEGO        = :SGTST048-CD-TIP-NEGO
           END-EXEC

           IF  QT-REG-SELD = 0
               SET ERRO-CODIFICADO     TO TRUE
               MOVE  0716              TO  CD-RTN-PGM
               MOVE 'Tipo negociação inexistente.'
                                       TO MSG-ERRO
               SET MSGKW999-MNT-VRV    TO TRUE
               PERFORM 999001-ERRO-001
           END-IF
           .
       170001-SAI.
           EXIT.
      *-----------------------------------------------------------------
       400000-INCLUIR                  SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO PSC-ERRO-PGM
      *
      * Desligar erro automático do DB2
      *
           EXEC SQL WHENEVER SQLERROR CONTINUE END-EXEC
      *
           EXEC SQL
                INSERT INTO DB2SGT.EST_TIP_NEGO
                ( CD_CTRA_TIT_VL
                , CD_TIP_NEGO
                , TS_EST_TIP_NEGO
                , CD_TIP_EST_DCS
                , CD_USU_ATL_EST
                , TX_OBS_EST_TIP )
                VALUES
                ( :SGTST048-E-CD-CTRA-TIT-VL
                , :SGTST048-CD-TIP-NEGO
                , :SGTST048-TS-EST-TIP-NEGO
                , :SGTST048-CD-TIP-EST-DCS
                , :SGTST048-CD-USU-ATL-EST
                , :SGTST048-TX-OBS-EST-TIP
                  :TX-OBS-EST-TIP-N )
           END-EXEC
      *
      * Ligar erro automático do DB2
      *
           EXEC SQL WHENEVER SQLERROR   GO TO 999002-ERRO-DB2 END-EXEC
      *
           EVALUATE SQLCODE
               WHEN -803
                   SET ERRO-CODIFICADO      TO TRUE
                   MOVE  0803               TO  CD-RTN-PGM
                   MOVE 'Inclusão não efetuada, registro existente'
                                            TO MSG-ERRO
                   SET MSGKW999-MNT-VRV     TO TRUE
                   PERFORM 999001-ERRO-001
               WHEN 0
                    CONTINUE
               WHEN OTHER
      * Aciona o tratamento padrão de erro padrão para os demais casos
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
       500000-ALTERAR                 SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO PSC-ERRO-PGM

           EXEC SQL
             UPDATE DB2SGT.EST_TIP_NEGO
             SET CD_TIP_EST_DCS     = :SGTST048-CD-TIP-EST-DCS
               , CD_USU_ATL_EST     = :SGTST048-CD-USU-ATL-EST
               , TX_OBS_EST_TIP     = :SGTST048-TX-OBS-EST-TIP
                                      :TX-OBS-EST-TIP-N
                WHERE CD_CTRA_TIT_VL     = :SGTST048-E-CD-CTRA-TIT-VL
                  AND CD_TIP_NEGO        = :SGTST048-CD-TIP-NEGO
                  AND TS_EST_TIP_NEGO    = :SGTST048-TS-EST-TIP-NEGO
           END-EXEC

           IF  SQLCODE EQUAL +100
               INITIALIZE CHAVE-CACHE
               SET ERRO-CODIFICADO     TO TRUE
               MOVE  0100              TO  CD-RTN-PGM
               MOVE 'Alteração não efetuada, registro inexistente'
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
           MOVE 0009                   TO PSC-ERRO-PGM
           INITIALIZE DADOS-LOG
           MOVE 'E'                    TO SGTS014G-CD-TIP-ACAO
           PERFORM 980102-GRAVA-LOG
           EXEC SQL
                DELETE FROM DB2SGT.EST_TIP_NEGO
                WHERE CD_CTRA_TIT_VL     = :SGTST048-E-CD-CTRA-TIT-VL
                  AND CD_TIP_NEGO        = :SGTST048-CD-TIP-NEGO
                  AND TS_EST_TIP_NEGO    = :SGTST048-TS-EST-TIP-NEGO
           END-EXEC
           INITIALIZE REG-CACHE
           IF  SQLCODE EQUAL +100
               SET ERRO-CODIFICADO     TO TRUE
               MOVE  0100              TO  CD-RTN-PGM
               MOVE 'Exclusão não efetuada, registro inexistente'
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
           MOVE 0010                   TO PSC-ERRO-PGM
      *    Campos nulos sao inicializados na consulta
           INITIALIZE SGTST048-TX-OBS-EST-TIP
           EXEC SQL
                 SELECT CD_CTRA_TIT_VL
                      , CD_TIP_EST_DCS
                      , CD_USU_ATL_EST
                      , TX_OBS_EST_TIP
                   INTO  :SGTST048-CD-CTRA-TIT-VL
                       , :SGTST048-CD-TIP-EST-DCS
                       , :SGTST048-CD-USU-ATL-EST
                       , :SGTST048-TX-OBS-EST-TIP
                         :TX-OBS-EST-TIP-N
                       FROM DB2SGT.EST_TIP_NEGO
                WHERE CD_CTRA_TIT_VL     = :SGTST048-E-CD-CTRA-TIT-VL
                  AND CD_TIP_NEGO        = :SGTST048-CD-TIP-NEGO
                  AND TS_EST_TIP_NEGO    = :SGTST048-TS-EST-TIP-NEGO
           END-EXEC

           IF  SQLCODE = +100
               INITIALIZE SGTST048-AREA-DADOS
               SET ERRO-CODIFICADO     TO TRUE
               MOVE  SQLCODE           TO  CD-RTN-PGM
               MOVE 'Estado tipo negociação inexistente'
                                       TO MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF
           PERFORM 980101-GUARDA-CACHE
           .
       700000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       800000-CONS-EST-ATU             SECTION.
      *-----------------------------------------------------------------
           MOVE 0011                   TO PSC-ERRO-PGM
      *
           EXEC SQL
                 SELECT CD_CTRA_TIT_VL
                      , CD_TIP_EST_DCS
                      , CD_USU_ATL_EST
                      , TX_OBS_EST_TIP
                      , TS_EST_TIP_NEGO
                   INTO  :SGTST048-CD-CTRA-TIT-VL
                       , :SGTST048-CD-TIP-EST-DCS
                       , :SGTST048-CD-USU-ATL-EST
                       , :SGTST048-TX-OBS-EST-TIP
                         :TX-OBS-EST-TIP-N
                       , :SGTST048-TS-EST-TIP-NEGO
                       FROM DB2SGT.EST_TIP_NEGO
                WHERE CD_CTRA_TIT_VL     = :SGTST048-E-CD-CTRA-TIT-VL
                  AND CD_TIP_NEGO        = :SGTST048-CD-TIP-NEGO
                  ORDER BY TS_EST_TIP_NEGO DESC
                FETCH FIRST 1 ROW ONLY
           END-EXEC

           IF  SQLCODE = +100
               INITIALIZE SGTST048-AREA-DADOS
               SET ERRO-CODIFICADO    TO TRUE
               MOVE  SQLCODE           TO  CD-RTN-PGM
               MOVE 'Estado tipo negociação inexistente.'
                                       TO MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF
           .
       800000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       900000-CONS-EST-INCL            SECTION.
      *-----------------------------------------------------------------
           MOVE 0012                   TO PSC-ERRO-PGM
      *
           EXEC SQL
                 SELECT CD_CTRA_TIT_VL
                      , CD_TIP_EST_DCS
                      , CD_USU_ATL_EST
                      , TX_OBS_EST_TIP
                      , TS_EST_TIP_NEGO
                   INTO  :SGTST048-CD-CTRA-TIT-VL
                       , :SGTST048-CD-TIP-EST-DCS
                       , :SGTST048-CD-USU-ATL-EST
                       , :SGTST048-TX-OBS-EST-TIP
                         :TX-OBS-EST-TIP-N
                       , :SGTST048-TS-EST-TIP-NEGO
                       FROM DB2SGT.EST_TIP_NEGO
                WHERE CD_CTRA_TIT_VL     = :SGTST048-E-CD-CTRA-TIT-VL
                  AND CD_TIP_NEGO        = :SGTST048-CD-TIP-NEGO
      * Pendente de confirmação
                  AND CD_TIP_EST_DCS     = :SGTST048-CD-TIP-EST-DCS
                  ORDER BY TS_EST_TIP_NEGO DESC
                FETCH FIRST 1 ROW ONLY
           END-EXEC

           IF  SQLCODE = +100
               INITIALIZE SGTST048-AREA-DADOS
               SET ERRO-CODIFICADO     TO TRUE
               MOVE  0100              TO  CD-RTN-PGM
               MOVE 'Estado informado inexistente.'
                                       TO MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF
           PERFORM 980101-GUARDA-CACHE
           .
       900000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       980101-GUARDA-CACHE             SECTION.
      *-----------------------------------------------------------------
           MOVE 0013                   TO PSC-ERRO-PGM

           MOVE SGTST048-CHAVE         TO CHAVE-CACHE
           MOVE SGTST048-AREA-DADOS         TO DADOS-CACHE
           .
       980101-SAI.
           EXIT.
      *-----------------------------------------------------------------
       980102-GRAVA-LOG                SECTION.
      *-----------------------------------------------------------------
           MOVE 0014                   TO PSC-ERRO-PGM

           MOVE 'EST_TIP_NEGO'         TO SGTS014G-NM-ENT

           IF  EXECUCAO-ONLINE
               MOVE SGTST048-CD-USU-ATL-EST TO SGTS014G-CD-CHV-USU
           ELSE
               MOVE 'SYSTEM'                TO SGTS014G-CD-CHV-USU
           END-IF

           MOVE 'SGTST048'                  TO SGTS014G-NM-PGM
           MOVE SGTST048-E-CD-CTRA-TIT-VL   TO CD-CTRA-TIT-VL-LOG
           MOVE SGTST048-CD-TIP-NEGO        TO CD-TIP-NEGO-LOG
           MOVE SGTST048-TS-EST-TIP-NEGO    TO TS-EST-TIP-NEGO-LOG

           STRING CD-CTRA-TIT-VL-LOG
           ';'    CD-TIP-NEGO-LOG
           ';'    TS-EST-TIP-NEGO-LOG
               DELIMITED BY SIZE INTO SGTS014G-CHV-PRMR

           MOVE LENGTH OF DADOS-LOG    TO EIBCALEN
           CALL SGTS014G USING DFHEIBLK DADOS-LOG

           IF  SGTS014G-CD-RTN-PGM  NOT EQUAL ZEROS
               SET   ERRO-DE-PGM           TO  TRUE
               MOVE  SGTS014G              TO  PGM-ERRO
               MOVE  0112                  TO  CD-RTN-PGM
               MOVE  'Erro na gravação das informações do Log.'
                                           TO  MSG-ERRO
               MOVE SGTS014G               TO  MSG1-NM-PGM-ACND
               MOVE SGTS014G-CD-RTN-PGM    TO  MSG1-CD-RTN-PGM-ACND
               MOVE SGTS014G-MSG-ERRO      TO  MSG1-TX-RTN-PGM-ACND
               SET MSGKW999-MNT-VRV        TO TRUE
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
           MOVE  SGTST048-TIP-RTN-PGM  TO  TIP-RTN-PGM
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

           MOVE 'SGTST048-CD-CHV-USU'            TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST048-CD-CHV-USU             TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTST048-ACAO'                  TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST048-ACAO                   TO  MCR-S9-15
           MOVE  MCR-S9-15                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTST048-E-CD-CTRA-TIT-VL'      TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST048-E-CD-CTRA-TIT-VL       TO  MCR-S9-15
           MOVE  MCR-S9-15                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTST048-CD-TIP-NEGO'           TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST048-CD-TIP-NEGO            TO  MCR-S9-15
           MOVE  MCR-S9-15                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTST048-TS-EST-TIP-NEGO'       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST048-TS-EST-TIP-NEGO        TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTST048-CD-TIP-EST-DCS'        TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST048-CD-TIP-EST-DCS         TO  MCR-S9-15
           MOVE  MCR-S9-15                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTST048-CD-USU-ATL-EST'        TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST048-CD-USU-ATL-EST         TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTST048-TX-OBS-EST-TIP'        TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST048-TX-OBS-EST-TIP         TO  MSGKW999-VL-VRV-MNT
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

               WHEN SBTIMEDT
                    MOVE  'HLPKE001-CD-FUC'  TO  MSGKW999-NM-VRV-MNT
                    MOVE  HLPKE001-CD-FUC    TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999500-ADICIONA-VRV-MNT

                    MOVE  'HLPKE001-AREA-TRB' TO  MSGKW999-NM-VRV-MNT
                    MOVE  HLPKE001-AREA-TRB   TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999500-ADICIONA-VRV-MNT

                    MOVE  'HLPKE001-FMT-TS'   TO  MSGKW999-NM-VRV-MNT
                    MOVE  HLPKE001-FMT-TS     TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999500-ADICIONA-VRV-MNT

                    MOVE  'HLPKE001-TAM-TS'   TO  MSGKW999-NM-VRV-MNT
                    MOVE  HLPKE001-TAM-TS     TO   MSGKW999-VL-VRV-MNT
                    PERFORM  999500-ADICIONA-VRV-MNT

                    MOVE  'HLPKE001-TS-FMTD'  TO  MSGKW999-NM-VRV-MNT
                    MOVE  HLPKE001-TS-FMTD    TO  MSGKW999-VL-VRV-MNT
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
           MOVE  RTN-PGM               TO  SGTST048-RTN-PGM

           IF  EXECUCAO-ONLINE  AND   MSGKW999-MNT-VRV
               PERFORM  999000-TRATAR-ERRO-MSG
           END-IF

           GOBACK
           .
       999999-SAI.
           EXIT.
      *---------------------- FIM SGTST048 ----------------------------