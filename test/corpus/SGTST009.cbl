      *-----------------------------------------------------------------
      * PROGRAMA..: SGTST009
      * ANALISTA..: F8785328 - ROSELAINE OGRIZEK
      * AUTOR.....: F8785328 - ROSELAINE OGRIZEK
      * DATA......: 19.05.2015
      * OBJETIVO..: PERSISTENCIA DA TABELA EST_OPR_ETRD
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS004 07/11/2018 F2804421 DE 2016/145195 - Inclusão do campo
      *         SGTST009-CD-MTV-ALT no SQL da ação CONSULTAR-EST-ATIVO.
      *
      *        26/07/2018 F8785286 DE 2016/145195 - Inclusão campo
      *              SGTST009-CD-MTV-ALT
      *
      *        16/02/2018 F8658869 DE 2016/145195 - Tratamento de mensa-
      *                                             geria
      * VRS003 06/08/2018 F2054109 DE 2016/145195 - Retirada do campo
      *         SGTST009-NR-VRS-OPR-ETRD do SQL da ação
      *         CONSULTAR-EST-ATIVO
      *        26/07/2018 F8785286 DE 2016/145195 - Inclusão campo
      *              SGTST009-CD-MTV-ALT
      *        16/02/2018 F8658869 DE 2016/145195 - Tratamento de mensa-
      *                                             geria
      * VRS002 17/05/2017 F9188847 DE 2016/145195 - Inclusão campo
      *              TS-EST-OPR-ETRD-S no cursor CONSULTAR-EST-ATIVO
      *        10/02/2017 F8785286 DE 2016/145195 - Inclusão campo
      *              CD-USU-RSP-ATL para chave log.
      *        08/09/2015 F2054109  DE 2015/120821 - RECOMPILAÇAO POR
      *                                ALTERAÇAO DA TABELA OPR_ETRD_MCP
      * VRS001 19.05.2015 - F8785328 - DE 2015/134565 - IMPLANTACAO.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTST009.

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
               '*** Storage Section comeca aqui ***'.
       77  CTE-PROG                    PIC  X(17)
                                       VALUE '*** SGTST009 ***'.
       77  CTE-VERS                    PIC  X(06) VALUE 'VRS002'.

      *--> Registro da tabela
       01  REG-CACHE.
           03  CHAVE-CACHE.
               05  CD-CTRA-TIT-VL      PIC S9(9) COMP-5 VALUE 0.
               05  CD-OPR-ETRD-MCP     PIC S9(11) COMP-3 VALUE 0.
               05  NR-VRS-OPR-ETRD     PIC S9(4) COMP-5 VALUE 0.
               05  TS-EST-OPR-ETRD     PIC  X(026) VALUE ' '.
               05  FILLER              PIC  X(20) VALUE ' '.
           03  DADOS-CACHE.
               05  CD-USU-RSP-EST      PIC  X(8) VALUE ' '.
               05  CD-TIP-EST-OPR      PIC S9(9) COMP-5 VALUE 0.
               05  TX-JST-EST          PIC  X(100) VALUE ' '.
               05  TS-EST-OPR-ETRD-S   PIC  X(26) VALUE ' '.

      *--> Definição dos Nomes dos Programas Acionados
       01  TAB-PROGRAMAS-CHAMADOS.
           03  SGTS014G                PIC  X(08) VALUE 'SGTS014G'.

      *--> Definição dos Variaveis do Cobolito
-INC ALMKCBL1

      *--> Book do programa de ambiente - CICS5000
      *    Definido na work para preservar as informacoes
-INC HLPKE01B

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

      *--> Area de trabalho
       01  QT-REG-SELD                 PIC S9(4) COMP-5 VALUE 0.
       01  TS-SQL-ATU                  PIC  X(26) VALUE ' '.

      *--> Indicadores de nulidades
       01  CD-TIP-EST-OPR-N            PIC S9(04) COMP-5 VALUE 0.
       01  TX-JST-EST-N                PIC S9(04) COMP-5 VALUE 0.
       01  CD-MTV-ALT-N                PIC S9(04) COMP-5 VALUE 0.

      *--> Variaveis para gravacao do log
       01  DADOS-LOG-DISPLAY.
           03  CD-CTRA-TIT-VL-LOG      PIC  9(10) VALUE 0.
           03  CD-OPR-ETRD-MCP-LOG     PIC  9(11) VALUE 0.
           03  NR-VRS-OPR-ETRD-LOG     PIC  9(05) VALUE 0.
           03  TS-EST-OPR-ETRD-LOG     PIC  X(26) VALUE ' '.

      *--> Book dos dados do log
       01  DADOS-LOG.
-INC SGTK014G

      *--> Book das variáveis auxiliares do tratamento de erro
-INC HLPKE001

      *-----------------------------------------------------------------
      *    Áreas e Books Utilizados no Tratamento da Mensageria
      *-----------------------------------------------------------------
       01  IC-TAB                      PIC  9(02) VALUE ZEROS.

      *--> Auxiliares Movimentacao Monitoração
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP-NR      PIC  9(11).

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

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

       01  CTE-FINAL-SS                PIC  X(40) VALUE
               '*** Storage Section termina aqui ***'.

      *-----------------------------------------------------------------
       LINKAGE                         SECTION.
      *-----------------------------------------------------------------
       01  DFHCOMMAREA.
-INC SGTKT009

      *-----------------------------------------------------------------
       PROCEDURE                       DIVISION USING DFHCOMMAREA.
      *-----------------------------------------------------------------

      *--> Teste obrigatorio do tamanho da linkage nao retirar
           IF  EIBCALEN NOT EQUAL LENGTH OF DFHCOMMAREA
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

           PERFORM 100000-VALIDA-PARAMETRO

           EVALUATE TRUE
              WHEN SGTST009-INCLUIR
                   PERFORM 400000-INCLUIR
              WHEN SGTST009-ALTERAR
                   PERFORM 500000-ALTERAR
              WHEN SGTST009-EXCLUIR
                   PERFORM 600000-EXCLUIR
              WHEN SGTST009-CONSULTAR
                   IF  CHAVE-CACHE         EQUAL SGTST009-CHAVE
                       MOVE  DADOS-CACHE
                                       TO  SGTST009-AREA-DADOS
                   ELSE
                       PERFORM 700000-CONSULTA
                   END-IF
              WHEN SGTST009-CONSULTAR-EST-ATIVO
                   PERFORM 800000-CONSULTA-EST-ATIVO
           END-EVALUATE

           GOBACK
           .
      *-----------------------------------------------------------------
       100000-VALIDA-PARAMETRO         SECTION.
      *-----------------------------------------------------------------
           MOVE  0002                  TO  PSC-ERRO-PGM

           INITIALIZE                      SGTST009-RTN-PGM

           IF  NOT SGTST009-INCLUIR
           AND NOT SGTST009-EXCLUIR
           AND NOT SGTST009-ALTERAR
           AND NOT SGTST009-CONSULTAR
           AND NOT SGTST009-CONSULTAR-EST-ATIVO
               MOVE  0116              TO  CD-RTN-PGM
               MOVE  'Ação inválida.'  TO  MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           PERFORM 130000-VALIDA-CAMPOS

           IF  SGTST009-INCLUIR
           OR  SGTST009-ALTERAR
               PERFORM 160000-TRATA-NULIDADE

               PERFORM 170001-VALIDA-OPR-ETRD-MCP
               IF  CD-TIP-EST-OPR-N        EQUAL ZEROS
                   PERFORM 170002-VALIDA-TIP-EST-OPR-ETRD
               END-IF

               IF  SGTST009-CD-MTV-ALT     NOT  EQUAL ZEROS
                   PERFORM 170003-VALIDA-CD-MTV-ALT
               END-IF
           END-IF
           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       130000-VALIDA-CAMPOS            SECTION.
      *-----------------------------------------------------------------
           MOVE  0003                  TO  PSC-ERRO-PGM

           IF  SGTST009-CONSULTAR
               IF  SGTST009-TS-EST-OPR-ETRD EQUAL SPACES
                   MOVE  0345          TO  CD-RTN-PGM
                   MOVE 'Timestamp estado operação estruturado não infor
      -                 'mado.'        TO  MSG-ERRO
                   PERFORM 999001-ERRO-001
               END-IF
           END-IF

           IF  SGTST009-CONSULTAR
           OR  SGTST009-CONSULTAR-EST-ATIVO

               IF  SGTST009-CD-CTRA-TIT-VL EQUAL ZEROS
                   MOVE  0002          TO  CD-RTN-PGM
                   MOVE  'Código da carteira de título e valores mobiliá
      -                  'rios não informado.'
                                       TO  MSG-ERRO
                   PERFORM 999001-ERRO-001
               END-IF

               IF  SGTST009-CD-OPR-ETRD-MCP EQUAL ZEROS
                   MOVE  0005          TO  CD-RTN-PGM
                   MOVE  'Código operação estruturada mercado de capitai
      -                  's não informado.'
                                       TO  MSG-ERRO
                   PERFORM 999001-ERRO-001
               END-IF

               IF NOT SGTST009-CONSULTAR-EST-ATIVO
                  IF  SGTST009-NR-VRS-OPR-ETRD EQUAL ZEROS
                      MOVE  0007          TO  CD-RTN-PGM
                   MOVE  'Número versão da operação estruturada não info
      -                  'rmado.'      TO  MSG-ERRO
                      PERFORM 999001-ERRO-001
                   END-IF
               END-IF
           END-IF

           IF  SGTST009-INCLUIR
           OR  SGTST009-ALTERAR
               IF  SGTST009-CD-USU-RSP-ATL EQUAL SPACES
                   MOVE  0278          TO  CD-RTN-PGM
                   MOVE  'Código do usuário responsável pela atualização
      -                  'não informado.'
                                       TO  MSG-ERRO
                   PERFORM 999001-ERRO-001
               END-IF
           END-IF

           IF  SGTST009-EXCLUIR
               IF  SGTST009-CD-USU-RSP-ATL EQUAL SPACES
                   MOVE  0278          TO  CD-RTN-PGM
                   MOVE  'Código do usuário responsável pela atualização
      -                  'não informado.'
                                       TO  MSG-ERRO
                   PERFORM 999001-ERRO-001
               END-IF
           END-IF

           PERFORM 140000-BUSCA-TS-DT

           MOVE  HLPKE001-TS-FMTD      TO  TS-SQL-ATU

           IF  SGTST009-TS-EST-OPR-ETRD    EQUAL 'CURRENT'
               MOVE TS-SQL-ATU         TO  SGTST009-TS-EST-OPR-ETRD
           END-IF
           .
       130000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       140000-BUSCA-TS-DT              SECTION.
      *-----------------------------------------------------------------
           MOVE  0004                  TO  PSC-ERRO-PGM

           CALL SBTIMEDT            USING  HLPKE001-CD-FUC
                                           HLPKE001-AREA-TRB
                                           HLPKE001-FMT-TS
                                           HLPKE001-TAM-TS
                                           HLPKE001-TS-FMTD

           IF  RETURN-CODE                 NOT EQUAL ZEROS
               SET   ERRO-DE-PGM       TO  TRUE
               MOVE  0050              TO  CD-RTN-PGM
               MOVE  'Erro ao obter data e hora atual.'
                                       TO  MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF
           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       160000-TRATA-NULIDADE           SECTION.
      *-----------------------------------------------------------------
           MOVE  0005                  TO  PSC-ERRO-PGM

           IF  SGTST009-CD-TIP-EST-OPR     EQUAL ZEROS
               MOVE  -001              TO  CD-TIP-EST-OPR-N
           END-IF

           IF  SGTST009-TX-JST-EST         EQUAL SPACES
               MOVE  -001              TO  TX-JST-EST-N
           END-IF

           IF  SGTST009-CD-MTV-ALT         EQUAL ZEROS
               MOVE  -001              TO  CD-MTV-ALT-N
           END-IF
           .
       160000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       170001-VALIDA-OPR-ETRD-MCP      SECTION.
      *-----------------------------------------------------------------
           MOVE  0006                  TO  PSC-ERRO-PGM

           MOVE  ZEROS                 TO  QT-REG-SELD

           EXEC SQL
             SELECT 1
              INTO :QT-REG-SELD
               FROM DB2SGT.OPR_ETRD_MCP
               WHERE CD_CTRA_TIT_VL     =  :SGTST009-CD-CTRA-TIT-VL
                 AND CD_OPR_ETRD_MCP    =  :SGTST009-CD-OPR-ETRD-MCP
                 AND NR_VRS_OPR_ETRD    =  :SGTST009-NR-VRS-OPR-ETRD
           END-EXEC

           IF  QT-REG-SELD                 EQUAL ZEROS
               MOVE  0061              TO  CD-RTN-PGM
               MOVE  'Operação estruturada inexistente.'
                                       TO  MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF
           .
       170001-SAI.
           EXIT.

      *-----------------------------------------------------------------
       170002-VALIDA-TIP-EST-OPR-ETRD  SECTION.
      *-----------------------------------------------------------------
           MOVE  0007                  TO  PSC-ERRO-PGM

           MOVE  ZEROS                 TO  QT-REG-SELD

           EXEC SQL
             SELECT 1
              INTO :QT-REG-SELD
               FROM DB2SGT.TIP_EST_OPR_ETRD
               WHERE CD_TIP_EST_OPR     =  :SGTST009-CD-TIP-EST-OPR
           END-EXEC

           IF  QT-REG-SELD                 EQUAL ZEROS
               MOVE  0349              TO  CD-RTN-PGM
               MOVE  'Tipo do estado da operação estruturado inexistente
      -              '.'               TO  MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF
           .
       170002-SAI.
           EXIT.
      *-----------------------------------------------------------------
       170003-VALIDA-CD-MTV-ALT  SECTION.
      *-----------------------------------------------------------------
           MOVE  0007                  TO  PSC-ERRO-PGM

           MOVE  ZEROS                 TO  QT-REG-SELD

           EXEC SQL
             SELECT 1
              INTO :QT-REG-SELD
               FROM DB2SGT.EST_MTV_ALT
               WHERE CD_MTV_ALT         =  :SGTST009-CD-MTV-ALT
                 AND CD_TIP_EST_OPR     =  :SGTST009-CD-TIP-EST-OPR
           END-EXEC

           IF  QT-REG-SELD                 EQUAL ZEROS
               MOVE  0760              TO  CD-RTN-PGM
               MOVE  'Tipo do motivo para estado o da operação estrutura
      -              'da inexistente.' TO  MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF
           .
       170003-SAI.
           EXIT.
      *-----------------------------------------------------------------
       400000-INCLUIR                  SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO  PSC-ERRO-PGM

      *--> Desligar erro automatico do DB2
           EXEC SQL WHENEVER
                    SQLERROR CONTINUE
           END-EXEC

           EXEC SQL
                INSERT INTO DB2SGT.EST_OPR_ETRD
                ( CD_CTRA_TIT_VL
                , CD_OPR_ETRD_MCP
                , NR_VRS_OPR_ETRD
                , TS_EST_OPR_ETRD
                , CD_USU_RSP_EST
                , CD_TIP_EST_OPR
                , TX_JST_EST
                , CD_MTV_ALT)
                VALUES
                ( :SGTST009-CD-CTRA-TIT-VL
                , :SGTST009-CD-OPR-ETRD-MCP
                , :SGTST009-NR-VRS-OPR-ETRD
                , :SGTST009-TS-EST-OPR-ETRD
                , :SGTST009-CD-USU-RSP-EST
                , :SGTST009-CD-TIP-EST-OPR
                  :CD-TIP-EST-OPR-N
                , :SGTST009-TX-JST-EST
                  :TX-JST-EST-N
                , :SGTST009-CD-MTV-ALT
                  :CD-MTV-ALT-N )
           END-EXEC

      *--> Ligar erro automatico do DB2
           EXEC SQL WHENEVER SQLERROR
                    GO TO 999002-ERRO-DB2
           END-EXEC

           EVALUATE  SQLCODE
               WHEN  -803
                     MOVE  0803        TO  CD-RTN-PGM
                     MOVE 'Inclusão não efetuada, registro existente.'
                                       TO  MSG-ERRO
                     PERFORM  999001-ERRO-001
               WHEN  ZEROS
                     CONTINUE
               WHEN  OTHER
      *--> Aciona o tratamento de erro padrao para os demais casos
                     PERFORM  999002-ERRO-DB2
           END-EVALUATE

           PERFORM 980101-GUARDA-CACHE

           INITIALIZE                      DADOS-LOG
           MOVE  'I'                   TO  SGTS014G-CD-TIP-ACAO
           PERFORM 980102-GRAVA-LOG
           .
       400000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       500000-ALTERAR                  SECTION.
      *-----------------------------------------------------------------
           MOVE  0009                  TO  PSC-ERRO-PGM

           EXEC SQL
             UPDATE DB2SGT.EST_OPR_ETRD
             SET CD_USU_RSP_EST         =  :SGTST009-CD-USU-RSP-EST
               , CD_TIP_EST_OPR         =  :SGTST009-CD-TIP-EST-OPR
                                           :CD-TIP-EST-OPR-N
               , TX_JST_EST             =  :SGTST009-TX-JST-EST
                                           :TX-JST-EST-N
               , CD_MTV_ALT             =  :SGTST009-CD-MTV-ALT
                                           :CD-MTV-ALT-N
                WHERE CD_CTRA_TIT_VL    =  :SGTST009-CD-CTRA-TIT-VL
                  AND CD_OPR_ETRD_MCP   =  :SGTST009-CD-OPR-ETRD-MCP
                  AND NR_VRS_OPR_ETRD   =  :SGTST009-NR-VRS-OPR-ETRD
                  AND TS_EST_OPR_ETRD   =  :SGTST009-TS-EST-OPR-ETRD
           END-EXEC

           IF  SQLCODE                     EQUAL +100
               INITIALIZE                  CHAVE-CACHE
               MOVE  0100              TO  CD-RTN-PGM
               MOVE  'Registro inexistente.'
                                       TO  MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           PERFORM 980101-GUARDA-CACHE

           INITIALIZE                      DADOS-LOG
           MOVE  'A'                   TO  SGTS014G-CD-TIP-ACAO
           PERFORM 980102-GRAVA-LOG
           .
       500000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       600000-EXCLUIR                  SECTION.
      *-----------------------------------------------------------------
           MOVE  0010                  TO  PSC-ERRO-PGM

           INITIALIZE                      DADOS-LOG
           MOVE  'E'                   TO  SGTS014G-CD-TIP-ACAO
           PERFORM 980102-GRAVA-LOG

           EXEC SQL
                DELETE FROM DB2SGT.EST_OPR_ETRD
                WHERE CD_CTRA_TIT_VL    =  :SGTST009-CD-CTRA-TIT-VL
                  AND CD_OPR_ETRD_MCP   =  :SGTST009-CD-OPR-ETRD-MCP
                  AND NR_VRS_OPR_ETRD   =  :SGTST009-NR-VRS-OPR-ETRD
                  AND TS_EST_OPR_ETRD   =  :SGTST009-TS-EST-OPR-ETRD
           END-EXEC

           INITIALIZE                      REG-CACHE

           IF  SQLCODE                     EQUAL +100
               MOVE  0100              TO  CD-RTN-PGM
               MOVE  'Registro inexistente.'
                                       TO  MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF
           .
       600000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       700000-CONSULTA                 SECTION.
      *-----------------------------------------------------------------
           MOVE  0011                  TO  PSC-ERRO-PGM

      *--> Campos nulos sao inicializados na consulta
           INITIALIZE                      SGTST009-CD-TIP-EST-OPR
                                           SGTST009-TX-JST-EST

           EXEC SQL
                 SELECT CD_USU_RSP_EST
                      , CD_TIP_EST_OPR
                      , TX_JST_EST
                      , CD_MTV_ALT
                   INTO  :SGTST009-CD-USU-RSP-EST
                       , :SGTST009-CD-TIP-EST-OPR
                         :CD-TIP-EST-OPR-N
                       , :SGTST009-TX-JST-EST
                         :TX-JST-EST-N
                       , :SGTST009-CD-MTV-ALT
                         :CD-MTV-ALT-N
                       FROM DB2SGT.EST_OPR_ETRD
                WHERE CD_CTRA_TIT_VL    =  :SGTST009-CD-CTRA-TIT-VL
                  AND CD_OPR_ETRD_MCP   =  :SGTST009-CD-OPR-ETRD-MCP
                  AND NR_VRS_OPR_ETRD   =  :SGTST009-NR-VRS-OPR-ETRD
                  AND TS_EST_OPR_ETRD   =  :SGTST009-TS-EST-OPR-ETRD
           END-EXEC

           IF  SQLCODE                     EQUAL +100
               INITIALIZE                  SGTST009-AREA-DADOS
               MOVE  0100              TO  CD-RTN-PGM
               MOVE  'Registro inexistente.'
                                       TO  MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           PERFORM 980101-GUARDA-CACHE
           .
       700000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       800000-CONSULTA-EST-ATIVO       SECTION.
      *-----------------------------------------------------------------
           MOVE  0012                  TO  PSC-ERRO-PGM

      *--> Campos nulos sao inicializados na consulta
           INITIALIZE                  SGTST009-CD-TIP-EST-OPR

      * Esta consulta não deve considerar a versão (NR_VRS_OPR_ETRD)
      * pois em todas as situaç es na qual esta consulta é usada
      * deve retornar o estado atual da operação independente da versão

           EXEC SQL
                 SELECT CD_TIP_EST_OPR
                      , TS_EST_OPR_ETRD
                      , CD_MTV_ALT
                   INTO    :SGTST009-CD-TIP-EST-OPR
                           :CD-TIP-EST-OPR-N
                       ,   :SGTST009-TS-EST-OPR-ETRD-S
                       , :SGTST009-CD-MTV-ALT
                         :CD-MTV-ALT-N
                       FROM DB2SGT.EST_OPR_ETRD
                WHERE CD_CTRA_TIT_VL     = :SGTST009-CD-CTRA-TIT-VL
                  AND CD_OPR_ETRD_MCP    = :SGTST009-CD-OPR-ETRD-MCP
                ORDER BY TS_EST_OPR_ETRD  DESC
                FETCH FIRST 1 ROW ONLY
           END-EXEC

           IF  SQLCODE                     EQUAL +100
               INITIALIZE                  SGTST009-AREA-DADOS
               MOVE  0100              TO  CD-RTN-PGM
               MOVE  'Registro inexistente.'
                                       TO  MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           PERFORM 980101-GUARDA-CACHE
           .
       800000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       980101-GUARDA-CACHE             SECTION.
      *-----------------------------------------------------------------
           MOVE  0013                  TO  PSC-ERRO-PGM

           MOVE  SGTST009-CHAVE        TO  CHAVE-CACHE
           MOVE  SGTST009-AREA-DADOS   TO  DADOS-CACHE
           .
       980101-SAI.
           EXIT.

      *-----------------------------------------------------------------
       980102-GRAVA-LOG                SECTION.
      *-----------------------------------------------------------------
           MOVE  0014                  TO  PSC-ERRO-PGM

           MOVE  'EST_OPR_ETRD      '  TO  SGTS014G-NM-ENT

           IF  EXECUCAO-ONLINE
               MOVE  SGTST009-CD-USU-RSP-ATL
                                       TO  SGTS014G-CD-CHV-USU
           ELSE
               MOVE  'SISTEMA'         TO  SGTS014G-CD-CHV-USU
           END-IF

           MOVE  'SGTST009'            TO  SGTS014G-NM-PGM
           MOVE  SGTST009-CD-CTRA-TIT-VL
                                       TO  CD-CTRA-TIT-VL-LOG
           MOVE  SGTST009-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP-LOG
           MOVE  SGTST009-NR-VRS-OPR-ETRD
                                       TO  NR-VRS-OPR-ETRD-LOG
           MOVE  SGTST009-TS-EST-OPR-ETRD
                                       TO  TS-EST-OPR-ETRD-LOG

           STRING  CD-CTRA-TIT-VL-LOG
            ';'    CD-OPR-ETRD-MCP-LOG
            ';'    NR-VRS-OPR-ETRD-LOG
            ';'    TS-EST-OPR-ETRD-LOG
                   DELIMITED BY SIZE
                   INTO                    SGTS014G-CHV-PRMR

           MOVE  LENGTH OF DADOS-LOG   TO  EIBCALEN

           CALL SGTS014G USING DFHEIBLK DADOS-LOG

           IF  SGTS014G-CD-RTN-PGM         NOT EQUAL  ZEROS
               SET   ERRO-DE-PGM       TO  TRUE
               MOVE  0112              TO  CD-RTN-PGM
               MOVE  'Erro na gravação das informações do Log.'
                                       TO  MSG-ERRO-PGM
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
           MOVE  SGTST009-TIP-RTN-PGM  TO  TIP-RTN-PGM
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

           MOVE 'SGTST009-CD-CHV-USU'          TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST009-CD-CHV-USU           TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTST009-ACAO'                TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST009-ACAO                 TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTST009-CD-CTRA-TIT-VL'      TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST009-CD-CTRA-TIT-VL       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTST009-CD-OPR-ETRD-MCP'     TO  MSGKW999-NM-VRV-MNT
           IF  SGTST009-CD-OPR-ETRD-MCP        IS NUMERIC
               MOVE  SGTST009-CD-OPR-ETRD-MCP  TO  CD-OPR-ETRD-MCP-NR
               MOVE  CD-OPR-ETRD-MCP-TX        TO  MSGKW999-VL-VRV-MNT
           ELSE
                   MOVE  'Valor não numérico'  TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTST009-NR-VRS-OPR-ETRD'     TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST009-NR-VRS-OPR-ETRD      TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTST009-TS-EST-OPR-ETRD'     TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST009-TS-EST-OPR-ETRD      TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTST009-CD-USU-RSP-ATL'      TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST009-CD-USU-RSP-ATL       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTST009-CD-USU-RSP-EST'      TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST009-CD-USU-RSP-EST       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTST009-CD-TIP-EST-OPR'      TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST009-CD-TIP-EST-OPR       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTST009-TX-JST-EST'          TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST009-TX-JST-EST           TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTST009-CD-MTV-ALT'          TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST009-CD-MTV-ALT           TO  MSGKW999-VL-VRV-MNT
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
           MOVE  RTN-PGM               TO  SGTST009-RTN-PGM

           IF  EXECUCAO-ONLINE
               PERFORM  999000-TRATAR-ERRO-MSG
           END-IF

           GOBACK
           .
       999999-SAI.
           EXIT.
      *---------------------- FIM SGTST009 ----------------------------