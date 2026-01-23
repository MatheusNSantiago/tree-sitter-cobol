      *-----------------------------------------------------------------
      * PROGRAMA..: SGTST011
      * ANALISTA..: F8186823 - CINTIA IGLESIAS
      * AUTOR.....: F8785328 - ROSELAINE OGRIZEK
      * DATA......: 19.05.2015
      * OBJETIVO..: PERSISTENCIA DA TABELA RMNC_OPR_ETRD
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS003 16/02/2018 F8658869  DE 2016/145195 - Tratamento de men-
      *                                              sageria
      * VRS002 05/12/2017 F2020377  DE 2016/145195 - Recompilação para
      *                                compatibilização do tamanho do
      *                                campo SGTST011-PC-TAXA-RMNC-OPR
      *                                com o respectivo campo da tabela
      *                                (de 4 para 10 casas decimais).
      *        13/02/2017 F8785286  DE 2016/145195 - Inclusão movimen-
      *                                tação campo book para chave log.
      *        08/09/2015 F2054109  DE 2015/120821 - Recompilação por
      *                                alteração da tabela OPR_ETRD_MCP
      * VRS001 19.05.2015 F8785328  DE 2015/134565 - Implantação
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTST011.

      *-----------------------------------------------------------------
       ENVIRONMENT                     DIVISION.
      *-----------------------------------------------------------------
       CONFIGURATION SECTION.
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
                                       VALUE '*** SGTST011 ***'.
       77  CTE-VERS                    PIC  X(06) VALUE 'VRS002'.

      *--> Registro da tabela
       01  REG-CACHE.
           03  CHAVE-CACHE.
               05  CD-CTRA-TIT-VL      PIC S9(09) COMP-5 VALUE 0.
               05  CD-OPR-ETRD-MCP     PIC S9(11) COMP-3 VALUE 0.
               05  NR-VRS-OPR-ETRD     PIC S9(04) COMP-5 VALUE 0.
               05  DT-INC-TAXA-RMNC    PIC  X(10) VALUE ' '.
               05  DT-INC-TAXA-RMNC-R  REDEFINES
                   DT-INC-TAXA-RMNC.
                   07  DIA             PIC  9(02).
                   07  FILLER          PIC  X(01).
                   07  MES             PIC  9(02).
                   07  FILLER          PIC  X(01).
                   07  ANO             PIC  9(04).
               05  FILLER              PIC  X(20) VALUE ' '.
           03  DADOS-CACHE.
               05  TS-ULT-ATL          PIC  X(26) VALUE ' '.
               05  CD-USU-RSP-ATL      PIC  X(08) VALUE ' '.
               05  PC-TAXA-RMNC-OPR    PIC S9(04)V9(10) COMP-3 VALUE 0.
               05  DT-FIM-TAXA-RMNC    PIC  X(10) VALUE ' '.
               05  DT-FIM-TAXA-RMNC-R REDEFINES
                   DT-FIM-TAXA-RMNC.
                   07  DIA             PIC  9(02).
                   07  FILLER          PIC  X(01).
                   07  MES             PIC  9(02).
                   07  FILLER          PIC  X(01).
                   07  ANO             PIC  9(04).

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
       01  QT-REG-SELD                 PIC S9(04) COMP-5 VALUE 0.
       01  DT-SQL-ATU                  PIC  X(10)
                                                 VALUE '  .  .    '.
       01  DT-SQL-ATU-R REDEFINES DT-SQL-ATU.
           03  DIA                     PIC  9(02).
           03  FILLER                  PIC  X(01).
           03  MES                     PIC  9(02).
           03  FILLER                  PIC  X(01).
           03  ANO                     PIC  9(04).
       01  TS-SQL-ATU                  PIC  X(26) VALUE ' '.

      *--> Indicadores de nulidades
       01  QT-REG-SELD-NULO            PIC S9(04) COMP-5 VALUE 0.
       01  DT-FIM-TAXA-RMNC-N          PIC S9(04) COMP-5 VALUE 0.

      *--> Variaveis para gravacao do log
       01  DADOS-LOG-DISPLAY.
           03  CD-CTRA-TIT-VL-LOG      PIC  9(10) VALUE 0.
           03  CD-OPR-ETRD-MCP-LOG     PIC  9(11) VALUE 0.
           03  NR-VRS-OPR-ETRD-LOG     PIC  9(05) VALUE 0.
           03  DT-INC-TAXA-RMNC-LOG    PIC  X(10) VALUE ' '.

      *--> Book dos dados do log
       01  DADOS-LOG.
-INC SGTK014G

      *--> Books da definicao de dados usados na subrotina de erros
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
       LINKAGE SECTION.
      *-----------------------------------------------------------------
       01  DFHCOMMAREA.
-INC SGTKT011

      *-----------------------------------------------------------------
       PROCEDURE                      DIVISION USING DFHCOMMAREA.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------

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
              WHEN SGTST011-INCLUIR
                   PERFORM 400000-INCLUIR
              WHEN SGTST011-ALTERAR
                   PERFORM 500000-ALTERAR
              WHEN SGTST011-EXCLUIR
                   PERFORM 600000-EXCLUIR
              WHEN SGTST011-CONSULTAR
                   IF  CHAVE-CACHE         EQUAL  SGTST011-CHAVE
                       MOVE DADOS-CACHE
                                      TO  SGTST011-AREA-DADOS
                   ELSE
                       PERFORM 700000-CONSULTA
                   END-IF
              WHEN SGTST011-CONSULTAR-TX-ATIVA
                   PERFORM 800000-CONSULTA-TX-ATIVA
              WHEN SGTST011-CSUL-TX-RMNC-DRIA
                   PERFORM 900000-CONSULTAR-TX-RMNC-DRIA
              WHEN SGTST011-CSUL-TX-RMNC-DT-INFD
                   PERFORM 950000-CONS-TX-RMNC-DT-INFD
           END-EVALUATE

           GOBACK
           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-VALIDA-PARAMETRO         SECTION.
      *-----------------------------------------------------------------
           MOVE  0002                 TO  PSC-ERRO-PGM
           INITIALIZE                      SGTST011-RTN-PGM

           IF  NOT SGTST011-INCLUIR
           AND NOT SGTST011-EXCLUIR
           AND NOT SGTST011-ALTERAR
           AND NOT SGTST011-CONSULTAR
           AND NOT SGTST011-CONSULTAR-TX-ATIVA
           AND NOT SGTST011-CSUL-TX-RMNC-DRIA
           AND NOT SGTST011-CSUL-TX-RMNC-DT-INFD
               MOVE  0116              TO  CD-RTN-PGM
               MOVE  'Ação inválida.'  TO  MSG-ERRO
               PERFORM  999001-ERRO-001
           END-IF

           PERFORM 130000-VALIDA-CAMPOS

           IF  SGTST011-INCLUIR
           OR  SGTST011-ALTERAR
               PERFORM 160000-TRATA-NULIDADE
               PERFORM 170001-VALIDA-OPR-ETRD-MCP
           END-IF
           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       130000-VALIDA-CAMPOS            SECTION.
      *-----------------------------------------------------------------
           MOVE  0003                 TO  PSC-ERRO-PGM

      *--> Validar Carteira do Título
           IF  SGTST011-CD-CTRA-TIT-VL     EQUAL ZEROS
               MOVE  0002              TO  CD-RTN-PGM
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' não informado.'
                                       TO  MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           IF  SGTST011-CD-CTRA-TIT-VL     NOT POSITIVE
               MOVE  0001              TO  CD-RTN-PGM
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' inválido.'      TO  MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

      *--> Validar Código da Operação Estruturada
           IF  SGTST011-CD-OPR-ETRD-MCP    NOT NUMERIC
               MOVE  0003              TO  CD-RTN-PGM
               MOVE  'Código operação estruturada mercado de capitais nã
      -              'o numérico.'     TO  MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF
           IF  SGTST011-CD-OPR-ETRD-MCP    EQUAL ZEROS
               MOVE  0005              TO  CD-RTN-PGM
               MOVE  'Código operação estruturada mercado de capitais nã
      -              'o informado.'    TO  MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF
           IF  SGTST011-CD-OPR-ETRD-MCP    NOT POSITIVE
               MOVE  0004              TO  CD-RTN-PGM
               MOVE  'Código operação estruturada mercado de capitais in
      -              'válido.'         TO  MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

      *--> Validar Versão da Operação Estruturada
           IF  SGTST011-NR-VRS-OPR-ETRD    EQUAL ZEROS
               MOVE  0007              TO  CD-RTN-PGM
               MOVE  'Número da versão da operação estruturada não infor
      -              'mado.'           TO  MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           IF  SGTST011-NR-VRS-OPR-ETRD    NOT POSITIVE
               MOVE  0006              TO  CD-RTN-PGM
               MOVE  'Número da versão da operação estruturada inválido.
      -              ''                TO  MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

      *--> Validar outros Campos
           IF  NOT SGTST011-CONSULTAR-TX-ATIVA
               IF  SGTST011-DT-INC-TAXA-RMNC
                                           EQUAL  SPACES
                    MOVE  0327          TO  CD-RTN-PGM
                    MOVE  'Data início da taxa remuneração não informada
      -                   '.'           TO  MSG-ERRO
                    PERFORM 999001-ERRO-001
               END-IF
           END-IF

           IF  SGTST011-INCLUIR
           OR  SGTST011-ALTERAR
               IF  SGTST011-TS-ULT-ATL     EQUAL  SPACES
                   MOVE  0106          TO  CD-RTN-PGM
                   MOVE  'Timestamp última atualização não informado.'
                                      TO  MSG-ERRO
                   PERFORM 999001-ERRO-001
               END-IF
               IF  SGTST011-CD-USU-RSP-ATL EQUAL  SPACES
                   MOVE  0226          TO  CD-RTN-PGM
                   MOVE  'Código do usuário responsável pela atualização
      -                  ' inválido.'      TO  MSG-ERRO
                   PERFORM 999001-ERRO-001
               END-IF
               IF  SGTST011-PC-TAXA-RMNC-OPR
                                           NOT NUMERIC
                   MOVE  0329          TO  CD-RTN-PGM
                   MOVE  'Percentual da taxa remuneração da operação não
      -                  ' númerico.'  TO  MSG-ERRO
                   PERFORM 999001-ERRO-001
               END-IF
               IF  SGTST011-PC-TAXA-RMNC-OPR
                                           EQUAL  ZEROS
                   MOVE  0330          TO  CD-RTN-PGM
                   MOVE  'Percentual da taxa remuneração da operação não
      -                  ' informado.'
                                       TO  MSG-ERRO
                   PERFORM 999001-ERRO-001

               END-IF
               IF  SGTST011-PC-TAXA-RMNC-OPR
                                           NOT POSITIVE
                   MOVE  0331          TO  CD-RTN-PGM
                   MOVE  'Percentual da taxa remuneração da operação inv
      -                  'álido.'      TO  MSG-ERRO
                   PERFORM 999001-ERRO-001

               END-IF
           END-IF

      *--> Obtem Timestamp e Data e Trata CURRENT
           PERFORM 140000-BUSCA-TS-DT

           MOVE  CORR HLPKE001-TS-FMTD
                                      TO  DT-SQL-ATU-R
           MOVE  HLPKE001-TS-FMTD     TO  TS-SQL-ATU

           IF  SGTST011-DT-INC-TAXA-RMNC   EQUAL  'CURRENT'
               MOVE  DT-SQL-ATU       TO  SGTST011-DT-INC-TAXA-RMNC
           END-IF
           IF  SGTST011-TS-ULT-ATL         EQUAL  'CURRENT'
               MOVE  TS-SQL-ATU       TO  SGTST011-TS-ULT-ATL
           END-IF
           IF  SGTST011-DT-FIM-TAXA-RMNC   EQUAL  'CURRENT'
               MOVE  DT-SQL-ATU       TO  SGTST011-DT-FIM-TAXA-RMNC
           END-IF
           .
       130000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       140000-BUSCA-TS-DT              SECTION.
      *-----------------------------------------------------------------
           MOVE  0004                 TO  PSC-ERRO-PGM
           CALL SBTIMEDT            USING HLPKE001-CD-FUC
                                          HLPKE001-AREA-TRB
                                          HLPKE001-FMT-TS
                                          HLPKE001-TAM-TS
                                          HLPKE001-TS-FMTD

           IF  RETURN-CODE                 NOT EQUAL ZEROS
               SET   SGTST011-ERRO-DE-PGM
                                       TO  TRUE
               MOVE  SBTIMEDT          TO  PGM-ERRO
               MOVE  0050              TO  CD-RTN-PGM
               MOVE  'Erro ao obter data e hora atual.'
                                       TO  MSG-ERRO
           END-IF
           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       160000-TRATA-NULIDADE           SECTION.
      *-----------------------------------------------------------------
           MOVE  0005                 TO  PSC-ERRO-PGM

           IF  SGTST011-DT-FIM-TAXA-RMNC   EQUAL  SPACES
               MOVE  -001             TO  DT-FIM-TAXA-RMNC-N
           END-IF

           .
       160000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       170001-VALIDA-OPR-ETRD-MCP      SECTION.
      *-----------------------------------------------------------------
           MOVE  0006                 TO  PSC-ERRO-PGM

           MOVE  ZEROS                TO  QT-REG-SELD

           EXEC SQL
             SELECT 1
              INTO :QT-REG-SELD
               FROM DB2SGT.OPR_ETRD_MCP
               WHERE CD_CTRA_TIT_VL     = :SGTST011-CD-CTRA-TIT-VL
                 AND CD_OPR_ETRD_MCP    = :SGTST011-CD-OPR-ETRD-MCP
                 AND NR_VRS_OPR_ETRD    = :SGTST011-NR-VRS-OPR-ETRD
           END-EXEC

           IF  QT-REG-SELD                 EQUAL  ZEROS
               MOVE  0061              TO  CD-RTN-PGM
               MOVE  'Operação estruturada inexistente.'
      -                                TO  MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF
           .
       170001-SAI.
           EXIT.

      *-----------------------------------------------------------------
       400000-INCLUIR                  SECTION.
      *-----------------------------------------------------------------
           MOVE  0007                 TO  PSC-ERRO-PGM

      *--> Desligar erro automatico do DB2
           EXEC SQL WHENEVER SQLERROR
                    CONTINUE
           END-EXEC

           EXEC SQL
                INSERT INTO DB2SGT.RMNC_OPR_ETRD
                ( CD_CTRA_TIT_VL
                , CD_OPR_ETRD_MCP
                , NR_VRS_OPR_ETRD
                , DT_INC_TAXA_RMNC
                , TS_ULT_ATL
                , CD_USU_RSP_ATL
                , PC_TAXA_RMNC_OPR
                , DT_FIM_TAXA_RMNC
                , CD_TIP_FMA_APL
                , CD_IEC_RMNC
                , CD_PERC_CPTZ_JUR )
                VALUES
                ( :SGTST011-CD-CTRA-TIT-VL
                , :SGTST011-CD-OPR-ETRD-MCP
                , :SGTST011-NR-VRS-OPR-ETRD
                , :SGTST011-DT-INC-TAXA-RMNC
                , :SGTST011-TS-ULT-ATL
                , :SGTST011-CD-USU-RSP-ATL
                , :SGTST011-PC-TAXA-RMNC-OPR
                , :SGTST011-DT-FIM-TAXA-RMNC
                  :DT-FIM-TAXA-RMNC-N
                , :SGTST011-CD-TIP-FMA-APL
                , :SGTST011-CD-IEC-RMNC
                , :SGTST011-CD-PERC-CPTZ-JUR)
           END-EXEC

      *--> Ligar erro automatico do DB2
           EXEC SQL WHENEVER
                    SQLERROR   GO TO  999002-ERRO-DB2
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
           MOVE  'I'                  TO  SGTS014G-CD-TIP-ACAO

           PERFORM 980102-GRAVA-LOG
           .
       400000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       500000-ALTERAR                  SECTION.
      *-----------------------------------------------------------------
           MOVE  0008                 TO  PSC-ERRO-PGM

           EXEC SQL
             UPDATE DB2SGT.RMNC_OPR_ETRD
             SET TS_ULT_ATL         = :SGTST011-TS-ULT-ATL
               , CD_USU_RSP_ATL     = :SGTST011-CD-USU-RSP-ATL
               , PC_TAXA_RMNC_OPR   = :SGTST011-PC-TAXA-RMNC-OPR
               , DT_FIM_TAXA_RMNC   = :SGTST011-DT-FIM-TAXA-RMNC
                                      :DT-FIM-TAXA-RMNC-N
               , CD_TIP_FMA_APL     = :SGTST011-CD-TIP-FMA-APL
               , CD_IEC_RMNC        = :SGTST011-CD-IEC-RMNC
               , CD_PERC_CPTZ_JUR   = :SGTST011-CD-PERC-CPTZ-JUR
                WHERE CD_CTRA_TIT_VL     = :SGTST011-CD-CTRA-TIT-VL
                  AND CD_OPR_ETRD_MCP    = :SGTST011-CD-OPR-ETRD-MCP
                  AND NR_VRS_OPR_ETRD    = :SGTST011-NR-VRS-OPR-ETRD
                  AND DT_INC_TAXA_RMNC   = :SGTST011-DT-INC-TAXA-RMNC
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
           MOVE  'A'                  TO  SGTS014G-CD-TIP-ACAO

           PERFORM 980102-GRAVA-LOG
           .
       500000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       600000-EXCLUIR                  SECTION.
      *-----------------------------------------------------------------
           MOVE  0009                 TO  PSC-ERRO-PGM

           INITIALIZE                      DADOS-LOG
           MOVE  'E'                  TO  SGTS014G-CD-TIP-ACAO

           PERFORM 980102-GRAVA-LOG

           EXEC SQL
                DELETE FROM DB2SGT.RMNC_OPR_ETRD
                WHERE CD_CTRA_TIT_VL     = :SGTST011-CD-CTRA-TIT-VL
                  AND CD_OPR_ETRD_MCP    = :SGTST011-CD-OPR-ETRD-MCP
                  AND NR_VRS_OPR_ETRD    = :SGTST011-NR-VRS-OPR-ETRD
                  AND DT_INC_TAXA_RMNC   = :SGTST011-DT-INC-TAXA-RMNC
           END-EXEC

           INITIALIZE                      REG-CACHE

           IF  SQLCODE                     EQUAL +100
               INITIALIZE                  CHAVE-CACHE
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
           MOVE  0010                 TO  PSC-ERRO-PGM

      *--> Campos nulos sao inicializados na consulta
           INITIALIZE                      SGTST011-DT-FIM-TAXA-RMNC

           EXEC SQL
                 SELECT TS_ULT_ATL
                      , CD_USU_RSP_ATL
                      , PC_TAXA_RMNC_OPR
                      , DT_FIM_TAXA_RMNC
                      , CD_TIP_FMA_APL
                      , CD_IEC_RMNC
                      , CD_PERC_CPTZ_JUR
                   INTO  :SGTST011-TS-ULT-ATL
                       , :SGTST011-CD-USU-RSP-ATL
                       , :SGTST011-PC-TAXA-RMNC-OPR
                       , :SGTST011-DT-FIM-TAXA-RMNC
                         :DT-FIM-TAXA-RMNC-N
                       , :SGTST011-CD-TIP-FMA-APL
                       , :SGTST011-CD-IEC-RMNC
                       , :SGTST011-CD-PERC-CPTZ-JUR
                       FROM DB2SGT.RMNC_OPR_ETRD
                WHERE CD_CTRA_TIT_VL     = :SGTST011-CD-CTRA-TIT-VL
                  AND CD_OPR_ETRD_MCP    = :SGTST011-CD-OPR-ETRD-MCP
                  AND NR_VRS_OPR_ETRD    = :SGTST011-NR-VRS-OPR-ETRD
                  AND DT_INC_TAXA_RMNC   = :SGTST011-DT-INC-TAXA-RMNC
           END-EXEC

           IF  SQLCODE                     EQUAL  +100
               INITIALIZE                  SGTST011-AREA-DADOS
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
       800000-CONSULTA-TX-ATIVA        SECTION.
      *-----------------------------------------------------------------
           MOVE  0011                 TO  PSC-ERRO-PGM

      *--> Campos nulos sao inicializados na consulta
           INITIALIZE                      SGTST011-DT-FIM-TAXA-RMNC

           EXEC SQL
                 SELECT TS_ULT_ATL
                      , CD_USU_RSP_ATL
                      , PC_TAXA_RMNC_OPR
                      , DT_INC_TAXA_RMNC
                      , CD_TIP_FMA_APL
                      , CD_IEC_RMNC
                      , CD_PERC_CPTZ_JUR
                   INTO  :SGTST011-TS-ULT-ATL
                       , :SGTST011-CD-USU-RSP-ATL
                       , :SGTST011-PC-TAXA-RMNC-OPR
                       , :SGTST011-DT-INC-TAXA-RMNC
                       , :SGTST011-CD-TIP-FMA-APL
                       , :SGTST011-CD-IEC-RMNC
                       , :SGTST011-CD-PERC-CPTZ-JUR
                       FROM DB2SGT.RMNC_OPR_ETRD
                WHERE CD_CTRA_TIT_VL     = :SGTST011-CD-CTRA-TIT-VL
                  AND CD_OPR_ETRD_MCP    = :SGTST011-CD-OPR-ETRD-MCP
                  AND NR_VRS_OPR_ETRD    = :SGTST011-NR-VRS-OPR-ETRD
                  AND DT_FIM_TAXA_RMNC IS NULL
               FETCH FIRST 1 ROW ONLY
           END-EXEC

           IF  SQLCODE                     EQUAL  +100
               INITIALIZE                  SGTST011-AREA-DADOS
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
       900000-CONSULTAR-TX-RMNC-DRIA   SECTION.
      *-----------------------------------------------------------------
           MOVE  0012                 TO  PSC-ERRO-PGM

      *--> Campos nulos sao inicializados na consulta
           INITIALIZE                      SGTST011-PC-TAXA-RMNC-OPR

           EXEC SQL
                 SELECT PC_TAXA_RMNC_OPR
                      , CD_TIP_FMA_APL
                      , CD_IEC_RMNC
                      , CD_PERC_CPTZ_JUR
                   INTO  :SGTST011-PC-TAXA-RMNC-OPR
                       , :SGTST011-CD-TIP-FMA-APL
                       , :SGTST011-CD-IEC-RMNC
                       , :SGTST011-CD-PERC-CPTZ-JUR
                       FROM DB2SGT.RMNC_OPR_ETRD
                WHERE CD_CTRA_TIT_VL     = :SGTST011-CD-CTRA-TIT-VL
                  AND CD_OPR_ETRD_MCP    = :SGTST011-CD-OPR-ETRD-MCP
                  AND NR_VRS_OPR_ETRD    = :SGTST011-NR-VRS-OPR-ETRD
                  AND DT_INC_TAXA_RMNC   <= :SGTST011-DT-INC-TAXA-RMNC
               ORDER BY DT_INC_TAXA_RMNC DESC
               FETCH FIRST 1 ROW ONLY
           END-EXEC

           IF  SQLCODE                     EQUAL  +100
               INITIALIZE                  SGTST011-AREA-DADOS
               MOVE  0100              TO  CD-RTN-PGM
               MOVE  'Registro inexistente.'
                                       TO  MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           PERFORM 980101-GUARDA-CACHE
           .
       900000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       950000-CONS-TX-RMNC-DT-INFD     SECTION.
      *-----------------------------------------------------------------
           MOVE  0015                 TO  PSC-ERRO-PGM

      *--> Campos nulos sao inicializados na consulta
           INITIALIZE                      SGTST011-PC-TAXA-RMNC-OPR

           EXEC SQL
                 SELECT PC_TAXA_RMNC_OPR
                      , CD_TIP_FMA_APL
                      , CD_IEC_RMNC
                      , CD_PERC_CPTZ_JUR
                   INTO  :SGTST011-PC-TAXA-RMNC-OPR
                       , :SGTST011-CD-TIP-FMA-APL
                       , :SGTST011-CD-IEC-RMNC
                       , :SGTST011-CD-PERC-CPTZ-JUR
                       FROM DB2SGT.RMNC_OPR_ETRD
                WHERE CD_CTRA_TIT_VL     = :SGTST011-CD-CTRA-TIT-VL
                  AND CD_OPR_ETRD_MCP    = :SGTST011-CD-OPR-ETRD-MCP
                  AND NR_VRS_OPR_ETRD    = :SGTST011-NR-VRS-OPR-ETRD
                  AND DT_INC_TAXA_RMNC   <= :SGTST011-DT-INC-TAXA-RMNC
                  AND (DT_FIM_TAXA_RMNC   IS NULL
                  OR DT_FIM_TAXA_RMNC   >= :SGTST011-DT-INC-TAXA-RMNC)
               ORDER BY DT_INC_TAXA_RMNC DESC
               FETCH FIRST 1 ROW ONLY
           END-EXEC

           IF  SQLCODE                     EQUAL  +100
               INITIALIZE                  SGTST011-AREA-DADOS
               MOVE  0100              TO  CD-RTN-PGM
               MOVE  'Registro inexistente.'
                                       TO  MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           PERFORM 980101-GUARDA-CACHE
           .
       950000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       980101-GUARDA-CACHE             SECTION.
      *-----------------------------------------------------------------
           MOVE  0013                 TO  PSC-ERRO-PGM

           MOVE  SGTST011-CHAVE       TO  CHAVE-CACHE
           MOVE  SGTST011-AREA-DADOS  TO  DADOS-CACHE
           .
       980101-SAI.
           EXIT.

      *-----------------------------------------------------------------
       980102-GRAVA-LOG                SECTION.
      *-----------------------------------------------------------------
           MOVE  0014                 TO  PSC-ERRO-PGM

           MOVE  'RMNC_OPR_ETRD'      TO  SGTS014G-NM-ENT

           IF  EXECUCAO-ONLINE
               MOVE  SGTST011-CD-USU-RSP-ATL
                                      TO  SGTS014G-CD-CHV-USU
      *        EXEC CICS
      *             ASSIGN USERID(SGTS014G-CD-CHV-USU)
      *        END-EXEC
           ELSE
               MOVE  'SISTEMA'        TO  SGTS014G-CD-CHV-USU
           END-IF
           MOVE  'SGTST011'           TO  SGTS014G-NM-PGM
           MOVE  SGTST011-CD-CTRA-TIT-VL
                                      TO  CD-CTRA-TIT-VL-LOG
           MOVE  SGTST011-CD-OPR-ETRD-MCP
                                      TO  CD-OPR-ETRD-MCP-LOG
           MOVE  SGTST011-NR-VRS-OPR-ETRD
                                      TO  NR-VRS-OPR-ETRD-LOG
           MOVE  SGTST011-DT-INC-TAXA-RMNC
                                      TO  DT-INC-TAXA-RMNC-LOG
           STRING CD-CTRA-TIT-VL-LOG
           ';'    CD-OPR-ETRD-MCP-LOG
           ';'    NR-VRS-OPR-ETRD-LOG
           ';'    DT-INC-TAXA-RMNC-LOG
                  DELIMITED BY SIZE
                  INTO                    SGTS014G-CHV-PRMR

           MOVE  LENGTH OF DADOS-LOG  TO  EIBCALEN
           CALL  SGTS014G          USING  DFHEIBLK DADOS-LOG

           IF  SGTS014G-CD-RTN-PGM         NOT EQUAL ZEROS
               SET   SGTST011-ERRO-DE-PGM
                                       TO  TRUE
               MOVE  SGTS014G          TO  PGM-ERRO
               MOVE  0112              TO  CD-RTN-PGM
               MOVE  'Erro na gravação das informações do Log.'
                                       TO  MSG-ERRO
               PERFORM  999001-ERRO-001
           END-IF
           .
       980102-SAI.
           EXIT.

      *--------------------
      * 999000-ERRO SECTION
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

      *-----------------------------------------------------------------
      *    As Sections e Books abaixo fazem o tratamento de erro
      *    através do Sistema MSG (rotina MSGS0001)
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       999000-TRATAR-ERRO-MSG          SECTION.
      *--> Envia a Ocorrência de Erro para o Sistema de Mensageria
      *-----------------------------------------------------------------

      *--> Alterar o MOVE conforme o Book do módulo
           MOVE  SGTST011-TIP-RTN-PGM  TO  TIP-RTN-PGM
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

      *--> Adicionar Campos Chaves
           MOVE '001-ST011-CD-CHV-USU'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST011-CD-CHV-USU
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE '002-ST011-ACAO'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST011-ACAO
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE '003-ST011-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST011-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE '004-ST011-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
           IF  SGTST011-CD-OPR-ETRD-MCP    IS NUMERIC
               MOVE  SGTST011-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP-NR
               MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
           ELSE
                   MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE '005-ST011-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST011-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE '006-ST011-DT-INC-TAXA-RMNC'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST011-DT-INC-TAXA-RMNC
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT


           PERFORM  999500-ADICIONA-VRV-MNT

      *--> Adicionar Area de Dados
           MOVE '007-ST011-TS-ULT-ATL'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST011-TS-ULT-ATL
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT


           MOVE '008-ST011-CD-USU-RSP-ATL'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST011-CD-USU-RSP-ATL
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE '009-ST011-DT-FIM-TAXA-RMNC'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST011-DT-FIM-TAXA-RMNC
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE '010-ST011-CD-TIP-FMA-APL'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST011-CD-TIP-FMA-APL
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE '011-ST011-CD-IEC-RMNC'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST011-CD-IEC-RMNC
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE '012-ST011-CD-PERC-CPTZ-JUR'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST011-CD-PERC-CPTZ-JUR
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

           MOVE  RTN-PGM               TO  SGTST011-RTN-PGM

           IF  EXECUCAO-ONLINE
               PERFORM  999000-TRATAR-ERRO-MSG
           END-IF

           GOBACK
           .
       999999-SAI.
           EXIT.
      *---------------------- FIM SGTST011 ----------------------------