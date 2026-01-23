      *-----------------------------------------------------------------
      * Programa..: SGTS0285
      * Analista..: F8650584 - Roberto Fernandes Lima
      * Autor.....: F8650584 - Roberto Fernandes Lima
      * Data......: 30/08/2018
      * Objetivo..: Listar Pagamentos da Operação Estruturada Vigente
      *             COM Vinculos Parciais e SEM Vinculos
      *             de Recebimento Previsto
      * Compilacao: Cobol/CICS
      *-----------------------------------------------------------------
      * VRS001 30/08/2019 - F8650584 - DE 2016/145195 - Implantacao
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTS0285.

      *-----------------------------------------------------------------
       ENVIRONMENT                     DIVISION.
      *-----------------------------------------------------------------

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
                                       '*** S.S. Começa Aqui ***'.
       77  CTE-PROG                    PIC  X(18)  VALUE
                                       '*** SGTS0285 ***'.
       77  CTE-VERS                    PIC  X(06)  VALUE 'VRS001'.

      *--> Definição dos Nomes dos Programas Acionados
       01  TAB-PROGRAMAS-CHAMADOS.
           03  SGTS0084                PIC  X(08) VALUE 'SGTS0084'.
           03  SGTSLC05                PIC  X(08) VALUE 'SGTSLC05'.

      *--> Definição dos Variaveis do Cobolito
-INC ALMKCBL1

      *--> Constantes Utilizadas


      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

      *--> Area de trabalho
       01  QT-REG-SELD                 PIC S9(04) COMP-5 VALUE ZEROS.
       01  CD-CHV-USU                  PIC  X(08) VALUE SPACES.
       01  IC-TAB                      PIC S9(04) COMP-5 VALUE ZEROS.

       01  CD-CTRA-TIT-VL-PSQ          PIC S9(09) COMP-5 VALUE ZEROS.
       01  CD-OPR-ETRD-MCP-PSQ         PIC S9(11) COMP-3 VALUE ZEROS.
       01  NR-VRS-OPR-ETRD-PSQ         PIC S9(04) COMP-5 VALUE ZEROS.

       01  AUXILIARES.
      *--> Informacoes Basicas da Operacao Estruturada
      *    Titulo e Divisao CETIP, Numero da Carteira TVM,
      *    Carteira, Codigo e Versao da Operacao Estruturada
      *    (Chave do Sistema SGT)
           03  CD-TIT-OPR-ETRD         PIC  X(12) VALUE SPACES.
           03  NR-DVS-TIT              PIC S9(04) VALUE ZEROS.
           03  CD-CTRA-TIT-VL          PIC S9(09) VALUE ZEROS.
               88  CARTEIRA-MAXIMA                VALUE 999999999.
           03  CD-OPR-ETRD-MCP         PIC S9(11) VALUE ZEROS.
           03  NR-VRS-OPR-ETRD         PIC S9(04) VALUE ZEROS.

      *--> Situacao da Operacao Estruturada
           03  CD-TIP-EST-OPR          PIC S9(04) VALUE ZEROS.
               88  EM-CADASTRAMENTO               VALUE 0001.
               88  AUTORIZAR-COMPRA               VALUE 0002.
               88  DISPARAR-COMPRA                VALUE 0003.
               88  A-ENCARTEIRAR                  VALUE 0004.
               88  ENCARTEIRADA                   VALUE 0005.
               88  ENCERRADA                      VALUE 0006.
               88  TRANSFERIDA-CARTEIRA           VALUE 0010.
           03  TX-DCR-EST-OPR          PIC  X(20) VALUE SPACES.

      *--> Situacao da Versão da Operacao Estruturada
           03 CD-TIP-EST-VRS           PIC S9(04) VALUE ZEROS.
               88  EM-EDICAO                      VALUE 0001.
               88  EM-AVALIACAO                   VALUE 0002.
               88  APROVADA                       VALUE 0003.

      *--> Controles de Nulidade
       01  NR-VRS-OPR-ETRD-N           PIC S9(04) COMP-5 VALUE ZEROS.

      *--> Auxiliares Movimentacao Monitoração
       01  CD-OPR-ETRD-TX.
           03  CD-OPR-ETRD             PIC  9(17).

      *-----------------------------------------------------------------
      *    Books Utilizados
      *-----------------------------------------------------------------

      *--> Book dos dados do usuário
-INC IIBKOPRD

      *--> Book do container de Serviços (para acionamento de serviços)
-INC IIBKSRVC

      *--> Book do Container de Erro
-INC IIBKERRO

      *--> Book para captura de abend cics da bloco codigo HLPKW999
-INC HLPKW999

      *--> Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1

      *--> Book para captura de erro via MSG1
-INC MSGKWMG1

      *--> Book do container de requisicao e resposta deste modulo
      *    Consultar Dados Básicos da Operação Estruturada
       01  SGTS0285-DADOS.
-INC SGTK0285

      *-----------------------------------------------------------------
      * Book dos Containers e das subrotinas chamadas
      *-----------------------------------------------------------------

      *--> Consulta da Tabela OPR_ETRD_MCP
       01  SGTSLC05-DADOS.
-INC SGTKLC05

      *--> Consultar Clientes da Operação
      *01  SGTS0084-DADOS.
-INC SGTK0084

      *--> Parâmetros para a utilizacao do DB2
           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

       77  CTE-FINAL-SS               PIC X(40) VALUE
                                      '*** S.S. Termina Aqui ***'.

      *-----------------------------------------------------------------
       PROCEDURE                       DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------
           MOVE  0001                  TO  MSG1-CD-SEQL-ERRO

      *--> Habilita Testes no Cobolito e Debug de Operacoes IIB
-INC ALMKCBL2

      *--> Habilita tratamento automatico de Erros DB2
-INC HLPKE997

      *--> Habilita captura automatica de abend e erros CICS
-INC HLPKE999

           PERFORM  100000-INICIAR

           PERFORM  200000-PROCESSAR

           PERFORM  300000-ENCERRAR

           GOBACK
           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-INICIAR                  SECTION.
      *-----------------------------------------------------------------
           MOVE  0002                  TO  MSG1-CD-SEQL-ERRO

           PERFORM  970000-LER-CANAL

      *--> Define o nome do programa p/ fins manipulacao container
           MOVE  'SGTS0285'            TO  NM-PGM

      *--> Limpa a resposta e erro antes de executar o modulo
           PERFORM  983000-EXCLUIR-RESPOSTA-ERRO

           PERFORM  971000-LER-REQUISICAO

           PERFORM  110000-VALIDAR-REQUISICAO

           EVALUATE  TRUE
               WHEN  SGTS0285-VIGENTES-CARTEIRA
                     PERFORM  120000-VALIDAR-CARTEIRA
               WHEN  OTHER
                     CONTINUE
           END-EVALUATE

           PERFORM  130000-VALIDAR-TITULO

           PERFORM  140000-VALIDAR-OPERACAO

           PERFORM  150000-VALIDAR-RECHAMADA

           PERFORM  160000-LER-DADOS-OPRD

           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-VALIDAR-REQUISICAO         SECTION.
      *--> Valida Campo a Campo da Requisicao
      *-----------------------------------------------------------------
           MOVE  0003                  TO  MSG1-CD-SEQL-ERRO

           IF  NOT SGTS0285-VIGENTES-TODOS     AND
               NOT SGTS0285-VIGENTES-CARTEIRA  AND
               NOT SGTS0285-VIGENTES-TITULO    AND
               NOT SGTS0285-VIGENTES-OPERACAO
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0107              TO  MSG1-CD-ERRO-TS
               MOVE  'Código tipo serviço inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0285-CD-CTRA-TIT-PSQ    NEGATIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0001              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' inválido.'      TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       110000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       120000-VALIDAR-CARTEIRA         SECTION.
      *-----------------------------------------------------------------
           MOVE  0004                  TO  MSG1-CD-SEQL-ERRO

           IF  SGTS0285-CD-CTRA-TIT-PSQ    EQUAL ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0002              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' não informado.' TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *--> Verifica se existe a Carteira TVM Pesquisada
           MOVE  ZEROS                 TO  QT-REG-SELD

           EXEC SQL
                SELECT  DISTINCT 1
                  INTO  :QT-REG-SELD
                  FROM  DB2SGT.CTRA_TIT_VL_MNTR
                 WHERE  CD_CTRA_TIT_VL  =  :SGTS0285-CD-CTRA-TIT-PSQ
           END-EXEC

           IF  QT-REG-SELD                 EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0247              TO  MSG1-CD-ERRO-TS
               MOVE  'Carteira de título e valores mobiliários inexisten
      -              'te.'             TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       120000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       130000-VALIDAR-TITULO           SECTION.
      *-----------------------------------------------------------------
           MOVE  0006                  TO  MSG1-CD-SEQL-ERRO

           IF  SGTS0285-CD-TIT-OPR-PSQ      EQUAL SPACES  AND
               SGTS0285-VIGENTES-TITULO
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0231              TO  MSG1-CD-ERRO-TS
               MOVE  'Código do título da operação estruturada não infor
      -              'mado.'           TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0285-NR-DVS-TIT-PSQ     NEGATIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0013              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da divisão de título inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       130000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       140000-VALIDAR-OPERACAO         SECTION.
      *--> Validar Chave Operação Estruturada Pesquisada
      *-----------------------------------------------------------------
           MOVE  0009                  TO  MSG1-CD-SEQL-ERRO

           IF  SGTS0285-CD-OPR-ETRD-PSQ    NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0003              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada do mercado de capit
      -              'ais não numérico.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0285-CD-OPR-ETRD-PSQ    NEGATIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0004              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada do mercado de capit
      -              'ais inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       150000-VALIDAR-RECHAMADA        SECTION.
      *--> Validar Campos de Rechamada
      *-----------------------------------------------------------------
           MOVE  0010                  TO  MSG1-CD-SEQL-ERRO

           IF  SGTS0285-CD-CTRA-TIT-ULT    NEGATIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0001              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' inválido.'      TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *--> Verifica se existe a Carteira TVM Pesquisada
           MOVE  ZEROS                 TO  QT-REG-SELD

           EVALUATE  TRUE
               WHEN  SGTS0285-CD-CTRA-TIT-ULT
                                           POSITIVE
                     EXEC SQL
                          SELECT  DISTINCT 1
                            INTO  :QT-REG-SELD
                            FROM  DB2SGT.CTRA_TIT_VL_MNTR
                           WHERE  CD_CTRA_TIT_VL  =
                                          :SGTS0285-CD-CTRA-TIT-ULT
                     END-EXEC
                     IF  QT-REG-SELD       EQUAL  ZEROS
                         SET   ERRO-PROGRAMADO
                                       TO  TRUE
                         MOVE  0247    TO  MSG1-CD-ERRO-TS
                         MOVE  'Carteira de título e valores mobiliários
      -                        ' inexistente.'
                                       TO  MSG1-TX-ERRO-TS
                         PERFORM  999000-GRAVA-ERRO-E-ENCERRA
                     END-IF
               WHEN  OTHER
                     CONTINUE
           END-EVALUATE

      *--> Validar Chave do Titulo Rechamada
           IF  SGTS0285-NR-DVS-TIT-ULT     NEGATIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0013              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da divisão de título inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *--> Validar Chave Operacao Estruturada Rechamada
           IF  SGTS0285-CD-OPR-ETRD-ULT    NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0003              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada do mercado de capit
      -              'ais não numérico.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0285-CD-OPR-ETRD-ULT    NEGATIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0004              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada do mercado de capit
      -              'ais inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0285-NR-VRS-OPR-ULT     NEGATIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0006              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação estruturada inválido.
      -              ''                TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       150000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       160000-LER-DADOS-OPRD           SECTION.
      *-----------------------------------------------------------------
           MOVE  0007                  TO  MSG1-CD-SEQL-ERRO

      *--> Configura dados para leitura do container de requisição
           SET   GET-CONTAINER         TO  TRUE
           MOVE  'PRM-INF-OPRD'        TO  NM-CTNR

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

           INITIALIZE                      PRM-INF-OPRD

           EXEC CICS  GET
                      CONTAINER ( NM-CTNR                )
                      INTO      ( PRM-INF-OPRD           )
                      FLENGTH   ( LENGTH OF PRM-INF-OPRD )
           END-EXEC.

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       160000-SAI.
           EXIT.



      *-----------------------------------------------------------------
       200000-PROCESSAR                SECTION.
      *-----------------------------------------------------------------
           MOVE  0008                  TO  MSG1-CD-SEQL-ERRO

           PERFORM  210000-LISTAR-OPR-VIGENTES

           PERFORM  220000-TRATAR-LISTA-OPERACOES
                    VARYING                 SGTS0285-IC-TAB
                    FROM                    0001 BY 0001
                    UNTIL SGTS0285-IC-TAB   GREATER  SGTS0285-QT-OCR-TAB
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       210000-LISTAR-OPR-VIGENTES      SECTION.
      *--> Listar as Operaçoes Estruturadas Vigentes
      *-----------------------------------------------------------------
           MOVE  0009                  TO  MSG1-CD-SEQL-ERRO

           MOVE  SPACES                TO  SGTSLC05-RQSC
           INITIALIZE                      SGTSLC05-RQSC
                                           REPLACING NUMERIC  BY ZEROS

      *--> Movimentar o nome do programa a ser executado
           MOVE  SGTSLC05              TO  NM-PGM

           EVALUATE  TRUE
               WHEN  SGTS0285-VIGENTES-TODOS
                     SET   SGTSLC05-OPR-VIGENTES-TODOS
                                       TO  TRUE
               WHEN  SGTS0285-VIGENTES-CARTEIRA
                     SET   SGTSLC05-OPR-VIGENTES-CTRA
                                       TO  TRUE
               WHEN  SGTS0285-VIGENTES-TITULO
                     SET   SGTSLC05-OPR-VIGENTES-TIT
                                       TO  TRUE
               WHEN  SGTS0285-VIGENTES-OPERACAO
                     SET   SGTSLC05-OPR-VIGENTES-OPR
                                       TO  TRUE
           END-EVALUATE

      *--> Movimentar os Campos de Pesquisa
           MOVE  SGTS0285-CD-CTRA-TIT-PSQ
                                       TO  SGTSLC05-CD-CTRA-TIT-PSQ
           MOVE  SGTS0285-CD-TIT-OPR-PSQ
                                       TO  SGTSLC05-CD-TIT-OPR-PSQ
           MOVE  SGTS0285-NR-DVS-TIT-PSQ
                                       TO  SGTSLC05-NR-DVS-TIT-PSQ
           MOVE  SGTS0285-CD-OPR-ETRD-PSQ
                                       TO  SGTSLC05-CD-OPR-ETRD-PSQ
      *--> Movimentar os Campos de Rechamada
           MOVE  SGTS0285-CD-CTRA-TIT-ULT
                                       TO  SGTSLC05-CD-CTRA-TIT-ULT
           MOVE  SGTS0285-CD-TIT-OPR-ULT
                                       TO  SGTSLC05-CD-TIT-OPR-ULT
           MOVE  SGTS0285-NR-DVS-TIT-ULT
                                       TO  SGTSLC05-NR-DVS-TIT-ULT
           MOVE  SGTS0285-CD-OPR-ETRD-ULT
                                       TO  SGTSLC05-CD-OPR-ETRD-ULT
           MOVE  SGTS0285-NR-VRS-OPR-ULT
                                       TO  SGTSLC05-NR-VRS-OPR-ULT

           PERFORM  900000-EXECUTAR-MODULO

      *--> Movimentar os Campos de Saida
           MOVE  SGTSLC05-QT-OCR-TAB
                                       TO  SGTS0285-QT-OCR-TAB
           MOVE  SGTSLC05-IN-CTNC-PSQ
                                       TO  SGTS0285-IN-CTNC-PSQ

           .
       210000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       220000-TRATAR-LISTA-OPERACOES   SECTION.
      *-----------------------------------------------------------------
           MOVE  0010                  TO  MSG1-CD-SEQL-ERRO

           PERFORM  221000-BUSCAR-NOME-CLIENTES

           PERFORM  222000-CALCULAR-VALOR-EMISAO
           .
       220000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       221000-BUSCAR-NOME-CLIENTES    SECTION.
      *-----------------------------------------------------------------
           MOVE  0011                  TO  MSG1-CD-SEQL-ERRO

           MOVE  SPACES                TO  SGTS0084-DADOS
           INITIALIZE                      SGTS0084-DADOS
                                           REPLACING NUMERIC BY  ZEROS

      *--> Movimentar o nome do programa a ser executado
           MOVE  SGTS0084              TO  NM-PGM

      *--> Movimentar os campos de entrada
           MOVE  SGTSLC05-CD-CTRA-TIT(SGTS0285-IC-TAB)
                                       TO  SGTS0084-CD-CTRA-TIT-VL
           MOVE  SGTSLC05-CD-OPR-ETRD(SGTS0285-IC-TAB)
                                       TO  SGTS0084-CD-OPR-ETRD
           MOVE  SGTSLC05-NR-VRS-OPR-ETRD(SGTS0285-IC-TAB)
                                       TO  SGTS0084-NR-VRS-OPR-ETRD

           PERFORM  900000-EXECUTAR-MODULO

      *--> Dados Cliente Emissor
           MOVE  SGTS0084-CD-CLI-EMT
             TO  SGTS0285-CD-CLI-EMT(SGTS0285-IC-TAB)
           MOVE  SGTS0084-NM-CLI-EMT
             TO  SGTS0285-NM-CLI-EMT(SGTS0285-IC-TAB)
      *--> Dados Cliente de Impacto no Limite de Credito
           MOVE  SGTS0084-CD-CLI-IFLC-LMCR
             TO  SGTS0285-CD-CLI-IFLC-LMCR(SGTS0285-IC-TAB)
           MOVE  SGTS0084-NM-CLI-IFLC-LMCR
             TO  SGTS0285-NM-CLI-IFLC-LMCR(SGTS0285-IC-TAB)
           .
       221000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       222000-CALCULAR-VALOR-EMISAO   SECTION.
      *-----------------------------------------------------------------
           MOVE  0012                  TO  MSG1-CD-SEQL-ERRO

      *----------> Dados Emissao Título
           MOVE  SGTSLC05-QT-PAPL-TIT-EMS(SGTS0285-IC-TAB)
             TO  SGTS0285-QT-PAPL-TIT-EMS(SGTS0285-IC-TAB)

           MOVE  SGTSLC05-VL-PU-EMS-TIT-OPR(SGTS0285-IC-TAB)
             TO  SGTS0285-VL-PU-EMS-TIT-OPR(SGTS0285-IC-TAB)

           MOVE  SGTSLC05-DT-EMS-TIT-OPR(SGTS0285-IC-TAB)
             TO  SGTS0285-DT-EMS-TIT-OPR(SGTS0285-IC-TAB)

           COMPUTE  SGTS0285-VL-EMS-TIT-OPR(SGTS0285-IC-TAB) ROUNDED =
                    SGTSLC05-QT-PAPL-TIT-EMS(SGTS0285-IC-TAB) *
                    SGTSLC05-VL-PU-EMS-TIT-OPR(SGTS0285-IC-TAB)
                    ON SIZE ERROR
                    SET   ERRO-PROGRAMADO
                                       TO  TRUE
                    MOVE  0062         TO  MSG1-CD-ERRO-TS
                    MOVE  'Erro no cálculo do valor de emissão do título
      -                   '.'          TO  MSG1-TX-ERRO-TS
                    PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-COMPUTE

           .
       220000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-ENCERRAR                 SECTION.
      *-----------------------------------------------------------------
           MOVE  0013                  TO  MSG1-CD-SEQL-ERRO

           PERFORM  310000-MONTAR-RESPOSTA
                    VARYING                 SGTS0285-IC-TAB
                    FROM                    0001 BY 0001
                    UNTIL SGTS0285-IC-TAB   GREATER  SGTS0285-QT-OCR-TAB

           PERFORM  980000-GRAVAR-CONTAINER-RPST

           PERFORM  999999-ENCERRAR
           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       310000-MONTAR-RESPOSTA          SECTION.
      *-----------------------------------------------------------------
           MOVE  0014                  TO  MSG1-CD-SEQL-ERRO

      *--> Informações Título e Operação Estruturada
           MOVE  SGTSLC05-CD-CTRA-TIT(SGTS0285-IC-TAB)
             TO  SGTS0285-CD-CTRA-TIT(SGTS0285-IC-TAB)
           MOVE  SGTSLC05-CD-OPR-ETRD(SGTS0285-IC-TAB)
             TO  SGTS0285-CD-OPR-ETRD(SGTS0285-IC-TAB)
           MOVE  SGTSLC05-NR-VRS-OPR-ETRD(SGTS0285-IC-TAB)
             TO  SGTS0285-NR-VRS-OPR-ETRD(SGTS0285-IC-TAB)
           MOVE  SGTSLC05-CD-TIT-OPR-ETRD(SGTS0285-IC-TAB)
             TO  SGTS0285-CD-TIT-OPR-ETRD(SGTS0285-IC-TAB)
           MOVE  SGTSLC05-NR-DVS-TIT(SGTS0285-IC-TAB)
             TO  SGTS0285-NR-DVS-TIT(SGTS0285-IC-TAB)

      *----------> Situação da Operação e da Versão
           MOVE  SGTSLC05-CD-TIP-VRS-OPR(SGTS0285-IC-TAB)
             TO  SGTS0285-CD-TIP-VRS-OPR(SGTS0285-IC-TAB)
           MOVE  SGTSLC05-CD-TIP-EST-OPR(SGTS0285-IC-TAB)
             TO  SGTS0285-CD-TIP-EST-OPR(SGTS0285-IC-TAB)
           MOVE  SGTSLC05-CD-TIP-EST-VRS(SGTS0285-IC-TAB)
             TO  SGTS0285-CD-TIP-EST-VRS(SGTS0285-IC-TAB)

      *----------> Produto e Modalidade
           MOVE  SGTSLC05-CD-PRD(SGTS0285-IC-TAB)
             TO  SGTS0285-CD-PRD(SGTS0285-IC-TAB)
           MOVE  SGTSLC05-CD-MDLD(SGTS0285-IC-TAB)
             TO  SGTS0285-CD-MDLD(SGTS0285-IC-TAB)

      *----------> Dados Compra Título
           MOVE  SGTSLC05-QT-PAPL-TIT-CPR(SGTS0285-IC-TAB)
             TO  SGTS0285-QT-PAPL-TIT-CPR(SGTS0285-IC-TAB)
           MOVE  SGTSLC05-VL-PU-CPR-TIT-OPR(SGTS0285-IC-TAB)
             TO  SGTS0285-VL-PU-CPR-TIT-OPR(SGTS0285-IC-TAB)
           MOVE  SGTSLC05-VL-CPR-OPR(SGTS0285-IC-TAB)
             TO  SGTS0285-VL-CPR-OPR(SGTS0285-IC-TAB)
           MOVE  SGTSLC05-DT-CPR-OPR(SGTS0285-IC-TAB)
             TO  SGTS0285-DT-CPR-OPR(SGTS0285-IC-TAB)
           MOVE  SGTSLC05-CD-UOR-DTTR-OPR(SGTS0285-IC-TAB)
             TO  SGTS0285-CD-UOR-DTTR-OPR(SGTS0285-IC-TAB)

      *----------> Datas de Vencimento e Liquidação
           MOVE  SGTSLC05-DT-VNCT-OPR(SGTS0285-IC-TAB)
             TO  SGTS0285-DT-VNCT-OPR(SGTS0285-IC-TAB)
           MOVE  SGTSLC05-DT-LQDC-OPR-DRIV(SGTS0285-IC-TAB)
             TO  SGTS0285-DT-LQDC-OPR-DRIV(SGTS0285-IC-TAB)

      *----------> Data Inicio Vigência Versão e Dados Evento Vinculado
           MOVE  SGTSLC05-DT-INC-VGC-VRS-OPR(SGTS0285-IC-TAB)
             TO  SGTS0285-DT-INC-VGC-VRS-OPR(SGTS0285-IC-TAB)
           MOVE  SGTSLC05-DT-EVT-VCLD-VRS(SGTS0285-IC-TAB)
             TO  SGTS0285-DT-EVT-VCLD-VRS(SGTS0285-IC-TAB)
           MOVE  SGTSLC05-VL-EVT-VCLD-VRS(SGTS0285-IC-TAB)
             TO  SGTS0285-VL-EVT-VCLD-VRS(SGTS0285-IC-TAB)
           MOVE  SGTSLC05-TX-OBS-EVT(SGTS0285-IC-TAB)
             TO  SGTS0285-TX-OBS-EVT(SGTS0285-IC-TAB)
           MOVE  SGTSLC05-CD-USU-ATL(SGTS0285-IC-TAB)
             TO  SGTS0285-CD-USU-ATL(SGTS0285-IC-TAB)

           .
       310000-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *  Section da Faixa de 900000 à 969900 para Demais Rotinas
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       900000-EXECUTAR-MODULO          SECTION.
      *--> Executa um modulo implementado com CONTAINER
      *    Nao incluir nessa section os modulos chamados via COMMAREA
      *-----------------------------------------------------------------
           MOVE  0015                  TO  MSG1-CD-SEQL-ERRO

      *--> Grava o container de requisicao do modulo a executar
           PERFORM  982000-GRAVAR-CONTAINER-RQSC

      *--> Limpa a resposta e erro antes de executar o modulo
           PERFORM  983000-EXCLUIR-RESPOSTA-ERRO

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EVALUATE  NM-PGM
               WHEN  SGTS0084
                     EXEC CICS  LINK
                                PROGRAM ( SGTS0084   )
                                CHANNEL ( NM-CNL     )
                                RESP    ( MSG1-RESP1 )
                                RESP2   ( MSG1-RESP2 )
                     END-EXEC
               WHEN  SGTSLC05
                     EXEC CICS  LINK
                                PROGRAM ( SGTSLC05   )
                                CHANNEL ( NM-CNL     )
                                RESP    ( MSG1-RESP1 )
                                RESP2   ( MSG1-RESP2 )
                     END-EXEC
               WHEN  OTHER
                     STRING
                         'Não definida a execução do módulo ' NM-PGM
                         DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
                     END-STRING
                     PERFORM  999000-ERRO
           END-EVALUATE

           EVALUATE  TRUE
               WHEN  MSG1-RESP1            EQUAL DFHRESP(NORMAL)
                     CONTINUE
               WHEN  OTHER
                     STRING
                         'Erro na execução do módulo ' NM-PGM ', '
                         'RESP  ='   ' ' MSG1-RESP1  ', '
                         'RESP2 =' ' ' MSG1-RESP2
                         DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
                     END-STRING
                     MOVE  3004        TO  MSG1-CD-ERRO-TS
                     SET   ERRO-CICS   TO  TRUE
                     PERFORM  999000-ERRO
           END-EVALUATE

      *--> Efetua a leitura do Container de Erro
           PERFORM  972000-LER-CONTAINER-DE-ERRO

      *--> Efetua a leitura do Container de Resposta
           PERFORM  973000-LER-RESPOSTA
           .
       900000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       969000-MONITORAR-VARIAVEIS      SECTION.
      *--> Monitora as variáveis dos módulos chamados
      *-----------------------------------------------------------------

           EVALUATE  NM-PGM

      *------> Monitora as Variaveis de Entrada do SGTS0084
               WHEN  SGTS0084
                     MOVE '0084-001-CD-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
      *------------> Verifica se Codigo da Operacao é Numerico
                     IF  SGTS0084-CD-OPR-ETRD          NUMERIC
                         MOVE  SGTS0084-CD-OPR-ETRD
                                       TO  CD-OPR-ETRD
                         MOVE  CD-OPR-ETRD-TX
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE '0084-002-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0084-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

      *------> Monitora as Variaveis de Entrada do SGTSC005
               WHEN  SGTSLC05
                     MOVE 'LC05-001-CD-TIP-SRVC'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSLC05-CD-TIP-SRVC
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT
                     MOVE 'LC05-002-CD-CTRA-TIT-PSQ'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSLC05-CD-CTRA-TIT-PSQ
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT
                     MOVE 'LC05-003-CD-TIT-OPR-PSQ'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSLC05-CD-TIT-OPR-PSQ
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT
                     MOVE 'LC05-004-NR-DVS-TIT-PSQ'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSLC05-NR-DVS-TIT-PSQ
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'LC05-005-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
      *------------> Verifica se Codigo da Operacao e Numerico
                     IF  SGTSLC05-CD-OPR-ETRD-PSQ
                                           NUMERIC
                         MOVE  SGTSLC05-CD-OPR-ETRD-PSQ
                                       TO  CD-OPR-ETRD
                         MOVE  CD-OPR-ETRD-TX
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'LC05-006-CD-CTRA-TIT-ULT'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSLC05-CD-CTRA-TIT-ULT
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'LC05-007-CD-TIT-OPR-ULT'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSLC05-CD-TIT-OPR-ULT
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'LC05-008-NR-DVS-TIT-ULT'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSLC05-NR-DVS-TIT-ULT
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'LC05-009-CD-OPR-ETRD-ULT'
                                       TO  MSGKW999-NM-VRV-MNT
      *------------> Verifica se Codigo da Operacao e Numerico
                     IF  SGTSLC05-CD-OPR-ETRD-ULT    NUMERIC
                         MOVE  SGTSLC05-CD-OPR-ETRD-ULT
                                       TO  CD-OPR-ETRD
                         MOVE  CD-OPR-ETRD-TX
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'LC05-010-NR-VRS-OPR-ULT'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSLC05-NR-VRS-OPR-ULT
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
           MOVE  0017                  TO  MSG1-CD-SEQL-ERRO

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CANAL            TO  TRUE

           EXEC CICS  ASSIGN
                      CHANNEL ( NM-CNL )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       970000-FIM.
           EXIT.

      *-----------------------------------------------------------------
       971000-LER-REQUISICAO           SECTION.
      *-----------------------------------------------------------------
           MOVE  0018                  TO  MSG1-CD-SEQL-ERRO

      *--> Monta o nome do container de requisicao
           MOVE  'SGTS0285'            TO  NM-PGM
           MOVE  'SGTS0285-RQSC'       TO  NM-CTNR

      *--> Recupera o tamanho do container de requisição
           PERFORM  973100-LER-CONTAINER-NODATA

      *--> Obtem o tamanho esperado da requisicao
           MOVE  LENGTH OF SGTS0285-RQSC
                                       TO  QT-CCT-EPRO

      *--> Valida tamanho esperado da requisicao X tamanho recebido
           IF  QT-CCT-LIDO                 NOT EQUAL  QT-CCT-EPRO
               SET   ERRO-TAMANHO-AREA
                                       TO  TRUE
               MOVE  9001              TO  MSG1-CD-ERRO-TS
               MOVE  QT-CCT-EPRO       TO  VL-EXBR-1
               MOVE  QT-CCT-LIDO       TO  VL-EXBR-2
               STRING  'Erro no tamanho de área de requisição '
                        NM-PGM  '.'
                       ', Tamanho Esperado: '  VL-EXBR-1
                       ', Tamanho Recebido: '  VL-EXBR-2
                       DELIMITED BY SIZE     INTO  MSG1-TX-ERRO-TS
               END-STRING
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *--> Configura dados para leitura do container de requisição
           SET   GET-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *--> Efetua a leitura do container
           EXEC CICS  GET
                      CONTAINER ( NM-CTNR       )
                      FLENGTH   ( QT-CCT-LIDO   )
                      INTO      ( SGTS0285-RQSC )
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
           MOVE  0019                  TO  MSG1-CD-SEQL-ERRO

      *--> Le o container de erro dos modulos chamados
           SET   GET-CONTAINER         TO  TRUE
           MOVE  'PRM-ERRO'            TO  NM-CTNR
           INITIALIZE                      IIBKERRO-PRM-ERRO

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS  GET
                      CONTAINER ( NM-CTNR                     )
                      INTO      ( IIBKERRO-PRM-ERRO           )
                      FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                      RESP      ( MSG1-RESP1                  )
                      RESP2     ( MSG1-RESP2                  )
           END-EXEC

      *--> Caso deseje personalizar ou não interromper o
      *    processamento, alterar para não dar erro aqui SOMENTE
      *    para o Modulo Chamado e tratar na section chamadora
           EVALUATE  TRUE
               WHEN  MSG1-RESP1             EQUAL  DFHRESP(CONTAINERERR)
                     CONTINUE
               WHEN  MSG1-RESP1             EQUAL  DFHRESP(NORMAL)
      *--> Trata o erro de programa acionado por ser módulo interno
                SET ERRO-PROGRAMA-ACIONADO   TO  TRUE
                MOVE NM-PGM                  TO  MSG1-NM-PGM-ACND
                IF NM-PGM(1:3) EQUAL 'SGT'
                    MOVE IIBKERRO-CD-ERRO    TO  MSG1-CD-RTN-PGM-ACND
                                                 MSG1-CD-ERRO-TS
                    MOVE IIBKERRO-TX-ERRO    TO  MSG1-TX-RTN-PGM-ACND
                                                 MSG1-TX-ERRO-TS
                ELSE
                    MOVE  3004               TO  MSG1-CD-ERRO-TS
                    STRING  'Erro ao acionar o módulo ' NM-PGM      '.'
                            DELIMITED BY SIZE
                            INTO        MSG1-TX-ERRO-TS
                    END-STRING
                    MOVE IIBKERRO-CD-ERRO     TO  MSG1-CD-RTN-PGM-ACND
                    MOVE IIBKERRO-TX-ERRO     TO  MSG1-TX-RTN-PGM-ACND
                END-IF
      *-->          Monitorar variáveis do módulo chamado
                    PERFORM 969000-MONITORAR-VARIAVEIS
                    PERFORM 999000-ERRO
               WHEN  OTHER
                     SET   ERRO-CONTAINER
                                       TO  TRUE
                     PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       972000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       973000-LER-RESPOSTA             SECTION.
      *--> Recupera o container com os dados de resposta (saída).
      *    Modulo CONTAINER  -     NOME_PGM-RPST
      *    Modulo IIB (FIXO) -     PRM-DADOS-RPST
      *-----------------------------------------------------------------
           MOVE  0020                  TO  MSG1-CD-SEQL-ERRO

      *--> Monta o nome dos container de acordo com modulo a executar
           MOVE  SPACES                TO  NM-CTNR

      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RPST,
      *    senão nome_modulo -RPST
           EVALUATE  NM-PGM
               WHEN  SGTS0084
                     MOVE  'SGTS0084-RPST'
                                       TO  NM-CTNR
               WHEN  SGTSLC05
                     MOVE  'SGTSLC05-RPST'
                                       TO  NM-CTNR
               WHEN  OTHER
                     MOVE  NM-PGM
                                       TO  NM-CTNR(1:8)
                     MOVE  '-RPST'
                                       TO  NM-CTNR(9:5)
           END-EVALUATE

      *--> Recupera o tamanho do container de resposta
           PERFORM  973100-LER-CONTAINER-NODATA

           MOVE  0021                  TO  MSG1-CD-SEQL-ERRO

      *--> Configura dados para leitura do container de resposta
           SET   GET-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE


      *--> Le a RESPOSTA de acordo com o programa que a gravou
           EVALUATE  NM-PGM
               WHEN  SGTS0084
                     EXEC CICS  GET
                                CONTAINER ( NM-CTNR       )
                                INTO      ( SGTS0084-RPST )
                                FLENGTH   ( QT-CCT-LIDO   )
                     END-EXEC
               WHEN  SGTSLC05
                     EXEC CICS  GET
                                CONTAINER ( NM-CTNR       )
                                INTO      ( SGTSLC05-RPST )
                                FLENGTH   ( QT-CCT-LIDO   )
                     END-EXEC
               WHEN  OTHER
                     SET   ERRO-PROGRAMADO
                                       TO  TRUE
                     MOVE  9003        TO  MSG1-CD-ERRO-TS
                     STRING  'Erro na leitura da resposta do módulo '
                             NM-PGM '.'
                             DELIMITED BY SIZE
                             INTO          MSG1-TX-ERRO-TS
                     END-STRING
                     PERFORM  999000-ERRO
           END-EVALUATE

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       973000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       973100-LER-CONTAINER-NODATA     SECTION.
      *-----------------------------------------------------------------
           MOVE  0021                  TO  MSG1-CD-SEQL-ERRO

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

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       973100-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *   Section da Faixa de 980000 a 989900 para Gravacao de Dados
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       980000-GRAVAR-CONTAINER-RPST    SECTION.
      *-----------------------------------------------------------------
           MOVE  0022                  TO  MSG1-CD-SEQL-ERRO

      *--> Exclui eventual container de erro de outros programas
           PERFORM  983100-EXCLUIR-CONTAINER-ERRO

      *--> Monta o nome do container de resposta
           MOVE  'SGTS0285-RPST'       TO  NM-CTNR
           SET   PUT-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

           EXEC CICS  PUT
                      CONTAINER ( NM-CTNR                 )
                      FROM      ( SGTS0285-RPST           )
                      FLENGTH   ( LENGTH OF SGTS0285-RPST )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       980000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       982000-GRAVAR-CONTAINER-RQSC    SECTION.
      *--> Postar container com dados de requisição (entrada)
      *    Modulo CONTAINER  -     NOME_PGM-RQSC
      *    Modulo IIB (FIXO) -     PRM-DADOS-RQSC
      *-----------------------------------------------------------------
           MOVE  0024                  TO  MSG1-CD-SEQL-ERRO

      *--> Monta o nome dos container de acordo com modulo a executar
           MOVE  SPACES                TO  NM-CTNR
           SET   PUT-CONTAINER         TO  TRUE

      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RQSC,
      *    senão nome_modulo -RQSC
           EVALUATE  NM-PGM
               WHEN  SGTS0084
                     MOVE  'SGTS0084-RQSC'
                                       TO  NM-CTNR
               WHEN  SGTSLC05
                     MOVE  'SGTSLC05-RQSC'
                                       TO  NM-CTNR
               WHEN  OTHER
                     MOVE  NM-PGM      TO  NM-CTNR(1:8)
                     MOVE  '-RQSC'     TO  NM-CTNR(9:5)
           END-EVALUATE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *--> Faz PUT da requisicao do programa a executar
           EVALUATE  NM-PGM
               WHEN  SGTS0084
                     EXEC CICS  PUT
                                CONTAINER ( NM-CTNR                  )
                                FROM      ( SGTS0084-RQSC            )
                                FLENGTH   ( LENGTH OF SGTS0084-RQSC  )
                     END-EXEC
               WHEN  SGTSLC05
                     EXEC CICS  PUT
                                CONTAINER ( NM-CTNR                  )
                                FROM      ( SGTSLC05-RQSC            )
                                FLENGTH   ( LENGTH OF SGTSLC05-RQSC  )
                     END-EXEC
               WHEN  OTHER
                     SET   ERRO-PROGRAMADO
                                       TO  TRUE
                     MOVE  9004        TO  MSG1-CD-ERRO-TS
                     STRING  'Erro na gravação da requisição do módulo.'
                             NM-PGM '.'
                             DELIMITED BY SIZE
                             INTO          MSG1-TX-ERRO-TS
                     END-STRING
                     PERFORM  999000-ERRO
           END-EVALUATE

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       982000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       983000-EXCLUIR-RESPOSTA-ERRO    SECTION.
      *--> Exclui o container de resposta e de erro para evitar
      *    problemas em modulos executados mais de uma vez
      *    dentro da mesma sessao do CICS.
      *    Equivale ao INITIALIZE na area de resposta dos programas
      *    que utilizam COMMAREA.
      *-----------------------------------------------------------------
           MOVE  0025                  TO  MSG1-CD-SEQL-ERRO

      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RPST,
      *    senão nome_modulo -RPST
           EVALUATE  NM-PGM
               WHEN  SGTS0084
                     MOVE  'SGTS0084-RPST'
                                       TO  NM-CTNR
               WHEN  SGTSLC05
                     MOVE  'SGTSLC05-RPST'
                                       TO  NM-CTNR
               WHEN  OTHER
                     MOVE  NM-PGM      TO  NM-CTNR(1:8)
                     MOVE  '-RPST'     TO  NM-CTNR(9:5)
           END-EVALUATE

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
           MOVE  0026                  TO  MSG1-CD-SEQL-ERRO

      *--> Limpar variaveis container de erro
           INITIALIZE                      IIBKERRO-PRM-ERRO

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
      *-----------------------------------------------------------------

      *--> Grava o erro gerado no container de erro
      *    Não há captura automática de erro CICS por usar RESP e RESP2
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
      * As sections abaixo iniciadas com 999000 representam os tratamen
      * tos de erro utilizando o Sistema MSG (rotina MSGS0001)
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       999000-ERRO                     SECTION.
      *-----------------------------------------------------------------

           IF  MSG1-CD-ERRO-TS             EQUAL  ZEROS
               MOVE  8888              TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS             EQUAL  SPACES
               MOVE  'Erro não especificado no módulo.'
                                       TO  MSG1-TX-ERRO-TS
           END-IF

           PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           .
       999000-ERRO-SAI.
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

      *--> Adiciona as Variaveis de Entrada para Monitoracao
           MOVE  '0285-001-CD-TIP-SRVC'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0285-CD-TIP-SRVC  TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  '0285-002-CD-CTRA-TIT-PSQ'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0285-CD-CTRA-TIT-PSQ
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  '0285-003-CD-TIT-OPR-PSQ'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0285-CD-TIT-OPR-PSQ
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  '0285-004-NR-DVS-TIT-PSQ'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0285-NR-DVS-TIT-PSQ
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  '0285-005-CD-OPR-ETRD-PSQ'
                                       TO  MSGKW999-NM-VRV-MNT
      *--> Verifica se o conteudo e numerico antes de adicionar
           IF  SGTS0285-CD-OPR-ETRD-PSQ    NUMERIC
               MOVE  SGTS0285-CD-OPR-ETRD-PSQ
                                       TO  CD-OPR-ETRD
               MOVE  CD-OPR-ETRD-TX
                                       TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  '0285-006-CD-CTRA-TIT-ULT'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0285-CD-CTRA-TIT-ULT
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  '0285-007-CD-TIT-OPR-ULT'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0285-CD-TIT-OPR-ULT
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  '0285-008-NR-DVS-TIT-ULT'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0285-NR-DVS-TIT-ULT
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  '0285-009-CD-OPR-ETRD-ULT'
                                       TO  MSGKW999-NM-VRV-MNT
      *--> Verifica se o conteudo e numerico antes de adicionar
           IF  SGTS0285-CD-OPR-ETRD-ULT    NUMERIC
               MOVE  SGTS0285-CD-OPR-ETRD-ULT
                                       TO  CD-OPR-ETRD
               MOVE  CD-OPR-ETRD-TX
                                       TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  '0285-010-NR-VRS-OPR-ULT'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0285-NR-VRS-OPR-ULT
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

      *--> Adiciona as Variaveis de Saida para Monitoracao
           MOVE  '0285-011-QT-OCR-TAB'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0285-QT-OCR-TAB
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  '0285-012-IC-TAB'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0285-IC-TAB
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

      *--> Grava o erro gerado no sistema MSG - Mensageria
           PERFORM  MSG1-NTF-MSG

           MOVE  MSG1-CD-ERRO-TS       TO  IIBKERRO-CD-ERRO
           MOVE  MSG1-TX-ERRO-TS       TO  IIBKERRO-TX-ERRO

      *--> Grava o erro gerado no container de erro
           PERFORM  984000-GRAVAR-CONTAINER-ERRO

      *--> Encerra a transacao
           PERFORM  999999-ENCERRAR
           .
       999000-SAI.
           EXIT.

      *----------------------------------------
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

      *---------------------- FIM SGTS0285 -----------------------------