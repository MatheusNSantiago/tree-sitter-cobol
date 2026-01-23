       CBL ARITH(EXTEND)
      *-----------------------------------------------------------------
      * Programa..: SGTS0083
      * Analista..: F2054109 - Cintia A.Iglesias
      * Autores...: F9188847 - Soila K. L. Akamatsu
      *             F8650584 - Roberto Fernandes Lima
      * Data......: 22/08/2016
      * Objetivo..: Consultar Dados Básicos da Operação Estruturada de
      *             Mercado de Capitais
      * Compilacao: Cobol/CICS
      *-----------------------------------------------------------------
      * VRS004 21/06/2024 - F9188847 - Inclusão dos campos:
      *                     CD-ESG-NEGL, NM-ESG-NEGL, NM-CLSC-ITMT-FNCO,
      *                     NM-TIP-VRS-OPR, CD-MTV-EST-NEGL
      *                     NM-MTV-EST-NEGL e NR-UNCO-CTR-OPR
      * VRS003 18/06/2024 - F9158937 - Recompilação por alteração no
      *                     book SGTK0300.
      * VRS002 13/11/2023 - F9507141 - Histórias 1094276 e 1094239.
      *                     Inclusão dos campos: TX-CMCP, TX-GR-MCP,
      *                     TX-LVRO-MC-CPTL, TX-SMCP, CD-TIP-VRS-OPR.
      * VRS002 26/10/2023 - F9507141 - Histórias 1094276 e 1094239.
      *                     Inclusão dos campos: CD-CMCP, CD-GR-MCP,
      *                     CD-LVRO-MC-CPTL, CD-SMCP e CD-CLSC-ITMT-FNCO
      * VRS001 21/01/2019 - F8785286 - DE 2016/145195 - Inclusão
      *                     monitoração variáveis para rotina SBDATA
      *        08/01/2018 - F8785286 - DE 2016/145195
      *                     Inclusão do campo SGTS0083-CD-TIP-EST-VRS
      *                     na área de saída e chamar o SGTSPE04 somente
      *                     se a oper possuir Título e Div.
      *        13/02/2017 - F8785286 - DE 2016/145195
      *                     Inclusão campo CD-USU-RSP-ATL
      *                     Movimentar a partir do container operador
      *        22/08/2016 - F9188847 - DE 2015/137233 - Implantacao
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTS0083.

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
                                       '*** SGTS0083 ***'.
       77  CTE-VERS                    PIC  X(06)  VALUE 'VRS004'.

      *--> Definição dos Nomes dos Programas Acionados
       01  TAB-PROGRAMAS-CHAMADOS.
           03  IIBP2000                PIC  X(08) VALUE 'IIBP2000'.
           03  ATCS6000                PIC  X(08) VALUE 'ATCS6000'.
           03  CACS153C                PIC  X(08) VALUE 'CACS153C'.
           03  CAFS0053                PIC  X(08) VALUE 'CAFS0053'.
           03  CAFS0007                PIC  X(08) VALUE 'CAFS0007'.
           03  ICLSI046                PIC  X(08) VALUE 'ICLSI046'.
           03  MSTS8042                PIC  X(08) VALUE 'MSTS8042'.
           03  SBDATAF                 PIC  X(08) VALUE 'SBDATAF'.
           03  SGTS0085                PIC  X(08) VALUE 'SGTS0085'.
           03  SGTS0084                PIC  X(08) VALUE 'SGTS0084'.
           03  SGTSC069                PIC  X(08) VALUE 'SGTSC069'.
           03  SGTSL083                PIC  X(08) VALUE 'SGTSL083'.
           03  SGTSL094                PIC  X(08) VALUE 'SGTSL094'.
           03  SGTSL506                PIC  X(08) VALUE 'SGTSL506'.
           03  SGTSPE04                PIC  X(08) VALUE 'SGTSPE04'.
           03  SGTST002                PIC  X(08) VALUE 'SGTST002'.
           03  SGTSX005                PIC  X(08) VALUE 'SGTSX005'.
           03  SGTS0300                PIC  X(08) VALUE 'SGTS0300'.
           03  SGTST009                PIC  X(08) VALUE 'SGTST009'.
           03  SGTST027                PIC  X(08) VALUE 'SGTST027'.
           03  SGTST029                PIC  X(08) VALUE 'SGTST029'.
           03  SGTST030                PIC  X(08) VALUE 'SGTST030'.

      *--> Definição dos Variaveis do Cobolito
       COPY ALMKCBL1.


      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

      *--> Areas de trabalho
       01  IN-FLZ-MNL-EXCO             PIC  X(01) VALUE SPACES.
           88  FLEXIBILIZACAO-ATIVA               VALUE 'S'.
       01  IN-IFLC-MNL-EXCO            PIC  X(01) VALUE SPACES.
           88  IMPACTO-MANUAL-VIGENTE             VALUE 'S'.
       01  IN-EXCO-ATV                 PIC  X(01) VALUE SPACES.
           88  EXCECAO-ATIVA                      VALUE 'S'.

      *--> Auxiliares Movimentacao Monitoração
       01  CD-OPR-ETRD-TX.
           03  CD-OPR-ETRD             PIC  9(11).

      *--> SBDATAF - Realiza Calculos Diversos com Datas
       01  FUNCAO                      PIC  X(03) VALUE 'F03'.
       01  AREA-TRABALHO               PIC  X(4095).
       01  AREA-TRABALHO-R             REDEFINES AREA-TRABALHO.
           03  AREA-TAMANHO            PIC  9(08) COMP.
           03  FILLER                  PIC  X(4071).
           03  AREA-FIM                PIC  X(20).
       01  ARG01                       PIC  9(08).
       01  ARG01-R                     REDEFINES  ARG01.
           03  DIA                     PIC  9(02).
           03  MES                     PIC  9(02).
           03  ANO                     PIC  9(04).
       01  ARG02                       PIC  9(08).
       01  ARG02-R                     REDEFINES  ARG02.
           03  DIA                     PIC  9(02).
           03  MES                     PIC  9(02).
           03  ANO                     PIC  9(04).
       01  ARG03                       PIC S9(05) VALUE ZEROS.

      *--> Parametros do CAFS0007
      *    Busca Dados cadastrais de um agencia externa
       01  CAFS0007-DADOS.
           03  PARM-DADOS.
               05  PARM-BANCO-INF      PIC  9(003) VALUE ZEROS.
               05  PARM-AGE-INF        PIC  9(004) VALUE ZEROS.
               05  PARM-MUNICIPIO      PIC  9(007) VALUE ZEROS.
               05  FILLER              REDEFINES  PARM-MUNICIPIO.
                   07  PARM-CD-MUN     PIC  9(006).
                   07  PARM-DV-MUN     PIC  9(001).
               05  PARM-RET-CODE       PIC  9(002) VALUE ZEROS.
               05  PARM-SQLCODE        PIC S9(005) VALUE ZEROS.
               05  PARM-SECTION        PIC  X(030) VALUE SPACES.
               05  PARM-SQLCA          PIC  X(146) VALUE SPACES.
               05  PARM-NM-MUNICIPIO   PIC  X(038) VALUE SPACES.
               05  PARM-UF-MUNICIPIO   PIC  X(002) VALUE SPACES.
               05  PARM-NOME-AGEN      PIC  X(030) VALUE SPACES.
               05  PARM-NOME-BANCO     PIC  X(050) VALUE SPACES.

      *--> Indice para filtrar a lista retornada pelo SGTS0300
       01  IC-0300                     PIC S9(04) COMP-5 VALUE ZEROS.

      *-----------------------------------------------------------------
      *    Books Utilizados
      *-----------------------------------------------------------------

      *--> Book dos dados do usuário
       COPY IIBKOPRD.

      *--> Book do container de Serviços (para acionamento de serviços)
       COPY IIBKSRVC.

      *--> Book do Container de Erro
       COPY IIBKERRO.

      *--> Book para captura de abend cics da bloco codigo HLPKW999
      *COPY HLPKW999.

      *--> Book das variáveis auxiliares do tratamento de erro via MSG
       COPY SGTKERR1.

      *--> Book para captura de erro via MSG1
       COPY MSGKWMG1.

      *--> Book do container de requisicao e resposta deste modulo
      *    Consultar Dados Básicos da Operação Estruturada
       COPY SGTK0083.

      *-----------------------------------------------------------------
      * Book dos Containers e das subrotinas chamadas
      *-----------------------------------------------------------------

      *--> Recuperar a Ultima Versão Aprovada da Operação Estruturada
       01  SGTSL506-DADOS.
       COPY SGTKL506.

      *--> Consultar Tabela DB2SGT.OPR_ETRD_MCP
       01  SGTSX005-DADOS.
       COPY SGTKX005.

      *--> Consultar Descrição da Taxonomia, no Sistema RSM
      *01  SGTS0300-DADOS.
       COPY SGTK0300.

      *--> Consultar Estado da Operação Estruturada
       01  SGTST009-DADOS.
       COPY SGTKT009.

      *--> Consultar Estado da Versão da Operação Estruturada
      *    na tabela DB2SGT.EST_VRS_OPR_ETRD
       01  SGTSC069-DADOS.
       COPY SGTKC069.

      *--> CRUD da tabela TIT_OPR_ETRD_MCP
       01 SGTST002-DADOS.
       COPY SGTKT002.

      *--> Consultar Indicadores da Operação
       COPY SGTK0085.

      *--> Consulta Tabela DB2SGT.IFLC_MNL_LIM_EXCO
      *01 SGTSL094-DADOS.
       COPY SGTKL094.

      *--> Consulta Tabela DB2SGT.EXCO_PRFL_SELD
      *01 SGTSL083-DADOS
       COPY SGTKL083.

      *--> Consultar Clientes da Operação
       COPY SGTK0084.

      *--> Consultar Posição Ultima Posição Diária do Saldo
       01  SGTSPE04-DADOS.
       COPY SGTKPE04.

      *--> Recuperar Nomes de Bancos - via IIB
       01  CAFS0053-DADOS.
       COPY CAFK0053.

      *--> CRUD TIP_COTA_TIT_OPR
       01  SGTST027-DADOS.
       COPY SGTKT027.

      *--> CRUD CTGR_BC_TVM
       01  SGTST029-DADOS.
       COPY SGTKT029.

      *--> CRUD ITC_CVM_OPR_ETRD
       01  SGTST030-DADOS.
       COPY SGTKT030.

      *--> Consultar dados de usuário - via IIB
       01  ATCS6000-DADOS.
       COPY ATCK6000.

      *--> Listar Clearing Ativa para Conciliacao - via IIB
       COPY ICLKI046.

      *--> Consultar dados da UOR - via IIB
       COPY MSTK8042.

      *--> Consultar nome da espécie debênture - via IIB
       01  CACS153C-DADOS.
       COPY CACK153C.

       01  CTE-FINAL-SS                PIC X(40) VALUE
                                       '*** S.S. Termina Aqui ***'.

      *-----------------------------------------------------------------
       PROCEDURE                       DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------
           MOVE  0001                  TO  MSG1-CD-SEQL-ERRO

      *--> Habilita Testes no Cobolito e Debug de Operacoes IIB
       COPY ALMKCBL2.

      *--> Ativa a captura automatica de abend do CICS
      *    WHENEVER CICS
       COPY HLPKE999.

           PERFORM  100000-INICIAR

           PERFORM  200000-PROCESSAR

           PERFORM  300000-ENCERRAR
           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-INICIAR                  SECTION.
      *-----------------------------------------------------------------
           MOVE  0002                  TO  MSG1-CD-SEQL-ERRO

           PERFORM  970000-LER-CANAL

      *--> Define o nome do programa p/ fins manipulacao container
           MOVE  'SGTS0083'            TO  NM-PGM

      *--> Limpa a resposta e erro antes de executar o modulo
           PERFORM  983000-EXCLUIR-RESPOSTA-ERRO

           PERFORM  971000-LER-REQUISICAO

           PERFORM  110000-VALIDAR-REQUISICAO

           PERFORM  120000-LER-DADOS-OPRD
           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-VALIDAR-REQUISICAO       SECTION.
      *--> Valida campo a campo da requisicao
      *-----------------------------------------------------------------
           MOVE  0003                  TO  MSG1-CD-SEQL-ERRO

      *--> Validar Chave da Operação Estruturada
           IF  SGTS0083-CD-CTRA-TIT-VL     NEGATIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0001              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' inválido.'      TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0083-CD-CTRA-TIT-VL     EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0002              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0083-CD-OPR-ETRD        NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0003              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada mercado de capitais nã
      -              'o numérico.'     TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0083-CD-OPR-ETRD        NEGATIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0004              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada mercado de capitais in
      -              'válido.'         TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0083-NR-VRS-OPR-ETRD    NEGATIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0006              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação estruturada inválido.
      -              ''                TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       110000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       120000-LER-DADOS-OPRD           SECTION.
      *-----------------------------------------------------------------
           MOVE  0004                  TO  MSG1-CD-SEQL-ERRO

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
       120000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-PROCESSAR                SECTION.
      *-----------------------------------------------------------------
           MOVE  0005                  TO  MSG1-CD-SEQL-ERRO

           MOVE  SPACES                TO  SGTS0083-RPST
           INITIALIZE                      SGTS0083-RPST
                                           REPLACING NUMERIC  BY ZEROS

           IF  SGTS0083-NR-VRS-OPR-ETRD    EQUAL  ZEROS
               PERFORM  210000-OBTER-ULT-VRS-ATI-OPR
           ELSE
               CONTINUE
           END-IF

           PERFORM  220000-CONSULTAR-OPERACOES

           PERFORM  230000-OBTER-DADOS-ADICIONAIS

           PERFORM  240000-CALCULAR

           PERFORM  250000-OBTER-AGENCIA-E-CONTA

           PERFORM  260000-OBTER-TEXTOS-INTERNOS

           PERFORM  270000-OBTER-TEXTOS-EXTERNOS

           IF  SGTSX005-CD-PRD  > 0  AND  SGTSX005-CD-MDLD   > 0  AND
               SGTSX005-CD-CMCP > 0  AND  SGTSX005-CD-GR-MCP > 0
               PERFORM  280000-OBTER-NOMES-TAXONOMIA
           END-IF

           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       210000-OBTER-ULT-VRS-ATI-OPR    SECTION.
      *-----------------------------------------------------------------
           MOVE  0006                  TO  MSG1-CD-SEQL-ERRO

           MOVE  SPACES                TO  SGTSL506-DADOS
           INITIALIZE                      SGTSL506-DADOS
                                           REPLACING NUMERIC  BY ZEROS

      *--> Movimentar o nome do programa a ser executado
           MOVE  SGTSL506              TO  NM-PGM

      *--> Movimentar os campos de entrada
           MOVE  SGTS0083-CD-CTRA-TIT-VL
                                       TO  SGTSL506-CD-CTRA-TIT-VL
           MOVE  SGTS0083-CD-OPR-ETRD  TO  SGTSL506-CD-OPR-ETRD-MCP

           PERFORM  900000-EXECUTAR-MODULO

           IF  SGTSL506-NR-VRS-OPR-ETRD    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0219              TO  MSG1-CD-ERRO-TS
               MOVE  'Não foi encontrada versão ativa e aprovada da oper
      -              'ação estruturada.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           MOVE SGTSL506-NR-VRS-OPR-ETRD
                                       TO  SGTS0083-NR-VRS-OPR-ETRD
           .
       210000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       220000-CONSULTAR-OPERACOES      SECTION.
      *-----------------------------------------------------------------
           MOVE  0007                  TO  MSG1-CD-SEQL-ERRO

           MOVE  SPACES                TO  SGTSX005-DADOS
           INITIALIZE                      SGTSX005-DADOS
                                           REPLACING NUMERIC  BY ZEROS

      *--> Movimento nome do módulo para monitoração de variáveis
           MOVE  SGTSX005              TO  NM-PGM

      *--> Movimentar os campos de entrada
           SET   SGTSX005-CONSULTAR    TO  TRUE
           MOVE  SGTS0083-CD-CTRA-TIT-VL
                                       TO  SGTSX005-CD-CTRA-TIT-VL
           MOVE  SGTS0083-CD-OPR-ETRD  TO  SGTSX005-CD-OPR-ETRD-MCP
           MOVE  SGTS0083-NR-VRS-OPR-ETRD
                                       TO  SGTSX005-NR-VRS-OPR-ETRD

           EXEC CICS  LINK
                      PROGRAM  ( SGTSX005       )
                      COMMAREA ( SGTSX005-DADOS )
           END-EXEC

           IF  SGTSX005-CD-RTN-PGM         NOT EQUAL  ZEROS
               SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE  3004              TO  MSG1-CD-ERRO-TS
               STRING  'Erro ao acionar o módulo ' NM-PGM '.'
                       DELIMITED BY SIZE
                       INTO                MSG1-TX-ERRO-TS
               END-STRING
               MOVE  NM-PGM            TO  MSG1-NM-PGM-ACND
               MOVE  SGTSX005-MSG-ERRO-PGM
                                       TO  MSG1-TX-RTN-PGM-ACND
               MOVE  SGTSX005-CD-RTN-PGM
                                       TO  MSG1-CD-RTN-PGM-ACND
               MOVE  SGTSX005-PSC-ERRO-PGM
                                       TO  MSG1-CD-SEQL-PGM-ACND
               PERFORM  969000-MONITORAR-VARIAVEIS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       220000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       230000-OBTER-DADOS-ADICIONAIS   SECTION.
      *-----------------------------------------------------------------
           MOVE  0008                  TO  MSG1-CD-SEQL-ERRO

           PERFORM  231000-OBTER-ESTADO-OPERACAO

           PERFORM  232000-OBTER-ESTADO-VERSAO

      *--> Obter Código de Identificação Internacional ISIN do Título
           IF  SGTSX005-CD-TIT-OPR-ETRD    NOT EQUAL  SPACES
               PERFORM  233000-OBTER-CD-ISIN
           END-IF

           PERFORM  234000-OBTER-INDICADORES

           IF  SGTSX005-CD-TIT-OPR-ETRD    NOT EQUAL  SPACES  AND
               SGTSX005-NR-DVS-TIT         POSITIVE
               PERFORM  235000-CONSULTAR-IMPACTO
           END-IF

           PERFORM  236000-CONSULTAR-EXCECAO

           PERFORM  237000-OBTER-CLIENTES-OPERACAO

      *--> Se estado da operação igual a encarteirado ou encerrado
      *    consultar ultima posição de saldo recebido da drive
           IF  SGTST009-CD-TIP-EST-OPR     EQUAL 0005  OR  0006
               IF  SGTSX005-CD-TIT-OPR-ETRD
                                           NOT EQUAL  SPACES  AND
                   SGTSX005-NR-DVS-TIT     POSITIVE
                   PERFORM  238000-OBTER-ULT-SDO-OPR
               END-IF
           END-IF

           .
       230000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       231000-OBTER-ESTADO-OPERACAO    SECTION.
      *-----------------------------------------------------------------
           MOVE  0009                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      SGTST009-DADOS

      *--> Movimento nome do módulo para monitoração de variáveis
           MOVE  SGTST009              TO  NM-PGM

      *--> Movimentar os campos de entrada
           SET   SGTST009-CONSULTAR-EST-ATIVO
                                       TO  TRUE
           MOVE  SGTS0083-CD-CTRA-TIT-VL
                                       TO  SGTST009-CD-CTRA-TIT-VL
           MOVE  SGTS0083-CD-OPR-ETRD  TO  SGTST009-CD-OPR-ETRD-MCP
           MOVE  SGTS0083-NR-VRS-OPR-ETRD
                                       TO  SGTST009-NR-VRS-OPR-ETRD
           MOVE  KOPRD-CD-OPRD         TO  SGTST009-CD-USU-RSP-ATL


           EXEC CICS  LINK
                      PROGRAM  ( SGTST009                 )
                      COMMAREA ( SGTST009-DADOS           )
                      LENGTH   ( LENGTH OF SGTST009-DADOS )
                      RESP     ( MSG1-RESP1               )
                      RESP2    ( MSG1-RESP2               )
           END-EXEC

           IF  SGTST009-CD-RTN-PGM         NOT EQUAL  ZEROS  AND  +100
               SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE  3004              TO  MSG1-CD-ERRO-TS
               STRING  'Erro ao acionar o módulo ' NM-PGM '.'
                       DELIMITED BY SIZE
                       INTO                MSG1-TX-ERRO-TS
               END-STRING
               MOVE  NM-PGM            TO  MSG1-NM-PGM-ACND
               MOVE  SGTST009-MSG-ERRO-PGM
                                       TO  MSG1-TX-RTN-PGM-ACND
               MOVE  SGTST009-CD-RTN-PGM
                                       TO  MSG1-CD-RTN-PGM-ACND
               MOVE  SGTST009-PSC-ERRO-PGM
                                       TO  MSG1-CD-SEQL-PGM-ACND
               PERFORM  969000-MONITORAR-VARIAVEIS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       231000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       232000-OBTER-ESTADO-VERSAO      SECTION.
      *-----------------------------------------------------------------
           MOVE  0010                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      SGTSC069-DADOS

      *--> Movimentar o nome do programa a ser executado
           MOVE  SGTSC069              TO  NM-PGM

      *--> Movimentar os campos de entrada
           MOVE  SGTS0083-CD-CTRA-TIT-VL
                                       TO  SGTSC069-CD-CTRA-TIT-VL
           MOVE  SGTS0083-CD-OPR-ETRD  TO  SGTSC069-CD-OPR-ETRD-MCP
           MOVE  SGTS0083-CD-OPR-ETRD  TO  SGTSC069-CD-OPR-ETRD-MCP
           MOVE  SGTS0083-NR-VRS-OPR-ETRD
                                       TO  SGTSC069-NR-VRS-OPR-ETRD
           MOVE  SPACES                TO  SGTSC069-TS-EST-VRS

           PERFORM 900000-EXECUTAR-MODULO

           .
       232000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       233000-OBTER-CD-ISIN            SECTION.
      *-----------------------------------------------------------------
           MOVE  0011                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      SGTST002-DADOS

      *--> Movimento nome do módulo para monitoração de variáveis
           MOVE  SGTST002              TO  NM-PGM

      *--> Movimentar os campos de entrada
           SET   SGTST002-CONSULTAR    TO  TRUE
           MOVE  SGTSX005-CD-TIT-OPR-ETRD
                                       TO  SGTST002-CD-TIT-OPR-ETRD
           MOVE  SGTSX005-NR-DVS-TIT   TO  SGTST002-NR-DVS-TIT

           EXEC CICS  LINK
                      PROGRAM  ( SGTST002       )
                      COMMAREA ( SGTST002-DADOS )
           END-EXEC

           IF  SGTST002-CD-RTN-PGM         NOT EQUAL  ZEROS
               SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE  3004              TO  MSG1-CD-ERRO-TS
               STRING  'Erro ao acionar o módulo ' NM-PGM '.'
                       DELIMITED BY SIZE
                       INTO                MSG1-TX-ERRO-TS
               END-STRING
               MOVE  NM-PGM            TO  MSG1-NM-PGM-ACND
               MOVE  SGTST002-MSG-ERRO-PGM
                                       TO  MSG1-TX-RTN-PGM-ACND
               MOVE  SGTST002-CD-RTN-PGM
                                       TO  MSG1-CD-RTN-PGM-ACND
               MOVE  SGTST002-PSC-ERRO-PGM
                                       TO  MSG1-CD-SEQL-PGM-ACND
               PERFORM  969000-MONITORAR-VARIAVEIS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       233000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       234000-OBTER-INDICADORES        SECTION.
      *-----------------------------------------------------------------
           MOVE  0012                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      SGTS0085-DADOS

      *--> Movimentar o nome do programa a ser executado
           MOVE  SGTS0085              TO  NM-PGM

      *--> Movimentar os campos de entrada
           MOVE  SGTS0083-CD-CTRA-TIT-VL TO
                                           SGTS0085-CD-CTRA-TIT-VL
           MOVE  SGTS0083-CD-OPR-ETRD  TO  SGTS0085-CD-OPR-ETRD-MCP
           MOVE  SGTS0083-NR-VRS-OPR-ETRD
                                       TO  SGTS0085-NR-VRS-OPR-ETRD
           PERFORM  900000-EXECUTAR-MODULO

           MOVE SGTS0085-IN-FLZ-EXCO-OPR
                                       TO  IN-FLZ-MNL-EXCO

           .
       234000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       235000-CONSULTAR-IMPACTO        SECTION.
      *--> Verifica se há impacto manual vigente cadastrado p/ operação
      *    Tabela IFLC_EXCO_OPR
      *-----------------------------------------------------------------
           MOVE  0013                  TO  MSG1-CD-SEQL-ERRO

      *--> Movimentar o nome do programa a ser executado
           MOVE  SGTSL094              TO  NM-PGM

      *--> Movimentar os campos de entrada
           MOVE  0001                  TO  SGTSL094-E-CD-TIP-SRVC
           MOVE  SGTS0083-CD-CTRA-TIT-VL
                                       TO  SGTSL094-E-CD-CTRA-TIT-VL
           MOVE  SGTSX005-CD-TIT-OPR-ETRD
                                       TO  SGTSL094-E-CD-TIT-OPR-ETRD
           MOVE  SGTSX005-NR-DVS-TIT   TO  SGTSL094-E-NR-DVS-TIT
           MOVE  'CURRENT'             TO  SGTSL094-E-TS-ATL-IFLC-MNL

           PERFORM 900000-EXECUTAR-MODULO

           IF  SGTSL094-QT-OCR-TAB         POSITIVE
               MOVE  'S'               TO  IN-IFLC-MNL-EXCO
           ELSE
               MOVE  'N'               TO  IN-IFLC-MNL-EXCO
           END-IF

           .
       235000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       236000-CONSULTAR-EXCECAO        SECTION.
      *--> Consultar se existe exceção ativa para a operação
      *-----------------------------------------------------------------
           MOVE  0014                  TO  MSG1-CD-SEQL-ERRO

      *--> Movimentar o nome do programa a ser executado
           MOVE  SGTSL083              TO  NM-PGM

      *--> Movimentar os campos de entrada
           MOVE  0001                  TO  SGTSL083-E-CD-TIP-SRVC
           MOVE  SGTS0083-CD-CTRA-TIT-VL
                                       TO  SGTSL083-CD-CTRA-TIT-VL
           MOVE  SGTS0083-CD-OPR-ETRD
                                       TO  SGTSL083-CD-OPR-ETRD-MCP
           MOVE  SGTS0083-NR-VRS-OPR-ETRD
                                       TO  SGTSL083-NR-VRS-OPR-ETRD

           PERFORM 900000-EXECUTAR-MODULO

           IF  SGTSL083-QT-OCR-TAB         POSITIVE
               MOVE  'S'               TO  IN-EXCO-ATV
           ELSE
               MOVE  'N'               TO  IN-EXCO-ATV
           END-IF

           .
       236000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       237000-OBTER-CLIENTES-OPERACAO  SECTION.
      *-----------------------------------------------------------------
           MOVE  0015                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      SGTS0084-DADOS

      *--> Movimentar o nome do programa a ser executado
           MOVE  SGTS0084              TO  NM-PGM

      *--> Movimentar os campos de entrada
           MOVE  SGTS0083-CD-CTRA-TIT-VL   TO
                                           SGTS0084-CD-CTRA-TIT-VL
           MOVE  SGTS0083-CD-OPR-ETRD  TO  SGTS0084-CD-OPR-ETRD
           MOVE  SGTS0083-NR-VRS-OPR-ETRD
                                       TO  SGTS0084-NR-VRS-OPR-ETRD

           PERFORM  900000-EXECUTAR-MODULO

           .
       237000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       238000-OBTER-ULT-SDO-OPR        SECTION.
      *-----------------------------------------------------------------
           MOVE  0016                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      SGTSPE04-DADOS

      *--> Movimentar o nome do programa a ser executado
           MOVE  SGTSPE04              TO  NM-PGM

      *--> Movimentar os campos de entrada
           MOVE  SGTS0083-CD-CTRA-TIT-VL
                                       TO  SGTSPE04-CD-CTRA-TIT-VL
           MOVE  SGTSX005-CD-TIT-OPR-ETRD
                                       TO  SGTSPE04-CD-TIT-OPR-ETRD
           MOVE  SGTSX005-NR-DVS-TIT   TO  SGTSPE04-NR-DVS-TIT

           PERFORM  900000-EXECUTAR-MODULO

           .
       238000-SAI.
           EXIT.


      *-----------------------------------------------------------------
       240000-CALCULAR                 SECTION.
      *-----------------------------------------------------------------
           MOVE  0017                  TO  MSG1-CD-SEQL-ERRO

           PERFORM  241000-CALCULA-VL-EMS-TIT-OPR

           PERFORM  242000-CALCULA-QT-AA-DRCO-OPR

           .
       240000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       241000-CALCULA-VL-EMS-TIT-OPR   SECTION.
      *--> Calcula o Valor de Emissão do Título da Operação
      *-----------------------------------------------------------------
           MOVE  0018                  TO  MSG1-CD-SEQL-ERRO

           MULTIPLY  SGTSX005-VL-PU-EMS-TIT-OPR
                     BY  SGTSX005-QT-PAPL-TIT-EMS
                     GIVING  SGTS0083-VL-EMS-TIT-OPR  ROUNDED
                     ON SIZE ERROR
                     SET   ERRO-PROGRAMADO
                                       TO  TRUE
                     MOVE  0062        TO  MSG1-CD-ERRO-TS
                     MOVE  'Erro no cálculo do valor de emissão do títul
      -                    'o.'        TO  MSG1-TX-ERRO-TS
                     PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-MULTIPLY

           .
       241000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       242000-CALCULA-QT-AA-DRCO-OPR   SECTION.
      *--> Calcula a Quantidade de Anos de Duracao da Operacao
      *-----------------------------------------------------------------
           MOVE  0019                  TO  MSG1-CD-SEQL-ERRO

           IF  SGTSX005-DT-EMS-TIT-OPR NOT EQUAL SPACES  AND
               SGTSX005-DT-VNCT-OPR    NOT EQUAL SPACES

               MOVE  SBDATAF           TO  NM-PGM

               MOVE  4095              TO  AREA-TAMANHO
               MOVE  'SBDATAF -WRKAREA-FIM'
                                       TO  AREA-FIM
               MOVE  CORR SGTSX005-DT-EMS-TIT-OPR-R
                                       TO  ARG01-R
               MOVE  CORR SGTSX005-DT-VNCT-OPR-R
                                       TO  ARG02-R

               CALL  SBDATAF           USING  FUNCAO
                                              AREA-TRABALHO
                                              ARG01  ARG02  ARG03

               IF  RETURN-CODE             NOT EQUAL  ZEROS
                   SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
                   MOVE  0067          TO  MSG1-CD-ERRO-TS
                   MOVE  'Erro na rotina que valida data.'
                                       TO  MSG1-TX-ERRO-TS
                   MOVE  SBDATAF       TO  MSG1-NM-PGM-ACND
                   MOVE  RETURN-CODE   TO  MSG1-CD-RTN-PGM-ACND
                   PERFORM  969000-MONITORAR-VARIAVEIS
                   PERFORM  999000-GRAVA-ERRO-E-ENCERRA
               END-IF

               IF  ARG03                   LESS  ZEROS  AND
                   RETURN-CODE             EQUAL  ZEROS
                   SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
                   MOVE  0063          TO  MSG1-CD-ERRO-TS
                   MOVE  'Data da emissão maior que data do vencimento.'
                                       TO  MSG1-TX-ERRO-TS
                   MOVE  SBDATAF       TO  MSG1-NM-PGM-ACND
                   MOVE  RETURN-CODE   TO  MSG1-CD-RTN-PGM-ACND
                   PERFORM  969000-MONITORAR-VARIAVEIS
                   PERFORM  999000-GRAVA-ERRO-E-ENCERRA
               END-IF

               IF  RETURN-CODE             EQUAL  ZEROS
                   COMPUTE  SGTS0083-QT-AA-DRCO-OPR  ROUNDED  =
                                   ARG03 / 365
                       ON SIZE ERROR
                       SET   ERRO-PROGRAMADO
                                       TO  TRUE
                       MOVE  0064      TO  MSG1-CD-ERRO-TS
                       MOVE  'Erro no cálculo do prazo em anos.'
                                       TO  MSG1-TX-ERRO-TS
                       PERFORM  999000-GRAVA-ERRO-E-ENCERRA
                   END-COMPUTE
               END-IF
           ELSE
               MOVE  ZEROS             TO  SGTS0083-QT-AA-DRCO-OPR
           END-IF
           .
       242000-SAI.
           EXIT.


      *-----------------------------------------------------------------
       250000-OBTER-AGENCIA-E-CONTA    SECTION.
      *-----------------------------------------------------------------
           MOVE  0020                  TO  MSG1-CD-SEQL-ERRO

           IF  SGTSX005-CD-BCO-CRD-VL-OPR  POSITIVE

               PERFORM  251000-OBTER-NOME-BANCO

               PERFORM  252000-OBTER-NOME-AGENCIA

           END-IF

           .
       250000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       251000-OBTER-NOME-BANCO         SECTION.
      *-----------------------------------------------------------------
           MOVE  0021                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      CAFS0053-DADOS

      *--> Movimentar o nome do programa a ser executado
           MOVE  CAFS0053              TO  NM-PGM

      *--> Movimentar os campos de entrada
           MOVE  0002                  TO  S053-FUC
           MOVE  SGTSX005-CD-BCO-CRD-VL-OPR
                                       TO  S053-CD-CPSO OF S053-ENTD

           PERFORM  910000-CHAMAR-SERVICO
           .
       251000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       252000-OBTER-NOME-AGENCIA       SECTION.
      *--> Aciona a rotina CAFS0007 e obtem o nome
      *    de uma Agencia Externa
      *-----------------------------------------------------------------
           MOVE  0022                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      PARM-DADOS

      *--> Movimentar o nome do programa a ser executado
           MOVE  CAFS0007              TO  NM-PGM

      *--> Movimentar os campos de entrada
           MOVE  SGTSX005-CD-BCO-CRD-VL-OPR
                                       TO  PARM-BANCO-INF
           MOVE  SGTSX005-CD-DEPE-CRD-VL-OPR
                                       TO  PARM-AGE-INF

           PERFORM  910000-CHAMAR-SERVICO
           .
       252000-SAI.
           EXIT.


      *-----------------------------------------------------------------
       260000-OBTER-TEXTOS-INTERNOS    SECTION.
      *-----------------------------------------------------------------
           MOVE  0023                  TO  MSG1-CD-SEQL-ERRO

      *--> Obter Nome da Categoria de Titulos e Valores Mobiliários
           IF  SGTSX005-CD-CTGR-TVM        GREATER  ZEROS
               PERFORM  261000-OBTER-NOME-CTGR-BC-TVM
           END-IF

      *--> Obter Nome da Instrução da Comissão de Valores Mobiliarios
           IF  SGTSX005-CD-ITC-CVM-OPR     GREATER  ZEROS
               PERFORM  262000-OBTER-NOME-ITC-CVM
           END-IF

      *--> Nome do Tipo de Quota do Titulo da Operação
           IF  SGTSX005-CD-TIP-COTA-TIT    GREATER  ZEROS
               PERFORM  263000-OBTER-NOME-TIPO-COTA
           END-IF

           .
       2600000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       261000-OBTER-NOME-CTGR-BC-TVM   SECTION.
      *--> Recupera nome da categoria Bacen
      *-----------------------------------------------------------------
           MOVE  0024                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      SGTST029-DADOS

      *--> Movimento nome do módulo para monitoração de variáveis
           MOVE  SGTST029              TO  NM-PGM

      *--> Movimentar os campos de entrada
           SET   SGTST029-CONSULTAR-COD-CTGR-BC
                                       TO  TRUE
           MOVE  SGTSX005-CD-CTGR-TVM  TO  SGTST029-CD-CTGR-TVM

           EXEC CICS  LINK
                      PROGRAM  ( SGTST029       )
                      COMMAREA ( SGTST029-DADOS )
           END-EXEC

           IF  SGTST029-CD-RTN-PGM         NOT EQUAL  ZEROS  AND  +100
               SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE  3004              TO  MSG1-CD-ERRO-TS
               STRING  'Erro ao acionar o módulo ' NM-PGM '.'
                       DELIMITED BY SIZE
                       INTO                MSG1-TX-ERRO-TS
               END-STRING
               MOVE  NM-PGM            TO  MSG1-NM-PGM-ACND
               MOVE  SGTST029-MSG-ERRO-PGM
                                       TO  MSG1-TX-RTN-PGM-ACND
               MOVE  SGTST029-CD-RTN-PGM
                                       TO  MSG1-CD-RTN-PGM-ACND
               MOVE  SGTST029-PSC-ERRO-PGM
                                       TO  MSG1-CD-SEQL-PGM-ACND
               PERFORM  969000-MONITORAR-VARIAVEIS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       261000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       262000-OBTER-NOME-ITC-CVM       SECTION.
      *--> Recupera nome da Instrução da Comissão de Valores Mobiliarios
      *-----------------------------------------------------------------
           MOVE  0025                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      SGTST030-DADOS

      *--> Movimento nome do módulo para monitoração de variáveis
           MOVE  SGTST030              TO  NM-PGM

      *--> Movimentar os campos de entrada
           SET   SGTST030-CONSULTAR    TO  TRUE
           MOVE  SGTSX005-CD-ITC-CVM-OPR
                                       TO  SGTST030-CD-ITC-CVM-OPR

           EXEC CICS  LINK
                      PROGRAM  ( SGTST030       )
                      COMMAREA ( SGTST030-DADOS )
           END-EXEC

           IF  SGTST030-CD-RTN-PGM         NOT EQUAL  ZEROS  AND  +100
               SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE  3004              TO  MSG1-CD-ERRO-TS
               STRING  'Erro ao acionar o módulo ' NM-PGM '.'
                       DELIMITED BY SIZE
                       INTO                MSG1-TX-ERRO-TS
               END-STRING
               MOVE  NM-PGM            TO  MSG1-NM-PGM-ACND
               MOVE  SGTST030-MSG-ERRO-PGM
                                       TO  MSG1-TX-RTN-PGM-ACND
               MOVE  SGTST030-CD-RTN-PGM
                                       TO  MSG1-CD-RTN-PGM-ACND
               MOVE  SGTST030-PSC-ERRO-PGM
                                       TO  MSG1-CD-SEQL-PGM-ACND
               PERFORM  969000-MONITORAR-VARIAVEIS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       262000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       263000-OBTER-NOME-TIPO-COTA     SECTION.
      *--> Recupera nome do tipo de cota
      *-----------------------------------------------------------------
           MOVE  0026                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      SGTST027-DADOS

      *--> Movimento nome do módulo para monitoração de variáveis
           MOVE  SGTST027              TO  NM-PGM

      *--> Movimentar os campos de entrada
           SET   SGTST027-CONSULTAR    TO  TRUE
           MOVE  SGTSX005-CD-TIP-COTA-TIT
                                       TO  SGTST027-CD-TIP-COTA-TIT
           MOVE  SGTSX005-NR-VRS-TIP-COTA
                                       TO  SGTST027-NR-VRS-TIP-COTA
           MOVE  KOPRD-CD-OPRD         TO  SGTST027-CD-USU-RSP-ATL

           EXEC CICS  LINK
                      PROGRAM  ( SGTST027       )
                      COMMAREA ( SGTST027-DADOS )
           END-EXEC

           IF  SGTST027-CD-RTN-PGM         NOT EQUAL  ZEROS  AND  +100
               SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE  3004              TO  MSG1-CD-ERRO-TS
               STRING  'Erro ao acionar o módulo ' NM-PGM '.'
                       DELIMITED BY SIZE
                       INTO                MSG1-TX-ERRO-TS
               END-STRING
               MOVE  NM-PGM            TO  MSG1-NM-PGM-ACND
               MOVE  SGTST027-MSG-ERRO-PGM
                                       TO  MSG1-TX-RTN-PGM-ACND
               MOVE  SGTST027-CD-RTN-PGM
                                       TO  MSG1-CD-RTN-PGM-ACND
               MOVE  SGTST027-PSC-ERRO-PGM
                                       TO  MSG1-CD-SEQL-PGM-ACND
               PERFORM  969000-MONITORAR-VARIAVEIS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       263000-SAI.
           EXIT.


      *-----------------------------------------------------------------
       270000-OBTER-TEXTOS-EXTERNOS    SECTION.
      *-----------------------------------------------------------------
           MOVE  0027                  TO  MSG1-CD-SEQL-ERRO

      *--> Obter Código do Usuário
           IF  SGTSX005-CD-USU-ULT-ATL     NOT EQUAL  SPACES
               PERFORM  271000-OBTER-NOME-USU-ATL
           END-IF

      *--> Obter Nome da Câmara do Registro do Título da Operação
           IF  SGTSX005-CD-CMRA-OPR-ETRD   GREATER  ZEROS
               PERFORM  272000-OBTER-NOME-CMRA-REG
           END-IF

      *--> Obter Nome da UOR Detentora da Operação
           IF  SGTSX005-CD-UOR-DTTR-OPR    GREATER  ZEROS
               PERFORM  273000-OBTER-DADOS-UOR-DTTR
           END-IF

      *--> Obter Nome da Espécie de Debênture
           IF  SGTSX005-CD-ESPC-DBTR-OPR   GREATER  ZEROS
               PERFORM  274000-OBTER-NOME-ESPC-DBTR
           END-IF
           .
       270000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       271000-OBTER-NOME-USU-ATL       SECTION.
      *--> Recupera o nome do usuário que efetuou a atualização
      *-----------------------------------------------------------------
           MOVE  0028                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      ATCS6000-DADOS

      *--> Movimentar o nome do programa a ser executado
           MOVE  ATCS6000              TO  NM-PGM

      *--> Movimentar os campos de entrada
           SET   K6000-PSQ-INF-USU     TO  TRUE
           MOVE  SGTSX005-CD-USU-ULT-ATL
                                       TO  K6000-CD-USU
                                           SGTS0083-CD-USU-ATL
           PERFORM  910000-CHAMAR-SERVICO

           IF  K6000-CD-RTN                EQUAL  ZEROS
               MOVE  K6000-NM-USU      TO  SGTS0083-NM-USU-ATL
           ELSE
               MOVE  SPACES            TO  SGTS0083-NM-USU-ATL
           END-IF
           .
       271000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       272000-OBTER-NOME-CMRA-REG      SECTION.
      *--> Recupera o nome da Câmara de Registro
      *-----------------------------------------------------------------
           MOVE  0029                  TO  MSG1-CD-SEQL-ERRO

      *--> Movimentar o nome do programa a ser executado
           MOVE  ICLSI046              TO  NM-PGM

      *--> Movimentar os campos de entrada
           MOVE  SGTSX005-CD-CMRA-OPR-ETRD
                                       TO  I046-CD-CLRG

           PERFORM  910000-CHAMAR-SERVICO

           .
       272000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       273000-OBTER-DADOS-UOR-DTTR     SECTION.
      *--> Recupera nome da UOR Detentora da Operacao
      *-----------------------------------------------------------------
           MOVE  0030                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      MSTS8042-DADOS

      *--> Movimentar o nome do programa a ser executado
           MOVE  MSTS8042              TO  NM-PGM

      *--> Movimentar os campos de entrada
           MOVE  SGTSX005-CD-UOR-DTTR-OPR
                                       TO  MSTS8042-CD-UOR

           PERFORM  910000-CHAMAR-SERVICO

           .
       273000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       274000-OBTER-NOME-ESPC-DBTR     SECTION.
      *--> Recupera Nome da Espécie de Debênture
      *-----------------------------------------------------------------
           MOVE  0031                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      CACS153C-DADOS

      *--> Movimento nome do módulo para monitoração de variáveis
           MOVE  CACS153C              TO  NM-PGM

      *--> Movimentar os campos de entrada
           MOVE  SGTSX005-CD-ESPC-DBTR-OPR
                                       TO  CACS153C-CD-TIP-GRT

           EXEC CICS  LINK
                      PROGRAM  ( CACS153C                 )
                      COMMAREA ( CACS153C-DADOS           )
                      LENGTH   ( LENGTH OF CACS153C-DADOS )
                      RESP     ( MSG1-RESP1               )
                      RESP2    ( MSG1-RESP2               )
           END-EXEC

           IF  CACS153C-CD-RTN-PGM         NOT EQUAL  ZEROS
               SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE  3004              TO  MSG1-CD-ERRO-TS
               STRING  'Erro ao acionar o módulo ' NM-PGM '.'
                       DELIMITED BY SIZE
                       INTO                MSG1-TX-ERRO-TS
               END-STRING
               MOVE  NM-PGM            TO  MSG1-NM-PGM-ACND
               MOVE  CACS153C-MSG-ERRO-PGM
                                       TO  MSG1-TX-RTN-PGM-ACND
               MOVE  CACS153C-CD-RTN-PGM
                                       TO  MSG1-CD-RTN-PGM-ACND
               MOVE  CACS153C-PSC-ERRO-PGM
                                       TO  MSG1-CD-SEQL-PGM-ACND
               PERFORM  969000-MONITORAR-VARIAVEIS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       275000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       280000-OBTER-NOMES-TAXONOMIA    SECTION.
      *--> Verifica se há impacto manual vigente cadastrado p/ operação
      *    Tabela IFLC_EXCO_OPR
      *-----------------------------------------------------------------
           MOVE  0032                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      SGTS0300-RQSC

      *--> Movimentar o nome do programa a ser executado
           MOVE  SGTS0300              TO  NM-PGM

      *--> Movimentar os campos de entrada
           MOVE  SGTSX005-CD-PRD       TO  SGTS0300-CD-PRD
           MOVE  SGTSX005-CD-MDLD      TO  SGTS0300-CD-MDLD
           MOVE  SGTSX005-CD-CMCP      TO  SGTS0300-CD-CMCP-ENTD
           MOVE  SGTSX005-CD-GR-MCP    TO  SGTS0300-CD-GR-MCP-ENTD

           PERFORM 900000-EXECUTAR-MODULO

           .
       280000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-ENCERRAR                 SECTION.
      *-----------------------------------------------------------------
           MOVE  0032                  TO  MSG1-CD-SEQL-ERRO

           PERFORM  310000-MOVER-DADOS-OPERACAO

           PERFORM  320000-MOVER-DADOS-TITULO

           PERFORM  330000-MOVER-DADOS-CONTA

           PERFORM  340000-MOVER-DATAS-E-VALORES

           PERFORM  350000-MOVER-INDICADORES

           PERFORM  360000-MOVER-DADOS-CLIENTES

           PERFORM  370000-MOVER-NOMES-TAXONOMIA

           PERFORM  980000-GRAVAR-CONTAINER-RPST

           PERFORM  999999-ENCERRAR
           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       310000-MOVER-DADOS-OPERACAO     SECTION.
      *-----------------------------------------------------------------
           MOVE  0033                  TO  MSG1-CD-SEQL-ERRO

      *--> Mover Número da Versão Aprovada, Estado da Operação e
      *    Estado da Versão da Operação Pesquisada
           MOVE  SGTS0083-NR-VRS-OPR-ETRD
                                       TO  SGTS0083-NR-VRS-OPR-PSQD
           MOVE  SGTST009-CD-TIP-EST-OPR
                                       TO  SGTS0083-CD-TIP-EST-OPR
           MOVE  SGTSC069-CD-TIP-EST-VRS
                                       TO  SGTS0083-CD-TIP-EST-VRS

      *--> Produto e Modalidade
           MOVE  SGTSX005-CD-PRD       TO  SGTS0083-CD-PRD
           MOVE  SGTSX005-CD-MDLD      TO  SGTS0083-CD-MDLD

      *--> Código e Nome da UOR Detentora da Operação
           MOVE  SGTSX005-CD-UOR-DTTR-OPR
                                       TO  SGTS0083-CD-UOR-DTTR-OPR
           IF  MSTS8042-CD-DEPE-UOR        POSITIVE  OR
               MSTS8042-NM-UOR-RDZ         NOT EQUAL  SPACES
               MOVE  MSTS8042-NM-UOR-RDZ
                                       TO  SGTS0083-NM-UOR-DTTR-OPR
           ELSE
               MOVE  SPACES            TO  SGTS0083-NM-UOR-DTTR-OPR
           END-IF

      *--> Taxonomia
           MOVE  SGTSX005-CD-CMCP      TO  SGTS0083-CD-CMCP
           MOVE  SGTSX005-CD-GR-MCP    TO  SGTS0083-CD-GR-MCP
           MOVE  SGTSX005-CD-LVRO-MC-CPTL
                                       TO  SGTS0083-CD-LVRO-MC-CPTL
           MOVE  SGTSX005-CD-SMCP      TO  SGTS0083-CD-SMCP

      *--> Tipo de Versão da Operação (Estado Negocial da Operação)
           MOVE SGTSX005-CD-TIP-VRS-OPR
                                       TO SGTS0083-CD-TIP-VRS-OPR

      *--> Classficação do Instrumento Financeiro

           MOVE  SGTSX005-CD-CLSC-ITMT-FNCO
                                       TO  SGTS0083-CD-CLSC-ITMT-FNCO

           IF SGTSX005-CD-CLSC-ITMT-FNCO EQUAL 1
              MOVE  'Normal'
                                       TO  SGTS0083-NM-CLSC-ITMT-FNCO
           END-IF

           IF SGTSX005-CD-CLSC-ITMT-FNCO EQUAL 2
              MOVE  'Risco de Crédito Aumentado'
                                       TO  SGTS0083-NM-CLSC-ITMT-FNCO
           END-IF

           IF SGTSX005-CD-CLSC-ITMT-FNCO EQUAL 3
              MOVE  'Problema de recuperação de crédito'
                                       TO  SGTS0083-NM-CLSC-ITMT-FNCO
           END-IF

      *--> Estágio Negocial

           MOVE SGTSX005-CD-ESG-NEGL
                                       TO  SGTS0083-CD-ESG-NEGL

           IF SGTSX005-CD-ESG-NEGL EQUAL 1
              MOVE 'Normalidade'
                                       TO  SGTS0083-NM-ESG-NEGL
           END-IF

           IF SGTSX005-CD-ESG-NEGL EQUAL 3
              MOVE 'Ativo Problemático'
                                       TO  SGTS0083-NM-ESG-NEGL
           END-IF

            IF SGTSX005-CD-TIP-VRS-OPR EQUAL 1
             MOVE 'Normalidade'
                                       TO SGTS0083-NM-TIP-VRS-OPR
           END-IF

      * Motivo do Estado Negocial quando Estado Negocial = Renegociação:

           IF SGTSX005-CD-TIP-VRS-OPR EQUAL 2
             MOVE 'Renegociação'
                                       TO SGTS0083-NM-TIP-VRS-OPR
           END-IF

      * Estado negocial = 2 ou 4- Alteração(pode ter operaçao que nasceu
      * renegociação e teve versionamento = alteração)
           IF SGTSX005-CD-TIP-VRS-OPR EQUAL 2 OR
              SGTSX005-CD-TIP-VRS-OPR EQUAL 4
             IF SGTSX005-CD-MTV-EST-NEGL  EQUAL 1
                MOVE 'Renegociação - Origem Atraso'
                                       TO SGTS0083-NM-MTV-EST-NEGL
             END-IF

              IF SGTSX005-CD-MTV-EST-NEGL  EQUAL 2
                 MOVE 'Renegociação - RAO'
                                       TO SGTS0083-NM-MTV-EST-NEGL
              END-IF

              IF SGTSX005-CD-MTV-EST-NEGL  EQUAL 3
                 MOVE 'Renegociação - Origem Renegociação'
                                       TO SGTS0083-NM-MTV-EST-NEGL
              END-IF

              IF SGTSX005-CD-MTV-EST-NEGL  EQUAL 99
                 MOVE 'Demais'
                                       TO SGTS0083-NM-MTV-EST-NEGL
              END-IF
           END-IF

           IF SGTSX005-CD-TIP-VRS-OPR = 3
             MOVE 'Repactuação'
                                       TO SGTS0083-NM-TIP-VRS-OPR
           END-IF

           IF SGTSX005-CD-TIP-VRS-OPR = 4
             MOVE 'Alteração'
                                       TO SGTS0083-NM-TIP-VRS-OPR
           END-IF

      * Motivo Estado Negocial quando Estado Negocial = Reestruturação:
           IF SGTSX005-CD-TIP-VRS-OPR = 5
             MOVE 'Reestruturação'
                                       TO SGTS0083-NM-TIP-VRS-OPR
           END-IF

      * Estado negocial = 5 ou 4- Alteração(pode ter operaçao que nasceu
      * reestruturada e teve versionamento = alteração)
           IF SGTSX005-CD-TIP-VRS-OPR EQUAL 5 OR
              SGTSX005-CD-TIP-VRS-OPR EQUAL 4

              IF SGTSX005-CD-MTV-EST-NEGL  EQUAL 13
                MOVE 'Reestruturação - Atraso'
                                       TO SGTS0083-NM-MTV-EST-NEGL
              END-IF

              IF SGTSX005-CD-MTV-EST-NEGL  EQUAL 14
                 MOVE 'Reestruturação - Abatimento Negocial'
                                       TO SGTS0083-NM-MTV-EST-NEGL
              END-IF

              IF SGTSX005-CD-MTV-EST-NEGL  EQUAL 15
                 MOVE 'Reestruturação - Origem Ativo Problemático'
                                       TO SGTS0083-NM-MTV-EST-NEGL
              END-IF

              IF SGTSX005-CD-MTV-EST-NEGL  EQUAL 16
                 MOVE 'Reestruturação - Origem Perdas'
                                       TO SGTS0083-NM-MTV-EST-NEGL
              END-IF

              IF SGTSX005-CD-MTV-EST-NEGL  EQUAL 17
                 MOVE 'Reestruturação - Stand Still'
                                       TO SGTS0083-NM-MTV-EST-NEGL
              END-IF

              IF SGTSX005-CD-MTV-EST-NEGL  EQUAL 18
                 MOVE 'Reestruturação - Outros - Agro'
                                       TO SGTS0083-NM-MTV-EST-NEGL
              END-IF

              IF SGTSX005-CD-MTV-EST-NEGL  EQUAL 19
                 MOVE 'Reestruturação - Outros'
                                       TO SGTS0083-NM-MTV-EST-NEGL
              END-IF

              IF SGTSX005-CD-MTV-EST-NEGL  EQUAL 99
                 MOVE 'Demais'
                                       TO SGTS0083-NM-MTV-EST-NEGL
              END-IF
           END-IF

      *--> Número Único OPR
           MOVE SGTSX005-NR-UNCO-CTR-OPR
                                       TO SGTS0083-NR-UNCO-CTR-OPR
      *--> Codigo do Motivo do Estado Negocial
           MOVE SGTSX005-CD-MTV-EST-NEGL
                                       TO SGTS0083-CD-MTV-EST-NEGL
           .
       310000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       320000-MOVER-DADOS-TITULO       SECTION.
      *-----------------------------------------------------------------
           MOVE  0034                  TO  MSG1-CD-SEQL-ERRO

      *--> Título CETIP
           MOVE  SGTSX005-CD-TIT-OPR-ETRD
                                       TO  SGTS0083-CD-TIT-OPR-ETRD
           MOVE  SGTSX005-NR-DVS-TIT   TO  SGTS0083-NR-DVS-TIT

      *--> Instrução CVM
           MOVE  SGTSX005-CD-ITC-CVM-OPR
                                       TO  SGTS0083-CD-ITC-CVM
           MOVE  SGTST030-NM-ITC-CVM   TO  SGTS0083-NM-ITC-CVM

      *--> Categoria Banco Central
           MOVE  SGTSX005-CD-CTGR-TVM  TO  SGTS0083-CD-CTGR-TVM
           MOVE  SGTST029-NM-CTGR-TVM  TO  SGTS0083-NM-CTGR-TVM

      *--> Código e Nome da Espécie de Debênture
           MOVE  SGTSX005-CD-ESPC-DBTR-OPR
                                       TO  SGTS0083-CD-ESPC-DBTR-OPR
           IF  CACS153C-CD-RTN-PGM         EQUAL  ZEROS  OR
               CACS153C-TX-DCR-TIP-GRT     NOT EQUAL  SPACES
               MOVE  CACS153C-TX-DCR-TIP-GRT
                                       TO  SGTS0083-NM-ESPC-DBTR-OPR
           ELSE
               MOVE  SPACES            TO  SGTS0083-NM-UOR-DTTR-OPR
           END-IF

      *--> Código, Versão e Nome do Tipo de Cota
           MOVE  SGTSX005-CD-TIP-COTA-TIT
                                       TO  SGTS0083-CD-TIP-COTA-TIT
           MOVE  SGTSX005-NR-VRS-TIP-COTA
                                       TO  SGTS0083-NR-VRS-TIP-COTA
           MOVE  SGTST027-NM-TIP-COTA-TIT
                                       TO  SGTS0083-NM-TIP-COTA-TIT

      *--> Código e Nome da Câmara do Registro do Título
           MOVE  SGTSX005-CD-CMRA-OPR-ETRD
                                       TO  SGTS0083-CD-CMRA-OPR-ETRD
           IF  I046-QT-OCR                 POSITIVE
               MOVE  I046-NM-CLRG (0001)
                                       TO  SGTS0083-NM-CMRA-OPR-ETRD
           ELSE
               MOVE  SPACES            TO  SGTS0083-NM-CMRA-OPR-ETRD
           END-IF

      *--> Informações do Título
           MOVE  SGTSX005-NR-EMS-TIT-OPR
                                       TO  SGTS0083-NR-EMS-TIT-OPR
           MOVE  SGTSX005-NR-SRE-EMS-TIT-OPR
                                       TO  SGTS0083-NR-SRE-EMS-TIT-OPR
           MOVE  SGTST002-CD-ISIN-TIT-OPR
                                       TO  SGTS0083-CD-ISIN-TIT-OPR
           MOVE  SGTSX005-CD-TIP-MC-CPR
                                       TO  SGTS0083-CD-TIP-MC-CPR

      *--> Código do Risco da Operação
           MOVE  SGTSX005-CD-RSCO-OPR  TO  SGTS0083-CD-RSCO-OPR

           .
       320000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       330000-MOVER-DADOS-CONTA        SECTION.
      *--> Dados Banco, Agência e Conta Corrente de Crédito de Valor
      *-----------------------------------------------------------------
           MOVE  0035                  TO  MSG1-CD-SEQL-ERRO

           IF  SGTSX005-CD-BCO-CRD-VL-OPR  POSITIVE

      *------> Movimentar Dados Banco de Crédito
               MOVE  SGTSX005-CD-BCO-CRD-VL-OPR
                                       TO  SGTS0083-CD-BCO-CRD-VL-OPR

               IF  S053-CD-RTN             EQUAL  ZEROS
                   MOVE  S053-NM-INST  TO  SGTS0083-NM-BCO-CRD-VL-OPR
               ELSE
                   MOVE  SPACES        TO  SGTS0083-NM-BCO-CRD-VL-OPR
               END-IF

      *------> Movimentar Dados Agência de Crédito
               MOVE  SGTSX005-CD-DEPE-CRD-VL-OPR
                                       TO  SGTS0083-CD-DEPE-CRD-VL-OPR

               IF  PARM-RET-CODE           EQUAL  ZEROS
                   MOVE  PARM-NOME-AGEN
                                       TO  SGTS0083-NM-DEPE-CRD-VL-OPR
               ELSE
                   MOVE  SPACES        TO  SGTS0083-NM-DEPE-CRD-VL-OPR
               END-IF

           END-IF

      *--> Movimentar Conta Corrente de Crédito
           IF  SGTSX005-CD-BCO-CRD-VL-OPR  EQUAL  0001
               MOVE  SGTSX005-NR-CC-BB
                                       TO  SGTS0083-NR-CC-CRD-VL-OPR
           ELSE
               MOVE  SGTSX005-NR-CC-OTR-BCO
                                       TO  SGTS0083-NR-CC-CRD-VL-OPR
           END-IF

           .
       330000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       340000-MOVER-DATAS-E-VALORES    SECTION.
      *-----------------------------------------------------------------
           MOVE  0036                  TO  MSG1-CD-SEQL-ERRO

      *--> Dados Emissão Título
           MOVE  CORR SGTSX005-DT-EMS-TIT-OPR-R
                                       TO  SGTS0083-DT-EMS-TIT-OPR-R
           MOVE  SGTSX005-QT-PAPL-TIT-EMS
                                       TO  SGTS0083-QT-PAPL-TIT-EMS
           MOVE  SGTSX005-VL-PU-EMS-TIT-OPR
                                       TO  SGTS0083-VL-PU-EMS-TIT-OPR

      *--> Dados Compra Título
           MOVE  CORR SGTSX005-DT-CPR-OPR-R
                                       TO  SGTS0083-DT-CPR-OPR-R
           MOVE  SGTSX005-QT-PAPL-TIT-CPR
                                       TO  SGTS0083-QT-PAPL-TIT-CPR
           MOVE  SGTSX005-VL-PU-CPR-TIT-OPR
                                       TO  SGTS0083-VL-PU-CPR-TIT-OPR
           MOVE  SGTSX005-VL-CPR-OPR   TO  SGTS0083-VL-CPR-OPR

      *--> Data Vencimento
           MOVE  CORR SGTSX005-DT-VNCT-OPR-R
                                       TO  SGTS0083-DT-VNCT-OPR-R

      *--> Valor Previsão de Encarteiramento e Valor Aprovado BB
           MOVE  SGTSX005-VL-PRV-ENCT  TO  SGTS0083-VL-PRV-ENCT
           MOVE  SGTSX005-VL-APVD-BB-OPR
                                       TO  SGTS0083-VL-APVD-BB-OPR

      *--> Data e Valor Mandato da Operação
           MOVE  CORR SGTSX005-DT-MNDT-OPR-R
                                       TO  SGTS0083-DT-MNDT-OPR-R
           MOVE  SGTSX005-VL-MNDT-OPR  TO  SGTS0083-VL-MNDT-OPR

      *--> Valor Garantia
           MOVE  SGTSX005-VL-GRT-BB-OPR
                                       TO  SGTS0083-VL-GRT-BB-OPR

      *--> Data e Valor do Ultimo Saldo
           MOVE  SGTSPE04-DT-PSC       TO  SGTS0083-DT-ULT-PSC-SDO
           MOVE  SGTSPE04-VL-SDO-TIT-DT-PSC
                                       TO  SGTS0083-VL-SDO-TIT-DT-PSC

      *--> Texto Pagamento Operação Vinculada
           MOVE  SGTSX005-TX-PGTO-OPR-VCLD
                                       TO  SGTS0083-TX-PGTO-OPR-VCLD

           .
       340000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       350000-MOVER-INDICADORES        SECTION.
      *--> Movimentar Indicadores da Operacao
      *-----------------------------------------------------------------
           MOVE  0037                  TO  MSG1-CD-SEQL-ERRO

           MOVE SGTS0085-IN-CNG-PBCO-OPR
                                       TO  SGTS0083-IN-CNG-PBCO-OPR

           IF  FLEXIBILIZACAO-ATIVA    OR
               IMPACTO-MANUAL-VIGENTE  OR
               EXCECAO-ATIVA
               MOVE  'S'               TO  SGTS0083-IN-FLZ-EXCO-OPR
           ELSE
               MOVE  'N'               TO  SGTS0083-IN-FLZ-EXCO-OPR
           END-IF

           MOVE SGTS0085-IN-OPR-ETRD-IETR
                                       TO  SGTS0083-IN-OPR-ETRD-IETR
           MOVE SGTS0085-IN-N-CNV-OPR-ACAO
                                       TO  SGTS0083-IN-N-CNV-OPR-ACAO
           MOVE SGTS0085-IN-OPR-SPT-FCTR
                                       TO  SGTS0083-IN-OPR-SPT-FCTR

           .
       350000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       360000-MOVER-DADOS-CLIENTES     SECTION.
      *-----------------------------------------------------------------
           MOVE  0038                  TO  MSG1-CD-SEQL-ERRO

      *--> Dados Cliente Emissor
           MOVE  SGTS0084-CD-CLI-EMT   TO  SGTS0083-CD-CLI-EMT
           MOVE  SGTS0084-NM-CLI-EMT   TO  SGTS0083-NM-CLI-EMT
           MOVE  SGTS0084-CD-RSCO-CLI-EMT
                                       TO  SGTS0083-CD-RSCO-CLI-EMT
           MOVE  SGTS0084-CD-GR-CLI-EMT
                                       TO  SGTS0083-CD-GR-CLI-EMT
           MOVE  SGTS0084-NM-GR-CLI-EMT
                                       TO  SGTS0083-NM-GR-CLI-EMT
           MOVE  SGTS0084-CD-TIP-PSS-EMT
                                       TO  SGTS0083-CD-TIP-PSS-EMT
           MOVE  SGTS0084-NR-CPF-CNPJ-EMT
                                       TO  SGTS0083-NR-CPF-CNPJ-EMT

      *--> Dados Cliente Impacto Limite de Crédito
           MOVE  SGTS0084-CD-CLI-IFLC-LMCR
             TO  SGTS0083-CD-CLI-IFLC-LMCR
           MOVE  SGTS0084-NM-CLI-IFLC-LMCR
             TO  SGTS0083-NM-CLI-IFLC-LMCR
           MOVE  SGTS0084-CD-RSCO-CLI-IFLC-LMCR
             TO  SGTS0083-CD-RSCO-CLI-IFLC-LMCR
           MOVE  SGTS0084-CD-GR-CLI-IFLC-LMCR
             TO  SGTS0083-CD-GR-CLI-IFLC-LMCR
           MOVE  SGTS0084-NM-GR-CLI-IFLC-LMCR
             TO  SGTS0083-NM-GR-CLI-IFLC-LMCR
           MOVE  SGTS0084-CD-TIP-PSS-IFLC-LMCR
             TO  SGTS0083-CD-TIP-PSS-IFLC-LMCR
           MOVE  SGTS0084-NR-CPF-CNPJ-IFLC-LMCR
             TO  SGTS0083-NR-CPF-CNPJ-IFLC-LMCR

      *--> Dados Cliente Cedente
           MOVE  SGTS0084-CD-CLI-CDT   TO  SGTS0083-CD-CLI-CDT
           MOVE  SGTS0084-NM-CLI-CDT   TO  SGTS0083-NM-CLI-CDT
           MOVE  SGTS0084-CD-RSCO-CLI-CDT
                                       TO  SGTS0083-CD-RSCO-CLI-CDT
           MOVE  SGTS0084-CD-GR-CLI-CDT
                                       TO  SGTS0083-CD-GR-CLI-CDT
           MOVE  SGTS0084-NM-GR-CLI-CDT
                                       TO  SGTS0083-NM-GR-CLI-CDT
           MOVE  SGTS0084-CD-TIP-PSS-CDT
                                       TO  SGTS0083-CD-TIP-PSS-CDT
           MOVE  SGTS0084-NR-CPF-CNPJ-CDT
                                       TO  SGTS0083-NR-CPF-CNPJ-CDT

      *--> Dados Cliente Administrador
           MOVE  SGTS0084-CD-CLI-ADM   TO  SGTS0083-CD-CLI-ADM
           MOVE  SGTS0084-NM-CLI-ADM   TO  SGTS0083-NM-CLI-ADM
           MOVE  SGTS0084-CD-GR-CLI-ADM
                                       TO  SGTS0083-CD-GR-CLI-ADM
           MOVE  SGTS0084-NM-GR-CLI-ADM
                                       TO  SGTS0083-NM-GR-CLI-ADM
           MOVE  SGTS0084-CD-TIP-PSS-ADM
                                       TO  SGTS0083-CD-TIP-PSS-ADM
           MOVE  SGTS0084-NR-CPF-CNPJ-ADM
                                       TO  SGTS0083-NR-CPF-CNPJ-ADM

      *--> Dados Cliente Agente Fiduciário
           MOVE  SGTS0084-CD-CLI-AGT-FDAR
                                       TO  SGTS0083-CD-CLI-AGT-FDAR
           MOVE  SGTS0084-NM-CLI-AGT-FDAR
                                       TO  SGTS0083-NM-CLI-AGT-FDAR
           MOVE  SGTS0084-CD-GR-CLI-AGT-FDAR
                                       TO  SGTS0083-CD-GR-CLI-AGT-FDAR
           MOVE  SGTS0084-NM-GR-CLI-AGT-FDAR
                                       TO  SGTS0083-NM-GR-CLI-AGT-FDAR
           MOVE  SGTS0084-CD-TIP-PSS-AGT-FDAR
                                       TO  SGTS0083-CD-TIP-PSS-AGT-FDAR
           MOVE  SGTS0084-NR-CPF-CNPJ-AGT-FDAR
                                       TO  SGTS0083-NR-CPF-CNPJ-AGT-FDAR

           .
       360000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       370000-MOVER-NOMES-TAXONOMIA    SECTION.
      *--> Movimentar Indicadores da Operacao
      *-----------------------------------------------------------------
           MOVE  0037                  TO  MSG1-CD-SEQL-ERRO

           MOVE  SGTS0300-TX-CMCP(1)   TO  SGTS0083-TX-CMCP
           MOVE  SGTS0300-TX-GR-MCP(1) TO  SGTS0083-TX-GR-MCP

           MOVE  SPACES                TO  SGTS0083-TX-LVRO-MC-CPTL
           MOVE  SPACES                TO  SGTS0083-TX-SMCP

           PERFORM  VARYING IC-0300  FROM 1 BY 1
                    UNTIL   IC-0300  GREATER  SGTS0300-QT-OCR-TAB
            IF  SGTS0300-CD-LMCP(IC-0300) EQUAL SGTS0083-CD-LVRO-MC-CPTL
                MOVE  SGTS0300-TX-LMCP(IC-0300)
                                       TO SGTS0083-TX-LVRO-MC-CPTL
                IF  SGTS0083-TX-SMCP  NOT EQUAL  SPACES
                    MOVE  SGTS0300-QT-OCR-TAB
                                       TO  IC-0300
                END-IF
            END-IF
            IF  SGTS0300-CD-SMCP(IC-0300) EQUAL SGTS0083-CD-SMCP
                MOVE  SGTS0300-TX-SMCP(IC-0300)
                                       TO SGTS0083-TX-SMCP
                IF  SGTS0083-TX-LVRO-MC-CPTL  NOT EQUAL  SPACES
                    MOVE  SGTS0300-QT-OCR-TAB
                                       TO  IC-0300
                END-IF
            END-IF
           END-PERFORM

           .
       370000-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *  Section da Faixa de 900000 à 969900 para Demais Rotinas
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       900000-EXECUTAR-MODULO          SECTION.
      *--> Executa um modulo implementado com CONTAINER
      *    Nao incluir nessa section os modulos chamados via COMMAREA
      *-----------------------------------------------------------------
           MOVE  0039                  TO  MSG1-CD-SEQL-ERRO

      *--> Grava o container de requisicao do modulo a executar
           PERFORM  982000-GRAVAR-CONTAINER-RQSC

      *--> Limpa a resposta e erro antes de executar o modulo
           PERFORM  983000-EXCLUIR-RESPOSTA-ERRO

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EVALUATE  NM-PGM
               WHEN  SGTSL506
                     EXEC CICS  LINK
                                PROGRAM ( SGTSL506   )
                                CHANNEL ( NM-CNL     )
                                RESP    ( MSG1-RESP1 )
                                RESP2   ( MSG1-RESP2 )
                     END-EXEC

               WHEN  SGTSC069
                     EXEC CICS  LINK
                                PROGRAM ( SGTSC069   )
                                CHANNEL ( NM-CNL     )
                                RESP    ( MSG1-RESP1 )
                                RESP2   ( MSG1-RESP2 )
                     END-EXEC

               WHEN  SGTS0085
                     EXEC CICS  LINK
                                PROGRAM ( SGTS0085   )
                                CHANNEL ( NM-CNL     )
                                RESP    ( MSG1-RESP1 )
                                RESP2   ( MSG1-RESP2 )
                     END-EXEC

               WHEN  SGTSL094
                     EXEC CICS  LINK
                                PROGRAM ( SGTSL094   )
                                CHANNEL ( NM-CNL     )
                                RESP    ( MSG1-RESP1 )
                                RESP2   ( MSG1-RESP2 )
                     END-EXEC

               WHEN  SGTSL083
                     EXEC CICS  LINK
                                PROGRAM ( SGTSL083   )
                                CHANNEL ( NM-CNL     )
                                RESP    ( MSG1-RESP1 )
                                RESP2   ( MSG1-RESP2 )
                     END-EXEC

               WHEN  SGTS0084
                     EXEC CICS  LINK
                                PROGRAM ( SGTS0084   )
                                CHANNEL ( NM-CNL     )
                                RESP    ( MSG1-RESP1 )
                                RESP2   ( MSG1-RESP2 )
                     END-EXEC

               WHEN  SGTSPE04
                     EXEC CICS  LINK
                                PROGRAM ( SGTSPE04   )
                                CHANNEL ( NM-CNL     )
                                RESP    ( MSG1-RESP1 )
                                RESP2   ( MSG1-RESP2 )
                     END-EXEC

               WHEN  SGTS0300
                     EXEC CICS  LINK
                                PROGRAM ( SGTS0300   )
                                CHANNEL ( NM-CNL     )
                                RESP    ( MSG1-RESP1 )
                                RESP2   ( MSG1-RESP2 )
                     END-EXEC

               WHEN  OTHER
                     STRING
                         'Não definida a execução do módulo ' NM-PGM
                         DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
                     END-STRING
                     PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

           EVALUATE  TRUE
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(NORMAL)
                     CONTINUE
               WHEN  OTHER
                     STRING  'Erro na execução do módulo ' NM-PGM
                             ', RESP  = '  MSG1-RESP1
                             ', RESP2 = '  MSG1-RESP2
                             DELIMITED BY SIZE
                             INTO          MSG1-TX-ERRO-TS
                     END-STRING
                     MOVE  3004        TO  MSG1-CD-ERRO-TS
                     SET   ERRO-CICS   TO  TRUE
                     PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *--> Efetua a leitura do Container de Erro
           PERFORM  972000-LER-CONTAINER-DE-ERRO

      *--> Efetua a leitura do Container de Resposta
           PERFORM  973000-LER-RESPOSTA
           .
       900000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       910000-CHAMAR-SERVICO           SECTION.
      *--> Acionar programa IIBP2000 para requisição do serviço.
      *-----------------------------------------------------------------
           MOVE  0040                  TO  MSG1-CD-SEQL-ERRO

      *--> Grava o container com dados do serviço/operação a chamar
           PERFORM  981000-GRAVAR-CONTAINER-SRVC

      *--> Grava o container de requisicao do modulo a executar
           PERFORM  982000-GRAVAR-CONTAINER-RQSC

      *--> Limpa a resposta e erro antes de executar o modulo
           PERFORM  983000-EXCLUIR-RESPOSTA-ERRO

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS  LINK
                      PROGRAM ( IIBP2000   )
                      CHANNEL ( NM-CNL     )
                      RESP    ( MSG1-RESP1 )
                      RESP2   ( MSG1-RESP2 )
           END-EXEC

           EVALUATE TRUE
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(NORMAL)
                     CONTINUE
               WHEN  OTHER
                     MOVE  9000        TO  MSG1-CD-ERRO-TS
                     STRING  'Erro ao acionar serviço. '
                             SRVC-ID-SRVC '.' SRVC-VRS-SRVC '.'
                             SRVC-ID-OPR  '.' SRVC-VRS-OPR
                             'via IIBP2000, ' 'RESP=' MSG1-RESP1
                             ', RESP-2='   MSG1-RESP2    '.'
                             DELIMITED BY SIZE
                             INTO          MSG1-TX-ERRO-TS
                     END-STRING
                     SET   ERRO-CICS   TO  TRUE
                     PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *--> Efetua a leitura do Container de Erro
           PERFORM  972000-LER-CONTAINER-DE-ERRO


      *--> Efetua a leitura do Container de Resposta
           PERFORM  973000-LER-RESPOSTA

           .
       910000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       969000-MONITORAR-VARIAVEIS      SECTION.
      *--> Monitora as variáveis dos módulos chamados
      *-----------------------------------------------------------------

           EVALUATE  NM-PGM
      *------> Monitora as Variaveis de Entrada do SGTSL506
               WHEN  SGTSL506
                     MOVE 'L506-001-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL506-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

      *------------> Verifica se Codigo da Operacao é Numerico
                     MOVE 'L506-002-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                     IF  SGTSL506-CD-OPR-ETRD-MCP
                                           NUMERIC
                         MOVE  SGTSL506-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD
                         MOVE  CD-OPR-ETRD-TX
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

      *------> Monitora as Variaveis de Entrada do SGTSX005
               WHEN  SGTSX005
                     MOVE 'X005-001-ACAO'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSX005-ACAO
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'X005-002-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSX005-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

      *------------> Verifica se Codigo da Operacao é Numerico
                     MOVE 'X005-003-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                     IF  SGTSX005-CD-OPR-ETRD-MCP
                                           NUMERIC
                         MOVE  SGTSX005-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD
                         MOVE  CD-OPR-ETRD-TX
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'X005-004-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSX005-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

      *------> Monitora as Variaveis de Entrada do SGTST009
               WHEN  SGTST009
                     MOVE 'T009-001-ACAO'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST009-ACAO
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'T009-002-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST009-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

      *------------> Verifica se Codigo da Operacao é Numerico
                     MOVE 'T009-003-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                     IF  SGTST009-CD-OPR-ETRD-MCP
                                           NUMERIC
                         MOVE  SGTST009-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD
                         MOVE  CD-OPR-ETRD-TX
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'T009-004-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST009-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

      *------> Monitora as Variaveis de Entrada do SGTST009
               WHEN  SGTSC069
                     MOVE 'C069-001-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSC069-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

      *------------> Verifica se Codigo da Operacao é Numerico
                     MOVE 'C069-002-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                     IF  SGTSC069-CD-OPR-ETRD-MCP
                                           NUMERIC
                         MOVE  SGTSC069-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD
                         MOVE  CD-OPR-ETRD-TX
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'C069-003-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSC069-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'C069-004-TS-EST-VRS'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSC069-TS-EST-VRS
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

      *------> Monitora as Variaveis de Entrada do SGTST002
               WHEN  SGTST002
                     MOVE 'T002-001-ACAO'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST002-ACAO
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'T002-002-CD-TIT-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST002-CD-TIT-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'T002-003-NR-DVS-TIT'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST002-NR-DVS-TIT
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

      *------> Monitora as Variaveis de Entrada do SGTS0085
               WHEN  SGTS0085
                     MOVE '0085-001-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0085-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

      *------------> Verifica se Codigo da Operacao é Numerico
                     MOVE '0085-002-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                     IF  SGTS0085-CD-OPR-ETRD-MCP
                                           NUMERIC
                         MOVE  SGTS0085-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD
                         MOVE  CD-OPR-ETRD-TX
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE '0085-003-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0085-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

      *------> Monitora as Variaveis de Entrada do
               WHEN  SGTSL094
                     MOVE 'L094-001-E-CD-TIP-SRVC'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL094-E-CD-TIP-SRVC
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L094-002-E-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL094-E-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L094-003-E-CD-TIT-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL094-E-CD-TIT-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L094-004-E-NR-DVS-TIT'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL094-E-NR-DVS-TIT
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L094-005-QT-OCR-TAB'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL094-QT-OCR-TAB
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L094-006-IC-TAB'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL094-IC-TAB
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

      *------> Monitora as Variaveis de Entrada do SGTSL083
               WHEN  SGTSL083
                     MOVE 'L083-001-E-CD-TIP-SRVC'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL083-E-CD-TIP-SRVC
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L083-002-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL083-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

      *------------> Verifica se Codigo da Operacao é Numerico
                     MOVE 'L083-003-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                     IF  SGTSL083-CD-OPR-ETRD-MCP
                                           NUMERIC
                         MOVE  SGTSL083-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD
                         MOVE  CD-OPR-ETRD-TX
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L083-004-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL083-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L083-005-QT-OCR-TAB'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL083-QT-OCR-TAB
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'L083-006-IC-TAB'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL083-IC-TAB
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

      *------> Monitora as Variaveis de Entrada do SGTS0084
               WHEN  SGTS0084
                     MOVE '0084-001-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0084-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

      *------------> Verifica se Codigo da Operacao é Numerico
                     MOVE '0084-002-CD-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     IF  SGTS0084-CD-OPR-ETRD
                                           NUMERIC
                         MOVE  SGTS0084-CD-OPR-ETRD
                                       TO  CD-OPR-ETRD
                         MOVE  CD-OPR-ETRD-TX
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                         PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE '0084-003-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0084-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

      *------> Monitora as Variaveis de Entrada do SGTSPE04
               WHEN  SGTSPE04
                     MOVE 'PE04-001-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSPE04-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'PE04-002-CD-TIT-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSPE04-CD-TIT-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'PE04-003-NR-DVS-TIT'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSPE04-NR-DVS-TIT
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

      *------> Monitora as Variaveis de Entrada do SBDATAF
               WHEN  SBDATAF
                     MOVE 'SBDATAF-001-FUNCAO'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  FUNCAO      TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SBDATAF-002-ARG01'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  ARG01       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SBDATAF-003-ARG02'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  ARG02       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SBDATAF-004-ARG03'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  ARG03       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSX005-DT-EMS-TIT-OPR'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSX005-DT-EMS-TIT-OPR
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSX005-DT-VNCT-OPR'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSX005-DT-VNCT-OPR
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

      *------> Monitora as Variaveis de Entrada do CAFS0007
               WHEN  CAFS0007
                     MOVE 'CAFS0007-001-PARM-BANCO-INF'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  PARM-BANCO-INF
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'CAFS0007-002-PARM-AGE-INF'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  PARM-AGE-INF
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

      *------> Monitora as Variaveis de Entrada do CAFS0053
               WHEN  CAFS0053
                     MOVE 'CAFS0053-001-FUC'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  S053-FUC    TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'CAFS0053-002-CD-CPSO'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  S053-CD-CPSO OF S053-ENTD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

      *------> Monitora as Variaveis de Entrada do SGTST027
               WHEN  SGTST027
                     MOVE 'SGTST027-ACAO'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST027-ACAO
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTST027-CD-TIP-COTA-TIT'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST027-CD-TIP-COTA-TIT
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTST027-NR-VRS-TIP-COTA'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST027-NR-VRS-TIP-COTA
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

      *------> Monitora as Variaveis de Entrada do SGTST029
               WHEN  SGTST029
                     MOVE 'SGTST029-ACAO'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST029-ACAO
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTST029-CD-CTGR-TVM'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST029-CD-CTGR-TVM
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

      *------> Monitora as Variaveis de Entrada do SGTST030
               WHEN  SGTST030
                     MOVE 'SGTST030-ACAO'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST030-ACAO
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTST030-CD-ITC-CVM-OPR'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST030-CD-ITC-CVM-OPR
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

      *------> Monitora as Variaveis de Entrada do ATCS6000
               WHEN  ATCS6000
                     MOVE 'K6000-CD-FUC'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  K6000-CD-FUC
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'K6000-CD-USU'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  K6000-CD-USU
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

      *------> Monitora as Variaveis de Entrada do ICLSI046
               WHEN  ICLSI046
                     MOVE 'I046-CD-CLRG'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  I046-CD-CLRG
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

      *------> Monitora as Variaveis de Entrada do MSTS8042
               WHEN  MSTS8042
                     MOVE 'MSTS8042-CD-UOR'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  MSTS8042-CD-UOR
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

      *------> Monitora as Variaveis de Entrada do CACS153C
               WHEN  CACS153C
                     MOVE 'CACS153C-CD-TIP-GRT '
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  CACS153C-CD-TIP-GRT
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

      *------> Monitora as Variaveis de Entrada do SGTS0300
               WHEN  SGTS0300
                     MOVE 'SGTS0300-CD-PRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0300-CD-PRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTS0300-CD-MDLD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0300-CD-MDLD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTS0300-CD-CMCP-ENTD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0300-CD-CMCP-ENTD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTS0300-CD-GR-MCP-ENTD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0300-CD-GR-MCP-ENTD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTS0300-QT-OCR-TAB'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0300-QT-OCR-TAB
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'IC-0300'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  IC-0300
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
           MOVE  0041                  TO  MSG1-CD-SEQL-ERRO

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
           MOVE  0042                  TO  MSG1-CD-SEQL-ERRO

      *--> Monta o nome do container de requisicao
           MOVE  'SGTS0083'            TO  NM-PGM
           MOVE  'SGTS0083-RQSC'       TO  NM-CTNR

      *--> Recupera o tamanho do container de requisição
           PERFORM  973100-LER-CONTAINER-NODATA

           MOVE  0043                  TO  MSG1-CD-SEQL-ERRO

      *--> Obtem o tamanho esperado da requisicao
           MOVE  LENGTH OF SGTS0083-RQSC
                                       TO  QT-CCT-EPRO

      *--> Valida tamanho esperado da requisicao X tamanho recebido
           IF  QT-CCT-LIDO                 NOT EQUAL  QT-CCT-EPRO
               SET   ERRO-TAMANHO-AREA
                                       TO  TRUE
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
                      INTO      ( SGTS0083-RQSC )
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
           MOVE  0044                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      IIBKERRO-PRM-ERRO

      *--> Le o container de erro dos modulos chamados
           SET   GET-CONTAINER         TO  TRUE
           MOVE  'PRM-ERRO'            TO  NM-CTNR

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
           EVALUATE TRUE
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(CONTAINERERR)
                     CONTINUE
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(NORMAL)
                     SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
                     MOVE  NM-PGM      TO  MSG1-NM-PGM-ACND

                     IF  NM-PGM(1:3)       EQUAL  'SGT'
                         MOVE  IIBKERRO-CD-ERRO
                                       TO  MSG1-CD-RTN-PGM-ACND
                                           MSG1-CD-ERRO-TS
                         MOVE  IIBKERRO-TX-ERRO
                                       TO  MSG1-TX-RTN-PGM-ACND
                                           MSG1-TX-ERRO-TS
                     ELSE
                         MOVE  3004    TO  MSG1-CD-ERRO-TS
                         STRING  'Erro ao acionar o módulo '
                                 NM-PGM '.'
                                 DELIMITED BY SIZE
                                 INTO      MSG1-TX-ERRO-TS
                         MOVE  IIBKERRO-TX-ERRO
                                       TO  MSG1-TX-RTN-PGM-ACND
                         MOVE  IIBKERRO-CD-ERRO
                                       TO  MSG1-CD-RTN-PGM-ACND
                     END-IF
      *------------> Monitorar variáveis do módulo chamado
                     PERFORM  969000-MONITORAR-VARIAVEIS
                     PERFORM  999000-GRAVA-ERRO-E-ENCERRA

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
      *--> Recupera o container com os dados de resposta (saída).
      *    Modulo CONTAINER  -     NOME_PGM-RPST
      *    Modulo IIB (FIXO) -     PRM-DADOS-RPST
      *-----------------------------------------------------------------
           MOVE  0045                  TO  MSG1-CD-SEQL-ERRO

      *--> Monta o nome dos container de acordo com modulo a executar
           MOVE  SPACES                TO  NM-CTNR

      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RPST,
      *    senão nome_modulo -RPST
           EVALUATE  NM-PGM
               WHEN  SGTSL506
                     MOVE  'SGTSL506-RPST'
                                       TO  NM-CTNR
               WHEN  SGTSC069
                     MOVE  'SGTSC069-RPST'
                                       TO  NM-CTNR
               WHEN  SGTS0085
                     MOVE  'SGTS0085-RPST'
                                       TO  NM-CTNR
               WHEN  SGTSL094
                     MOVE  'SGTSL094-RPST'
                                       TO  NM-CTNR
               WHEN  SGTSL083
                     MOVE  'SGTSL083-RPST'
                                       TO  NM-CTNR
               WHEN  SGTS0084
                     MOVE  'SGTS0084-RPST'
                                       TO  NM-CTNR
               WHEN  SGTSPE04
                     MOVE  'SGTSPE04-RPST'
                                       TO  NM-CTNR
               WHEN  CAFS0007
                     MOVE  'PRM-DADOS-RPST'
                                       TO  NM-CTNR
               WHEN  CAFS0053
                     MOVE  'PRM-DADOS-RPST'
                                       TO  NM-CTNR
               WHEN  ATCS6000
                     MOVE  'PRM-DADOS-RPST'
                                       TO  NM-CTNR
               WHEN  ICLSI046
                     MOVE  'PRM-DADOS-RPST'
                                       TO  NM-CTNR
               WHEN  MSTS8042
                     MOVE  'PRM-DADOS-RPST'
                                       TO  NM-CTNR
               WHEN  SGTS0300
                     MOVE  'SGTS0300-RPST'
                                       TO  NM-CTNR
               WHEN  OTHER
                     MOVE  NM-PGM
                                       TO  NM-CTNR(1:8)
                     MOVE  '-RPST'
                                       TO  NM-CTNR(9:5)
           END-EVALUATE

      *--> Recupera o tamanho do container de resposta
           PERFORM  973100-LER-CONTAINER-NODATA

           IF  NM-PGM                      EQUAL  SGTSL506  OR  SGTSC069
                                              OR  SGTS0085  OR  SGTS0084
                                              OR  SGTSPE04  OR  MSTS8042
               EVALUATE NM-PGM
                 WHEN  SGTSL506
                       MOVE  LENGTH OF SGTSL506-RPST
                                       TO  QT-CCT-EPRO
                 WHEN  SGTSC069
                       MOVE  LENGTH OF SGTSC069-RPST
                                       TO  QT-CCT-EPRO
                 WHEN  SGTS0085
                       MOVE  LENGTH OF SGTS0085-RPST
                                       TO  QT-CCT-EPRO
                 WHEN  SGTS0084
                       MOVE  LENGTH OF SGTS0084-RPST
                                       TO  QT-CCT-EPRO
                 WHEN  SGTSPE04
                       MOVE  LENGTH OF SGTSPE04-RPST
                                       TO  QT-CCT-EPRO
                 WHEN  MSTS8042
                       MOVE  LENGTH OF MSTS8042-RPST
                                       TO  QT-CCT-EPRO
               END-EVALUATE

               IF  QT-CCT-LIDO             NOT EQUAL  QT-CCT-EPRO
                   SET   ERRO-TAMANHO-AREA
                                       TO  TRUE
                   PERFORM  999000-GRAVA-ERRO-E-ENCERRA
               END-IF
            END-IF

      *--> Configura dados para leitura do container de resposta
           SET   GET-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE


      *--> Le a RESPOSTA de acordo com o programa que a gravou
           EVALUATE  NM-PGM
               WHEN  SGTSL506
                     INITIALIZE            SGTSL506-RPST
                     EXEC CICS  GET
                                CONTAINER ( NM-CTNR       )
                                INTO      ( SGTSL506-RPST )
                                FLENGTH   ( QT-CCT-LIDO   )
                     END-EXEC

               WHEN  SGTSC069
                     INITIALIZE            SGTSC069-RPST
                     EXEC CICS  GET
                                CONTAINER ( NM-CTNR       )
                                INTO      ( SGTSC069-RPST )
                                FLENGTH   ( QT-CCT-LIDO   )
                     END-EXEC

               WHEN  SGTS0085
                     INITIALIZE            SGTS0085-RPST
                     EXEC CICS  GET
                                CONTAINER ( NM-CTNR       )
                                INTO      ( SGTS0085-RPST )
                                FLENGTH   ( QT-CCT-LIDO   )
                     END-EXEC

               WHEN  SGTSL094
                     EXEC CICS  GET
                                CONTAINER ( NM-CTNR       )
                                INTO      ( SGTSL094-RPST )
                                FLENGTH   ( QT-CCT-LIDO   )
                     END-EXEC

               WHEN  SGTSL083
                     EXEC CICS  GET
                                CONTAINER ( NM-CTNR       )
                                INTO      ( SGTSL083-RPST )
                                FLENGTH   ( QT-CCT-LIDO   )
                     END-EXEC

               WHEN  SGTS0084
                     INITIALIZE            SGTS0084-RPST
                     EXEC CICS  GET
                                CONTAINER ( NM-CTNR       )
                                INTO      ( SGTS0084-RPST )
                                FLENGTH   ( QT-CCT-LIDO   )
                     END-EXEC
               WHEN  SGTSPE04
                     INITIALIZE            SGTSPE04-RPST
                     EXEC CICS  GET
                                CONTAINER ( NM-CTNR       )
                                INTO      ( SGTSPE04-RPST )
                                FLENGTH   ( QT-CCT-LIDO   )
                     END-EXEC

               WHEN  CAFS0007
                     INITIALIZE            CAFS0007-DADOS
                     EXEC CICS  GET
                                CONTAINER ( NM-CTNR        )
                                INTO      ( CAFS0007-DADOS )
                                FLENGTH   ( QT-CCT-LIDO    )
                     END-EXEC
               WHEN  CAFS0053
                     INITIALIZE            CAFS0053-DADOS
                     EXEC CICS  GET
                                CONTAINER ( NM-CTNR        )
                                INTO      ( CAFS0053-DADOS )
                                FLENGTH   ( QT-CCT-LIDO    )
                     END-EXEC

               WHEN  ATCS6000
                     INITIALIZE            ATCS6000-DADOS
                     EXEC CICS  GET
                                CONTAINER ( NM-CTNR        )
                                INTO      ( ATCS6000-DADOS )
                                FLENGTH   ( QT-CCT-LIDO    )
                     END-EXEC
               WHEN  ICLSI046
                     EXEC CICS  GET
                                CONTAINER ( NM-CTNR       )
                                INTO      ( ICLSI046-RPST )
                                FLENGTH   ( QT-CCT-LIDO   )
                     END-EXEC
               WHEN  MSTS8042
                     INITIALIZE            MSTS8042-RPST
                     EXEC CICS  GET
                                CONTAINER ( NM-CTNR       )
                                INTO      ( MSTS8042-RPST )
                                FLENGTH   ( QT-CCT-LIDO   )
                     END-EXEC
               WHEN  SGTS0300
                     EXEC CICS  GET
                                CONTAINER ( NM-CTNR       )
                                INTO      ( SGTS0300-RPST )
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
                     PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       973000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       973100-LER-CONTAINER-NODATA     SECTION.
      *-----------------------------------------------------------------
           MOVE  0046                  TO  MSG1-CD-SEQL-ERRO

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
           MOVE  0047                  TO  MSG1-CD-SEQL-ERRO

      *--> Exclui eventual container de erro de outros programas
           PERFORM  983100-EXCLUIR-CONTAINER-ERRO

      *--> Monta o nome do container de resposta
           MOVE  'SGTS0083-RPST'       TO  NM-CTNR
           SET   PUT-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

           EXEC CICS  PUT
                      CONTAINER ( NM-CTNR                 )
                      FROM      ( SGTS0083-RPST           )
                      FLENGTH   ( LENGTH OF SGTS0083-RPST )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       980000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       981000-GRAVAR-CONTAINER-SRVC    SECTION.
      *--> Postar container de requisição de serviço.
      *-----------------------------------------------------------------
           MOVE  0048                  TO  MSG1-CD-SEQL-ERRO

      *--> Monta o nome do container de requisicao do servico
           MOVE  'PRM-RQSC-SRVC'       TO  NM-CTNR

           SET   PUT-CONTAINER         TO  TRUE

           EVALUATE  NM-PGM
               WHEN  CAFS0007
                     MOVE  00150834    TO  SRVC-ID-SRVC
                     MOVE  0001        TO  SRVC-VRS-SRVC
                     MOVE  01062294    TO  SRVC-ID-OPR
                     MOVE  0001        TO  SRVC-VRS-OPR
               WHEN  CAFS0053
                     MOVE  00150834    TO  SRVC-ID-SRVC
                     MOVE  0001        TO  SRVC-VRS-SRVC
                     MOVE  01092761    TO  SRVC-ID-OPR
                     MOVE  0001        TO  SRVC-VRS-OPR

               WHEN  ATCS6000
                     MOVE  00071739    TO  SRVC-ID-SRVC
                     MOVE  0001        TO  SRVC-VRS-SRVC
                     MOVE  00292018    TO  SRVC-ID-OPR
                     MOVE  0001        TO  SRVC-VRS-OPR
               WHEN  ICLSI046
                     MOVE  00058340    TO  SRVC-ID-SRVC
                     MOVE  0001        TO  SRVC-VRS-SRVC
                     MOVE  01824215    TO  SRVC-ID-OPR
                     MOVE  0001        TO  SRVC-VRS-OPR
               WHEN  MSTS8042
                     MOVE  ZEROS       TO  SRVC-ID-SRVC
                     MOVE  ZEROS       TO  SRVC-VRS-SRVC
                     MOVE  01998646    TO  SRVC-ID-OPR
                     MOVE  0001        TO  SRVC-VRS-OPR
           END-EVALUATE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

           EXEC CICS  PUT
                      CONTAINER ( NM-CTNR                      )
                      FROM      ( SRVC-PRM-RQSC-SRVC           )
                      FLENGTH   ( LENGTH OF SRVC-PRM-RQSC-SRVC )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       981000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       982000-GRAVAR-CONTAINER-RQSC    SECTION.
      *--> Postar container com dados de requisição (entrada)
      *    Modulo CONTAINER  -     NOME_PGM-RQSC
      *    Modulo IIB (FIXO) -     PRM-DADOS-RQSC
      *-----------------------------------------------------------------
           MOVE  0049                  TO  MSG1-CD-SEQL-ERRO

      *--> Monta o nome dos container de acordo com modulo a executar
           MOVE  SPACES                TO  NM-CTNR
           SET   PUT-CONTAINER         TO  TRUE

      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RQSC,
      *    senão nome_modulo -RQSC
           EVALUATE  NM-PGM
               WHEN  SGTSL506
                     MOVE  'SGTSL506-RQSC'
                                       TO  NM-CTNR
               WHEN  SGTSC069
                     MOVE  'SGTSC069-RQSC'
                                       TO  NM-CTNR
               WHEN  SGTS0085
                     MOVE  'SGTS0085-RQSC'
                                       TO  NM-CTNR
               WHEN  SGTSL094
                     MOVE  'SGTSL094-RQSC'
                                       TO  NM-CTNR
               WHEN  SGTSL083
                     MOVE  'SGTSL083-RQSC'
                                       TO  NM-CTNR
               WHEN  SGTS0084
                     MOVE  'SGTS0084-RQSC'
                                       TO  NM-CTNR
               WHEN  SGTSPE04
                     MOVE  'SGTSPE04-RQSC'
                                       TO  NM-CTNR

               WHEN  CAFS0007
                     MOVE  'PRM-DADOS-RQSC'
                                       TO  NM-CTNR
               WHEN  CAFS0053
                     MOVE  'PRM-DADOS-RQSC'
                                       TO  NM-CTNR

               WHEN  ATCS6000
                     MOVE  'PRM-DADOS-RQSC'
                                       TO  NM-CTNR
               WHEN  ICLSI046
                     MOVE  'PRM-DADOS-RQSC'
                                       TO  NM-CTNR
               WHEN  MSTS8042
                     MOVE  'PRM-DADOS-RQSC'
                                       TO  NM-CTNR
               WHEN  SGTS0300
                     MOVE  'SGTS0300-RQSC'
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
               WHEN  SGTSL506
                     EXEC CICS  PUT
                                CONTAINER ( NM-CTNR                 )
                                FROM      ( SGTSL506-RQSC           )
                                FLENGTH   ( LENGTH OF SGTSL506-RQSC )
                     END-EXEC
               WHEN  SGTSC069
                     EXEC CICS  PUT
                                CONTAINER ( NM-CTNR                 )
                                FROM      ( SGTSC069-RQSC           )
                                FLENGTH   ( LENGTH OF SGTSC069-RQSC )
                     END-EXEC
               WHEN  SGTS0085
                     EXEC CICS  PUT
                                CONTAINER ( NM-CTNR                 )
                                FROM      ( SGTS0085-RQSC           )
                                FLENGTH   ( LENGTH OF SGTS0085-RQSC )
                     END-EXEC
               WHEN  SGTSL094
                     EXEC CICS  PUT
                                CONTAINER ( NM-CTNR                 )
                                FROM      ( SGTSL094-RQSC           )
                                FLENGTH   ( LENGTH OF SGTSL094-RQSC )
                     END-EXEC
               WHEN  SGTSL083
                     EXEC CICS  PUT
                                CONTAINER ( NM-CTNR                 )
                                FROM      ( SGTSL083-RQSC           )
                                FLENGTH   ( LENGTH OF SGTSL083-RQSC )
                     END-EXEC
               WHEN  SGTS0084
                     EXEC CICS  PUT
                                CONTAINER ( NM-CTNR                 )
                                FROM      ( SGTS0084-RQSC           )
                                FLENGTH   ( LENGTH OF SGTS0084-RQSC )
                     END-EXEC
               WHEN  SGTSPE04
                     EXEC CICS  PUT
                                CONTAINER ( NM-CTNR                 )
                                FROM      ( SGTSPE04-RQSC           )
                                FLENGTH   ( LENGTH OF SGTSPE04-RQSC )
                     END-EXEC

               WHEN  CAFS0007
                     EXEC CICS  PUT
                                CONTAINER ( NM-CTNR                  )
                                FROM      ( CAFS0007-DADOS           )
                                FLENGTH   ( LENGTH OF CAFS0007-DADOS )
                     END-EXEC
               WHEN  CAFS0053
                     EXEC CICS  PUT
                                CONTAINER (NM-CTNR                  )
                                FROM      (CAFS0053-DADOS           )
                                FLENGTH   (LENGTH OF CAFS0053-DADOS )
                     END-EXEC

               WHEN  ATCS6000
                     EXEC CICS  PUT
                                CONTAINER ( NM-CTNR                  )
                                FROM      ( ATCS6000-DADOS           )
                                FLENGTH   ( LENGTH OF ATCS6000-DADOS )
                     END-EXEC
               WHEN  ICLSI046
                     EXEC CICS  PUT
                                CONTAINER ( NM-CTNR                  )
                                FROM      ( ICLSI046-DADOS           )
                                FLENGTH   ( LENGTH OF ICLSI046-DADOS )
                     END-EXEC
               WHEN  MSTS8042
                     EXEC CICS  PUT
                                CONTAINER ( NM-CTNR                 )
                                FROM      ( MSTS8042-RQSC           )
                                FLENGTH   ( LENGTH OF MSTS8042-RQSC )
                     END-EXEC
               WHEN  SGTS0300
                     EXEC CICS  PUT
                                CONTAINER ( NM-CTNR                 )
                                FROM      ( SGTS0300-RQSC           )
                                FLENGTH   ( LENGTH OF SGTS0300-RQSC )
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
                     PERFORM  999000-GRAVA-ERRO-E-ENCERRA
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
           MOVE  0050                  TO  MSG1-CD-SEQL-ERRO

      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RPST,
      *    senão nome_modulo -RPST
           EVALUATE  NM-PGM
               WHEN  SGTSL506
                     MOVE  'SGTSL506-RPST'
                                       TO  NM-CTNR
               WHEN  SGTSC069
                     MOVE  'SGTSC069-RPST'
                                       TO  NM-CTNR
               WHEN  SGTS0085
                     MOVE  'SGTS0085-RPST'
                                       TO  NM-CTNR
               WHEN  SGTSL094
                     MOVE  'SGTSL094-RPST'
                                       TO  NM-CTNR
               WHEN  SGTSL083
                     MOVE  'SGTSL083-RPST'
                                       TO  NM-CTNR
               WHEN  SGTS0084
                     MOVE  'SGTS0084-RPST'
                                       TO  NM-CTNR
               WHEN  SGTSPE04
                     MOVE  'SGTSPE04-RPST'
                                       TO  NM-CTNR
               WHEN  CAFS0007
                     MOVE  'PRM-DADOS-RPST'
                                       TO  NM-CTNR
               WHEN  CAFS0053
                     MOVE  'PRM-DADOS-RPST'
                                       TO  NM-CTNR
               WHEN  ATCS6000
                     MOVE  'PRM-DADOS-RPST'
                                       TO  NM-CTNR
               WHEN  ICLSI046
                     MOVE  'PRM-DADOS-RPST'
                                       TO  NM-CTNR
               WHEN  MSTS8042
                     MOVE  'PRM-DADOS-RPST'
                                       TO  NM-CTNR
               WHEN  SGTS0300
                     MOVE  'SGTS0300-RPST'
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
           MOVE  0051                  TO  MSG1-CD-SEQL-ERRO

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
      * As sections abaixo iniciadas com 999000 representam os tratamen
      * tos de erro utilizando o Sistema MSG (rotina MSGS0001)
      *-----------------------------------------------------------------

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
           MOVE '0083-001-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0083-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  '0083-002-CD-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
      *--> Verifica se o conteudo e numerico antes de adicionar
           IF  SGTS0083-CD-OPR-ETRD        NUMERIC
               MOVE  SGTS0083-CD-OPR-ETRD
                                       TO  CD-OPR-ETRD
               MOVE  CD-OPR-ETRD-TX
                                       TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE '0083-003-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0083-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT


      *--> Grava a Ocorrencia de Erro no sistema MSG - Mensageria
           PERFORM  MSG1-NTF-MSG

           MOVE  MSG1-CD-ERRO-TS       TO  IIBKERRO-CD-ERRO
           MOVE  MSG1-TX-ERRO-TS       TO  IIBKERRO-TX-ERRO

      *--> Grava a Ocorrencia de Erro no Container de Erro
           EXEC CICS  PUT
                      CONTAINER ( 'PRM-ERRO'                  )
                      FROM      ( IIBKERRO-PRM-ERRO           )
                      FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                      RESP      ( MSG1-RESP1                  )
                      RESP2     ( MSG1-RESP2                  )
           END-EXEC

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
       COPY HLPKP999.
      *----------------------------------------
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *----------------------------------------
       COPY SGTKP999.
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
       999999-ENCERRAR                 SECTION.
      *-----------------------------------------------------------------

           EXEC CICS
                RETURN
           END-EXEC
           .
       999999-SAI.
           EXIT.

      *---------------------- FIM SGTS0083 -----------------------------
