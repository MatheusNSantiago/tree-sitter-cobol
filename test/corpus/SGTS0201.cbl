      *-----------------------------------------------------------------
      * PROGRAMA..: SGTS0201
      * ANALISTA..: F2054109 - Cintia Aparecida Soares Iglesias
      * AUTOR.....: F2054109 - Cintia Aparecida Soares Iglesias
      * DATA......: 03/04/2018
      * OBJETIVO..: Negócio  - Efetuar as validações IMPEDITIVAS no
      *             processo de finalização de cadastramento da versão
      *             da operação
      * COMPILACAO: 54 - PSOSE600 - Cobol 6.3 c/otimizacao p/producao
      *-----------------------------------------------------------------
      * VRS007 03/07/2024 - C1334842 - Recompilação devido a Inclusão
      *                     dos campos:CD_MTV_EST_NEGL e
      *                     CD_TIT_OPR_DRIV - tabela OPR_ETRD_MCP
      * VRS006 20/02/2024 F9188847 Inclusao de validacao campos:
      *        CD-CMCP, CD-GR-MCP, CD-LVRO-MC-CPTL, CD-SMCP,
      *        CD-CLSC-ITMT-FNCO na Section 220000-VALIDA-DADOS-OPERACAO
      *        Inclusao da section 210001-VER-ESTADO-OPR-VRS
      *        E utilizar SGTSX005 em novas alteracoes ou em programas
      *        novos no lugar do SGTST005
      * VRS005 06/01/2020 F9188847 Alteração CD_TIP_FUC_CLI na Section
      *        320000-VRFR-RISCO-CLIENTE De: 1 - Emmissor
      *                                Para: 2 - Impacto Limite Crédito
      * VRS004 03/01/2020 F8090555 Inibe validacao das condicoes nego-
      *                            ciadas.
      * VRS003 16/09/2019 F8090555 Permite SGTSX005-PC-TARF-GRT-TAXA
      *                            zerado.
      * VRS002 23/08/2019 F9188847 DE 2016/145195 - Retirar consistencia
      *        CD-TIP-IN = 19 => Indicador de Ordem de Compra Atualizada
      *        10/07/2019 F2804421 DE 2016/145195 - Retirar consistencia
      *        no caso de operação versionada para:
      *        - Valor de Garantia firme  x valor da compra e
      *        - Valor de emissão x valor da compra.
      *
      *        19/06/2019 F2804421 DE 2016/145195 - Na consistência de
      *         verificação de título utilizado em outra oper.ativa, re-
      *         tirar a consistencia de número de versão, somente para
      *         os casos de consistencia do versionamento da operação
      *         opção 'Editar Versão'.
      *
      * VRS001 30/01/2019 F9188847 DE 2016/145195 - Inclusão monitoração
      *          variáveis para rotina SBDATAF
      *        16/01/2019 F2804421 - Retirar consistencias de:
      *                   - UOR Detentora- chamada do SGTS0209.
      *                   - Existência de indicador de  infra-estrurura
      *                     para Debenture.
      *        07/01/2019 F9158937 - Inclusão envio data evento
      *                              (SGTS0204-DT-EVT-VCLD-VRS)
      *        03/04/2018 F2020377  DE 2016/145195 - Implantação
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTS0201.
      *
      *-----------------------------------------------------------------
       ENVIRONMENT                     DIVISION.
      *-----------------------------------------------------------------
      *
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
                                            '*** SGTS0201 ***'.
       77  CTE-VERS                    PIC  X(06)  VALUE 'VRS007'.

      *--> Definição dos Nomes dos Programas Acionados
       01  TAB-PROGRAMAS-CHAMADOS.
           03  IIBP2000                PIC  X(08) VALUE 'IIBP2000'.
           03  ANCS0001                PIC  X(08) VALUE 'ANCS0001'.
           03  SBDATAF                 PIC  X(08) VALUE 'SBDATAF'.
           03  SGTSX005                PIC  X(08) VALUE 'SGTSX005'.
           03  SGTS0204                PIC  X(08) VALUE 'SGTS0204'.
           03  SGTS0205                PIC  X(08) VALUE 'SGTS0205'.
           03  SGTS0207                PIC  X(08) VALUE 'SGTS0207'.
           03  SGTS0208                PIC  X(08) VALUE 'SGTS0208'.
           03  SGTS0211                PIC  X(08) VALUE 'SGTS0211'.

      *--> Definição dos Variaveis do Cobolito
-INC ALMKCBL1

      *-----------------------------------------------------------------
      *                     Containers utilizados
      *-----------------------------------------------------------------

      *--> Container de erro
       77 CTNR-PRM-ERRO                PIC X(16) VALUE 'PRM-ERRO'.
      *--> Container do usuário logado
       77 CTNR-PRM-INF-OPRD            PIC X(16) VALUE 'PRM-INF-OPRD'.
      *--> Container de requisição desta subrotina
       77 CTNR-SGTS0201-RQSC           PIC X(16) VALUE 'SGTS0201-RQSC'.

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

      *--> Variaveis utilizados para leitura
       01  REG-WS.
           03 CD-USU-RSP-EST           PIC  X(08)     VALUE ' '.
           03 CD-TIP-EST-OPR           PIC S9(09)     COMP-5 VALUE 0.
           03 TX-JST-EST               PIC  X(100)    VALUE ' '.
           03 CD-MTV-ALT               PIC S9(04)     COMP-5 VALUE 0.
           03 CD-TIP-EST-VRS           PIC S9(04)     COMP-5 VALUE 0.
           03 TX-OBS-EST-VRS           PIC  X(200)    VALUE ' '.

      *--> Indicadores de nulidades
       01  CD-TIP-EST-OPR-N            PIC S9(04)     COMP-5 VALUE 0.
       01  TX-JST-EST-N                PIC S9(04)     COMP-5 VALUE 0.
       01  TX-OBS-EST-VRS-N            PIC S9(04)     COMP-5 VALUE 0.
       01  CD-MTV-ALT-N                PIC S9(04)     COMP-5 VALUE 0.

      *--> Areas de trabalho
       01  IC                          PIC S9(04) COMP-5 VALUE ZEROS.
       01  IC-INTC                     PIC S9(04) COMP-5 VALUE ZEROS.
       01  QT-REG-SEL                  PIC S9(04) COMP-5 VALUE ZEROS.
       01  K-ZERO                      PIC S9(09) COMP-5 VALUE ZEROS.

       01  QT-AA-DRCO-OPR              PIC S9(04)V9(02) COMP-3
                                                        VALUE ZEROS.
       01  VL-EMS-TIT-OPR              PIC S9(15)V9(02) COMP-3
                                                        VALUE ZEROS.
       01  SUM-PC-TARF-OPR             PIC S9(04)V9(10) COMP-3
                                                        VALUE ZEROS.

       01  CD-ERRO-CLC                 PIC  9(01) VALUE ZEROS.
           88 COM-ERRO-CLC                        VALUE 0001.
       01  IN-EXISTENCIA-REGISTRO      PIC  X(01).
           88  REGISTRO-NAO-EXISTE                VALUE  'N'.
           88  REGISTRO-EXISTE                    VALUE  'S'.
       01  IN-EXISTE-REG               PIC  X(01).
           88  REG-NAO-EXISTE                     VALUE  'N'.
           88  REG-EXISTE                         VALUE  'S'.

       01  CD-TIP-IN                   PIC S9(04) COMP-5 VALUE ZEROS.
           88  CONTINGENCIAMENTO-PUBLICO                 VALUE 0001.
           88  FLEXIBILIZACAO-EXCECAO                    VALUE 0002.
           88  INFRAESTRUTURA                            VALUE 0003.
           88  NAO-CONVERSAO-EM-ACAO                     VALUE 0004.
           88  SUPORTE-FACILITADOR                       VALUE 0005.
       01  CD-TIP-FUC-CLI              PIC S9(04) COMP-5 VALUE ZEROS.
           88  CLIENTE-EMISSOR                           VALUE 0001.
           88  CLIENTE-IMPACTO-LIMITE                    VALUE 0002.
           88  CLIENTE-ORIGINADOR-CEDENTE                VALUE 0003.
           88  CLIENTE-AGENTE-FIDUCIARIO                 VALUE 0004.
           88  CLIENTE-ADMINISTRADOR                     VALUE 0005.
       01  CD-CLI                      PIC S9(09) COMP-5 VALUE ZEROS.

      *--> Auxiliares Movimentacao Monitoração
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).
       01  VL-04V02                    PIC  --.--9,99.
       01  VL-04V10                    PIC  --.--9,9999999999.
       01  VL-15V02                    PIC  ---.---.---.---.--9,99.


      *--> Chamada à subrotina SBDATAF
       01  WKSBDATA                    PIC  X(4099).
       01  AREA-RED REDEFINES WKSBDATA.
           03  AREA-TAM                PIC  9(08) COMP.
           03  FILLER                  PIC  X(4071).
           03  AREA-FIM                PIC  X(20).

       01  FUNCAO                      PIC  X(03) VALUE 'F03'.
       01  ARG01                       PIC  9(08).
       01  ARG01-R  REDEFINES  ARG01.
         03  DIA                       PIC  9(02).
         03  MES                       PIC  9(02).
         03  ANO                       PIC  9(04).
       01  ARG02                       PIC  9(08).
       01  ARG02-R  REDEFINES  ARG02.
         03  DIA                       PIC  9(02).
         03  MES                       PIC  9(02).
         03  ANO                       PIC  9(04).
       01  ARG03                       PIC S9(05) VALUE ZEROS.

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

      *--> Book do Container de Requisicao e Resposta deste Módulo
       01 SGTS0201-DADOS.
-INC SGTK0201

      *-----------------------------------------------------------------
      * Book dos Containers e das subrotinas chamadas
      *-----------------------------------------------------------------

      *--> Consultar Operação Estruturada
      *    Persistência da Tabela OPR_ETRD_MCP
       01  SGTSX005-DADOS.
-INC SGTKX005

      *--> Validar Integridade do Fluxo de Recebimento Previsto
       01  SGTS0204-DADOS.
-INC SGTK0204

      *--> Validar Integridade da Precificação da Operação
       01  SGTS0205-DADOS.
-INC SGTK0205

      *--> Validar Integridade da Remuneração da Operação
       01  SGTS0207-DADOS.
-INC SGTK0207

      *--> Validar Integridade das Condições Pactuadas da Operação
       01  SGTS0208-DADOS.
-INC SGTK0208

      *--> Verifica se o Título está vinculado em alguma
      *    Operação Estruturada Ativa do SGT
       01  SGTS0211-DADOS.
-INC SGTK0211

      *--> Consultar Risco  do Cliente
       01  ANCS0001-DADOS.
-INC ANCB0001

      *--> Parâmetros para a utilizacao do DB2
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

       01  CTE-FINAL-SS                PIC X(40) VALUE
                                       '*** S.S. Termina Aqui ***'.

      *-----------------------------------------------------------------
       PROCEDURE                       DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------

      *--> Habilita Testes no Cobolito e Debug de Operacoes IIB
-INC ALMKCBL2

           PERFORM 100000-INICIAR

           PERFORM 200000-PROCESSAR

           PERFORM 980000-GRAVAR-CONTAINER-RPST

           PERFORM 999999-ENCERRAR

           GOBACK
           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-INICIAR                  SECTION.
      *-----------------------------------------------------------------
           MOVE 0001                   TO MSG1-CD-SEQL-ERRO

      *--> WHENEVER DB2
-INC HLPKE997

      *    Ativa a captura automática de abend do CICS.
      *    WHENEVER CICS
-INC HLPKE999

           PERFORM 970000-LER-CANAL

      *    Define o nome do programa p/ fins manipulação container
           MOVE CTE-PROG(5:8)          TO NM-PGM

      *    Limpa a resposta e erro antes de executar o módulo
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO

           PERFORM 971000-LER-REQUISICAO

           PERFORM 974000-LER-DADOS-OPRD

           PERFORM 110000-VALIDAR-REQUISICAO
           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-VALIDAR-REQUISICAO       SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO

      *--> Validar Chave da Operação Estruturada
           IF  SGTS0201-CD-CTRA-TIT-VL     LESS ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0001              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' inválido.'      TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0201-CD-CTRA-TIT-VL     EQUAL ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0002              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0201-CD-OPR-ETRD-MCP    NOT NUMERIC
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE  0003              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada mercado de capitais nã
      -              'o numérico.'     TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0201-CD-OPR-ETRD-MCP    LESS  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0004              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada do mercado de capit
      -              'ais inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0201-CD-OPR-ETRD-MCP    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0005              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada mercado de capitais nã
      -              'o informado.'    TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0201-NR-VRS-OPR-ETRD    LESS  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0006              TO  MSG1-CD-ERRO-TS
               MOVE  'Número versão operação estruturada inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0201-NR-VRS-OPR-ETRD    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0007              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação estruturada não infor
      -              'mado.'           TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       110000-SAI.
           EXIT.


      *-----------------------------------------------------------------
       200000-PROCESSAR                SECTION.
      *-----------------------------------------------------------------

           PERFORM 210000-CONSULTA-OPERACAO

           PERFORM 210001-VER-ESTADO-OPR-VRS

           PERFORM 220000-VALIDA-DADOS-OPERACAO

           PERFORM 230000-VALIDA-DADOS-MANDATO

           PERFORM 240000-VALIDA-DADOS-PRD-MDLD

           IF SGTSX005-CD-TIT-OPR-ETRD NOT EQUAL SPACES
              PERFORM 250000-VALIDA-OPR-ATI-TIT
           END-IF

           PERFORM 260000-VALIDA-RCBT-PRVT

           PERFORM 270000-VALIDA-PREF

           PERFORM 280000-VALIDA-RMNC

      *    PERFORM 290000-VALIDA-CND-NEGD

           PERFORM 300000-VALIDA-TARIFAS
      *  Não validar existencia de ordem de compra neste momento
      *     PERFORM 310000-VALIDA-ORD-COMPRA

           EVALUATE  SGTS0201-CD-CTRA-TIT-VL
               WHEN  300000
                     PERFORM 320000-VRFR-RISCO-CLIENTE
               WHEN  OTHER
                     CONTINUE
           END-EVALUATE

      **     PERFORM 330000-VALIDA-PMS-ACSS-UOR

           MOVE IC-INTC                TO SGTS0201-QT-OCR-TAB

           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       210000-CONSULTA-OPERACAO      SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                   TO MSG1-CD-SEQL-ERRO

           MOVE  SPACES                TO  SGTSX005-DADOS
           INITIALIZE                      SGTSX005-DADOS
                                           REPLACING NUMERIC  BY ZEROS

      *--> Movimento nome do módulo para monitoração de variáveis
           MOVE  SGTSX005              TO  NM-PGM

      *--> Movimentar os campos de entrada
           SET   SGTSX005-CONSULTAR    TO  TRUE
           MOVE  SGTS0201-CD-CTRA-TIT-VL
                                       TO  SGTSX005-CD-CTRA-TIT-VL
           MOVE  SGTS0201-CD-OPR-ETRD-MCP
                                       TO  SGTSX005-CD-OPR-ETRD-MCP
           MOVE  SGTS0201-NR-VRS-OPR-ETRD
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
       210000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       210001-VER-ESTADO-OPR-VRS       SECTION.
      *-----------------------------------------------------------------
           MOVE 0004                   TO  MSG1-CD-SEQL-ERRO
      *-----------------------------------------------------------------
      *--> Consulta ultimo estado da Operacao e versao para verficacao
      *--> dados da taxonomia e classificacao negocial da operacao
      *--> ao finalizar versao de uma operacao versinada
      *-----------------------------------------------------------------
           EXEC SQL
                SELECT B.CD_USU_ULT_ATL
                     , A.CD_TIP_EST_OPR
                     , A.TX_JST_EST
                     , A.CD_MTV_ALT
                     , B.CD_TIP_EST_VRS
                     , B.TX_OBS_EST_VRS
                  INTO  :CD-USU-RSP-EST
                     ,  :CD-TIP-EST-OPR
                        :CD-TIP-EST-OPR-N
                     ,  :TX-JST-EST
                        :TX-JST-EST-N
                     ,  :CD-MTV-ALT
                        :CD-MTV-ALT-N
                     ,  :CD-TIP-EST-VRS
                     ,  :TX-OBS-EST-VRS
                        :TX-OBS-EST-VRS-N
                FROM DB2SGT.EST_OPR_ETRD  A
                     JOIN DB2SGT.EST_VRS_OPR_ETRD  B
                       ON   B.CD_CTRA_TIT_VL  =  A.CD_CTRA_TIT_VL
                       AND  B.CD_OPR_ETRD_MCP =  A.CD_OPR_ETRD_MCP
                       AND  B.NR_VRS_OPR_ETRD =  A.NR_VRS_OPR_ETRD
                WHERE A.CD_CTRA_TIT_VL    = :SGTS0201-CD-CTRA-TIT-VL
                  AND A.CD_OPR_ETRD_MCP   = :SGTS0201-CD-OPR-ETRD-MCP
                  AND A.NR_VRS_OPR_ETRD   = :SGTS0201-NR-VRS-OPR-ETRD
                  AND B.TS_EST_VRS = (SELECT MAX (C.TS_EST_VRS)
                FROM DB2SGT.EST_VRS_OPR_ETRD C
                     WHERE C.CD_CTRA_TIT_VL  =  A.CD_CTRA_TIT_VL
                       AND C.CD_OPR_ETRD_MCP =  A.CD_OPR_ETRD_MCP
                       AND C.NR_VRS_OPR_ETRD =  A.NR_VRS_OPR_ETRD)
                ORDER BY A.TS_EST_OPR_ETRD DESC
                FETCH FIRST 1 ROW ONLY
           END-EXEC
           .

       210001-SAI.
           EXIT.

      *-----------------------------------------------------------------
       220000-VALIDA-DADOS-OPERACAO      SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

           IF SGTSX005-DT-EMS-TIT-OPR EQUAL SPACES OR
              '  .  .    '
              ADD  1               TO IC-INTC
              MOVE IC-INTC         TO SGTS0201-CD-IDFR-INTC(IC-INTC)
              MOVE 'Data emissão deve ser informada.' TO
                                      SGTS0201-TX-DCR-INTC(IC-INTC)
           END-IF

           IF SGTSX005-DT-VNCT-OPR EQUAL SPACES OR
              '  .  .    '
              ADD  1               TO IC-INTC
              MOVE IC-INTC         TO SGTS0201-CD-IDFR-INTC(IC-INTC)
              MOVE 'Data vencimento deve ser informada.' TO
                                      SGTS0201-TX-DCR-INTC(IC-INTC)
           END-IF

           IF SGTSX005-DT-CPR-OPR EQUAL SPACES OR
              '  .  .    '
              ADD  1               TO IC-INTC
              MOVE IC-INTC         TO SGTS0201-CD-IDFR-INTC(IC-INTC)
              MOVE 'Data compra deve ser informada.' TO
                                      SGTS0201-TX-DCR-INTC(IC-INTC)
           END-IF

           IF SGTSX005-VL-PU-EMS-TIT-OPR EQUAL ZEROS
              ADD  1               TO IC-INTC
              MOVE IC-INTC         TO SGTS0201-CD-IDFR-INTC(IC-INTC)
              MOVE 'Valor emissão deve ser informado.' TO
                                      SGTS0201-TX-DCR-INTC(IC-INTC)
           END-IF

           IF SGTSX005-VL-PU-CPR-TIT-OPR EQUAL ZEROS
              ADD  1               TO IC-INTC
              MOVE IC-INTC         TO SGTS0201-CD-IDFR-INTC(IC-INTC)
              MOVE 'Valor compra deve ser informado.' TO
                                      SGTS0201-TX-DCR-INTC(IC-INTC)
           END-IF

           IF SGTSX005-QT-PAPL-TIT-EMS EQUAL ZEROS
              ADD  1               TO IC-INTC
              MOVE IC-INTC         TO SGTS0201-CD-IDFR-INTC(IC-INTC)
              MOVE 'Quantidade de papeis da emissão deve ser informada.'
                             TO   SGTS0201-TX-DCR-INTC(IC-INTC)
           END-IF

           IF SGTSX005-QT-PAPL-TIT-CPR EQUAL ZEROS
              ADD  1               TO IC-INTC
              MOVE IC-INTC         TO SGTS0201-CD-IDFR-INTC(IC-INTC)
              MOVE 'Quantidade de papeis de compra deve ser informada.'
                             TO   SGTS0201-TX-DCR-INTC(IC-INTC)
           END-IF

           PERFORM 221000-CALCULA-QT-AA-DRCO-OPR

           IF QT-AA-DRCO-OPR EQUAL ZEROS
              ADD  1               TO IC-INTC
              MOVE IC-INTC         TO SGTS0201-CD-IDFR-INTC(IC-INTC)
              MOVE 'Duração da operação inválida.'
                             TO   SGTS0201-TX-DCR-INTC(IC-INTC)
           END-IF

           IF SGTSX005-NR-EMS-TIT-OPR EQUAL ZEROS
              ADD  1               TO IC-INTC
              MOVE IC-INTC         TO SGTS0201-CD-IDFR-INTC(IC-INTC)
              MOVE 'Número de emissão do título deve ser informado.'
                             TO   SGTS0201-TX-DCR-INTC(IC-INTC)
           END-IF

           IF SGTSX005-CD-CMRA-OPR-ETRD EQUAL ZEROS
              ADD  1               TO IC-INTC
              MOVE IC-INTC         TO SGTS0201-CD-IDFR-INTC(IC-INTC)
              MOVE 'Câmara de registro do título da operação deve ser in
      -            'formada.'
                             TO   SGTS0201-TX-DCR-INTC(IC-INTC)
           END-IF

           IF SGTSX005-CD-CTGR-TVM EQUAL ZEROS
              ADD  1               TO IC-INTC
              MOVE IC-INTC         TO SGTS0201-CD-IDFR-INTC(IC-INTC)
              MOVE 'Código da ICVM deve ser informado.'
                             TO   SGTS0201-TX-DCR-INTC(IC-INTC)
           END-IF

           IF SGTSX005-CD-BCO-CRD-VL-OPR EQUAL ZEROS
              ADD  1               TO IC-INTC
              MOVE IC-INTC         TO SGTS0201-CD-IDFR-INTC(IC-INTC)
              MOVE 'Código do Banco de crédito do valor da operação deve
      -            ' ser informado.'
                             TO   SGTS0201-TX-DCR-INTC(IC-INTC)
           END-IF

           IF SGTSX005-CD-DEPE-CRD-VL-OPR EQUAL ZEROS
              ADD  1               TO IC-INTC
              MOVE IC-INTC         TO SGTS0201-CD-IDFR-INTC(IC-INTC)
              MOVE 'Código da Agência do Banco de crédito da Operação de
      -            've ser informado.'
                             TO   SGTS0201-TX-DCR-INTC(IC-INTC)
           END-IF

           IF SGTSX005-NR-CC-BB EQUAL ZEROS
              AND SGTSX005-NR-CC-OTR-BCO EQUAL ZEROS
              ADD  1               TO IC-INTC
              MOVE IC-INTC         TO SGTS0201-CD-IDFR-INTC(IC-INTC)
              MOVE 'Número da Conta Corrente para crédito da operação de
      -            've ser informado.'
                             TO   SGTS0201-TX-DCR-INTC(IC-INTC)
           END-IF

           IF SGTSX005-VL-APVD-BB-OPR EQUAL ZEROS
              ADD  1               TO IC-INTC
              MOVE IC-INTC         TO SGTS0201-CD-IDFR-INTC(IC-INTC)
              MOVE 'Valor Aprovado pelo BB para Emissão da operação deve
      -            ' ser informado.'
                             TO   SGTS0201-TX-DCR-INTC(IC-INTC)
           END-IF

      *--> Verificacao da taxonomia e classificacao negocial ao
      *--> Finalizar Versao - fluxo Gerar Versao da Operacao
      *--> Permitido somente para Operacao no estado:
      *    5.1 (CD-TIP-EST-OPR = 5- Encarteirada
      *       e CD-TIP-EST-VRS = 1- Em edicao)

           IF (CD-TIP-EST-OPR     = 5    AND
               CD-TIP-EST-VRS     = 1)

               IF SGTSX005-CD-CMCP EQUAL ZEROS
                  ADD  1               TO IC-INTC
                  MOVE IC-INTC         TO SGTS0201-CD-IDFR-INTC(IC-INTC)
                  MOVE 'Codigo da carteira de mercado de capitais deve s
      -               'er informado na edição da Operação.'
                                 TO   SGTS0201-TX-DCR-INTC(IC-INTC)
               END-IF

               IF SGTSX005-CD-GR-MCP EQUAL ZEROS
                  ADD  1               TO IC-INTC
                  MOVE IC-INTC         TO SGTS0201-CD-IDFR-INTC(IC-INTC)
                  MOVE 'Codigo do Grupo Mercado de Capitais deve ser inf
      -                'ormado na edição da Operação.'
                                 TO   SGTS0201-TX-DCR-INTC(IC-INTC)
               END-IF

               IF SGTSX005-CD-LVRO-MC-CPTL EQUAL ZEROS
                  ADD  1               TO IC-INTC
                  MOVE IC-INTC         TO SGTS0201-CD-IDFR-INTC(IC-INTC)
                  MOVE   'Codigo Livro Mercado Capitais deve ser informa
      -                  'do na edição da Operação.'
                                 TO   SGTS0201-TX-DCR-INTC(IC-INTC)
               END-IF

               IF SGTSX005-CD-SMCP EQUAL ZEROS
                 ADD  1               TO IC-INTC
                 MOVE IC-INTC         TO SGTS0201-CD-IDFR-INTC(IC-INTC)
                 MOVE 'Codigo Sublivro Mercado Capitais deve ser informa
      -               'do na edição da Operação.'
                                 TO   SGTS0201-TX-DCR-INTC(IC-INTC)
               END-IF

               IF SGTSX005-CD-CLSC-ITMT-FNCO EQUAL ZEROS
                  ADD  1               TO IC-INTC
                  MOVE IC-INTC         TO SGTS0201-CD-IDFR-INTC(IC-INTC)
                  MOVE 'Codigo do Classificacao do Instrumento Financeir
      -                'o deve ser informado na edição da Operação.'
                                 TO   SGTS0201-TX-DCR-INTC(IC-INTC)
               END-IF
           END-IF
           .
       220000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       221000-CALCULA-QT-AA-DRCO-OPR   SECTION.
      *--> Calcula a Quantidade de Anos de Duracao da Operacao
      *-----------------------------------------------------------------
           MOVE 0006                   TO  MSG1-CD-SEQL-ERRO

           IF  SGTSX005-DT-EMS-TIT-OPR NOT EQUAL SPACES  AND
               SGTSX005-DT-VNCT-OPR    NOT EQUAL SPACES

               MOVE  4095              TO  AREA-TAM
               MOVE  'SBDATAF -WRKAREA-FIM'
                                       TO  AREA-FIM

               MOVE  CORR SGTSX005-DT-EMS-TIT-OPR-R
                                       TO  ARG01-R

               MOVE  CORR SGTSX005-DT-VNCT-OPR-R
                                       TO  ARG02-R

               CALL  SBDATAF           USING  FUNCAO
                                              WKSBDATA
                                              ARG01  ARG02  ARG03

               IF  RETURN-CODE         NOT EQUAL ZEROS
                   SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
                   MOVE  0051          TO  MSG1-CD-ERRO-TS
                   MOVE  RETURN-CODE   TO  MSG1-CD-RTN-PGM-ACND
                   MOVE  'Erro no tratamento de datas. '
                                       TO  MSG1-TX-RTN-PGM-ACND
                                           MSG1-TX-ERRO-TS
                   MOVE  SBDATAF       TO  MSG1-NM-PGM-ACND
                                           NM-PGM
      *------> Monitorar variáveis do módulo chamado
                   PERFORM  969000-MONITORAR-VARIAVEIS
                   PERFORM  999000-GRAVA-ERRO-E-ENCERRA
               END-IF

               IF  ARG03               LESS  ZEROS  AND
                     RETURN-CODE       EQUAL  ZEROS
                   SET  ERRO-PROGRAMA-ACIONADO     TO  TRUE
                   MOVE  0063          TO  MSG1-CD-ERRO-TS
                   MOVE  'Data da emissão maior que data do vencimento.'
                                       TO  MSG1-TX-RTN-PGM-ACND
                                           MSG1-TX-ERRO-TS
                   MOVE  RETURN-CODE   TO  MSG1-CD-RTN-PGM-ACND
                   MOVE  SBDATAF       TO  MSG1-NM-PGM-ACND
                                           NM-PGM
                   PERFORM  969000-MONITORAR-VARIAVEIS
                   PERFORM  999000-GRAVA-ERRO-E-ENCERRA
               END-IF

               IF  RETURN-CODE         EQUAL  ZEROS
                   COMPUTE QT-AA-DRCO-OPR  ROUNDED  =
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
               MOVE ZEROES             TO  QT-AA-DRCO-OPR
           END-IF
           .
       221000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       230000-VALIDA-DADOS-MANDATO      SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

           IF SGTSX005-DT-MNDT-OPR EQUAL SPACES OR
              '  .  .    '
              ADD  1               TO IC-INTC
              MOVE IC-INTC         TO SGTS0201-CD-IDFR-INTC(IC-INTC)
              MOVE 'Data do mandato deve ser informada.' TO
                                      SGTS0201-TX-DCR-INTC(IC-INTC)
           END-IF

           IF SGTSX005-VL-MNDT-OPR EQUAL ZEROS
              ADD  1               TO IC-INTC
              MOVE IC-INTC         TO SGTS0201-CD-IDFR-INTC(IC-INTC)
              MOVE 'Valor mandatado deve ser informado.' TO
                                      SGTS0201-TX-DCR-INTC(IC-INTC)
           END-IF

           IF SGTSX005-VL-MNDT-OPR < SGTSX005-VL-PRV-ENCT
              ADD  1               TO IC-INTC
              MOVE IC-INTC         TO SGTS0201-CD-IDFR-INTC(IC-INTC)
               MOVE 'Valor mandatado deve ser maior ou igual ao valor de
      -             ' previsão de encarteiramento.'
                                       TO SGTS0201-TX-DCR-INTC(IC-INTC)
           END-IF

           PERFORM 231000-CALCULA-VL-EMS-TIT-OPR

           IF SGTSX005-VL-MNDT-OPR > VL-EMS-TIT-OPR
              ADD  1               TO IC-INTC
              MOVE IC-INTC         TO SGTS0201-CD-IDFR-INTC(IC-INTC)
               MOVE 'Valor mandatado deve ser menor ou igual ao valor de
      -             ' emissão do título.'
                                       TO SGTS0201-TX-DCR-INTC(IC-INTC)
           END-IF

           IF SGTSX005-VL-GRT-BB-OPR > ZEROS
              AND SGTSX005-VL-GRT-BB-OPR > SGTSX005-VL-MNDT-OPR
              ADD  1               TO IC-INTC
              MOVE IC-INTC         TO SGTS0201-CD-IDFR-INTC(IC-INTC)
               MOVE 'Valor da garantia firme BB deve ser menor ou igual
      -             'ao valor mandatado.'
                                       TO SGTS0201-TX-DCR-INTC(IC-INTC)
           END-IF

           IF SGTSX005-VL-GRT-BB-OPR > ZEROS
              AND SGTSX005-VL-GRT-BB-OPR > VL-EMS-TIT-OPR
              ADD  1               TO IC-INTC
              MOVE IC-INTC         TO SGTS0201-CD-IDFR-INTC(IC-INTC)
               MOVE 'Valor da garantia firme BB deve ser menor ou igual
      -             'ao valor da emissão do título.'
                                       TO SGTS0201-TX-DCR-INTC(IC-INTC)
           END-IF

           IF SGTSX005-VL-GRT-BB-OPR > ZEROS
              AND SGTSX005-VL-GRT-BB-OPR < SGTSX005-VL-PRV-ENCT
              ADD  1               TO IC-INTC
              MOVE IC-INTC         TO SGTS0201-CD-IDFR-INTC(IC-INTC)
               MOVE 'Valor da garantia firme BB deve ser maior ou igual
      -             'ao valor de previsão de encarteiramento.'
                                       TO SGTS0201-TX-DCR-INTC(IC-INTC)
           END-IF

      *--> Realizar esta consistencia somente para versão 1. VRS002
           IF SGTSX005-NR-VRS-OPR-ETRD EQUAL 1
              IF SGTSX005-VL-GRT-BB-OPR > ZEROS
                 AND SGTSX005-VL-GRT-BB-OPR < SGTSX005-VL-CPR-OPR
                 ADD  1               TO IC-INTC
                 MOVE IC-INTC         TO SGTS0201-CD-IDFR-INTC(IC-INTC)
                 MOVE 'Valor da garantia firme BB deve ser maior ou igua
      -             'l ao valor de compra da operação.'
                                      TO SGTS0201-TX-DCR-INTC(IC-INTC)
              END-IF
            END-IF

      *--> Realizar esta consistencia somente para versão 1. VRS002
           IF SGTSX005-NR-VRS-OPR-ETRD EQUAL 1
              IF SGTSX005-VL-APVD-BB-OPR <  SGTSX005-VL-CPR-OPR
                 ADD  1               TO IC-INTC
                 MOVE IC-INTC         TO SGTS0201-CD-IDFR-INTC(IC-INTC)
                 MOVE 'Valor Aprovado pelo BB para Emissão da operação d
      -            'eve ser maior ou igual ao valor de compra.'
                                      TO   SGTS0201-TX-DCR-INTC(IC-INTC)
              END-IF
           END-IF
           .
       230000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       231000-CALCULA-VL-EMS-TIT-OPR    SECTION.
      * Calcula o valor de emissão do título da operação
      *-----------------------------------------------------------------
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

           MULTIPLY SGTSX005-VL-PU-EMS-TIT-OPR BY
                                              SGTSX005-QT-PAPL-TIT-EMS
                                     GIVING VL-EMS-TIT-OPR
                 ON SIZE ERROR
                 SET ERRO-PROGRAMADO   TO TRUE
                 SET COM-ERRO-CLC      TO TRUE
                 MOVE 0062             TO MSG1-CD-ERRO-TS
                 MOVE 'Erro no cálculo do valor de emissão do título.'
                                       TO MSG1-TX-ERRO-TS
                 PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-MULTIPLY
           .
       231000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       240000-VALIDA-DADOS-PRD-MDLD    SECTION.
      *-----------------------------------------------------------------

           IF  SGTSX005-CD-PRD             EQUAL 0427  AND
               SGTSX005-CD-MDLD            EQUAL 0002
               PERFORM  241000-VALIDAR-DEBENTURE
           END-IF

           IF  SGTSX005-CD-PRD             EQUAL 0427  AND
               SGTSX005-CD-MDLD            EQUAL 0003
               PERFORM  242000-VALIDAR-FIDC
           END-IF

           IF  SGTSX005-CD-PRD             EQUAL 0427  AND
               SGTSX005-CD-MDLD            EQUAL 0004
               PERFORM  243000-VALIDAR-RECEBIVEL
           END-IF

           .
       240000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       241000-VALIDAR-DEBENTURE        SECTION.
      *-----------------------------------------------------------------

           IF SGTSX005-CD-ESPC-DBTR-OPR EQUAL ZEROS
              ADD  1               TO IC-INTC
              MOVE IC-INTC         TO SGTS0201-CD-IDFR-INTC(IC-INTC)
              MOVE 'Espécie de Debenture deve ser informada.'
                             TO   SGTS0201-TX-DCR-INTC(IC-INTC)
           END-IF

           IF SGTSX005-NR-SRE-EMS-TIT-OPR EQUAL ZEROS
              ADD  1               TO IC-INTC
              MOVE IC-INTC         TO SGTS0201-CD-IDFR-INTC(IC-INTC)
              MOVE 'Número de série de emissão deve ser informada para D
      -            'ebenture.'
                             TO   SGTS0201-TX-DCR-INTC(IC-INTC)
           END-IF

      * CD-TIP-IN = 3 => Operação Estruturada de Infraestrutura
      **     MOVE  3            TO   CD-TIP-IN
      **     PERFORM 800001-CONSULTA-INDICADOR

      **     IF REGISTRO-NAO-EXISTE
      **        ADD  1               TO IC-INTC
      **        MOVE IC-INTC         TO SGTS0201-CD-IDFR-INTC(IC-INTC)
      **      MOVE  'Indicador de Operação Estruturada de Infraestrutura
      **-            ' deve ser informada para Debenture.'
      **                       TO   SGTS0201-TX-DCR-INTC(IC-INTC)
      **     END-IF

      *--> Código Tipo do Indicador da Operação Estruturada (CD-TIP-IN)
      *    0004 - Operação Estruturada Não Convertida em Ação
           SET   NAO-CONVERSAO-EM-ACAO
                                       TO  TRUE

           PERFORM  800001-CONSULTA-INDICADOR

           IF  REGISTRO-NAO-EXISTE
               ADD   0001              TO  IC-INTC
               MOVE  IC-INTC           TO SGTS0201-CD-IDFR-INTC(IC-INTC)
               MOVE  'Indicador de Não Conversibilidade da Operação em A
      -              'ções deve ser informada para Debenture.'
                                       TO  SGTS0201-TX-DCR-INTC(IC-INTC)
           END-IF

           .
       241000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       242000-VALIDAR-FIDC             SECTION.
      *-----------------------------------------------------------------

           IF SGTSX005-CD-TIP-COTA-TIT EQUAL ZEROS
              ADD  1               TO IC-INTC
              MOVE IC-INTC         TO SGTS0201-CD-IDFR-INTC(IC-INTC)
              MOVE 'Tipo de Quota deve ser informado para FIDC.'
                             TO   SGTS0201-TX-DCR-INTC(IC-INTC)
           END-IF

           MOVE  2            TO   CD-TIP-FUC-CLI
           PERFORM 800002-PSQ-EXISTENCIA-CLI

           IF REGISTRO-NAO-EXISTE
              ADD  1               TO IC-INTC
              MOVE IC-INTC         TO SGTS0201-CD-IDFR-INTC(IC-INTC)
              MOVE  'Cliente para impacto no limite de crédito deve ser
      -            'informado para FIDC.'
                             TO   SGTS0201-TX-DCR-INTC(IC-INTC)
           END-IF

           MOVE  3            TO   CD-TIP-FUC-CLI
           PERFORM 800002-PSQ-EXISTENCIA-CLI

           IF REGISTRO-NAO-EXISTE
              ADD  1               TO IC-INTC
              MOVE IC-INTC         TO SGTS0201-CD-IDFR-INTC(IC-INTC)
              MOVE  'Cliente originador/cedente deve ser informado para
      -            'FIDC.'
                             TO   SGTS0201-TX-DCR-INTC(IC-INTC)
           END-IF

           MOVE  5            TO   CD-TIP-FUC-CLI
           PERFORM 800002-PSQ-EXISTENCIA-CLI

           IF REGISTRO-NAO-EXISTE
              ADD  1               TO IC-INTC
              MOVE IC-INTC         TO SGTS0201-CD-IDFR-INTC(IC-INTC)
              MOVE 'Cliente administrador deve ser informado para FIDC.'
                                   TO   SGTS0201-TX-DCR-INTC(IC-INTC)
           END-IF
           .
       242000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       243000-VALIDAR-RECEBIVEL       SECTION.
      *-----------------------------------------------------------------

      *--> Validar Cliente Impacto no Limite de Crédito
           SET   CLIENTE-IMPACTO-LIMITE
                                       TO  TRUE

           PERFORM 800002-PSQ-EXISTENCIA-CLI

           IF  REGISTRO-NAO-EXISTE
               ADD   0001              TO  IC-INTC
               MOVE  IC-INTC           TO SGTS0201-CD-IDFR-INTC(IC-INTC)
               MOVE  'Cliente para impacto no limite de crédito deve ser
      -              ' informado para Certificados de Recebíveis.'
                                       TO  SGTS0201-TX-DCR-INTC(IC-INTC)
           END-IF

      *--> Validar Cliente Originador/Cedente
           SET   CLIENTE-ORIGINADOR-CEDENTE
                                       TO  TRUE

           PERFORM 800002-PSQ-EXISTENCIA-CLI

           IF  REGISTRO-NAO-EXISTE
               ADD   0001              TO  IC-INTC
               MOVE  IC-INTC           TO SGTS0201-CD-IDFR-INTC(IC-INTC)
               MOVE  'Cliente Originador/Cedente deve ser informado para
      -              ' Certificados de Recebíveis.'
                                       TO  SGTS0201-TX-DCR-INTC(IC-INTC)
           END-IF

           .
       243000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       250000-VALIDA-OPR-ATI-TIT     SECTION.
      *-----------------------------------------------------------------
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO

           MOVE SGTS0211               TO NM-PGM

           MOVE SGTSX005-CD-TIT-OPR-ETRD
                                       TO SGTS0211-CD-TIT-OPR-ETRD
           MOVE SGTSX005-NR-DVS-TIT
                                       TO SGTS0211-NR-DVS-TIT

           PERFORM 900000-EXECUTAR-MODULO-CTNR

      **--> Validação em caso de versionamento da operação.
      **    não consiste se número de versão é igual a versão em valida-
      **    ção.
           IF  SGTS0201-NR-VRS-OPR-ETRD  >  1
            IF SGTS0201-CD-CTRA-TIT-VL NOT EQUAL SGTS0211-CD-CTRA-TIT-VL
               OR SGTS0201-CD-OPR-ETRD-MCP NOT EQUAL
                                               SGTS0211-CD-OPR-ETRD-MCP
               ADD  1               TO IC-INTC
               MOVE IC-INTC         TO SGTS0201-CD-IDFR-INTC(IC-INTC)
               MOVE   'Existe outra operação ativa para o mesmo Título.'
                                    TO   SGTS0201-TX-DCR-INTC(IC-INTC)
            END-IF
           ELSE

      **--> Validação em caso de finalização cadastro da operação.
            IF SGTS0201-CD-CTRA-TIT-VL NOT EQUAL SGTS0211-CD-CTRA-TIT-VL
               OR SGTS0201-CD-OPR-ETRD-MCP NOT EQUAL
                                               SGTS0211-CD-OPR-ETRD-MCP
               OR SGTS0201-NR-VRS-OPR-ETRD NOT EQUAL
                                               SGTS0211-NR-VRS-OPR-ETRD
               ADD  1               TO IC-INTC
               MOVE IC-INTC         TO SGTS0201-CD-IDFR-INTC(IC-INTC)
               MOVE   'Existe outra operação ativa para o mesmo Título.'
                             TO   SGTS0201-TX-DCR-INTC(IC-INTC)
            END-IF
           END-IF

           .
       250000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       260000-VALIDA-RCBT-PRVT     SECTION.
      *-----------------------------------------------------------------
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO

           MOVE SGTS0204               TO NM-PGM

           MOVE SGTS0201-CD-CTRA-TIT-VL
                                       TO SGTS0204-CD-CTRA-TIT-VL
           MOVE SGTS0201-CD-OPR-ETRD-MCP
                                       TO SGTS0204-CD-OPR-ETRD-MCP
           MOVE SGTS0201-NR-VRS-OPR-ETRD
                                       TO SGTS0204-NR-VRS-OPR-ETRD
           MOVE 'N'                    TO SGTS0204-IN-PSQ-OPR

           MOVE SGTSX005-DT-EMS-TIT-OPR  TO SGTS0204-DT-EMS-TIT-OPR
           MOVE SGTSX005-DT-VNCT-OPR     TO SGTS0204-DT-VNCT-OPR
           MOVE SGTSX005-DT-EVT-VCLD-VRS TO SGTS0204-DT-EVT-VCLD-VRS
           MOVE SGTSX005-VL-EVT-VCLD-VRS TO SGTS0204-VL-EVT-VCLD-VRS

           PERFORM 900000-EXECUTAR-MODULO-CTNR

           PERFORM VARYING IC FROM 1 BY 1
                        UNTIL IC GREATER SGTS0204-QT-OCR-TAB
               ADD 1 TO IC-INTC
               MOVE IC-INTC                  TO
                            SGTS0201-CD-IDFR-INTC(IC-INTC)
               MOVE SGTS0204-TX-DCR-INTC(IC) TO
                            SGTS0201-TX-DCR-INTC(IC-INTC)

           END-PERFORM

           .
       260000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       270000-VALIDA-PREF     SECTION.
      *-----------------------------------------------------------------

           MOVE 0011                   TO MSG1-CD-SEQL-ERRO

           MOVE SGTS0205               TO NM-PGM

           MOVE SGTS0201-CD-CTRA-TIT-VL
                                       TO SGTS0205-CD-CTRA-TIT-VL
           MOVE SGTS0201-CD-OPR-ETRD-MCP
                                       TO SGTS0205-CD-OPR-ETRD-MCP
           MOVE SGTS0201-NR-VRS-OPR-ETRD
                                       TO SGTS0205-NR-VRS-OPR-ETRD
           MOVE 'N'                    TO SGTS0205-IN-PSQ-OPR

           MOVE SGTSX005-DT-EMS-TIT-OPR  TO SGTS0205-DT-EMS-TIT-OPR
           MOVE SGTSX005-DT-VNCT-OPR     TO SGTS0205-DT-VNCT-OPR
           MOVE SGTSX005-DT-EMS-PREF     TO SGTS0205-DT-EMS-PREF
           MOVE SGTSX005-DT-CPR-OPR      TO SGTS0205-DT-CPR-OPR
           MOVE SGTSX005-QT-DD-PZ-VLD-PREF TO SGTS0205-QT-DD-PZ-VLD-PREF
           MOVE SGTSX005-PC-TARF-GRT-TAXA TO SGTS0205-PC-TARF-GRT-TAXA
           MOVE SGTSX005-QT-MM-CARE-JUR  TO SGTS0205-QT-MM-CARE-JUR
           MOVE SGTSX005-QT-MM-CARE-OPR  TO SGTS0205-QT-MM-CARE-OPR

           PERFORM 900000-EXECUTAR-MODULO-CTNR

           PERFORM VARYING IC FROM 1 BY 1
                        UNTIL IC GREATER SGTS0205-QT-OCR-TAB
               ADD 1 TO IC-INTC
               MOVE IC-INTC                  TO
                            SGTS0201-CD-IDFR-INTC(IC-INTC)
               MOVE SGTS0205-TX-DCR-INTC(IC) TO
                            SGTS0201-TX-DCR-INTC(IC-INTC)

           END-PERFORM

           .
       270000-SAI.
           EXIT.


      *-----------------------------------------------------------------
       280000-VALIDA-RMNC     SECTION.
      *-----------------------------------------------------------------

           MOVE 0012                   TO MSG1-CD-SEQL-ERRO

           MOVE SGTS0207               TO NM-PGM

           MOVE SGTS0201-CD-CTRA-TIT-VL
                                       TO SGTS0207-CD-CTRA-TIT-VL
           MOVE SGTS0201-CD-OPR-ETRD-MCP
                                       TO SGTS0207-CD-OPR-ETRD-MCP
           MOVE SGTS0201-NR-VRS-OPR-ETRD
                                       TO SGTS0207-NR-VRS-OPR-ETRD
           MOVE 'N'                    TO SGTS0207-IN-PSQ-OPR

           MOVE SGTSX005-DT-EMS-TIT-OPR  TO SGTS0207-DT-EMS-TIT-OPR
           MOVE SGTSX005-DT-VNCT-OPR     TO SGTS0207-DT-VNCT-OPR
           MOVE SGTSX005-DT-CPR-OPR      TO SGTS0207-DT-CPR-OPR

           PERFORM 900000-EXECUTAR-MODULO-CTNR

           PERFORM VARYING IC FROM 1 BY 1
                        UNTIL IC GREATER SGTS0207-QT-OCR-TAB
               ADD 1 TO IC-INTC
              MOVE IC-INTC                   TO
                            SGTS0201-CD-IDFR-INTC(IC-INTC)
               MOVE SGTS0207-TX-DCR-INTC(IC) TO
                            SGTS0201-TX-DCR-INTC(IC-INTC)

           END-PERFORM

           .
       280000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       290000-VALIDA-CND-NEGD     SECTION.
      *-----------------------------------------------------------------

           MOVE 0013                   TO MSG1-CD-SEQL-ERRO

           MOVE SGTS0208               TO NM-PGM

           MOVE SGTS0201-CD-CTRA-TIT-VL
                                       TO SGTS0208-CD-CTRA-TIT-VL
           MOVE SGTS0201-CD-OPR-ETRD-MCP
                                       TO SGTS0208-CD-OPR-ETRD-MCP
           MOVE SGTS0201-NR-VRS-OPR-ETRD
                                       TO SGTS0208-NR-VRS-OPR-ETRD
           MOVE 'N'                    TO SGTS0208-IN-PSQ-OPR

           MOVE SGTSX005-DT-EMS-TIT-OPR  TO SGTS0208-DT-EMS-TIT-OPR
           MOVE SGTSX005-DT-VNCT-OPR     TO SGTS0208-DT-VNCT-OPR
           MOVE SGTSX005-DT-CPR-OPR      TO SGTS0208-DT-CPR-OPR
           MOVE SGTSX005-QT-MM-CARE-RSGT TO SGTS0208-QT-MM-CARE-RSGT
           MOVE SGTSX005-QT-MM-CARE-AMTZ TO SGTS0208-QT-MM-CARE-AMTZ
           MOVE SGTSX005-QT-MM-CARE-AQS  TO SGTS0208-QT-MM-CARE-AQS

           PERFORM 900000-EXECUTAR-MODULO-CTNR

           PERFORM VARYING IC FROM 1 BY 1
                        UNTIL IC GREATER SGTS0208-QT-OCR-TAB
               ADD 1 TO IC-INTC
               MOVE IC-INTC                  TO
                            SGTS0201-CD-IDFR-INTC(IC-INTC)
               MOVE SGTS0208-TX-DCR-INTC(IC) TO
                            SGTS0201-TX-DCR-INTC(IC-INTC)
           END-PERFORM

           .
       290000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-VALIDA-TARIFAS     SECTION.
      *-----------------------------------------------------------------
           MOVE 0014                   TO MSG1-CD-SEQL-ERRO

           SET REG-NAO-EXISTE TO TRUE
           MOVE ZEROS         TO QT-REG-SEL

            EXEC SQL
                 SELECT 1
                   INTO  :QT-REG-SEL
                   FROM DB2SGT.TARF_OPR_ETRD
                WHERE
                     CD_CTRA_TIT_VL = :SGTS0201-CD-CTRA-TIT-VL
                AND  CD_OPR_ETRD_MCP = :SGTS0201-CD-OPR-ETRD-MCP
                AND  NR_VRS_OPR_ETRD = :SGTS0201-NR-VRS-OPR-ETRD
                FETCH FIRST 1 ROW ONLY
           END-EXEC

            IF  QT-REG-SEL > ZEROS
               SET REG-EXISTE TO TRUE
           END-IF

           IF REG-EXISTE
               EXEC SQL
                     SELECT SUM(PC_TARF_OPR)
                       INTO  :SUM-PC-TARF-OPR
                       FROM DB2SGT.TARF_OPR_ETRD
                    WHERE
                         CD_CTRA_TIT_VL  = :SGTS0201-CD-CTRA-TIT-VL
                    AND  CD_OPR_ETRD_MCP = :SGTS0201-CD-OPR-ETRD-MCP
                    AND  NR_VRS_OPR_ETRD = :SGTS0201-NR-VRS-OPR-ETRD
                    FETCH FIRST 1 ROW ONLY
               END-EXEC
           END-IF


           IF REG-EXISTE

             IF SUM-PC-TARF-OPR  < SGTSX005-PC-TARF-GRT-TAXA
               ADD  1               TO IC-INTC
               MOVE IC-INTC         TO SGTS0201-CD-IDFR-INTC(IC-INTC)
               MOVE   'Soma das tarifas menor que a precificação do fee
      -               'de garantia da taxa.'
                             TO   SGTS0201-TX-DCR-INTC(IC-INTC)
             END-IF
           ELSE
             ADD  1               TO IC-INTC
             MOVE IC-INTC         TO SGTS0201-CD-IDFR-INTC(IC-INTC)
             MOVE   'Tarifa ou Precificação do fee de garantia da taxa n
      -              'ão cadastrados.'
                             TO   SGTS0201-TX-DCR-INTC(IC-INTC)
           END-IF
           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       310000-VALIDA-ORD-COMPRA     SECTION.
      *-----------------------------------------------------------------

           MOVE 0015                   TO MSG1-CD-SEQL-ERRO

           MOVE ZEROS                  TO QT-REG-SEL

           EXEC SQL
                SELECT  1
                INTO  :QT-REG-SEL
                FROM DB2SGT.ORD_NEGO_OPR A
                INNER JOIN
                DB2SGT.MOD_ORD_NEGO B
                ON
                A.CD_MOD_ORD_NEGO = B.CD_MOD_ORD_NEGO
                WHERE
                     A.CD_CTRA_TIT_VL = :SGTS0201-CD-CTRA-TIT-VL
                AND  A.CD_OPR_ETRD_MCP = :SGTS0201-CD-OPR-ETRD-MCP
                AND  A.NR_VRS_OPR_ETRD = :SGTS0201-NR-VRS-OPR-ETRD
                AND  B.CD_TIP_NEGO = 1
           END-EXEC

           IF QT-REG-SEL  EQUAL ZEROS
              ADD  1               TO IC-INTC
              MOVE IC-INTC         TO SGTS0201-CD-IDFR-INTC(IC-INTC)
              MOVE   'A Ordem de Compra deve ser cadastrada.'
                             TO   SGTS0201-TX-DCR-INTC(IC-INTC)
      * Obs:Retirada consistência devido não ter mais alteração de dados
      * da operação após finalização da mesma.
      *    ELSE

      * CD-TIP-IN = 19 => Indicador de Ordem de Compra Atualizada
      *       MOVE  19            TO   CD-TIP-IN
      *       PERFORM 800001-CONSULTA-INDICADOR
      *
      *       IF REGISTRO-NAO-EXISTE
      *          ADD  1               TO IC-INTC
      *          MOVE IC-INTC         TO SGTS0201-CD-IDFR-INTC(IC-INTC)
      *           MOVE   'A Ordem de Compra deve ser atualizada devido a
      *                  'alterações no cadastro da operação.'
      *                           TO   SGTS0201-TX-DCR-INTC(IC-INTC)
      *       END-IF
           END-IF

           .
       310000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       320000-VRFR-RISCO-CLIENTE       SECTION.
      *--> Recupera o Risco do Cliente de Impacto de Limite de Crédito
      *-----------------------------------------------------------------
           MOVE 0016                   TO MSG1-CD-SEQL-ERRO

           MOVE ZEROS                  TO CD-CLI
      * Cliente impacto limite de credito = 2

           EXEC SQL
                 SELECT CD_CLI
                   INTO  :CD-CLI
                   FROM DB2SGT.CLI_OPR_ETRD
                WHERE
                     CD_CTRA_TIT_VL  = :SGTS0201-CD-CTRA-TIT-VL
                AND  CD_OPR_ETRD_MCP = :SGTS0201-CD-OPR-ETRD-MCP
                AND  NR_VRS_OPR_ETRD = :SGTS0201-NR-VRS-OPR-ETRD
                AND  CD_TIP_FUC_CLI  = 2
                FETCH FIRST 1 ROW ONLY
           END-EXEC

           INITIALIZE                 ANCS0001-DADOS.

           IF  CD-CLI EQUAL ZEROS
               ADD  1               TO IC-INTC
               MOVE IC-INTC         TO SGTS0201-CD-IDFR-INTC(IC-INTC)
               MOVE   'Cliente com impacto limite de credito não cadastr
      -               'ado na operação.'
                                  TO   SGTS0201-TX-DCR-INTC(IC-INTC)
           ELSE
              MOVE ANCS0001               TO NM-PGM

      *    Movimentar os campos de entrada
              MOVE CD-CLI           TO ANCS0001-CD-CLI
              MOVE 'N'              TO ANCS0001-IN-PSQ-SUBLIM

              PERFORM 910000-CHAMAR-SERVICO

              IF  ANCS0001-CD-RISCO        EQUAL SPACES
                  ADD  1               TO  IC-INTC
                  MOVE IC-INTC         TO SGTS0201-CD-IDFR-INTC(IC-INTC)
                  MOVE  'Cliente com impacto limite de crédito não possu
      -                 'i risco cadastrado no ANC.'
                                       TO  SGTS0201-TX-DCR-INTC(IC-INTC)
              END-IF
           END-IF

           .
       320000-SAI.
           EXIT.

      *-----------------------------------------------------------------
      * 330000-VALIDA-PMS-ACSS-UOR     SECTION.
      *-----------------------------------------------------------------

      *     MOVE 0016                   TO MSG1-CD-SEQL-ERRO

      *     MOVE SGTS0209               TO NM-PGM

      *     MOVE SGTSX005-CD-UOR-DTTR-OPR
      *                                 TO SGTS0209-CD-UOR-DTTR-OPR
      *     MOVE KOPRD-CD-OPRD
      *                                 TO SGTS0209-CD-USU-PMS

      *     PERFORM 900000-EXECUTAR-MODULO-CTNR

      *     PERFORM VARYING IC FROM 1 BY 1
      *                  UNTIL IC GREATER SGTS0209-QT-OCR-TAB
      *         ADD 1 TO IC-INTC
      *         MOVE IC-INTC                  TO
      *                      SGTS0201-CD-IDFR-INTC(IC-INTC)
      *         MOVE SGTS0209-TX-DCR-INTC(IC) TO
      *                      SGTS0201-TX-DCR-INTC(IC-INTC)
      *     END-PERFORM

      *     .
      * 330000-SAI.
      *     EXIT.

      *-----------------------------------------------------------------
       800001-CONSULTA-INDICADOR     SECTION.
      *-----------------------------------------------------------------
           MOVE 0017                   TO MSG1-CD-SEQL-ERRO

           MOVE ZEROS                  TO QT-REG-SEL

           SET REGISTRO-NAO-EXISTE TO TRUE

           EXEC SQL
                 SELECT 1
                   INTO  :QT-REG-SEL
                   FROM DB2SGT.IN_OPR_ETRD
                WHERE
                     CD_CTRA_TIT_VL = :SGTS0201-CD-CTRA-TIT-VL
                AND  CD_OPR_ETRD_MCP = :SGTS0201-CD-OPR-ETRD-MCP
                AND  NR_VRS_OPR_ETRD = :SGTS0201-NR-VRS-OPR-ETRD
                AND  CD_TIP_IN = :CD-TIP-IN
                FETCH FIRST 1 ROW ONLY
           END-EXEC

           IF  QT-REG-SEL GREATER ZEROS
               SET REGISTRO-EXISTE TO TRUE
           END-IF

           .
       800001-SAI.
           EXIT.

      *-----------------------------------------------------------------
       800002-PSQ-EXISTENCIA-CLI     SECTION.
      *-----------------------------------------------------------------
           MOVE 0018                   TO MSG1-CD-SEQL-ERRO

           MOVE ZEROS                  TO QT-REG-SEL
      * CD-TIP-FUC-CLI
      *
      * 1 - Emissor
      * 2 - Impacto Limite
      * 3 - Originador/Cedente
      * 4 - Agente Fiduciário/Notas
      * 5 - Administrador

           SET REGISTRO-NAO-EXISTE TO TRUE

           EXEC SQL
                 SELECT 1
                   INTO  :QT-REG-SEL
                   FROM DB2SGT.CLI_OPR_ETRD
                WHERE
                     CD_CTRA_TIT_VL = :SGTS0201-CD-CTRA-TIT-VL
                AND  CD_OPR_ETRD_MCP = :SGTS0201-CD-OPR-ETRD-MCP
                AND  NR_VRS_OPR_ETRD = :SGTS0201-NR-VRS-OPR-ETRD
                AND  CD_TIP_FUC_CLI = :CD-TIP-FUC-CLI
                FETCH FIRST 1 ROW ONLY
           END-EXEC

           IF  QT-REG-SEL GREATER ZEROS
               SET REGISTRO-EXISTE TO TRUE
           END-IF

           .
       800002-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *                                                                *
      *  SECTION DA FAIXA DE 900000 A 969900 PARA DEMAIS ROTINAS       *
      *                                                                *
      *-----------------------------------------------------------------
      * Executa um módulo implementado com CONTAINER                   *
      *-----------------------------------------------------------------
       900000-EXECUTAR-MODULO-CTNR     SECTION.
      *-----------------------------------------------------------------

      *    Grava o container de requisição do módulo a executar
           PERFORM 982000-GRAVAR-CONTAINER-RQSC

      *    Limpa a resposta e erro antes de executar o módulo
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO

      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EVALUATE NM-PGM
               WHEN SGTS0204
                    EXEC CICS LINK
                              PROGRAM ( SGTS0204   )
                              CHANNEL ( NM-CNL     )
                              RESP    ( MSG1-RESP1 )
                              RESP2   ( MSG1-RESP2 )
                    END-EXEC
               WHEN SGTS0205
                    EXEC CICS LINK
                              PROGRAM ( SGTS0205   )
                              CHANNEL ( NM-CNL     )
                              RESP    ( MSG1-RESP1 )
                              RESP2   ( MSG1-RESP2 )
                    END-EXEC
               WHEN SGTS0207
                    EXEC CICS LINK
                              PROGRAM ( SGTS0207   )
                              CHANNEL ( NM-CNL     )
                              RESP    ( MSG1-RESP1 )
                              RESP2   ( MSG1-RESP2 )
                    END-EXEC

               WHEN SGTS0208
                    EXEC CICS LINK
                              PROGRAM ( SGTS0208   )
                              CHANNEL ( NM-CNL     )
                              RESP    ( MSG1-RESP1 )
                              RESP2   ( MSG1-RESP2 )
                    END-EXEC

               WHEN SGTS0211
                    EXEC CICS LINK
                              PROGRAM ( SGTS0211   )
                              CHANNEL ( NM-CNL     )
                              RESP    ( MSG1-RESP1 )
                              RESP2   ( MSG1-RESP2 )
                    END-EXEC

               WHEN OTHER
                    MOVE 9006          TO MSG1-CD-ERRO-TS
                    MOVE 'Não definida a execução do módulo.'
                                       TO MSG1-TX-ERRO-TS
                    PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE
      *
           EVALUATE TRUE
               WHEN MSG1-RESP1        EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    SET ERRO-CICS      TO TRUE
                    MOVE 3004          TO MSG1-CD-ERRO-TS
                    STRING
                    'Erro  ao acionar o módulo  ' NM-PGM  ', '
                    'RESP  = '  MSG1-RESP1  ', '
                    'RESP2 = '  MSG1-RESP2
                    DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
                    PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *    Efetua a leitura do Container de Erro
           PERFORM 972000-LER-CONTAINER-DE-ERRO

      *    Efetua a leitura do Container de Resposta
           PERFORM 973000-LER-RESPOSTA
           .
       900000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       910000-CHAMAR-SERVICO           SECTION.
      *--> Acionar programa IIBP2000 para requisição do serviço.
      *-----------------------------------------------------------------

      *--> Grava o container com dados do serviço/operação a chamar
           PERFORM 981000-GRAVAR-CONTAINER-SRVC

      *--> Grava o container de requisicao do modulo a executar
           PERFORM 982000-GRAVAR-CONTAINER-RQSC

      *--> Limpa a resposta e erro antes de executar o modulo
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO


      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS LINK
                     PROGRAM   ( IIBP2000     )
                     CHANNEL   ( NM-CNL       )
                     RESP      ( MSG1-RESP1   )
                     RESP2     ( MSG1-RESP2   )
           END-EXEC

           EVALUATE TRUE
               WHEN MSG1-RESP1             EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    SET   ERRO-CICS    TO  TRUE
                    MOVE  9000         TO  MSG1-CD-ERRO-TS
                    STRING  'Erro ao acionar serviço: '
                            SRVC-ID-SRVC '.' SRVC-VRS-SRVC '.'
                            SRVC-ID-OPR  '.' SRVC-VRS-OPR
                            'via IIBP2000, ' 'RESP=' MSG1-RESP1
                                           ', RESP-2=' MSG1-RESP2 '.'
                            DELIMITED BY SIZE
                            INTO  MSG1-TX-ERRO-TS
                    END-STRING

                    PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *--> Efetua a leitura do Container de Erro
           PERFORM 972000-LER-CONTAINER-DE-ERRO

      *--> Efetua a leitura do Container de Resposta
           PERFORM 973000-LER-RESPOSTA

           .
       910000-SAI.
           EXIT.


      *-----------------------------------------------------------------
       969000-MONITORAR-VARIAVEIS      SECTION.
      *--> Monitora as variáveis dos módulos chamados
      *-----------------------------------------------------------------

           EVALUATE  NM-PGM
      *------> Monitora as Variaveis de Entrada do ANCS0001
               WHEN  ANCS0001
                     MOVE 'ANCS0001-001-CD-CLI'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  ANCS0001-CD-CLI
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'ANCS0001-002-IN-PSQ-SUBLIM'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  ANCS0001-IN-PSQ-SUBLIM
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

      *------> Monitora as Variaveis de Entrada do SGTSX005
               WHEN  SGTSX005
                     MOVE 'T005-001-ACAO'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSX005-ACAO
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'T005-002-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSX005-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

      *------------> Verifica se Codigo da Operacao é Numerico
                     MOVE 'T005-003-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                     IF  SGTSX005-CD-OPR-ETRD-MCP
                                           NUMERIC
                         MOVE  SGTSX005-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP
                         MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'T005-004-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSX005-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

      *------> Monitora as Variaveis de Entrada do SGTS0204
               WHEN  SGTS0204
                     MOVE '0204-001-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0204-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

      *------------> Verifica se Codigo da Operacao é Numerico
                     MOVE '0204-002-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                     IF  SGTS0204-CD-OPR-ETRD-MCP
                                           NUMERIC
                         MOVE  SGTS0204-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP
                         MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE '0204-003-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0204-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE '0204-004-IN-PSQ-OPR'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0204-IN-PSQ-OPR
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE '0204-005-DT-EMS-TIT-OPR'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0204-DT-EMS-TIT-OPR
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE '0204-006-DT-VNCT-OPR'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0204-DT-VNCT-OPR
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE '0204-007-VL-EVT-VCLD-VRS'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0204-VL-EVT-VCLD-VRS
                                       TO  VL-15V02
                     MOVE  VL-15V02    TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

      *------> Monitora as Variaveis de Entrada do SGTS0205
               WHEN  SGTS0205
                     MOVE '0205-001-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0205-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

      *------------> Verifica se Codigo da Operacao é Numerico
                     MOVE '0205-002-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                     IF  SGTS0205-CD-OPR-ETRD-MCP
                                           NUMERIC
                         MOVE  SGTS0205-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP
                         MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE '0205-003-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0205-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE '0205-004-IN-PSQ-OPR'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0205-IN-PSQ-OPR
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE '0205-005-DT-EMS-TIT-OPR'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0205-DT-EMS-TIT-OPR
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE '0205-006-DT-VNCT-OPR'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0205-DT-VNCT-OPR
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE '0205-007-DT-EMS-PREF'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0205-DT-EMS-PREF
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE '0205-008-DT-CPR-OPR'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0205-DT-CPR-OPR
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE '0205-009-QT-DD-PZ-VLD-PREF'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0205-QT-DD-PZ-VLD-PREF
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE '0205-010-PC-TARF-GRT-TAXA'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0205-PC-TARF-GRT-TAXA
                                       TO  VL-04V10
                     MOVE  VL-04V10    TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE '0205-011-QT-MM-CARE-JUR'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0205-QT-MM-CARE-JUR
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE '0205-012-QT-MM-CARE-OPR'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0205-QT-MM-CARE-OPR
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT


      *------> Monitora as Variaveis de Entrada do SGTS0207
               WHEN  SGTS0207
                     MOVE '0207-001-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0207-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

      *------------> Verifica se Codigo da Operacao é Numerico
                     MOVE '0207-002-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                     IF  SGTS0207-CD-OPR-ETRD-MCP
                                           NUMERIC
                         MOVE  SGTS0207-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP
                         MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE '0207-003-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0207-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE '0207-004-IN-PSQ-OPR'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0207-IN-PSQ-OPR
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM 999003-ADICIONA-VRV-MNT

                     MOVE '0207-005-DT-EMS-TIT-OPR'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0207-DT-EMS-TIT-OPR
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM 999003-ADICIONA-VRV-MNT

                     MOVE 'S0207-006-DT-VNCT-OPR'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0207-DT-VNCT-OPR
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM 999003-ADICIONA-VRV-MNT

                     MOVE '0207-007-DT-CPR-OPR'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0207-DT-CPR-OPR
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM 999003-ADICIONA-VRV-MNT

      *------> Monitora as Variaveis de Entrada do SGTS0208
               WHEN  SGTS0208
                     MOVE '0208-001-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0208-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

      *------------> Verifica se Codigo da Operacao é Numerico
                     MOVE '0208-002-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                     IF  SGTS0208-CD-OPR-ETRD-MCP
                                           NUMERIC
                         MOVE  SGTS0208-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP
                         MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE '0208-003-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0208-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE '0208-004-IN-PSQ-OPR'
                                       TO MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0208-IN-PSQ-OPR
                                       TO MSGKW999-VL-VRV-MNT
                     PERFORM 999003-ADICIONA-VRV-MNT

                     MOVE '0208-005-DT-EMS-TIT-OPR'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0208-DT-EMS-TIT-OPR
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM 999003-ADICIONA-VRV-MNT

                     MOVE '0208-006-DT-VNCT-OPR'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0208-DT-VNCT-OPR
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM 999003-ADICIONA-VRV-MNT

                     MOVE '0208-007-DT-CPR-OPR'
                                       TO MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0208-DT-CPR-OPR
                                       TO MSGKW999-VL-VRV-MNT
                     PERFORM 999003-ADICIONA-VRV-MNT

                     MOVE '0208-008-QT-MM-CARE-RSGT'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0208-QT-MM-CARE-RSGT
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM 999003-ADICIONA-VRV-MNT

                     MOVE '0208-009-QT-MM-CARE-AMTZ'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0208-QT-MM-CARE-AMTZ
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM 999003-ADICIONA-VRV-MNT

                     MOVE '0208-010-QT-MM-CARE-AQS'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0208-QT-MM-CARE-AQS
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM 999003-ADICIONA-VRV-MNT

               WHEN  SGTS0211
                     MOVE 'SGTS0211-CD-TIT-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0211-CD-TIT-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                     PERFORM 999003-ADICIONA-VRV-MNT

                     MOVE 'SGTS0211-NR-DVS-TIT' TO MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0211-NR-DVS-TIT  TO MSGKW999-VL-VRV-MNT
                     PERFORM 999003-ADICIONA-VRV-MNT

               WHEN  OTHER
                     CONTINUE
           END-EVALUATE

           .
       969000-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *                                                                *
      * SECTION DA FAIXA DE 970000 A 979900 PARA LEITURA DE DADOS      *
      *                                                                *
      *-----------------------------------------------------------------
       970000-LER-CANAL                SECTION.
      *    Obtem o canal corrente
      *-----------------------------------------------------------------
           MOVE 0019                   TO MSG1-CD-SEQL-ERRO

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CANAL             TO TRUE

           EXEC CICS ASSIGN
                     CHANNEL ( NM-CNL )
           END-EXEC

      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       970000-FIM.
           EXIT.

      *-----------------------------------------------------------------
       971000-LER-REQUISICAO           SECTION.
      *-----------------------------------------------------------------
           MOVE 0020                   TO MSG1-CD-SEQL-ERRO

      *    Monta o nome do container de requisição
           MOVE CTE-PROG(5:8)          TO NM-PGM
           MOVE CTNR-SGTS0201-RQSC     TO NM-CTNR

      *    Recupera o tamanho do container de requisição
           PERFORM 973100-LER-CONTAINER-NODATA

      *--> Obtem o tamanho esperado da requisição
           MOVE  LENGTH OF SGTS0201-RQSC
                                       TO  QT-CCT-EPRO

      *--> Valida tamanho esperado da requisição X tamanho recebido
           IF  QT-CCT-LIDO             NOT EQUAL  QT-CCT-EPRO
               SET   ERRO-TAMANHO-AREA TO  TRUE
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           MOVE 0021                   TO MSG1-CD-SEQL-ERRO

      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Efetua a leitura do container
           EXEC CICS GET
                     CONTAINER ( NM-CTNR       )
                     FLENGTH   ( QT-CCT-LIDO   )
                     INTO      ( SGTS0201-RQSC )
           END-EXEC

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       971000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       972000-LER-CONTAINER-DE-ERRO    SECTION.
      *-----------------------------------------------------------------
      *    A presença do container PRM-ERRO indica que houve erro de
      *    integração, a ausência do container indica sucesso.
      *-----------------------------------------------------------------

      *    Lê o container de erro dos módulos chamados
           SET GET-CONTAINER           TO TRUE
           MOVE CTNR-PRM-ERRO          TO NM-CTNR
           INITIALIZE IIBKERRO-PRM-ERRO

      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS GET
                     CONTAINER ( NM-CTNR                     )
                     INTO      ( IIBKERRO-PRM-ERRO           )
                     FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                     RESP      ( MSG1-RESP1                  )
                     RESP2     ( MSG1-RESP2                  )
           END-EXEC

           EVALUATE TRUE
      *        Caso deseje personalizar ou não interromper o
      *        processamento, alterar para não dar erro aqui SOMENTE
      *        PARA O módulo CHAMADO e tratar na section chamadora
               WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
                    CONTINUE
               WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
                    SET ERRO-PROGRAMA-ACIONADO TO TRUE
                    MOVE  NM-PGM       TO  MSG1-NM-PGM-ACND

                    IF  NM-PGM(1:3) EQUAL 'SGT'
                        MOVE  IIBKERRO-CD-ERRO
                                       TO  MSG1-CD-RTN-PGM-ACND
                                           MSG1-CD-ERRO-TS
                        MOVE  IIBKERRO-TX-ERRO
                                       TO  MSG1-TX-RTN-PGM-ACND
                                           MSG1-TX-ERRO-TS
                    ELSE
                       MOVE 3004          TO MSG1-CD-ERRO-TS
                       STRING  'Erro ao acionar o módulo ' NM-PGM '.'
                         DELIMITED BY SIZE
                         INTO           MSG1-TX-ERRO-TS

                       MOVE IIBKERRO-TX-ERRO TO MSG1-TX-RTN-PGM-ACND
                       MOVE IIBKERRO-CD-ERRO TO MSG1-CD-RTN-PGM-ACND
                    END-IF

      *             Monitorar variáveis do módulo chamado
                    PERFORM  969000-MONITORAR-VARIAVEIS
                    PERFORM  999000-GRAVA-ERRO-E-ENCERRA
               WHEN OTHER
                    SET ERRO-CONTAINER TO TRUE
                    PERFORM 999002-TRATA-CONDITION-CICS
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
           EVALUATE NM-PGM
               WHEN ANCS0001
                    MOVE 'PRM-DADOS-RPST'         TO NM-CTNR
               WHEN OTHER
                    MOVE NM-PGM        TO NM-CTNR(1:8)
                    MOVE '-RPST'       TO NM-CTNR(9:5)
           END-EVALUATE

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

               WHEN ANCS0001
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (ANCS0001-DADOS   )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN SGTS0204
                    MOVE LENGTH OF SGTS0204-RPST TO QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER ( NM-CTNR       )
                              INTO      ( SGTS0204-RPST )
                              FLENGTH   ( QT-CCT-LIDO   )
                    END-EXEC
               WHEN SGTS0205
                    MOVE LENGTH OF SGTS0205-RPST TO QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER ( NM-CTNR       )
                              INTO      ( SGTS0205-RPST )
                              FLENGTH   ( QT-CCT-LIDO   )
                    END-EXEC
               WHEN SGTS0207
                    MOVE LENGTH OF SGTS0205-RPST TO QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER ( NM-CTNR       )
                              INTO      ( SGTS0207-RPST )
                              FLENGTH   ( QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTS0208
                    MOVE LENGTH OF SGTS0205-RPST TO QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER ( NM-CTNR       )
                              INTO      ( SGTS0208-RPST )
                              FLENGTH   ( QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTS0211
                    EXEC CICS GET
                              CONTAINER ( NM-CTNR       )
                              INTO      ( SGTS0211-RPST )
                              FLENGTH   ( QT-CCT-LIDO   )
                    END-EXEC

               WHEN OTHER
                    SET   ERRO-PROGRAMADO
                                       TO  TRUE
                    MOVE  9003         TO  MSG1-CD-ERRO-TS
                    STRING  'Erro na leitura da resposta do módulo '
                            NM-PGM '.'
                            DELIMITED BY SIZE
                            INTO           MSG1-TX-ERRO-TS
                    PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       973000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       973100-LER-CONTAINER-NODATA     SECTION.
      *-----------------------------------------------------------------

      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Obtém a quantidade de caracteres no container
           MOVE ZEROS                  TO QT-CCT-LIDO
           EXEC CICS GET
                     CONTAINER ( NM-CTNR     )
                     NODATA
                     FLENGTH   ( QT-CCT-LIDO )
           END-EXEC

      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       973100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       974000-LER-DADOS-OPRD           SECTION.
      *-----------------------------------------------------------------
           MOVE 0022                   TO MSG1-CD-SEQL-ERRO

      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE
           MOVE CTNR-PRM-INF-OPRD      TO NM-CTNR

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

           INITIALIZE PRM-INF-OPRD
           EXEC CICS GET
                CONTAINER ( NM-CTNR                )
                INTO      ( PRM-INF-OPRD           )
                FLENGTH   ( LENGTH OF PRM-INF-OPRD )
           END-EXEC.

      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       974000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       980000-GRAVAR-CONTAINER-RPST    SECTION.
      *----------------------------------------------------------------*

      *    Exclui eventual container de erro de outros programas
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO

           MOVE 0023                   TO MSG1-CD-SEQL-ERRO

      *    Monta o nome do container de resposta
           MOVE 'SGTS0201-RPST'        TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

           EXEC CICS PUT
                     CONTAINER (NM-CTNR                )
                     FROM      (SGTS0201-RPST          )
                     FLENGTH   (LENGTH OF SGTS0201-RPST)
           END-EXEC

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       980000-SAI.
           EXIT.

      *---------------------------------------------------------------*
       981000-GRAVAR-CONTAINER-SRVC    SECTION.
      *---------------------------------------------------------------*
      *    Postar container de requisição de serviço.
      *---------------------------------------------------------------*

      *    Monta o nome do container de requisicao do servico
           MOVE 'PRM-RQSC-SRVC'        TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE

           EVALUATE NM-PGM

               WHEN ANCS0001
                    MOVE 38449          TO SRVC-ID-SRVC
                    MOVE 1             TO SRVC-VRS-SRVC
                    MOVE 388584        TO SRVC-ID-OPR
                    MOVE 1             TO SRVC-VRS-OPR

           END-EVALUATE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

           EXEC CICS PUT
                     CONTAINER ( NM-CTNR                     )
                     FROM      ( SRVC-PRM-RQSC-SRVC          )
                     FLENGTH   ( LENGTH OF SRVC-PRM-RQSC-SRVC)
           END-EXEC

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       981000-SAI.
           EXIT.


      *-----------------------------------------------------------------
      *                                                                *
      * SECTION DA FAIXA DE 980000 A 989900 PARA GRAVACAO DE DADOS     *
      *                                                                *
      *-----------------------------------------------------------------
       982000-GRAVAR-CONTAINER-RQSC    SECTION.
      *-----------------------------------------------------------------
      *    Postar container com dados de requisição (entrada)
      *-----------------------------------------------------------------

      *    Monta o nome dos container de acordo com modulo a executar
           MOVE SPACES                 TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE

      *    Quando módulos via IIB, fixo nome PRM-DADOS-RQSC, senão
      *    nome_modulo -RQSC
           EVALUATE NM-PGM
               WHEN ANCS0001
                    MOVE 'PRM-DADOS-RQSC'
                                       TO NM-CTNR
               WHEN OTHER
                    MOVE NM-PGM        TO NM-CTNR(1:8)
                    MOVE '-RQSC'       TO NM-CTNR(9:5)
           END-EVALUATE


      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Faz put da requisição do programa a executar
           EVALUATE NM-PGM
               WHEN SGTS0204
                    EXEC CICS PUT
                              CONTAINER ( NM-CTNR                 )
                              FROM      ( SGTS0204-RQSC           )
                              FLENGTH   ( LENGTH OF SGTS0204-RQSC )
                    END-EXEC
               WHEN SGTS0205
                    EXEC CICS PUT
                              CONTAINER ( NM-CTNR                 )
                              FROM      ( SGTS0205-RQSC           )
                              FLENGTH   ( LENGTH OF SGTS0205-RQSC )
                    END-EXEC
               WHEN SGTS0207
                    EXEC CICS PUT
                              CONTAINER ( NM-CTNR                 )
                              FROM      ( SGTS0207-RQSC           )
                              FLENGTH   ( LENGTH OF SGTS0207-RQSC )
                    END-EXEC
               WHEN SGTS0208
                    EXEC CICS PUT
                              CONTAINER ( NM-CTNR                 )
                              FROM      ( SGTS0208-RQSC           )
                              FLENGTH   ( LENGTH OF SGTS0208-RQSC )
                    END-EXEC

               WHEN SGTS0211
                    EXEC CICS PUT
                              CONTAINER ( NM-CTNR                 )
                              FROM      ( SGTS0211-RQSC           )
                              FLENGTH   ( LENGTH OF SGTS0211-RQSC )
                    END-EXEC

               WHEN ANCS0001
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (ANCS0001-DADOS )
                              FLENGTH   (LENGTH OF ANCS0001-DADOS )
                    END-EXEC
               WHEN OTHER
                    SET   ERRO-PROGRAMADO
                                       TO  TRUE
                    MOVE  9004         TO  MSG1-CD-ERRO-TS
                    STRING  'Erro na gravação da requisição do módulo '
                             NM-PGM '.'
                             DELIMITED BY SIZE
                             INTO           MSG1-TX-ERRO-TS
                    PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       982000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       983000-EXCLUIR-RESPOSTA-ERRO    SECTION.
      *-----------------------------------------------------------------
      * Exclui o container de resposta e de erro para evitar problemas *
      * em modulos executados mais de uma vez dentro da mesma sessao do*
      * CICS. Equivale ao initialize na area de resposta dos programas *
      * que utilizam COMMAREA.                                         *
      *-----------------------------------------------------------------


      *    Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *    nome_modulo -RPST
           EVALUATE NM-PGM
               WHEN ANCS0001
                    MOVE 'PRM-DADOS-RPST'
                                       TO NM-CTNR
               WHEN OTHER
                    MOVE NM-PGM        TO NM-CTNR(1:8)
                    MOVE '-RPST'       TO NM-CTNR(9:5)
           END-EVALUATE

           SET DEL-CONTAINER           TO TRUE

      *    Exclui o container de resposta
      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS DELETE
                     CONTAINER ( NM-CTNR    )
                     RESP      ( MSG1-RESP1 )
                     RESP2     ( MSG1-RESP2 )
           END-EXEC

           EVALUATE TRUE
               WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
               WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    SET ERRO-CONTAINER TO TRUE
                    PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE

      *    Excluir o container de erro
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO
           .
       983000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       983100-EXCLUIR-CONTAINER-ERRO   SECTION.
      *-----------------------------------------------------------------

      *    Limpar variáveis container de erro
           INITIALIZE IIBKERRO-PRM-ERRO

      *    Exclui o container de erro
           MOVE CTNR-PRM-ERRO          TO NM-CTNR
           SET DEL-CONTAINER           TO TRUE

      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS DELETE
                     CONTAINER ( NM-CTNR    )
                     RESP      ( MSG1-RESP1 )
                     RESP2     ( MSG1-RESP2 )
           END-EXEC

           EVALUATE TRUE
               WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
               WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    SET ERRO-CONTAINER TO TRUE
                    PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       983100-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *    As sections abaixo iniciadas com 999000 representam os
      *    tratamentos de erro utilizando o Sistema MSG
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------

           IF  MSG1-CD-ERRO-TS             EQUAL  ZEROS
               MOVE  8888              TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS             EQUAL  SPACES
               MOVE  'Erro não especificado no módulo.'
                                       TO  MSG1-TX-ERRO-TS
           END-IF

      *--> Em caso de erro, desfaz as atualizações em banco de dados
           EXEC CICS SYNCPOINT
                     ROLLBACK
           END-EXEC

      *--> Verifica se o tipo de erro gerado possui tratamento interno
           EVALUATE TRUE
               WHEN ERRO-CONTAINER
                    PERFORM 999004-TRATA-ERRO-CTNR
               WHEN ERRO-TAMANHO-AREA
                    PERFORM 999005-TRATA-ERRO-AREA-DADOS
               WHEN ERRO-CANAL
                    PERFORM 999006-TRATA-ERRO-CANAL
               WHEN OTHER
                    CONTINUE
           END-EVALUATE

      *--> Adiciona as Variáveis de Entrada para Monitoração
           MOVE '0201-001-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0201-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE '0201-002-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
           IF  SGTS0201-CD-OPR-ETRD-MCP    NUMERIC
               MOVE  SGTS0201-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP
               MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE '0201-003-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0201-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

      *--> Adiciona as Variáveis Internas para Monitoração
           MOVE '0201-004-IC'          TO  MSGKW999-NM-VRV-MNT
           MOVE  IC                    TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE '0201-005-IC-INTC'     TO  MSGKW999-NM-VRV-MNT
           MOVE  IC-INTC               TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE '0201-006-QT-REG-SEL'  TO  MSGKW999-NM-VRV-MNT
           MOVE  QT-REG-SEL            TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE '0201-007-QT-AA-DRCO-OPR'
                                       TO  MSGKW999-NM-VRV-MNT
           IF  QT-AA-DRCO-OPR              NUMERIC
               MOVE  QT-AA-DRCO-OPR    TO  VL-04V02
               MOVE  VL-04V02          TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE '0201-008-VL-EMS-TIT-OPR'
                                       TO  MSGKW999-NM-VRV-MNT
           IF  VL-EMS-TIT-OPR              NUMERIC
               MOVE  VL-EMS-TIT-OPR    TO  VL-15V02
               MOVE  VL-15V02          TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE '0201-009-SUM-PC-TARF-OPR'
                                       TO  MSGKW999-NM-VRV-MNT
           IF  SUM-PC-TARF-OPR             NUMERIC
               MOVE  SUM-PC-TARF-OPR   TO  VL-04V10
               MOVE  VL-04V10          TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE '0201-010-CD-ERRO-CLC'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  CD-ERRO-CLC           TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE '0201-011-EXISTENCIA-REGISTRO'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  IN-EXISTENCIA-REGISTRO
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE '0201-012-IN-EXISTE-REG'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  IN-EXISTE-REG         TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE '0201-013-CD-TIP-IN'   TO  MSGKW999-NM-VRV-MNT
           MOVE  CD-TIP-IN             TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE '0201-014-CD-TIP-FUC-CLI'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  CD-TIP-FUC-CLI
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE '0201-015-CD-CLI'      TO  MSGKW999-NM-VRV-MNT
           MOVE  CD-CLI                TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT


      *--> Grava a Ocorrência de Erro no sistema MSG - Mensageria
           PERFORM MSG1-NTF-MSG

           MOVE  MSG1-CD-ERRO-TS       TO  IIBKERRO-CD-ERRO
           MOVE  MSG1-TX-ERRO-TS       TO  IIBKERRO-TX-ERRO

      *--> Grava a Ocorrência de Erro no Container de erro
           EXEC CICS  PUT
                      CONTAINER ( 'PRM-ERRO'                  )
                      FROM      ( IIBKERRO-PRM-ERRO           )
                      FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                      RESP      ( MSG1-RESP1                  )
                      RESP2     ( MSG1-RESP2                  )
           END-EXEC

      *--> Encerra a transacao
           PERFORM 999999-ENCERRAR
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
      *---------------------- Fim SGTS0201 -----------------------------