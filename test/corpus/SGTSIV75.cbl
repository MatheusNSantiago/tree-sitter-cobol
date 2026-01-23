      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSIV75
      * ANALISTA..: F0950413 - António Francisco Mendonça Silva
      * AUTOR.....: F0950413 - António Francisco Mendonça Silva
      * DATA......: 09/11/2020
      * OBJETIVO..: Inclusão container na tabela ORD_NEGO_OPR
      * COMPILACAO: 52 - PSOSE600 - Cobol 4.2 c/otimizacao p/producao
      *-----------------------------------------------------------------
      * VRS001 09.11.2020 - F0950413 - 2020/155748 - Implantação
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTSIV75.

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

       77  CTE-INICIO-SS               PIC  X(40)  VALUE
                                       '*** S.S. COMECA AQUI ***'.

       77  CTE-PROG                    PIC X(18) VALUE
                                       '*** SGTSIV75 ***'.
       77  CTE-VERS                    PIC X(06) VALUE 'VRS001'.

      *--> Book do programa SBDATA para validacao de datas
       77  SBDATAF                     PIC  X(008) VALUE 'SBDATAF'.

      *--> Book do programa SBTIMEDT para obter timestamp atual
       77  SBTIMEDT                    PIC  X(08)   VALUE 'SBTIMEDT'.

      *--> Definição dos Nomes dos Programas Acionados
       01  TAB-PROGRAMAS-CHAMADOS.
           03  SGTS014G                PIC  X(08) VALUE 'SGTS014G'.

      *--> Definição dos Variaveis do Cobolito
-INC ALMKCBL1

      *--> Book do programa de ambiente - CICS5000
      *    Definido na working para preservar as informaçoes
-INC HLPKE01B

      *-----------------------------------------------------------------
      *                            OUTROS
      *-----------------------------------------------------------------

      *--> Constante informada para se obter timestamp atual do sistema
       77  OBTER-TS-ATUAL              PIC X(08) VALUE 'CURRENT '.

      *--> Constante informada para se obter data atual do sistema
       77  OBTER-DATA-ATUAL           PIC X(08) VALUE 'CURRENT '.


      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

      *--> Auxiliares para movimentação na monitoração de variáveis.
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).

      *--> Armazena a data emissão da ordem de negociação
       01  DT-EMS-ORD-NEGO            PIC  X(10)  VALUE '00.00.0000'.
       01  DT-EMS-ORD-NEGO-R REDEFINES DT-EMS-ORD-NEGO.
           03  DD                      PIC  9(02).
           03  FILLER                  PIC  X(01).
           03  MM                      PIC  9(02).
           03  FILLER                  PIC  X(01).
           03  AA                      PIC  9(04).

      *--> Armazena a data estimativa da taxa de negociação de venda
       01  DT-ESTV-TAXA-VND            PIC  X(10)  VALUE '00.00.0000'.
       01  DT-ESTV-TAXA-VND-R REDEFINES DT-ESTV-TAXA-VND.
           03  DD                      PIC  9(02).
           03  FILLER                  PIC  X(01).
           03  MM                      PIC  9(02).
           03  FILLER                  PIC  X(01).
           03  AA                      PIC  9(04).

       01  DT-VND                      PIC  X(10)  VALUE '00.00.0000'.
       01  DT-VND-R                    REDEFINES DT-VND.
           03  DD                      PIC  9(02).
           03  FILLER                  PIC  X(01).
           03  MM                      PIC  9(02).
           03  FILLER                  PIC  X(01).
           03  AA                      PIC  9(04).

      *--> Indicador da Data Verificada pelo SBDATAF
       01  AX-IND-DT                   PIC  X(01)  VALUE SPACES.
           88  DATA-EMISSAO                        VALUE 'I'.
           88  DATA-ESTIMATIVA-TAXA                VALUE 'T'.
           88  DATA-VENDA                          VALUE 'V'.

      *-----------------------------------------------------------------
      *                 AREA DA SUBROTINA SBDATAF
      *-----------------------------------------------------------------

      *--> Campos utilizados para validação de data Gregoriana.
       01  SBDATAF-FUNCAO-01           PIC  X(003) VALUE 'F01'.
       01  SBDATAF-ARG01-01            PIC  9(008) VALUE 000.
       01  SBDATAF-ARG01-01-R          REDEFINES  SBDATAF-ARG01-01.
           03  DD                      PIC  9(002).
           03  MM                      PIC  9(002).
           03  AA                      PIC  9(004).
       01  SBDATAF-ARG02-01            PIC  9(001) VALUE 004.

       01  AREAWORK                    PIC  X(4095) VALUE SPACES.
       01  AREA-RED REDEFINES AREAWORK.
           03  A-TAM    PIC 9(0008) COMP.
           03  FILLER   PIC X(4071).
           03  A-FIM    PIC X(0020).

      *--> Variáveis para gravação do log
       01  DADOS-LOG-DISPLAY.
           03  CD-CTRA-TIT-VL-LOG      PIC S9(09)       VALUE 0.
           03  CD-OPR-ETRD-MCP-LOG     PIC S9(11)       VALUE 0.
           03  NR-VRS-OPR-ETRD-LOG     PIC S9(04)       VALUE 0.
           03  NR-SEQL-ORD-NEGO-LOG    PIC S9(09)       VALUE 0.

      *--> Campos utilizados para obtencao de timestamp atual
       01  DADOS-SBTIMEDT.
           03  HLPKE001-CD-FUC         PIC X(03) VALUE 'F01'.
           03  HLPKE001-AREA-TRB.
               05  HLPKE001-AREA-TRB-TAM
                                       PIC S9(9) COMP VALUE 1024.
               05  HLPKE001-AREA-TRB-DADOS
                                       PIC  X(1024) VALUE ' '.
           03  HLPKE001-FMT-TS         PIC S9(4) COMP VALUE 1.
           03  HLPKE001-TAM-TS         PIC S9(4) COMP VALUE 30.
           03  HLPKE001-TS-FMTD.
               05  AA                  PIC  9(04).
               05  FILLER              PIC  X(01).
               05  MM                  PIC  9(02).
               05  FILLER              PIC  X(01).
               05  DD                  PIC  9(02).
               05  FILLER              PIC  X(01).
               05  HH                  PIC  X(02).
               05  FILLER              PIC  X(01).
               05  MNTO                PIC  X(02).
               05  FILLER              PIC  X(01).
               05  SGDO                PIC  X(02).
               05  FILLER              PIC  X(01).
               05  MSGD                PIC  X(06).

       01  TS-SQL-ATU                  PIC  X(26)
           VALUE '0000-00-00-00.00.00.000000'.
       01  TS-SQL-ATU-R
           REDEFINES TS-SQL-ATU.
           03  AA                      PIC  9(04).
           03  FILLER                  PIC  X(01).
           03  MM                      PIC  9(02).
           03  FILLER                  PIC  X(01).
           03  DD                      PIC  9(02).
           03  FILLER                  PIC  X(01).
           03  HH                      PIC  X(02).
           03  FILLER                  PIC  X(01).
           03  MNTO                    PIC  X(02).
           03  FILLER                  PIC  X(01).
           03  SGDO                    PIC  X(02).
           03  FILLER                  PIC  X(01).
           03  MSGD                    PIC  X(06).

      *--> Quantidade de registros obtidos na consulta
       01  QT-REG-SELD                 PIC  S9(4) COMP-5 VALUE 0.

      *--> Indicadores de nulidades
       01  CD-ARQ-OPR-ETRD-N           PIC S9(4) COMP-5 VALUE 0.
       01  CD-MTV-NEGO-VND-N           PIC S9(4) COMP-5 VALUE 0.
       01  CD-TIP-APL-TAXA-N           PIC S9(4) COMP-5 VALUE 0.
       01  CD-IEC-TAXA-VND-N           PIC S9(4) COMP-5 VALUE 0.
       01  DT-EMS-ORD-NEGO-N           PIC S9(4) COMP-5 VALUE 0.
       01  DT-ESTV-TAXA-VND-N          PIC S9(4) COMP-5 VALUE 0.
       01  VL-PU-PAPL-VND-N            PIC S9(4) COMP-5 VALUE 0.
       01  VL-TAXA-NEGO-VND-N          PIC S9(4) COMP-5 VALUE 0.
       01  TX-JST-VND-OPR-N            PIC S9(4) COMP-5 VALUE 0.
       01  TX-OBS-ORD-NEGO-N           PIC S9(4) COMP-5 VALUE 0.
       01  DT-VND-N                    PIC S9(4) COMP-5 VALUE 0.
       01  TX-MSG-ORD-NEGO-N           PIC S9(4) COMP-5 VALUE 0.
       01  QT-TTL-PAPL-N               PIC S9(4) COMP-5 VALUE 0.
       01  QT-PAPL-CMPD-N              PIC S9(4) COMP-5 VALUE 0.
       01  QT-PAPL-LVRE-N              PIC S9(4) COMP-5 VALUE 0.

      *--> Auxiliar para o monitoramento de variáveis
       01  VL-PU-PAPL-VND.
           03  PC-PU-PAPL-DISPLAY      PIC  ----.---.---.---.--9,99.

       01  VL-TAXA-NEGO-VND.
           03  VL-TAXA-NEGO-DISPLAY    PIC  ----.---.---.---.--9,99.

       01  MCR-S9-15                   PIC  ---------------9.

      *--> Registro da tabela
       01  TX-OBS-ORD-NEGO.
           49  TX-OBS-ORD-NEGO-LEN     PIC S9(004) COMP-5 VALUE 0.
           49  TX-OBS-ORD-NEGO-TXT     PIC  X(400)        VALUE ' '.

      *--> Variaveis para tratamento de CONTAINER
       01  GRP-CONTAINER.
           03  NM-PGM                  PIC  X(08) VALUE SPACES.
           03  NM-CNL                  PIC  X(16) VALUE SPACES.
           03  QT-CCT-LIDO             PIC S9(09) COMP-5 VALUE ZEROS.
           03  QT-CCT-EPRO             PIC S9(09) COMP-5 VALUE ZEROS.
           03  VL-EXBR-1               PIC  9(09) VALUE ZEROS.
           03  VL-EXBR-2               PIC  9(09) VALUE ZEROS.

      *--> Book do container de erro
-INC IIBKERRO

      *--> Book para captura de erro via MSG1
-INC MSGKWMG1

      *--> Variaveis para tratamento de Abend e Condition
-INC HLPKW999

      *--> Book dos dados do log
       01  DADOS-LOG.
-INC SGTK014G

      *--> Book do programa SGTSIV75
       01  SGTSIV75-DADOS.
-INC SGTKIV75

      *--> Area de trabalho para programas que utilizam SQL (DB2).
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

       01  CTE-FINAL-SS                PIC  X(40) VALUE
                                       '*** S.S. TERMINA AQUI ***'.

      *-----------------------------------------------------------------
       PROCEDURE                       DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------
           MOVE  0001                  TO  MSG1-CD-SEQL-ERRO

           IF  NOT EXECUCAO-ONLINE  AND
               NOT EXECUCAO-BATCH
               CALL  CICS5000       USING  CICS5000-COMMAREA
           END-IF

      *--> Habilita Testes no Cobolito e Debug de Operacoes IIB
-INC ALMKCBL2

      *--> Habilita tratamento automatico de Erros DB2
-INC HLPKE997

      *--> Habilita captura automatica de abend e erros CICS
-INC HLPKE999

           PERFORM  100000-INICIAR

           PERFORM  200000-INCLUIR

           PERFORM  300000-ENCERRAR

           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-INICIAR                  SECTION.
      *-----------------------------------------------------------------
           MOVE  0002                  TO  MSG1-CD-SEQL-ERRO

      *--> Define o nome do programa p/ fins manipulação container
           MOVE 'SGTSIV75'             TO  NM-PGM

           PERFORM  110000-DESCOBRIR-CANAL

           PERFORM  120000-EXCLUIR-CTNR-ERRO

           PERFORM  130000-RECEBER-REQUISICAO

           PERFORM  140000-BUSCA-TS-DT

           PERFORM  150000-VALIDA-CAMPOS

           PERFORM  160000-TRATA-NULIDADE

           PERFORM  170001-VLDR-OPR-ETRD-MCP

           IF  SGTSIV75-CD-ARQ-OPR-ETRD    POSITIVE
               PERFORM  170002-VLDR-ARQ-OPR-ETRD
           END-IF

           PERFORM  170003-VLDR-MOD-ORD-NEGO

           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-DESCOBRIR-CANAL          SECTION.
      *-----------------------------------------------------------------
           MOVE  0003                  TO  MSG1-CD-SEQL-ERRO

           EXEC CICS  ASSIGN
                      CHANNEL ( NM-CNL )
           END-EXEC

           .
       110000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       120000-EXCLUIR-CTNR-ERRO        SECTION.
      *-----------------------------------------------------------------
           MOVE  0004                  TO  MSG1-CD-SEQL-ERRO

           EXEC CICS  DELETE
                      CONTAINER ( 'PRM-ERRO' )
                      CHANNEL   ( NM-CNL     )
                      NOHANDLE
           END-EXEC

           IF  NOT (EIBRESP                EQUAL  DFHRESP(NORMAL)  OR
                    EIBRESP                EQUAL  DFHRESP(CONTAINERERR))
               PERFORM  999002-TRATA-CONDITION-CICS
           END-IF
           .
       120000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       130000-RECEBER-REQUISICAO       SECTION.
      *-----------------------------------------------------------------
           MOVE  0005                  TO  MSG1-CD-SEQL-ERRO

           MOVE  ZEROS                 TO  QT-CCT-LIDO
           MOVE  LENGTH OF SGTSIV75-RQSC
                                       TO  QT-CCT-EPRO

           EXEC CICS  GET
                      CONTAINER ( 'SGTSIV75-RQSC' )
                      CHANNEL   ( NM-CNL          )
                      NODATA
                      FLENGTH   ( QT-CCT-LIDO     )
           END-EXEC

           IF  QT-CCT-LIDO                 NOT EQUAL  QT-CCT-EPRO
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  9001              TO  MSG1-CD-ERRO-TS
               MOVE  QT-CCT-EPRO       TO  VL-EXBR-1
               MOVE  QT-CCT-LIDO       TO  VL-EXBR-2
               STRING  'Erro no tamanho de área de requisição do '
                       NM-PGM
                       '. Tamanho Esperado: '  VL-EXBR-1
                       ', Tamanho Recebido: '  VL-EXBR-2
                       DELIMITED BY SIZE
                       INTO  MSG1-TX-ERRO-TS
               END-STRING
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS  GET
                      CONTAINER ( 'SGTSIV75-RQSC'         )
                      CHANNEL   ( NM-CNL                  )
                      FLENGTH   ( LENGTH OF SGTSIV75-RQSC )
                      INTO      ( SGTSIV75-RQSC           )
           END-EXEC

           .
       130000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       140000-BUSCA-TS-DT              SECTION.
      *-----------------------------------------------------------------
           MOVE  0006                  TO  MSG1-CD-SEQL-ERRO

           CALL SBTIMEDT            USING  HLPKE001-CD-FUC
                                           HLPKE001-AREA-TRB
                                           HLPKE001-FMT-TS
                                           HLPKE001-TAM-TS
                                           HLPKE001-TS-FMTD

           IF  RETURN-CODE             NOT EQUAL ZEROS
               SET   ERRO-PROGRAMA-ACIONADO  TO  TRUE
               MOVE  0050              TO  MSG1-CD-ERRO-TS
               MOVE  'Erro ao obter data e hora atual.'
                                       TO  MSG1-TX-ERRO-TS
                                           MSG1-TX-RTN-PGM-ACND
               MOVE  SBTIMEDT          TO  MSG1-NM-PGM-ACND
                                           NM-PGM
               MOVE  RETURN-CODE       TO  MSG1-CD-RTN-PGM-ACND
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           MOVE      HLPKE001-TS-FMTD  TO  TS-SQL-ATU
           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       150000-VALIDA-CAMPOS            SECTION.
      *-----------------------------------------------------------------
           MOVE  0007                  TO  MSG1-CD-SEQL-ERRO

      *--> Valida Campos Chave da Operação Estruturada
           IF  SGTSIV75-CD-CTRA-TIT-VL     NEGATIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0001              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' inválido.'      TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSIV75-CD-CTRA-TIT-VL     EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0002              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de títulos de valores mobiliári
      -              'os não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSIV75-CD-OPR-ETRD-MCP    NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0003              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada mercado de capitais nã
      -              'o numérico.'     TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSIV75-CD-OPR-ETRD-MCP    NEGATIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0004              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada mercado de capitais in
      -              'válido.'         TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSIV75-CD-OPR-ETRD-MCP    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0005              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada mercado de capitais nã
      -              'o informado.'    TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSIV75-NR-VRS-OPR-ETRD    NEGATIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0006              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação inválildo.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSIV75-NR-VRS-OPR-ETRD    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0007              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *--> Valida Campos da Ordem de Negociação
           IF  SGTSIV75-NR-SEQL-ORD-NEGO   NEGATIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  1084              TO  MSG1-CD-ERRO-TS
               MOVE  'Número sequencial da ordem de negociação inválido.
      -              ''                TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSIV75-NR-SEQL-ORD-NEGO   EQUAL ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0542              TO  MSG1-CD-ERRO-TS
               MOVE  'Número sequencial da ordem da negociação não infor
      -              'mado.'           TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSIV75-CD-USU-ULT-ATL     EQUAL  SPACES
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0651              TO  MSG1-CD-ERRO-TS
               MOVE  'Código do usuário responsável pela ultima atualiza
      -              'ção não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSIV75-TS-ULT-ATL         EQUAL  SPACES
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0106              TO  MSG1-CD-ERRO-TS
               MOVE  'Timestamp ultima atualização não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSIV75-CD-MOD-ORD-NEGO    NEGATIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  1019              TO  MSG1-CD-ERRO-TS
               MOVE  'Código do modelo de ordem de negociação inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSIV75-CD-MOD-ORD-NEGO    EQUAL ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0641              TO  MSG1-CD-ERRO-TS
               MOVE  'Código do modelo de ordem de negociação não inform
      -              'ado.'            TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF


      *--> Validação de datas
      *    Verifica se o sistema enviou a palavra CURRENT; sendo
      *    necessário obter a data atual do sistema.
           IF  SGTSIV75-DT-EMS-ORD-NEGO    EQUAL  OBTER-DATA-ATUAL
               MOVE  CORR HLPKE001-TS-FMTD
                                       TO  DT-EMS-ORD-NEGO-R
               MOVE  CORR DT-EMS-ORD-NEGO-R
                                       TO  SGTSIV75-DT-EMS-ORD-NEGO-R
           END-IF
           IF  SGTSIV75-DT-EMS-ORD-NEGO    NOT EQUAL SPACES
               MOVE  SGTSIV75-DT-EMS-ORD-NEGO
                                       TO  SBDATAF-ARG01-01
               SET   DATA-EMISSAO      TO  TRUE
               PERFORM  910000-CHAMA-SBDATAF-01
      *-->     MOVE '00.00.0000'       TO  DT-EMS-ORD-NEGO
               MOVE  CORR SGTSIV75-DT-EMS-ORD-NEGO-R
                                       TO  DT-EMS-ORD-NEGO-R
           END-IF

      *--> Verifica se o sistema enviou a palavra CURRENT; sendo
      *--> necessário obter a data atual do sistema.
           IF  SGTSIV75-DT-ESTV-TAXA-VND   EQUAL  OBTER-DATA-ATUAL
               MOVE  CORR HLPKE001-TS-FMTD
                                       TO  DT-ESTV-TAXA-VND-R
               MOVE  CORR DT-ESTV-TAXA-VND-R
                                       TO  SGTSIV75-DT-ESTV-TAXA-VND-R
           END-IF
           IF  SGTSIV75-DT-ESTV-TAXA-VND   NOT EQUAL  SPACES
               MOVE  SGTSIV75-DT-ESTV-TAXA-VND
                                       TO  SBDATAF-ARG01-01
               SET   DATA-ESTIMATIVA-TAXA
                                       TO  TRUE
               PERFORM  910000-CHAMA-SBDATAF-01
      *-->     MOVE  '00.00.0000'      TO  DT-ESTV-TAXA-VND
               MOVE  CORR SGTSIV75-DT-ESTV-TAXA-VND-R
                                       TO  DT-ESTV-TAXA-VND-R
           END-IF

           IF  SGTSIV75-DT-VND             NOT EQUAL  SPACES
               MOVE  SGTSIV75-DT-VND   TO  SBDATAF-ARG01-01
               SET   DATA-VENDA        TO  TRUE
               PERFORM  910000-CHAMA-SBDATAF-01
      *-->     MOVE '00.00.0000'       TO  DT-ESTV-TAXA-VND
               MOVE  CORR SGTSIV75-DT-VND-R
                                       TO  DT-VND-R
           END-IF

           IF  SGTSIV75-VL-PU-PAPL-VND     NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0642              TO  MSG1-CD-ERRO-TS
               MOVE  'Valor do preço único do papel de venda não numéric
      -              'o.'               TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSIV75-VL-TAXA-NEGO-VND   NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0645              TO  MSG1-CD-ERRO-TS
               MOVE  'Valor da taxa de negociação de venda não numérico.
      -              ''                TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           MOVE  ZEROS                 TO  TX-OBS-ORD-NEGO-LEN

           INSPECT FUNCTION REVERSE(SGTSIV75-TX-OBS-ORD-NEGO)
                   TALLYING TX-OBS-ORD-NEGO-LEN
                   FOR LEADING SPACES

           COMPUTE  TX-OBS-ORD-NEGO-LEN =
                    LENGTH OF SGTSIV75-TX-OBS-ORD-NEGO -
                    TX-OBS-ORD-NEGO-LEN
                    ON SIZE ERROR
                       SET   ERRO-PROGRAMADO
                                       TO  TRUE
                       MOVE  0143      TO  MSG1-CD-ERRO-TS
                       MOVE  'Erro no calculo do tamanho do texto.'
                                       TO  MSG1-TX-ERRO-TS
                       PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-COMPUTE

           MOVE  SGTSIV75-TX-OBS-ORD-NEGO
                                       TO  TX-OBS-ORD-NEGO-TXT
           .
       150000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       160000-TRATA-NULIDADE           SECTION.
      *-----------------------------------------------------------------
           MOVE  0008                  TO  MSG1-CD-SEQL-ERRO

           IF  SGTSIV75-CD-ARQ-OPR-ETRD    EQUAL ZEROS
               MOVE  -001              TO  CD-ARQ-OPR-ETRD-N
           END-IF

           IF  SGTSIV75-CD-MTV-NEGO-VND    EQUAL ZEROS
               MOVE  -001              TO  CD-MTV-NEGO-VND-N
           END-IF

           IF  SGTSIV75-CD-TIP-APL-TAXA    EQUAL ZEROS
               MOVE  -001              TO  CD-TIP-APL-TAXA-N
           END-IF

           IF  SGTSIV75-CD-IEC-TAXA-VND    EQUAL ZEROS
               MOVE  -001              TO  CD-IEC-TAXA-VND-N
           END-IF

           IF  SGTSIV75-DT-EMS-ORD-NEGO    EQUAL SPACES
               MOVE  -001              TO  DT-EMS-ORD-NEGO-N
           END-IF

           IF  SGTSIV75-DT-ESTV-TAXA-VND   EQUAL SPACES
               MOVE  -001              TO  DT-ESTV-TAXA-VND-N
           END-IF

           IF  SGTSIV75-VL-PU-PAPL-VND     EQUAL ZEROS
               MOVE  -001              TO  VL-PU-PAPL-VND-N
           END-IF

           IF  SGTSIV75-VL-TAXA-NEGO-VND   EQUAL ZEROS
               MOVE  -001              TO  VL-TAXA-NEGO-VND-N
           END-IF

           IF  SGTSIV75-TX-JST-VND-OPR     EQUAL SPACES
               MOVE  -001              TO  TX-JST-VND-OPR-N
           END-IF

           IF  SGTSIV75-TX-OBS-ORD-NEGO    EQUAL SPACES
               MOVE  -001              TO  TX-OBS-ORD-NEGO-N
           END-IF

           IF  SGTSIV75-DT-VND             EQUAL SPACES
               MOVE  -001              TO  DT-VND-N
           END-IF

           IF  SGTSIV75-TX-MSG-ORD-NEGO    EQUAL SPACES
               MOVE  -001              TO  TX-MSG-ORD-NEGO-N
           END-IF

           IF  SGTSIV75-QT-TTL-PAPL        EQUAL ZEROES
               MOVE  -001              TO  QT-TTL-PAPL-N
           END-IF

           IF  SGTSIV75-QT-PAPL-CMPD       EQUAL ZEROES
               MOVE  -001              TO  QT-PAPL-CMPD-N
           END-IF

           IF  SGTSIV75-QT-PAPL-LVRE       EQUAL ZEROES
               MOVE  -001              TO  QT-PAPL-LVRE-N
           END-IF
           .
       160000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       170001-VLDR-OPR-ETRD-MCP        SECTION.
      *-----------------------------------------------------------------
           MOVE  0009                  TO  MSG1-CD-SEQL-ERRO

           MOVE ZEROS                  TO  QT-REG-SELD
           EXEC SQL
                SELECT 1
                  INTO :QT-REG-SELD
                  FROM DB2SGT.OPR_ETRD_MCP
                 WHERE CD_CTRA_TIT_VL   =  :SGTSIV75-CD-CTRA-TIT-VL
                   AND CD_OPR_ETRD_MCP  =  :SGTSIV75-CD-OPR-ETRD-MCP
                   AND NR_VRS_OPR_ETRD  =  :SGTSIV75-NR-VRS-OPR-ETRD
                 FETCH FIRST 1 ROW ONLY
           END-EXEC

           IF  QT-REG-SELD EQUAL ZEROS
               MOVE  0061              TO  MSG1-CD-ERRO-TS
               MOVE  'Operação estruturada inexistente.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       170001-SAI.
           EXIT.

      *-----------------------------------------------------------------
       170002-VLDR-ARQ-OPR-ETRD        SECTION.
      *-----------------------------------------------------------------
           MOVE  0010                  TO  MSG1-CD-SEQL-ERRO

           MOVE ZEROS                  TO  QT-REG-SELD
           EXEC SQL
                SELECT 1
                  INTO :QT-REG-SELD
                  FROM DB2SGT.ARQ_OPR_ETRD
                 WHERE CD_ARQ_OPR_ETRD  =  :SGTSIV75-CD-ARQ-OPR-ETRD
                 FETCH FIRST 1 ROW ONLY
           END-EXEC

           IF  QT-REG-SELD EQUAL ZEROS
               MOVE  0151              TO  MSG1-CD-ERRO-TS
               MOVE  'Arquivo da operação estruturada inexistente.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       170002-SAI.
           EXIT.

      *-----------------------------------------------------------------
       170003-VLDR-MOD-ORD-NEGO        SECTION.
      *-----------------------------------------------------------------
           MOVE  0011                  TO  MSG1-CD-SEQL-ERRO

           MOVE  ZEROS                 TO  QT-REG-SELD
           EXEC SQL
                SELECT 1
                  INTO :QT-REG-SELD
                  FROM DB2SGT.MOD_ORD_NEGO
                 WHERE CD_MOD_ORD_NEGO  =  :SGTSIV75-CD-MOD-ORD-NEGO
                 FETCH FIRST 1 ROW ONLY
           END-EXEC

           IF  QT-REG-SELD EQUAL ZEROS
               MOVE  0643              TO  MSG1-CD-ERRO-TS
               MOVE 'Não há registro correspondente vinculado na tabela
      -             'modelo ordem negociação.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       170003-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-INCLUIR                  SECTION.
      *-----------------------------------------------------------------
           MOVE  0012                  TO  MSG1-CD-SEQL-ERRO

      *--> Desligar erro automático do DB2
           EXEC SQL
                WHENEVER SQLERROR CONTINUE
           END-EXEC

           EXEC SQL
                INSERT INTO DB2SGT.ORD_NEGO_OPR
                           ( CD_CTRA_TIT_VL
                           , CD_OPR_ETRD_MCP
                           , NR_VRS_OPR_ETRD
                           , NR_SEQL_ORD_NEGO
                           , CD_USU_ULT_ATL
                           , TS_ULT_ATL
                           , CD_MOD_ORD_NEGO
                           , CD_ARQ_OPR_ETRD
                           , CD_MTV_NEGO_VND
                           , CD_TIP_APL_TAXA
                           , CD_IEC_TAXA_VND
                           , DT_EMS_ORD_NEGO
                           , DT_ESTV_TAXA_VND
                           , VL_PU_PAPL_VND
                           , VL_TAXA_NEGO_VND
                           , TX_JST_VND_OPR
                           , TX_OBS_ORD_NEGO
                           , TX_MSG_ORD_NEGO
                           , DT_VND
                           , QT_TTL_PAPL
                           , QT_PAPL_CMPD
                           , QT_PAPL_LVRE
                           )
                VALUES
                           ( :SGTSIV75-CD-CTRA-TIT-VL
                           , :SGTSIV75-CD-OPR-ETRD-MCP
                           , :SGTSIV75-NR-VRS-OPR-ETRD
                           , :SGTSIV75-NR-SEQL-ORD-NEGO
                           , :SGTSIV75-CD-USU-ULT-ATL
                           , :SGTSIV75-TS-ULT-ATL
                           , :SGTSIV75-CD-MOD-ORD-NEGO
                           , :SGTSIV75-CD-ARQ-OPR-ETRD
                             :CD-ARQ-OPR-ETRD-N
                           , :SGTSIV75-CD-MTV-NEGO-VND
                             :CD-MTV-NEGO-VND-N
                           , :SGTSIV75-CD-TIP-APL-TAXA
                             :CD-TIP-APL-TAXA-N
                           , :SGTSIV75-CD-IEC-TAXA-VND
                             :CD-IEC-TAXA-VND-N
                           , :DT-EMS-ORD-NEGO
                             :DT-EMS-ORD-NEGO-N
                           , :DT-ESTV-TAXA-VND
                             :DT-ESTV-TAXA-VND-N
                           , :SGTSIV75-VL-PU-PAPL-VND
                             :VL-PU-PAPL-VND-N
                           , :SGTSIV75-VL-TAXA-NEGO-VND
                             :VL-TAXA-NEGO-VND-N
                           , :SGTSIV75-TX-JST-VND-OPR
                             :TX-JST-VND-OPR-N
                           , :TX-OBS-ORD-NEGO
                             :TX-OBS-ORD-NEGO-N
                           , :SGTSIV75-TX-MSG-ORD-NEGO
                             :TX-MSG-ORD-NEGO-N
                           , :DT-VND
                             :DT-VND-N
                           , :SGTSIV75-QT-TTL-PAPL
                             :QT-TTL-PAPL-N
                           , :SGTSIV75-QT-PAPL-CMPD
                             :QT-PAPL-CMPD-N
                           , :SGTSIV75-QT-PAPL-LVRE
                             :QT-PAPL-LVRE-N
                           )
                  END-EXEC

      *--> Ligar erro automatico do DB2
           EXEC SQL WHENEVER SQLERROR
                    GO TO  999010-TRATA-ERRO-DB2
           END-EXEC

           EVALUATE  SQLCODE
               WHEN  -803
                     SET  ERRO-PROGRAMADO
                                       TO  TRUE
                     MOVE  0803        TO  MSG1-CD-ERRO-TS
                     MOVE  'Inclusão não efetuada, registro existente.'
                                       TO  MSG1-TX-ERRO-TS
                     PERFORM  999000-GRAVA-ERRO-E-ENCERRA
               WHEN  ZEROS
                     CONTINUE
               WHEN  OTHER
      *--> Aciona o tratamento padrão de erro para os demais casos
                     PERFORM  999010-TRATA-ERRO-DB2
           END-EVALUATE

           PERFORM  210000-GRAVAR-LOG

           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       210000-GRAVAR-LOG               SECTION.
      *-----------------------------------------------------------------
           MOVE  0013                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      DADOS-LOG

           MOVE  'ORD_NEGO_OPR'        TO  SGTS014G-NM-ENT
           SET   SGTS014G-INCLUSAO     TO  TRUE
           MOVE  SGTSIV75-CD-USU-ULT-ATL
                                       TO  SGTS014G-CD-CHV-USU
           MOVE  'SGTSIV75'            TO  SGTS014G-NM-PGM

           MOVE  SGTSIV75-CD-CTRA-TIT-VL
                                       TO  CD-CTRA-TIT-VL-LOG
           MOVE  SGTSIV75-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP-LOG
           MOVE  SGTSIV75-NR-VRS-OPR-ETRD
                                       TO  NR-VRS-OPR-ETRD-LOG
           MOVE  SGTSIV75-NR-SEQL-ORD-NEGO
                                       TO  NR-SEQL-ORD-NEGO-LOG

           STRING  CD-CTRA-TIT-VL-LOG
              ';'  CD-OPR-ETRD-MCP-LOG
              ';'  NR-VRS-OPR-ETRD-LOG
              ';'  NR-SEQL-ORD-NEGO-LOG
                   DELIMITED BY SIZE
                   INTO                    SGTS014G-CHV-PRMR
           END-STRING

           MOVE  LENGTH OF DADOS-LOG   TO  EIBCALEN

           CALL  SGTS014G           USING  DFHEIBLK DADOS-LOG

           IF  SGTS014G-CD-RTN-PGM         NOT EQUAL  ZEROS
               SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE  0112              TO  MSG1-CD-ERRO-TS
               MOVE  'Erro na gravação das informações do Log.'
                                       TO  MSG1-TX-ERRO-TS
               MOVE  SGTS014G          TO  MSG1-NM-PGM-ACND
                                           NM-PGM
               MOVE  SGTS014G-CD-RTN-PGM
                                       TO  MSG1-CD-RTN-PGM-ACND
               MOVE  SGTS014G-MSG-ERRO-PGM
                                       TO  MSG1-TX-RTN-PGM-ACND
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       210000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-ENCERRAR                 SECTION.
      *-----------------------------------------------------------------
           MOVE  0014                  TO  MSG1-CD-SEQL-ERRO

           PERFORM  310000-POSTAR-RESPOSTA

           PERFORM  999999-ENCERRAR

           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       310000-POSTAR-RESPOSTA          SECTION.
      *-----------------------------------------------------------------
           MOVE  0015                  TO  MSG1-CD-SEQL-ERRO

           MOVE 'Inclusão efetuada com sucesso.'
                                       TO  SGTSIV75-TX-RTN-PRCT
           EXEC CICS PUT
              CONTAINER ( 'SGTSIV75-RPST'         )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSIV75-RPST )
              FROM      ( SGTSIV75-RPST           )
           END-EXEC
           .
       310000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       910000-CHAMA-SBDATAF-01         SECTION.
      *-----------------------------------------------------------------
           MOVE  0016                  TO  MSG1-CD-SEQL-ERRO

           MOVE  4095                  TO  A-TAM
           MOVE  'SBDATAF -WRKAREA-FIM'
                                       TO  A-FIM

           CALL  SBDATAF            USING  SBDATAF-FUNCAO-01
                                           AREAWORK
                                           SBDATAF-ARG01-01
                                           SBDATAF-ARG02-01

           IF  RETURN-CODE                 NOT EQUAL  ZEROS
               SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE  0051              TO  MSG1-CD-ERRO-TS
               MOVE  'Erro na rotina que valida data.'
                                       TO  MSG1-TX-ERRO-TS
               MOVE SBDATAF            TO  MSG1-NM-PGM-ACND
                                           NM-PGM
               MOVE  RETURN-CODE       TO  MSG1-CD-RTN-PGM-ACND

               EVALUATE  TRUE
                   WHEN  DATA-EMISSAO
                         MOVE  1026    TO  MSG1-CD-ERRO-TS
                         MOVE  'Data emissão da ordem de negociação invá
      -                        'lida.'
                                       TO  MSG1-TX-RTN-PGM-ACND
                                           MSG1-TX-ERRO-TS
                   WHEN  DATA-ESTIMATIVA-TAXA
                         MOVE  1027    TO  MSG1-CD-ERRO-TS
                         MOVE  'Data estimada taxa de negociação inválid
      -                        'a.'    TO  MSG1-TX-RTN-PGM-ACND
                                           MSG1-TX-ERRO-TS
                   WHEN  DATA-VENDA
                         MOVE  1077    TO  MSG1-CD-ERRO-TS
                         MOVE  'Data venda inválida.'
                                       TO  MSG1-TX-RTN-PGM-ACND
                                           MSG1-TX-ERRO-TS
               END-EVALUATE

               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       910000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------

           IF  ERRO-PROGRAMA-ACIONADO
               PERFORM 999100-MONITORAR-VRV-PGM-CHMD
           END-IF

           MOVE 'IV75-001-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSIV75-CD-CTRA-TIT-VL
                                       TO  MCR-S9-15
           MOVE  MCR-S9-15             TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'IV75-002-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
      *-----------> Verifica se o conteudo e numerico antes de adicionar
           IF  SGTSIV75-CD-OPR-ETRD-MCP    NUMERIC
               MOVE  SGTSIV75-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP
               MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'IV75-003-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSIV75-NR-VRS-OPR-ETRD
                                       TO  MCR-S9-15
           MOVE  MCR-S9-15             TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'IV75-004-NR-SEQL-ORD-NEGO'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSIV75-NR-SEQL-ORD-NEGO
                                       TO  MCR-S9-15
           MOVE  MCR-S9-15             TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'IV75-005-CD-USU-ULT-ATL'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSIV75-CD-USU-ULT-ATL
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'IV75-006-TS-ULT-ATL'  TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSIV75-TS-ULT-ATL   TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'IV75-007-CD-MOD-ORD-NEGO'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSIV75-CD-MOD-ORD-NEGO
                                       TO  MCR-S9-15
           MOVE  MCR-S9-15             TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'IV75-008-CD-ARQ-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSIV75-CD-ARQ-OPR-ETRD
                                       TO  MCR-S9-15
           MOVE  MCR-S9-15             TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'IV75-009-CD-MTV-NEGO-VND'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSIV75-CD-MTV-NEGO-VND
                                       TO  MCR-S9-15
           MOVE  MCR-S9-15             TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'IV75-010-CD-TIP-APL-TAXA'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSIV75-CD-TIP-APL-TAXA
                                       TO  MCR-S9-15
           MOVE  MCR-S9-15             TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'IV75-011-CD-IEC-TAXA-VND'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSIV75-CD-IEC-TAXA-VND
                                       TO  MCR-S9-15
           MOVE  MCR-S9-15             TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'IV75-012-DT-EMS-ORD-NEGO'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSIV75-DT-EMS-ORD-NEGO
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'IV75-013-DT-ESTV-TAXA-VND'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSIV75-DT-ESTV-TAXA-VND
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'IV75-014-VL-PU-PAPL-VND'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSIV75-VL-PU-PAPL-VND
                                       TO  PC-PU-PAPL-DISPLAY
           MOVE  VL-PU-PAPL-VND        TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'IV75-015-VL-TAXA-NEGO-VND'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSIV75-VL-TAXA-NEGO-VND
                                       TO  VL-TAXA-NEGO-DISPLAY
           MOVE  VL-TAXA-NEGO-VND      TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'IV75-016-TX-JST-VND-OPR'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSIV75-TX-JST-VND-OPR
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'IV75-017-TX-OBS-ORD-NEGO'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSIV75-TX-OBS-ORD-NEGO
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'IV75-018-TX-MSG-ORD-NEGO'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSIV75-TX-MSG-ORD-NEGO
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'IV75-019-DT-VND'      TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSIV75-DT-VND       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'IV75-020-QT-TTL-PAPL' TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSIV75-QT-TTL-PAPL  TO  MCR-S9-15
           MOVE  MCR-S9-15             TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'IV75-021-QT-PAPL-CMPD'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSIV75-QT-PAPL-CMPD
                                       TO  MCR-S9-15
           MOVE  MCR-S9-15             TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'IV75-022-QT-PAPL-LVRE'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSIV75-QT-PAPL-LVRE
                                       TO  MCR-S9-15
           MOVE  MCR-S9-15             TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT


      *--> Grava a Ocorrência de Erro no sistema MSG - Mensageria
           PERFORM  MSG1-NTF-MSG

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
           PERFORM  999999-ENCERRAR
           .
       999000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999100-MONITORAR-VRV-PGM-CHMD   SECTION.
      *--> Monitora as variáveis dos módulos chamados
      *-----------------------------------------------------------------

           EVALUATE  NM-PGM
              WHEN   SGTS014G
                     MOVE  'SGTS014G-CD-TIP-ACAO'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS014G-CD-TIP-ACAO
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTS014G-NM-ENT'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS014G-NM-ENT
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTS014G-CD-CHV-USU'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS014G-CD-CHV-USU
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTS014G-NM-PGM'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS014G-NM-PGM
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTS014G-CHV-PRMR'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS014G-CHV-PRMR
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  SBDATAF
                     MOVE 'SBDATAF-01-FUNCAO'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SBDATAF-FUNCAO-01
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM 999003-ADICIONA-VRV-MNT

                     MOVE 'SBDATAF-02-A-TAM'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  A-TAM       TO  MSGKW999-VL-VRV-MNT
                     PERFORM 999003-ADICIONA-VRV-MNT

                     MOVE 'SBDATAF-03-A-FIM'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  A-FIM       TO  MSGKW999-VL-VRV-MNT
                     PERFORM 999003-ADICIONA-VRV-MNT

                     MOVE 'SBDATAF-04-ARG01-01'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SBDATAF-ARG01-01
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM 999003-ADICIONA-VRV-MNT

                     MOVE 'SBDATAF-05-ARG02-01'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SBDATAF-ARG02-01
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM 999003-ADICIONA-VRV-MNT

               WHEN  SBTIMEDT
                     MOVE  'HLPKE001-CD-FUC'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  HLPKE001-CD-FUC
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'HLPKE001-AREA-TRB'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  HLPKE001-AREA-TRB
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'HLPKE001-FMT-TS'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  HLPKE001-FMT-TS
                                       TO  MCR-S9-15
                     MOVE  MCR-S9-15   TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'HLPKE001-TAM-TS'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  HLPKE001-TAM-TS
                                       TO  MCR-S9-15
                     MOVE  MCR-S9-15   TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'HLPKE001-TS-FMTD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  HLPKE001-TS-FMTD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  OTHER
                     CONTINUE
           END-EVALUATE
           .
       999100-SAI.
           EXIT.

      *----------------------------------------
      * 999001-TRATA-ABEND-CICS        SECTION.
      * 999002-TRATA-CONDITION-CICS    SECTION.
      * 999003-ADICIONA-VRV-MNT        SECTION.
      * 999004-ALINHA-NUMERO-ESQUERDA  SECTION.
      *----------------------------------------
-INC HLPKP999
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

      *---------------------- FIM SGTSIV75 ----------------------------