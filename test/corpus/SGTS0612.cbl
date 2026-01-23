      *-----------------------------------------------------------------
      * PROGRAMA..: SGTS0612
      * ANALISTA..: F9188847 - Soila K. L. Akamatsu
      * AUTOR.....: F9188847 - Soila K. L. Akamatsu
      * DATA......: 16/04/2025
      * OBJETIVO..: Alterar Cronograma de Eventos Excluídos B3
      *             (Câmara de Custódia e Liquidação ).
      * COMPILACAO: 54 - PSOSE600 - Cobol 6.3 c/otimizacao p/producao
      *-----------------------------------------------------------------
      * VRS001 16/04/2025 F9188847 Historia 1852713 - Eventos Exclusos
      *                B3 -  (Amortizações, juros e etc) - Implantacao
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                 DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTS0612.

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

       77  CTE-INICIO-SS               PIC  X(040)  VALUE
                                       '*** S.S. COMEÇA AQUI ***'.

       77  CTE-PROG                    PIC  X(018)  VALUE
                                       '*** SGTS0612 ***'.
       77  CTE-VERS                    PIC  X(006)  VALUE 'VRS001'.

      *--> Definição dos Nomes dos Programas Acionados
       01 TAB-PROGRAMAS-CHAMADOS.
          03  IIBP2000                    PIC  X(08) VALUE 'IIBP2000'.
          03  SGTST138                    PIC  X(08) VALUE 'SGTST138'.
          03  SGTST139                    PIC  X(08) VALUE 'SGTST139'.
          03  SGTST140                    PIC  X(08) VALUE 'SGTST140'.
          03  PRDS0203                    PIC  X(08) VALUE 'PRDS0203'.

      *--> Definição dos Variaveis do Cobolito
-INC ALMKCBL1
      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

      *--> Areas de trabalho
       01  IN-CONTAINER-ERRO           PIC  X(01) VALUE SPACES.
           88  TEM-CONTAINER-ERRO                 VALUE 'S'.
           88  NAO-TEM-CONTAINER-ERRO             VALUE 'N'.

       01  AX-VALOR                    PIC ----.---.---,99999999.

       01  AX-VALOR2                   PIC ----.---.---.---.---,99.

       01  AX-VALOR3                   PIC --.---.---.---.---,99.

       01  CD-OPR-ETRD-MCP-DISPLAY     PIC  9(11) VALUE 0.

      * Comparar datas
       01  DT-PSQ1                     PIC  9(08) VALUE ZEROS.
       01  DT-PSQ1-R                   REDEFINES DT-PSQ1.
           03  ANO                     PIC  9(04).
           03  MES                     PIC  9(02).
           03  DIA                     PIC  9(02).

       01  DT-PSQ2                     PIC  9(08) VALUE ZEROS.
       01  DT-PSQ2-R                   REDEFINES DT-PSQ2.
           03  ANO                     PIC  9(04).
           03  MES                     PIC  9(02).
           03  DIA                     PIC  9(02).

      *    Chamada à subrotina SBDATAF

       01  WKSBDATA                    PIC X(4099).
       01  AREA-RED                    REDEFINES WKSBDATA.
           03  AREA-TAM                PIC 9(0008) COMP.
           03  FILLER                  PIC X(4071).
           03  AREA-FIM                PIC X(0020).

       01  SBDATAF                     PIC  X(08) VALUE 'SBDATAF'.
      * 01  AREA-SBDATAF.
       01  FUNCAO                      PIC  X(03) VALUE 'F01'.
       01  ARG01                       PIC  9(08) VALUE 0.
       01  ARG01-R                     REDEFINES ARG01.
           03  DIA                     PIC  9(02).
           03  MES                     PIC  9(02).
           03  ANO                     PIC  9(04).
       01  ARG02                       PIC  9(01) VALUE 0.

      *-----------------------------------------------------------------
      * Book dos Containers e das subrotinas chamadas
      *-----------------------------------------------------------------

      *--> Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1

      *--> Book para captura de abend cics da bloco codigo HLPKW999
-INC HLPKW999

      *--> Book para captura de erro via MSG1
-INC MSGKWMG1

      *--> Book do container de requisição e resposta deste modulo
      *--> Alterar Evento Excluido B3
-INC SGTK0612


      *--> CRUD EST_EVT_TIT_EXCD
       01  SGTST139-DADOS.
-INC SGTKT139

      *--> Consultar TABELA CRNG_EVT_TIT_EXCD
       01  SGTST138-DADOS.
-INC SGTKT138

      *--> CRUD da tabela PRCT_CRNG_TIT_EXCD
       01 SGTST140-DADOS.
-INC SGTKT140

      *--> Book do Container de Erro
      *--> Consultar Produlo/Modalidade - via IIB
       01 PRDS0203-DADOS.
-INC PRDK0203

-INC IIBKERRO

      *--> Book do container de Serviços (para acionamento de serviços)
-INC IIBKSRVC

      *--> Book dos dados do usuário
-INC IIBKOPRD



       77  CTE-FINAL-SS                PIC X(40) VALUE
                                       '*** S.S. TERMINA AQUI ***'.

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

           PERFORM 999999-ENCERRAR

           GOBACK

           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-INICIAR                  SECTION.
      *-----------------------------------------------------------------
           MOVE 0001                   TO MSG1-CD-SEQL-ERRO

      * Habilita tratamento automatico de Erros DB2
      * -INC HLPKE997

      *--> Ativa a captura automatica de abend do CICS.
      *    WHENEVER CICS
-INC HLPKE999

           PERFORM 970000-LER-CANAL

      *--> Define o nome do programa p/ fins manipulacao container
           MOVE 'SGTS0612'             TO NM-PGM

      *--> Limpa a resposta e erro antes de executar o modulo
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

      *--> Valida campo a campo da requisicao

           IF  SGTS0612-CD-ITMT-FNCO EQUAL SPACES
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  1160             TO  MSG1-CD-ERRO-TS
               MOVE  'Código do Título do Evento não informado.'
                                       TO  MSG1-TX-ERRO-TS
               SET  MNT-VRV            TO  TRUE
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0612-CD-GR-EVT-FDAR < ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  1161              TO  MSG1-CD-ERRO-TS
               MOVE  'Código do Tipo de Evento inválido.'
                                       TO  MSG1-TX-ERRO-TS
               SET  MNT-VRV            TO  TRUE
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0612-CD-GR-EVT-FDAR EQUAL ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  1186              TO  MSG1-CD-ERRO-TS
               MOVE  'Código do Tipo de Evento não informado.'
                                       TO  MSG1-TX-ERRO-TS
               SET  MNT-VRV            TO  TRUE
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0612-NR-SEQL-EVT-EXCD EQUAL ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  1179              TO  MSG1-CD-ERRO-TS
               MOVE 'Numero do Evento SGT  não informado.'
                                         TO MSG1-TX-ERRO-TS
               SET MNT-VRV               TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF.

           IF  SGTS0612-NR-SEQL-EVT-EXCD < ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  1180              TO  MSG1-CD-ERRO-TS
               MOVE 'Numero do Evento SGT  inválido.'
                                         TO MSG1-TX-ERRO-TS
               SET MNT-VRV               TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF.

      *CD-GR-EVT-FDAR que serão utilizados no DE PARA do CAC PARA tipo
      * do evento do SGT:
      *AMORTIZACAO           = 01
      *AMORTIZACAO EXTRAORD. = 02
      *ANTECIPAÇÃO           = 03
      *JUROS                 = 05
      *PARTICIPAÇAO LUCROS   = 08
      *PREMIO                = 09
      *RENDIMENTO            = 12
      *RESGATE               = 13
      *VENCIMENTO            = 15
      *INCORPORAÇÃO DE JUROS = 65

           MOVE 0003                   TO MSG1-CD-SEQL-ERRO

           IF ((SGTS0612-CD-GR-EVT-FDAR NOT EQUAL 01) AND
               (SGTS0612-CD-GR-EVT-FDAR NOT EQUAL 02) AND
               (SGTS0612-CD-GR-EVT-FDAR NOT EQUAL 03) AND
               (SGTS0612-CD-GR-EVT-FDAR NOT EQUAL 05) AND
               (SGTS0612-CD-GR-EVT-FDAR NOT EQUAL 08) AND
               (SGTS0612-CD-GR-EVT-FDAR NOT EQUAL 09) AND
               (SGTS0612-CD-GR-EVT-FDAR NOT EQUAL 12) AND
               (SGTS0612-CD-GR-EVT-FDAR NOT EQUAL 13) AND
               (SGTS0612-CD-GR-EVT-FDAR NOT EQUAL 15) AND
               (SGTS0612-CD-GR-EVT-FDAR NOT EQUAL 65) )

               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  1161              TO  MSG1-CD-ERRO-TS
               MOVE  'Código do Grupo do Evento Fiduciário inválido.'
                                       TO  MSG1-TX-ERRO-TS
               SET  MNT-VRV            TO  TRUE
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0612-CD-PRD < ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0010               TO MSG1-CD-ERRO-TS
               MOVE 'Código do produto inválido.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0612-CD-PRD  EQUAL ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0068               TO MSG1-CD-ERRO-TS
               MOVE 'Código do produto não informado.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0612-CD-MDLD < ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0011               TO MSG1-CD-ERRO-TS
               MOVE 'Código da modalidade do produto inválido.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0612-CD-MDLD EQUAL ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0069               TO MSG1-CD-ERRO-TS
               MOVE 'Código da modalidade do produto não informado.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0612-NR-CT-LQDC  < ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 1163               TO MSG1-CD-ERRO-TS
               MOVE 'Número da Conta de Liquidação inválido.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0612-NR-CT-LQDC EQUAL ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 1162               TO MSG1-CD-ERRO-TS
               MOVE 'Número da Conta de Liquidação não informado.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF


           IF  SGTS0612-VL-EVT     NOT NUMERIC
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 1166               TO MSG1-CD-ERRO-TS
               MOVE 'Valor do Evento inválido.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0612-VL-PU-EVT  NOT NUMERIC
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 1167               TO MSG1-CD-ERRO-TS
               MOVE 'Valor do Preço Unitário do Evento invalido.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0612-QT-PAPL-EVT  < ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 1168               TO MSG1-CD-ERRO-TS
               MOVE 'Quantidade de Papéis do Evento inválido.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0612-QT-PAPL-EVT  EQUAL ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 1169               TO MSG1-CD-ERRO-TS
               MOVE 'Quantidade de Papéis do Evento não informado.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF

           IF (SGTS0612-IN-EVT-CTBD NOT EQUAL 'S' AND
               SGTS0612-IN-EVT-CTBD NOT EQUAL 'N')
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 1170               TO MSG1-CD-ERRO-TS
               MOVE 'Indicador de Evento Contabilizado inválido.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF

           IF (SGTS0612-IN-ISN-IR-EVT  NOT EQUAL 'S' AND
               SGTS0612-IN-ISN-IR-EVT  NOT EQUAL 'N')
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 1171               TO MSG1-CD-ERRO-TS
               MOVE 'Indicador de Isenção Imposto de Renda do Evento inv
      -             'álido.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF

            IF (SGTS0612-IN-RTC-IR-EMT NOT EQUAL 'S' AND
               SGTS0612-IN-RTC-IR-EMT  NOT EQUAL 'N')
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 1171               TO MSG1-CD-ERRO-TS
               MOVE 'Indicador Retenção de Imposto de Renda do Emissor i
      -             'nválido.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0612-DT-EVT    EQUAL SPACES
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 1173               TO MSG1-CD-ERRO-TS
               MOVE 'Data do Evento não informada.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0612-DT-EVT    NOT EQUAL SPACES
                MOVE CORR SGTS0612-DT-EVT-R      TO  ARG01-R
      *    Valida data no formato Gregoriano
                MOVE 4                               TO  ARG02
                PERFORM 920000-VALIDAR-DATAS
           END-IF

            IF  SGTS0612-CD-CTRA-TIT-VL < ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
                MOVE  0001          TO  MSG1-CD-ERRO-TS
                MOVE  'Código da carteira de título e valores mobiliário
      -                's inválido.'
                                       TO  MSG1-TX-ERRO-TS
               SET  MNT-VRV            TO  TRUE
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0612-CD-CTRA-TIT-VL EQUAL ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0002          TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -               ' não informado.'
                                       TO  MSG1-TX-ERRO-TS
               SET  MNT-VRV            TO  TRUE
               PERFORM  999000-ERRO
           END-IF

           IF SGTS0612-IN-RTC-IR-EMT  EQUAL 'S'
               IF  SGTS0612-VL-IR-EMT     EQUAL ZEROS
                   SET ERRO-PROGRAMADO     TO TRUE
                   MOVE 1174               TO MSG1-CD-ERRO-TS
                   MOVE 'Valor do Imposto de Renda do Emissor deve ser i
      -                  'nformado.'
                                           TO MSG1-TX-ERRO-TS
                   SET MNT-VRV             TO TRUE
                   PERFORM 999000-ERRO
               END-IF
            END-IF

            IF SGTS0612-IN-RTC-IR-EMT  EQUAL 'S'
              IF  SGTS0612-VL-IR-EMT     NOT NUMERIC
                  SET ERRO-PROGRAMADO     TO TRUE
                  MOVE 1175               TO MSG1-CD-ERRO-TS
                  MOVE 'Valor do Imposto de Renda do Emissor não numéric
      -              'o.'
                                       TO MSG1-TX-ERRO-TS
                  SET MNT-VRV             TO TRUE
                  PERFORM 999000-ERRO
              END-IF
            END-IF

           .
       110000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-PROCESSAR                SECTION.
      *-----------------------------------------------------------------

           PERFORM 210000-VALIDA-PRD-MDLD

           PERFORM 220000-CONSULTA-CRNG-EVT

           PERFORM 230000-VER-ULT-ESTADO-EVT

           PERFORM 240000-VER-ULT-ESTADO-PRCT

           PERFORM 250000-ALTERAR-CRNG-EVT-EXCD

           PERFORM 300000-MOVIMENTAR-SAIDA
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       210000-VALIDA-PRD-MDLD       SECTION.
      *-->Verifica existencia de produto/modalidade
      *-----------------------------------------------------------------
           MOVE 0004                   TO MSG1-CD-SEQL-ERRO

      *--> Movimentar o nome do programa a ser executado
           INITIALIZE                  PRDS0203-DADOS.

           MOVE PRDS0203               TO NM-PGM

      *--> Movimentar os campos de entrada

           MOVE 1                      TO S0203-CD-FUNCAO
           MOVE SGTS0612-CD-PRD        TO S0203-CD-PRD
           MOVE SGTS0612-CD-MDLD       TO S0203-CD-MDLD

           PERFORM 910000-CHAMAR-SERVICO

           IF  S0203-CD-RTN     NOT EQUAL ZEROS
               SET ERRO-PROGRAMA-ACIONADO TO TRUE
               MOVE 3004               TO MSG1-CD-ERRO-TS
               STRING  'Erro ao acionar o módulo ' NM-PGM '.'
                            DELIMITED BY SIZE
                            INTO           MSG1-TX-ERRO-TS
               MOVE NM-PGM             TO MSG1-NM-PGM-ACND
               MOVE S0203-TX-RTN       TO MSG1-TX-RTN-PGM-ACND
               MOVE S0203-CD-RTN       TO MSG1-CD-RTN-PGM-ACND

      *        Monitorar variáveis do módulo chamado
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-ERRO
           END-IF
           .
       210000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       220000-CONSULTA-CRNG-EVT      SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE         SGTST138-DADOS
           MOVE SGTST138               TO NM-PGM

      *--> Movimentar os campos de entrada

           SET SGTST138-CONSULTAR         TO TRUE
           MOVE SGTS0612-CD-ITMT-FNCO     TO SGTST138-CD-ITMT-FNCO
           MOVE SGTS0612-CD-GR-EVT-FDAR   TO SGTST138-CD-GR-EVT-FDAR
           MOVE SGTS0612-NR-SEQL-EVT-EXCD TO SGTST138-NR-SEQL-EVT-EXCD

           PERFORM 930000-CHAMA-SGTST138
           .
       220000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       230000-VER-ULT-ESTADO-EVT    SECTION.
      * Verifica o estado mais atual do Evento
      *-----------------------------------------------------------------
           MOVE 0006                   TO  MSG1-CD-SEQL-ERRO

           INITIALIZE         SGTST139-DADOS

      *--> Movimentar os campos de entrada

           SET SGTST139-CONSULTAR-EST-ATIVO TO TRUE
           MOVE SGTS0612-CD-ITMT-FNCO
                                            TO SGTST139-CD-ITMT-FNCO
           MOVE SGTS0612-CD-GR-EVT-FDAR
                                            TO SGTST139-CD-GR-EVT-FDAR
           MOVE SGTS0612-NR-SEQL-EVT-EXCD
                                            TO SGTST139-NR-SEQL-EVT-EXCD
           PERFORM 940000-CHAMA-SGTST139

      * CD-EST-EVT-EXCD = 1  Em Cadastramento

           IF SGTST139-CD-EST-EVT-EXCD NOT EQUAL 1
              SET   ERRO-PROGRAMADO   TO  TRUE
              MOVE  1183              TO  MSG1-CD-ERRO-TS
              MOVE 'Alteração não permitida para o estado atual do event
      -               'o.'
                                      TO MSG1-TX-ERRO-TS
               SET   MNT-VRV          TO  TRUE
               PERFORM 999000-ERRO
           END-IF

           .
       230000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       240000-VER-ULT-ESTADO-PRCT    SECTION.
      * * Verifica o estado mais atual do processamento do Cronograma
      *-----------------------------------------------------------------
           MOVE 0007                   TO  MSG1-CD-SEQL-ERRO

           INITIALIZE         SGTST140-DADOS

      *--> Movimentar os campos de entrada

           SET SGTST140-CONSULTAR-EST-ATIVO TO TRUE
           MOVE SGTS0612-CD-ITMT-FNCO
                                            TO SGTST140-CD-ITMT-FNCO
           MOVE SGTS0612-CD-GR-EVT-FDAR
                                            TO SGTST140-CD-GR-EVT-FDAR
           MOVE SGTS0612-NR-SEQL-EVT-EXCD
                                            TO SGTST140-NR-SEQL-EVT-EXCD
           PERFORM 950000-CHAMA-SGTST140

      * CD-EST-EVT-EXCD = 1 - Não enviado para processamento

           IF SGTST140-CD-EST-PRCT-CRNG NOT EQUAL 1
              SET   ERRO-PROGRAMADO   TO  TRUE
              MOVE  1184              TO  MSG1-CD-ERRO-TS
              MOVE 'Alteração não permitida para o estado atual do proce
      -             'ssamento do cronograma.'
                                      TO MSG1-TX-ERRO-TS
              SET   MNT-VRV           TO  TRUE
              PERFORM 999000-ERRO
           END-IF

           .
       240000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       250000-ALTERAR-CRNG-EVT-EXCD        SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

           MOVE SGTST138               TO NM-PGM

      *--> Movimentar os campos de entrada

           SET SGTST138-ALTERAR         TO TRUE
           MOVE SGTS0612-CD-ITMT-FNCO   TO SGTST138-CD-ITMT-FNCO
           MOVE SGTS0612-CD-GR-EVT-FDAR TO SGTST138-CD-GR-EVT-FDAR
           MOVE SGTS0612-NR-SEQL-EVT-EXCD
                                        TO SGTST138-NR-SEQL-EVT-EXCD

           MOVE SGTS0612-CD-PRD         TO SGTST138-CD-PRD

           MOVE SGTS0612-CD-MDLD        TO SGTST138-CD-MDLD

           MOVE SGTS0612-NR-CT-LQDC     TO SGTST138-NR-CT-LQDC

      *    Definido com gestor esta regra fixa.
      *    CD-MDLD-LQDC = 3 - Evento Fora da B3
      *     MOVE 3                       TO SGTST138-CD-MDLD-LQDC

           MOVE SGTS0612-VL-EVT         TO SGTST138-VL-EVT

           MOVE SGTS0612-VL-PU-EVT      TO SGTST138-VL-PU-EVT

           MOVE SGTS0612-QT-PAPL-EVT    TO SGTST138-QT-PAPL-EVT

           MOVE SGTS0612-IN-EVT-CTBD    TO SGTST138-IN-EVT-CTBD

           MOVE SGTS0612-IN-ISN-IR-EVT  TO SGTST138-IN-ISN-IR-EVT

           MOVE SGTS0612-IN-RTC-IR-EMT  TO SGTST138-IN-RTC-IR-EMT

           MOVE '  .  .    '            TO SGTST138-DT-EVT
           MOVE CORR SGTS0612-DT-EVT-R
                                        TO SGTST138-DT-EVT-R

           MOVE KOPRD-CD-OPRD           TO SGTST138-CD-USU-RSP-ATL

           MOVE 'CURRENT'               TO SGTST138-TS-ULT-ATL

           MOVE SGTS0612-CD-CTRA-TIT-VL TO SGTST138-CD-CTRA-TIT-VL

           IF SGTS0612-IN-RTC-IR-EMT = 'S'
              MOVE SGTS0612-VL-IR-EMT TO SGTST138-VL-IR-EMT
           END-IF


           PERFORM 930000-CHAMA-SGTST138
           .
       240000-SAI.
           EXIT.


      *-----------------------------------------------------------------
       300000-MOVIMENTAR-SAIDA         SECTION.
      *-----------------------------------------------------------------
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO

           MOVE 'S'
                                       TO SGTS0612-IN-SCS

      *--> Gravando o container de resposta da subrotina

           PERFORM 980000-GRAVAR-CONTAINER-RPST
           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *                                                                *
      *  SECTION DA FAIXA DE 900000 A 969900 PARA DEMAIS ROTINAS       *
      *                                                                *
      *-----------------------------------------------------------------
      * Executa um modulo implementado com CONTAINER                   *
      * Nao incluir nessa section os modulos chamados via COMMAREA     *
      *                                                                *
      *-----------------------------------------------------------------

      *----------------------------------------------------------------
       910000-CHAMAR-SERVICO           SECTION.
      *----------------------------------------------------------------
      *--> Acionar programa IIBP2000 para requisição do serviço.
      *----------------------------------------------------------------


      *--> Grava o container com dados do serviço/operação a chamar
           PERFORM 981000-GRAVAR-CONTAINER-SRVC

      *--> Grava o container de requisicao do modulo a executar
           PERFORM 982000-GRAVAR-CONTAINER-RQSC

      *--> Limpa a resposta e erro antes de executar o modulo
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           PERFORM 911000-EXECUTA-IIBP200

      *--> Efetua a leitura do Container de Erro
           PERFORM 972000-LER-CONTAINER-DE-ERRO

           IF NOT TEM-CONTAINER-ERRO
      *--> Efetua a leitura do Container de Resposta
              PERFORM 973000-LER-RESPOSTA
           END-IF
           MOVE SPACES                 TO IN-CONTAINER-ERRO
           .
       910000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       911000-EXECUTA-IIBP200                 SECTION.
      *-----------------------------------------------------------------
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS LINK
                     PROGRAM   ( IIBP2000     )
                     CHANNEL   ( NM-CNL       )
                     RESP      ( MSG1-RESP1   )
                     RESP2     ( MSG1-RESP2   )
           END-EXEC

           EVALUATE TRUE
               WHEN MSG1-RESP1        EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    SET ERRO-CICS      TO TRUE
                    MOVE 9000          TO MSG1-CD-ERRO-TS
                    STRING 'Erro ao acionar serviço: '
                           SRVC-ID-SRVC '.' SRVC-VRS-SRVC '.'
                           SRVC-ID-OPR  '.' SRVC-VRS-OPR
                           'via IIBP2000, '
                           'RESP=' MSG1-RESP1 ', RESP-2=' MSG1-RESP2
                           '.'         DELIMITED BY SIZE
                                       INTO MSG1-TX-ERRO-TS
                    END-STRING

                    PERFORM 999000-ERRO
           END-EVALUATE
           .
       911000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       920000-VALIDAR-DATAS                 SECTION.
      *-----------------------------------------------------------------
           MOVE 0011                   TO MSG1-CD-SEQL-ERRO

           MOVE 4095 TO AREA-TAM.
           MOVE 'SBDATAF -WRKAREA-FIM' TO AREA-FIM.

           CALL SBDATAF     USING FUNCAO WKSBDATA  ARG01 ARG02.

      *--> Return code = 24: Data Inválida.

           IF  RETURN-CODE             NOT EQUAL ZEROS AND 24 AND 22
                                                       AND 46
               SET ERRO-PROGRAMA-ACIONADO TO TRUE
               MOVE 0067               TO MSG1-CD-ERRO-TS

               MOVE 'Erro na rotina que valida data.'
                                       TO MSG1-TX-ERRO-TS
                                          MSG1-TX-RTN-PGM-ACND

               MOVE SBDATAF            TO MSG1-NM-PGM-ACND
                                          NM-PGM
               MOVE RETURN-CODE        TO MSG1-CD-RTN-PGM-ACND

      *        Monitorar variáveis do módulo chamado
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-ERRO

           END-IF

           IF  RETURN-CODE             EQUAL 24
               SET ERRO-PROGRAMA-ACIONADO     TO TRUE
               MOVE 0049               TO MSG1-CD-ERRO-TS
               MOVE 'Data informada inválida'
                                       TO MSG1-TX-ERRO-TS
                                          MSG1-TX-RTN-PGM-ACND
               MOVE SBDATAF            TO MSG1-NM-PGM-ACND
                                          NM-PGM
               MOVE RETURN-CODE        TO MSG1-CD-RTN-PGM-ACND

      *        Monitorar variáveis do módulo chamado
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-ERRO
           END-IF
           .
       920000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       930000-CHAMA-SGTST138           SECTION.
      *-----------------------------------------------------------------
           MOVE 0012                   TO MSG1-CD-SEQL-ERRO

      *--> Movimenta nome do módulo para monitoração de variáveis
           MOVE SGTST138               TO NM-PGM
           EXEC CICS LINK
                PROGRAM  (SGTST138)
                COMMAREA (SGTST138-DADOS)
           END-EXEC

           IF  SGTST138-CD-RTN-PGM     NOT EQUAL ZEROS
               SET ERRO-PROGRAMA-ACIONADO  TO TRUE
               MOVE NM-PGM                 TO MSG1-NM-PGM-ACND
               MOVE SGTST138-MSG-ERRO-PGM  TO MSG1-TX-RTN-PGM-ACND
                                              MSG1-TX-ERRO-TS
               MOVE SGTST138-CD-RTN-PGM    TO MSG1-CD-RTN-PGM-ACND
                                              MSG1-CD-ERRO-TS
      *        Monitorar variáveis do módulo chamado
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-ERRO
           END-IF
           .
       930000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       940000-CHAMA-SGTST139         SECTION.
      *-----------------------------------------------------------------
           MOVE 0013                   TO MSG1-CD-SEQL-ERRO

      *--> Movimenta nome do módulo para monitoração de variáveis
           MOVE SGTST139               TO NM-PGM
           EXEC CICS LINK
                PROGRAM  (SGTST139)
                COMMAREA (SGTST139-DADOS)
           END-EXEC

           IF  SGTST139-CD-RTN-PGM     NOT EQUAL ZEROS
               SET ERRO-PROGRAMA-ACIONADO TO TRUE
               MOVE NM-PGM                TO MSG1-NM-PGM-ACND
               MOVE SGTST139-MSG-ERRO-PGM TO MSG1-TX-RTN-PGM-ACND
                                             MSG1-TX-ERRO-TS
               MOVE SGTST139-CD-RTN-PGM   TO MSG1-CD-RTN-PGM-ACND
                                             MSG1-CD-ERRO-TS
      *        Monitorar variáveis do módulo chamado
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-ERRO
           END-IF
           .
       940000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       950000-CHAMA-SGTST140         SECTION.
      *-----------------------------------------------------------------
           MOVE 0014                   TO MSG1-CD-SEQL-ERRO

      *--> Movimenta nome do módulo para monitoração de variáveis
           MOVE SGTST140               TO NM-PGM
           EXEC CICS LINK
                PROGRAM  (SGTST140)
                COMMAREA (SGTST140-DADOS)
           END-EXEC

           IF  SGTST140-CD-RTN-PGM     NOT EQUAL ZEROS
               SET ERRO-PROGRAMA-ACIONADO TO TRUE
               MOVE NM-PGM                TO MSG1-NM-PGM-ACND
               MOVE SGTST140-MSG-ERRO-PGM TO MSG1-TX-RTN-PGM-ACND
                                             MSG1-TX-ERRO-TS
               MOVE SGTST140-CD-RTN-PGM   TO MSG1-CD-RTN-PGM-ACND
                                             MSG1-CD-ERRO-TS
      *        Monitorar variáveis do módulo chamado
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-ERRO
           END-IF
           .
       950000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       969000-MONITORAR-VARIAVEIS      SECTION.
      *--> Monitora as variáveis dos módulos chamados
      *-----------------------------------------------------------------

      * MOCK-POINT MCKV3
           EVALUATE NM-PGM

                WHEN SGTST138
                    MOVE 'SGTST138-ACAO'       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST138-ACAO         TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST138-CD-ITMT-FNCO'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST138-CD-ITMT-FNCO
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST138-CD-GR-EVT-FDAR'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST138-CD-GR-EVT-FDAR
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST138-NR-SEQL-EVT-EXCD'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST138-NR-SEQL-EVT-EXCD
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST138-CD-PRD'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST138-CD-PRD
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST138-CD-MDLD'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST138-CD-MDLD
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST138-NR-CT-LQDC'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST138-NR-CT-LQDC
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST138-CD-MDLD-LQDC'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST138-CD-MDLD-LQDC
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST138-VL-EVT'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTST138-VL-EVT
                                       TO AX-VALOR2
                    MOVE AX-VALOR2          TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST138-VL-PU-EVT'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTST138-VL-PU-EVT
                                       TO AX-VALOR
                    MOVE AX-VALOR           TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST138-QT-PAPL-EVT'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTST138-QT-PAPL-EVT
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST138-IN-EVT-CTBD'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTST138-IN-EVT-CTBD
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST138-IN-ISN-IR-EVT'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTST138-IN-ISN-IR-EVT
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST138-IN-RTC-IR-EMT'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTST138-IN-RTC-IR-EMT
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST138-DT-EVT'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTST138-DT-EVT
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST138-CD-USU-RSP-ATL'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST138-CD-USU-RSP-ATL
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST138-TS-ULT-ATL'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST138-TS-ULT-ATL
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST138-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTST138-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST138-VL-IR-EMT'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTS0612-VL-IR-EMT
                                       TO AX-VALOR3
                    MOVE AX-VALOR3     TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                WHEN SGTST139
                    MOVE 'SGTST139-ACAO'       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST139-ACAO         TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST139-CD-ITMT-FNCO'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST139-CD-ITMT-FNCO
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST139-CD-GR-EVT-FDAR'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST139-CD-GR-EVT-FDAR
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST139-NR-SEQL-EVT-EXCD'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST139-NR-SEQL-EVT-EXCD
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST139-TS-EST-EVT-EXCD'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST139-TS-EST-EVT-EXCD
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTST140
                    MOVE 'SGTST140-ACAO'       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST140-ACAO         TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST140-CD-ITMT-FNCO'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST140-CD-ITMT-FNCO
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST140-CD-GR-EVT-FDAR'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST140-CD-GR-EVT-FDAR
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST140-NR-SEQL-EVT-EXCD'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST140-NR-SEQL-EVT-EXCD
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST140-TS-EST-ENVO-CRNG'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST140-TS-EST-ENVO-CRNG
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                WHEN PRDS0203
                    MOVE 'S0203-CD-FUNCAO'     TO MSGKW999-NM-VRV-MNT
                    MOVE S0203-CD-FUNCAO       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'S0203-CD-PRD'        TO MSGKW999-NM-VRV-MNT
                    MOVE S0203-CD-PRD          TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'S0203-CD-MDLD'       TO MSGKW999-NM-VRV-MNT
                    MOVE S0203-CD-MDLD         TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SBDATAF
                    MOVE 'FUNCAO'              TO MSGKW999-NM-VRV-MNT
                    MOVE FUNCAO                TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'ARG01'               TO MSGKW999-NM-VRV-MNT
                    MOVE ARG01                 TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'ARG02'               TO MSGKW999-NM-VRV-MNT
                    MOVE ARG02                 TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT


               WHEN OTHER
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
           MOVE 0015                   TO MSG1-CD-SEQL-ERRO

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET  ERRO-CANAL             TO TRUE

           EXEC CICS ASSIGN
                     CHANNEL ( NM-CNL )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       970000-FIM.
           EXIT.

      *-----------------------------------------------------------------
       971000-LER-REQUISICAO           SECTION.
      *-----------------------------------------------------------------

           MOVE 0016                   TO MSG1-CD-SEQL-ERRO

      *--> Monta o nome do container de requisicao
           MOVE 'SGTS0612'             TO NM-PGM
           MOVE 'SGTS0612-RQSC'        TO NM-CTNR
           MOVE LENGTH OF SGTS0612-RQSC   TO  QT-CCT-EPRO

      *--> Recupera o tamanho do container de requisição
           PERFORM 973100-LER-CONTAINER-NODATA

      *     Se depois dos testes se verificar que não há mesmo
      *     necessidade destas linhas DELETAR o abaixo
      *     IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
      *         SET  ERRO-TAMANHO-AREA      TO  TRUE
      *         PERFORM 999000-ERRO
      *     END-IF

      *--> Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *--> Efetua a leitura do container
           EXEC CICS GET
                     CONTAINER ( NM-CTNR       )
                     FLENGTH   ( QT-CCT-LIDO   )
                     INTO      ( SGTS0612-RQSC )
           END-EXEC

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       971000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       972000-LER-CONTAINER-DE-ERRO    SECTION.
      *--> A presença do container PRM-ERRO indica que houve erro de
      *    integração, a ausência do container indica sucesso.
      *-----------------------------------------------------------------

           MOVE 0017                   TO MSG1-CD-SEQL-ERRO

      *--> Le o container de erro dos modulos chamados
           SET GET-CONTAINER           TO TRUE
           MOVE 'PRM-ERRO'             TO NM-CTNR
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
      *        PARA O MÓDULO CHAMADO e tratar na section chamadora
               WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
                    CONTINUE
               WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
                       SET TEM-CONTAINER-ERRO TO TRUE
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
                       PERFORM 969000-MONITORAR-VARIAVEIS
                       PERFORM 999000-ERRO

               WHEN OTHER
                    SET ERRO-CONTAINER  TO TRUE
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

           MOVE 0018                   TO MSG1-CD-SEQL-ERRO

      *--> Monta o nome dos container de acordo com modulo a executar
           MOVE SPACES                 TO NM-CTNR

      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *    nome_modulo -RPST

           EVALUATE NM-PGM

               WHEN PRDS0203
                    MOVE LENGTH OF PRDS0203-DADOS TO  QT-CCT-EPRO
                    MOVE 'PRM-DADOS-RPST'         TO NM-CTNR

               WHEN OTHER
                   SET  ERRO-PROGRAMADO        TO  TRUE
                   MOVE 9005                   TO  MSG1-CD-ERRO-TS
                    STRING
                    'Módulo não definido: ' NM-PGM
                    DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
                    PERFORM 999000-ERRO
           END-EVALUATE

      *--> Recupera o tamanho do container de resposta
           PERFORM 973100-LER-CONTAINER-NODATA

           IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
               SET   ERRO-TAMANHO-AREA     TO  TRUE
               PERFORM 999000-ERRO
           END-IF

      *--> Configura dados para leitura do container de resposta
           SET  GET-CONTAINER          TO TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET  ERRO-CONTAINER         TO TRUE


      *--> Le a RESPOSTA de acordo com o programa que a gravou

           MOVE 0019                   TO MSG1-CD-SEQL-ERRO

           EVALUATE NM-PGM

      *        Calcular a quantidade de caracteres esperado e limpar
      *        a area de dados antes de executar o GET
      *        Para matrizes com dados variaveis (clausula depending
      *        on) nao ha como utilizar o comando INITIALIZE

               WHEN PRDS0203
                    INITIALIZE PRDS0203-DADOS
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (PRDS0203-DADOS   )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN OTHER
                    SET   ERRO-PROGRAMADO
                                       TO  TRUE
                    MOVE  9003         TO  MSG1-CD-ERRO-TS
                    STRING  'Erro na leitura da resposta do módulo '
                            NM-PGM '.'
                            DELIMITED BY SIZE
                            INTO           MSG1-TX-ERRO-TS
                    PERFORM 999000-ERRO
           END-EVALUATE

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       973000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       973100-LER-CONTAINER-NODATA     SECTION.
      *-----------------------------------------------------------------

           MOVE 0020                   TO MSG1-CD-SEQL-ERRO

      *--> Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *--> Obtem a quantidade de caracteres no container
           MOVE ZEROS                  TO QT-CCT-LIDO
           EXEC CICS GET
                     CONTAINER (NM-CTNR)
                     NODATA
                     FLENGTH   (QT-CCT-LIDO)
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       973100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       974000-LER-DADOS-OPRD           SECTION.
      *-----------------------------------------------------------------
           MOVE 0021                   TO MSG1-CD-SEQL-ERRO

      *--> Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE
           MOVE 'PRM-INF-OPRD'         TO NM-CTNR

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET  ERRO-CONTAINER         TO TRUE

           INITIALIZE PRM-INF-OPRD
           EXEC CICS GET
                CONTAINER ( NM-CTNR                )
                INTO      ( PRM-INF-OPRD           )
                FLENGTH   ( LENGTH OF PRM-INF-OPRD )
           END-EXEC.

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       974000-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *   Section da Faixa de 980000 a 989900 para Gravacao de Dados
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       980000-GRAVAR-CONTAINER-RPST    SECTION.
      *-----------------------------------------------------------------

      *    Exclui eventual container de erro de outros programas
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO

           MOVE 0022                   TO MSG1-CD-SEQL-ERRO

      *    Monta o nome do container de resposta
           MOVE 'SGTS0612-RPST'        TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET  ERRO-CONTAINER         TO TRUE

           EXEC CICS PUT
                     CONTAINER (NM-CTNR                    )
                     FROM      (SGTS0612-RPST          )
                     FLENGTH   (LENGTH OF SGTS0612-RPST)
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       980000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       981000-GRAVAR-CONTAINER-SRVC    SECTION.
      *--> Postar container de requisição de serviço.
      *-----------------------------------------------------------------
           MOVE 0023                   TO MSG1-CD-SEQL-ERRO

      *--> Monta o nome do container de requisicao do servico
           MOVE 'PRM-RQSC-SRVC'        TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE

           EVALUATE NM-PGM

               WHEN PRDS0203
                    MOVE 214106        TO SRVC-ID-SRVC
                    MOVE 1             TO SRVC-VRS-SRVC
                    MOVE 543363        TO SRVC-ID-OPR
                    MOVE 1             TO SRVC-VRS-OPR

           END-EVALUATE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET  ERRO-CONTAINER         TO TRUE

           EXEC CICS PUT
                     CONTAINER ( NM-CTNR                     )
                     FROM      ( SRVC-PRM-RQSC-SRVC          )
                     FLENGTH   ( LENGTH OF SRVC-PRM-RQSC-SRVC)
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       981000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       982000-GRAVAR-CONTAINER-RQSC    SECTION.
      *    Postar container com dados de requisição (entrada)
      *    Modulo CONTAINER  -     NOME_PGM-RQSC
      *    Modulo IIB (FIXO) -     PRM-DADOS-RQSC
      *-----------------------------------------------------------------

           MOVE 0024                   TO MSG1-CD-SEQL-ERRO

      *--> Monta o nome dos container de acordo com modulo a executar
           MOVE SPACES                 TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE

      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RQSC, senão
      *    nome_modulo -RQSC
           EVALUATE NM-PGM

               WHEN PRDS0203
                    MOVE 'PRM-DADOS-RQSC'
                                       TO NM-CTNR
               WHEN OTHER
                    MOVE NM-PGM        TO NM-CTNR(1:8)
                    MOVE '-RQSC'       TO NM-CTNR(9:5)
           END-EVALUATE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *--> Faz put da requisicao do programa a executar
           EVALUATE NM-PGM

               WHEN PRDS0203
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (PRDS0203-DADOS )
                              FLENGTH   (LENGTH OF PRDS0203-DADOS )
                    END-EXEC

               WHEN OTHER
                    SET   ERRO-PROGRAMADO
                                       TO  TRUE
                    MOVE  9004         TO  MSG1-CD-ERRO-TS
                    STRING  'Erro na gravação da requisição do módulo '
                            NM-PGM '.'
                            DELIMITED BY SIZE
                            INTO           MSG1-TX-ERRO-TS
                    PERFORM 999000-ERRO
           END-EVALUATE

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
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
           MOVE 0025                   TO MSG1-CD-SEQL-ERRO

      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *    nome_modulo -RPST
           EVALUATE NM-PGM

               WHEN PRDS0203
                    MOVE 'PRM-DADOS-RPST'
                                       TO NM-CTNR
               WHEN OTHER
                    MOVE NM-PGM        TO NM-CTNR(1:8)
                    MOVE '-RPST'       TO NM-CTNR(9:5)
           END-EVALUATE

           SET DEL-CONTAINER           TO TRUE

      *--> Exclui o container de resposta
      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS DELETE
                     CONTAINER (NM-CTNR)
                     RESP      (MSG1-RESP1)
                     RESP2     (MSG1-RESP2)
           END-EXEC

           EVALUATE TRUE
               WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
               WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    SET ERRO-CONTAINER TO TRUE
                    PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE

      *--> Excluir o container de erro
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO
           .
       983000-SAI.
           EXIT.
      *
      *-----------------------------------------------------------------
       983100-EXCLUIR-CONTAINER-ERRO   SECTION.
      *-----------------------------------------------------------------
           MOVE 0026                   TO MSG1-CD-SEQL-ERRO

      *--> Limpar variaveis container de erro
           INITIALIZE IIBKERRO-PRM-ERRO

      *--> Exclui o container de erro
           MOVE 'PRM-ERRO'             TO NM-CTNR
           SET DEL-CONTAINER           TO TRUE

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS DELETE
                     CONTAINER (NM-CTNR)
                     RESP      (MSG1-RESP1)
                     RESP2     (MSG1-RESP2)
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
       984000-GRAVAR-CONTAINER-ERRO    SECTION.
      *-----------------------------------------------------------------

      *--> Grava o erro gerado no container de erro
      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS PUT
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
      *    As sections abaixo iniciadas com 999000 representam os
      *    tratamentos de erro utilizando o Sistema MSG(rotina MSGS0001)
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       999000-ERRO                     SECTION.
      *-----------------------------------------------------------------

           IF  MSG1-CD-ERRO-TS             EQUAL ZEROS
               MOVE  8888              TO MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS             EQUAL SPACES
               MOVE  'Erro não especificado no módulo.'
                                       TO  MSG1-TX-ERRO-TS
           END-IF
           PERFORM 999000-GRAVA-ERRO-E-ENCERRA
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

      *--> Adiciona as variáveis de entrada para monitoração
           IF  MNT-VRV

               MOVE 'SGTS0612-CD-ITMT-FNCO'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0612-CD-ITMT-FNCO
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT


               MOVE 'SGTS0612-CD-GR-EVT-FDAR'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0612-CD-GR-EVT-FDAR
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT


               MOVE 'SGTS0612-NR-SEQL-EVT-EXCD'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0612-NR-SEQL-EVT-EXCD
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0612-CD-PRD'  TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0612-CD-PRD   TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT


               MOVE 'SGTS0612-CD-MDLD' TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0612-CD-MDLD  TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT


               MOVE 'SGTS0612-NR-CT-LQDC'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0612-NR-CT-LQDC
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0612-VL-EVT'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0612-VL-EVT
                                       TO AX-VALOR2
               MOVE AX-VALOR2          TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT


               MOVE 'SGTS0612-VL-PU-EVT'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0612-VL-PU-EVT
                                       TO AX-VALOR
               MOVE AX-VALOR           TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT


               MOVE 'SGTS0612-QT-PAPL-EVT'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0612-QT-PAPL-EVT
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT


               MOVE 'SGTS0612-IN-EVT-CTBD'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0612-IN-EVT-CTBD
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT


               MOVE 'SGTS0612-IN-ISN-IR-EVT'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0612-IN-ISN-IR-EVT
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT


               MOVE 'SGTS0612-IN-RTC-IR-EMT'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0612-IN-RTC-IR-EMT
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT


               MOVE 'SGTS0612-DT-EVT'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0612-DT-EVT
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT


               MOVE 'SGTS0612-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0612-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT


               MOVE 'SGTS0612-VL-IR-EMT'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0612-VL-IR-EMT
                                       TO AX-VALOR3
               MOVE AX-VALOR3          TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

           END-IF

      *--> Grava o erro gerado no sistema MSG - Mensageria
           PERFORM MSG1-NTF-MSG

           MOVE MSG1-CD-ERRO-TS        TO IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS        TO IIBKERRO-TX-ERRO

      *--> Grava o erro gerado no container de erro
           PERFORM 984000-GRAVAR-CONTAINER-ERRO

      *--> Encerra a transacao
           PERFORM 999999-ENCERRAR
           .
       999000-SAI.
           EXIT.


      * ---------------------------------------
      * 999001-TRATA-ABEND-CICS        SECTION.
      * 999002-TRATA-CONDITION-CICS    SECTION.
      * 999003-ADICIONA-VRV-MNT        SECTION.
      * 999004-ALINHA-NUMERO-ESQUERDA  SECTION.
      * ---------------------------------------
-INC HLPKP999
      * ---------------------------------------
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *----------------------------------------
-INC SGTKP999

      *----------------------------------------
      * 999010-TRATA-ERRO-DB2          SECTION.
      *----------------------------------------
      * -INC HLPKP997

      * ---------------------------------------
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
      * ---------------------------------------
-INC MSGKPMG1
      *-----------------------------------------------------------------
       999999-ENCERRAR                 SECTION.
      *-----------------------------------------------------------------

           EXEC CICS
                RETURN
           END-EXEC
           .
       999999-SAI.
           EXIT.                                                           00009
      *---------------------- FIM SGTS0612 -----------------------------
