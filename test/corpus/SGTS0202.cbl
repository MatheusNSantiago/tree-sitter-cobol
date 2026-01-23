       CBL ARITH(EXTEND)
      *-----------------------------------------------------------------
      * PROGRAMA..: SGTS0202
      * ANALISTA..: F2804421 - Eliana Cristina Ramos da Silva
      * AUTOR.....: F2804421 - Eliana Cristina Ramos da Silva
      * DATA......: 03/09/2018
      * OBJETIVO..: Negócio  - Efetuar as validações NÃO IMPEDITIVAS no
      *             processo de finalização de cadastramento da versão
      *             da operaçãO.
      * COMPILACAO: COBOL/CICS
      *-----------------------------------------------------------------
      * VRS004 23/01/2020- F2804421 2016/223992 -Incluir tratamento de
      *                 anexo tipo 16-Escritura para 'Debentures' e
      *                 anexo tipo 4 ='Instrumento de Form.Registrado'
      *                 para outros ativos.
      *        07/02/2020 - F9158937 - Acerto no calculo dos campos
      *                                VL-TARF.
      * VRS003 05/06/2019 - F2804421 2016/145195 -Incluir campo editado
      *              para monitorar campos comp-3 com decimais.
      * VRS002 04/04/2019 - F2804421 2016/145195 - Incluir os campos
      *              SGTS0224-CD-RGR-SCDR , SGTS0224-NR-VRS-RGR-SCDR e
      *              SGTS0224-SGTS0224-NR-SEQL-OGNL para atende a grava-
      *              ção de regra secundária que é chamada a partir de
      *              uma regra principal de exceção.
      *        20/02/2019 F2804421 DE 2016/145195 -Acerto mensageria
      *         para os códigos 9003 e 9004.
      * VRS001  10/01/2018 F2804421 DE 2016/145195 - Aceitar valor
      *         do saldo da carteira(SGTS0202-VL-SDO-CTRA)igual a zeros,
      *         e sinalizar justificativa para tarifa calculada = zeros.
      *         03/09/2018 F2804421 DE 2016/145195 - Implantação
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTS0202.
      *
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
      *-----------------------------------------------------------------
      *
      *-----------------------------------------------------------------
       CONFIGURATION                   SECTION.
      *-----------------------------------------------------------------
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------
      *
       WORKING-STORAGE                 SECTION.
      *-----------------------------------------------------------------

       77  CTE-INICIO-SS               PIC  X(40)  VALUE
                                            '*** S.S. COMEÇA AQUI ***'.
       77  CTE-PROG                    PIC  X(18)  VALUE
                                            '*** SGTS0202 ***'.
       77  CTE-VERS                    PIC  X(06)  VALUE 'VRS004'.

      * Módulos acionados
       77  SGTST005                    PIC  X(08) VALUE 'SGTST005'.
       77  SGTSL081                    PIC  X(08) VALUE 'SGTSL081'.
       77  SGTSL084                    PIC  X(08) VALUE 'SGTSL084'.
       77  SGTS0224                    PIC  X(08) VALUE 'SGTS0224'.

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------
      * ?reas de trabalho

       01 IN-EXITENCIA-REGISTRO            PIC X(1).
          88 REGISTRO-NAO-EXISTE           VALUE  'N'.
          88 REGISTRO-EXISTE               VALUE  'S'.

       01  IND                         PIC S9(04) COMP-5 VALUE 0.

       01  QT-REG-SEL                  PIC S9(4) COMP-5 VALUE 0.
       01  CD-TIP-ARQ                  PIC S9(4) COMP-5 VALUE 0.
       01  CD-TIP-FUC-CLI              PIC S9(4) COMP-5 VALUE 0.

      * Variáveis utilizadas no tratamento de tarifas.
       01  VL-CALC-01                  PIC  S9(15)V9(02) COMP-3 VALUE 0.
       01  VL-CALC-02                  PIC  S9(15)V9(02) COMP-3 VALUE 0.

      * Variáveis utilizadas no tratamento de tarifas.
       01  CD-TIP-TARF                 PIC  S9(4)        COMP-5 VALUE 0.
       01  PC-TARF-OPR-DIST            PIC  S9(04)V9(10) COMP-3 VALUE 0.
       01  PC-TARF-OPR-ETRO            PIC  S9(04)V9(10) COMP-3 VALUE 0.
       01  PC-TARF-OPR-GRT             PIC  S9(04)V9(10) COMP-3 VALUE 0.
       01  VL-TARF-DIST                PIC  S9(15)V9(02) COMP-3 VALUE 0.
       01  VL-TARF-ETRO                PIC  S9(15)V9(02) COMP-3 VALUE 0.
       01  VL-TARF-GRT                 PIC  S9(15)V9(02) COMP-3 VALUE 0.

      * Variáveis utilizadas p/campos numericos com decimais mensageria.
      *--> Máscaras edição de variáveis p/ monitoração mensagens de erro
       01  MCR-S9-15-V9-02            PIC  -.---.---.---.--9,99.

      **** mensagens de erro emitidas neste módulo - mensageria *******
      *
      * 001 'Código da carteira de título valores mobiliários inválido!
      * 002 'Código da carteira de título e valores mobiliários
      *              ' não informado.'
      * 003 'Código operação estruturada mercado de capitais não
      *     numérico.'
      * 004 'Código da operação estruturada do mercado de capit
      *              'ais inválido.'
      * 005 'Código operação estruturada mercado de capitais nã
      *              'o informado.'
      * 006 'Número versão operação estruturada inválido.'
      * 007 'Número da versão da operação estruturada não informado.'
      *     'Erro no cálculo do valor de tarifa Distribuição.'
      *     'Erro no cálculo do valor de tarifa Estruturação.'
      *     'Erro no cálculo do valor de tarifa Garantia Firme.'
      *     'Erro na totalização de valor de tarifas.'
      * 082 'Erro no incremento(totalização)do índice(indexador) da
      *     'tabela.' - 1825911
      * 796 'Erro no cálculo do valor da tarifa de distribuição.'
      * 797 'Erro no cálculo do valor da tarifa de estruturação.'
      * 798 'Erro no cálculo do valor da tarifa de garantia firme.'
      * 799 'Erro na totalização do valor das tarifas.'
      * 800 'Indexador de ocorrências da tabela é maior que o limite.'
      * 841 'Valor do limite financeiro não numérico'
      * 842 'Valor do limite financeiro não informado'
      * ??? 'Valor do limite financeiro negativo'
      * 843 'Valor do saldo da carteira não numérico'
      * 844 'Valor do saldo da carteira não informado.'
      * 845 'Valor do saldo estimado para encarteiramento não numérico'
      * 851 'Valor do limite financeiro não numérico'
      *3004 'Erro ao acionar o módulo '
      *----------------------------------------------------------------
      *CD-PRM-VLDD   TX-DCR-PRM-VLDD
      *
      *  1         'Aviso de Liquidação Financeira'
      *  2         'Contrato de Garantia'
      *  3         'Declaração de Conformidade'
      *  4         'Instrumento de Formalização Registrado'** p/ outros
      *  4         'Escritura' **somente para ativo = Debentures.
      *  5         'Limite de Crédito'
      *  6         'Súmula da Agência'
      *  7         'Precificação Válida'
      *  8         'Tarifa'
      *  9         'Operação com Garantia'
      * 10         'Nota Técnica Aprovação'
      * 11         'Agente Fiduciário'
      * 12         'Limite Financeiro da Carteira'
      * 13         'Exceção'
      *
      *
      *-----------------------------------------------------------------
      * Books dos Containers e das subrotinas chamadas
      *-----------------------------------------------------------------

      * Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1
      * Book para captura de abend cics do bloco código HLPKW999
-INC HLPKW999
      * Book para captura de erro via MSG1
-INC MSGKWMG1
      * Book do container de requisição e resposta deste módulo
       01 SGTS0202-DADOS.
-INC SGTK0202
      * tabela OPR_ETRD_MCP
       01  SGTST005-DADOS.
-INC SGTKT005
      * Consulta lista da tabela PRV_CMPS_GRT
-INC SGTKL081
      * Consulta lista da tabela TARF_OPR_ETRD
-INC SGTKL084
      *Negocial Verificar regra de exceção da operação e traz a lista
      *         dos dados para serem apresentados e gravados.
       01  SGTS0224-DADOS.
-INC SGTK0224
      * Book do Container de Erro
-INC IIBKERRO

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

       01  CTE-FINAL-SS                PIC  X(40)      VALUE
                    '*** S.S. TERMINA AQUI ***'.

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------

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
           MOVE 001                    TO MSG1-CD-SEQL-ERRO

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

           PERFORM 110000-VALIDAR-REQUISICAO
           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-VALIDAR-REQUISICAO       SECTION.
      *-----------------------------------------------------------------
           MOVE 002                    TO MSG1-CD-SEQL-ERRO

      *--> Validar Chave da Operação Estruturada
           IF  SGTS0202-CD-CTRA-TIT-VL     LESS ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0001              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' inválido.'      TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0202-CD-CTRA-TIT-VL     EQUAL ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0002              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0202-CD-OPR-ETRD-MCP    NOT NUMERIC
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE  0003              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada mercado de capitais nã
      -              'o numérico.'     TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0202-CD-OPR-ETRD-MCP    LESS  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0004              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada do mercado de capit
      -              'ais inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0202-CD-OPR-ETRD-MCP    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0005              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada mercado de capitais nã
      -              'o informado.'    TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0202-NR-VRS-OPR-ETRD    LESS  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0006              TO  MSG1-CD-ERRO-TS
               MOVE  'Número versão operação estruturada inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0202-NR-VRS-OPR-ETRD    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0007              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação estruturada não infor
      -              'mado.'           TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF
      *
      * Valores da carteira:

           IF  SGTS0202-VL-LIM-FNCO    NOT NUMERIC
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE  0841              TO  MSG1-CD-ERRO-TS
               MOVE  'Valor limite financeiro não númerico.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0202-VL-LIM-FNCO    EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE  0842              TO  MSG1-CD-ERRO-TS
               MOVE  'Valor limite financeiro não informado.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0202-VL-LIM-FNCO    LESS ZEROS
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE  0851              TO  MSG1-CD-ERRO-TS
               MOVE  'Valor do limite financeiro negativo.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0202-VL-SDO-CTRA    NOT NUMERIC
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE  0843              TO  MSG1-CD-ERRO-TS
               MOVE  'Valor saldo da carteira não numérico'
                                       TO MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *     IF  SGTS0202-VL-SDO-CTRA    EQUAL ZEROS
      *         SET  ERRO-PROGRAMADO    TO  TRUE
      *         MOVE  0844              TO  MSG1-CD-ERRO-TS
      *         MOVE  'Valor saldo da carteira não informado.'
      *                                 TO MSG1-TX-ERRO-TS
      *         PERFORM  999000-GRAVA-ERRO-E-ENCERRA
      *     END-IF

           IF  SGTS0202-VL-SDO-PRVT-ENCT    NOT NUMERIC
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE  0845              TO  MSG1-CD-ERRO-TS
               MOVE  'Valor saldo estimado para encarteiramento não numé
      -              'rico.'          TO MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       110000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-PROCESSAR                SECTION.
      *-----------------------------------------------------------------

           PERFORM 210000-CONSULTA-OPERACAO

           PERFORM 220000-VALIDA-ANEXOS

           PERFORM 230000-VALIDA-TARIFAS

           PERFORM 240000-VALIDA-GARANTIA

           PERFORM 250000-VALIDA-NOTA-TECNICA

           PERFORM 260000-VALIDA-AGENTE

           PERFORM 270000-VALIDA-LIM-FINANCEIRO

           PERFORM 280000-VALIDA-EXCECAO

           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       210000-CONSULTA-OPERACAO      SECTION.
      *-----------------------------------------------------------------
           MOVE 003                    TO MSG1-CD-SEQL-ERRO

      *    Movimenta o nome do programa a ser executado
           MOVE SGTST005               TO NM-PGM

           INITIALIZE  SGTST005-DADOS

           MOVE SGTS0202-CD-CTRA-TIT-VL
                                       TO SGTST005-CD-CTRA-TIT-VL
           MOVE SGTS0202-CD-OPR-ETRD-MCP
                                       TO SGTST005-CD-OPR-ETRD-MCP
           MOVE SGTS0202-NR-VRS-OPR-ETRD
                                       TO SGTST005-NR-VRS-OPR-ETRD

      *    Movimentar os campos de entrada
           SET SGTST005-CONSULTAR      TO TRUE

           PERFORM 910000-EXECUTAR-PGM-COMMAREA

           .
       210000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       220000-VALIDA-ANEXOS            SECTION.
      *-----------------------------------------------------------------
           MOVE 004                    TO MSG1-CD-SEQL-ERRO
      *
      *Parametro  cd_tip_arq       (descrição do anexo na tela)
      * da tela  (arq_opr_etrd)
      *  1         1              -'Aviso de Liquidação Financeira'
      *  2         2              -'Contrato de Garantia'
      *  3         3             -'Declaração de Conformidade'
      *  4        16  - 'Escritura' (somente para ativo =  debentures!)
      *  4         4  -'Instrumento de Formalização Registrado' (outros)
      *  5         5             -'Limite de Crédito'
      *  6         8             -'Precificação Válida'
      *  7         6             -'Súmula da Agência.'

      *  Verifica parametro 1 tela - 1='Aviso de Liquidação Financeira'
           MOVE 1                      TO SGTS0202-CD-PRM-AVS-LQDC
                                          CD-TIP-ARQ
           PERFORM 800001-CONSULTA-ANEXO
           IF REGISTRO-NAO-EXISTE
              MOVE 'N'                 TO SGTS0202-IN-PRM-AVS-LQDC
           ELSE
              MOVE 'S'                 TO SGTS0202-IN-PRM-AVS-LQDC
           END-IF

      *  Verifica parametro 2 tela - 2= 'Contrato de Garantia'
           MOVE 005                    TO MSG1-CD-SEQL-ERRO
           MOVE 2                      TO SGTS0202-CD-PRM-CTR-GRT
                                          CD-TIP-ARQ
           PERFORM 800001-CONSULTA-ANEXO
           IF REGISTRO-NAO-EXISTE
              MOVE 'N'                 TO SGTS0202-IN-PRM-CTR-GRT
           ELSE
              MOVE 'S'                 TO SGTS0202-IN-PRM-CTR-GRT
           END-IF

      *  Verifica parametro 3 tela - 3= 'Declaração de Conformidade'
           MOVE 006                    TO MSG1-CD-SEQL-ERRO
           MOVE 3                      TO SGTS0202-CD-PRM-DCL-CFMD
                                          CD-TIP-ARQ
           PERFORM 800001-CONSULTA-ANEXO
           IF REGISTRO-NAO-EXISTE
              MOVE 'N'                 TO SGTS0202-IN-PRM-DCL-CFMD
           ELSE
              MOVE 'S'                 TO SGTS0202-IN-PRM-DCL-CFMD
           END-IF

      *  Verifica parametro 4 tela  onde o codigo anexo pode ser:
      *   16= 'Escritura' somente para  Debentures OU
      *   4 = 'Instrumento de Form. Registrado' para outros.
           MOVE 007                    TO MSG1-CD-SEQL-ERRO
           MOVE 4                      TO SGTS0202-CD-PRM-ITMT-FRMZ

           IF  SGTST005-CD-PRD         EQUAL 427    AND
               SGTST005-CD-MDLD        EQUAL 2
               MOVE  16                TO CD-TIP-ARQ
           ELSE
               MOVE   4                TO CD-TIP-ARQ
           END-IF
           PERFORM 800001-CONSULTA-ANEXO
           IF REGISTRO-NAO-EXISTE
              MOVE 'N'                 TO SGTS0202-IN-PRM-ITMT-FRMZ
           ELSE
              MOVE 'S'                 TO SGTS0202-IN-PRM-ITMT-FRMZ
           END-IF

      *  Verifica parametro 5 tela - 5 = 'Limite de Crédito
           MOVE 008                    TO MSG1-CD-SEQL-ERRO
           MOVE  5                     TO SGTS0202-CD-PRM-LIM-CRD
                                          CD-TIP-ARQ
           PERFORM 800001-CONSULTA-ANEXO
           IF REGISTRO-NAO-EXISTE
              MOVE 'N'                 TO SGTS0202-IN-PRM-LIM-CRD
           ELSE
              MOVE 'S'                 TO SGTS0202-IN-PRM-LIM-CRD
           END-IF

      *  Verifica parametro 6 tela - 8 = 'Precificação Válida' *
           MOVE 009                    TO MSG1-CD-SEQL-ERRO
           MOVE  6                     TO SGTS0202-CD-PRM-PREF-VLDC
           MOVE  8                     TO CD-TIP-ARQ
           PERFORM 800001-CONSULTA-ANEXO
           IF REGISTRO-NAO-EXISTE
              MOVE 'N'                 TO SGTS0202-IN-PRM-PREF-VLDC
           ELSE
              MOVE 'S'                 TO SGTS0202-IN-PRM-PREF-VLDC
           END-IF

      *  Verifica parametro 7 tela - 6= 'Súmula da Agência'
           MOVE 010                    TO MSG1-CD-SEQL-ERRO
           MOVE  7                     TO SGTS0202-CD-PRM-SUMULA-AG
           MOVE  6                     TO CD-TIP-ARQ
           PERFORM 800001-CONSULTA-ANEXO
           IF REGISTRO-NAO-EXISTE
              MOVE 'N'                 TO SGTS0202-IN-PRM-SUMULA-AG
           ELSE
              MOVE 'S'                 TO SGTS0202-IN-PRM-SUMULA-AG
           END-IF

           .
       220000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       230000-VALIDA-TARIFAS           SECTION.
      *-----------------------------------------------------------------
           MOVE 011                    TO MSG1-CD-SEQL-ERRO

      *  Verifica parametro 8 da tela - 'Tarifa'
           MOVE 8                      TO SGTS0202-CD-PRM-TARF

      * Consulta lista de tarifas conforme o CD-TIP-TARF:
      * 1 - Distribuição
      * 2 - Estruturação
      * 3 - Garantia Firme
      *
           PERFORM 800002-CONSULTA-TARIFAS

           COMPUTE PC-TARF-OPR-DIST = PC-TARF-OPR-DIST / 100
           IF PC-TARF-OPR-DIST > ZEROS
              MULTIPLY PC-TARF-OPR-DIST BY     SGTST005-VL-CPR-OPR
                                        GIVING VL-TARF-DIST
                   ON SIZE ERROR
                   SET ERRO-PROGRAMADO   TO TRUE
                   MOVE 796              TO MSG1-CD-ERRO-TS
                   MOVE 'Erro no cálculo do valor de tarifa Distribuição
      -                 '.'              TO MSG1-TX-ERRO-TS
                   PERFORM 999000-GRAVA-ERRO-E-ENCERRA
              END-MULTIPLY
           END-IF

           COMPUTE PC-TARF-OPR-ETRO = PC-TARF-OPR-ETRO / 100
           IF PC-TARF-OPR-ETRO  > ZEROS
              MULTIPLY PC-TARF-OPR-ETRO BY     SGTST005-VL-CPR-OPR
                                        GIVING VL-TARF-ETRO
                   ON SIZE ERROR
                   SET ERRO-PROGRAMADO   TO TRUE

                   MOVE 797              TO MSG1-CD-ERRO-TS
                   MOVE 'Erro no cálculo do valor de tarifa Estruturação
      -                 '.'              TO MSG1-TX-ERRO-TS
                   PERFORM 999000-GRAVA-ERRO-E-ENCERRA
              END-MULTIPLY
           END-IF

           COMPUTE PC-TARF-OPR-GRT = PC-TARF-OPR-GRT / 100
           IF PC-TARF-OPR-GRT   > ZEROS
              MULTIPLY PC-TARF-OPR-GRT  BY     SGTST005-VL-CPR-OPR
                                        GIVING VL-TARF-GRT
                   ON SIZE ERROR
                   SET ERRO-PROGRAMADO   TO TRUE
                   MOVE 798              TO MSG1-CD-ERRO-TS
                   MOVE 'Erro no cálculo do valor de tarifa Garantia Fir
      -                 'me.'            TO MSG1-TX-ERRO-TS
                   PERFORM 999000-GRAVA-ERRO-E-ENCERRA
              END-MULTIPLY
           END-IF

      *  Calcula valor total das tarifas.
      *  VL-TTL-TARF
      *
           ADD   VL-TARF-DIST  VL-TARF-ETRO VL-TARF-GRT
                                            TO SGTS0202-VL-TTL-TARF
                 ON SIZE ERROR
                 SET   ERRO-PROGRAMADO    TO  TRUE
                 MOVE  799                TO  MSG1-CD-ERRO-TS
                 MOVE  'Erro na totalização de valor de tarifas'
                                          TO  MSG1-TX-ERRO-TS
                 PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-ADD

           IF   SGTS0202-VL-TTL-TARF    >   ZEROS
               MOVE 'S'                 TO SGTS0202-IN-PRM-TARF
           ELSE
               MOVE 'N'                 TO SGTS0202-IN-PRM-TARF
           END-IF

           .
       230000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       240000-VALIDA-GARANTIA          SECTION.
      *-----------------------------------------------------------------
           MOVE 012                    TO MSG1-CD-SEQL-ERRO

      *  Verifica parametro 9 da tela - 'Operação com Garantia'
           MOVE 9                      TO SGTS0202-CD-PRM-OPR-GRT

           PERFORM 800003-CONSULTA-GARANTIA

           IF SGTSL081-QT-OCR-TAB  = ZEROS
              MOVE 'N'                 TO SGTS0202-IN-PRM-OPR-GRT
           ELSE
              MOVE 'S'                 TO SGTS0202-IN-PRM-OPR-GRT
           END-IF

           .
       240000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       250000-VALIDA-NOTA-TECNICA      SECTION.
      *-----------------------------------------------------------------
           MOVE 013                    TO MSG1-CD-SEQL-ERRO

      *  Verifica parametro 10 da tela - 'Nota Técnica Aprovação.'
           MOVE 10                     TO SGTS0202-CD-PRM-NOTA-TCN

           PERFORM 800004-CONSULTA-NOTA-TECNICA

           IF REGISTRO-NAO-EXISTE
              MOVE 'N'                 TO SGTS0202-IN-PRM-NOTA-TCN
           ELSE
              MOVE 'S'                 TO SGTS0202-IN-PRM-NOTA-TCN
           END-IF

           .
       250000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       260000-VALIDA-AGENTE            SECTION.
      *-----------------------------------------------------------------
           MOVE 014                    TO MSG1-CD-SEQL-ERRO

      *  Verifica parametro 11 da tela - 'Agente Fiduciário'
           MOVE 11                     TO SGTS0202-CD-PRM-AGT-FDAR

      *  Verifica se existe agente fiduciário *
           MOVE 4                      TO CD-TIP-FUC-CLI

           PERFORM 800005-CONSULTA-AGENTE
           IF REGISTRO-NAO-EXISTE
              MOVE 'N'                 TO SGTS0202-IN-PRM-AGT-FDAR
           ELSE
              MOVE 'S'                 TO SGTS0202-IN-PRM-AGT-FDAR
           END-IF

           .
       260000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       270000-VALIDA-LIM-FINANCEIRO    SECTION.
      *-----------------------------------------------------------------
      *  Verifica parametro 12       - 'Limite Financeiro'
            MOVE 12                     TO SGTS0202-CD-PRM-LIM-FNCO

      *-----------------------------------------------------------------
      *  Verifica limite financeiro da carteira *
      *  SGTST005-VL-CPR-OPR   = Valor da transação desembolsado pelo
      *                        banco para encarteirar uma operação
      *                        estruturada no mercado de capitais.
      *  Valor da compra deverá ser < =
      *       (Valor Limite Financeiro da Carteira) -
      * VL-CALC-01:
      *      (Saldo da Carteira + Saldo estimado para Encarteiramento).
      *
      * Calcular também o "Saldo disponível":
      *  "Saldo disponível" = VL-CALC-02 -  SGTST005-VL-CPR-OPR
      *
      * Se for negativo deverá ser apresentado na tela, neste caso
      * houve uma inconsistencia ( SGTS0202-IN-PRM-LIM-FNCO = 'S')
      *-----------------------------------------------------------------
      *
           ADD SGTS0202-VL-SDO-CTRA TO SGTS0202-VL-SDO-PRVT-ENCT
              GIVING  VL-CALC-01
              ON SIZE ERROR
              MOVE  0839               TO  MSG1-CD-ERRO-TS
              MOVE 'Erro no cálculo de soma.'
                                       TO  MSG1-TX-ERRO-TS
              MOVE 016                 TO  MSG1-CD-SEQL-ERRO
              PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-ADD
      *
           SUBTRACT VL-CALC-01 FROM  SGTS0202-VL-LIM-FNCO
              GIVING  VL-CALC-02
              ON SIZE ERROR
              MOVE  0840               TO  MSG1-CD-ERRO-TS
              MOVE 'Erro no cálculo de subtração.'
                                       TO  MSG1-TX-ERRO-TS
              MOVE 017                 TO  MSG1-CD-SEQL-ERRO
              PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-SUBTRACT
      *
           SUBTRACT SGTST005-VL-CPR-OPR  FROM   VL-CALC-02
              GIVING  SGTS0202-VL-SDO-DSPN
              ON SIZE ERROR
              MOVE  0840               TO  MSG1-CD-ERRO-TS
              MOVE 'Erro no cálculo de subtração.'
                                       TO  MSG1-TX-ERRO-TS
              MOVE 017                 TO  MSG1-CD-SEQL-ERRO
              PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-SUBTRACT
      *
           IF  SGTS0202-VL-SDO-DSPN   LESS ZEROS
               MOVE 'N'         TO SGTS0202-IN-PRM-LIM-FNCO
           ELSE
               MOVE 'S'         TO SGTS0202-IN-PRM-LIM-FNCO
           END-IF

           .
       270000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       280000-VALIDA-EXCECAO           SECTION.
      *-----------------------------------------------------------------
      *   Verifica parametro 13       - 'Limite Exceção'
           MOVE 13                     TO SGTS0202-CD-PRM-EXCO

      *  Repassa resultados do SGTS0224 (que aciona o SGTS0163),
      *  no registro de saída do SGTS0202.
      *  Todos os dados de saída deste módulo deverão ser armazenados
      *  até o momento da efetivação do cadastro da operação.
      *  (Via botão confirmar da tela).
      * Para a apresentação na tela deverá ser considerado como regra
      * principal o campo SGTS0202-CD-RGR-VLDC-ITC  e o o campo sequen-
      * cial das instruções o campo SGTS0202-NR-SEQL-ITC.
      *
      * Após acionar o botão de 'Finalizar' na tela, os campos a serem
      * passados para o programa de atualização (SGTS0230) para a atua-
      * lização são:
      * SGTS0202-CD-RGR-SCDR (campo regra secundária, que no caso de ter
      * somente regra principal, retorna nesse campo a regra principal)
      * SGTS0202-NR-VRS-RGR-SCDR
      * SGTS0202-NR-SEQL-OGNL (possui o nr sequencial de instruções da
      * regra que deverá ser gravado na tabela.
      *
      *****************************************************************

           PERFORM 800006-CONSULTA-EXCECAO

           MOVE 018                    TO MSG1-CD-SEQL-ERRO

           IF SGTS0224-QT-REG-RGR      EQUAL  ZEROS
              MOVE 'S'                 TO SGTS0202-IN-PRM-EXCO
           ELSE
              MOVE 'N'                 TO SGTS0202-IN-PRM-EXCO

              PERFORM 281000-CARREGA-EXCECAO

           END-IF

           .
       280000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       281000-CARREGA-EXCECAO          SECTION.
      *-----------------------------------------------------------------
           MOVE 019                    TO MSG1-CD-SEQL-ERRO

      *  Carrega Timestamp do momento de excecução da regra.
      *  que ocorre no programa SGTS0224 após chamada do SGTS0163.

           MOVE SGTS0224-TS-ALT        TO SGTS0202-TS-ALT

           MOVE SGTS0224-QT-REG-RGR    TO SGTS0202-QT-REG-RGR
           MOVE SGTS0224-QT-REG-ITEM   TO SGTS0202-QT-REG-ITEM
           MOVE SGTS0224-QT-REG-RLZD   TO SGTS0202-QT-REG-RLZD

      *  Carrega dados da regra principal
           PERFORM VARYING IND   FROM 1 BY  1
                     UNTIL IND   GREATER SGTS0224-QT-REG-RGR
               MOVE SGTS0224-CD-RGR-VLDC (IND)
                                       TO SGTS0202-CD-RGR-VLDC (IND)
               MOVE SGTS0224-NR-VRS-RGR-VLDC (IND)
                                       TO SGTS0202-NR-VRS-RGR-VLDC (IND)
           END-PERFORM

      *  Carrega dados dos itens da regra.
           MOVE 020                    TO MSG1-CD-SEQL-ERRO
           PERFORM VARYING IND   FROM 1 BY  1
                     UNTIL IND   GREATER SGTS0224-QT-REG-ITEM

               MOVE SGTS0224-CD-RGR-VLDC-ITC (IND)
                                       TO SGTS0202-CD-RGR-VLDC-ITC(IND)
               MOVE SGTS0224-CD-RGR-SCDR (IND)
                                       TO SGTS0202-CD-RGR-SCDR(IND)
               MOVE SGTS0224-NR-VRS-RGR-SCDR (IND)
                                       TO SGTS0202-NR-VRS-RGR-SCDR(IND)
               MOVE SGTS0224-QT-INDENTACAO (IND)
                                       TO SGTS0202-QT-INDENTACAO (IND)
               MOVE SGTS0224-IN-LNH-EXCO (IND)
                                       TO SGTS0202-IN-LNH-EXCO  (IND)
               MOVE SGTS0224-NR-SEQL-ITC (IND)
                                       TO SGTS0202-NR-SEQL-ITC (IND)
               MOVE SGTS0224-NR-SEQL-OGNL (IND)
                                       TO SGTS0202-NR-SEQL-OGNL (IND)
               MOVE SGTS0224-TIP-CMD-VLDC (IND)
                                       TO SGTS0202-TIP-CMD-VLDC(IND)
               MOVE SGTS0224-NR-GR-CND    (IND)
                                       TO SGTS0202-NR-GR-CND (IND)
               MOVE SGTS0224-CD-ATB-VLDC (IND)
                                       TO SGTS0202-CD-ATB-VLDC (IND)
               MOVE SGTS0224-CD-TIP-CFT-LGC (IND)
                                       TO SGTS0202-CD-TIP-CFT-LGC (IND)
               MOVE SGTS0224-CD-TIP-CFT-ITEM (IND)
                                       TO SGTS0202-CD-TIP-CFT-ITEM(IND)
               MOVE SGTS0224-VL-ITC-RGR (IND)
                                       TO SGTS0202-VL-ITC-RGR (IND)
               MOVE SGTS0224-NR-ITC-RGR (IND)
                                       TO SGTS0202-NR-ITC-RGR (IND)
               MOVE SGTS0224-TX-ITC-RGR (IND)
                                       TO SGTS0202-TX-ITC-RGR (IND)
           END-PERFORM

      *  Carrega dados dos valores realizados na regra.
           MOVE 021                    TO MSG1-CD-SEQL-ERRO
           PERFORM VARYING IND   FROM 1 BY  1
                     UNTIL IND   GREATER SGTS0224-QT-REG-RLZD

               MOVE SGTS0224-CD-RGR-VLDC-RLZD (IND)
                                     TO SGTS0202-CD-RGR-VLDC-RLZD (IND)
               MOVE SGTS0224-CD-ATB-RLZD (IND)
                                       TO SGTS0202-CD-ATB-RLZD (IND)
               MOVE SGTS0224-VL-ITC-RLZD (IND)
                                       TO SGTS0202-VL-ITC-RLZD (IND)
               MOVE SGTS0224-NR-ITC-RLZD (IND)
                                       TO SGTS0202-NR-ITC-RLZD (IND)
               MOVE SGTS0224-TX-ITC-RLZD (IND)
                                       TO SGTS0202-TX-ITC-RLZD (IND)
           END-PERFORM

           .
       281000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       800001-CONSULTA-ANEXO           SECTION.
      *-----------------------------------------------------------------
           MOVE 022                    TO MSG1-CD-SEQL-ERRO

           SET REGISTRO-NAO-EXISTE     TO TRUE
           MOVE    ZEROS               TO QT-REG-SEL

      *--- Tipos de arquivos validados pesquisados nestas tabelas*
      *     1   Aviso de Liquidação Financeira
      *     2   Contrato de Garantia
      *     3   Declaração de Conformidade
      *     4   Instrumento de Formalização Registrado
      *     5   Limite de Crédito
      *     6   Súmula da Agência
      *     8   Precificação Válida

           EXEC SQL
                SELECT 1
                INTO  :QT-REG-SEL
                FROM  DB2SGT.ANX_OPR_ETRD A
                INNER JOIN
                      DB2SGT.ARQ_OPR_ETRD B
                ON
                      A.CD_ARQ_OPR_ETRD = B.CD_ARQ_OPR_ETRD
                WHERE
                      A.CD_CTRA_TIT_VL   = :SGTS0202-CD-CTRA-TIT-VL
                  AND A.CD_OPR_ETRD_MCP  = :SGTS0202-CD-OPR-ETRD-MCP
                  AND A.NR_VRS_OPR_ETRD  = :SGTS0202-NR-VRS-OPR-ETRD
                  AND B.CD_TIP_ARQ       = :CD-TIP-ARQ
                FETCH FIRST 1 ROW ONLY
             END-EXEC

           IF  QT-REG-SEL GREATER ZEROS
               SET REGISTRO-EXISTE TO TRUE
           END-IF

           .
       800001-SAI.
           EXIT.

      *-----------------------------------------------------------------
       800002-CONSULTA-TARIFAS         SECTION.
      *-----------------------------------------------------------------
           MOVE 023                    TO MSG1-CD-SEQL-ERRO

      * CD-TIP-TARF
      * 1 - Distribuição
      * 2 - Estruturação
      * 3 - Garantia Firme
      *
           MOVE ZEROS                  TO SGTSL084-QT-OCR-TAB
                                          SGTSL084-IC-TAB
           MOVE 'N'                    TO SGTSL084-IN-CNTO-PSQ
      *
      *    Movimentar os campos de entrada da lista
           MOVE SGTS0202-CD-CTRA-TIT-VL
                                       TO SGTSL084-CD-CTRA-TIT-VL
           MOVE SGTS0202-CD-OPR-ETRD-MCP
                                       TO SGTSL084-CD-OPR-ETRD-MCP
           MOVE SGTS0202-NR-VRS-OPR-ETRD
                                       TO SGTSL084-NR-VRS-OPR-ETRD
           MOVE ZEROS                  TO SGTSL084-CD-TIP-TARF-E

           MOVE SGTSL084               TO NM-PGM

           PERFORM 900000-EXECUTAR-MODULO

      * Obtem % de cada tarifa encontrada para esta cart./oper/versão.
           PERFORM VARYING SGTSL084-IC-TAB FROM 1 BY 1
                     UNTIL SGTSL084-IC-TAB > SGTSL084-QT-OCR-TAB

               EVALUATE SGTSL084-CD-TIP-TARF (SGTSL084-IC-TAB)
                   WHEN 1
                         MOVE SGTSL084-PC-TARF-OPR (SGTSL084-IC-TAB)
                                       TO PC-TARF-OPR-DIST
                   WHEN 2
                         MOVE SGTSL084-PC-TARF-OPR (SGTSL084-IC-TAB)
                                       TO PC-TARF-OPR-ETRO
                   WHEN 3
                         MOVE SGTSL084-PC-TARF-OPR (SGTSL084-IC-TAB)
                                       TO PC-TARF-OPR-GRT
                   WHEN OTHER
                         CONTINUE
               END-EVALUATE
           END-PERFORM

           .
       800002-SAI.
           EXIT.

      *-----------------------------------------------------------------
       800003-CONSULTA-GARANTIA        SECTION.
      *-----------------------------------------------------------------
           MOVE 024                    TO MSG1-CD-SEQL-ERRO

      *    Movimentar os campos de entrada da lista
           MOVE ZEROS                  TO SGTSL081-QT-OCR-TAB
                                          SGTSL081-IC-TAB
           MOVE 'N'                    TO SGTSL081-IN-CTNC-PSQ

           MOVE SGTS0202-CD-CTRA-TIT-VL
                                       TO SGTSL081-CD-CTRA-TIT-VL
           MOVE SGTS0202-CD-OPR-ETRD-MCP
                                       TO SGTSL081-CD-OPR-ETRD-MCP
           MOVE SGTS0202-NR-VRS-OPR-ETRD
                                       TO SGTSL081-NR-VRS-OPR-ETRD

           MOVE SGTSL081               TO NM-PGM
           PERFORM 900000-EXECUTAR-MODULO

           .
       800003-SAI.
           EXIT.

      *-----------------------------------------------------------------
       800004-CONSULTA-NOTA-TECNICA    SECTION.
      *-----------------------------------------------------------------
           MOVE 025                    TO MSG1-CD-SEQL-ERRO

           SET REGISTRO-NAO-EXISTE     TO TRUE
           MOVE    ZEROS               TO QT-REG-SEL

      *--- Tipo de documento da nota a ser pesquisado.*
      *     1 -  'Aprovação da Operação'

           EXEC SQL

                SELECT 1
                INTO  :QT-REG-SEL
                FROM   DB2SGT.NOTA_TCN_VCLD_OPR  A
                INNER JOIN
                      DB2SGT.NOTA_TCN_OPR_ETRD B
                   ON
                      A.NR_NOTA_TCN_OPR = B.NR_NOTA_TCN_OPR
                WHERE    A.CD_CTRA_TIT_VL   = :SGTS0202-CD-CTRA-TIT-VL
                  AND    A.CD_OPR_ETRD_MCP  = :SGTS0202-CD-OPR-ETRD-MCP
                  AND    A.NR_VRS_OPR_ETRD  = :SGTS0202-NR-VRS-OPR-ETRD
                  AND    B.CD_TIP_DOC_TCN   = 1
                FETCH FIRST 1 ROW ONLY
           END-EXEC

           IF  QT-REG-SEL GREATER ZEROS
               SET REGISTRO-EXISTE TO TRUE
           END-IF

           .
       800004-SAI.
           EXIT.

      *-----------------------------------------------------------------
       800005-CONSULTA-AGENTE          SECTION.
      *-----------------------------------------------------------------
           MOVE 026                    TO MSG1-CD-SEQL-ERRO

      * CD-TIP-FUC-CLI
      * 1 - Emissor
      * 2 - Impacto Limite
      * 3 - Originador/Cedente
      * 4 - Agente Fiduciário/Notas
      * 5 - Administrador

           SET REGISTRO-NAO-EXISTE     TO TRUE
           MOVE    ZEROS               TO QT-REG-SEL

           EXEC SQL
                 SELECT 1
                   INTO  :QT-REG-SEL
                   FROM DB2SGT.CLI_OPR_ETRD
                WHERE
                     CD_CTRA_TIT_VL  = :SGTS0202-CD-CTRA-TIT-VL
                AND  CD_OPR_ETRD_MCP = :SGTS0202-CD-OPR-ETRD-MCP
                AND  NR_VRS_OPR_ETRD = :SGTS0202-NR-VRS-OPR-ETRD
                AND  CD_TIP_FUC_CLI  = :CD-TIP-FUC-CLI
                FETCH FIRST 1 ROW ONLY
           END-EXEC

           IF  QT-REG-SEL GREATER ZEROS
               SET REGISTRO-EXISTE TO TRUE
           END-IF

           .
       800005-SAI.
           EXIT.

      *-----------------------------------------------------------------
       800006-CONSULTA-EXCECAO         SECTION.
      *-----------------------------------------------------------------
           MOVE 027                    TO MSG1-CD-SEQL-ERRO

      *    Movimentar os campos de entrada da lista
           MOVE SGTS0202-CD-CTRA-TIT-VL
                                       TO SGTS0224-CD-CTRA-TIT-VL
           MOVE SGTS0202-CD-OPR-ETRD-MCP
                                       TO SGTS0224-CD-OPR-ETRD-MCP
           MOVE SGTS0202-NR-VRS-OPR-ETRD
                                       TO SGTS0224-NR-VRS-OPR-ETRD

           MOVE SGTS0224              TO NM-PGM

           PERFORM 900000-EXECUTAR-MODULO

           .
       800006-SAI.
           EXIT.
      *
      *-----------------------------------------------------------------
      *                                                                *
      *  SECTION DA FAIXA DE 900000 A 969900 PARA DEMAIS ROTINAS       *
      *                                                                *
      *-----------------------------------------------------------------
      * Executa um módulo implementado com CONTAINER                   *
      *-----------------------------------------------------------------
       900000-EXECUTAR-MODULO          SECTION.
      *-----------------------------------------------------------------

      *    Grava o container de requisição do módulo a executar
           PERFORM 982000-GRAVAR-CONTAINER-RQSC

      *    Limpa a resposta e erro antes de executar o módulo
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO

      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EVALUATE NM-PGM
               WHEN SGTSL081
                    EXEC CICS LINK
                              PROGRAM ( SGTSL081   )
                              CHANNEL ( NM-CNL     )
                              RESP    ( MSG1-RESP1 )
                              RESP2   ( MSG1-RESP2 )
                    END-EXEC
               WHEN SGTSL084
                    EXEC CICS LINK
                              PROGRAM ( SGTSL084   )
                              CHANNEL ( NM-CNL     )
                              RESP    ( MSG1-RESP1 )
                              RESP2   ( MSG1-RESP2 )
                    END-EXEC

               WHEN SGTS0224
                    EXEC CICS LINK
                              PROGRAM ( SGTS0224   )
                              CHANNEL ( NM-CNL     )
                              RESP    ( MSG1-RESP1 )
                              RESP2   ( MSG1-RESP2 )
                    END-EXEC
               WHEN OTHER
                    MOVE 9006          TO MSG1-CD-ERRO-TS
                    MOVE 'Não definida a execução do módulo.'
                                       TO MSG1-TX-ERRO-TS
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE
      *
           EVALUATE TRUE
               WHEN MSG1-RESP1        EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    MOVE 3004          TO MSG1-CD-ERRO-TS
                    MOVE 'Erro ao acionar o módulo.'
                                       TO MSG1-TX-ERRO-TS
                    SET ERRO-CICS      TO TRUE
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *    Efetua a leitura do Container de Erro
           PERFORM 972000-LER-CONTAINER-DE-ERRO

      *    Efetua a leitura do Container de Resposta
           PERFORM 973000-LER-RESPOSTA
           .
       900000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       910000-EXECUTAR-PGM-COMMAREA    SECTION.
      *-----------------------------------------------------------------

      *    Movimento nome do módulo para monitoração de variáveis
           MOVE SGTST005               TO NM-PGM

           EXEC CICS LINK
                PROGRAM  ( SGTST005       )
                COMMAREA ( SGTST005-DADOS )
           END-EXEC

           IF  SGTST005-CD-RTN-PGM     NOT EQUAL ZEROS
               SET ERRO-PROGRAMA-ACIONADO TO TRUE
               MOVE NM-PGM             TO MSG1-NM-PGM-ACND              D
               MOVE SGTST005-MSG-ERRO-PGM
                                       TO MSG1-TX-RTN-PGM-ACND
                                          MSG1-TX-ERRO-TS
               MOVE SGTST005-CD-RTN-PGM
                                       TO MSG1-CD-RTN-PGM-ACND
                                          MSG1-CD-ERRO-TS
      *        Monitorar variáveis do módulo chamado
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       910000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       969000-MONITORAR-VARIAVEIS      SECTION.
      *-----------------------------------------------------------------
      * Monitora as variáveis dos módulos chamados
      *-----------------------------------------------------------------
      * Variáveis exclusivas de cada programa acionado
           EVALUATE NM-PGM
             WHEN SGTST005
               MOVE 'SGTST005-CD-CTRA-TIT-VL' TO MSGKW999-NM-VRV-MNT
               MOVE  SGTST005-CD-CTRA-TIT-VL  TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTST005-CD-OPR-ETRD-MCP' TO MSGKW999-NM-VRV-MNT
               MOVE SGTST005-CD-OPR-ETRD-MCP   TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTST005-NR-VRS-OPR-ETRD' TO MSGKW999-NM-VRV-MNT
               MOVE  SGTST005-NR-VRS-OPR-ETRD TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTST005-ACAO' TO MSGKW999-NM-VRV-MNT
               MOVE  SGTST005-ACAO  TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

             WHEN SGTSL081
               MOVE 'SGTSL081-CD-CTRA-TIT-VL' TO MSGKW999-NM-VRV-MNT
               MOVE  SGTSL081-CD-CTRA-TIT-VL  TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTSL081-CD-OPR-ETRD-MCP' TO MSGKW999-NM-VRV-MNT
               MOVE SGTSL081-CD-OPR-ETRD-MCP   TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTSL081-NR-VRS-OPR-ETRD' TO MSGKW999-NM-VRV-MNT
               MOVE  SGTSL081-NR-VRS-OPR-ETRD  TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTSL081-NR-ULT-PRV-CMPS-GRT'
                                              TO MSGKW999-NM-VRV-MNT
               MOVE  SGTSL081-NR-ULT-PRV-CMPS-GRT
                                              TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

             WHEN SGTSL084
               MOVE 'SGTSL084-CD-CTRA-TIT-VL' TO MSGKW999-NM-VRV-MNT
               MOVE  SGTSL084-CD-CTRA-TIT-VL  TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTSL084-CD-OPR-ETRD-MCP' TO MSGKW999-NM-VRV-MNT
               MOVE SGTSL084-CD-OPR-ETRD-MCP   TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTSL084-NR-VRS-OPR-ETRD' TO MSGKW999-NM-VRV-MNT
               MOVE  SGTSL084-NR-VRS-OPR-ETRD  TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTSL084-CD-TIP-TARF-E'   TO MSGKW999-NM-VRV-MNT
               MOVE  SGTSL084-CD-TIP-TARF-E    TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

             WHEN SGTS0224
               MOVE 'SGTS0224-CD-CTRA-TIT-VL' TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0224-CD-CTRA-TIT-VL  TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0224-CD-OPR-ETRD-MCP' TO MSGKW999-NM-VRV-MNT
               MOVE SGTS0224-CD-OPR-ETRD-MCP   TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0224-NR-VRS-OPR-ETRD' TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0224-NR-VRS-OPR-ETRD  TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

              WHEN OTHER
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
           MOVE 028                    TO MSG1-CD-SEQL-ERRO

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
           MOVE 029                    TO MSG1-CD-SEQL-ERRO

      *    Monta o nome do container de requisição
           MOVE CTE-PROG(5:8)          TO NM-PGM
           MOVE 'SGTS0202-RQSC'        TO NM-CTNR

      *    Recupera o tamanho do container de requisição
           PERFORM 973100-LER-CONTAINER-NODATA

      *--> Obtem o tamanho esperado da requisição
           MOVE  LENGTH OF SGTS0202-RQSC
                                       TO  QT-CCT-EPRO

      *--> Valida tamanho esperado da requisição X tamanho recebido
           IF  QT-CCT-LIDO             NOT EQUAL  QT-CCT-EPRO
               SET   ERRO-TAMANHO-AREA TO  TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           MOVE 030                    TO MSG1-CD-SEQL-ERRO

      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Efetua a leitura do container
           EXEC CICS GET
                     CONTAINER ( NM-CTNR       )
                     FLENGTH   ( QT-CCT-LIDO   )
                     INTO      ( SGTS0202-RQSC )
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
                    PERFORM 969000-MONITORAR-VARIAVEIS
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
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

           MOVE NM-PGM                 TO NM-CTNR(1:8)
           MOVE '-RPST'                TO NM-CTNR(9:5)

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

               WHEN SGTSL081
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSL081-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN SGTSL084
                    EXEC CICS GET
                              CONTAINER ( NM-CTNR       )
                              INTO      ( SGTSL084-RPST )
                              FLENGTH   ( QT-CCT-LIDO   )
                    END-EXEC


               WHEN SGTS0224
                    EXEC CICS GET
                              CONTAINER ( NM-CTNR       )
                              INTO      ( SGTS0224-RPST )
                              FLENGTH   ( QT-CCT-LIDO   )
                    END-EXEC

               WHEN OTHER
                    MOVE 9003          TO MSG1-CD-ERRO-TS
                    MOVE 'Erro na leitura de resposta do módulo.'
                                       TO MSG1-TX-ERRO-TS
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
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
      *                                                                *
      * SECTION DA FAIXA DE 980000 A 989900 PARA GRAVACAO DE DADOS     *
      *                                                                *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       980000-GRAVAR-CONTAINER-RPST    SECTION.
      *----------------------------------------------------------------*

      *    Exclui eventual container de erro de outros programas
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO

           MOVE 031                    TO MSG1-CD-SEQL-ERRO

      *    Monta o nome do container de resposta

           MOVE 'SGTS0202-RPST'        TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

           EXEC CICS PUT
                     CONTAINER (NM-CTNR                )
                     FROM      (SGTS0202-RPST          )
                     FLENGTH   (LENGTH OF SGTS0202-RPST)
           END-EXEC

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       980000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       982000-GRAVAR-CONTAINER-RQSC    SECTION.
      *-----------------------------------------------------------------
      *    Postar container com dados de requisição (entrada)
      *-----------------------------------------------------------------

      *    Monta o nome dos container de acordo com modulo a executar
           MOVE SPACES                 TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE

      *    Quando módulos via IIB, fixo nome PRM-DADOS-RQSC, senão
      *    nome_modulo -RQSC

           MOVE NM-PGM        TO NM-CTNR(1:8)
           MOVE '-RQSC'       TO NM-CTNR(9:5)


      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Faz put da requisição do programa a executar
           EVALUATE NM-PGM
               WHEN SGTSL081
                    EXEC CICS PUT
                              CONTAINER ( NM-CTNR                 )
                              FROM      ( SGTSL081-RQSC           )
                              FLENGTH   ( LENGTH OF SGTSL081-RQSC )
                    END-EXEC
      *
               WHEN SGTSL084
                    EXEC CICS PUT
                              CONTAINER ( NM-CTNR                 )
                              FROM      ( SGTSL084-RQSC           )
                              FLENGTH   ( LENGTH OF SGTSL084-RQSC )
                    END-EXEC
      *
               WHEN SGTS0224
                    EXEC CICS PUT
                              CONTAINER ( NM-CTNR                 )
                              FROM      ( SGTS0224-RQSC           )
                              FLENGTH   ( LENGTH OF SGTS0224-RQSC )
                    END-EXEC

               WHEN OTHER
                    MOVE 9004          TO MSG1-CD-ERRO-TS
                    MOVE 'Erro de gravação da requisição do módulo.'
                                       TO MSG1-TX-ERRO-TS
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
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
           MOVE NM-PGM        TO NM-CTNR(1:8)
           MOVE '-RPST'       TO NM-CTNR(9:5)


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
           MOVE 032                    TO MSG1-CD-SEQL-ERRO
           MOVE 'PRM-ERRO'             TO NM-CTNR
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
       984000-GRAVAR-CONTAINER-ERRO    SECTION.
      *-----------------------------------------------------------------
           MOVE 033                    TO MSG1-CD-SEQL-ERRO

      *    Grava o erro gerado no container de erro
      *    Não há captura automática de erro CICS por usar RESP e RESP2
           MOVE 'PRM-ERRO'             TO NM-CTNR
           EXEC CICS PUT
                     CONTAINER ( NM-CTNR                     )
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

      *--> Adiciona as variáveis de entrada para monitoração
           MOVE 'SGTS0202-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0202-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0202-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
           IF  SGTS0202-CD-OPR-ETRD-MCP    NUMERIC
               MOVE  SGTS0202-CD-OPR-ETRD-MCP
                                       TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0202-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0202-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

      *
           MOVE 'SGTS0202-VL-LIM-FNCO'
                                       TO  MSGKW999-NM-VRV-MNT
           IF  SGTS0202-VL-LIM-FNCO    NUMERIC
               MOVE  SGTS0202-VL-LIM-FNCO
                                       TO  MCR-S9-15-V9-02
               MOVE  MCR-S9-15-V9-02   TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM 999003-ADICIONA-VRV-MNT
      *
           MOVE 'SGTS0202-VL-SDO-CTRA'
                                       TO  MSGKW999-NM-VRV-MNT
           IF  SGTS0202-VL-SDO-CTRA    NUMERIC
               MOVE  SGTS0202-VL-SDO-CTRA
                                       TO  MCR-S9-15-V9-02
               MOVE  MCR-S9-15-V9-02   TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM 999003-ADICIONA-VRV-MNT
      *
           MOVE 'SGTS0202-VL-SDO-PRVT-ENCT'
                                       TO  MSGKW999-NM-VRV-MNT
           IF  SGTS0202-VL-SDO-PRVT-ENCT    NUMERIC
               MOVE  SGTS0202-VL-SDO-PRVT-ENCT
                                       TO  MCR-S9-15-V9-02
               MOVE  MCR-S9-15-V9-02   TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM 999003-ADICIONA-VRV-MNT


      *--> Grava a Ocorrência de Erro no sistema MSG - Mensageria
           PERFORM MSG1-NTF-MSG

           MOVE  MSG1-CD-ERRO-TS       TO  IIBKERRO-CD-ERRO
           MOVE  MSG1-TX-ERRO-TS       TO  IIBKERRO-TX-ERRO

      *--> Grava a Ocorrência de Erro no Container de erro
           PERFORM 984000-GRAVAR-CONTAINER-ERRO

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
      *---------------------- Fim SGTS0202 ----------------------------*