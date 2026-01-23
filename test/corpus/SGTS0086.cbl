      *-----------------------------------------------------------------
      * PROGRAMA..: SGTS0086
      * ANALISTA..: F2054109 - Cintia A.Iglesias
      * AUTOR.....: F9188847 - Soila K. L. Akamatsu
      * DATA......: 28/11/2016
      * OBJETIVO..: Alterar Mandato da Operação Estruturada de Mercado
      *             de Capitais
      * COMPILACAO: COBOL/CICS
      *-----------------------------------------------------------------
      * VRS002 21/01/2019 F8785286 DE 2016/145195 - Inclusão monitoração
      *              variáveis para rotina SBDATA
      * VRS001 -  10/05/2018 - F8658869 - DE 2015/137233 - Inclusão do
      *                        campo carteira
      *        -  28/02/2018 - F9158937 - DE 2015/137233 - Retirada con-
      *                        tainer de resposta
      *        -  23/12/2016 - F2054109 - DE 2015/137233 -
      *                        compilado devido alteracao book sgtkt005
      *        -  28/11/2016 - F9188847 - DE 2015/137233 - IMPLANTACAO
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID. SGTS0086.
      *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *
      *----------------------------------------------------------------*
       DATA DIVISION.
      *----------------------------------------------------------------*
      *

       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

       77  CTE-INICIO-SS                 PIC  X(040)  VALUE
                    '*** S.S. COMEÇA AQUI ***'.

       77  CTE-PROG                      PIC  X(018)  VALUE
                    '*** SGTS0086 ***'.
       77  CTE-VERS                      PIC  X(006)  VALUE 'VRS001'.

      * Módulos acionados
       77  IIBP2000                      PIC  X(008) VALUE 'IIBP2000'.
       77  SGTST005                      PIC  X(008) VALUE 'SGTST005'.
       77  SGTS0081                      PIC  X(008) VALUE 'SGTS0081'.

      *----------------------------------------------------------------*
       LOCAL-STORAGE                   SECTION.
      *----------------------------------------------------------------*

      * Areas de trabalho

       01  VL-EMS-TIT-OPR               PIC S9(15)V9(2) COMP-3 VALUE 0.
       01  CD-ERRO-CLC                  PIC  9(01)             VALUE 0.
           88 COM-ERRO-CLC              VALUE 1.

       01  AX-VL-15V2                   PIC ----.---.---.---.--9,99.
       01  AX-VL-9V8                    PIC ----.---.--9,99999999.

      *--> Auxiliares Movimentacao Monitoração
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).

      *    Chamada à subrotina SBDATAF

       01  WKSBDATA        PIC X(4095).
       01  AREA-RED REDEFINES WKSBDATA.
           03  AREA-TAM    PIC 9(0008) COMP.
           03  FILLER      PIC X(4071).
           03  AREA-FIM    PIC X(0020).

       01  SBDATAF                     PIC  X(08) VALUE 'SBDATAF'.

       01  FUNCAO                     PIC  X(3) VALUE 'F01'.
       01  ARG01                      PIC  9(8) VALUE 0.
       01  ARG01-R REDEFINES ARG01.
           03  DIA                    PIC  9(2).
           03  MES                    PIC  9(2).
           03  ANO                    PIC  9(4).
       01  ARG02                      PIC  9(1) VALUE 0.
      *----------------------------------------------------------------*
      * Book dos Containers e das subrotinas chamadas
      *----------------------------------------------------------------*

      * Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1
      * Book para captura de abend cics da bloco codigo HLPKP999
-INC HLPKW999

      * Book para captura de erro via MSG1
-INC MSGKWMG1

      * Book do container de requisição e resposta deste modulo
      * Alterar Mandato da Operação Estruturada
-INC SGTK0086

      * Consultar TABELA OPR_ETRD_MCP
       01  SGTST005-DADOS.
-INC SGTKT005
      * Verifica se Operação Pode ser alterada
-INC SGTK0081
      * Book do Container de Erro
-INC IIBKERRO
      * Book do container de Serviços (para acionamento de serviços)
-INC IIBKSRVC
      * Book dos dados do usuário
-INC IIBKOPRD

       01  CTE-FINAL-SS                  PIC X(40)      VALUE
                    '*** S.S. TERMINA AQUI ***'.

      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       000000-ROTINA-PRINCIPAL         SECTION.
      *----------------------------------------------------------------*

           PERFORM 100000-INICIAR

           PERFORM 200000-PROCESSAR

           PERFORM 999999-ENCERRAR

           GOBACK
           .
       000000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       100000-INICIAR                  SECTION.
      *----------------------------------------------------------------*

      *    Ativa a captura automatica de abend do CICS.
      *    WHENEVER CICS
-INC HLPKE999

           PERFORM 970000-LER-CANAL

      *    Define o nome do programa p/ fins manipulacao container
           MOVE 'SGTS0086'             TO NM-PGM

      *    Limpa a resposta e erro antes de executar o modulo
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO

           PERFORM 971000-LER-REQUISICAO

      *    Recupera Usuário Logado através do Container do Operador
           PERFORM 974000-LER-DADOS-OPRD

           PERFORM 110000-VALIDAR-REQUISICAO
           .
       100000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       110000-VALIDAR-REQUISICAO       SECTION.
      *----------------------------------------------------------------*
           MOVE 0001                   TO MSG1-CD-SEQL-ERRO

      *    Valida campo a campo da requisição

           IF  SGTS0086-CD-CTRA-TIT-VL < ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0001               TO MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de título e valores mobiliários
      -             'inválido.'        TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0086-CD-CTRA-TIT-VL EQUAL ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0002               TO MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de título e valores mobiliários
      -             'não informado.'   TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0086-CD-OPR-ETRD NOT NUMERIC
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0003               TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -             'numérico.'        TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0086-NR-VRS-OPR-ETRD < ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0006               TO MSG1-CD-ERRO-TS
               MOVE 'Número da versão da operação estruturada inválido.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0086-VL-MNDT-OPR     NOT NUMERIC
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0136               TO MSG1-CD-ERRO-TS
               MOVE 'Valor do mandato da operação não numérico.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0086-DT-MNDT-OPR     NOT EQUAL SPACES
                MOVE CORR SGTS0086-DT-MNDT-OPR-R     TO  ARG01-R
      *    Valida no formato Gregoriano
                MOVE 4                               TO  ARG02
                PERFORM 910000-VALIDAR-DATAS
           END-IF

           IF  SGTS0086-VL-GRT-BB-OPR     NOT NUMERIC
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0137               TO MSG1-CD-ERRO-TS
               MOVE 'Valor da garantia firme BB da operação não numérico
      -             '.'                TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0086-DT-MNDT-OPR NOT EQUAL SPACES AND
               SGTS0086-VL-MNDT-OPR EQUAL ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0138               TO MSG1-CD-ERRO-TS
               MOVE 'Ao informar a data do mandato, também deverá ser in
      -             'formado o valor do mesmo.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

            IF SGTS0086-DT-MNDT-OPR  EQUAL SPACES AND
               SGTS0086-VL-MNDT-OPR NOT EQUAL ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0139               TO MSG1-CD-ERRO-TS
               MOVE 'Ao informar o valor do mandato, também deverá ser i
      -             'nformada a data do mesmo.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF
           .
       110000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       200000-PROCESSAR                SECTION.
      *----------------------------------------------------------------*

           PERFORM 210000-VERIFICAR-ALT-OPR
           PERFORM 220000-CONSULTAR-OPERACOES
           PERFORM 221000-CALCULA-VL-EMS-TIT-OPR
           PERFORM 222000-VALIDAR-DADOS-OPR
           PERFORM 230000-ALTERAR-OPR-ETRD-MCP
           .
       200000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       210000-VERIFICAR-ALT-OPR          SECTION.
      *----------------------------------------------------------------*
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTS0081-DADOS

           MOVE SGTS0081               TO NM-PGM

      *    Movimentar os campos de entrada
           MOVE SGTS0086-CD-CTRA-TIT-VL   TO SGTS0081-CD-CTRA-TIT-VL
           MOVE SGTS0086-CD-OPR-ETRD      TO SGTS0081-CD-OPR-ETRD
           MOVE SGTS0086-NR-VRS-OPR-ETRD  TO SGTS0081-NR-VRS-OPR-ETRD

           PERFORM 900000-EXECUTAR-MODULO

           MOVE LENGTH OF SGTS0081-RPST TO QT-CCT-EPRO
           PERFORM 973000-LER-RESPOSTA

           IF SGTS0081-IN-HBLT-MENU-MNDT EQUAL 'N'
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0098               TO MSG1-CD-ERRO-TS
               MOVE 'Alteração não permitida para o estado da operação.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF
           .
       210000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       220000-CONSULTAR-OPERACOES         SECTION.
      *----------------------------------------------------------------*
           MOVE 0003                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE         SGTST005-DADOS

      *    Movimentar os campos de entrada

           SET SGTST005-CONSULTAR        TO TRUE
           MOVE SGTS0086-CD-CTRA-TIT-VL  TO SGTST005-CD-CTRA-TIT-VL
           MOVE SGTS0086-CD-OPR-ETRD     TO SGTST005-CD-OPR-ETRD-MCP
           MOVE SGTS0086-NR-VRS-OPR-ETRD TO SGTST005-NR-VRS-OPR-ETRD

           PERFORM 920000-CHAMA-SGTST005
           .
       220000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       221000-CALCULA-VL-EMS-TIT-OPR    SECTION.
      * Calcula o valor de emissão do título da operação
      *----------------------------------------------------------------*
           MOVE 0004                   TO MSG1-CD-SEQL-ERRO

           MULTIPLY SGTST005-VL-PU-EMS-TIT-OPR BY
                                              SGTST005-QT-PAPL-TIT-EMS
                                     GIVING VL-EMS-TIT-OPR
                 ON SIZE ERROR
                 SET ERRO-PROGRAMADO   TO TRUE
                 SET COM-ERRO-CLC      TO TRUE
                 MOVE 0062             TO MSG1-CD-ERRO-TS
                 MOVE 'Erro no cálculo do valor de emissão do título.'
                                       TO MSG1-TX-ERRO-TS
                 PERFORM 999000-ERRO
           END-MULTIPLY
           .
       221000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       222000-VALIDAR-DADOS-OPR        SECTION.
      *----------------------------------------------------------------*
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

           IF SGTS0086-VL-GRT-BB-OPR > ZERO
                          AND (SGTS0086-VL-MNDT-OPR = ZEROS
                          OR  SGTS0086-DT-MNDT-OPR = SPACES)
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0140               TO MSG1-CD-ERRO-TS
               MOVE 'Ao informar valor da garantia firme BB também devem
      -             'ser informados data e valor do mandato.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF SGTS0086-VL-GRT-BB-OPR > SGTS0086-VL-MNDT-OPR
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0145               TO MSG1-CD-ERRO-TS
               MOVE 'Valor da garantia firme BB deve ser menor do que o
      -             'valor do mandato da operação.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF SGTS0086-VL-GRT-BB-OPR > VL-EMS-TIT-OPR
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0146               TO MSG1-CD-ERRO-TS
               MOVE 'Valor da garantia firme BB deve ser menor ou igual
      -             'ao valor da emissão do título da operação.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF SGTS0086-VL-GRT-BB-OPR > ZEROS
              AND SGTS0086-VL-GRT-BB-OPR < SGTST005-VL-PRV-ENCT
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0147               TO MSG1-CD-ERRO-TS
               MOVE 'Valor da garantia firme BB deve ser maior ou igual
      -             'ao valor de previsão de encarteiramento.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF SGTS0086-VL-GRT-BB-OPR > ZEROS
              AND SGTS0086-VL-GRT-BB-OPR < SGTST005-VL-CPR-OPR
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0148               TO MSG1-CD-ERRO-TS
               MOVE 'Valor da garantia firme BB deve ser maior ou igual
      -             'ao valor da compra do título da operação.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF
           .
       222000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       230000-ALTERAR-OPR-ETRD-MCP         SECTION.
      *----------------------------------------------------------------*
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

      *    Movimentar os campos de entrada

           SET SGTST005-ALTERAR          TO TRUE
           MOVE SGTS0086-CD-CTRA-TIT-VL  TO SGTST005-CD-CTRA-TIT-VL
           MOVE SGTS0086-CD-OPR-ETRD     TO SGTST005-CD-OPR-ETRD-MCP
           MOVE SGTS0086-NR-VRS-OPR-ETRD TO SGTST005-NR-VRS-OPR-ETRD
           MOVE SGTS0086-VL-MNDT-OPR     TO SGTST005-VL-MNDT-OPR

           MOVE SPACES                   TO SGTST005-DT-MNDT-OPR
           IF  SGTS0086-DT-MNDT-OPR    NOT EQUAL SPACES
               MOVE '  .  .    '         TO SGTST005-DT-MNDT-OPR
               MOVE CORR SGTS0086-DT-MNDT-OPR-R
                                         TO SGTST005-DT-MNDT-OPR-R
           END-IF

           MOVE SGTS0086-VL-GRT-BB-OPR   TO SGTST005-VL-GRT-BB-OPR
           MOVE 'CURRENT'                TO SGTST005-TS-ULT-ATL
           MOVE  PRM-INF-OPRD            TO SGTST005-CD-USU-ULT-ATL

           PERFORM 920000-CHAMA-SGTST005
           .
       230000-SAI.
           EXIT.

      *----------------------------------------------------------------*
      *                                                                *
      *  SECTION DA FAIXA DE 900000 A 969900 PARA DEMAIS ROTINAS       *
      *                                                                *
      *----------------------------------------------------------------*

       900000-EXECUTAR-MODULO          SECTION.
      *----------------------------------------------------------------*

      *    Grava o container de requisicao do modulo a executar
           PERFORM 982000-GRAVAR-CONTAINER-RQSC

      *    Limpa a resposta e erro antes de executar o modulo
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO

           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EVALUATE NM-PGM
               WHEN SGTS0081
                   EXEC CICS LINK
                             PROGRAM ( SGTS0081  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC
               WHEN OTHER
                    SET   ERRO-PROGRAMADO
                                        TO  TRUE
                    MOVE  9006          TO  MSG1-CD-ERRO-TS
                    STRING  'Não definida a execução do módulo'
                            NM-PGM '.'
                            DELIMITED BY SIZE
                            INTO            MSG1-TX-ERRO-TS
                    PERFORM 999000-ERRO
           END-EVALUATE
      *
           EVALUATE TRUE
               WHEN MSG1-RESP1        EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    SET   ERRO-CICS    TO  TRUE
                    MOVE  3004         TO  MSG1-CD-ERRO-TS
                    STRING
                    'Erro  ao acionar o módulo  ' NM-PGM  ', '
                    'RESP  = '  MSG1-RESP1  ', '
                    'RESP2 = '  MSG1-RESP2
                    DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
                    PERFORM 999000-ERRO
           END-EVALUATE

      *    Efetua a leitura do Container de Erro
           PERFORM 972000-LER-CONTAINER-DE-ERRO
           .
       900000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       910000-VALIDAR-DATAS                 SECTION.
      *----------------------------------------------------------------*
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO

           MOVE 4095 TO AREA-TAM.
           MOVE 'SBDATAF -WRKAREA-FIM' TO AREA-FIM.

           CALL SBDATAF     USING FUNCAO WKSBDATA  ARG01 ARG02.

      *--> Return code = 24: Data Inválida.

           IF  RETURN-CODE             NOT EQUAL ZEROS AND 24 AND 22
                                                       AND 46
               SET ERRO-PROGRAMA-ACIONADO TO TRUE
               MOVE 0067               TO MSG1-CD-ERRO-TS
               MOVE RETURN-CODE        TO MSG1-CD-RTN-PGM-ACND
               MOVE 'Erro na rotina que valida data.'
                                       TO MSG1-TX-RTN-PGM-ACND
                                          MSG1-TX-ERRO-TS
               MOVE SBDATAF            TO MSG1-NM-PGM-ACND
                                          NM-PGM
      *        Monitorar variáveis do módulo chamado
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-ERRO

           END-IF

           IF  RETURN-CODE               EQUAL 24
               SET ERRO-PROGRAMA-ACIONADO     TO TRUE
               MOVE 0049               TO MSG1-CD-ERRO-TS
               MOVE 'Data informada inválida.'
                                       TO MSG1-TX-RTN-PGM-ACND
                                          MSG1-TX-ERRO-TS
               MOVE RETURN-CODE        TO MSG1-CD-RTN-PGM-ACND
               MOVE SBDATAF            TO MSG1-NM-PGM-ACND
                                          NM-PGM
      *        Monitorar variáveis do módulo chamado
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-ERRO
           END-IF
           .
       910000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       920000-CHAMA-SGTST005           SECTION.
      *-----------------------------------------------------------------
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO

      *    Movimento nome do módulo para monitoração de variáveis
           MOVE SGTST005               TO NM-PGM

           EXEC CICS LINK
                PROGRAM  (SGTST005)
                COMMAREA (SGTST005-DADOS)
           END-EXEC

           IF  SGTST005-CD-RTN-PGM     NOT EQUAL ZEROS
               SET   ERRO-PROGRAMA-ACIONADO
                                       TO TRUE
               MOVE  NM-PGM            TO  MSG1-NM-PGM-ACND
               MOVE  SGTST005-MSG-ERRO
                                       TO  MSG1-TX-RTN-PGM-ACND
                                           MSG1-TX-ERRO-TS
               MOVE  SGTST005-CD-RTN-PGM
                                       TO  MSG1-CD-RTN-PGM-ACND
                                           MSG1-CD-ERRO-TS
      *        Monitorar variáveis do módulo chamado
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-ERRO
           END-IF
           .
       920000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       969000-MONITORAR-VARIAVEIS      SECTION.
      *----------------------------------------------------------------*
      * Monitora as variáveis dos módulos chamados
      *----------------------------------------------------------------*

           EVALUATE NM-PGM

                WHEN SGTST005
                    MOVE 'SGTST005-ACAO'          TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST005-ACAO            TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST005-CD-CTRA-TIT-VL'
                                                  TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST005-CD-CTRA-TIT-VL  TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST005-CD-OPR-ETRD-MCP'
                                                  TO MSGKW999-NM-VRV-MNT
                    IF   SGTST005-CD-OPR-ETRD-MCP IS NUMERIC
                       MOVE SGTST005-CD-OPR-ETRD-MCP TO CD-OPR-ETRD-MCP
                       MOVE CD-OPR-ETRD-MCP-TX    TO MSGKW999-VL-VRV-MNT
                    ELSE
                       MOVE 'Código de operação estruturada de mercado d
      -                     'e capitais não numérico.'
                                                  TO MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST005-NR-VRS-OPR-ETRD'
                                                  TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST005-NR-VRS-OPR-ETRD TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                WHEN SGTS0081
                    MOVE 'SGTS0081-CD-CTRA-TIT-VL'
                                                  TO MSGKW999-NM-VRV-MNT
                    MOVE SGTS0081-CD-CTRA-TIT-VL  TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTS0081-CD-OPR-ETRD'   TO MSGKW999-NM-VRV-MNT
                    IF SGTS0081-CD-OPR-ETRD       IS NUMERIC
                       MOVE SGTS0081-CD-OPR-ETRD  TO CD-OPR-ETRD-MCP
                       MOVE CD-OPR-ETRD-MCP-TX    TO MSGKW999-VL-VRV-MNT
                    ELSE
                       MOVE 'Código de operação estruturada de mercado d
      -                     'e capitais não numérico.'
                                                  TO MSGKW999-VL-VRV-MNT
                    END-IF

                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTS0081-NR-VRS-OPR-ETRD'
                                                  TO MSGKW999-NM-VRV-MNT
                    MOVE SGTS0081-NR-VRS-OPR-ETRD TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SBDATAF
                    MOVE 'FUNCAO'                 TO MSGKW999-NM-VRV-MNT
                    MOVE FUNCAO                   TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'ARG01'                  TO MSGKW999-NM-VRV-MNT
                    MOVE ARG01                    TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'ARG02'                  TO MSGKW999-NM-VRV-MNT
                    MOVE ARG02                    TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN OTHER
                    CONTINUE
           END-EVALUATE
           .
       969000-SAI.
           EXIT.

      *----------------------------------------------------------------*
      *                                                                *
      * SECTION DA FAIXA DE 970000 A 979900 PARA LEITURA DE DADOS      *
      *                                                                *
      *----------------------------------------------------------------*
       970000-LER-CANAL                SECTION.
      *    Obtem o canal corrente
      *----------------------------------------------------------------*
           MOVE 0011                   TO MSG1-CD-SEQL-ERRO

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

      *----------------------------------------------------------------*
       971000-LER-REQUISICAO           SECTION.
      *----------------------------------------------------------------*
           MOVE 0012                   TO MSG1-CD-SEQL-ERRO

      *    Monta o nome do container de requisicao
           MOVE 'SGTS0086'             TO NM-PGM
           MOVE 'SGTS0086-RQSC'        TO NM-CTNR

      *    Recupera o tamanho do container de requisição
           PERFORM 973100-LER-CONTAINER-NODATA

      *    Obtem o tamanho esperado da requisicao
           MOVE LENGTH OF SGTS0086-RQSC TO QT-CCT-EPRO

      *    Valida tamanho esperado da requisicao X tamanho recebido

           IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
               SET  ERRO-TAMANHO-AREA      TO  TRUE
               PERFORM 999000-ERRO
           END-IF

      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Efetua a leitura do container
           EXEC CICS GET
                     CONTAINER (NM-CTNR)
                     FLENGTH   (QT-CCT-LIDO)
                     INTO      (SGTS0086-RQSC)
           END-EXEC

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       971000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       972000-LER-CONTAINER-DE-ERRO    SECTION.
      *----------------------------------------------------------------*
      *    A presença do container PRM-ERRO indica que houve erro de
      *    integração, a ausência do container indica sucesso.
      *----------------------------------------------------------------*
           MOVE 0013                   TO MSG1-CD-SEQL-ERRO

      *    Le o container de erro dos modulos chamados
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
      *        Para o Modulo Chamado e tratar na section chamadora
               WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
                    CONTINUE
               WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
                    SET ERRO-PROGRAMA-ACIONADO TO TRUE
                    MOVE  NM-PGM       TO  MSG1-NM-PGM-ACND
                    MOVE  IIBKERRO-TX-ERRO
                                       TO  MSG1-TX-RTN-PGM-ACND
                                           MSG1-TX-ERRO-TS
                    MOVE  IIBKERRO-CD-ERRO
                                       TO  MSG1-CD-RTN-PGM-ACND
                                           MSG1-CD-ERRO-TS
      *       Monitorar variáveis do módulo chamado
                   PERFORM 969000-MONITORAR-VARIAVEIS
                   PERFORM 999000-ERRO
               WHEN OTHER
                    SET ERRO-CONTAINER  TO TRUE
                    PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       972000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       973000-LER-RESPOSTA             SECTION.
      *----------------------------------------------------------------*
      *    Recupera o container com os dados de resposta (saída).

      *                        NOME DO CONTAINER RESPOSTA
      *                        --------------------------

      *    Modulo CONTAINER  -     NOME_PGM-RPST
      *    Modulo IIB (FIXO) -     PRM-DADOS-RPST

      *---------------------------------------------------------------*
           MOVE 0014                   TO MSG1-CD-SEQL-ERRO

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

      *    Le a RESPOSTA de acordo com o programa que a gravou

           EVALUATE NM-PGM

      *        Calcular a quantidade de caracteres esperado e limpar
      *        a area de dados antes de executar o GET
      *        Para matrizes com dados variaveis (clausula depending
      *        on) nao ha como utilizar o comando INITIALIZE

               WHEN SGTS0081
                    INITIALIZE SGTS0081-RPST
                    MOVE LENGTH OF SGTS0081-RPST TO QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTS0081-RPST    )
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

      * Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       973000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       973100-LER-CONTAINER-NODATA     SECTION.
      *----------------------------------------------------------------*
      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Obtem a quantidade de caracteres no container
           MOVE ZEROS                  TO QT-CCT-LIDO
           EXEC CICS GET
                     CONTAINER (NM-CTNR     )
                     NODATA
                     FLENGTH   (QT-CCT-LIDO )
                     RESP      ( MSG1-RESP1 )
                     RESP2     ( MSG1-RESP2 )
           END-EXEC

      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       973100-SAI.
           EXIT.

      *----------------------------------------------------------------*
       974000-LER-DADOS-OPRD           SECTION.
      *----------------------------------------------------------------*
           MOVE 0015                   TO MSG1-CD-SEQL-ERRO

      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE
           MOVE 'PRM-INF-OPRD'         TO NM-CTNR

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
      *                                                                *
      * SECTION DA FAIXA DE 980000 A 989900 PARA GRAVACAO DE DADOS     *
      *                                                                *
      *----------------------------------------------------------------*
       982000-GRAVAR-CONTAINER-RQSC    SECTION.
      *----------------------------------------------------------------*
      *    Postar container com dados de requisição (entrada)
      *
      *                        NOME DO CONTAINER REQUISICAO
      *                        ----------------------------
      *
      *    Modulo CONTAINER  -     NOME_PGM-RQSC
      *    Modulo IIB (FIXO) -     PRM-DADOS-RQSC
      *
      *----------------------------------------------------------------*
           MOVE 0017                   TO MSG1-CD-SEQL-ERRO

      *    Monta o nome dos container de acordo com modulo a executar
           MOVE SPACES                 TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE

      *    Quando módulos via IIB, fixo nome PRM-DADOS-RQSC, senão
      *    nome_modulo -RQSC

           MOVE NM-PGM                 TO NM-CTNR(1:8)
           MOVE '-RQSC'                TO NM-CTNR(9:5)

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Faz put da requisicao do programa a executar
           EVALUATE NM-PGM
               WHEN SGTS0081
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (SGTS0081-RQSC )
                              FLENGTH   (LENGTH OF SGTS0081-RQSC )
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

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       982000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       983000-EXCLUIR-RESPOSTA-ERRO    SECTION.
      *----------------------------------------------------------------*
      * Exclui o container de resposta e de erro para evitar problemas *
      * em modulos executados mais de uma vez dentro da mesma sessao do*
      * CICS. Equivale ao initialize na area de resposta dos programas *
      * que utilizam COMMAREA.                                         *
      *----------------------------------------------------------------*
           MOVE 0018                   TO MSG1-CD-SEQL-ERRO

      *    Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *    nome_modulo -RPST

           MOVE NM-PGM        TO NM-CTNR(1:8)
           MOVE '-RPST'       TO NM-CTNR(9:5)

           SET DEL-CONTAINER           TO TRUE

      *    Exclui o container de resposta
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

      *    Excluir o container de erro
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO
           .
       983000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       983100-EXCLUIR-CONTAINER-ERRO   SECTION.
      *----------------------------------------------------------------*
      *    Limpar variaveis container de erro
           INITIALIZE IIBKERRO-PRM-ERRO

      *    Exclui o container de erro
           MOVE 'PRM-ERRO'             TO NM-CTNR
           SET DEL-CONTAINER           TO TRUE

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
           .
       983100-SAI.
           EXIT.

      *----------------------------------------------------------------*
       984000-GRAVAR-CONTAINER-ERRO    SECTION.
      *----------------------------------------------------------------*

      *    Grava o erro gerado no container de erro
      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS PUT
                     CONTAINER ( 'PRM-ERRO'                  )
                     FROM      ( IIBKERRO-PRM-ERRO           )
                     FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                     RESP      (MSG1-RESP1                   )
                     RESP2     (MSG1-RESP2                   )
           END-EXEC
           .
       984000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
      *                                                                *
      * As sections abaixo iniciadas com 999000 representam os tratamen*
      * tos de erro utilizando o Sistema MSG (rotina MSGS0001)         *
      *                                                                *
      *----------------------------------------------------------------*
       999000-ERRO                     SECTION.
      *----------------------------------------------------------------*

           IF  MSG1-CD-ERRO-TS EQUAL ZEROS
               MOVE 8888                TO MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS EQUAL SPACES
               MOVE 'Erro não especificado no módulo.'
                                        TO MSG1-TX-ERRO-TS
           END-IF

           PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           .
       999000-ERRO-SAI.
           EXIT.

      *----------------------------------------------------------------*
      *     As sections abaixo representam os tratamentos de erro
      *    utilizando o sistema MSG de mensageria (rotina MSGS0001)
      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------

      *    Em caso de erro, desfaz as atualizações em banco de dados

           EXEC CICS SYNCPOINT
                     ROLLBACK
           END-EXEC

      *Adiciona as variáveis de entrada para monitoração

           MOVE  'SGTS0086-CD-CTRA-TIT-VL' TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0086-CD-CTRA-TIT-VL   TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0086-CD-OPR-ETRD'      TO MSGKW999-NM-VRV-MNT
           IF  SGTS0086-CD-OPR-ETRD         IS NUMERIC
               MOVE  SGTS0086-CD-OPR-ETRD   TO CD-OPR-ETRD-MCP
               MOVE  CD-OPR-ETRD-MCP-TX     TO MSGKW999-VL-VRV-MNT
           ELSE
               MOVE 'Código de operação estruturada de mercado de ca
      -             'pitais não numérico.'  TO MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0086-NR-VRS-OPR-ETRD'  TO MSGKW999-NM-VRV-MNT
           MOVE  SGTS0086-NR-VRS-OPR-ETRD   TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0086-VL-MNDT-OPR'      TO MSGKW999-NM-VRV-MNT
           IF  SGTS0086-VL-MNDT-OPR       IS NUMERIC
               MOVE  SGTS0086-VL-MNDT-OPR   TO AX-VL-15V2
               MOVE  AX-VL-15V2             TO MSGKW999-VL-VRV-MNT
           ELSE
               MOVE 'Valor do mandato da operação não numérico.'
                                            TO MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0086-DT-MNDT-OPR'      TO MSGKW999-NM-VRV-MNT
           MOVE  SGTS0086-DT-MNDT-OPR       TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0086-VL-GRT-BB-OPR'    TO MSGKW999-NM-VRV-MNT
           IF  SGTS0086-VL-GRT-BB-OPR       IS NUMERIC
               MOVE  SGTS0086-VL-GRT-BB-OPR TO AX-VL-15V2
               MOVE  AX-VL-15V2             TO MSGKW999-VL-VRV-MNT
           ELSE
               MOVE 'Valor da garantia Firme BB não numérico.'
                                            TO MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM 999003-ADICIONA-VRV-MNT

           IF  COM-ERRO-CLC
               PERFORM 999100-ADICIONA-OCR-CLC
           END-IF

      *    Grava o erro gerado no sistema MSG - Mensageria
           PERFORM MSG1-NTF-MSG

           MOVE MSG1-CD-ERRO-TS        TO IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS        TO IIBKERRO-TX-ERRO

      *    Grava o erro gerado no container de erro
           PERFORM 984000-GRAVAR-CONTAINER-ERRO

      *    Encerra a transacao
           PERFORM 999999-ENCERRAR
           .
       999000-SAI.
           EXIT.

      * --------------------------------------*
      * 999001-TRATA-ABEND-CICS        SECTION.
      * 999002-TRATA-CONDITION-CICS    SECTION.
      * 999003-ADICIONA-VRV-MNT        SECTION.
      * 999004-ALINHA-NUMERO-ESQUERDA  SECTION.
      * --------------------------------------*
-INC HLPKP999
      * --------------------------------------*
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *---------------------------------------*
-INC SGTKP999
      * --------------------------------------*
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
      * --------------------------------------*
-INC MSGKPMG1

      *-----------------------------------------------------------------
       999100-ADICIONA-OCR-CLC        SECTION.
      *--> Adiciona Ocorrencia do cálculo na monitoração
      *-----------------------------------------------------------------

           MOVE  'SGTST005-VL-PU-EMS-TIT-OPR'    TO  MSGKW999-NM-VRV-MNT
           IF    SGTST005-VL-PU-EMS-TIT-OPR      IS  NUMERIC
               MOVE  SGTST005-VL-PU-EMS-TIT-OPR  TO  AX-VL-9V8
               MOVE  AX-VL-9V8                   TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE 'Valor do precço unitário de emissão do título não n
      -             'umérico.'                   TO  MSGKW999-VL-VRV-MNT
           END-IF

           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-QT-PAPL-TIT-EMS'       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-QT-PAPL-TIT-EMS        TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'VL-EMS-TIT-OPR'                 TO  MSGKW999-NM-VRV-MNT
           IF  VL-EMS-TIT-OPR                    IS  NUMERIC
               MOVE  VL-EMS-TIT-OPR              TO  AX-VL-15V2
               MOVE  AX-VL-15V2                  TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE 'Valor de emissão do título não numérico.'
                                                 TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM 999003-ADICIONA-VRV-MNT
           .
       999100-SAI.
           EXIT.

      *----------------------------------------------------------------*
       999999-ENCERRAR                 SECTION.
      *----------------------------------------------------------------*

           EXEC CICS
                RETURN
           END-EXEC
           .
       999999-SAI.
           EXIT.
      *---------------------- FIM SGTS0086 -----------------------------