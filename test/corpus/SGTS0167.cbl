       CBL ARITH(EXTEND)
      *-----------------------------------------------------------------
      * PROGRAMA..: SGTS0167
      * ANALISTA..: EDILENE AKEMI HORITA
      * AUTOR.....: EDILENE AKEMI HORITA
      * DATA......: 20/03/2018
      * OBJETIVO..: Internaliza a movimentação financeira dos títulos
      *             a partir do arquivo reformatado da Drive.
      * COMPILACAO: 52 - PSOSE600 - Cobol 4.2 c/otimizacao p/producao
      *-----------------------------------------------------------------
      * VRS005 16/09/2024 - F2054109 - Inclusao
      *                            SGTST086-CD-TIP-OGM-INF = 1
      * VRS004 24/06/2022 - F9158937 - Emitir campo AUX-SITUACAO e
      *                     Título na mensagem de erro 707.
      * VRS003 09/12/2019 - F9158937 - Só gravar vinculo na tabela
      *                     RCBT_RLZD_OPR quando CD-TIP-LCTO-CTB
      *                     corresponda a "D". Ou seja na tabela
      *                     IMP_MVTC_FNCR_TIT, CD-TIP-LCTO-CTB = 1 e
      *                     na DMTV_CX_TIT, CD-TIP-LCTO-CTB = 2.
      *        04/12/2019 - F0950413 -DE 2016/145195 Monitorar variável
      *                     NM-EMPR da mensagem 0891.
      * VRS002 31/01/2019 - F2634012 -DE 2016/145195 Inclusão do crono-
      *                     grama de juros para vínculo de pagamento.
      * VRS001 05/03/2018 - F2634012 - DE 2016/145195 - IMPLANTACAO
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------

       PROGRAM-ID. SGTS0167.

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

       77  CTE-INICIO-SS    PIC  X(40) VALUE
           '*** STORAGE SECTION COMECA AQUI ***'.
       77  CTE-PROG     PIC  X(17) VALUE '*** SGTS0167 ***'.
       77  CTE-VERS     PIC  X(6)  VALUE 'VRS005'.

      *
      * Subrotinas Utilizadas

       77  SBABEND  VALUE 'SBABEND'    PIC  X(008).
       77  SBVERSAO VALUE 'SBVERSAO'   PIC  X(008).
       77  SBDATAF  VALUE 'SBDATAF'    PIC  X(008).
      *
       77  SGTST065 VALUE 'SGTST065'   PIC  X(008).
       77  SGTST086 VALUE 'SGTST086'   PIC  X(008).
       77  SGTST087 VALUE 'SGTST087'   PIC  X(008).
       77  SGTSL600 VALUE 'SGTSL600'   PIC  X(008).
       77  SGTSL601 VALUE 'SGTSL601'   PIC  X(008).
       77  SGTSLJ11 VALUE 'SGTSLJ11'   PIC  X(008).

      *----------------------------------------------------------------*
      *           Book do programa de ambiente - CICS5000              *
      *       Definido na work, para preservar as informacoes          *
      *----------------------------------------------------------------*
-INC HLPKE01B


      *----------------------------------------------------------------
       LOCAL-STORAGE SECTION.
      *----------------------------------------------------------------

      * Areas de trabalho

       01 CH-ACABOU-REG                PIC  X(003) VALUE 'NAO'.

       01 IN-VINCULO                   PIC X(01)   VALUE SPACES.
              88  VINCULOU                         VALUE 'S'.

       01 IN-ATRASO                    PIC X(01)   VALUE SPACES.
              88  ATRASO                           VALUE 'S'.

       01 NM-EMPR                      PIC X(06)   VALUE 'Drive*'.

       01  DT-HOJE                     PIC  9(008).
       01  DT-HOJE-R     REDEFINES  DT-HOJE.
           03 ANO                      PIC  9(004).
           03 MES                      PIC  9(002).
           03 DIA                      PIC  9(002).

       01  DT-HOJE-D-1DU               PIC  9(008).
       01  DT-HOJE-D-1DU-R REDEFINES   DT-HOJE-D-1DU.
           03 ANO                      PIC  9(004).
           03 MES                      PIC  9(002).
           03 DIA                      PIC  9(002).

       01  DT-FMTD.
           03 DIA                      PIC  9(002)     VALUE ZEROS.
           03 FILLER                   PIC  X(001)     VALUE '.'.
           03 MES                      PIC  9(002)     VALUE ZEROS.
           03 FILLER                   PIC  X(001)     VALUE '.'.
           03 ANO                      PIC  9(004)     VALUE ZEROS.


       01  DT-REF-INI                  PIC  9(008).
       01  DT-REF-INI-R  REDEFINES  DT-REF-INI.
           03 ANO                      PIC  9(004).
           03 MES                      PIC  9(002).
           03 DIA                      PIC  9(002).
       01  DT-REF-FIM                  PIC  9(008).
       01  DT-REF-FIM-R  REDEFINES  DT-REF-FIM.
           03 ANO                      PIC  9(004).
           03 MES                      PIC  9(002).
           03 DIA                      PIC  9(002).

       01  DT-PRMO-CRNG-ABTO           PIC  9(008).
       01  DT-PRMO-CRNG-ABTO-R REDEFINES DT-PRMO-CRNG-ABTO.
           03 ANO                      PIC  9(004).
           03 MES                      PIC  9(002).
           03 DIA                      PIC  9(002).

      * Area da subrotina SBDATA
       01  FUNCAO                      PIC  X(003).
       01  AREA-WORK                   PIC  X(4095).
       01  AREA-WORK-RED  REDEFINES  AREA-WORK.
           03 A-TAM                    PIC  9(0008) COMP.
           03 FILLER                   PIC  X(4071).
           03 A-FIM                    PIC  X(0020).
       01  ARG01                       PIC  9(008) VALUE 0.
       01  ARG01-R REDEFINES ARG01.
           03  DIA                     PIC  9(002).
           03  MES                     PIC  9(002).
           03  ANO                     PIC  9(004).
       01  ARG02                       PIC  9(001) VALUE 0.

       01  ARG02-1                     PIC  9(002) VALUE 1.
       01  ARG02-1-R  REDEFINES ARG02-1.
           03 IND-FERIADO              PIC  9(001).
           03 IND-DIA-SEMANA           PIC  9(001).

       01  ARG02-2                     PIC  9(005) VALUE 1.
       01  ARG03                       PIC  9(001) VALUE 2.
       01  ARG04                       PIC  9(008).
       01  ARG04-R  REDEFINES ARG04.
           03 DIA                      PIC  9(002).
           03 MES                      PIC  9(002).
           03 ANO                      PIC  9(004).
      *
       01  IA-1                        PIC  9(003) VALUE 0.
       01  IB-1                        PIC  9(003) VALUE 0.

      * Máscaras de edição, para transição de campos numéricos, com ou
      * sem sinal, compactados ou zonados, com ou sem casas decimais.
      * Estas máscaras, suprimem todos os zeros à esquerda da parte
      * inteira, colocando o sinal "-" (quando o número for negativo)
      * junto ao dígito mais significativo do número.
      * No caso de campos de números inteiros ou com as mesmas quantida-
      * des de casas decimais, será utilizado uma máscara com o maior
      * tamanho da parte inteira, para todos os campos.
       01  MCR-S9-15-V9-14   PIC  ----.---.---.---.--9,99999999999999.
       01  MCR-S9-13-V9-02   PIC    --.---.---.---.--9,99.
       01  MCR-S9-11         PIC       ---.---.---.--9.
       01  MCR-S9-01         PIC                    -9.

      * ?rea para recepção de dados


       01  AUX-VALOR                   PIC S9(013)V9(002).
       01  AUX-VL-ABSOLUTO             PIC S9(013)V9(002).
       01  AUX-TP-LCTO                 PIC S9(001) COMP-3.

       01  AUX-SITUACAO.
           03  AUX-SITUACAO-R          PIC X(1) OCCURS 15 TIMES.

       01  AUX-TEXTO.
           03  AUX-TEXTO-R             PIC X(1) OCCURS 40 TIMES.

       01  WS-RESP.
           03  WS-RESP1                PIC  9(008).
           03  WS-RESP2                PIC  9(008).

       01 AUX-ERRO                     PIC X(30).
       01 AUX-ERRO-R  REDEFINES AUX-ERRO.
          03 AUX-CD-ERRO               PIC S9(04) COMP.

       01 AUX-MSG1                      PIC X(029)
                           VALUE 'Tipo de Motivo não previsto: '.

       01 AUX-MSG2                      PIC X(008)
                           VALUE 'Titulo: '.

       01  SGTST065-DADOS.
-INC SGTKT065

       01  SGTST086-DADOS.
-INC SGTKT086

       01  SGTST087-DADOS.
-INC SGTKT087

       01  SGTSL600-DADOS.
-INC SGTKL600

       01  SGTSL601-DADOS.
-INC SGTKL601

       01  SGTSLJ11-DADOS.
-INC SGTKLJ11

      *--> Book definição dados a serem usados na subrotina de erros
-INC HLPKE001

      * Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1

-INC MSGKWMG1

      *--> HLPKW999
      *    Variaveis Auxiliares para Captura de Erro CICS
      *    e Adicionar Variaveis Monitoradas
       01  MSGKW999-VRV-ERRO-CICS.
           03  MSGKW999-RESP1-X8       PIC  X(08) VALUE SPACES.
           03  MSGKW999-RESP2-X8       PIC  X(08) VALUE SPACES.
       01  MSGKW999-VRV-ALINHA-NUMERO.
           03  MSGKW999-NUMERO-Z17     PIC  ZZZZZZZZZZZZZZZZ9.
           03  MSGKW999-NUMERO-X17     PIC  X(17) VALUE SPACES.
           03  MSGKW999-BRANCOS        PIC  9(08) VALUE ZEROS.
       01  MSGKW999-VRV-AUX.
           03  MSGKW999-NM-VRV-MNT     PIC  X(030) VALUE SPACES.
           03  MSGKW999-VL-VRV-MNT     PIC  X(100) VALUE SPACES.

      *-----------------------------------------------------------------
       LINKAGE SECTION.
      *-----------------------------------------------------------------
       01  DFHCOMMAREA.
-INC SGTK0167

       01  CTE-SAI-SS                  PIC  X(040) VALUE
           '*** Storage Section Termina Aqui ***'.

      *-----------------------------------------------------------------
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL SECTION.
      *-----------------------------------------------------------------

      * teste obrigatorio do tamanho da linkage nao retirar
           IF  EIBCALEN NOT EQUAL LENGTH OF DFHCOMMAREA
               MOVE  0220              TO  CD-RTN-PGM
               MOVE  'Area com tamanho inválido.'
                                       TO  MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           IF  NOT EXECUCAO-ONLINE     AND NOT EXECUCAO-BATCH
               CALL  CICS5000 USING CICS5000-COMMAREA
           END-IF

           MOVE 0001                     TO PSC-ERRO-PGM

      * WHENEVER CICS   **********
-INC HLPKE003

           MOVE 4095                   TO A-TAM
           MOVE 'SBDATAF -WRKAREA-FIM' TO A-FIM
           PERFORM 100000-VALIDA-PARAMETRO

           MOVE CORR SGTS0167-DT-EFT-MVTC-R
                                       TO    ARG01-R
           PERFORM 910007-CALCULA-DATA
           MOVE CORR ARG01-R           TO    DT-REF-INI-R
           MOVE CORR SGTS0167-DT-EFT-MVTC-R
                                       TO    DT-REF-FIM-R
           IF DT-REF-FIM-R             LESS  DT-REF-INI-R
              MOVE  0410               TO    CD-RTN-PGM
              MOVE 'Data início da pesquisa não pode ser maior que data
      -            'fim.'              TO    MSG-ERRO
              PERFORM 999001-ERRO-001
           END-IF
      *         END-IF
      *         DISPLAY 'DT REF INI ' DT-REF-INI
      *         DISPLAY 'DT RF FIM  ' DT-REF-FIM

           PERFORM 300000-VERIF-IMPORT

           PERFORM 920001-BUSCA-TS-DT
           MOVE CORR HLPKE001-TS-FMTD   TO   DT-HOJE-R
      * calcula data de hoje -1 dia útil para verificar se a operação
      * está em atraso
           MOVE CORR DT-HOJE-R          TO   ARG01-R
           PERFORM 910007-CALCULA-DATA
           MOVE CORR ARG01-R            TO   DT-HOJE-D-1DU-R

           PERFORM 200000-INTERNALIZAR

           GOBACK
           .

       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-VALIDA-PARAMETRO               SECTION.
      *-----------------------------------------------------------------
      *
           MOVE 0002                   TO    PSC-ERRO-PGM

           IF SGTS0167-DT-EFT-MVTC   EQUAL   SPACES
              MOVE  0336               TO  CD-RTN-PGM
              MOVE 'Data efetiva da movimentação não informada.'
                                       TO  MSG-ERRO
              PERFORM 999001-ERRO-001
           ELSE
              PERFORM 910001-VALIDA-DATA
              IF RETURN-CODE   EQUAL   24
                 MOVE  0342            TO  CD-RTN-PGM
                 MOVE 'Data efetiva da movimentação inválida.'
                                       TO  MSG-ERRO
                 PERFORM 999001-ERRO-001
              END-IF
           END-IF

           IF SGTS0167-CD-TIT-OPR-ETRD   EQUAL   SPACES
              MOVE  0231               TO  CD-RTN-PGM
              MOVE 'Código do título da operação estruturada não informa
      -            'do.'               TO  MSG-ERRO
              PERFORM 999001-ERRO-001
           END-IF

           IF SGTS0167-QT-EVT   EQUAL   SPACES
              MOVE  0700               TO  CD-RTN-PGM
              MOVE 'Quantidade de papéis não informada.'
                                       TO  MSG-ERRO
              PERFORM 999001-ERRO-001
           END-IF

           IF SGTS0167-CD-TIP-LCTO-CTB   NOT NUMERIC
              MOVE  0365               TO  CD-RTN-PGM
              MOVE 'Código do tipo do lançamento contábil não numérico.'
                                       TO  MSG-ERRO
              PERFORM 999001-ERRO-001
           END-IF

           IF SGTS0167-CD-TIP-LCTO-CTB   NOT GREATER   ZEROS
              MOVE  0701               TO  CD-RTN-PGM
              MOVE 'Código do tipo do lançamento contábil inválido.'
                                       TO  MSG-ERRO
              PERFORM 999001-ERRO-001
           END-IF

           IF SGTS0167-VL-EFT-MVTC-R   EQUAL   ZEROS
              MOVE  0702               TO  CD-RTN-PGM
              MOVE 'Valor efetivo da movimentação não informado.'
                                       TO   MSG-ERRO
              PERFORM 999001-ERRO-001
           END-IF

           IF SGTS0167-TX-DCR-TIP-CPTR   EQUAL   SPACES
              MOVE  0703               TO  CD-RTN-PGM
              MOVE 'Tipo de captura não informado.'
                                       TO  MSG-ERRO
              PERFORM 999001-ERRO-001
           END-IF

           IF SGTS0167-TX-OBS-MVTC   EQUAL   SPACES
              MOVE  0704               TO  CD-RTN-PGM
              MOVE 'Texto de Observação da Movimentação não informado.'
                                       TO  MSG-ERRO
              PERFORM 999001-ERRO-001
           END-IF

           IF SGTS0167-CD-OPR-ETRD-MCP   NOT NUMERIC
              MOVE  0003               TO  CD-RTN-PGM
              MOVE  'Código operação estruturada mercado de capitais não
      -             ' numérico.'       TO  MSG-ERRO
              PERFORM 999001-ERRO-001
           END-IF

           IF SGTS0167-CD-OPR-ETRD-MCP   EQUAL   ZEROS
              MOVE  0005               TO  CD-RTN-PGM
              MOVE  'Código operação estruturada mercado de capitais não
      -             ' informado.'      TO  MSG-ERRO
              PERFORM 999001-ERRO-001
           END-IF

           IF SGTS0167-NR-VRS-OPR-ETRD    EQUAL ZEROS
              MOVE  0007               TO  CD-RTN-PGM
              MOVE 'Número da versão da operação não informado.'
                                       TO  MSG-ERRO
              PERFORM 999001-ERRO-001
           END-IF

           IF SGTS0167-NR-SEQL-RCBT-PRVT   EQUAL ZEROS
              MOVE  0315               TO  CD-RTN-PGM
              MOVE 'Número sequencial do recebimento previsto não inform
      -            'ado.'              TO  MSG-ERRO
              PERFORM 999001-ERRO-001
           END-IF

           IF SGTS0167-DT-IMP-MVTC-FNCR    EQUAL SPACES
              MOVE  0368               TO  CD-RTN-PGM
              MOVE 'Data da importação da movimentação financeira não in
      -            'formada.'          TO  MSG-ERRO
              PERFORM 999001-ERRO-001
           END-IF

           IF SGTS0167-HR-IMP-MVTC-FNCR    EQUAL SPACES
              MOVE  0369               TO  CD-RTN-PGM
              MOVE 'Horário da importação da movimentação financeira não
      -            ' informado.'       TO  MSG-ERRO
              PERFORM 999001-ERRO-001
           END-IF

           IF SGTS0167-NR-IMP-MVTC-FNCR   EQUAL ZEROS
              MOVE  0370               TO  CD-RTN-PGM
              MOVE  'Número da importação da movimentação financeira não
      -             ' informado.'      TO  MSG-ERRO
              PERFORM 999001-ERRO-001
            END-IF

           IF SGTS0167-CD-USU-IMP         EQUAL SPACES
              MOVE  0705               TO  CD-RTN-PGM
              MOVE 'Código do usuário da importação não informado.'
                                       TO  MSG-ERRO
              PERFORM 999001-ERRO-001
           END-IF

           IF SGTS0167-CD-CTRA-TIT-VL     EQUAL  ZEROS
              MOVE  0002                 TO  CD-RTN-PGM
              MOVE 'Código da carteira de título e valores mobiliários n
      -       'ão informado.'            TO  MSG-ERRO
              PERFORM 999001-ERRO-001
           END-IF

              .

       100000-SAI.
           EXIT.


      *-----------------------------------------------------------------
       300000-VERIF-IMPORT                    SECTION.
      *-----------------------------------------------------------------
      *
           MOVE 0002                      TO PSC-ERRO-PGM
           SET  SGTST065-CONSULTAR        TO TRUE
           MOVE SGTS0167-DT-IMP-MVTC-FNCR TO SGTST065-DT-IMP-MVTC-FNCR
           MOVE SGTS0167-HR-IMP-MVTC-FNCR TO SGTST065-HR-IMP-MVTC-FNCR
           MOVE SGTS0167-NR-IMP-MVTC-FNCR TO SGTST065-NR-IMP-MVTC-FNCR
           PERFORM 910008-CHAMA-SGTST065

           .

       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-INTERNALIZAR                    SECTION.
      *-----------------------------------------------------------------
      * Os campos data e valor da recebimento previsto não serão
      * gravados na DTMTV_CX_TIT
      *

           MOVE 0003                   TO    PSC-ERRO-PGM
           MOVE SPACES                 TO    AUX-SITUACAO
           MOVE 1                      TO    IA-1 IB-1
           MOVE SGTS0167-TX-OBS-MVTC   TO    AUX-TEXTO
           PERFORM UNTIL  IA-1           GREATER 40
             IF   AUX-TEXTO-R(IA-1)      NOT EQUAL  ' '
              IF  IB-1                   NOT GREATER 15
                  MOVE AUX-TEXTO-R(IA-1)
                                       TO AUX-SITUACAO-R(IB-1)
                  ADD   1   TO   IB-1   ON SIZE ERROR
                      MOVE  0706       TO  CD-RTN-PGM
                      MOVE 'Erro em operação aritmética.'
                                       TO  MSG-ERRO
                      PERFORM 999001-ERRO-001
                  END-ADD
              ELSE
                IF  AUX-SITUACAO        EQUAL  'AQUISIÇÃO' OR 'RESGATE'
                                        OR     'VENDA'     OR 'PREMIO'
                                        OR     'AJUSTE'    OR 'ACERTO'
                                        OR     'LIQUIDA'   OR
                                               'Aquisição' OR
                                               'Resgate  ' OR 'Venda'
                                        OR     'Premio'    OR 'Ajuste'
                                        OR     'Acerto'    OR 'Liquida'

                    MOVE 90            TO     IA-1
                ELSE
                    MOVE 1             TO     IB-1
                    MOVE SPACES        TO     AUX-SITUACAO
                    SUBTRACT   1   FROM   IA-1   ON SIZE ERROR
                       MOVE  0706      TO  CD-RTN-PGM
                       MOVE 'Erro em operação aritmética.'
                                       TO  MSG-ERRO
                       PERFORM 999001-ERRO-001
                    END-SUBTRACT
                END-IF
              END-IF
            ELSE
               IF  AUX-SITUACAO         EQUAL  'AQUISIÇÃO' OR 'RESGATE'
                                        OR     'VENDA'     OR 'PREMIO'
                                        OR     'AJUSTE'    OR 'ACERTO'
                                        OR     'LIQUIDA'   OR
                                               'Aquisição' OR
                                               'Resgate  ' OR 'Venda'
                                        OR     'Premio'    OR 'Ajuste'
                                        OR     'Acerto'    OR 'Liquida'

                   MOVE 90             TO     IA-1
                ELSE
                    MOVE 1             TO     IB-1
                    MOVE SPACES        TO     AUX-SITUACAO
                END-IF
             END-IF
             ADD   1   TO   IA-1   ON SIZE ERROR
                MOVE  0706             TO  CD-RTN-PGM
                MOVE 'Erro em operação aritmética.'
                                       TO  MSG-ERRO
                PERFORM 999001-ERRO-001
            END-ADD

           END-PERFORM

           INITIALIZE SGTST086-DADOS
           PERFORM 210000-OBTEM-TIPO-MOVTO
           MOVE SGTS0167-VL-EFT-MVTC-R TO AUX-VL-ABSOLUTO
           IF   AUX-VL-ABSOLUTO        LESS ZEROS
                MULTIPLY -1  BY AUX-VL-ABSOLUTO ON SIZE ERROR
                  MOVE  0706             TO  CD-RTN-PGM
                  MOVE 'Erro em operação aritmética.'
                                         TO  MSG-ERRO
                   PERFORM 999001-ERRO-001
                END-MULTIPLY
           END-IF
           MOVE AUX-VL-ABSOLUTO          TO AUX-VALOR

           IF   SGTS0167-CD-TIP-LCTO-CTB EQUAL 1
                MOVE 2                   TO    AUX-TP-LCTO
           ELSE
                MOVE 1                   TO    AUX-TP-LCTO
           END-IF

           PERFORM 220000-GRAVA-DEMONSTRATIVO

           MOVE 'NAO'                  TO  CH-ACABOU-REG
           MOVE HLPKE001-TS-FMTD       TO  SGTSL601-E-TS-REG-PGT0
           MOVE CORR SGTS0167-DT-EFT-MVTC-R
                                       TO  DT-FMTD
           MOVE DT-FMTD                TO  SGTSL601-E-DT-EFT-MVTC
           MOVE 'S'                    TO  SGTSL601-E-IN-MVTC-ATV
           MOVE SGTS0167-CD-TIT-OPR-ETRD
                                       TO  SGTSL601-E-CD-TIT-OPR-ETRD
           MOVE 1                      TO  SGTSL601-E-NR-DVS-TIT
           MOVE SGTS0167-CD-CTRA-TIT-VL
                                       TO SGTSL601-E-CD-CTRA-TIT-VL
           SET  SGTSL601-LST-ATV-TIT   TO  TRUE
           PERFORM 240000-DESATI-DMTV-ANT UNTIL
                   CH-ACABOU-REG          EQUAL 'SIM'

           IF  (SGTS0167-TX-DCR-TIP-CPTR EQUAL 'AUT' OR 'DIG'
                                         OR 'ALT' OR 'IMP' OR 'Aut' OR
                                         'Dig' OR 'Alt' OR 'Imp') AND
               (AUX-SITUACAO         NOT EQUAL 'AQUISIÇÃO' AND 'RESGATE'
                                         AND 'VENDA' AND 'PREMIO' AND
                                         'AJUSTE' AND 'ACERTO' AND
                                         'LIQUIDA' AND 'Aquisição' AND
                                         'Resgate' AND 'Venda' AND
                                         'Premio' AND 'Ajuste' AND
                                         'Acerto' AND 'Liquida' )

      *-----  Só considera Tipo de Lcto Contábil "D" para vinculo
              IF  SGTS0167-CD-TIP-LCTO-CTB = 1
                 MOVE  SPACES          TO        IN-ATRASO
                 PERFORM 260000-VERIF-ATRASO
                 IF    NOT ATRASO
                      PERFORM 270000-TRATA-PAGAMENTO
                 ELSE
                      MOVE  0894             TO  CD-RTN-PGM
                      MOVE 'Operação em atraso'
                                             TO  MSG-ERRO
                                               SGTST065-TX-DCR-ERRO-IMP
                      PERFORM 280000-VERIF-IMP
                 END-IF
              END-IF
           END-IF

           .

       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       210000-OBTEM-TIPO-MOVTO                SECTION.
      *-----------------------------------------------------------------
           MOVE 0004                    TO    PSC-ERRO-PGM

           EVALUATE AUX-SITUACAO

           WHEN    ' '
                   MOVE  1              TO    SGTST086-CD-TIP-MVTC-TIT

           WHEN    'AQUISIÇÃO'
                   MOVE  5              TO    SGTST086-CD-TIP-MVTC-TIT

           WHEN    'RESGATE'
                   MOVE  6              TO    SGTST086-CD-TIP-MVTC-TIT

           WHEN    'VENDA'
                   MOVE  4              TO    SGTST086-CD-TIP-MVTC-TIT

           WHEN    'PREMIO'
                   MOVE  7              TO    SGTST086-CD-TIP-MVTC-TIT

           WHEN    'AJUSTE'
                   MOVE  9              TO    SGTST086-CD-TIP-MVTC-TIT

           WHEN    'ACERTO'
                   MOVE  12             TO    SGTST086-CD-TIP-MVTC-TIT

           WHEN    'LIQUIDA'
                   MOVE  13             TO    SGTST086-CD-TIP-MVTC-TIT

           WHEN    'Aquisição'
                   MOVE  5              TO    SGTST086-CD-TIP-MVTC-TIT

           WHEN    'Resgate'
                   MOVE  6              TO    SGTST086-CD-TIP-MVTC-TIT

           WHEN    'Venda'
                   MOVE  4              TO    SGTST086-CD-TIP-MVTC-TIT

           WHEN    'Premio'
                   MOVE  7              TO    SGTST086-CD-TIP-MVTC-TIT

           WHEN    'Ajuste'
                   MOVE  9              TO    SGTST086-CD-TIP-MVTC-TIT

           WHEN    'Acerto'
                   MOVE  12             TO    SGTST086-CD-TIP-MVTC-TIT

           WHEN    'Liquida'
                   MOVE  13             TO    SGTST086-CD-TIP-MVTC-TIT

           WHEN    OTHER
                   MOVE 0707            TO    CD-RTN-PGM
                   STRING
                      AUX-MSG1       DELIMITED BY SIZE
                      AUX-SITUACAO   DELIMITED BY SPACES
                      AUX-MSG2       DELIMITED BY SIZE
                      SGTS0167-CD-TIT-OPR-ETRD DELIMITED BY SPACES
                   INTO
                      MSG-ERRO
                   END-STRING
                   PERFORM 999001-ERRO-001

           END-EVALUATE
           .

       210000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       220000-GRAVA-DEMONSTRATIVO             SECTION.
      *-----------------------------------------------------------------

           MOVE 0005                   TO   PSC-ERRO-PGM
           MOVE 'SISTEMA'              TO   SGTST086-CD-CHV-USU
           SET  SGTST086-INCLUIR       TO   TRUE
           MOVE SGTS0167-CD-TIT-OPR-ETRD
                                       TO   SGTST086-CD-TIT-OPR-ETRD
           MOVE 1                      TO   SGTST086-NR-DVS-TIT
           PERFORM  920001-BUSCA-TS-DT
           MOVE HLPKE001-TS-FMTD       TO   SGTST086-TS-REG-PGTO
           MOVE CORR SGTS0167-DT-EFT-MVTC-R
                                       TO   DT-FMTD
           MOVE DT-FMTD                TO   SGTST086-DT-EFT-MVTC
           MOVE AUX-TP-LCTO            TO   SGTST086-CD-TIP-LCTO-CTB
           MOVE SPACES                 TO   SGTST086-DT-PRVT-MVTC
           MOVE ZEROS                  TO   SGTST086-VL-PRVT-MVTC
           MOVE AUX-VL-ABSOLUTO        TO   SGTST086-VL-EFT-MVTC
           MOVE 'S'                    TO   SGTST086-IN-MVTC-ATV
           MOVE SGTS0167-TX-OBS-MVTC   TO   SGTST086-TX-DCR-MVTC
           MOVE SGTS0167-CD-USU-IMP    TO   SGTST086-CD-USU-IMP
           MOVE SGTS0167-DT-IMP-MVTC-FNCR
                                       TO   SGTST086-DT-IMP-MVTC-FNCR
           MOVE SGTS0167-HR-IMP-MVTC-FNCR
                                       TO   SGTST086-HR-IMP-MVTC-FNCR
           MOVE SGTS0167-NR-IMP-MVTC-FNCR
                                       TO   SGTST086-NR-IMP-MVTC-FNCR
           MOVE SGTS0167-CD-CTRA-TIT-VL
                                       TO   SGTST086-CD-CTRA-TIT-VL
           MOVE 1                      TO   SGTST086-CD-TIP-OGM-INF

           IF EXECUCAO-ONLINE
              EXEC CICS LINK
                        PROGRAM ( SGTST086       )
                        COMMAREA( SGTST086-DADOS )
                        RESP    ( WS-RESP1)
                        RESP2   ( WS-RESP2)
              END-EXEC
              IF  WS-RESP1 NOT EQUAL  DFHRESP(NORMAL)
                  MOVE  3004           TO  CD-RTN-PGM
                  MOVE  'Erro ao acionar o módulo.'
                                       TO  MSG-ERRO
                  PERFORM 999001-ERRO-001
              END-IF
           ELSE
                  MOVE LENGTH OF SGTST086-DADOS TO EIBCALEN
                  CALL SGTST086 USING DFHEIBLK SGTST086-DADOS
           END-IF

           IF SGTST086-CD-RTN-PGM     NOT EQUAL ZEROS
              MOVE SGTST086-RTN-PGM     TO RTN-PGM
              IF SGTST086-PGM-ERRO      EQUAL 'SGTS014G'
                 MOVE 'SGTST086'        TO PGM-ERRO
              END-IF
              PERFORM 999001-ERRO-001
           END-IF

           .

       220000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       230000-RCB-PRV-CAPITAL              SECTION.
      *-----------------------------------------------------------------

           MOVE 0006                     TO PSC-ERRO-PGM

           INITIALIZE SGTSL600-AREA-DADOS SGTSL600-QT-OCR-TAB

           PERFORM 231000-BUSCA-RCB-PREVISTO
           MOVE 1                        TO  IA-1
           IF   SGTSL600-QT-OCR-TAB      GREATER ZEROS AND
                AUX-VALOR                GREATER ZEROS
                MOVE 2                   TO SGTST087-CD-TIP-RCBT-PRVT
                PERFORM  232000-TRATA-REALIZADO
                         UNTIL  IA-1     GREATER SGTSL600-QT-OCR-TAB
           END-IF

           .

       230000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       231000-BUSCA-RCB-PREVISTO              SECTION.
      *-----------------------------------------------------------------
      * Para o período que está rotina trabalha no máximo pode haver
      * 5 ocorrências para recebimento previsto que é o feriado do
      * carnaval, por isso não há a necessidade de fazer rechamada.

           MOVE 0007                     TO PSC-ERRO-PGM

           IF EXECUCAO-ONLINE
              EXEC CICS LINK
                        PROGRAM ( SGTSL600       )
                        COMMAREA( SGTSL600-DADOS )
                        RESP    ( WS-RESP1)
                        RESP2   ( WS-RESP2)
              END-EXEC
              IF  WS-RESP1 NOT EQUAL  DFHRESP(NORMAL)
                  MOVE  3004           TO  CD-RTN-PGM
                  MOVE  'Erro ao acionar o módulo.'
                                       TO  MSG-ERRO
                  PERFORM 999001-ERRO-001
              END-IF
           ELSE
                  MOVE LENGTH OF SGTSL600-DADOS TO EIBCALEN
                  CALL SGTSL600 USING DFHEIBLK SGTSL600-DADOS
           END-IF

           IF SGTSL600-CD-RTN-PGM NOT EQUAL ZEROS
              MOVE SGTSL600-RTN-PGM TO RTN-PGM
              PERFORM 999001-ERRO-001
           END-IF

           .

       231000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       232000-TRATA-REALIZADO                  SECTION.
      *-----------------------------------------------------------------

           MOVE 0008                     TO PSC-ERRO-PGM

           IF  AUX-VALOR                 NOT LESS
                                         SGTSL600-VL-RCBT-PRVT(IA-1)
               MOVE SGTSL600-VL-RCBT-PRVT(IA-1)
                                         TO SGTST087-VL-RLZD-VCLD
               PERFORM 232100-GRAVA-REALIZADO
                SUBTRACT SGTSL600-VL-RCBT-PRVT(IA-1) FROM AUX-VALOR
                  ON SIZE ERROR
                     MOVE  0706        TO  CD-RTN-PGM
                     MOVE 'Erro em operação aritmética.'
                                       TO  MSG-ERRO
                     PERFORM 999001-ERRO-001
                 END-SUBTRACT
           ELSE
                 MOVE  900               TO   IA-1
           END-IF
           ADD   1                       TO IA-1
                 ON SIZE ERROR
                     MOVE  0706        TO  CD-RTN-PGM
                     MOVE 'Erro em operação aritmética.'
                                       TO  MSG-ERRO
                     PERFORM 999001-ERRO-001
           END-ADD

           .

       232000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       232100-GRAVA-REALIZADO                 SECTION.
      *-----------------------------------------------------------------

           MOVE 0009                     TO PSC-ERRO-PGM
           SET  VINCULOU                 TO TRUE
           SET  SGTST087-INCLUIR         TO TRUE
           MOVE SGTS0167-CD-CTRA-TIT-VL  TO SGTST087-CD-CTRA-TIT-VL
           MOVE SGTS0167-CD-OPR-ETRD-MCP TO SGTST087-CD-OPR-ETRD-MCP
           MOVE SGTS0167-NR-VRS-OPR-ETRD TO SGTST087-NR-VRS-OPR-ETRD
           MOVE SGTS0167-NR-SEQL-RCBT-PRVT
                                         TO SGTST087-NR-SEQL-RCBT-PRVT
           MOVE SGTSL600-DT-RCBT-PRVT(IA-1)
                                         TO SGTST087-DT-RCBT-PRVT
           MOVE SGTS0167-CD-TIT-OPR-ETRD
                                         TO SGTST087-CD-TIT-OPR-ETRD
           MOVE 1                        TO SGTST087-NR-DVS-TIT
           MOVE HLPKE001-TS-FMTD         TO SGTST087-TS-REG-PGTO
           MOVE 'N'                      TO SGTST087-IN-VCL-MNL
           MOVE SGTS0167-CD-USU-IMP      TO SGTST087-CD-USU-VCL
           MOVE HLPKE001-TS-FMTD         TO SGTST087-TS-ULT-ATL

           IF EXECUCAO-ONLINE
              EXEC CICS LINK
                        PROGRAM ( SGTST087       )
                        COMMAREA( SGTST087-DADOS )
                        RESP    ( WS-RESP1)
                        RESP2   ( WS-RESP2)
              END-EXEC
              IF  WS-RESP1 NOT EQUAL  DFHRESP(NORMAL)
                  MOVE  3004           TO  CD-RTN-PGM
                  MOVE  'Erro ao acionar o módulo.'
                                       TO  MSG-ERRO
                  PERFORM 999001-ERRO-001
              END-IF
           ELSE
                  MOVE LENGTH OF SGTST087-DADOS TO EIBCALEN
                  CALL SGTST087 USING DFHEIBLK SGTST087-DADOS
           END-IF

           IF SGTST087-CD-RTN-PGM NOT EQUAL ZEROS
              MOVE SGTST087-RTN-PGM TO RTN-PGM
              IF SGTST087-PGM-ERRO      EQUAL 'SGTS014G'
                 MOVE 'SGTST087'        TO PGM-ERRO
              END-IF
              PERFORM 999001-ERRO-001
           END-IF

           .

       232100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       240000-DESATI-DMTV-ANT                 SECTION.
      *-----------------------------------------------------------------

           MOVE 0010                     TO PSC-ERRO-PGM
           MOVE 1                        TO IB-1
           IF EXECUCAO-ONLINE
              EXEC CICS LINK
                        PROGRAM ( SGTSL601       )
                        COMMAREA( SGTSL601-DADOS )
                        RESP    ( WS-RESP1)
                        RESP2   ( WS-RESP2)
              END-EXEC
              IF  WS-RESP1 NOT EQUAL  DFHRESP(NORMAL)
                  MOVE  3004           TO  CD-RTN-PGM
                  MOVE  'Erro ao acionar o módulo.'
                                       TO  MSG-ERRO
                  PERFORM 999001-ERRO-001
              END-IF
           ELSE
                  MOVE LENGTH OF SGTSL601-DADOS TO EIBCALEN
                  CALL SGTSL601 USING DFHEIBLK SGTSL601-DADOS
           END-IF

           IF SGTSL601-CD-RTN-PGM NOT EQUAL ZEROS
              MOVE SGTSL601-RTN-PGM TO RTN-PGM
              PERFORM 999001-ERRO-001
           END-IF

           IF   SGTSL601-QT-OCR-TAB      GREATER ZEROS
                PERFORM  241000-DESATIVAR
                         UNTIL  IB-1     GREATER SGTSL601-QT-OCR-TAB
                IF NOT SGTSL601-TEM-MAIS-REGISTRO
                   MOVE 'SIM'            TO      CH-ACABOU-REG
                ELSE
                   MOVE SGTSL601-TS-REG-PGTO(SGTSL601-QT-OCR-TAB)
                                         TO SGTSL601-E-TS-REG-PGT0
                END-IF
           ELSE
                MOVE 'SIM'               TO      CH-ACABOU-REG
           END-IF

           .

       240000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       241000-DESATIVAR                    SECTION.
      *-----------------------------------------------------------------

           MOVE 0011                      TO PSC-ERRO-PGM
           INITIALIZE SGTST086-DADOS
           SET  SGTST086-ALTERAR     TO TRUE
           MOVE SGTSL601-CD-TIT-OPR-ETRD(IB-1)
                                     TO SGTST086-CD-TIT-OPR-ETRD
           MOVE SGTSL601-NR-DVS-TIT(IB-1)
                                     TO SGTST086-NR-DVS-TIT
           MOVE SGTSL601-TS-REG-PGTO(IB-1)
                                     TO SGTST086-TS-REG-PGTO
           MOVE SGTSL601-DT-EFT-MVTC(IB-1)
                                     TO SGTST086-DT-EFT-MVTC
           MOVE SGTSL601-CD-TIP-MVTC-TIT(IB-1)
                                     TO SGTST086-CD-TIP-MVTC-TIT
           MOVE SGTSL601-CD-TIP-LCTO-CTB(IB-1)
                                     TO SGTST086-CD-TIP-LCTO-CTB
           MOVE SGTSL601-DT-PRVT-MVTC(IB-1)
                                     TO SGTST086-DT-PRVT-MVTC
           MOVE SGTSL601-VL-PRVT-MVTC(IB-1)
                                     TO SGTST086-VL-PRVT-MVTC
           MOVE SGTSL601-VL-EFT-MVTC(IB-1)
                                     TO SGTST086-VL-EFT-MVTC
           MOVE 'N'                  TO SGTST086-IN-MVTC-ATV
           MOVE SGTSL601-TX-DCR-MVTC(IB-1)
                                     TO SGTST086-TX-DCR-MVTC
           MOVE SGTS0167-CD-USU-IMP  TO SGTST086-CD-USU-IMP
           MOVE SGTSL601-DT-IMP-MVTC-FNCR(IB-1)
                                     TO SGTST086-DT-IMP-MVTC-FNCR
           MOVE SGTSL601-HR-IMP-MVTC-FNCR(IB-1)
                                     TO SGTST086-HR-IMP-MVTC-FNCR
           MOVE SGTSL601-NR-IMP-MVTC-FNCR(IB-1)
                                     TO SGTST086-NR-IMP-MVTC-FNCR
           MOVE SGTSL601-CD-CTRA-TIT-VL(IB-1)
                                     TO SGTST086-CD-CTRA-TIT-VL
           MOVE 1                    TO   SGTST086-CD-TIP-OGM-INF

           IF EXECUCAO-ONLINE
              EXEC CICS LINK
                        PROGRAM ( SGTST086       )
                        COMMAREA( SGTST086-DADOS )
                        RESP    ( WS-RESP1)
                        RESP2   ( WS-RESP2)
              END-EXEC
              IF  WS-RESP1 NOT EQUAL  DFHRESP(NORMAL)
                  MOVE  3004           TO  CD-RTN-PGM
                  MOVE  'Erro ao acionar o módulo.'
                                       TO  MSG-ERRO
                  PERFORM 999001-ERRO-001
              END-IF
           ELSE
                  MOVE LENGTH OF SGTST086-DADOS TO EIBCALEN
                  CALL SGTST086 USING DFHEIBLK SGTST086-DADOS
           END-IF

           IF SGTST086-CD-RTN-PGM     NOT EQUAL ZEROS
              MOVE SGTST086-RTN-PGM    TO  RTN-PGM
              PERFORM 999001-ERRO-001
           END-IF

           ADD   1   TO   IB-1   ON SIZE ERROR
              MOVE  0706               TO  CD-RTN-PGM
              MOVE 'Erro em operação aritmética.'
                                       TO  MSG-ERRO
              PERFORM 999001-ERRO-001
           END-ADD

           .

       241000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       250000-RCB-PRV-JUROS                 SECTION.
      *-----------------------------------------------------------------
      * Grava todo o valor que sobrou na última parcela de juros
      *
           MOVE 0010                     TO PSC-ERRO-PGM
           INITIALIZE SGTSL600-AREA-DADOS SGTSL600-QT-OCR-TAB

           PERFORM 231000-BUSCA-RCB-PREVISTO
           IF   SGTSL600-QT-OCR-TAB      GREATER ZEROS AND
                AUX-VALOR                GREATER ZEROS
                MOVE 1                   TO SGTST087-CD-TIP-RCBT-PRVT
                MOVE AUX-VALOR           TO SGTST087-VL-RLZD-VCLD
                MOVE SGTSL600-QT-OCR-TAB TO IA-1
                PERFORM  232100-GRAVA-REALIZADO
                MOVE ZEROS               TO AUX-VALOR
           END-IF

           .

       250000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       260000-VERIF-ATRASO                    SECTION.
      *-----------------------------------------------------------------
      *
           MOVE 0010                       TO PSC-ERRO-PGM
           INITIALIZE SGTSLJ11-DADOS DT-PRMO-CRNG-ABTO
           SET  SGTSLJ11-PRIM-DT-ABTO      TO TRUE
           MOVE SGTS0167-CD-CTRA-TIT-VL    TO SGTSLJ11-CD-CTRA-TIT-VL
           MOVE SGTS0167-CD-OPR-ETRD-MCP   TO SGTSLJ11-CD-OPR-ETRD-MCP
           MOVE SGTS0167-NR-VRS-OPR-ETRD   TO SGTSLJ11-NR-VRS-OPR-ETRD
           MOVE SGTS0167-NR-SEQL-RCBT-PRVT TO SGTSLJ11-NR-SEQL-RCBT-PRVT
           IF EXECUCAO-ONLINE
              EXEC CICS LINK
                        PROGRAM ( SGTSLJ11       )
                        COMMAREA( SGTSLJ11-DADOS )
                        RESP    ( WS-RESP1)
                        RESP2   ( WS-RESP2)
              END-EXEC
              IF  WS-RESP1 NOT EQUAL  DFHRESP(NORMAL)
                  MOVE  3004           TO  CD-RTN-PGM
                  MOVE  'Erro ao acionar o módulo.'
                                       TO  MSG-ERRO
                  PERFORM 999001-ERRO-001
              END-IF
           ELSE
                  MOVE LENGTH OF SGTSLJ11-DADOS TO EIBCALEN
                  CALL SGTSLJ11 USING DFHEIBLK SGTSLJ11-DADOS
           END-IF

           IF SGTSLJ11-CD-RTN-PGM NOT EQUAL ZEROS
              MOVE SGTSLJ11-RTN-PGM TO RTN-PGM
              PERFORM 999001-ERRO-001
           END-IF
           IF SGTSLJ11-DT-PRMO-CRNG-ABTO NOT EQUAL SPACES AND
              SGTSLJ11-DT-PRMO-CRNG-ABTO NOT EQUAL '01.01.1900'
              MOVE CORR SGTSLJ11-DT-PRMO-CRNG-ABTO-R
                                         TO        DT-PRMO-CRNG-ABTO-R
              IF DT-PRMO-CRNG-ABTO   NOT GREATER   DT-REF-INI
                 SET ATRASO              TO        TRUE
              END-IF

      *        IF DT-PRMO-CRNG-ABTO       LESS      DT-HOJE
      *           IF DT-HOJE              EQUAL     DT-REF-FIM
      *              IF DT-PRMO-CRNG-ABTO NOT GREATER  DT-HOJE-D-1DU
      *                 SET ATRASO        TO        TRUE
      *              END-IF
      *           ELSE
      *              IF DT-PRMO-CRNG-ABTO LESS      DT-HOJE-D-1DU
      *                 SET ATRASO        TO        TRUE
      *              END-IF
      *           END-IF
      *         END-IF
            END-IF

           .

       260000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       270000-TRATA-PAGAMENTO                 SECTION.
      *-----------------------------------------------------------------
      *
           MOVE 0010                     TO  PSC-ERRO-PGM
           SET  SGTSL600-LST-TP-DATA     TO  TRUE
           MOVE SGTS0167-CD-CTRA-TIT-VL  TO  SGTSL600-E-CD-CTRA-TIT-VL
           MOVE SGTS0167-CD-OPR-ETRD-MCP
                                         TO  SGTSL600-E-CD-OPR-ETRD-MCP
           MOVE SGTS0167-NR-VRS-OPR-ETRD
                                         TO  SGTSL600-E-NR-VRS-OPR-ETRD
           MOVE SGTS0167-NR-SEQL-RCBT-PRVT
                                        TO  SGTSL600-E-NR-SEQL-RCBT-PRVT
           MOVE 2                       TO  SGTSL600-E-CD-TIP-RCBT-PRVT
           MOVE CORR DT-REF-INI-R       TO  DT-FMTD
           MOVE DT-FMTD                 TO  SGTSL600-E-DT-RCBT-PRVT-INI
           MOVE CORR DT-REF-FIM-R       TO  DT-FMTD
           MOVE DT-FMTD                 TO  SGTSL600-E-DT-RCBT-PRVT-FIM
           MOVE SPACES                  TO  IN-VINCULO
           PERFORM 230000-RCB-PRV-CAPITAL
           IF AUX-VALOR             GREATER ZEROS
              IF  AUX-VL-ABSOLUTO    EQUAL  AUX-VALOR
      * Nesta situação ou o valor do previsto é maior que o valor da
      * drive ou não não há cronograma de capital. Não foi realizado
      * vínculo de pagamento de capital. O campo AUX-VL-ABSOLUTO é
      * o valor absoluto vindo da drive.
                   IF SGTSL600-VL-RCBT-PRVT(1) NOT GREATER ZEROS
      * Não tem cronograma de capital então vai buscar o de juros
                      INITIALIZE SGTSL600-AREA-DADOS
                      MOVE SPACES   TO       IN-VINCULO
                      MOVE 1        TO       SGTSL600-E-CD-TIP-RCBT-PRVT
                      PERFORM 250000-RCB-PRV-JUROS
                      IF NOT VINCULOU
      * Não tem cronograma de juros
                         MOVE  0890 TO      CD-RTN-PGM
                         MOVE 'Operação sem cronograma de recebimento pr
      -                  'evisto capital/juros.'
                                    TO      MSG-ERRO
                                            SGTST065-TX-DCR-ERRO-IMP
                         PERFORM 280000-VERIF-IMP
                      END-IF
                    ELSE
      * Nesta condição o valor da drive é menor que o previsto de capi-
      * tal.
                      MOVE  0891    TO       CD-RTN-PGM
                      STRING 'Valor recebido da ' NM-EMPR ' menor que o
      -               'valor previsto de capital.' DELIMITED BY '*'
                                    INTO     MSG-ERRO
                      MOVE MSG-ERRO TO       SGTST065-TX-DCR-ERRO-IMP
                      PERFORM 280000-VERIF-IMP
                   END-IF
              ELSE
      * Nessa condição vinculou o capital e falta vincular o de juros
                   INITIALIZE SGTSL600-AREA-DADOS
                   MOVE SPACES      TO       IN-VINCULO
                   MOVE 1           TO       SGTSL600-E-CD-TIP-RCBT-PRVT
                   PERFORM 250000-RCB-PRV-JUROS
                   IF NOT VINCULOU
      * Não tem cronograma de juros
                      MOVE  0892    TO      CD-RTN-PGM
                      MOVE 'Operação sem cronograma de recebimento previ
      -               'sto de juros.'
                                    TO      MSG-ERRO
                                            SGTST065-TX-DCR-ERRO-IMP
                      PERFORM 280000-VERIF-IMP
                    END-IF
              END-IF
           ELSE
      * Nessa condição vinculou capital. Vou verificar se tem juros
      *
                INITIALIZE SGTSL600-AREA-DADOS
                MOVE 1              TO       SGTSL600-E-CD-TIP-RCBT-PRVT
                PERFORM 250000-RCB-PRV-JUROS
                IF SGTSL600-QT-OCR-TAB      GREATER  ZEROS
      * Tem juros para vínculo e o valor da drive só vinculou capital
                   MOVE  0893       TO      CD-RTN-PGM
                   MOVE 'Valor recebido da Drive só foi suficiente para
      -               'vincular no capital.'
                                    TO      MSG-ERRO
                                            SGTST065-TX-DCR-ERRO-IMP
                      PERFORM 280000-VERIF-IMP
                END-IF
            END-IF

           .

       270000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       280000-VERIF-IMP                     SECTION.
      *-----------------------------------------------------------------
      *
           MOVE 0010                     TO  PSC-ERRO-PGM
           IF  EXECUCAO-ONLINE
               PERFORM 999001-ERRO-001
           ELSE
               SET  SGTST065-ALTERAR  TO TRUE
               MOVE 6                 TO SGTST065-CD-EST-IMP
               PERFORM 910008-CHAMA-SGTST065
           END-IF

           .

       280000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       910001-VALIDA-DATA                     SECTION.
      *-----------------------------------------------------------------
              MOVE 0012                TO PSC-ERRO-PGM

              MOVE 'F01'               TO FUNCAO
              MOVE 4                   TO ARG02
              MOVE CORR  SGTS0167-DT-EFT-MVTC-R
                                       TO    ARG01-R
              CALL  SBDATAF  USING  FUNCAO  AREA-WORK  ARG01
                                                       ARG02

      *--> Return code = 24: Data Inválida.
              IF  RETURN-CODE   NOT EQUAL   ZEROS   AND   24
                  MOVE 51              TO CD-RTN-PGM
                  MOVE 'Erro no tratamento de datas.' TO MSG-ERRO
                  PERFORM 999001-ERRO-001
              END-IF
            .

       910001-SAI.
            EXIT.



      *-----------------------------------------------------------------
       910007-CALCULA-DATA                    SECTION.
      *-----------------------------------------------------------------
      *
      * calcula o dia util anterior a data efetiva da movimentação e
      * da data do sistema.
      * IND-FERIADO = 1 ----> feriado
      *             = 0 ----> nao feriado
      * IND-DIA-SEMANA = 6 ---> sabado
      *                = 7 ----> domingo

              MOVE 0013                   TO    PSC-ERRO-PGM

              MOVE 0                      TO    ARG04
              MOVE 1                      TO    IND-FERIADO
              PERFORM UNTIL (IND-FERIADO  EQUAL 0) AND
                            (IND-DIA-SEMANA NOT EQUAL 6 AND
                             IND-DIA-SEMANA NOT EQUAL 7)
                MOVE 'F05'                TO    FUNCAO
                CALL SBDATAF              USING FUNCAO AREA-WORK ARG01
                                                ARG02-2 ARG03 ARG04

                 IF  RETURN-CODE           NOT EQUAL ZEROS
                     MOVE 51               TO   CD-RTN-PGM
                     MOVE 'Erro no tratamento de datas.' TO MSG-ERRO
                     PERFORM 999001-ERRO-001
                 ELSE
                     MOVE ARG04            TO    ARG01
                     MOVE 0                TO    ARG04 IND-FERIADO
                     MOVE 'F23'            TO    FUNCAO
                     CALL SBDATAF          USING FUNCAO AREA-WORK
                                                ARG01 ARG02-1

                     IF  RETURN-CODE       NOT EQUAL ZEROS
                         MOVE 51           TO   CD-RTN-PGM
                         MOVE 'Erro no tratamento de datas.' TO MSG-ERRO
                         PERFORM 999001-ERRO-001
                     ELSE
                      IF (IND-FERIADO       EQUAL  1) OR
                         (IND-DIA-SEMANA    EQUAL  6 OR 7)
                          MOVE 0            TO     ARG04
                      END-IF
                     END-IF
                  END-IF
                END-PERFORM
            .

       910007-SAI.
            EXIT.

      *-----------------------------------------------------------------
       910008-CHAMA-SGTST065               SECTION.
      *-----------------------------------------------------------------
      *
             MOVE 0014                   TO    PSC-ERRO-PGM

           IF EXECUCAO-ONLINE
              EXEC CICS LINK
                        PROGRAM ( SGTST065       )
                        COMMAREA( SGTST065-DADOS )
                        RESP    ( WS-RESP1)
                        RESP2   ( WS-RESP2)
              END-EXEC
              IF  WS-RESP1 NOT EQUAL  DFHRESP(NORMAL)
                  MOVE  3004           TO  CD-RTN-PGM
                  MOVE  'Erro ao acionar o módulo.'
                                       TO  MSG-ERRO
                  PERFORM 999001-ERRO-001
              END-IF
           ELSE
                  MOVE LENGTH OF SGTST065-DADOS TO EIBCALEN
                  CALL SGTST065 USING DFHEIBLK SGTST065-DADOS
           END-IF
           IF SGTST065-CD-RTN-PGM NOT EQUAL ZEROS
              MOVE SGTST065-RTN-PGM TO RTN-PGM
              PERFORM 999001-ERRO-001
           END-IF

           .

       910008-SAI.
            EXIT.

      *-----------------------------------------------------------------
       920001-BUSCA-TS-DT             SECTION.
      *-----------------------------------------------------------------
           MOVE 0015                   TO PSC-ERRO-PGM
           CALL SBTIMEDT            USING HLPKE001-CD-FUC
                                          HLPKE001-AREA-TRB
                                          HLPKE001-FMT-TS
                                          HLPKE001-TAM-TS
                                          HLPKE001-TS-FMTD
           IF  RETURN-CODE      NOT EQUAL ZEROS
               MOVE 50                 TO CD-RTN-PGM
               MOVE 'Erro ao obter data e hora atual.'
                                       TO MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF
           .

       920001-SAI.
           EXIT.

      *----------------------------------------------------------------*
      * 998000-GRAVA-APPL-ERRO         SECTION.                        *
      * 999082-RECUPERA-MSG-CODIFICADA SECTION.                        *
      * 999001-ERRO-001                SECTION.                        *
      *----------------------------------------------------------------*
-INC HLPKE004
           PERFORM  999999-ENCERRAR
           .
       999001-SAI.
           EXIT.
      *----------------------------------------------------------------*
      * 999003-TRATA-ABEND-CICS        SECTION.                        *
      * 999004-TRATA-CONDITION-CICS    SECTION.                        *
      * 999005-CMPT-ERRO-CICS          SECTION.                        *
      *----------------------------------------------------------------*
-INC HLPKE006

      *-----------------------------------------------------------------
      *    As Sections e Books abaixo fazem o tratamento de erro
      *    através do Sistema MSG (rotina MSGS0001)
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       999000-TRATAR-ERRO-MSG          SECTION.
      *--> Envia a Ocorrência de Erro para o Sistema de Mensageria
      *-----------------------------------------------------------------
      * MOCK-POINT MCKV1
      *--> Alterar o MOVE conforme o Book do módulo
           MOVE  SGTS0167-TIP-RTN-PGM  TO  TIP-RTN-PGM
      *--> Alterar os Campos de Entrada conforme o Book do módulo
           PERFORM  999100-MONITORAR-ENTRADA

      *--> Movimenta Campos Básicos da Ocorrência de Erro
           MOVE  CD-RTN-PGM            TO  MSG1-CD-ERRO-TS
           MOVE  PSC-ERRO-PGM          TO  MSG1-CD-SEQL-ERRO
           MOVE  MSG-ERRO              TO  MSG1-TX-ERRO-TS

      *--> Realiza a tradução do tipo de erro do COMMAREA
      *    para tipos de erro da Mensageria
           EVALUATE  TRUE
               WHEN  ERRO-DE-CICS
                     PERFORM  999300-MONITORAR-CICS
               WHEN  ERRO-DE-PGM
                     SET  ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
                     MOVE CD-RTN-PGM   TO  MSG1-CD-RTN-PGM-ACND
                     MOVE MSG-ERRO     TO  MSG1-TX-RTN-PGM-ACND
                     MOVE PGM-ERRO     TO  MSG1-NM-PGM-ACND
                     MOVE PSC-ERRO-PGM TO  MSG1-CD-SEQL-PGM-ACND
                     PERFORM  999400-MONITORAR-VARIAVEIS
               WHEN  ERRO-CODIFICADO
                     SET  ERRO-PROGRAMADO
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

           MOVE 'SGTS0167-DT-EFT-MVTC'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0167-DT-EFT-MVTC
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTS0167-CD-TIT-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0167-CD-TIT-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTS0167-QT-EVT'      TO  MSGKW999-NM-VRV-MNT
           IF  SGTS0167-QT-EVT-R    IS NUMERIC
               MOVE  SGTS0167-QT-EVT-R TO  MCR-S9-15-V9-14
               MOVE  MCR-S9-15-V9-14   TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTS0167-CD-TIP-LCTO-CTB'
                                       TO  MSGKW999-NM-VRV-MNT
           IF  SGTS0167-CD-TIP-LCTO-CTB   IS   NUMERIC
               MOVE  SGTS0167-CD-TIP-LCTO-CTB
                                       TO  MCR-S9-01
               MOVE  MCR-S9-01         TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTS0167-VL-EFT-MVTC-R'
                                       TO  MSGKW999-NM-VRV-MNT
           IF  SGTS0167-VL-EFT-MVTC-R   IS   NUMERIC
               MOVE  SGTS0167-VL-EFT-MVTC-R
                                       TO  MCR-S9-13-V9-02
               MOVE  MCR-S9-13-V9-02   TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTS0167-TX-DCR-TIP-CPTR'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0167-TX-DCR-TIP-CPTR
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTS0167-TX-OBS-MVTC' TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0167-TX-OBS-MVTC  TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTS0167-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
           IF  SGTS0167-CD-OPR-ETRD-MCP   IS   NUMERIC
               MOVE  SGTS0167-CD-OPR-ETRD-MCP
                                       TO  MCR-S9-11
               MOVE  MCR-S9-11         TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTS0167-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0167-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTS0167-NR-SEQL-RCBT-PRVT'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0167-NR-SEQL-RCBT-PRVT
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTS0167-DT-IMP-MVTC-FNCR'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0167-DT-IMP-MVTC-FNCR
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTS0167-HR-IMP-MVTC-FNCR'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0167-HR-IMP-MVTC-FNCR
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTS0167-NR-IMP-MVTC-FNCR'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0167-NR-IMP-MVTC-FNCR
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTS0167-CD-USU-IMP'  TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0167-CD-USU-IMP   TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTS0167-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0167-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SBDATA-RETURN-CODE'   TO  MSGKW999-NM-VRV-MNT
           MOVE  RETURN-CODE           TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

      *--> Monitorar variável da mensagem dinâmica 0891
           MOVE 'NM-EMPR'              TO  MSGKW999-NM-VRV-MNT
           MOVE  NM-EMPR               TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT
           .
       999100-SAI.
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
      * MOCK-POINT MCKV2
           EVALUATE  PGM-ERRO
               WHEN  SGTST065
                     MOVE 'SGTST065-CD-CHV-USU'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST065-CD-CHV-USU
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTST065-ACAO'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST065-ACAO
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTST065-DT-IMP-MVTC-FNCR'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST065-DT-IMP-MVTC-FNCR
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTST065-HR-IMP-MVTC-FNCR'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST065-HR-IMP-MVTC-FNCR
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTST065-NR-IMP-MVTC-FNCR'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST065-NR-IMP-MVTC-FNCR
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTST065-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST065-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE TX-IDFC-ATNT-MSG-E
                                       TO AUX-ERRO
                     IF  AUX-ERRO      NOT EQUAL  SPACES
                      IF AUX-CD-ERRO   EQUAL 0050
                         MOVE 'SBDATA-RETURN-CODE'
                                       TO  MSGKW999-NM-VRV-MNT
                      ELSE
                         MOVE 'SGTS014G-RETURN-CODE'
                                       TO  MSGKW999-NM-VRV-MNT
                      END-IF
                      MOVE  AUX-CD-ERRO
                                       TO  MSGKW999-VL-VRV-MNT
                      PERFORM  999500-ADICIONA-VRV-MNT
                     END-IF

               WHEN  SGTST086
                     MOVE 'SGTST086-CD-CHV-USU'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST086-CD-CHV-USU
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTST086-ACAO'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST086-ACAO
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTST086-CD-TIT-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST086-CD-TIT-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTST086-NR-DVS-TIT'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST086-NR-DVS-TIT
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTST086-TS-REG-PGTO'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST086-TS-REG-PGTO
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE TX-IDFC-ATNT-MSG-E
                                       TO AUX-ERRO
                     IF  AUX-ERRO      NOT EQUAL  SPACES
                      IF AUX-CD-ERRO   EQUAL 0050
                         MOVE 'SBDATA-RETURN-CODE'
                                       TO  MSGKW999-NM-VRV-MNT
                      ELSE
                         MOVE 'SGTS014G-RETURN-CODE'
                                       TO  MSGKW999-NM-VRV-MNT
                      END-IF
                      MOVE  AUX-CD-ERRO
                                       TO  MSGKW999-VL-VRV-MNT
                      PERFORM  999500-ADICIONA-VRV-MNT
                     END-IF

               WHEN  SGTST087
                     MOVE 'SGTST087-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST087-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTST087-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                     IF  SGTST087-CD-OPR-ETRD-MCP    IS NUMERIC
                         MOVE  SGTST087-CD-OPR-ETRD-MCP
                                       TO  MCR-S9-11
                         MOVE  MCR-S9-11
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTST087-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST087-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTST087-NR-SEQL-RCBT-PRVT'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST087-NR-SEQL-RCBT-PRVT
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTST087-CD-TIP-RCBT-PRVT'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST087-CD-TIP-RCBT-PRVT
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTST087-DT-RCBT-PRVT'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST087-DT-RCBT-PRVT
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTST087-CD-TIT-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST087-CD-TIT-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTST087-NR-DVS-TIT'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST087-NR-DVS-TIT
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTST087-TS-REG-PGTO'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST087-TS-REG-PGTO
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE TX-IDFC-ATNT-MSG-E
                                       TO AUX-ERRO
                     IF  AUX-ERRO      NOT EQUAL  SPACES
                      IF AUX-CD-ERRO   EQUAL 0050
                         MOVE 'SBDATA-RETURN-CODE'
                                       TO  MSGKW999-NM-VRV-MNT
                      ELSE
                         MOVE 'SGTS014G-RETURN-CODE'
                                       TO  MSGKW999-NM-VRV-MNT
                      END-IF
                      MOVE  AUX-CD-ERRO
                                       TO  MSGKW999-VL-VRV-MNT
                      PERFORM  999500-ADICIONA-VRV-MNT
                     END-IF

               WHEN  SGTSL600
                     MOVE 'SGTSL600-CD-FUNCAO'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL600-CD-FUNCAO
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTSL600-E-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL600-E-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTSL600-E-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                     IF  SGTSL600-E-CD-OPR-ETRD-MCP    IS NUMERIC
                         MOVE  SGTSL600-E-CD-OPR-ETRD-MCP
                                       TO  MCR-S9-11
                         MOVE  MCR-S9-11
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTSL600-E-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL600-E-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTSL600-E-NR-SEQL-RCBT-PRVT'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL600-E-NR-SEQL-RCBT-PRVT
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTSL600-E-CD-TIP-RCBT-PRVT'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL600-E-CD-TIP-RCBT-PRVT
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTSL600-E-DT-RCBT-PRVT-INI'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL600-E-DT-RCBT-PRVT-INI
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTSL600-E-DT-RCBT-PRVT-FIM'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL600-E-DT-RCBT-PRVT-FIM
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

               WHEN  SGTSL601
                     MOVE 'SGTSL601-CD-FUNCAO'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL601-CD-FUNCAO
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTSL601-E-CD-TIT-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL601-E-CD-TIT-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTSL601-E-NR-DVS-TIT'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL601-E-NR-DVS-TIT
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTSL601-E-TS-REG-PGT0'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL601-E-TS-REG-PGT0
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTSL601-E-IN-MVTC-ATV'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL601-E-IN-MVTC-ATV
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTSL601-E-DT-EFT-MVTC'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL601-E-DT-EFT-MVTC
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTSL601-E-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL601-E-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE TX-IDFC-ATNT-MSG-E
                                       TO AUX-ERRO
                     IF  AUX-ERRO      NOT EQUAL  SPACES
                         MOVE 'SBDATA-RETURN-CODE'
                                       TO  MSGKW999-NM-VRV-MNT
                         MOVE  AUX-CD-ERRO
                                       TO  MSGKW999-VL-VRV-MNT
                         PERFORM  999500-ADICIONA-VRV-MNT
                     END-IF

               WHEN  SGTSLJ11
                    MOVE  'SGTSLJ11-ACAO'  TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSLJ11-ACAO    TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999500-ADICIONA-VRV-MNT

                    MOVE  'SGTSLJ11-DT-IMP-PSC-SDO'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSLJ11-DT-IMP-PSC-SDO
                                           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999500-ADICIONA-VRV-MNT

                    MOVE  'SGTSLJ11-HR-IMP-PSC-SDO'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSLJ11-HR-IMP-PSC-SDO
                                           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999500-ADICIONA-VRV-MNT

                    MOVE  'SGTSLJ11-DT-PSC-E'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSLJ11-DT-PSC-E
                                           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999500-ADICIONA-VRV-MNT

                    MOVE  'SGTSLJ11-CD-CTRA-TIT-VL'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSLJ11-CD-CTRA-TIT-VL
                                           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999500-ADICIONA-VRV-MNT

                    MOVE  'SGTSLJ11-CD-OPR-ETRD-MCP '
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSLJ11-CD-OPR-ETRD-MCP
                                           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999500-ADICIONA-VRV-MNT

                    MOVE  'SGTSLJ11-NR-VRS-OPR-ETRD '
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSLJ11-NR-VRS-OPR-ETRD
                                           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999500-ADICIONA-VRV-MNT

                    MOVE  'SGTSLJ11-NR-SEQL-RCBT-PRVT '
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSLJ11-NR-SEQL-RCBT-PRVT
                                           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999500-ADICIONA-VRV-MNT

                    MOVE  'SGTSLJ11-CD-TIP-FUC-CLI '
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSLJ11-CD-TIP-FUC-CLI
                                           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999500-ADICIONA-VRV-MNT

                    MOVE  'SGTSLJ11-CD-TIT-OPR-ETRD '
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSLJ11-CD-TIT-OPR-ETRD
                                           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999500-ADICIONA-VRV-MNT

                    MOVE  'SGTSLJ11-NR-DVS-TIT '
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSLJ11-NR-DVS-TIT
                                           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999500-ADICIONA-VRV-MNT

                    MOVE  'SGTSLJ11-DT-PRMO-CRNG-ABTO '
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSLJ11-DT-PRMO-CRNG-ABTO
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
       999999-ENCERRAR                 SECTION.
      *-----------------------------------------------------------------
           MOVE  RTN-PGM               TO  SGTS0167-RTN-PGM

           IF  EXECUCAO-ONLINE
               PERFORM  999000-TRATAR-ERRO-MSG
           END-IF

           GOBACK
           .
       999999-SAI.
           EXIT.
      *------------------------> FIM SGTS0167 <------------------------*