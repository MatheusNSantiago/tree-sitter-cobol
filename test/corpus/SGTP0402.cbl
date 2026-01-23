      *-----------------------------------------------------------------
      * PROGRAMA..: SGTS0116
      * ANALISTA..: F0175702 ADRIANO SEIJI NAKAMURA
      * AUTOR.....: F0175702 ADRIANO SEIJI NAKAMURA
      * DATA......: 19/12/2016
      * OBJETIVO..: Alterar Remuneracao da Operacao Estruturada.
      *
      * COMPILACAO: COBOL/CICS
      *-----------------------------------------------------------------
      * VRS001 31/01/2019 F0950413 inclusão da sigla 'SGT' na section
      *                   972000-LER-CONTAINER-DE-ERRO
      *        21/01/2019 F8658869 - Incluir monitoração para a rotina
      *                   SBDATAF
      *        09/10/2018 F9158937 - Incluir consistência data inicio
      *                   Precificação >= que início da Remuneração
      *        15/05/2018 F8658869 DE 2015/137233 - Tornar carteira
      *                   variável.
      *        19/12/2016 F0175702 DE 2015/137233 - IMPLANTACAO
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTS0116.
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
      *
      *-----------------------------------------------------------------
       DATA                            DIVISION.
      *-----------------------------------------------------------------
      *

       WORKING-STORAGE                 SECTION.
      *-----------------------------------------------------------------

       77  CTE-INICIO-SS               PIC  X(40) VALUE
                                       '*** S.S. Comeca Aqui ***'.
       77  CTE-PROG                    PIC  X(18)  VALUE
                                       '*** SGTS0116 ***'.
       77  CTE-VERS                    PIC  X(06)  VALUE 'VRS001'.

      *--> Módulos acionados
       77  SGTST005                    PIC  X(08) VALUE 'SGTST005'.
       77  SGTSI011                    PIC  X(08) VALUE 'SGTSI011'.
       77  SGTSL011                    PIC  X(08) VALUE 'SGTSL011'.
       77  SGTSE011                    PIC  X(08) VALUE 'SGTSE011'.
       77  SGTS0185                    PIC  X(08) VALUE 'SGTS0185'.
       77  SGTSC063                    PIC  X(08) VALUE 'SGTSC063'.
       77  SGTS0183                    PIC  X(08) VALUE 'SGTS0183'.
       77  SBTIMEDT                    PIC  X(08) VALUE 'SBTIMEDT'.
       77  SBDATAF                     PIC  X(08) VALUE 'SBDATAF '.

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

      *--> Áreas de trabalho
       01  VL-05V10                     PIC  --.--9,9999999999.
       01  VL-15V02                     PIC  ----.---.---.---.--9,99.
       01  VL-09V08                     PIC  ----.---.--9,99999999.

      *--> Auxiliares Movimentacao Monitoração
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).

      *--> Area da SBDATAF
       01  SBDATAF-AREA.
           03  FUNCAO                  PIC  X(003) VALUE 'F05'.
           03  ARG01                   PIC  9(008) VALUE 0.
           03  ARG02                   PIC  9(005) VALUE 0.
           03  ARG03                   PIC  9(001) VALUE 0.
           03  ARG04                   PIC  9(008) VALUE 0.

       01  AREAWORK                    PIC  X(4095).
       01  AREA-RED REDEFINES AREAWORK.
           03  A-TAM                   PIC  9(0008) COMP.
           03  FILLER                  PIC  X(4071).
           03  A-FIM                   PIC  X(0020).

       01  DT-AUX-DMA                  PIC  X(10) VALUE ' '.
       01  DT-AUX-DMA-R                REDEFINES DT-AUX-DMA.
           02 DIA                      PIC  X(2).
           02 PTO1                     PIC  X(1).
           02 MES                      PIC  X(2).
           02 PTO2                     PIC  X(1).
           02 ANO                      PIC  X(4).

       01 AUX-DT-08                    PIC  X(08) VALUE SPACES.
       01 AUX-DT-08-R                  REDEFINES AUX-DT-08.
          03 DIA                       PIC  9(2).
          03 MES                       PIC  9(2).
          03 ANO                       PIC  9(4).

       01  DT-AMD-INI                  PIC  X(08) VALUE ' '.
       01  DT-AMD-INI-R         REDEFINES  DT-AMD-INI.
           03 ANO                      PIC  9(4).
           03 MES                      PIC  9(2).
           03 DIA                      PIC  9(2).

       01  DT-AMD-AUX                  PIC  X(08) VALUE ' '.
       01  DT-AMD-AUX-R         REDEFINES  DT-AMD-AUX.
           03 ANO                      PIC  9(4).
           03 MES                      PIC  9(2).
           03 DIA                      PIC  9(2).

       01  DT-INI-PREF                 PIC  X(08) VALUE ' '.
       01  DT-INI-PREF-R        REDEFINES  DT-INI-PREF.
           03 ANO                      PIC  9(4).
           03 MES                      PIC  9(2).
           03 DIA                      PIC  9(2).

      *-----------------------------------------------------------------
      *--> Área para Recuperar Timestamp e Data
      *-----------------------------------------------------------------
       01  DADOS-SBTIMEDT.
           03  WK-CD-FUC               PIC X(03) VALUE 'F01'.
           03  WK-AREA-TRB.
               05  WK-AREA-TRB-TAM     PIC S9(09) COMP VALUE 1024.
               05  WK-AREA-TRB-DADOS   PIC  X(1024) VALUE ' '.
      *        X'0002' - DD.MM.AAAA-HH:MM:SS.CCCCCCXXXX
           03  WK-FMT-TS               PIC S9(04) COMP VALUE 1.
      *         X'001A'(26)
           03  WK-TAM-TS               PIC S9(04) COMP VALUE 30.
           03  WK-TS-FMTD.
               05  ANO                 PIC  9(04).
               05  FILLER              PIC  X(01).
               05  MES                 PIC  9(02).
               05  FILLER              PIC  X(01).
               05  DIA                 PIC  9(02).
               05  FILLER              PIC  X(01).
               05  HORA                PIC  9(02).
               05  FILLER              PIC  X(01).
               05  MINUTO              PIC  9(02).
               05  FILLER              PIC  X(01).
               05  SEGUNDO             PIC  9(02).
               05  FILLER              PIC  X(01).
               05  MICRO-SEGUNDO       PIC  9(06).
               05  FILLER              PIC  X(04).

       01  SGTS0116-IC-TAB             PIC S9(04) COMP-5.
                 88 SGTS0116-ESTOURO-TAB          VALUE 0101 THRU 9999.
                 88 SGTS0116-PREENCHIDO-TAB       VALUE 0100.

       01 IC-TAB                       PIC S9(4) COMP-5.

      *--> Campo para definição de estado de erro em tabela.
       01  CD-ERRO-TAB                 PIC  9(01) VALUE 0.
           88 COM-ERRO-TAB                        VALUE 1.

       01  ERRO-DT-INC                 PIC  X(01) VALUE 'N'.
       01  ERRO-DT-FIM                 PIC  X(01) VALUE 'N'.
       01  ERRO-PC-TAXA                PIC  X(01) VALUE 'N'.
       01  ERRO-CD-IEC                 PIC  X(01) VALUE 'N'.
       01  ERRO-CD-TIP                 PIC  X(01) VALUE 'N'.
       01  ERRO-CD-PERC                PIC  X(01) VALUE 'N'.
       01  ERRO-DT-VCT                 PIC  X(01) VALUE 'N'.

      *-----------------------------------------------------------------
      *    Books Utilizados
      *-----------------------------------------------------------------

      *--> Book dos dados do Usuario
       COPY IIBKOPRD.

      *--> Book do Container de Erro
       COPY IIBKERRO.

      *--> Book para captura de abend cics da bloco codigo HLPKW999
       COPY HLPKW999.

      *--> Book das variáveis auxiliares do tratamento de erro via MSG
       COPY SGTKERR1.

      *--> Book para captura de erro via MSG1
       COPY MSGKWMG1.

      *--> Book do container de requisicao e resposta deste modulo
       COPY SGTK0116.

      *-----------------------------------------------------------------
      * Book dos Containers e das subrotinas chamadas
      *-----------------------------------------------------------------
      *--> CRUD OPR_ETRD_MCP
       01 SGTST005-DADOS.
       COPY SGTKT005.

      *--> Listar Remuneração Operação Estruturada
       01 SGTSL011-DADOS.
       COPY SGTKL011.

      *--> Incluir Remuneração Operação Estruturada
       COPY SGTKI011.

      *--> Excluir Remuneração Operação Estruturada
       COPY SGTKE011.

      *--> Comparar Taxas de Precificação com Taxas Remuneração
       01 SGTS0185-DADOS.
       COPY SGTK0185.

      *--> Consultar Indicador da  Operação Estruturada
       COPY SGTKC063.

      *--> Consulta Lista de Precificação da Operação Estruturada
       01 SGTS0183-DADOS.
       COPY SGTK0183.

       01  CTE-FINAL-SS                PIC X(40) VALUE
                                       '*** S.S. Termina Aqui ***'.

      *-----------------------------------------------------------------
       PROCEDURE                       DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------

           PERFORM  100000-INICIAR

           PERFORM  200000-PROCESSAR

           PERFORM  999999-ENCERRAR
           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-INICIAR                  SECTION.
      *-----------------------------------------------------------------
           MOVE  0001                  TO  MSG1-CD-SEQL-ERRO

      *--> Ativa a captura automática de abend do CICS.
      *    WHENEVER CICS
       COPY HLPKE999.

           PERFORM  970000-LER-CANAL

      *--> Define o nome do programa p/ fins manipulação container
           MOVE  'SGTS0116'            TO  NM-PGM

      *--> Limpa a resposta e erro antes de executar o módulo
           PERFORM  983000-EXCLUIR-RESPOSTA-ERRO

           PERFORM  971000-LER-REQUISICAO
           PERFORM  974000-LER-DADOS-OPRD

           PERFORM  110000-VALIDAR-REQUISICAO
           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-VALIDAR-REQUISICAO       SECTION.
      *-----------------------------------------------------------------
           MOVE  0002                  TO  MSG1-CD-SEQL-ERRO

      *--> Validar Chave da Operação Estruturada

           IF  SGTS0116-CD-CTRA-TIT-VL  < ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0001               TO MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de título e valores mobiliários
      -             'inválido.'        TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0116-CD-CTRA-TIT-VL  EQUAL ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0002               TO MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de título e valores mobiliários
      -             'não informado.'   TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0116-CD-OPR-ETRD-MCP    NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0003              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada do mercado de capit
      -              'ais não numérico.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0116-CD-OPR-ETRD-MCP    NOT POSITIVE
               SET ERRO-PROGRAMADO     TO  TRUE
               MOVE  0004              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada do mercado de capit
      -              'ais inválida.'   TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0116-NR-VRS-OPR-ETRD    NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0006              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação estruturado inválido.
      -              ''                   TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

      *--> Validar demais Campos de Entrada
           IF  SGTS0116-IN-RMNC-VRV        EQUAL SPACES
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0424              TO  MSG1-CD-ERRO-TS
               MOVE  'Indicador de remuneração variável não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0116-QT-OCR-TAB         <= ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0425              TO  MSG1-CD-ERRO-TS
               MOVE  'Quantidade de períodos inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0116-IN-RMNC-VRV        EQUAL 'N' AND
               SGTS0116-QT-OCR-TAB         >  0001
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0426              TO  MSG1-CD-ERRO-TS
               MOVE 'Quando remuneração for não escalonada, deve ser inf
      -             'ormado apenas um período de remuneração.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           PERFORM  111000-PESQ-OPERACAO-ESTRUT

      *--> Caso o campo "Escalonado"=N grava as datas da Opr na Remun.
           IF  SGTS0116-IN-RMNC-VRV        EQUAL 'N'
                 MOVE  CORR SGTST005-DT-EMS-TIT-OPR-R
                                       TO  AUX-DT-08-R
                 IF  AUX-DT-08             NOT EQUAL
                     SGTS0116-DT-INC-TAXA-RMNC-R(1)
                     SET   ERRO-PROGRAMADO
                                       TO  TRUE
                     MOVE  0427        TO  MSG1-CD-ERRO-TS
                     MOVE  'Data início da remuneração divergente da dat
      -                    'a de emissão do título.'
                                       TO  MSG1-TX-ERRO-TS
                     PERFORM  999000-ERRO
                 END-IF

                 MOVE  CORR SGTST005-DT-VNCT-OPR-R
                                       TO  AUX-DT-08-R
                 IF  AUX-DT-08             NOT EQUAL
                     SGTS0116-DT-FIM-TAXA-RMNC-R(1)
                     SET   ERRO-PROGRAMADO
                                       TO  TRUE
                     MOVE  0428        TO  MSG1-CD-ERRO-TS
                     MOVE  'Data fim da remuneração divergente da data d
      -                    'e vencimento do título.'
                                       TO  MSG1-TX-ERRO-TS
                     PERFORM  999000-ERRO
                 END-IF
           ELSE
                 MOVE  CORR SGTST005-DT-EMS-TIT-OPR-R
                                             TO  DT-AMD-INI-R
                 MOVE  CORR SGTS0116-DT-INC-TAXA-RMNC-R(1)
                                             TO  DT-AMD-AUX-R
                 IF  DT-AMD-AUX < DT-AMD-INI
                   SET   ERRO-PROGRAMADO   TO  TRUE
                   MOVE  0443              TO  MSG1-CD-ERRO-TS
                   MOVE  'Data inicial fora do período de vigência.'
                                       TO  MSG1-TX-ERRO-TS
                   PERFORM  999000-ERRO
                 END-IF
           END-IF

      *--> Validar se Periodo de Carencia e o mesmo da Operacao
           IF  SGTS0116-QT-MM-CARE-JUR     > SGTST005-QT-MM-CARE-JUR
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0429              TO  MSG1-CD-ERRO-TS
               MOVE  'Quantidade de meses da carência juros maior do que
      -              ' a precificada.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

      *--> Validar Ocorrências da Lista de Entrada
           PERFORM  VARYING SGTS0116-IC-TAB FROM 1 BY 1
             UNTIL SGTS0116-IC-TAB GREATER SGTS0116-QT-OCR-TAB
             IF  SGTS0116-DT-INC-TAXA-RMNC(SGTS0116-IC-TAB)
                                           =  SPACES
                 SET   ERRO-PROGRAMADO
                                       TO  TRUE
                 MOVE  0327            TO  MSG1-CD-ERRO-TS
                 MOVE  'Data início da taxa remuneração não informada.'
                                       TO  MSG1-TX-ERRO-TS
                 SET   COM-ERRO-TAB    TO  TRUE
                 MOVE  'S'             TO  ERRO-DT-INC
                 PERFORM  999000-ERRO
             END-IF

             IF  SGTS0116-DT-FIM-TAXA-RMNC(SGTS0116-IC-TAB)
                                           =  SPACES
                 SET   ERRO-PROGRAMADO
                                       TO  TRUE
                 MOVE  0430            TO  MSG1-CD-ERRO-TS
                 MOVE  'Data fim da taxa remuneração não informada.'
                                       TO  MSG1-TX-ERRO-TS
                 SET   COM-ERRO-TAB    TO  TRUE
                 MOVE  'S'             TO  ERRO-DT-FIM
                 PERFORM  999000-ERRO
             END-IF

      *--> Valida as datas para que todo o período da operação seja con-
      *    plado pelo intervalo de data e se a data-fim = data vnct opr.
             IF  SGTS0116-IC-TAB           <  SGTS0116-QT-OCR-TAB
                 PERFORM  112000-CALCULA-DATA
                 MOVE  CORR DT-AUX-DMA-R
                                       TO  AUX-DT-08-R
                 IF  AUX-DT-08 EQUAL  SGTS0116-DT-INC-TAXA-RMNC
                     (SGTS0116-IC-TAB + 1)
                     CONTINUE
                 ELSE
                    MOVE   CORR  AUX-DT-08-R       TO DT-AMD-AUX-R
                    MOVE   CORR
                       SGTS0116-DT-INC-TAXA-RMNC-R (SGTS0116-IC-TAB + 1)
                                                   TO DT-AMD-INI-R

                    MOVE 'DT-AMD-AUX'  TO MSGKW999-NM-VRV-MNT
                    MOVE  DT-AMD-AUX   TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT
                    MOVE 'DT-AMD-INI +1'  TO MSGKW999-NM-VRV-MNT
                    MOVE  DT-AMD-INI      TO MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    IF  DT-AMD-AUX  >  DT-AMD-INI
                        SET   ERRO-PROGRAMADO
                                          TO  TRUE
                        MOVE  0447        TO  MSG1-CD-ERRO-TS
                        MOVE  'Ocorreu sobreposição de datas nos período
      -                       's informados.'
                                          TO  MSG1-TX-ERRO-TS
                        SET   COM-ERRO-TAB
                                       TO  TRUE
                        MOVE  'S'         TO  ERRO-DT-INC
                        PERFORM  999000-ERRO

                    ELSE
                        SET   ERRO-PROGRAMADO
                                          TO  TRUE
                        MOVE  0432        TO  MSG1-CD-ERRO-TS
                        MOVE  'Deve ser preenchido todo o período da ope
      -                       'ração estruturada.'
                                          TO  MSG1-TX-ERRO-TS
                        SET   COM-ERRO-TAB
                                       TO  TRUE
                        MOVE  'S'         TO  ERRO-DT-INC
                        PERFORM  999000-ERRO
                    END-IF
                 END-IF
             ELSE
                 MOVE  CORR SGTST005-DT-VNCT-OPR-R
                                       TO  AUX-DT-08-R
                 IF  AUX-DT-08             =   SGTS0116-DT-FIM-TAXA-RMNC
                     (SGTS0116-IC-TAB)
                     CONTINUE
                 ELSE
                     SET   ERRO-PROGRAMADO
                                       TO  TRUE
                     MOVE  0433        TO  MSG1-CD-ERRO-TS
                     MOVE  'Data fim deve ser igual a data vencimento da
      -                    ' operação estruturada.'
                                       TO  MSG1-TX-ERRO-TS
                     SET   COM-ERRO-TAB
                                       TO  TRUE
                     MOVE  'S'         TO  ERRO-DT-FIM
                     PERFORM  999000-ERRO
                 END-IF
             END-IF

             IF  SGTS0116-PC-TAXA-RMNC-OPR(SGTS0116-IC-TAB) NOT NUMERIC
                 SET   ERRO-PROGRAMADO
                                       TO  TRUE
                 MOVE  0329            TO  MSG1-CD-ERRO-TS
                 MOVE  'Percentual da taxa de remuneração da operação nã
      -                'o numérico.'   TO  MSG1-TX-ERRO-TS
                 SET   COM-ERRO-TAB    TO  TRUE
                 MOVE  'S'             TO  ERRO-PC-TAXA
                 PERFORM  999000-ERRO
             END-IF

             IF  SGTS0116-PC-TAXA-RMNC-OPR(SGTS0116-IC-TAB)
                                           <=  ZEROS
                 SET   ERRO-PROGRAMADO
                                       TO  TRUE
                 MOVE  0331            TO  MSG1-CD-ERRO-TS
                 MOVE  'Percentual da taxa de remuneração da operação in
      -                'válido.'       TO  MSG1-TX-ERRO-TS
                 SET   COM-ERRO-TAB    TO  TRUE
                 MOVE  'S'             TO  ERRO-PC-TAXA
                 PERFORM  999000-ERRO
             END-IF

             IF  SGTS0116-CD-IEC-RMNC-OPR(SGTS0116-IC-TAB)
                                           <=  ZEROS
                 SET   ERRO-PROGRAMADO
                                       TO  TRUE
                 MOVE  0180            TO  MSG1-CD-ERRO-TS
                 MOVE  'Código do indicador econômico da remuneração inv
      -                'álido.'        TO  MSG1-TX-ERRO-TS
                 SET   COM-ERRO-TAB    TO  TRUE
                 MOVE  'S'             TO  ERRO-CD-IEC
                 PERFORM  999000-ERRO
             END-IF

             IF  SGTS0116-CD-IEC-RMNC-OPR(SGTS0116-IC-TAB)
                                           =  1313
                 MOVE  0003
                   TO  SGTS0116-CD-TIP-FMA-APL(SGTS0116-IC-TAB)
             ELSE
                 IF  SGTS0116-CD-TIP-FMA-APL(SGTS0116-IC-TAB)
                                           <=  ZEROS
                 OR  SGTS0116-CD-TIP-FMA-APL(SGTS0116-IC-TAB)
                                           > 0002
                     SET   ERRO-PROGRAMADO
                                       TO  TRUE
                     MOVE  0176        TO  MSG1-CD-ERRO-TS
                     MOVE  'Código do tipo da forma aplicação da taxa in
      -                    'válido.'   TO  MSG1-TX-ERRO-TS
                     SET   COM-ERRO-TAB
                                       TO  TRUE
                     MOVE  'S'         TO  ERRO-CD-TIP
                     PERFORM  999000-ERRO
                 END-IF
             END-IF

             IF  SGTS0116-CD-PERC-RMNC(SGTS0116-IC-TAB)
                                           <=  ZEROS
                 SET   ERRO-PROGRAMADO
                                       TO  TRUE
                 MOVE  0434            TO  MSG1-CD-ERRO-TS
                 MOVE  'Código da periodicidade da remuneração inválido.
      -                ''              TO  MSG1-TX-ERRO-TS
                 SET   COM-ERRO-TAB    TO  TRUE
                 MOVE  'S'             TO  ERRO-CD-PERC
                 PERFORM  999000-ERRO
             END-IF

           END-PERFORM

      *    Quando Remuneração Escalonada verifica Datas Precificação
           IF   SGTS0116-IN-RMNC-VRV EQUAL 'S'
                PERFORM 113000-VERIFICA-DTAS-PRECIFI
           END-IF

           PERFORM  140000-BUSCA-TS-DT
           .
       110000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       111000-PESQ-OPERACAO-ESTRUT     SECTION.
      *--> Pesquisa se existe uma nota tecnica cadastrada
      *    com o Numero da Nota da entrada.
      *-----------------------------------------------------------------
           MOVE  0003                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      SGTST005-CHAVE.

      *--> Movimentar o nome do programa a ser executado
           MOVE  'SGTST005'            TO  NM-PGM
      *--> Consultar
           MOVE  0004                  TO  SGTST005-ACAO
           MOVE  SGTS0116-CD-CTRA-TIT-VL
                                       TO  SGTST005-CD-CTRA-TIT-VL
           MOVE  SGTS0116-CD-OPR-ETRD-MCP
                                       TO  SGTST005-CD-OPR-ETRD-MCP
           MOVE  SGTS0116-NR-VRS-OPR-ETRD
                                       TO  SGTST005-NR-VRS-OPR-ETRD

           PERFORM  920000-CHAMAR-SGTST005
           .
       111000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       112000-CALCULA-DATA             SECTION.
      *--> Soma um dia a Data Fim da Taxa de Remuneração
      *-----------------------------------------------------------------
           MOVE  0004                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      SBDATAF-AREA

           MOVE CORR SGTS0116-DT-FIM-TAXA-RMNC-R(SGTS0116-IC-TAB)
                                       TO   DT-AUX-DMA-R
           MOVE  'F05'                 TO  FUNCAO
           MOVE  CORR DT-AUX-DMA-R     TO  AUX-DT-08-R
           MOVE  AUX-DT-08             TO  ARG01
           MOVE  0001                  TO  ARG02
           MOVE  0001                  TO  ARG03
           MOVE  4095                  TO  A-TAM
           MOVE  'SBDATAF -WRKAREA-FIM'
                                       TO  A-FIM

           CALL  SBDATAF            USING  FUNCAO
                                           AREAWORK
                                           ARG01
                                           ARG02
                                           ARG03
                                           ARG04

           IF  RETURN-CODE                 NOT EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0051              TO  MSG1-CD-ERRO-TS
               MOVE RETURN-CODE        TO  MSG1-CD-RTN-PGM-ACND
               MOVE 'Erro no tratamento de datas.'
                                       TO  MSG1-TX-RTN-PGM-ACND
                                           MSG1-TX-ERRO-TS
               MOVE SBDATAF            TO  MSG1-NM-PGM-ACND
                                           NM-PGM
      *  Monitorar variáveis do módulo chamado
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM  999000-ERRO
           END-IF

           MOVE  ARG04                 TO  AUX-DT-08
           MOVE  CORR AUX-DT-08-R      TO  DT-AUX-DMA-R
           MOVE  '.'                   TO  PTO1 OF DT-AUX-DMA-R
                                           PTO2 OF DT-AUX-DMA-R
           .
       112000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       113000-VERIFICA-DTAS-PRECIFI    SECTION.
      *  Compara data inicial Remuneração com data inicial Precificação
      *-----------------------------------------------------------------
           MOVE     0005                  TO  MSG1-CD-SEQL-ERRO

           MOVE     SGTS0183              TO  NM-PGM
           MOVE SGTS0116-CD-CTRA-TIT-VL   TO SGTS0183-CD-CTRA-TIT-VL
           MOVE SGTS0116-CD-OPR-ETRD-MCP  TO SGTS0183-CD-OPR-ETRD-MCP
           MOVE SGTS0116-NR-VRS-OPR-ETRD  TO SGTS0183-NR-VRS-OPR-ETRD
           MOVE         1                 TO SGTS0183-CD-TIP-TAXA
           MOVE         1                 TO SGTS0183-CD-ULT-TIP-TAXA
           MOVE        01010001           TO SGTS0183-DT-ULT-INC-VLD

           PERFORM  900000-EXECUTAR-MODULO

           IF SGTS0183-QT-OCR-TAB NOT EQUAL ZEROS
      *    Tem Precificação
             MOVE  0006                  TO  MSG1-CD-SEQL-ERRO

             MOVE  SGTSC063              TO  NM-PGM

             MOVE  SGTS0116-CD-CTRA-TIT-VL   TO SGTSC063-CD-CTRA-TIT-VL
             MOVE  SGTS0116-CD-OPR-ETRD-MCP  TO SGTSC063-CD-OPR-ETRD-MCP
             MOVE  SGTS0116-NR-VRS-OPR-ETRD  TO SGTSC063-NR-VRS-OPR-ETRD
             MOVE          16                TO SGTSC063-CD-TIP-IN

             PERFORM  900000-EXECUTAR-MODULO

             IF  SGTSC063-IN-EXT-TIP  = 'N'
      *      Precificação é Não Escalonada
      *      Data inicial Precificação = Data Emissão
                MOVE  CORR SGTST005-DT-EMS-TIT-OPR-R TO  AUX-DT-08-R
                IF  AUX-DT-08  NOT EQUAL  SGTS0116-DT-INC-TAXA-RMNC-R(1)
                    MOVE  0007            TO  MSG1-CD-SEQL-ERRO
                    SET   ERRO-PROGRAMADO TO  TRUE
                    MOVE  0427            TO  MSG1-CD-ERRO-TS
                    MOVE  'Data início da remuneração divergente da data
      -                   ' de emissão do título.'
                                          TO  MSG1-TX-ERRO-TS
                    PERFORM  969000-MONITORAR-VARIAVEIS
                    PERFORM  999000-ERRO
                END-IF

             ELSE
      *      Precificação é Escalonada
                MOVE  CORR SGTS0183-DT-INC-CND-TAXA-R (1)
                                             TO  DT-INI-PREF-R
                MOVE  CORR SGTS0116-DT-INC-TAXA-RMNC-R(1)
                                             TO  DT-AMD-AUX-R
                IF   DT-AMD-AUX  GREATER   DT-INI-PREF
                     MOVE  0008              TO  MSG1-CD-SEQL-ERRO
                     SET  ERRO-PROGRAMADO    TO  TRUE
                     MOVE  0846              TO  MSG1-CD-ERRO-TS
                     MOVE  'Data início da Remuneração deve ser menor ou
      -                    ' igual ao início da Precificação.'
                                             TO  MSG1-TX-ERRO-TS
                     MOVE     SGTS0183              TO  NM-PGM
                     PERFORM  969000-MONITORAR-VARIAVEIS
                     PERFORM  999000-ERRO
                END-IF
             END-IF
           END-IF
           .
       113000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       140000-BUSCA-TS-DT              SECTION.
      *-----------------------------------------------------------------
           MOVE  0009                  TO  MSG1-CD-SEQL-ERRO

           CALL SBTIMEDT            USING  WK-CD-FUC
                                           WK-AREA-TRB
                                           WK-FMT-TS
                                           WK-TAM-TS
                                           WK-TS-FMTD

           IF  RETURN-CODE                 NOT EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0050              TO  MSG1-CD-ERRO-TS
               MOVE  'Erro ao obter data e hora atual.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-PROCESSAR                SECTION.
      *-----------------------------------------------------------------
           MOVE  0010                  TO  MSG1-CD-SEQL-ERRO

           PERFORM  210000-TRATAR-REMUNERACAO-ANT

           PERFORM  220000-GRAVA-REMUNERACAO-OPR
                    VARYING SGTS0116-IC-TAB
                    FROM  0001 BY 0001
                    UNTIL SGTS0116-IC-TAB GREATER SGTS0116-QT-OCR-TAB

           PERFORM  230000-GRAVAR-OPERACAO-ESTRT

           PERFORM  240000-VERIFICA-DFRC-PREF

           MOVE  'Alteração efetuada com sucesso.'
                                       TO  SGTS0116-TX-MSG-RTN

           PERFORM  980000-GRAVAR-CONTAINER-RPST
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       210000-TRATAR-REMUNERACAO-ANT   SECTION.
      *--> Exclui as Remuneraç es Anteriores
      *-----------------------------------------------------------------
           MOVE  0011                  TO  MSG1-CD-SEQL-ERRO

           MOVE  SGTSL011              TO  NM-PGM

           MOVE  SGTS0116-CD-CTRA-TIT-VL    TO  SGTSL011-CD-CTRA-TIT-VL
           MOVE  SGTS0116-CD-OPR-ETRD-MCP   TO  SGTSL011-CD-OPR-ETRD-MCP
           MOVE  SGTS0116-NR-VRS-OPR-ETRD   TO  SGTSL011-NR-VRS-OPR-ETRD
           MOVE  '01010001'                 TO  SGTSL011-DT-INC-TAXA-ULT

           PERFORM  900000-EXECUTAR-MODULO

           PERFORM  VARYING SGTSL011-IC-TAB
                    FROM  0001 BY 0001
                    UNTIL SGTSL011-IC-TAB GREATER SGTSL011-QT-OCR-TAB
             MOVE  SGTSE011                 TO  NM-PGM
             MOVE  SGTS0116-CD-CTRA-TIT-VL  TO  SGTSE011-CD-CTRA-TIT-VL
             MOVE  SGTS0116-CD-OPR-ETRD-MCP TO  SGTSE011-CD-OPR-ETRD-MCP
             MOVE  SGTS0116-NR-VRS-OPR-ETRD TO  SGTSE011-NR-VRS-OPR-ETRD
             MOVE  '  .  .    '        TO  SGTSE011-DT-INC-TAXA-RMNC
             MOVE  CORR SGTSL011-DT-INC-TAXA-RMNC-R(SGTSL011-IC-TAB)
                                       TO  SGTSE011-DT-INC-TAXA-RMNC-R

             PERFORM  900000-EXECUTAR-MODULO
           END-PERFORM
           .
       210000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       220000-GRAVA-REMUNERACAO-OPR    SECTION.
      *--> Realiza a Inclusão das Novas de Remuneracoes da Operacao
      *-----------------------------------------------------------------
           MOVE  0012                  TO  MSG1-CD-SEQL-ERRO

      *--> Movimentar o nome do programa a ser executado
           MOVE  SGTSI011              TO  NM-PGM

           MOVE  SGTS0116-CD-CTRA-TIT-VL   TO  SGTSI011-CD-CTRA-TIT-VL
           MOVE  SGTS0116-CD-OPR-ETRD-MCP  TO  SGTSI011-CD-OPR-ETRD-MCP
           MOVE  SGTS0116-NR-VRS-OPR-ETRD  TO  SGTSI011-NR-VRS-OPR-ETRD
           MOVE  SGTS0116-CD-IEC-RMNC-OPR(SGTS0116-IC-TAB)
                                           TO  SGTSI011-CD-IEC-RMNC
           MOVE  SGTS0116-CD-TIP-FMA-APL(SGTS0116-IC-TAB)
                                           TO  SGTSI011-CD-TIP-FMA-APL
           MOVE  SGTS0116-CD-PERC-RMNC(SGTS0116-IC-TAB)
                                           TO  SGTSI011-CD-PERC-CPTZ-JUR

           MOVE  '  .  .    '              TO  SGTSI011-DT-INC-TAXA-RMNC
                                               SGTSI011-DT-FIM-TAXA-RMNC

           MOVE  CORR SGTS0116-DT-INC-TAXA-RMNC-R(SGTS0116-IC-TAB)
                                       TO  SGTSI011-DT-INC-TAXA-RMNC-R
           MOVE  CORR SGTS0116-DT-FIM-TAXA-RMNC-R(SGTS0116-IC-TAB)
                                       TO  SGTSI011-DT-FIM-TAXA-RMNC-R
           MOVE  SGTS0116-PC-TAXA-RMNC-OPR(SGTS0116-IC-TAB)
                                       TO  SGTSI011-PC-TAXA-RMNC-OPR
           MOVE  KOPRD-CD-OPRD         TO  SGTSI011-CD-USU-RSP-ATL
           MOVE  WK-TS-FMTD            TO  SGTSI011-TS-ULT-ATL

           PERFORM  900000-EXECUTAR-MODULO
           .
       220000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       230000-GRAVAR-OPERACAO-ESTRT    SECTION.
      *--> Atualiza dados da Operacao Estruturada
      *-----------------------------------------------------------------
           MOVE  0013                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      SGTST005-CHAVE.

      *--> Movimentar o nome do programa a ser executado
           MOVE  'SGTST005'            TO  NM-PGM

      *--> Movimentar os campos de entrada
      *--> Alterar
           MOVE  0002                  TO  SGTST005-ACAO

           MOVE  SGTS0116-CD-CTRA-TIT-VL    TO  SGTST005-CD-CTRA-TIT-VL
           MOVE  SGTS0116-CD-OPR-ETRD-MCP   TO  SGTST005-CD-OPR-ETRD-MCP
           MOVE  SGTS0116-NR-VRS-OPR-ETRD   TO  SGTST005-NR-VRS-OPR-ETRD
           MOVE  KOPRD-CD-OPRD              TO  SGTST005-CD-USU-ULT-ATL
           MOVE  'CURRENT'                  TO  SGTST005-TS-ULT-ATL

           MOVE  SGTS0116-IN-RMNC-VRV  TO  SGTST005-IN-RMNC-VRV

           PERFORM  920000-CHAMAR-SGTST005
           .
       230000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       240000-VERIFICA-DFRC-PREF       SECTION.
      *-----------------------------------------------------------------
           MOVE  0014                  TO  MSG1-CD-SEQL-ERRO

      *--> Movimentar o nome do programa a ser executado
           MOVE  SGTS0185              TO  NM-PGM

           MOVE  SGTS0116-CD-CTRA-TIT-VL    TO  SGTS0185-CD-CTRA-TIT-VL
           MOVE  SGTS0116-CD-OPR-ETRD-MCP   TO  SGTS0185-CD-OPR-ETRD-MCP
           MOVE  SGTS0116-NR-VRS-OPR-ETRD   TO  SGTS0185-NR-VRS-OPR-ETRD
           SET   SGTS0185-LS-PREF           TO  TRUE
           PERFORM  900000-EXECUTAR-MODULO

           MOVE  SGTS0185-QT-OCR-TAB   TO  SGTS0116-QT-OCR-DFRC

           IF  SGTS0185-QT-OCR-TAB         > ZEROS
               PERFORM  241000-CARREGA-DFRC-PREF
                        VARYING IC-TAB
                        FROM 0001 BY 0001
                        UNTIL IC-TAB       > SGTS0185-QT-OCR-TAB
           END-IF
           .
       240000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       241000-CARREGA-DFRC-PREF        SECTION.
      *-----------------------------------------------------------------
           MOVE  0015                  TO  MSG1-CD-SEQL-ERRO

           MOVE  CORR SGTS0185-DT-INC-CND-TAXA-R(IC-TAB)
                                TO  SGTS0116-DT-INC-CND-TAXA-R(IC-TAB)
           MOVE  CORR SGTS0185-DT-FIM-CND-TAXA-R(IC-TAB)
                                TO  SGTS0116-DT-FIM-CND-TAXA-R(IC-TAB)
           MOVE  SGTS0185-TX-DCR-TAXA-OPR(IC-TAB)
                                TO  SGTS0116-TX-DCR-PREF-OPR(IC-TAB)
           MOVE  SGTS0185-TX-DCR-DFRC(IC-TAB)
                                TO  SGTS0116-TX-DCR-DFRC(IC-TAB)
           .
       241000-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *   Section da Faixa de 900000 a 969900 para Demais Rotinas
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       900000-EXECUTAR-MODULO          SECTION.
      *--> Executa um modulo Online implementado com CONTAINER
      *    Nao incluir nessa section os módulos chamados via COMMAREA
      *-----------------------------------------------------------------

      *--> Grava o container de requisição do módulo a executar
           PERFORM  982000-GRAVAR-CONTAINER-RQSC

      *--> Limpa a resposta e erro antes de executar o módulo
           PERFORM  983000-EXCLUIR-RESPOSTA-ERRO

           MOVE  0016                  TO  MSG1-CD-SEQL-ERRO

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EVALUATE  NM-PGM
               WHEN  SGTSE011
                     EXEC CICS LINK
                               PROGRAM ( SGTSE011  )
                               CHANNEL ( NM-CNL    )
                               RESP    ( MSG1-RESP1)
                               RESP2   ( MSG1-RESP2)
                     END-EXEC
               WHEN  SGTSL011
                     EXEC CICS LINK
                               PROGRAM ( SGTSL011  )
                               CHANNEL ( NM-CNL    )
                               RESP    ( MSG1-RESP1)
                               RESP2   ( MSG1-RESP2)
                     END-EXEC
               WHEN  SGTSI011
                     EXEC CICS LINK
                               PROGRAM ( SGTSI011  )
                               CHANNEL ( NM-CNL    )
                               RESP    ( MSG1-RESP1)
                               RESP2   ( MSG1-RESP2)
                     END-EXEC
               WHEN  SGTS0185
                     EXEC CICS LINK
                               PROGRAM ( SGTS0185  )
                               CHANNEL ( NM-CNL    )
                               RESP    ( MSG1-RESP1)
                               RESP2   ( MSG1-RESP2)
                     END-EXEC
               WHEN  SGTSC063
                     EXEC CICS LINK
                               PROGRAM ( SGTSC063  )
                               CHANNEL ( NM-CNL    )
                               RESP    ( MSG1-RESP1)
                               RESP2   ( MSG1-RESP2)
                     END-EXEC
               WHEN  SGTS0183
                     EXEC CICS LINK
                               PROGRAM ( SGTS0183  )
                               CHANNEL ( NM-CNL    )
                               RESP    ( MSG1-RESP1)
                               RESP2   ( MSG1-RESP2)
                     END-EXEC
               WHEN  OTHER
                     SET   ERRO-PROGRAMADO
                                       TO  TRUE
                     MOVE  9006        TO  MSG1-CD-ERRO-TS
                     STRING  'Não definida a execução do módulo '
                             NM-PGM '.'
                             DELIMITED BY SIZE
                             INTO          MSG1-TX-ERRO-TS
                     END-STRING
                     PERFORM  999000-ERRO
           END-EVALUATE

           EVALUATE  TRUE
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(NORMAL)
                    CONTINUE
               WHEN  OTHER
                     MOVE  3004        TO  MSG1-CD-ERRO-TS
                     STRING  'Erro na execução do módulo ' NM-PGM ', '
                             'RESP  = '    MSG1-RESP1  ', '
                             'RESP2 = '    MSG1-RESP2
                             DELIMITED BY SIZE
                             INTO          MSG1-TX-ERRO-TS
                             SET   ERRO-CICS
                                       TO  TRUE
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
       920000-CHAMAR-SGTST005          SECTION.
      *-----------------------------------------------------------------
           MOVE  0017                  TO  MSG1-CD-SEQL-ERRO

           MOVE  SGTST005              TO  NM-PGM

           EXEC CICS LINK
                     PROGRAM    ( SGTST005                 )
                     COMMAREA   ( SGTST005-DADOS           )
                     LENGTH     ( LENGTH OF SGTST005-DADOS )
           END-EXEC

           IF  SGTST005-CD-RTN-PGM         NOT EQUAL  ZEROS  AND 0100
               SET   ERRO-PROGRAMA-ACIONADO TO  TRUE
               MOVE  NM-PGM                 TO  MSG1-NM-PGM-ACND
               MOVE  SGTST005-CD-RTN-PGM    TO  MSG1-CD-ERRO-TS
                                                MSG1-CD-RTN-PGM-ACND
               MOVE  SGTST005-MSG-ERRO      TO  MSG1-TX-RTN-PGM-ACND
                                                MSG1-TX-ERRO-TS
               MOVE  SGTST005-PSC-ERRO-PGM  TO  MSG1-CD-SEQL-PGM-ACND
               PERFORM  969000-MONITORAR-VARIAVEIS
               PERFORM  999000-ERRO
           END-IF
           .
       920000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       969000-MONITORAR-VARIAVEIS      SECTION.
      *--> Monitora as variaveis dos modulos chamados
      *-----------------------------------------------------------------

           EVALUATE  NM-PGM
               WHEN  SGTST005
                     MOVE 'SGTST005-ACAO'       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST005-ACAO        TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTST005-CD-CTRA-TIT-VL'
                                                TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST005-CD-CTRA-TIT-VL
                                                TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTST005-CD-OPR-ETRD-MCP'
                                                TO  MSGKW999-NM-VRV-MNT
                     IF  SGTST005-CD-OPR-ETRD-MCP   IS NUMERIC
                         MOVE  SGTST005-CD-OPR-ETRD-MCP
                                                TO  CD-OPR-ETRD-MCP
                         MOVE  CD-OPR-ETRD-MCP-TX
                                                TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE  'Valor não numérico.'
                                                TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTST005-NR-VRS-OPR-ETRD'
                                                TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST005-NR-VRS-OPR-ETRD
                                                TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     IF  SGTST005-ACAO      EQUAL  0002
                         PERFORM  969100-MONITORA-ALTERAR
                     END-IF

               WHEN  SGTSE011
                     MOVE 'SGTSE011-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSE011-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSE011-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                     IF  SGTSE011-CD-OPR-ETRD-MCP
                                           IS NUMERIC
                         MOVE  SGTSE011-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP
                         MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE  'Valor não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSE011-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSE011-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSE011-DT-INC-TAXA-RMNC'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSE011-DT-INC-TAXA-RMNC
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  SGTSL011
                     MOVE 'SGTSL011-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL011-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSL011-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                     IF  SGTSL011-CD-OPR-ETRD-MCP
                                           IS NUMERIC
                         MOVE  SGTSL011-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP
                         MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE  'Valor não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSL011-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL011-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSL011-DT-INC-TAXA-ULT'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL011-DT-INC-TAXA-ULT
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  SGTSI011
                     MOVE 'SGTSI011-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI011-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSI011-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                     IF  SGTSI011-CD-OPR-ETRD-MCP
                                           IS NUMERIC
                         MOVE  SGTSI011-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP
                         MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE  'Valor não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSI011-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI011-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSI011-DT-INC-TAXA-RMNC'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI011-DT-INC-TAXA-RMNC
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSI011-DT-FIM-TAXA-RMNC'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI011-DT-FIM-TAXA-RMNC
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSI011-PC-TAXA-RMNC-OPR'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI011-PC-TAXA-RMNC-OPR
                                       TO  VL-05V10
                     MOVE  VL-05V10    TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSI011-CD-USU-RSP-ATL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI011-CD-USU-RSP-ATL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSI011-TS-ULT-ATL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI011-TS-ULT-ATL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSI011-CD-TIP-FMA-APL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI011-CD-TIP-FMA-APL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSI011-CD-IEC-RMNC'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI011-CD-IEC-RMNC
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSI011-CD-PERC-CPTZ-JUR'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI011-CD-PERC-CPTZ-JUR
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  SGTS0185
                     MOVE 'SGTS0185-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0185-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTS0185-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                     IF  SGTS0185-CD-OPR-ETRD-MCP
                                           IS NUMERIC
                         MOVE  SGTS0185-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP
                         MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE  'Valor não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTS0185-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0185-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTS0185-CD-LS-RTN'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0185-CD-LS-RTN
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  SGTSC063
                     MOVE 'SGTS0183-QT-OCR-TAB'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0183-QT-OCR-TAB
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT
                     MOVE 'SGTSC063-IN-EXT-TIP'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSC063-IN-EXT-TIP
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT
                     MOVE 'SGTST005-DT-EMS-TIT-OPR-R'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST005-DT-EMS-TIT-OPR
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT
                     MOVE 'SGTS0116-DT-INC-TAXA-RMNC-R(1)'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0116-DT-INC-TAXA-RMNC-R(1)
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT
               WHEN  SGTS0183
                     MOVE 'SGTS0183-QT-OCR-TAB'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0183-QT-OCR-TAB
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT
                     MOVE 'SGTSC063-IN-EXT-TIP'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSC063-IN-EXT-TIP
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT
                     MOVE 'Início Precificação'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0183-DT-INC-CND-TAXA-R (1)
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT
                     MOVE 'SGTS0116-DT-INC-TAXA-RMNC-R(1)'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0116-DT-INC-TAXA-RMNC-R(1)
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  SBDATAF
                     MOVE 'RETURN-CODE'    TO  MSGKW999-NM-VRV-MNT
                     MOVE  RETURN-CODE     TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'FUNCAO'     TO  MSGKW999-NM-VRV-MNT
                     MOVE  FUNCAO      TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'A-TAM'      TO  MSGKW999-NM-VRV-MNT
                     MOVE  A-TAM       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'ARG01'      TO  MSGKW999-NM-VRV-MNT
                     MOVE  ARG01       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'ARG02'      TO  MSGKW999-NM-VRV-MNT
                     MOVE  ARG02       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'ARG03'      TO  MSGKW999-NM-VRV-MNT
                     MOVE  ARG03       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  OTHER
                     CONTINUE
           END-EVALUATE
           .
       969000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       969100-MONITORA-ALTERAR         SECTION.
      *-----------------------------------------------------------------

           MOVE 'SGTST005-CD-USU-ULT-ATL'       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-CD-USU-ULT-ATL        TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-TS-ULT-ATL'           TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-TS-ULT-ATL            TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-CD-BCO-CRD-VL-OPR'    TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-CD-BCO-CRD-VL-OPR     TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-CD-DEPE-CRD-VL-OPR'   TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-CD-DEPE-CRD-VL-OPR    TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-CD-SUB-DEPE-CRD-VL'   TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-CD-SUB-DEPE-CRD-VL    TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-CD-UOR-DTTR-OPR'      TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-CD-UOR-DTTR-OPR       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-CD-PRD'               TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-CD-PRD                TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-CD-MDLD'              TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-CD-MDLD               TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SSGTST005-CD-UOR-BB-CRD-VL'    TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-CD-UOR-BB-CRD-VL      TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-CD-UOR-BB'            TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-CD-UOR-BB             TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-NR-CC-BB'             TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-NR-CC-BB              TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-IN-RMNC-VRV'          TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-IN-RMNC-VRV           TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-NR-CC-OTR-BCO'        TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-NR-CC-OTR-BCO         TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-DV-CC-OTR-BCO'        TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-DV-CC-OTR-BCO         TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-QT-PAPL-TIT-EMS'      TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-QT-PAPL-TIT-EMS       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-QT-PAPL-TIT-CPR'      TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-QT-PAPL-TIT-CPR       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-DT-CPR-OPR'           TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-DT-CPR-OPR            TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-DT-VNCT-OPR'          TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-DT-VNCT-OPR           TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-DT-LQDC-OPR-DRIV'     TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-DT-LQDC-OPR-DRIV      TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-CD-TIT-OPR-ETRD'      TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-CD-TIT-OPR-ETRD       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-NR-DVS-TIT'           TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-NR-DVS-TIT            TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-NR-UNCO-CTR-OPR'      TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-NR-UNCO-CTR-OPR       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-CD-TIP-OPR-ETRD'      TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-CD-TIP-OPR-ETRD       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-IN-OPR-ETRD-SPRO'     TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-IN-OPR-ETRD-SPRO      TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-IN-OPR-ETRD-VRJ'      TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-IN-OPR-ETRD-VRJ       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-IN-OPR-ETRD-DSCP'     TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-IN-OPR-ETRD-DSCP      TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-IN-REG-OPR-BB'        TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-IN-REG-OPR-BB         TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-IN-OPR-N-NEGO'        TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-IN-OPR-N-NEGO         TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-IN-EXPO-VCLD-GRT'     TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-IN-EXPO-VCLD-GRT      TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-CD-ITC-CVM-OPR'       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-CD-ITC-CVM-OPR        TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-CD-CTGR-TVM'          TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-CD-CTGR-TVM           TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-CD-ESPC-DBTR-OPR'     TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-CD-ESPC-DBTR-OPR      TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-CD-TIP-COTA-TIT'      TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-CD-TIP-COTA-TIT       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-NR-VRS-TIP-COTA'      TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-NR-VRS-TIP-COTA       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-CD-TIP-MC-CPR'        TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-CD-TIP-MC-CPR         TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-CD-RSCO-OPR'          TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-CD-RSCO-OPR           TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-CD-CMRA-OPR-ETRD'     TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-CD-CMRA-OPR-ETRD      TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-CD-TIP-VRS-OPR'       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-CD-TIP-VRS-OPR        TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-NR-EMS-TIT-OPR'       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-NR-EMS-TIT-OPR        TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-NR-SRE-EMS-TIT-OPR'   TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-NR-SRE-EMS-TIT-OPR    TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-DT-INC-VGC-VRS-OPR'   TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-DT-INC-VGC-VRS-OPR    TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-DT-MNDT-OPR'          TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-DT-MNDT-OPR           TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-DT-EMS-TIT-OPR'       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-DT-EMS-TIT-OPR        TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-DT-EMS-PREF'          TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-DT-EMS-PREF           TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-DT-VNCT-OPR'          TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-DT-VNCT-OPR           TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-QT-DD-PZ-VLD-PREF'    TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-QT-DD-PZ-VLD-PREF     TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-IN-RMNC-VRV'          TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-IN-RMNC-VRV           TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-QT-MM-CARE-JUR'       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-QT-MM-CARE-JUR        TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-QT-MM-CARE-OPR'       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-QT-MM-CARE-OPR        TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-QT-MM-CARE-RSGT'      TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-QT-MM-CARE-RSGT       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-QT-MM-CARE-AMTZ'      TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-QT-MM-CARE-AMTZ       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-QT-MM-CARE-AQS'       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-QT-MM-CARE-AQS        TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-PC-TARF-GRT-TAXA'     TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-PC-TARF-GRT-TAXA      TO  VL-05V10
           MOVE  VL-05V10                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-VL-PRV-ENCT'          TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-VL-PRV-ENCT           TO  VL-15V02
           MOVE  VL-15V02                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-VL-CPR-OPR'           TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-VL-CPR-OPR            TO  VL-15V02
           MOVE  VL-15V02                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-VL-MNDT-OPR'          TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-VL-MNDT-OPR           TO  VL-15V02
           MOVE  VL-15V02                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-VL-PU-EMS-TIT-OPR'    TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-VL-PU-EMS-TIT-OPR     TO  VL-09V08
           MOVE  VL-09V08                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-VL-APVD-BB-OPR'       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-VL-APVD-BB-OPR        TO  VL-15V02
           MOVE  VL-15V02                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-VL-PU-CPR-TIT-OPR'    TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-VL-PU-CPR-TIT-OPR     TO  VL-09V08
           MOVE  VL-09V08                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-VL-GRT-BB-OPR'        TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-VL-GRT-BB-OPR         TO  VL-15V02
           MOVE  VL-15V02                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-TX-PGTO-OPR-VCLD'     TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-TX-PGTO-OPR-VCLD      TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-DT-CMPS-FUT-NOTA'     TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-DT-CMPS-FUT-NOTA      TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-NR-VRS-REF'           TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-NR-VRS-REF            TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-VL-EVT-VCLD-VRS'      TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-VL-EVT-VCLD-VRS       TO  VL-15V02
           MOVE  VL-15V02                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-DT-EVT-VCLD-VRS'      TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-DT-EVT-VCLD-VRS       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTST005-TX-OBS-EVT'           TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST005-TX-OBS-EVT            TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT
           .
       969100-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *    Section da Faixa de 970000 a 979900 para Leitura de Dados
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       970000-LER-CANAL                SECTION.
      *--> Obtem o canal corrente
      *-----------------------------------------------------------------
           MOVE  0018                  TO  MSG1-CD-SEQL-ERRO

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CANAL            TO  TRUE

           EXEC CICS ASSIGN
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

      *--> Monta o nome do container de requisição
           MOVE  'SGTS0116'            TO  NM-PGM
           MOVE  'SGTS0116-RQSC'       TO  NM-CTNR

      *--> Recupera o tamanho do container de requisição
           PERFORM  973100-LER-CONTAINER-NODATA

           MOVE  0019                  TO  MSG1-CD-SEQL-ERRO

      *--> Configura dados para leitura do container de requisicao
           SET   GET-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *--> Efetua a leitura do container
           EXEC CICS GET
                     CONTAINER ( NM-CTNR       )
                     FLENGTH   ( QT-CCT-LIDO   )
                     INTO      ( SGTS0116-RQSC )
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
           MOVE  0020                  TO  MSG1-CD-SEQL-ERRO

      *--> Le o container de erro dos módulos chamados
           SET   GET-CONTAINER         TO  TRUE
           MOVE  'PRM-ERRO'            TO  NM-CTNR
           INITIALIZE                      IIBKERRO-PRM-ERRO

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS GET
                     CONTAINER ( NM-CTNR                     )
                     INTO      ( IIBKERRO-PRM-ERRO           )
                     FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                     RESP      ( MSG1-RESP1                  )
                     RESP2     ( MSG1-RESP2                  )
           END-EXEC

           EVALUATE  TRUE
      *        Caso deseje personalizar ou não interromper o
      *        processamento, alterar para não dar erro aqui SOMENTE
      *        PARA O MÓDULO CHAMADO e tratar na section chamadora
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(CONTAINERERR)
                     CONTINUE
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(NORMAL)
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
                     PERFORM  999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       972000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       973000-LER-RESPOSTA             SECTION.
      *-----------------------------------------------------------------

      *--> Monta o nome dos container de acordo com módulo a executar
           MOVE  SPACES                TO  NM-CTNR

           MOVE  NM-PGM                TO  NM-CTNR(1:8)
           MOVE  '-RPST'               TO  NM-CTNR(9:5)

      *--> Recupera o tamanho do container de resposta
           PERFORM  973100-LER-CONTAINER-NODATA

           MOVE  0021                  TO  MSG1-CD-SEQL-ERRO

           EVALUATE  NM-PGM
               WHEN  SGTSE011
                     MOVE  LENGTH OF SGTSE011-RPST  TO  QT-CCT-EPRO
               WHEN  SGTSI011
                     MOVE  LENGTH OF SGTSI011-RPST  TO  QT-CCT-EPRO
               WHEN  OTHER
                     CONTINUE
           END-EVALUATE

           IF  NM-PGM    EQUAL  SGTSE011 OR  SGTSI011
               IF  QT-CCT-LIDO       NOT EQUAL QT-CCT-EPRO
                   SET   ERRO-TAMANHO-AREA     TO  TRUE
                   PERFORM  969000-MONITORAR-VARIAVEIS
                   PERFORM  999000-ERRO
               END-IF
           END-IF

      *--> Configura dados para leitura do container de resposta
           SET   GET-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *--> Le a RESPOSTA de acordo com o programa que a gravou

           MOVE  0022                  TO  MSG1-CD-SEQL-ERRO

           EVALUATE  NM-PGM
      *        Calcular a quantidade de caracteres esperado e limpar
      *        a area de dados antes de executar o GET
      *        Para matrizes com dados variaveis (clausula depending
      *        on) nao ha como utilizar o comando INITIALIZE
               WHEN  SGTSE011
                     EXEC CICS GET
                               CONTAINER ( NM-CTNR          )
                               INTO      ( SGTSE011-RPST    )
                               FLENGTH   ( QT-CCT-LIDO      )
                     END-EXEC

               WHEN  SGTSL011
                     EXEC CICS GET
                               CONTAINER ( NM-CTNR          )
                               INTO      ( SGTSL011-RPST    )
                               FLENGTH   ( QT-CCT-LIDO      )
                     END-EXEC

               WHEN  SGTSI011
                     EXEC CICS GET
                               CONTAINER ( NM-CTNR          )
                               INTO      ( SGTSI011-RPST    )
                               FLENGTH   ( QT-CCT-LIDO      )
                     END-EXEC

               WHEN  SGTS0185
                     EXEC CICS GET
                               CONTAINER ( NM-CTNR          )
                               INTO      ( SGTS0185-RPST    )
                               FLENGTH   ( QT-CCT-LIDO      )
                     END-EXEC

               WHEN  SGTSC063
                     EXEC CICS GET
                               CONTAINER ( NM-CTNR          )
                               INTO      ( SGTSC063-RPST    )
                               FLENGTH   ( QT-CCT-LIDO      )
                     END-EXEC

               WHEN  SGTS0183
                     EXEC CICS GET
                               CONTAINER ( NM-CTNR          )
                               INTO      ( SGTS0183-RPST    )
                               FLENGTH   ( QT-CCT-LIDO      )
                     END-EXEC

               WHEN  OTHER
                     SET   ERRO-PROGRAMADO TO  TRUE
                     MOVE  9003            TO  MSG1-CD-ERRO-TS
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
           MOVE  0023                  TO  MSG1-CD-SEQL-ERRO

      *--> Configura dados para leitura do container de requisição
           SET   GET-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *--> Obtem a quantidade de caracteres no container
           MOVE  ZEROS                 TO  QT-CCT-LIDO

           EXEC CICS GET
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
       974000-LER-DADOS-OPRD           SECTION.
      *-----------------------------------------------------------------
           MOVE  0024                  TO  MSG1-CD-SEQL-ERRO

      *--> Configura dados para leitura do container de requisio
           SET   GET-CONTAINER         TO  TRUE
           MOVE  'PRM-INF-OPRD'        TO  NM-CTNR

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

           INITIALIZE                      PRM-INF-OPRD
           EXEC CICS GET
                CONTAINER ( NM-CTNR                )
                INTO      ( PRM-INF-OPRD           )
                FLENGTH   ( LENGTH OF PRM-INF-OPRD )
           END-EXEC.

      *--> Ao chegar aqui, h certeza de que no houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       974000-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *   Section da Faixa de 980000 a 989900 para Gravação de Dados
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       980000-GRAVAR-CONTAINER-RPST    SECTION.
      *-----------------------------------------------------------------

      *--> Exclui eventual container de erro de outros programas
           PERFORM  983100-EXCLUIR-CONTAINER-ERRO

           MOVE  0025                  TO  MSG1-CD-SEQL-ERRO

      *--> Monta o nome do container de resposta
           MOVE  'SGTS0116-RPST'       TO  NM-CTNR
           SET   PUT-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

           EXEC CICS PUT
                     CONTAINER ( NM-CTNR                 )
                     FROM      ( SGTS0116-RPST           )
                     FLENGTH   ( LENGTH OF SGTS0116-RPST )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       980000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       982000-GRAVAR-CONTAINER-RQSC    SECTION.
      *-----------------------------------------------------------------
           MOVE  0026                  TO  MSG1-CD-SEQL-ERRO

           MOVE  SPACES                TO  NM-CTNR
           SET   PUT-CONTAINER         TO  TRUE

           MOVE  NM-PGM                TO  NM-CTNR(1:8)
           MOVE  '-RQSC'               TO  NM-CTNR(9:5)

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *--> Faz PUT da requisição do programa a executar
           EVALUATE  NM-PGM
               WHEN  SGTSI011
                     EXEC CICS PUT
                               CONTAINER ( NM-CTNR                 )
                               FROM      ( SGTSI011-RQSC           )
                               FLENGTH   ( LENGTH OF SGTSI011-RQSC )
                     END-EXEC
               WHEN  SGTSE011
                     EXEC CICS PUT
                               CONTAINER ( NM-CTNR                 )
                               FROM      ( SGTSE011-RQSC           )
                               FLENGTH   ( LENGTH OF SGTSE011-RQSC )
                     END-EXEC
               WHEN  SGTSL011
                     EXEC CICS PUT
                               CONTAINER ( NM-CTNR                 )
                               FROM      ( SGTSL011-RQSC           )
                               FLENGTH   ( LENGTH OF SGTSL011-RQSC )
                     END-EXEC
               WHEN  SGTS0185
                     EXEC CICS PUT
                               CONTAINER ( NM-CTNR                 )
                               FROM      ( SGTS0185-RQSC           )
                               FLENGTH   ( LENGTH OF SGTS0185-RQSC )
                     END-EXEC
               WHEN  SGTSC063
                     EXEC CICS PUT
                               CONTAINER ( NM-CTNR                 )
                               FROM      ( SGTSC063-RQSC           )
                               FLENGTH   ( LENGTH OF SGTSC063-RQSC )
                     END-EXEC
               WHEN  SGTS0183
                     EXEC CICS PUT
                               CONTAINER ( NM-CTNR                 )
                               FROM      ( SGTS0183-RQSC           )
                               FLENGTH   ( LENGTH OF SGTS0183-RQSC )
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
      *    problemas em módulos executados mais de uma vez dentro
      *    da mesma sessão do CICS.
      *    Equivale ao initialize na área de resposta dos
      *    programas que utilizam COMMAREA.
      *-----------------------------------------------------------------
           MOVE  0027                  TO  MSG1-CD-SEQL-ERRO

           MOVE  NM-PGM                TO  NM-CTNR(1:8)
           MOVE  '-RPST'               TO  NM-CTNR(9:5)

           SET   DEL-CONTAINER         TO  TRUE

      *--> Exclui o container de resposta
      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS DELETE
                     CONTAINER ( NM-CTNR    )
                     RESP      ( MSG1-RESP1 )
                     RESP2     ( MSG1-RESP2 )
           END-EXEC

           EVALUATE  TRUE
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(CONTAINERERR)
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(NORMAL)
                     CONTINUE
               WHEN  OTHER
                     SET   ERRO-CONTAINER  TO  TRUE
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
           MOVE  0028                  TO  MSG1-CD-SEQL-ERRO

      *--> Limpar variáveis container de erro
           INITIALIZE                      IIBKERRO-PRM-ERRO

      *--> Exclui o container de erro
           MOVE  'PRM-ERRO'            TO  NM-CTNR
           SET   DEL-CONTAINER         TO  TRUE

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS DELETE
                     CONTAINER ( NM-CTNR    )
                     RESP      ( MSG1-RESP1 )
                     RESP2     ( MSG1-RESP2 )
           END-EXEC

           EVALUATE  TRUE
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(CONTAINERERR)
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(NORMAL)
                     CONTINUE
               WHEN  OTHER
                     SET   ERRO-CONTAINER  TO  TRUE
                     PERFORM  999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       983100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       984000-GRAVAR-CONTAINER-ERRO    SECTION.
      *-----------------------------------------------------------------

      *--> Grava o erro gerado no container de erro
      * Não há captura automática de erro CICS por usar RESP e RESP2
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
      *    tratamentos de erro através do Sistema MSG (rotina MSGS0001)
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       999000-ERRO                     SECTION.
      *-----------------------------------------------------------------

           IF  MSG1-CD-ERRO-TS             EQUAL  ZEROS
               MOVE  8888              TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS             EQUAL  SPACES
               MOVE 'Erro não especificado no módulo.'
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

      *--> Adiciona as variáveis de entrada para monitoração
           MOVE 'SGTS0116-CD-CTRA-TIT-VL'       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0116-CD-CTRA-TIT-VL        TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0116-CD-OPR-ETRD-MCP'      TO  MSGKW999-NM-VRV-MNT
           IF  SGTS0116-CD-OPR-ETRD-MCP    IS NUMERIC
               MOVE  SGTS0116-CD-OPR-ETRD-MCP   TO  CD-OPR-ETRD-MCP
               MOVE  CD-OPR-ETRD-MCP-TX         TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico.'      TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0116-NR-VRS-OPR-ETRD'      TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0116-NR-VRS-OPR-ETRD       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0116-IN-RMNC-VRV'          TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0116-IN-RMNC-VRV           TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0116-QT-MM-CARE-JUR'       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0116-QT-MM-CARE-JUR        TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0116-QT-OCR-TAB'           TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0116-QT-OCR-TAB            TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

      *--> Verifica se o erro foi em registro de Tabela
           IF  COM-ERRO-TAB
               PERFORM  999100-ADICIONA-OCR-TAB
           END-IF

      *--> Grava o erro gerado no sistema MSG - Mensageria
           PERFORM  MSG1-NTF-MSG

           MOVE  MSG1-CD-ERRO-TS       TO  IIBKERRO-CD-ERRO
           MOVE  MSG1-TX-ERRO-TS       TO  IIBKERRO-TX-ERRO

      *--> Grava o erro gerado no container de erro
           PERFORM  984000-GRAVAR-CONTAINER-ERRO

      *--> Encerra a transação
           PERFORM  999999-ENCERRAR
           .
       999000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999100-ADICIONA-OCR-TAB         SECTION.
      *--> Adiciona Ocorrência da Tabela para Monitoração
      *-----------------------------------------------------------------

           MOVE 'SGTS0116-IC-TAB'      TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0116-IC-TAB       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           IF  ERRO-DT-INC                 EQUAL  'S'
               MOVE 'SGTS0116-DT-INC-TAXA-RMNC'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTS0116-DT-INC-TAXA-RMNC(SGTS0116-IC-TAB)

                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT
           END-IF

           IF  ERRO-PC-TAXA                EQUAL  'S'
               MOVE 'SGTS0116-PC-TAXA-RMNC-OPR'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTS0116-PC-TAXA-RMNC-OPR(SGTS0116-IC-TAB)
                                       TO  VL-05V10
               MOVE  VL-05V10          TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT
           END-IF

           IF  ERRO-DT-FIM                 EQUAL  'S'
               MOVE 'SGTS0116-DT-FIM-TAXA-RMNC'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTS0116-DT-FIM-TAXA-RMNC(SGTS0116-IC-TAB)
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT
           END-IF

           IF  ERRO-CD-IEC                 EQUAL  'S'
               MOVE 'SGTS0116-CD-IEC-RMNC-OPR'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTS0116-CD-IEC-RMNC-OPR(SGTS0116-IC-TAB)
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT
           END-IF

           IF  ERRO-CD-TIP                 EQUAL  'S'
               MOVE 'SGTS0116-CD-TIP-FMA-APL'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTS0116-CD-TIP-FMA-APL(SGTS0116-IC-TAB)
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT
           END-IF

           IF  ERRO-CD-PERC                EQUAL  'S'
               MOVE 'SGTS0116-CD-PERC-RMNC'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTS0116-CD-PERC-RMNC(SGTS0116-IC-TAB)
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT
           END-IF
           .
       999100-SAI.
           EXIT.

      *----------------------------------------
      * 999001-TRATA-ABEND-CICS        SECTION.
      * 999002-TRATA-CONDITION-CICS    SECTION.
      * 999003-ADICIONA-VRV-MNT        SECTION.
      * 999004-ALINHA-NUMERO-ESQUERDA  SECTION.
      * ---------------------------------------
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

           GOBACK
           .
       999999-SAI.
           EXIT.

      *---------------------- FIM SGTS0116 -----------------------------
