       CBL ARITH(EXTEND)
      *-----------------------------------------------------------------
      * PROGRAMA..: SGTS0224
      * ANALISTA..: F2804421 - Eliana Cristina Ramos da Silva
      * AUTOR.....: F2804421 - Eliana Cristina Ramos da Silva
      * DATA......: 25/09/2018
      * OBJETIVO..: Negócio  - Verificar regra de exceção da operação.
      * COMPILACAO: COBOL/CICS
      *-----------------------------------------------------------------
      * VRS003 15.08.2019 - F2804421 2016/145195 - Retirar duplicidade
      *                   de código de atributos realizados.
      * VRS002 10.04.2019 - F2804421 2016/145195 - Repassar o campo
      *             SGTS0163-CD-RGR-SCDR para SGTS0224-CD-RGR-SCDR-RLZD.
      *              SGTS0224-NR-VRS-RGR-SCDR e SGTS0224-NR-SEQL-OGNL.
      *        21/02/2019 F2804421 DE 2016/145195 -Acerto mensageria
      *         para os códigos 9003 e 9004.
      * VRS001 20/09/2018 F2804421  DE 2016/145195 - Implantação
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTS0224 .
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
                                            '*** SGTS0224 ***'.
       77  CTE-VERS                    PIC  X(06)  VALUE 'VRS003'.

      * Módulos acionados
       77  SGTS0163                    PIC  X(08) VALUE 'SGTS0163'.
       77  SBTIMEDT                    PIC  X(08) VALUE 'SBTIMEDT'.

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------
      * Areas de trabalho
      * Indexadores utilizados para as tabelas auxiliares.
       01 IND                          PIC S9(04) COMP-5 VALUE 0.
       01 IND-163                      PIC S9(04) COMP-5 VALUE 0.
       01 IND-ITEM                     PIC S9(04) COMP-5 VALUE 0.
       01 IND-RLZD                     PIC S9(04) COMP-5 VALUE 0.
       01 IND-ATB                      PIC S9(04) COMP-5 VALUE 0.

      * Area auxiliar para gravação do atributo.
       01 AX-ACHOU                     PIC  X(01) VALUE 'N'.

      * Campo de controle para calculo da indentação das instruções.
       01 IDX                          PIC S9(04) COMP-5 VALUE 0.
       01 GR-ANT                       PIC S9(04) COMP-5 VALUE 0.
       01 IC-ANT                       PIC S9(04) COMP-5 VALUE 0.
       01 IC-TAB                       PIC S9(04) COMP-5 VALUE 0.
       01 CD-TIP-CMD-ANT               PIC S9(04) COMP-5 VALUE 0.

      * Campo de quebra por regra principal recebido do SGTS0163.
       01 CD-RGR-PPL-ANT               PIC S9(09) COMP-5.

      * Tabelas auxiliares para regras/instruções e realizados na
      * para as regras de exceção.
       01 TABS-AUX.
          03 TB1-AUX-ITC   OCCURS 500 TIMES.
             05 TB1-CD-RGR-VLDC-ITC    PIC S9(09) COMP-5.
             05 TB1-CD-RGR-SCDR        PIC S9(09) COMP-5.
             05 TB1-NR-VRS-RGR-SCDR    PIC S9(09) COMP-5.
             05 TB1-QT-INDENTACAO      PIC S9(04) COMP-5.
             05 TB1-IN-LNH-EXCO        PIC X(1).
             05 TB1-NR-SEQL-ITC        PIC S9(04) COMP-5.
             05 TB1-NR-SEQL-OGNL       PIC S9(04) COMP-5.
             05 TB1-TIP-CMD-VLDC       PIC S9(04) COMP-5.
             05 TB1-NR-GR-CND          PIC S9(04) COMP-5.
             05 TB1-CD-ATB-VLDC        PIC S9(04) COMP-5.
             05 TB1-CD-TIP-CFT-LGC     PIC S9(04) COMP-5.
             05 TB1-CD-TIP-CFT-ITEM    PIC S9(04) COMP-5.
             05 TB1-VL-ITC-RGR         PIC S9(15)V9(4) COMP-3.
             05 TB1-NR-ITC-RGR         PIC S9(09) COMP-5.
             05 TB1-TX-ITC-RGR         PIC X(20).
      *
          03 TB2-AUX-RLZD    OCCURS 50 TIMES.
             05 TB2-CD-RGR-VLDC-RLZD   PIC S9(09) COMP-5.
             05 TB2-CD-ATB-RLZD        PIC S9(04) COMP-5.
             05 TB2-VL-ITC-RLZD        PIC S9(15)V9(4) COMP-3.
             05 TB2-NR-ITC-RLZD        PIC S9(09) COMP-5.
             05 TB2-TX-ITC-RLZD        PIC X(20).
      *
      * Tabelas de saída semelhante ao book de saída;
      *
       01 TBS-SGTS0224.
          03 TBS-QT-REG-RGR         PIC S9(04) COMP-5 VALUE 0.
          03 TBS-QT-REG-ITEM        PIC S9(04) COMP-5 VALUE 0.
          03 TBS-QT-REG-RLZD        PIC S9(04) COMP-5 VALUE 0.

          03 TBS-PPL     OCCURS 100 TIMES.
             05 TBS-CD-RGR-VLDC       PIC S9(09) COMP-5.
             05 TBS-NR-VRS-RGR-VLDC   PIC S9(09) COMP-5.

          03 TBS-ITEM    OCCURS 500 TIMES.
             05 TBS-CD-RGR-VLDC-ITC    PIC S9(09) COMP-5.
             05 TBS-CD-RGR-SCDR        PIC S9(09) COMP-5.
             05 TBS-NR-VRS-RGR-SCDR    PIC S9(09) COMP-5.
             05 TBS-QT-INDENTACAO      PIC S9(04) COMP-5.
             05 TBS-IN-LNH-EXCO        PIC X(1).
             05 TBS-NR-SEQL-ITC        PIC S9(04) COMP-5.
             05 TBS-NR-SEQL-OGNL       PIC S9(04) COMP-5.
             05 TBS-TIP-CMD-VLDC       PIC S9(04) COMP-5.
             05 TBS-NR-GR-CND          PIC S9(04) COMP-5.
             05 TBS-CD-ATB-VLDC        PIC S9(04) COMP-5.
             05 TBS-CD-TIP-CFT-LGC     PIC S9(04) COMP-5.
             05 TBS-CD-TIP-CFT-ITEM    PIC S9(04) COMP-5.
             05 TBS-VL-ITC-RGR         PIC S9(15)V9(4) COMP-3.
             05 TBS-NR-ITC-RGR         PIC S9(09) COMP-5.
             05 TBS-TX-ITC-RGR         PIC X(20).
      *
          03 TBS-RLZD    OCCURS 50 TIMES.
             05 TBS-CD-RGR-VLDC-RLZD   PIC S9(09) COMP-5.
             05 TBS-CD-ATB-RLZD        PIC S9(04) COMP-5.
             05 TBS-VL-ITC-RLZD        PIC S9(15)V9(4) COMP-3.
             05 TBS-NR-ITC-RLZD        PIC S9(09) COMP-5.
             05 TBS-TX-ITC-RLZD        PIC X(20).
      *
      *    ?rea para recuperar Timestamp e data.
      *
       01  DADOS-SBTIMEDT.
           03  WK-CD-FUC                   PIC X(03)      VALUE 'F01'.
           03  WK-AREA-TRB.
               05  WK-AREA-TRB-TAM         PIC S9(9) COMP VALUE 1024.
               05  WK-AREA-TRB-DADOS       PIC  X(1024)   VALUE ' '.
      *        X'0002' - DD.MM.AAAA-HH:MM:SS.CCCCCCXXXX
           03  WK-FMT-TS                   PIC S9(4) COMP VALUE 1.
      *         X'001A'(26)
           03  WK-TAM-TS                   PIC S9(4) COMP VALUE 30.
           03  WK-TS-FMTD.
               05  ANO                     PIC  9(4).
               05  FILLER                  PIC  X(1).
               05  MES                     PIC  9(2).
               05  FILLER                  PIC  X(1).
               05  DIA                     PIC  9(2).
               05  FILLER                  PIC  X(1).
               05  HORA                    PIC  9(2).
               05  FILLER                  PIC  X(1).
               05  MINUTO                  PIC  9(2).
               05  FILLER                  PIC  X(1).
               05  SEGUNDO                 PIC  9(2).
               05  FILLER                  PIC  X(1).
               05  MICRO-SEGUNDO           PIC  9(6).
               05  FILLER                  PIC  X(4).

      *--> Data atual
       01  TS-SQL-ATU                      PIC  X(26) VALUE ' '.
       01  TS-SQL-ATU-R                    REDEFINES TS-SQL-ATU.
           03 DT-REF                       PIC  X(10).
           03 DT-REF-R                     REDEFINES   DT-REF.
             05 ANO                        PIC  9(04).
             05 FILLER                     PIC  X(01).
             05 MES                        PIC  9(02).
             05 FILLER                     PIC  X(01).
             05 DIA                        PIC  9(02).
           03  ANO                         PIC  9(04).
           03  FILLER                      PIC  X(01).
           03  MES                         PIC  9(02).
           03  FILLER                      PIC  X(01).
           03  DIA                         PIC  9(02).
           03  FILLER                      PIC  X(01).
           03  HORA                        PIC  9(02).
           03  FILLER                      PIC  X(01).
           03  MINUTO                      PIC  9(02).
           03  FILLER                      PIC  X(01).
           03  SEGUNDO                     PIC  9(02).
           03  FILLER                      PIC  X(01).
           03  MICRO-SEGUNDO               PIC  9(06).

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
      * 050 'Erro ao obter data e hora atual'
      * ??? 'Erro no calculo de subtração.'
      * 800 'Indexador de ocorrências da tabela é maior que o limite'
      * 839 'Erro no cálculo de soma'
      * 840 'Erro no cálculo de subtracao'

      *3004 'Erro ao acionar o módulo '

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
       01  SGTS0224-DADOS.
-INC SGTK0224

      *Negocial verifica se uma operação entra ou não em exceção.
-INC SGTK0163

      * Book do Container de Erro
-INC IIBKERRO

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

      *  Obtem campo timestamp
           PERFORM 120000-BUSCA-TS-DT
           MOVE WK-TS-FMTD       TO  TS-SQL-ATU
           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-VALIDAR-REQUISICAO       SECTION.
      *-----------------------------------------------------------------
           MOVE 002                    TO MSG1-CD-SEQL-ERRO

      *--> Validar Chave da Operação Estruturada
           IF   SGTS0224-CD-CTRA-TIT-VL     LESS ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0001              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' inválido.'      TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF   SGTS0224-CD-CTRA-TIT-VL     EQUAL ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0002              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF   SGTS0224-CD-OPR-ETRD-MCP    NOT NUMERIC
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE  0003              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada mercado de capitais nã
      -              'o numérico.'     TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF   SGTS0224-CD-OPR-ETRD-MCP    LESS  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0004              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada do mercado de capit
      -              'ais inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF   SGTS0224-CD-OPR-ETRD-MCP    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0005              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada mercado de capitais nã
      -              'o informado.'    TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF   SGTS0224-NR-VRS-OPR-ETRD    LESS  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0006              TO  MSG1-CD-ERRO-TS
               MOVE  'Número versão operação estruturada inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF   SGTS0224-NR-VRS-OPR-ETRD    EQUAL  ZEROS
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
       120000-BUSCA-TS-DT              SECTION.
      *-----------------------------------------------------------------
           MOVE 003                    TO MSG1-CD-SEQL-ERRO

           CALL SBTIMEDT            USING  WK-CD-FUC
                                           WK-AREA-TRB
                                           WK-FMT-TS
                                           WK-TAM-TS
                                           WK-TS-FMTD

           IF  RETURN-CODE      NOT EQUAL ZEROS
               SET  ERRO-PROGRAMA-ACIONADO TO  TRUE
               MOVE  0050                  TO  MSG1-CD-ERRO-TS
               MOVE 'Erro ao obter data e hora atual.'
                                           TO  MSG1-TX-ERRO-TS
                                               MSG1-TX-RTN-PGM-ACND
               MOVE SBTIMEDT               TO  MSG1-NM-PGM-ACND
               MOVE RETURN-CODE            TO  MSG1-CD-RTN-PGM-ACND
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       120000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-PROCESSAR                SECTION.
      *-----------------------------------------------------------------
           MOVE 004                    TO MSG1-CD-SEQL-ERRO

           PERFORM 800000-LIMPA-CAMPOS

           PERFORM 205000-CONSULTA-EXCECAO

      * Tratamento da lista, se operação entrou em alguma regra exceção.
           IF  SGTS0163-QT-OCR-TAB   GREATER ZEROS

               MOVE TS-SQL-ATU  TO   SGTS0224-TS-ALT

               PERFORM 210000-TRATA-EXCECAO
                               VARYING IND-163 FROM 1 BY 1
                                 UNTIL IND-163 > SGTS0163-QT-OCR-TAB

      *** Descarrega dados nas tabelas auxiliares.
               PERFORM 220000-CALCULA-INDENTACAO

               PERFORM 220200-SALVA-ITEM
                                 VARYING IND FROM 1 BY 1
                                   UNTIL IND GREATER IND-ITEM
               PERFORM 230000-SALVA-RLZD
                                 VARYING IND FROM 1 BY 1
                                   UNTIL IND GREATER IND-RLZD

               PERFORM 250000-CARREGA-SGTS0224

           END-IF

           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       205000-CONSULTA-EXCECAO         SECTION.
      *-----------------------------------------------------------------
           MOVE 005                    TO MSG1-CD-SEQL-ERRO

      * Negocial verifica se uma operação entra ou não em trat. exceção.
           MOVE ZEROS                  TO SGTS0163-QT-OCR-TAB
                                          SGTS0163-IC-TAB
           MOVE 'N'                    TO SGTS0163-IN-CNTD-PSQ

           MOVE  SGTS0224-CD-CTRA-TIT-VL
                                       TO SGTS0163-CD-CTRA-TIT-VL
           MOVE  SGTS0224-CD-OPR-ETRD-MCP
                                       TO SGTS0163-CD-OPR-ETRD-MCP
           MOVE  SGTS0224-NR-VRS-OPR-ETRD
                                       TO SGTS0163-NR-VRS-OPR-ETRD
      * Não existe Lista de regras a simular.
           MOVE ZEROS                  TO SGTS0163-QT-OCR-TAB-E

           MOVE SGTS0163              TO NM-PGM
           PERFORM 900000-EXECUTAR-MODULO

           .
       205000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       210000-TRATA-EXCECAO            SECTION.
      *-----------------------------------------------------------------
           MOVE 006                    TO MSG1-CD-SEQL-ERRO

      * Trata primeira regra principal encontrada.
           IF  IND-163     EQUAL  1
               PERFORM 240000-SALVA-PPL
           END-IF

      * Trata quebra de regra principal.
           IF SGTS0163-CD-RGR-PPL (IND-163) NOT EQUAL
                                                 CD-RGR-PPL-ANT
              IF  IND-ITEM  GREATER ZEROS
                  PERFORM 220000-CALCULA-INDENTACAO

                  PERFORM 220200-SALVA-ITEM
                            VARYING IND FROM 1 BY 1
                              UNTIL IND GREATER IND-ITEM

                  PERFORM 230000-SALVA-RLZD
                            VARYING IND FROM 1 BY 1
                              UNTIL IND GREATER IND-RLZD

                  PERFORM 240000-SALVA-PPL

                  PERFORM 800000-LIMPA-CAMPOS
              END-IF
           END-IF

      *** Carrega dados nas tabelas auxiliares.
           PERFORM 210100-CARREGA-ITEM

           IF  SGTS0163-CD-ATB-VLDC (IND-163) GREATER ZEROS
               PERFORM 210200-CARREGA-RLZD
           END-IF

           .
       210000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       210100-CARREGA-ITEM             SECTION.
      *-----------------------------------------------------------------
           MOVE 007                    TO MSG1-CD-SEQL-ERRO
      * Carrega dados do item na tabela auxiliar.
           ADD  1           TO         IND-ITEM

           IF  IND-ITEM    GREATER     500
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0800              TO  MSG1-CD-ERRO-TS
               MOVE  'Indexador de ocorrências da tabela é maior que o l
      -              'imite.'          TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           MOVE SGTS0163-CD-RGR-PPL (IND-163)      TO
                                         TB1-CD-RGR-VLDC-ITC(IND-ITEM)
           MOVE SGTS0163-CD-RGR-SCDR (IND-163)     TO
                                         TB1-CD-RGR-SCDR(IND-ITEM)
           MOVE SGTS0163-NR-VRS-RGR-SCDR (IND-163)     TO
                                         TB1-NR-VRS-RGR-SCDR(IND-ITEM)
           MOVE ZEROS                              TO
                                         TB1-QT-INDENTACAO  (IND-ITEM)
           MOVE SGTS0163-IN-LNH-EXCO (IND-163)     TO
                                         TB1-IN-LNH-EXCO    (IND-ITEM)
           MOVE SGTS0163-NR-SEQL-ITC (IND-163)     TO
                                         TB1-NR-SEQL-ITC    (IND-ITEM)
           MOVE SGTS0163-NR-SEQL-OGNL (IND-163)     TO
                                         TB1-NR-SEQL-OGNL   (IND-ITEM)
           MOVE SGTS0163-TIP-CMD-VLDC (IND-163)    TO
                                         TB1-TIP-CMD-VLDC   (IND-ITEM)
           MOVE SGTS0163-NR-GR-CND (IND-163)       TO
                                         TB1-NR-GR-CND      (IND-ITEM)
           MOVE SGTS0163-CD-ATB-VLDC (IND-163)     TO
                                         TB1-CD-ATB-VLDC    (IND-ITEM)
           MOVE SGTS0163-CD-TIP-CFT-LGC(IND-163)   TO
                                          TB1-CD-TIP-CFT-LGC(IND-ITEM)
           MOVE SGTS0163-CD-TIP-CFT-ITEM (IND-163) TO
                                         TB1-CD-TIP-CFT-ITEM (IND-ITEM)
           MOVE SGTS0163-VL-ITC-RGR (IND-163)      TO
                                         TB1-VL-ITC-RGR      (IND-ITEM)
           MOVE SGTS0163-NR-ITC-RGR (IND-163)      TO
                                         TB1-NR-ITC-RGR      (IND-ITEM)
           MOVE SGTS0163-TX-ITC-RGR (IND-163)      TO
                                         TB1-TX-ITC-RGR      (IND-ITEM)
           .
       210100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       210200-CARREGA-RLZD             SECTION.
      *-----------------------------------------------------------------
           MOVE 008                    TO MSG1-CD-SEQL-ERRO

      * Verifica se código de atributo já foi carregado na tabela de
      * atributos realizados mesmo que este se repita em outra regra.
      * SGTS0163-CD-ATB-VLDC (IND-163) campo a ser pesquisado.

           MOVE  'N'                   TO AX-ACHOU
           PERFORM VARYING IND-ATB FROM 1 BY 1
                     UNTIL IND-ATB > IND-RLZD
              IF  TB2-CD-ATB-RLZD (IND-ATB) EQUAL
                                          SGTS0163-CD-ATB-VLDC (IND-163)
                  MOVE 'S'  TO AX-ACHOU
              END-IF
           END-PERFORM

      *  Verifica se os dados deste atributo serão carregados na tabela
      *  auxlixar.
           IF AX-ACHOU    EQUAL  'N'
              ADD 1           TO          IND-RLZD
              IF  IND-RLZD    GREATER     50
                  SET   ERRO-PROGRAMADO   TO  TRUE
                  MOVE  0800              TO  MSG1-CD-ERRO-TS
                  MOVE  'Indexador de ocorrências da tabela é maior que
      -                 'o limite.'       TO MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
              END-IF

      *       MOVE SGTS0163-CD-RGR-PPL (IND-163)    TO
      *                                 TB2-CD-RGR-VLDC-RLZD (IND-RLZD)
              MOVE SGTS0163-CD-RGR-SCDR (IND-163)   TO
                                        TB2-CD-RGR-VLDC-RLZD (IND-RLZD)
              MOVE SGTS0163-CD-ATB-VLDC (IND-163)    TO
                                        TB2-CD-ATB-RLZD      (IND-RLZD)
              MOVE SGTS0163-VL-ITC-RLZD (IND-163)    TO
                                        TB2-VL-ITC-RLZD      (IND-RLZD)
              MOVE SGTS0163-NR-ITC-RLZD (IND-163)    TO
                                        TB2-NR-ITC-RLZD      (IND-RLZD)
              MOVE SGTS0163-TX-ITC-RLZD (IND-163)    TO
                                        TB2-TX-ITC-RLZD      (IND-RLZD)

           END-IF
           .
       210200-SAI.
           EXIT.

      *-----------------------------------------------------------------
       220000-CALCULA-INDENTACAO       SECTION.
      *-----------------------------------------------------------------
           MOVE 009                    TO MSG1-CD-SEQL-ERRO
      *    Calcula tabulação baseado nos grupos, condições e instruções.
      **  Tipos de comando:
      **  1 - Atender condição
      **  2 - Negar condição
      **  3 - Finalizar condição
      **  4 - Não gravar Exceção
      **  5 - Gravar Exceção
      **  6 - Executar Regra
      *
      *    Carrega áreas de controle para cálculo de indentação;
           MOVE  -1          TO IC-TAB
           MOVE  ZEROS       TO GR-ANT
                                CD-TIP-CMD-ANT
      *
           PERFORM 220100-CALCULA
                          VARYING IDX FROM 1 BY 1
                              UNTIL IDX GREATER IND-ITEM

      *  Reformatar campo de tipo de comando para atender camada de
      *  apresentação Java para mostrar os comandos 'E' / 'OU'.
      *
           PERFORM VARYING IDX FROM 1 BY 1
                      UNTIL IDX GREATER IND-ITEM
                IF TB1-CD-TIP-CFT-LGC (IDX)  >  ZEROS
                   MOVE  ZEROS         TO  TB1-TIP-CMD-VLDC (IDX)
                END-IF
           END-PERFORM
           .
       220000-SAI.
           EXIT.

      *---------------------------------------------------------------  --
       220100-CALCULA                  SECTION.
      *-----------------------------------------------------------------
           MOVE 010                    TO MSG1-CD-SEQL-ERRO
      *
           SUBTRACT 1 FROM IDX
                      GIVING IC-ANT  ON SIZE ERROR
                      SET ERRO-PROGRAMADO     TO TRUE
                      MOVE 840                TO MSG1-CD-ERRO-TS
                      MOVE 'Erro cálculo de subtração.'
                                              TO MSG1-TX-ERRO-TS
                      PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-SUBTRACT
      *
           IF    TB1-NR-GR-CND (IDX)    >   GR-ANT
                 IF  IC-ANT     =  0
                     ADD  +1    TO IC-TAB
                 ELSE
                     IF  TB1-TIP-CMD-VLDC (IC-ANT) = 1 OR 2
                         ADD  +1    TO  IC-TAB
                     END-IF
                 END-IF
                 MOVE  TB1-NR-GR-CND (IDX)  TO GR-ANT
           ELSE
                 IF    TB1-NR-GR-CND (IDX)     =   GR-ANT
                    IF TB1-TIP-CMD-VLDC (IDX)  >   3
                       IF  CD-TIP-CMD-ANT     NOT = 6
                           ADD  +1    TO    IC-TAB
                       END-IF
                    ELSE
                        IF  TB1-TIP-CMD-VLDC (IDX) = 2 OR 3
                           SUBTRACT 1   FROM  IC-TAB
                       END-IF
                    END-IF
                 ELSE
                    IF  TB1-TIP-CMD-VLDC (IDX) = 2 OR 3
                        SUBTRACT 1   FROM  IC-TAB
                    END-IF
                    MOVE  TB1-NR-GR-CND (IDX)  TO  GR-ANT
                 END-IF
           END-IF

           MOVE  TB1-TIP-CMD-VLDC (IDX)  TO CD-TIP-CMD-ANT

      *  Carrega campo quantidade de indentação calculado, para
      *  facilitar a apresentação apresentação Java.
           MOVE  IC-TAB               TO  TB1-QT-INDENTACAO (IDX)

           .
       220100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       220200-SALVA-ITEM               SECTION.
      *-----------------------------------------------------------------
           MOVE 011                    TO MSG1-CD-SEQL-ERRO

      * Descarrega dados dos itens da tabela auxiliar.
      * Calcula indentação da instrução para enviar a camada Java de
      * apresentação na tela.
      * Movimenta dados da tabela para a área de saída (SGTS0224).
      *  O campo TBS-QT-REG-ITEM não poderá ser zerado até o final
      *  da execução deste módulo.

           ADD 1       TO TBS-QT-REG-ITEM

           IF  TBS-QT-REG-ITEM    GREATER   500
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0800              TO  MSG1-CD-ERRO-TS
               MOVE  'Indexador de ocorrências da tabela é maior que o l
      -              'imite.'          TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           MOVE TB1-CD-RGR-VLDC-ITC (IND)    TO
                        TBS-CD-RGR-VLDC-ITC(TBS-QT-REG-ITEM)
           MOVE TB1-CD-RGR-SCDR (IND)        TO
                        TBS-CD-RGR-SCDR     (TBS-QT-REG-ITEM)
           MOVE TB1-NR-VRS-RGR-SCDR (IND)    TO
                        TBS-NR-VRS-RGR-SCDR (TBS-QT-REG-ITEM)
           MOVE TB1-QT-INDENTACAO   (IND)    TO
                        TBS-QT-INDENTACAO  (TBS-QT-REG-ITEM)
           MOVE TB1-IN-LNH-EXCO     (IND)    TO
                        TBS-IN-LNH-EXCO    (TBS-QT-REG-ITEM)
           MOVE TB1-NR-SEQL-ITC     (IND)    TO
                        TBS-NR-SEQL-ITC    (TBS-QT-REG-ITEM)
           MOVE TB1-NR-SEQL-OGNL   (IND)    TO
                        TBS-NR-SEQL-OGNL   (TBS-QT-REG-ITEM)
           MOVE TB1-TIP-CMD-VLDC    (IND)    TO
                        TBS-TIP-CMD-VLDC   (TBS-QT-REG-ITEM)
           MOVE TB1-NR-GR-CND       (IND)    TO
                        TBS-NR-GR-CND      (TBS-QT-REG-ITEM)
           MOVE TB1-CD-ATB-VLDC     (IND)    TO
                        TBS-CD-ATB-VLDC    (TBS-QT-REG-ITEM)
           MOVE TB1-CD-TIP-CFT-LGC  (IND)    TO
                       TBS-CD-TIP-CFT-LGC  (TBS-QT-REG-ITEM)
           MOVE TB1-CD-TIP-CFT-ITEM (IND)    TO
                       TBS-CD-TIP-CFT-ITEM (TBS-QT-REG-ITEM)
           MOVE TB1-VL-ITC-RGR      (IND)    TO
                       TBS-VL-ITC-RGR      (TBS-QT-REG-ITEM)
           MOVE TB1-NR-ITC-RGR      (IND)    TO
                       TBS-NR-ITC-RGR      (TBS-QT-REG-ITEM)
           MOVE TB1-TX-ITC-RGR      (IND)    TO
                       TBS-TX-ITC-RGR      (TBS-QT-REG-ITEM)
           .
       220200-SAI.
           EXIT.

      *-----------------------------------------------------------------
       230000-SALVA-RLZD               SECTION.
      *-----------------------------------------------------------------
           MOVE 012                    TO MSG1-CD-SEQL-ERRO
      * Descarrega dados dos atributos realizados.(max.50 ocor)
      *  O campo TBS-QT-REG-RLZD não poderá ser zerado até o final
      *  da execução deste módulo.

           ADD 1       TO TBS-QT-REG-RLZD
           IF  TBS-QT-REG-RLZD    GREATER   50
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0800              TO  MSG1-CD-ERRO-TS
               MOVE  'Indexador de ocorrências da tabela é maior que o l
      -              'imite.'          TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           MOVE  TB2-CD-RGR-VLDC-RLZD (IND)    TO
                        TBS-CD-RGR-VLDC-RLZD(TBS-QT-REG-RLZD)
           MOVE  TB2-CD-ATB-RLZD      (IND)    TO
                        TBS-CD-ATB-RLZD     (TBS-QT-REG-RLZD)
           MOVE  TB2-VL-ITC-RLZD      (IND)    TO
                        TBS-VL-ITC-RLZD     (TBS-QT-REG-RLZD)
           MOVE  TB2-NR-ITC-RLZD      (IND)    TO
                        TBS-NR-ITC-RLZD     (TBS-QT-REG-RLZD)
           MOVE  TB2-TX-ITC-RLZD      (IND)    TO
                        TBS-TX-ITC-RLZD     (TBS-QT-REG-RLZD)

           .
       230000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       240000-SALVA-PPL                SECTION.
      *-----------------------------------------------------------------
           MOVE 013                    TO MSG1-CD-SEQL-ERRO

      *  Carrega dados da regra principal (max.100 ocor. previstas)
      *  no book de saída.
      *  O campo TBS-QT-REG-RGR  não poderá ser zerado até o final
      *  da execução deste módulo.

           ADD 1       TO TBS-QT-REG-RGR

           IF  TBS-QT-REG-RGR     GREATER   100
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0800              TO  MSG1-CD-ERRO-TS
               MOVE  'Indexador de ocorrências da tabela é maior que o l
      -              'imite.'          TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           MOVE SGTS0163-CD-RGR-PPL (IND-163)     TO
                              TBS-CD-RGR-VLDC(TBS-QT-REG-RGR)

                              CD-RGR-PPL-ANT

           MOVE SGTS0163-NR-VRS-RGR-PPL (IND-163) TO
                         TBS-NR-VRS-RGR-VLDC (TBS-QT-REG-RGR)

           .
       240000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       250000-CARREGA-SGTS0224         SECTION.
      *-----------------------------------------------------------------
           MOVE 014                    TO MSG1-CD-SEQL-ERRO

      *  Movimenta dados para o container de saída de todas as tabelas
      *  auxiliares utilizadas.

      *  Grava regras principais retornados do SGTS0163.
           MOVE  TBS-QT-REG-RGR        TO SGTS0224-QT-REG-RGR

           PERFORM VARYING IND FROM 1 BY 1
                     UNTIL IND GREATER TBS-QT-REG-RGR
              MOVE TBS-CD-RGR-VLDC (IND)     TO
                                         SGTS0224-CD-RGR-VLDC (IND)
              MOVE TBS-NR-VRS-RGR-VLDC (IND) TO
                                         SGTS0224-NR-VRS-RGR-VLDC (IND)
           END-PERFORM

      * Grava itens retornados pelo SGTS0163.
           MOVE  TBS-QT-REG-ITEM       TO SGTS0224-QT-REG-ITEM

           PERFORM VARYING IND FROM 1 BY 1
                     UNTIL IND GREATER TBS-QT-REG-ITEM

              MOVE TBS-CD-RGR-VLDC-ITC (IND)    TO
                                         SGTS0224-CD-RGR-VLDC-ITC(IND)
              MOVE TBS-CD-RGR-SCDR     (IND)    TO
                                         SGTS0224-CD-RGR-SCDR     (IND)
              MOVE TBS-NR-VRS-RGR-SCDR (IND)    TO
                                         SGTS0224-NR-VRS-RGR-SCDR (IND)
              MOVE TBS-QT-INDENTACAO   (IND)      TO
                                         SGTS0224-QT-INDENTACAO  (IND)
              MOVE TBS-IN-LNH-EXCO     (IND)    TO
                                         SGTS0224-IN-LNH-EXCO    (IND)
              MOVE TBS-NR-SEQL-ITC     (IND)    TO
                                        SGTS0224-NR-SEQL-ITC     (IND)
              MOVE TBS-NR-SEQL-OGNL     (IND)    TO
                                        SGTS0224-NR-SEQL-OGNL    (IND)
              MOVE TBS-TIP-CMD-VLDC    (IND)    TO
                                         SGTS0224-TIP-CMD-VLDC   (IND)
              MOVE TBS-NR-GR-CND       (IND)    TO
                                         SGTS0224-NR-GR-CND      (IND)
              MOVE TBS-CD-ATB-VLDC     (IND)    TO
                                         SGTS0224-CD-ATB-VLDC    (IND)
              MOVE TBS-CD-TIP-CFT-LGC  (IND)    TO
                                         SGTS0224-CD-TIP-CFT-LGC (IND)
              MOVE TBS-CD-TIP-CFT-ITEM (IND)    TO
                                        SGTS0224-CD-TIP-CFT-ITEM (IND)
              MOVE TBS-VL-ITC-RGR      (IND)    TO
                                         SGTS0224-VL-ITC-RGR     (IND)
              MOVE TBS-NR-ITC-RGR      (IND)    TO
                                        SGTS0224-NR-ITC-RGR      (IND)
              MOVE TBS-TX-ITC-RGR      (IND)    TO
                                        SGTS0224-TX-ITC-RGR      (IND)
           END-PERFORM

      * Grava totais de atributos com valor retornados do SGTS0163.
           MOVE  TBS-QT-REG-RLZD    TO SGTS0224-QT-REG-RLZD

           PERFORM VARYING IND FROM 1 BY 1
                     UNTIL IND GREATER TBS-QT-REG-RLZD
              MOVE  TBS-CD-RGR-VLDC-RLZD (IND)    TO
                                        SGTS0224-CD-RGR-VLDC-RLZD(IND)
              MOVE  TBS-CD-ATB-RLZD      (IND)    TO
                                        SGTS0224-CD-ATB-RLZD     (IND)
              MOVE  TBS-VL-ITC-RLZD      (IND)    TO
                                        SGTS0224-VL-ITC-RLZD     (IND)
              MOVE  TBS-NR-ITC-RLZD      (IND)    TO
                                        SGTS0224-NR-ITC-RLZD     (IND)
              MOVE  TBS-TX-ITC-RLZD      (IND)    TO
                                       SGTS0224-TX-ITC-RLZD      (IND)
           END-PERFORM

           .
       250000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       800000-LIMPA-CAMPOS             SECTION.
      *-----------------------------------------------------------------
           MOVE 015                    TO MSG1-CD-SEQL-ERRO

      *  Inicializa campos de trabalho na quebra de regra principal.

           PERFORM VARYING IND-ITEM FROM 1 BY 1
                     UNTIL IND-ITEM GREATER 500
                   MOVE  ZEROS      TO TB1-CD-RGR-VLDC-ITC  (IND-ITEM)
                   MOVE  ZEROS      TO TB1-CD-RGR-SCDR      (IND-ITEM)
                   MOVE  ZEROS      TO TB1-NR-VRS-RGR-SCDR  (IND-ITEM)
                   MOVE  ZEROS      TO TB1-QT-INDENTACAO    (IND-ITEM)
                   MOVE  SPACES     TO TB1-IN-LNH-EXCO      (IND-ITEM)
                   MOVE  ZEROS      TO TB1-NR-SEQL-ITC      (IND-ITEM)
                   MOVE  ZEROS      TO TB1-NR-SEQL-OGNL     (IND-ITEM)
                   MOVE  ZEROS      TO TB1-TIP-CMD-VLDC     (IND-ITEM)
                   MOVE  ZEROS      TO TB1-NR-GR-CND        (IND-ITEM)
                   MOVE  ZEROS      TO TB1-CD-ATB-VLDC      (IND-ITEM)
                   MOVE  ZEROS      TO TB1-CD-TIP-CFT-LGC   (IND-ITEM)
                   MOVE  ZEROS      TO TB1-CD-TIP-CFT-ITEM  (IND-ITEM)
                   MOVE  ZEROS      TO TB1-VL-ITC-RGR       (IND-ITEM)
                   MOVE  ZEROS      TO TB1-NR-ITC-RGR       (IND-ITEM)
                   MOVE  SPACES     TO TB1-TX-ITC-RGR       (IND-ITEM)
           END-PERFORM

           PERFORM VARYING IND-RLZD FROM 1 BY 1
                     UNTIL IND-RLZD GREATER 50
                   MOVE  ZEROS      TO TB2-CD-RGR-VLDC-RLZD (IND-RLZD)
                   MOVE  ZEROS      TO TB2-CD-ATB-RLZD      (IND-RLZD)
                   MOVE  ZEROS      TO TB2-VL-ITC-RLZD      (IND-RLZD)
                   MOVE  ZEROS      TO TB2-NR-ITC-RLZD      (IND-RLZD)
                   MOVE  ZEROS      TO TB2-TX-ITC-RLZD      (IND-RLZD)
           END-PERFORM

           MOVE ZEROS        TO     IND-ITEM
                                    IND-RLZD
           .
       800000-SAI.
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

               WHEN SGTS0163
                    EXEC CICS LINK
                              PROGRAM ( SGTS0163   )
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
       969000-MONITORAR-VARIAVEIS      SECTION.
      *-----------------------------------------------------------------
      * Monitora as variáveis dos módulos chamados
      *-----------------------------------------------------------------
      * Variáveis exclusivas de cada programa acionado
           EVALUATE NM-PGM
             WHEN SGTS0163
               MOVE 'SGTS0163-CD-CTRA-TIT-VL' TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0163-CD-CTRA-TIT-VL  TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0163-CD-OPR-ETRD-MCP' TO MSGKW999-NM-VRV-MNT
               MOVE SGTS0163-CD-OPR-ETRD-MCP   TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0163-NR-VRS-OPR-ETRD' TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0163-NR-VRS-OPR-ETRD  TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0163-QT-OCR-TAB-E'    TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0163-QT-OCR-TAB-E     TO MSGKW999-VL-VRV-MNT
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
           MOVE 016                    TO MSG1-CD-SEQL-ERRO

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
           MOVE 017                    TO MSG1-CD-SEQL-ERRO

      *    Monta o nome do container de requisição
           MOVE CTE-PROG(5:8)          TO NM-PGM
           MOVE 'SGTS0224-RQSC'        TO NM-CTNR

      *    Recupera o tamanho do container de requisição
           PERFORM 973100-LER-CONTAINER-NODATA

      *--> Obtem o tamanho esperado da requisição
           MOVE  LENGTH OF  SGTS0224-RQSC
                                       TO  QT-CCT-EPRO

      *--> Valida tamanho esperado da requisição X tamanho recebido
           IF  QT-CCT-LIDO             NOT EQUAL  QT-CCT-EPRO
               SET   ERRO-TAMANHO-AREA TO  TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           MOVE 018                    TO MSG1-CD-SEQL-ERRO

      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Efetua a leitura do container
           EXEC CICS GET
                     CONTAINER ( NM-CTNR       )
                     FLENGTH   ( QT-CCT-LIDO   )
                     INTO      (  SGTS0224-RQSC )
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

               WHEN SGTS0163
                    EXEC CICS GET
                              CONTAINER ( NM-CTNR       )
                              INTO      ( SGTS0163-RPST )
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

           MOVE 019                    TO MSG1-CD-SEQL-ERRO

      *    Monta o nome do container de resposta
           MOVE 'SGTS0224-RPST'        TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

           EXEC CICS PUT
                     CONTAINER (NM-CTNR                )
                     FROM      (SGTS0224-RPST          )
                     FLENGTH   (LENGTH OF  SGTS0224-RPST)
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
               WHEN SGTS0163
                    EXEC CICS PUT
                              CONTAINER ( NM-CTNR                 )
                              FROM      ( SGTS0163-RQSC           )
                              FLENGTH   ( LENGTH OF SGTS0163-RQSC )
                    END-EXEC

               WHEN OTHER
                    MOVE 9004          TO MSG1-CD-ERRO-TS
                    MOVE 'Erro na gravação da requisição do módulo.'
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
           MOVE 020                    TO MSG1-CD-SEQL-ERRO
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
           MOVE 021                    TO MSG1-CD-SEQL-ERRO

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
           MOVE ' SGTS0224-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE   SGTS0224-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE ' SGTS0224-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
           IF   SGTS0224-CD-OPR-ETRD-MCP    NUMERIC
               MOVE   SGTS0224-CD-OPR-ETRD-MCP
                                       TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE ' SGTS0224-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE   SGTS0224-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
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
      *---------------------- Fim  SGTS022 ----------------------------*