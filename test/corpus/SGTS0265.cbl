       CBL ARITH(EXTEND)
      *-----------------------------------------------------------------
      * PROGRAMA..: SGTS0265
      * ANALISTA..: F2804421 - Eliana Cristina Ramos da Silva
      * AUTOR.....: F2804421 - Eliana Cristina Ramos da Silva
      * DATA......: 28/08/2019
      * OBJETIVO..: Listar operações com indicador de flexibilização até
      *            até a data informada.
      * COMPILACAO: COBOL/CICS
      *-----------------------------------------------------------------
      * VRS001 24/10/2019 F2804421 DE 2016/145195 - Alterar a pesquisa
      *               do campo A.DT_FIM_VLD_IN para  > :AUX-DATA-PSC.
      * VRS001 21/10/2019 F2804421 DE 2016/145195 - Incluir ORDER BY.
      *        15/10/2019 F2804421  DE 2016/145195 -Acerto SQl(upper).
      *        28/08/2019 F2804421  DE 2016/145195 - Implantação
      *-----------------------------------------------------------------
        IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTS0265 .
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
                                            '*** SGTS0265 ***'.
       77  CTE-VERS                    PIC  X(06)  VALUE 'VRS001'.
      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------
      * Areas de trabalho
       01  CTE-INICIO-LS               PIC X(30)  VALUE
                                       '*** LS COMECA AQUI   ***'.
      *--> Areas de trabalho
       01  IC-TAB                      PIC S9(04)     COMP-5 VALUE 0.
       01  QT-REG-SELD                 PIC S9(04)     COMP-5 VALUE 0.

      *--> Chamada à subrotina SBDATAF
       01  WKSBDATA                    PIC X(4099).
       01  AREA-RED REDEFINES WKSBDATA.
           03  AREA-TAM                PIC 9(0008) COMP.
           03  FILLER                  PIC X(4071).
           03  AREA-FIM                PIC X(0020).
       01  SBDATAF                     PIC  X(08) VALUE 'SBDATAF'.
      * 01  AREA-SBDATAF.
        01  FUNCAO                     PIC  X(3) VALUE 'F01'.
        01  ARG01                      PIC  9(8) VALUE 0.
        01  ARG01-R REDEFINES ARG01.
            03  DIA                    PIC  9(2).
            03  MES                    PIC  9(2).
            03  ANO                    PIC  9(4).
        01  ARG02                      PIC  9(1) VALUE 0.

      *--> Máscaras edição de variáveis p/ monitoração mensagens de erro
       01  MCR-S9-15                   PIC      -999999999999999.

      *--> Variáveis de pesquisa.
       01  FLEXIB              PIC S9(04) COMP-5 VALUE 2.
       01  EMISSOR             PIC S9(04) COMP-5 VALUE 1.
       01  OPR-APVD            PIC S9(09) COMP-5 VALUE 5.
       01  VRS-APVD            PIC S9(04) COMP-5 VALUE 3.
       01  CD-CTRA-TIT-VL-INC  PIC S9(09) COMP-5 VALUE 0.
       01  CD-CTRA-TIT-VL-FIM  PIC S9(09) COMP-5 VALUE 999999999.
       01  CD-OPR-ETRD-MCP-INC PIC S9(11) COMP-3 VALUE 0.
       01  CD-OPR-ETRD-MCP-FIM PIC S9(11) COMP-3 VALUE 99999999999.
       01  CD-CLI-INC          PIC S9(09) COMP-5 VALUE 0.
       01  CD-CLI-FIM          PIC S9(09) COMP-5 VALUE 999999999.

       01  AUX-DATA-PSC                     PIC X(010)
                                            VALUE '  .  .    '.
       01  AUX-DATA-PSC-R  REDEFINES  AUX-DATA-PSC.
           03  DIA                          PIC 9(02).
           03  FILLER                       PIC X(01).
           03  MES                          PIC 9(02).
           03  FILLER                       PIC X(01).
           03  ANO                          PIC 9(04).

      *--> Variaveis utilizados para leitura
       01  REG-WS.
           03 CD-CTRA-TIT-VL           PIC S9(09)     COMP-5
                                       OCCURS 101 TIMES         VALUE 0.
           03 CD-OPR-ETRD-MCP          PIC S9(11)     COMP-3
                                       OCCURS 101 TIMES         VALUE 0.
           03 NR-VRS-OPR-ETRD          PIC S9(04)      COMP-5
                                       OCCURS 101 TIMES         VALUE 0.
           03 CD-TIT-OPR-ETRD          PIC  X(12)
                                       OCCURS 101 TIMES       VALUE ' '.
           03 CD-CLI                   PIC S9(09)      COMP-5
                                       OCCURS 101 TIMES         VALUE 0.
           03 TS-EST-OPR-ETRD          PIC  X(26)
                                       OCCURS 101 TIMES      VALUE ' '.
           03 NR-IN-OPR-ETRD           PIC S9(04)      COMP-5
                                       OCCURS 101 TIMES         VALUE 0.

      *--> Variavel(is) de controle do cursor(es)
       01  CTL-CURSOR-PSQ-1.
           03  CD-FIM-CURSOR-PSQ-1     PIC S9(04)     COMP-5 VALUE 0.
               88  FIM-CURSOR-PSQ-1                          VALUE +100.
           03  IN-CURSOR-PSQ-ABTO-1    PIC  X(01)            VALUE 'N'.
               88  CURSOR-PSQ-ABTO-1   VALUE 'S'.
      *-----------------------------------------------------------------
      * Books dos Containers e das subrotinas chamadas
      *-----------------------------------------------------------------
      * Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1

      * Book para captura de abend cics do bloco código HLPKW999
-INC HLPKW999

      * Book para captura de erro via MSG1
-INC MSGKWMG1

      * Book do container de requisição e resposta deste mdulo
       01  SGTS0265-DADOS.
-INC SGTK0265

      * Book do Container de Erro
-INC IIBKERRO
      *--> Area de trabalho SQL/ DB2.
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

      *--> Declaração do cursor para pesquisa
           EXEC SQL
             DECLARE CURSOR-PSQ-1 CURSOR
                      WITH ROWSET POSITIONING FOR
             SELECT X.CD_CTRA_TIT_VL
                   ,X.CD_OPR_ETRD_MCP
                   ,X.NR_VRS_OPR_ETRD
                   ,B.CD_TIT_OPR_ETRD
                   ,C.CD_CLI
                   ,MAX(X.TS_EST_OPR_ETRD)
                   ,MAX(A.NR_IN_OPR_ETRD)
             FROM DB2SGT.IN_OPR_ETRD A
--
             INNER JOIN DB2SGT.OPR_ETRD_MCP B
                     ON B.CD_CTRA_TIT_VL    =  A.CD_CTRA_TIT_VL
                    AND B.CD_OPR_ETRD_MCP   =  A.CD_OPR_ETRD_MCP
                    AND B.NR_VRS_OPR_ETRD   =  A.NR_VRS_OPR_ETRD
--
             INNER JOIN DB2SGT.EST_OPR_ETRD X
                     ON X.CD_CTRA_TIT_VL     =  B.CD_CTRA_TIT_VL
                    AND X.CD_OPR_ETRD_MCP    =  B.CD_OPR_ETRD_MCP
                    AND X.NR_VRS_OPR_ETRD    =  B.NR_VRS_OPR_ETRD
--
             INNER JOIN DB2SGT.CLI_OPR_ETRD C
                     ON C.CD_CTRA_TIT_VL     =  X.CD_CTRA_TIT_VL
                    AND C.CD_OPR_ETRD_MCP    =  X.CD_OPR_ETRD_MCP
                    AND C.NR_VRS_OPR_ETRD    =  X.NR_VRS_OPR_ETRD
             WHERE
                    A.CD_TIP_IN              = :FLEXIB
               AND  ((A.DT_FIM_VLD_IN IS NULL)  OR
                     (A.DT_FIM_VLD_IN  > :AUX-DATA-PSC))
               AND  X.CD_CTRA_TIT_VL        >= :CD-CTRA-TIT-VL-INC
               AND  X.CD_CTRA_TIT_VL        <= :CD-CTRA-TIT-VL-FIM
               AND  X.CD_OPR_ETRD_MCP       >= :CD-OPR-ETRD-MCP-INC
               AND  X.CD_OPR_ETRD_MCP       <= :CD-OPR-ETRD-MCP-FIM
               AND  UPPER(COALESCE(B.CD_TIT_OPR_ETRD, ' ')) LIKE
                     UPPER(:SGTS0265-CD-TIT-OPR-ETRD-E)
               AND  C.CD_CLI                >= :CD-CLI-INC
               AND  C.CD_CLI                <= :CD-CLI-FIM
               AND  DATE(X.TS_EST_OPR_ETRD) <= :AUX-DATA-PSC
               AND  X.CD_TIP_EST_OPR         = :OPR-APVD
               AND  X.NR_VRS_OPR_ETRD        =
                      (SELECT  MAX(Y.NR_VRS_OPR_ETRD)
                         FROM DB2SGT.EST_VRS_OPR_ETRD Y
                        WHERE Y.CD_CTRA_TIT_VL    = X.CD_CTRA_TIT_VL
                          AND Y.CD_OPR_ETRD_MCP   = X.CD_OPR_ETRD_MCP
                          AND DATE(Y.TS_EST_VRS) <= :AUX-DATA-PSC
                          AND Y.CD_TIP_EST_VRS    = :VRS-APVD)
               AND  C.CD_TIP_FUC_CLI        =  :EMISSOR
               AND  C.DT_FIM_VLD_FUC        = '9999-12-31'
      *--> paginacao
             AND (
                 (X.CD_CTRA_TIT_VL      > :SGTS0265-CD-CTRA-TIT-VL-ULT)
               OR  (X.CD_CTRA_TIT_VL    = :SGTS0265-CD-CTRA-TIT-VL-ULT
                 AND X.CD_OPR_ETRD_MCP  > :SGTS0265-CD-OPR-ETRD-MCP-ULT)
                  )
           GROUP BY
                    X.CD_CTRA_TIT_VL
                   ,X.CD_OPR_ETRD_MCP
                   ,X.NR_VRS_OPR_ETRD
                   ,B.CD_TIT_OPR_ETRD
                   ,C.CD_CLI
           ORDER BY
                    X.CD_CTRA_TIT_VL
                   ,X.CD_OPR_ETRD_MCP
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

           PERFORM 970101-ABRE-CURSOR-PSQ-1
           PERFORM 970102-LER-CURSOR-PSQ-1

           PERFORM 200000-PROCESSAR
                   VARYING IC-TAB FROM 1 BY 1
                     UNTIL IC-TAB > SGTS0265-QT-OCR-TAB

           PERFORM 970103-FECHA-CURSOR-PSQ-1
           PERFORM 980000-GRAVAR-CTNR-RPST

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

           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-VALIDAR-REQUISICAO       SECTION.
      *-----------------------------------------------------------------
           MOVE 002                    TO MSG1-CD-SEQL-ERRO

      *--------- Data da posição de saldo para pesquisa  -------------
           IF  SGTS0265-DT-PSC-E  EQUAL  SPACES
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 1068               TO MSG1-CD-ERRO-TS
               MOVE 'Data para efetuar a pesquisa não informada.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           ELSE
               MOVE CORR SGTS0265-DT-PSC-R      TO  ARG01-R
               MOVE 4                           TO  ARG02
               PERFORM 910000-VALIDAR-DATAS
           END-IF

               MOVE CORR SGTS0265-DT-PSC-R      TO  AUX-DATA-PSC-R

      *---------- Carteira     -------------------------------------
           IF   SGTS0265-CD-CTRA-TIT-VL-E  LESS ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0001              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' inválido.'      TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0265-CD-CTRA-TIT-VL-E    GREATER ZEROS
               MOVE SGTS0265-CD-CTRA-TIT-VL-E TO  CD-CTRA-TIT-VL-INC
                                                  CD-CTRA-TIT-VL-FIM
           END-IF

      *----------  Operação    ---------------------------------------
           IF   SGTS0265-CD-OPR-ETRD-MCP-E   NOT NUMERIC
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE  0003              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada mercado de capitais nã
      -              'o numérico.'     TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF   SGTS0265-CD-OPR-ETRD-MCP-E    LESS  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0004              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada do mercado de capit
      -              'ais inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF   SGTS0265-CD-OPR-ETRD-MCP-E    GREATER  ZEROS
                MOVE SGTS0265-CD-OPR-ETRD-MCP-E TO CD-OPR-ETRD-MCP-INC
                                                   CD-OPR-ETRD-MCP-FIM
           END-IF

      *---------     Cliente emissor --------------------------------
           IF   SGTS0265-CD-CLI-E    LESS  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0028              TO  MSG1-CD-ERRO-TS
               MOVE  'Código cliente emissor inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF   SGTS0265-CD-CLI-E    GREATER  ZEROS
                MOVE SGTS0265-CD-CLI-E TO CD-CLI-INC
                                          CD-CLI-FIM
           END-IF

      *-------     Título - trocar brancos por % para pesquisa. -----
           INSPECT SGTS0265-CD-TIT-OPR-ETRD-E
                                    REPLACING ALL SPACES BY '%'

      *--> Carteira-paginacao
           IF  SGTS0265-CD-CTRA-TIT-VL-ULT < 0
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0001               TO MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de títulos e valores mobiliários
      -             ' inválido.'       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *--> Operação-paginacao
           IF  SGTS0265-CD-OPR-ETRD-MCP-ULT NOT NUMERIC
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0003               TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -             ' numérico.'       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           ELSE
            IF SGTS0265-CD-OPR-ETRD-MCP-ULT < 0
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0004               TO MSG1-CD-ERRO-TS
               MOVE 'Código da operação estruturada do mercado de capita
      -             'is inválido.'     TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
            END-IF
           END-IF

           MOVE 'N'                    TO SGTS0265-IN-CTNC-PSQ

           .
       110000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-PROCESSAR                SECTION.
      *-----------------------------------------------------------------
           MOVE 004                    TO MSG1-CD-SEQL-ERRO

      * Tratamento dos registros retornados.

           MOVE CD-CTRA-TIT-VL  (IC-TAB)
                                  TO SGTS0265-CD-CTRA-TIT-VL-S  (IC-TAB)
           MOVE CD-OPR-ETRD-MCP  (IC-TAB)
                                  TO SGTS0265-CD-OPR-ETRD-MCP-S (IC-TAB)
           MOVE NR-VRS-OPR-ETRD  (IC-TAB)
                                  TO SGTS0265-NR-VRS-OPR-ETRD-S (IC-TAB)
           MOVE CD-TIT-OPR-ETRD       (IC-TAB)
                                  TO SGTS0265-CD-TIT-OPR-ETRD-S (IC-TAB)
           MOVE CD-CLI  (IC-TAB)
                                  TO SGTS0265-CD-CLI-S          (IC-TAB)

           .
       200000-SAI.
           EXIT.
      *
      *-----------------------------------------------------------------
      *                                                                *
      *  SECTION DA FAIXA DE 900000 A 969900 PARA DEMAIS ROTINAS       *
      *                                                                *
      *-----------------------------------------------------------------

      *----------------------------------------------------------------*
       910000-VALIDAR-DATAS                 SECTION.
      *----------------------------------------------------------------*
           MOVE 0083                   TO MSG1-CD-SEQL-ERRO

           MOVE 4095 TO AREA-TAM.
           MOVE 'SBDATAF -WRKAREA-FIM' TO AREA-FIM.

           CALL SBDATAF     USING FUNCAO WKSBDATA  ARG01 ARG02.

      *--> Return code = 24: Data Inválida.

           IF  RETURN-CODE             NOT EQUAL ZEROS AND 24 AND 22
                                                       AND 46
               SET ERRO-PROGRAMA-ACIONADO
                                       TO TRUE
               MOVE RETURN-CODE        TO MSG1-CD-RTN-PGM-ACND
               MOVE SBDATAF            TO MSG1-NM-PGM-ACND
                                          NM-PGM
               MOVE 0051               TO MSG1-CD-ERRO-TS
               MOVE 'Erro no tratamento de datas.'
                                       TO MSG1-TX-ERRO-TS
                                          MSG1-TX-RTN-PGM-ACND
      *        Monitorar variáveis do módulo chamado
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA

           END-IF

           IF  RETURN-CODE               EQUAL 24
               SET ERRO-PROGRAMA-ACIONADO
                                       TO TRUE
               MOVE 0735               TO MSG1-CD-ERRO-TS
               MOVE 'Data posição saldo inválida.'
                                       TO MSG1-TX-ERRO-TS
                                          MSG1-TX-RTN-PGM-ACND
               MOVE RETURN-CODE        TO MSG1-CD-RTN-PGM-ACND
               MOVE SBDATAF            TO MSG1-NM-PGM-ACND
                                          NM-PGM
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
             WHEN SBDATAF
                  MOVE 'FUNCAO' TO MSGKW999-NM-VRV-MNT
                  MOVE FUNCAO   TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'ARG01'  TO MSGKW999-NM-VRV-MNT
                  MOVE ARG01    TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'ARG02'  TO MSGKW999-NM-VRV-MNT
                  MOVE ARG02    TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'AREA-TAM' TO MSGKW999-NM-VRV-MNT
                  MOVE AREA-TAM   TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'AREA-FIM' TO MSGKW999-NM-VRV-MNT
                  MOVE AREA-FIM   TO MSGKW999-VL-VRV-MNT
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
           MOVE 'SGTS0265-RQSC'        TO NM-CTNR

      *    Recupera o tamanho do container de requisição
           PERFORM 973100-LER-CONTAINER-NODATA

      *--> Obtem o tamanho esperado da requisição
           MOVE  LENGTH OF  SGTS0265-RQSC
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
                     INTO      (  SGTS0265-RQSC )
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
       970101-ABRE-CURSOR-PSQ-1          SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

           EXEC SQL
                OPEN CURSOR-PSQ-1
           END-EXEC
           MOVE 'S'                    TO IN-CURSOR-PSQ-ABTO-1
           .
       970101-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970102-LER-CURSOR-PSQ-1         SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE REG-WS

           EXEC SQL
                FETCH NEXT ROWSET
                      CURSOR-PSQ-1 FOR 0101 ROWS

                INTO  :CD-CTRA-TIT-VL
                     ,:CD-OPR-ETRD-MCP
                     ,:NR-VRS-OPR-ETRD
                     ,:CD-TIT-OPR-ETRD
                     ,:CD-CLI
                     ,:TS-EST-OPR-ETRD
                     ,:NR-IN-OPR-ETRD
           END-EXEC

           MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ-1
           MOVE SQLERRD(3)             TO QT-REG-SELD

           IF  QT-REG-SELD EQUAL 0101
               SET SGTS0265-TEM-MAIS-REGISTRO TO TRUE
               MOVE 0100               TO QT-REG-SELD
           END-IF

           MOVE QT-REG-SELD            TO SGTS0265-QT-OCR-TAB
           MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ-1
           .
       970102-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970103-FECHA-CURSOR-PSQ-1       SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

           MOVE 'N'                    TO IN-CURSOR-PSQ-ABTO-1
           EXEC SQL
                CLOSE CURSOR-PSQ-1
           END-EXEC
           .
       970103-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *                                                                *
      * SECTION DA FAIXA DE 980000 A 989900 PARA GRAVACAO DE DADOS     *
      *                                                                *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       980000-GRAVAR-CTNR-RPST         SECTION.
      *----------------------------------------------------------------*

      *    Exclui eventual container de erro de outros programas
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO

           MOVE 019                    TO MSG1-CD-SEQL-ERRO

      *    Monta o nome do container de resposta
           MOVE 'SGTS0265-RPST'        TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

           EXEC CICS PUT
                     CONTAINER (NM-CTNR                )
                     FROM      (SGTS0265-RPST          )
                     FLENGTH   (LENGTH OF  SGTS0265-RPST)
           END-EXEC

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       980000-SAI.
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
           MOVE 'SGTS0265-CD-CTRA-TIT-VL-E'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0265-CD-CTRA-TIT-VL-E
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0265-CD-OPR-ETRD-MCP-E'
                                       TO  MSGKW999-NM-VRV-MNT
           IF   SGTS0265-CD-OPR-ETRD-MCP-E    NUMERIC
               MOVE   SGTS0265-CD-OPR-ETRD-MCP-E
                                       TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0265-CD-TIT-OPR-ETRD-E'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0265-CD-TIT-OPR-ETRD-E
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0265-CD-CLI-E'    TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0265-CD-CLI-E     TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT
      *--> Paginacao
           MOVE 'SGTS0265-CD-CTRA-TIT-VL-ULT'    TO MSGKW999-NM-VRV-MNT
           MOVE SGTS0265-CD-CTRA-TIT-VL-ULT      TO MCR-S9-15
           MOVE MCR-S9-15                        TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE  'SGTS0265-CD-OPR-ETRD-MCP-ULT'  TO  MSGKW999-NM-VRV-MNT
      *--> Verifica se o conteudo e numerico antes de adicionar
           IF  SGTS0265-CD-OPR-ETRD-MCP-ULT IS NUMERIC
               MOVE  SGTS0265-CD-OPR-ETRD-MCP-ULT TO  MCR-S9-15
               MOVE  MCR-S9-15                   TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico'        TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

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
      *---------------------- Fim  SGTS0265---------------------------*