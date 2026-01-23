      *----------------------------------------------------------------*
      * PROGRAMA..: SGTSL011
      * ANALISTA..: IVONE DE MELO COSTA RIBEIRO
      * AUTOR.....: IVONE DE MELO COSTA RIBEIRO
      * OBJETIVO..: LISTA CONTAINER TABELA RMNC_OPR_ETRD
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS003 25/07/2019 F2804421 DE 2016/145195 - Incluir a condição
      *         no SQL A.DT_INC_TAXA_RMNC   > ou = data de entrada.
      * VRS002 05/02/2019 F9188847 DE 2016/145195 - Inclusão monitoração
      *          variáveis para rotina SBDATAF
      * VRS001 21/10/2016 F4535118 DE 2015/137233 - Implantação
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSL011.

      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
      *-----------------------------------------------------------------
       CONFIGURATION                   SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------
       WORKING-STORAGE                 SECTION.
      *-----------------------------------------------------------------

       77  CTE-INICIO-SS               PIC  X(40)
                                       VALUE '*** S.S. COMEÇA AQUI ***'.
       77  CTE-PROG                    PIC  X(17)
                                       VALUE '*** SGTSL011 ***'.
       77  CTE-VERS                    PIC  X(06) VALUE 'VRS003'.

      *-----------------------------------------------------------------
      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informacoes
-INC HLPKE01B

       77  DATA-MINIMA                 PIC  X(10) VALUE '01010001'.
      *----------------------------------------------------------------*
      *                    Subrotinas utilizadas
      *----------------------------------------------------------------*
      * Valida datas
       77  SBDATAF                     PIC  X(08) VALUE 'SBDATAF'.
      *----------------------------------------------------------------*
      *                     Containers utilizados
      *----------------------------------------------------------------*
      * Container de erro
       77  CTNR-PRM-ERRO               PIC  X(16) VALUE 'PRM-ERRO'.

      * Container do usuário logado

      * ---------------------------------------------------------------

      * Container de requisição desta subrotina
       77  CTNR-SGTSL011-RQSC          PIC  X(16) VALUE 'SGTSL011-RQSC'.

      * CONTAINER de resposta   desta subrotina
       77  CTNR-SGTSL011-RPST          PIC  X(16) VALUE 'SGTSL011-RPST'.
      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------
-INC IIBKERRO
      * Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1
      * book de para acionamento do msg1
-INC MSGKWMG1
-INC HLPKW999

      * ?rea de trabalho
      *--> Auxiliares para movimentação na monitoração de variáveis.
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).

       01  DT-INC-TAXA-RMNC-PSQ        PIC  X(10) VALUE SPACES.
       01  DT-INC-TAXA-RMNC-PSQ-R      REDEFINES DT-INC-TAXA-RMNC-PSQ.
           03  DIA                     PIC  9(02).
           03  MES                     PIC  9(02).
           03  ANO                     PIC  9(04).

       01  QT-REG-SELD                 PIC S9(04) COMP-5 VALUE 0.

      * Campos utilizados para leitura

       01  REG-WS.
           03  DT-INC-TAXA-RMNC        PIC  X(08)
               OCCURS 101 TIMES        VALUE ' '.
           03  TS-ULT-ATL              PIC  X(26)
               OCCURS 101 TIMES        VALUE ' '.
           03  CD-USU-RSP-ATL          PIC  X(08)
               OCCURS 101 TIMES        VALUE ' '.
           03  PC-TAXA-RMNC-OPR        PIC S9(04)V9(10) COMP-3
               OCCURS 101 TIMES        VALUE 0.
           03  DT-FIM-TAXA-RMNC        PIC  X(08)
               OCCURS 101 TIMES        VALUE ' '.
           03  DT-FIM-TAXA-RMNC-N      PIC S9(04) COMP-5
               OCCURS 101 TIMES        VALUE 0.
           03  CD-TIP-FMA-APL          PIC S9(04) COMP-5
               OCCURS 101 TIMES        VALUE 0.
           03  CD-IEC-RMNC             PIC S9(04) COMP-5
               OCCURS 101 TIMES        VALUE 0.
           03  CD-PERC-CPTZ-JUR        PIC S9(04) COMP-5
               OCCURS 101 TIMES        VALUE 0.

      * ?rea para execução da rotina SBDATA - função 01 - Valida data

       01  AREA-SBDATAF                PIC  X(4095).
       01  AREA-SBDATAF-R REDEFINES AREA-SBDATAF.
           03  TAM                     PIC  9(08) COMP.
           03  FILLER                  PIC  X(4071).
           03  FIM                     PIC  X(020).


       01  ARG01                            PIC  9(08) VALUE 0.
       01  ARG01-R REDEFINES ARG01.
         03  DIA                            PIC  9(02).
         03  MES                            PIC  9(02).
         03  ANO                            PIC  9(04).
      *    Valida no formato Gregoriano
       01  ARG02                            PIC  9(01) VALUE 4.

       01  FUC                         PIC  X(03) VALUE 'F01'.

      * Variável(is) de controle do cursor(es)

       01  CTL-CURSOR-PSQ.
           03  CD-FIM-CURSOR-PSQ       PIC S9(04) COMP-5 VALUE 0.
               88  FIM-CURSOR-PSQ                        VALUE +100.
           03  IN-CURSOR-PSQ-ABTO      PIC  X(01)        VALUE 'N'.
               88  CURSOR-PSQ-ABTO                       VALUE 'S'.

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.
      * Book desta subrotina
-INC SGTKL011

           EXEC SQL
             DECLARE CURSOR-PSQ CURSOR
             WITH ROWSET POSITIONING FOR
             SELECT CHAR(TO_CHAR(A.DT_INC_TAXA_RMNC,'DDMMYYYY') , 8)
                  , A.TS_ULT_ATL
                  , A.CD_USU_RSP_ATL
                  , A.PC_TAXA_RMNC_OPR
                  , CHAR(TO_CHAR(A.DT_FIM_TAXA_RMNC,'DDMMYYYY') , 8)
                  , A.CD_TIP_FMA_APL
                  , A.CD_IEC_RMNC
                  , A.CD_PERC_CPTZ_JUR
               FROM DB2SGT.RMNC_OPR_ETRD      A
              WHERE A.CD_CTRA_TIT_VL     = :SGTSL011-CD-CTRA-TIT-VL
                AND A.CD_OPR_ETRD_MCP    = :SGTSL011-CD-OPR-ETRD-MCP
                AND A.NR_VRS_OPR_ETRD    = :SGTSL011-NR-VRS-OPR-ETRD
                AND A.DT_INC_TAXA_RMNC   >= TO_CHAR(TO_DATE(
                   :DT-INC-TAXA-RMNC-PSQ,'DDMMYYYY'), 'DD.MM.YYYY')
              ORDER  BY
                A.DT_INC_TAXA_RMNC
           END-EXEC.

       01  CTE-FINAL-SS                PIC  X(40) VALUE
                                            '*** S.S. TERMINA AQUI ***'.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------
      * Habilita tratamento automatico de erros DB2
-INC HLPKE997
      * Habilita captura automatica de abend e erros CICS
-INC HLPKE999

           PERFORM 100000-DESCOBRIR-CANAL
           PERFORM 110000-EXCLUIR-CTNR-RPST
           PERFORM 120000-EXCLUIR-CTNR-ERRO
           PERFORM 130000-RECEBER-REQUISICAO
           PERFORM 140000-VALIDAR-PARAMETRO

           PERFORM 970101-ABRE-CURSOR-PSQ
           PERFORM 970102-LER-CURSOR-PSQ

           PERFORM 200000-PROCESSA-PESQUISA
               VARYING SGTSL011-IC-TAB FROM 1 BY 1
               UNTIL   SGTSL011-IC-TAB GREATER SGTSL011-QT-OCR-TAB

           PERFORM 970103-FECHA-CURSOR-PSQ
           PERFORM 300000-POSTA-RESPOSTA

           GOBACK
           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-DESCOBRIR-CANAL         SECTION.
      *-----------------------------------------------------------------
           MOVE 0001                   TO MSG1-CD-SEQL-ERRO

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CANAL             TO TRUE

           EXEC CICS ASSIGN
                     CHANNEL ( NM-CNL )
           END-EXEC

      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-EXCLUIR-CTNR-RPST        SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO

           SET DEL-CONTAINER           TO TRUE
           MOVE CTNR-SGTSL011-RPST     TO NM-CTNR

           EXEC CICS DELETE
                     CONTAINER ( NM-CTNR )
                     CHANNEL   ( NM-CNL  )
                     NOHANDLE
           END-EXEC
           IF  NOT (EIBRESP            EQUAL DFHRESP(NORMAL)
           OR       EIBRESP            EQUAL DFHRESP(CONTAINERERR) )
               SET ERRO-CONTAINER      TO TRUE
               PERFORM 999002-TRATA-CONDITION-CICS
           END-IF
           .
       110000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       120000-EXCLUIR-CTNR-ERRO        SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                   TO MSG1-CD-SEQL-ERRO

      *    Limpar variáveis container de erro
           INITIALIZE IIBKERRO-PRM-ERRO

      *    Exclui o container de erro
           MOVE CTNR-PRM-ERRO          TO NM-CTNR
           SET DEL-CONTAINER           TO TRUE
           EXEC CICS DELETE
                     CONTAINER ( NM-CTNR )
                     CHANNEL   ( NM-CNL  )
                     NOHANDLE
           END-EXEC

           IF  NOT (EIBRESP            EQUAL DFHRESP(NORMAL)
           OR       EIBRESP            EQUAL DFHRESP(CONTAINERERR) )
               SET ERRO-CONTAINER      TO TRUE
               PERFORM 999002-TRATA-CONDITION-CICS
           END-IF
           .
       120000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       130000-RECEBER-REQUISICAO       SECTION.
      *-----------------------------------------------------------------
           MOVE 0004                   TO MSG1-CD-SEQL-ERRO

      *    Monta o nome do container de requisição
           MOVE CTE-PROG(5:8)          TO NM-PGM
           MOVE CTNR-SGTSL011-RQSC     TO NM-CTNR

      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Obtém a quantidade de caracteres no container
           MOVE ZEROS                  TO QT-CCT-LIDO

           EXEC CICS GET
                     CONTAINER ( NM-CTNR     )
                     CHANNEL   ( NM-CNL      )
                     NODATA
                     FLENGTH   ( QT-CCT-LIDO )
           END-EXEC

      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE

           MOVE LENGTH OF SGTSL011-RQSC
                                       TO QT-CCT-EPRO

      *    Valida tamanho esperado da requisição X tamanho recebido
           IF  QT-CCT-LIDO             NOT EQUAL QT-CCT-EPRO
               SET ERRO-TAMANHO-AREA   TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

           EXEC CICS GET
                     CONTAINER ( NM-CTNR       )
                     CHANNEL   ( NM-CNL        )
                     FLENGTH   ( QT-CCT-LIDO   )
                     INTO      ( SGTSL011-RQSC )
           END-EXEC

      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       130000-SAI.
           EXIT.

      *---------------------------------------------------------------
       140000-VALIDAR-PARAMETRO        SECTION.
      *---------------------------------------------------------------
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

           MOVE 'N'                    TO SGTSL011-IN-CTNC-PSQ

           IF  SGTSL011-CD-CTRA-TIT-VL EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0002               TO MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de título e valores monetário nã
      -             'o informado.'     TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL011-CD-OPR-ETRD-MCP
                                       NOT NUMERIC
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0003               TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -        ' numérico.'            TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL011-CD-OPR-ETRD-MCP
                                       EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0005               TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -        ' informado.'           TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL011-NR-VRS-OPR-ETRD EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0007               TO MSG1-CD-ERRO-TS
               MOVE 'Número da versão da operação não informado.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL011-DT-INC-TAXA-ULT
                                       EQUAL SPACES OR ZEROS
               MOVE DATA-MINIMA        TO DT-INC-TAXA-RMNC-PSQ
           ELSE
               PERFORM 150000-CHAMAR-SBDATAF-F01

               MOVE CORR SGTSL011-DT-INC-TAXA-ULT-R
                                       TO DT-INC-TAXA-RMNC-PSQ-R
           END-IF
           .
       140000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       150000-CHAMAR-SBDATAF-F01        SECTION.
      *----------------------------------------------------------------*
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

           MOVE  CORR SGTSL011-DT-INC-TAXA-ULT-R
                                       TO ARG01-R
           MOVE  4095                    TO TAM
           MOVE  'SBDATAF -WRKAREA-FIM'  TO FIM

           CALL SBDATAF                USING FUC
                                             AREA-SBDATAF
                                             ARG01
                                             ARG02

           IF  RETURN-CODE             EQUAL 24
               SET  ERRO-PROGRAMA-ACIONADO    TO TRUE
               MOVE 0164               TO MSG1-CD-ERRO-TS
               MOVE 'Data início taxa remuneração de rechamada inválida.
      -             ''                 TO MSG1-TX-ERRO-TS
                                          MSG1-TX-RTN-PGM-ACND
               MOVE  RETURN-CODE       TO MSG1-CD-RTN-PGM-ACND
               MOVE  SBDATAF           TO MSG1-NM-PGM-ACND

               MOVE 'FUC'               TO  MSGKW999-NM-VRV-MNT
               MOVE  FUC                TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'TAM'             TO MSGKW999-NM-VRV-MNT
               MOVE  TAM              TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'FIM'             TO MSGKW999-NM-VRV-MNT
               MOVE  FIM              TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'ARG01'           TO  MSGKW999-NM-VRV-MNT
               MOVE  ARG01            TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'ARG02'           TO  MSGKW999-NM-VRV-MNT
               MOVE  ARG02            TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           ELSE
               IF  RETURN-CODE         NOT EQUAL ZEROS
                   SET  ERRO-PROGRAMA-ACIONADO
                                       TO TRUE
                   MOVE 0051           TO MSG1-CD-ERRO-TS
                   MOVE 'Erro no tratamento de datas.'
                                       TO MSG1-TX-RTN-PGM-ACND
                                          MSG1-TX-ERRO-TS
                   MOVE SBDATAF        TO MSG1-NM-PGM-ACND
                   MOVE RETURN-CODE    TO MSG1-CD-RTN-PGM-ACND

                   MOVE 'FUC'               TO  MSGKW999-NM-VRV-MNT
                   MOVE  FUC                TO  MSGKW999-VL-VRV-MNT
                   PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'TAM'             TO MSGKW999-NM-VRV-MNT
                  MOVE  TAM              TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'FIM'             TO MSGKW999-NM-VRV-MNT
                  MOVE  FIM              TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'ARG01'           TO  MSGKW999-NM-VRV-MNT
                  MOVE  ARG01            TO  MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'ARG02'           TO  MSGKW999-NM-VRV-MNT
                  MOVE  ARG02            TO  MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT
                  PERFORM 999000-GRAVA-ERRO-E-ENCERRA
               END-IF
           END-IF
           .
       150000-SAI.
           EXIT.

      *---------------------------------------------------------------
       200000-PROCESSA-PESQUISA        SECTION.
      *---------------------------------------------------------------
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

           MOVE DT-INC-TAXA-RMNC          (SGTSL011-IC-TAB)
             TO SGTSL011-DT-INC-TAXA-RMNC (SGTSL011-IC-TAB)
           MOVE TS-ULT-ATL                (SGTSL011-IC-TAB)
             TO SGTSL011-TS-ULT-ATL       (SGTSL011-IC-TAB)
           MOVE CD-USU-RSP-ATL            (SGTSL011-IC-TAB)
             TO SGTSL011-CD-USU-RSP-ATL   (SGTSL011-IC-TAB)
           MOVE PC-TAXA-RMNC-OPR          (SGTSL011-IC-TAB)
             TO SGTSL011-PC-TAXA-RMNC-OPR (SGTSL011-IC-TAB)
           IF   DT-FIM-TAXA-RMNC-N        (SGTSL011-IC-TAB) EQUAL -1
                MOVE SPACES            TO  SGTSL011-DT-FIM-TAXA-RMNC
                                                       (SGTSL011-IC-TAB)
           ELSE MOVE DT-FIM-TAXA-RMNC     (SGTSL011-IC-TAB)
                                       TO  SGTSL011-DT-FIM-TAXA-RMNC
                                                       (SGTSL011-IC-TAB)
           END-IF
           MOVE CD-TIP-FMA-APL            (SGTSL011-IC-TAB)
             TO SGTSL011-CD-TIP-FMA-APL   (SGTSL011-IC-TAB)
           MOVE CD-IEC-RMNC               (SGTSL011-IC-TAB)
             TO SGTSL011-CD-IEC-RMNC      (SGTSL011-IC-TAB)
           MOVE CD-PERC-CPTZ-JUR          (SGTSL011-IC-TAB)
             TO SGTSL011-CD-PERC-CPTZ-JUR (SGTSL011-IC-TAB)
           .
       200000-SAI.
           EXIT.

      *----------------------------------------------------------------
       300000-POSTA-RESPOSTA           SECTION.
      *----------------------------------------------------------------
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

      *    Monta o nome do container de resposta
           MOVE CTNR-SGTSL011-RPST     TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

           EXEC CICS PUT
                     CONTAINER ( NM-CTNR                 )
                     CHANNEL   ( NM-CNL                  )
                     FLENGTH   ( LENGTH OF SGTSL011-RPST )
                     FROM      ( SGTSL011-RPST           )
           END-EXEC

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       300000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970101-ABRE-CURSOR-PSQ          SECTION.
      *-----------------------------------------------------------------
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO

           EXEC SQL
                OPEN CURSOR-PSQ
           END-EXEC

           MOVE 'S'                    TO IN-CURSOR-PSQ-ABTO
           .
       970101-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970102-LER-CURSOR-PSQ           SECTION.
      *-----------------------------------------------------------------
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE REG-WS
           EXEC SQL
                FETCH NEXT ROWSET
                CURSOR-PSQ FOR 101 ROWS
               INTO :DT-INC-TAXA-RMNC
                  , :TS-ULT-ATL
                  , :CD-USU-RSP-ATL
                  , :PC-TAXA-RMNC-OPR
                  , :DT-FIM-TAXA-RMNC
                    :DT-FIM-TAXA-RMNC-N
                  , :CD-TIP-FMA-APL
                  , :CD-IEC-RMNC
                  , :CD-PERC-CPTZ-JUR
           END-EXEC

           MOVE SQLERRD(3)             TO QT-REG-SELD

           IF  QT-REG-SELD EQUAL 0101
               SET SGTSL011-TEM-MAIS-REGISTRO TO TRUE
               MOVE 0100               TO QT-REG-SELD
           END-IF

           MOVE QT-REG-SELD            TO SGTSL011-QT-OCR-TAB
           MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ
           .
       970102-SAI.
           EXIT.

      *----------------------------------------------------------------
       970103-FECHA-CURSOR-PSQ         SECTION.
      *----------------------------------------------------------------
           MOVE 0011                   TO MSG1-CD-SEQL-ERRO

           MOVE 'N'                    TO IN-CURSOR-PSQ-ABTO
           EXEC SQL
               CLOSE CURSOR-PSQ
           END-EXEC
           .
       970103-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------
           IF  IN-CURSOR-PSQ-ABTO  EQUAL  'S'
               PERFORM 970103-FECHA-CURSOR-PSQ
           END-IF

           IF  MSG1-CD-ERRO-TS EQUAL ZEROS
               MOVE 8888                TO MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS EQUAL SPACES
               MOVE 'Erro não especificado no módulo.'
                                        TO MSG1-TX-ERRO-TS
           END-IF

      *Verifica se o tipo de erro gerado possui tratamento interno
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

           MOVE 'SGTSL011-CD-CTRA-TIT-VL'        TO MSGKW999-NM-VRV-MNT
           MOVE  SGTSL011-CD-CTRA-TIT-VL         TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL011-CD-OPR-ETRD-MCP'       TO MSGKW999-NM-VRV-MNT
      *------> Verifica se o conteudo e numerico antes de adicionar
           IF  SGTSL011-CD-OPR-ETRD-MCP    IS NUMERIC
               MOVE SGTSL011-CD-OPR-ETRD-MCP     TO CD-OPR-ETRD-MCP
               MOVE CD-OPR-ETRD-MCP-TX           TO MSGKW999-VL-VRV-MNT
           ELSE
               MOVE 'Valor não numérico.'        TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL011-NR-VRS-OPR-ETRD'       TO MSGKW999-NM-VRV-MNT
           MOVE  SGTSL011-NR-VRS-OPR-ETRD        TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL011-DT-INC-TAXA-ULT'       TO MSGKW999-NM-VRV-MNT
           MOVE  SGTSL011-DT-INC-TAXA-ULT        TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

      * --> grava o erro no msg
           PERFORM MSG1-NTF-MSG

           MOVE MSG1-CD-ERRO-TS        TO IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS        TO IIBKERRO-TX-ERRO

           EXEC CICS PUT
                     CONTAINER ( CTNR-PRM-ERRO               )
                     CHANNEL   ( NM-CNL                      )
                     FROM      ( IIBKERRO-PRM-ERRO           )
                     FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                     NOHANDLE
           END-EXEC

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
      * 999010-TRATA-ERRO-DB2          SECTION.
-INC HLPKP997
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
      * --------------------------------------*
       999999-ENCERRAR                 SECTION.
      * --------------------------------------*
           GOBACK
           .
       999999-SAI.
           EXIT.
      *---------------------- FIM SGTSL011 ----------------------------*