      *-----------------------------------------------------------------
      * PROGRAMA..: SGTS0264
      * ANALISTA..: F0950413 - António Francisco Mendonça Silva
      * AUTOR.....: F0950413 - António Francisco Mendonça Silva
      * DATA......: 27.08.2019
      * OBJETIVO..: Lista de operaçoes com impacto manual ativo.
      * TAB.DB2...: IFLC_MNL_LIM_EXCO, OPR_ETRD_MCP, EST_OPR_ETRD,
      *             EST_VRS_OPR_ETRD e CLI_OPR_ETRD
      * COMPILACAO: COBOL/CICS/DB2 (On-line)
      *-----------------------------------------------------------------
      * VRS001 21.10.2019   F2804421 DE 2016/145195 - Incluir ORDER BY.
      *        15.10.2019 - F2804421 DE 2016/145195 - Acerto no SQL.
      *        27.08.2019 - F0950413 - 2016/145195 - Implantação
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTS0264.

      *-----------------------------------------------------------------
       ENVIRONMENT                     DIVISION.
      *-----------------------------------------------------------------
       CONFIGURATION                   SECTION.
      *-----------------------------------------------------------------
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *-----------------------------------------------------------------
       DATA                            DIVISION.
      *-----------------------------------------------------------------
       WORKING-STORAGE                 SECTION.
      *-----------------------------------------------------------------
       77  CTE-INICIO-WS               PIC X(30) VALUE
                                       '*** WSS COMECA AQUI ***'.
       77  CTE-PROG                    PIC X(18) VALUE
                                       '*** SGTS0264 ***'.
       77  CTE-VERS                    PIC X(06) VALUE 'VRS001'.

      *--> Subrotinas Utilizadas
       77  SBDATAF                     PIC X(08) VALUE 'SBDATAF '.

      *--> Container de erro
       77  CTNR-PRM-ERRO               PIC X(16) VALUE 'PRM-ERRO'.

      *-----------------------------------------------------------------
      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informacoes
      *-----------------------------------------------------------------
-INC HLPKE01B

       01  CTE-FIM-WS                  PIC  X(30)  VALUE
                                       '*** WSS TERMINA AQUI ***'.
      *-----------------------------------------------------------------
       LOCAL-STORAGE SECTION.
      *-----------------------------------------------------------------
       01  CTE-INICIO-LS               PIC X(30)  VALUE
                                       '*** LS COMECA AQUI   ***'.
      *--> Areas de trabalho
       01  IC-TAB                      PIC S9(04)     COMP-5 VALUE 0.
       01  QT-REG-SELD                 PIC S9(04)     COMP-5 VALUE 0.

      *--> Area da subrotina SBDATAF
       01  FUNCAO                        PIC  X(3) VALUE 'F01'.
       01  AREA-WORK                     PIC  X(4095).
       01  AREA-WORK-RED  REDEFINES  AREA-WORK.
           03 A-TAM                      PIC  9(0008) COMP.
           03 FILLER                     PIC  X(4071).
           03 A-FIM                      PIC  X(0020).
       01  ARG01                         PIC  9(8) VALUE 0.
       01  ARG01-R REDEFINES ARG01.
           03  DIA                       PIC  9(2).
           03  MES                       PIC  9(2).
           03  ANO                       PIC  9(4).
       01  ARG02                         PIC  9(1) VALUE 4.

       01 MSG-SBDATAF                       PIC X(30).
       01 MSG-SBDATAF-R  REDEFINES MSG-SBDATAF.
          03 RTN-SBDATAF                    PIC S9(04) COMP.

      *--> Máscaras edição de variáveis p/ monitoração mensagens de erro
       01  MCR-S9-15                   PIC      -999999999999999.

      *--> Variáveis de pesquisa.
       01  EMISSOR             PIC S9(04) COMP-5 VALUE 1.
       01  OPR-APVD            PIC S9(09) COMP-5 VALUE 5.
       01  VRS-APVD            PIC S9(04) COMP-5 VALUE 3.
       01  EXCO-ATIVA          PIC  X(01)        VALUE 'S'.
       01  DT-VLDO             PIC  X(10)        VALUE '9999-12-31'.
       01  CD-CTRA-TIT-VL-INC  PIC S9(09) COMP-5 VALUE 0.
       01  CD-CTRA-TIT-VL-FIM  PIC S9(09) COMP-5 VALUE 999999999.
       01  CD-OPR-ETRD-MCP-INC PIC S9(11) COMP-3 VALUE 0.
       01  CD-OPR-ETRD-MCP-FIM PIC S9(11) COMP-3 VALUE 99999999999.
       01  CD-CLI-INC          PIC S9(09) COMP-5 VALUE 0.
       01  CD-CLI-FIM          PIC S9(09) COMP-5 VALUE 999999999.
       01  DT-IMP-PSC-SDO      PIC  X(10)        VALUE '01.01.0001'.
       01  HR-IMP-PSC-SDO      PIC  X(08)        VALUE '00:00:00'.

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
           03 CD-CTRA-TIT-VL           PIC S9(09)            COMP-5
                                       OCCURS 101 TIMES      VALUE 0.
           03 CD-OPR-ETRD-MCP          PIC S9(11)            COMP-3
                                       OCCURS 101 TIMES      VALUE 0.
           03 NR-VRS-OPR-ETRD          PIC S9(04)            COMP-5
                                       OCCURS 101 TIMES      VALUE 0.
           03 CD-TIT-OPR-ETRD          PIC  X(12)
                                       OCCURS 101 TIMES      VALUE ' '.
           03 CD-CLI                   PIC S9(09)            COMP-5
                                       OCCURS 101 TIMES      VALUE 0.
           03 VL-CPR-OPR               PIC S9(15)V9(2)       COMP-3
                                       OCCURS 101 TIMES      VALUE 0.
           03 VL-SDO-TIT-DT-PSC        PIC S9(15)V9(2)       COMP-3
                                       OCCURS 101 TIMES      VALUE 0.
           03 VL-SDO-TIT-DT-PSC-N      PIC S9(4) COMP-5
                                       OCCURS 101 TIMES      VALUE 0.
           03 TS-EST-OPR-ETRD          PIC  X(26)
                                       OCCURS 101 TIMES      VALUE ' '.
           03 TS-ATL-IFLC-MNL          PIC  X(26)
                                       OCCURS 101 TIMES      VALUE ' '.

      *--> Variavel(is) de controle do cursor(es)
       01  CTL-CURSOR-PSQ-1.
           03  CD-FIM-CURSOR-PSQ-1     PIC S9(04)     COMP-5 VALUE 0.
               88  FIM-CURSOR-PSQ-1                          VALUE +100.
           03  IN-CURSOR-PSQ-ABTO-1    PIC  X(01)            VALUE 'N'.
               88  CURSOR-PSQ-ABTO-1   VALUE 'S'.

      *-----------------------------------------------------------------
      * Books dos Containers e das subrotinas chamadas
      *-----------------------------------------------------------------
      *--> Book do container de erro
-INC IIBKERRO

      *--> Book para captura de erro via MSG1
-INC MSGKWMG1

      *--> Variaveis para tratamento de Abend e Condition
-INC HLPKW999

      *--> Variaveis para tratamento de CONTAINER
-INC SGTKERR1

      *--> Book do container de requisição e resposta deste módulo
       01  SGTS0264-DADOS.
-INC SGTK0264

      *--> Area de trabalho SQL/ DB2.
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

      *--> Declaração do cursor para pesquisa da lista de operaçoes com
      *--> impacto manual ativo
           EXEC SQL
              DECLARE CURSOR-PSQ-1 CURSOR
                      WITH ROWSET POSITIONING FOR
              SELECT X.CD_CTRA_TIT_VL
                    ,X.CD_OPR_ETRD_MCP
                    ,X.NR_VRS_OPR_ETRD
                    ,B.CD_TIT_OPR_ETRD
                    ,C.CD_CLI
                    ,B.VL_CPR_OPR
                    ,IFNULL((G.VL_SDO_TIT_DT_PSC), 0)
                    ,MAX(X.TS_EST_OPR_ETRD)
                    ,MAX(A.TS_ATL_IFLC_MNL)
              FROM DB2SGT.IFLC_MNL_LIM_EXCO A
              INNER JOIN DB2SGT.OPR_ETRD_MCP B
                    ON   B.CD_CTRA_TIT_VL    =  A.CD_CTRA_TIT_VL
                   AND   B.CD_TIT_OPR_ETRD   =  A.CD_TIT_OPR_ETRD
                   AND   B.NR_DVS_TIT        =  A.NR_DVS_TIT
              INNER JOIN DB2SGT.EST_OPR_ETRD X
                    ON   X.CD_CTRA_TIT_VL     =  B.CD_CTRA_TIT_VL
                   AND   X.CD_OPR_ETRD_MCP    =  B.CD_OPR_ETRD_MCP
                   AND   X.NR_VRS_OPR_ETRD    =  B.NR_VRS_OPR_ETRD
              INNER JOIN DB2SGT.CLI_OPR_ETRD C
                    ON   C.CD_CTRA_TIT_VL     =  X.CD_CTRA_TIT_VL
                   AND   C.CD_OPR_ETRD_MCP    =  X.CD_OPR_ETRD_MCP
                   AND   C.NR_VRS_OPR_ETRD    =  X.NR_VRS_OPR_ETRD
              LEFT OUTER JOIN
                   (SELECT   H.VL_SDO_TIT_DT_PSC
                            ,H.CD_CTRA_TIT_VL
                            ,H.CD_TIT_OPR_ETRD
                            ,H.DT_PSC
                      FROM  DB2SGT.PSC_DRIA_SDO_OPR H
                       WHERE H.DT_PSC         = :AUX-DATA-PSC
                         AND H.DT_IMP_PSC_SDO = :DT-IMP-PSC-SDO
                         AND H.HR_IMP_PSC_SDO = :HR-IMP-PSC-SDO
                      ORDER BY
                             H.CD_CTRA_TIT_VL ASC
                            ,H.CD_TIT_OPR_ETRD ASC
                            ,H.DT_IMP_PSC_SDO DESC
                            ,H.HR_IMP_PSC_SDO DESC) G
                   ON
                        G.CD_CTRA_TIT_VL   = X.CD_CTRA_TIT_VL
                   AND  G.CD_TIT_OPR_ETRD  = B.CD_TIT_OPR_ETRD
             WHERE
                  A.IN_IFLC_MNL_EXCO       = :EXCO-ATIVA
             AND  DATE(A.TS_ATL_IFLC_MNL) <= :AUX-DATA-PSC
             AND  DATE(X.TS_EST_OPR_ETRD) <= :AUX-DATA-PSC
             AND  X.CD_TIP_EST_OPR   = :OPR-APVD
             AND  X.NR_VRS_OPR_ETRD  =
                 (SELECT   MAX(Y.NR_VRS_OPR_ETRD)
                    FROM DB2SGT.EST_VRS_OPR_ETRD Y
                   WHERE   Y.CD_CTRA_TIT_VL  = X.CD_CTRA_TIT_VL
                     AND   Y.CD_OPR_ETRD_MCP = X.CD_OPR_ETRD_MCP
                     AND DATE(Y.TS_EST_VRS) <= :AUX-DATA-PSC
                     AND   Y.CD_TIP_EST_VRS  = :VRS-APVD)
             AND  C.CD_TIP_FUC_CLI    = :EMISSOR
             AND  C.DT_FIM_VLD_FUC    = :DT-VLDO
      *--> filtros
             AND  X.CD_CTRA_TIT_VL    >= :CD-CTRA-TIT-VL-INC
             AND  X.CD_CTRA_TIT_VL    <= :CD-CTRA-TIT-VL-FIM
             AND  X.CD_OPR_ETRD_MCP   >= :CD-OPR-ETRD-MCP-INC
             AND  X.CD_OPR_ETRD_MCP   <= :CD-OPR-ETRD-MCP-FIM
             AND  UPPER(COALESCE(B.CD_TIT_OPR_ETRD, ' ')) LIKE
                  UPPER(:SGTS0264-CD-TIT-OPR-ETRD-E)
             AND   C.CD_CLI           >= :CD-CLI-INC
             AND   C.CD_CLI           <= :CD-CLI-FIM
      *--> paginacao
             AND (
                 (X.CD_CTRA_TIT_VL      > :SGTS0264-CD-CTRA-TIT-VL-ULT)
               OR  (X.CD_CTRA_TIT_VL    = :SGTS0264-CD-CTRA-TIT-VL-ULT
                 AND X.CD_OPR_ETRD_MCP  > :SGTS0264-CD-OPR-ETRD-MCP-ULT)
                  )
             GROUP BY
                 X.CD_CTRA_TIT_VL
                ,X.CD_OPR_ETRD_MCP
                ,X.NR_VRS_OPR_ETRD
                ,B.CD_TIT_OPR_ETRD
                ,C.CD_CLI
                ,B.VL_CPR_OPR
                ,G.VL_SDO_TIT_DT_PSC
             ORDER BY
                 X.CD_CTRA_TIT_VL
                ,X.CD_OPR_ETRD_MCP
           END-EXEC.

       01  CTE-FIM-LS                  PIC X(30)  VALUE
                                       '*** LS TERMINA AQUI  ***'.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------
           MOVE 0001                   TO MSG1-CD-SEQL-ERRO

      *--> Habilita tratamento automatico de Erros DB2
-INC HLPKE997
      *--> Habilita captura automatica de abend e erros CICS
-INC HLPKE999
           MOVE CTE-PROG(5:8)          TO NM-PGM

           PERFORM 100000-OBTER-NOME-DO-CANAL
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO
           PERFORM 970000-LER-REQUISICAO-RECEBIDA

           PERFORM 110000-VALIDAR-REQUISICAO
           PERFORM 120000-BUSCAR-DT-HR-IMPORTACAO

           PERFORM 970101-ABRE-CURSOR-PSQ-1
           PERFORM 970102-LER-CURSOR-PSQ-1
           PERFORM 970103-FECHA-CURSOR-PSQ-1

           PERFORM 200000-PROCESSA-PESQUISA
                   VARYING IC-TAB FROM 1 BY 1
                     UNTIL IC-TAB > SGTS0264-QT-OCR-TAB

           PERFORM 980000-GRAVAR-CTNR-RESPOSTA
           PERFORM 999999-ENCERRAR

           GOBACK
           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-OBTER-NOME-DO-CANAL      SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO

      *--> Por causa da captura automática de erro CICS, deve ser
      *--> previsto o erro de container
           SET  ERRO-CANAL             TO TRUE

           EXEC CICS ASSIGN
              CHANNEL ( NM-CNL  )

              END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-VALIDAR-REQUISICAO       SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                   TO MSG1-CD-SEQL-ERRO

      *--> Data da posição de saldo para pesquisa
           IF  SGTS0264-DT-PSC-E  EQUAL  SPACES
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 1068               TO MSG1-CD-ERRO-TS
               MOVE 'Data para efetuar a pesquisa não informada.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           ELSE
               PERFORM 920000-VALIDAR-DATA-INFORMADA
           END-IF
           MOVE CORR SGTS0264-DT-PSC-E-R        TO  AUX-DATA-PSC-R

      *--> Carteira
           IF  SGTS0264-CD-CTRA-TIT-VL-E  <  0
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0001               TO MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de títulos e valores mobiliários
      -             ' inválido.'       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0264-CD-CTRA-TIT-VL-E  GREATER  ZEROS
               MOVE SGTS0264-CD-CTRA-TIT-VL-E TO  CD-CTRA-TIT-VL-INC
                                                  CD-CTRA-TIT-VL-FIM
           END-IF

      *--> Operação
           IF  SGTS0264-CD-OPR-ETRD-MCP-E NOT NUMERIC
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0003               TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -             ' numérico.'       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           ELSE
            IF SGTS0264-CD-OPR-ETRD-MCP-E < 0
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0004               TO MSG1-CD-ERRO-TS
               MOVE 'Código da operação estruturada do mercado de capita
      -             'is inválido.'     TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
            END-IF
           END-IF

           IF  SGTS0264-CD-OPR-ETRD-MCP-E  GREATER  ZEROS
               MOVE SGTS0264-CD-OPR-ETRD-MCP-E TO CD-OPR-ETRD-MCP-INC
                                                  CD-OPR-ETRD-MCP-FIM
           END-IF

      *--> Título - trocar brancos por % para pesquisa
           INSPECT SGTS0264-CD-TIT-OPR-ETRD-E
                                    REPLACING ALL SPACES BY '%'
      *--> Cliente emissor
           IF  SGTS0264-CD-CLI-E    < 0
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0028              TO  MSG1-CD-ERRO-TS
               MOVE  'Código cliente emissor inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF   SGTS0264-CD-CLI-E      GREATER  ZEROS
                MOVE SGTS0264-CD-CLI-E TO CD-CLI-INC
                                          CD-CLI-FIM
           END-IF

      *--- campos paginacao ---------------------
      *--> Carteira-paginacao
           IF  SGTS0264-CD-CTRA-TIT-VL-ULT < 0
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0001               TO MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de títulos e valores mobiliários
      -             ' inválido.'       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *--> Operação-paginacao
           IF  SGTS0264-CD-OPR-ETRD-MCP-ULT NOT NUMERIC
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0003               TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -             ' numérico.'       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           ELSE
            IF SGTS0264-CD-OPR-ETRD-MCP-ULT < 0
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0004               TO MSG1-CD-ERRO-TS
               MOVE 'Código da operação estruturada do mercado de capita
      -             'is inválido.'     TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
            END-IF
           END-IF

           MOVE 'N'                    TO SGTS0264-IN-CTNC-PSQ
           .
       110000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       120000-BUSCAR-DT-HR-IMPORTACAO  SECTION.
      *-----------------------------------------------------------------
           MOVE 0004                   TO MSG1-CD-SEQL-ERRO
      *--> A partir da data da posição para pesquisa, buscar a ultima
      *--> data e hora da importação do saldo, pois podem existir mais
      *--> de uma importação para uma data de posição de saldo.

           EXEC SQL
             SELECT   A.DT_IMP_PSC_SDO
                     ,A.HR_IMP_PSC_SDO
               INTO   :DT-IMP-PSC-SDO
                     ,:HR-IMP-PSC-SDO
               FROM   DB2SGT.PSC_DRIA_SDO_OPR A
               WHERE  A.DT_PSC    = :AUX-DATA-PSC
               ORDER BY
                      A.DT_IMP_PSC_SDO DESC
                     ,A.HR_IMP_PSC_SDO DESC
             FETCH FIRST 1 ROW ONLY
           END-EXEC

           IF  SQLCODE = +100
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 1070               TO  MSG1-CD-ERRO-TS
               MOVE 'Data posição de saldo não encontrada.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       120000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-PROCESSA-PESQUISA        SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

      *--> Tratamento dos registros retornados.
           MOVE CD-CTRA-TIT-VL   (IC-TAB)
                                  TO SGTS0264-CD-CTRA-TIT-VL    (IC-TAB)
           MOVE CD-OPR-ETRD-MCP   (IC-TAB)
                                  TO SGTS0264-CD-OPR-ETRD-MCP   (IC-TAB)
           MOVE NR-VRS-OPR-ETRD   (IC-TAB)
                                  TO SGTS0264-NR-VRS-OPR-ETRD   (IC-TAB)
           MOVE CD-TIT-OPR-ETRD   (IC-TAB)
                                  TO SGTS0264-CD-TIT-OPR-ETRD   (IC-TAB)
           MOVE CD-CLI            (IC-TAB)
                                  TO SGTS0264-CD-CLI            (IC-TAB)
           MOVE VL-CPR-OPR        (IC-TAB)
                                  TO SGTS0264-VL-CPR            (IC-TAB)
           MOVE VL-SDO-TIT-DT-PSC (IC-TAB)
                                  TO SGTS0264-VL-SDO-TIT-DT-PSC (IC-TAB)
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       920000-VALIDAR-DATA-INFORMADA   SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

           MOVE 4095                   TO A-TAM
           MOVE 'SBDATAF -WRKAREA-FIM' TO A-FIM

           MOVE  CORR SGTS0264-DT-PSC-E-R
                                       TO ARG01-R

           MOVE  ZEROS                 TO RTN-SBDATAF
           CALL SBDATAF                USING FUNCAO AREA-WORK ARG01
                                             ARG02
           IF  RETURN-CODE             NOT EQUAL  ZEROS  AND
                                       0022  AND  0024   AND 0046
               SET  ERRO-PROGRAMA-ACIONADO
                                       TO TRUE
               MOVE SBDATAF            TO NM-PGM
                                          MSG1-NM-PGM-ACND
               MOVE RETURN-CODE        TO MSG1-CD-RTN-PGM-ACND
               MOVE 0051               TO MSG1-CD-ERRO-TS
               MOVE 'Erro no tratamento de datas.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  RETURN-CODE          EQUAL 022 OR 0024 OR 0046
               SET  ERRO-PROGRAMA-ACIONADO
                                       TO TRUE
               MOVE SBDATAF            TO NM-PGM
                                          MSG1-NM-PGM-ACND
               MOVE RETURN-CODE        TO MSG1-CD-RTN-PGM-ACND
               MOVE 0049               TO MSG1-CD-ERRO-TS
               MOVE 'Data informada inválida.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       920000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970000-LER-REQUISICAO-RECEBIDA  SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

           MOVE 'SGTS0264-RQSC'             TO NM-CTNR
           MOVE ZEROS                       TO QT-CCT-LIDO
           MOVE LENGTH OF SGTS0264-RQSC     TO QT-CCT-EPRO
           SET  GET-CONTAINER               TO TRUE
           SET  ERRO-CONTAINER              TO TRUE

           EXEC CICS
                GET  CONTAINER ( NM-CTNR     )
                     CHANNEL   ( NM-CNL      )
                     NODATA
                     FLENGTH   ( QT-CCT-LIDO )
           END-EXEC

           IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
               SET  ERRO-TAMANHO-AREA  TO  TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS
                GET  CONTAINER ( NM-CTNR                 )
                     CHANNEL   ( NM-CNL                  )
                     FLENGTH   ( LENGTH OF SGTS0264-RQSC )
                     INTO      ( SGTS0264-RQSC           )
           END-EXEC

      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       970000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970101-ABRE-CURSOR-PSQ-1        SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

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
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE REG-WS

           EXEC SQL
                FETCH NEXT ROWSET
                      CURSOR-PSQ-1 FOR 0101 ROWS
                INTO :CD-CTRA-TIT-VL
                    ,:CD-OPR-ETRD-MCP
                    ,:NR-VRS-OPR-ETRD
                    ,:CD-TIT-OPR-ETRD
                    ,:CD-CLI
                    ,:VL-CPR-OPR
                    ,:VL-SDO-TIT-DT-PSC
                     :VL-SDO-TIT-DT-PSC-N
                    ,:TS-EST-OPR-ETRD
                    ,:TS-ATL-IFLC-MNL

           END-EXEC

           MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ-1
           MOVE SQLERRD(3)             TO QT-REG-SELD

           IF  QT-REG-SELD EQUAL 0101
               SET SGTS0264-TEM-MAIS-REGISTRO TO TRUE
               MOVE 0100               TO QT-REG-SELD
           END-IF

           MOVE QT-REG-SELD            TO SGTS0264-QT-OCR-TAB
           MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ-1
           .
       970102-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970103-FECHA-CURSOR-PSQ-1       SECTION.
      *-----------------------------------------------------------------
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO

           MOVE 'N'                    TO IN-CURSOR-PSQ-ABTO-1
           EXEC SQL
                CLOSE CURSOR-PSQ-1
           END-EXEC
           .
       970103-SAI.
           EXIT.

      *-----------------------------------------------------------------
       980000-GRAVAR-CTNR-RESPOSTA     SECTION.
      *-----------------------------------------------------------------
           MOVE 0011                   TO MSG1-CD-SEQL-ERRO

      *--> Exclui eventual container de erro de outros programas
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO

      *--> Monta o nome do container de resposta
           MOVE 'SGTS0264-RPST'        TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE

      *--> Por causa da captura automtica de erro CICS, deve ser
      *--> previsto o erro de container
           SET  ERRO-CONTAINER         TO TRUE

           EXEC CICS
                PUT  CONTAINER ( NM-CTNR                 )
                     CHANNEL   ( NM-CNL                  )
                     FLENGTH   ( LENGTH OF SGTS0264-RPST )
                     FROM      ( SGTS0264-RPST           )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       980000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       983000-EXCLUIR-RESPOSTA-ERRO    SECTION.
      *----------------------------------------------------------------*
           MOVE 0012                   TO MSG1-CD-SEQL-ERRO
      *----------------------------------------------------------------*
      * Exclui o container de resposta e de erro para evitar problemas *
      * em modulos executados mais de uma vez dentro da mesma sessao do*
      * CICS. Equivale ao initialize na area de resposta dos programas *
      * que utilizam COMMAREA.                                         *
      *----------------------------------------------------------------*
      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *--> nome_modulo -RPST

           MOVE NM-PGM                 TO NM-CTNR(1:8)
           MOVE '-RPST'                TO NM-CTNR(9:5)

           SET DEL-CONTAINER           TO TRUE

      *--> Exclui o container de resposta
      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS
                DELETE CONTAINER (NM-CTNR   )
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

      *----------------------------------------------------------------*
       983100-EXCLUIR-CONTAINER-ERRO   SECTION.
      *----------------------------------------------------------------*
           MOVE 0013                   TO MSG1-CD-SEQL-ERRO

      *--> Limpar variaveis container de erro
           INITIALIZE IIBKERRO-PRM-ERRO

      *--> Exclui o container de erro
           MOVE CTNR-PRM-ERRO          TO NM-CTNR
           SET DEL-CONTAINER           TO TRUE

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS
                DELETE CONTAINER (NM-CTNR   )
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
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------

           IF  ERRO-PROGRAMA-ACIONADO
               PERFORM 999100-MONITORAR-VRV-PGM-CHMD
           END-IF

           PERFORM 999200-MONITORAR-VRV-PGM-E-ENC
           .
       999000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999100-MONITORAR-VRV-PGM-CHMD   SECTION.
      *-----------------------------------------------------------------
      *--> Monitora as variáveis dos mdulos chamados

           EVALUATE  NM-PGM
      *-->>SBDATAF<<
             WHEN SBDATAF
               MOVE  'FUNCAO'                    TO  MSGKW999-NM-VRV-MNT
               MOVE  FUNCAO                      TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE  'A-TAM'                     TO  MSGKW999-NM-VRV-MNT
               MOVE   A-TAM                      TO  MCR-S9-15
               MOVE  MCR-S9-15                   TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE  'A-FIM'                     TO  MSGKW999-NM-VRV-MNT
               MOVE   A-FIM
               TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

             WHEN  OTHER
                   CONTINUE
           END-EVALUATE
           .
       999100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999200-MONITORAR-VRV-PGM-E-ENC  SECTION.
      *-----------------------------------------------------------------
      *--> Monitora variàveis do pgm, grava e encerra processamento

           IF  MSG1-CD-ERRO-TS             EQUAL  ZEROS
              MOVE  8888               TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS             EQUAL  SPACES
               MOVE  'Erro não especificado no módulo.'
                                       TO  MSG1-TX-ERRO-TS
           END-IF

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

      *--> Adiciona as variáveis para monitoração
           MOVE 'SGTS0264-DT-PSC-E'              TO MSGKW999-NM-VRV-MNT
           MOVE SGTS0264-DT-PSC-E                TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0264-CD-CTRA-TIT-VL-E'      TO MSGKW999-NM-VRV-MNT
           MOVE SGTS0264-CD-CTRA-TIT-VL-E        TO MCR-S9-15
           MOVE MCR-S9-15                        TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE  'SGTS0264-CD-OPR-ETRD-MCP'      TO  MSGKW999-NM-VRV-MNT
      *--> Verifica se o conteudo e numerico antes de adicionar
           IF  SGTS0264-CD-OPR-ETRD-MCP-E  IS NUMERIC
               MOVE  SGTS0264-CD-OPR-ETRD-MCP-E  TO  MCR-S9-15
               MOVE  MCR-S9-15                   TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico'        TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0264-CD-TIT-OPR-ETRD-E'     TO MSGKW999-NM-VRV-MNT
           MOVE SGTS0264-CD-TIT-OPR-ETRD-E       TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0264-CD-CLI-E'              TO MSGKW999-NM-VRV-MNT
           MOVE SGTS0264-CD-CLI-E                TO MCR-S9-15
           MOVE MCR-S9-15                        TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

      *--> Paginacao
           MOVE 'SGTS0264-CD-CTRA-TIT-VL-ULT'    TO MSGKW999-NM-VRV-MNT
           MOVE SGTS0264-CD-CTRA-TIT-VL-ULT      TO MCR-S9-15
           MOVE MCR-S9-15                        TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE  'SGTS0264-CD-OPR-ETRD-MCP-ULT'  TO  MSGKW999-NM-VRV-MNT
      *--> Verifica se o conteudo e numerico antes de adicionar
           IF  SGTS0264-CD-OPR-ETRD-MCP-ULT IS NUMERIC
               MOVE  SGTS0264-CD-OPR-ETRD-MCP-ULT TO  MCR-S9-15
               MOVE  MCR-S9-15                   TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico'        TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

      *--> grava o erro no msg
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

      *---------------------------------------*
      * 999001-TRATA-ABEND-CICS        SECTION.
      * 999002-TRATA-CONDITION-CICS    SECTION.
      * 999003-ADICIONA-VRV-MNT        SECTION.
      *---------------------------------------*
-INC HLPKP999

      *---------------------------------------*
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *---------------------------------------*
-INC SGTKP999

      *---------------------------------------*
      * 999010-TRATA-ERRO-DB2          SECTION.
      *---------------------------------------*
-INC HLPKP997

      *---------------------------------------*
      * MSG1-NTF-MSG                   SECTION.
      *---------------------------------------*
-INC MSGKPMG1

      *---------------------------------------*
       999999-ENCERRAR                 SECTION.
      *---------------------------------------*

           IF  IN-CURSOR-PSQ-ABTO-1  EQUAL  'S'
               PERFORM 970103-FECHA-CURSOR-PSQ-1
           END-IF

           EXEC CICS
                RETURN
           END-EXEC
           .
       999999-SAI.
           EXIT.
      *---------------------- FIM SGTS0264 ----------------------------