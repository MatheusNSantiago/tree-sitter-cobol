      *-----------------------------------------------------------------
      * Programa..: SGTSL087
      * Analista..: F2054109 Cintia Ap Soares Iglesias
      * Autores...: F0175702 Adriano Seiji Nakamura
      *             F8650584 Roberto Fernandes Lima
      * Objetivo..: LISTAR RECEBIMENTOS REALIZADOS DA OPERAÇÃO ESTRUTURA
      *             DA
      * Data......: 24/02/2017
      * Compilacao: Cobol/CICS/DB2
      *-----------------------------------------------------------------
      * VRS004 22.10.2024 F2054109 - Tratamento CD_TIP_OGM_INF = 2
      * VRS003 04/12/2019 F2804421 DE 2016/145195 - Incluir condição pa-
      *                   CD_TIP_LCTO_CTB = 2 (crédito),para os tipos de
      *                   serviço - cursor 2 e cursor 5.
      * VRS002 25/04/2019 F2634012 DE 2016/145195 - Alteração no cursor2
      *                   para selecionar os pagamentos só com a dmtv_
      *                   cx_tit ativa e inclusão do cursor 5
      *
      * VRS001 04/02/2019 F2804421 DE 2016/145195 - Alteração monitora-
      *                   ção de variávels SBDATAF.
      *                   - Troca da msg 067 pelo cód 051.
      *                   - Acerto tamanho arg02 para 1 posição.
      *        06/08/2018 F8785286 DE 2016/145195 - Inclusão cursor 004
      *        20/06/2017 F9188847 DE 2016/145195 - Inclusão cursor 003
      *        24/02/2017 F0175702 DE 2016/145195 - Implantação
      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSL087.
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

      *-----------------------------------------------------------------
       WORKING-STORAGE                 SECTION.
      *-----------------------------------------------------------------
       77  CTE-INICIO-SS               PIC X(35) VALUE
               '*** STORAGE SECTION COMECA AQUI ***'.
       77  CTE-PROG     PIC  X(17)     VALUE '*** SGTSL087 ***'.
       77  CTE-VERS     PIC  X(06)     VALUE 'VRS003'.

      *-----------------------------------------------------------------
      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informacoes
      *-----------------------------------------------------------------
-INC HLPKE01B

      *-----------------------------------------------------------------
       LOCAL-STORAGE SECTION.
      *-----------------------------------------------------------------

      *--> Area de trabalho
       01  QT-REG-SELD                 PIC S9(04) COMP-5 VALUE ZEROS.

      *--> Auxiliares para movimentação na monitoração de variáveis.
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).

      *--> Indicadores de nulidades

      *--> Datas Auxiliares
       01  DT-OITO-DD-MM-AA            PIC  X(08) VALUE SPACES.
       01  DT-OITO-DD-MM-AA-R          REDEFINES DT-OITO-DD-MM-AA.
           03  DIA                     PIC  X(02).
           03  MES                     PIC  X(02).
           03  ANO                     PIC  X(04).

       01  DT-DB2-DD-MM-AA             PIC  X(10) VALUE SPACES.
       01  DT-DB2-DD-MM-AA-R           REDEFINES DT-DB2-DD-MM-AA.
           03  DIA                     PIC  X(02).
           03  FILLER                  PIC  X(01).
           03  MES                     PIC  X(02).
           03  FILLER                  PIC  X(01).
           03  ANO                     PIC  X(04).

       01  DT-RCBT-PRVT-DD-MM-AA       PIC  X(10) VALUE SPACES.
       01  DT-RCBT-PRVT-DD-MM-AA-R     REDEFINES DT-RCBT-PRVT-DD-MM-AA.
           03  DIA                     PIC  X(02).
           03  PONTO-01                PIC  X(01).
           03  MES                     PIC  X(02).
           03  PONTO-02                PIC  X(01).
           03  ANO                     PIC  X(04).


      *--> Areas Auxiliares para Tabela DB2SGT.RCBT_RLZD_OPR
       01  RCBT-RLZD-OPR.
           03  NR-SEQL-RCBT-PRVT       PIC S9(04) COMP-5
                                       OCCURS 0101 VALUE ZEROS.
           03  CD-TIP-RCBT-PRVT        PIC S9(04) COMP-5
                                       OCCURS 0101 TIMES VALUE ZEROS.
           03  DT-RCBT-PRVT            PIC  X(10)
                                       OCCURS 0101 TIMES VALUE SPACES.
           03  TS-REG-PGTO             PIC  X(26)
                                       OCCURS 0101 TIMES VALUE SPACES.
           03  IN-VCL-MNL              PIC  X(01)
                                       OCCURS 0101 TIMES VALUE SPACES.
           03  CD-USU-VCL              PIC  X(08)
                                       OCCURS 0101 TIMES VALUE SPACES.
           03  TS-ULT-ATL              PIC  X(26)
                                       OCCURS 0101 TIMES VALUE SPACES.
           03  VL-RLZD-VCLD            PIC S9(15)V9(2) COMP-3
                                       OCCURS 0101 TIMES VALUE ZEROS.
           03  VL-RLZD-VCLD-N          PIC S9(4) COMP-5
                                       OCCURS 0101 TIMES VALUE ZEROS.
           03  CD-TIT-OPR-ETRD         PIC  X(12)
                                       OCCURS 0101 TIMES VALUE SPACES.
           03  NR-DVS-TIT              PIC S9(04) COMP-5
                                       OCCURS 0101 TIMES VALUE ZEROS.

      *--> Variavel(is) de controle do cursor(es)
       01  CTL-CURSOR-PSQ.
           03  CD-FIM-CURSOR-PSQ       PIC S9(04) COMP-5 VALUE ZEROS.
               88  FIM-CURSOR-PSQ                        VALUE +100.
           03  IN-CURSOR-PSQ-ABTO      PIC  X(01) VALUE 'N'.
               88  CURSOR-PSQ-ABTO                VALUE 'S'.

      *--> Campos utilizados para manipulacao de container
-INC SGTKERR1

      *--> Area de Parametros da Rotina SBDATAF
       01  WKSBDATA                    PIC  X(4099).
       01  AREA-RED                    REDEFINES WKSBDATA.
           03  AREA-TAM                PIC  9(0008) COMP.
           03  FILLER                  PIC  X(4071).
           03  AREA-FIM                PIC  X(0020).

       01  SBDATAF                     PIC  X(08) VALUE 'SBDATAF'.
       01  FUC                         PIC  X(03) VALUE 'F01'.
       01  ARG01                       PIC  9(08).
       01  ARG01-R                     REDEFINES  ARG01.
           03  DIA                     PIC  9(02).
           03  MES                     PIC  9(02).
           03  ANO                     PIC  9(04).
       01  ARG02                       PIC  9(01) VALUE 0004.

      *--> Book do Container de Erro
-INC IIBKERRO

      *--> Book para captura de erro via MSG1
-INC MSGKWMG1

      *--> Book para captura de abend cics da bloco codigo MSGKP999
-INC HLPKW999

      *--> Book do container de requisição e resposta deste módulo
      *    DB2SGT.RCBT_RLZD_OPR
       01  SGTSL087-DADOS.
-INC SGTKL087

      *-----------------------------------------------------------------
      *    Cursores de Pesquisa
      *-----------------------------------------------------------------

      *--> Cursor de Pesquisa 01
      *    Lista Todos os Recebimentos Realizados de uma Operacao
      *    Ordenados por Tipo de Movimentacao, Data de Recebimento
      *    e Timestamp de Pagamento

           EXEC SQL
            DECLARE  CURSOR-PSQ-01 CURSOR
                     WITH ROWSET POSITIONING FOR
             SELECT  A.CD_TIP_RCBT_PRVT
                  ,  A.DT_RCBT_PRVT
                  ,  A. TS_REG_PGTO
                  ,  A. IN_VCL_MNL
                  ,  A. CD_USU_VCL
                  ,  A. TS_ULT_ATL
                  ,  A. VL_RLZD_VCLD
               FROM  DB2SGT.RCBT_RLZD_OPR  A
              WHERE
                     A.CD_CTRA_TIT_VL  =  :SGTSL087-CD-CTRA-TIT-VL
                AND  A.CD_OPR_ETRD_MCP =  :SGTSL087-CD-OPR-ETRD-MCP
                AND  A.NR_VRS_OPR_ETRD =  :SGTSL087-NR-VRS-OPR-ETRD
                AND  A.NR_SEQL_RCBT_PRVT
                                       =  :SGTSL087-NR-SEQL-RCBT-PRVT
                AND  A.CD_TIT_OPR_ETRD
                                       =  :SGTSL087-CD-TIT-OPR-ETRD
                AND  A.NR_DVS_TIT      =  :SGTSL087-NR-DVS-TIT

                AND (A.CD_TIP_RCBT_PRVT
                                       >  :SGTSL087-CD-TIP-RCBT-PRVT-ULT
                 OR (A.CD_TIP_RCBT_PRVT
                                       =  :SGTSL087-CD-TIP-RCBT-PRVT-ULT
                AND  A.DT_RCBT_PRVT    > :DT-RCBT-PRVT-DD-MM-AA )
                 OR (A.CD_TIP_RCBT_PRVT
                                       =  :SGTSL087-CD-TIP-RCBT-PRVT-ULT
                AND  A.DT_RCBT_PRVT    =  :DT-RCBT-PRVT-DD-MM-AA
                AND  A.TS_REG_PGTO     >  :SGTSL087-TS-REG-PGTO-ULT))

           ORDER BY  A.CD_TIP_RCBT_PRVT
                  ,  A.DT_RCBT_PRVT
                  ,  A.TS_REG_PGTO

           END-EXEC.


      *--> Cursor de Pesquisa 02
      *    Lista Todos os Recebimentos Realizados de uma Operacao
      *    de um Tipo e Data de Recebimento Previsto específico
      *    Ordenados por Tipo de Movimentacao, Data de Recebimento
      *    e Timestamp de Pagamento.
      *    E tipo de lançamento = 2 (crédito).

           EXEC SQL
            DECLARE  CURSOR-PSQ-02 CURSOR
                     WITH ROWSET POSITIONING FOR
             SELECT  A.CD_TIP_RCBT_PRVT
                  ,  A.DT_RCBT_PRVT
                  ,  A. TS_REG_PGTO
                  ,  A. IN_VCL_MNL
                  ,  A. CD_USU_VCL
                  ,  A. TS_ULT_ATL
                  ,  A. VL_RLZD_VCLD
               FROM  DB2SGT.RCBT_RLZD_OPR  A
                     INNER JOIN
                        DB2SGT.DMTV_CX_TIT B
                        ON A.CD_TIT_OPR_ETRD = B.CD_TIT_OPR_ETRD
                       AND A.NR_DVS_TIT      = B.NR_DVS_TIT
                       AND A.TS_REG_PGTO     = B.TS_REG_PGTO
                WHERE
                A.CD_CTRA_TIT_VL       =  :SGTSL087-CD-CTRA-TIT-VL
                AND  A.CD_OPR_ETRD_MCP =  :SGTSL087-CD-OPR-ETRD-MCP
                AND  A.NR_VRS_OPR_ETRD =  :SGTSL087-NR-VRS-OPR-ETRD
                AND  A.NR_SEQL_RCBT_PRVT
                                       =  :SGTSL087-NR-SEQL-RCBT-PRVT
                AND  A.CD_TIP_RCBT_PRVT
                                       =  :SGTSL087-CD-TIP-RCBT-PRVT-ULT
                AND  A.DT_RCBT_PRVT    =  :DT-RCBT-PRVT-DD-MM-AA
      *         AND  A.CD_TIT_OPR_ETRD =  :SGTSL087-CD-TIT-OPR-ETRD
      *         AND  A.NR_DVS_TIT      =  :SGTSL087-NR-DVS-TIT
                AND  A.TS_REG_PGTO     >  :SGTSL087-TS-REG-PGTO-ULT
                AND  B.IN_MVTC_ATV     =  'S'
                AND  (B.CD_TIP_LCTO_CTB =   2
                     OR B.CD_TIP_OGM_INF = 2)

           ORDER BY  A.CD_TIP_RCBT_PRVT
                  ,  A.DT_RCBT_PRVT
                  ,  A.TS_REG_PGTO

           END-EXEC.


      *--> Cursor de Pesquisa 03
      *    Lista Recebimento Realizado por versão

           EXEC SQL
            DECLARE  CURSOR-PSQ-03 CURSOR
                     WITH ROWSET POSITIONING FOR
             SELECT  A.NR_SEQL_RCBT_PRVT
                  ,  A.CD_TIP_RCBT_PRVT
                  ,  A.DT_RCBT_PRVT
                  ,  A. TS_REG_PGTO
                  ,  A. IN_VCL_MNL
                  ,  A. CD_USU_VCL
                  ,  A. TS_ULT_ATL
                  ,  A. VL_RLZD_VCLD
               FROM  DB2SGT.RCBT_RLZD_OPR  A
              WHERE
                     A.CD_CTRA_TIT_VL  =  :SGTSL087-CD-CTRA-TIT-VL
                AND  A.CD_OPR_ETRD_MCP =  :SGTSL087-CD-OPR-ETRD-MCP
                AND  A.NR_VRS_OPR_ETRD =  :SGTSL087-NR-VRS-OPR-ETRD
                AND  A.CD_TIT_OPR_ETRD =  :SGTSL087-CD-TIT-OPR-ETRD
                AND  A.NR_DVS_TIT      =  :SGTSL087-NR-DVS-TIT
      * A partir daqui é paginação
                AND  (A.NR_SEQL_RCBT_PRVT
                                      >  :SGTSL087-NR-SEQL-RCBT-PRVT-ULT
                OR ( A.NR_SEQL_RCBT_PRVT
                                      =  :SGTSL087-NR-SEQL-RCBT-PRVT-ULT

                AND  A.CD_TIP_RCBT_PRVT
                                      >  :SGTSL087-CD-TIP-RCBT-PRVT-ULT)
                OR ( A.NR_SEQL_RCBT_PRVT
                                      =  :SGTSL087-NR-SEQL-RCBT-PRVT-ULT

                AND  A.CD_TIP_RCBT_PRVT
                                      =  :SGTSL087-CD-TIP-RCBT-PRVT-ULT
                AND  A.DT_RCBT_PRVT    >  :DT-RCBT-PRVT-DD-MM-AA)

                OR  ( A.NR_SEQL_RCBT_PRVT
                                      =  :SGTSL087-NR-SEQL-RCBT-PRVT-ULT

                AND  A.CD_TIP_RCBT_PRVT
                                      =  :SGTSL087-CD-TIP-RCBT-PRVT-ULT
                AND  A.DT_RCBT_PRVT    =  :DT-RCBT-PRVT-DD-MM-AA

                AND  A.TS_REG_PGTO     >  :SGTSL087-TS-REG-PGTO-ULT))

           ORDER BY  A.NR_SEQL_RCBT_PRVT
                  ,  A.CD_TIP_RCBT_PRVT
                  ,  A.DT_RCBT_PRVT
                  ,  A.TS_REG_PGTO

           END-EXEC.


      *--> Cursor de Pesquisa 04
      *    Lista Recebimento Realizado por Operação

           EXEC SQL
            DECLARE  CURSOR-PSQ-04 CURSOR
                     WITH ROWSET POSITIONING FOR
             SELECT  A.NR_SEQL_RCBT_PRVT
                  ,  A.CD_TIP_RCBT_PRVT
                  ,  A.DT_RCBT_PRVT
                  ,  A.TS_REG_PGTO
                  ,  A.IN_VCL_MNL
                  ,  A.CD_USU_VCL
                  ,  A.TS_ULT_ATL
                  ,  A.VL_RLZD_VCLD
                  ,  A.CD_TIT_OPR_ETRD
                  ,  A.NR_DVS_TIT
               FROM  DB2SGT.RCBT_RLZD_OPR  A
              WHERE
                     A.CD_CTRA_TIT_VL  =  :SGTSL087-CD-CTRA-TIT-VL
                AND  A.CD_OPR_ETRD_MCP =  :SGTSL087-CD-OPR-ETRD-MCP
                AND  A.NR_VRS_OPR_ETRD =  :SGTSL087-NR-VRS-OPR-ETRD
      * A partir daqui é paginação
                AND (A.NR_SEQL_RCBT_PRVT >
                                        :SGTSL087-NR-SEQL-RCBT-PRVT-ULT

                OR ( A.NR_SEQL_RCBT_PRVT
                                      =  :SGTSL087-NR-SEQL-RCBT-PRVT-ULT
                AND  A.CD_TIP_RCBT_PRVT
                                      >  :SGTSL087-CD-TIP-RCBT-PRVT-ULT)

                OR ( A.NR_SEQL_RCBT_PRVT
                                      =  :SGTSL087-NR-SEQL-RCBT-PRVT-ULT
                AND  A.CD_TIP_RCBT_PRVT
                                      =  :SGTSL087-CD-TIP-RCBT-PRVT-ULT
                AND  A.DT_RCBT_PRVT    >  :DT-RCBT-PRVT-DD-MM-AA)

                OR  ( A.NR_SEQL_RCBT_PRVT
                                      =  :SGTSL087-NR-SEQL-RCBT-PRVT-ULT
                AND  A.CD_TIP_RCBT_PRVT
                                      =  :SGTSL087-CD-TIP-RCBT-PRVT-ULT
                AND  A.DT_RCBT_PRVT    =  :DT-RCBT-PRVT-DD-MM-AA
                AND  A.TS_REG_PGTO     >  :SGTSL087-TS-REG-PGTO-ULT))

           ORDER BY  A.NR_SEQL_RCBT_PRVT
                  ,  A.CD_TIP_RCBT_PRVT
                  ,  A.DT_RCBT_PRVT
                  ,  A.TS_REG_PGTO

           END-EXEC.

      *--> Cursor de Pesquisa 05
      *    Lista Todos os Recebimentos Realizados, ativos, de uma Opera-
      *    cao Ordenados por Tipo de Movimentacao, Data de Recebimento
      *    e Timestamp de Pagamento.
      *    E tipo de lançamento = 2 (crédito).

           EXEC SQL
            DECLARE  CURSOR-PSQ-05 CURSOR
                     WITH ROWSET POSITIONING FOR
             SELECT  A.CD_TIP_RCBT_PRVT
                  ,  A.DT_RCBT_PRVT
                  ,  A. TS_REG_PGTO
                  ,  A. IN_VCL_MNL
                  ,  A. CD_USU_VCL
                  ,  A. TS_ULT_ATL
                  ,  A. VL_RLZD_VCLD
               FROM  DB2SGT.RCBT_RLZD_OPR  A
                     INNER JOIN
                        DB2SGT.DMTV_CX_TIT B
                        ON A.CD_TIT_OPR_ETRD = B.CD_TIT_OPR_ETRD
                       AND A.NR_DVS_TIT      = B.NR_DVS_TIT
                       AND A.TS_REG_PGTO     = B.TS_REG_PGTO
              WHERE
                     A.CD_CTRA_TIT_VL  =  :SGTSL087-CD-CTRA-TIT-VL
                AND  A.CD_OPR_ETRD_MCP =  :SGTSL087-CD-OPR-ETRD-MCP
                AND  A.NR_VRS_OPR_ETRD =  :SGTSL087-NR-VRS-OPR-ETRD
                AND  A.NR_SEQL_RCBT_PRVT
                                       =  :SGTSL087-NR-SEQL-RCBT-PRVT
                AND  A.CD_TIT_OPR_ETRD
                                       =  :SGTSL087-CD-TIT-OPR-ETRD
                AND  A.NR_DVS_TIT      =  :SGTSL087-NR-DVS-TIT
                AND  B.IN_MVTC_ATV     = 'S'
                AND  B.CD_TIP_LCTO_CTB =   2

                AND (A.CD_TIP_RCBT_PRVT
                                       >  :SGTSL087-CD-TIP-RCBT-PRVT-ULT
                 OR (A.CD_TIP_RCBT_PRVT
                                       =  :SGTSL087-CD-TIP-RCBT-PRVT-ULT
                AND  A.DT_RCBT_PRVT    > :DT-RCBT-PRVT-DD-MM-AA )
                 OR (A.CD_TIP_RCBT_PRVT
                                       =  :SGTSL087-CD-TIP-RCBT-PRVT-ULT
                AND  A.DT_RCBT_PRVT    =  :DT-RCBT-PRVT-DD-MM-AA
                AND  A.TS_REG_PGTO     >  :SGTSL087-TS-REG-PGTO-ULT))

           ORDER BY  A.CD_TIP_RCBT_PRVT
                  ,  A.DT_RCBT_PRVT
                  ,  A.TS_REG_PGTO

           END-EXEC.


      *--> Parâmetros para a utilizacao do DB2
           EXEC SQL
                     INCLUDE SQLCA
           END-EXEC.

       01  CTE-FINAL-SS                PIC  X(40) VALUE
                                       '*** S.S. Termina Aqui ***'.


      *-----------------------------------------------------------------
       PROCEDURE                       DIVISION.
      *-----------------------------------------------------------------

      *--> Habilita tratamento automatico de erros DB2
-INC HLPKE997
      *--> Habilita captura automatica de abend e erros CICS
-INC HLPKE999

           PERFORM  100000-DESCOBRIR-CANAL
           PERFORM  110000-EXCLUIR-CTNR-RPST
           PERFORM  120000-EXCLUIR-CTNR-ERRO
           PERFORM  130000-RECEBER-REQUISICAO

           PERFORM  140000-VALIDAR-PARAMETRO

           PERFORM  200000-PROCESSAR-PESQUISA

           PERFORM  300000-POSTA-RESPOSTA
           GOBACK
           .

      *-----------------------------------------------------------------
       100000-DESCOBRIR-CANAL          SECTION.
      *-----------------------------------------------------------------
           MOVE  0001                  TO  MSG1-CD-SEQL-ERRO

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CANAL            TO  TRUE

           EXEC CICS  ASSIGN
                      CHANNEL ( NM-CNL )
           END-EXEC
      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE

           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-EXCLUIR-CTNR-RPST        SECTION.
      *-----------------------------------------------------------------
           MOVE  0002                  TO  MSG1-CD-SEQL-ERRO

           EXEC CICS  DELETE
                      CONTAINER ( 'SGTSL087-RPST' )
                      CHANNEL   ( NM-CNL          )
                      NOHANDLE
           END-EXEC

           IF  NOT (EIBRESP                EQUAL  DFHRESP(NORMAL)  OR
                    EIBRESP                EQUAL  DFHRESP(CONTAINERERR))
               PERFORM  999002-TRATA-CONDITION-CICS
           END-IF
           .
       110000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       120000-EXCLUIR-CTNR-ERRO        SECTION.
      *-----------------------------------------------------------------
           MOVE  0003                  TO  MSG1-CD-SEQL-ERRO

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
           MOVE  0004                  TO  MSG1-CD-SEQL-ERRO

           MOVE  ZEROS                 TO  QT-CCT-LIDO
           MOVE  LENGTH OF  SGTSL087-RQSC
                                       TO  QT-CCT-EPRO

           EXEC CICS  GET
                      CONTAINER ( 'SGTSL087-RQSC' )
                      CHANNEL   ( NM-CNL          )
                      NODATA
                      FLENGTH   ( QT-CCT-LIDO     )
           END-EXEC

           IF  QT-CCT-LIDO                 NOT EQUAL  QT-CCT-EPRO
               SET   ERRO-TAMANHO-AREA   TO  TRUE
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS  GET
                      CONTAINER ( 'SGTSL087-RQSC'         )
                      CHANNEL   ( NM-CNL                  )
                      FLENGTH   ( LENGTH OF SGTSL087-RQSC )
                      INTO      ( SGTSL087-RQSC           )
           END-EXEC
           .
       130000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       140000-VALIDAR-PARAMETRO        SECTION.
      *-----------------------------------------------------------------
           MOVE  0005                  TO  MSG1-CD-SEQL-ERRO

           IF  SGTSL087-CD-CTRA-TIT-VL     NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0001              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' inválido.'      TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL087-CD-OPR-ETRD-MCP    NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0003              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada não numérico.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL087-CD-OPR-ETRD-MCP    NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0004              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  NOT SGTSL087-LISTA-POR-OPR
               IF  SGTSL087-NR-VRS-OPR-ETRD    NOT POSITIVE
                  SET   ERRO-PROGRAMADO   TO  TRUE
                  MOVE  0006              TO  MSG1-CD-ERRO-TS
                  MOVE  'Número versão operação estruturada inválido.'
                                          TO  MSG1-TX-ERRO-TS
                  PERFORM  999000-GRAVA-ERRO-E-ENCERRA
              END-IF
           END-IF

           IF  NOT SGTSL087-LISTA-POR-VERSAO
               IF  NOT SGTSL087-LISTA-POR-OPR
                   IF  SGTSL087-NR-SEQL-RCBT-PRVT  NOT GREATER ZEROS
                      SET   ERRO-PROGRAMADO   TO  TRUE
                      MOVE  0108              TO  MSG1-CD-ERRO-TS
                      MOVE  'Número sequêncial recebimento previsto invá
      -                  'lido.'              TO  MSG1-TX-ERRO-TS
                      PERFORM  999000-GRAVA-ERRO-E-ENCERRA
                  END-IF
               END-IF
           END-IF

           IF  NOT SGTSL087-LISTA-POR-OPR
             IF NOT  SGTSL087-LISTA-TIPO-E-DATA
              IF  SGTSL087-CD-TIT-OPR-ETRD    EQUAL SPACES
                  SET   ERRO-PROGRAMADO   TO  TRUE
                  MOVE  0231              TO  MSG1-CD-ERRO-TS
                  MOVE  'Código do título da operação estruturada não in
      -              'formado.'           TO  MSG1-TX-ERRO-TS
                  PERFORM  999000-GRAVA-ERRO-E-ENCERRA
              END-IF
             END-IF
           END-IF

           IF  NOT SGTSL087-LISTA-POR-OPR  AND
               SGTSL087-NR-DVS-TIT         NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0013              TO  MSG1-CD-ERRO-TS
               MOVE  'Número divisão titulo inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *--> Validar Tipo de Recebimento Previsto
      *    01 - Normal de Juros
      *    02 - Normal de Capital
      *    03 - Extraordinario de Juros
      *    04 - Extraordinario de Capital
           EVALUATE  TRUE
               WHEN  SGTSL087-LISTA-TODOS
                     IF  SGTSL087-CD-TIP-RCBT-PRVT-ULT
                                           LESS  0000  OR
                         SGTSL087-CD-TIP-RCBT-PRVT-ULT
                                           GREATER  0004
                         SET   ERRO-PROGRAMADO
                                       TO  TRUE
                         MOVE  0109    TO  MSG1-CD-ERRO-TS
                         MOVE  'Tipo do recebimento previsto inválido.'
                                       TO  MSG1-TX-ERRO-TS
                         PERFORM  999000-GRAVA-ERRO-E-ENCERRA
                     END-IF
               WHEN  SGTSL087-LISTA-TIPO-E-DATA
                     IF  SGTSL087-CD-TIP-RCBT-PRVT-ULT
                                           LESS  0000  OR
                         SGTSL087-CD-TIP-RCBT-PRVT-ULT
                                           GREATER  0004
                         SET   ERRO-PROGRAMADO
                                       TO  TRUE
                         MOVE  0109    TO  MSG1-CD-ERRO-TS
                         MOVE  'Tipo do recebimento previsto inválido.'
                                       TO  MSG1-TX-ERRO-TS
                         PERFORM  999000-GRAVA-ERRO-E-ENCERRA
                     END-IF

                WHEN  SGTSL087-LISTA-ATV
                     IF  SGTSL087-CD-TIP-RCBT-PRVT-ULT
                                           LESS  0000  OR
                         SGTSL087-CD-TIP-RCBT-PRVT-ULT
                                           GREATER  0004
                         SET   ERRO-PROGRAMADO
                                       TO  TRUE
                         MOVE  0109    TO  MSG1-CD-ERRO-TS
                         MOVE  'Tipo do recebimento previsto inválido.'
                                       TO  MSG1-TX-ERRO-TS
                         PERFORM  999000-GRAVA-ERRO-E-ENCERRA
                     END-IF

           END-EVALUATE

      *--> Validar Data do Recebimento Previsto
           IF  SGTSL087-DT-RCBT-PRVT-ULT   EQUAL  SPACES  OR
               SGTSL087-DT-RCBT-PRVT-ULT   EQUAL  '00000000'
               MOVE  '01.01.1900'      TO  DT-RCBT-PRVT-DD-MM-AA
           ELSE
               MOVE  CORR  SGTSL087-DT-RCBT-PRVT-ULT-R
                                       TO  DT-RCBT-PRVT-DD-MM-AA-R
               MOVE  '.'               TO  PONTO-01
                                           PONTO-02
               PERFORM  141000-VALIDAR-DATA
           END-IF
      *--> Se TS não informado, formatar padrão DB2
           IF SGTSL087-TS-REG-PGTO-ULT = ' '
              MOVE '0001-01-01-00.00.00.000000'
                                       TO SGTSL087-TS-REG-PGTO-ULT
           END-IF
           .
       140000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       141000-VALIDAR-DATA             SECTION.
      *--> Acionar a Funcao 01 da Rotina SBDATAF e verificar se
      *    a Data de Recebimento Previsto é válida
      *----------------------------------------------------------------*
           MOVE  0006                  TO  MSG1-CD-SEQL-ERRO

           MOVE  4095                  TO  AREA-TAM
           MOVE  'SBDATAF -WRKAREA-FIM'
                                       TO  AREA-FIM
           MOVE  CORR  DT-RCBT-PRVT-DD-MM-AA-R
                                       TO  ARG01-R

           CALL  SBDATAF            USING  FUC
                                           WKSBDATA
                                           ARG01
                                           ARG02

      *--> RETURN-CODE do SBDATA
      *    22 -  Conteudo do Argumento Nao Numerico
      *    24 -  Data Informada Invalida
      *    46 -  Data Fora da Faixa de 01/01/0001 a 31/12/9999

           IF  RETURN-CODE             NOT EQUAL  ZEROS
               SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE  RETURN-CODE       TO  MSG1-CD-RTN-PGM-ACND
               MOVE  SBDATAF           TO  MSG1-NM-PGM-ACND

               MOVE 'FUC'              TO  MSGKW999-NM-VRV-MNT
               MOVE  FUC               TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'AREA-TAM'         TO  MSGKW999-NM-VRV-MNT
               MOVE  AREA-TAM          TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'AREA-FIM'         TO  MSGKW999-NM-VRV-MNT
               MOVE  AREA-FIM          TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'ARG01'            TO  MSGKW999-NM-VRV-MNT
               MOVE  ARG01             TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'ARG02'            TO  MSGKW999-NM-VRV-MNT
               MOVE  ARG02             TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT


               IF  RETURN-CODE         EQUAL  0022 OR 0024 OR 0046
                   MOVE  0120          TO  MSG1-CD-ERRO-TS
                   MOVE  'Data de recebimento previsto inválida.'
                                       TO  MSG1-TX-ERRO-TS
               ELSE
                   MOVE  0051          TO  MSG1-CD-ERRO-TS
                   MOVE  'Erro na rotina que valida data.'
                                       TO  MSG1-TX-RTN-PGM-ACND
                                           MSG1-TX-ERRO-TS
               END-IF

               PERFORM 999000-GRAVA-ERRO-E-ENCERRA

           END-IF

           .
       141000-SAI.
           EXIT.


      *-----------------------------------------------------------------
       200000-PROCESSAR-PESQUISA       SECTION.
      *-----------------------------------------------------------------
           MOVE  0007                  TO  MSG1-CD-SEQL-ERRO

           MOVE  'N'                   TO  SGTSL087-IN-CTNC-PSQ

           PERFORM  971000-ABRE-CURSOR-PSQ
           PERFORM  972000-LER-CURSOR-PSQ
           PERFORM  973000-FECHA-CURSOR-PSQ

           PERFORM  210000-MONTAR-TABELA-RESPOSTA
                    VARYING SGTSL087-IC-TAB FROM  0001  BY  0001
                    UNTIL SGTSL087-IC-TAB  GREATER  SGTSL087-QT-OCR
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       210000-MONTAR-TABELA-RESPOSTA   SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

           MOVE NR-SEQL-RCBT-PRVT                      (SGTSL087-IC-TAB)
             TO SGTSL087-NR-SEQL-RCBT-PRVT-S           (SGTSL087-IC-TAB)

           MOVE  CD-TIP-RCBT-PRVT                      (SGTSL087-IC-TAB)
             TO  SGTSL087-CD-TIP-RCBT-PRVT             (SGTSL087-IC-TAB)

           MOVE  DT-RCBT-PRVT                          (SGTSL087-IC-TAB)
             TO  DT-DB2-DD-MM-AA
           MOVE  CORR  DT-DB2-DD-MM-AA-R
                                       TO  DT-OITO-DD-MM-AA-R
           MOVE  DT-OITO-DD-MM-AA
             TO  SGTSL087-DT-RCBT-PRVT                 (SGTSL087-IC-TAB)

           MOVE  TS-REG-PGTO                           (SGTSL087-IC-TAB)
             TO  SGTSL087-TS-REG-PGTO                  (SGTSL087-IC-TAB)

           MOVE  IN-VCL-MNL                            (SGTSL087-IC-TAB)
             TO  SGTSL087-IN-VCL-MNL                   (SGTSL087-IC-TAB)

           MOVE  CD-USU-VCL                            (SGTSL087-IC-TAB)
             TO  SGTSL087-CD-USU                       (SGTSL087-IC-TAB)

           MOVE  TS-ULT-ATL                            (SGTSL087-IC-TAB)
             TO  SGTSL087-TS-ULT-ATL                   (SGTSL087-IC-TAB)

           MOVE  VL-RLZD-VCLD                          (SGTSL087-IC-TAB)
             TO  SGTSL087-VL-RLZD-VCLD                 (SGTSL087-IC-TAB)

           IF SGTSL087-LISTA-POR-OPR
              MOVE  CD-TIT-OPR-ETRD                    (SGTSL087-IC-TAB)
                TO  SGTSL087-CD-TIT-OPR-ETRD-S         (SGTSL087-IC-TAB)
              MOVE  NR-DVS-TIT                         (SGTSL087-IC-TAB)
                TO  SGTSL087-NR-DVS-TIT-S              (SGTSL087-IC-TAB)
           END-IF
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-POSTA-RESPOSTA           SECTION.
      *-----------------------------------------------------------------
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS  PUT
                      CONTAINER ( 'SGTSL087-RPST'         )
                      CHANNEL   ( NM-CNL                  )
                      FLENGTH   ( LENGTH OF SGTSL087-RPST )
                      FROM      ( SGTSL087-RPST           )
           END-EXEC
           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       971000-ABRE-CURSOR-PSQ          SECTION.
      *-----------------------------------------------------------------
           MOVE  0010                  TO  MSG1-CD-SEQL-ERRO

           EVALUATE  TRUE
               WHEN  SGTSL087-LISTA-TODOS
                     EXEC SQL
                          OPEN CURSOR-PSQ-01
                     END-EXEC
               WHEN  SGTSL087-LISTA-TIPO-E-DATA
                     EXEC SQL
                          OPEN CURSOR-PSQ-02
                     END-EXEC
               WHEN  SGTSL087-LISTA-POR-VERSAO
                     EXEC SQL
                          OPEN CURSOR-PSQ-03
                     END-EXEC
               WHEN  SGTSL087-LISTA-POR-OPR
                     EXEC SQL
                          OPEN CURSOR-PSQ-04
                     END-EXEC

               WHEN  SGTSL087-LISTA-ATV
                     EXEC SQL
                          OPEN CURSOR-PSQ-05
                     END-EXEC

           END-EVALUATE

           MOVE  'S'                   TO  IN-CURSOR-PSQ-ABTO
           .
       971000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       972000-LER-CURSOR-PSQ           SECTION.
      *-----------------------------------------------------------------
           MOVE  0011                  TO  MSG1-CD-SEQL-ERRO

           MOVE  ZEROS                 TO  QT-REG-SELD

           MOVE  SPACES                TO  RCBT-RLZD-OPR
           INITIALIZE                      RCBT-RLZD-OPR
                                           REPLACING NUMERIC BY  ZEROS

           EVALUATE  TRUE

               WHEN  SGTSL087-LISTA-TODOS
                     EXEC SQL
                          FETCH NEXT ROWSET CURSOR-PSQ-01
                                FOR 0101 ROWS
                           INTO :CD-TIP-RCBT-PRVT
                              , :DT-RCBT-PRVT
                              , :TS-REG-PGTO
                              , :IN-VCL-MNL
                              , :CD-USU-VCL
                              , :TS-ULT-ATL
                              , :VL-RLZD-VCLD
                                :VL-RLZD-VCLD-N
                     END-EXEC
               WHEN  SGTSL087-LISTA-TIPO-E-DATA
                     EXEC SQL
                          FETCH NEXT ROWSET CURSOR-PSQ-02
                                FOR 0101 ROWS
                           INTO :CD-TIP-RCBT-PRVT
                              , :DT-RCBT-PRVT
                              , :TS-REG-PGTO
                              , :IN-VCL-MNL
                              , :CD-USU-VCL
                              , :TS-ULT-ATL
                              , :VL-RLZD-VCLD
                                :VL-RLZD-VCLD-N
                     END-EXEC
                WHEN  SGTSL087-LISTA-POR-VERSAO
                     EXEC SQL
                          FETCH NEXT ROWSET CURSOR-PSQ-03
                                FOR 0101 ROWS
                           INTO :NR-SEQL-RCBT-PRVT
                              , :CD-TIP-RCBT-PRVT
                              , :DT-RCBT-PRVT
                              , :TS-REG-PGTO
                              , :IN-VCL-MNL
                              , :CD-USU-VCL
                              , :TS-ULT-ATL
                              , :VL-RLZD-VCLD
                                :VL-RLZD-VCLD-N
                     END-EXEC
                WHEN  SGTSL087-LISTA-POR-OPR
                     EXEC SQL
                          FETCH NEXT ROWSET CURSOR-PSQ-04
                                FOR 0101 ROWS
                           INTO :NR-SEQL-RCBT-PRVT
                              , :CD-TIP-RCBT-PRVT
                              , :DT-RCBT-PRVT
                              , :TS-REG-PGTO
                              , :IN-VCL-MNL
                              , :CD-USU-VCL
                              , :TS-ULT-ATL
                              , :VL-RLZD-VCLD
                                :VL-RLZD-VCLD-N
                              , :CD-TIT-OPR-ETRD
                              , :NR-DVS-TIT

                     END-EXEC

               WHEN  SGTSL087-LISTA-ATV
                     EXEC SQL
                          FETCH NEXT ROWSET CURSOR-PSQ-05
                                FOR 0101 ROWS
                           INTO :CD-TIP-RCBT-PRVT
                              , :DT-RCBT-PRVT
                              , :TS-REG-PGTO
                              , :IN-VCL-MNL
                              , :CD-USU-VCL
                              , :TS-ULT-ATL
                              , :VL-RLZD-VCLD
                                :VL-RLZD-VCLD-N
                     END-EXEC

           END-EVALUATE

           MOVE  SQLERRD(3)            TO  QT-REG-SELD

           IF  QT-REG-SELD                 EQUAL  0101
               SET   SGTSL087-TEM-MAIS-REGISTRO
                                       TO  TRUE
               MOVE  0100              TO  QT-REG-SELD
           END-IF

           MOVE  QT-REG-SELD           TO  SGTSL087-QT-OCR

           MOVE  SQLCODE               TO  CD-FIM-CURSOR-PSQ
           .
       972000-SAI.
           EXIT.


      *-----------------------------------------------------------------
       973000-FECHA-CURSOR-PSQ         SECTION.
      *-----------------------------------------------------------------
           MOVE  0012                  TO  MSG1-CD-SEQL-ERRO

           MOVE  'N'                   TO  IN-CURSOR-PSQ-ABTO

           EVALUATE  TRUE
               WHEN  SGTSL087-LISTA-TODOS
                     EXEC SQL
                          CLOSE  CURSOR-PSQ-01
                     END-EXEC

               WHEN  SGTSL087-LISTA-TIPO-E-DATA
                     EXEC SQL
                          CLOSE  CURSOR-PSQ-02
                     END-EXEC

               WHEN  SGTSL087-LISTA-POR-VERSAO
                     EXEC SQL
                          CLOSE  CURSOR-PSQ-03
                     END-EXEC

               WHEN  SGTSL087-LISTA-POR-OPR
                     EXEC SQL
                          CLOSE  CURSOR-PSQ-04
                     END-EXEC

               WHEN  SGTSL087-LISTA-ATV
                     EXEC SQL
                          CLOSE  CURSOR-PSQ-05
                     END-EXEC

           END-EVALUATE

           .
       973000-SAI.
           EXIT.

      *--------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *--------------------------------------------------------------
           MOVE  0013                  TO  MSG1-CD-SEQL-ERRO

           IF  IN-CURSOR-PSQ-ABTO  EQUAL  'S'
               PERFORM 973000-FECHA-CURSOR-PSQ
           END-IF

           IF  MSG1-CD-ERRO-TS EQUAL ZEROS
               MOVE 8888                TO MSG1-CD-ERRO-TS
           END-IF
           IF  MSG1-TX-ERRO-TS EQUAL SPACES
               MOVE 'Erro não especificado no módulo.'
                                        TO MSG1-TX-ERRO-TS
           END-IF

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

           MOVE  'SGTSL087-CD-TIP-SRVC'         TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL087-CD-TIP-SRVC           TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSL087-CD-CTRA-TIT-VL'      TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL087-CD-CTRA-TIT-VL        TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSL087-CD-OPR-ETRD-MCP'     TO  MSGKW999-NM-VRV-MNT
      *------> Verifica se o conteudo e numerico antes de adicionar
           IF  SGTSL087-CD-OPR-ETRD-MCP         IS NUMERIC
               MOVE  SGTSL087-CD-OPR-ETRD-MCP   TO  CD-OPR-ETRD-MCP
               MOVE  CD-OPR-ETRD-MCP-TX         TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico'       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSL087-NR-VRS-OPR-ETRD'     TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL087-NR-VRS-OPR-ETRD       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSL087-NR-SEQL-RCBT-PRVT'   TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL087-NR-SEQL-RCBT-PRVT     TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSL087-CD-TIT-OPR-ETRD'     TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL087-CD-TIT-OPR-ETRD       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSL087-NR-DVS-TIT'          TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL087-NR-DVS-TIT            TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

      *--> Informação utilizada para rechamada
           MOVE  'SGTSL087-NR-SEQL-RCBT-PRVT-ULT'
                                                TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL087-NR-SEQL-RCBT-PRVT-ULT TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSL087-CD-TIP-RCBT-PRVT-ULT'
                                                TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL087-CD-TIP-RCBT-PRVT-ULT  TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSL087-DT-RCBT-PRVT-ULT'    TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL087-DT-RCBT-PRVT-ULT      TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSL087-TS-REG-PGTO-ULT'     TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL087-TS-REG-PGTO-ULT       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

      *--> Grava o Erro no MSG

           PERFORM MSG1-NTF-MSG

           MOVE  MSG1-CD-ERRO-TS       TO  IIBKERRO-CD-ERRO
           MOVE  MSG1-TX-ERRO-TS       TO  IIBKERRO-TX-ERRO

           EXEC CICS  PUT
                      CONTAINER ( 'PRM-ERRO'                  )
                      CHANNEL   ( NM-CNL                      )
                      FROM      ( IIBKERRO-PRM-ERRO           )
                      FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                      NOHANDLE
           END-EXEC

           PERFORM  999999-ENCERRAR
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
      * --------------------------------------*
-INC SGTKP999
      * --------------------------------------*
      * 999010-TRATA-ERRO-DB2          SECTION.
      * --------------------------------------*
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
           EXEC CICS
                RETURN
           END-EXEC
           .
       999999-SAI.
           EXIT.