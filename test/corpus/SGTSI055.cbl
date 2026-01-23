      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSI055
      * ANALISTA..: F0175702 - ADRIANO SEIJI NAKAMURA
      * AUTOR.....: F0175702 - ADRIANO SEIJI NAKAMURA
      * OBJETIVO..: INCLUSAO DA TABELA NOTA_TCN_OPR_ETRD
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 01/02/2018 F8785286 DE 2016/145195 - Inclusão section
      *           998000-MONITORAR-VRV-PGM-CHMD para mensageria.
      *        01/02/2017 F8785286 DE 2016/145195 - Inclusão leitura
      *              container operador para gravação no LOG.
      *        16.11.2016 - F0175702 - DE 2015/137233 - IMPLANTACAO
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSI055.
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
      *-----------------------------------------------------------------
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
      *-----------------------------------------------------------------
       01  CTE-PROG     PIC  X(17) VALUE '*** SGTSI055 ***'.
       01  CTE-VERS     PIC  X(06) VALUE 'VRS001'.

      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informacoes
       COPY HLPKE01B.
      *-----------------------------------------------------------------
       LOCAL-STORAGE SECTION.
      *-----------------------------------------------------------------
      * Book de para acionamento do MSG1
       COPY IIBKERRO.
       COPY MSGKWMG1.
       COPY HLPKW999.

       01  GRP-CONTAINER.
           03  CANAL                        PIC X(16) VALUE SPACES.

      * Area de trabalho
       01  QT-REG-SELD                      PIC S9(4) COMP-5 VALUE 0.

       COPY HLPKTIME.
       01  DT-SQL-ATU                       PIC  X(10)
                                                 VALUE '  .  .    '.
       01  DT-SQL-ATU-R REDEFINES DT-SQL-ATU.
           03 DIA                           PIC  9(02).
           03 FILLER                        PIC  X(01).
           03 MES                           PIC  9(02).
           03 FILLER                        PIC  X(01).
           03 ANO                           PIC  9(04).
       01  TS-SQL-ATU                       PIC  X(26) VALUE ' '.

      * Indicadores de nulidades
       01  QT-REG-SELD-NULO                 PIC S9(4) COMP-5 VALUE 0.
       01  CD-ETR-DCS-N                     PIC S9(4) COMP-5 VALUE 0.
       01  CD-TIP-DOC-TCN-N                 PIC S9(4) COMP-5 VALUE 0.
       01  CD-ARQ-OPR-ETRD-N                PIC S9(4) COMP-5 VALUE 0.

      * Variaveis para gravacao do log
       01  DADOS-LOG-DISPLAY.
           03  NR-NOTA-TCN-OPR-LOG           PIC  9(5) VALUE 0.

      * Book dos dados do log
       01  DADOS-LOG.
       COPY SGTK014G.

       01  SGTS014G VALUE 'SGTS014G'         PIC X(8).

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

       COPY SGTKI055.

      * Book dos dados do usuário
       COPY IIBKOPRD.

      * Book das variáveis auxiliares do tratamento de erro via MSG
       COPY SGTKERR1.

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
           IF NOT EXECUCAO-ONLINE AND NOT  EXECUCAO-BATCH
              CALL CICS5000 USING CICS5000-COMMAREA
           END-IF

           MOVE 0001                   TO  MSG1-CD-SEQL-ERRO
      * Habilita tratamento automatico de Erros DB2
       COPY HLPKE997.
      * Habilita captura automatica de Abend e Erros CICS
       COPY HLPKE999.

           PERFORM 100000-DESCOBRIR-CANAL
           PERFORM 120000-EXCLUIR-CTNR-ERRO
           PERFORM 130000-RECEBER-REQUISICAO
           PERFORM 140000-VALIDA-PARAMETRO

           PERFORM 200000-INCLUIR

           MOVE SGTSI055-NR-NOTA-TCN-OPR   TO  SGTSI055-NR-NOTA-TCN-GRVD
           MOVE 'Inclusão efetuada com sucesso.' TO SGTSI055-TX-RTN-PRCT
           PERFORM 300000-POSTA-RESPOSTA

           GOBACK
           .
      *-----------------------------------------------------------------
       100000-DESCOBRIR-CANAL          SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO  MSG1-CD-SEQL-ERRO

           SET  ERRO-CANAL             TO  TRUE

           EXEC CICS ASSIGN
              CHANNEL ( CANAL )
           END-EXEC

           SET  SEM-ERRO               TO  TRUE
           .
       100000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       120000-EXCLUIR-CTNR-ERRO        SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                   TO  MSG1-CD-SEQL-ERRO

           EXEC CICS DELETE
              CONTAINER ( 'PRM-ERRO' )
              CHANNEL   ( CANAL      )
              NOHANDLE
           END-EXEC

           IF  NOT (EIBRESP EQUAL DFHRESP(NORMAL)   OR
               EIBRESP EQUAL DFHRESP(CONTAINERERR) )
               PERFORM 999002-TRATA-CONDITION-CICS
           END-IF
           .
       120000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       130000-RECEBER-REQUISICAO       SECTION.
      *-----------------------------------------------------------------
           MOVE 0004                   TO  MSG1-CD-SEQL-ERRO

           MOVE ZEROS                      TO  QT-CCT-LIDO
           MOVE LENGTH OF SGTSI055-RQSC    TO  QT-CCT-EPRO

           EXEC CICS GET
              CONTAINER ( 'SGTSI055-RQSC' )
              CHANNEL   ( CANAL      )
              NODATA
              FLENGTH ( QT-CCT-LIDO )
           END-EXEC

           IF  QT-CCT-LIDO             NOT EQUAL QT-CCT-EPRO
               SET   ERRO-TAMANHO-AREA   TO  TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS GET
              CONTAINER ( 'SGTSI055-RQSC' )
              CHANNEL   ( CANAL      )
              FLENGTH ( LENGTH OF SGTSI055-RQSC )
              INTO    ( SGTSI055-RQSC )
           END-EXEC
           .
       130000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       140000-VALIDA-PARAMETRO         SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                   TO  MSG1-CD-SEQL-ERRO

           PERFORM 150000-VALIDA-CAMPOS

           PERFORM 160000-TRATA-NULIDADE

           IF  CD-TIP-DOC-TCN-N    EQUAL 0
               PERFORM 170001-VALIDA-TIP-DOC-NOTA-TCN
           END-IF
           .
       140000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       150000-VALIDA-CAMPOS            SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO  MSG1-CD-SEQL-ERRO

           PERFORM 151000-BUSCA-TS-DT

           MOVE  CORR HLPKE001-TS-FMTD  TO  DT-SQL-ATU-R
           MOVE HLPKE001-TS-FMTD       TO  TS-SQL-ATU

           IF  SGTSI055-CD-UOR-DRTA-OGNL   EQUAL ZEROS
               SET  ERRO-PROGRAMADO        TO  TRUE
               MOVE 304                    TO  MSG1-CD-ERRO-TS
               MOVE 'Código unidade organizacional diretoria original in
      -        'válido.'                   TO  MSG1-TX-ERRO-TS
      *        SET MNT-VRV             TO  TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI055-CD-REF-NOTA-TCN    EQUAL SPACES
               SET  ERRO-PROGRAMADO        TO  TRUE
               MOVE 284                    TO  MSG1-CD-ERRO-TS
               MOVE 'Código referência nota técnica inválido.'
                                           TO  MSG1-TX-ERRO-TS
      *        SET MNT-VRV             TO  TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI055-DT-EMS-NOTA              EQUAL SPACES
               SET  ERRO-PROGRAMADO              TO  TRUE
               MOVE 305                          TO  MSG1-CD-ERRO-TS
               MOVE 'Data emissão nota inválido.' TO  MSG1-TX-ERRO-TS
      *        SET MNT-VRV             TO  TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI055-DT-APVC-NOTA       EQUAL SPACES
               SET  ERRO-PROGRAMADO        TO  TRUE
               MOVE 306                    TO  MSG1-CD-ERRO-TS
               MOVE 'Data aprovação nota inválido.' TO  MSG1-TX-ERRO-TS
      *        SET MNT-VRV             TO  TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI055-CD-USU-RSP-ULT-ATL EQUAL SPACES
               SET  ERRO-PROGRAMADO        TO  TRUE
               MOVE 226                    TO  MSG1-CD-ERRO-TS
               MOVE 'Código usuário responsável última atualização invál
      -        'ido.'                      TO  MSG1-TX-ERRO-TS
      *        SET MNT-VRV             TO  TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI055-TS-ULT-ATL         EQUAL SPACES
               SET  ERRO-PROGRAMADO        TO  TRUE
               MOVE 277                    TO  MSG1-CD-ERRO-TS
               MOVE 'Timestamp última atualização inválido.'
                                           TO  MSG1-TX-ERRO-TS
      *        SET MNT-VRV             TO  TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI055-DT-EMS-NOTA        EQUAL 'CURRENT'
               MOVE DT-SQL-ATU             TO  SGTSI055-DT-EMS-NOTA
           END-IF

           IF  SGTSI055-DT-APVC-NOTA       EQUAL 'CURRENT'
               MOVE DT-SQL-ATU             TO  SGTSI055-DT-APVC-NOTA
           END-IF

           IF  SGTSI055-TS-ULT-ATL         EQUAL 'CURRENT'
               MOVE TS-SQL-ATU             TO  SGTSI055-TS-ULT-ATL
           END-IF
           .
       150000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       151000-BUSCA-TS-DT              SECTION.
      *-----------------------------------------------------------------
           CALL SBTIMEDT            USING  HLPKE001-CD-FUC
                                           HLPKE001-AREA-TRB
                                           HLPKE001-FMT-TS
                                           HLPKE001-TAM-TS
                                           HLPKE001-TS-FMTD

           IF  RETURN-CODE      NOT EQUAL  ZEROS
               SET  ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE RETURN-CODE        TO  MSG1-CD-RTN-PGM-ACND
               MOVE 'SBTIMEDT'         TO  MSG1-NM-PGM-ACND
                                           NM-PGM
               MOVE 0050               TO  MSG1-CD-ERRO-TS
               MOVE 'Erro ao obter data e hora atual.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 998000-MONITORAR-VRV-PGM-CHMD
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       151000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       160000-TRATA-NULIDADE           SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO  MSG1-CD-SEQL-ERRO

           IF  SGTSI055-CD-ETR-DCS         EQUAL ZEROS
               MOVE -1                     TO  CD-ETR-DCS-N
           END-IF

           IF  SGTSI055-CD-TIP-DOC-TCN     EQUAL ZEROS
               MOVE -1                     TO  CD-TIP-DOC-TCN-N
           END-IF

           IF  SGTSI055-CD-ARQ-OPR-ETRD    EQUAL ZEROS
               MOVE -1                     TO  CD-ARQ-OPR-ETRD-N
           END-IF

           .
       160000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       170001-VALIDA-TIP-DOC-NOTA-TCN  SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO  MSG1-CD-SEQL-ERRO

           MOVE ZEROS                  TO  QT-REG-SELD

           EXEC SQL
             SELECT 1
              INTO :QT-REG-SELD
               FROM DB2SGT.TIP_DOC_NOTA_TCN
               WHERE CD_TIP_DOC_TCN     =  :SGTSI055-CD-TIP-DOC-TCN
           END-EXEC

           IF  QT-REG-SELD = 0
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE 308                TO  MSG1-CD-ERRO-TS
               MOVE 'Tipo documento nota técnica inexistente'
                                       TO  MSG1-TX-ERRO-TS
      *        SET MNT-VRV             TO  TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       170001-SAI.
           EXIT.
      *-----------------------------------------------------------------
       200000-INCLUIR                  SECTION.
      *-----------------------------------------------------------------
           MOVE 0009                   TO  MSG1-CD-SEQL-ERRO

           PERFORM 430000-BUSCA-NR-SEQUENCIA

           EXEC SQL
                INSERT INTO DB2SGT.NOTA_TCN_OPR_ETRD
                ( NR_NOTA_TCN_OPR
                , CD_UOR_DRTA_OGNL
                , CD_REF_NOTA_TCN
                , DT_EMS_NOTA
                , DT_APVC_NOTA
                , CD_ETR_DCS
                , CD_TIP_DOC_TCN
                , CD_USU_RSP_ULT_ATL
                , TS_ULT_ATL
                , CD_ARQ_OPR_ETRD )
                VALUES
                ( :SGTSI055-NR-NOTA-TCN-OPR
                , :SGTSI055-CD-UOR-DRTA-OGNL
                , :SGTSI055-CD-REF-NOTA-TCN
                , :SGTSI055-DT-EMS-NOTA
                , :SGTSI055-DT-APVC-NOTA
                , :SGTSI055-CD-ETR-DCS
                  :CD-ETR-DCS-N
                , :SGTSI055-CD-TIP-DOC-TCN
                  :CD-TIP-DOC-TCN-N
                , :SGTSI055-CD-USU-RSP-ULT-ATL
                , :SGTSI055-TS-ULT-ATL
                , :SGTSI055-CD-ARQ-OPR-ETRD
                  :CD-ARQ-OPR-ETRD-N )
           END-EXEC

           INITIALIZE DADOS-LOG
           MOVE 'I'                    TO  SGTS014G-CD-TIP-ACAO
           PERFORM 980102-GRAVA-LOG
           .
       200000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       300000-POSTA-RESPOSTA           SECTION.
      *-----------------------------------------------------------------
           MOVE 0010                   TO  MSG1-CD-SEQL-ERRO

           EXEC CICS PUT
              CONTAINER ( 'SGTSI055-RPST' )
              CHANNEL   ( CANAL      )
              FLENGTH ( LENGTH OF SGTSI055-RPST )
              FROM    ( SGTSI055-RPST )
           END-EXEC
           .
       300000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       430000-BUSCA-NR-SEQUENCIA       SECTION.
      *-----------------------------------------------------------------
           MOVE 0011                   TO  MSG1-CD-SEQL-ERRO

           EXEC SQL
               SELECT VALUE (MAX(NR_NOTA_TCN_OPR) , 0 )
                INTO :SGTSI055-NR-NOTA-TCN-OPR
                FROM DB2SGT.NOTA_TCN_OPR_ETRD
           END-EXEC

           ADD 1                       TO  SGTSI055-NR-NOTA-TCN-OPR
           .
       430000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       980102-GRAVA-LOG                SECTION.
      *-----------------------------------------------------------------
           MOVE 0012                   TO  MSG1-CD-SEQL-ERRO

           MOVE 'NOTA_TCN_OPR_ETRD '   TO  SGTS014G-NM-ENT

           IF  EXECUCAO-ONLINE
               PERFORM 980103-LER-DADOS-OPRD
               MOVE  KOPRD-CD-OPRD         TO  SGTS014G-CD-CHV-USU
           ELSE
               MOVE 'SYSTEM'           TO  SGTS014G-CD-CHV-USU
           END-IF

           MOVE 'SGTSI055'                 TO  SGTS014G-NM-PGM
           MOVE SGTSI055-NR-NOTA-TCN-OPR   TO  NR-NOTA-TCN-OPR-LOG

           STRING NR-NOTA-TCN-OPR-LOG
               DELIMITED BY SIZE     INTO  SGTS014G-CHV-PRMR

           MOVE SGTS014G               TO  NM-PGM
           MOVE LENGTH OF DADOS-LOG    TO  EIBCALEN
           CALL SGTS014G USING DFHEIBLK DADOS-LOG

           IF  SGTS014G-CD-RTN-PGM    NOT  EQUAL ZEROS
               SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE  0112              TO  MSG1-CD-ERRO-TS
               MOVE  'Erro na gravação das informações do Log.'
                                       TO  MSG1-TX-ERRO-TS
               MOVE  SGTS014G          TO  MSG1-NM-PGM-ACND
               MOVE  SGTS014G-CD-RTN-PGM
                                       TO  MSG1-CD-RTN-PGM-ACND
               MOVE  SGTS014G-MSG-ERRO-PGM
                                       TO  MSG1-TX-RTN-PGM-ACND
               PERFORM 998000-MONITORAR-VRV-PGM-CHMD
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA

           END-IF
           .
       980102-SAI.
           EXIT.
      *----------------------------------------------------------------*
       980103-LER-DADOS-OPRD           SECTION.
      *----------------------------------------------------------------*
           MOVE 0013                   TO  MSG1-CD-SEQL-ERRO
      
      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO  TRUE
           MOVE 'PRM-INF-OPRD'         TO  NM-CTNR
      
      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO  TRUE
      
           INITIALIZE PRM-INF-OPRD
           EXEC CICS GET
                CONTAINER ( NM-CTNR                )
                INTO      ( PRM-INF-OPRD           )
                FLENGTH   ( LENGTH OF PRM-INF-OPRD )
           END-EXEC.
      
      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO  TRUE
           .
       980103-SAI.
           EXIT.
      *----------------------------------------------------------------
       998000-MONITORAR-VRV-PGM-CHMD   SECTION.
      *----------------------------------------------------------------
      *--> Monitora as variáveis dos módulos chamados

           EVALUATE  NM-PGM

               WHEN  SGTS014G
                    MOVE  'SGTS014G-CD-TIP-ACAO' TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTS014G-CD-TIP-ACAO   TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTS014G-NM-ENT'      TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTS014G-NM-ENT        TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTS014G-CD-CHV-USU'  TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTS014G-CD-CHV-USU    TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTS014G-NM-PGM'      TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTS014G-NM-PGM        TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTS014G-CHV-PRMR'    TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTS014G-CHV-PRMR      TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  SBTIMEDT
                    MOVE  'HLPKE001-CD-FUC'      TO  MSGKW999-NM-VRV-MNT
                    MOVE  HLPKE001-CD-FUC        TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'HLPKE001-AREA-TRB'    TO  MSGKW999-NM-VRV-MNT
                    MOVE  HLPKE001-AREA-TRB      TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'HLPKE001-FMT-TS'      TO  MSGKW999-NM-VRV-MNT
                    MOVE  HLPKE001-FMT-TS        TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'HLPKE001-TAM-TS'      TO  MSGKW999-NM-VRV-MNT
                    MOVE  HLPKE001-TAM-TS        TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'HLPKE001-TS-FMTD'     TO  MSGKW999-NM-VRV-MNT
                    MOVE  HLPKE001-TS-FMTD       TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  OTHER
                    CONTINUE
           END-EVALUATE
           .
       998000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------

           IF  MSG1-CD-ERRO-TS         EQUAL  ZEROS
              MOVE  8888               TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS         EQUAL  SPACES
               MOVE  'Erro não especificado no módulo.'
                                       TO  MSG1-TX-ERRO-TS
           END-IF

      *--> VERIFICA SE O TIPO DE ERRO GERADO POSSUI TRATAMENTO INTERNO
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

      *    IF  MNT-VRV
               MOVE 'SGTSI055-NR-NOTA-TCN-OPR'   TO  MSGKW999-NM-VRV-MNT
               MOVE SGTSI055-NR-NOTA-TCN-OPR     TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTSI055-CD-ARQ-OPR-ETRD'   TO  MSGKW999-NM-VRV-MNT
               MOVE SGTSI055-CD-ARQ-OPR-ETRD     TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTSI055-CD-UOR-DRTA-OGNL'  TO  MSGKW999-NM-VRV-MNT
               MOVE SGTSI055-CD-UOR-DRTA-OGNL    TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTSI055-CD-REF-NOTA-TCN'   TO  MSGKW999-NM-VRV-MNT
               MOVE SGTSI055-CD-REF-NOTA-TCN     TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTSI055-DT-EMS-NOTA'       TO  MSGKW999-NM-VRV-MNT
               MOVE SGTSI055-DT-EMS-NOTA         TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTSI055-DT-APVC-NOTA'      TO  MSGKW999-NM-VRV-MNT
               MOVE SGTSI055-DT-APVC-NOTA        TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTSI055-CD-ETR-DCS'        TO  MSGKW999-NM-VRV-MNT
               MOVE SGTSI055-CD-ETR-DCS          TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTSI055-CD-TIP-DOC-TCN'    TO  MSGKW999-NM-VRV-MNT
               MOVE SGTSI055-CD-TIP-DOC-TCN      TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTSI055-CD-USU-RSP-ULT-ATL'
                                                 TO  MSGKW999-NM-VRV-MNT
               MOVE SGTSI055-CD-USU-RSP-ULT-ATL  TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTSI055-TS-ULT-ATL'        TO  MSGKW999-NM-VRV-MNT
               MOVE SGTSI055-TS-ULT-ATL          TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

      *    END-IF

      * --> grava o erro no msg
           PERFORM MSG1-NTF-MSG
           MOVE MSG1-CD-ERRO-TS                TO  IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS                TO  IIBKERRO-TX-ERRO

           EXEC CICS PUT
              CONTAINER ( 'PRM-ERRO' )
              CHANNEL   ( CANAL      )
              FROM      ( IIBKERRO-PRM-ERRO)
              FLENGTH ( LENGTH OF IIBKERRO-PRM-ERRO )
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
      * --------------------------------------*
       COPY HLPKP999.
      * --------------------------------------*
      * 999010-TRATA-ERRO-DB2          SECTION.
       COPY HLPKP997.
      * --------------------------------------*
      * MSG1-NTF-MSG                   SECTION.
       COPY MSGKPMG1.
      * --------------------------------------*
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *---------------------------------------*
       COPY SGTKP999.
      * --------------------------------------*
       999999-ENCERRAR                 SECTION.
      * --------------------------------------*
           EXEC CICS
                RETURN
           END-EXEC.
