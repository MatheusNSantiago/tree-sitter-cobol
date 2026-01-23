      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSI056
      * ANALISTA..: F0175702 - ADRIANO SEIJI NAKAMURA
      * AUTOR.....: F0175702 - ADRIANO SEIJI NAKAMURA
      * OBJETIVO..: INCLUSAO DA TABELA UOR_ITVT_NOTA_TCN
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 06/02/2017 F8785286 DE 2016/145195 - Acertos dos códi-
      *          gos de mensagem para mensageria.
      *        01/02/2017 F8785286 DE 2016/145195 - Inclusão leitura
      *              container operador para gravação no LOG.
      *        17.11.2016 - F0175702 - IMPLANTACAO
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSI056.
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
       01  CTE-PROG     PIC  X(17) VALUE '*** SGTSI056 ***'.
       01  CTE-VERS     PIC  X(06) VALUE 'VRS001'.
       01  SGTS014G     PIC X(8)   VALUE 'SGTS014G'.

      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informacoes
-INC HLPKE01B
      *-----------------------------------------------------------------
       LOCAL-STORAGE SECTION.
      *-----------------------------------------------------------------
      * Book de para acionamento do MSG1
-INC IIBKERRO
-INC MSGKWMG1
-INC HLPKW999

       01  GRP-CONTAINER.
           03  CANAL                        PIC X(16) VALUE SPACES.

      * Area de trabalho
       01  QT-REG-SELD                      PIC S9(4) COMP-5 VALUE 0.

      * Indicadores de nulidades
       01  QT-REG-SELD-NULO                 PIC S9(4) COMP-5 VALUE 0.

      * Variaveis para gravacao do log
       01  DADOS-LOG-DISPLAY.
           03  CD-UOR-ITVT-NOTA-LOG          PIC  9(10) VALUE 0.
           03  NR-NOTA-TCN-OPR-LOG           PIC  9(5) VALUE 0.

      * Book dos dados do log
       01  DADOS-LOG.
-INC SGTK014G

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

-INC SGTKI056

001400* Book dos dados do usuário
-INC IIBKOPRD

000600* Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1

       01  CTE-FINAL-SS                PIC  X(40)      VALUE
                    '*** S.S. TERMINA AQUI ***'.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
           IF NOT EXECUCAO-ONLINE AND NOT  EXECUCAO-BATCH
              CALL CICS5000 USING CICS5000-COMMAREA
           END-IF

           MOVE 0001                   TO  MSG1-CD-SEQL-ERRO

      * Habilita tratamento automatico de Erros DB2
-INC HLPKE997
      * Habilita captura automatica de Abend e Erros CICS
-INC HLPE999

           PERFORM 100000-DESCOBRIR-CANAL
           PERFORM 120000-EXCLUIR-CTNR-ERRO
           PERFORM 130000-RECEBER-REQUISICAO
           PERFORM 140000-VALIDA-PARAMETRO

           PERFORM 200000-INCLUIR

           MOVE 'Inclusão efetuada com sucesso.' TO SGTSI056-TX-RTN-PRCT
           PERFORM 300000-POSTA-RESPOSTA

           GOBACK
           .
      *-----------------------------------------------------------------
       100000-DESCOBRIR-CANAL          SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO  MSG1-CD-SEQL-ERRO

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CANAL             TO  TRUE

           EXEC CICS ASSIGN
              CHANNEL ( CANAL )
           END-EXEC

      *    Ao chegar aqui, há certeza de que não houve erro algum
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
           MOVE LENGTH OF SGTSI056-RQSC    TO  QT-CCT-EPRO

           EXEC CICS GET
              CONTAINER ( 'SGTSI056-RQSC' )
              CHANNEL   ( CANAL      )
              NODATA
              FLENGTH ( QT-CCT-LIDO )
           END-EXEC

           IF  QT-CCT-LIDO             NOT EQUAL QT-CCT-EPRO
               SET   ERRO-TAMANHO-AREA   TO  TRUE
               PERFORM 999000-ERRO
           END-IF

           EXEC CICS GET
              CONTAINER ( 'SGTSI056-RQSC' )
              CHANNEL   ( CANAL      )
              FLENGTH ( LENGTH OF SGTSI056-RQSC )
              INTO    ( SGTSI056-RQSC )
           END-EXEC
           .
       130000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       140000-VALIDA-PARAMETRO         SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                   TO  MSG1-CD-SEQL-ERRO

           PERFORM 150000-VALIDA-CAMPOS

           PERFORM 170001-VALIDA-NOTA-TCN-OPR-ETR
           .
       140000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       150000-VALIDA-CAMPOS            SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO  MSG1-CD-SEQL-ERRO

           IF  SGTSI056-CD-UOR-ITVT-NOTA   EQUAL ZEROS
               SET  ERRO-PROGRAMADO        TO  TRUE
               MOVE 292                    TO  MSG1-CD-ERRO-TS
               MOVE 'Código unidade organizacional interveniente nota in
      -        'válido.'                   TO  MSG1-TX-ERRO-TS
      *        SET MNT-VRV             TO  TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI056-NR-NOTA-TCN-OPR    EQUAL ZEROS
               SET  ERRO-PROGRAMADO        TO  TRUE
               MOVE 182                    TO  MSG1-CD-ERRO-TS
               MOVE 'Número nota técnica operação inválido.'
                                           TO  MSG1-TX-ERRO-TS
      *        SET MNT-VRV             TO  TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       150000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       170001-VALIDA-NOTA-TCN-OPR-ETR  SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO  MSG1-CD-SEQL-ERRO

           MOVE ZEROS                  TO  QT-REG-SELD

           EXEC SQL
             SELECT 1
              INTO :QT-REG-SELD
               FROM DB2SGT.NOTA_TCN_OPR_ETRD
               WHERE NR_NOTA_TCN_OPR    =  :SGTSI056-NR-NOTA-TCN-OPR
           END-EXEC

           IF  QT-REG-SELD = 0
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE 293                TO  MSG1-CD-ERRO-TS
               MOVE 'Nota técnica operação estruturada inexistente.'
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
           MOVE 0008                   TO  MSG1-CD-SEQL-ERRO
      *
      * Desligar erro automatico do DB2
      *
           EXEC SQL WHENEVER SQLERROR CONTINUE END-EXEC
      *
           EXEC SQL
                INSERT INTO DB2SGT.UOR_ITVT_NOTA_TCN
                ( CD_UOR_ITVT_NOTA
                , NR_NOTA_TCN_OPR )
                VALUES
                ( :SGTSI056-CD-UOR-ITVT-NOTA
                , :SGTSI056-NR-NOTA-TCN-OPR )
           END-EXEC
      *
      * Ligar erro automatico do DB2
      *
           EXEC SQL WHENEVER SQLERROR
                GO TO 999010-TRATA-ERRO-DB2
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN -803
                   SET  ERRO-PROGRAMADO    TO  TRUE
                   MOVE 803                TO  MSG1-CD-ERRO-TS
                   MOVE 'Inclusão não efetuada, registro existente.'
                                       TO  MSG1-TX-ERRO-TS
                   PERFORM 999000-ERRO

               WHEN 0
                    CONTINUE

               WHEN OTHER
      * Aciona o tratamento padrao de erro padrao para os demais casos
                    GO TO 999010-TRATA-ERRO-DB2
           END-EVALUATE

           INITIALIZE DADOS-LOG
           MOVE 'I'                    TO  SGTS014G-CD-TIP-ACAO
           PERFORM 980102-GRAVA-LOG
           .
       200000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       300000-POSTA-RESPOSTA           SECTION.
      *-----------------------------------------------------------------
           MOVE 0009                   TO  MSG1-CD-SEQL-ERRO

           EXEC CICS PUT
              CONTAINER ( 'SGTSI056-RPST' )
              CHANNEL   ( CANAL      )
              FLENGTH ( LENGTH OF SGTSI056-RPST )
              FROM    ( SGTSI056-RPST )
           END-EXEC
           .
       300000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       969000-MONITORAR-VARIAVEIS      SECTION.
      *----------------------------------------------------------------*
      * Monitora as variaveis dos modulos chamados
      *----------------------------------------------------------------*
           EVALUATE NM-PGM

           WHEN 'SGTS014G'
                MOVE 'SGTS014G-NM-ENT'           TO MSGKW999-NM-VRV-MNT
                MOVE SGTS014G-NM-ENT             TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTS014G-CD-TIP-ACAO'      TO MSGKW999-NM-VRV-MNT
                MOVE SGTS014G-CD-TIP-ACAO        TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTS014G-CD-CHV-USU'       TO MSGKW999-NM-VRV-MNT
                MOVE SGTS014G-CD-CHV-USU         TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTS014G-NM-PGM'           TO MSGKW999-NM-VRV-MNT
                MOVE SGTS014G-NM-PGM             TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTS014G-CHV-PRMR'         TO MSGKW999-NM-VRV-MNT
                MOVE SGTS014G-CHV-PRMR           TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTS014G-CD-RTN-PGM'       TO MSGKW999-NM-VRV-MNT
                MOVE SGTS014G-CD-RTN-PGM         TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTS014G-MSG-ERRO-PGM'     TO MSGKW999-NM-VRV-MNT
                MOVE SGTS014G-MSG-ERRO-PGM       TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

           WHEN OTHER
                CONTINUE
           END-EVALUATE
           .
       969000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       980102-GRAVA-LOG                SECTION.
      *-----------------------------------------------------------------

           MOVE 'UOR_ITVT_NOTA_TCN '   TO  SGTS014G-NM-ENT

           IF  EXECUCAO-ONLINE
               PERFORM 980103-LER-DADOS-OPRD
               MOVE  KOPRD-CD-OPRD         TO  SGTS014G-CD-CHV-USU
           ELSE
               MOVE 'SYSTEM'           TO  SGTS014G-CD-CHV-USU
           END-IF

           MOVE 'SGTSI056'                 TO  SGTS014G-NM-PGM
           MOVE SGTSI056-CD-UOR-ITVT-NOTA  TO  CD-UOR-ITVT-NOTA-LOG
           MOVE SGTSI056-NR-NOTA-TCN-OPR   TO  NR-NOTA-TCN-OPR-LOG

           STRING CD-UOR-ITVT-NOTA-LOG
           ';'    NR-NOTA-TCN-OPR-LOG
               DELIMITED BY SIZE         INTO  SGTS014G-CHV-PRMR

           MOVE LENGTH OF DADOS-LOG        TO  EIBCALEN
           CALL SGTS014G USING DFHEIBLK DADOS-LOG

           IF  SGTS014G-CD-RTN-PGM     NOT EQUAL ZEROS
               SET  ERRO-PROGRAMA-ACIONADO TO  TRUE
               MOVE 112                    TO  MSG1-CD-ERRO-TS
               MOVE SGTS014G-MSG-ERRO-PGM  TO  MSG1-TX-ERRO-TS
               MOVE 'SGTS014G'             TO  NM-PGM
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       980102-SAI.
           EXIT.
      *----------------------------------------------------------------*
       980103-LER-DADOS-OPRD           SECTION.
      *----------------------------------------------------------------*
           MOVE 0010                   TO  MSG1-CD-SEQL-ERRO

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
      *----------------------------------------------------------------*
      *                                                                *
      * As sections abaixo iniciadas com 999000 representam os trata-  *
      * mentos de erro utilizando o Sistema MSG (rotina MSGS0001)      *
      *                                                                *
      *----------------------------------------------------------------*
       999000-ERRO                     SECTION.
      *----------------------------------------------------------------*

           IF MSG1-CD-ERRO-TS EQUAL ZEROS
              MOVE 8888                TO MSG1-CD-ERRO-TS
           END-IF

           IF MSG1-TX-ERRO-TS EQUAL SPACES
              MOVE 'Erro não especificado no módulo.' TO MSG1-TX-ERRO-TS
           END-IF

           PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           .
       999000-ERRO-SAI.
           EXIT.
      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------

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

      *    IF  MNT-VRV
               MOVE 'SGTSI056-CD-UOR-ITVT-NOTA'  TO  MSGKW999-NM-VRV-MNT
               MOVE SGTSI056-CD-UOR-ITVT-NOTA    TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTSI056-NR-NOTA-TCN-OPR'   TO  MSGKW999-NM-VRV-MNT
               MOVE SGTSI056-NR-NOTA-TCN-OPR     TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT
      *    END-IF

      * --> grava o erro no msg
           PERFORM MSG1-NTF-MSG
           MOVE MSG1-CD-ERRO-TS              TO  IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS              TO  IIBKERRO-TX-ERRO
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
-INC HLPKP999
      * --------------------------------------*
      * 999010-TRATA-ERRO-DB2          SECTION.
-INC HLPKP997
      * --------------------------------------*
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *---------------------------------------*
-INC SGTKP999
      * --------------------------------------*
      * MSG1-NTF-MSG                   SECTION.
-INC MSGKPMG1
      * --------------------------------------*
       999999-ENCERRAR                 SECTION.
      * --------------------------------------*
           GOBACK.