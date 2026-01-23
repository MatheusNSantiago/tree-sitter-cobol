      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSE055
      * ANALISTA..: F0175702 - ADRIANO SEIJI NAKAMURA
      * AUTOR.....: F0175702 - ADRIANO SEIJI NAKAMURA
      * OBJETIVO..: EXCLUSAO DA TABELA NOTA_TCN_OPR_ETRD
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 09/02/2018 F8785286 DE 2016/145195 - Acerto dos códigos
      *          das mensagens para mensageria.
      *        31/01/2017 F8785286 DE 2016/145195 - Inclusão leitura
      *              container operador para gravação no LOG.
      *        16.11.2016 - F0175702 - DE 2015/137233 - IMPLANTACAO
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSE055.
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
       01  CTE-PROG     PIC  X(17) VALUE '*** SGTSE055 ***'.
       01  CTE-VERS     PIC  X(06) VALUE 'VRS001'.

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
      *    03  QT-CCT-LIDO                  PIC S9(9) COMP-5 VALUE 0.
      *    03  QT-CCT-EPRO                  PIC S9(9) COMP-5 VALUE 0.
      *    03  VL-EXBR-1                    PIC 9(10)        VALUE 0.
      *    03  VL-EXBR-2                    PIC 9(10)        VALUE 0.
      * Area de trabalho

       01  QT-REG-SELD                      PIC S9(4) COMP-5 VALUE 0.
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

      * Variaveis para gravacao do log
       01  DADOS-LOG-DISPLAY.
           03  NR-NOTA-TCN-OPR-LOG           PIC  9(5) VALUE 0.

      * Book dos dados do log
       01  DADOS-LOG.
-INC SGTK014G

       01  SGTS014G VALUE 'SGTS014G'         PIC X(8).

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

-INC SGTKE055

001400* Book dos dados do usuário
-INC IIBKOPRD

000600* Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1

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
-INC HLPKE999

           PERFORM 100000-DESCOBRIR-CANAL
           PERFORM 120000-EXCLUIR-CTNR-ERRO
           PERFORM 130000-RECEBER-REQUISICAO
           PERFORM 140000-VALIDA-PARAMETRO

           PERFORM 200000-EXCLUIR
      *    MOVE 'Exclusão efetuada com sucesso.' TO SGTSE055-TX-RTN-PRCT
      *    PERFORM 300000-POSTA-RESPOSTA

           GOBACK
           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-DESCOBRIR-CANAL SECTION.
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
       120000-EXCLUIR-CTNR-ERRO SECTION.
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
       130000-RECEBER-REQUISICAO SECTION.
      *-----------------------------------------------------------------
           MOVE 0004                   TO  MSG1-CD-SEQL-ERRO

           MOVE ZEROS                      TO  QT-CCT-LIDO
           MOVE LENGTH OF SGTSE055-RQSC    TO  QT-CCT-EPRO

           EXEC CICS GET
              CONTAINER ( 'SGTSE055-RQSC' )
              CHANNEL   ( CANAL      )
              NODATA
              FLENGTH ( QT-CCT-LIDO )
           END-EXEC

           IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
               SET   ERRO-TAMANHO-AREA   TO  TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS GET
              CONTAINER ( 'SGTSE055-RQSC' )
              CHANNEL   ( CANAL      )
              FLENGTH ( LENGTH OF SGTSE055-RQSC )
              INTO    ( SGTSE055-RQSC )
           END-EXEC
           .
       130000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       140000-VALIDA-PARAMETRO SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                   TO  MSG1-CD-SEQL-ERRO

           PERFORM 150000-VALIDA-CAMPOS

           PERFORM 170002-VALIDA-UOR-ITVT-NOTA-TC
           .
       140000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       150000-VALIDA-CAMPOS SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO  MSG1-CD-SEQL-ERRO

           IF  SGTSE055-NR-NOTA-TCN-OPR    EQUAL ZEROS
               SET  ERRO-PROGRAMADO        TO  TRUE
               MOVE 181                    TO  MSG1-CD-ERRO-TS
               MOVE 'Numero nota tecnico operacao inválido.'
                                           TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       150000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       170002-VALIDA-UOR-ITVT-NOTA-TC SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO  MSG1-CD-SEQL-ERRO

           MOVE ZEROS                  TO  QT-REG-SELD

           EXEC SQL
             SELECT 1
              INTO :QT-REG-SELD
               FROM DB2SGT.UOR_ITVT_NOTA_TCN
               WHERE NR_NOTA_TCN_OPR    =  :SGTSE055-NR-NOTA-TCN-OPR
             FETCH FIRST 1 ROW ONLY
           END-EXEC

           IF  QT-REG-SELD NOT EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE 0388               TO  MSG1-CD-ERRO-TS
               MOVE 'Foi encontrada UOR interveniente vinculada à nota t
      -        'écnica'                TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       170002-SAI.
           EXIT.
      *-----------------------------------------------------------------
       200000-EXCLUIR SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO  MSG1-CD-SEQL-ERRO

           INITIALIZE DADOS-LOG
           MOVE 'E'                    TO  SGTS014G-CD-TIP-ACAO
           PERFORM 980102-GRAVA-LOG

           EXEC SQL
                DELETE FROM DB2SGT.NOTA_TCN_OPR_ETRD
                WHERE NR_NOTA_TCN_OPR   =  :SGTSE055-NR-NOTA-TCN-OPR
           END-EXEC

           IF  SQLCODE EQUAL +100
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE 100                TO  MSG1-CD-ERRO-TS
               MOVE 'Exclusao nao efetuada, registro inexistente'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       200000-SAI.
           EXIT.
      *-----------------------------------------------------------------
      *300000-POSTA-RESPOSTA           SECTION.
      *-----------------------------------------------------------------
      *    MOVE 0009                   TO  MSG1-CD-SEQL-ERRO
      *
      *    EXEC CICS PUT
      *       CONTAINER ( 'SGTSE055-RPST' )
      *       CHANNEL   ( CANAL      )
      *       FLENGTH ( LENGTH OF SGTSE055-RPST )
      *       FROM    ( SGTSE055-RPST )
      *    END-EXEC
      *    .
      *300000-SAI.
      *    EXIT.
      *-----------------------------------------------------------------
       980102-GRAVA-LOG SECTION.
      *-----------------------------------------------------------------
           MOVE 0010                   TO  MSG1-CD-SEQL-ERRO

           MOVE 'NOTA_TCN_OPR_ETRD '   TO  SGTS014G-NM-ENT

           IF  EXECUCAO-ONLINE
               PERFORM 980103-LER-DADOS-OPRD
               MOVE  KOPRD-CD-OPRD         TO  SGTS014G-CD-CHV-USU
           ELSE
               MOVE 'SYSTEM'           TO  SGTS014G-CD-CHV-USU
           END-IF

           MOVE 'SGTSE055'                 TO  SGTS014G-NM-PGM
           MOVE SGTSE055-NR-NOTA-TCN-OPR   TO  NR-NOTA-TCN-OPR-LOG

           STRING NR-NOTA-TCN-OPR-LOG
               DELIMITED BY SIZE INTO SGTS014G-CHV-PRMR

           MOVE SGTS014G               TO  NM-PGM
           MOVE LENGTH OF DADOS-LOG    TO  EIBCALEN
           CALL SGTS014G USING DFHEIBLK DADOS-LOG

           IF  SGTS014G-CD-RTN-PGM  NOT    EQUAL ZEROS
               SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE  SGTS014G          TO  MSG1-NM-PGM-ACND
               MOVE  112               TO MSG1-CD-ERRO-TS
               MOVE  'Erro na gravação das informações do Log.'
                                       TO MSG1-TX-ERRO-TS

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
           MOVE 0011                   TO MSG1-CD-SEQL-ERRO

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

           WHEN  OTHER
                 CONTINUE
           END-EVALUATE
           .
       998000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA SECTION.
      *-----------------------------------------------------------------
      *--> Monitora variàveis do pgm, grava e encerra

           IF  MSG1-CD-ERRO-TS             EQUAL  ZEROS
              MOVE  8888               TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS             EQUAL  SPACES
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

           MOVE 'SGTSE055-NR-NOTA-TCN-OPR'   TO  MSGKW999-NM-VRV-MNT
           MOVE SGTSE055-NR-NOTA-TCN-OPR     TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

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
      * MSG1-NTF-MSG                   SECTION.
-INC MSGKPMG1
      * --------------------------------------*
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *---------------------------------------*
-INC SGTKP999
      * --------------------------------------*
       999999-ENCERRAR                SECTION.
      * --------------------------------------*
           GOBACK.