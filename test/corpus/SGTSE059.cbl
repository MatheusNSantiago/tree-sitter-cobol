      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSE059
      * ANALISTA..: F0175702 - ADRIANO SEIJI NAKAMURA
      * AUTOR.....: F0175702 - ADRIANO SEIJI NAKAMURA
      * OBJETIVO..: EXCLUSAO DA TABELA ARQ_OPR_ETRD
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 31/01/2017 F8785286 DE 2016/145195 - Inclusão leitura
      *              container operador para gravação no LOG.
      *              Inclusão da section 300000-POSTA-RESPOSTA.
      *        09.11.2016 - F0175702 - DE 2015/137233 - IMPLANTACAO
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSE059.

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
       77  CTE-INICIO-SS               PIC  X(40)  VALUE
                   '*** S.S. COMEÇA AQUI ***'.
       77  CTE-PROG     PIC  X(17) VALUE '*** SGTSE059 ***'.
       77  CTE-VERS     PIC  X(06) VALUE 'VRS001'.

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

       01  QT-REG-SELD                      PIC S9(4) COMP-5 VALUE 0.
       01  TS-SQL-ATU                       PIC  X(26) VALUE ' '.

       01  K-INTEGER                        PIC S9(9) COMP-5 VALUE 0.
       01  K-SMALLINT                       PIC S9(4) COMP-5 VALUE 0.

      * Indicadores de nulidades

       01  QT-REG-SELD-NULO                 PIC S9(4) COMP-5 VALUE 0.

      * Variaveis para gravacao do log
       01  DADOS-LOG-DISPLAY.
           03  CD-ARQ-OPR-ETRD-LOG           PIC  9(10) VALUE 0.

      * Book dos dados do log
       01  DADOS-LOG.
-INC SGTK014G

       01  SGTS014G VALUE 'SGTS014G'         PIC X(8).


           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

-INC SGTKE059

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
-INC HLPKE999

           PERFORM 100000-DESCOBRIR-CANAL
           PERFORM 120000-EXCLUIR-CTNR-ERRO
           PERFORM 130000-RECEBER-REQUISICAO
           PERFORM 140000-VALIDA-PARAMETRO

           PERFORM 200000-EXCLUIR

           PERFORM 999999-ENCERRAR
           .
      *-----------------------------------------------------------------
       100000-DESCOBRIR-CANAL SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO  MSG1-CD-SEQL-ERRO

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET  ERRO-CANAL             TO TRUE
001254
001254     EXEC CICS ASSIGN
001254        CHANNEL ( NM-CNL )
001254     END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
001254     .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       120000-EXCLUIR-CTNR-ERRO SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                   TO  MSG1-CD-SEQL-ERRO

           EXEC CICS DELETE
              CONTAINER ( 'PRM-ERRO' )
              CHANNEL   ( NM-CNL     )
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
           MOVE LENGTH OF SGTSE059-RQSC    TO  QT-CCT-EPRO

           EXEC CICS GET
              CONTAINER ( 'SGTSE059-RQSC' )
              CHANNEL   ( NM-CNL          )
              NODATA
              FLENGTH   ( QT-CCT-LIDO     )
           END-EXEC

      * 3001
           IF  QT-CCT-LIDO   NOT   EQUAL   QT-CCT-EPRO
               SET  ERRO-TAMANHO-AREA  TO  TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS GET
              CONTAINER ( 'SGTSE059-RQSC'         )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSE059-RQSC )
              INTO      ( SGTSE059-RQSC           )
           END-EXEC
           .
       130000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       140000-VALIDA-PARAMETRO SECTION.
      *-----------------------------------------------------------------

           PERFORM 150000-VALIDA-CAMPOS

           PERFORM 170002-VALIDA-ANX-OPR-ETRD

           PERFORM 170003-VALIDA-ORD-NEGO-OPR

           PERFORM 170004-VALIDA-NOTA-TCN-OPR

           PERFORM 170005-VALIDA-ISLC-OPR-ETRD

           PERFORM 170006-VALIDA-NOTA-CLSC-PCT

           PERFORM 170007-VALIDA-NOTA-CLSC-OPR
           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       150000-VALIDA-CAMPOS SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                   TO  MSG1-CD-SEQL-ERRO

           IF  SGTSE059-CD-ARQ-OPR-ETRD    EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE 149                    TO  MSG1-CD-ERRO-TS
               MOVE 'Código arquivo da operação estruturada não informad
      -        'o.'                    TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSE059-CD-ARQ-OPR-ETRD    LESS ZEROS
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE 150                TO  MSG1-CD-ERRO-TS
               MOVE 'Código arquivo da operação estruturada inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       150000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       170002-VALIDA-ANX-OPR-ETRD     SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO  MSG1-CD-SEQL-ERRO

           MOVE ZEROS                  TO  QT-REG-SELD

           EXEC SQL
             SELECT 1
              INTO :QT-REG-SELD
               FROM DB2SGT.ANX_OPR_ETRD
               WHERE CD_ARQ_OPR_ETRD    =  :SGTSE059-CD-ARQ-OPR-ETRD
             FETCH FIRST 1 ROW ONLY
           END-EXEC

           IF  QT-REG-SELD   NOT   EQUAL   ZEROS
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE 373                    TO  MSG1-CD-ERRO-TS
               MOVE 'Existe anexo vinculado a operação estruturada.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       170002-SAI.
           EXIT.

      *-----------------------------------------------------------------
       170003-VALIDA-ORD-NEGO-OPR     SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO  MSG1-CD-SEQL-ERRO

           MOVE ZEROS                  TO  QT-REG-SELD

           EXEC SQL
             SELECT 1
              INTO :QT-REG-SELD
               FROM DB2SGT.ORD_NEGO_OPR
               WHERE CD_ARQ_OPR_ETRD    =  :SGTSE059-CD-ARQ-OPR-ETRD
               AND CD_CTRA_TIT_VL > :K-INTEGER
             FETCH FIRST 1 ROW ONLY
           END-EXEC

           IF  QT-REG-SELD   NOT   EQUAL   ZEROS
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE 374                    TO  MSG1-CD-ERRO-TS
               MOVE 'Existe ordem de negociação vinculada ao arquivo.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       170003-SAI.
           EXIT.

      *-----------------------------------------------------------------
       170004-VALIDA-NOTA-TCN-OPR     SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO  MSG1-CD-SEQL-ERRO

           MOVE ZEROS                  TO  QT-REG-SELD

           EXEC SQL
             SELECT 1
              INTO :QT-REG-SELD
               FROM DB2SGT.NOTA_TCN_OPR_ETRD
               WHERE CD_ARQ_OPR_ETRD    =  :SGTSE059-CD-ARQ-OPR-ETRD
               AND NR_NOTA_TCN_OPR > :K-SMALLINT
             FETCH FIRST 1 ROW ONLY
           END-EXEC

           IF  QT-REG-SELD   NOT   EQUAL   ZEROS
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE 375                    TO  MSG1-CD-ERRO-TS
               MOVE 'Existe nota técnica vinculada ao arquivo.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       170004-SAI.
           EXIT.

      *-----------------------------------------------------------------
       170005-VALIDA-ISLC-OPR-ETRD     SECTION.
      *-----------------------------------------------------------------
           MOVE 0009                   TO  MSG1-CD-SEQL-ERRO

           MOVE ZEROS                  TO  QT-REG-SELD

           EXEC SQL
             SELECT 1
              INTO :QT-REG-SELD
               FROM DB2SGT.ISLC_OPR_ETRD
               WHERE CD_ARQ_OPR_ETRD    =  :SGTSE059-CD-ARQ-OPR-ETRD
               AND CD_CTRA_TIT_VL > :K-INTEGER
             FETCH FIRST 1 ROW ONLY
           END-EXEC

           IF  QT-REG-SELD   NOT   EQUAL   ZEROS
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE 376                    TO  MSG1-CD-ERRO-TS
               MOVE 'Existe documento de default vinculado ao arquivo.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       170005-SAI.
           EXIT.

      *-----------------------------------------------------------------
       170006-VALIDA-NOTA-CLSC-PCT     SECTION.
      *-----------------------------------------------------------------
           MOVE 0010                   TO  MSG1-CD-SEQL-ERRO

           MOVE ZEROS                  TO  QT-REG-SELD

           EXEC SQL
             SELECT 1
              INTO :QT-REG-SELD
               FROM DB2SGT.NOTA_CLSC_PCT_OPR
               WHERE CD_ARQ_OPR_ETRD    =  :SGTSE059-CD-ARQ-OPR-ETRD
               AND CD_CLI > :K-INTEGER
             FETCH FIRST 1 ROW ONLY
           END-EXEC

           IF  QT-REG-SELD   NOT   EQUAL   ZEROS
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE 377                    TO  MSG1-CD-ERRO-TS
               MOVE 'Existe nota de classificação do participante vincul
      -           'ada ao arquivo.'    TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       170006-SAI.
           EXIT.

      *-----------------------------------------------------------------
       170007-VALIDA-NOTA-CLSC-OPR     SECTION.
      *-----------------------------------------------------------------
           MOVE 0011                   TO  MSG1-CD-SEQL-ERRO

           MOVE ZEROS                  TO  QT-REG-SELD

           EXEC SQL
             SELECT 1
              INTO :QT-REG-SELD
               FROM DB2SGT.NOTA_CLSC_OPR
               WHERE CD_ARQ_OPR_ETRD    =  :SGTSE059-CD-ARQ-OPR-ETRD
               AND CD_CTRA_TIT_VL > :K-INTEGER
             FETCH FIRST 1 ROW ONLY
           END-EXEC

           IF  QT-REG-SELD   NOT   EQUAL   ZEROS
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE 378                    TO  MSG1-CD-ERRO-TS
               MOVE 'Existe nota de classificação da operação vinculada
      -        'ao arquivo.'           TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       170007-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-EXCLUIR SECTION.
      *-----------------------------------------------------------------
           MOVE 0012                   TO  MSG1-CD-SEQL-ERRO

           INITIALIZE DADOS-LOG
           MOVE 'E'                    TO  SGTS014G-CD-TIP-ACAO
           PERFORM 980102-GRAVA-LOG

           EXEC SQL
                DELETE FROM DB2SGT.ARQ_OPR_ETRD
                WHERE CD_ARQ_OPR_ETRD   =  :SGTSE059-CD-ARQ-OPR-ETRD
           END-EXEC

           IF  SQLCODE EQUAL +100
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE 100                    TO  MSG1-CD-ERRO-TS
               MOVE 'Registro inexistente.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       980102-GRAVA-LOG SECTION.
      *-----------------------------------------------------------------
           MOVE 0013                   TO  MSG1-CD-SEQL-ERRO

           MOVE 'ARQ_OPR_ETRD      '   TO  SGTS014G-NM-ENT

           IF  EXECUCAO-ONLINE
               PERFORM 980103-LER-DADOS-OPRD
               MOVE  KOPRD-CD-OPRD     TO  SGTS014G-CD-CHV-USU
           ELSE
               MOVE 'SYSTEM'           TO  SGTS014G-CD-CHV-USU
           END-IF

           MOVE 'SGTSE059'                 TO  SGTS014G-NM-PGM
           MOVE SGTSE059-CD-ARQ-OPR-ETRD   TO  CD-ARQ-OPR-ETRD-LOG

           STRING CD-ARQ-OPR-ETRD-LOG
               DELIMITED BY SIZE INTO SGTS014G-CHV-PRMR

           MOVE SGTS014G               TO  NM-PGM
           MOVE LENGTH OF DADOS-LOG    TO  EIBCALEN
           CALL  SGTS014G           USING  DFHEIBLK DADOS-LOG

           IF  SGTS014G-CD-RTN-PGM         NOT EQUAL  ZEROS
               SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE  SGTS014G          TO  MSG1-NM-PGM-ACND
               MOVE  SGTS014G-CD-RTN-PGM
                                       TO  MSG1-CD-RTN-PGM-ACND
               MOVE  SGTS014G-MSG-ERRO-PGM
                                       TO  MSG1-TX-RTN-PGM-ACND
               MOVE  112               TO  MSG1-CD-ERRO-TS
               MOVE 'Erro na gravação das informações do Log.'
                                       TO  MSG1-TX-ERRO-TS
      *
               MOVE  'SGTS014G-CD-TIP-ACAO'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTS014G-CD-TIP-ACAO
                                       TO    MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE  'SGTS014G-NM-ENT'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTS014G-NM-ENT
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE  'SGTS014G-CD-CHV-USU'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTS014G-CD-CHV-USU
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE  'SGTS014G-NM-PGM'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTS014G-NM-PGM
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE  'SGTS014G-CHV-PRMR'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTS014G-CHV-PRMR
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       980102-SAI.
           EXIT.

008160*----------------------------------------------------------------*
008170 980103-LER-DADOS-OPRD           SECTION.
008180*----------------------------------------------------------------*
008190     MOVE 0014                   TO  MSG1-CD-SEQL-ERRO
008200
008210*Configura dados para leitura do container de requisição
008220     SET  GET-CONTAINER          TO  TRUE
008230     MOVE 'PRM-INF-OPRD'         TO  NM-CTNR
008240
008250*Por causa da captura automática de erro CICS, deve ser previsto
008260*o erro de container
008270     SET  ERRO-CONTAINER         TO  TRUE
008280
008290     INITIALIZE PRM-INF-OPRD
008300     EXEC CICS GET
008310          CONTAINER ( NM-CTNR                )
008320          INTO      ( PRM-INF-OPRD           )
008330          FLENGTH   ( LENGTH OF PRM-INF-OPRD )
008340     END-EXEC.
008350
008360*    Ao chegar aqui, há certeza de que não houve erro algum
008370     SET  SEM-ERRO               TO  TRUE
008380     .
008390 980103-SAI.
008400     EXIT.

      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA SECTION.
      *-----------------------------------------------------------------
      *    Em caso de erro, desfaz as atualizaç es em banco de dados

           EXEC CICS SYNCPOINT
                     ROLLBACK

           END-EXEC
      *
           IF  MSG1-CD-ERRO-TS EQUAL ZEROS
               MOVE 8888                     TO MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS EQUAL SPACES
               MOVE 'Erro não especificado no módulo.'
                                             TO MSG1-TX-ERRO-TS
           END-IF

      *--> Verifica se o tipo de erro gerado possui tratamento interno.
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

      *==> Observação <=================================================
      *    Normalmente se Monitora apenas as Variáveis da Requisição
      *    para Monitoração da Mensageria.
      *    O Analista que especificou o módulo e o Analista Programador
      *    O Analista que especificou o módulo e o Analista Programador
      *    tem liberdade para Monitorar outras Variáveis que forem
      *    relevantes ao Suporte do Sistema.
      *=================================================================

           MOVE 'SGTSE059-CD-ARQ-OPR-ETRD'   TO  MSGKW999-NM-VRV-MNT
           MOVE SGTSE059-CD-ARQ-OPR-ETRD     TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

      * --> grava o erro no msg
           PERFORM MSG1-NTF-MSG
           MOVE MSG1-CD-ERRO-TS              TO  IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS              TO  IIBKERRO-TX-ERRO

           EXEC CICS PUT
              CONTAINER ( 'PRM-ERRO'                  )
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
-INC MSGKPMG1

      * --------------------------------------*
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * MSG1-CD-ERRO-TS = 3000
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * MSG1-CD-ERRO-TS = 3001
      * 999006-TRATA-ERRO-CANAL        SECTION.
      * MSG1-CD-ERRO-TS = 3003
-INC SGTKP999
      * --------------------------------------*
       999999-ENCERRAR                SECTION.
      * --------------------------------------*
           GOBACK
           .
       999999-SAI.
           EXIT.