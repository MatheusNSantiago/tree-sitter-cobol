      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSI059
      * ANALISTA..: F0175702 - ADRIANO SEIJI NAKAMURA
      * AUTOR.....: F0175702 - ADRIANO SEIJI NAKAMURA
      * OBJETIVO..: INCLUSAO DA TABELA ARQ_OPR_ETRD
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 02/02/2017 F8785286 DE 2016/145195 - Inclusão leitura
      *              container operador para gravação no LOG.
      *        09.11.2016 - F0175702 DE 2015/137233 - IMPLANTACAO
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSI059.

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
       77  CTE-INICIO-SS                 PIC  X(040)  VALUE
                   '*** S.S. COMEÇA AQUI ***'.

       77  CTE-PROG     PIC  X(17) VALUE '*** SGTSI059 ***'.

       77  CTE-VERS     PIC  X(06) VALUE 'VRS001'.

      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informações
-INC HLPKE01B
      *-----------------------------------------------------------------
       LOCAL-STORAGE SECTION.
      *-----------------------------------------------------------------
      * Book de para acionamento do MSG1
      * Book das variáveis auxiliares do tratamento de erro via MSG       VRS001
-INC SGTKERR1

-INC IIBKERRO

-INC MSGKWMG1

-INC HLPKW999

      * Area de trabalho

       01  QT-REG-SELD                      PIC S9(4) COMP-5 VALUE 0.

-INC HLPKTIME
       01  TS-SQL-ATU                       PIC  X(26) VALUE ' '.

      * Indicadores de nulidades

      * Variaveis para gravacao do log
       01  DADOS-LOG-DISPLAY.
           03  CD-ARQ-OPR-ETRD-LOG           PIC  9(10) VALUE 0.

      * Book dos dados do usurio                                         VRS001
-INC IIBKOPRD

      * Book dos dados do log
       01  DADOS-LOG.
-INC SGTK014G

       01  SGTS014G VALUE 'SGTS014G'         PIC X(8).

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

-INC SGTKI059

       77  CTE-FINAL-SS                PIC  X(40)      VALUE
                    '*** S.S. TERMINA AQUI ***'.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
      *
       000000-PRINCIPAL                SECTION.
           IF NOT EXECUCAO-ONLINE AND NOT  EXECUCAO-BATCH
              CALL CICS5000 USING CICS5000-COMMAREA
           END-IF

           MOVE 0001                   TO  MSG1-CD-SEQL-ERRO

      * Habilita tratamento automático de Erros DB2
-INC HLPKE997
      * Habilita captura automática de Abend e Erros CICS
-INC HLPKE999

           PERFORM 100000-DESCOBRIR-CANAL
           PERFORM 120000-EXCLUIR-CTNR-ERRO
           PERFORM 130000-RECEBER-REQUISICAO
           PERFORM 140000-VALIDA-PARAMETRO
           PERFORM 980103-LER-DADOS-OPRD

           PERFORM 200000-INCLUIR
           MOVE  SGTSI059-CD-ARQ-OPR-ETRD        TO SGTSI059-CD-ARQ-OPR
           PERFORM 300000-POSTA-RESPOSTA

           GOBACK
           .
       000000-SAI.
           EXIT.
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
001254 100000-SAI.
001254     EXIT.

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
           MOVE LENGTH OF SGTSI059-RQSC    TO  QT-CCT-EPRO

           EXEC CICS GET
              CONTAINER ( 'SGTSI059-RQSC' )
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
              CONTAINER ( 'SGTSI059-RQSC'         )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSI059-RQSC )
              INTO      ( SGTSI059-RQSC           )
           END-EXEC
           .
       130000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       140000-VALIDA-PARAMETRO SECTION.
      *-----------------------------------------------------------------

           PERFORM 150000-VALIDA-CAMPOS

           PERFORM 170000-VALIDA-TIP-ARQ-OPR
           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       150000-VALIDA-CAMPOS SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                   TO  MSG1-CD-SEQL-ERRO

           PERFORM 151000-BUSCA-TS-DT
           MOVE HLPKE001-TS-FMTD       TO  TS-SQL-ATU

           IF  SGTSI059-CD-TIP-ARQ         LESS ZEROS
               SET  ERRO-PROGRAMADO                 TO  TRUE
               MOVE 322                             TO  MSG1-CD-ERRO-TS
               MOVE 'Código tipo arquivo inválido.' TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI059-CD-TIP-ARQ         EQUAL ZEROS
               SET  ERRO-PROGRAMADO        TO  TRUE
               MOVE 323                    TO  MSG1-CD-ERRO-TS
               MOVE 'Código tipo arquivo não informado.'
                                           TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI059-TS-INCL-ARQ-OPR    EQUAL SPACES
               SET  ERRO-PROGRAMADO        TO  TRUE
               MOVE 324                    TO  MSG1-CD-ERRO-TS
               MOVE 'Data e hora inclusão arquivo operação não informado
      -             '.'                    TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI059-TS-INCL-ARQ-OPR EQUAL 'CURRENT'
               MOVE TS-SQL-ATU             TO  SGTSI059-TS-INCL-ARQ-OPR
           END-IF
           .
       150000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       151000-BUSCA-TS-DT SECTION.
      *-----------------------------------------------------------------
001254
001254     CALL SBTIMEDT            USING  HLPKE001-CD-FUC
001254                                     HLPKE001-AREA-TRB
001254                                     HLPKE001-FMT-TS
001254                                     HLPKE001-TAM-TS
001254                                     HLPKE001-TS-FMTD

           IF  RETURN-CODE      NOT EQUAL ZEROS
               SET  ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE RETURN-CODE        TO  MSG1-CD-RTN-PGM-ACND
               MOVE 'SBTIMEDT'         TO  MSG1-NM-PGM-ACND
                                           NM-PGM
               MOVE 0050               TO  MSG1-CD-ERRO-TS
               MOVE 'Erro ao obter data e hora atual.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 998000-MONITORAR-VRV-PGM-CHMD
001254         PERFORM 999000-GRAVA-ERRO-E-ENCERRA
001254     END-IF
           .
       151000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       170000-VALIDA-TIP-ARQ-OPR      SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO  MSG1-CD-SEQL-ERRO

           MOVE ZEROS                  TO  QT-REG-SELD

           EXEC SQL
             SELECT 1
              INTO :QT-REG-SELD
               FROM DB2SGT.TIP_ARQ_OPR
               WHERE CD_TIP_ARQ         =  :SGTSI059-CD-TIP-ARQ
           END-EXEC

           IF  QT-REG-SELD = 0
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE 325                TO  MSG1-CD-ERRO-TS
               MOVE 'Tipo arquivo operação inexistente.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       170000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-INCLUIR SECTION.
      *-----------------------------------------------------------------

           PERFORM 430000-BUSCA-NR-SEQUENCIA

           MOVE 0008                   TO  MSG1-CD-SEQL-ERRO

           EXEC SQL
                INSERT INTO DB2SGT.ARQ_OPR_ETRD
                ( CD_ARQ_OPR_ETRD
                , CD_TIP_ARQ
                , CD_USU_RSP_INCL
                , TS_INCL_ARQ_OPR )
                VALUES
                ( :SGTSI059-CD-ARQ-OPR-ETRD
                , :SGTSI059-CD-TIP-ARQ
                , :KOPRD-CD-OPRD
                , :SGTSI059-TS-INCL-ARQ-OPR )
           END-EXEC

           INITIALIZE DADOS-LOG
           MOVE 'I'                    TO  SGTS014G-CD-TIP-ACAO
           PERFORM 980102-GRAVA-LOG
           .
       200000-SAI.
           EXIT.

      *----------------------------------------------------------------
       300000-POSTA-RESPOSTA SECTION.
      *----------------------------------------------------------------
           MOVE 0009                   TO  MSG1-CD-SEQL-ERRO

           EXEC CICS PUT
             CONTAINER ( 'SGTSI059-RPST'         )
              CHANNEL  ( NM-CNL                  )
              FLENGTH  ( LENGTH OF SGTSI059-RPST )
              FROM     ( SGTSI059-RPST           )
           END-EXEC
           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       430000-BUSCA-NR-SEQUENCIA SECTION.
      *-----------------------------------------------------------------
           MOVE 0010                   TO  MSG1-CD-SEQL-ERRO

           EXEC SQL
               SELECT VALUE (MAX(CD_ARQ_OPR_ETRD) , 0 )
                INTO :SGTSI059-CD-ARQ-OPR-ETRD
                FROM DB2SGT.ARQ_OPR_ETRD
           END-EXEC

           ADD 1                       TO  SGTSI059-CD-ARQ-OPR-ETRD
           .
       430000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       980102-GRAVA-LOG SECTION.
      *-----------------------------------------------------------------
           MOVE 0011                   TO  MSG1-CD-SEQL-ERRO

           MOVE 'ARQ_OPR_ETRD      '       TO  SGTS014G-NM-ENT

           IF  EXECUCAO-ONLINE
               MOVE  KOPRD-CD-OPRD         TO  SGTS014G-CD-CHV-USU
           ELSE
               MOVE 'SYSTEM'               TO  SGTS014G-CD-CHV-USU
           END-IF

           MOVE 'SGTSI059'                 TO  SGTS014G-NM-PGM
           MOVE SGTSI059-CD-ARQ-OPR-ETRD   TO  CD-ARQ-OPR-ETRD-LOG

           STRING CD-ARQ-OPR-ETRD-LOG
               DELIMITED BY SIZE INTO SGTS014G-CHV-PRMR
           END-STRING

           MOVE LENGTH OF DADOS-LOG        TO  EIBCALEN
           CALL SGTS014G USING DFHEIBLK DADOS-LOG

           IF  SGTS014G-CD-RTN-PGM    NOT  EQUAL ZEROS
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

               PERFORM 998000-MONITORAR-VRV-PGM-CHMD
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       980102-SAI.                                                        VRS001
           EXIT.

      *----------------------------------------------------------------*  VRS001
       980103-LER-DADOS-OPRD           SECTION.                           VRS001
      *----------------------------------------------------------------*  VRS001
           MOVE 0012                   TO  MSG1-CD-SEQL-ERRO              VRS001
                                                                          VRS001
      *Configura dados para leitura do container de requisição            VRS001
           SET  GET-CONTAINER          TO  TRUE                           VRS001
           MOVE 'PRM-INF-OPRD'         TO  NM-CTNR                        VRS001
                                                                          VRS001
      *Por causa da captura automática de erro CICS, deve ser previsto    VRS001
      *o erro de container                                                VRS001
           SET  ERRO-CONTAINER         TO  TRUE                           VRS001
                                                                          VRS001
           INITIALIZE PRM-INF-OPRD                                        VRS001
           EXEC CICS GET                                                  VRS001
                CONTAINER ( NM-CTNR                )                      VRS001
                INTO      ( PRM-INF-OPRD           )                      VRS001
                FLENGTH   ( LENGTH OF PRM-INF-OPRD )                      VRS001
           END-EXEC.                                                      VRS001
                                                                          VRS001
      *    Ao chegar aqui, há certeza de que não houve erro algum         VRS001
           SET  SEM-ERRO               TO  TRUE                           VRS001
           .                                                              VRS001
       980103-SAI.                                                        VRS001
           EXIT.

      *----------------------------------------------------------------
       998000-MONITORAR-VRV-PGM-CHMD   SECTION.
      *----------------------------------------------------------------
      *--> Monitora as variáveis dos módulos chamados

           EVALUATE  NM-PGM

               WHEN  SGTS014G
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

               WHEN  SBTIMEDT
                    MOVE  'HLPKE001-CD-FUC'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  HLPKE001-CD-FUC
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'HLPKE001-AREA-TRB'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  HLPKE001-AREA-TRB
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'HLPKE001-FMT-TS'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  HLPKE001-FMT-TS
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'HLPKE001-TAM-TS'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  HLPKE001-TAM-TS
                                       TO   MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'HLPKE001-TS-FMTD'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  HLPKE001-TS-FMTD
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  OTHER
                    CONTINUE
           END-EVALUATE
           .
       998000-SAI.
           EXIT.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA SECTION.
      *-----------------------------------------------------------------
      *    Em caso de erro, desfaz as atualizaç es em banco de dados
      *    Foi tratado rollback neste modulo por estar exposto ao IIB.
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

           MOVE 'SGTSI059-CD-ARQ-OPR-ETRD'   TO  MSGKW999-NM-VRV-MNT
           MOVE SGTSI059-CD-ARQ-OPR-ETRD     TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSI059-CD-TIP-ARQ'        TO  MSGKW999-NM-VRV-MNT
           MOVE SGTSI059-CD-TIP-ARQ          TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'KOPRD-CD-OPRD'              TO  MSGKW999-NM-VRV-MNT
           MOVE KOPRD-CD-OPRD                TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSI059-TS-INCL-ARQ-OPR'   TO  MSGKW999-NM-VRV-MNT
           MOVE SGTSI059-TS-INCL-ARQ-OPR     TO  MSGKW999-VL-VRV-MNT
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
-INC MSGKPMG1
      * --------------------------------------*
       999999-ENCERRAR                SECTION.
      * --------------------------------------*
           GOBACK
           .
       999999-SAI.
           EXIT.