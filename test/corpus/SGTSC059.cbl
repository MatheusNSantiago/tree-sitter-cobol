      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSC059
      * ANALISTA..: F2054109 - CINTIA AP SOARES IGLESIAS
      * AUTOR.....: F2054109 - CINTIA AP SOARES IGLESIAS
      * OBJETIVO..: CONSULTA DA TABELA ARQ_OPR_ETRD
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 09/02/2018 F8785286 DE 2016/145195 - Acerto dos códigos
      *          das mensagens para mensageria.
      *        31.10.2017 - F2054109 DE 2016/145195 - Implantação
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSC059.
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
       77  CTE-PROG     PIC  X(17) VALUE '*** SGTSC059 ***'.
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

      * Area de trabalho

      * Indicadores de nulidades

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

-INC SGTKC059

      * Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1

       01  CTE-FINAL-SS                PIC  X(40)      VALUE
                    '*** S.S. TERMINA AQUI ***'.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
           IF NOT EXECUCAO-ONLINE AND NOT EXECUCAO-BATCH
             CALL CICS5000 USING CICS5000-COMMAREA
           END-IF

           MOVE 0001                   TO MSG1-CD-SEQL-ERRO

      * Habilita tratamento automatico de Erros DB2
-INC HLPKE997
      * Habilita captura automatica de Abend e Erros CICS
-INC HLPKE999

           PERFORM 100000-DESCOBRIR-CANAL
           PERFORM 110000-EXCLUIR-CTNR-RPST
           PERFORM 120000-EXCLUIR-CTNR-ERRO
           PERFORM 130000-RECEBER-REQUISICAO
           PERFORM 150000-VALIDA-CAMPOS

           PERFORM 200000-CONSULTAR
           PERFORM 300000-POSTA-RESPOSTA

           GOBACK
           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-DESCOBRIR-CANAL SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CANAL             TO  TRUE

           EXEC CICS ASSIGN
              CHANNEL ( NM-CNL )
           END-EXEC

      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO  TRUE
           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-EXCLUIR-CTNR-RPST SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS DELETE
              CONTAINER ( 'SGTSC059-RPST' )
              CHANNEL   ( NM-CNL          )
              NOHANDLE
           END-EXEC

           IF  NOT (EIBRESP EQUAL DFHRESP(NORMAL)   OR
               EIBRESP EQUAL DFHRESP(CONTAINERERR) )
               PERFORM 999002-TRATA-CONDITION-CICS
           END-IF
           .
       110000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       120000-EXCLUIR-CTNR-ERRO SECTION.
      *-----------------------------------------------------------------
           MOVE 0004                   TO MSG1-CD-SEQL-ERRO

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
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

           MOVE ZEROS                       TO QT-CCT-LIDO
           MOVE LENGTH OF SGTSC059-RQSC      TO QT-CCT-EPRO

           EXEC CICS GET
              CONTAINER ( 'SGTSC059-RQSC' )
              CHANNEL   ( NM-CNL          )
              NODATA
              FLENGTH   ( QT-CCT-LIDO     )
           END-EXEC

           IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
               SET   ERRO-TAMANHO-AREA   TO  TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS GET
              CONTAINER ( 'SGTSC059-RQSC'         )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSC059-RQSC )
              INTO      ( SGTSC059-RQSC           )
           END-EXEC
           .
       130000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       150000-VALIDA-CAMPOS SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

           IF  SGTSC059-CD-ARQ-OPR-ETRD EQUAL ZEROS
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 0150                TO  MSG1-CD-ERRO-TS
               MOVE 'Código arquivo operação estruturado inválido.'
                                        TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       150000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-CONSULTAR SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

           EXEC SQL
                 SELECT CD_TIP_ARQ
                      , CD_USU_RSP_INCL
                      , TS_INCL_ARQ_OPR
                   INTO  :SGTSC059-CD-TIP-ARQ
                       , :SGTSC059-CD-USU-RSP-INCL
                       , :SGTSC059-TS-INCL-ARQ-OPR
                       FROM DB2SGT.ARQ_OPR_ETRD
                WHERE CD_ARQ_OPR_ETRD    = :SGTSC059-CD-ARQ-OPR-ETRD
           END-EXEC

           IF  SQLCODE = +100
               SET  ERRO-PROGRAMADO     TO TRUE

      * Manter o codigo do erro = 100 para que o modulo de negocio
      * consiga identificar inexistencia de registro

               MOVE 100                 TO  MSG1-CD-ERRO-TS
               MOVE 'Arquivo operacao estruturado inexistente'
                                        TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       200000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       300000-POSTA-RESPOSTA SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS PUT
              CONTAINER ( 'SGTSC059-RPST'         )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSC059-RPST )
              FROM      ( SGTSC059-RPST           )
           END-EXEC
           .
       300000-SAI.
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

           MOVE 'SGTSC059-CD-ARQ-OPR-ETRD'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC059-CD-ARQ-OPR-ETRD     TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSC059-CD-TIP-ARQ'        TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC059-CD-TIP-ARQ          TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSC059-CD-USU-RSP-INCL'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC059-CD-USU-RSP-INCL     TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSC059-TS-INCL-ARQ-OPR'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC059-TS-INCL-ARQ-OPR     TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

      * --> grava o erro no msg
           PERFORM MSG1-NTF-MSG
           MOVE MSG1-CD-ERRO-TS             TO IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS             TO IIBKERRO-TX-ERRO

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
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *---------------------------------------*
-INC SGTKP999
      * --------------------------------------*
       999999-ENCERRAR                SECTION.
      * --------------------------------------*
           EXEC CICS
                RETURN
           END-EXEC
           .
       999999-SAI.
           EXIT.