      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSC090
      * ANALISTA..: F2804421 - ELIANA CRISTINA RAMOS DA SILVA
      * AUTOR.....: F2804421 - ELIANA CRISTINA RAMOS DA SILVA
      * OBJETIVO..: CONSULTA DA TABELA ATB_VLDC
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 07.12.2018 - F2804421 - de 2016/145195 - Carregar variá-
      *                     vel NM-CNTR
      *        22.09.2017 - F2804421 - IMPLANTACAO
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSC090.

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
       77  CTE-PROG     PIC  X(17) VALUE '*** SGTSC090 ***'.
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

      *--> Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1

      * Area de trabalho

      * Indicadores de nulidades

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

-INC SGTKC090

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
      *-----------------------------------------------------------------
       100000-DESCOBRIR-CANAL SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CANAL            TO  TRUE

           EXEC CICS ASSIGN
              CHANNEL ( NM-CNL )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-EXCLUIR-CTNR-RPST SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                   TO MSG1-CD-SEQL-ERRO

           SET  DEL-CONTAINER               TO TRUE
           MOVE 'SGTSC090-RPST'             TO NM-CTNR

           EXEC CICS DELETE
              CONTAINER ( NM-CTNR         )
              CHANNEL   ( NM-CNL          )
              NOHANDLE
           END-EXEC

           IF  NOT (EIBRESP EQUAL DFHRESP(NORMAL)   OR
               EIBRESP EQUAL DFHRESP(CONTAINERERR) )
               PERFORM 999002-TRATA-CONDITION-CICS
           END-IF

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
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
           MOVE LENGTH OF SGTSC090-RQSC     TO QT-CCT-EPRO
           SET  GET-CONTAINER               TO TRUE
           MOVE 'SGTSC090-RQSC'             TO NM-CTNR

           EXEC CICS GET
              CONTAINER ( NM-CTNR         )
              CHANNEL   ( NM-CNL          )
              NODATA
              FLENGTH   ( QT-CCT-LIDO     )
           END-EXEC

           IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
               SET  ERRO-TAMANHO-AREA         TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS GET
              CONTAINER ( NM-CTNR                 )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSC090-RQSC )
              INTO      ( SGTSC090-RQSC           )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       130000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       150000-VALIDA-CAMPOS SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

           IF  SGTSC090-CD-ATB-VLDC EQUAL ZEROS
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 686                 TO MSG1-CD-ERRO-TS
               MOVE 'Código do atributo de validação não informado.'
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
                 SELECT NM_ATB_VLDC
                      , CD_TIP_ATB_VLDC
                      , IN_ETR_PRTD
                      , NR_MOD_DADO
                      , NR_ENT_MOD
                      , NR_ATB_ENT
                   INTO  :SGTSC090-NM-ATB-VLDC
                       , :SGTSC090-CD-TIP-ATB-VLDC
                       , :SGTSC090-IN-ETR-PRTD
                       , :SGTSC090-NR-MOD-DADO
                       , :SGTSC090-NR-ENT-MOD
                       , :SGTSC090-NR-ATB-ENT
                       FROM DB2SGT.ATB_VLDC
                WHERE CD_ATB_VLDC        = :SGTSC090-CD-ATB-VLDC
           END-EXEC

           IF  SQLCODE = +100
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 100                 TO  MSG1-CD-ERRO-TS
               MOVE 'Registro inexistente'
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

           SET  PUT-CONTAINER               TO TRUE
           MOVE 'SGTSC090-RPST'             TO NM-CTNR

           EXEC CICS PUT
              CONTAINER ( NM-CTNR                 )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSC090-RPST )
              FROM      ( SGTSC090-RPST           )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA SECTION.
      *-----------------------------------------------------------------
           IF  MSG1-CD-ERRO-TS             EQUAL  ZEROS
               MOVE  8888                  TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS             EQUAL  SPACES
               MOVE  'Erro não especificado no módulo.'
                                           TO  MSG1-TX-ERRO-TS
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

           MOVE 'SGTSC090-CD-ATB-VLDC'       TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC090-CD-ATB-VLDC         TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSC090-NM-ATB-VLDC'       TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC090-NM-ATB-VLDC         TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSC090-CD-TIP-ATB-VLDC'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC090-CD-TIP-ATB-VLDC     TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSC090-IN-ETR-PRTD'       TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC090-IN-ETR-PRTD         TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSC090-NR-MOD-DADO'       TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC090-NR-MOD-DADO         TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSC090-NR-ENT-MOD'        TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC090-NR-ENT-MOD          TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSC090-NR-ATB-ENT'        TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC090-NR-ATB-ENT          TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

      * --> grava o erro no msg
           PERFORM MSG1-NTF-MSG
           MOVE MSG1-CD-ERRO-TS              TO IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS              TO IIBKERRO-TX-ERRO

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
      * MSG1-CD-ERRO-TS = 3000
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * MSG1-CD-ERRO-TS = 3001
      * 999006-TRATA-ERRO-CANAL        SECTION.
      * MSG1-CD-ERRO-TS = 3003
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
-INC MSGKPMG1

      *---------------------------------------*
       999999-ENCERRAR                SECTION.
      *---------------------------------------*
           EXEC CICS
                RETURN
           END-EXEC
           .
       999999-SAI.
           EXIT.