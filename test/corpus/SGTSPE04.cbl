      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSPE04
      * ANALISTA..: F6361711 - Luis A P Valamede
      * AUTOR.....: F2054109 - Cintia Ap Soares Iglesias
      * OBJETIVO..: Consultar última posição diária do saldo da Operação
      *             na tabela: DB2SGT.PSC_DRIA_SDO_OPR
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS002 19.06.2024 F9458161 Inclui validacao parm entrada
      * VRS001 09.01.2018 F8785286  DE 2016/145195 - Inibir gravação do
      *           container de erro caso não encontre registro.
      *        30.01.2017 F6361711  DE 2016/145195 - Implantação
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSPE04.

      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
      *-----------------------------------------------------------------
       CONFIGURATION                   SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       WORKING-STORAGE                 SECTION.
      *-----------------------------------------------------------------
      ***** CODIGO GERADO INJETADO ACESSO A CONTAINERES * INICIO ******
      *****************************************************************
      ********                                                 ********
      ********   NAO EDITE MANUALMENTE ESSE TRECHO DO CODIGO   ********
      ********   CONTROLE: 3D9B437AEEE92DAA0515AC8F132DE8A3    ********
      *****************************************************************
-INC ALMKCNTA
      ***** CODIGO GERADO INJETADO ACESSO A CONTAINERES *** FIM *******
       77  CTE-INICIO-SS               PIC  X(040)  VALUE
                             '*** S.S. COMECA AQUI ***'.
       77  CTE-PROG                    PIC  X(17) VALUE
                             '*** SGTSPE04 ***'.
       77  CTE-VERS                    PIC  X(06) VALUE 'VRS002'.

      *----------------------------------------------------------------*
      *                     CONTAINERS UTILIZADOS                      *
      *----------------------------------------------------------------*
      *--> Container de erro
       77 CTNR-PRM-ERRO               PIC X(16) VALUE 'PRM-ERRO'.
      * ---------------------------------------------------------------
      * CONTAINER DA REQUISIÇÃO DESTA SUBROTINA
       77 CTNR-SGTSPE04-RQSC           PIC X(16) VALUE 'SGTSPE04-RQSC'.

      * CONTAINER DA RESPOSTA   DESTA SUBROTINA
       77 CTNR-SGTSPE04-RPST           PIC X(16) VALUE 'SGTSPE04-RPST'.
      * ---------------------------------------------------------------*

      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informacoes
-INC HLPKE01B
      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------
      * Book de para acionamento do MSG1
-INC IIBKERRO

-INC MSGKWMG1

-INC MSGKW999

      *--> Book para captura de abend cics da bloco codigo HLPKW999
-INC HLPKW999
      * Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1
      * Book do programa
       01  SGTSPE04-DADOS.
-INC SGTKPE04

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

       77  CTE-FINAL-SS                PIC X(40) VALUE
               '*** STORAGE SECTION TERMINA AQUI ***'.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------
           MOVE 0001                   TO MSG1-CD-SEQL-ERRO

           IF NOT EXECUCAO-ONLINE AND NOT EXECUCAO-BATCH
             CALL CICS5000 USING CICS5000-COMMAREA
           END-IF

      * Habilita tratamento automatico de Erros DB2
-INC MSGKE997
      * Habilita captura automatica de Abend e Erros CICS
-INC HLPKE999

           PERFORM 100000-DESCOBRIR-CANAL
           PERFORM 200000-EXCLUIR-CTNR-RPST
           PERFORM 300000-EXCLUIR-CTNR-ERRO
           PERFORM 400000-RECEBER-REQUISICAO
           PERFORM 500000-VALIDA-PARAMETRO
           PERFORM 600000-CONSULTAR
           PERFORM 700000-POSTA-RESPOSTA
           GOBACK
           .
       000000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       100000-DESCOBRIR-CANAL          SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO

      * Devido à captura automática de erro CICS, deve ser previsto o
      * tipo de erro.
           SET  ERRO-CANAL             TO TRUE

           EXEC CICS ASSIGN
                     CHANNEL ( NM-CNL )
           END-EXEC

      *    Ao chegar aqui, há certeza de que não houve erro algum.
           SET  SEM-ERRO               TO TRUE
           .
       100000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       200000-EXCLUIR-CTNR-RPST        SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                   TO MSG1-CD-SEQL-ERRO

      *    Monta o nome do container de requisição.
           MOVE CTE-PROG(5:8)          TO NM-PGM

           MOVE CTNR-SGTSPE04-RPST     TO NM-CTNR

           SET DEL-CONTAINER           TO TRUE

           EXEC CICS DELETE
              CONTAINER ( NM-CTNR )
              CHANNEL   ( NM-CNL  )
              NOHANDLE
           END-EXEC
           IF  NOT (EIBRESP EQUAL DFHRESP(NORMAL)   OR
               EIBRESP EQUAL DFHRESP(CONTAINERERR) )
               PERFORM 999002-TRATA-CONDITION-CICS
           END-IF
           .
       200000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       300000-EXCLUIR-CTNR-ERRO        SECTION.
      *-----------------------------------------------------------------
           MOVE 0004                   TO MSG1-CD-SEQL-ERRO

           MOVE CTNR-PRM-ERRO          TO NM-CTNR

           SET DEL-CONTAINER           TO TRUE

           EXEC CICS DELETE
              CONTAINER ( NM-CTNR )
              CHANNEL   ( NM-CNL     )
              NOHANDLE
           END-EXEC
           IF  NOT (EIBRESP EQUAL DFHRESP(NORMAL)   OR
               EIBRESP EQUAL DFHRESP(CONTAINERERR) )
               PERFORM 999002-TRATA-CONDITION-CICS
           END-IF
           .
       300000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       400000-RECEBER-REQUISICAO       SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

           MOVE ZEROS                  TO QT-CCT-LIDO
           MOVE CTNR-SGTSPE04-RQSC     TO NM-CTNR
           SET  GET-CONTAINER          TO TRUE

           EXEC CICS GET
              CONTAINER ( NM-CTNR     )
              CHANNEL   ( NM-CNL      )
              NODATA
              FLENGTH   ( QT-CCT-LIDO )
           END-EXEC

           MOVE LENGTH OF SGTSPE04-RQSC
                                       TO QT-CCT-EPRO

      *Devido à captura automática de erro CICS, deve ser previsto o
      *erro de container.
           SET  ERRO-CONTAINER         TO TRUE

           IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
               SET   ERRO-TAMANHO-AREA   TO  TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS GET
              CONTAINER ( NM-CTNR               )
              CHANNEL   ( NM-CNL                )
              FLENGTH ( LENGTH OF SGTSPE04-RQSC )
              INTO    ( SGTSPE04-RQSC           )
           END-EXEC

      *Ao chegar aqui, há certeza de que não houve erro algum.
           SET  SEM-ERRO               TO TRUE
           .
       400000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       500000-VALIDA-PARAMETRO         SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

           IF  SGTSPE04-CD-CTRA-TIT-VL  <= 0
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 0001                TO MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de título e valores mobiliários
      -             'inválido.'
                 TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSPE04-CD-TIT-OPR-ETRD  EQUAL SPACES
               SET  ERRO-PROGRAMADO      TO TRUE
               MOVE 0231                 TO MSG1-CD-ERRO-TS
               MOVE 'Código do título da operação não informado.'
                                         TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSPE04-NR-DVS-TIT       <= 0
               SET  ERRO-PROGRAMADO      TO TRUE
               MOVE 0013                 TO MSG1-CD-ERRO-TS
               MOVE 'Número da divisão do título inválido.'
                                         TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       500000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       600000-CONSULTAR                SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

           EXEC SQL
                  SELECT  CHAR(TO_CHAR(DT_PSC,'DDMMYYYY') , 8)
                       ,  VL_SDO_TIT_DT_PSC
                    INTO  :SGTSPE04-DT-PSC
                       ,  :SGTSPE04-VL-SDO-TIT-DT-PSC
                    FROM  DB2SGT.PSC_DRIA_SDO_OPR
                   WHERE  CD_CTRA_TIT_VL  = :SGTSPE04-CD-CTRA-TIT-VL
                     AND  CD_TIT_OPR_ETRD = :SGTSPE04-CD-TIT-OPR-ETRD
                     AND  NR_DVS_TIT      = :SGTSPE04-NR-DVS-TIT
                ORDER BY  DT_PSC           DESC
                       ,  NR_SEQL_PSC_DRIA DESC
                FETCH FIRST 1 ROW ONLY
           END-EXEC

           .
       600000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       700000-POSTA-RESPOSTA           SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

           MOVE CTNR-SGTSPE04-RPST     TO NM-CTNR
           SET  PUT-CONTAINER          TO TRUE

      *Devido à captura automática de erro CICS, deve ser previsto o
      *erro de container.
           SET  ERRO-CONTAINER         TO TRUE

           EXEC CICS PUT
              CONTAINER ( NM-CTNR               )
              CHANNEL   ( NM-CNL                )
              FLENGTH ( LENGTH OF SGTSPE04-RPST )
              FROM    ( SGTSPE04-RPST           )
           END-EXEC

      *Ao chegar aqui, há certeza de que não houve erro algum.
           SET  SEM-ERRO               TO TRUE
           .
       700000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO

      *Verifica se o tipo de erro gerado possui tratamento interno.
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

           MOVE 'SGTSPE04-CD-CTRA-TIT-VL'    TO MSGKW999-NM-VRV-MNT
           MOVE  SGTSPE04-CD-CTRA-TIT-VL     TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSPE04-CD-TIT-OPR-ETRD'   TO MSGKW999-NM-VRV-MNT
           MOVE  SGTSPE04-CD-TIT-OPR-ETRD    TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSPE04-NR-DVS-TIT'        TO MSGKW999-NM-VRV-MNT
           MOVE  SGTSPE04-NR-DVS-TIT         TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

      * --> grava o erro no msg
           PERFORM MSG1-NTF-MSG
           MOVE MSG1-CD-ERRO-TS             TO IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS             TO IIBKERRO-TX-ERRO
           EXEC CICS PUT
              CONTAINER ( CTNR-PRM-ERRO                )
              CHANNEL   ( NM-CNL                    )
              FROM      ( IIBKERRO-PRM-ERRO         )
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
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *---------------------------------------*
-INC SGTKP999
      * --------------------------------------*
      * 999010-TRATA-ERRO-DB2          SECTION.
-INC MSGKP997
      * --------------------------------------*
      * MSG1-NTF-MSG                   SECTION.
-INC MSGKPMG1
      * --------------------------------------*
       999999-ENCERRAR                 SECTION.
      * --------------------------------------*
           EXEC CICS
                RETURN
           END-EXEC.