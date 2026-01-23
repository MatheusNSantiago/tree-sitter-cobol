      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSC071
      * ANALISTA..: F2054109 - Cintia Ap Soares Iglesias
      * AUTOR.....: F6361711 - Luis A P Valamede
      * OBJETIVO..: CONSULTA CONTAINER DA TABELA OPR_PG_OTR_OPR
      * COMPILAÇÃO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 08.05.2017 F6361711  DE 2016/145195 - Implantação
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSC071.

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
       77  CTE-INICIO-SS               PIC  X(040)  VALUE
                             '*** S.S. COMECA AQUI ***'.
       77  CTE-PROG                    PIC  X(17) VALUE
                             '*** SGTSC071 ***'.
       77  CTE-VERS                    PIC  X(06) VALUE 'VRS001'.

      *----------------------------------------------------------------*
      *                     CONTAINERS UTILIZADOS                      *
      *----------------------------------------------------------------*
      *--> Container de erro
       77 CTNR-PRM-ERRO               PIC X(16) VALUE 'PRM-ERRO'.
      * ---------------------------------------------------------------
      * Container da requisição desta subrotina
       77 CTNR-SGTSC071-RQSC           PIC X(16) VALUE 'SGTSC071-RQSC'.

      * Container da resposta desta subrotina
       77 CTNR-SGTSC071-RPST           PIC X(16) VALUE 'SGTSC071-RPST'.
      * ---------------------------------------------------------------*

      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informacoes
-INC HLPKE01B
      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------
      *--> Auxiliares para movimentação na monitoração de variáveis.
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).

      * Registro da tabela
       01  REG-WS.
           03  DADOS.
               05  TS-ULT-ATL
                   PIC X(26) VALUE '0000-00-00-00.00.00.000000'.
               05  TS-ULT-ATL-R
                   REDEFINES TS-ULT-ATL.
                   07  AA              PIC X(4).
                   07  ANRC            PIC X(1).
                   07  MM              PIC X(2).
                   07  ANRC            PIC X(1).
                   07  DD              PIC X(2).
                   07  ANRC            PIC X(1).
                   07  HH              PIC X(2).
                   07  ANRC            PIC X(1).
                   07  MNTO            PIC X(2).
                   07  ANRC            PIC X(1).
                   07  SGDO            PIC X(2).
                   07  ANRC            PIC X(1).
                   07  MSGD            PIC X(6).

      * Book de para acionamento do MSG1
-INC IIBKERRO

-INC MSGKWMG1

-INC HLPKW999

-INC SGTKERR1
      * Book do programa
       01  SGTSC071-DADOS.
-INC SGTKC071

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

       01  CTE-FINAL-SS                PIC X(40) VALUE
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
-INC HLPKE997
      * Habilita captura automatica de Abend e Erros CICS
-INC HLPKE999

           PERFORM 100000-DESCOBRIR-CANAL
           PERFORM 200000-EXCLUIR-CTNR-RPST
           PERFORM 300000-EXCLUIR-CTNR-ERRO
           PERFORM 400000-RECEBER-REQUISICAO
           PERFORM 500000-VALIDA-CAMPOS
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

           MOVE CTNR-SGTSC071-RPST     TO NM-CTNR

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
              CONTAINER ( NM-CTNR    )
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
           MOVE CTNR-SGTSC071-RQSC     TO NM-CTNR
           SET  GET-CONTAINER          TO TRUE

           EXEC CICS GET
              CONTAINER ( NM-CTNR     )
              CHANNEL   ( NM-CNL      )
              NODATA
              FLENGTH   ( QT-CCT-LIDO )
           END-EXEC

           MOVE LENGTH OF SGTSC071-RQSC
                                       TO QT-CCT-EPRO

      *Devido à captura automática de erro CICS, deve ser previsto o
      *erro de container.
           SET  ERRO-CONTAINER         TO TRUE
           IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
               SET ERRO-TAMANHO-AREA TO TRUE
               PERFORM 999000-ERRO
           END-IF

           EXEC CICS GET
              CONTAINER ( NM-CTNR                 )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSC071-RQSC )
              INTO      ( SGTSC071-RQSC           )
           END-EXEC

      *Ao chegar aqui, há certeza de que não houve erro algum.
           SET  SEM-ERRO               TO TRUE
           .
       400000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       500000-VALIDA-CAMPOS            SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  REG-WS

           IF  SGTSC071-CD-CTRA-OPR-PGDR   EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE 0496               TO  MSG1-CD-ERRO-TS
               MOVE 'Código da carteira da operação pagadora não informa
      -             'do.'   TO  MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTSC071-CD-OPR-PGDR        IS NOT NUMERIC
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE 0498               TO  MSG1-CD-ERRO-TS
               MOVE 'Código da operação estruturada pagadora não numéric
      -             'o.'               TO  MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTSC071-CD-OPR-PGDR        EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE 0499               TO  MSG1-CD-ERRO-TS
               MOVE 'Código da operação estruturada pagadora não informa
      -             'do.'              TO  MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTSC071-NR-VRS-OPR-PGDR    EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE 0501               TO  MSG1-CD-ERRO-TS
               MOVE 'Número da versão da operação pagadora não informado
      -             '.'                TO  MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTSC071-CD-CTRA-OPR-VCLD   EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE 0503               TO  MSG1-CD-ERRO-TS
               MOVE 'Código da carteira da operação vinculada não inform
      -             'ado.'             TO  MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTSC071-CD-OPR-ETRD-VCLD   IS NOT NUMERIC
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE 0505               TO  MSG1-CD-ERRO-TS
               MOVE 'Código da operação estruturada vinculada não numéri
      -             'co.'              TO  MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTSC071-CD-OPR-ETRD-VCLD   EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE 0506               TO  MSG1-CD-ERRO-TS
               MOVE 'Código da operação estruturada vinculada não inform
      -             'ado.'             TO  MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTSC071-NR-VRS-OPR-VCLD    EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE 0508               TO  MSG1-CD-ERRO-TS
               MOVE 'Número da versão da operação vinculada não informad
      -             'o.'               TO  MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF
           .
       500000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       600000-CONSULTAR                     SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

      *    Campos nulos sao inicializados na consulta.
           INITIALIZE SGTSC071-CD-USU-ULT-ATL
                      TS-ULT-ATL
                      SGTSC071-VL-PGTO

           EXEC SQL
                SELECT CD_USU_ULT_ATL
                     , TS_ULT_ATL
                     , VL_PGTO
                  INTO :SGTSC071-CD-USU-ULT-ATL
                     , :TS-ULT-ATL
                     , :SGTSC071-VL-PGTO
                  FROM DB2SGT.OPR_PG_OTR_OPR
                 WHERE CD_CTRA_OPR_PGDR  =  :SGTSC071-CD-CTRA-OPR-PGDR
                   AND CD_OPR_PGDR       =  :SGTSC071-CD-OPR-PGDR
                   AND NR_VRS_OPR_PGDR   =  :SGTSC071-NR-VRS-OPR-PGDR
                   AND CD_CTRA_OPR_VCLD  =  :SGTSC071-CD-CTRA-OPR-VCLD
                   AND CD_OPR_ETRD_VCLD  =  :SGTSC071-CD-OPR-ETRD-VCLD
                   AND NR_VRS_OPR_VCLD   =  :SGTSC071-NR-VRS-OPR-VCLD
           END-EXEC

           IF  SQLCODE = +100
               INITIALIZE SGTSC071-RPST
               MOVE 100                TO MSG1-CD-ERRO-TS
               MOVE 'Operação paga por outra operação inexistente.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF
           MOVE '00000000000000000000' TO SGTSC071-TS-ULT-ATL-R
           MOVE CORR TS-ULT-ATL-R      TO SGTSC071-TS-ULT-ATL-R
           .
       600000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       700000-POSTA-RESPOSTA           SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

           MOVE CTNR-SGTSC071-RPST     TO NM-CTNR
           SET  PUT-CONTAINER          TO TRUE

      *Devido à captura automática de erro CICS, deve ser previsto o
      *erro de container.
           SET  ERRO-CONTAINER         TO TRUE

           EXEC CICS PUT
              CONTAINER ( NM-CTNR                 )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSC071-RPST )
              FROM      ( SGTSC071-RPST           )
           END-EXEC

      *Ao chegar aqui, há certeza de que não houve erro algum.
           SET  SEM-ERRO               TO TRUE
           .
       700000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       999000-ERRO                     SECTION.
      *----------------------------------------------------------------*

           IF  MSG1-CD-ERRO-TS EQUAL ZEROS
               MOVE 8888                TO MSG1-CD-ERRO-TS
           END-IF
           IF  MSG1-TX-ERRO-TS EQUAL SPACES
               MOVE 'Erro não especificado no módulo.'
                                        TO MSG1-TX-ERRO-TS
           END-IF
           PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           .
       999000-ERRO-SAI.
           EXIT.
      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------

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

           MOVE 'SGTSC071-CD-CTRA-OPR-PGDR              '
                                       TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC071-CD-CTRA-OPR-PGDR
                                       TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT


           MOVE 'SGTSC071-CD-OPR-PGDR                   '
                                       TO MSGKW999-NM-VRV-MNT
      *------> Verifica se o conteudo e numerico antes de adicionar
           IF  SGTSC071-CD-OPR-PGDR    IS NUMERIC
               MOVE  SGTSC071-CD-OPR-PGDR
                                       TO  CD-OPR-ETRD-MCP
               MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM 999003-ADICIONA-VRV-MNT


           MOVE 'SGTSC071-NR-VRS-OPR-PGDR               '
                                       TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC071-NR-VRS-OPR-PGDR
                                       TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT


           MOVE 'SGTSC071-CD-CTRA-OPR-VCLD              '
                                       TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC071-CD-CTRA-OPR-VCLD
                                       TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT


           MOVE 'SGTSC071-CD-OPR-ETRD-VCLD              '
                                       TO MSGKW999-NM-VRV-MNT
      *------> Verifica se o conteudo e numerico antes de adicionar
           IF  SGTSC071-CD-OPR-ETRD-VCLD    IS NUMERIC
               MOVE  SGTSC071-CD-OPR-ETRD-VCLD
                                       TO  CD-OPR-ETRD-MCP
               MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM 999003-ADICIONA-VRV-MNT


           MOVE 'SGTSC071-NR-VRS-OPR-VCLD               '
                                       TO MSGKW999-NM-VRV-MNT
           MOVE SGTSC071-NR-VRS-OPR-VCLD
                                       TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

      * --> grava o erro no msg
           PERFORM MSG1-NTF-MSG
           MOVE MSG1-CD-ERRO-TS             TO IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS             TO IIBKERRO-TX-ERRO
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
      *----------------------------------------
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