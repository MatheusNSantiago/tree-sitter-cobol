      *-----------------------------------------------------------------
      * Programa..: SGTSE087
      * Analista..: F6361711 - Luis A P Valamede
      * Autor.....: F6361711 - Luis A P Valamede
      * Objetivo..: CONTAINER DE EXCLUSAO TABELA RCBT_RLZD_OPR
      * Compilação: COBOL/CICS/DB2
      **----------------------------------------------------------------
      * VRS001 24.05.2017 F6361711  DE 2016/145195 - Implantação
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSE087.

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
       77  CTE-INICIO-SS               PIC X(35) VALUE
               '*** STORAGE SECTION COMECA AQUI ***'.
       77  CTE-PROG                    PIC  X(17) VALUE
               '*** SGTSE087 ***'.
       77  CTE-VERS                    PIC  X(06) VALUE 'VRS001'.

      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informações
-INC HLPKE01B
      *-----------------------------------------------------------------
       LOCAL-STORAGE SECTION.
      *-----------------------------------------------------------------
      * Book de para acionamento do MSG1
-INC IIBKERRO
-INC MSGKWMG1
-INC HLPKW999

      *--> Auxiliares para movimentação na monitoração de variáveis.
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).

      *--> Áreas auxiliares da tabela RCBT_PRVT_OPR_ETRD
       01  DT-RCBT-PRVT                PIC  X(10) VALUE '  .  .    '.
       01  DT-RCBT-PRVT-R              REDEFINES  DT-RCBT-PRVT.
           03 DIA                      PIC  9(02).
           03 FILLER                   PIC  X(01).
           03 MES                      PIC  9(02).
           03 FILLER                   PIC  X(01).
           03 ANO                      PIC  9(04).

       01  TS-REG-PGTO                 PIC  X(26)
                                     VALUE '    -  -  -  .  .  .      '.
       01  TS-REG-PGTO-R               REDEFINES TS-REG-PGTO.
           03  ANO                     PIC  9(04).
           03  FILLER                  PIC  X(01).
           03  MES                     PIC  9(02).
           03  FILLER                  PIC  X(01).
           03  DIA                     PIC  9(02).
           03  FILLER                  PIC  X(01).
           03  HORA                    PIC  9(02).
           03  FILLE                   PIC  X(01).
           03  MINUTO                  PIC  9(02).
           03  FILLER                  PIC  X(01).
           03  SEGUNDO                 PIC  9(02).
           03  FILLER                  PIC  X(01).
           03  MICRO-SEGUNDO           PIC  9(06).

      * Variáveis para gravação do log
       01  DADOS-LOG-DISPLAY.
           03  CD-CTRA-TIT-VL-LOG      PIC  9(09) VALUE 0.
           03  CD-OPR-ETRD-MCP-LOG     PIC  9(11) VALUE 0.
           03  NR-VRS-OPR-ETRD-LOG     PIC  9(04) VALUE 0.
           03  NR-SEQL-RCBT-PRVT-LOG   PIC  9(04) VALUE 0.
           03  CD-TIP-RCBT-PRVT-LOG    PIC  9(04) VALUE 0.
           03  DT-RCBT-PRVT-LOG        PIC  X(10) VALUE '  .  .    '.
           03  DT-RCBT-PRVT-LOG-R      REDEFINES  DT-RCBT-PRVT-LOG.
               05 DIA                  PIC  9(02).
               05 FILLER               PIC  X(01).
               05 MES                  PIC  9(02).
               05 FILLER               PIC  X(01).
               05 ANO                  PIC  9(04).
           03  CD-TIT-OPR-ETRD-LOG     PIC  X(12) VALUE ' '.
           03  NR-DVS-TIT-LOG          PIC  9(04) VALUE 0.
           03  TS-REG-PGTO-LOG         PIC  X(26) VALUE ' '.


      * Book dos dados do log
       01  DADOS-LOG.
-INC SGTK014G

       01  SGTS014G VALUE 'SGTS014G'   PIC X(8).

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

-INC SGTKE087

      * Book dos dados do usuário
-INC IIBKOPRD

      * Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1

       01  CTE-FINAL-SS                PIC  X(40)      VALUE
                    '*** S.S. TERMINA AQUI ***'.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------
           MOVE 0001                   TO MSG1-CD-SEQL-ERRO

           IF  NOT EXECUCAO-ONLINE AND NOT EXECUCAO-BATCH
               CALL CICS5000 USING CICS5000-COMMAREA
           END-IF
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO
      * Habilita tratamento automático de Erros DB2
-INC HLPKE997
      * Habilita captura automática de Abend e Erros CICS
-INC HLPKE999

           PERFORM 100000-DESCOBRIR-CANAL
           PERFORM 120000-EXCLUIR-CTNR-ERRO
           PERFORM 130000-RECEBER-REQUISICAO
           PERFORM 140000-VALIDAR-PARAMETRO

           PERFORM 980102-GRAVA-LOG
           PERFORM 982000-EXCLUIR-BANCO

           PERFORM 300000-POSTAR-RESPOSTA

           GOBACK
           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-DESCOBRIR-CANAL SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                   TO MSG1-CD-SEQL-ERRO

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

           MOVE ZEROS                  TO QT-CCT-LIDO
           MOVE LENGTH OF SGTSE087-RQSC
                                       TO QT-CCT-EPRO
           EXEC CICS GET
              CONTAINER ( 'SGTSE087-RQSC' )
              CHANNEL   ( NM-CNL          )
              NODATA
              FLENGTH   ( QT-CCT-LIDO     )
           END-EXEC

           IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
               SET  ERRO-TAMANHO-AREA        TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS GET
              CONTAINER ( 'SGTSE087-RQSC'         )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSE087-RQSC )
              INTO      ( SGTSE087-RQSC           )
           END-EXEC
           .
       130000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       140000-VALIDAR-PARAMETRO SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

           IF  SGTSE087-CD-CTRA-TIT-VL                   EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0002               TO MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de título e valores mobiliários
      -             'não informado.'   TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSE087-CD-OPR-ETRD-MCP                  IS NOT NUMERIC
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0003               TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturado mercado de capitais não
      -             ' numérico.'       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSE087-CD-OPR-ETRD-MCP                  EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0005               TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturado mercado de capitais não
      -        ' informado.'           TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSE087-NR-VRS-OPR-ETRD                  EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0007               TO MSG1-CD-ERRO-TS
               MOVE 'Número da versão da operação não informado.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSE087-NR-SEQL-RCBT-PRVT                EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0315               TO MSG1-CD-ERRO-TS
               MOVE 'Número sequencial do recebimento previsto não infor
      -             'mado.'            TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *--> Validar Tipo de Recebimento Previsto
      *    01 - Normal de Juros
      *    02 - Normal de Capital
      *    03 - Extraordinario de Juros
      *    04 - Extraordinario de Capital
           IF  SGTSE087-CD-TIP-RCBT-PRVT LESS 0001 OR
               SGTSE087-CD-TIP-RCBT-PRVT GREATER 0004
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0109               TO MSG1-CD-ERRO-TS
               MOVE 'Tipo de recebimento previsto inválido.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSE087-DT-RCBT-PRVT                     EQUAL SPACES
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0119               TO MSG1-CD-ERRO-TS
               MOVE 'Data do recebimento previsto não informado.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSE087-CD-TIT-OPR-ETRD                  EQUAL SPACES
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0231               TO MSG1-CD-ERRO-TS
               MOVE 'Código do título da operação estruturada não inform
      -             'ado.'             TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSE087-NR-DVS-TIT                       EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0232               TO MSG1-CD-ERRO-TS
               MOVE 'Número da divisão do título da operaçãonão informad
      -             'o.'               TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSE087-TS-REG-PGTO                      EQUAL SPACES
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0316               TO MSG1-CD-ERRO-TS
               MOVE 'Timestamp do registro de pagamento não informado.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-POSTAR-RESPOSTA           SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

           MOVE 'Exclusão efetuada com sucesso.'
                                       TO  SGTSE087-TX-RTN-PRCT
           EXEC CICS PUT
              CONTAINER ( 'SGTSE087-RPST'         )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSE087-RPST )
              FROM      ( SGTSE087-RPST           )
           END-EXEC
           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       980102-GRAVA-LOG SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE DADOS-LOG
           MOVE 'E'                    TO  SGTS014G-CD-TIP-ACAO
           MOVE 'RCBT_RLZD_OPR'        TO  SGTS014G-NM-ENT

           IF  EXECUCAO-ONLINE
               PERFORM 980103-LER-DADOS-OPRD
               MOVE  KOPRD-CD-OPRD     TO  SGTS014G-CD-CHV-USU
           ELSE
               MOVE 'SYSTEM'           TO  SGTS014G-CD-CHV-USU
           END-IF

           MOVE 'SGTSE087'                    TO  SGTS014G-NM-PGM
           MOVE SGTSE087-CD-CTRA-TIT-VL       TO  CD-CTRA-TIT-VL-LOG
           MOVE SGTSE087-CD-OPR-ETRD-MCP      TO  CD-OPR-ETRD-MCP-LOG
           MOVE SGTSE087-NR-VRS-OPR-ETRD      TO  NR-VRS-OPR-ETRD-LOG
           MOVE SGTSE087-NR-SEQL-RCBT-PRVT    TO  NR-SEQL-RCBT-PRVT-LOG
           MOVE SGTSE087-CD-TIP-RCBT-PRVT     TO  CD-TIP-RCBT-PRVT-LOG
           MOVE CORR SGTSE087-DT-RCBT-PRVT-R  TO DT-RCBT-PRVT-LOG-R
           MOVE SGTSE087-CD-TIT-OPR-ETRD      TO CD-TIT-OPR-ETRD-LOG
           MOVE SGTSE087-NR-DVS-TIT           TO NR-DVS-TIT-LOG
           MOVE SGTSE087-TS-REG-PGTO          TO TS-REG-PGTO-LOG
           STRING CD-CTRA-TIT-VL-LOG
           ';'    CD-OPR-ETRD-MCP-LOG
           ';'    NR-VRS-OPR-ETRD-LOG
           ';'    NR-SEQL-RCBT-PRVT-LOG
           ';'    CD-TIP-RCBT-PRVT-LOG
           ';'    DT-RCBT-PRVT-LOG
           ';'    CD-TIT-OPR-ETRD-LOG
           ';'    NR-DVS-TIT-LOG
           ';'    TS-REG-PGTO-LOG
               DELIMITED BY SIZE INTO SGTS014G-CHV-PRMR

           MOVE LENGTH OF DADOS-LOG    TO  EIBCALEN
           CALL SGTS014G USING DFHEIBLK DADOS-LOG

           IF  SGTS014G-CD-RTN-PGM   NOT EQUAL  ZEROS
               SET  ERRO-PROGRAMA-ACIONADO    TO  TRUE
               MOVE 0112                      TO MSG1-CD-ERRO-TS
               MOVE 'Erro na gravação das informações do Log.'
                                              TO MSG1-TX-ERRO-TS
               MOVE SGTS014G                  TO MSG1-NM-PGM-ACND
               MOVE SGTS014G-CD-RTN-PGM       TO MSG1-CD-RTN-PGM-ACND
               MOVE SGTS014G-MSG-ERRO-PGM     TO MSG1-TX-RTN-PGM-ACND
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       980102-SAI.
           EXIT.

      *----------------------------------------------------------------*
       980103-LER-DADOS-OPRD           SECTION.
      *----------------------------------------------------------------*
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO

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

      *-----------------------------------------------------------------
       982000-EXCLUIR-BANCO SECTION.
      *-----------------------------------------------------------------
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO

           MOVE '00.00.0000'           TO DT-RCBT-PRVT-R
           MOVE CORR SGTSE087-DT-RCBT-PRVT-R
                                       TO DT-RCBT-PRVT-R

      *--> Ajusta o formato do Timestamp para o padrão do DB2.
           MOVE CORR SGTSE087-TS-REG-PGTO-R
                                       TO  TS-REG-PGTO-R

           EXEC SQL
                DELETE FROM DB2SGT.RCBT_RLZD_OPR
                WHERE CD_CTRA_TIT_VL     = :SGTSE087-CD-CTRA-TIT-VL
                  AND CD_OPR_ETRD_MCP    = :SGTSE087-CD-OPR-ETRD-MCP
                  AND NR_VRS_OPR_ETRD    = :SGTSE087-NR-VRS-OPR-ETRD
                  AND NR_SEQL_RCBT_PRVT  = :SGTSE087-NR-SEQL-RCBT-PRVT
                  AND CD_TIP_RCBT_PRVT   = :SGTSE087-CD-TIP-RCBT-PRVT
                  AND DT_RCBT_PRVT       = :DT-RCBT-PRVT
                  AND CD_TIT_OPR_ETRD    = :SGTSE087-CD-TIT-OPR-ETRD
                  AND NR_DVS_TIT         = :SGTSE087-NR-DVS-TIT
                  AND TS_REG_PGTO        = :TS-REG-PGTO
           END-EXEC

           IF  SQLCODE EQUAL +100
               SET  ERRO-PROGRAMADO           TO TRUE
               MOVE 0100                      TO MSG1-CD-ERRO-TS
               MOVE 'Registro inexistente.'   TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       982000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA SECTION.
      *-----------------------------------------------------------------
           MOVE 0011                   TO MSG1-CD-SEQL-ERRO

           IF  MSG1-CD-ERRO-TS EQUAL ZEROS
               MOVE 8888                      TO MSG1-CD-ERRO-TS
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

           MOVE 'SGTSE087-CD-CTRA-TIT-VL'    TO MSGKW999-NM-VRV-MNT
           MOVE SGTSE087-CD-CTRA-TIT-VL      TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSE087-CD-OPR-ETRD-MCP'   TO MSGKW999-NM-VRV-MNT
      *-----------> Verifica se o conteudo e numerico antes de adicionar
           IF  SGTSE087-CD-OPR-ETRD-MCP      IS NUMERIC
               MOVE SGTSE087-CD-OPR-ETRD-MCP TO  CD-OPR-ETRD-MCP
               MOVE CD-OPR-ETRD-MCP-TX       TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico.'   TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSE087-NR-VRS-OPR-ETRD'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSE087-NR-VRS-OPR-ETRD     TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSE087-NR-SEQL-RCBT-PRVT' TO MSGKW999-NM-VRV-MNT
           MOVE SGTSE087-NR-SEQL-RCBT-PRVT   TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSE087-CD-TIP-RCBT-PRVT'  TO MSGKW999-NM-VRV-MNT
           MOVE SGTSE087-CD-TIP-RCBT-PRVT    TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSE087-DT-RCBT-PRVT'      TO MSGKW999-NM-VRV-MNT
           MOVE SGTSE087-DT-RCBT-PRVT        TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSE087-CD-TIT-OPR-ETRD'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSE087-CD-TIT-OPR-ETRD     TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSE087-NR-DVS-TIT'        TO MSGKW999-NM-VRV-MNT
           MOVE SGTSE087-NR-DVS-TIT          TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSE087-TS-REG-PGTO'       TO MSGKW999-NM-VRV-MNT
           MOVE SGTSE087-TS-REG-PGTO         TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

      * --> grava o erro no msg
           PERFORM MSG1-NTF-MSG

           MOVE MSG1-CD-ERRO-TS        TO IIBKERRO-CD-ERRO
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
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      * --------------------------------------*
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
      * --------------------------------------*
-INC MSGKPMG1
      * --------------------------------------*
       999999-ENCERRAR                SECTION.
      * --------------------------------------*
           EXEC CICS
                RETURN
           END-EXEC
           .
       999999-SAI.
           EXIT.
      *------------------------- Fim SGTSE087 -------------------------*