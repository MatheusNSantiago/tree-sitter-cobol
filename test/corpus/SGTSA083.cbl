      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSA083
      * ANALISTA..: F2054109 Cintia Ap Soares Iglesias
      * AUTOR.....: F9188847 - Soila Akamatsu
      * OBJETIVO..: ALTERACAO DA TABELA EXCO_PRFL_SELD
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 20.04.2018 - F8785286 - 2016/145195 - Deleção dos campos
      *             SGTSA083-CD-RGR-VLDC e GTSA083-NR-VRS-RGR-VLDC por
      *             alteração na tabela EXCO_PRFL_SELD.
      *        17.08.2017 - F9188847 - 2016/145195 - IMPLANTACAO
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSA083.

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
       01  CTE-PROG     PIC  X(17) VALUE '*** SGTSA083 ***'.
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

      * Registro da tabela
       01  TX-OBS-EXCO-PRFL-GRP.
           49  TX-OBS-EXCO-PRFL-LEN    PIC S9(04) COMP-5 VALUE 0.
           49  TX-OBS-EXCO-PRFL-TX     PIC X(400) VALUE ' '.

      * Area de trabalho

       01  QT-REG-SELD                      PIC S9(4) COMP-5 VALUE 0.
-INC HLPKTIME
       01  TS-SQL-ATU                       PIC  X(26) VALUE ' '.

      * Indicadores de nulidades

       01  QT-REG-SELD-NULO                 PIC S9(4) COMP-5 VALUE 0.

      * Variaveis para gravacao do log
       01  DADOS-LOG-DISPLAY.
           03  CD-CTRA-TIT-VL-LOG            PIC  9(10) VALUE 0.
           03  CD-OPR-ETRD-MCP-LOG           PIC  9(11) VALUE 0.
           03  NR-VRS-OPR-ETRD-LOG           PIC  9(5) VALUE 0.
           03  CD-RGR-VLDC-LOG               PIC  9(10) VALUE 0.
           03  NR-VRS-RGR-VLDC-LOG           PIC  9(10) VALUE 0.
           03  NR-SEQL-EXCO-PRFL-LOG         PIC  9(10) VALUE 0.

      * Book dos dados do log
       01  DADOS-LOG.
-INC SGTK014G

       01  SGTS014G VALUE 'SGTS014G'         PIC X(8).

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

-INC SGTKA083

      * Book dos dados do usuário
-INC IIBKOPRD

      * Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
       000000-ROTINA-PRINCIPAL         SECTION.
      *----------------------------------------------------------------*
           IF NOT EXECUCAO-ONLINE AND NOT EXECUCAO-BATCH
             CALL CICS5000 USING CICS5000-COMMAREA
           END-IF

           MOVE 0001                   TO MSG1-CD-SEQL-ERRO
      * Habilita tratamento automatico de Erros DB2
-INC HLPKE997
      * Habilita captura automatica de Abend e Erros CICS
-INC HLPKE999

           PERFORM 100000-DESCOBRIR-CANAL
           PERFORM 120000-EXCLUIR-CTNR-ERRO
           PERFORM 130000-RECEBER-REQUISICAO
           PERFORM 140000-VALIDA-PARAMETRO

           PERFORM 200000-ALTERAR

           PERFORM 300000-POSTAR-RESPOSTA

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
           MOVE 0003                   TO MSG1-CD-SEQL-ERRO

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
           MOVE 0004                   TO MSG1-CD-SEQL-ERRO

           MOVE ZEROS                      TO QT-CCT-LIDO
           MOVE LENGTH OF SGTSA083-RQSC    TO QT-CCT-EPRO
           EXEC CICS GET
              CONTAINER ( 'SGTSA083-RQSC' )
              CHANNEL   ( CANAL      )
              NODATA
              FLENGTH ( QT-CCT-LIDO )
           END-EXEC

           IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
               SET   ERRO-TAMANHO-AREA     TO  TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS GET
              CONTAINER ( 'SGTSA083-RQSC' )
              CHANNEL   ( CANAL      )
              FLENGTH ( LENGTH OF SGTSA083-RQSC )
              INTO    ( SGTSA083-RQSC )
           END-EXEC
           .
       130000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       140000-VALIDA-PARAMETRO SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

           PERFORM 150000-VALIDA-CAMPOS

           PERFORM 152000-VALIDA-OPR-ETRD-MCP

      *    PERFORM 153000-VALIDA-RGR-VLDC-CRCT-OP
           .
       140000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       150000-VALIDA-CAMPOS SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

           PERFORM 151000-BUSCA-TS-DT
           MOVE HLPKE001-TS-FMTD           TO TS-SQL-ATU

           IF  SGTSA083-CD-CTRA-TIT-VL     EQUAL ZEROS
               SET  ERRO-PROGRAMADO        TO TRUE
               MOVE 0001                   TO  MSG1-CD-ERRO-TS
               MOVE 'Codigo carteira titulo valor inválido.'
                                           TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSA083-CD-OPR-ETRD-MCP    NOT NUMERIC
               SET  ERRO-PROGRAMADO        TO TRUE
               MOVE 0005                   TO  MSG1-CD-ERRO-TS
               MOVE 'Codigo operacao estruturado mercado de capitais inv
      -        'álido.'                    TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSA083-CD-OPR-ETRD-MCP    EQUAL ZEROS
               SET  ERRO-PROGRAMADO        TO TRUE
               MOVE 0004                   TO  MSG1-CD-ERRO-TS
               MOVE 'Codigo operacao estruturado mercado de capitais inv
      -        'álido.'                    TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSA083-NR-VRS-OPR-ETRD    EQUAL ZEROS
               SET  ERRO-PROGRAMADO        TO TRUE
               MOVE 0007                   TO  MSG1-CD-ERRO-TS
               MOVE 'Numero versao operacao estruturado inválido.'
                                           TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *    IF  SGTSA083-CD-RGR-VLDC        EQUAL ZEROS
      *        SET  ERRO-PROGRAMADO        TO TRUE
      *        MOVE 0076                   TO  MSG1-CD-ERRO-TS
      *        MOVE 'Codigo regra validacao inválido.'
      *                                    TO MSG1-TX-ERRO-TS
      *        PERFORM 999000-GRAVA-ERRO-E-ENCERRA
      *    END-IF
      *
      *    IF  SGTSA083-NR-VRS-RGR-VLDC    EQUAL ZEROS
      *        SET  ERRO-PROGRAMADO        TO TRUE
      *        MOVE 0083                   TO  MSG1-CD-ERRO-TS
      *        MOVE 'Numero versao regra validacao inválido.'
      *                                    TO MSG1-TX-ERRO-TS
      *        PERFORM 999000-GRAVA-ERRO-E-ENCERRA
      *    END-IF

           IF  SGTSA083-NR-SEQL-EXCO-PRFL  EQUAL ZEROS
               SET  ERRO-PROGRAMADO        TO TRUE
               MOVE 0638                   TO  MSG1-CD-ERRO-TS
               MOVE 'Numero sequencial excecao perfil inválido.'
                                           TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSA083-CD-USU-ULT-ATL     EQUAL SPACES
               SET  ERRO-PROGRAMADO        TO TRUE
               MOVE 0226                   TO  MSG1-CD-ERRO-TS
               MOVE 'Codigo usuario ultimo atualizacao invalido'
                                           TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSA083-TS-ULT-ATL         EQUAL SPACES
               SET  ERRO-PROGRAMADO        TO TRUE
               MOVE 0277                   TO  MSG1-CD-ERRO-TS
               MOVE 'Timestamp ultimo atualizacao invalido'
                                           TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSA083-IN-EXCO-ATV        EQUAL SPACES
               SET  ERRO-PROGRAMADO        TO TRUE
               MOVE 0687                   TO  MSG1-CD-ERRO-TS
               MOVE 'Indicador excecao ativa invalido'
                                           TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      * CALCULA O TAMANHO DO CAMPO VARCHAR
           MOVE 0                      TO TX-OBS-EXCO-PRFL-LEN
           INSPECT FUNCTION REVERSE(SGTSA083-TX-OBS-EXCO-PRFL)
           TALLYING TX-OBS-EXCO-PRFL-LEN FOR LEADING SPACES
           COMPUTE TX-OBS-EXCO-PRFL-LEN =
                    LENGTH OF SGTSA083-TX-OBS-EXCO-PRFL -
                    TX-OBS-EXCO-PRFL-LEN
                    ON SIZE ERROR
                       SET  ERRO-PROGRAMADO  TO TRUE
                       MOVE 0143            TO MSG1-CD-ERRO-TS
                       MOVE 'Ocorreu erro ao calcular o tamanho do texto
      -                     ' de observação da exceção do perfil.'
                                             TO MSG1-TX-ERRO-TS
                       PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-COMPUTE

           MOVE SGTSA083-TX-OBS-EXCO-PRFL  TO TX-OBS-EXCO-PRFL-TX

           IF  SGTSA083-TS-ULT-ATL         EQUAL 'CURRENT'
               MOVE TS-SQL-ATU             TO SGTSA083-TS-ULT-ATL
           END-IF
           .
       150000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       151000-BUSCA-TS-DT SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

           CALL SBTIMEDT            USING  HLPKE001-CD-FUC
                                           HLPKE001-AREA-TRB
                                           HLPKE001-FMT-TS
                                           HLPKE001-TAM-TS
                                           HLPKE001-TS-FMTD

           IF  RETURN-CODE      NOT EQUAL ZEROS
               SET  ERRO-PROGRAMADO        TO TRUE
               MOVE 0050                   TO  MSG1-CD-ERRO-TS
               MOVE 'Erro na chamada da SBTIMEDT' TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       151000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       152000-VALIDA-OPR-ETRD-MCP      SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

           MOVE ZEROS                  TO  QT-REG-SELD
           EXEC SQL
                SELECT 1
                  INTO :QT-REG-SELD
                  FROM DB2SGT.OPR_ETRD_MCP
                 WHERE CD_CTRA_TIT_VL   =  :SGTSA083-CD-CTRA-TIT-VL
                   AND CD_OPR_ETRD_MCP  =  :SGTSA083-CD-OPR-ETRD-MCP
                   AND NR_VRS_OPR_ETRD  =  :SGTSA083-NR-VRS-OPR-ETRD
                        FETCH FIRST 1 ROW ONLY
           END-EXEC

           IF  QT-REG-SELD = 0
               MOVE 0061               TO  MSG1-CD-ERRO-TS
               MOVE 'Operação estruturada mercado de capitais inexistent
      -        'e.'                    TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       152000-SAI.
           EXIT.
      *-----------------------------------------------------------------
      *153000-VALIDA-RGR-VLDC-CRCT-OP SECTION.
      *-----------------------------------------------------------------
      *    MOVE 0009                   TO MSG1-CD-SEQL-ERRO
      *
      *    MOVE ZEROS                  TO QT-REG-SELD
      *
      *    EXEC SQL
      *      SELECT 1
      *       INTO :QT-REG-SELD
      *        FROM DB2SGT.RGR_VLDC_CRCT_OPR
      *        WHERE CD_RGR_VLDC        = :SGTSA083-CD-RGR-VLDC
      *          AND NR_VRS_RGR_VLDC    = :SGTSA083-NR-VRS-RGR-VLDC
      *    END-EXEC
      *
      *    IF  QT-REG-SELD = 0
      *        SET  ERRO-PROGRAMADO    TO TRUE
      *        MOVE 0422               TO  MSG1-CD-ERRO-TS
      *        MOVE 'Regra validacao caracteristica operacao inexistente
      *        ''                      TO MSG1-TX-ERRO-TS
      *        PERFORM 999000-GRAVA-ERRO-E-ENCERRA
      *    END-IF
      *    .
      *153000-SAI.
      *    EXIT.
      *-----------------------------------------------------------------
       200000-ALTERAR SECTION.
      *-----------------------------------------------------------------
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO

           EXEC SQL
             UPDATE DB2SGT.EXCO_PRFL_SELD
             SET CD_USU_ULT_ATL          = :SGTSA083-CD-USU-ULT-ATL
               , TS_ULT_ATL              = :SGTSA083-TS-ULT-ATL
               , IN_EXCO_ATV             = :SGTSA083-IN-EXCO-ATV
               , TX_OBS_EXCO_PRFL        = :TX-OBS-EXCO-PRFL-GRP
                WHERE CD_CTRA_TIT_VL     = :SGTSA083-CD-CTRA-TIT-VL
                  AND CD_OPR_ETRD_MCP    = :SGTSA083-CD-OPR-ETRD-MCP
                  AND NR_VRS_OPR_ETRD    = :SGTSA083-NR-VRS-OPR-ETRD
      *           AND CD_RGR_VLDC        = :SGTSA083-CD-RGR-VLDC
      *           AND NR_VRS_RGR_VLDC    = :SGTSA083-NR-VRS-RGR-VLDC
                  AND NR_SEQL_EXCO_PRFL  = :SGTSA083-NR-SEQL-EXCO-PRFL
           END-EXEC

           IF  SQLCODE EQUAL +100

               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0100               TO  MSG1-CD-ERRO-TS
               MOVE 'Alteracao nao efetuada, registro inexistente'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           INITIALIZE DADOS-LOG
           MOVE 'A'                    TO SGTS014G-CD-TIP-ACAO
           PERFORM 980102-GRAVA-LOG
           .
       200000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       300000-POSTAR-RESPOSTA           SECTION.
      *-----------------------------------------------------------------
           MOVE 0011                   TO MSG1-CD-SEQL-ERRO

           MOVE 'Alteração efetuada com sucesso.'
                                       TO  SGTSA083-TX-RTN-PRCT
           EXEC CICS PUT
              CONTAINER ( 'SGTSA083-RPST' )
              CHANNEL   ( CANAL      )
              FLENGTH ( LENGTH OF SGTSA083-RPST )
              FROM    ( SGTSA083-RPST )
           END-EXEC
           .
       300000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       980102-GRAVA-LOG SECTION.
      *-----------------------------------------------------------------
           MOVE 0012                   TO MSG1-CD-SEQL-ERRO

           MOVE 'EXCO_PRFL_SELD    '   TO SGTS014G-NM-ENT

           IF  EXECUCAO-ONLINE
               PERFORM 980103-LER-DADOS-OPRD
               MOVE  KOPRD-CD-OPRD     TO  SGTS014G-CD-CHV-USU
           ELSE
               MOVE 'SYSTEM'           TO  SGTS014G-CD-CHV-USU
           END-IF

           MOVE 'SGTSA083'                 TO SGTS014G-NM-PGM
           MOVE SGTSA083-CD-CTRA-TIT-VL    TO CD-CTRA-TIT-VL-LOG
           MOVE SGTSA083-CD-OPR-ETRD-MCP   TO CD-OPR-ETRD-MCP-LOG
           MOVE SGTSA083-NR-VRS-OPR-ETRD   TO NR-VRS-OPR-ETRD-LOG
      *    MOVE SGTSA083-CD-RGR-VLDC       TO CD-RGR-VLDC-LOG
      *    MOVE SGTSA083-NR-VRS-RGR-VLDC   TO NR-VRS-RGR-VLDC-LOG
           MOVE SGTSA083-NR-SEQL-EXCO-PRFL TO NR-SEQL-EXCO-PRFL-LOG

           STRING CD-CTRA-TIT-VL-LOG
           ';'    CD-OPR-ETRD-MCP-LOG
           ';'    NR-VRS-OPR-ETRD-LOG
      *    ';'    CD-RGR-VLDC-LOG
      *    ';'    NR-VRS-RGR-VLDC-LOG
           ';'    NR-SEQL-EXCO-PRFL-LOG
               DELIMITED BY SIZE INTO SGTS014G-CHV-PRMR

           MOVE LENGTH OF DADOS-LOG        TO EIBCALEN
           CALL SGTS014G USING DFHEIBLK DADOS-LOG

           IF  SGTS014G-CD-RTN-PGM   NOT   EQUAL ZEROS
               SET  ERRO-PROGRAMADO        TO TRUE
               MOVE 0112                   TO  MSG1-CD-ERRO-TS
               MOVE SGTS014G-MSG-ERRO-PGM  TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       980102-SAI.
           EXIT.
      *----------------------------------------------------------------*
       980103-LER-DADOS-OPRD           SECTION.
      *----------------------------------------------------------------*
           MOVE 0013                   TO MSG1-CD-SEQL-ERRO

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
       999000-GRAVA-ERRO-E-ENCERRA SECTION.
      *-----------------------------------------------------------------

           MOVE 'SGTSA083-CD-CTRA-TIT-VL'    TO MSGKW999-NM-VRV-MNT
           MOVE SGTSA083-CD-CTRA-TIT-VL      TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSA083-CD-OPR-ETRD-MCP'   TO MSGKW999-NM-VRV-MNT
           IF  SGTSA083-CD-OPR-ETRD-MCP      IS NUMERIC
              MOVE SGTSA083-CD-OPR-ETRD-MCP  TO MSGKW999-VL-VRV-MNT
           ELSE
              MOVE 99999999999               TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSA083-NR-VRS-OPR-ETRD'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSA083-NR-VRS-OPR-ETRD     TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

      *    MOVE 'SGTSA083-CD-RGR-VLDC'       TO MSGKW999-NM-VRV-MNT
      *    MOVE SGTSA083-CD-RGR-VLDC         TO MSGKW999-VL-VRV-MNT
      *    PERFORM 999003-ADICIONA-VRV-MNT
      *
      *    MOVE 'SGTSA083-NR-VRS-RGR-VLDC'   TO MSGKW999-NM-VRV-MNT
      *    MOVE SGTSA083-NR-VRS-RGR-VLDC     TO MSGKW999-VL-VRV-MNT
      *    PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSA083-NR-SEQL-EXCO-PRFL' TO MSGKW999-NM-VRV-MNT
           MOVE SGTSA083-NR-SEQL-EXCO-PRFL   TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSA083-CD-USU-ULT-ATL'    TO MSGKW999-NM-VRV-MNT
           MOVE  SGTSA083-CD-USU-ULT-ATL     TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSA083-TS-ULT-ATL'        TO MSGKW999-NM-VRV-MNT
           MOVE SGTSA083-TS-ULT-ATL          TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSA083-IN-EXCO-ATV'       TO MSGKW999-NM-VRV-MNT
           MOVE SGTSA083-IN-EXCO-ATV         TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSA083-TX-OBS-EXCO-PRFL'  TO MSGKW999-NM-VRV-MNT
           MOVE SGTSA083-TX-OBS-EXCO-PRFL    TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

      * --> grava o erro no msg
           PERFORM MSG1-NTF-MSG
           MOVE MSG1-CD-ERRO-TS              TO IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS              TO IIBKERRO-TX-ERRO
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
       999999-ENCERRAR                SECTION.
      * --------------------------------------*
           EXEC CICS
                RETURN
           END-EXEC.