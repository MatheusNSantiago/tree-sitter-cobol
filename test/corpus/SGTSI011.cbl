      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSI011
      * ANALISTA..: F4535118 - IVONE DE MELO COSTA RIBEIRO
      * AUTOR.....: F4535118 - IVONE DE MELO COSTA RIBEIRO
      * OBJETIVO..: INCLUSAO DA TABELA RMNC_OPR_ETRD
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 19.06.2017 F9188847 Inclusão dos campos: CD-TIP-FMA-APL,
      *                            CD-IEC-RMNC e CD-PERC-CPTZ-JUR
      *                            Inclusão leitura container operador
      *                            para gravação no LOG.
      *        11/01/2017 F0175702 DE 2015/137233 - Ajuste book recomp.
      *        07.11.2016 F4535118 DE 2015/137233 - IMPLANTACAO
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSI011.

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
                   '*** S.S. COMECA AQUI ***'.
       77  CTE-PROG     PIC  X(17) VALUE '*** SGTSI011 ***'.
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
      *--> Auxiliares para movimentação na monitoração de variáveis.
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).

       01  QT-REG-SELD                      PIC S9(4) COMP-5 VALUE 0.
-INC HLPKTIME
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

       01  DT-FIM-TAXA-RMNC-N               PIC S9(4) COMP-5 VALUE 0.

      * Variaveis para gravacao do log
       01  DADOS-LOG-DISPLAY.
           03  CD-CTRA-TIT-VL-LOG            PIC  9(10) VALUE 0.
           03  CD-OPR-ETRD-MCP-LOG           PIC  9(11) VALUE 0.
           03  NR-VRS-OPR-ETRD-LOG           PIC  9(5) VALUE 0.
           03  DT-INC-TAXA-RMNC-LOG          PIC  X(10) VALUE ' '.

      * Book dos dados do log
       01  DADOS-LOG.
-INC SGTK014G

       01  SGTS014G VALUE 'SGTS014G'         PIC X(8).

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

-INC SGTKI011
001400* Book dos dados do usuário
-INC IIBKOPRD
000600* Book das variáveis auxiliares do tratamento de erro via MSG
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
           PERFORM 120000-EXCLUIR-CTNR-ERRO
           PERFORM 130000-RECEBER-REQUISICAO
           PERFORM 140000-VALIDA-PARAMETRO

           PERFORM 200000-INCLUIR
           PERFORM 300000-POSTA-RESPOSTA

           GOBACK
           .

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
       120000-EXCLUIR-CTNR-ERRO SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                   TO MSG1-CD-SEQL-ERRO

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
           MOVE 0004                   TO MSG1-CD-SEQL-ERRO

           MOVE ZEROS                       TO QT-CCT-LIDO
           MOVE LENGTH OF SGTSI011-RQSC      TO QT-CCT-EPRO
           EXEC CICS GET
              CONTAINER ( 'SGTSI011-RQSC' )
              CHANNEL   ( NM-CNL          )
              NODATA
              FLENGTH   ( QT-CCT-LIDO     )
           END-EXEC

           IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
               SET  ERRO-TAMANHO-AREA         TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS GET
              CONTAINER ( 'SGTSI011-RQSC'         )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSI011-RQSC )
              INTO      ( SGTSI011-RQSC           )
           END-EXEC
           .
       130000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       140000-VALIDA-PARAMETRO SECTION.
      *-----------------------------------------------------------------

           PERFORM 150000-VALIDA-CAMPOS
           PERFORM 160000-TRATA-NULIDADE
           PERFORM 170001-VALIDA-OPR-ETRD-MCP
           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       150000-VALIDA-CAMPOS SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

           PERFORM 151000-BUSCA-TS-DT
           MOVE CORR HLPKE001-TS-FMTD  TO DT-SQL-ATU-R
           MOVE HLPKE001-TS-FMTD       TO TS-SQL-ATU

           IF  SGTSI011-CD-CTRA-TIT-VL EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 002                TO MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de título e valores mobiliários
      -             'não informado.'   TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI011-CD-OPR-ETRD-MCP NOT NUMERIC
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 003                 TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -             ' numérico.'        TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI011-CD-OPR-ETRD-MCP EQUAL ZEROS
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 005                 TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -             ' informado.'       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI011-NR-VRS-OPR-ETRD EQUAL ZEROS
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 007                 TO  MSG1-CD-ERRO-TS
               MOVE 'Número da versão da operação não informado.'
                                        TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI011-DT-INC-TAXA-RMNC EQUAL SPACES
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 546                 TO  MSG1-CD-ERRO-TS
               MOVE 'Data início taxa remuneração não informado.'
                                        TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI011-TS-ULT-ATL EQUAL SPACES
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 106                 TO  MSG1-CD-ERRO-TS
               MOVE 'Timestamp última atualização não informado.'
                                        TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI011-CD-USU-RSP-ATL EQUAL SPACES
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 278                 TO  MSG1-CD-ERRO-TS
               MOVE 'Código usuário responsável atualização não informad
      -             'o.'                TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI011-PC-TAXA-RMNC-OPR EQUAL ZEROS
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 330                 TO MSG1-CD-ERRO-TS
               MOVE 'Percentual da taxa remuneração da operação não info
      -             'rmado.'            TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI011-DT-INC-TAXA-RMNC EQUAL 'CURRENT'
               MOVE DT-SQL-ATU         TO SGTSI011-DT-INC-TAXA-RMNC
           END-IF

           IF  SGTSI011-TS-ULT-ATL EQUAL 'CURRENT'
               MOVE TS-SQL-ATU         TO SGTSI011-TS-ULT-ATL
           END-IF

           IF  SGTSI011-DT-FIM-TAXA-RMNC EQUAL 'CURRENT'
               MOVE DT-SQL-ATU         TO SGTSI011-DT-FIM-TAXA-RMNC
           END-IF

           IF  SGTSI011-CD-TIP-FMA-APL EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 175                TO MSG1-CD-ERRO-TS
               MOVE 'Código tipo forma de aplicação não informado.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI011-CD-IEC-RMNC EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 179                TO MSG1-CD-ERRO-TS
               MOVE 'Código do indicador econômico não informado.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI011-CD-PERC-CPTZ-JUR EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 547                TO MSG1-CD-ERRO-TS
               MOVE 'Código da periodicidade de capitalização de juros n
      -             'ão informado.'    TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       150000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       151000-BUSCA-TS-DT SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

           CALL SBTIMEDT            USING HLPKE001-CD-FUC
                                          HLPKE001-AREA-TRB
                                          HLPKE001-FMT-TS
                                          HLPKE001-TAM-TS
                                          HLPKE001-TS-FMTD
           IF  RETURN-CODE      NOT EQUAL ZEROS
               SET  ERRO-PROGRAMA-ACIONADO     TO TRUE
               MOVE 050                        TO MSG1-CD-ERRO-TS
               MOVE 'Erro ao obter data e hora atual.'
                                               TO MSG1-TX-ERRO-TS
                                                  MSG1-TX-RTN-PGM-ACND
               MOVE SBTIMEDT                   TO MSG1-NM-PGM-ACND
               MOVE RETURN-CODE                TO MSG1-CD-RTN-PGM-ACND
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       151000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       160000-TRATA-NULIDADE SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

           IF  SGTSI011-DT-FIM-TAXA-RMNC EQUAL SPACES
               MOVE -1                 TO DT-FIM-TAXA-RMNC-N
           END-IF
           .
       160000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       170001-VALIDA-OPR-ETRD-MCP     SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

           MOVE ZEROS                  TO QT-REG-SELD
           EXEC SQL
             SELECT 1
              INTO :QT-REG-SELD
               FROM DB2SGT.OPR_ETRD_MCP
               WHERE CD_CTRA_TIT_VL     = :SGTSI011-CD-CTRA-TIT-VL
                 AND CD_OPR_ETRD_MCP    = :SGTSI011-CD-OPR-ETRD-MCP
                 AND NR_VRS_OPR_ETRD    = :SGTSI011-NR-VRS-OPR-ETRD
           END-EXEC

           IF  QT-REG-SELD = 0
               SET  ERRO-PROGRAMADO     TO TRUE
               MOVE 348                 TO  MSG1-CD-ERRO-TS
               MOVE 'Operacao estruturado mercado de capitais inexistent
      -             'e'                 TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       170001-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-INCLUIR SECTION.
      *-----------------------------------------------------------------
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO
      *
      * Desligar erro automatico do DB2
      *
           EXEC SQL WHENEVER SQLERROR CONTINUE END-EXEC
      *
           EXEC SQL
                INSERT INTO DB2SGT.RMNC_OPR_ETRD
                ( CD_CTRA_TIT_VL
                , CD_OPR_ETRD_MCP
                , NR_VRS_OPR_ETRD
                , DT_INC_TAXA_RMNC
                , TS_ULT_ATL
                , CD_USU_RSP_ATL
                , PC_TAXA_RMNC_OPR
                , DT_FIM_TAXA_RMNC
                , CD_TIP_FMA_APL
                , CD_IEC_RMNC
                , CD_PERC_CPTZ_JUR)
                VALUES
                ( :SGTSI011-CD-CTRA-TIT-VL
                , :SGTSI011-CD-OPR-ETRD-MCP
                , :SGTSI011-NR-VRS-OPR-ETRD
                , :SGTSI011-DT-INC-TAXA-RMNC
                , :SGTSI011-TS-ULT-ATL
                , :SGTSI011-CD-USU-RSP-ATL
                , :SGTSI011-PC-TAXA-RMNC-OPR
                , :SGTSI011-DT-FIM-TAXA-RMNC
                , :SGTSI011-CD-TIP-FMA-APL
                , :SGTSI011-CD-IEC-RMNC
                , :SGTSI011-CD-PERC-CPTZ-JUR)
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
                    SET  ERRO-PROGRAMADO     TO TRUE
                    MOVE 803                 TO  MSG1-CD-ERRO-TS
                    MOVE 'Inclusão não efetuada, registro existente.'
                                        TO MSG1-TX-ERRO-TS
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
               WHEN 0
                    CONTINUE
               WHEN OTHER
      * Aciona o tratamento padrao de erro padrao para os demais casos
                    GO TO 999010-TRATA-ERRO-DB2
           END-EVALUATE
           INITIALIZE DADOS-LOG
           MOVE 'I'                    TO SGTS014G-CD-TIP-ACAO
           PERFORM 980102-GRAVA-LOG
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-POSTA-RESPOSTA SECTION.
      *-----------------------------------------------------------------
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO

           MOVE 'Inclusão efetuada com sucesso.'
                                       TO SGTSI011-TX-RTN-PRCT
           EXEC CICS PUT
              CONTAINER ( 'SGTSI011-RPST'         )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSI011-RPST )
              FROM      ( SGTSI011-RPST           )
           END-EXEC
           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       980102-GRAVA-LOG SECTION.
      *-----------------------------------------------------------------
           MOVE 0011                   TO MSG1-CD-SEQL-ERRO

           MOVE 'RMNC_OPR_ETRD     '        TO SGTS014G-NM-ENT
           IF  EXECUCAO-ONLINE
               PERFORM 980103-LER-DADOS-OPRD
               MOVE  KOPRD-CD-OPRD          TO  SGTS014G-CD-CHV-USU
           ELSE
               MOVE 'SYSTEM'                TO SGTS014G-CD-CHV-USU
           END-IF
           MOVE 'SGTSI011'                  TO SGTS014G-NM-PGM
           MOVE SGTSI011-CD-CTRA-TIT-VL        TO CD-CTRA-TIT-VL-LOG
           MOVE SGTSI011-CD-OPR-ETRD-MCP       TO CD-OPR-ETRD-MCP-LOG
           MOVE SGTSI011-NR-VRS-OPR-ETRD       TO NR-VRS-OPR-ETRD-LOG
           MOVE SGTSI011-DT-INC-TAXA-RMNC      TO DT-INC-TAXA-RMNC-LOG
           STRING CD-CTRA-TIT-VL-LOG
           ';'    CD-OPR-ETRD-MCP-LOG
           ';'    NR-VRS-OPR-ETRD-LOG
           ';'    DT-INC-TAXA-RMNC-LOG
               DELIMITED BY SIZE INTO SGTS014G-CHV-PRMR

           MOVE LENGTH OF DADOS-LOG    TO EIBCALEN
           CALL SGTS014G USING DFHEIBLK DADOS-LOG

           IF  SGTS014G-CD-RTN-PGM  NOT EQUAL ZEROS
               SET  ERRO-PROGRAMA-ACIONADO     TO TRUE
               MOVE 112                        TO MSG1-CD-ERRO-TS
               MOVE 'Erro na gravação das informações do Log.'
                                               TO MSG1-TX-ERRO-TS
               MOVE SGTS014G                   TO MSG1-NM-PGM-ACND
               MOVE SGTS014G-CD-RTN-PGM        TO MSG1-CD-RTN-PGM-ACND
               MOVE SGTS014G-MSG-ERRO-PGM      TO MSG1-TX-RTN-PGM-ACND
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       980102-SAI.
           EXIT.

008160*----------------------------------------------------------------*
008170 980103-LER-DADOS-OPRD           SECTION.
008180*----------------------------------------------------------------*
008190     MOVE 0012                   TO  MSG1-CD-SEQL-ERRO
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
           IF  MSG1-CD-ERRO-TS             EQUAL  ZEROS
               MOVE  8888                    TO MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS             EQUAL  SPACES
               MOVE  'Erro não especificado no módulo.'
                                             TO MSG1-TX-ERRO-TS
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

           MOVE 'SGTSI011-CD-CTRA-TIT-VL'    TO MSGKW999-NM-VRV-MNT
           MOVE SGTSI011-CD-CTRA-TIT-VL      TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSI011-CD-OPR-ETRD-MCP'   TO MSGKW999-NM-VRV-MNT
      *------> Verifica se o conteudo e numerico antes de adicionar
           IF  SGTSI011-CD-OPR-ETRD-MCP      IS NUMERIC
               MOVE SGTSI011-CD-OPR-ETRD-MCP TO  CD-OPR-ETRD-MCP
               MOVE CD-OPR-ETRD-MCP-TX       TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico'    TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSI011-NR-VRS-OPR-ETRD'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSI011-NR-VRS-OPR-ETRD     TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSI011-DT-INC-TAXA-RMNC'  TO MSGKW999-NM-VRV-MNT
           MOVE SGTSI011-DT-INC-TAXA-RMNC    TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSI011-TS-ULT-ATL'        TO MSGKW999-NM-VRV-MNT
           MOVE SGTSI011-TS-ULT-ATL          TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSI011-CD-USU-RSP-ATL'    TO MSGKW999-NM-VRV-MNT
           MOVE SGTSI011-CD-USU-RSP-ATL      TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSI011-DT-FIM-TAXA-RMNC'  TO MSGKW999-NM-VRV-MNT
           MOVE SGTSI011-DT-FIM-TAXA-RMNC    TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSI011-CD-TIP-FMA-APL'    TO MSGKW999-NM-VRV-MNT
           MOVE SGTSI011-CD-TIP-FMA-APL      TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSI011-CD-IEC-RMNC'       TO MSGKW999-NM-VRV-MNT
           MOVE SGTSI011-CD-IEC-RMNC         TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSI011-CD-PERC-CPTZ-JUR'  TO MSGKW999-NM-VRV-MNT
           MOVE SGTSI011-CD-PERC-CPTZ-JUR    TO MSGKW999-VL-VRV-MNT
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
      *----------------------------------------
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