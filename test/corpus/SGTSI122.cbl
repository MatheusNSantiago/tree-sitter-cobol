      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSI122
      * ANALISTA..: F8785286 Roseli Akemi Kawahata
      * AUTOR.....: F8785286 Roseli Akemi Kawahata
      * OBJETIVO..: Manter Parâmetros da tabela LDR_SIS
      * COMPILAÇÃO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 27/06/2019 F9188847 - Inclusão de tratamento no caso de
      *               existência de registro em determinada DT-INCL-LDR
      *        13/06/2019 F8785286 - DE 2015/137233 - Implantação
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSI122.

      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
      *-----------------------------------------------------------------
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *-----------------------------------------------------------------
       DATA                            DIVISION.
      *-----------------------------------------------------------------
       WORKING-STORAGE                 SECTION.
      *-----------------------------------------------------------------

       77  CTE-INICIO-SS               PIC X(35) VALUE
               '*** STORAGE SECTION COMEÇA AQUI ***'.
       77  CTE-PROG                    PIC X(18) VALUE
               '*** SGTSI122 ***'.
       77  CTE-VERS                    PIC X(06) VALUE 'VRS001'.

      * Book do programa SBDATA para validacao de datas
       77  SBDATAF                     PIC  X(008) VALUE 'SBDATAF'.

      * Book do programa SBTIMEDT para obter timestamp atual
       77  SBTIMEDT                    PIC  X(8)   VALUE 'SBTIMEDT'.

      *    Book do programa de ambiente - CICS5000
      *    Definido na working para preservar as informações
       COPY HLPKE01B.

      *----------------------------------------------------------------*
      *                     CONTAINERS UTILIZADOS                      *
      *----------------------------------------------------------------*
      *--> Container de erro

      *--> Container do usuário logado

      *----------------------------------------------------------------*
      *                            OUTROS
      *----------------------------------------------------------------*

      * Constante informada para se obter timestamp atual do sistema
       77 OBTER-TS-ATUAL               PIC X(08) VALUE 'CURRENT '.

      * Constante informada para se obter data atual do sistema
       77 OBTER-DATA-ATUAL            PIC X(08) VALUE 'CURRENT '.

      *-----------------------------------------------------------------
       LOCAL-STORAGE SECTION.
      *-----------------------------------------------------------------
      *--> Auxiliares para movimentação na monitoração de variáveis.
      *01  CD-OPR-ETRD-MCP-TX.
      *    03  CD-OPR-ETRD-MCP         PIC  9(11).
      *
      * Armazena a data para chamada ao SBDATAF
       01  DT-DATAF                    PIC  X(08).
       01  DT-DATAF-R   REDEFINES DT-DATAF.
           03  DIA                      PIC  9(02).
           03  MES                      PIC  9(02).
           03  ANO                      PIC  9(04).
      *
      * Armazena a data corrente
       01  DT-HJ                       PIC  X(10)  VALUE '00.00.0000'.
       01  DT-HJ-R      REDEFINES      DT-HJ.
           03  DD                      PIC  9(02).
           03  FILLER                  PIC  X(01).
           03  MM                      PIC  9(02).
           03  FILLER                  PIC  X(01).
           03  AA                      PIC  9(04).

      * Indicador da data testada.
       01  AX-IND-DT                   PIC  X(01)  VALUE SPACES.
           88  DATA-INCL                           VALUE 'I'.
           88  DATA-MVT                            VALUE 'M'.

       01  QT-REG                      PIC S9(4) COMP-5 VALUE 0.
      *----------------------------------------------------------------*
      *                 AREA DA SUBROTINA SBDATAF
      *----------------------------------------------------------------*
      *    Campos utilizados para validação de data Gregoriana.
       01  SBDATAF-FUNCAO-01           PIC  X(003) VALUE 'F01'.
       01  SBDATAF-ARG01-01            PIC  9(008) VALUE 000.
       01  SBDATAF-ARG01-01-R          REDEFINES  SBDATAF-ARG01-01.
           03  DD                      PIC  9(002).
           03  MM                      PIC  9(002).
           03  AA                      PIC  9(004).
       01  SBDATAF-ARG02-01            PIC  9(001) VALUE 004.

       01  AREAWORK                    PIC  X(4095) VALUE SPACES.
       01  AREA-RED REDEFINES AREAWORK.
           03  A-TAM    PIC 9(0008) COMP.
           03  FILLER   PIC X(4071).
           03  A-FIM    PIC X(0020).

      * Variáveis para gravação do log
       01  DADOS-LOG-DISPLAY.
           03  DT-INCL-LDR-LOG         PIC  X(10)  VALUE SPACES.
           03  DT-ULT-MVT-ENV-LOG      PIC  X(10)  VALUE SPACES.
           03  TS-FCHT-CMCO-ONL-LOG    PIC  X(26)  VALUE SPACES.
           03  TS-RBT-CMCO-ONL-LOG     PIC  X(26)  VALUE SPACES.

      *    Campos utilizados para obtencao de timestamp atual
       01  DADOS-SBTIMEDT.
           03  HLPKE001-CD-FUC         PIC  X(03)  VALUE 'F01'.
           03  HLPKE001-AREA-TRB.
               05  HLPKE001-AREA-TRB-TAM
                                       PIC S9(9)   COMP VALUE 1024.
               05  HLPKE001-AREA-TRB-DADOS
                                       PIC  X(1024) VALUE ' '.
           03  HLPKE001-FMT-TS         PIC S9(4)   COMP VALUE 1.
           03  HLPKE001-TAM-TS         PIC S9(4)   COMP VALUE 30.
           03  HLPKE001-TS-FMTD.
               05  AA                  PIC  9(04).
               05  FILLER              PIC  X(01).
               05  MM                  PIC  9(02).
               05  FILLER              PIC  X(01).
               05  DD                  PIC  9(02).
               05  FILLER              PIC  X(01).
               05  HH                  PIC  X(02).
               05  FILLER              PIC  X(01).
               05  MNTO                PIC  X(02).
               05  FILLER              PIC  X(01).
               05  SGDO                PIC  X(02).
               05  FILLER              PIC  X(01).
               05  MSGD                PIC  X(06).

       01  TS-SQL-ATU                  PIC  X(26)
                             VALUE '0000-00-00-00.00.00.000000'.
       01  TS-SQL-ATU-R      REDEFINES TS-SQL-ATU.
           03  DT-SQL-ATU              PIC  X(10).
           03  DT-SQL-ATU-R  REDEFINES DT-SQL-ATU.
               05  AA                  PIC  9(04).
               05  FILLER              PIC  X(01).
               05  MM                  PIC  9(02).
               05  FILLER              PIC  X(01).
               05  DD                  PIC  9(02).
           03  FILLER                  PIC  X(01).
           03  HH                      PIC  X(02).
           03  FILLER                  PIC  X(01).
           03  MNTO                    PIC  X(02).
           03  FILLER                  PIC  X(01).
           03  SGDO                    PIC  X(02).
           03  FILLER                  PIC  X(01).
           03  MSGD                    PIC  X(06).

      * Indicadores de nulidades
       01  TS-FCHT-CMCO-ONL-N          PIC S9(4) COMP-5 VALUE 0.
       01  TS-RBT-CMCO-ONL-N           PIC S9(4) COMP-5 VALUE 0.

      *    Campos utilizados para leitura de container
       COPY SGTKERR1.

      * Book do container de erro
       COPY IIBKERRO.

      * Book para captura de erro via MSG1
       COPY MSGKWMG1.
      *
       COPY HLPKW999.

      *--> Book do container com dados do operador
       COPY IIBKOPRD.

      * Book dos dados do log
       01  DADOS-LOG.
       COPY SGTK014G.
       01  SGTS014G VALUE 'SGTS014G'         PIC X(8).

       COPY SGTKI122.

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
           MOVE 0001                   TO  MSG1-CD-SEQL-ERRO

           IF NOT EXECUCAO-ONLINE AND NOT  EXECUCAO-BATCH
              CALL CICS5000 USING CICS5000-COMMAREA
           END-IF

      * Habilita tratamento automatico de Erros DB2
       COPY HLPKE997.
      * Habilita captura automatica de Abend e Erros CICS
       COPY HLPKE999.

           PERFORM 100000-DESCOBRIR-CANAL
           PERFORM 120000-EXCLUIR-CTNR-ERRO
           PERFORM 130000-RECEBER-REQUISICAO
           PERFORM 140000-BUSCA-TS-DT
           PERFORM 150000-VALIDA-CAMPOS
           PERFORM 160000-TRATA-NULIDADE
           PERFORM 200000-CONSULTAR
           GOBACK
           .
       000000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       100000-DESCOBRIR-CANAL SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO  MSG1-CD-SEQL-ERRO

           SET   ERRO-CANAL            TO  TRUE

           EXEC CICS ASSIGN
              CHANNEL ( NM-CNL )
           END-EXEC

      * Se chegou até aqui, não há erro
           SET   SEM-ERRO              TO  TRUE
           .
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
           MOVE LENGTH OF SGTSI122-RQSC    TO  QT-CCT-EPRO

           SET  GET-CONTAINER          TO TRUE
           SET  ERRO-CONTAINER         TO TRUE

           EXEC CICS GET
              CONTAINER ( 'SGTSI122-RQSC' )
              CHANNEL   ( NM-CNL          )
              NODATA
              FLENGTH   ( QT-CCT-LIDO     )
           END-EXEC

           SET  SEM-ERRO               TO TRUE

           IF  QT-CCT-LIDO         NOT EQUAL   QT-CCT-EPRO
               SET  ERRO-TAMANHO-AREA    TO  TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           SET  GET-CONTAINER          TO TRUE
           SET  ERRO-CONTAINER         TO TRUE

           EXEC CICS GET
              CONTAINER ( 'SGTSI122-RQSC'         )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSI122-RQSC )
              INTO      ( SGTSI122-RQSC           )
           END-EXEC

           SET  SEM-ERRO               TO TRUE
           .
       130000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       140000-BUSCA-TS-DT SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                   TO  MSG1-CD-SEQL-ERRO

           CALL SBTIMEDT            USING  HLPKE001-CD-FUC
                                           HLPKE001-AREA-TRB
                                           HLPKE001-FMT-TS
                                           HLPKE001-TAM-TS
                                           HLPKE001-TS-FMTD

           IF  RETURN-CODE             NOT EQUAL ZEROS
               SET ERRO-PROGRAMA-ACIONADO  TO  TRUE
               MOVE 050                    TO  MSG1-CD-ERRO-TS
               MOVE 'Erro ao obter data e hora atual.'
                                           TO  MSG1-TX-ERRO-TS
                                               MSG1-TX-RTN-PGM-ACND
               MOVE SBTIMEDT               TO  MSG1-NM-PGM-ACND
               MOVE RETURN-CODE            TO  MSG1-CD-RTN-PGM-ACND
               MOVE MSG1-CD-SEQL-ERRO      TO  MSG1-CD-SEQL-PGM-ACND
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           MOVE      HLPKE001-TS-FMTD  TO TS-SQL-ATU
           .
       140000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       150000-VALIDA-CAMPOS SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO  MSG1-CD-SEQL-ERRO

           EVALUATE  SGTSI122-DT-INCL-LDR
           WHEN 'CURRENT'
               MOVE CORR  DT-SQL-ATU-R TO  DT-HJ-R
               MOVE DT-HJ              TO  SGTSI122-DT-INCL-LDR

           WHEN SPACES
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE 1055               TO  MSG1-CD-ERRO-TS
               MOVE 'Data da Inclusão do registro lider deve ser informa
      -             'da.'              TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA

           WHEN OTHER
               SET DATA-INCL           TO  TRUE
               MOVE CORR SGTSI122-DT-INCL-LDR-R   TO  DT-DATAF-R
               MOVE DT-DATAF           TO  SBDATAF-ARG01-01
               PERFORM  970100-CHAMA-SBDATAF-01
           END-EVALUATE


           EVALUATE  SGTSI122-DT-ULT-MVT-ENV
      *    WHEN 'CURRENT'
      *        MOVE CORR  TS-SQL-ATU-R TO  DT-HJ-R
      *        MOVE DT-HJ              TO  SGTSI122-DT-ULT-MVT-ENV

           WHEN SPACES
               IF SGTSI122-TS-FCHT-CMCO-ONL  EQUAL SPACES AND
                  SGTSI122-TS-RBT-CMCO-ONL   EQUAL SPACES
                  SET  ERRO-PROGRAMADO TO  TRUE
                  MOVE 1056            TO  MSG1-CD-ERRO-TS
                  MOVE 'Deve ser informado ao menos um parâmetro de cont
      -             'role.'            TO  MSG1-TX-ERRO-TS
                  PERFORM 999000-GRAVA-ERRO-E-ENCERRA
               END-IF

           WHEN OTHER
               SET DATA-MVT            TO  TRUE
               MOVE CORR SGTSI122-DT-ULT-MVT-ENV-R   TO  DT-DATAF-R
               MOVE DT-DATAF           TO  SBDATAF-ARG01-01
               PERFORM  970100-CHAMA-SBDATAF-01
           END-EVALUATE


           IF  SGTSI122-TS-FCHT-CMCO-ONL EQUAL 'CURRENT'
               MOVE TS-SQL-ATU-R       TO  SGTSI122-TS-FCHT-CMCO-ONL
           END-IF

           IF  SGTSI122-TS-RBT-CMCO-ONL  EQUAL 'CURRENT'
               MOVE TS-SQL-ATU-R       TO  SGTSI122-TS-RBT-CMCO-ONL
           END-IF

           MOVE '.'  TO  PTO1  PTO2  PTO3  PTO4

           .
       150000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       160000-TRATA-NULIDADE SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO  MSG1-CD-SEQL-ERRO

           IF  SGTSI122-TS-FCHT-CMCO-ONL   EQUAL SPACES
               MOVE -1                 TO  TS-FCHT-CMCO-ONL-N
           END-IF

           IF  SGTSI122-TS-RBT-CMCO-ONL    EQUAL SPACES
               MOVE -1                 TO  TS-RBT-CMCO-ONL-N
           END-IF
           .
       160000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       200000-CONSULTAR          SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

           MOVE ZEROS                  TO QT-REG

           EXEC SQL
                SELECT 1
                   INTO  :QT-REG
                FROM  DB2SGT.LDR_SIS
                WHERE DT_INCL_LDR = :SGTSI122-DT-INCL-LDR
                FETCH FIRST 1 ROW ONLY
           END-EXEC

           IF  QT-REG GREATER ZEROS
               PERFORM 400000-ALTERAR
           ELSE
               PERFORM 300000-INCLUIR
           END-IF
           .
       200000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       300000-INCLUIR SECTION.
      *-----------------------------------------------------------------
           MOVE 0009                   TO  MSG1-CD-SEQL-ERRO

           PERFORM 980103-LER-DADOS-OPRD
      *
      * Desligar erro automático do DB2
      *
           EXEC SQL WHENEVER SQLERROR CONTINUE END-EXEC
      *
           EXEC SQL
                INSERT INTO DB2SGT.LDR_SIS
               ( DT_INCL_LDR,
                 DT_ULT_MVT_ENV,
                 TS_FCHT_CMCO_ONL,
                 TS_RBT_CMCO_ONL )

           VALUES
               ( :SGTSI122-DT-INCL-LDR,
                 :SGTSI122-DT-ULT-MVT-ENV,
                 :SGTSI122-TS-FCHT-CMCO-ONL
                 :TS-FCHT-CMCO-ONL-N,
                 :SGTSI122-TS-RBT-CMCO-ONL
                 :TS-RBT-CMCO-ONL-N )
           END-EXEC

      *
      * Ligar erro automático do DB2
      *
           EXEC SQL WHENEVER SQLERROR
                GO TO 999010-TRATA-ERRO-DB2
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN -803
               SET  ERRO-PROGRAMADO        TO  TRUE
               MOVE 0803                   TO  MSG1-CD-ERRO-TS
               MOVE 'Inclusão não efetuada, registro existente.'
                                           TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA

               WHEN 0
                    CONTINUE

               WHEN OTHER
      * Aciona o tratamento padrão de erro para os demais casos
                    GO TO 999010-TRATA-ERRO-DB2
           END-EVALUATE

           INITIALIZE DADOS-LOG
           MOVE 'I'                    TO  SGTS014G-CD-TIP-ACAO
           PERFORM 980102-GRAVA-LOG
           .
       300000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       400000-ALTERAR SECTION.
      *-----------------------------------------------------------------
           MOVE 0010                   TO  MSG1-CD-SEQL-ERRO

           PERFORM 980103-LER-DADOS-OPRD
      *
           EXEC SQL
             UPDATE DB2SGT.LDR_SIS
                SET DT_ULT_MVT_ENV = :SGTSI122-DT-ULT-MVT-ENV
               , TS_FCHT_CMCO_ONL  = :SGTSI122-TS-FCHT-CMCO-ONL
                                     :TS-FCHT-CMCO-ONL-N
               , TS_RBT_CMCO_ONL   = :SGTSI122-TS-RBT-CMCO-ONL
                                     :TS-RBT-CMCO-ONL-N
             WHERE   DT_INCL_LDR   = :SGTSI122-DT-INCL-LDR
           END-EXEC

           IF  SQLCODE EQUAL +100
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE 100                TO  MSG1-CD-ERRO-TS
               MOVE 'Registro inexistente.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF


           INITIALIZE DADOS-LOG
           MOVE 'A'                    TO  SGTS014G-CD-TIP-ACAO
           PERFORM 980102-GRAVA-LOG
           .
       400000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970100-CHAMA-SBDATAF-01         SECTION.
      *-----------------------------------------------------------------
           MOVE 0011                   TO  MSG1-CD-SEQL-ERRO

           MOVE 4095                   TO A-TAM
           MOVE 'SBDATAF -WRKAREA-FIM' TO A-FIM

           CALL  SBDATAF               USING  SBDATAF-FUNCAO-01
                                              AREAWORK
                                              SBDATAF-ARG01-01
                                              SBDATAF-ARG02-01

           IF  RETURN-CODE             NOT EQUAL ZEROS
               SET  ERRO-PROGRAMA-ACIONADO TO  TRUE
               MOVE 0051                   TO  MSG1-CD-ERRO-TS
               MOVE 'Erro na rotina que valida data.'
                                           TO  MSG1-TX-ERRO-TS
               MOVE SBDATAF                TO  MSG1-NM-PGM-ACND
               MOVE RETURN-CODE            TO  MSG1-CD-RTN-PGM-ACND
               IF  DATA-INCL
                   MOVE 'Data inclusão tabela lider inválida.'
                                           TO  MSG1-TX-RTN-PGM-ACND
               ELSE
                   IF  DATA-MVT
                       MOVE 'Data último movimento enviado inválida.'
                                           TO  MSG1-TX-RTN-PGM-ACND
                   END-IF
               END-IF

               MOVE 'SBDATAF-FUNCAO-01'  TO  MSGKW999-NM-VRV-MNT
               MOVE  SBDATAF-FUNCAO-01   TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'A-TAM'              TO  MSGKW999-NM-VRV-MNT
               MOVE  A-TAM               TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'A-FIM'              TO  MSGKW999-NM-VRV-MNT
               MOVE  A-FIM               TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SBDATAF-ARG01-01'   TO  MSGKW999-NM-VRV-MNT
               MOVE  SBDATAF-ARG01-01    TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SBDATAF-ARG02-01'   TO  MSGKW999-NM-VRV-MNT
               MOVE  SBDATAF-ARG02-01    TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       970100-SAI.
           EXIT.
      *-----------------------------------------------------------------
       980102-GRAVA-LOG SECTION.
      *-----------------------------------------------------------------
           MOVE 0012                   TO  MSG1-CD-SEQL-ERRO

           MOVE 'LDR_SIS'              TO SGTS014G-NM-ENT
           IF  EXECUCAO-ONLINE
               MOVE  KOPRD-CD-OPRD     TO  SGTS014G-CD-CHV-USU
           ELSE
               MOVE 'SYSTEM'           TO  SGTS014G-CD-CHV-USU
           END-IF

           MOVE 'SGTSI122'             TO  SGTS014G-NM-PGM
           MOVE SGTSI122-DT-INCL-LDR   TO  DT-INCL-LDR-LOG
      *    MOVE SGTSI122-DT-ULT-MVT-ENV
      *                                TO  DT-ULT-MVT-ENV-LOG
      *    MOVE SGTSI122-TS-FCHT-CMCO-ONL
      *                                TO  TS-FCHT-CMCO-ONL-LOG
      *    MOVE SGTSI122-TS-RBT-CMCO-ONL
      *                                TO  TS-RBT-CMCO-ONL-LOG

           MOVE DT-INCL-LDR-LOG        TO SGTS014G-CHV-PRMR

           MOVE LENGTH OF DADOS-LOG    TO  EIBCALEN
           CALL SGTS014G USING DFHEIBLK DADOS-LOG

           IF  SGTS014G-CD-RTN-PGM    NOT  EQUAL ZEROS
               SET  ERRO-PROGRAMA-ACIONADO  TO TRUE
               MOVE 112                     TO MSG1-CD-ERRO-TS
               MOVE 'Erro na gravação das informações do Log.'
                                            TO MSG1-TX-ERRO-TS
               MOVE SGTS014G                TO MSG1-NM-PGM-ACND
               MOVE SGTS014G-CD-RTN-PGM     TO MSG1-CD-RTN-PGM-ACND
               MOVE SGTS014G-MSG-ERRO-PGM   TO MSG1-TX-RTN-PGM-ACND
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       980102-SAI.
           EXIT.
      *----------------------------------------------------------------*
       980103-LER-DADOS-OPRD           SECTION.
      *----------------------------------------------------------------*
           MOVE 0013                   TO  MSG1-CD-SEQL-ERRO

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

           IF  MSG1-CD-ERRO-TS EQUAL ZEROS
               MOVE 8888               TO MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS EQUAL SPACES
               MOVE 'Erro não especificado no módulo.'
                                       TO MSG1-TX-ERRO-TS
           END-IF

           MOVE 'SGTSI122-DT-INCL-LDR'        TO MSGKW999-NM-VRV-MNT
           MOVE SGTSI122-DT-INCL-LDR          TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSI122-DT-ULT-MVT-ENV'       TO MSGKW999-NM-VRV-MNT
           MOVE SGTSI122-DT-ULT-MVT-ENV         TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSI122-TS-FCHT-CMCO-ONL'      TO MSGKW999-NM-VRV-MNT
           MOVE SGTSI122-TS-FCHT-CMCO-ONL        TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSI122-TS-RBT-CMCO-ONL'       TO  MSGKW999-NM-VRV-MNT
           MOVE SGTSI122-TS-RBT-CMCO-ONL         TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

      * --> grava o erro no msg
           PERFORM MSG1-NTF-MSG
           MOVE MSG1-CD-ERRO-TS        TO IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS        TO IIBKERRO-TX-ERRO

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
       COPY HLPKP999.
      * --------------------------------------*
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *---------------------------------------*
       COPY SGTKP999.
      * --------------------------------------*
      * 999010-TRATA-ERRO-DB2          SECTION.
       COPY HLPKP997.
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
       COPY MSGKPMG1.

      * --------------------------------------*
       999999-ENCERRAR                SECTION.
      * --------------------------------------*
           EXEC CICS
                RETURN
           END-EXEC
           .
       999999-SAI.
           EXIT.
