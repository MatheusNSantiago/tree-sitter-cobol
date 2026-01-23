      *-----------------------------------------------------------------
      * Programa..: SGTSC100
      * Analista..: F8650584 - Roberto Fernandes Lima
      * Autor.....: F8650584 - Roberto Fernandes Lima
      * Data......: 20/09/2018
      * Objetivo..: Gerar Relatorio Sobre Projecao da Reserva Bancaria
      *             Consulta em Lista da Tabela DB2SGT.CTRA_SIS_DRIV
      * Compilação: Cobol/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 20/09/2018 - F8650584 - DE 2016/145195 - Implantação
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTSC100.

      *-----------------------------------------------------------------
       ENVIRONMENT                     DIVISION.
      *-----------------------------------------------------------------
       CONFIGURATION                   SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *-----------------------------------------------------------------
       DATA                            DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       WORKING-STORAGE                 SECTION.
      *-----------------------------------------------------------------
       77  CTE-INICIO-SS               PIC  X(40) VALUE
                                 '*** Storage Section começa aqui ***'.
       77  CTE-PROG                    PIC  X(17) VALUE
                                       '*** SGTSC100  ***'.
       77  CTE-VERS                    PIC  X(06) VALUE 'VRS001'.



      *--> Definição dos Nomes dos Programas Acionados
       01  TAB-PROGRAMAS-CHAMADOS.
           03  MSTS8002                PIC  X(08) VALUE 'MSTS8002'.

      *--> Definição dos Variaveis do Cobolito
-INC ALMKCBL1

      *--> Book do programa de ambiente - CICS5000
      *    Definido na work para preservar as informacoes
-INC HLPKE01B

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

      *--> Area de trabalho
       01  QT-REG-SELD                 PIC S9(04) COMP-5 VALUE ZEROS.
       01  IC-TAB                      PIC  9(04) VALUE ZEROS.

       01  GDA-CD-UOR                  PIC  9(09) VALUE ZEROS.
       01  U                           PIC  9(04) VALUE ZEROS.
       01  IDX                         PIC  9(04) VALUE ZEROS.
       01  IND                         PIC  9(04) VALUE ZEROS.
       01  WIDX                        PIC  9(09) VALUE ZEROS.
       01  WIND                        PIC  9(09) VALUE ZEROS.
       01  IND-FIM-CPR1                PIC  X(01) VALUE 'N'.
       01  IND-FIM-CPR2                PIC  X(01) VALUE 'N'.
       01  GDA-CTRA-I                  PIC S9(09) USAGE COMP.
       01  GDA-CTRA-F                  PIC S9(09) USAGE COMP.

      *--> Registro da tabela DB2SGT.CTRA_SIS_DRIV
       01  CTRA-SIS-DRIV.
           03 CD-CTRA-SIS-DRIV         PIC S9(09) COMP-5 VALUE ZEROS.
           03 NM-PGM-RSP-ATL           PIC  X(08) VALUE SPACES.
           03 DT-INC-ENVO-ATI          PIC  X(10) VALUE SPACES.
           03 DT-FIM-ENVO-ATI          PIC  X(10) VALUE SPACES.
           03 CD-UOR-GST               PIC S9(09) COMP-5 VALUE ZEROS.
           03 CD-UOR-OPR               PIC S9(09) COMP-5 VALUE ZEROS.
           03 CD-EST-CTRA-SIS          PIC S9(04) COMP-5 VALUE ZEROS.
           03 CD-USU-MNTC-CTRA         PIC  X(08) VALUE SPACES.
           03 CD-USU-CNFC-CTRA         PIC  X(08) VALUE SPACES.
           03 NM-CTRA-SIS-DRIV         PIC  X(50) VALUE SPACES.

      *--> Indicadores de nulidades
       01  QT-REG-SELD-NULO            PIC S9(04) COMP-5 VALUE ZEROS.
       01  DT-INC-ENVO-ATI-N           PIC S9(04) COMP-5 VALUE ZEROS.
       01  DT-FIM-ENVO-ATI-N           PIC S9(04) COMP-5 VALUE ZEROS.
       01  CD-UOR-GST-N                PIC S9(04) COMP-5 VALUE ZEROS.
       01  CD-UOR-OPR-N                PIC S9(04) COMP-5 VALUE ZEROS.
       01  CD-USU-MNTC-CTRA-N          PIC S9(04) COMP-5 VALUE ZEROS.
       01  CD-USU-CNFC-CTRA-N          PIC S9(04) COMP-5 VALUE ZEROS.

      *-----------------------------------------------------------------
       01  SERVICO-IIB-GENERICO.
           03  KIIBG-SRVC-ID-SRVC      PIC  9(09).
           03  KIIBG-SRVC-VRS-SRVC     PIC  9(04).
           03  KIIBG-SRVC-ID-OPR       PIC  9(08).
           03  KIIBG-SRVC-VRS-OPR      PIC  9(04).
           03  KIIBG-NM-PGM-CHAMAR     PIC  X(08).

       01  WS-DADOS-RQSC               PIC  X(65000) VALUE SPACES.
       01  WS-DADOS-RPST               PIC  X(65000) VALUE SPACES.
       01  WS-TAM-DADOS-RQSC           PIC S9(09) COMP VALUE ZEROS.
       01  WS-TAM-DADOS-RPST           PIC S9(09) COMP VALUE ZEROS.

       01  WS-NM-CANAL                 PIC  X(16) VALUE 'CANAL-'.
       01  FILLER REDEFINES WS-NM-CANAL.
           03  WS-CANAL                PIC  X(06).
           03  WS-NM-PGM               PIC  X(08).
           03  FILLER                  PIC  X(02).

      *---> Variaveis CICS de erro
       01  WS-RESP-CICS.
           05 WS-RESP                  PIC  9(08) VALUE ZEROS.
           05 WS-RESP2                 PIC  9(08) VALUE ZEROS.


-INC IIBKSRVC
-INC IIBKERRO
-INC IIBKOPRD

       01 MSTK8002-PRM.
-INC MSTK8002

      *--> Book Usado na Subrotina de Ocorrência de Erro
-INC HLPKE001

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

      *-----------------------------------------------------------------
       LINKAGE                         SECTION.
      *-----------------------------------------------------------------
       01  DFHCOMMAREA.
-INC SGTKC100

      *-----------------------------------------------------------------
       PROCEDURE                       DIVISION USING DFHCOMMAREA.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------

           IF  EIBCALEN  NOT EQUAL LENGTH OF  DFHCOMMAREA
               SET   ERRO-CODIFICADO   TO  TRUE
               MOVE  0001              TO  PSC-ERRO-PGM
               MOVE  0220              TO  CD-RTN-PGM
               MOVE '?rea com tamanho inválido - verifique book/local.'
                                       TO  MSG-ERRO
               PERFORM  999001-ERRO-001
           END-IF

           MOVE  0001                  TO  PSC-ERRO-PGM

           IF  NOT EXECUCAO-ONLINE AND NOT EXECUCAO-BATCH
               CALL CICS5000 USING CICS5000-COMMAREA
           END-IF

      *--> Habilita Testes no Cobolito e Debug de Operacoes IIB
-INC ALMKCBL2

      *--> Habilita tratamento automatico de Erros DB2
-INC HLPKE002

      *--> Habilita captura automatica de abend e erros CICS
-INC HLPKE003

           PERFORM  100000-INICIAR

           EVALUATE  TRUE
               WHEN  SGTSC100-CONSULTAR
                     PERFORM 200000-PROCESSA-NEGOCIO
           END-EVALUATE

           GOBACK
           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-INICIAR                  SECTION.
      *-----------------------------------------------------------------
           MOVE  0002                  TO  PSC-ERRO-PGM

           INITIALIZE                      SGTSC100-RTN-PGM

           PERFORM  110000-VALIDAR-PARAMETRO

           PERFORM  120000-VALIDAR-CAMPOS

           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-VALIDAR-PARAMETRO        SECTION.
      *-----------------------------------------------------------------
           MOVE  0003                  TO  PSC-ERRO-PGM

           IF  NOT SGTSC100-CONSULTAR
               SET   ERRO-CODIFICADO   TO  TRUE
               MOVE  0116              TO  CD-RTN-PGM
               MOVE  'Ação inválida.'  TO  MSG-ERRO
               PERFORM  999001-ERRO-001
           END-IF

           .
       110000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       120000-VALIDAR-CAMPOS           SECTION.
      *-----------------------------------------------------------------
           MOVE  0004                  TO  PSC-ERRO-PGM

           COMPUTE  SGTSC100-CD-CTRA-SIS-DRIV-ENTD =
                    SGTSC100-CD-CTRA-SIS-DRIV-ENTD * 1
                    ON SIZE ERROR
                    MOVE  ZEROS
                                       TO SGTSC100-CD-CTRA-SIS-DRIV-ENTD
           END-COMPUTE

           COMPUTE  SGTSC100-CD-EST-CTRA-ENTD    =
                    SGTSC100-CD-EST-CTRA-ENTD    * 1
                    ON SIZE ERROR
                    MOVE  ZEROS        TO  SGTSC100-CD-EST-CTRA-ENTD
           END-COMPUTE

           COMPUTE  SGTSC100-NR-PSQ-INC-ENTD     =
                    SGTSC100-NR-PSQ-INC-ENTD     * 1
                    ON SIZE ERROR
                    MOVE  ZEROS        TO  SGTSC100-NR-PSQ-INC-ENTD
           END-COMPUTE

           MOVE  ZEROS                 TO  IC-TAB

           .
       110000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-PROCESSA-NEGOCIO         SECTION.
      *-----------------------------------------------------------------
           MOVE  0005                  TO  PSC-ERRO-PGM

           PERFORM  210000-MONTAR-PESQUISAS

           PERFORM  220000-GERAR-RELATORIO

           MOVE  IC-TAB                TO  SGTSC100-QT-REG-DET
           MOVE  IND                   TO  SGTSC100-NR-PSQ-FIM
           MOVE  IND-FIM-CPR1          TO  SGTSC100-IN-CTND
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       210000-MONTAR-PESQUISAS         SECTION.
      *-----------------------------------------------------------------
           MOVE  0006                  TO  PSC-ERRO-PGM

           MOVE  ZEROS                 TO  WIDX
           MOVE  0001                  TO  WIND

      *--> Mover '_' até a Penultima Posição da Guarda para Recuperar
      *    Todos os Registros LIKE T133-NM-BON-APRC

           MOVE  '%'                   TO  NM-CTRA-SIS-DRIV (1:1)

           PERFORM  VARYING WIDX
                    FROM 0001 BY 0001
                    UNTIL WIDX EQUAL 0050
                    ADD   0001         TO  WIND
                    IF  SGTSC100-NM-CTRA-SIS-DRIV-ENTD (WIDX:1)
                                           NOT EQUAL SPACES
                        MOVE  SGTSC100-NM-CTRA-SIS-DRIV-ENTD(WIDX:1)
                                       TO  NM-CTRA-SIS-DRIV (WIND:1)
                    ELSE
                        MOVE  '%'      TO  NM-CTRA-SIS-DRIV (WIND:1)
                    END-IF
           END-PERFORM

           MOVE  '%'                   TO  NM-CTRA-SIS-DRIV (WIDX:1)

           .
       210000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       220000-GERAR-RELATORIO          SECTION.
      *-----------------------------------------------------------------
           MOVE  0007                  TO  PSC-ERRO-PGM

           MOVE  ZEROS                 TO  SQLCODE
           MOVE  SGTSC100-CD-CTRA-SIS-DRIV-ENTD
                                       TO  CD-CTRA-SIS-DRIV
           MOVE  SGTSC100-CD-EST-CTRA-ENTD
                                       TO  CD-EST-CTRA-SIS

           IF  CD-CTRA-SIS-DRIV            EQUAL  ZEROS
               MOVE  ZEROS             TO  GDA-CTRA-I
               MOVE  999999999         TO  GDA-CTRA-F
           ELSE
               MOVE  CD-CTRA-SIS-DRIV
                                       TO  GDA-CTRA-I
               MOVE  999999999         TO  GDA-CTRA-F
      *        MOVE  CD-CTRA-SIS-DRIV
      *                                TO  GDA-CTRA-F
           END-IF

           EXEC SQL
              DECLARE CURSOR-PR1 CURSOR FOR
               SELECT A.CD_CTRA_SIS_DRIV
                    , A.NM_CTRA_SIS_DRIV
                    , A.DT_INC_ENVO_ATI
                    , A.DT_FIM_ENVO_ATI
                    , IFNULL((A.CD_UOR_GST), 0000)
                    , IFNULL((A.CD_UOR_OPR), 0000)
                    , A.CD_EST_CTRA_SIS
                    , IFNULL((A.CD_USU_MNTC_CTRA), ' ')
                    , IFNULL((A.CD_USU_CNFC_CTRA), ' ')
                    , A.NM_PGM_RSP_ATL
                 FROM DB2SGT.CTRA_SIS_DRIV A
                WHERE A.CD_EST_CTRA_SIS IN ( 1 , 2 , 3 , 4 )
                  AND A.CD_CTRA_SIS_DRIV  BETWEEN :GDA-CTRA-I
                  AND                             :GDA-CTRA-F
                  AND A.NM_CTRA_SIS_DRIV  LIKE :NM-CTRA-SIS-DRIV
             ORDER BY A.CD_CTRA_SIS_DRIV
           END-EXEC

           EXEC SQL
               OPEN CURSOR-PR1
           END-EXEC

           IF  SQLCODE                     NOT EQUAL  ZEROS
               SET   ERRO-CODIFICADO   TO  TRUE
               MOVE  0003              TO  CD-RTN-PGM
               MOVE  'Erro ao iniciar leitura das carteiras.'
                                       TO  MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           MOVE  'S'                   TO  IND-FIM-CPR1

           PERFORM  221000-FETCH-CURSOR-PR1
                    UNTIL IND-FIM-CPR1     EQUAL  'N'

           EXEC SQL
              CLOSE CURSOR-PR1
           END-EXEC

           IF  SQLCODE                     NOT EQUAL  ZEROS
               SET   ERRO-CODIFICADO   TO  TRUE
               MOVE  0004              TO  CD-RTN-PGM
               MOVE  'Erro ao finalizar leitura das carteiras.'
                                       TO  MSG-ERRO
               PERFORM  999001-ERRO-001
           END-IF

           .
       220000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       221000-FETCH-CURSOR-PR1         SECTION.
      *-----------------------------------------------------------------
           MOVE  0008                  TO  PSC-ERRO-PGM

           INITIALIZE                      CTRA-SIS-DRIV

           EXEC SQL
              FETCH CURSOR-PR1
               INTO :CD-CTRA-SIS-DRIV
                  , :NM-CTRA-SIS-DRIV
                  , :DT-INC-ENVO-ATI
                  , :DT-FIM-ENVO-ATI       :DT-FIM-ENVO-ATI-N
                  , :CD-UOR-GST            :CD-UOR-GST-N
                  , :CD-UOR-OPR            :CD-UOR-GST-N
                  , :CD-EST-CTRA-SIS
                  , :CD-USU-MNTC-CTRA      :CD-USU-MNTC-CTRA-N
                  , :CD-USU-CNFC-CTRA      :CD-USU-CNFC-CTRA-N
                  , :NM-PGM-RSP-ATL
           END-EXEC

           EVALUATE  SQLCODE
               WHEN  ZEROS
                     ADD   0001        TO  IND
                     IF  SGTSC100-NR-PSQ-INC-ENTD
                                           LESS IND
                         IF  IC-TAB        >= 0080
                             COMPUTE  IND = IND - 0001
                             MOVE  'S'
                                       TO  IND-FIM-CPR1
                             MOVE  'S'
                                       TO  SGTSC100-IN-CTND
                         ELSE
                             PERFORM  210000-MONTA-SAIDA-DET
                         END-IF
                     END-IF
               WHEN  0100
                     MOVE  'N'         TO  IND-FIM-CPR1
               WHEN  OTHER
                     SET   ERRO-CODIFICADO
                                       TO  TRUE
                     MOVE  0005        TO  CD-RTN-PGM
                     MOVE  'Erro ao processar carteiras.'
                                       TO  MSG-ERRO
                     PERFORM 999001-ERRO-001
           END-EVALUATE

           .
       221000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       210000-MONTA-SAIDA-DET          SECTION.
      *-----------------------------------------------------------------
           MOVE  0009                  TO  PSC-ERRO-PGM

           ADD   0001                  TO  IC-TAB

           MOVE  CD-CTRA-SIS-DRIV
             TO  SGTSC100-CD-CTRA-SIS-DRIV (IC-TAB)
           MOVE  NM-CTRA-SIS-DRIV
             TO  SGTSC100-NM-CTRA-SIS-DRIV (IC-TAB)
           MOVE  DT-INC-ENVO-ATI
             TO  SGTSC100-DT-INC-ENVO-ATI  (IC-TAB)

           IF  DT-FIM-ENVO-ATI-N           EQUAL  -001
               MOVE  SPACES            TO  DT-FIM-ENVO-ATI
           END-IF
           MOVE  DT-FIM-ENVO-ATI
             TO  SGTSC100-DT-FIM-ENVO-ATI  (IC-TAB)

           MOVE  CD-UOR-GST
             TO  SGTSC100-CD-UOR-GST       (IC-TAB)
           MOVE  CD-UOR-OPR
             TO  SGTSC100-CD-UOR-OPR       (IC-TAB)
           MOVE  CD-EST-CTRA-SIS
             TO  SGTSC100-CD-EST-CTRA-SIS  (IC-TAB)
           MOVE  CD-USU-MNTC-CTRA
             TO  SGTSC100-CD-USU-MNTC-CTRA (IC-TAB)
           MOVE  CD-USU-CNFC-CTRA
             TO  SGTSC100-CD-USU-CNFC-CTRA (IC-TAB)
           MOVE  NM-PGM-RSP-ATL
             TO  SGTSC100-NM-PGM-RSP-ATL   (IC-TAB)

           IF  CD-UOR-GST                  NOT EQUAL  ZEROS
               MOVE  CD-UOR-GST        TO  GDA-CD-UOR
               PERFORM  900000-CONSULTAR-NOME-UOR
               MOVE  MSTS8002-NM-UOR-RDZ-S
                                       TO  SGTSC100-NM-UOR-GST (IC-TAB)
           END-IF

           IF  CD-UOR-OPR                  NOT EQUAL  ZEROS
               MOVE  CD-UOR-OPR        TO  GDA-CD-UOR
               PERFORM  900000-CONSULTAR-NOME-UOR
               MOVE  MSTS8002-NM-UOR-RDZ-S
                                       TO  SGTSC100-NM-UOR-OPR (IC-TAB)
           END-IF

           EVALUATE  CD-EST-CTRA-SIS
               WHEN  0001
                     MOVE  'Ativa'
                       TO  SGTSC100-NM-EST-CTRA-ATI(IC-TAB)
               WHEN  0002
                     MOVE  'Vencida'
                       TO  SGTSC100-NM-EST-CTRA-ATI(IC-TAB)
               WHEN  0003
                     MOVE  'Incl DRIVE'
                       TO  SGTSC100-NM-EST-CTRA-ATI(IC-TAB)
               WHEN  0004
                     MOVE  'Alter Pend'
                       TO  SGTSC100-NM-EST-CTRA-ATI(IC-TAB)
               WHEN  0005
                     MOVE  'Excl Pend'
                       TO  SGTSC100-NM-EST-CTRA-ATI(IC-TAB)
               WHEN  0009
                     MOVE  'Cancelada'
                       TO  SGTSC100-NM-EST-CTRA-ATI(IC-TAB)
           END-EVALUATE

           .
       210000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       900000-CONSULTAR-NOME-UOR       SECTION.
      *-----------------------------------------------------------------
           MOVE  0010                  TO  PSC-ERRO-PGM

           INITIALIZE                      MSTS8002-CONTROLE
                                           MSTS8002-ENTRADA
                                           MSTS8002-IN-ENTRADA
                                           MSTS8002-SAIDA
                                           REPLACING
                                           ALPHANUMERIC BY SPACES
                                           NUMERIC      BY ZEROS

           MOVE  0001                  TO  MSTS8002-CD-FUNCAO
           MOVE  GDA-CD-UOR            TO  MSTS8002-CD-UOR-E
           MOVE  'S'                   TO  MSTS8002-IN-DADOS-BASICOS-UOR
           MOVE  'N'                   TO  MSTS8002-IN-ENDERECO
           MOVE  'N'                   TO  MSTS8002-IN-EMAIL-PPL-UOR
           MOVE  'N'                   TO  MSTS8002-IN-HR-FUNCIONAMENTO
           MOVE  'N'                   TO  MSTS8002-IN-LOCALIZACAO-GEO
           MOVE  'N'                   TO  MSTS8002-IN-UOR-VINCULADORA
           MOVE  'N'                   TO  MSTS8002-IN-DADOS-OUTORGA
           MOVE  'N'                   TO  MSTS8002-IN-DADOS-UOR-BB-ACTB
           MOVE  'N'                   TO  MSTS8002-IN-DADOS-UOR-BB
           MOVE  'N'                   TO MSTS8002-IN-AGRUPAMENTO-UOR-BB

           MOVE  00021880              TO  KIIBG-SRVC-ID-SRVC
           MOVE  0001                  TO  KIIBG-SRVC-VRS-SRVC
           MOVE  00079618              TO  KIIBG-SRVC-ID-OPR
           MOVE  0001                  TO  KIIBG-SRVC-VRS-OPR
           MOVE  'MSTS8002'            TO  KIIBG-NM-PGM-CHAMAR

           MOVE  LENGTH OF MSTK8002-PRM
                                       TO  WS-TAM-DADOS-RQSC
           MOVE  MSTK8002-PRM
                                       TO  WS-DADOS-RQSC

           PERFORM  500000-RQSC-SRVC-IIB-GENERICO

           MOVE  WS-DADOS-RPST         TO  MSTK8002-PRM

           IF  MSTS8002-CD-RTN             NOT EQUAL  ZEROS
               SET   ERRO-DE-PGM       TO  TRUE
               MOVE  'MST'             TO  SG-SIS-MSG
               MOVE  MSTS8002-CD-RTN   TO  CD-RTN-PGM
               MOVE  MSTS8002-TX-RTN   TO  MSG-ERRO
               PERFORM  999001-ERRO-001
           END-IF
           .
       900000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       500000-RQSC-SRVC-IIB-GENERICO   SECTION.
      *-----------------------------------------------------------------
           MOVE  0011                  TO  PSC-ERRO-PGM

           PERFORM  510000-PUT-CTNR-RQSC-SRVC

           PERFORM  520000-PUT-CTNR-DADOS-RQSC

           PERFORM  530000-LINK-IIBP2000

           PERFORM  540000-GET-CTRN-ERRO

           IF  WS-RESP                     EQUAL  DFHRESP(CONTAINERERR)
               PERFORM  550000-GET-CTRN-RPST
           END-IF
           .
       500099-RQSC-SRVC-IIB-GENERICO.
           EXIT.

      *-----------------------------------------------------------------
       510000-PUT-CTNR-RQSC-SRVC       SECTION.
      *--> Grava container de requisição de serviço.
      *    Os dados a serem enviados nesse container devem ter sidos
      *    previamente cadastrados no sistema IIB.
      *-----------------------------------------------------------------
           MOVE  0012                  TO  PSC-ERRO-PGM

           MOVE  KIIBG-SRVC-ID-SRVC    TO  SRVC-ID-SRVC.
           MOVE  KIIBG-SRVC-VRS-SRVC   TO  SRVC-VRS-SRVC.
           MOVE  KIIBG-SRVC-ID-OPR     TO  SRVC-ID-OPR.
           MOVE  KIIBG-SRVC-VRS-OPR    TO  SRVC-VRS-OPR.
           MOVE  KIIBG-NM-PGM-CHAMAR   TO  WS-NM-PGM

           EXEC CICS PUT
                     CONTAINER ( 'PRM-RQSC-SRVC'              )
                     CHANNEL   ( WS-NM-CANAL                  )
                     FROM      ( SRVC-PRM-RQSC-SRVC           )
                     FLENGTH   ( LENGTH OF SRVC-PRM-RQSC-SRVC )
                     RESP      ( WS-RESP                      )
                     RESP2     ( WS-RESP2                     )
           END-EXEC.

           EVALUATE  TRUE
               WHEN  WS-RESP               EQUAL  DFHRESP(NORMAL)
                     CONTINUE
               WHEN  OTHER
                     SET   ERRO-CODIFICADO
                                       TO  TRUE
                     MOVE  0006        TO  CD-RTN-PGM
                     STRING  'SIIBG-Indisponibilidade de recurso ('
                             SRVC-ID-SRVC  '-'
                             SRVC-ID-OPR   '-'
                             WS-RESP       '-'
                             WS-RESP2      ')'
                             DELIMITED BY SIZE
                             INTO          MSG-ERRO
                    PERFORM  999001-ERRO-001
           END-EVALUATE.

       510099-PUT-CTNR-RQSC-SRVC.
           EXIT.

      *-----------------------------------------------------------------
       520000-PUT-CTNR-DADOS-RQSC      SECTION.
      *---> Grava container com parametros do serviço.
      *-----------------------------------------------------------------
           MOVE  0013                  TO  PSC-ERRO-PGM

           EXEC CICS PUT
                     CONTAINER ( 'PRM-DADOS-RQSC'     )
                     CHANNEL   ( WS-NM-CANAL          )
                     FROM      ( WS-DADOS-RQSC        )
                     FLENGTH   ( WS-TAM-DADOS-RQSC    )
                     RESP      ( WS-RESP              )
                     RESP2     ( WS-RESP2             )
           END-EXEC.

           EVALUATE  TRUE
               WHEN  WS-RESP               EQUAL  DFHRESP(NORMAL)
                     CONTINUE
               WHEN  OTHER
                     SET   ERRO-CODIFICADO
                                       TO  TRUE
                     MOVE  0007        TO  CD-RTN-PGM
                     STRING  'SIIBG-Indisponibilidade de recurso ('
                             SRVC-ID-SRVC  '-'
                             SRVC-ID-OPR   '-'
                             WS-RESP       '-'
                             WS-RESP2      ')'
                             DELIMITED BY SIZE
                             INTO          MSG-ERRO
                     PERFORM  999001-ERRO-001
           END-EVALUATE.

       520099-PUT-CTNR-DADOS-RQSC.
           EXIT.

      *-----------------------------------------------------------------
       530000-LINK-IIBP2000            SECTION.
      *--> Aciona o programa IIBP2000 para executar o serviço.
      *-----------------------------------------------------------------
           MOVE  0014                  TO  PSC-ERRO-PGM

           EXEC CICS LINK
                     PROGRAM ( 'IIBP2000'       )
                     CHANNEL ( WS-NM-CANAL      )
                     RESP    ( WS-RESP          )
                     RESP2   ( WS-RESP2         )
           END-EXEC.

           EVALUATE  TRUE
               WHEN  WS-RESP               EQUAL  DFHRESP(NORMAL)
                     CONTINUE
               WHEN  OTHER
                     SET   ERRO-CODIFICADO
                                       TO  TRUE
                     MOVE  0008        TO  CD-RTN-PGM
                     STRING  'SIIBG-Indisponibilidade de recurso ('
                             SRVC-ID-SRVC  '-'
                             SRVC-ID-OPR   '-'
                             WS-RESP       '-'
                             'IIBP2000'    ')'
                             DELIMITED BY SIZE
                             INTO          MSG-ERRO
                    PERFORM  999001-ERRO-001
           END-EVALUATE.

       530099-LINK-IIBP2000.
           EXIT.

      *-----------------------------------------------------------------
       540000-GET-CTRN-ERRO            SECTION.
      *--> A presença do container PRM-ERRO indica que houve erro de
      *    integração, a ausência do container indica sucesso.
      *-----------------------------------------------------------------
           MOVE  0015                  TO  PSC-ERRO-PGM

           EXEC CICS GET
                     CONTAINER ( 'PRM-ERRO'                  )
                     CHANNEL   ( WS-NM-CANAL                 )
                     INTO      ( IIBKERRO-PRM-ERRO           )
                     FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                     RESP      ( WS-RESP                     )
                     RESP2     ( WS-RESP2                    )
           END-EXEC.

           EVALUATE  TRUE
               WHEN  WS-RESP               EQUAL  DFHRESP(CONTAINERERR)
                     CONTINUE
               WHEN  WS-RESP               EQUAL  DFHRESP(NORMAL)
                     CONTINUE
               WHEN  OTHER
                     SET   ERRO-CODIFICADO
                                       TO  TRUE
                     MOVE  0009        TO  CD-RTN-PGM
                     STRING  'SIIBG-Indisponibilidade de recurso ('
                             SRVC-ID-SRVC  '-'
                             SRVC-ID-OPR   '-'
                             WS-NM-PGM     '-'
                             WS-RESP       '-'
                             WS-RESP2      ')'
                             DELIMITED BY SIZE
                             INTO          MSG-ERRO
                    PERFORM  999001-ERRO-001
           END-EVALUATE.

       540099-GET-CTRN-ERRO.
           EXIT.

      *-----------------------------------------------------------------
       550000-GET-CTRN-RPST            SECTION.
      *--> Recupera o container com os dados devolvidos pelo provedor
      *    do serviço.
      *-----------------------------------------------------------------
           MOVE  0016                  TO  PSC-ERRO-PGM

           MOVE  ZEROS                 TO  WS-RESP
                                           WS-RESP2
           EXEC CICS  GET
                      CONTAINER ( 'PRM-DADOS-RPST'  )
                      CHANNEL   ( WS-NM-CANAL       )
                      NODATA
                      FLENGTH   ( WS-TAM-DADOS-RPST )
                      RESP      ( WS-RESP           )
                      RESP2     ( WS-RESP2          )
           END-EXEC.

           EVALUATE  TRUE
               WHEN  WS-RESP               EQUAL  DFHRESP(NORMAL)
                     CONTINUE
               WHEN  OTHER
                     SET   ERRO-CODIFICADO
                                       TO  TRUE
                     MOVE  0010        TO  CD-RTN-PGM
                     STRING  'Erro get-1 container PRM-DADOS-RPST, '
                             SRVC-ID-OPR  '-'
                             'RESP='       WS-RESP
                             ', RESP-2='   WS-RESP2
                             ' - '         CTE-PROG
                             DELIMITED BY SIZE
                             INTO          MSG-ERRO
                     PERFORM  999001-ERRO-001
           END-EVALUATE.

           EXEC CICS  GET
                      CONTAINER ( 'PRM-DADOS-RPST'  )
                      CHANNEL   ( WS-NM-CANAL       )
                      INTO      ( WS-DADOS-RPST     )
                      FLENGTH   ( WS-TAM-DADOS-RPST )
                      RESP      ( WS-RESP           )
                      RESP2     ( WS-RESP2          )
           END-EXEC.

           EVALUATE  TRUE
               WHEN  WS-RESP               EQUAL  DFHRESP(NORMAL)
                     CONTINUE
               WHEN  OTHER
                     SET   ERRO-CODIFICADO
                                       TO  TRUE
                     MOVE  0011        TO  CD-RTN-PGM
                     STRING  'Erro get-2 container PRM-DADOS-RPST, '
                             'RESP=' WS-RESP ', RESP-2=' WS-RESP2
                             ' - ' CTE-PROG
                             DELIMITED BY SIZE
                             INTO          MSG-ERRO
                     PERFORM  999001-ERRO-001
           END-EVALUATE.

       550099-GET-CTRN-RPST.
           EXIT.

      *-----------------------------------------------------------------
      * 999000-ERRO SECTION
      *-----------------------------------------------------------------
      *----------------------------
      * 999001-ERRO-001.
      *----------------------------
-INC HLPKE004
-INC HLPKE012

           MOVE  RTN-PGM               TO  SGTSC100-RTN-PGM

           PERFORM  999999-ENCERRAR

           .
       999000-SAI.
           EXIT.

      *----------------------------
      * 999002-ERRO-DB2
      *-----------------------------
-INC HLPKE005

      *----------------------------
      * 999003-TRATA-ABEND-CICS
      * 999004-TRATA-CONDITION-CICS
      *----------------------------
-INC HLPKE006

      *-----------------------------------------------------------------
       999999-ENCERRAR                 SECTION.
      *-----------------------------------------------------------------

           GOBACK
           .
       999999-SAI.
           EXIT.

      *---------------------- FIM SGTSC100 ----------------------------