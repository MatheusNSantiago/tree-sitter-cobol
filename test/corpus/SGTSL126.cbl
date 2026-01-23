      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSL126
      * ANALISTA..: F9158937 - SILVIA AGUIAR DE OLIVEIRA
      * AUTOR.....: F9158937 - SILVIA AGUIAR DE OLIVEIRA
      * OBJETIVO..: CONSULTA DE LISTA DA TABELA DB2SGT.ACSS_CTRA_TVM
      * COMPILACAO: 54 - PSOSE600 - Cobol 6.3 c/otimizacao p/producao
      *-----------------------------------------------------------------
      * Lista para consulta do Acesso Carteira-UOR                     *
      *-----------------------------------------------------------------
      * VRS001 25.07.2023 - F9158937 - IMPLANTACAO
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSL126.

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
       WORKING-STORAGE                 SECTION.
      *-----------------------------------------------------------------
       77  CTE-INICIO-WS               PIC X(30) VALUE
                                       '*** WSS COMECA AQUI ***'.
       77  CTE-PROG                    PIC X(18) VALUE
                                       '*** SGTSL126 ***'.
       77  CTE-VERS                    PIC X(06) VALUE 'VRS001'.

       77  IIBP2000                    PIC  X(008)  VALUE 'IIBP2000'.

       77  ATCS6000                    PIC  X(008)  VALUE 'ATCS6000'.

       77  MSTS8042                    PIC  X(008)  VALUE 'MSTS8042'.

      *-----------------------------------------------------------------
      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informacoes
-INC HLPKE01B

       01  CTE-FIM-WS                  PIC  X(30)  VALUE
                                       '*** WSS TERMINA AQUI ***'.
      *-----------------------------------------------------------------
       LOCAL-STORAGE SECTION.
      *-----------------------------------------------------------------
       01  CTE-INICIO-LS               PIC X(30)  VALUE
                                       '*** LS COMECA AQUI   ***'.
      *--> Areas de trabalho
       01  IC-TAB                      PIC S9(04)     COMP-5 VALUE 0.
       01  QT-REG-SELD                 PIC S9(04)     COMP-5 VALUE 0.

       01  NM-PGM-CHAMADO              PIC X(008)     VALUE SPACES.

      *--> Book do container de erro
-INC IIBKERRO
      *--> Book para captura de erro via MSG1
-INC MSGKWMG1
      *--> Variaveis para tratamento de Abend e Condition
-INC HLPKW999
      *--> Variaveis para tratamento de CONTAINER
-INC SGTKERR1

      *--> Container de erro
       01  CTNR-PRM-ERRO               PIC X(16) VALUE 'PRM-ERRO'.

      *--> Variaveis utilizados para leitura
       01  REG-WS.
           03 CD-CTRA-TVM              PIC S9(09)    COMP-5
              OCCURS 101 TIMES                       VALUE 0.
           03 CD-UOR-ACSS-CTRA         PIC S9(09)    COMP-5
              OCCURS 101 TIMES                       VALUE 0.
           03 CD-USU-ULT-ATL           PIC X(8)
              OCCURS 101 TIMES                       VALUE ' '.
           03 TS-ULT-ATL               PIC X(26)
              OCCURS 101 TIMES                       VALUE ' '.

      *--> Variavel(is) de controle do cursor(es)
       01  CTL-CURSOR-PSQ-1.
           03  CD-FIM-CURSOR-PSQ-1     PIC S9(04)     COMP-5 VALUE 0.
               88  FIM-CURSOR-PSQ-1                          VALUE +100.
           03  IN-CURSOR-PSQ-ABTO-1    PIC  X(01)            VALUE 'N'.
               88  CURSOR-PSQ-ABTO-1   VALUE 'S'.
       01  CTL-CURSOR-PSQ-2.
           03  CD-FIM-CURSOR-PSQ-2     PIC S9(04)     COMP-5 VALUE 0.
               88  FIM-CURSOR-PSQ-2                          VALUE +100.
           03  IN-CURSOR-PSQ-ABTO-2    PIC  X(01)            VALUE 'N'.
               88  CURSOR-PSQ-ABTO-2   VALUE 'S'.
       01  CTL-CURSOR-PSQ-3.
           03  CD-FIM-CURSOR-PSQ-3     PIC S9(04)     COMP-5 VALUE 0.
               88  FIM-CURSOR-PSQ-3                          VALUE +100.
           03  IN-CURSOR-PSQ-ABTO-3    PIC  X(01)            VALUE 'N'.
               88  CURSOR-PSQ-ABTO-3   VALUE 'S'.
       01  CTL-CURSOR-PSQ-4.
           03  CD-FIM-CURSOR-PSQ-4     PIC S9(04)     COMP-5 VALUE 0.
               88  FIM-CURSOR-PSQ-4                          VALUE +100.
           03  IN-CURSOR-PSQ-ABTO-4    PIC  X(01)            VALUE 'N'.
               88  CURSOR-PSQ-ABTO-4   VALUE 'S'.

      *--> Book do programa
       01  SGTSL126-DADOS.
-INC SGTKL126

      *-----> Buscar Nome da UOR
-INC MSTK8042

      *--> Consulta nome usuário
       01  ATCS6000-DADOS.
-INC ATCK6000

      * Book do container de Serviços (para acionamento de serviços)
-INC IIBKSRVC

      *--> Area de trabalho SQL/ DB2.
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

      *--> Declaração do cursor 1 para pesquisa
           EXEC SQL
             DECLARE  CURSOR-PSQ-1 CURSOR
                      WITH ROWSET POSITIONING FOR
               SELECT A.CD_CTRA_TVM
                    , A.CD_UOR_ACSS_CTRA
                    , A.CD_USU_ULT_ATL
                    , A.TS_ULT_ATL
               FROM DB2SGT.ACSS_CTRA_TVM      A
               WHERE A.CD_CTRA_TVM        > :SGTSL126-CD-CTRA-TVM
                AND  A.CD_UOR_ACSS_CTRA   > :SGTSL126-CD-UOR-ACSS-CTRA
                AND ( (A.CD_CTRA_TVM   = :SGTSL126-CD-CTRA-TVM-ULT
                      AND  A.CD_UOR_ACSS_CTRA   >
                                     :SGTSL126-CD-UOR-ACSS-CTRA-ULT)
                     OR
                      (A.CD_CTRA_TVM   > :SGTSL126-CD-CTRA-TVM-ULT))
               ORDER BY  A.CD_CTRA_TVM,
                         A.CD_UOR_ACSS_CTRA
           END-EXEC.

      *--> Declaração do cursor 2 para pesquisa
           EXEC SQL
             DECLARE  CURSOR-PSQ-2 CURSOR
                      WITH ROWSET POSITIONING FOR
               SELECT A.CD_CTRA_TVM
                    , A.CD_UOR_ACSS_CTRA
                    , A.CD_USU_ULT_ATL
                    , A.TS_ULT_ATL
               FROM DB2SGT.ACSS_CTRA_TVM      A
               WHERE A.CD_CTRA_TVM        = :SGTSL126-CD-CTRA-TVM
                AND  A.CD_UOR_ACSS_CTRA   > :SGTSL126-CD-UOR-ACSS-CTRA
                AND (A.CD_UOR_ACSS_CTRA   >
                                     :SGTSL126-CD-UOR-ACSS-CTRA-ULT)
                ORDER BY  A.CD_CTRA_TVM,
                          A.CD_UOR_ACSS_CTRA
           END-EXEC.
      *--> Declaração do cursor 3 para pesquisa
           EXEC SQL
             DECLARE  CURSOR-PSQ-3 CURSOR
                      WITH ROWSET POSITIONING FOR
               SELECT A.CD_CTRA_TVM
                    , A.CD_UOR_ACSS_CTRA
                    , A.CD_USU_ULT_ATL
                    , A.TS_ULT_ATL
               FROM DB2SGT.ACSS_CTRA_TVM      A
               WHERE A.CD_CTRA_TVM        > :SGTSL126-CD-CTRA-TVM
                AND  A.CD_UOR_ACSS_CTRA   = :SGTSL126-CD-UOR-ACSS-CTRA
                AND (A.CD_CTRA_TVM        > :SGTSL126-CD-CTRA-TVM-ULT)
                ORDER BY  A.CD_CTRA_TVM,
                          A.CD_UOR_ACSS_CTRA
           END-EXEC.
      *--> Declaração do cursor 4 para pesquisa
           EXEC SQL
             DECLARE  CURSOR-PSQ-4 CURSOR
                      WITH ROWSET POSITIONING FOR
               SELECT A.CD_CTRA_TVM
                    , A.CD_UOR_ACSS_CTRA
                    , A.CD_USU_ULT_ATL
                    , A.TS_ULT_ATL
               FROM DB2SGT.ACSS_CTRA_TVM      A
               WHERE A.CD_CTRA_TVM        = :SGTSL126-CD-CTRA-TVM
                AND  A.CD_UOR_ACSS_CTRA   = :SGTSL126-CD-UOR-ACSS-CTRA
                ORDER BY  A.CD_CTRA_TVM,
                          A.CD_UOR_ACSS_CTRA
           END-EXEC.
       01  CTE-FIM-LS                  PIC X(30)  VALUE
                                       '*** LS TERMINA AQUI  ***'.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------
           MOVE 0001                   TO MSG1-CD-SEQL-ERRO

      *--> Habilita tratamento automatico de Erros DB2
-INC HLPKE997
      *--> Habilita captura automatica de abend e erros CICS
-INC HLPKE999
           MOVE CTE-PROG(5:8)          TO NM-PGM

           PERFORM 100000-OBTER-NOME-DO-CANAL
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO
           PERFORM 970000-LER-REQUISICAO-RECEBIDA

           PERFORM 140000-VALIDAR-PARAMETROS

           PERFORM 200000-PROCESSA

           PERFORM 980000-GRAVAR-CTNR-RESPOSTA
           PERFORM 999999-ENCERRAR

           GOBACK
           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-OBTER-NOME-DO-CANAL      SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO

      *--> Por causa da captura automática de erro CICS, deve ser
      *--> previsto o erro de container
           SET  ERRO-CANAL             TO TRUE

           EXEC CICS ASSIGN
              CHANNEL ( NM-CNL  )

              END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       140000-VALIDAR-PARAMETROS       SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                   TO MSG1-CD-SEQL-ERRO

      *--> Validar Chave

           IF  SGTSL126-CD-CTRA-TVM     LESS ZEROS
               SET   ERRO-PROGRAMADO    TO  TRUE
               MOVE  0001               TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' inválido.'       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL126-CD-UOR-ACSS-CTRA   LESS ZEROS
               SET   ERRO-PROGRAMADO    TO  TRUE
               MOVE  604                TO  MSG1-CD-ERRO-TS
               MOVE  'Código UOR informado inválido.'
                                        TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       140000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       200000-PROCESSA        SECTION.
      *-----------------------------------------------------------------
           MOVE 0004                   TO MSG1-CD-SEQL-ERRO
      *---------- Pesquisa com ambos campos zerados --------------------
           IF  SGTSL126-CD-CTRA-TVM        EQUAL ZEROS  AND
               SGTSL126-CD-UOR-ACSS-CTRA   EQUAL ZEROS
               PERFORM 971101-ABRE-CURSOR-PSQ-1
               PERFORM 971102-LER-CURSOR-PSQ-1

               PERFORM 300000-MOVE
                   VARYING IC-TAB FROM 1 BY 1
                     UNTIL IC-TAB > SGTSL126-QT-OCR-TAB

               PERFORM 971103-FECHA-CURSOR-PSQ-1
           END-IF
      *---------- Pesquisa só com Carteira preenchido  -----------------
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO
           IF  SGTSL126-CD-CTRA-TVM        NOT EQUAL ZEROS  AND
               SGTSL126-CD-UOR-ACSS-CTRA   EQUAL ZEROS
               PERFORM 972101-ABRE-CURSOR-PSQ-2
               PERFORM 972102-LER-CURSOR-PSQ-2

               PERFORM 300000-MOVE
                   VARYING IC-TAB FROM 1 BY 1
                     UNTIL IC-TAB > SGTSL126-QT-OCR-TAB

               PERFORM 972103-FECHA-CURSOR-PSQ-2
           END-IF
      *---------- Pesquisa só com UOR preenchido -----------------------
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO
           IF  SGTSL126-CD-CTRA-TVM        EQUAL ZEROS  AND
               SGTSL126-CD-UOR-ACSS-CTRA   NOT EQUAL ZEROS
               PERFORM 973101-ABRE-CURSOR-PSQ-3
               PERFORM 973102-LER-CURSOR-PSQ-3

               PERFORM 300000-MOVE
                   VARYING IC-TAB FROM 1 BY 1
                     UNTIL IC-TAB > SGTSL126-QT-OCR-TAB

               PERFORM 973103-FECHA-CURSOR-PSQ-3
           END-IF
      *---------- Pesquisa com Carteira e UOR preenchido  --------------
           MOVE 0007                  TO MSG1-CD-SEQL-ERRO
           IF  SGTSL126-CD-CTRA-TVM        NOT EQUAL ZEROS  AND
               SGTSL126-CD-UOR-ACSS-CTRA   NOT EQUAL ZEROS
               PERFORM 974101-ABRE-CURSOR-PSQ-4
               PERFORM 974102-LER-CURSOR-PSQ-4

               PERFORM 300000-MOVE
                   VARYING IC-TAB FROM 1 BY 1
                     UNTIL IC-TAB > SGTSL126-QT-OCR-TAB

               PERFORM 974103-FECHA-CURSOR-PSQ-4
           END-IF
           .
       200000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       300000-MOVE        SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

           MOVE CD-CTRA-TVM        (IC-TAB) TO
                                   SGTSL126-CD-CTRA-TVM-S      (IC-TAB)
           MOVE CD-UOR-ACSS-CTRA   (IC-TAB) TO
                                   SGTSL126-CD-UOR-ACSS-CTRA-S (IC-TAB)
           MOVE CD-USU-ULT-ATL     (IC-TAB) TO
                                   SGTSL126-CD-USU-ULT-ATL-S   (IC-TAB)
           MOVE TS-ULT-ATL         (IC-TAB) TO
                                   SGTSL126-TS-ULT-ATL-S       (IC-TAB)

           PERFORM 900001-BUSCA-NOME-USUARIO
           MOVE    K6000-NM-USU
                                  TO SGTSL126-NM-USU-ULT-ATL-S (IC-TAB)
           PERFORM 900002-BUSCA-NOME-UOR
           MOVE  MSTS8042-NM-UOR-RDZ
                                  TO SGTSL126-NM-UOR-RDZ-S     (IC-TAB)
           .
       300000-SAI.
           EXIT.
      *----------------------------------------------------------------*
004050 900001-BUSCA-NOME-USUARIO            SECTION.
004060*----------------------------------------------------------------*
004070     MOVE  0009                  TO  MSG1-CD-SEQL-ERRO
           MOVE  ATCS6000              TO  NM-PGM-CHAMADO

004110*    carregar o nº serviço / versão / operação / versão
004120
004130     MOVE 71739                  TO SRVC-ID-SRVC
004140     MOVE 1                      TO SRVC-VRS-SRVC
004150     MOVE 292018                 TO SRVC-ID-OPR
004160     MOVE 1                      TO SRVC-VRS-OPR
004170
004180     PERFORM 900003-GRAVAR-SERVICO
004190
004200     INITIALIZE ATCS6000-DADOS
004210     SET K6000-PSQ-INF-USU       TO   TRUE
004220     MOVE SGTSL126-CD-USU-ULT-ATL-S   (IC-TAB)
004230                                 TO  K6000-CD-USU
004240
004250     PERFORM 900004-GRAVAR-REQUISICAO
004260     PERFORM 900005-CHAMAR-SERVICO
004270     PERFORM 900006-BUSCAR-RESPOSTA-ERRO
004280     PERFORM 900007-BUSCAR-RESPOSTA
004290
004300     .
004310 900001-SAI.
004320     EXIT.
      *----------------------------------------------------------------*
004050 900002-BUSCA-NOME-UOR            SECTION.
004060*----------------------------------------------------------------*
004070     MOVE  0031                  TO  MSG1-CD-SEQL-ERRO
           MOVE  MSTS8042              TO  NM-PGM-CHAMADO
004110*    carregar o nº serviço / versão / operação / versão
004120
004130     MOVE ZEROS                  TO  SRVC-ID-SRVC
004140     MOVE ZEROS                  TO  SRVC-VRS-SRVC
004150     MOVE 1998646                TO  SRVC-ID-OPR
004160     MOVE 1                      TO  SRVC-VRS-OPR

004180     PERFORM 900003-GRAVAR-SERVICO

           INITIALIZE   MSTS8042-DADOS
                           REPLACING NUMERIC BY  ZEROS
           MOVE SGTSL126-CD-UOR-ACSS-CTRA-S    (IC-TAB)
                                         TO MSTS8042-CD-UOR
004240
004250     PERFORM 900004-GRAVAR-REQUISICAO
004260     PERFORM 900005-CHAMAR-SERVICO
004270     PERFORM 900006-BUSCAR-RESPOSTA-ERRO
004280     PERFORM 900007-BUSCAR-RESPOSTA
004290     .
004310 900002-SAI.
004320     EXIT.
      *----------------------------------------------------------------*
004630 900003-GRAVAR-SERVICO           SECTION.
004640*----------------------------------------------------------------*
004650*    Postar container de requisição de serviço.
004660*----------------------------------------------------------------*
004670     MOVE  0010                  TO  MSG1-CD-SEQL-ERRO

           MOVE  'PRM-RQSC-SRVC'       TO  NM-CTNR

           EXEC CICS  PUT
                     CONTAINER ( NM-CTNR                      )
004710               FROM      ( SRVC-PRM-RQSC-SRVC           )
004720               FLENGTH   ( LENGTH OF SRVC-PRM-RQSC-SRVC )
004730               RESP      ( MSG1-RESP1                   )
004740               RESP2     ( MSG1-RESP2                   )
004750     END-EXEC
004760
004770     EVALUATE TRUE
004780         WHEN MSG1-RESP1         EQUAL DFHRESP(NORMAL)
004790              CONTINUE
004800         WHEN OTHER
                     SET   ERRO-CONTAINER
                                       TO  TRUE
                     MOVE  9000        TO  MSG1-CD-ERRO-TS
                     MOVE  'Erro ao acionar o serviço.'
                                       TO  MSG1-TX-ERRO-TS
                     PERFORM  999000-GRAVA-ERRO-E-ENCERRA
004860     END-EVALUATE
004870
004880     .
004890 900003-FIM.
004900     EXIT.
004910*----------------------------------------------------------------*
004930 900004-GRAVAR-REQUISICAO        SECTION.
004940*----------------------------------------------------------------*
004950*    Postar container com dados de requisição (entrada)
004960*----------------------------------------------------------------*
004970     MOVE  0011                  TO  MSG1-CD-SEQL-ERRO
004980
           MOVE  'PRM-DADOS-RQSC'      TO  NM-CTNR

           EVALUATE NM-PGM-CHAMADO
           WHEN ATCS6000
004990         EXEC CICS PUT
005000               CONTAINER ( NM-CTNR                    )
005010               FROM      ( ATCS6000-DADOS             )
005020               FLENGTH   ( LENGTH OF ATCS6000-DADOS   )
005030               RESP      ( MSG1-RESP1                 )
005040               RESP2     ( MSG1-RESP2                 )
005050         END-EXEC
005060     WHEN MSTS8042
004990         EXEC CICS PUT
005000               CONTAINER ( NM-CTNR                    )
005010               FROM      ( MSTS8042-RQSC              )
005020               FLENGTH   ( LENGTH OF MSTS8042-RQSC    )
005030               RESP      ( MSG1-RESP1                 )
005040               RESP2     ( MSG1-RESP2                 )
005050         END-EXEC
           END-EVALUATE

005070     EVALUATE TRUE
005080         WHEN MSG1-RESP1         EQUAL DFHRESP(NORMAL)
005090              CONTINUE
005100         WHEN OTHER
                     SET   ERRO-CONTAINER
                                       TO  TRUE
                     MOVE  9004        TO  MSG1-CD-ERRO-TS
                     STRING 'Erro na gravação da requisição do módulo.'
                             NM-PGM-CHAMADO '.'
                             DELIMITED BY SIZE
                             INTO          MSG1-TX-ERRO-TS
                     PERFORM  999000-GRAVA-ERRO-E-ENCERRA
005160     END-EVALUATE
005170
005180     .
005190 900004-FIM.
005200     EXIT.
005220*----------------------------------------------------------------*
005230 900005-CHAMAR-SERVICO           SECTION.
005240*----------------------------------------------------------------*
005250*    Acionar programa IIBP2000 para requisição do serviço.
005260*----------------------------------------------------------------*
005270     MOVE  0012                  TO  MSG1-CD-SEQL-ERRO
005280
005290     EXEC CICS LINK
005300               PROGRAM   ( IIBP2000     )
005310               CHANNEL   ( NM-CNL       )
005320               RESP      ( MSG1-RESP1   )
005330               RESP2     ( MSG1-RESP2   )
005340     END-EXEC
005350
005360     EVALUATE TRUE
005370         WHEN MSG1-RESP1         EQUAL DFHRESP(NORMAL)
005380              CONTINUE
005390         WHEN OTHER
                     SET   ERRO-CONTAINER
                                       TO  TRUE
                     MOVE  9000        TO  MSG1-CD-ERRO-TS
                     MOVE  'Erro ao acionar o serviço.'
                                       TO  MSG1-TX-ERRO-TS
                     PERFORM  999000-GRAVA-ERRO-E-ENCERRA
005450     END-EVALUATE
005460
005470     .
005480 900005-FIM.
005490     EXIT.
      *----------------------------------------------------------------*
005520 900006-BUSCAR-RESPOSTA-ERRO     SECTION.
005530*----------------------------------------------------------------*
005540*    A presença do container PRM-ERRO indica que houve erro de
005550*    integração, a ausência do container indica sucesso.
005560*----------------------------------------------------------------*
005570     MOVE  0013                  TO  MSG1-CD-SEQL-ERRO
           MOVE  'PRM-ERRO'            TO  NM-CTNR
           EXEC CICS  GET
                     CONTAINER ( NM-CTNR                     )
005610               INTO      ( IIBKERRO-PRM-ERRO           )
005620               FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
005630               RESP      ( MSG1-RESP1                  )
005640               RESP2     ( MSG1-RESP2                  )
005650     END-EXEC.
005660
005670     EVALUATE TRUE
005680         WHEN MSG1-RESP1         EQUAL DFHRESP(CONTAINERERR)
005690              CONTINUE
005700         WHEN MSG1-RESP1         EQUAL DFHRESP(NORMAL)
                     SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
                     MOVE  3004        TO  MSG1-CD-ERRO-TS
                     STRING  'Erro ao acionar o módulo '
                             NM-PGM-CHAMADO '.'
                             DELIMITED BY SIZE
                             INTO          MSG1-TX-ERRO-TS
                     END-STRING
                     MOVE  NM-PGM-CHAMADO  TO  MSG1-NM-PGM-ACND
                     MOVE  IIBKERRO-CD-ERRO
                                       TO  MSG1-CD-RTN-PGM-ACND
                     MOVE  IIBKERRO-TX-ERRO
                                       TO  MSG1-TX-RTN-PGM-ACND
                     PERFORM  900008-MONITORAR-VARIAVEIS
                     PERFORM  999000-GRAVA-ERRO-E-ENCERRA
               WHEN  OTHER
                     SET   ERRO-CONTAINER
                                       TO  TRUE
                     MOVE  3000        TO  MSG1-CD-ERRO-TS
                     MOVE  'Erro na manipulação de container de dados.'
                                       TO  MSG1-TX-ERRO-TS
                     PERFORM  999000-GRAVA-ERRO-E-ENCERRA
005780     END-EVALUATE
005800     .
005810 900006-FIM.
005820     EXIT.
005840*----------------------------------------------------------------*
005850 900007-BUSCAR-RESPOSTA          SECTION.
005860*----------------------------------------------------------------*
005870*    Recupera o container com os dados devolvidos pelo provedor
005880*    do serviço.
005890*----------------------------------------------------------------*
005900     MOVE  0014                  TO  MSG1-CD-SEQL-ERRO
           MOVE  'PRM-DADOS-RPST'      TO  NM-CTNR

           EVALUATE NM-PGM-CHAMADO
           WHEN ATCS6000
               EXEC CICS  GET
                     CONTAINER ( NM-CTNR                  )
005940               INTO      ( ATCS6000-DADOS           )
005950               FLENGTH   ( LENGTH OF ATCS6000-DADOS )
005960               RESP      ( MSG1-RESP1               )
005970               RESP2     ( MSG1-RESP2               )
005980         END-EXEC
           WHEN MSTS8042
               EXEC CICS  GET
                     CONTAINER ( NM-CTNR                  )
005940               INTO      ( MSTS8042-RPST            )
005950               FLENGTH   ( LENGTH OF MSTS8042-RPST  )
005960               RESP      ( MSG1-RESP1               )
005970               RESP2     ( MSG1-RESP2               )
               END-EXEC
           END-EVALUATE
005990
006000     EVALUATE TRUE
006010         WHEN MSG1-RESP1         EQUAL DFHRESP(NORMAL)
006020              CONTINUE
006030         WHEN OTHER
                     SET   ERRO-CONTAINER
                                       TO  TRUE
                     MOVE  9003        TO  MSG1-CD-ERRO-TS
                     MOVE  'Erro na leitura da resposta do módulo .'
                                       TO  MSG1-TX-ERRO-TS
                     PERFORM  999000-GRAVA-ERRO-E-ENCERRA
006090     END-EVALUATE
006110     .
006120 900007-FIM.
006130     EXIT.
006140*-----------------------------------------------------------------
       900008-MONITORAR-VARIAVEIS      SECTION.
      *--> Monitora as variáveis dos módulos chamados
      *-----------------------------------------------------------------

           EVALUATE  NM-PGM-CHAMADO
      *------> Monitora as Variaveis de Programas chamados
               WHEN  ATCS6000
                     MOVE 'K6000-CD-USU'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE K6000-CD-USU
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT
               WHEN  MSTS8042
                     MOVE 'MSTS8042-CD-UOR'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  MSTS8042-CD-UOR
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  OTHER
                     CONTINUE
           END-EVALUATE
           .
       900008-SAI.
           EXIT.
      *-----------------------------------------------------------------
       970000-LER-REQUISICAO-RECEBIDA  SECTION.
      *-----------------------------------------------------------------
           MOVE 0015                   TO MSG1-CD-SEQL-ERRO

           MOVE ZEROS                       TO QT-CCT-LIDO
           MOVE LENGTH OF SGTSL126-RQSC     TO QT-CCT-EPRO

           EXEC CICS
                GET  CONTAINER ( 'SGTSL126-RQSC' )
                     CHANNEL   ( NM-CNL          )
                     NODATA
                     FLENGTH   ( QT-CCT-LIDO     )
           END-EXEC

           IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
               SET  ERRO-TAMANHO-AREA  TO  TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS
                GET  CONTAINER ( 'SGTSL126-RQSC'         )
                     CHANNEL   ( NM-CNL                  )
                     FLENGTH   ( LENGTH OF SGTSL126-RQSC )
                     INTO      ( SGTSL126-RQSC           )
           END-EXEC
           .
       970000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       971101-ABRE-CURSOR-PSQ-1          SECTION.
      *-----------------------------------------------------------------
           MOVE 0016                   TO MSG1-CD-SEQL-ERRO

           EXEC SQL
                OPEN CURSOR-PSQ-1
           END-EXEC
           MOVE 'S'                    TO IN-CURSOR-PSQ-ABTO-1
           .
       971101-SAI.
           EXIT.

      *-----------------------------------------------------------------
       971102-LER-CURSOR-PSQ-1         SECTION.
      *-----------------------------------------------------------------
           MOVE 0017                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE REG-WS

           EXEC SQL
                FETCH NEXT ROWSET
                      CURSOR-PSQ-1 FOR 0101 ROWS
                INTO :CD-CTRA-TVM
                   , :CD-UOR-ACSS-CTRA
                   , :CD-USU-ULT-ATL
                   , :TS-ULT-ATL
           END-EXEC

           MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ-1
           MOVE SQLERRD(3)             TO QT-REG-SELD

           IF  QT-REG-SELD EQUAL 0101
               SET SGTSL126-TEM-MAIS-REGISTRO TO TRUE
               MOVE 0100               TO QT-REG-SELD
           END-IF

           MOVE QT-REG-SELD            TO SGTSL126-QT-OCR-TAB
           MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ-1
           .
       971102-SAI.
           EXIT.

      *-----------------------------------------------------------------
       971103-FECHA-CURSOR-PSQ-1       SECTION.
      *-----------------------------------------------------------------
           MOVE 0018                   TO MSG1-CD-SEQL-ERRO

           MOVE 'N'                    TO IN-CURSOR-PSQ-ABTO-1
           EXEC SQL
                CLOSE CURSOR-PSQ-1
           END-EXEC
           .
       971103-SAI.
           EXIT.

      *-----------------------------------------------------------------
       972101-ABRE-CURSOR-PSQ-2          SECTION.
      *-----------------------------------------------------------------
           MOVE 0019                   TO MSG1-CD-SEQL-ERRO

           EXEC SQL
                OPEN CURSOR-PSQ-2
           END-EXEC
           MOVE 'S'                    TO IN-CURSOR-PSQ-ABTO-2
           .
       972101-SAI.
           EXIT.

      *-----------------------------------------------------------------
       972102-LER-CURSOR-PSQ-2         SECTION.
      *-----------------------------------------------------------------
           MOVE 0020                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE REG-WS

           EXEC SQL
                FETCH NEXT ROWSET
                      CURSOR-PSQ-2 FOR 0101 ROWS
                INTO :CD-CTRA-TVM
                   , :CD-UOR-ACSS-CTRA
                   , :CD-USU-ULT-ATL
                   , :TS-ULT-ATL
           END-EXEC

           MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ-2
           MOVE SQLERRD(3)             TO QT-REG-SELD

           IF  QT-REG-SELD EQUAL 0101
               SET SGTSL126-TEM-MAIS-REGISTRO TO TRUE
               MOVE 0100               TO QT-REG-SELD
           END-IF

           MOVE QT-REG-SELD            TO SGTSL126-QT-OCR-TAB
           MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ-2
           .
       972102-SAI.
           EXIT.

      *-----------------------------------------------------------------
       972103-FECHA-CURSOR-PSQ-2       SECTION.
      *-----------------------------------------------------------------
           MOVE 0021                  TO MSG1-CD-SEQL-ERRO

           MOVE 'N'                    TO IN-CURSOR-PSQ-ABTO-2
           EXEC SQL
                CLOSE CURSOR-PSQ-2
           END-EXEC
           .
       972103-SAI.
           EXIT.

      *-----------------------------------------------------------------
       973101-ABRE-CURSOR-PSQ-3          SECTION.
      *-----------------------------------------------------------------
           MOVE 0022                   TO MSG1-CD-SEQL-ERRO

           EXEC SQL
                OPEN CURSOR-PSQ-3
           END-EXEC
           MOVE 'S'                    TO IN-CURSOR-PSQ-ABTO-3
           .
       973101-SAI.
           EXIT.

      *-----------------------------------------------------------------
       973102-LER-CURSOR-PSQ-3         SECTION.
      *-----------------------------------------------------------------
           MOVE 0023                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE REG-WS

           EXEC SQL
                FETCH NEXT ROWSET
                      CURSOR-PSQ-3 FOR 0101 ROWS
                INTO :CD-CTRA-TVM
                   , :CD-UOR-ACSS-CTRA
                   , :CD-USU-ULT-ATL
                   , :TS-ULT-ATL
           END-EXEC

           MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ-3
           MOVE SQLERRD(3)             TO QT-REG-SELD

           IF  QT-REG-SELD EQUAL 0101
               SET SGTSL126-TEM-MAIS-REGISTRO TO TRUE
               MOVE 0100               TO QT-REG-SELD
           END-IF

           MOVE QT-REG-SELD            TO SGTSL126-QT-OCR-TAB
           MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ-3
           .
       973102-SAI.
           EXIT.

      *-----------------------------------------------------------------
       973103-FECHA-CURSOR-PSQ-3       SECTION.
      *-----------------------------------------------------------------
           MOVE 0024                   TO MSG1-CD-SEQL-ERRO

           MOVE 'N'                    TO IN-CURSOR-PSQ-ABTO-3
           EXEC SQL
                CLOSE CURSOR-PSQ-3
           END-EXEC
           .
       973103-SAI.
           EXIT.

      *-----------------------------------------------------------------
       974101-ABRE-CURSOR-PSQ-4          SECTION.
      *-----------------------------------------------------------------
           MOVE 0025                   TO MSG1-CD-SEQL-ERRO

           EXEC SQL
                OPEN CURSOR-PSQ-4
           END-EXEC
           MOVE 'S'                    TO IN-CURSOR-PSQ-ABTO-4
           .
       974101-SAI.
           EXIT.

      *-----------------------------------------------------------------
       974102-LER-CURSOR-PSQ-4         SECTION.
      *-----------------------------------------------------------------
           MOVE 0026                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE REG-WS

           EXEC SQL
                FETCH NEXT ROWSET
                      CURSOR-PSQ-4 FOR 0101 ROWS
                INTO :CD-CTRA-TVM
                   , :CD-UOR-ACSS-CTRA
                   , :CD-USU-ULT-ATL
                   , :TS-ULT-ATL
           END-EXEC

           MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ-4
           MOVE SQLERRD(3)             TO QT-REG-SELD

           IF  QT-REG-SELD EQUAL 0101
               SET SGTSL126-TEM-MAIS-REGISTRO TO TRUE
               MOVE 0100               TO QT-REG-SELD
           END-IF

           MOVE QT-REG-SELD            TO SGTSL126-QT-OCR-TAB
           MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ-4
           .
       974102-SAI.
           EXIT.

      *-----------------------------------------------------------------
       974103-FECHA-CURSOR-PSQ-4       SECTION.
      *-----------------------------------------------------------------
           MOVE 0027                   TO MSG1-CD-SEQL-ERRO

           MOVE 'N'                    TO IN-CURSOR-PSQ-ABTO-4
           EXEC SQL
                CLOSE CURSOR-PSQ-4
           END-EXEC
           .
       974103-SAI.
           EXIT.

      *-----------------------------------------------------------------
       980000-GRAVAR-CTNR-RESPOSTA     SECTION.
      *-----------------------------------------------------------------
           MOVE 0028                   TO MSG1-CD-SEQL-ERRO

      *--> Exclui eventual container de erro de outros programas
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO

      *--> Monta o nome do container de resposta
           MOVE 'SGTSL126-RPST'        TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *--> previsto o erro de container
           SET  ERRO-CONTAINER         TO TRUE

           EXEC CICS
                PUT  CONTAINER ( NM-CTNR                 )
                     CHANNEL   ( NM-CNL                  )
                     FLENGTH   ( LENGTH OF SGTSL126-RPST )
                     FROM      ( SGTSL126-RPST           )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       980000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       983000-EXCLUIR-RESPOSTA-ERRO    SECTION.
      *----------------------------------------------------------------*
           MOVE 0029                   TO MSG1-CD-SEQL-ERRO
      *----------------------------------------------------------------*
      * Exclui o container de resposta e de erro para evitar problemas *
      * em modulos executados mais de uma vez dentro da mesma sessao do*
      * CICS. Equivale ao initialize na area de resposta dos programas *
      * que utilizam COMMAREA.                                         *
      *----------------------------------------------------------------*
      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *--> nome_modulo -RPST

           MOVE NM-PGM                 TO NM-CTNR(1:8)
           MOVE '-RPST'                TO NM-CTNR(9:5)

           SET DEL-CONTAINER           TO TRUE

      *--> Exclui o container de resposta
      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS
                DELETE CONTAINER (NM-CTNR   )
                       RESP      (MSG1-RESP1)
                       RESP2     (MSG1-RESP2)
           END-EXEC

           EVALUATE TRUE
               WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
               WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    SET ERRO-CONTAINER TO TRUE
                    PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE

      *--> Excluir o container de erro
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO
           .
       983000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       983100-EXCLUIR-CONTAINER-ERRO   SECTION.
      *----------------------------------------------------------------*
           MOVE 0030                   TO MSG1-CD-SEQL-ERRO

      *--> Limpar variaveis container de erro
           INITIALIZE IIBKERRO-PRM-ERRO

      *--> Exclui o container de erro
           MOVE CTNR-PRM-ERRO          TO NM-CTNR
           SET DEL-CONTAINER           TO TRUE

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS
                DELETE CONTAINER (NM-CTNR   )
                       RESP      (MSG1-RESP1)
                       RESP2     (MSG1-RESP2)
           END-EXEC

           EVALUATE TRUE
               WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
               WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    SET ERRO-CONTAINER TO TRUE
                    PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       983100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------

      *--> Caso exista utilização de cursor
           IF  IN-CURSOR-PSQ-ABTO-1  EQUAL  'S'
               PERFORM 971103-FECHA-CURSOR-PSQ-1
           END-IF

           PERFORM 999200-MONITORAR-VRV-PGM-E-ENC
           .
       999000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999200-MONITORAR-VRV-PGM-E-ENC  SECTION.
      *-----------------------------------------------------------------
      *--> Monitora variàveis do pgm, grava e encerra processamento

           IF  MSG1-CD-ERRO-TS             EQUAL  ZEROS
               MOVE  8888              TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS             EQUAL  SPACES
               MOVE  'Erro não especificado no módulo.'
                                       TO  MSG1-TX-ERRO-TS
           END-IF

      *--> Verifica se o tipo de erro gerado possui tratamento interno
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

      *--> Adiciona as variáveis para monitoração
           MOVE 'SGTSL126-CD-CTRA-TVM'          TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL126-CD-CTRA-TVM            TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL126-CD-UOR-ACSS-CTRA'     TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL126-CD-UOR-ACSS-CTRA       TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL126-CD-CTRA-TVM-ULT'      TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL126-CD-CTRA-TVM-ULT        TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL126-CD-UOR-ACSS-CTRA-ULT' TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL126-CD-UOR-ACSS-CTRA-ULT   TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

      *--> grava o erro no msg
           PERFORM MSG1-NTF-MSG
           MOVE MSG1-CD-ERRO-TS        TO IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS        TO IIBKERRO-TX-ERRO

           EXEC CICS PUT
              CONTAINER ( CTNR-PRM-ERRO               )
              CHANNEL   ( NM-CNL                      )
              FROM      ( IIBKERRO-PRM-ERRO           )
              FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
              NOHANDLE
           END-EXEC

           PERFORM 999999-ENCERRAR
           .
       999200-SAI.
           EXIT.

      *---------------------------------------*
      * 999001-TRATA-ABEND-CICS        SECTION.
      * 999002-TRATA-CONDITION-CICS    SECTION.
      * 999003-ADICIONA-VRV-MNT        SECTION.
      *---------------------------------------*
-INC HLPKP999

      *---------------------------------------*
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *---------------------------------------*
-INC SGTKP999

      *---------------------------------------*
      * 999010-TRATA-ERRO-DB2          SECTION.
      *---------------------------------------*
-INC HLPKP997

      *---------------------------------------*
      * MSG1-NTF-MSG                   SECTION.
      *---------------------------------------*
-INC MSGKPMG1

      *---------------------------------------*
       999999-ENCERRAR                 SECTION.
      *---------------------------------------*

           EXEC CICS
                RETURN
           END-EXEC
           .
       999999-SAI.
           EXIT.
      *---------------------- FIM SGTSL126 ----------------------------