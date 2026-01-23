      *-----------------------------------------------------------------
      * PROGRAMA..: SGTS0619
      * ANALISTA..: F9188847 - Soila K. L. Akamatsu
      * AUTOR.....: F7023235 - MATHEUS SANTIAGO
      * DATA......: 01/04/2025
      * COMPILACAO: 54 - PSOSE600 - Cobol 6.3 c/otimizacao p/producao
      * OBJETIVO..: Listar Histórico da Situação do Evento Excluído
      *================================================================*
      *   VRS002  | 13/08/2025 F7464259 - alteracao: ORDER BY TS Desc,
      *   Inclusao dados tabelas: EST_EVT_TIT_EXCD, PRCT_CRNG_TIT_EXCD
      *   VRS001  | 01/04/2025 F7023235 - IMPLANTAÇÃO
      *================================================================*

      *************************
       IDENTIFICATION DIVISION.
        PROGRAM-ID. SGTS0619.
        AUTHOR. F7023235.
      *************************
      *
      **********************
       ENVIRONMENT DIVISION.
      **********************
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *
      ***************
       DATA DIVISION.
      ***************
      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
      *-----------------------------------------------------------------

       77  CTE-PROG                    PIC  X(18)  VALUE
                                       '*** SGTS0619 ***'.
       77  CTE-VERS                    PIC  X(06)  VALUE 'VRS002'.

      *--- Definição dos Nomes dos Programas Acionados
       01  TAB-PROGRAMAS-CHAMADOS.
           03  IIBP2000                PIC  X(08) VALUE 'IIBP2000'.
           03  ATCS6000                PIC  X(08) VALUE 'ATCS6000'.
      *
      *--- Definição dos Variaveis do Cobolito
-INC ALMKCBL1
      *-----------------------------------------------------------------
      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informacoes
-INC HLPKE01B
      *-----------------------------------------------------------------
       LOCAL-STORAGE SECTION.
      *-----------------------------------------------------------------
      *
      *--- Container de erro
       01  CTNR-PRM-ERRO               PIC X(16) VALUE 'PRM-ERRO'.

      *--- Container do usuário logado
       01  CTNR-PRM-INF-OPRD           PIC X(16) VALUE 'PRM-INF-OPRD'.

       01 IN-CONTAINER-ERRO            PIC  X(001) VALUE SPACES.
          88 TEM-CONTAINER-ERRO                    VALUE 'S'.
          88 NAO-TEM-CONTAINER-ERRO                VALUE 'N'.

      *
      *------------------------ G U A R D A S  ------------------------*
      *
       01 QT-REG-SELD                         PIC S9(4) COMP-5 VALUE 0.

      * Campos utilizados para leitura
       01  REG-WS.
           03  CD-EST-CRNG-EVT         PIC S9(4) COMP-5
                                            OCCURS 051 TIMES VALUE 0.
           03  TS-ULT-ATL              PIC  X(26)
                                            OCCURS 051 TIMES VALUE ' '.
           03  CD-USU-ULT-ATL          PIC  X(08)
                                            OCCURS 051 TIMES VALUE ' '.
           03  CD-EST-EVT-EXCD         PIC  S9(4) COMP-5
                                            OCCURS 051 TIMES VALUE 0.
           03  TS-EST-EVT-EXCD         PIC  X(26)
                                            OCCURS 051 TIMES VALUE ' '.
           03  CD-USU-ULT-EVT          PIC  X(08)
                                            OCCURS 051 TIMES VALUE ' '.
           03  TX-JST-EVT-EXCD         PIC  X(200)
                                            OCCURS 051 TIMES VALUE ' '.
           03  TX-JST-EVT-EXCD-N       PIC S9(4) COMP-5
                                            OCCURS 051 TIMES VALUE 0.
           03  CD-EST-PRCT-CRNG        PIC  S9(4) COMP-5
                                            OCCURS 051 TIMES VALUE 0.
           03  TS-EST-ENVO-CRNG        PIC  X(26)
                                            OCCURS 051 TIMES VALUE ' '.
           03  CD-USU-ULT-PRCT         PIC  X(08)
                                            OCCURS 051 TIMES VALUE ' '.
           03  TS-PRCT-RTN-EVT         PIC  X(26)
                                            OCCURS 051 TIMES VALUE ' '.
           03  TS-PRCT-RTN-EVT-N       PIC S9(4) COMP-5
                                            OCCURS 051 TIMES VALUE 0.
           03  TX-PRCT-EVT             PIC  X(50)
                                            OCCURS 051 TIMES VALUE ' '.
           03  TX-PRCT-EVT-N           PIC S9(4) COMP-5
                                            OCCURS 051 TIMES VALUE 0.
      *
      *------------------- S U B S C R I T O R E S --------------------*
      *
       77  SBS                           PIC S9(4) COMP-5 VALUE ZEROES.

      *
      *-------------------------- B O O K S ---------------------------*
      *
      *----------------------------------------------------------------*
      * SGTK0619 - BOOK DE ENTRADA E SAIDA DO PROGRAMA.                *
      *----------------------------------------------------------------*
      *
       01  SGTS0619-DADOS.

-INC SGTK0619

       01  ATCS6000-DADOS.
-INC ATCK6000 *> Consultar dados de usuário - via IIB
      *
      *----------------------------------------------------------------*
      *                            BOOKS IIB                           *
      *----------------------------------------------------------------*
      *
-INC IIBKERRO *> Variaveis para container de erro do IIB
      *
-INC MSGKWMG1 *> Variaveis para captura de erro via MSG1
      *
-INC HLPKW999 *> Variaveis para tratamento de Abend e Condition
      *
-INC SGTKERR1 *> Variaveis para tratamento de de erro via MSG
      *
-INC IIBKSRVC *> Interface com o sistema IIB
      *
-INC IIBKOPRD *> Book contendo o codigo do usuario logado
      *
      *
      *--------------------------- S Q L -------------------------------
      *
           EXEC SQL
              INCLUDE SQLCA
           END-EXEC.
      *
      *----------------------- C U R S O R E S -------------------------
      *
           EXEC SQL
             DECLARE  HIST_CURSOR CURSOR WITH ROWSET POSITIONING FOR
              SELECT  A.CD_EST_CRNG_EVT
                   ,  A.TS_ULT_ATL
                   ,  A.CD_USU_ULT_ATL
                   ,  B.CD_EST_EVT_EXCD
                   ,  B.TS_EST_EVT_EXCD
                   ,  B.CD_USU_ULT_ATL
                   ,  B.TX_JST_EVT_EXCD
                   ,  C.CD_EST_PRCT_CRNG
                   ,  C.TS_EST_ENVO_CRNG
                   ,  C.CD_USU_RSP_ULT_ATL
                   ,  C.TS_PRCT_RTN_EVT
                   ,  C.TX_PRCT_EVT
                FROM  DB2SGT.EST_PRCT_CRNG_EVT A
               INNER JOIN DB2SGT.EST_EVT_TIT_EXCD B
                      ON  B.CD_ITMT_FNCO     = A.CD_ITMT_FNCO
                      AND B.CD_GR_EVT_FDAR   = A.CD_GR_EVT_FDAR
                      AND B.NR_SEQL_EVT_EXCD = A.NR_SEQL_EVT_EXCD
                      AND B.TS_EST_EVT_EXCD  = A.TS_EST_EVT_EXCD
               INNER JOIN DB2SGT.PRCT_CRNG_TIT_EXCD C
                      ON  C.CD_ITMT_FNCO     = A.CD_ITMT_FNCO
                      AND C.CD_GR_EVT_FDAR   = A.CD_GR_EVT_FDAR
                      AND C.NR_SEQL_EVT_EXCD = A.NR_SEQL_EVT_EXCD
                      AND C.TS_EST_ENVO_CRNG = A.TS_EST_ENVO_CRNG
               WHERE A.CD_ITMT_FNCO          = :SGTS0619-CD-ITMT-FNCO
                  AND A.NR_SEQL_EVT_EXCD = :SGTS0619-NR-SEQL-EVT-EXCD
                  AND A.CD_GR_EVT_FDAR   = :SGTS0619-CD-GR-EVT-FDAR
               ORDER BY A.TS_ULT_ATL DESC
           END-EXEC.
      *
      *
      ********************
       PROCEDURE DIVISION.
      ********************
      *
      *----------------------------------------------------------------*
       000000-ROTINA-PRINCIPAL                                  SECTION.
      *----------------------------------------------------------------*
      *
           PERFORM  100000-INICIALIZA.
           PERFORM  200000-PROCESSA.
           PERFORM  300000-ENCERRAR.

           GOBACK.
      *
       000099-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       100000-INICIALIZA                                        SECTION.
      *----------------------------------------------------------------*
      *
-INC ALMKCBL2 *> Habilita Testes e Debug de Operacoes IIB
-INC HLPKE999 *> Habilita captura automatica de abend do CICS

           MOVE CTE-PROG(5:8)          TO NM-PGM.

           PERFORM 110000-INICIA-VARIAVEIS.
      * ----------------------------------------------------------------

           PERFORM 970000-LER-CANAL.

           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO.

           PERFORM 974000-LER-DADOS-OPRD.

           PERFORM 971000-LER-REQUISICAO.
      * ----------------------------------------------------------------
           PERFORM 120000-VALIDAR-REQUISICAO.
      *
       100099-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       110000-INICIA-VARIAVEIS                                  SECTION.
      *----------------------------------------------------------------*
      *
           MOVE  0001                  TO  MSG1-CD-SEQL-ERRO.

           INITIALIZE SGTS0619-RQSC.

           MOVE 0  TO SGTS0619-QT-OCR.

           PERFORM VARYING SBS FROM 1 BY 1 UNTIL SBS > 50
                INITIALIZE SGTS0619-LS-EST-EVT-EXCD (SBS)
           END-PERFORM.
      *
       110099-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       120000-VALIDAR-REQUISICAO                                SECTION.
      *----------------------------------------------------------------*
      *
           MOVE 0005                     TO MSG1-CD-SEQL-ERRO.

           IF  SGTS0619-CD-ITMT-FNCO EQUAL SPACES
               MOVE  1160                TO MSG1-CD-ERRO-TS
               MOVE 'Código do Título do Evento não informado.'
                                         TO MSG1-TX-ERRO-TS
               SET MNT-VRV               TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF.

           IF  SGTS0619-CD-GR-EVT-FDAR EQUAL ZEROS
               MOVE  1186                TO  MSG1-CD-ERRO-TS
               MOVE 'Código do Tipo de Evento inválido.'
                                         TO MSG1-TX-ERRO-TS
               SET MNT-VRV               TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF.

           IF  SGTS0619-NR-SEQL-EVT-EXCD EQUAL ZEROS
               MOVE  1179              TO  MSG1-CD-ERRO-TS
               MOVE 'Número do Evento SGT não informado.'
                                         TO MSG1-TX-ERRO-TS
               SET MNT-VRV               TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF.

      *
       120099-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       200000-PROCESSA                                          SECTION.
      *----------------------------------------------------------------*
      *
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO.

      * Não esta neste momento prevista rechamada:
           MOVE 'N'                 TO SGTS0619-IN-CTNC-PSQ.

           PERFORM 210000-LER-CURSOR-HISTORICO.
           PERFORM 220000-MONTA-HISTORICO.
      *
       200099-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       210000-LER-CURSOR-HISTORICO                              SECTION.
      *----------------------------------------------------------------*
      *
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO.


           EXEC SQL
               OPEN HIST_CURSOR
           END-EXEC.

           INITIALIZE REG-WS.

           EXEC SQL
                FETCH NEXT ROWSET HIST_CURSOR FOR 051 ROWS
                 INTO  :CD-EST-CRNG-EVT
                     , :TS-ULT-ATL
                     , :CD-USU-ULT-ATL
                     , :CD-EST-EVT-EXCD
                     , :TS-EST-EVT-EXCD
                     , :CD-USU-ULT-EVT
                     , :TX-JST-EVT-EXCD
                       :TX-JST-EVT-EXCD-N
                     , :CD-EST-PRCT-CRNG
                     , :TS-EST-ENVO-CRNG
                     , :CD-USU-ULT-PRCT
                     , :TS-PRCT-RTN-EVT
                       :TS-PRCT-RTN-EVT-N
                     , :TX-PRCT-EVT
                       :TX-PRCT-EVT-N
           END-EXEC.

           MOVE SQLERRD(3)             TO QT-REG-SELD.

           MOVE QT-REG-SELD            TO SGTS0619-QT-OCR.


           EXEC SQL
               CLOSE HIST_CURSOR
           END-EXEC.
      *
       210099-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       220000-MONTA-HISTORICO                                   SECTION.
      *----------------------------------------------------------------*
      *
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO.

           PERFORM VARYING SBS FROM 1 BY 1 UNTIL SBS > SGTS0619-QT-OCR

               MOVE CD-EST-CRNG-EVT (SBS)
                                     TO SGTS0619-CD-EST-CRNG-EVT (SBS)
               MOVE TS-ULT-ATL (SBS)
                                     TO SGTS0619-TS-ULT-ATL (SBS)
               MOVE CD-USU-ULT-ATL (SBS)
                                     TO SGTS0619-CD-USU-ULT-ATL (SBS)
               MOVE CD-EST-EVT-EXCD (SBS)
                                     TO SGTS0619-CD-EST-EVT-EXCD (SBS)
               MOVE TS-EST-EVT-EXCD (SBS)
                                     TO SGTS0619-TS-EST-EVT-EXCD (SBS)
               MOVE CD-USU-ULT-EVT (SBS)
                                     TO SGTS0619-CD-USU-ULT-EVT  (SBS)
               MOVE TX-JST-EVT-EXCD (SBS)
                                     TO SGTS0619-TX-JST-EVT-EXCD (SBS)
               MOVE CD-EST-PRCT-CRNG (SBS)
                                     TO SGTS0619-CD-EST-PRCT-CRNG(SBS)
               MOVE TS-EST-ENVO-CRNG (SBS)
                                     TO SGTS0619-TS-EST-ENVO-CRNG(SBS)
               MOVE CD-USU-ULT-PRCT (SBS)
                                     TO SGTS0619-CD-USU-ULT-PRCT (SBS)
               MOVE TS-PRCT-RTN-EVT (SBS)
                                     TO SGTS0619-TS-PRCT-RTN-EVT (SBS)
               MOVE TX-PRCT-EVT (SBS)
                                     TO SGTS0619-TX-PRCT-EVT (SBS)

               PERFORM 220100-MONTA-TEXTO
               PERFORM 220200-MONTA-NOME-USU-ULT-ATL
           END-PERFORM.

       220099-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       220100-MONTA-TEXTO                                       SECTION.
      *----------------------------------------------------------------*
      *
           MOVE 0011                   TO MSG1-CD-SEQL-ERRO.
      * ----------------------------------------------------------------
      * 1) Cadastrado: Evento em cadastramento (1) e não enviado para
      *    processamento processamento (1)
      *
      * 2) Pendente Aprovação: Evento finalizado (2) e não enviado para
      *    processamento processamento (1)
      *
      * 3) Indeferido: Evento indeferido (4) e não enviado para
      *    processamento processamento (1)
      *
      * 4) Aprovado a Processar CAC:  Evento aprovado (3) e foi liberado
      *    para ser processado pelo CAC (2)
      *
      * 5) Aprovado com Erro CAC: Evento aprovado (3) e deu algum erro
      *    de processamento no CAC (4)
      *
      * 6) Indeferido CAC: Evento aprovado (3) e teve alguma pendência
      *    negocial que o CAC apontou (5)
      *
      * 7) Finalizado: Evento aprovado aprovado (3) e foi processado sem
      *    pendencia por parte do CAC (3)
      *
      * 8) Cancelado a Processar CAC: Evento foi cancelado (5) e foi
      *    liberado para ser processado pelo CAC (2)
      *
      * 9) Cancelado com Erro CAC: Evento foi cancelado (5) e deu algum
      *    erro de processamento no CAC (4)
      */
      * 10) Cancelado: Evento foi cancelado (5) e foi processado sem
      *     pendencia por parte do CAC (3)

           EVALUATE CD-EST-CRNG-EVT (SBS)
               WHEN 1
                   MOVE 'Cadastrado' TO SGTS0619-TX-EST-EVT-EXCD (SBS)

               WHEN 2
                   MOVE 'Pendente Aprovação' TO
                                      SGTS0619-TX-EST-EVT-EXCD (SBS)
               WHEN 3
                   MOVE 'Indeferido' TO SGTS0619-TX-EST-EVT-EXCD(SBS)

               WHEN 4
                   MOVE 'Aprovado a Processar CAC' TO
                                       SGTS0619-TX-EST-EVT-EXCD (SBS)

               WHEN 5
                   MOVE 'Aprovado com Erro CAC' TO
                                       SGTS0619-TX-EST-EVT-EXCD (SBS)

               WHEN 6
                   MOVE 'Indeferido CAC' TO
                                       SGTS0619-TX-EST-EVT-EXCD (SBS)

               WHEN 7
                   MOVE 'Finalizado' TO SGTS0619-TX-EST-EVT-EXCD (SBS)

               WHEN 8
                   MOVE 'Cancelado a Processar CAC' TO
                                        SGTS0619-TX-EST-EVT-EXCD (SBS)

               WHEN 9
                   MOVE 'Cancelado com Erro CAC' TO
                                        SGTS0619-TX-EST-EVT-EXCD (SBS)

               WHEN 10
                   MOVE 'Cancelado' TO SGTS0619-TX-EST-EVT-EXCD (SBS)
           END-EVALUATE.

      *-----------------------------------------------------------------
       220200-MONTA-NOME-USU-ULT-ATL                            SECTION.
      *-----------------------------------------------------------------
      *--- Recupera o nome do usuário que efetuou a última atualização
      *-----------------------------------------------------------------

           MOVE  0009                  TO  MSG1-CD-SEQL-ERRO.

      *    suprime nome do usuário com ok no retorno do processamento
           IF SGTS0619-CD-EST-CRNG-EVT (SBS) NOT EQUAL 5 AND
              SGTS0619-CD-EST-CRNG-EVT (SBS) NOT EQUAL 6 AND
              SGTS0619-CD-EST-CRNG-EVT (SBS) NOT EQUAL 7 AND
              SGTS0619-CD-EST-CRNG-EVT (SBS) NOT EQUAL 9 AND
              SGTS0619-CD-EST-CRNG-EVT (SBS) NOT EQUAL 10

      *--     Se for um status normal, busca o nome do usuario
               INITIALIZE ATCS6000-DADOS

               MOVE  ATCS6000              TO  NM-PGM

               SET   K6000-PSQ-INF-USU     TO TRUE
               MOVE  SGTS0619-CD-USU-ULT-ATL (SBS) TO K6000-CD-USU
               PERFORM 910000-CHAMAR-SERVICO

               IF  IIBKERRO-CD-ERRO EQUAL ZEROS
                   MOVE  K6000-NM-USU TO SGTS0619-NM-USU-ULT-ATL (SBS)
               ELSE
                   MOVE  SPACES  TO SGTS0619-NM-USU-ULT-ATL (SBS)
               END-IF
           ELSE
      *--     Se for status 5, 6, 7, 9 ou 10
               MOVE  SPACES  TO SGTS0619-CD-USU-ULT-ATL (SBS)
               MOVE  SPACES  TO SGTS0619-NM-USU-ULT-ATL (SBS)
           END-IF.

       220299-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       300000-ENCERRAR                                          SECTION.
      *----------------------------------------------------------------*
      *
           MOVE 0014                   TO MSG1-CD-SEQL-ERRO.

           PERFORM 980000-GRAVAR-CONTAINER-RPST.
           PERFORM 999999-ENCERRAR.
      *
       300099-SAI.
           EXIT.
      *----------------------------------------------------------------
       910000-CHAMAR-SERVICO           SECTION.
      *----------------------------------------------------------------
      *--- Acionar programa IIBP2000 para requisição do serviço.
      *----------------------------------------------------------------
           MOVE  0011                  TO  MSG1-CD-SEQL-ERRO

      *--- Grava o container com dados do serviço/operação a chamar
           PERFORM 981000-GRAVAR-CONTAINER-SRVC

      *--- Grava o container de requisicao do modulo a executar
           PERFORM 982000-GRAVAR-CONTAINER-RQSC

      *--- Limpa a resposta e erro antes de executar o modulo
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO

      *--- Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS LINK
                     PROGRAM   ( IIBP2000     )
                     CHANNEL   ( NM-CNL       )
                     RESP      ( MSG1-RESP1   )
                     RESP2     ( MSG1-RESP2   )
           END-EXEC

           EVALUATE TRUE
               WHEN MSG1-RESP1         EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    STRING 'Erro ao acionar serviço: '
                           SRVC-ID-SRVC '.' SRVC-VRS-SRVC '.'
                           SRVC-ID-OPR  '.' SRVC-VRS-OPR
                           'via IIBP2000, ' 'RESP=' MSG1-RESP1
                           ', RESP-2=' MSG1-RESP2    '.'
                           DELIMITED BY SIZE
                           INTO            MSG1-TX-ERRO-TS
                    END-STRING
                    MOVE  9000         TO  MSG1-CD-ERRO-TS
                    SET   ERRO-CICS    TO  TRUE
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *--> Efetua a leitura do Container de Erro
           PERFORM 972000-LER-CONTAINER-DE-ERRO

           IF NOT TEM-CONTAINER-ERRO
      *--> Efetua a leitura do Container de Resposta
              PERFORM 973000-LER-RESPOSTA
           END-IF
           MOVE  SPACES                TO  IN-CONTAINER-ERRO
           .
       910000-SAI.
           EXIT.
      *
      *-----------------------------------------------------------------
       969000-MONITORAR-VARIAVEIS      SECTION.
      *--> Monitora as variáveis dos módulos chamados
      *-----------------------------------------------------------------
           MOVE  0015                  TO  MSG1-CD-SEQL-ERRO.

           EVALUATE  NM-PGM
               WHEN ATCS6000
                  MOVE  'K6000-CD-FUC'
                                       TO  MSGKW999-NM-VRV-MNT
                  MOVE  K6000-CD-FUC
                                       TO  MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE  'K6000-CD-USU'
                                       TO  MSGKW999-NM-VRV-MNT
                  MOVE  K6000-CD-USU
                                       TO  MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

               WHEN  OTHER
                    CONTINUE
           END-EVALUATE.

       969000-SAI.
           EXIT.

      *-----------------------------------------------------------------
      * SECTION DA FAIXA DE 970000 A 979900 PARA LEITURA DE DADOS
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       970000-LER-CANAL                                         SECTION.
      *-----------------------------------------------------------------
      *--- Obtem o canal corrente
      *-----------------------------------------------------------------
           MOVE  0016                  TO  MSG1-CD-SEQL-ERRO.

      *--- Por causa da captura automática de erro CICS, deve ser
      *--- previsto o erro de container
           SET   ERRO-CANAL            TO  TRUE.

           EXEC CICS ASSIGN
                     CHANNEL ( NM-CNL )
           END-EXEC.

      *--- Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE.
      *
       970000-FIM.
           EXIT.
      *-----------------------------------------------------------------
       971000-LER-REQUISICAO                                    SECTION.
      *-----------------------------------------------------------------
           MOVE  0017                    TO  MSG1-CD-SEQL-ERRO.

           MOVE  'SGTS0619'              TO  NM-PGM.
      *--- Monta o nome do container de requisição
           MOVE  'SGTS0619-RQSC'         TO  NM-CTNR.

      *--- Recupera o tamanho do container de requisição
           PERFORM 973100-LER-CONTAINER-NODATA.

      *--- Obtem o tamanho esperado da requisição
           MOVE  LENGTH OF SGTS0619-RQSC TO  QT-CCT-EPRO.

      *--- Valida tamanho esperado da requisição X tamanho recebido
           IF  QT-CCT-LIDO                 NOT EQUAL  QT-CCT-EPRO
               SET   ERRO-TAMANHO-AREA
                                       TO  TRUE
               MOVE  9001              TO  MSG1-CD-ERRO-TS
               MOVE  QT-CCT-EPRO       TO  VL-EXBR-1
               MOVE  QT-CCT-LIDO       TO  VL-EXBR-2
               STRING  'Erro no tamanho de área de requisição '
                        NM-PGM  '.'
                       ', Tamanho Esperado: '  VL-EXBR-1
                       ', Tamanho Recebido: '  VL-EXBR-2
                       DELIMITED BY SIZE     INTO  MSG1-TX-ERRO-TS
               END-STRING
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF.

           MOVE  0018                  TO  MSG1-CD-SEQL-ERRO.

      *--- Configura dados para leitura do container de requisição
           SET   GET-CONTAINER         TO  TRUE.

      *--- Por causa da captura automática de erro CICS, deve ser
      *--- previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE.

      *--- Efetua a leitura do container
           EXEC CICS GET
                     CONTAINER ( NM-CTNR       )
                     FLENGTH   ( QT-CCT-LIDO   )
                     INTO      ( SGTS0619-RQSC )
           END-EXEC.

      *--- Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE.
      *
       971000-SAI.
           EXIT.
      *
      *-----------------------------------------------------------------
       972000-LER-CONTAINER-DE-ERRO    SECTION.
      *--> A presença do container PRM-ERRO indica que houve erro de
      *    integração, a ausência do container indica sucesso.
      *-----------------------------------------------------------------
           MOVE  0019                  TO  MSG1-CD-SEQL-ERRO

      *--> Le o container de erro dos módulos chamados
           SET   GET-CONTAINER         TO  TRUE
           MOVE  'PRM-ERRO'            TO  NM-CTNR
           INITIALIZE                  IIBKERRO-PRM-ERRO

           MOVE  SPACES                TO  IN-CONTAINER-ERRO

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS GET
                     CONTAINER ( NM-CTNR                     )
                     INTO      ( IIBKERRO-PRM-ERRO           )
                     FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                     RESP      ( MSG1-RESP1                  )
                     RESP2     ( MSG1-RESP2                  )
           END-EXEC

      *==> Observação <=================================================
      *        Caso deseje personalizar ou não interromper o
      *        processamento, alterar para não dar erro aqui SOMENTE
      *        PARA O MÓDULO CHAMADO e tratar na section chamadora
      *=================================================================
           EVALUATE  TRUE
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(CONTAINERERR)
                     CONTINUE
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(NORMAL)
                     SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
                     MOVE  NM-PGM                TO  MSG1-NM-PGM-ACND

                        IF  NM-PGM(1:3) EQUAL 'SGT'
                            MOVE  IIBKERRO-CD-ERRO
                                       TO  MSG1-CD-RTN-PGM-ACND
                                           MSG1-CD-ERRO-TS
                            MOVE  IIBKERRO-TX-ERRO
                                       TO  MSG1-TX-RTN-PGM-ACND
                                           MSG1-TX-ERRO-TS

                       ELSE
                          MOVE 3004          TO MSG1-CD-ERRO-TS
                          STRING 'Erro ao acionar o módulo ' NM-PGM '.'
                            DELIMITED BY SIZE
                            INTO           MSG1-TX-ERRO-TS

                          MOVE IIBKERRO-TX-ERRO TO MSG1-TX-RTN-PGM-ACND
                          MOVE IIBKERRO-CD-ERRO TO MSG1-CD-RTN-PGM-ACND
                       END-IF
                       PERFORM 969000-MONITORAR-VARIAVEIS
                       PERFORM 999000-GRAVA-ERRO-E-ENCERRA
               WHEN  OTHER
                     SET   ERRO-CONTAINER
                                       TO  TRUE
                     PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       972000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       973000-LER-RESPOSTA                                      SECTION.
      *-----------------------------------------------------------------
      *--- Recupera o container com os dados de resposta (saída).
      *-----------------------------------------------------------------

      *--- Monta o nome dos container de acordo com modulo a executar
           MOVE SPACES                 TO NM-CTNR.

      *--- Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *--- nome_modulo -RPST

           EVALUATE NM-PGM
               WHEN ATCS6000
                    MOVE  'PRM-DADOS-RPST'
                                       TO  NM-CTNR
               WHEN OTHER
                    MOVE  NM-PGM       TO  NM-CTNR(1:8)
                    MOVE  '-RPST'      TO  NM-CTNR(9:5)
           END-EVALUATE.

      *--- Recupera o tamanho do container de resposta
           PERFORM 973100-LER-CONTAINER-NODATA.

      *--- Configura dados para leitura do container de resposta
           SET  GET-CONTAINER          TO TRUE.

      *--- Por causa da captura automática de erro CICS, deve ser
      *--- previsto o erro de container
           SET  ERRO-CONTAINER         TO TRUE.

      *--- Lê a RESPOSTA de acordo com o programa que a gravou

           EVALUATE NM-PGM
      *      Calcula a quantidade de caracteres esperado e limpa a área
      *      de dados antes de executar o GET (somentepara matrizes com
      *      dados não variáveis)
               WHEN ATCS6000
                    INITIALIZE ATCS6000-DADOS
                    EXEC CICS GET
                              CONTAINER (NM-CTNR        )
                              INTO      (ATCS6000-DADOS )
                              FLENGTH   (QT-CCT-LIDO    )
                    END-EXEC

               WHEN OTHER
                    MOVE 9003          TO MSG1-CD-ERRO-TS
                    MOVE 'Erro na leitura da resposta do módulo.'
                                       TO MSG1-TX-ERRO-TS
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE.

      *--- Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE.
      *
       973000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       973100-LER-CONTAINER-NODATA                              SECTION.
      *----------------------------------------------------------------*
      *--- Por causa da captura automática de erro CICS, deve ser
      *--- previsto o erro de container
           SET  ERRO-CONTAINER         TO TRUE.

      *--- Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE.

      *--- Obtem a quantidade de caracteres no container
           MOVE ZEROS                  TO QT-CCT-LIDO.
           EXEC CICS GET
                     CONTAINER (NM-CTNR    )
                     NODATA
                     FLENGTH   (QT-CCT-LIDO)
           END-EXEC.

      *--- Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE.
      *
       973100-SAI.
           EXIT.
      *----------------------------------------------------------------*
       974000-LER-DADOS-OPRD           SECTION.
      *----------------------------------------------------------------*
           MOVE  0020                  TO  MSG1-CD-SEQL-ERRO

      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE
           MOVE 'PRM-INF-OPRD'         TO NM-CTNR

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

           INITIALIZE PRM-INF-OPRD
           EXEC CICS GET
                CONTAINER ( NM-CTNR                )
                INTO      ( PRM-INF-OPRD           )
                FLENGTH   ( LENGTH OF PRM-INF-OPRD )
           END-EXEC.

      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       974000-SAI.
           EXIT.

      *-----------------------------------------------------------------
      * SECTION DA FAIXA DE 980000 A 989900 PARA GRAVAÇÃO DE DADOS
      *-----------------------------------------------------------------
      *-----------------------------------------------------------------
       980000-GRAVAR-CONTAINER-RPST    SECTION.
      *-----------------------------------------------------------------

      *    Exclui eventual container de erro de outros programas
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO

           MOVE  0021                  TO  MSG1-CD-SEQL-ERRO

      *    Monta o nome do container de resposta
           MOVE 'SGTS0619-RPST'        TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET  ERRO-CONTAINER         TO TRUE

           EXEC CICS PUT
                     CONTAINER ( NM-CTNR                 )
                     FROM      ( SGTS0619-RPST           )
                     FLENGTH   ( LENGTH OF SGTS0619-RPST )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       980000-SAI.
           EXIT.
      *---------------------------------------------------------------  -
       981000-GRAVAR-CONTAINER-SRVC    SECTION.
      *--> Postar container de requisição de serviço.
      *----------------------------------------------------------------
           MOVE  0022                  TO  MSG1-CD-SEQL-ERRO

      *--> Monta o nome do container de requisicao do servico
           MOVE  'PRM-RQSC-SRVC'       TO  NM-CTNR
           SET   PUT-CONTAINER         TO  TRUE

           EVALUATE NM-PGM
               WHEN ATCS6000
                    MOVE  71739        TO  SRVC-ID-SRVC
                    MOVE  0001         TO  SRVC-VRS-SRVC
                    MOVE  292018       TO  SRVC-ID-OPR
                    MOVE  0001         TO  SRVC-VRS-OPR

           END-EVALUATE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

           EXEC CICS PUT
                     CONTAINER ( NM-CTNR                     )
                     FROM      ( SRVC-PRM-RQSC-SRVC          )
                     FLENGTH   ( LENGTH OF SRVC-PRM-RQSC-SRVC)
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       981000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       982000-GRAVAR-CONTAINER-RQSC    SECTION.
      *--> Postar container com dados de requisição (entrada)
      *                            Nome do Container de Requisição
      *    Módulo CONTAINER  -     NOME_PGM-RQSC
      *    Módulo IIB (FIXO) -     PRM-DADOS-RQSC
      *-----------------------------------------------------------------
           MOVE  0023                  TO  MSG1-CD-SEQL-ERRO

      *--> Monta o nome dos container de acordo com módulo a executar
           MOVE  SPACES                TO  NM-CTNR
           SET   PUT-CONTAINER         TO  TRUE

      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *    nome_módulo -RPST
           EVALUATE  NM-PGM
               WHEN ATCS6000
                    MOVE  'PRM-DADOS-RQSC'
                                       TO  NM-CTNR
               WHEN  OTHER
                     MOVE  NM-PGM      TO  NM-CTNR(1:8)
                     MOVE  '-RQSC'     TO  NM-CTNR(9:5)
           END-EVALUATE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *--> Faz PUT da requisição do programa a executar
           EVALUATE  NM-PGM
               WHEN ATCS6000
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (ATCS6000-DADOS           )
                              FLENGTH   (LENGTH OF ATCS6000-DADOS )
                    END-EXEC
               WHEN  OTHER
                     SET   ERRO-PROGRAMADO
                                       TO  TRUE
                     MOVE  9004        TO  MSG1-CD-ERRO-TS
                     STRING 'Erro na gravação da requisição do módulo.'
                             NM-PGM '.'
                             DELIMITED BY SIZE
                             INTO          MSG1-TX-ERRO-TS
                     END-STRING
                     PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       982000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       983000-EXCLUIR-RESPOSTA-ERRO    SECTION.
      *----------------------------------------------------------------*
      * Exclui o container de resposta e de erro para evitar problemas *
      * em modulos executados mais de uma vez dentro da mesma sessao do*
      * CICS. Equivale ao initialize na area de resposta dos programas *
      * que utilizam COMMAREA.                                         *
      *----------------------------------------------------------------*
      *    Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *    nome_modulo -RPST

           EVALUATE NM-PGM
               WHEN ATCS6000
                    MOVE  'PRM-DADOS-RPST'
                                       TO  NM-CTNR
               WHEN OTHER
                    MOVE  NM-PGM       TO  NM-CTNR(1:8)
                    MOVE  '-RPST'      TO  NM-CTNR(9:5)
           END-EVALUATE.

           SET DEL-CONTAINER           TO TRUE.

      *    Exclui o container de resposta
      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS DELETE
                     CONTAINER (NM-CTNR)
                     RESP      (MSG1-RESP1)
                     RESP2     (MSG1-RESP2)
           END-EXEC.

           EVALUATE TRUE
               WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
               WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    SET ERRO-CONTAINER TO TRUE
                    PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE.

      *    Excluir o container de erro
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO.
      *
       983000-SAI.
           EXIT.
      *
      *-----------------------------------------------------------------
       983100-EXCLUIR-CONTAINER-ERRO   SECTION.
      *-----------------------------------------------------------------
           MOVE  0024                  TO MSG1-CD-SEQL-ERRO

      *--> Limpar variaveis container de erro
           INITIALIZE IIBKERRO-PRM-ERRO

      *--> Exclui o container de erro
           MOVE 'PRM-ERRO'             TO NM-CTNR
           SET DEL-CONTAINER           TO TRUE

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS DELETE
                     CONTAINER (NM-CTNR)
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
       984000-GRAVAR-CONTAINER-ERRO    SECTION.
      *-----------------------------------------------------------------

      *--> Grava o erro gerado no container de erro
      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS PUT
                     CONTAINER ( 'PRM-ERRO'                  )
                     FROM      ( IIBKERRO-PRM-ERRO           )
                     FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                     RESP      ( MSG1-RESP1                  )
                     RESP2     ( MSG1-RESP2                  )
           END-EXEC
           .
       984000-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *    As sections abaixo iniciadas com 999000 representam os
      *    tratamentos de erro pelo Sistema MSG (rotina MSGS0001)
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       999000-ERRO                     SECTION.
      *-----------------------------------------------------------------

           IF  MSG1-CD-ERRO-TS             EQUAL  ZEROS
              MOVE  8888               TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS             EQUAL  SPACES
               MOVE  'Erro não especificado no módulo.'
                                       TO  MSG1-TX-ERRO-TS
           END-IF

           PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           .
       999000-ERRO-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------

      *--> Em caso de erro, desfaz as atualizações em banco de dados
           EXEC CICS SYNCPOINT
                     ROLLBACK
           END-EXEC

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

      *Adiciona as variáveis de entrada para monitoração

               MOVE 'SGTS0619-CD-ITMT-FNCO'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0619-CD-ITMT-FNCO
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0619-CD-GR-EVT-FDAR'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0619-CD-GR-EVT-FDAR
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0619-NR-SEQL-EVT-EXCD'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE SGTS0619-NR-SEQL-EVT-EXCD
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

      *--> Grava o erro gerado no Sistema MSG - Mensageria
           PERFORM  MSG1-NTF-MSG

           MOVE  MSG1-CD-ERRO-TS       TO  IIBKERRO-CD-ERRO
           MOVE  MSG1-TX-ERRO-TS       TO  IIBKERRO-TX-ERRO

      *--> Grava o erro no container de erro
           PERFORM 984000-GRAVAR-CONTAINER-ERRO

      *--> Encerra a Transação
           PERFORM 999999-ENCERRAR
           .
       999000-SAI.
           EXIT.
      *----------------------------------------
      * 999001-TRATA-ABEND-CICS        SECTION.
      * 999002-TRATA-CONDITION-CICS    SECTION.
      * 999003-ADICIONA-VRV-MNT        SECTION.
      *----------------------------------------
-INC HLPKP999
      *----------------------------------------
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *----------------------------------------
-INC SGTKP999
      *----------------------------------------
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
      *-----------------------------------------------------------------
       999999-ENCERRAR                 SECTION.
      *-----------------------------------------------------------------

           EXEC CICS
                RETURN
           END-EXEC
           .
       999999-SAI.
           EXIT.
