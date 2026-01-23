      *----------------------------------------------------------------*
      * PROGRAMA..: SGTS0209
      * ANALISTA..: F2054109 - Cintia Aparecida Soares Iglesias
      * AUTOR.....: F2054109 - Cintia Aparecida Soares Iglesias
      * DATA......: 03/04/2018
      * OBJETIVO..: Validar Permissão de Usuário na UOR Detentora da
      *             Operação na data atual
      * COMPILACAO: COBOL/CICS
      *-----------------------------------------------------------------
      * VRS001 03/04/2018 F2020377  DE 2016/145195 - Implantação
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID. SGTS0209.
      *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *
      *----------------------------------------------------------------*
       DATA DIVISION.
      *----------------------------------------------------------------*
      *
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

       77  CTE-INICIO-SS               PIC  X(40)  VALUE
                                            '*** S.S. COMEÇA AQUI ***'.
       77  CTE-PROG                    PIC  X(18)  VALUE
                                            '*** SGTS0209 ***'.
       77  CTE-VERS                    PIC  X(06)  VALUE 'VRS001'.

      * Módulos acionados
       77  IIBP2000                    PIC  X(08) VALUE 'IIBP2000'.
       77  ARHSOLOC                    PIC  X(08) VALUE 'ARHSOLOC'.
       77  SBTIMEDT                    PIC  X(08) VALUE 'SBTIMEDT'.

      *----------------------------------------------------------------*
      *                     Containers utilizados
      *----------------------------------------------------------------*

      * Container de erro
       77 CTNR-PRM-ERRO                PIC X(16) VALUE 'PRM-ERRO'.

      * Container de requisição desta subrotina
       77 CTNR-SGTS0209-RQSC           PIC X(16) VALUE 'SGTS0209-RQSC'.

      *----------------------------------------------------------------*
       LOCAL-STORAGE                   SECTION.
      *----------------------------------------------------------------*

      * Áreas de trabalho

       01  IC-INTC                     PIC S9(04) COMP-5 VALUE 0.

       01  QT-REG-SEL                  PIC S9(4) COMP-5 VALUE 0.

001920*-----------------------------------------------------------------
001930* AREA PARA RECUPERAR TIMESTAMP E DATA
001940*-----------------------------------------------------------------
       01  DADOS-SBTIMEDT.
           03  WK-CD-FUC               PIC X(03)      VALUE 'F01'.
           03  WK-AREA-TRB.
               05  WK-AREA-TRB-TAM     PIC S9(9) COMP VALUE 1024.
               05  WK-AREA-TRB-DADOS   PIC  X(1024)   VALUE ' '.
      *        X'0002' - DD.MM.AAAA-HH:MM:SS.CCCCCCXXXX
           03  WK-FMT-TS               PIC S9(4) COMP VALUE 1.
      *         X'001A'(26)
           03  WK-TAM-TS               PIC S9(4) COMP VALUE 30.
           03  WK-TS-FMTD.
               05  ANO                 PIC  9(4).
               05  FILLER              PIC  X(1).
               05  MES                 PIC  9(2).
               05  FILLER              PIC  X(1).
               05  DIA                 PIC  9(2).
               05  FILLER              PIC  X(1).
               05  HORA                PIC  9(2).
               05  FILLER              PIC  X(1).
               05  MINUTO              PIC  9(2).
               05  FILLER              PIC  X(1).
               05  SEGUNDO             PIC  9(2).
               05  FILLER              PIC  X(1).
               05  MICRO-SEGUNDO       PIC  9(6).
               05  FILLER              PIC  X(4).

      *--> Data atual
       01  TS-SQL-ATU                  PIC  X(26) VALUE ' '.
       01  TS-SQL-ATU-R                REDEFINES TS-SQL-ATU.
           03 DT-REF                   PIC  X(10).
           03 DT-REF-R                 REDEFINES   DT-REF.
             05 ANO                    PIC  9(04).
             05 FILLER                 PIC  X(01).
             05 MES                    PIC  9(02).
             05 FILLER                 PIC  X(01).
             05 DIA                    PIC  9(02).
           03  ANO                     PIC  9(04).
           03  FILLER                  PIC  X(01).
           03  MES                     PIC  9(02).
           03  FILLER                  PIC  X(01).
           03  DIA                     PIC  9(02).
           03  FILLER                  PIC  X(01).
           03  HORA                    PIC  9(02).
           03  FILLER                  PIC  X(01).
           03  MINUTO                  PIC  9(02).
           03  FILLER                  PIC  X(01).
           03  SEGUNDO                 PIC  9(02).
           03  FILLER                  PIC  X(01).
           03  MICRO-SEGUNDO           PIC  9(06).


       01  DT-SQL-ATU                  PIC  X(10) VALUE '  .  .    '.
       01  DT-SQL-ATU-R REDEFINES DT-SQL-ATU.
           03  DIA                     PIC  9(02).
           03  FILLER                  PIC  X(01).
           03  MES                     PIC  9(02).
           03  FILLER                  PIC  X(01).
           03  ANO                     PIC  9(04).


      * Consulta Localizacao do Usuario - via IIB
       01  ARHSOLOC-DADOS.
           03  LK-ENTRADA.
               05  LKE-TIP-TRBR        PIC  9(001).
               05  LKE-TIP-CMP-CHV     PIC  9(001).
               05  LKE-MATRICULA       PIC  9(009).
               05  LKE-CHAVE           PIC  X(008).
               05  FILLER REDEFINES LKE-CHAVE.
                   07  LKE-CHVF-LETRA  PIC  X(001).
                   07  LKE-CHVF-NR     PIC  9(007).
               05  FILLER REDEFINES LKE-CHAVE.
                   07  LKE-CHVT-LETRA  PIC  X(001).
                   07  LKE-CHVT-NR     PIC  9(006).
                   07  LKE-CHVT-ESP    PIC  X(001).
               05  LKE-CPF             PIC  9(011).
               05  FILLER REDEFINES LKE-CPF.
                   07  LKE-CPF-NR      PIC  9(009).
                   07  LKE-CPF-DV      PIC  9(002).
               05  LKE-MCI             PIC  9(010).
               05  FILLER              PIC  X(050).
           03  LK-SAIDA.
               05  LKS-CHAVE           PIC  X(008).
               05  LKS-MATRICULA       PIC  9(009).
               05  LKS-NOME            PIC  X(050).
               05  LKS-CPF             PIC  9(011).
               05  LKS-UOR-LCZC        PIC  9(009).
               05  LKS-UOR-PSC         PIC  9(009).
               05  LKS-PREF-LCZC       PIC  9(004).
               05  LKS-DT-DMA-LCZC     PIC  9(008).
               05  LKS-MCI             PIC  9(010).
               05  LKS-POS-RETORNO     PIC  9(003).
               05  LKS-COD-RETORNO     PIC  9(009).
               05  LKS-MSG-RETORNO     PIC  X(078).
               05  LKS-COD-FUNCAO      PIC  9(005).
               05  LKS-AGENCIA         PIC  9(004).
               05  LKS-CONTA           PIC  9(008).
               05  FILLER              PIC  X(083).
           03  TWA.
               05  PARMLIST            PIC S9(008) COMP OCCURS 7 TIMES.

      *----------------------------------------------------------------*
      * Books dos Containers e das subrotinas chamadas
      *----------------------------------------------------------------*

      * Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1
      * Book para captura de abend cics do bloco código HLPKW999
-INC HLPKW999

      * Book para captura de erro via MSG1
-INC MSGKWMG1

      * Book do container de requisição e resposta deste módulo
       01 SGTS0209-DADOS.
-INC SGTK0209

      * Book do Container de Erro
-INC IIBKERRO

      * Book do container de Serviços (para acionamento de serviços)
-INC IIBKSRVC

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

       01  CTE-FINAL-SS                PIC  X(40)      VALUE
                    '*** S.S. TERMINA AQUI ***'.

      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       000000-ROTINA-PRINCIPAL         SECTION.
      *----------------------------------------------------------------*

           PERFORM 100000-INICIAR

           PERFORM 200000-PROCESSAR

           PERFORM 980000-GRAVAR-CONTAINER-RPST

           PERFORM 999999-ENCERRAR

           GOBACK
           .
       000000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       100000-INICIAR                  SECTION.
      *----------------------------------------------------------------*
           MOVE 0001                   TO MSG1-CD-SEQL-ERRO

      *--> WHENEVER DB2
-INC HLPKE997

      *    Ativa a captura automática de abend do CICS.
      *    WHENEVER CICS
-INC HLPKE999

           PERFORM 970000-LER-CANAL

      *    Define o nome do programa p/ fins manipulação container
           MOVE CTE-PROG(5:8)          TO NM-PGM

      *    Limpa a resposta e erro antes de executar o módulo
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO

           PERFORM 971000-LER-REQUISICAO

           PERFORM 110000-VALIDAR-REQUISICAO
           .
       100000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       110000-VALIDAR-REQUISICAO       SECTION.
      *----------------------------------------------------------------*
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO

           IF  SGTS0209-CD-UOR-DTTR-OPR LESS OR EQUAL ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0012               TO MSG1-CD-ERRO-TS
               MOVE  'Código da UOR detentora da operação inválido.'
                                      TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0209-CD-USU-PMS  EQUAL SPACES
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0677               TO MSG1-CD-ERRO-TS
              MOVE 'Código do usuário com permissão de acesso inválido.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF

           .
       110000-SAI.
           EXIT.


      *----------------------------------------------------------------*
       200000-PROCESSAR                SECTION.
      *----------------------------------------------------------------*

           PERFORM 210000-CONSULTA-UOR-USU

           IF LKS-UOR-PSC NOT EQUAL SGTS0209-CD-UOR-DTTR-OPR
              PERFORM 220000-CONSULTA-PMS-ACSS-UOR

           END-IF

           MOVE IC-INTC        TO SGTS0209-QT-OCR-TAB

           .
       200000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       210000-CONSULTA-UOR-USU    SECTION.
      *----------------------------------------------------------------*
           MOVE 0003                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  ARHSOLOC-DADOS.

      *    Movimentar o nome do programa a ser executado
           MOVE ARHSOLOC               TO NM-PGM

      *    Movimentar os campos de entrada

           IF SGTS0209-CD-USU-PMS(1:1) EQUAL 'F'
              MOVE 1                   TO LKE-TIP-TRBR
           END-IF
           IF SGTS0209-CD-USU-PMS(1:1) EQUAL 'C'
              MOVE 2                   TO LKE-TIP-TRBR
           END-IF

           MOVE 2                      TO LKE-TIP-CMP-CHV

           MOVE SGTS0209-CD-USU-PMS    TO LKE-CHAVE

           PERFORM 910000-CHAMAR-SERVICO

           .
       210000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       220000-CONSULTA-PMS-ACSS-UOR     SECTION.
      *----------------------------------------------------------------*

           MOVE 0004                   TO MSG1-CD-SEQL-ERRO

           PERFORM 221000-BUSCA-TS-DT

           EXEC SQL
                SELECT  1
                INTO  :QT-REG-SEL
                FROM DB2SGT.PMS_USU_OPR_UOR
                WHERE
                   CD_UOR_DTTR_OPR = :SGTS0209-CD-UOR-DTTR-OPR
                AND CD_USU_PMS = :SGTS0209-CD-USU-PMS
                AND DT_INC_VGC_PMS <= :DT-SQL-ATU
                AND DT_FIM_VGC_PMS >=  :DT-SQL-ATU
                FETCH FIRST 1 ROW ONLY
           END-EXEC

           IF QT-REG-SEL EQUAL ZEROS
              ADD  1               TO IC-INTC
              MOVE IC-INTC         TO SGTS0209-CD-IDFR-INTC(IC-INTC)
              MOVE   'Usuário não possui permissão na UOR Detentora da O
      -               'peração.'
                             TO   SGTS0209-TX-DCR-INTC(IC-INTC)
           END-IF
           .
       220000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       221000-BUSCA-TS-DT              SECTION.
      *-----------------------------------------------------------------

           CALL SBTIMEDT            USING  WK-CD-FUC
                                           WK-AREA-TRB
                                           WK-FMT-TS
                                           WK-TAM-TS
                                           WK-TS-FMTD

           IF  RETURN-CODE      NOT EQUAL ZEROS
               SET  ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE RETURN-CODE        TO  MSG1-CD-RTN-PGM-ACND
               MOVE 'SBTIMEDT'         TO  MSG1-NM-PGM-ACND
                                           NM-PGM
               MOVE 0050               TO  MSG1-CD-ERRO-TS
               MOVE 'Erro ao obter data e hora atual.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA

           END-IF

           MOVE WK-TS-FMTD             TO  TS-SQL-ATU
           MOVE CORR DT-REF-R          TO  DT-SQL-ATU-R
           .
       221000-SAI.
           EXIT.

      *----------------------------------------------------------------*
      *                                                                *
      *  SECTION DA FAIXA DE 900000 A 969900 PARA DEMAIS ROTINAS       *
      *                                                                *
      *----------------------------------------------------------------*
      * Executa um módulo implementado com CONTAINER                   *
      *-----------------------------------------------------------------
       910000-CHAMAR-SERVICO           SECTION.
      *--> Acionar programa IIBP2000 para requisição do serviço.
      *-----------------------------------------------------------------

      *--> Grava o container com dados do serviço/operação a chamar
           PERFORM 981000-GRAVAR-CONTAINER-SRVC

      *--> Grava o container de requisicao do modulo a executar
           PERFORM 982000-GRAVAR-CONTAINER-RQSC

      *--> Limpa a resposta e erro antes de executar o modulo
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO


      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS LINK
                     PROGRAM   ( IIBP2000     )
                     CHANNEL   ( NM-CNL       )
                     RESP      ( MSG1-RESP1   )
                     RESP2     ( MSG1-RESP2   )
           END-EXEC

           EVALUATE TRUE
               WHEN MSG1-RESP1             EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    SET   ERRO-CICS    TO  TRUE
                    MOVE  9000         TO  MSG1-CD-ERRO-TS
                    STRING  'Erro ao acionar serviço: '
                            SRVC-ID-SRVC '.' SRVC-VRS-SRVC '.'
                            SRVC-ID-OPR  '.' SRVC-VRS-OPR
                            'via IIBP2000, ' 'RESP=' MSG1-RESP1
                                           ', RESP-2=' MSG1-RESP2 '.'
                            DELIMITED BY SIZE
                            INTO  MSG1-TX-ERRO-TS
                    END-STRING

                    PERFORM 999000-ERRO
           END-EVALUATE

      *--> Efetua a leitura do Container de Erro
           PERFORM 972000-LER-CONTAINER-DE-ERRO

      *--> Efetua a leitura do Container de Resposta
           PERFORM 973000-LER-RESPOSTA

           .
       910000-SAI.
           EXIT.


      *----------------------------------------------------------------*
       969000-MONITORAR-VARIAVEIS      SECTION.
      *----------------------------------------------------------------*
      * Monitora as variáveis dos módulos chamados
      *----------------------------------------------------------------*

      * Variáveis exclusivas de cada programa acionado
           EVALUATE NM-PGM
               WHEN ARHSOLOC
                    MOVE 'LKE-TIP-TRBR' TO MSGKW999-NM-VRV-MNT
                    MOVE LKE-TIP-TRBR   TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'LKE-TIP-CMP-CHV'  TO MSGKW999-NM-VRV-MNT
                    MOVE LKE-TIP-CMP-CHV    TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'LKE-CHAVE'   TO MSGKW999-NM-VRV-MNT
                    MOVE LKE-CHAVE     TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

           WHEN  SBTIMEDT
                    MOVE  'WK-CD-FUC'  TO  MSGKW999-NM-VRV-MNT
                    MOVE  WK-CD-FUC    TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'WK-AREA-TRB' TO  MSGKW999-NM-VRV-MNT
                    MOVE  WK-AREA-TRB   TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'WK-FMT-TS'   TO  MSGKW999-NM-VRV-MNT
                    MOVE  WK-FMT-TS     TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'WK-TAM-TS'   TO  MSGKW999-NM-VRV-MNT
                    MOVE  WK-TAM-TS     TO   MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'WK-TS-FMTD'  TO  MSGKW999-NM-VRV-MNT
                    MOVE  WK-TS-FMTD    TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

             WHEN OTHER
                  CONTINUE
           END-EVALUATE
           .
       969000-SAI.
           EXIT.

      *----------------------------------------------------------------*
      *                                                                *
      * SECTION DA FAIXA DE 970000 A 979900 PARA LEITURA DE DADOS      *
      *                                                                *
      *----------------------------------------------------------------*
       970000-LER-CANAL                SECTION.
      *    Obtem o canal corrente
      *----------------------------------------------------------------*
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CANAL             TO TRUE

           EXEC CICS ASSIGN
                     CHANNEL ( NM-CNL )
           END-EXEC

      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       970000-FIM.
           EXIT.

      *----------------------------------------------------------------*
       971000-LER-REQUISICAO           SECTION.
      *----------------------------------------------------------------*
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

      *    Monta o nome do container de requisição
           MOVE CTE-PROG(5:8)          TO NM-PGM
           MOVE CTNR-SGTS0209-RQSC     TO NM-CTNR

      *    Recupera o tamanho do container de requisição
           PERFORM 973100-LER-CONTAINER-NODATA

           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Efetua a leitura do container
           EXEC CICS GET
                     CONTAINER ( NM-CTNR       )
                     FLENGTH   ( QT-CCT-LIDO   )
                     INTO      ( SGTS0209-RQSC )
           END-EXEC

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       971000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       972000-LER-CONTAINER-DE-ERRO    SECTION.
      *----------------------------------------------------------------*
      *    A presença do container PRM-ERRO indica que houve erro de
      *    integração, a ausência do container indica sucesso.
      *----------------------------------------------------------------*

      *    Lê o container de erro dos módulos chamados
           SET GET-CONTAINER           TO TRUE
           MOVE CTNR-PRM-ERRO          TO NM-CTNR
           INITIALIZE IIBKERRO-PRM-ERRO

      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS GET
                     CONTAINER ( NM-CTNR                     )
                     INTO      ( IIBKERRO-PRM-ERRO           )
                     FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                     RESP      ( MSG1-RESP1                  )
                     RESP2     ( MSG1-RESP2                  )
           END-EXEC

           EVALUATE TRUE
      *        Caso deseje personalizar ou não interromper o
      *        processamento, alterar para não dar erro aqui SOMENTE
      *        PARA O módulo CHAMADO e tratar na section chamadora
               WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
                    CONTINUE
               WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
                    SET ERRO-PROGRAMA-ACIONADO TO TRUE
                    MOVE  NM-PGM       TO  MSG1-NM-PGM-ACND

                    IF  NM-PGM(1:3) EQUAL 'SGT'
                        MOVE  IIBKERRO-CD-ERRO
                                       TO  MSG1-CD-RTN-PGM-ACND
                                           MSG1-CD-ERRO-TS
                        MOVE  IIBKERRO-TX-ERRO
                                       TO  MSG1-TX-RTN-PGM-ACND
                                           MSG1-TX-ERRO-TS
                    ELSE
                       MOVE 3004          TO MSG1-CD-ERRO-TS
                       STRING  'Erro ao acionar o módulo ' NM-PGM '.'
                         DELIMITED BY SIZE
                         INTO           MSG1-TX-ERRO-TS

                       MOVE IIBKERRO-TX-ERRO TO MSG1-TX-RTN-PGM-ACND
                       MOVE IIBKERRO-CD-ERRO TO MSG1-CD-RTN-PGM-ACND
                    END-IF

      *             Monitorar variáveis do módulo chamado
                    PERFORM 969000-MONITORAR-VARIAVEIS
                    PERFORM 999000-ERRO
               WHEN OTHER
                    SET ERRO-CONTAINER TO TRUE
                    PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       972000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       973000-LER-RESPOSTA             SECTION.
      *----------------------------------------------------------------*
      *    Recupera o container com os dados de resposta (saída).
      *----------------------------------------------------------------*

      *    Monta o nome dos container de acordo com modulo a executar
           MOVE SPACES                 TO NM-CTNR

      *    Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *    nome_modulo -RPST

           EVALUATE NM-PGM
               WHEN ARHSOLOC
                    MOVE 'PRM-DADOS-RPST'
                                       TO NM-CTNR
               WHEN OTHER
                    MOVE NM-PGM        TO NM-CTNR(1:8)
                    MOVE '-RPST'       TO NM-CTNR(9:5)
           END-EVALUATE

      *    Recupera o tamanho do container de resposta
           PERFORM 973100-LER-CONTAINER-NODATA

      *Configura dados para leitura do container de resposta
           SET  GET-CONTAINER          TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Lê a RESPOSTA de acordo com o programa que a gravou

           EVALUATE NM-PGM

      *    Calcula a quantidade de caracteres esperado e limpa a área de
      *    dados antes de executar o GET (somentepara matrizes com dados
      *    não variáveis)

               WHEN ARHSOLOC
                    INITIALIZE ARHSOLOC-DADOS
                    MOVE LENGTH OF ARHSOLOC-DADOS     TO QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (ARHSOLOC-DADOS   )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN OTHER
                    SET   ERRO-PROGRAMADO
                                       TO  TRUE
                    MOVE  9003         TO  MSG1-CD-ERRO-TS
                    STRING  'Erro na leitura da resposta do módulo '
                            NM-PGM '.'
                            DELIMITED BY SIZE
                            INTO           MSG1-TX-ERRO-TS
                    PERFORM 999000-ERRO
           END-EVALUATE

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       973000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       973100-LER-CONTAINER-NODATA     SECTION.
      *----------------------------------------------------------------*

      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Obtém a quantidade de caracteres no container
           MOVE ZEROS                  TO QT-CCT-LIDO
           EXEC CICS GET
                     CONTAINER ( NM-CTNR     )
                     NODATA
                     FLENGTH   ( QT-CCT-LIDO )
           END-EXEC

      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       973100-SAI.
           EXIT.

      *----------------------------------------------------------------*
       980000-GRAVAR-CONTAINER-RPST    SECTION.
      *----------------------------------------------------------------*

      *    Exclui eventual container de erro de outros programas
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO

           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

      *    Monta o nome do container de resposta
           MOVE 'SGTS0209-RPST'        TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

           EXEC CICS PUT
                     CONTAINER (NM-CTNR                )
                     FROM      (SGTS0209-RPST          )
                     FLENGTH   (LENGTH OF SGTS0209-RPST)
           END-EXEC

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       980000-SAI.
           EXIT.
      *---------------------------------------------------------------*
       981000-GRAVAR-CONTAINER-SRVC    SECTION.
      *---------------------------------------------------------------*
      *    Postar container de requisição de serviço.
      *---------------------------------------------------------------*

      *    Monta o nome do container de requisicao do servico
           MOVE 'PRM-RQSC-SRVC'        TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE

           EVALUATE NM-PGM

               WHEN ARHSOLOC
                    MOVE 70663         TO SRVC-ID-SRVC
                    MOVE 1             TO SRVC-VRS-SRVC
                    MOVE 255605        TO SRVC-ID-OPR
                    MOVE 1             TO SRVC-VRS-OPR

           END-EVALUATE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

           EXEC CICS PUT
                     CONTAINER ( NM-CTNR                     )
                     FROM      ( SRVC-PRM-RQSC-SRVC          )
                     FLENGTH   ( LENGTH OF SRVC-PRM-RQSC-SRVC)
           END-EXEC

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       981000-SAI.
           EXIT.


      *----------------------------------------------------------------*
      *                                                                *
      * SECTION DA FAIXA DE 980000 A 989900 PARA GRAVACAO DE DADOS     *
      *                                                                *
      *----------------------------------------------------------------*
       982000-GRAVAR-CONTAINER-RQSC    SECTION.
      *----------------------------------------------------------------*
      *    Postar container com dados de requisição (entrada)
      *----------------------------------------------------------------*

      *    Monta o nome dos container de acordo com modulo a executar
           MOVE SPACES                 TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE

      *    Quando módulos via IIB, fixo nome PRM-DADOS-RQSC, senão
      *    nome_modulo -RQSC
           EVALUATE NM-PGM
               WHEN ARHSOLOC
                    MOVE 'PRM-DADOS-RQSC'
                                       TO NM-CTNR
               WHEN OTHER
                    MOVE NM-PGM        TO NM-CTNR(1:8)
                    MOVE '-RQSC'       TO NM-CTNR(9:5)
           END-EVALUATE


      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Faz put da requisição do programa a executar
           EVALUATE NM-PGM
               WHEN ARHSOLOC
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (ARHSOLOC-DADOS   )
                              FLENGTH   (LENGTH OF ARHSOLOC-DADOS   )
                    END-EXEC
               WHEN OTHER
                    MOVE 9006          TO MSG1-CD-ERRO-TS
                    MOVE 'Não definida a execução do módulo.'
                                       TO MSG1-TX-ERRO-TS
                    PERFORM 999000-ERRO
           END-EVALUATE

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
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
               WHEN ARHSOLOC
                    MOVE 'PRM-DADOS-RPST'
                                       TO NM-CTNR
               WHEN OTHER
                    MOVE NM-PGM        TO NM-CTNR(1:8)
                    MOVE '-RPST'       TO NM-CTNR(9:5)
           END-EVALUATE

           SET DEL-CONTAINER           TO TRUE

      *    Exclui o container de resposta
      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS DELETE
                     CONTAINER ( NM-CTNR    )
                     RESP      ( MSG1-RESP1 )
                     RESP2     ( MSG1-RESP2 )
           END-EXEC

           EVALUATE TRUE
               WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
               WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    SET ERRO-CONTAINER TO TRUE
                    PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE

      *    Excluir o container de erro
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO
           .
       983000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       983100-EXCLUIR-CONTAINER-ERRO   SECTION.
      *----------------------------------------------------------------*

      *    Limpar variáveis container de erro
           INITIALIZE IIBKERRO-PRM-ERRO

      *    Exclui o container de erro
           MOVE CTNR-PRM-ERRO          TO NM-CTNR
           SET DEL-CONTAINER           TO TRUE

      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS DELETE
                     CONTAINER ( NM-CTNR    )
                     RESP      ( MSG1-RESP1 )
                     RESP2     ( MSG1-RESP2 )
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

      *----------------------------------------------------------------*
       984000-GRAVAR-CONTAINER-ERRO    SECTION.
      *----------------------------------------------------------------*

      *    Grava o erro gerado no container de erro
      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS PUT
                     CONTAINER ( CTNR-PRM-ERRO               )
                     FROM      ( IIBKERRO-PRM-ERRO           )
                     FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                     RESP      ( MSG1-RESP1                  )
                     RESP2     ( MSG1-RESP2                  )
           END-EXEC
           .
       984000-SAI.
           EXIT.

      *----------------------------------------------------------------*
      *                                                                *
      * As sections abaixo iniciadas com 999000 representam os tratamen*
      * tos de erro utilizando o Sistema MSG (rotina MSGS0001)         *
      *                                                                *
      *----------------------------------------------------------------*
       999000-ERRO                     SECTION.
      *----------------------------------------------------------------*

           IF  MSG1-CD-ERRO-TS         EQUAL ZEROS
               MOVE 8888               TO MSG1-CD-ERRO-TS
           END-IF
           IF  MSG1-TX-ERRO-TS         EQUAL SPACES
               MOVE 'Erro não especificado no módulo.'
                                       TO MSG1-TX-ERRO-TS
           END-IF

           PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           .
       999000-ERRO-SAI.
           EXIT.

      *----------------------------------------------------------------*
      *     As sections abaixo representam os tratamentos de erro
      *    utilizando o sistema MSG de mensageria (rotina MSGS0001)
      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------

      *    Em caso de erro, desfaz as atualizações em banco de dados

           EXEC CICS SYNCPOINT
                     ROLLBACK
           END-EXEC

      *Verifica se o tipo de erro gerado possui tratamento interno
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

      *Adiciona as variáveis de entrada para monitoração
           MOVE 'SGTS0209-CD-UOR-DTTR-OPR' TO MSGKW999-NM-VRV-MNT
           MOVE  SGTS0209-CD-UOR-DTTR-OPR  TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0209-CD-USU-PMS' TO MSGKW999-NM-VRV-MNT
           MOVE  SGTS0209-CD-USU-PMS  TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

      *    Grava o erro gerado no sistema MSG - Mensageria
           PERFORM MSG1-NTF-MSG

           MOVE MSG1-CD-ERRO-TS        TO IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS        TO IIBKERRO-TX-ERRO

      *    Grava o erro gerado no container de erro
           PERFORM 984000-GRAVAR-CONTAINER-ERRO

      *    Encerra a transação
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
      *----------------------------------------------------------------*
       999999-ENCERRAR                 SECTION.
      *----------------------------------------------------------------*

           EXEC CICS
                RETURN
           END-EXEC
           .
       999999-SAI.
           EXIT.
      *---------------------- Fim SGTS0209 ----------------------------*