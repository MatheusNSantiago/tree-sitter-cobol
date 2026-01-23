      *-----------------------------------------------------------------
      * PROGRAMA..: SGTS0142
      * ANALISTA..: F2054109 - Cintia Iglesias
      * AUTOR.....: F2804421 - Eliana Cristina Ramos da Silva
      * DATA......: 10/03/2017
      * OBJETIVO..: Negocial - Detalhamento dados AgenteFiduciário /
      *             Emissor da Operação
      * COMPILACAO: COBOL/CICS
      *-----------------------------------------------------------------
      * VRS001 13/03/2017 F2804421 DE 2016/145195 - IMPLANTACAO
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTS0142.
      *
      *-----------------------------------------------------------------
       ENVIRONMENT                     DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       CONFIGURATION                   SECTION.
      *-----------------------------------------------------------------
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *
      *-----------------------------------------------------------------
       DATA                            DIVISION.
      *-----------------------------------------------------------------


       WORKING-STORAGE                 SECTION.
      *-----------------------------------------------------------------

       77  CTE-INICIO-SS                 PIC  X(040)  VALUE
                    '*** S.S. COMECA AQUI ***'.

       77  CTE-PROG                      PIC  X(018)  VALUE
                    '*** SGTS0142 ***'.
       77  CTE-VERS                      PIC  X(006)  VALUE 'VRS001'.

      *--> Módulos acionados
       77  IIBP2000                      PIC  X(008) VALUE 'IIBP2000'.
       77  ACES2100                      PIC  X(008) VALUE 'ACES2100'.
       77  ARGSO006                      PIC  X(008) VALUE 'ARGSO006'.
       77  ARHP0883                      PIC  X(008) VALUE 'ARHP0883'.
       77  ARHS007V                      PIC  X(008) VALUE 'ARHS007V'.
       77  MSTS8041                      PIC  X(008) VALUE 'MSTS8041'.
       77  MCIS0001                      PIC  X(008) VALUE 'MCIS0001'.
       77  MCIS0024                      PIC  X(008) VALUE 'MCIS0024'.
       77  MCIS1263                      PIC  X(008) VALUE 'MCIS1263'.
       77  MCIS1340                      PIC  X(008) VALUE 'MCIS1340'.
       77  MCIS1342                      PIC  X(008) VALUE 'MCIS1342'.

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

      *--> Areas de trabalho
       01  IND                           PIC  S9(4) COMP-5 VALUE 0.

       01  WS-RESP-CICS.
           03  WS-RESP                 PIC  9(08)    VALUE 0.
           03  WS-RESP2                PIC  9(08)    VALUE 0.
       01  TB-DADOS-MUN                PIC  X(96)    VALUE SPACES.
       01  TB-DADOS-MUN-R REDEFINES TB-DADOS-MUN.
           03 TB-LS OCCURS 3 TIMES.
              05  TB-MUN               PIC  X(30).
              05  TB-TR                PIC  X(01).
              05  TB-UF                PIC  X(02).
              05  TB-BR                PIC  X(01).

       01  TX-END-CML                  PIC  X(498)   VALUE SPACES.
       01  TB-END-CML-R.
           03 TB-END-LS OCCURS 498 TIMES.
              05  TB-END-CML           PIC  X(001).

      *-----------------------------------------------------------------
      * Book dos Containers e das subrotinas chamadas
      *-----------------------------------------------------------------

      * Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1
      * Book para captura de abend cics da bloco codigo MSGKP999
-INC HLPKW999

      * Book para captura de erro via MSG1
-INC MSGKWMG1

      * Book do container de requisição e resposta deste modulo
       01 SGTS0142-DADOS.
-INC SGTK0142

      * Book ACE - Consultar dados de localização do usuário e controla
      *             sua atualização
       01  ACES2100-DADOS.
-INC ACEKRTNW
-INC ACEK2100

      * Book ARH - Consultar Funcionário BB a partir MCI - via IIB
       01  ARHP0883-DADOS.
           03 K0883-ENTRADA.
              05 LKS-CD-MCI           PIC 9(010).
           03 K0883-SAIDA.
              05  LKS-CD-MATR         PIC  9(009) OCCURS 10 TIMES.
              05  LKS-TX-NOME         PIC  X(050) OCCURS 10 TIMES.
              05  LKS-CD-SIT          PIC  9(003) OCCURS 10 TIMES.
              05  LKS-CD-LOCAL        PIC  9(004) OCCURS 10 TIMES.
              05  LKS-CD-CMSS         PIC  9(005) OCCURS 10 TIMES.
              05  LKS-CD-LOT          PIC  9(004) OCCURS 10 TIMES.
              05  LKS-CD-FUN-LOT      PIC  9(005) OCCURS 10 TIMES.
              05  LKS-CD-AG           PIC  9(004) OCCURS 10 TIMES.
              05  LKS-CD-CTA          PIC  9(008) OCCURS 10 TIMES.
              05  LKS-QTDE-REG-RTND   PIC S9(008) COMP.
              05  LKS-CD-RTN          PIC  9(004).
              05  LKS-POS-ERRO-RTN    PIC  9(002).
              05  LKS-ERRO-RTN        PIC  X(030).

      *  -INC ARHK0883

      * Book ARH - Validar Comissão de Funcionário - via IIB
      * 01  ARHS007V-DADOS.
-INC ARHK007V

      * Book ARG - Consultar Nome Município e Sigla UF - via IIB
       01  ARGSO906-DADOS.
-INC ARGSO906

      * Book MST - Consultar Unidade Organizacional - via IIB
      * 01  MSTS8041-DADOS.
-INC MSTK8041

      * Book MCI - Consultar dados básicos do cliente - via IIB
       01  MCIS0001-DADOS.
-INC MCIB0001

      * Book MCI - Consultar Ocupação, Cônjuge(Companheiro) e Dados
      *             Pessoais de Cliente Pessoa Física - via IIB
       01  MCIS0024-DADOS.
-INC MCIB0024

      * Book MCI - Listar emails ativos do cliente por código do cliente
      *             - via IIB
       01  MCIS1263-DADOS.
-INC MCIB1263

      * Book MCI - Consultar telefone - via IIB
       01  MCIS1340-DADOS.
-INC MCIB1340

      * Book MCI - Listar endereço por código do cliente - via IIB
       01  MCIS1342-DADOS.
-INC MCIB1342

      * Book do Container de Erro
-INC IIBKERRO
      * Book do container de Serviços (para acionamento de serviços)
-INC IIBKSRVC
      * Book dos dados do usuário
-INC IIBKOPRD

       01  CTE-FINAL-SS                  PIC X(40)      VALUE
                    '*** S.S. TERMINA AQUI ***'.

      *-----------------------------------------------------------------
       PROCEDURE                       DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------

           PERFORM 100000-INICIAR

           PERFORM 200000-PROCESSAR

           PERFORM 999999-ENCERRAR

           GOBACK
           .
       000000-SAI.

      *-----------------------------------------------------------------
       100000-INICIAR                  SECTION.
      *-----------------------------------------------------------------
           MOVE  0001                  TO  MSG1-CD-SEQL-ERRO

      *--> Ativa a captura automatica de abend do CICS.
      *    WHENEVER CICS
-INC HLPKE999

           PERFORM 970000-LER-CANAL

           MOVE  'SGTS0142'            TO  NM-PGM

      *     PERFORM 983000-EXCLUIR-RESPOSTA-ERRO

           PERFORM 971000-LER-REQUISICAO

      *    Caso não seja necessário utilização do Usuário Logado,
      *    a section 974000-LER-DADOS-OPRD poderá ser excluída
           PERFORM 974000-LER-DADOS-OPRD

           PERFORM 110000-VALIDAR-REQUISICAO
           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-VALIDAR-REQUISICAO       SECTION.
      *--> Valida campo a campo da requisicao
      *-----------------------------------------------------------------
           MOVE  0002                  TO  MSG1-CD-SEQL-ERRO

           IF  SGTS0142-CD-CLI             NEGATIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0188              TO  MSG1-CD-ERRO-TS
               MOVE  'Código do cliente inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0142-CD-CLI             EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0189              TO  MSG1-CD-ERRO-TS
               MOVE  'Código do cliente não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF
           .
       110000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-PROCESSAR                SECTION.
      *-----------------------------------------------------------------
           MOVE  0003                  TO  MSG1-CD-SEQL-ERRO

           PERFORM 220000-MONTAR-RESPOSTA

           PERFORM 980000-GRAVAR-CONTAINER-RPST
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       220000-MONTAR-RESPOSTA          SECTION.
      *-----------------------------------------------------------------
           MOVE  0005                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      ARHP0883-DADOS
           MOVE  ARHP0883              TO  NM-PGM
           MOVE  SGTS0142-CD-CLI       TO  LKS-CD-MCI

           PERFORM 910000-CHAMAR-SERVICO

           IF LKS-CD-RTN                   EQUAL ZEROS
              PERFORM 221000-TRATAR-FUNC
           ELSE
              PERFORM 222000-TRATAR-CTT
           END-IF

           .
       220000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       221000-TRATAR-FUNC              SECTION.
      *-----------------------------------------------------------------
           MOVE  0006                  TO  MSG1-CD-SEQL-ERRO

           STRING LKS-CD-MATR(1)  ' / '  LKS-TX-NOME(1)
                  DELIMITED BY SIZE  INTO
                               SGTS0142-NM-CTT

           PERFORM 221100-TRATAR-NOME-DEPEND
           PERFORM 221200-TRATAR-NOME-CRG
           PERFORM 221300-TRATAR-TEL-COML

           .
       221000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       221100-TRATAR-NOME-DEPEND       SECTION.
      *-----------------------------------------------------------------
           MOVE  0007                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      MSTS8041-DADOS

      *--> Movimentar o nome do programa a ser executado
           MOVE  MSTS8041              TO  NM-PGM

      *--> Movimentar os campos de entrada
           MOVE  LKS-CD-LOT (1)        TO  MSTS8041-CD-DEPE-UOR-RQSC
           MOVE  ZEROS                 TO  MSTS8041-NR-ORD-SBDD-RQSC
           PERFORM 910000-CHAMAR-SERVICO

           MOVE  MSTS8041-NM-UOR-RDZ   TO  SGTS0142-NM-CIA

           .
       221100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       221200-TRATAR-NOME-CRG          SECTION.
      *-----------------------------------------------------------------
           MOVE  0008                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      ARHS007V-RQSC

      *--> Movimentar o nome do programa a ser executado
           MOVE  ARHS007V              TO  NM-PGM

      *--> Movimentar os campos de entrada
           MOVE  LKS-CD-CMSS (1)       TO  ARHS007V-CD-TIP-CMSS-FUC
           PERFORM 910000-CHAMAR-SERVICO

           MOVE  ARHS007V-NM-TIP-CMSS-FUC
                                       TO  SGTS0142-NM-CRG-CTT

           .
       221200-SAI.
           EXIT.

      *-----------------------------------------------------------------
       221300-TRATAR-TEL-COML          SECTION.
      *-----------------------------------------------------------------
           MOVE  0009                  TO  MSG1-CD-SEQL-ERRO

           IF  LKS-CD-MATR(1)              NOT EQUAL  ZEROS
               INITIALIZE                  ACES2100-DADOS
               MOVE  0001              TO  K2100-CD-FUC
               MOVE  LKS-CD-MATR(1)    TO  K2100-CD-USU
               MOVE  ACES2100          TO  NM-PGM
               PERFORM 920000-CHAMAR-COMMAREA

               IF  K2100-CNT-TCEL          NOT EQUAL  ZEROS
                   STRING  K2100-CD-DDD-TEL-TRBD(1)
                           K2100-NR-TEL-TRBD(1)
                      ' '  K2100-CD-DDD-TEL-TRBD(2)
                           K2100-NR-TEL-TRBD(2)
                      ' '  K2100-CD-DDD-TEL-TRBD(3)
                           K2100-NR-TEL-TRBD(3)
                           DELIMITED BY SIZE
                           INTO            SGTS0142-NR-TEL-CML-CTT

                   STRING  K2100-CD-DDD-TCEL(1)  K2100-NR-TCEL(1)
                     ' '   K2100-CD-DDD-TCEL(2)  K2100-NR-TCEL(2)
                     ' '   K2100-CD-DDD-TCEL(3)  K2100-NR-TCEL(3)
                           DELIMITED BY SIZE
                           INTO            SGTS0142-NR-TCEL-CTT

                   MOVE K2100-END-EMAI-CPTV TO
                             SGTS0142-TX-EMAI-CTT
               END-IF
           END-IF

           .
       221300-SAI.
           EXIT.

      *-----------------------------------------------------------------
       222000-TRATAR-CTT               SECTION.
      *-----------------------------------------------------------------
           MOVE  0010                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      MCIS0001-DADOS
           MOVE  MCIS0001              TO  NM-PGM

      *--> Movimentar os campos de entrada
           MOVE  SGTS0142-CD-CLI       TO  MCIS0001-CLIENTE

           PERFORM 910000-CHAMAR-SERVICO

           MOVE  MCIS0001-NOME         TO  SGTS0142-NM-CTT

           EVALUATE  MCIS0001-TIPO
           WHEN  0002
              MOVE  MCIS0001-NOME      TO  SGTS0142-NM-CIA

           WHEN  0001
              INITIALIZE                   MCIS0024-DADOS
              MOVE  MCIS0024           TO  NM-PGM

      *--> Movimentar os campos de entrada
              MOVE  SGTS0142-CD-CLI    TO  MCIS0024-CLIENTE
              MOVE  'S'                TO  MCIS0024-IND-OCUP-PRIN
                                           MCIS0024-IND-CONJUGE

              PERFORM 910000-CHAMAR-SERVICO
              MOVE  MCIS0024-NOME-EMPREGADOR     TO
                                    SGTS0142-NM-CIA
              MOVE  MCIS0024-TXT-CARGO TO  SGTS0142-NM-CRG-CTT

           WHEN  OTHER
      *       STRING 'Erro chamada MCIS0001. MCIS0001-TIPO =  '
      *                 MCIS0001-TIPO '.'
      *        DELIMITED BY  SIZE       INTO IIBKERRO-TX-ERRO
      *        PERFORM 990002-GRAVAR-CTNR-ERRO

               CONTINUE

           END-EVALUATE

           PERFORM 222100-TRATAR-TEL-COML
           PERFORM 222200-TRATAR-EMAIL
           PERFORM 222300-TRATAR-END-CML

           .
       222000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       222100-TRATAR-TEL-COML          SECTION.
      *-----------------------------------------------------------------
           MOVE  0011                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      MCIS1340-DADOS
           MOVE  MCIS1340              TO  NM-PGM

           MOVE  'SGT'                 TO  MCIS1340-SYS-CHAMADOR
           MOVE  CTE-PROG(5:8)         TO  MCIS1340-PRG-CHAMADOR
      *--> Consulta por Codigo Cliente, ordenado pela Data de Gravação
      *    (registro mais novos primeiro)
           MOVE  0004                  TO  MCIS1340-FUNCAO
           MOVE  KOPRD-CD-OPRD         TO  MCIS1340-USUARIO
           MOVE  SGTS0142-CD-CLI       TO  MCIS1340-CD-CLI

           PERFORM 910000-CHAMAR-SERVICO

           IF  MCIS1340-CD-CLI-S           NOT EQUAL  ZEROS
               PERFORM  VARYING  IND
                        FROM 0001 BY 0001
                        UNTIL  IND         GREATER  0003
      *---------> Codigo da linha de comunicacao:
      *           0002 - Comercial valor fixo
      *           0003 - Celular
                  EVALUATE  MCIS1340-CD-LNH-CMCO-S (IND)
                      WHEN  0002
                            STRING  '(' MCIS1340-NR-DDD-S (1) ') '
                                        MCIS1340-NR-TEL-S (1)  ' / '
                                    '(' MCIS1340-NR-DDD-S (2) ') '
                                        MCIS1340-NR-TEL-S (2)  ' / '
                                    '(' MCIS1340-NR-DDD-S (3) ') '
                                        MCIS1340-NR-TEL-S (3)
                                    DELIMITED BY SIZE
                                    INTO   SGTS0142-NR-TEL-CML-CTT
                      WHEN  0003
                            STRING  '(' MCIS1340-NR-DDD-S (1) ') '
                                        MCIS1340-NR-TEL-S (1)  ' / '
                                    '(' MCIS1340-NR-DDD-S (2) ') '
                                        MCIS1340-NR-TEL-S (2)  ' / '
                                    '(' MCIS1340-NR-DDD-S (3) ') '
                                        MCIS1340-NR-TEL-S (3)
                                    DELIMITED BY SIZE
                                    INTO   SGTS0142-NR-TCEL-CTT
                  END-EVALUATE
               END-PERFORM
           END-IF

           .
       222100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       222200-TRATAR-EMAIL             SECTION.
      *-----------------------------------------------------------------
           MOVE  0012                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      MCIS1263-DADOS
           MOVE  MCIS1263              TO  NM-PGM

           MOVE  'SGT'                 TO  MCIS1263-SYS-CHAMADOR
           MOVE  CTE-PROG(5:8)         TO  MCIS1263-PRG-CHAMADOR
           MOVE   4                    TO  MCIS1263-FUNCAO
           MOVE   KOPRD-CD-OPRD        TO  MCIS1263-USUARIO
           MOVE   SGTS0142-CD-CLI      TO  MCIS1263-CD-CLI

           PERFORM 910000-CHAMAR-SERVICO

           IF  MCIS1263-CD-CLI-S           NOT EQUAL  ZEROS
              MOVE  MCIS1263-TXT-EMAIL-S(1)  TO
                             SGTS0142-TX-EMAI-CTT
           END-IF

           .
       222200-SAI.
           EXIT.

      *-----------------------------------------------------------------
       222300-TRATAR-END-CML           SECTION.
      *-----------------------------------------------------------------
           MOVE  0013                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      MCIS1342-DADOS
                                       TB-DADOS-MUN
           MOVE  MCIS1342              TO  NM-PGM

           MOVE  'SGT'                 TO  MCIS1342-SYS-CHAMADOR
           MOVE  CTE-PROG(5:8)         TO  MCIS1342-PRG-CHAMADOR
           MOVE  0003                  TO  MCIS1342-FUNCAO
           MOVE  0002                  TO  MCIS1342-CD-TIP-END
      *** somente para teste utilizar função = 1 para trazer endereços.
      *** após testes retornar função = 3 e tipo end = 2 (comercial)
      ***  MOVE   1                    TO  MCIS1342-FUNCAO

           MOVE  KOPRD-CD-OPRD         TO  MCIS1342-USUARIO
           MOVE  SGTS0142-CD-CLI       TO  MCIS1342-CD-CLI
      *
           PERFORM 910000-CHAMAR-SERVICO

      *  Buscar até 3 endereços
               IF  MCIS1342-RET-CODE       EQUAL ZEROS
              PERFORM
                  VARYING  IND FROM 1 BY 1
                  UNTIL    IND > 3
                  IF  MCIS1342-CD-MUN-S  (IND)
                                           NOT EQUAL ZEROS
                     PERFORM 222310-BUSCAR-MUNICIPIO
                  END-IF
              END-PERFORM

               STRING  MCIS1342-TX-LGR-S  (1) ' '
                       MCIS1342-TX-CMPT-S (1) ' '
                       MCIS1342-TX-BAI-S  (1) ' '
                       TB-MUN (1) TB-TR (1) TB-UF (1) TB-BR (1)
                       MCIS1342-TX-LGR-S  (2) ' '
                       MCIS1342-TX-CMPT-S (2) ' '
                       MCIS1342-TX-BAI-S  (2) ' '
                       TB-MUN (2) TB-TR (2) TB-UF (2) TB-BR (2)
                       MCIS1342-TX-LGR-S  (3) ' '
                       MCIS1342-TX-CMPT-S (3) ' '
                       MCIS1342-TX-BAI-S  (3) ' '
                       TB-MUN (3) TB-TR (3) TB-UF (3) TB-BR (3)
                       '@'
                       DELIMITED BY '  ' INTO
                             TX-END-CML
               MOVE   TX-END-CML       TO  TB-END-CML-R
               PERFORM 222320-FORMATA-ENDERECO
           END-IF
           .
       222300-SAI.
           EXIT.

      *-----------------------------------------------------------------
       222310-BUSCAR-MUNICIPIO         SECTION.
      *-----------------------------------------------------------------
           MOVE  0014                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      ARGSO906-DADOS
           MOVE  ARGSO006              TO  NM-PGM

           MOVE  0001                  TO  O906-OPCAO
           MOVE   MCIS1342-CD-MUN-S (IND)
                                       TO  O906-CD-MUN
      *  Buscar nome municipio e UF.
           PERFORM 910000-CHAMAR-SERVICO

           IF  O906-COD-RETORNO            EQUAL  ZEROS
              MOVE  O906-NM-MUN        TO  TB-MUN (IND)
              MOVE  O906-SG-UF         TO  TB-UF  (IND)
              MOVE  '-'                TO  TB-TR  (IND)
              MOVE  '/'                TO  TB-BR  (IND)
           END-IF

           .
       222310-SAI.
           EXIT.
      *-----------------------------------------------------------------
       222320-FORMATA-ENDERECO         SECTION.
      *-----------------------------------------------------------------
           MOVE  0015                  TO  MSG1-CD-SEQL-ERRO

      *
           MOVE  ZEROS                 TO  SGTS0142-QT-END-CML

           PERFORM
                  VARYING  IND FROM 1 BY 1
                  UNTIL    IND > 498
                     OR    TB-END-LS (IND) EQUAL  '@'

           ADD   0001                  TO  SGTS0142-QT-END-CML
               ON SIZE ERROR
               MOVE 'Erro no cálculo do tamanho do endereço comercial
      -               '.'              TO  MSG1-TX-ERRO-TS
               SET ERRO-PROGRAMADO     TO  TRUE
               SET MNT-VRV             TO  TRUE
               PERFORM 999000-ERRO
           END-ADD

           MOVE  TB-END-LS (IND)       TO  SGTS0142-TX-END-CML (IND)

           END-PERFORM
           .
       222320-SAI.
           EXIT.

      *-----------------------------------------------------------------
       899000-MOVIMENTAR-SAIDA         SECTION.
      *-----------------------------------------------------------------
           MOVE  0016                  TO  MSG1-CD-SEQL-ERRO

      *    Gravando o container de resposta da subrotina
           PERFORM 980000-GRAVAR-CONTAINER-RPST

           .
       899000-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *  Section da Faixa de 900000 à 969900 para Demais Rotinas
      *-----------------------------------------------------------------

      *----------------------------------------------------------------
       910000-CHAMAR-SERVICO           SECTION.
      *--> Acionar programa IIBP2000 para requisição do serviço.
      *----------------------------------------------------------------

      *--> Grava o container com dados do serviço/operação a chamar
           PERFORM 981000-GRAVAR-CONTAINER-SRVC

      *--> Grava o container de requisicao do modulo a executar
           PERFORM 982000-GRAVAR-CONTAINER-RQSC

      *--> Limpa a resposta e erro antes de executar o modulo
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO

           MOVE  0017                  TO  MSG1-CD-SEQL-ERRO

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS LINK
                     PROGRAM   ( IIBP2000     )
                     CHANNEL   ( NM-CNL       )
                     RESP      ( MSG1-RESP1   )
                     RESP2     ( MSG1-RESP2   )
           END-EXEC

           EVALUATE  TRUE
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(NORMAL)
                     CONTINUE
               WHEN  OTHER
                     SET   ERRO-CICS   TO  TRUE
                     MOVE  9000        TO  MSG1-CD-ERRO-TS
                     STRING  'Erro ao acionar serviço: '
                             SRVC-ID-SRVC '.' SRVC-VRS-SRVC '.'
                             SRVC-ID-OPR  '.' SRVC-VRS-OPR
                             'via IIBP2000, '
                             'RESP='       MSG1-RESP1
                             ', RESP-2='   MSG1-RESP2  '.'
                             DELIMITED BY SIZE
                             INTO          MSG1-TX-ERRO-TS
                     PERFORM  999000-ERRO
           END-EVALUATE

      *--> Efetua a leitura do Container de Erro
           PERFORM 972000-LER-CONTAINER-DE-ERRO

      *--> Efetua a leitura do Container de Resposta
           PERFORM 973000-LER-RESPOSTA

           .
       910000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       920000-CHAMAR-COMMAREA          SECTION.
      *-----------------------------------------------------------------
           MOVE  0018                  TO  MSG1-CD-SEQL-ERRO

           EVALUATE  NM-PGM
               WHEN  ACES2100
                     EXEC CICS  LINK
                                PROGRAM  ( ACES2100                 )
                                COMMAREA ( ACES2100-DADOS           )
                                LENGTH   ( LENGTH OF ACES2100-DADOS )
                     END-EXEC
               WHEN  OTHER
                     CONTINUE
           END-EVALUATE
           .
       920000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       969000-MONITORAR-VARIAVEIS      SECTION.
      *--> Monitora as variáveis dos módulos chamados
      *-----------------------------------------------------------------

           EVALUATE  NM-PGM
               WHEN  ACES2100
                    MOVE 'K2100-CD-FUC'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  K2100-CD-FUC
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'K2100-CD-USU'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  K2100-CD-USU
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN  ARHP0883
                    MOVE 'LKS-CD-MCI'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  LKS-CD-MCI
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'IIBKERRO-TX-ERRO'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  IIBKERRO-TX-ERRO
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'IIBKERRO-CD-ERRO'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  IIBKERRO-CD-ERRO
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN  ARHS007V
                    MOVE 'ARHS007V-CD-TIP-CMSS-FUC'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  ARHS007V-CD-TIP-CMSS-FUC
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'IIBKERRO-TX-ERRO'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  IIBKERRO-TX-ERRO
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'IIBKERRO-CD-ERRO'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  IIBKERRO-CD-ERRO
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN  ARGSO006
                    MOVE 'O906-OPCAO'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  O906-OPCAO
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'O906-CD-MUN '
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  O906-CD-MUN
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'IIBKERRO-TX-ERRO'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  IIBKERRO-TX-ERRO
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'IIBKERRO-CD-ERRO'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  IIBKERRO-CD-ERRO
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN  MSTS8041
                    MOVE 'MSTS8041-CD-DEPE-UOR-RQSC'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  MSTS8041-CD-DEPE-UOR-RQSC
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'MSTS8041-NR-ORD-SBDD-RQSC'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  MSTS8041-NR-ORD-SBDD-RQSC
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'IIBKERRO-TX-ERRO'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  IIBKERRO-TX-ERRO
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'IIBKERRO-CD-ERRO'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  IIBKERRO-CD-ERRO
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN  MCIS0001
                    MOVE 'MCIS0001-CLIENTE'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  MCIS0001-CLIENTE
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'IIBKERRO-TX-ERRO'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  IIBKERRO-TX-ERRO
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'IIBKERRO-CD-ERRO'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  IIBKERRO-CD-ERRO
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN  MCIS1263
                    MOVE 'MCIS1263-SYS-CHAMADOR'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  MCIS1263-SYS-CHAMADOR
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'MCIS1263-PRG-CHAMADOR'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  MCIS1263-PRG-CHAMADOR
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'MCIS1263-FUNCAO'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  MCIS1263-FUNCAO
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'MCIS1263-USUARIO'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  MCIS1263-USUARIO
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'MCIS1263-CD-CLI'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  MCIS1263-CD-CLI
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'IIBKERRO-TX-ERRO'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  IIBKERRO-TX-ERRO
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'IIBKERRO-CD-ERRO'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE IIBKERRO-CD-ERRO
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN  MCIS1340
                    MOVE 'MCIS1340-SYS-CHAMADOR'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  MCIS1340-SYS-CHAMADOR
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'MCIS1340-PRG-CHAMADOR'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  MCIS1340-PRG-CHAMADOR
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'MCIS1340-FUNCAO'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  MCIS1340-FUNCAO
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'MCIS1340-COD-MCI'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  MCIS1340-COD-MCI
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'MCIS1340-CD-CLI'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  MCIS1340-CD-CLI
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'LKS-CD-MCI'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  LKS-CD-MCI
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'IIBKERRO-TX-ERRO'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  IIBKERRO-TX-ERRO
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'IIBKERRO-CD-ERRO'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  IIBKERRO-CD-ERRO
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN  MCIS1342
                    MOVE 'MCIS1342-SYS-CHAMADOR'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  MCIS1342-SYS-CHAMADOR
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'MCIS1342-PRG-CHAMADOR'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  MCIS1342-PRG-CHAMADOR
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'MCIS1342-FUNCAO'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  MCIS1342-FUNCAO
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'MCIS1342-COD-MCI'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  MCIS1342-COD-MCI
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'MCIS1342-CD-CLI'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  MCIS1342-CD-CLI
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'MCIS1342-CD-TIP-END'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  MCIS1342-CD-TIP-END
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'LKS-CD-MCI'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  LKS-CD-MCI
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'IIBKERRO-TX-ERRO'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  IIBKERRO-TX-ERRO
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'IIBKERRO-CD-ERRO'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  IIBKERRO-CD-ERRO
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN  OTHER
                    CONTINUE
           END-EVALUATE

           .
       969000-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *    Section da Faixa de 970000 a 979900 para Leitura de Dados
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       970000-LER-CANAL                SECTION.
      *--> Obtem o canal corrente
      *-----------------------------------------------------------------
           MOVE  0019                  TO  MSG1-CD-SEQL-ERRO

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CANAL            TO  TRUE

           EXEC CICS ASSIGN
                     CHANNEL ( NM-CNL )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       970000-FIM.
           EXIT.

      *-----------------------------------------------------------------
       971000-LER-REQUISICAO           SECTION.
      *-----------------------------------------------------------------
           MOVE  0020                  TO  MSG1-CD-SEQL-ERRO

      *--> Monta o nome do container de requisicao
           MOVE  'SGTS0142'            TO  NM-PGM
           MOVE  'SGTS0142-RQSC'       TO  NM-CTNR

      *--> Recupera o tamanho do container de requisição
           PERFORM 973100-LER-CONTAINER-NODATA

      *--> Obtem o tamanho esperado da requisicao
           MOVE  LENGTH OF SGTS0142-RQSC
                                       TO  QT-CCT-EPRO

      *--> Valida tamanho esperado da requisicao X tamanho recebido
           IF  QT-CCT-LIDO                 NOT EQUAL  QT-CCT-EPRO
               SET   ERRO-TAMANHO-AREA
                                       TO  TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           MOVE  0021                  TO  MSG1-CD-SEQL-ERRO

      *--> Configura dados para leitura do container de requisição
           SET   GET-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *--> Efetua a leitura do container
           EXEC CICS  GET
                      CONTAINER ( NM-CTNR       )
                      FLENGTH   ( QT-CCT-LIDO   )
                      INTO      ( SGTS0142-RQSC )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE

           .
       971000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       972000-LER-CONTAINER-DE-ERRO    SECTION.
      *--> A presença do container PRM-ERRO indica que houve erro de
      *    integração, a ausência do container indica sucesso.
      *-----------------------------------------------------------------
           MOVE  0022                  TO  MSG1-CD-SEQL-ERRO

      *--> Le o container de erro dos modulos chamados
           SET   GET-CONTAINER         TO  TRUE
           MOVE  'PRM-ERRO'            TO  NM-CTNR
           INITIALIZE                      IIBKERRO-PRM-ERRO

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS GET
                     CONTAINER ( NM-CTNR                     )
                     INTO      ( IIBKERRO-PRM-ERRO           )
                     FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                     RESP      ( MSG1-RESP1                  )
                     RESP2     ( MSG1-RESP2                  )
           END-EXEC

           EVALUATE  TRUE
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(CONTAINERERR)
                     CONTINUE
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(NORMAL)
                     SET   ERRO-PROGRAMA-ACIONADO
                                        TO  TRUE
                     MOVE  NM-PGM       TO  MSG1-NM-PGM-ACND
                     IF  NM-PGM(1:3)        EQUAL  'SGT'
                         MOVE  IIBKERRO-CD-ERRO
                                       TO  MSG1-CD-RTN-PGM-ACND
                                           MSG1-CD-ERRO-TS
                         MOVE  IIBKERRO-TX-ERRO
                                       TO  MSG1-TX-RTN-PGM-ACND
                                           MSG1-TX-ERRO-TS
                     ELSE
                         MOVE  3004     TO  MSG1-CD-ERRO-TS
                         STRING  'Erro ao acionar o módulo ' NM-PGM '.'
                                 DELIMITED BY SIZE
                                 INTO       MSG1-TX-ERRO-TS
                         MOVE  IIBKERRO-TX-ERRO
                                       TO  MSG1-TX-RTN-PGM-ACND
                         MOVE  IIBKERRO-CD-ERRO
                                       TO  MSG1-CD-RTN-PGM-ACND
                     END-IF
               WHEN  OTHER
                     SET ERRO-CONTAINER
                                       TO  TRUE
                     PERFORM  999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       972000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       973000-LER-RESPOSTA             SECTION.
      *--> Recupera o container com os dados de resposta (saída).
      *    Modulo CONTAINER  -     NOME_PGM-RPST
      *    Modulo IIB (FIXO) -     PRM-DADOS-RPST
      *-----------------------------------------------------------------
           MOVE  0023                  TO  MSG1-CD-SEQL-ERRO

      *--> Monta o nome dos container de acordo com modulo a executar
           MOVE  SPACES                TO  NM-CTNR

      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *    nome_modulo -RPST
           EVALUATE  NM-PGM
               WHEN  'ARHP0883'
               WHEN  'MSTS8041'
               WHEN  'ARHS007V'
               WHEN  'ARGSO006'
               WHEN  'MCIS0001'
               WHEN  'MCIS1340'
               WHEN  'MCIS1342'
               WHEN  'MCIS0024'
               WHEN  'MCIS1263'
                    MOVE  'PRM-DADOS-RPST'
                                       TO  NM-CTNR

               WHEN  OTHER
                    MOVE  NM-PGM       TO  NM-CTNR(1:8)
                    MOVE  '-RPST'      TO  NM-CTNR(9:5)
           END-EVALUATE

      *--> Recupera o tamanho do container de resposta
           PERFORM 973100-LER-CONTAINER-NODATA

      *--> Configura dados para leitura do container de resposta
           SET   GET-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE


      *--> Le a RESPOSTA de acordo com o programa que a gravou

           MOVE  0024                  TO  MSG1-CD-SEQL-ERRO

           EVALUATE  NM-PGM

      *        Calcular a quantidade de caracteres esperado e limpar
      *        a area de dados antes de executar o GET
      *        Para matrizes com dados variaveis (clausula depending
      *        on) nao ha como utilizar o comando INITIALIZE

               WHEN  ARHP0883
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (ARHP0883-DADOS   )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN  MSTS8041
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (MSTS8041-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN  ARHS007V
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (ARHS007V-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN  ARGSO006
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (ARGSO906-DADOS   )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC


               WHEN  MCIS0001
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (MCIS0001-DADOS   )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN  MCIS0024
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (MCIS0024-DADOS   )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN  MCIS1340
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (MCIS1340-DADOS   )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN  MCIS1342
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (MCIS1342-DADOS   )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN  MCIS1263
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (MCIS1263-DADOS   )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN  OTHER
                     SET   ERRO-PROGRAMADO
                                       TO  TRUE
                     MOVE  9003         TO  MSG1-CD-ERRO-TS
                     STRING  'Erro na leitura da resposta do módulo.'
                             NM-PGM '.'
                             DELIMITED BY SIZE
                             INTO           MSG1-TX-ERRO-TS
                     PERFORM  999000-ERRO
           END-EVALUATE

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE

           .
       973000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       973100-LER-CONTAINER-NODATA     SECTION.
      *-----------------------------------------------------------------

      *--> Configura dados para leitura do container de requisição
           SET   GET-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *    Obtem a quantidade de caracteres no container
           MOVE  ZEROS                 TO  QT-CCT-LIDO
           EXEC CICS GET
                     CONTAINER (NM-CTNR)
                     NODATA
                     FLENGTH   (QT-CCT-LIDO)
           END-EXEC

      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       973100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       974000-LER-DADOS-OPRD           SECTION.
      *-----------------------------------------------------------------
           MOVE  0025                  TO  MSG1-CD-SEQL-ERRO

      *--> Configura dados para leitura do container de requisição
           SET   GET-CONTAINER         TO  TRUE
           MOVE  'PRM-INF-OPRD'        TO  NM-CTNR

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

           INITIALIZE                      PRM-INF-OPRD
           EXEC CICS GET
                CONTAINER ( NM-CTNR                )
                INTO      ( PRM-INF-OPRD           )
                FLENGTH   ( LENGTH OF PRM-INF-OPRD )
           END-EXEC.

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE

           .
       974000-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *   Section da Faixa de 980000 a 989900 para Gravacao de Dados
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       980000-GRAVAR-CONTAINER-RPST    SECTION.
      *-----------------------------------------------------------------

      *--> Exclui eventual container de erro de outros programas
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO

           MOVE  0026                  TO  MSG1-CD-SEQL-ERRO

      *--> Monta o nome do container de resposta
           MOVE  'SGTS0142-RPST'       TO  NM-CTNR
           SET   PUT-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

           EXEC CICS PUT
                     CONTAINER (NM-CTNR                    )
                     FROM      (SGTS0142-RPST          )
                     FLENGTH   (LENGTH OF SGTS0142-RPST)
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       980000-SAI.
           EXIT.

      *----------------------------------------------------------------
       981000-GRAVAR-CONTAINER-SRVC    SECTION.
      *--> Postar container de requisição de serviço.
      *----------------------------------------------------------------
           MOVE  0027                  TO  MSG1-CD-SEQL-ERRO

      *--> Monta o nome do container de requisicao do servico
           MOVE  'PRM-RQSC-SRVC'       TO  NM-CTNR
           SET PUT-CONTAINER           TO  TRUE

           EVALUATE  NM-PGM

               WHEN  ARGSO006
                    MOVE  22746        TO  SRVC-ID-SRVC
                    MOVE  0001         TO  SRVC-VRS-SRVC
                    MOVE  1151329      TO  SRVC-ID-OPR
                    MOVE  0001         TO  SRVC-VRS-OPR

               WHEN  ARHP0883
                    MOVE  70663        TO  SRVC-ID-SRVC
                    MOVE  0001         TO  SRVC-VRS-SRVC
                    MOVE  258428       TO  SRVC-ID-OPR
                    MOVE  0001         TO  SRVC-VRS-OPR

               WHEN  ARHS007V
                    MOVE  837553       TO  SRVC-ID-SRVC
                    MOVE  0001         TO  SRVC-VRS-SRVC
                    MOVE  1008844      TO  SRVC-ID-OPR
                    MOVE  0001         TO  SRVC-VRS-OPR

               WHEN  MSTS8041
                    MOVE  ZEROS        TO  SRVC-ID-SRVC
                    MOVE  ZEROS        TO  SRVC-VRS-SRVC
                    MOVE  1963705      TO  SRVC-ID-OPR
                    MOVE  0001         TO  SRVC-VRS-OPR

               WHEN  MCIS0001
                    MOVE  19485        TO  SRVC-ID-SRVC
                    MOVE  0001         TO  SRVC-VRS-SRVC
                    MOVE  207827       TO  SRVC-ID-OPR
                    MOVE  0001         TO  SRVC-VRS-OPR

               WHEN  MCIS0024
                    MOVE  19485        TO  SRVC-ID-SRVC
                    MOVE  0001         TO  SRVC-VRS-SRVC
                    MOVE  747809       TO  SRVC-ID-OPR
                    MOVE  0001         TO  SRVC-VRS-OPR

               WHEN  MCIS1340
                    MOVE  19485        TO  SRVC-ID-SRVC
                    MOVE  0001         TO  SRVC-VRS-SRVC
                    MOVE  63195        TO  SRVC-ID-OPR
                    MOVE  0001         TO  SRVC-VRS-OPR

               WHEN  MCIS1342
                    MOVE  19485        TO  SRVC-ID-SRVC
                    MOVE  0001         TO  SRVC-VRS-SRVC
                    MOVE  63024        TO  SRVC-ID-OPR
                    MOVE  0001         TO  SRVC-VRS-OPR

               WHEN  MCIS1263
                    MOVE  19485        TO  SRVC-ID-SRVC
                    MOVE  0001         TO  SRVC-VRS-SRVC
                    MOVE  1257116      TO  SRVC-ID-OPR
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
      *    Modulo CONTAINER  -     NOME_PGM-RQSC
      *    Modulo IIB (FIXO) -     PRM-DADOS-RQSC
      *-----------------------------------------------------------------
           MOVE  0028                  TO  MSG1-CD-SEQL-ERRO

      *--> Monta o nome dos container de acordo com modulo a executar
           MOVE  SPACES                TO  NM-CTNR
           SET   PUT-CONTAINER         TO  TRUE

      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *    nome_modulo -RPST
           EVALUATE  NM-PGM
               WHEN  'ARHP0883'
               WHEN  'ARGSO006'
               WHEN  'ARHS007V'
               WHEN  'MSTS8041'
               WHEN  'MCIS0001'
               WHEN  'MCIS0024'
               WHEN  'MCIS1340'
               WHEN  'MCIS1342'
               WHEN  'MCIS1263'
                     MOVE 'PRM-DADOS-RQSC'
                                       TO  NM-CTNR
               WHEN  OTHER
                     MOVE  NM-PGM      TO  NM-CTNR(1:8)
                     MOVE  '-RQSC'     TO  NM-CTNR(9:5)
           END-EVALUATE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *--> Faz PUT da requisicao do programa a executar
           EVALUATE  NM-PGM
               WHEN  ARHP0883
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (ARHP0883-DADOS )
                              FLENGTH   (LENGTH OF ARHP0883-DADOS )
                    END-EXEC

               WHEN  ARGSO006
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (ARGSO906-DADOS    )
                              FLENGTH   (LENGTH OF ARGSO906-DADOS    )
                    END-EXEC

               WHEN  ARHS007V
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (ARHS007V-RQSC )
                              FLENGTH   (LENGTH OF ARHS007V-RQSC )
                    END-EXEC

               WHEN  MSTS8041
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (MSTS8041-RQSC )
                              FLENGTH   (LENGTH OF MSTS8041-RQSC )
                    END-EXEC

               WHEN  MCIS0001
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (MCIS0001-DADOS )
                              FLENGTH   (LENGTH OF MCIS0001-DADOS )
                    END-EXEC

               WHEN  MCIS0024
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (MCIS0024-DADOS )
                              FLENGTH   (LENGTH OF MCIS0024-DADOS )
                    END-EXEC

               WHEN  MCIS1340
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (MCIS1340-DADOS )
                              FLENGTH   (LENGTH OF MCIS1340-DADOS )
                    END-EXEC

               WHEN  MCIS1342
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (MCIS1342-DADOS )
                              FLENGTH   (LENGTH OF MCIS1342-DADOS )
                    END-EXEC

               WHEN  MCIS1263
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (MCIS1263-DADOS )
                              FLENGTH   (LENGTH OF MCIS1263-DADOS )
                    END-EXEC

               WHEN  OTHER
                     SET   ERRO-PROGRAMADO
                                       TO  TRUE
                     MOVE  9004        TO  MSG1-CD-ERRO-TS
                     STRING  'Erro na gravação da requisição do módulo '
                             NM-PGM '.'
                             DELIMITED BY SIZE
                             INTO          MSG1-TX-ERRO-TS
                     PERFORM  999000-ERRO
           END-EVALUATE

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE

           .
       982000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       983000-EXCLUIR-RESPOSTA-ERRO    SECTION.
      *--> Exclui o container de resposta e de erro para evitar
      *    problemas em modulos executados mais de uma vez
      *    dentro da mesma sessao do CICS.
      *    Equivale ao INITIALIZE na area de resposta dos programas
      *    que utilizam COMMAREA.
      *-----------------------------------------------------------------
           MOVE  0029                  TO  MSG1-CD-SEQL-ERRO

      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *    nome_modulo -RPST
           EVALUATE  NM-PGM
               WHEN  'ARHP0883'
               WHEN  'ARHS007V'
               WHEN  'MSTS8041'
               WHEN  'MCIS0001'
               WHEN  'MCIS0024'
               WHEN  'MCIS1340'
               WHEN  'MCIS1263'
      *        WHEN 'MODULO-VIA-IIB' --> incluir todos via IIB
                    MOVE 'PRM-DADOS-RPST'
                                       TO  NM-CTNR
               WHEN  OTHER
                    MOVE  NM-PGM       TO  NM-CTNR(1:8)
                    MOVE  '-RPST'      TO  NM-CTNR(9:5)
           END-EVALUATE

           SET  DEL-CONTAINER          TO  TRUE

      *--> Exclui o container de resposta
      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS DELETE
                     CONTAINER ( NM-CTNR )
                     RESP      ( MSG1-RESP1 )
                     RESP2     ( MSG1-RESP2 )
           END-EXEC

           EVALUATE  TRUE
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(CONTAINERERR)
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(NORMAL)
                    CONTINUE
               WHEN  OTHER
                    SET  ERRO-CONTAINER
                                       TO  TRUE
                    PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE

      *--> Excluir o container de erro
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO
           .
       983000-SAI.
           EXIT.
      *
      *-----------------------------------------------------------------
       983100-EXCLUIR-CONTAINER-ERRO   SECTION.
      *-----------------------------------------------------------------

      *--> Limpar variaveis container de erro
           INITIALIZE                      IIBKERRO-PRM-ERRO

      *--> Exclui o container de erro
           MOVE  0030                  TO  MSG1-CD-SEQL-ERRO
           MOVE  'PRM-ERRO'            TO  NM-CTNR
           SET   DEL-CONTAINER         TO  TRUE

      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS DELETE
                     CONTAINER (NM-CTNR)
                     RESP      (MSG1-RESP1)
                     RESP2     (MSG1-RESP2)
           END-EXEC

           EVALUATE  TRUE
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(CONTAINERERR)
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(NORMAL)
                    CONTINUE
               WHEN  OTHER
                    SET   ERRO-CONTAINER
                                       TO  TRUE
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
           EXEC CICS  PUT
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
      * As sections abaixo iniciadas com 999000 representam os tratamen
      * tos de erro utilizando o Sistema MSG (rotina MSGS0001)
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       999000-ERRO                     SECTION.
      *-----------------------------------------------------------------

           IF  MSG1-CD-ERRO-TS             EQUAL  ZEROS
               MOVE  8888              TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS             EQUAL  SPACES
               MOVE  'Erro não especificado no módulo.'
                                       TO  MSG1-TX-ERRO-TS
           END-IF

           PERFORM  999000-GRAVA-ERRO-E-ENCERRA
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
                    PERFORM 999004-TRATA-ERRO-CTNR
               WHEN  ERRO-TAMANHO-AREA
                    PERFORM 999005-TRATA-ERRO-AREA-DADOS
               WHEN  ERRO-CANAL
                    PERFORM 999006-TRATA-ERRO-CANAL
               WHEN  OTHER
                    CONTINUE
           END-EVALUATE

      *--> Adiciona as variáveis de entrada para monitoração
           MOVE 'SGTS0142-CD-CLI'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE SGTS0142-CD-CLI
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT


      *--> Grava o erro gerado no sistema MSG - Mensageria
           PERFORM  MSG1-NTF-MSG

           MOVE  MSG1-CD-ERRO-TS       TO  IIBKERRO-CD-ERRO
           MOVE  MSG1-TX-ERRO-TS       TO  IIBKERRO-TX-ERRO

      *--> Grava o erro gerado no container de erro
           PERFORM  984000-GRAVAR-CONTAINER-ERRO

      *--> Encerra a transacao
           PERFORM  999999-ENCERRAR
           .
       999000-SAI.
           EXIT.

      *----------------------------------------
      * 999001-TRATA-ABEND-CICS        SECTION.
      * 999002-TRATA-CONDITION-CICS    SECTION.
      * 999003-ADICIONA-VRV-MNT        SECTION.
      * 999004-ALINHA-NUMERO-ESQUERDA  SECTION.
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
      *---------------------- FIM SGTS0142 -----------------------------