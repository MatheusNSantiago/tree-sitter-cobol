      *-----------------------------------------------------------------
      * Programa..: SGTS0079
      * Analista..: F2054109 - Cintia Ap Soares Iglesias
      * Autor.....: F8785286 - Roseli Akemi Kawahata
      * Data......: 24/10/2016
      * Objetivo..: Negocial Listar Pessoas de Contato Interno da
      *             Operação Estruturada
      * Compilacao: COBOL/CICS
      *-----------------------------------------------------------------
      * VRS001 06.11.2018 - F9158937 Acerto formatação telefones cliente
      *        31.10.2017 - F8785286 Trocar book de MSGKW999 p/ HLPKW999
      *        24.10.2016 - F8785286 DE 2015/137233 - IMPLANTACAO
      * VSR002 06.12.2018 - F0950413 DE 2015/137233 - Alteração  chama-
      *                   da ACES2100 pelas operaçoes 291261 para buscar
      *                   os telefones comerciais e celulares, e 292124
      *                   para buscar o e-mail do funcionário.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTS0079.

      *-----------------------------------------------------------------
       ENVIRONMENT                     DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       CONFIGURATION                   SECTION.
      *-----------------------------------------------------------------
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *-----------------------------------------------------------------
       DATA                            DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       WORKING-STORAGE                 SECTION.
      *-----------------------------------------------------------------
       77  CTE-INICIO-SS               PIC  X(40) VALUE
                                       '*** S.S. Comeca Aqui ***'.
       77  CTE-PROG                    PIC  X(018)  VALUE
                                       '*** SGTS0079 ***'.
       77  CTE-VERS                    PIC  X(006)  VALUE 'VRS002'.

       77  IDX-MCIS134                   PIC  S9(4) COMP-5 VALUE 0.

      *--> Definição dos Nomes dos Programas Acionados
       01  TAB-PROGRAMAS-CHAMADOS.
           03  IIBP2000                PIC  X(08) VALUE 'IIBP2000'.
           03  ATCS6000                PIC  X(08) VALUE 'ATCS6000'.
           03  ARHP0883                PIC  X(08) VALUE 'ARHP0883'.
           03  ARHPI40D                PIC  X(08) VALUE 'ARHPI40D'.
           03  MSTS8078                PIC  X(08) VALUE 'MSTS8078'.
           03  MCISI082                PIC  X(08) VALUE 'MCISI082'.
           03  MCIS1340                PIC  X(08) VALUE 'MCIS1340'.
           03  MCIS1263                PIC  X(08) VALUE 'MCIS1263'.
           03  MCIS0024                PIC  X(08) VALUE 'MCIS0024'.
           03  SGTSLA06                PIC  X(08) VALUE 'SGTSLA06'.

      *--> Definição dos Variaveis do Cobolito
-INC ALMKCBL1

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

      *--> Areas de Trabalho
       01  IC-TAB                      PIC S9(004) COMP-5 VALUE 0.
       01  IDX-FUC                     PIC S9(004) COMP-5 VALUE 0.
       01  IDX-CTT                     PIC S9(004) COMP-5 VALUE 0.
       01  IDX                         PIC S9(004) COMP-5 VALUE 0.
       01  IDX-CML                     PIC S9(004) COMP-5 VALUE 0.
       01  IDX-CEL                     PIC S9(004) COMP-5 VALUE 0.
       01  CT-REG                      PIC S9(004) COMP-5 VALUE 0.
       01  SAI-PESQUISA                PIC  X(001) VALUE SPACES.
       01  CD-LOT-FUC                  PIC  9(004) VALUE 0.

       01  IDX-TRBD                    PIC S9(004) COMP-5 VALUE 0.
       01  TB-FONES-TRBD.
           03 TB-DDD-NR-TRBD OCCURS 3 TIMES.
              05  TB-CD-ABRE-DDD-TRBD  PIC  X(001).
              05  TB-CD-DDD-TRBD       PIC  9(004).
              05  TB-CD-FECHA-DDD-TRBD PIC  X(001).
              05  TB-NR-TRBD           PIC  9(009).
              05  TB-CD-BARRA-TRBD     PIC  X(001).

       01  IDX-TCEL                    PIC S9(004) COMP-5 VALUE 0.
       01  TB-TCEL.
           03 TB-DDD-NR-TCEL OCCURS 3 TIMES.
              05  TB-CD-ABRE-DDD-TCEL  PIC  X(001).
              05  TB-CD-DDD-TCEL       PIC  9(004).
              05  TB-CD-FECHA-DDD-TCEL PIC  X(001).
              05  TB-NR-TCEL           PIC  9(009).
              05  TB-CD-BARRA-TCEL     PIC  X(001).

       01  TB-FUC-QT-OCR               PIC S9(004) COMP-5 VALUE 0.

       01  TB-FUC.
           03  TB-FUC-OCR              OCCURS 0100 TIMES.
               05  TB-FUC-CD-CLI       PIC  9(010) VALUE 0.
               05  TB-FUC-CD-MATR      PIC  9(009) VALUE 0.
               05  TB-FUC-TX-NOME      PIC  X(050) VALUE SPACES.
               05  TB-FUC-CD-LOT       PIC  9(005) VALUE 0.

       01  CD-MATR-USU.
           03                          PIC  X(001) VALUE 'F'.
           03  NR-CD-MATR              PIC  9(007) VALUE 0.

       01  TB-CTT-QT-OCR               PIC S9(004) COMP-5 VALUE 0.

       01  TB-CTT.
           03  TB-CTT-OCR     OCCURS 0100 TIMES.
               05  TB-CTT-CD-CLI           PIC  9(010) VALUE 0.

      *--> Auxiliares para movimentação de telefones.
       01  AUX-NR-TEL-CML-CTT              PIC  X(50) VALUE SPACES.
       01  AUX-NR-TEL-CML-CTT-R      REDEFINES AUX-NR-TEL-CML-CTT.
           03  CML-FILL1                   PIC X(01).
           03  CML-DDD-1                   PIC 9(04).
           03  CML-FILL2                   PIC X(01).
           03  CML-1                       PIC 9(09).
           03  CML-FILL3                   PIC X(01).
           03  CML-FILL4                   PIC X(01).
           03  CML-DDD-2                   PIC 9(04).
           03  CML-FILL5                   PIC X(01).
           03  CML-2                       PIC 9(09).
           03  CML-FILL6                   PIC X(01).
           03  CML-FILL7                   PIC X(01).
           03  CML-DDD-3                   PIC 9(04).
           03  CML-FILL8                   PIC X(01).
           03  CML-3                       PIC 9(09).
           03                              PIC X(03).

       01  AUX-NR-TCEL-CTT              PIC  X(50) VALUE SPACES.
       01  AUX-NR-TCEL-CTT-R      REDEFINES AUX-NR-TCEL-CTT.
           03  CEL-FILL1                   PIC X(01).
           03  CEL-DDD-1                   PIC 9(04).
           03  CEL-FILL2                   PIC X(01).
           03  CEL-1                       PIC 9(09).
           03  CEL-FILL3                   PIC X(01).
           03  CEL-FILL4                   PIC X(01).
           03  CEL-DDD-2                   PIC 9(04).
           03  CEL-FILL5                   PIC X(01).
           03  CEL-2                       PIC 9(09).
           03  CEL-FILL6                   PIC X(01).
           03  CEL-FILL7                   PIC X(01).
           03  CEL-DDD-3                   PIC 9(04).
           03  CEL-FILL8                   PIC X(01).
           03  CEL-3                       PIC 9(09).
           03                              PIC X(03).

      *--> Auxiliares para movimentação na monitoração de variáveis.
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).

       01  WS-RESP-CICS.
           03  WS-RESP                 PIC  9(08)       VALUE 0.
           03  WS-RESP2                PIC  9(08)       VALUE 0.

      *-----------------------------------------------------------------
      * Book dos Containers e das subrotinas chamadas
      *-----------------------------------------------------------------

      *--> Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1

      *--> Book para captura de abend cics da bloco codigo HLPKW999
-INC HLPKW999

      *--> Book para captura de erro via MSG1
-INC MSGKWMG1

      *--> Book do container de requisição e resposta deste modulo
       01 SGTS0079-DADOS.
-INC SGTK0079

      *--> Book SGT - Listar cliente
       01  SGTSLA06-DADOS.
-INC SGTKLA06

      *--> Consulta dados do usuário - via IIB
       01  ATCS6000-DADOS.
-INC ATCK6000                                                           00000870

      *--> Book ARH - Consultar Funcionário BB a partir MCI - via IIB
       01  ARHP0883-DADOS.
           03 K0883-ENTRADA.
              05 LKS-CD-MCI            PIC 9(010).
           03 K0883-SAIDA.
              05  LKS-CD-MATR          PIC  9(009) OCCURS 10 TIMES.
              05  LKS-TX-NOME          PIC  X(050) OCCURS 10 TIMES.
              05  LKS-CD-SIT           PIC  9(003) OCCURS 10 TIMES.
              05  LKS-CD-LOCAL         PIC  9(004) OCCURS 10 TIMES.
              05  LKS-CD-CMSS          PIC  9(005) OCCURS 10 TIMES.
              05  LKS-CD-LOT           PIC  9(004) OCCURS 10 TIMES.
              05  LKS-CD-FUN-LOT       PIC  9(005) OCCURS 10 TIMES.
              05  LKS-CD-AG            PIC  9(004) OCCURS 10 TIMES.
              05  LKS-CD-CTA           PIC  9(008) OCCURS 10 TIMES.
              05  LKS-QTDE-REG-RTND    PIC S9(008) COMP.
              05  LKS-CD-RTN           PIC  9(004).
              05  LKS-POS-ERRO-RTN     PIC  9(002).
              05  LKS-ERRO-RTN         PIC  X(030).

      *--> Book ARH - Validar Comissão de Funcionário - via IIB
-INC ARHKI40D

      *--> Book MST - Consultar Unidade Organizacional - via IIB
-INC MSTK8078

      *--> Book MCI - Consultar dados básicos do cliente - via IIB
-INC MCIKI082

      *--> Book MCI - Consultar telefone - via IIB
       01  MCIS1340-DADOS.
-INC MCIB1340

      *--> Book MCI - Listar emails ativos do cliente por código do
      *-->            cliente - via IIB
       01  MCIS1263-DADOS.
-INC MCIB1263

      *--> Book MCI - Consultar Ocupação, Cônjuge(Companheiro) e Dados
      *-->            Pessoais de Cliente Pessoa Física - via IIB
       01  MCIS0024-DADOS.
-INC MCIB0024

      *--> Book do Container de Erro
-INC IIBKERRO

      *--> Book do container de Serviços (para acionamento de serviços)
-INC IIBKSRVC

      *--> Book dos dados do usuário
-INC IIBKOPRD

       01  CTE-FINAL-SS                  PIC X(40)      VALUE
                    '*** S.S. TERMINA AQUI ***'.

      *-----------------------------------------------------------------
       PROCEDURE                       DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------

      *--> Habilita Testes no Cobolito
-INC ALMKCBL2

      *--> Habilita captura automatica de Abend e Erros CICS
-INC HLPKE999

           PERFORM  100000-INICIAR

           PERFORM  200000-PROCESSAR

           PERFORM  999999-ENCERRAR

           GOBACK
           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-INICIAR                  SECTION.
      *-----------------------------------------------------------------
           MOVE  0001                  TO  MSG1-CD-SEQL-ERRO

           PERFORM  970000-LER-CANAL

           MOVE  'SGTS0079'            TO  NM-PGM

           PERFORM  971000-LER-REQUISICAO

           PERFORM  974000-LER-DADOS-OPRD

           PERFORM  110000-VALIDAR-REQUISICAO
           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-VALIDAR-REQUISICAO       SECTION.
      *-----------------------------------------------------------------
      *--> Valida campo a campo da requisicao
      *-----------------------------------------------------------------
           MOVE  0002                  TO  MSG1-CD-SEQL-ERRO

      *--> Validar Chave da Operação Estruturada
           IF  SGTS0079-CD-CTRA-TVM        LESS  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0001              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' inválido.'      TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0079-CD-CTRA-TVM        EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0002              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0079-CD-OPR-ETRD-MCP    NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0003              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada mercado de capitais nã
      -              'o numérico.'     TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0079-CD-OPR-ETRD-MCP    < ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0004              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada mercado de capitais in
      -              'válido.'         TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0079-CD-OPR-ETRD-MCP    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0005              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada mercado de capitais nã
      -              'o informado.'    TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0079-NR-VRS-OPR-ETRD    < ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0006              TO  MSG1-CD-ERRO-TS
               MOVE 'Número da versão da operação estruturada inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0079-NR-VRS-OPR-ETRD    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0007              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação estruturada não infor
      -              'mado.'           TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0079-CD-CLI             < ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0188              TO  MSG1-CD-ERRO-TS
               MOVE  'Código do cliente inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF
           .
       110000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-PROCESSAR                SECTION.
      *-----------------------------------------------------------------

      *--> SGTSLA06 - Listar Pessoas de Contato Interno
           PERFORM  210000-LISTAR-CONTATO-INTERNO

           PERFORM  VARYING SGTSLA06-IC-TAB  FROM  1 BY 1
                      UNTIL SGTSLA06-IC-TAB  > SGTSLA06-QT-OCR-TAB
                    MOVE  SPACES       TO
                          SGTS0079-S-NM-CTT         (SGTSLA06-IC-TAB)
                          SGTS0079-S-NM-CIA         (SGTSLA06-IC-TAB)
                          SGTS0079-S-NM-CRG-CTT     (SGTSLA06-IC-TAB)
                          SGTS0079-S-NR-TEL-CML-CTT (SGTSLA06-IC-TAB)
                          SGTS0079-S-NR-TCEL-CTT    (SGTSLA06-IC-TAB)
                          SGTS0079-S-TX-EMAI-CTT    (SGTSLA06-IC-TAB)
                    MOVE  0            TO
                          SGTS0079-S-CD-CLI         (SGTSLA06-IC-TAB)
                          SGTS0079-S-CD-TIP-CLI     (SGTSLA06-IC-TAB)
           END-PERFORM

           PERFORM  220000-FUNCAO-CONTATO
                    VARYING SGTSLA06-IC-TAB  FROM  1 BY 1
                      UNTIL SGTSLA06-IC-TAB  > SGTSLA06-QT-OCR-TAB

           PERFORM  230000-MONTAR-RESPOSTA

           PERFORM  980000-GRAVAR-CONTAINER-RPST
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       210000-LISTAR-CONTATO-INTERNO   SECTION.
      *-----------------------------------------------------------------
      *--> Aciona o Módulo SGTSLA06 para buscar os Contatos Internos
      *-----------------------------------------------------------------
           MOVE  0003                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      SGTSLA06-RQSC

      *--> Movimentar o nome do programa a ser executado
           MOVE  SGTSLA06              TO  NM-PGM

      *--> Movimentar os campos de entrada
           MOVE  SGTS0079-CD-CTRA-TVM
                                       TO  SGTSLA06-CD-CTRA-TVM
           MOVE  SGTS0079-CD-OPR-ETRD-MCP
                                       TO  SGTSLA06-CD-OPR-ETRD-MCP
           MOVE  SGTS0079-NR-VRS-OPR-ETRD
                                       TO  SGTSLA06-NR-VRS-OPR-ETRD
           MOVE  SGTS0079-CD-CLI       TO  SGTSLA06-CD-CLI
      *--> Código Tipo Função Cliente = 0006 - Cliente Interno
           MOVE  0006                  TO  SGTSLA06-CD-TIP-FUC-CLI

           PERFORM  900000-EXECUTAR-MODULO

           MOVE  SGTSLA06-QT-OCR-TAB   TO  SGTS0079-QT-OCR-TAB
           MOVE  SGTSLA06-IN-CNTO-PSQ  TO  SGTS0079-IN-CNTO-PSQ
           .
       210000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       220000-FUNCAO-CONTATO           SECTION.
      *-----------------------------------------------------------------
           MOVE  0004                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE   ARHP0883-DADOS

           MOVE ARHP0883               TO  NM-PGM

           MOVE SGTSLA06-S-CD-CLI (SGTSLA06-IC-TAB)
                                       TO  LKS-CD-MCI

           PERFORM 910000-CHAMAR-SERVICO

           IF LKS-CD-RTN               EQUAL ZEROS
      *--> Tratamento para funcionário
              ADD  1                   TO TB-FUC-QT-OCR
              MOVE LKS-CD-MCI          TO TB-FUC-CD-CLI  (TB-FUC-QT-OCR)
              MOVE LKS-CD-MATR     (1) TO TB-FUC-CD-MATR (TB-FUC-QT-OCR)
              MOVE LKS-TX-NOME     (1) TO TB-FUC-TX-NOME (TB-FUC-QT-OCR)
              MOVE LKS-CD-LOT      (1) TO TB-FUC-CD-LOT  (TB-FUC-QT-OCR)
           ELSE
      *--> Tratamento para não funcionário
              ADD   1                  TO TB-CTT-QT-OCR
              MOVE LKS-CD-MCI          TO TB-CTT-CD-CLI  (TB-CTT-QT-OCR)
           END-IF
           .
       220000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       230000-MONTAR-RESPOSTA          SECTION.
      *-----------------------------------------------------------------
           MOVE  0005                  TO  MSG1-CD-SEQL-ERRO

              IF  TB-FUC-QT-OCR        GREATER  ZEROS
                  PERFORM  231000-TRATAR-FUNCAO
              END-IF

           MOVE  CT-REG                TO  IDX

           IF  TB-CTT-QT-OCR           GREATER  ZEROS
               PERFORM  232000-TRATAR-CONTATO
           END-IF
           .
       230000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       231000-TRATAR-FUNCAO            SECTION.
      *-----------------------------------------------------------------
           MOVE  0006                  TO  MSG1-CD-SEQL-ERRO

           PERFORM   VARYING IDX-FUC   FROM 1 BY 1
                       UNTIL IDX-FUC   >  TB-FUC-QT-OCR
                     MOVE TB-FUC-CD-CLI (IDX-FUC)
                                       TO  SGTS0079-S-CD-CLI (IDX-FUC)
           END-PERFORM

           PERFORM  231100-TRATAR-NM-DEPENDENCIA

           MOVE  ZEROS                 TO  IDX
           PERFORM  231200-TRATAR-NM-CARGO
           IF  IDX                     GREATER  CT-REG
               MOVE  IDX               TO  CT-REG
           END-IF

           MOVE  ZEROS                 TO  IDX

           PERFORM  231300-TRATAR-TEL-COML
                    VARYING IDX-FUC    FROM  1 BY 1
                      UNTIL IDX-FUC    > TB-FUC-QT-OCR

           IF  IDX  GREATER  CT-REG
               MOVE  IDX               TO  CT-REG
           END-IF
           .
       231000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       231100-TRATAR-NM-DEPENDENCIA    SECTION.
      *-----------------------------------------------------------------
           MOVE  0007                  TO  MSG1-CD-SEQL-ERRO

      *--> Movimentar o nome do programa a ser executado
           MOVE  MSTS8078              TO  NM-PGM

      *--> Movimentar os campos de entrada
           MOVE  TB-FUC-QT-OCR
                                       TO  MSTS8078-QT-OCR-TAB-UOR

           PERFORM   VARYING IDX-FUC  FROM  1  BY  1
                       UNTIL IDX-FUC        > TB-FUC-QT-OCR
                     MOVE  TB-FUC-CD-LOT (IDX-FUC)
                                       TO  MSTS8078-CD-UOR (IDX-FUC)
           END-PERFORM

           PERFORM  910000-CHAMAR-SERVICO

           PERFORM  VARYING IDX-FUC  FROM  1  BY  1
                      UNTIL IDX-FUC        > TB-FUC-QT-OCR
                    ADD  1             TO  IDX

                    MOVE TB-FUC-CD-LOT (IDX-FUC)
                                       TO CD-LOT-FUC
                    MOVE CD-LOT-FUC    TO SGTS0079-S-NM-CIA (IDX)

                    MOVE  TB-FUC-CD-MATR(IDX-FUC) TO  NR-CD-MATR

                    STRING  CD-MATR-USU  ' / '
                            TB-FUC-TX-NOME(IDX-FUC)
                            DELIMITED BY SIZE
                            INTO           SGTS0079-S-NM-CTT (IDX)
                    END-STRING
           END-PERFORM
           .
       231100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       231200-TRATAR-NM-CARGO          SECTION.
      *-----------------------------------------------------------------
      *--> Aciona o Programa Natural ARHPI40D
      *    A Entrada é um Array de Matrículas e devolve
      *    Nome do Funcionário, Dependência, Comissão Exercida
      *-----------------------------------------------------------------
           MOVE  0008                  TO  MSG1-CD-SEQL-ERRO

      *--> Movimentar o nome do programa a ser executado
           MOVE  ARHPI40D              TO  NM-PGM

      *--> Movimentar os campos de entrada
           PERFORM  VARYING IDX-FUC FROM  1  BY  1
                      UNTIL IDX-FUC   > TB-FUC-QT-OCR
                    MOVE TB-FUC-CD-MATR (IDX-FUC)
                                       TO  ARHPI40D-CD-MTC-FUN (IDX-FUC)
           END-PERFORM

           PERFORM  910000-CHAMAR-SERVICO

           PERFORM  VARYING IDX-FUC FROM  1  BY  1
                      UNTIL IDX-FUC       > TB-FUC-QT-OCR
                    ADD   1            TO  IDX
                    MOVE  ARHPI40D-NM-CMSS (IDX-FUC)
                                       TO  SGTS0079-S-NM-CRG-CTT(IDX)
           END-PERFORM
           .
       231200-SAI.
           EXIT.

      *-----------------------------------------------------------------
       231300-TRATAR-TEL-COML          SECTION.
      *-----------------------------------------------------------------
      *--> Consultar Dados de Localização do Usuário e Controla sua
      *    Atualização.
      *-----------------------------------------------------------------
           MOVE  0009                  TO  MSG1-CD-SEQL-ERRO

           ADD   1                     TO  IDX

           IF  TB-FUC-CD-MATR(IDX-FUC)     NOT EQUAL  ZEROS
               INITIALIZE                  ATCS6000-DADOS

      *-->     Função 0, para buscar o e-mail do funcionário
               MOVE 0                  TO K6000-CD-FUC
               SET   K6000-PSQ-EMAI-CPTV TO  TRUE
               MOVE  TB-FUC-CD-MATR(IDX-FUC)     TO  NR-CD-MATR
               MOVE  CD-MATR-USU                 TO  K6000-USUARIO
               MOVE  ATCS6000                    TO  NM-PGM
               PERFORM   910000-CHAMAR-SERVICO

               MOVE  K6000-TX-END-EMAI-CPTV-CMT
                                       TO  SGTS0079-S-TX-EMAI-CTT (IDX)

      *-->     Função 1, para buscar telefones comerciais e celulares
               SET   K6000-PSQ-TEL-USU TO  TRUE

               PERFORM   910000-CHAMAR-SERVICO

                MOVE SPACES            TO TB-FONES-TRBD
                                          TB-TCEL

                MOVE 0                 TO IDX-TRBD
                                          IDX-TCEL

                PERFORM 231310-TRATAR-TEL-CEL
                        VARYING IC-TAB FROM 1 BY 1
                          UNTIL IC-TAB > 3

                MOVE TB-FONES-TRBD    TO SGTS0079-S-NR-TEL-CML-CTT (IDX)
                MOVE TB-TCEL          TO SGTS0079-S-NR-TCEL-CTT(IDX)
           END-IF
           .
       231300-SAI.
           EXIT.

      *-----------------------------------------------------------------
       231310-TRATAR-TEL-CEL           SECTION.
      *-----------------------------------------------------------------
           MOVE  0010                  TO  MSG1-CD-SEQL-ERRO

      *--> Trata telefones comerciais
           IF K6000-CD-DDD-TRBD (IC-TAB) NOT = 0

              IF  IDX-TRBD  > 0
                  MOVE '/'    TO TB-CD-BARRA-TRBD  (IDX-TRBD)
              END-IF

              ADD 1 TO IDX-TRBD
              MOVE '('    TO TB-CD-ABRE-DDD-TRBD (IDX-TRBD)
              MOVE K6000-CD-DDD-TRBD (IC-TAB)
                                       TO TB-CD-DDD-TRBD (IDX-TRBD)
              MOVE ')'  TO TB-CD-FECHA-DDD-TRBD (IDX-TRBD)
              MOVE K6000-NR-TRBD (IC-TAB)
                                       TO TB-NR-TRBD (IDX-TRBD)
           END-IF

      *--> Trata telefones celulares
           IF K6000-CD-DDD-TCEL (IC-TAB) NOT = 0

              IF  IDX-TCEL  > 0
                  MOVE '/'    TO TB-CD-BARRA-TCEL (IDX-TCEL)
              END-IF

              ADD 1 TO IDX-TCEL
              MOVE '('    TO TB-CD-ABRE-DDD-TCEL (IDX-TCEL)
              MOVE K6000-CD-DDD-TCEL (IC-TAB)
                                       TO TB-CD-DDD-TCEL (IDX-TCEL)
              MOVE ')'  TO TB-CD-FECHA-DDD-TCEL (IDX-TCEL)
              MOVE K6000-NR-TCEL (IC-TAB)
                                       TO TB-NR-TCEL (IDX-TCEL)
           END-IF
           .
       231310-SAI.
           EXIT.

      *-----------------------------------------------------------------
       232000-TRATAR-CONTATO           SECTION.
      *-----------------------------------------------------------------
           MOVE  0011                  TO  MSG1-CD-SEQL-ERRO

           MOVE  CT-REG                TO  IDX

           PERFORM VARYING IDX-CTT FROM  1 BY 1
                     UNTIL IDX-CTT       > TB-CTT-QT-OCR
                   ADD   1             TO  IDX
                   MOVE  TB-CTT-CD-CLI (IDX-CTT)
                                       TO  SGTS0079-S-CD-CLI (IDX)
           END-PERFORM

      *--> Aciona o Módulo MCISI082
      *--> A partir de uma lista de MCIs, retorna Nome e Tipo de Cliente
           MOVE  MCISI082              TO  NM-PGM

      *--> Movimentar os campos de entrada
           MOVE  TB-CTT-QT-OCR         TO  MCISI082-QT-REG-E

           PERFORM VARYING IDX-CTT FROM  1  BY  1
                     UNTIL IDX-CTT   > TB-CTT-QT-OCR
                   MOVE TB-CTT-CD-CLI (IDX-CTT)
                                       TO  MCISI082-CD-CLI-E (IDX-CTT)
           END-PERFORM

           IF  TB-CTT-QT-OCR               GREATER  ZEROS
               PERFORM  910000-CHAMAR-SERVICO

               MOVE  CT-REG            TO  IDX

               MOVE  ZEROS             TO  IDX-CTT

               IF  TB-CTT-QT-OCR  EQUAL  MCISI082-QT-REG-S
                   PERFORM VARYING IDX-CTT  FROM  1  BY  1
                             UNTIL IDX-CTT      > TB-CTT-QT-OCR
                           ADD  1  TO IDX
                           MOVE  MCISI082-NM-CLI-S (IDX-CTT)
                                       TO  SGTS0079-S-NM-CTT (IDX)

                           MOVE  MCISI082-CD-TIP-CLI-S (IDX-CTT)
                                       TO  SGTS0079-S-CD-TIP-CLI (IDX)
                           PERFORM  232100-TRATAR-TIPO
                           PERFORM  232200-TRATAR-TEL-COML
                           PERFORM  232300-TRATAR-EMAIL
                   END-PERFORM
               ELSE
                   PERFORM VARYING IDX FROM  CT-REG  BY  1
                             UNTIL IDX  >  TB-CTT-QT-OCR
                           MOVE 'N'    TO  SAI-PESQUISA
                     PERFORM VARYING IDX-CTT FROM  1 BY 1
                               UNTIL IDX-CTT  > MCISI082-QT-REG-S OR
                                     SAI-PESQUISA  EQUAL 'S'
                       IF  SGTS0079-S-CD-CLI (IDX) EQUAL
                           MCISI082-CD-CLI-S (IDX-CTT)
                           MOVE 'S'    TO  SAI-PESQUISA
                           MOVE  MCISI082-NM-CLI-S (IDX-CTT)
                                       TO  SGTS0079-S-NM-CTT (IDX)

                           MOVE  MCISI082-CD-TIP-CLI-S (IDX-CTT)
                                       TO  SGTS0079-S-CD-TIP-CLI (IDX)
                           PERFORM  232100-TRATAR-TIPO
                           PERFORM  232200-TRATAR-TEL-COML
                           PERFORM  232300-TRATAR-EMAIL
                       END-IF
                     END-PERFORM
                    IF  SAI-PESQUISA  EQUAL  'N'
                        PERFORM  232200-TRATAR-TEL-COML
                        PERFORM  232300-TRATAR-EMAIL
                    END-IF
                   END-PERFORM
               END-IF
           END-IF
           .
       232000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       232100-TRATAR-TIPO              SECTION.
      *-----------------------------------------------------------------
      *--> Aciona o Módulo MCIS0024 para cd-tip-cli igual 1
      *    A partir do codigo do cliente, retorna dados básicos
      *    da companhia.
      *--> Aciona o módulo MCISI082 para cd-tip-cli igual 2
      *    A partir do codigo do cliente, retorna dados basicos
      *    da pessoa fisica.
      *-----------------------------------------------------------------
           MOVE  0012                  TO  MSG1-CD-SEQL-ERRO

           EVALUATE  MCISI082-CD-TIP-CLI-S (IDX-CTT)
               WHEN  0002
                     MOVE  MCISI082-NM-CLI-S (IDX-CTT)
                                       TO  SGTS0079-S-NM-CIA (IDX)
               WHEN  0001
                     INITIALIZE            MCIS0024-DADOS
                     MOVE  MCIS0024    TO  NM-PGM
      *--> Movimentar os campos de entrada
                     MOVE TB-CTT-CD-CLI (IDX-CTT)
                                       TO  MCIS0024-CLIENTE
                     MOVE  'S'         TO  MCIS0024-IND-OCUP-PRIN
                                           MCIS0024-IND-CONJUGE
                     PERFORM  910000-CHAMAR-SERVICO

                     MOVE  MCIS0024-NOME-EMPREGADOR
                                       TO  SGTS0079-S-NM-CIA (IDX)

                     MOVE  MCIS0024-TXT-CARGO
                                       TO  SGTS0079-S-NM-CRG-CTT (IDX)
               WHEN  OTHER
                     CONTINUE
           END-EVALUATE
           .
       232100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       232200-TRATAR-TEL-COML          SECTION.
      *-----------------------------------------------------------------
      *--> Aciona a Função 04 do Módulo MCIS1340
      *    Retornar Lista de telefones a partir de Codigo de Cliente
      *    Ordenado pela Data de Gravacao (registro mais novos primeiro)
      *    Consulta dados da Tabela DB2MCI.LNH_CMCO_CLI
      *-----------------------------------------------------------------
           MOVE  0013                  TO  MSG1-CD-SEQL-ERRO

      *--> Prepara áreas
           MOVE   SPACES               TO AUX-NR-TEL-CML-CTT
           MOVE   SPACES               TO AUX-NR-TCEL-CTT

      *--> MCIS1340-Controle
           INITIALIZE                      MCIS1340-DADOS
           MOVE  MCIS1340              TO  NM-PGM
           MOVE  'SGT'                 TO  MCIS1340-SYS-CHAMADOR
           MOVE  CTE-PROG(5:8)         TO  MCIS1340-PRG-CHAMADOR
           MOVE  0004                  TO  MCIS1340-FUNCAO
           MOVE  KOPRD-CD-OPRD         TO  MCIS1340-USUARIO

      *--> MCIS1340-Entrada - Código do Cliente
           MOVE  TB-CTT-CD-CLI (IDX-CTT)    TO  MCIS1340-CD-CLI

           PERFORM  910000-CHAMAR-SERVICO

           IF  MCIS1340-CD-CLI-S       NOT EQUAL  ZEROS
               MOVE ZEROS TO IDX-CML
               MOVE ZEROS TO IDX-CEL
               PERFORM  VARYING  IDX-MCIS134   FROM  1  BY  1
                          UNTIL  IDX-MCIS134    > 10

      *-->     Constantes locais: Comercial valor fixo = 0002
      *-->                        Celular   valor fixo = 0003

                  EVALUATE  MCIS1340-CD-LNH-CMCO-S (IDX-MCIS134)
                  WHEN 0002
                      ADD 1 TO IDX-CML
                      IF IDX-CML < 4
                      EVALUATE  IDX-CML
                      WHEN 1
                         MOVE MCIS1340-NR-DDD-S (IDX-MCIS134) TO
                                                               CML-DDD-1
                         MOVE MCIS1340-NR-TEL-S (IDX-MCIS134) TO   CML-1
                         MOVE  '('               TO  CML-FILL1
                         MOVE  ')'               TO  CML-FILL2
                      WHEN 2
                         MOVE MCIS1340-NR-DDD-S (IDX-MCIS134) TO
                                                               CML-DDD-2
                         MOVE MCIS1340-NR-TEL-S (IDX-MCIS134) TO   CML-2
                         MOVE  '/'               TO  CML-FILL3
                         MOVE  '('               TO  CML-FILL4
                         MOVE  ')'               TO  CML-FILL5
                      WHEN 3
                         MOVE MCIS1340-NR-DDD-S (IDX-MCIS134) TO
                                                               CML-DDD-3
                         MOVE MCIS1340-NR-TEL-S (IDX-MCIS134) TO   CML-3
                         MOVE  '/'               TO  CML-FILL6
                         MOVE  '('               TO  CML-FILL7
                         MOVE  ')'               TO  CML-FILL8
                      END-EVALUATE
                      END-IF
                  WHEN 0003
                      ADD 1 TO IDX-CEL
                      IF IDX-CEL < 4
                      EVALUATE  IDX-CEL
                      WHEN 1
                         MOVE MCIS1340-NR-DDD-S (IDX-MCIS134) TO
                                                               CEL-DDD-1
                         MOVE MCIS1340-NR-TEL-S (IDX-MCIS134) TO   CEL-1
                         MOVE  '('               TO  CEL-FILL1
                         MOVE  ')'               TO  CEL-FILL2
                      WHEN 2
                         MOVE MCIS1340-NR-DDD-S (IDX-MCIS134) TO
                                                               CEL-DDD-2
                         MOVE MCIS1340-NR-TEL-S (IDX-MCIS134) TO   CEL-2
                         MOVE  '/'               TO  CEL-FILL3
                         MOVE  '('               TO  CEL-FILL4
                         MOVE  ')'               TO  CEL-FILL5
                      WHEN 3
                         MOVE MCIS1340-NR-DDD-S (IDX-MCIS134) TO
                                                               CEL-DDD-3
                         MOVE MCIS1340-NR-TEL-S (IDX-MCIS134) TO   CEL-3
                         MOVE  '/'               TO  CEL-FILL6
                         MOVE  '('               TO  CEL-FILL7
                         MOVE  ')'               TO  CEL-FILL8
                      END-EVALUATE
                      END-IF
                 END-EVALUATE
              END-PERFORM
              MOVE AUX-NR-TEL-CML-CTT TO SGTS0079-S-NR-TEL-CML-CTT(IDX)
              MOVE AUX-NR-TCEL-CTT    TO SGTS0079-S-NR-TCEL-CTT (IDX)
           END-IF
           .
       232200-SAI.
           EXIT.

      *-----------------------------------------------------------------
       232300-TRATAR-EMAIL             SECTION.
      *-----------------------------------------------------------------
      *--> Aciona a Função 04 do Módulo MCIS1263
      *    Consulta por Cliente todos os E-mail's Ativos
      *    Consulta tabela DB2MCI.EMAIL
      *-----------------------------------------------------------------
           MOVE  0014                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      MCIS1263-DADOS
           MOVE  MCIS1263              TO  NM-PGM

      *--> MCIS1263-Controle
           MOVE  'SGT'                 TO  MCIS1263-SYS-CHAMADOR
           MOVE  CTE-PROG(5:8)         TO  MCIS1263-PRG-CHAMADOR
           MOVE  0004                  TO  MCIS1263-FUNCAO
           MOVE  KOPRD-CD-OPRD         TO  MCIS1263-USUARIO

      *--> MCIS1263-Entrada - Código do Cliente
           MOVE  TB-CTT-CD-CLI (IDX-CTT)    TO  MCIS1263-CD-CLI

           PERFORM  910000-CHAMAR-SERVICO

           IF  MCIS1263-CD-CLI-S           NOT EQUAL  ZEROS
               MOVE  MCIS1263-TXT-EMAIL-S(1)
                                       TO  SGTS0079-S-TX-EMAI-CTT (IDX)
           END-IF
           .
       232300-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *   Section da Faixa de 900000 a 969900 para Demais Rotinas
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       900000-EXECUTAR-MODULO          SECTION.
      *-----------------------------------------------------------------
      *--> Executa um modulo Online implementado com CONTAINER
      *    Nao incluir nessa section os módulos chamados via COMMAREA
      *-----------------------------------------------------------------

      *--> Grava o container de requisicao do modulo a executar
           PERFORM  982000-GRAVAR-CONTAINER-RQSC

      *--> Limpa a resposta e erro antes de executar o modulo
           PERFORM  983000-EXCLUIR-RESPOSTA-ERRO

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EVALUATE  NM-PGM
               WHEN  SGTSLA06
                     EXEC CICS LINK
                               PROGRAM ( SGTSLA06  )
                               CHANNEL ( NM-CNL    )
                               RESP    ( MSG1-RESP1)
                               RESP2   ( MSG1-RESP2)
                     END-EXEC
               WHEN  OTHER
                     SET   ERRO-PROGRAMADO
                                       TO  TRUE
                     MOVE  9006        TO  MSG1-CD-ERRO-TS
                     STRING  'Não definida a execução do módulo.' NM-PGM
                             DELIMITED BY SIZE
                             INTO          MSG1-TX-ERRO-TS
                     PERFORM  999000-ERRO
           END-EVALUATE

           EVALUATE  TRUE
               WHEN  MSG1-RESP1            EQUAL DFHRESP(NORMAL)
                     CONTINUE
               WHEN  OTHER
                     SET   ERRO-CICS   TO  TRUE
                     MOVE  3004        TO  MSG1-CD-ERRO-TS
                     STRING  'Erro  ao acionar o módulo  ' NM-PGM  ', '
                             'RESP  = '  MSG1-RESP1  ', '
                             'RESP2 = '  MSG1-RESP2
                             DELIMITED BY SIZE
                             INTO          MSG1-TX-ERRO-TS
                     PERFORM  999000-ERRO
           END-EVALUATE

      *--> Efetua a leitura do Container de Erro
           PERFORM  972000-LER-CONTAINER-DE-ERRO

      *--> Efetua a le itura do Container de Resposta
           PERFORM  973000-LER-RESPOSTA
           .
       900000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       910000-CHAMAR-SERVICO           SECTION.
      *-----------------------------------------------------------------
      *--> Acionar programa IIBP2000 para requisição do serviço.
      *-----------------------------------------------------------------

      *--> Grava o container com dados do serviço/operação a chamar
           PERFORM  981000-GRAVAR-CONTAINER-SRVC

      *--> Grava o container de requisicao do modulo a executar
           PERFORM  982000-GRAVAR-CONTAINER-RQSC

      *--> Limpa a resposta e erro antes de executar o modulo
           PERFORM  983000-EXCLUIR-RESPOSTA-ERRO

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS  LINK
                      PROGRAM   ( IIBP2000   )
                      CHANNEL   ( NM-CNL     )
                      RESP      ( MSG1-RESP1 )
                      RESP2     ( MSG1-RESP2 )
           END-EXEC

           EVALUATE  TRUE
               WHEN  MSG1-RESP1        EQUAL DFHRESP(NORMAL)
                     CONTINUE
               WHEN  OTHER
                     SET   ERRO-CICS   TO  TRUE
                     MOVE  9000        TO  MSG1-CD-ERRO-TS
                     STRING  'Erro ao acionar serviço: '
                             SRVC-ID-SRVC '.' SRVC-VRS-SRVC '.'
                             SRVC-ID-OPR  '.' SRVC-VRS-OPR
                             'via IIBP2000, '
                             'RESP=' MSG1-RESP1 ', RESP-2=' MSG1-RESP2
                             '.'
                             DELIMITED BY SIZE
                             INTO  MSG1-TX-ERRO-TS
                     PERFORM  999000-ERRO
           END-EVALUATE

      *--> Efetua a leitura do Container de Erro
           PERFORM   972000-LER-CONTAINER-DE-ERRO

      *--> Efetua a leitura do Container de Resposta
           PERFORM   973000-LER-RESPOSTA
           .
       910000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       969000-MONITORAR-VARIAVEIS      SECTION.
      *-----------------------------------------------------------------
      *--> Monitora as variáveis dos módulos chamados
      *-----------------------------------------------------------------

           EVALUATE NM-PGM

               WHEN ATCS6000
                    MOVE 'K6000-CD-FUC'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  K6000-CD-FUC
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE 'K6000-CD-USU'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  K6000-CD-USU
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

               WHEN SGTSLA06
                    MOVE 'LA06-001-CD-CTRA-TVM'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSLA06-CD-CTRA-TVM
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE 'LA06-002-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
      *------> Verifica se o conteudo e numerico antes de adicionar
                    IF  SGTSLA06-CD-OPR-ETRD-MCP    NUMERIC
                        MOVE  SGTSLA06-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP
                        MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
                    ELSE
                        MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE 'LA06-003NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSLA06-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE 'LA06-004-CD-CLI'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSLA06-CD-CLI
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE 'LA06-005-CD-TIP-FUC-CLI'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSLA06-CD-TIP-FUC-CLI
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

               WHEN ARHP0883
                    MOVE 'LKS-CD-MCI'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE LKS-CD-MCI
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

               WHEN ARHPI40D
                    MOVE 'ARHPI40D-CD-MTC-FUN(1)'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  ARHPI40D-CD-MTC-FUN(1)
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

               WHEN MSTS8078
                    MOVE 'MSTS8078-001-QT-OCR'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  MSTS8078-QT-OCR-TAB-UOR
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE 'MSTS8078-002-CD-UOR(1)'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  MSTS8078-CD-UOR(1)
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    IF  MSTS8078-QT-OCR-TAB-UOR
                                           GREATER  0001  AND
                        MSTS8078-QT-OCR-TAB-UOR
                                           LESS  0051
                        MOVE  'MSTS8078-003-CD-UOR(MAX)'
                                       TO  MSGKW999-NM-VRV-MNT
                        MOVE  MSTS8078-CD-UOR(MSTS8078-QT-OCR-TAB-UOR)
                                       TO  MSGKW999-VL-VRV-MNT
                        PERFORM  999003-ADICIONA-VRV-MNT
                    END-IF

               WHEN MCISI082
                    MOVE 'MCISI082-001-QT-REG-E'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  MCISI082-QT-REG-E
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE 'MCISI082-002-CD-CLI-E(1)'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  MCISI082-CD-CLI-E(1)
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    IF  MCISI082-QT-REG-E
                                           GREATER  0001  AND
                        MCISI082-QT-REG-E
                                           LESS  0231
                        MOVE  'MCISI082-003-CD-CLI-E(MAX)'
                                       TO  MSGKW999-NM-VRV-MNT
                        MOVE  MCISI082-CD-CLI-E(MCISI082-QT-REG-E)
                                       TO  MSGKW999-VL-VRV-MNT
                        PERFORM  999003-ADICIONA-VRV-MNT
                    END-IF

               WHEN MCIS1340
                    MOVE 'MCIS1340-SYS-CHAMADOR'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  MCIS1340-SYS-CHAMADOR
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE 'MCIS1340-PRG-CHAMADOR'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  MCIS1340-PRG-CHAMADOR
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE 'MCIS1340-FUNCAO'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  MCIS1340-FUNCAO
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE 'MCIS1340-USUARIO'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  MCIS1340-USUARIO
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE 'MCIS1340-CD-CLI'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  MCIS1340-CD-CLI
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

               WHEN MCIS1263
                    MOVE 'MCIS1263-SYS-CHAMADOR'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  MCIS1263-SYS-CHAMADOR
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE 'MCIS1263-PRG-CHAMADOR'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  MCIS1263-PRG-CHAMADOR
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE 'MCIS1263-FUNCAO'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  MCIS1263-FUNCAO
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE 'MCIS1263-USUARIO'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  MCIS1263-USUARIO
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE 'MCIS1263-CD-CLI'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  MCIS1263-CD-CLI
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

               WHEN OTHER
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
      *-----------------------------------------------------------------
      *--> Obtem o canal corrente
      *-----------------------------------------------------------------

      *--> Por causa da captura automática de erro CICS, deve ser
      *--> previsto o erro de container
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
           MOVE  0015                  TO  MSG1-CD-SEQL-ERRO

      *--> Monta o nome do container de requisicao
           MOVE  'SGTS0079'            TO  NM-PGM
           MOVE  'SGTS0079-RQSC'       TO  NM-CTNR

      *--> Recupera o tamanho do container de requisição
           PERFORM  973100-LER-CONTAINER-NODATA

      *--> Obtem o tamanho esperado da requisicao
           MOVE  LENGTH OF SGTS0079-RQSC
                                       TO  QT-CCT-EPRO

      *--> Valida tamanho esperado da requisicao X tamanho recebido
           IF  QT-CCT-LIDO                 NOT EQUAL  QT-CCT-EPRO
               SET   ERRO-TAMANHO-AREA
                                       TO  TRUE
               PERFORM  999000-ERRO
           END-IF

      *--> Configura dados para leitura do container de requisição
           SET   GET-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *--> previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *--> Efetua a leitura do container
           EXEC CICS GET
                     CONTAINER (NM-CTNR      )
                     FLENGTH   (QT-CCT-LIDO  )
                     INTO      (SGTS0079-RQSC)
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       971000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       972000-LER-CONTAINER-DE-ERRO    SECTION.
      *-----------------------------------------------------------------
      *    A presença do container PRM-ERRO indica que houve erro de
      *    integração, a ausência do container indica sucesso.
      *-----------------------------------------------------------------
           MOVE  0016                  TO  MSG1-CD-SEQL-ERRO

      *--> Le o container de erro dos modulos chamados
           SET   GET-CONTAINER         TO  TRUE
           MOVE  'PRM-ERRO'            TO  NM-CTNR
           INITIALIZE                      IIBKERRO-PRM-ERRO

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS  GET
                      CONTAINER ( NM-CTNR                     )
                      INTO      ( IIBKERRO-PRM-ERRO           )
                      FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                      RESP      ( MSG1-RESP1                  )
                      RESP2     ( MSG1-RESP2                  )
           END-EXEC

           EVALUATE TRUE
      *-->     Caso deseje personalizar ou não interromper o
      *-->     processamento, alterar para não dar erro aqui SOMENTE
      *-->     PARA O MODULO CHAMADO e tratar na section chamadora
               WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
                    CONTINUE
               WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
                    SET  ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
                    MOVE NM-PGM
                                       TO  MSG1-NM-PGM-ACND

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
                            INTO           MSG1-TX-ERRO-TS

                        MOVE IIBKERRO-TX-ERRO
                                       TO  MSG1-TX-RTN-PGM-ACND
                        MOVE IIBKERRO-CD-ERRO
                                       TO  MSG1-CD-RTN-PGM-ACND
                    END-IF

      *-->          Monitorar variáveis do módulo chamado
                     PERFORM  969000-MONITORAR-VARIAVEIS
                     PERFORM  999000-ERRO
               WHEN OTHER
                    SET   ERRO-CONTAINER
                                       TO  TRUE
                    PERFORM  999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       972000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       973000-LER-RESPOSTA             SECTION.
      *----------------------------------------------------------------
      *--> Recupera o container com os dados de resposta (saída).
      *    Modulo CONTAINER  -     NOME_PGM-RPST
      *    Modulo IIB (FIXO) -     PRM-DADOS-RPST
      *----------------------------------------------------------------
           MOVE  0017                  TO  MSG1-CD-SEQL-ERRO

      *--> Monta o nome dos container de acordo com modulo a executar
           MOVE  SPACES                TO  NM-CTNR

      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *--> nome_modulo -RPST
           EVALUATE NM-PGM
               WHEN 'ARHP0883'
                    MOVE 'PRM-DADOS-RPST'
                                       TO  NM-CTNR
                    MOVE  LENGTH OF ARHP0883-DADOS
                                       TO  QT-CCT-EPRO

               WHEN 'MSTS8078'
                    MOVE  'PRM-DADOS-RPST'
                                       TO  NM-CTNR

               WHEN 'ARHPI40D'
                    MOVE  'PRM-DADOS-RPST'
                                       TO  NM-CTNR

               WHEN 'MCISI082'
                    MOVE  'PRM-DADOS-RPST'
                                       TO  NM-CTNR

               WHEN 'MCIS1340'
                    MOVE  'PRM-DADOS-RPST'
                                       TO  NM-CTNR
                    MOVE  LENGTH OF MCIS1340-DADOS
                                       TO  QT-CCT-EPRO

               WHEN 'MCIS0024'
                    MOVE  'PRM-DADOS-RPST'
                                       TO  NM-CTNR
                    MOVE  LENGTH OF MCIS0024-DADOS
                                       TO  QT-CCT-EPRO

               WHEN 'MCIS1263'
                    MOVE  'PRM-DADOS-RPST'
                                       TO  NM-CTNR
                    MOVE  LENGTH OF MCIS1263-DADOS
                                       TO  QT-CCT-EPRO

               WHEN ATCS6000
                    MOVE  LENGTH OF ATCS6000-DADOS
                                       TO  QT-CCT-EPRO
                    MOVE  'PRM-DADOS-RPST'
                                       TO  NM-CTNR

               WHEN SGTSLA06
                    MOVE  LENGTH OF SGTSLA06-RPST
                                       TO  QT-CCT-EPRO
                    MOVE  NM-PGM       TO  NM-CTNR(1:8)
                    MOVE  '-RPST'      TO  NM-CTNR(9:5)

               WHEN OTHER
                    SET   ERRO-PROGRAMADO
                                       TO  TRUE
                    MOVE  9005         TO  MSG1-CD-ERRO-TS
                    STRING
                    'Módulo não definido: ' NM-PGM
                    DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
                    PERFORM  999000-ERRO
           END-EVALUATE

      *--> Recupera o tamanho do container de resposta
           PERFORM  973100-LER-CONTAINER-NODATA

           IF  NM-PGM   NOT EQUAL 'SGTSLA06' AND 'ATCS6000' AND
               'MCIS1340' AND 'MCIS1263' AND 'ARHP0883'  AND 'MSTS8078'
               AND 'ARHPI40D' AND 'MCISI082'
      *--> Valida tamanho esperado da requisicao X tamanho recebido
              IF  QT-CCT-LIDO              NOT EQUAL  QT-CCT-EPRO
                  SET ERRO-TAMANHO-AREA
                                       TO  TRUE
                  PERFORM  999000-ERRO
             END-IF
           END-IF

      *--> Configura dados para leitura do container de resposta
           SET  GET-CONTAINER          TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *--> previsto o erro de container
           SET  ERRO-CONTAINER         TO  TRUE

      *--> Le a RESPOSTA de acordo com o programa que a gravou
           EVALUATE NM-PGM
               WHEN SGTSLA06
                    EXEC CICS GET
                              CONTAINER ( NM-CTNR          )
                              INTO      ( SGTSLA06-RPST    )
                              FLENGTH   ( QT-CCT-LIDO      )
                    END-EXEC

               WHEN ARHP0883
                    EXEC CICS GET
                              CONTAINER ( NM-CTNR          )
                              INTO      ( ARHP0883-DADOS   )
                              FLENGTH   ( QT-CCT-LIDO      )
                    END-EXEC

               WHEN MSTS8078
                    EXEC CICS GET
                              CONTAINER ( NM-CTNR          )
                              INTO      ( MSTS8078-RPST    )
                              FLENGTH   ( QT-CCT-LIDO      )
                    END-EXEC

               WHEN ARHPI40D
                    EXEC CICS GET
                              CONTAINER ( NM-CTNR          )
                              INTO      ( ARHPI40D-RPST    )
                              FLENGTH   ( QT-CCT-LIDO      )
                    END-EXEC

               WHEN ATCS6000
                    EXEC CICS GET
                              CONTAINER ( NM-CTNR          )
                              INTO      ( ATCS6000-DADOS   )
                              FLENGTH   ( QT-CCT-LIDO      )
                    END-EXEC

               WHEN MCISI082
                    EXEC CICS GET
                              CONTAINER ( NM-CTNR          )
                              INTO      ( MCISI082-RPST   )
                              FLENGTH   ( QT-CCT-LIDO      )
                    END-EXEC

               WHEN MCIS0024
                    EXEC CICS GET
                              CONTAINER ( NM-CTNR          )
                              INTO      ( MCIS0024-DADOS   )
                              FLENGTH   ( QT-CCT-LIDO      )
                    END-EXEC

               WHEN MCIS1340
                    EXEC CICS GET
                              CONTAINER ( NM-CTNR          )
                              INTO      ( MCIS1340-DADOS   )
                              FLENGTH   ( QT-CCT-LIDO      )
                    END-EXEC

               WHEN MCIS1263
                    EXEC CICS GET
                              CONTAINER ( NM-CTNR          )
                              INTO      ( MCIS1263-DADOS   )
                              FLENGTH   ( QT-CCT-LIDO      )
                    END-EXEC

               WHEN OTHER
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
      *--> previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *--> Obtem a quantidade de caracteres no container
           MOVE  ZEROS                 TO  QT-CCT-LIDO
           EXEC CICS GET
                     CONTAINER (NM-CTNR    )
                     NODATA
                     FLENGTH   (QT-CCT-LIDO)
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       973100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       974000-LER-DADOS-OPRD           SECTION.
      *-----------------------------------------------------------------
           MOVE  0018                  TO  MSG1-CD-SEQL-ERRO

      *--> Configura dados para leitura do container de requisição
           SET   GET-CONTAINER         TO  TRUE
           MOVE  'PRM-INF-OPRD'        TO  NM-CTNR

      *--> Por causa da captura automática de erro CICS, deve ser
      *--> previsto o erro de container
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
      *   Section da Faixa de 980000 a 989900 para Gravação de Dados
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       980000-GRAVAR-CONTAINER-RPST    SECTION.
      *-----------------------------------------------------------------

      *--> Exclui eventual container de erro de outros programas
           PERFORM  983100-EXCLUIR-CONTAINER-ERRO

           MOVE  0019                  TO  MSG1-CD-SEQL-ERRO

      *--> Monta o nome do container de resposta
           MOVE  'SGTS0079-RPST'       TO  NM-CTNR
           SET   PUT-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *--> previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

           EXEC CICS PUT
                     CONTAINER (NM-CTNR                )
                     FROM      (SGTS0079-RPST          )
                     FLENGTH   (LENGTH OF SGTS0079-RPST)
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       980000-SAI.
           EXIT.

      *----------------------------------------------------------------
       981000-GRAVAR-CONTAINER-SRVC    SECTION.
      *----------------------------------------------------------------
      *--> Postar container de requisição de serviço.
      *----------------------------------------------------------------

      *--> Monta o nome do container de requisicao do servico
           MOVE  'PRM-RQSC-SRVC'       TO  NM-CTNR
           SET   PUT-CONTAINER         TO  TRUE

           EVALUATE  NM-PGM
               WHEN  SGTSLA06
                     MOVE  2126712     TO  SRVC-ID-SRVC
                     MOVE  0001        TO  SRVC-VRS-SRVC
                     MOVE  2320699     TO  SRVC-ID-OPR
                     MOVE  0001        TO  SRVC-VRS-OPR

               WHEN  ARHP0883
                     MOVE  70663       TO  SRVC-ID-SRVC
                     MOVE  0001        TO  SRVC-VRS-SRVC
                     MOVE  258428      TO  SRVC-ID-OPR
                     MOVE  0001        TO  SRVC-VRS-OPR

               WHEN  ARHPI40D
                     MOVE  ZEROS       TO  SRVC-ID-SRVC
                     MOVE  ZEROS       TO  SRVC-VRS-SRVC
                     MOVE  2418135     TO  SRVC-ID-OPR
                     MOVE  0001        TO  SRVC-VRS-OPR

               WHEN  ATCS6000
                     IF  K6000-PSQ-TEL-USU
                         MOVE  0071739 TO  SRVC-ID-SRVC
                         MOVE  0001    TO  SRVC-VRS-SRVC
                         MOVE  0291261 TO  SRVC-ID-OPR
                         MOVE  0001    TO  SRVC-VRS-OPR
                     ELSE
                         MOVE  0071739 TO  SRVC-ID-SRVC
                         MOVE  0001    TO  SRVC-VRS-SRVC
                         MOVE  0292124 TO  SRVC-ID-OPR
                         MOVE  0001    TO  SRVC-VRS-OPR
                     END-IF

               WHEN  MSTS8078
                     MOVE  ZEROS       TO  SRVC-ID-SRVC
                     MOVE  ZEROS       TO  SRVC-VRS-SRVC
                     MOVE  1768186     TO  SRVC-ID-OPR
                     MOVE  3           TO  SRVC-VRS-OPR

               WHEN  MCISI082
                     MOVE  ZEROS       TO  SRVC-ID-SRVC
                     MOVE  ZEROS       TO  SRVC-VRS-SRVC
                     MOVE  2170822     TO  SRVC-ID-OPR
                     MOVE  0001        TO  SRVC-VRS-OPR

               WHEN  MCIS0024
                     MOVE  19485       TO  SRVC-ID-SRVC
                     MOVE  0001        TO  SRVC-VRS-SRVC
                     MOVE  747809      TO  SRVC-ID-OPR
                     MOVE  0001        TO  SRVC-VRS-OPR

               WHEN  MCIS1340
                     MOVE  19485       TO  SRVC-ID-SRVC
                     MOVE  0001        TO  SRVC-VRS-SRVC
                     MOVE  63195       TO  SRVC-ID-OPR
                     MOVE  0001        TO  SRVC-VRS-OPR

               WHEN  MCIS1263
                     MOVE  19485       TO  SRVC-ID-SRVC
                     MOVE  0001        TO  SRVC-VRS-SRVC
                     MOVE  1257116     TO  SRVC-ID-OPR
                     MOVE  0001        TO  SRVC-VRS-OPR
           END-EVALUATE

      *--> Por causa da captura automática de erro CICS, deve ser
      *--> previsto o erro de container
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
      *-----------------------------------------------------------------
      *    Postar container com dados de requisição (entrada)
      *
      *                        NOME DO CONTAINER REQUISICAO
      *                        ----------------------------
      *
      *    Modulo CONTAINER  -     NOME_PGM-RQSC
      *    Modulo IIB (FIXO) -     PRM-DADOS-RQSC
      *
      *-----------------------------------------------------------------

      *--> Monta o nome dos container de acordo com modulo a executar
           MOVE  SPACES                TO  NM-CTNR
           SET   PUT-CONTAINER         TO  TRUE

      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *--> nome_modulo -RPST
           EVALUATE NM-PGM
               WHEN 'ARHP0883'
               WHEN 'ARHPI40D'
               WHEN 'ATCS6000'
               WHEN 'MSTS8078'
               WHEN 'MCISI082'
               WHEN 'MCIS0024'
               WHEN 'MCIS1340'
               WHEN 'MCIS1263'
                    MOVE  'PRM-DADOS-RQSC'
                                       TO  NM-CTNR
               WHEN OTHER
                    MOVE  NM-PGM       TO  NM-CTNR(1:8)
                    MOVE  '-RQSC'      TO  NM-CTNR(9:5)
           END-EVALUATE

      *--> Por causa da captura automática de erro CICS, deve ser
      *--> previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *--> Faz PUT da requisicao do programa a executar
           EVALUATE NM-PGM
               WHEN ARHP0883
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (ARHP0883-DADOS           )
                              FLENGTH   (LENGTH OF ARHP0883-DADOS )
                    END-EXEC

               WHEN SGTSLA06
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (SGTSLA06-RQSC           )
                              FLENGTH   (LENGTH OF SGTSLA06-RQSC )
                    END-EXEC

               WHEN ARHPI40D
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (ARHPI40D-RQSC           )
                              FLENGTH   (LENGTH OF ARHPI40D-RQSC )
                    END-EXEC

               WHEN ATCS6000
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (ATCS6000-DADOS          )
                              FLENGTH   (LENGTH OF ATCS6000-DADOS)
                    END-EXEC

               WHEN MSTS8078
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (MSTS8078-RQSC           )
                              FLENGTH   (LENGTH OF MSTS8078-RQSC )
                    END-EXEC

               WHEN MCISI082
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (MCISI082-RQSC           )
                              FLENGTH   (LENGTH OF MCISI082-RQSC )
                    END-EXEC

               WHEN MCIS0024
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (MCIS0024-DADOS           )
                              FLENGTH   (LENGTH OF MCIS0024-DADOS )
                    END-EXEC

               WHEN MCIS1340
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (MCIS1340-DADOS           )
                              FLENGTH   (LENGTH OF MCIS1340-DADOS )
                    END-EXEC

               WHEN MCIS1263
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (MCIS1263-DADOS           )
                              FLENGTH   (LENGTH OF MCIS1263-DADOS )
                    END-EXEC

               WHEN OTHER
                    SET   ERRO-PROGRAMADO
                                       TO  TRUE
                    MOVE  9004         TO  MSG1-CD-ERRO-TS
                    STRING
                     'Erro na gravação da requisição do módulo '
                            NM-PGM '.'
                    DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
                    PERFORM  999000-ERRO
           END-EVALUATE

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       982000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       983000-EXCLUIR-RESPOSTA-ERRO    SECTION.
      *-----------------------------------------------------------------
      * Exclui o container de resposta e de erro para evitar problemas *
      * em modulos executados mais de uma vez dentro da mesma sessao do*
      * CICS. Equivale ao initialize na area de resposta dos programas *
      * que utilizam COMMAREA.                                         *
      *-----------------------------------------------------------------

           MOVE  0020                  TO  MSG1-CD-SEQL-ERRO

      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *--> nome_modulo -RPST
           EVALUATE NM-PGM
               WHEN 'ARHP0883'
               WHEN 'ARHPI40D'
               WHEN 'ATCS6000'
               WHEN 'MSTS8078'
               WHEN 'MCISI082'
               WHEN 'MCIS0024'
               WHEN 'MCIS1340'
               WHEN 'MCIS1263'
      *-->     WHEN 'MODULO-VIA-IIB' --> incluir todos via IIB
                    MOVE 'PRM-DADOS-RPST'
                                       TO  NM-CTNR
               WHEN OTHER
                    MOVE  NM-PGM       TO  NM-CTNR(1:8)
                    MOVE  '-RPST'      TO  NM-CTNR(9:5)
           END-EVALUATE

           SET   DEL-CONTAINER         TO  TRUE

      *--> Exclui o container de resposta
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
                    SET   ERRO-CONTAINER
                                       TO  TRUE
                    PERFORM  999002-TRATA-CONDITION-CICS
           END-EVALUATE

      *--> Excluir o container de erro
           PERFORM  983100-EXCLUIR-CONTAINER-ERRO
           .
       983000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       983100-EXCLUIR-CONTAINER-ERRO   SECTION.
      *-----------------------------------------------------------------

      *--> Limpar variaveis container de erro
           INITIALIZE                      IIBKERRO-PRM-ERRO

      *--> Exclui o container de erro
           MOVE  0021                  TO  MSG1-CD-SEQL-ERRO
           MOVE  'PRM-ERRO'            TO  NM-CTNR
           SET   DEL-CONTAINER         TO  TRUE

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS DELETE
                     CONTAINER (NM-CTNR   )
                     RESP      (MSG1-RESP1)
                     RESP2     (MSG1-RESP2)
           END-EXEC

           EVALUATE TRUE
               WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
               WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    SET   ERRO-CONTAINER
                                       TO  TRUE
                    PERFORM  999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       983100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       984000-GRAVAR-CONTAINER-ERRO    SECTION.
      *-----------------------------------------------------------------

      *--> Grava o erro gerado no container de erro
      *--> Não há captura automática de erro CICS por usar RESP e RESP2

           EXEC CICS PUT
                     CONTAINER ( 'PRM-ERRO'                  )
                     FROM      ( IIBKERRO-PRM-ERRO           )
                     FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                     RESP      (MSG1-RESP1                   )
                     RESP2     (MSG1-RESP2                   )
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
      *-----------------------------------------------------------------

           IF  MSG1-CD-ERRO-TS             EQUAL  ZEROS
               MOVE  8888              TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS             EQUAL  SPACES
               MOVE 'Erro não especificado no módulo.'
                                       TO  MSG1-TX-ERRO-TS
           END-IF
           PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           .
       999000-ERRO-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *     As sections abaixo representam os tratamentos de erro
      *    utilizando o sistema MSG de mensageria (rotina MSGS0001)
      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------

      *--> Em caso de erro, desfaz as atualizações em banco de dados

           EXEC CICS SYNCPOINT
                     ROLLBACK
           END-EXEC

      *--> Verifica se o tipo de erro gerado possui tratamento interno
           EVALUATE TRUE
               WHEN ERRO-CONTAINER
                    PERFORM  999004-TRATA-ERRO-CTNR
               WHEN ERRO-TAMANHO-AREA
                    PERFORM  999005-TRATA-ERRO-AREA-DADOS
               WHEN ERRO-CANAL
                    PERFORM  999006-TRATA-ERRO-CANAL
               WHEN OTHER
                    CONTINUE
           END-EVALUATE

      *--> Adiciona as variáveis de entrada para monitoração

           MOVE '0079-001-CD-CTRA-TVM'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0079-CD-CTRA-TVM
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE '0079-002-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
      *------> Verifica se o conteudo e numerico antes de adicionar
           IF  SGTS0079-CD-OPR-ETRD-MCP    NUMERIC
               MOVE  SGTS0079-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP
               MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE '0079-003-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0079-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE '0079-004-CD-CLI'      TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0079-CD-CLI       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

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

      *---------------------------------------
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

           GOBACK
           .
       999999-SAI.
           EXIT.
      *---------------------- FIM SGTS0079 -----------------------------