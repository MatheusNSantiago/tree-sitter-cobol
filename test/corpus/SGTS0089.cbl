      *-----------------------------------------------------------------
      * PROGRAMA..: SGTS0089
      * ANALISTA..: F0175702 - ADRIANO SEIJI NAKAMURA
      * AUTOR.....: F0175702 - ADRIANO SEIJI NAKAMURA
      * DATA......: 24/10/2016
      * OBJETIVO..: Atualizar lista de repactuações programadas
      *             EST_OPR_ETRD, RPAC_PGMD_OPR
      * COMPILACAO: COBOL/CICS
      *-----------------------------------------------------------------
      * VRS001 08/08/2018 F8658869 DE 2016/145195 - exclusão do campo
      *                   resposta e adequação ao lvf.
      *        13/02/2017 F8785286 DE 2016/145195 - Inclusão campo
      *             CD-USU-RSP-ATL para chave log (movimentar
      *              a partir do container operador).
      *        24/10/2016 F0175702 DE 2016/145195 - IMPLANTACAO
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID. SGTS0089.
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

       77  CTE-INICIO-SS                 PIC  X(040)  VALUE
                    '*** S.S. COMEÇA AQUI ***'.

       77  CTE-PROG                      PIC  X(018)  VALUE
                    '*** SGTS0089 ***'.
       77  CTE-VERS                      PIC  X(006)  VALUE 'VRS001'.

      * Módulos acionados
       77  SGTST009                      PIC  X(008) VALUE 'SGTST009'.
       77  SGTSI061                      PIC  X(008) VALUE 'SGTSI061'.
       77  SGTSE061                      PIC  X(008) VALUE 'SGTSE061'.
       77  SGTSL061                      PIC  X(008) VALUE 'SGTSL061'.
      *----------------------------------------------------------------*
       LOCAL-STORAGE                   SECTION.
      *----------------------------------------------------------------*

      * Areas de trabalho
       01  IDX                           PIC S9(4) COMP-5 VALUE 0.
       01  IN-LISTA-RPAC                 PIC X(1)  VALUE SPACES.
           88  FIM-LISTA-RPAC             VALUE 'S'.

       01  ULT-DT-RPAC-PGMD          PIC X(10)    VALUE '01-01-1900'.
       01  ULT-DT-RPAC-PGMD-R    REDEFINES   ULT-DT-RPAC-PGMD.
           03 DIA                    PIC 9(02).
           03 FILLER                 PIC X(01).
           03 MES                    PIC 9(02).
           03 FILLER                 PIC X(01).
           03 ANO                    PIC 9(04).

       01  DT-FMTD.
           03 DIA                        PIC  X(002).
           03 FILLER                     PIC  X(001)     VALUE '.'.
           03 MES                        PIC  X(002).
           03 FILLER                     PIC  X(001)     VALUE '.'.
           03 ANO                        PIC  X(004).

      *    Chamada à subrotina SBDATAF
      *
       01  WKSBDATA        PIC X(4095).
       01  AREA-RED REDEFINES WKSBDATA.
           03  AREA-TAM    PIC 9(0008) COMP.
           03  FILLER      PIC X(4071).
           03  AREA-FIM    PIC X(0020).

       01  SBDATAF                     PIC  X(08) VALUE 'SBDATAF'.
       01  FUNCAO                      PIC  X(03) VALUE 'F01'.
       01  ARG01                       PIC  9(08).
       01  ARG01-R  REDEFINES  ARG01.
         03  DIA                       PIC  9(02).
         03  MES                       PIC  9(02).
         03  ANO                       PIC  9(04).
       01  ARG02                       PIC  9(01) VALUE 4.

      *--> Auxiliares para movimentação na monitoração de variáveis.
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).
      *----------------------------------------------------------------*
      * Book dos Containers e das subrotinas chamadas
      *----------------------------------------------------------------*

      * Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1
      * Book para captura de abend cics da bloco codigo MSGKW999
-INC HLPKW999
      * Book para captura de erro via MSG1
-INC MSGKWMG1

      * Book do container de requisição e resposta deste modulo
       01 SGTS0089-DADOS.
-INC SGTK0089

      * CRUD EST_OPR_ETRD
       01  SGTST009-DADOS.
-INC SGTKT009

      * Incluir Tabela RPAC_PGMD_OPR
-INC SGTKI061

      * Excluir Tabela RPAC_PGMD_OPR
-INC SGTKE061

      * Listar Tabela RPAC_PGMD_OPR
-INC SGTKL061

      * Book do Container de Erro
-INC IIBKERRO
      * Book do container de Serviços (para acionamento de serviços)

      * Book dos dados do usuário
-INC IIBKOPRD

       01  CTE-FINAL-SS                  PIC X(40)      VALUE
                    '*** S.S. TERMINA AQUI ***'.

      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       000000-ROTINA-PRINCIPAL         SECTION.
      *----------------------------------------------------------------*

           PERFORM 100000-INICIAR

           PERFORM 200000-PROCESSAR

           PERFORM 999999-ENCERRAR

           GOBACK
           .
       000000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       100000-INICIAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE 0001                   TO MSG1-CD-SEQL-ERRO

      *    Ativa a captura automática de abend do CICS.
      *    WHENEVER CICS
-INC HLPKE999

           PERFORM 970000-LER-CANAL

      *    Define o nome do programa p/ fins manipulação container
           MOVE 'SGTS0089'             TO NM-PGM

      *    Limpa a resposta e erro antes de executar o módulo
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO

           PERFORM 971000-LER-REQUISICAO

      *    Caso não seja necessário utilização do Usuário Logado,
      *    a section 974000-LER-DADOS-OPRD poderá ser excluída
           PERFORM 974000-LER-DADOS-OPRD

           PERFORM 110000-VALIDAR-REQUISICAO
           .
       100000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       110000-VALIDAR-REQUISICAO       SECTION.
      *----------------------------------------------------------------*
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO

      *    Validação tamanho esperado da requisicao X tamanho recebido
      *    na section de leitura de container de requisição 971000

      *    Valida campo a campo da requisição

           IF  SGTS0089-CD-CTRA-TIT-VL  LESS  ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0001               TO MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de títulos e valores mobiliários
      -        ' inválido.'            TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0089-CD-CTRA-TIT-VL  EQUAL  ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0002               TO MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de título e valores mobiliários
      -        'não informado.'        TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0089-CD-OPR-ETRD-MCP NOT NUMERIC
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0003               TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -        ' numérico.'            TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0089-CD-OPR-ETRD-MCP <= ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0004               TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais inv
      -        'álido.'                TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0089-NR-VRS-OPR-ETRD <= ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0006               TO MSG1-CD-ERRO-TS
               MOVE 'Número da versão da operação estruturada inválido.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           PERFORM VARYING IDX FROM 1 BY 1
             UNTIL IDX GREATER SGTS0089-QT-OCR-TAB
             IF  SGTS0089-DT-RPAC-PGMD(IDX) = SPACES
                 SET ERRO-PROGRAMADO   TO TRUE
                 MOVE 0152             TO MSG1-CD-ERRO-TS
                 MOVE 'Data repactuação programada não informado.'
                                       TO MSG1-TX-ERRO-TS
                 PERFORM 999000-ERRO
             ELSE
                 MOVE CORR SGTS0089-DT-RPAC-PGMD-R(IDX)
                                                        TO  ARG01-R
                 PERFORM 111000-VALIDAR-DATA
             END-IF
             IF  SGTS0089-TX-MTV-RPAC-PGMD(IDX) = SPACES
                 SET ERRO-PROGRAMADO   TO TRUE
                 MOVE 0778             TO MSG1-CD-ERRO-TS
                 MOVE 'Texto motivo repactuação programada não informado
      -          '.'                   TO MSG1-TX-ERRO-TS
                 PERFORM 999000-ERRO
             END-IF
           END-PERFORM
           .
       110000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       111000-VALIDAR-DATA                SECTION.
      *----------------------------------------------------------------*
           MOVE 0003                   TO MSG1-CD-SEQL-ERRO

           MOVE 4095                   TO AREA-TAM
           MOVE 'SBDATAF -WRKAREA-FIM' TO AREA-FIM

           CALL SBDATAF     USING FUNCAO WKSBDATA  ARG01 ARG02

      *--> Return code = 24: Data Inválida.

           IF  RETURN-CODE             NOT EQUAL ZEROS AND 24 AND 22
                                                       AND 46
               SET ERRO-PROGRAMA-ACIONADO TO TRUE
               MOVE 0051                  TO MSG1-CD-ERRO-TS
               MOVE RETURN-CODE           TO MSG1-CD-RTN-PGM-ACND
               MOVE 'Erro no tratamento de datas.'
                                          TO MSG1-TX-RTN-PGM-ACND
                                             MSG1-TX-ERRO-TS
               MOVE SBDATAF               TO MSG1-NM-PGM-ACND
                                             NM-PGM
      *        Monitorar variáveis do módulo chamado
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-ERRO

           END-IF

           IF  RETURN-CODE               EQUAL 24 OR 22 OR 46
               SET ERRO-PROGRAMA-ACIONADO       TO TRUE
               MOVE 0049                        TO MSG1-CD-ERRO-TS
               MOVE 'Data informada inválida.'  TO MSG1-TX-ERRO-TS
                                                   MSG1-TX-RTN-PGM-ACND
               MOVE RETURN-CODE                 TO MSG1-CD-RTN-PGM-ACND
               MOVE SBDATAF               TO MSG1-NM-PGM-ACND
                                             NM-PGM
      *        Monitorar variáveis do módulo chamado
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-ERRO
           END-IF
           .
       111000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       200000-PROCESSAR                SECTION.
      *----------------------------------------------------------------*
           MOVE 0004                   TO MSG1-CD-SEQL-ERRO

           MOVE  '01-01-1900'        TO  ULT-DT-RPAC-PGMD
           MOVE  SPACES              TO  IN-LISTA-RPAC

           PERFORM 2110000-LISTAR-REPAC-PROG

           IF  SGTSL061-QT-OCR-TAB > ZEROS
               PERFORM 210000-EXCLUIR-REPAC-PROG UNTIL
                       FIM-LISTA-RPAC
           END-IF
      *
           IF SGTS0089-QT-OCR-TAB > ZEROS
              PERFORM 220000-INCLUIR-REPAC-PROG
                  VARYING IDX FROM 1 BY 1
                       UNTIL IDX > SGTS0089-QT-OCR-TAB
           END-IF
           .
       200000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       210000-EXCLUIR-REPAC-PROG    SECTION.
      *----------------------------------------------------------------*
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

           ADD     1                   TO  SGTSL061-IC-TAB

           INITIALIZE                  SGTSE061-DADOS

      *    Movimentar o nome do programa a ser executado
           MOVE SGTSE061               TO NM-PGM

      *    Movimentar os campos de entrada

           MOVE SGTS0089-CD-CTRA-TIT-VL  TO SGTSE061-CD-CTRA-TIT-VL
           MOVE SGTS0089-CD-OPR-ETRD-MCP TO SGTSE061-CD-OPR-ETRD-MCP
           MOVE SGTS0089-NR-VRS-OPR-ETRD TO SGTSE061-NR-VRS-OPR-ETRD
           MOVE CORR SGTSL061-DT-RPAC-PGMD-R(SGTSL061-IC-TAB)
                                         TO  DT-FMTD
           MOVE DT-FMTD                  TO SGTSE061-DT-RPAC-PGMD

           PERFORM 900000-EXECUTAR-MODULO

           IF SGTSL061-IC-TAB =  SGTSL061-QT-OCR-TAB
              IF SGTSL061-TEM-MAIS-REGISTRO
                 MOVE CORR SGTSL061-DT-RPAC-PGMD-R(SGTSL061-IC-TAB)
                                              TO  ULT-DT-RPAC-PGMD-R
                 PERFORM 2110000-LISTAR-REPAC-PROG
              ELSE
                 SET FIM-LISTA-RPAC  TO  TRUE
              END-IF
           END-IF
           .
       210000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       2110000-LISTAR-REPAC-PROG    SECTION.
      *----------------------------------------------------------------*
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

      *    Movimentar o nome do programa a ser executado
           MOVE SGTSL061               TO NM-PGM

      *    Movimentar os campos de entrada

           MOVE SGTS0089-CD-CTRA-TIT-VL  TO SGTSL061-CD-CTRA-TIT-VL
           MOVE SGTS0089-CD-OPR-ETRD-MCP TO SGTSL061-CD-OPR-ETRD-MCP
           MOVE SGTS0089-NR-VRS-OPR-ETRD TO SGTSL061-NR-VRS-OPR-ETRD
           MOVE CORR ULT-DT-RPAC-PGMD-R  TO SGTSL061-DT-RPAC-ULT-R

           PERFORM 900000-EXECUTAR-MODULO

           MOVE ZEROS    TO   SGTSL061-IC-TAB
           .
       2110000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       220000-INCLUIR-REPAC-PROG    SECTION.
      *----------------------------------------------------------------*
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSI061-DADOS

      *    Movimentar o nome do programa a ser executado
           MOVE SGTSI061               TO NM-PGM

      *    Movimentar os campos de entrada

           MOVE SGTS0089-CD-CTRA-TIT-VL  TO SGTSI061-CD-CTRA-TIT-VL
           MOVE SGTS0089-CD-OPR-ETRD-MCP TO SGTSI061-CD-OPR-ETRD-MCP
           MOVE SGTS0089-NR-VRS-OPR-ETRD TO SGTSI061-NR-VRS-OPR-ETRD
           MOVE CORR SGTS0089-DT-RPAC-PGMD-R(IDX)
                                         TO  DT-FMTD
           MOVE DT-FMTD                  TO SGTSI061-DT-RPAC-PGMD
           MOVE KOPRD-CD-OPRD            TO SGTSI061-CD-USU-ULT-ATL
           MOVE 'CURRENT'                TO SGTSI061-TS-ULT-ATL
           MOVE SGTS0089-TX-MTV-RPAC-PGMD(IDX)
                                         TO SGTSI061-TX-MTV-RPAC-PGMD

           PERFORM 900000-EXECUTAR-MODULO
           .
       220000-SAI.
           EXIT.

      *----------------------------------------------------------------*
      *                                                                *
      *  SECTION DA FAIXA DE 900000 A 969900 PARA DEMAIS ROTINAS       *
      *                                                                *
      *----------------------------------------------------------------*
      * Executa um modulo implementado com CONTAINER                   *
      * Nao incluir nessa section os modulos chamados via COMMAREA     *
      *                                                                *
      *----------------------------------------------------------------*
       900000-EXECUTAR-MODULO          SECTION.
      *----------------------------------------------------------------*
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

      *    Grava o container de requisição do módulo a executar
           PERFORM 982000-GRAVAR-CONTAINER-RQSC

      *    Limpa a resposta e erro antes de executar o módulo
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO

      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EVALUATE NM-PGM
               WHEN SGTSL061
                   EXEC CICS LINK
                             PROGRAM ( SGTSL061  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC

               WHEN SGTSI061
                   EXEC CICS LINK
                             PROGRAM ( SGTSI061  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC

               WHEN SGTSE061
                   EXEC CICS LINK
                             PROGRAM ( SGTSE061  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC

               WHEN OTHER
                   MOVE 9006          TO MSG1-CD-ERRO-TS
                   STRING
                     'Não definida a execução do módulo ' NM-PGM
                     DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
                   END-STRING
                   PERFORM 999000-ERRO
           END-EVALUATE
      *
           EVALUATE TRUE
               WHEN MSG1-RESP1        EQUAL DFHRESP(NORMAL)
                    CONTINUE

               WHEN OTHER
                    MOVE 3004          TO MSG1-CD-ERRO-TS
                    STRING
                      'Erro na execução do módulo ' NM-PGM ', '
                      'RESP  ='   ' ' MSG1-RESP1  ', '
                      'RESP2 =' ' ' MSG1-RESP2
                      DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
                    END-STRING
                    SET ERRO-CICS      TO TRUE
                    PERFORM 999000-ERRO
           END-EVALUATE

      *    Efetua a leitura do Container de Erro
           PERFORM 972000-LER-CONTAINER-DE-ERRO

      *    Efetua a leitura do Container de Resposta
           PERFORM 973000-LER-RESPOSTA
           .
       900000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       920000-CHAMAR-SGTST009          SECTION.
      *----------------------------------------------------------------*
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO

      *    Movimento nome do módulo para monitoração de variáveis
           MOVE SGTST009               TO NM-PGM
           MOVE  KOPRD-CD-OPRD         TO  SGTST009-CD-USU-RSP-ATL

           EXEC CICS LINK
                PROGRAM  (SGTST009      )
                COMMAREA (SGTST009-DADOS)
           END-EXEC

           IF  SGTST009-CD-RTN-PGM     NOT EQUAL ZEROS
               SET ERRO-PROGRAMA-ACIONADO        TO TRUE
               MOVE 3004                         TO MSG1-CD-ERRO-TS
               MOVE 'Erro ao acionar o módulo.'  TO MSG1-TX-ERRO-TS
               MOVE SGTST009-MSG-ERRO-PGM TO MSG1-TX-RTN-PGM-ACND
               MOVE NM-PGM                      TO MSG1-NM-PGM-ACND
               MOVE SGTST009-CD-RTN-PGM         TO MSG1-CD-RTN-PGM-ACND

      *        Monitorar variáveis do módulo chamado
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-ERRO
           END-IF
           .
       920000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       969000-MONITORAR-VARIAVEIS      SECTION.
      *----------------------------------------------------------------*
      * Monitora as variáveis dos módulos chamados
      *----------------------------------------------------------------*
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO

           EVALUATE NM-PGM
               WHEN SGTSL061
                    MOVE 'SGTSL061-CD-CTRA-TIT-VL'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL061-CD-CTRA-TIT-VL
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL061-CD-OPR-ETRD-MCP'
                                               TO MSGKW999-NM-VRV-MNT
      *-----------> Verifica se o conteudo e numerico antes de adicionar
                    IF  SGTSL061-CD-OPR-ETRD-MCP  IS NUMERIC
                        MOVE  SGTSL061-CD-OPR-ETRD-MCP
                                               TO  CD-OPR-ETRD-MCP
                        MOVE  CD-OPR-ETRD-MCP-TX
                                               TO  MSGKW999-VL-VRV-MNT
                    ELSE
                        MOVE  'Valor não numérico.'
                                               TO  MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL061-NR-VRS-OPR-ETRD'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL061-NR-VRS-OPR-ETRD
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL061-DT-RPAC-ULT'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL061-DT-RPAC-ULT  TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSI061
                    MOVE 'SGTSI061-CD-CTRA-TIT-VL  '
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI061-CD-CTRA-TIT-VL
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI061-CD-OPR-ETRD-MCP'
                                               TO MSGKW999-NM-VRV-MNT
      *-----------> Verifica se o conteudo e numerico antes de adicionar
                    IF  SGTSI061-CD-OPR-ETRD-MCP  IS NUMERIC
                        MOVE  SGTSI061-CD-OPR-ETRD-MCP
                                               TO  CD-OPR-ETRD-MCP
                        MOVE  CD-OPR-ETRD-MCP-TX
                                               TO  MSGKW999-VL-VRV-MNT
                    ELSE
                        MOVE  'Valor não numérico.'
                                               TO  MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI061-NR-VRS-OPR-ETRD'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI061-NR-VRS-OPR-ETRD
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI061-DT-RPAC-PGMD'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI061-DT-RPAC-PGMD TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI061-CD-USU-ULT-ATL'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI061-CD-USU-ULT-ATL
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI061-TS-ULT-ATL' TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI061-TS-ULT-ATL   TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI061-TX-MTV-RPAC-PGMD'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI061-TX-MTV-RPAC-PGMD
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSE061
                    MOVE 'SGTSE061-CD-CTRA-TIT-VL'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSE061-CD-CTRA-TIT-VL
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSE061-CD-OPR-ETRD-MCP'
                                               TO MSGKW999-NM-VRV-MNT
      *-----------> Verifica se o conteudo e numerico antes de adicionar
                    IF  SGTSE061-CD-OPR-ETRD-MCP  IS NUMERIC
                        MOVE  SGTSE061-CD-OPR-ETRD-MCP
                                               TO  CD-OPR-ETRD-MCP
                        MOVE  CD-OPR-ETRD-MCP-TX
                                               TO  MSGKW999-VL-VRV-MNT
                    ELSE
                        MOVE  'Valor não numérico.'
                                               TO  MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSE061-NR-VRS-OPR-ETRD'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSE061-NR-VRS-OPR-ETRD
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSE061-DT-RPAC-PGMD'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSE061-DT-RPAC-PGMD TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SBDATAF
                    MOVE 'FUNCAO'              TO MSGKW999-NM-VRV-MNT
                    MOVE FUNCAO                TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'ARG01'               TO MSGKW999-NM-VRV-MNT
                    MOVE ARG01                 TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'ARG02'               TO MSGKW999-NM-VRV-MNT
                    MOVE ARG02                 TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

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
           MOVE 0011                   TO MSG1-CD-SEQL-ERRO

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

           MOVE 0012                   TO MSG1-CD-SEQL-ERRO

      *    Monta o nome do container de requisicao
           MOVE 'SGTS0089'             TO NM-PGM
           MOVE 'SGTS0089-RQSC'        TO NM-CTNR

      *    Recupera o tamanho do container de requisição
           PERFORM 973100-LER-CONTAINER-NODATA

           MOVE 0013                   TO MSG1-CD-SEQL-ERRO

      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Efetua a leitura do container
           EXEC CICS GET
                     CONTAINER (NM-CTNR      )
                     FLENGTH   (QT-CCT-LIDO  )
                     INTO      (SGTS0089-RQSC)
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
           MOVE 0014                   TO MSG1-CD-SEQL-ERRO

      *    Le o container de erro dos modulos chamados
           SET GET-CONTAINER           TO TRUE
           MOVE 'PRM-ERRO'             TO NM-CTNR
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
      *        PARA O MÓDULO CHAMADO e tratar na section chamadora
               WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
                    CONTINUE

               WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
                    SET ERRO-PROGRAMA-ACIONADO TO TRUE
                    MOVE IIBKERRO-TX-ERRO      TO MSG1-TX-RTN-PGM-ACND
                                                  MSG1-TX-ERRO-TS
                    MOVE NM-PGM                TO MSG1-NM-PGM-ACND
                    MOVE IIBKERRO-CD-ERRO      TO MSG1-CD-RTN-PGM-ACND
                                                  MSG1-CD-ERRO-TS

      *             Monitorar variáveis do módulo chamado
                    PERFORM 969000-MONITORAR-VARIAVEIS
                    PERFORM 999000-ERRO

               WHEN OTHER
                    SET ERRO-CONTAINER  TO TRUE
                    PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       972000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       973000-LER-RESPOSTA             SECTION.
      *----------------------------------------------------------------*
      *    Recupera o container com os dados de resposta (saída).
      *
      *                        NOME DO CONTAINER RESPOSTA
      *                        --------------------------
      *
      *    Modulo CONTAINER  -     NOME_PGM-RPST
      *    Modulo IIB (FIXO) -     PRM-DADOS-RPST
      *
      *---------------------------------------------------------------*

           MOVE 0015                   TO MSG1-CD-SEQL-ERRO

      *    Monta o nome dos container de acordo com modulo a executar
           MOVE SPACES                 TO NM-CTNR

      *    Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *    nome_modulo -RPST
           MOVE NM-PGM        TO NM-CTNR(1:8)
           MOVE '-RPST'       TO NM-CTNR(9:5)

      *    Recupera o tamanho do container de resposta
           PERFORM 973100-LER-CONTAINER-NODATA

           EVALUATE NM-PGM
               WHEN SGTSI061
                    MOVE LENGTH OF SGTSI061-RPST TO QT-CCT-EPRO

               WHEN SGTSE061
                    MOVE LENGTH OF SGTSE061-RPST TO QT-CCT-EPRO

               WHEN OTHER
                    CONTINUE
           END-EVALUATE

           IF  NM-PGM  EQUAL SGTSI061 OR  SGTSE061
               IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
                   SET  ERRO-TAMANHO-AREA       TO TRUE
                   MOVE QT-CCT-EPRO             TO VL-EXBR-1
                   MOVE QT-CCT-LIDO             TO VL-EXBR-2
                   STRING
                      'Container RPST ' NM-PGM ' '
                      ', TAMANHO ESPERADO: '  VL-EXBR-1
                      ', TAMANHO RECEBIDO: '  VL-EXBR-2
                      DELIMITED BY SIZE      INTO MSG1-TX-ERRO-TS
                  END-STRING
                  PERFORM 999000-ERRO
               END-IF
           END-IF

      *Configura dados para leitura do container de resposta
           SET  GET-CONTAINER          TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Le a RESPOSTA de acordo com o programa que a gravou

           MOVE 0016                   TO MSG1-CD-SEQL-ERRO

           EVALUATE NM-PGM

      *        Calcular a quantidade de caracteres esperado e limpar
      *        a area de dados antes de executar o GET
      *        Para matrizes com dados variaveis (clausula depending
      *        on) nao ha como utilizar o comando INITIALIZE

               WHEN SGTSI061
                    INITIALIZE SGTSI061-RPST
                    MOVE LENGTH OF SGTSI061-RPST TO QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSI061-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN SGTSE061
                    INITIALIZE SGTSE061-RPST
                    MOVE LENGTH OF SGTSE061-RPST TO QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSE061-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN SGTSL061
                    MOVE LENGTH OF SGTSL061-RPST TO QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSL061-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN OTHER
                    MOVE 9006    TO MSG1-CD-ERRO-TS
                    STRING
                        'Não definida a execução do módulo.'
                         NM-PGM  DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
                    END-STRING
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
           MOVE 0017                   TO MSG1-CD-SEQL-ERRO

      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Obtem a quantidade de caracteres no container
           MOVE ZEROS                  TO QT-CCT-LIDO
           EXEC CICS GET
                     CONTAINER (NM-CTNR    )
                     NODATA
                     FLENGTH   (QT-CCT-LIDO)
           END-EXEC

      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       973100-SAI.
           EXIT.

      *----------------------------------------------------------------*
       974000-LER-DADOS-OPRD           SECTION.
      *----------------------------------------------------------------*
           MOVE 0018                   TO MSG1-CD-SEQL-ERRO

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

      *----------------------------------------------------------------*
      *                                                                *
      * SECTION DA FAIXA DE 980000 A 989900 PARA GRAVACAO DE DADOS     *
      *                                                                *
      *----------------------------------------------------------------*
       982000-GRAVAR-CONTAINER-RQSC    SECTION.
      *----------------------------------------------------------------*
      *    Postar container com dados de requisição (entrada)
      *
      *                        NOME DO CONTAINER REQUISICAO
      *                        ----------------------------
      *
      *    Modulo CONTAINER  -     NOME_PGM-RQSC
      *    Modulo IIB (FIXO) -     PRM-DADOS-RQSC
      *
      *----------------------------------------------------------------*

           MOVE 0019                   TO MSG1-CD-SEQL-ERRO

      *    Monta o nome dos container de acordo com módulo a executar
           MOVE SPACES                 TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE

      *    Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *    nome_modulo -RPST
           MOVE NM-PGM        TO NM-CTNR(1:8)
           MOVE '-RQSC'       TO NM-CTNR(9:5)

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Faz put da requisicao do programa a executar
           EVALUATE NM-PGM
               WHEN SGTSI061
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (SGTSI061-RQSC           )
                              FLENGTH   (LENGTH OF SGTSI061-RQSC )
                    END-EXEC

               WHEN SGTSE061
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (SGTSE061-RQSC           )
                              FLENGTH   (LENGTH OF SGTSE061-RQSC )
                    END-EXEC

               WHEN SGTSL061
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (SGTSL061-RQSC           )
                              FLENGTH   (LENGTH OF SGTSL061-RQSC )
                    END-EXEC

               WHEN OTHER
                    MOVE 9006    TO MSG1-CD-ERRO-TS
                    STRING
                      'Não definida a execução do módulo.'
                       NM-PGM  DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
                    END-STRING
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

           MOVE 0020                   TO MSG1-CD-SEQL-ERRO

      *    Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *    nome_modulo -RPST

           MOVE NM-PGM        TO NM-CTNR(1:8)
           MOVE '-RPST'       TO NM-CTNR(9:5)

           SET DEL-CONTAINER           TO TRUE

      *    Exclui o container de resposta
      *    Não há captura automática de erro CICS por usar RESP e RESP2
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
                    SET ERRO-CONTAINER TO TRUE
                    PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE

      *    Excluir o container de erro
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO
           .
       983000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       983100-EXCLUIR-CONTAINER-ERRO   SECTION.
      *----------------------------------------------------------------*

      *    Limpar variáveis container de erro
           INITIALIZE IIBKERRO-PRM-ERRO

      *    Exclui o container de erro
           MOVE 0021                   TO MSG1-CD-SEQL-ERRO
           MOVE 'PRM-ERRO'             TO NM-CTNR
           SET DEL-CONTAINER           TO TRUE

      *    Não há captura automática de erro CICS por usar RESP e RESP2
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
                    SET ERRO-CONTAINER TO TRUE
                    PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       983100-SAI.
           EXIT.

      *----------------------------------------------------------------*
       984000-GRAVAR-CONTAINER-ERRO    SECTION.
      *----------------------------------------------------------------*
           MOVE 0022                   TO MSG1-CD-SEQL-ERRO

      *    Grava o erro gerado no container de erro
      *    Não há captura automática de erro CICS por usar RESP e RESP2
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
      *----------------------------------------------------------------*

           IF  MSG1-CD-ERRO-TS EQUAL ZEROS
               MOVE 8888                TO MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS EQUAL SPACES
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
           MOVE 'SGTS0089-CD-CTRA-TIT-VL'       TO MSGKW999-NM-VRV-MNT
           MOVE SGTS0089-CD-CTRA-TIT-VL         TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0089-CD-OPR-ETRD-MCP'   TO MSGKW999-NM-VRV-MNT
      *----> Verifica se o conteudo e numerico antes de adicionar
           IF  SGTS0089-CD-OPR-ETRD-MCP  IS NUMERIC
               MOVE  SGTS0089-CD-OPR-ETRD-MCP   TO  CD-OPR-ETRD-MCP
               MOVE  CD-OPR-ETRD-MCP-TX         TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico.'      TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0089-NR-VRS-OPR-ETRD'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTS0089-NR-VRS-OPR-ETRD     TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0089-QT-OCR-TAB'        TO MSGKW999-NM-VRV-MNT
           MOVE SGTS0089-QT-OCR-TAB          TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT


           IF  SGTS0089-QT-OCR-TAB > ZEROS
               PERFORM    VARYING IDX FROM 1 BY 1
                          UNTIL IDX   >  SGTS0089-QT-OCR-TAB

                  MOVE 'Indice'                   TO MSGKW999-NM-VRV-MNT
                  MOVE IDX                        TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTS0089-DT-RPAC-PGMD'    TO MSGKW999-NM-VRV-MNT
                  MOVE SGTS0089-DT-RPAC-PGMD(IDX) TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'SGTS0089-TX-MTV-RPAC-PGMD'
                                                 TO MSGKW999-NM-VRV-MNT
                  MOVE SGTS0089-TX-MTV-RPAC-PGMD(IDX)
                                                 TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT
               END-PERFORM
           END-IF


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
      *
      *---------------------- FIM SGTS0089 -----------------------------