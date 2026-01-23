      *-----------------------------------------------------------------
      * PROGRAMA..: SGTS0139
      * ANALISTA..: F2054109 - Cintia Ap Soares Iglesias
      * AUTOR.....: F4535118 - Ivone de Melo Costa
      * DATA......: 03/03/2017
      * OBJETIVO..: Calcular Parâmetros de Carência
      * Tipo de comunicação: Container
      * COMPILACAO: COBOL/CICS
      *-----------------------------------------------------------------
      * VRS001 23/01/2019 F8785286 DE 2016/145195 - Inclusão monitoração
      *              variáveis para rotina SBDATA
      *        06/02/2017 - F4535118 2016/145195 - Implantação
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID. SGTS0139.
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

       77  CTE-INICIO-SS               PIC  X(040)  VALUE
                    '*** S.S. COMECA AQUI ***'.

       77  CTE-PROG                    PIC  X(018)  VALUE
                    '*** SGTS0139 ***'.
       77  CTE-VERS                    PIC  X(006)  VALUE 'VRS001'.

      * Módulos acionados
       77  IIBP2000                    PIC  X(008)  VALUE 'IIBP2000'.
       77  SGTST005                    PIC  X(008)  VALUE 'SGTST005'.
       77  SBDATAF                     PIC  X(008)  VALUE 'SBDATAF'.
      *----------------------------------------------------------------*
       LOCAL-STORAGE                   SECTION.
      *----------------------------------------------------------------*

      * Áreas de trabalho

       01  CD-OPR-ETRD-MCP-DISPLAY     PIC 9(011) VALUE 0.

      *    Chamada  subrotina SBDATAF
      * F26 - ADICIONAR MESES A UMA DATA BASE
       01  AREA-SBDATA-F26.
           03  FUNCAO-F26              PIC  X(3)  VALUE 'F26'.
      * ==> Informar a data base
           03  ARG01-F26               PIC  9(8)  VALUE 0.
           03  ARG01-F26-R REDEFINES ARG01-F26.
               05  DIA                 PIC  9(2).
               05  MES                 PIC  9(2).
               05  ANO                 PIC  9(4).
      * ==> Quantidade de meses a ser adcionado
           03  ARG02-F26               PIC  9(3)  VALUE 0.
      * ==> Data Calculada
           03  ARG03-F26               PIC  9(8)  VALUE 0.
           03  ARG03-F26-R REDEFINES ARG03-F26.
               05  DIA                 PIC  9(2).
               05  MES                 PIC  9(2).
               05  ANO                 PIC  9(4).


      * F05 - CALCULAR DATA FINAL = DATA BASE +/- (QUANTIDADE DE DIAS)
       01  AREA-SBDATA-F05.
           03  FUNCAO-F05              PIC  X(3)  VALUE 'F05'.
      * ==> Informar a data base
           03  ARG01-F05               PIC  9(8)  VALUE 0.
           03  ARG01-F05-R REDEFINES ARG01-F05.
               05  DIA                 PIC  9(2).
               05  MES                 PIC  9(2).
               05  ANO                 PIC  9(4).
      * ==> Quantidade de dias a ser adicionado ou subtraído
           03  ARG02-F05               PIC  9(5)  VALUE 0.
      * ==> Opção a ser executado ( 1 - Adiciona / 2 - Subtrai)
           03  ARG03-F05               PIC  9(1)  VALUE 0.
      * ==> Data Calculada
           03  ARG04-F05               PIC  9(8)  VALUE 0.
           03  ARG04-F05-R REDEFINES ARG04-F05.
               05  DIA                 PIC  9(2).
               05  MES                 PIC  9(2).
               05  ANO                 PIC  9(4).



       01 WKSBDATA                     PIC X(4099).
       01 AREA-RED REDEFINES WKSBDATA.
          03  AREA-TAM                 PIC 9(0008) COMP.
          03  FILLER                   PIC X(4071).
          03  A-FIM                    PIC X(0020).

      *----------------------------------------------------------------*
      * Book dos Containers e das subrotinas chamadas
      *----------------------------------------------------------------*

      * Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1

      * Book para captura de abend cics da bloco código MSGKP999
-INC HLPKW999

      * Book para captura de erro via MSG1
-INC MSGKWMG1

      * Book do container de requisição e resposta deste módulo
       01 SGTS0139-DADOS.
-INC SGTK0139

      * CRUD OPR_ETRD_MCP
       01 SGTST005-DADOS.
-INC SGTKT005

      * Book do Container de Erro
-INC IIBKERRO

       01  CTE-FINAL-SS                PIC X(40)      VALUE
                    '          *** S.S. TERMINA AQUI ***'.
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
       000000-ROTINA-PRINCIPAL         SECTION.
      *----------------------------------------------------------------*

           PERFORM 100000-BUSCA-REQUISICAO

           PERFORM 200000-PROCESSAR

           PERFORM 990000-ENCERRAR
           .
       000000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       100000-BUSCA-REQUISICAO         SECTION.
      *----------------------------------------------------------------*
           MOVE 0001                   TO MSG1-CD-SEQL-ERRO

      *    Ativa a captura automática de abend do CICS.
      *    WHENEVER CICS
-INC HLPKE999

           PERFORM 970000-LER-CANAL

      *    Define o nome do programa p/ fins manipulação container
           MOVE 'SGTS0139'             TO NM-PGM
           MOVE 'SGTS0139-RPST'        TO NM-CTNR

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

      *    Validação tamanho esperado da requisição X tamanho recebido
      *    na section de leitura de container de requisição 971000

      *    Valida campo a campo da requisição

           IF  SGTS0139-CD-CTRA-TIT-VL <= ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0001               TO MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' inválido.'      TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 998900-ERRO
           END-IF

           IF  SGTS0139-CD-OPR-ETRD-MCP IS NOT NUMERIC
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0003               TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -            ' numérico.'        TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 998900-ERRO
           END-IF

           IF  SGTS0139-CD-OPR-ETRD-MCP EQUAL ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0005               TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -             ' informado.'      TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 998900-ERRO
           END-IF

           IF  SGTS0139-NR-VRS-OPR-ETRD  <= ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0006               TO MSG1-CD-ERRO-TS
               MOVE 'Número da versão da operação estruturada inválido.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 998900-ERRO
           END-IF

           IF  SGTS0139-QT-CARE-MM     < ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0439               TO MSG1-CD-ERRO-TS
               MOVE 'Quantidade de meses de carência inválida.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 998900-ERRO
           END-IF

           .
       110000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       200000-PROCESSAR                SECTION.
      *----------------------------------------------------------------*
           MOVE 0003                   TO MSG1-CD-SEQL-ERRO

           PERFORM 210000-CONSULTAR-OPERACAO

           PERFORM 220000-TRATAR-DATAS

           PERFORM 980000-GRAVAR-CONTAINER-RPST
           .
       200000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       210000-CONSULTAR-OPERACAO       SECTION.
      *----------------------------------------------------------------*
           MOVE 0004                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTST005-CHAVE.

      *    Movimentar o nome do programa a ser executado
           MOVE 'SGTST005'             TO NM-PGM

      *    Movimentar os campos de entrada

           MOVE  4                     TO  SGTST005-ACAO

           MOVE  SGTS0139-CD-CTRA-TIT-VL
                                       TO  SGTST005-CD-CTRA-TIT-VL
           MOVE  SGTS0139-CD-OPR-ETRD-MCP
                                       TO  SGTST005-CD-OPR-ETRD-MCP
           MOVE  SGTS0139-NR-VRS-OPR-ETRD
                                       TO  SGTST005-NR-VRS-OPR-ETRD

           PERFORM   920000-CHAMAR-SGTST005
           .
       210000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       220000-TRATAR-DATAS    SECTION.
      *----------------------------------------------------------------*
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

           IF SGTST005-DT-EMS-TIT-OPR EQUAL SPACES
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0539               TO MSG1-CD-ERRO-TS
               MOVE 'Data de emissão não cadastrada.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 998900-ERRO

           END-IF

           IF SGTST005-DT-VNCT-OPR EQUAL SPACES
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0540               TO MSG1-CD-ERRO-TS
               MOVE 'Data de vencimento não cadastrada.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 998900-ERRO

           END-IF

           IF  SGTS0139-QT-CARE-MM  NOT EQUAL ZEROS
               PERFORM  221000-ADICIONAR-DIAS
               MOVE CORR  ARG04-F05-R      TO  ARG01-F26-R
           ELSE
               MOVE CORR SGTST005-DT-EMS-TIT-OPR-R
                                           TO  ARG01-F26-R
           END-IF

           MOVE  SGTS0139-QT-CARE-MM   TO  ARG02-F26

           PERFORM  222000-CALCULAR-DATAS

           MOVE  CORR  ARG03-F26-R     TO  SGTS0139-DT-INC-EFT-R

           MOVE  CORR  SGTST005-DT-VNCT-OPR-R
                                       TO  SGTS0139-DT-FIM-EFT-R
           .
       220000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       221000-ADICIONAR-DIAS                  SECTION.
      *----------------------------------------------------------------*
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

           MOVE CORR SGTST005-DT-EMS-TIT-OPR-R
                                       TO  ARG01-F05-R
           MOVE 1                      TO  ARG02-F05
           MOVE 1                      TO  ARG03-F05

           MOVE 4095                   TO AREA-TAM.
           MOVE 'SBDATAF -WRKAREA-FIM' TO A-FIM.

           CALL SBDATAF          USING FUNCAO-F05
                                       WKSBDATA
                                       ARG01-F05 ARG02-F05
                                       ARG03-F05 ARG04-F05.

      *--> Return code = 24: Data Inválida.

           IF  RETURN-CODE             NOT EQUAL ZEROS AND 24 AND 22
                                                       AND 46
               SET ERRO-PROGRAMA-ACIONADO TO TRUE
               MOVE 0051               TO MSG1-CD-ERRO-TS
               MOVE RETURN-CODE        TO MSG1-CD-RTN-PGM-ACND
               MOVE 'Erro no tratamento de datas.'
                                       TO MSG1-TX-RTN-PGM-ACND
                                          MSG1-TX-ERRO-TS
               MOVE SBDATAF            TO MSG1-NM-PGM-ACND
                                          NM-PGM
      *        Monitorar variáveis do módulo chamado
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 998900-ERRO

           END-IF

           IF  RETURN-CODE             EQUAL 24
               SET ERRO-PROGRAMA-ACIONADO TO TRUE
               MOVE 0049               TO MSG1-CD-ERRO-TS
               MOVE RETURN-CODE        TO MSG1-CD-RTN-PGM-ACND
               MOVE 'Data informada inválida.'
                                       TO MSG1-TX-ERRO-TS
                                          MSG1-TX-ERRO-TS
               MOVE SBDATAF            TO MSG1-NM-PGM-ACND
                                          NM-PGM
      *        Monitorar variáveis do módulo chamado
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 998900-ERRO
           END-IF
           .
       221000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       222000-CALCULAR-DATAS               SECTION.
      *----------------------------------------------------------------
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

           MOVE 4095                   TO AREA-TAM.
           MOVE 'SBDATAF -WRKAREA-FIM' TO A-FIM.

           CALL SBDATAF          USING FUNCAO-F26
                                       WKSBDATA
                                       ARG01-F26 ARG02-F26 ARG03-F26.

      *--> Return code = 24: Data Inválida.

           IF  RETURN-CODE             NOT EQUAL ZEROS AND 24 AND 22
                                                       AND 46
               SET ERRO-PROGRAMA-ACIONADO TO TRUE
               MOVE 0051               TO MSG1-CD-ERRO-TS
               MOVE RETURN-CODE        TO MSG1-CD-RTN-PGM-ACND
               MOVE 'Erro no tratamento de datas.'
                                       TO MSG1-TX-RTN-PGM-ACND
                                          MSG1-TX-ERRO-TS
               MOVE SBDATAF            TO MSG1-NM-PGM-ACND
                                          NM-PGM

      *        Monitorar variáveis do módulo chamado
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 998900-ERRO

           END-IF

           IF  RETURN-CODE             EQUAL 24
               SET ERRO-PROGRAMA-ACIONADO TO TRUE
               MOVE 0049               TO MSG1-CD-ERRO-TS
               MOVE 'Data informada inválida.'
                                       TO MSG1-TX-ERRO-TS
                                          MSG1-TX-ERRO-TS
               MOVE SBDATAF            TO MSG1-NM-PGM-ACND
                                          NM-PGM

      *        Monitorar variáveis do módulo chamado
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 998900-ERRO
           END-IF
           .
       222000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       920000-CHAMAR-SGTST005          SECTION.
      *----------------------------------------------------------------*
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

           MOVE  SGTST005              TO  NM-PGM
           EXEC CICS LINK
                PROGRAM    ( SGTST005                 )
                COMMAREA   ( SGTST005-DADOS           )
                LENGTH     ( LENGTH OF SGTST005-DADOS )
           END-EXEC

           IF  SGTST005-CD-RTN-PGM     NOT EQUAL ZEROS
               SET ERRO-PROGRAMA-ACIONADO
                                       TO TRUE
               MOVE 3004               TO MSG1-CD-ERRO-TS
               MOVE 'Erro ao acionar o módulo.'
                                       TO MSG1-TX-ERRO-TS
               MOVE NM-PGM             TO MSG1-NM-PGM-ACND
               MOVE SGTST005-CD-RTN-PGM
                                       TO MSG1-CD-RTN-PGM-ACND
               MOVE SGTST005-MSG-ERRO-PGM
                                       TO MSG1-TX-RTN-PGM-ACND

      *        Monitorar variáveis do módulo chamado
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 998900-ERRO
           END-IF

           .
       920000-SAI.
           EXIT.
      *----------------------------------------------------------------
       969000-MONITORAR-VARIAVEIS      SECTION.
      *----------------------------------------------------------------*
      * Monitora as variáveis dos módulos chamados
      *----------------------------------------------------------------*
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO

           EVALUATE NM-PGM

               WHEN SBDATAF
                    MOVE 'FUNCAO-F05'  TO MSGKW999-NM-VRV-MNT
                    MOVE FUNCAO-F05    TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'ARG01-F05'   TO MSGKW999-NM-VRV-MNT
                    MOVE ARG01-F05     TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'ARG02-F05'   TO MSGKW999-NM-VRV-MNT
                    MOVE ARG02-F05     TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'ARG03-F05'   TO MSGKW999-NM-VRV-MNT
                    MOVE ARG03-F05     TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'ARG04-F05'   TO MSGKW999-NM-VRV-MNT
                    MOVE ARG04-F05     TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'FUNCAO-F26'  TO MSGKW999-NM-VRV-MNT
                    MOVE FUNCAO-F26    TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'ARG01-F26 '  TO MSGKW999-NM-VRV-MNT
                    MOVE ARG01-F26     TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'ARG02-F26'   TO MSGKW999-NM-VRV-MNT
                    MOVE ARG02-F26     TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'ARG03-F26'   TO MSGKW999-NM-VRV-MNT
                    MOVE ARG03-F26     TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTST005
                    MOVE 'SGTST005-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST005-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST005-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
                    IF  SGTST005-CD-OPR-ETRD-MCP IS NUMERIC
                       MOVE SGTST005-CD-OPR-ETRD-MCP
                                       TO CD-OPR-ETRD-MCP-DISPLAY
                       MOVE CD-OPR-ETRD-MCP-DISPLAY
                                       TO MSGKW999-VL-VRV-MNT
                    ELSE
                       MOVE 'Código de Operação Estruturada deve ser núm
      -                     'ero.'
                                       TO MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST005-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST005-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
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
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO

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

           MOVE 0011                   TO MSG1-CD-SEQL-ERRO

      *    Monta o nome do container de requisição
           MOVE 'SGTS0139'             TO NM-PGM
           MOVE 'SGTS0139-RQSC'        TO NM-CTNR

      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Obtem o tamanho esperado da requisição
           MOVE LENGTH OF SGTS0139-RQSC TO QT-CCT-EPRO

           MOVE ZEROS                  TO QT-CCT-LIDO

      *    Obtem a quantidade de caracteres no container
           EXEC CICS GET
                     CONTAINER (NM-CTNR)
                     NODATA
                     FLENGTH   (QT-CCT-LIDO)
           END-EXEC

      *    Valida tamanho esperado da requisição X tamanho recebido
           IF  QT-CCT-LIDO             NOT EQUAL QT-CCT-EPRO
               SET ERRO-TAMANHO-AREA   TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           MOVE 0012                   TO MSG1-CD-SEQL-ERRO

      *    Efetua a leitura do container
           EXEC CICS GET
                     CONTAINER (NM-CTNR)
                     FLENGTH   (QT-CCT-LIDO)
                     INTO      (SGTS0139-RQSC)
           END-EXEC

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       971000-SAI.
           EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      * SECTION DA FAIXA DE 980000 A 989900 PARA GRAVACAO DE DADOS     *
      *                                                                *
      *----------------------------------------------------------------*
       980000-GRAVAR-CONTAINER-RPST    SECTION.
      *----------------------------------------------------------------*

           MOVE 0013                   TO MSG1-CD-SEQL-ERRO

      *    Exclui eventual container de erro de outros programas
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO

      *    Monta o nome do container de resposta
           MOVE 'SGTS0139-RPST'        TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

           EXEC CICS PUT
                     CONTAINER (NM-CTNR                    )
                     FROM      (SGTS0139-RPST          )
                     FLENGTH   (LENGTH OF SGTS0139-RPST)
           END-EXEC

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       980000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       983000-EXCLUIR-RESPOSTA-ERRO    SECTION.
      *----------------------------------------------------------------*
      * Exclui o container de resposta e de erro para evitar problemas *
      * em módulos executados mais de uma vez dentro da mesma sessão do*
      * CICS. Equivale ao initialize na área de resposta dos programas *
      * que utilizam COMMAREA.                                         *
      *----------------------------------------------------------------*

           MOVE 0014                   TO MSG1-CD-SEQL-ERRO

           MOVE NM-PGM        TO NM-CTNR(1:8)
           MOVE '-RPST'       TO NM-CTNR(9:5)

           SET DEL-CONTAINER           TO TRUE

      *    Exclui o container de resposta
      *    Não há captura automática de erro CICS por usar RESP e
      *    RESP2   VRS001
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
           MOVE 0015                   TO MSG1-CD-SEQL-ERRO
           MOVE 'PRM-ERRO'             TO NM-CTNR
           SET DEL-CONTAINER           TO TRUE

      *    Não há captura automática de erro CICS por usar RESP e
      *    RESP2   VRS001
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
      *----------------------------------------------------------------*
       984000-GRAVAR-CONTAINER-ERRO    SECTION.
      *----------------------------------------------------------------*
           MOVE 0016                   TO MSG1-CD-SEQL-ERRO

      *    Grava o erro gerado no container de erro
      *    Não há captura automática de erro CICS por usar RESP e
      *    RESP2   VRS001
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
       990000-ENCERRAR                 SECTION.
      *----------------------------------------------------------------*

           EXEC CICS
                RETURN
           END-EXEC
           .
       990000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
      *                                                                *
      * As sections abaixo iniciadas com 999000 representam os tratamen*
      * tos de erro utilizando o Sistema MSG (rotina MSGS0001)         *
      *                                                                *
      *----------------------------------------------------------------*
       998900-ERRO                     SECTION.
      *----------------------------------------------------------------*
           MOVE 0017                   TO MSG1-CD-SEQL-ERRO

           IF MSG1-CD-ERRO-TS EQUAL ZEROS
              MOVE 8888                TO MSG1-CD-ERRO-TS
           END-IF

           IF MSG1-TX-ERRO-TS EQUAL SPACES
              MOVE 'Erro não específicado no módulo.' TO MSG1-TX-ERRO-TS
           END-IF

           PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           .
       998900-SAI.
           EXIT.

      *----------------------------------------------------------------*
      *     As sections abaixo representam os tratamentos de erro
      *    utilizando o sistema MSG de mensageria (rotina MSGS0001)
      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------

           MOVE 0018                   TO MSG1-CD-SEQL-ERRO

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
           IF  MNT-VRV
               MOVE 'SGTS0139-CD-CTRA-TIT-VL '
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0139-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0139-CD-OPR-ETRD-MCP'
                                       TO MSGKW999-NM-VRV-MNT
               IF SGTS0139-CD-OPR-ETRD-MCP IS NUMERIC
                  MOVE SGTS0139-CD-OPR-ETRD-MCP
                                       TO CD-OPR-ETRD-MCP-DISPLAY
                  MOVE CD-OPR-ETRD-MCP-DISPLAY
                                       TO MSGKW999-VL-VRV-MNT
               ELSE
                  MOVE 'Código de Operação Estruturada deve ser número.'
                                       TO MSGKW999-VL-VRV-MNT
               END-IF

               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0139-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE SGTS0139-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0139-QT-CARE-MM'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE SGTS0139-QT-CARE-MM
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

           END-IF

      *    Grava o erro gerado no sistema MSG - Mensageria
           PERFORM MSG1-NTF-MSG

           MOVE MSG1-CD-ERRO-TS        TO IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS        TO IIBKERRO-TX-ERRO

      *    Em caso de abend da MSGS001, adiciona o nome da rotina,
      *    código do erro, retornos cics e nível execução
           IF MSG1-CD-ERRO NOT EQUAL ZEROS
              STRING
                 '(MSG1/' MSG1-CD-ERRO '/' MSG1-RET-RESP '/'
                 MSG1-RET-RESP2  '/' MSG1-NVL-EXEC ') '
                 MSG1-TX-ERRO-TS
                         DELIMITED BY SIZE INTO IIBKERRO-TX-ERRO
              END-STRING
           END-IF

      *    Grava o erro gerado no container de erro
           PERFORM 984000-GRAVAR-CONTAINER-ERRO

      *    Encerra a transação
           PERFORM 990000-ENCERRAR
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
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *---------------------------------------*
-INC SGTKP999
      * --------------------------------------*
      * MSG1-NTF-MSG                   SECTION.
      * --------------------------------------*
-INC MSGKPMG1
      *---------------------- FIM SGTS0139 ----------------------------