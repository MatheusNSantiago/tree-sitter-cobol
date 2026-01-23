      *-----------------------------------------------------------------  VRS001
      * PROGRAMA..: SGTS0091                                              VRS001
      * ANALISTA..: F0175702 - ADRIANO SEIJI NAKAMURA                     VRS001
      * AUTOR.....: F0175702 - ADRIANO SEIJI NAKAMURA                     VRS001
      * DATA......: 04/11/2016                                            VRS001
      * OBJETIVO..: Atualizar Bancos participantes da Operao
      * COMPILACAO: COBOL/CICS                                            VRS001
      *-----------------------------------------------------------------  VRS001
      * VRS001 11/05/2018 F8658869 DE 2015/137233 - mensageria e inclu-
      *                   são da carteira.
      *        07/02/2017 F0175702 DE 2015/137233 - Recompilado SGTS0081
      *        04/11/2016 F0175702 DE 2015/137233 - IMPLANTACAO           VRS001
      *-----------------------------------------------------------------  VRS001
                                                                          VRS001
      *-----------------------------------------------------------------  VRS001
       IDENTIFICATION DIVISION.                                           VRS001
      *----------------------------------------------------------------*  VRS001
       PROGRAM-ID. SGTS0091.                                              VRS001
      *                                                                   VRS001
      *----------------------------------------------------------------*  VRS001
       ENVIRONMENT DIVISION.                                              VRS001
      *----------------------------------------------------------------*  VRS001
      *                                                                   VRS001
      *----------------------------------------------------------------*  VRS001
       CONFIGURATION                   SECTION.                           VRS001
      *----------------------------------------------------------------*  VRS001
       SPECIAL-NAMES.                                                     VRS001
           DECIMAL-POINT IS COMMA.                                        VRS001
      *                                                                   VRS001
      *----------------------------------------------------------------*  VRS001
       DATA DIVISION.                                                     VRS001
      *----------------------------------------------------------------*  VRS001
      *                                                                   VRS001
                                                                          VRS001
       WORKING-STORAGE                 SECTION.                           VRS001
      *----------------------------------------------------------------*  VRS001
                                                                          VRS001
       77  CTE-INICIO-SS                 PIC  X(040)  VALUE               VRS001
                    '*** S.S. COMEÇA AQUI ***'.                           VRS001
                                                                          VRS001
       77  CTE-PROG                      PIC  X(018)  VALUE               VRS001
                    '*** SGTS0091 ***'.                                   VRS001
       77  CTE-VERS                      PIC  X(006)  VALUE 'VRS001'.     VRS001
                                                                          VRS001
      * Módulos acionados                                                 VRS001
       77  SGTSI064                      PIC  X(008) VALUE 'SGTSI064'.    VRS001
       77  SGTSE064                      PIC  X(008) VALUE 'SGTSE064'.    VRS001
       77  SGTSL064                      PIC  X(008) VALUE 'SGTSL064'.
       77  SGTS0081                      PIC  X(008) VALUE 'SGTS0081'.
      *----------------------------------------------------------------*  VRS001
       LOCAL-STORAGE                   SECTION.                           VRS001
      *----------------------------------------------------------------*  VRS001
                                                                          VRS001
      * Areas de trabalho                                                 VRS001
       01  IDX                           PIC S9(4) COMP-5 VALUE 0.
       01  QT-BCO-LDR                    PIC S9(4) COMP-5 VALUE 0.
       01  CD-BCO-PCT                    PIC S9(4) COMP-5 VALUE 0.

       01  AX-VL-4V4                     PIC  --.--9,9999.
       01  AX-CD-BCO-PCT                 PIC  X(1) VALUE SPACES.
       01  AX-IN-BCO-LDR                 PIC  X(1) VALUE SPACES.
       01  AX-PC-PRTC-BCO                PIC  X(1) VALUE SPACES.

      *--> Auxiliares para movimentação na monitoração de variáveis.
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).

      *--> Campo para definição de estado de erro em tabela.
       01  CD-ERRO-TAB                 PIC  9(01)             VALUE 0.
           88 COM-ERRO-TAB                                    VALUE 1.

      *----------------------------------------------------------------*  VRS001
      * Book dos Containers e das subrotinas chamadas                     VRS001
      *----------------------------------------------------------------*  VRS001
                                                                          VRS001
      * Book das variáveis auxiliares do tratamento de erro via MSG       VRS001
-INC SGTKERR1                                                           00001140
      * Book para captura de abend cics da bloco codigo MSGKP999          VRS001
-INC HLPKW999                                                           00001160
      * Book para captura de erro via MSG1                                VRS001
-INC MSGKWMG1                                                           00001190
                                                                          VRS001
      * Book do container de requisição e resposta deste módulo           VRS001
       01 SGTS0091-DADOS.                                                 VRS001
-INC SGTK0091                                                           00001230
                                                                          VRS001
      * Inclusão na BCO_PCT_OPR                                           VRS001
-INC SGTKI064                                                           00001270
                                                                          VRS001
      * Excluir Tabela BCO_PCT_OPR                                        VRS001
-INC SGTKE064
      * Listar Tabela BCO_PCT_OPR                                         VRS001
-INC SGTKL064
      * Verifica se estado permite alteração                              VRS001
-INC SGTK0081
                                                                          VRS001
      * Book do Container de Erro                                         VRS001
-INC IIBKERRO                                                           00001370
      * Book do container de Serviços (para acionamento de serviços)      VRS001
                                                                        00001390
      * Book dos dados do usuário                                         VRS001
-INC IIBKOPRD                                                           00001410
                                                                          VRS001
       01  CTE-FINAL-SS                  PIC X(40)      VALUE             VRS001
                    '*** S.S. TERMINA AQUI ***'.                          VRS001
                                                                          VRS001
      *----------------------------------------------------------------*  VRS001
       PROCEDURE DIVISION.                                                VRS001
      *----------------------------------------------------------------*  VRS001
                                                                          VRS001
      *----------------------------------------------------------------*  VRS001
       000000-ROTINA-PRINCIPAL         SECTION.                           VRS001
      *----------------------------------------------------------------*  VRS001
                                                                          VRS001
           PERFORM 100000-INICIAR                                         VRS001
                                                                          VRS001
           PERFORM 200000-PROCESSAR                                       VRS001
                                                                          VRS001
           PERFORM 999999-ENCERRAR                                        VRS001
           .                                                              VRS001
       000000-SAI.
           EXIT.
                                                                          VRS001
      *----------------------------------------------------------------*  VRS001
       100000-INICIAR                  SECTION.                           VRS001
      *----------------------------------------------------------------*  VRS001
                                                                          VRS001
           MOVE 0001                   TO MSG1-CD-SEQL-ERRO               VRS001
                                                                          VRS001
      *    Ativa a captura automática de abend do CICS.                   VRS001
      *    WHENEVER CICS                                                  VRS001
-INC HLPKE999                                                           00001720
                                                                          VRS001
           PERFORM 970000-LER-CANAL                                       VRS001
                                                                          VRS001
      *    Define o nome do programa p/ fins manipulação container        VRS001
           MOVE 'SGTS0091'             TO NM-PGM                          VRS001
                                                                          VRS001
      *    Limpa a resposta e erro antes de executar o módulo             VRS001
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO                           VRS001
                                                                          VRS001
           PERFORM 971000-LER-REQUISICAO                                  VRS001
                                                                          VRS001
           PERFORM 974000-LER-DADOS-OPRD
                                                                          VRS001
           PERFORM 110000-VALIDAR-REQUISICAO                              VRS001
           .                                                              VRS001
       100000-SAI.                                                        VRS001
           EXIT.                                                          VRS001
                                                                          VRS001
      *----------------------------------------------------------------*  VRS001
       110000-VALIDAR-REQUISICAO       SECTION.                           VRS001
      *----------------------------------------------------------------*  VRS001
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO               VRS001
                                                                          VRS001
      *    Validação tamanho esperado da requisicao X tamanho recebido    VRS001
      *    na section de leitura de container de requisição 971000        VRS001
                                                                          VRS001
      *    Valida campo a campo da requisicao

           IF  SGTS0091-CD-CTRA-TIT-VL < ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0001               TO MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de título e valores mobiliários
      -             'inválido.'        TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0091-CD-CTRA-TIT-VL EQUAL ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0002               TO MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de título e valores mobiliários
      -             'não informado.'   TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF
                                                                          VRS001
           IF  SGTS0091-CD-OPR-ETRD-MCP NOT NUMERIC                       VRS001
               SET ERRO-PROGRAMADO     TO TRUE                            VRS001
               MOVE 0003               TO MSG1-CD-ERRO-TS                 VRS001
               MOVE 'Código operação estruturada mercado capital não num
      -             'érico.'           TO MSG1-TX-ERRO-TS                 VRS001
               SET MNT-VRV             TO TRUE                            VRS001
               PERFORM 999000-ERRO                                        VRS001
           END-IF                                                         VRS001

           IF  SGTS0091-CD-OPR-ETRD-MCP < ZEROS                           VRS001
               SET ERRO-PROGRAMADO     TO TRUE                            VRS001
               MOVE 0004               TO MSG1-CD-ERRO-TS                 VRS001
               MOVE 'Código operação estruturada mercado capital inválid
      -        'o.'                    TO MSG1-TX-ERRO-TS                 VRS001
               SET MNT-VRV             TO TRUE                            VRS001
               PERFORM 999000-ERRO                                        VRS001
           END-IF

           IF  SGTS0091-CD-OPR-ETRD-MCP EQUAL ZEROS                       VRS001
               SET ERRO-PROGRAMADO     TO TRUE                            VRS001
               MOVE 0004               TO MSG1-CD-ERRO-TS                 VRS001
               MOVE 'Código operação estruturada mercado capital não inf
      -        'ormado.'               TO MSG1-TX-ERRO-TS                 VRS001
               SET MNT-VRV             TO TRUE                            VRS001
               PERFORM 999000-ERRO                                        VRS001
           END-IF

           IF  SGTS0091-NR-VRS-OPR-ETRD <= ZEROS                          VRS001
               SET ERRO-PROGRAMADO     TO TRUE                            VRS001
               MOVE 0006               TO MSG1-CD-ERRO-TS                 VRS001
               MOVE 'Número da versão da operação estruturada inválido.'
                                       TO MSG1-TX-ERRO-TS                 VRS001
               SET MNT-VRV             TO TRUE                            VRS001
               PERFORM 999000-ERRO                                        VRS001
           END-IF
                                                                          VRS001
           PERFORM VARYING IDX FROM 1 BY 1
             UNTIL IDX GREATER SGTS0091-QT-OCR-TAB
             IF  SGTS0091-CD-BCO-PCT(IDX) = ZEROS                         VRS001
                 SET ERRO-PROGRAMADO   TO TRUE                            VRS001
                 MOVE 0134             TO MSG1-CD-ERRO-TS                 VRS001
                 MOVE 'Código do banco participante não informado.'       VRS001
                                       TO MSG1-TX-ERRO-TS                 VRS001
                 SET MNT-VRV           TO TRUE                            VRS001
                 SET COM-ERRO-TAB      TO TRUE
                 MOVE  'S'             TO AX-CD-BCO-PCT
                 PERFORM 999000-ERRO                                      VRS001
             END-IF

             IF  SGTS0091-IN-BCO-LDR (IDX) = 'S'
                 ADD 1                 TO QT-BCO-LDR
                   ON SIZE ERROR
                   MOVE 'Erro ao calcular total de banco lider.'
                                         TO MSG1-TX-ERRO-TS
                   SET ERRO-PROGRAMADO   TO TRUE                          VRS001
                   MOVE 0717             TO MSG1-CD-ERRO-TS               VRS001
                   SET MNT-VRV           TO TRUE
                   SET COM-ERRO-TAB      TO TRUE
                   MOVE  'S'             TO AX-IN-BCO-LDR
                   PERFORM 999000-ERRO
                 END-ADD
             END-IF

           END-PERFORM
           EVALUATE TRUE
           WHEN QT-BCO-LDR = 0
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 'Deve ser informado um banco lider.'
                                       TO MSG1-TX-ERRO-TS
               MOVE 0718               TO MSG1-CD-ERRO-TS
               SET MNT-VRV             TO TRUE                            VRS001
               PERFORM 999000-ERRO
           WHEN QT-BCO-LDR > 1
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 'Deve ser informado apenas um banco lider.'
                                       TO MSG1-TX-ERRO-TS
               MOVE 0719               TO MSG1-CD-ERRO-TS
               SET MNT-VRV             TO TRUE                            VRS001
               PERFORM 999000-ERRO
           END-EVALUATE
           .                                                              VRS001
       110000-SAI.                                                        VRS001
           EXIT.                                                          VRS001
                                                                          VRS001
      *----------------------------------------------------------------*  VRS001
       200000-PROCESSAR                SECTION.                           VRS001
      *----------------------------------------------------------------*  VRS001
           MOVE 0003                   TO MSG1-CD-SEQL-ERRO               VRS001
                                                                          VRS001
      *    Movimentar o nome do programa a ser executado                  VRS001
           MOVE SGTS0081               TO NM-PGM
      *    Verificar se a operação pode ser alterada
           MOVE SGTS0091-CD-CTRA-TIT-VL  TO SGTS0081-CD-CTRA-TIT-VL
           MOVE SGTS0091-CD-OPR-ETRD-MCP TO SGTS0081-CD-OPR-ETRD
           MOVE SGTS0091-NR-VRS-OPR-ETRD TO SGTS0081-NR-VRS-OPR-ETRD
           PERFORM 900000-EXECUTAR-MODULO

           IF  SGTS0081-IN-HBLT-MENU-OPR = 'N'
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 'Alteração não permitida para o estado da operação
      -        '.'                     TO MSG1-TX-ERRO-TS
               MOVE 0098               TO MSG1-CD-ERRO-TS
               SET MNT-VRV             TO TRUE                            VRS001
               PERFORM 999000-ERRO
           ELSE
               PERFORM 220000-TRATAR-SINDICATO-BCO
                 UNTIL SGTSL064-IN-CNTO-PSQ EQUAL 'N'
               PERFORM 230000-INCLUI-SINDICATO-BCO
                 VARYING IDX FROM 1 BY 1
                 UNTIL IDX GREATER SGTS0091-QT-OCR-TAB
           END-IF
           .                                                              VRS001
       200000-SAI.                                                        VRS001
           EXIT.                                                          VRS001
                                                                          VRS001
      *----------------------------------------------------------------*  VRS001
       220000-TRATAR-SINDICATO-BCO    SECTION.                            VRS001
      *----------------------------------------------------------------*  VRS001
           MOVE 0004                   TO MSG1-CD-SEQL-ERRO               VRS001
                                                                          VRS001
      *    Movimentar o nome do programa a ser executado                  VRS001
           MOVE SGTSL064               TO NM-PGM                          VRS001
                                                                          VRS001
      *    Movimentar os campos de entrada                                VRS001

           MOVE SGTS0091-CD-CTRA-TIT-VL  TO SGTSL064-CD-CTRA-TIT-VL
           MOVE SGTS0091-CD-OPR-ETRD-MCP TO SGTSL064-CD-OPR-ETRD-MCP
           MOVE SGTS0091-NR-VRS-OPR-ETRD TO SGTSL064-NR-VRS-OPR-ETRD
           MOVE 1                        TO SGTSL064-CD-TIP-PRTC
           MOVE CD-BCO-PCT               TO SGTSL064-CD-ULT-BCO-PCT
                                                                          VRS001
           PERFORM 900000-EXECUTAR-MODULO                                 VRS001

           IF  SGTSL064-IN-CNTO-PSQ EQUAL 'S'
               MOVE SGTSL064-CD-BCO-PCT(100) TO CD-BCO-PCT
           END-IF

           PERFORM 221000-PROCESSA-SAIDA
             VARYING SGTSL064-IC-TAB FROM 1 BY 1
             UNTIL SGTSL064-IC-TAB GREATER SGTSL064-QT-OCR-TAB
           .                                                              VRS001
       220000-SAI.                                                        VRS001
           EXIT.                                                          VRS001
                                                                          VRS001
      *----------------------------------------------------------------*  VRS001
       221000-PROCESSA-SAIDA     SECTION.                                 VRS001
      *----------------------------------------------------------------*  VRS001
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO               VRS001
                                                                          VRS001
           INITIALIZE                  SGTSE064-DADOS                     VRS001
                                                                          VRS001
      *    Movimentar os campos de entrada                                VRS001
           MOVE SGTS0091-CD-CTRA-TIT-VL  TO SGTSE064-CD-CTRA-TIT-VL
           MOVE SGTS0091-CD-OPR-ETRD-MCP TO SGTSE064-CD-OPR-ETRD-MCP
           MOVE SGTS0091-NR-VRS-OPR-ETRD TO SGTSE064-NR-VRS-OPR-ETRD
           MOVE 1                        TO SGTSE064-CD-TIP-PRTC
           MOVE SGTSL064-CD-BCO-PCT(SGTSL064-IC-TAB)
                                         TO SGTSE064-CD-BCO-PCT
           MOVE 'SGTSE064'             TO NM-PGM

           PERFORM 900000-EXECUTAR-MODULO                                 VRS001
           .                                                              VRS001
       221000-SAI.                                                        VRS001
           EXIT.                                                          VRS001

      *----------------------------------------------------------------*  VRS001
       230000-INCLUI-SINDICATO-BCO    SECTION.                            VRS001
      *----------------------------------------------------------------*  VRS001
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO               VRS001
                                                                          VRS001
           INITIALIZE                  SGTSI064-DADOS                     VRS001
                                                                          VRS001
      *    Movimentar o nome do programa a ser executado                  VRS001
           MOVE SGTSI064               TO NM-PGM                          VRS001
                                                                          VRS001
      *    Movimentar os campos de entrada                                VRS001

           MOVE SGTS0091-CD-CTRA-TIT-VL   TO SGTSI064-CD-CTRA-TIT-VL
           MOVE SGTS0091-CD-OPR-ETRD-MCP  TO SGTSI064-CD-OPR-ETRD-MCP
           MOVE SGTS0091-NR-VRS-OPR-ETRD  TO SGTSI064-NR-VRS-OPR-ETRD
           MOVE 1                         TO SGTSI064-CD-TIP-PRTC
           MOVE SGTS0091-CD-BCO-PCT(IDX)  TO SGTSI064-CD-BCO-PCT
           MOVE SGTS0091-IN-BCO-LDR(IDX)  TO SGTSI064-IN-BCO-LDR
           MOVE SGTS0091-PC-PRTC-BCO(IDX) TO SGTSI064-PC-PRTC-BCO
           MOVE KOPRD-CD-OPRD             TO SGTSI064-CD-USU-ULT-ATL
           MOVE 'CURRENT'                 TO SGTSI064-TS-ULT-ATL
           MOVE 'SGTSI064'                TO NM-PGM
                                                                          VRS001
           PERFORM 900000-EXECUTAR-MODULO                                 VRS001
           .                                                              VRS001
       230000-SAI.                                                        VRS001
           EXIT.                                                          VRS001

      *----------------------------------------------------------------*  VRS001
       900000-EXECUTAR-MODULO          SECTION.                           VRS001
      *----------------------------------------------------------------*  VRS001
                                                                          VRS001
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO               VRS001
      *    Grava o container de requisicao do modulo a executar           VRS001
           PERFORM 982000-GRAVAR-CONTAINER-RQSC                           VRS001
                                                                          VRS001
      *    Limpa a resposta e erro antes de executar o modulo             VRS001
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO                           VRS001
                                                                          VRS001
      *    No h captura automtica de erro CICS por usar RESP e RESP2   VRS001
           EVALUATE NM-PGM                                                VRS001
               WHEN SGTS0081                                              VRS001
                   EXEC CICS LINK                                         VRS001
                             PROGRAM ( SGTS0081  )                        VRS001
                             CHANNEL ( NM-CNL    )                        VRS001
                             RESP    ( MSG1-RESP1)                        VRS001
                             RESP2   ( MSG1-RESP2)                        VRS001
                   END-EXEC
               WHEN SGTSL064                                              VRS001
                   EXEC CICS LINK                                         VRS001
                             PROGRAM ( SGTSL064  )                        VRS001
                             CHANNEL ( NM-CNL    )                        VRS001
                             RESP    ( MSG1-RESP1)                        VRS001
                             RESP2   ( MSG1-RESP2)                        VRS001
                   END-EXEC
               WHEN SGTSE064                                              VRS001
                   EXEC CICS LINK                                         VRS001
                             PROGRAM ( SGTSE064  )                        VRS001
                             CHANNEL ( NM-CNL    )                        VRS001
                             RESP    ( MSG1-RESP1)                        VRS001
                             RESP2   ( MSG1-RESP2)                        VRS001
                   END-EXEC
               WHEN SGTSI064                                              VRS001
                   EXEC CICS LINK                                         VRS001
                             PROGRAM ( SGTSI064  )                        VRS001
                             CHANNEL ( NM-CNL    )                        VRS001
                             RESP    ( MSG1-RESP1)                        VRS001
                             RESP2   ( MSG1-RESP2)                        VRS001
                   END-EXEC
               WHEN OTHER
                   MOVE 9006 TO MSG1-CD-ERRO-TS                           VRS001
                   STRING                                                 VRS001
                   'Não definida a execução do módulo ' NM-PGM            VRS001
                   DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS                 VRS001
                   PERFORM 999000-ERRO                                    VRS001
           END-EVALUATE                                                   VRS001
      *                                                                   VRS001
           EVALUATE TRUE                                                  VRS001
               WHEN MSG1-RESP1        EQUAL DFHRESP(NORMAL)               VRS001
                    CONTINUE                                              VRS001
               WHEN OTHER                                                 VRS001
                    STRING                                                VRS001
                    'Erro na execução do módulo ' NM-PGM ', '             VRS001
                    'RESP  ='   ' ' MSG1-RESP1  ', '                      VRS001
                    'RESP2 =' ' ' MSG1-RESP2                              VRS001
                    DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS                VRS001
                    MOVE 3004          TO MSG1-CD-ERRO-TS                 VRS001
                    SET ERRO-CICS      TO TRUE                            VRS001
                    PERFORM 999000-ERRO                                   VRS001
           END-EVALUATE                                                   VRS001
                                                                          VRS001
      *    Efetua a leitura do Container de Erro                          VRS001
           PERFORM 972000-LER-CONTAINER-DE-ERRO                           VRS001
                                                                          VRS001
      *    Efetua a leitura do Container de Resposta                      VRS001
           PERFORM 973000-LER-RESPOSTA                                    VRS001
           .                                                              VRS001
       900000-SAI.                                                        VRS001
           EXIT.                                                          VRS001
                                                                          VRS001
      *----------------------------------------------------------------*  VRS001
       969000-MONITORAR-VARIAVEIS      SECTION.                           VRS001
      *----------------------------------------------------------------*  VRS001
      * Monitora as variveis dos mdulos chamados                        VRS001
      *----------------------------------------------------------------*  VRS001
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO               VRS001
                                                                          VRS001
           EVALUATE NM-PGM                                                VRS001
               WHEN SGTS0081
                    MOVE 'SGTS0081-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTS0081-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT
                                                                          VRS001
                    MOVE 'SGTS0081-CD-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT             VRS001
                    MOVE SGTS0081-CD-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT             VRS001
                    PERFORM 999003-ADICIONA-VRV-MNT                       VRS001
                                                                          VRS001
                    MOVE 'SGTS0081-NR-VRS-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT             VRS001
                    MOVE SGTS0081-NR-VRS-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT             VRS001
                    PERFORM 999003-ADICIONA-VRV-MNT                       VRS001
                                                                          VRS001
               WHEN SGTSL064                                              VRS001
                    MOVE 'SGTSL064-CD-CTRA-TIT-VL'                        VRS001
                                       TO MSGKW999-NM-VRV-MNT             VRS001
                    MOVE SGTSL064-CD-CTRA-TIT-VL                          VRS001
                                       TO MSGKW999-VL-VRV-MNT             VRS001
                    PERFORM 999003-ADICIONA-VRV-MNT                       VRS001
                                                                          VRS001
                    MOVE 'SGTSL064-CD-OPR-ETRD-MCP'                       VRS001
                                       TO MSGKW999-NM-VRV-MNT             VRS001
                    MOVE SGTSL064-CD-OPR-ETRD-MCP                         VRS001
                                       TO MSGKW999-VL-VRV-MNT             VRS001
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL064-NR-VRS-OPR-ETRD'                       VRS001
                                       TO MSGKW999-NM-VRV-MNT             VRS001
                    MOVE SGTSL064-NR-VRS-OPR-ETRD                         VRS001
                                       TO MSGKW999-VL-VRV-MNT             VRS001
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL064-CD-TIP-PRTC'                           VRS001
                                       TO MSGKW999-NM-VRV-MNT             VRS001
                    MOVE SGTSL064-CD-TIP-PRTC                             VRS001
                                       TO MSGKW999-VL-VRV-MNT             VRS001
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL064-CD-ULT-TIP-PRTC'                       VRS001
                                       TO MSGKW999-NM-VRV-MNT             VRS001
                    MOVE SGTSL064-CD-ULT-TIP-PRTC                         VRS001
                                       TO MSGKW999-VL-VRV-MNT             VRS001
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL064-CD-ULT-BCO-PCT'                        VRS001
                                       TO MSGKW999-NM-VRV-MNT             VRS001
                    MOVE SGTSL064-CD-ULT-BCO-PCT                          VRS001
                                       TO MSGKW999-VL-VRV-MNT             VRS001
                    PERFORM 999003-ADICIONA-VRV-MNT
                                                                          VRS001
               WHEN SGTSE064                                              VRS001
                    MOVE 'SGTSE064-CD-CTRA-TIT-VL'                        VRS001
                                       TO MSGKW999-NM-VRV-MNT             VRS001
                    MOVE SGTSE064-CD-CTRA-TIT-VL                          VRS001
                                       TO MSGKW999-VL-VRV-MNT             VRS001
                    PERFORM 999003-ADICIONA-VRV-MNT                       VRS001
                                                                          VRS001
                    MOVE 'SGTSE064-CD-OPR-ETRD-MCP'                       VRS001
                                       TO MSGKW999-NM-VRV-MNT             VRS001
                    MOVE SGTSE064-CD-OPR-ETRD-MCP                         VRS001
                                       TO MSGKW999-VL-VRV-MNT             VRS001
                    PERFORM 999003-ADICIONA-VRV-MNT                       VRS001

                    MOVE 'SGTSE064-NR-VRS-OPR-ETRD'                       VRS001
                                       TO MSGKW999-NM-VRV-MNT             VRS001
                    MOVE SGTSE064-NR-VRS-OPR-ETRD                         VRS001
                                       TO MSGKW999-VL-VRV-MNT             VRS001
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSE064-CD-TIP-PRTC'                           VRS001
                                       TO MSGKW999-NM-VRV-MNT             VRS001
                    MOVE SGTSE064-CD-TIP-PRTC                             VRS001
                                       TO MSGKW999-VL-VRV-MNT             VRS001
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSE064-CD-BCO-PCT'                            VRS001
                                       TO MSGKW999-NM-VRV-MNT             VRS001
                    MOVE SGTSE064-CD-BCO-PCT                              VRS001
                                       TO MSGKW999-VL-VRV-MNT             VRS001
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSI064                                              VRS001
                    MOVE 'SGTSI064-CD-CTRA-TIT-VL'                        VRS001
                                       TO MSGKW999-NM-VRV-MNT             VRS001
                    MOVE SGTSI064-CD-CTRA-TIT-VL                          VRS001
                                       TO MSGKW999-VL-VRV-MNT             VRS001
                    PERFORM 999003-ADICIONA-VRV-MNT                       VRS001
                                                                          VRS001
                    MOVE 'SGTSI064-CD-OPR-ETRD-MCP'                       VRS001
                                       TO MSGKW999-NM-VRV-MNT             VRS001
                    MOVE SGTSI064-CD-OPR-ETRD-MCP                         VRS001
                                       TO MSGKW999-VL-VRV-MNT             VRS001
                    PERFORM 999003-ADICIONA-VRV-MNT                       VRS001

                    MOVE 'SGTSI064-NR-VRS-OPR-ETRD'                       VRS001
                                       TO MSGKW999-NM-VRV-MNT             VRS001
                    MOVE SGTSI064-NR-VRS-OPR-ETRD                         VRS001
                                       TO MSGKW999-VL-VRV-MNT             VRS001
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI064-CD-TIP-PRTC'                           VRS001
                                       TO MSGKW999-NM-VRV-MNT             VRS001
                    MOVE SGTSI064-CD-TIP-PRTC                             VRS001
                                       TO MSGKW999-VL-VRV-MNT             VRS001
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI064-CD-BCO-PCT'                            VRS001
                                       TO MSGKW999-NM-VRV-MNT             VRS001
                    MOVE SGTSI064-CD-BCO-PCT                              VRS001
                                       TO MSGKW999-VL-VRV-MNT             VRS001
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI064-IN-BCO-LDR'                            VRS001
                                       TO MSGKW999-NM-VRV-MNT             VRS001
                    MOVE SGTSI064-IN-BCO-LDR                              VRS001
                                       TO MSGKW999-VL-VRV-MNT             VRS001
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI064-PC-PRTC-BCO'                           VRS001
                                       TO MSGKW999-NM-VRV-MNT             VRS001
                    MOVE SGTSI064-PC-PRTC-BCO                             VRS001
                                       TO AX-VL-4V4
                    MOVE AX-VL-4V4     TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI064-TS-ULT-ATL'                            VRS001
                                       TO MSGKW999-NM-VRV-MNT             VRS001
                    MOVE SGTSI064-TS-ULT-ATL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT
                                                                          VRS001
               WHEN OTHER                                                 VRS001
                    CONTINUE                                              VRS001
           END-EVALUATE                                                   VRS001
           .                                                              VRS001
       969000-SAI.                                                        VRS001
           EXIT.                                                          VRS001
                                                                          VRS001
      *----------------------------------------------------------------*  VRS001
      *                                                                *  VRS001
      * SECTION DA FAIXA DE 970000 A 979900 PARA LEITURA DE DADOS      *  VRS001
      *                                                                *  VRS001
      *----------------------------------------------------------------*  VRS001
       970000-LER-CANAL                SECTION.                           VRS001
      *    Obtem o canal corrente                                         VRS001
      *----------------------------------------------------------------*  VRS001
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO               VRS001
                                                                          VRS001
      *Por causa da captura automtica de erro CICS, deve ser previsto    VRS001
      *o erro de container                                                VRS001
           SET  ERRO-CANAL             TO TRUE                            VRS001
                                                                          VRS001
           EXEC CICS ASSIGN                                               VRS001
                     CHANNEL ( NM-CNL )                                   VRS001
           END-EXEC                                                       VRS001
                                                                          VRS001
      *    Ao chegar aqui, h certeza de que no houve erro algum         VRS001
           SET  SEM-ERRO               TO TRUE                            VRS001
           .                                                              VRS001
       970000-FIM.                                                        VRS001
           EXIT.                                                          VRS001
                                                                          VRS001
      *----------------------------------------------------------------*  VRS001
       971000-LER-REQUISICAO           SECTION.                           VRS001
      *----------------------------------------------------------------*  VRS001
                                                                          VRS001
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO               VRS001
                                                                          VRS001
      *    Monta o nome do container de requisição                        VRS001
           MOVE 'SGTS0091'             TO NM-PGM                          VRS001
           MOVE 'SGTS0091-RQSC'        TO NM-CTNR                         VRS001
                                                                          VRS001
      *    Recupera o tamanho do container de requisição                  VRS001
           PERFORM 973100-LER-CONTAINER-NODATA                            VRS001
                                                                          VRS001
      *Configura dados para leitura do container de requisição            VRS001
           SET  GET-CONTAINER          TO TRUE                            VRS001
                                                                          VRS001
      *Por causa da captura automática de erro CICS, deve ser previsto    VRS001
      *o erro de container                                                VRS001
           SET  ERRO-CONTAINER         TO TRUE                            VRS001
                                                                          VRS001
      *    Efetua a leitura do container                                  VRS001
           EXEC CICS GET                                                  VRS001
                     CONTAINER (NM-CTNR)                                  VRS001
                     FLENGTH   (QT-CCT-LIDO)                              VRS001
                     INTO      (SGTS0091-RQSC)                            VRS001
           END-EXEC                                                       VRS001
                                                                          VRS001
      *Ao chegar aqui, há certeza de que não houve erro algum             VRS001
           SET  SEM-ERRO               TO TRUE                            VRS001
           .                                                              VRS001
       971000-SAI.                                                        VRS001
           EXIT.                                                          VRS001
                                                                          VRS001
      *----------------------------------------------------------------*  VRS001
       972000-LER-CONTAINER-DE-ERRO    SECTION.                           VRS001
      *----------------------------------------------------------------*  VRS001
      *    A presena do container PRM-ERRO indica que houve erro de      VRS001
      *    integrao, a ausncia do container indica sucesso.            VRS001
      *----------------------------------------------------------------*  VRS001
                                                                          VRS001
           MOVE 0011                   TO MSG1-CD-SEQL-ERRO               VRS001
                                                                          VRS001
      *    Le o container de erro dos módulos chamados                    VRS001
           SET GET-CONTAINER           TO TRUE                            VRS001
           MOVE 'PRM-ERRO'             TO NM-CTNR                         VRS001
           INITIALIZE IIBKERRO-PRM-ERRO                                   VRS001
                                                                          VRS001
      *    Não há captura automática de erro CICS por usar RESP e RESP2   VRS001
           EXEC CICS GET                                                  VRS001
                     CONTAINER ( NM-CTNR                     )            VRS001
                     INTO      ( IIBKERRO-PRM-ERRO           )            VRS001
                     FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )            VRS001
                     RESP      ( MSG1-RESP1                  )            VRS001
                     RESP2     ( MSG1-RESP2                  )            VRS001
           END-EXEC                                                       VRS001
                                                                          VRS001
           EVALUATE TRUE                                                  VRS001
      *        Caso deseje personalizar ou no interromper o              VRS001
      *        processamento, alterar para não dar erro aqui SOMENTE      VRS001
      *        PARA O MÓDULO CHAMADO e tratar na section chamadora        VRS001
               WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)                VRS001
                    CONTINUE                                              VRS001
               WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)                      VRS001
                    SET ERRO-PROGRAMA-ACIONADO TO TRUE                    VRS001
                    MOVE IIBKERRO-TX-ERRO  TO MSG1-TX-RTN-PGM-ACND        VRS001
                                              MSG1-TX-ERRO-TS             VRS001
                    MOVE NM-PGM            TO MSG1-NM-PGM-ACND            VRS001
                    MOVE IIBKERRO-CD-ERRO  TO MSG1-CD-RTN-PGM-ACND        VRS001
                                              MSG1-CD-ERRO-TS
                                                                          VRS001
      *             Monitorar variveis do mdulo chamado                 VRS001
                    PERFORM 969000-MONITORAR-VARIAVEIS                    VRS001
                    PERFORM 999000-ERRO                                   VRS001
               WHEN OTHER                                                 VRS001
                    SET ERRO-CONTAINER  TO TRUE                           VRS001
                    PERFORM 999002-TRATA-CONDITION-CICS                   VRS001
           END-EVALUATE                                                   VRS001
           .                                                              VRS001
       972000-SAI.                                                        VRS001
           EXIT.                                                          VRS001
                                                                          VRS001
      *----------------------------------------------------------------*  VRS001
       973000-LER-RESPOSTA             SECTION.                           VRS001
      *----------------------------------------------------------------*  VRS001
                                                                          VRS001
           MOVE 0012                   TO MSG1-CD-SEQL-ERRO               VRS001
                                                                          VRS001
      *    Monta o nome dos container de acordo com modulo a executar     VRS001
           MOVE SPACES                 TO NM-CTNR                         VRS001
                                                                          VRS001
      *    Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão        VRS001
      *    nome_módulo -RPST                                              VRS001

           MOVE NM-PGM        TO NM-CTNR(1:8)                             VRS001
           MOVE '-RPST'       TO NM-CTNR(9:5)                             VRS001
                                                                          VRS001
      *    Recupera o tamanho do container de resposta                    VRS001
           PERFORM 973100-LER-CONTAINER-NODATA

           IF  NM-PGM  NOT EQUAL 'SGTSL064'
      *        Movimentando o tamanho esperado.                           VRS001
               EVALUATE NM-PGM                                            VRS001
                 WHEN SGTS0081                                            VRS001
                    MOVE LENGTH OF SGTS0081-RPST TO QT-CCT-EPRO           VRS001

                 WHEN SGTSE064                                            VRS001
                    MOVE LENGTH OF SGTSE064-RPST TO QT-CCT-EPRO           VRS001

                 WHEN SGTSI064                                            VRS001
                    MOVE LENGTH OF SGTSI064-RPST TO QT-CCT-EPRO           VRS001

                 WHEN OTHER                                               VRS001
                    STRING                                                VRS001
                       'Leitura não definido para resposta do módulo '
                        NM-PGM DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
                    END-STRING
                    PERFORM 999000-ERRO                                   VRS001
               END-EVALUATE                                               VRS001

      *    Valida tamanho esperado da requisição X tamanho recebido

               IF  QT-CCT-LIDO             NOT EQUAL QT-CCT-EPRO
                   SET ERRO-TAMANHO-AREA        TO TRUE
                   PERFORM 999000-ERRO
               END-IF
           END-IF
                                                                          VRS001
      *Configura dados para leitura do container de resposta              VRS001
           SET  GET-CONTAINER          TO TRUE                            VRS001
                                                                          VRS001
      *Por causa da captura automtica de erro CICS, deve ser previsto    VRS001
      *o erro de container                                                VRS001
           SET  ERRO-CONTAINER         TO TRUE                            VRS001
                                                                          VRS001
      *    Le a RESPOSTA de acordo com o programa que a gravou            VRS001
                                                                          VRS001
           MOVE 0013                   TO MSG1-CD-SEQL-ERRO               VRS001
                                                                          VRS001
           EVALUATE NM-PGM                                                VRS001
                                                                          VRS001
      *        Calcular a quantidade de caracteres esperado e limpar      VRS001
      *        a area de dados antes de executar o GET                    VRS001
      *        Para matrizes com dados variaveis (clausula depending      VRS001
      *        on) nao ha como utilizar o comando INITIALIZE              VRS001
                                                                          VRS001
               WHEN SGTS0081                                              VRS001
                    INITIALIZE SGTS0081-RPST                              VRS001
                    EXEC CICS GET                                         VRS001
                              CONTAINER (NM-CTNR          )               VRS001
                              INTO      (SGTS0081-RPST    )               VRS001
                              FLENGTH   (QT-CCT-LIDO      )               VRS001
                    END-EXEC                                              VRS001
               WHEN SGTSL064                                              VRS001
                    EXEC CICS GET                                         VRS001
                              CONTAINER (NM-CTNR          )               VRS001
                              INTO      (SGTSL064-RPST    )               VRS001
                              FLENGTH   (QT-CCT-LIDO      )               VRS001
                    END-EXEC                                              VRS001
               WHEN SGTSE064                                              VRS001
                    INITIALIZE SGTSE064-RPST                              VRS001
                    EXEC CICS GET                                         VRS001
                              CONTAINER (NM-CTNR          )               VRS001
                              INTO      (SGTSE064-RPST    )               VRS001
                              FLENGTH   (QT-CCT-LIDO      )               VRS001
                    END-EXEC
               WHEN SGTSI064                                              VRS001
                    INITIALIZE SGTSI064-RPST                              VRS001
                    EXEC CICS GET                                         VRS001
                              CONTAINER (NM-CTNR          )               VRS001
                              INTO      (SGTSI064-RPST    )               VRS001
                              FLENGTH   (QT-CCT-LIDO      )               VRS001
                    END-EXEC
               WHEN OTHER
                    MOVE 9006 TO MSG1-CD-ERRO-TS                          VRS001
                    MOVE 'Não definida a execução do módulo.'             VRS001
                              TO MSG1-TX-ERRO-TS
                    PERFORM 999000-ERRO                                   VRS001
           END-EVALUATE                                                   VRS001
                                                                          VRS001
      *Ao chegar aqui, h certeza de que no houve erro algum             VRS001
           SET  SEM-ERRO               TO TRUE                            VRS001
                                                                          VRS001
           .                                                              VRS001
       973000-SAI.                                                        VRS001
           EXIT.
                                                                          VRS001
      *----------------------------------------------------------------*  VRS001
       973100-LER-CONTAINER-NODATA     SECTION.                           VRS001
      *----------------------------------------------------------------*  VRS001
                                                                          VRS001
           MOVE 0014                   TO MSG1-CD-SEQL-ERRO               VRS001
                                                                          VRS001
      *Configura dados para leitura do container de requisição            VRS001
           SET  GET-CONTAINER          TO TRUE                            VRS001
                                                                          VRS001
      *Por causa da captura automática de erro CICS, deve ser previsto    VRS001
      *o erro de container                                                VRS001
           SET  ERRO-CONTAINER         TO TRUE                            VRS001
                                                                          VRS001
      *    Obtem a quantidade de caracteres no container                  VRS001
           MOVE ZEROS                  TO QT-CCT-LIDO                     VRS001
           EXEC CICS GET                                                  VRS001
                     CONTAINER (NM-CTNR)                                  VRS001
                     NODATA                                               VRS001
                     FLENGTH   (QT-CCT-LIDO)                              VRS001
           END-EXEC                                                       VRS001
                                                                          VRS001
      *    Ao chegar aqui, há certeza de que não houve erro algum         VRS001
           SET  SEM-ERRO               TO TRUE                            VRS001
           .                                                              VRS001
       973100-SAI.                                                        VRS001
           EXIT.                                                          VRS001

      *----------------------------------------------------------------*  VRS001
       974000-LER-DADOS-OPRD           SECTION.                           VRS001
      *----------------------------------------------------------------*  VRS001
           MOVE 0015                   TO MSG1-CD-SEQL-ERRO               VRS001
                                                                          VRS001
      *Configura dados para leitura do container de requisição            VRS001
           SET  GET-CONTAINER          TO TRUE                            VRS001
           MOVE 'PRM-INF-OPRD'         TO NM-CTNR                         VRS001
                                                                          VRS001
      *Por causa da captura automática de erro CICS, deve ser previsto    VRS001
      *o erro de container                                                VRS001
           SET  ERRO-CONTAINER         TO TRUE                            VRS001
                                                                          VRS001
           INITIALIZE PRM-INF-OPRD                                        VRS001
           EXEC CICS GET                                                  VRS001
                CONTAINER ( NM-CTNR                )                      VRS001
                INTO      ( PRM-INF-OPRD           )                      VRS001
                FLENGTH   ( LENGTH OF PRM-INF-OPRD )                      VRS001
           END-EXEC.                                                      VRS001
                                                                          VRS001
      *    Ao chegar aqui, há certeza de que não houve erro algum         VRS001
           SET  SEM-ERRO               TO TRUE                            VRS001
           .                                                              VRS001
       974000-SAI.                                                        VRS001
           EXIT.

      *----------------------------------------------------------------*  VRS001
       982000-GRAVAR-CONTAINER-RQSC    SECTION.                           VRS001
      *----------------------------------------------------------------*  VRS001
      *    Postar container com dados de requisição (entrada)             VRS001
      *----------------------------------------------------------------*  VRS001
                                                                          VRS001
           MOVE 0016                   TO MSG1-CD-SEQL-ERRO               VRS001
                                                                          VRS001
      *    Monta o nome dos container de acordo com módulo a executar     VRS001
           MOVE SPACES                 TO NM-CTNR                         VRS001
           SET PUT-CONTAINER           TO TRUE                            VRS001
                                                                          VRS001
      *    Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão        VRS001
      *    nome_módulo -RPST                                              VRS001

           MOVE NM-PGM        TO NM-CTNR(1:8)                             VRS001
           MOVE '-RQSC'       TO NM-CTNR(9:5)                             VRS001

                                                                          VRS001
      *Por causa da captura automática de erro CICS, deve ser previsto    VRS001
      *o erro de container                                                VRS001
           SET  ERRO-CONTAINER         TO TRUE                            VRS001
                                                                          VRS001
      *    Faz put da requisição do programa a executar                   VRS001
           EVALUATE NM-PGM                                                VRS001
               WHEN SGTS0081                                              VRS001
                    EXEC CICS PUT                                         VRS001
                              CONTAINER (NM-CTNR                 )        VRS001
                              FROM      (SGTS0081-RQSC           )        VRS001
                              FLENGTH   (LENGTH OF SGTS0081-RQSC )        VRS001
                    END-EXEC                                              VRS001
               WHEN SGTSL064                                              VRS001
                    EXEC CICS PUT                                         VRS001
                              CONTAINER (NM-CTNR                 )        VRS001
                              FROM      (SGTSL064-RQSC           )        VRS001
                              FLENGTH   (LENGTH OF SGTSL064-RQSC )        VRS001
                    END-EXEC                                              VRS001
               WHEN SGTSE064                                              VRS001
                    EXEC CICS PUT                                         VRS001
                              CONTAINER (NM-CTNR                 )        VRS001
                              FROM      (SGTSE064-RQSC           )        VRS001
                              FLENGTH   (LENGTH OF SGTSE064-RQSC )        VRS001
                    END-EXEC
               WHEN SGTSI064                                              VRS001
                    EXEC CICS PUT                                         VRS001
                              CONTAINER (NM-CTNR                 )        VRS001
                              FROM      (SGTSI064-RQSC           )        VRS001
                              FLENGTH   (LENGTH OF SGTSI064-RQSC )        VRS001
                    END-EXEC
                                                                          VRS001
               WHEN OTHER                                                 VRS001
                    MOVE 9006 TO MSG1-CD-ERRO-TS                          VRS001
                    MOVE 'Não definida a execução do módulo.'             VRS001
                              TO MSG1-TX-ERRO-TS
                    PERFORM 999000-ERRO                                   VRS001
           END-EVALUATE                                                   VRS001
                                                                          VRS001
      *Ao chegar aqui, há certeza de que não houve erro algum             VRS001
           SET  SEM-ERRO               TO TRUE                            VRS001
           .                                                              VRS001
       982000-SAI.                                                        VRS001
           EXIT.                                                          VRS001
                                                                          VRS001
      *----------------------------------------------------------------*  VRS001
       983000-EXCLUIR-RESPOSTA-ERRO    SECTION.                           VRS001
      *----------------------------------------------------------------*  VRS001
      * Exclui o container de resposta e de erro para evitar problemas *  VRS001
      * em módulos executados mais de uma vez dentro da mesma sessão do*  VRS001
      * CICS. Equivale ao initialize na área de resposta dos programas *  VRS001
      * que utilizam COMMAREA.                                         *  VRS001
      *----------------------------------------------------------------*  VRS001
                                                                          VRS001
           MOVE 0017                   TO MSG1-CD-SEQL-ERRO               VRS001
                                                                          VRS001
      *    Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão        VRS001
      *    nome_módulo -RPST                                              VRS001

           MOVE NM-PGM        TO NM-CTNR(1:8)                             VRS001
           MOVE '-RPST'       TO NM-CTNR(9:5)                             VRS001
                                                                          VRS001
           SET DEL-CONTAINER           TO TRUE                            VRS001
                                                                          VRS001
      *    Exclui o container de resposta                                 VRS001
      *    Não há captura automática de erro CICS por usar RESP e RESP2   VRS001
           EXEC CICS DELETE                                               VRS001
                     CONTAINER (NM-CTNR   )                               VRS001
                     RESP      (MSG1-RESP1)                               VRS001
                     RESP2     (MSG1-RESP2)                               VRS001
           END-EXEC                                                       VRS001
                                                                          VRS001
           EVALUATE TRUE                                                  VRS001
               WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)                VRS001
               WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)                      VRS001
                    CONTINUE                                              VRS001
               WHEN OTHER                                                 VRS001
                    SET ERRO-CONTAINER TO TRUE                            VRS001
                    PERFORM 999002-TRATA-CONDITION-CICS                   VRS001
           END-EVALUATE                                                   VRS001
                                                                          VRS001
      *    Excluir o container de erro                                    VRS001
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO                          VRS001
           .                                                              VRS001
       983000-SAI.                                                        VRS001
           EXIT.                                                          VRS001
      *                                                                   VRS001
      *----------------------------------------------------------------*  VRS001
       983100-EXCLUIR-CONTAINER-ERRO   SECTION.                           VRS001
      *----------------------------------------------------------------*  VRS001
                                                                          VRS001
      *    Limpar variáveis container de erro                             VRS001
           INITIALIZE IIBKERRO-PRM-ERRO                                   VRS001
                                                                          VRS001
      *    Exclui o container de erro                                     VRS001
           MOVE 0018                   TO MSG1-CD-SEQL-ERRO               VRS001
           MOVE 'PRM-ERRO'             TO NM-CTNR                         VRS001
           SET DEL-CONTAINER           TO TRUE                            VRS001
                                                                          VRS001
      *    Não há captura automática de erro CICS por usar RESP e RESP2   VRS001
           EXEC CICS DELETE                                               VRS001
                     CONTAINER (NM-CTNR)                                  VRS001
                     RESP      (MSG1-RESP1)                               VRS001
                     RESP2     (MSG1-RESP2)                               VRS001
           END-EXEC                                                       VRS001
                                                                          VRS001
           EVALUATE TRUE                                                  VRS001
               WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)                VRS001
               WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)                      VRS001
                    CONTINUE                                              VRS001
               WHEN OTHER                                                 VRS001
                    SET ERRO-CONTAINER TO TRUE                            VRS001
                    PERFORM 999002-TRATA-CONDITION-CICS                   VRS001
           END-EVALUATE                                                   VRS001
           .                                                              VRS001
       983100-SAI.                                                        VRS001
           EXIT.                                                          VRS001

      *----------------------------------------------------------------*  VRS001
       984000-GRAVAR-CONTAINER-ERRO    SECTION.                           VRS001
      *----------------------------------------------------------------*  VRS001
           MOVE 0019                   TO MSG1-CD-SEQL-ERRO               VRS001
                                                                          VRS001
      *    Grava o erro gerado no container de erro                       VRS001
      *    Não há captura automática de erro CICS por usar RESP e RESP2   VRS001
           EXEC CICS PUT                                                  VRS001
                     CONTAINER ( 'PRM-ERRO'                  )            VRS001
                     FROM      ( IIBKERRO-PRM-ERRO           )            VRS001
                     FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )            VRS001
                     RESP      (MSG1-RESP1                   )            VRS001
                     RESP2     (MSG1-RESP2                   )            VRS001
           END-EXEC
           .                                                              VRS001
       984000-SAI.                                                        VRS001
           EXIT.                                                          VRS001
      *                                                                   VRS001
      *----------------------------------------------------------------*  VRS001
      *                                                                *  VRS001
      * As sections abaixo iniciadas com 999000 representam os tratamen*  VRS001
      * tos de erro utilizando o Sistema MSG (rotina MSGS0001)         *  VRS001
      *                                                                *  VRS001
      *----------------------------------------------------------------*  VRS001
       999000-ERRO                     SECTION.                           VRS001
      *----------------------------------------------------------------*  VRS001
                                                                          VRS001
           IF  MSG1-CD-ERRO-TS EQUAL ZEROS                                VRS001
               MOVE 8888                TO MSG1-CD-ERRO-TS                VRS001
           END-IF                                                         VRS001

           IF  MSG1-TX-ERRO-TS EQUAL SPACES                               VRS001
               MOVE 'Erro não especificado no módulo.'
                                        TO MSG1-TX-ERRO-TS                VRS001
           END-IF                                                         VRS001
           PERFORM 999000-GRAVA-ERRO-E-ENCERRA                            VRS001
           .                                                              VRS001
       999000-ERRO-SAI.                                                   VRS001
           EXIT.
                                                                          VRS001
      *----------------------------------------------------------------*  VRS001
      *     As sections abaixo representam os tratamentos de erro         VRS001
      *    utilizando o sistema MSG de mensageria (rotina MSGS0001)       VRS001
      *-----------------------------------------------------------------  VRS001
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.                           VRS001
      *-----------------------------------------------------------------  VRS001
                                                                          VRS001
      *    Em caso de erro, desfaz as atualizações em banco de dados      VRS001
                                                                          VRS001
           EXEC CICS SYNCPOINT                                            VRS001
                     ROLLBACK                                             VRS001
           END-EXEC                                                       VRS001
                                                                          VRS001
      *Verifica se o tipo de erro gerado possui tratamento interno        VRS001
           EVALUATE TRUE                                                  VRS001
               WHEN ERRO-CONTAINER                                        VRS001
                    PERFORM 999004-TRATA-ERRO-CTNR                        VRS001
               WHEN ERRO-TAMANHO-AREA                                     VRS001
                    PERFORM 999005-TRATA-ERRO-AREA-DADOS                  VRS001
               WHEN ERRO-CANAL                                            VRS001
                    PERFORM 999006-TRATA-ERRO-CANAL                       VRS001
               WHEN OTHER                                                 VRS001
                    CONTINUE                                              VRS001
           END-EVALUATE                                                   VRS001
                                                                          VRS001
      *Adiciona as variáveis de entrada para monitoração                  VRS001
           MOVE 'SGTS0091-CD-CTRA-TIT-VL'       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0091-CD-CTRA-TIT-VL        TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0091-CD-OPR-ETRD-MCP'      TO MSGKW999-NM-VRV-MNT    VRS001
      *------> Verifica se o conteudo e numerico antes de adicionar
           IF  SGTS0091-CD-OPR-ETRD-MCP    IS NUMERIC
               MOVE  SGTS0091-CD-OPR-ETRD-MCP   TO  CD-OPR-ETRD-MCP
               MOVE  CD-OPR-ETRD-MCP-TX         TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico.'      TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM 999003-ADICIONA-VRV-MNT                                VRS001
                                                                          VRS001
           MOVE 'SGTS0091-NR-VRS-OPR-ETRD'      TO MSGKW999-NM-VRV-MNT    VRS001
           MOVE SGTS0091-NR-VRS-OPR-ETRD        TO MSGKW999-VL-VRV-MNT    VRS001
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0091-QT-OCR-TAB'           TO MSGKW999-NM-VRV-MNT    VRS001
           MOVE SGTS0091-QT-OCR-TAB             TO MSGKW999-VL-VRV-MNT    VRS001
           PERFORM 999003-ADICIONA-VRV-MNT

           IF  COM-ERRO-TAB
               PERFORM 999910-ADICIONA-OCR-TAB
           END-IF

      *    Grava o erro gerado no sistema MSG - Mensageria                VRS001
           PERFORM MSG1-NTF-MSG                                           VRS001
                                                                          VRS001
           MOVE MSG1-CD-ERRO-TS        TO IIBKERRO-CD-ERRO                VRS001
           MOVE MSG1-TX-ERRO-TS        TO IIBKERRO-TX-ERRO                VRS001
                                                                          VRS001
      *    Grava o erro gerado no container de erro                       VRS001
           PERFORM 984000-GRAVAR-CONTAINER-ERRO                           VRS001
                                                                          VRS001
      *    Encerra a transacao                                            VRS001
           PERFORM 999999-ENCERRAR                                        VRS001
           .                                                              VRS001
       999000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999910-ADICIONA-OCR-TAB         SECTION.
      *--> Adiciona ocorrência da tabela para monitoração
      *-----------------------------------------------------------------

           MOVE  'IDX'                 TO  MSGKW999-NM-VRV-MNT
           MOVE  IDX                   TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           IF  AX-CD-BCO-PCT  EQUAL  'S'
               MOVE 'SGTS0091-CD-BCO-PCT'        TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0091-CD-BCO-PCT (IDX)   TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT
           END-IF

           IF  AX-IN-BCO-LDR  EQUAL  'S'
               MOVE 'SGTS0091-IN-BCO-LDR'        TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0091-IN-BCO-LDR (IDX)   TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT
           END-IF

           IF  AX-PC-PRTC-BCO  EQUAL  'S'
               MOVE 'SGTS0091-PC-PRTC-BCO'     TO  MSGKW999-NM-VRV-MNT
               MOVE SGTS0091-PC-PRTC-BCO (IDX) TO  AX-VL-4V4
               MOVE  AX-VL-4V4                 TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT
           END-IF
           .
       999910-SAI.
           EXIT.

      * --------------------------------------*                           VRS001
      * 999001-TRATA-ABEND-CICS        SECTION.                           VRS001
      * 999002-TRATA-CONDITION-CICS    SECTION.                           VRS001
      * 999003-ADICIONA-VRV-MNT        SECTION.
      * 999004-ALINHA-NUMERO-ESQUERDA  SECTION.                           VRS001
      * --------------------------------------*                           VRS001
-INC HLPKP999                                                           00011770
      * --------------------------------------*                           VRS001
      * 999004-TRATA-ERRO-CTNR         SECTION.                           VRS001
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.                           VRS001
      * 999006-TRATA-ERRO-CANAL        SECTION.                           VRS001
      *---------------------------------------*                           VRS001
-INC SGTKP999                                                           00011830
      * --------------------------------------*                           VRS001
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
      * MSG1-GET-CTNR-ERRO             SECTION.                           VRS001
      * --------------------------------------*                           VRS001
-INC MSGKPMG1                                                           00011870

      *----------------------------------------------------------------*
       999999-ENCERRAR                 SECTION.
      *----------------------------------------------------------------*

           GOBACK
           .
       999999-SAI.
           EXIT.

      *---------------------- FIM SGTS0091 -----------------------------  VRS001