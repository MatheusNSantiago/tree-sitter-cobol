      *-----------------------------------------------------------------
      * PROGRAMA..: SGTS0188
      * ANALISTA..: F2054109 CINTIA IGLESIAS
      * AUTOR.....: F2054109 CINTIA IGLESIAS
      * DATA......: 19/10/2017
      * OBJETIVO..: Alterar Anexo da Operacao Estruturada.
      * COMPILACAO: COBOL/CICS
      *-----------------------------------------------------------------
      * VRS001 14/05/2018 F8658869 DE 2016/145195 - Tornar a carteira
      *                   variável.
      *        19/10/2017 F2054109 DE 2016/145195 - Implantação
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID. SGTS0188.
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
      *----------------------------------------------------------------*000350
       77  CTE-INICIO-SS                 PIC  X(040)  VALUE
                    '*** S.S. COMEÇA AQUI ***'.

       77  CTE-PROG                      PIC  X(018)  VALUE
                    '*** SGTS0188 ***'.
       77  CTE-VERS                      PIC  X(006)  VALUE 'VRS001'.

      * Módulos acionados
       77  SGTSI060                      PIC  X(008) VALUE 'SGTSI060'.
       77  SGTSA060                      PIC  X(008) VALUE 'SGTSA060'.
       77  SGTSE060                      PIC  X(008) VALUE 'SGTSE060'.
       77  SGTSC060                      PIC  X(008) VALUE 'SGTSC060'.
       77  SGTS0081                      PIC  X(008) VALUE 'SGTS0081'.
       77  SGTSP016                      PIC  X(008) VALUE 'SGTSP016'.
       77  SGTSI059                      PIC  X(008) VALUE 'SGTSI059'.
       77  SGTSA059                      PIC  X(008) VALUE 'SGTSA059'.

      *----------------------------------------------------------------*
       LOCAL-STORAGE                   SECTION.
      *----------------------------------------------------------------*
      * Areas de trabalho
       01 IN-EXITENCIA-REGISTRO            PIC X(1).
          88 REGISTRO-NAO-EXISTE           VALUE  'N'.
          88 REGISTRO-EXISTE               VALUE  'S'.

      *--> Auxiliares para movimentação na monitoração de variáveis.
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).

      * Book dos Containers e das subrotinas chamadas
      *----------------------------------------------------------------*

      * Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1                                                           00000610
      * Book para captura de abend cics da bloco codigo MSGKP999
-INC HLPKW999                                                           00000630
      * Book para captura de erro via MSG1
-INC MSGKWMG1                                                           00000660

      * Book do container de requisicao e resposta deste modulo
       01 SGTS0188-DADOS.
-INC SGTK0188                                                           00000700

-INC SGTK0081

-INC SGTKI060

-INC SGTKA060

-INC SGTKE060

-INC SGTKC060

-INC SGTKI059

-INC SGTKA059

-INC SGTKP016

      * Book do Container de Erro
-INC IIBKERRO
      * Book do container de Servicos (para acionamento de servicos)

      * Book dos dados do usurio                                         VRS001
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

           .
       000000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       100000-INICIAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE 0001                   TO MSG1-CD-SEQL-ERRO

      *    Ativa a captura automatica de abend do CICS.
      *    WHENEVER CICS
-INC HLPKE999                                                           00001140

           PERFORM 970000-LER-CANAL

      *    Define o nome do programa p/ fins manipulacao container
           MOVE 'SGTS0188'             TO NM-PGM

      *    Limpa a resposta e erro antes de executar o modulo
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO

           PERFORM 971000-LER-REQUISICAO
           PERFORM 974000-LER-DADOS-OPRD

           PERFORM 110000-VALIDAR-REQUISICAO
           .
       100000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       110000-VALIDAR-REQUISICAO       SECTION.
      *----------------------------------------------------------------*
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO

           IF  SGTS0188-CD-CTRA-TIT-VL     LESS ZEROS
               SET  ERRO-PROGRAMADO        TO  TRUE
               MOVE 001                    TO  MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de título e valores mobiliários
      -             ' inválido.'           TO  MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0188-CD-CTRA-TIT-VL     EQUAL ZEROS
               SET  ERRO-PROGRAMADO        TO  TRUE
               MOVE 002                    TO  MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de título e valores mobiliários
      -             'não informado.'       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0188-CD-OPR-ETRD-MCP    NOT NUMERIC
               SET  ERRO-PROGRAMADO        TO  TRUE
               MOVE 003                    TO  MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturado mercado de capitais não
      -        ' numérico.'                TO  MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0188-CD-OPR-ETRD-MCP    LESS  ZEROS
               SET  ERRO-PROGRAMADO        TO  TRUE
               MOVE 004                    TO  MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturado mercado de capitais inv
      -        'álido.'                    TO  MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0188-CD-OPR-ETRD-MCP    EQUAL  ZEROS
               SET  ERRO-PROGRAMADO        TO  TRUE
               MOVE 005                    TO  MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturado mercado de capitais não
      -        ' informado.'               TO  MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0188-NR-VRS-OPR-ETRD    LESS ZEROS
               SET  ERRO-PROGRAMADO        TO  TRUE
               MOVE 006                    TO  MSG1-CD-ERRO-TS
               MOVE 'Número da versão da operação estruturada inválido.'
                                           TO  MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0188-NR-VRS-OPR-ETRD    EQUAL ZEROS
               SET  ERRO-PROGRAMADO        TO  TRUE
               MOVE 007                    TO  MSG1-CD-ERRO-TS
               MOVE 'Número da versão da operação não informado.'
                                           TO  MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0188-CD-ARQ-OPR-ETRD    EQUAL ZEROS
               SET  ERRO-PROGRAMADO        TO  TRUE
               MOVE 149                    TO  MSG1-CD-ERRO-TS
               MOVE 'Código do arquivo da operação estruturada não infor
      -             'mado.'                TO  MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF
           .
       110000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       200000-PROCESSAR                SECTION.
      *----------------------------------------------------------------*

           PERFORM 210000-VERIFICAR-PMS-ALT

           PERFORM 280000-CONSULTA-ANEXO-OPR

           IF REGISTRO-NAO-EXISTE
               SET  ERRO-PROGRAMADO        TO  TRUE
               MOVE 414                    TO  MSG1-CD-ERRO-TS
               MOVE 'Anexo não vinculado ao arquivo informado.'
                                           TO  MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           PERFORM 220000-VERIFICAR-VCL-ANX

           IF SGTSP016-IN-ARQ-UTZD-OTR-ANX = 'N'
              PERFORM 230000-ALTERA-ARQUIVO
              PERFORM 240000-ALTERA-ANEXO-OPR
              MOVE SGTS0188-CD-ARQ-OPR-ETRD    TO SGTS0188-CD-ARQ-OPR
           ELSE
              PERFORM 250000-EXCLUI-ANEXO-OPR
              PERFORM 260000-INCLUI-ARQUIVO
              MOVE SGTSI059-CD-ARQ-OPR         TO SGTS0188-CD-ARQ-OPR
              PERFORM 270000-INCLUI-ANEXO-OPR
           END-IF

           PERFORM 980000-GRAVAR-CONTAINER-RPST
           .
       200000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       210000-VERIFICAR-PMS-ALT        SECTION.
      *Verificar se Anexo Pode ser alterado.
      *----------------------------------------------------------------*
           MOVE 0003                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE         SGTS0081-DADOS

           MOVE SGTS0081                   TO NM-PGM

      *    Movimentar os campos de entrada
           MOVE SGTS0188-CD-CTRA-TIT-VL    TO SGTS0081-CD-CTRA-TIT-VL
           MOVE SGTS0188-CD-OPR-ETRD-MCP   TO SGTS0081-CD-OPR-ETRD
           MOVE SGTS0188-NR-VRS-OPR-ETRD   TO SGTS0081-NR-VRS-OPR-ETRD

           PERFORM 900000-EXECUTAR-MODULO

           IF SGTS0081-IN-HBLT-MENU-ANX EQUAL 'N'
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 098                TO MSG1-CD-ERRO-TS
               MOVE 'Alteração não permitida para o estado da operação.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF
           .
       210000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       220000-VERIFICAR-VCL-ANX        SECTION.
      *----------------------------------------------------------------*
           MOVE 0004                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE         SGTSP016-DADOS

            MOVE SGTSP016              TO NM-PGM

      *    Movimentar os campos de entrada

           MOVE SGTS0188-CD-CTRA-TIT-VL    TO SGTSP016-CD-CTRA-TIT-VL
           MOVE SGTS0188-CD-OPR-ETRD-MCP   TO SGTSP016-CD-OPR-ETRD-MCP
           MOVE SGTS0188-NR-VRS-OPR-ETRD   TO SGTSP016-NR-VRS-OPR-ETRD
           MOVE SGTS0188-CD-ARQ-OPR-ETRD   TO SGTSP016-CD-ARQ-OPR-ETRD

           PERFORM 900000-EXECUTAR-MODULO
           .
       220000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       230000-ALTERA-ARQUIVO       SECTION.
      *----------------------------------------------------------------*
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

      *    Movimentar o nome do programa a ser executado
           MOVE SGTSA059                    TO NM-PGM

           MOVE SGTS0188-CD-ARQ-OPR-ETRD    TO SGTSA059-CD-ARQ-OPR-ETRD
           MOVE SGTS0188-CD-TIP-ARQ         TO SGTSA059-CD-TIP-ARQ
           MOVE 'CURRENT'                   TO SGTSA059-TS-INCL-ARQ-OPR

           PERFORM 900000-EXECUTAR-MODULO
           .
       230000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       240000-ALTERA-ANEXO-OPR       SECTION.
      *----------------------------------------------------------------*
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

      *    Movimentar o nome do programa a ser executado
           MOVE SGTSA060               TO NM-PGM

           MOVE SGTS0188-CD-CTRA-TIT-VL    TO SGTSA060-CD-CTRA-TIT-VL
           MOVE SGTS0188-CD-OPR-ETRD-MCP   TO SGTSA060-CD-OPR-ETRD-MCP
           MOVE SGTS0188-NR-VRS-OPR-ETRD    TO SGTSA060-NR-VRS-OPR-ETRD
           MOVE SGTS0188-CD-ARQ-OPR-ETRD    TO SGTSA060-CD-ARQ-OPR-ETRD
           MOVE SGTS0188-TX-DCR-ANX         TO SGTSA060-TX-DCR-ANX
           MOVE KOPRD-CD-OPRD               TO SGTSA060-CD-USU-ULT-ATL
           MOVE 'CURRENT'                   TO SGTSA060-TS-ULT-ATL

           PERFORM 900000-EXECUTAR-MODULO
           .
       240000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       250000-EXCLUI-ANEXO-OPR       SECTION.
      *----------------------------------------------------------------*
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

      *    Movimentar o nome do programa a ser executado
           MOVE SGTSE060               TO NM-PGM

           MOVE SGTS0188-CD-CTRA-TIT-VL    TO SGTSE060-CD-CTRA-TIT-VL
           MOVE SGTS0188-CD-OPR-ETRD-MCP   TO SGTSE060-CD-OPR-ETRD-MCP
           MOVE SGTS0188-NR-VRS-OPR-ETRD   TO SGTSE060-NR-VRS-OPR-ETRD
           MOVE SGTS0188-CD-ARQ-OPR-ETRD   TO SGTSE060-CD-ARQ-OPR-ETRD

           PERFORM 900000-EXECUTAR-MODULO
           .
       250000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       260000-INCLUI-ARQUIVO       SECTION.
      *----------------------------------------------------------------*
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

      *    Movimentar o nome do programa a ser executado
           MOVE SGTSI059                    TO NM-PGM

           MOVE ZEROS                       TO SGTSI059-CD-ARQ-OPR-ETRD
           MOVE SGTS0188-CD-TIP-ARQ         TO SGTSI059-CD-TIP-ARQ
           MOVE 'CURRENT'                   TO SGTSI059-TS-INCL-ARQ-OPR

           PERFORM 900000-EXECUTAR-MODULO
           .
       260000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       270000-INCLUI-ANEXO-OPR       SECTION.
      *----------------------------------------------------------------*
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO

      *    Movimentar o nome do programa a ser executado
           MOVE SGTSI060                    TO NM-PGM

           MOVE SGTS0188-CD-CTRA-TIT-VL     TO SGTSI060-CD-CTRA-TIT-VL
           MOVE SGTS0188-CD-OPR-ETRD-MCP    TO SGTSI060-CD-OPR-ETRD-MCP
           MOVE SGTS0188-NR-VRS-OPR-ETRD    TO SGTSI060-NR-VRS-OPR-ETRD
           MOVE SGTSI059-CD-ARQ-OPR         TO SGTSI060-CD-ARQ-OPR-ETRD
           MOVE SGTS0188-TX-DCR-ANX         TO SGTSI060-TX-DCR-ANX
           MOVE KOPRD-CD-OPRD               TO SGTSI060-CD-USU-ULT-ATL
           MOVE 'CURRENT'                   TO SGTSI060-TS-ULT-ATL

           PERFORM 900000-EXECUTAR-MODULO
           .
       270000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       280000-CONSULTA-ANEXO-OPR       SECTION.
      *----------------------------------------------------------------*
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO

      *    Movimentar o nome do programa a ser executado
           MOVE SGTSC060               TO NM-PGM

           MOVE SGTS0188-CD-CTRA-TIT-VL    TO SGTSC060-CD-CTRA-TIT-VL
           MOVE SGTS0188-CD-OPR-ETRD-MCP   TO SGTSC060-CD-OPR-ETRD-MCP
           MOVE SGTS0188-NR-VRS-OPR-ETRD   TO SGTSC060-NR-VRS-OPR-ETRD
           MOVE SGTS0188-CD-ARQ-OPR-ETRD   TO SGTSC060-CD-ARQ-OPR-ETRD

           PERFORM 900000-EXECUTAR-MODULO
           .
       280000-SAI.
           EXIT.

      *----------------------------------------------------------------*
      *                                                                *
      *  SECTION DA FAIXA DE 900000 A 969900 PARA DEMAIS ROTINAS       *
      *                                                                *
      *----------------------------------------------------------------*
      * Executa um módulo implementado com CONTAINER                   *
      * Não incluir nessa section os módulos chamados via COMMAREA     *
      *                                                                *
      *----------------------------------------------------------------*
       900000-EXECUTAR-MODULO          SECTION.
      *----------------------------------------------------------------*
           MOVE 0011                   TO MSG1-CD-SEQL-ERRO

      *    Grava o container de requisicao do módulo a executar
           PERFORM 982000-GRAVAR-CONTAINER-RQSC

      *    Limpa a resposta e erro antes de executar o módulo
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO

           MOVE SPACES TO IN-EXITENCIA-REGISTRO

      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EVALUATE NM-PGM
               WHEN SGTSI060
                   EXEC CICS LINK
                             PROGRAM ( SGTSI060  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC

               WHEN SGTSE060
                   EXEC CICS LINK
                             PROGRAM ( SGTSE060  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC

               WHEN SGTSC060
                   EXEC CICS LINK
                             PROGRAM ( SGTSC060  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC

               WHEN SGTSA060
                   EXEC CICS LINK
                             PROGRAM ( SGTSA060  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC

               WHEN SGTSI059
                   EXEC CICS LINK
                             PROGRAM ( SGTSI059  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC

               WHEN SGTSA059
                   EXEC CICS LINK
                             PROGRAM ( SGTSA059  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC

               WHEN SGTSP016
                   EXEC CICS LINK
                             PROGRAM ( SGTSP016  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC

               WHEN SGTS0081
                   EXEC CICS LINK
                             PROGRAM ( SGTS0081  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC

               WHEN OTHER
                   MOVE  9006  TO  MSG1-CD-ERRO-TS
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
                    STRING
                        'Erro na execução do módulo ' NM-PGM ', '
                        'RESP  ='   ' ' MSG1-RESP1  ', '
                        'RESP2 =' ' ' MSG1-RESP2
                        DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
                    END-STRING
                    MOVE 3004          TO MSG1-CD-ERRO-TS
                    SET ERRO-CICS      TO TRUE
                    PERFORM 999000-ERRO
           END-EVALUATE

      *    Efetua a leitura do Container de Erro
           PERFORM 972000-LER-CONTAINER-DE-ERRO

           IF  NM-PGM  NOT EQUAL SGTSA059
               IF IN-EXITENCIA-REGISTRO = SPACES
                  OR REGISTRO-EXISTE
      *    Efetua a leitura do Container de Resposta
                  PERFORM 973000-LER-RESPOSTA
               END-IF
           END-IF
           .
       900000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       969000-MONITORAR-VARIAVEIS      SECTION.
      *----------------------------------------------------------------*
      * Monitora as variáveis dos módulos chamados
      *----------------------------------------------------------------*

           EVALUATE NM-PGM
           WHEN SGTSI059
                MOVE 'SGTSI059-CD-ARQ-OPR-ETRD'  TO MSGKW999-NM-VRV-MNT
                MOVE SGTSI059-CD-ARQ-OPR-ETRD    TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTSI059-CD-TIP-ARQ'       TO MSGKW999-NM-VRV-MNT
                MOVE SGTSI059-CD-TIP-ARQ         TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTSI059-TS-INCL-ARQ-OPR'  TO MSGKW999-NM-VRV-MNT
                MOVE SGTSI059-TS-INCL-ARQ-OPR    TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

           WHEN SGTSA059
                MOVE 'SGTSA059-CD-ARQ-OPR-ETRD'  TO MSGKW999-NM-VRV-MNT
                MOVE SGTSA059-CD-ARQ-OPR-ETRD    TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTSA059-CD-TIP-ARQ'       TO MSGKW999-NM-VRV-MNT
                MOVE SGTSA059-CD-TIP-ARQ         TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTSA059-TS-INCL-ARQ-OPR'  TO MSGKW999-NM-VRV-MNT
                MOVE SGTSA059-TS-INCL-ARQ-OPR    TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

           WHEN SGTSI060
                MOVE 'SGTSI060-CD-CTRA-TIT-VL'   TO MSGKW999-NM-VRV-MNT
                MOVE SGTSI060-CD-CTRA-TIT-VL     TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTSI060-CD-OPR-ETRD-MCP'  TO MSGKW999-NM-VRV-MNT
      *------> Verifica se o conteúdo e numérico antes de adicionar
                IF  SGTSI060-CD-OPR-ETRD-MCP    IS NUMERIC
                    MOVE  SGTSI060-CD-OPR-ETRD-MCP   TO  CD-OPR-ETRD-MCP
                    MOVE  CD-OPR-ETRD-MCP-TX    TO  MSGKW999-VL-VRV-MNT
                ELSE
                    MOVE  'Valor não numérico.' TO  MSGKW999-VL-VRV-MNT
                END-IF
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTSI060-NR-VRS-OPR-ETRD'  TO MSGKW999-NM-VRV-MNT
                MOVE SGTSI060-NR-VRS-OPR-ETRD    TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTSI060-CD-ARQ-OPR-ETRD'  TO MSGKW999-NM-VRV-MNT
                MOVE SGTSI060-CD-ARQ-OPR-ETRD    TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTSI060-CD-USU-ULT-ATL'   TO MSGKW999-NM-VRV-MNT
                MOVE SGTSI060-CD-USU-ULT-ATL     TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTSI060-TS-ULT-ATL'       TO MSGKW999-NM-VRV-MNT
                MOVE SGTSI060-TS-ULT-ATL         TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTSI060-TX-DCR-ANX'       TO MSGKW999-NM-VRV-MNT
                MOVE SGTSI060-TX-DCR-ANX         TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

           WHEN SGTSA060
                MOVE 'SGTSA060-CD-CTRA-TIT-VL'   TO MSGKW999-NM-VRV-MNT
                MOVE SGTSA060-CD-CTRA-TIT-VL     TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTSA060-CD-OPR-ETRD-MCP'  TO MSGKW999-NM-VRV-MNT
      *------> Verifica se o conteúdo e numérico antes de adicionar
                IF  SGTSA060-CD-OPR-ETRD-MCP    IS NUMERIC
                    MOVE  SGTSA060-CD-OPR-ETRD-MCP   TO  CD-OPR-ETRD-MCP
                    MOVE  CD-OPR-ETRD-MCP-TX    TO  MSGKW999-VL-VRV-MNT
                ELSE
                    MOVE  'Valor não numérico.' TO  MSGKW999-VL-VRV-MNT
                END-IF
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTSA060-NR-VRS-OPR-ETRD'  TO MSGKW999-NM-VRV-MNT
                MOVE SGTSA060-NR-VRS-OPR-ETRD    TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTSA060-CD-ARQ-OPR-ETRD'  TO MSGKW999-NM-VRV-MNT
                MOVE SGTSA060-CD-ARQ-OPR-ETRD    TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTSA060-CD-USU-ULT-ATL'   TO MSGKW999-NM-VRV-MNT
                MOVE SGTSA060-CD-USU-ULT-ATL     TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTSA060-TS-ULT-ATL'       TO MSGKW999-NM-VRV-MNT
                MOVE SGTSA060-TS-ULT-ATL         TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTSA060-TX-DCR-ANX'       TO MSGKW999-NM-VRV-MNT
                MOVE SGTSA060-TX-DCR-ANX         TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

           WHEN SGTSE060
                MOVE 'SGTSE060-CD-CTRA-TIT-VL'   TO MSGKW999-NM-VRV-MNT
                MOVE SGTSE060-CD-CTRA-TIT-VL     TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTSE060-CD-OPR-ETRD-MCP'  TO MSGKW999-NM-VRV-MNT
      *------> Verifica se o conteúdo e numérico antes de adicionar
                IF  SGTSE060-CD-OPR-ETRD-MCP    IS NUMERIC
                    MOVE  SGTSE060-CD-OPR-ETRD-MCP   TO  CD-OPR-ETRD-MCP
                    MOVE  CD-OPR-ETRD-MCP-TX    TO  MSGKW999-VL-VRV-MNT
                ELSE
                    MOVE  'Valor não numérico.' TO  MSGKW999-VL-VRV-MNT
                END-IF
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTSE060-NR-VRS-OPR-ETRD'  TO MSGKW999-NM-VRV-MNT
                MOVE SGTSE060-NR-VRS-OPR-ETRD    TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTSE060-CD-ARQ-OPR-ETRD'  TO MSGKW999-NM-VRV-MNT
                MOVE SGTSE060-CD-ARQ-OPR-ETRD    TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

           WHEN SGTSC060
                MOVE 'SGTSC060-CD-CTRA-TIT-VL'   TO MSGKW999-NM-VRV-MNT
                MOVE SGTSC060-CD-CTRA-TIT-VL     TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTSC060-CD-OPR-ETRD-MCP'  TO MSGKW999-NM-VRV-MNT
      *------> Verifica se o conteúdo e numérico antes de adicionar
                IF  SGTSC060-CD-OPR-ETRD-MCP    IS NUMERIC
                    MOVE  SGTSC060-CD-OPR-ETRD-MCP   TO  CD-OPR-ETRD-MCP
                    MOVE  CD-OPR-ETRD-MCP-TX    TO  MSGKW999-VL-VRV-MNT
                ELSE
                    MOVE  'Valor não numérico.' TO  MSGKW999-VL-VRV-MNT
                END-IF
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTSC060-NR-VRS-OPR-ETRD'  TO MSGKW999-NM-VRV-MNT
                MOVE SGTSC060-NR-VRS-OPR-ETRD    TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTSC060-CD-ARQ-OPR-ETRD'  TO MSGKW999-NM-VRV-MNT
                MOVE SGTSC060-CD-ARQ-OPR-ETRD    TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

           WHEN SGTS0081
                MOVE 'SGTS0081-CD-CTRA-TIT-VL'   TO MSGKW999-NM-VRV-MNT
                MOVE SGTS0081-CD-CTRA-TIT-VL     TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTS0081-CD-OPR-ETRD'      TO MSGKW999-NM-VRV-MNT
      *------> Verifica se o conteúdo e numérico antes de adicionar
                IF  SGTS0081-CD-OPR-ETRD    IS NUMERIC
                    MOVE  SGTS0081-CD-OPR-ETRD  TO  CD-OPR-ETRD-MCP
                    MOVE  CD-OPR-ETRD-MCP-TX    TO  MSGKW999-VL-VRV-MNT
                ELSE
                    MOVE  'Valor não numérico.' TO  MSGKW999-VL-VRV-MNT
                END-IF
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTS0081-NR-VRS-OPR-ETRD'  TO MSGKW999-NM-VRV-MNT
                MOVE SGTS0081-NR-VRS-OPR-ETRD    TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

           WHEN SGTSP016
                MOVE 'SGTSP016-CD-CTRA-TIT-VL'   TO MSGKW999-NM-VRV-MNT
                MOVE SGTSP016-CD-CTRA-TIT-VL     TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTSP016-CD-OPR-ETRD-MCP'  TO MSGKW999-NM-VRV-MNT
      *------> Verifica se o conteúdo e numérico antes de adicionar
                IF  SGTSP016-CD-OPR-ETRD-MCP    IS NUMERIC
                    MOVE  SGTSP016-CD-OPR-ETRD-MCP   TO  CD-OPR-ETRD-MCP
                    MOVE  CD-OPR-ETRD-MCP-TX    TO  MSGKW999-VL-VRV-MNT
                ELSE
                    MOVE  'Valor não numérico.' TO  MSGKW999-VL-VRV-MNT
                END-IF
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTSP016-NR-VRS-OPR-ETRD'  TO MSGKW999-NM-VRV-MNT
                MOVE SGTSP016-NR-VRS-OPR-ETRD    TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTSP016-CD-ARQ-OPR-ETRD'  TO MSGKW999-NM-VRV-MNT
                MOVE SGTSP016-CD-ARQ-OPR-ETRD    TO MSGKW999-VL-VRV-MNT
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
           MOVE 0012                   TO MSG1-CD-SEQL-ERRO

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
           MOVE 0013                   TO MSG1-CD-SEQL-ERRO

      *    Monta o nome do container de requisição
           MOVE 'SGTS0188'             TO NM-PGM
           MOVE 'SGTS0188-RQSC'        TO NM-CTNR

      *    Recupera o tamanho do container de requisição
           PERFORM 973100-LER-CONTAINER-NODATA

      *    Obtem o tamanho esperado da requisição
           MOVE LENGTH OF SGTS0188-RQSC TO QT-CCT-EPRO

      *    Valida tamanho esperado da requisição X tamanho recebido

           IF  QT-CCT-LIDO             NOT EQUAL QT-CCT-EPRO
               SET ERRO-TAMANHO-AREA   TO TRUE
               PERFORM 999000-ERRO
           END-IF

           MOVE 0014                   TO MSG1-CD-SEQL-ERRO

      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Efetua a leitura do container
           EXEC CICS GET
                     CONTAINER (NM-CTNR      )
                     FLENGTH   (QT-CCT-LIDO  )
                     INTO      (SGTS0188-RQSC)
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
           MOVE 0015                   TO MSG1-CD-SEQL-ERRO

      *    Lê o container de erro dos módulos chamados
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
      *        PARA O MODULO CHAMADO e tratar na section chamadora
               WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
                    IF NM-PGM = 'SGTSC060'
                       SET REGISTRO-EXISTE TO TRUE
                       CONTINUE
                    END-IF

               WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
                    IF NM-PGM = 'SGTSC060'
                    AND IIBKERRO-CD-ERRO = 100
                       SET REGISTRO-NAO-EXISTE TO TRUE
                       CONTINUE
                    ELSE
                       SET ERRO-PROGRAMA-ACIONADO TO TRUE
                       MOVE NM-PGM             TO MSG1-NM-PGM-ACND

                       MOVE IIBKERRO-TX-ERRO   TO MSG1-TX-RTN-PGM-ACND
                                                  MSG1-TX-ERRO-TS

                       MOVE IIBKERRO-CD-ERRO   TO MSG1-CD-RTN-PGM-ACND
                                                  MSG1-CD-ERRO-TS

      *                Monitorar variáveis do módulo chamado
                       PERFORM 969000-MONITORAR-VARIAVEIS
                       PERFORM 999000-ERRO
                    END-IF
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
           MOVE 0016                   TO MSG1-CD-SEQL-ERRO

      *    Monta o nome dos container de acordo com módulo a executar
           MOVE SPACES                 TO NM-CTNR
           MOVE NM-PGM                 TO NM-CTNR(1:8)
           MOVE '-RPST'                TO NM-CTNR(9:5)

      *    Recupera o tamanho do container de resposta
           PERFORM 973100-LER-CONTAINER-NODATA

           EVALUATE NM-PGM
               WHEN SGTS0081
                    MOVE LENGTH OF SGTS0081-RPST TO QT-CCT-EPRO

               WHEN SGTSI060
                    MOVE LENGTH OF SGTSI060-RPST TO QT-CCT-EPRO

               WHEN SGTSA060
                    MOVE LENGTH OF SGTSA060-RPST TO QT-CCT-EPRO

               WHEN SGTSE060
                    MOVE LENGTH OF SGTSE060-RPST TO QT-CCT-EPRO

               WHEN SGTSC060
                    MOVE LENGTH OF SGTSC060-RPST TO QT-CCT-EPRO

               WHEN SGTSP016
                    MOVE LENGTH OF SGTSP016-RPST TO QT-CCT-EPRO

               WHEN SGTSI059
                    MOVE LENGTH OF SGTSI059-RPST TO QT-CCT-EPRO

           END-EVALUATE


      *    Valida tamanho esperado da resposta X tamanho recebido

           IF  QT-CCT-LIDO             NOT EQUAL QT-CCT-EPRO
               SET ERRO-TAMANHO-AREA   TO TRUE
               PERFORM 999000-ERRO
           END-IF

      *Configura dados para leitura do container de resposta
           SET  GET-CONTAINER          TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE


      *    Lê a RESPOSTA de acordo com o programa que a gravou

           MOVE 0017                   TO MSG1-CD-SEQL-ERRO

           EVALUATE NM-PGM

      *        Calcular a quantidade de caracteres esperado e limpar
      *        a area de dados antes de executar o GET
      *        Para matrizes com dados variaveis (clausula depending
      *        on) nao ha como utilizar o comando INITIALIZE

               WHEN SGTS0081
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTS0081-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN SGTSP016
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSP016-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN SGTSA060
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSA060-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN SGTSC060
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSC060-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN SGTSE060
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSE060-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN SGTSI060
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSI060-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN SGTSI059
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSI059-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN OTHER
                    MOVE  9003  TO  MSG1-CD-ERRO-TS
                    STRING
                        'Erro na leitura da resposta do módulo. '
                         NM-PGM DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
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
           MOVE 0018                   TO MSG1-CD-SEQL-ERRO

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

      *----------------------------------------------------------------*  VRS001
       974000-LER-DADOS-OPRD           SECTION.                           VRS001
      *----------------------------------------------------------------*  VRS001
           MOVE 0019                   TO MSG1-CD-SEQL-ERRO               VRS001
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

      *----------------------------------------------------------------*
      *                                                                *
      * SECTION DA FAIXA DE 980000 A 989900 PARA GRAVACAO DE DADOS     *
      *                                                                *
      *----------------------------------------------------------------*
       980000-GRAVAR-CONTAINER-RPST    SECTION.
      *----------------------------------------------------------------*

      *    Exclui eventual container de erro de outros programas
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO

           MOVE 0020                   TO MSG1-CD-SEQL-ERRO

      *    Monta o nome do container de resposta
           MOVE 'SGTS0188-RPST'        TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

           EXEC CICS PUT
                     CONTAINER (NM-CTNR                )
                     FROM      (SGTS0188-RPST          )
                     FLENGTH   (LENGTH OF SGTS0188-RPST)
           END-EXEC

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       980000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       982000-GRAVAR-CONTAINER-RQSC    SECTION.
      *----------------------------------------------------------------*
           MOVE 0021                   TO MSG1-CD-SEQL-ERRO

      *    Monta o nome dos container de acordo com módulo a executar
           MOVE SPACES                 TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE
           MOVE NM-PGM                 TO NM-CTNR(1:8)
           MOVE '-RQSC'                TO NM-CTNR(9:5)

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Faz put da requisição do programa a executar
           EVALUATE NM-PGM
               WHEN SGTSI060
                   EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (SGTSI060-RQSC           )
                              FLENGTH   (LENGTH OF SGTSI060-RQSC )
                    END-EXEC

               WHEN SGTSE060
                   EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (SGTSE060-RQSC           )
                              FLENGTH   (LENGTH OF SGTSE060-RQSC )
                    END-EXEC

               WHEN SGTSA060
                   EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (SGTSA060-RQSC           )
                              FLENGTH   (LENGTH OF SGTSA060-RQSC )
                    END-EXEC

               WHEN SGTSC060
                   EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (SGTSC060-RQSC           )
                              FLENGTH   (LENGTH OF SGTSC060-RQSC )
                    END-EXEC

               WHEN SGTSI059
                   EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (SGTSI059-RQSC           )
                              FLENGTH   (LENGTH OF SGTSI059-RQSC )
                    END-EXEC

               WHEN SGTSA059
                   EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (SGTSA059-RQSC           )
                              FLENGTH   (LENGTH OF SGTSA059-RQSC )
                    END-EXEC

               WHEN SGTS0081
                   EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (SGTS0081-RQSC           )
                              FLENGTH   (LENGTH OF SGTS0081-RQSC )
                    END-EXEC

               WHEN SGTSP016
                   EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (SGTSP016-RQSC           )
                              FLENGTH   (LENGTH OF SGTSP016-RQSC )
                    END-EXEC

               WHEN OTHER
                    MOVE  9004  TO  MSG1-CD-ERRO-TS
                    STRING
                      'Erro na gravação da requisição do módulo.'
                       NM-PGM DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
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
      * em módulos executados mais de uma vez dentro da mesma sessão do*
      * CICS. Equivale ao initialize na area de resposta dos programas *
      * que utilizam COMMAREA.                                         *
      *----------------------------------------------------------------*
           MOVE 0022                   TO MSG1-CD-SEQL-ERRO

           MOVE NM-PGM                 TO NM-CTNR(1:8)
           MOVE '-RPST'                TO NM-CTNR(9:5)
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
           MOVE 0023                   TO MSG1-CD-SEQL-ERRO
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
           MOVE 0024                   TO MSG1-CD-SEQL-ERRO

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
      *
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

           MOVE 'SGTS0188-CD-CTRA-TIT-VL'    TO MSGKW999-NM-VRV-MNT
           MOVE  SGTS0188-CD-CTRA-TIT-VL     TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0188-CD-OPR-ETRD-MCP'   TO MSGKW999-NM-VRV-MNT
      *------> Verifica se o conteúdo e numérico antes de adicionar
           IF  SGTS0188-CD-OPR-ETRD-MCP    IS NUMERIC
               MOVE SGTS0188-CD-OPR-ETRD-MCP   TO  CD-OPR-ETRD-MCP
               MOVE  CD-OPR-ETRD-MCP-TX    TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico.' TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0188-NR-VRS-OPR-ETRD'   TO MSGKW999-NM-VRV-MNT
           MOVE  SGTS0188-NR-VRS-OPR-ETRD    TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0188-CD-ARQ-OPR-ETRD'   TO MSGKW999-NM-VRV-MNT
           MOVE  SGTS0188-CD-ARQ-OPR-ETRD    TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0188-CD-TIP-ARQ'        TO MSGKW999-NM-VRV-MNT
           MOVE  SGTS0188-CD-TIP-ARQ         TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0188-TX-DCR-ANX'        TO MSGKW999-NM-VRV-MNT
           MOVE  SGTS0188-TX-DCR-ANX         TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

      *    Grava o erro gerado no sistema MSG - Mensageria
           PERFORM MSG1-NTF-MSG

           MOVE MSG1-CD-ERRO-TS        TO IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS        TO IIBKERRO-TX-ERRO

      *    Grava o erro gerado no container de erro
           PERFORM 984000-GRAVAR-CONTAINER-ERRO

      *    Encerra a transacao
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
-INC HLPKP999                                                           00009600
      * --------------------------------------*
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *---------------------------------------*
-INC SGTKP999                                                           00009660
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

           GOBACK
           .
       999999-SAI.
           EXIT.                                                            0000
      *---------------------- FIM SGTS0188 -----------------------------