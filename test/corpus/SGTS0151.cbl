000010*-----------------------------------------------------------------
000020* PROGRAMA..: SGTS0151
000030* ANALISTA..: F9188847 - Soila K. L. Akamatsu
000040* AUTOR.....: F9188847 - Soila K. L. Akamatsu
000050* DATA......: 29/03/2017
000060* OBJETIVO..: Devolver Versão da Operação Estruturada.
000080* COMPILACAO: COBOL/CICS
000090*-----------------------------------------------------------------
      * VRS002 24/04/2019 F2804421 DE 2016/145195 - Alterar módulo ape-
      *        para operação estruturada com estado 5.2 (Encarteirada-
      *        A Avaliar).
      *        - Inativar indicadores de flexibilização e de impacto ma-
      *          nual de exceção caso estejam ativos.
      * VRS001 29/03/2017 F9188847 DE 2016/145195 - IMPLANTACAO
000110*-----------------------------------------------------------------
000120
000130*-----------------------------------------------------------------
000140 IDENTIFICATION DIVISION.
000150*----------------------------------------------------------------*
000160 PROGRAM-ID. SGTS0151.
000170*
000180*----------------------------------------------------------------*
000190 ENVIRONMENT DIVISION.
000200*----------------------------------------------------------------*
000210*
000220*----------------------------------------------------------------*
000230 CONFIGURATION                   SECTION.
000240*----------------------------------------------------------------*
000250 SPECIAL-NAMES.
000260     DECIMAL-POINT IS COMMA.
000270*
000280*----------------------------------------------------------------*
000290 DATA DIVISION.
000300*----------------------------------------------------------------*
000310*
000320
000330 WORKING-STORAGE                 SECTION.
000340*----------------------------------------------------------------*
000350
000360 77  CTE-INICIO-SS                 PIC  X(040)  VALUE
000370              '*** S.S. COMECA AQUI ***'.
000380
000390 77  CTE-PROG                      PIC  X(018)  VALUE
000400              '*** SGTS0151 ***'.
000410 77  CTE-VERS                      PIC  X(006)  VALUE 'VRS001'.
000420
000430* Módulos acionados
000460 77  SGTST005                      PIC  X(008) VALUE 'SGTST005'.
000460 77  SGTST009                      PIC  X(008) VALUE 'SGTST009'.
000460 77  SGTSI069                      PIC  X(008) VALUE 'SGTSI069'.
000460 77  SGTSC069                      PIC  X(008) VALUE 'SGTSC069'.
000460 77  SGTSI094                      PIC  X(008) VALUE 'SGTSI094'.
000460 77  SGTS0128                      PIC  X(008) VALUE 'SGTS0128'.
000460 77  SGTS0254                      PIC  X(008) VALUE 'SGTS0254'.
000470*----------------------------------------------------------------*
000480 LOCAL-STORAGE                   SECTION.
000490*----------------------------------------------------------------*
000500 01 IN-CONTAINER-ERRO              PIC  X(001) VALUE SPACES.
          88 TEM-CONTAINER-ERRO                 VALUE 'S'.
          88 NAO-TEM-CONTAINER-ERRO             VALUE 'N'.

       01  CD-OPR-ETRD-MCP-DISPLAY     PIC  9(11) VALUE 0.

      * Variável de movimentação
       01 EST-VRS-OPR                    PIC S9(4) COMP-5 VALUE 0.
       01 CD-TIP-EST-OPR                 PIC S9(9) COMP-5 VALUE 0.

000560*----------------------------------------------------------------*
000550
000570* Book dos Containers e das subrotinas chamadas
000580*----------------------------------------------------------------*
000590
000600* Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1
000620* Book para captura de abend cics da bloco codigo MSGKP999
-INC HLPKW999
000640
000650* Book para captura de erro via MSG1
-INC MSGKWMG1
000670
000680* Book do container de requisição e resposta deste modulo
000690*
-INC SGTK0151
      * Book SGT -  Lista da tabela DB2SGT.OPR_ETRD_MCP
       01  SGTST005-DADOS.
-INC SGTKT005
000760* Consula tabela EST_OPR_ETRD
000770 01  SGTST009-DADOS.
-INC SGTKT009
000720* Consulta tabela EST_VRS_OPR_ETRD
000770 01  SGTSC069-DADOS.
-INC SGTKC069
000720* Inclusão tabela EST_VRS_OPR_ETRD
000770 01  SGTSI069-DADOS.
-INC SGTKI069

      *--> Inclusão na tabela 'DB2SGT.IFLC_MNL_LIM_EXCO'
-INC SGTKI094

      *-->  Atualizar indicadores da operação.
-INC SGTK0128

      *-->  Consultar indicadores Flexibilização e Impacto Manual.
       01  SGTS0254-DADOS.
-INC SGTK0254
000800* Book do Container de Erro
-INC IIBKERRO
000820* Book do container de Serviços (para acionamento de serviços)
-INC IIBKSRVC
001400* Book dos dados do usuário
-INC IIBKOPRD
000840
000850 77  CTE-FINAL-SS                  PIC X(40)      VALUE
000860              '*** S.S. TERMINA AQUI ***'.
000870
000880*----------------------------------------------------------------*
000890 PROCEDURE DIVISION.
000900*----------------------------------------------------------------*
000910
000920*----------------------------------------------------------------*
000930 000000-ROTINA-PRINCIPAL         SECTION.
000940*----------------------------------------------------------------*
000950
000960     PERFORM 100000-INICIAR
000970
000980     PERFORM 200000-PROCESSAR
000990
001000     PERFORM 990000-ENCERRAR

           GOBACK
001010
001020     .
001030 000000-SAI.
001040     EXIT.
001050
001060*----------------------------------------------------------------*
001070 100000-INICIAR                  SECTION.
001080*----------------------------------------------------------------*
001090
001100     MOVE 0001                   TO MSG1-CD-SEQL-ERRO
001110
001120*    Ativa a captura automatica de abend do CICS.
001130*    WHENEVER CICS
-INC HLPKE999
001150
001160     PERFORM 970000-LER-CANAL
001170
001180*    Define o nome do programa p/ fins manipulacao container
001190     MOVE 'SGTS0151'             TO NM-PGM
001200
001210*    Limpa a resposta e erro antes de executar o modulo
001220     PERFORM 983000-EXCLUIR-RESPOSTA-ERRO
001230
001240     PERFORM 971000-LER-REQUISICAO

           PERFORM 974000-LER-DADOS-OPRD

001260     PERFORM 110000-VALIDAR-REQUISICAO
001270     .
001280 100000-SAI.
001290     EXIT.
001300
001310*----------------------------------------------------------------*
001320 110000-VALIDAR-REQUISICAO       SECTION.
001330*----------------------------------------------------------------*
001340     MOVE 0002                   TO MSG1-CD-SEQL-ERRO
001350
001360*    Valida campo a campo da requisicao

           IF  SGTS0151-CD-CTRA-TIT-VL < ZEROS
000410         SET  ERRO-PROGRAMADO    TO  TRUE
000410         MOVE 0001               TO  MSG1-CD-ERRO-TS
000410         MOVE 'Código da carteira de título e valores mobiliários
      -              'inválido.'
000410                                 TO  MSG1-TX-ERRO-TS

               PERFORM 999000-ERRO
000410     END-IF

           IF  SGTS0151-CD-CTRA-TIT-VL EQUAL ZEROS
000410         SET  ERRO-PROGRAMADO    TO  TRUE
000410         MOVE 0002               TO  MSG1-CD-ERRO-TS
000410         MOVE 'Código da carteira de título e valores mobiliários
      -             'não informado.'
000410                                 TO  MSG1-TX-ERRO-TS

               PERFORM 999000-ERRO
000410     END-IF
001370
           IF  SGTS0151-CD-OPR-ETRD-MCP NOT NUMERIC
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0003               TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -         ' numérico.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0151-CD-OPR-ETRD-MCP < ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0004               TO  MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais inv
      -             'álido.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

            IF  SGTS0151-CD-OPR-ETRD-MCP = ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0005               TO  MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -             'informado.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0151-NR-VRS-OPR-ETRD  < ZEROS
001860         SET ERRO-PROGRAMADO     TO TRUE
001870         MOVE 0006               TO MSG1-CD-ERRO-TS
               MOVE 'Número da versão da operação estruturada inválido.'
001890                                 TO MSG1-TX-ERRO-TS
001910         PERFORM 999000-ERRO
001920     END-IF

           IF  SGTS0151-NR-VRS-OPR-ETRD  = ZEROS
001860         SET ERRO-PROGRAMADO     TO TRUE
001870         MOVE 0007               TO MSG1-CD-ERRO-TS
               MOVE 'Número da versão da operação não informado.'
001890                                 TO MSG1-TX-ERRO-TS
001910         PERFORM 999000-ERRO
001920     END-IF

           IF  SGTS0151-TX-OBS-VRS EQUAL SPACES
001860         SET ERRO-PROGRAMADO     TO TRUE
001870         MOVE 0623               TO MSG1-CD-ERRO-TS
001880         MOVE 'Texto observação do versionamento não informado.'
001890                                 TO MSG1-TX-ERRO-TS
001910         PERFORM 999000-ERRO
001920     END-IF

001470     .
001480 110000-SAI.
001490     EXIT.
001500
001510*----------------------------------------------------------------*
001520 200000-PROCESSAR                SECTION.
001530*----------------------------------------------------------------*
           MOVE 0003                   TO MSG1-CD-SEQL-ERRO

      *--> Consulta estado da operação.
           PERFORM 210000-CONSULTA-EST-OPR-ETRD

      *--> Consulta estado da versão da operação.
           PERFORM 220000-CONSULTA-EST-VRS-OPR

      *--> Devolução somente será feita para estados 5.2 (Encarteirada -
      *    A Avaliar)
           IF  SGTST009-CD-TIP-EST-OPR EQUAL 5
               IF SGTSC069-CD-TIP-EST-VRS  EQUAL 2
                  PERFORM 230000-VOLTAR-EST-VRS-OPR
               ELSE
                  SET ERRO-PROGRAMA-ACIONADO TO TRUE
                  MOVE 0644                  TO MSG1-CD-ERRO-TS
                  MOVE 'Não permitido devolver estado da versão da
      -                  ' operação.'        TO MSG1-TX-ERRO-TS
                  PERFORM 999000-ERRO
               END-IF

           ELSE
               MOVE 0004                  TO MSG1-CD-SEQL-ERRO
003010         SET ERRO-PROGRAMA-ACIONADO TO TRUE
005010         MOVE 0644                  TO MSG1-CD-ERRO-TS
005020         MOVE 'Não permitido devolver estado da versão da ope
      -             'ração.'
005030                                 TO MSG1-TX-ERRO-TS
005110         PERFORM 999000-ERRO
           END-IF

      *--> Consultar indicadores atuais de exceção :
      *--  Flexibilização e Impacto Manual.
           MOVE SGTS0254               TO NM-PGM
           MOVE SGTS0151-CD-CTRA-TIT-VL
                                       TO SGTS0254-CD-CTRA-TIT-VL
           MOVE SGTS0151-CD-OPR-ETRD-MCP
                                       TO SGTS0254-CD-OPR-ETRD-MCP
           MOVE SGTS0151-NR-VRS-OPR-ETRD
                                       TO SGTS0254-NR-VRS-OPR-ETRD
           PERFORM 900000-EXECUTAR-MODULO

      *--> Se indicador flexibilização está ativo, desativar
           IF  SGTS0254-IN-FLZ-MNL-EXCO  EQUAL 'S'
               PERFORM 250000-ALT-FLEXIBILIZACAO
           END-IF

      *--> Se indicador impacto manual está ativo, desativar
           IF  SGTS0254-IN-IFLC-MNL-EXCO  EQUAL 'S'
               PERFORM 260000-ATL-IMPACTO-MANUAL
           END-IF
           .
       200000-SAI.
           EXIT.
001680*----------------------------------------------------------------*
001690 210000-CONSULTA-EST-OPR-ETRD         SECTION.
      * Consultar Estado Ativo da Operação Estruturada
001700*----------------------------------------------------------------*
001710     MOVE 0005                   TO MSG1-CD-SEQL-ERRO
001720
001730*    Movimentar o nome do programa a ser executado

001740     INITIALIZE                 SGTST009-DADOS.

002040     MOVE SGTST009               TO NM-PGM
002050
002060*    Movimentar os campos de entrada

           SET SGTST009-CONSULTAR-EST-ATIVO
                                       TO TRUE
           MOVE SGTS0151-CD-CTRA-TIT-VL
                                       TO SGTST009-CD-CTRA-TIT-VL
           MOVE SGTS0151-CD-OPR-ETRD-MCP
                                       TO SGTST009-CD-OPR-ETRD-MCP

002200     PERFORM 910000-CHAMAR-SGTST009

001800     .
001810 210000-SAI.
001820     EXIT.
001680*----------------------------------------------------------------*
001690 220000-CONSULTA-EST-VRS-OPR         SECTION.
      * Consultar Estado da Versão Operação Estruturada
001700*----------------------------------------------------------------*
001710     MOVE 0006                   TO MSG1-CD-SEQL-ERRO
001720
001730*    Movimentar o nome do programa a ser executado

001740     INITIALIZE                 SGTSC069-DADOS.

002040     MOVE SGTSC069               TO NM-PGM
002050
002060*    Movimentar os campos de entrada

           MOVE SGTS0151-CD-CTRA-TIT-VL
                                       TO SGTSC069-CD-CTRA-TIT-VL
           MOVE SGTS0151-CD-OPR-ETRD-MCP
                                       TO SGTSC069-CD-OPR-ETRD-MCP
           MOVE SGTS0151-NR-VRS-OPR-ETRD
                                       TO SGTSC069-NR-VRS-OPR-ETRD
      * Recupera o maior timestamp
           MOVE SPACES
                                       TO SGTSC069-TS-EST-VRS
002100
002200     PERFORM 900000-EXECUTAR-MODULO

001800     .
001810 220000-SAI.
001820     EXIT.
001680*----------------------------------------------------------------*
001690 230000-VOLTAR-EST-VRS-OPR        SECTION.
001700*----------------------------------------------------------------*
001710     MOVE 0007                   TO MSG1-CD-SEQL-ERRO
001720
001730*--> Inclui novo registro de estado da versão da operação.
      *--  Volta para estado 1 = Em edição.

001740     INITIALIZE                 SGTSI069-DADOS.

002040     MOVE SGTSI069               TO NM-PGM
002050
002060*    Movimentar os campos de entrada

           MOVE SGTS0151-CD-CTRA-TIT-VL
                                       TO SGTSI069-CD-CTRA-TIT-VL
           MOVE SGTS0151-CD-OPR-ETRD-MCP
                                       TO SGTSI069-CD-OPR-ETRD-MCP
           MOVE SGTS0151-NR-VRS-OPR-ETRD
                                       TO SGTSI069-NR-VRS-OPR-ETRD
           MOVE 1                      TO SGTSI069-CD-TIP-EST-VRS
           MOVE SGTS0151-TX-OBS-VRS    TO SGTSI069-TX-OBS-EST-VRS

002200     PERFORM 900000-EXECUTAR-MODULO
001800     .
001810 230000-SAI.
001820     EXIT.
      *----------------------------------------------------------------*
       250000-ALT-FLEXIBILIZACAO       SECTION.
      *----------------------------------------------------------------*
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO

      *--> Atualiza indicador de flexibilização de regra de exceção.
      *--  Código = 2.  tab.IN_OPR_ETRD
      *--  'N' = inativa indicador

           MOVE SGTS0128               TO NM-PGM

           MOVE SGTS0151-CD-CTRA-TIT-VL
                                       TO SGTS0128-CD-CTRA-TIT-VL
           MOVE SGTS0151-CD-OPR-ETRD-MCP
                                       TO SGTS0128-CD-OPR-ETRD-MCP
           MOVE SGTS0151-NR-VRS-OPR-ETRD
                                       TO SGTS0128-NR-VRS-OPR-ETRD
           MOVE  2                     TO SGTS0128-CD-TIP-IN
           MOVE 'N'                    TO SGTS0128-IN-OPR

           PERFORM 900000-EXECUTAR-MODULO
           .
       250000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       260000-ATL-IMPACTO-MANUAL       SECTION.
      *-----------------------------------------------------------------
           MOVE 0010                   TO  MSG1-CD-SEQL-ERRO

      *--> Consulta dados da operação.
      *--  Necessário obter o código do título da operação.
           PERFORM 261000-CONSULTA-OPR

      *--> Inativa indicador de impacto manual da regra de exceção.
      *--  tab.IFLC_MNL_LIM_EXCO

           MOVE SGTSI094               TO NM-PGM

           MOVE SGTS0151-CD-CTRA-TIT-VL
                                       TO SGTSI094-CD-CTRA-TIT-VL
           MOVE SGTST005-CD-TIT-OPR-ETRD
                                       TO SGTSI094-CD-TIT-OPR-ETRD
           MOVE SGTST005-NR-DVS-TIT    TO SGTSI094-NR-DVS-TIT
           MOVE 'CURRENT'              TO SGTSI094-TS-ATL-IFLC-MNL
           MOVE KOPRD-CD-OPRD          TO SGTSI094-CD-USU-RSP-ULT-ATL

           MOVE 'N'                    TO SGTSI094-IN-IFLC-MNL-EXCO

           PERFORM 900000-EXECUTAR-MODULO

           .
       260000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       261000-CONSULTA-OPR             SECTION.
      *  Efetua busca de dados da operação.
      *-----------------------------------------------------------------
           MOVE 0011                   TO  MSG1-CD-SEQL-ERRO

           MOVE SGTST005               TO  NM-PGM
           SET  SGTST005-CONSULTAR     TO  TRUE

           MOVE SGTS0151-CD-CTRA-TIT-VL
                                       TO  SGTST005-CD-CTRA-TIT-VL
           MOVE SGTS0151-CD-OPR-ETRD-MCP
                                       TO  SGTST005-CD-OPR-ETRD-MCP
           MOVE SGTS0151-NR-VRS-OPR-ETRD
                                       TO  SGTST005-NR-VRS-OPR-ETRD

           PERFORM 930000-CHAMAR-SGTST005

           IF  SGTST005-CD-RTN-PGM     EQUAL 100
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0061               TO MSG1-CD-ERRO-TS
               MOVE 'Operação estruturada inexistente.'
                                       TO MSG1-TX-ERRO-TS

               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA

           END-IF

           .
       261000-SAI.
           EXIT.
      *----------------------------------------------------------------*
002730*                                                                *
002740*  SECTION DA FAIXA DE 900000 A 969900 PARA DEMAIS ROTINAS       *
002750*                                                                *
002760*----------------------------------------------------------------*
002770* Executa um modulo implementado com CONTAINER                   *
002780* Nao incluir nessa section os modulos chamados via COMMAREA     *
002790*
002800*----------------------------------------------------------------*
002810 900000-EXECUTAR-MODULO          SECTION.
002820*----------------------------------------------------------------*
002850*    Grava o container de requisicao do modulo a executar
002860     PERFORM 982000-GRAVAR-CONTAINER-RQSC
002870
002880*    Limpa a resposta e erro antes de executar o modulo
002890     PERFORM 983000-EXCLUIR-RESPOSTA-ERRO
002900
002910*    Não há captura automática de erro CICS por usar RESP e RESP2
002920     EVALUATE NM-PGM
002930         WHEN SGTSC069
002940             EXEC CICS LINK
002950                       PROGRAM ( SGTSC069  )
002960                       CHANNEL ( NM-CNL    )
002970                       RESP    ( MSG1-RESP1)
002980                       RESP2   ( MSG1-RESP2)
002990             END-EXEC
               WHEN SGTSI069
002940             EXEC CICS LINK
002950                       PROGRAM ( SGTSI069  )
002960                       CHANNEL ( NM-CNL    )
002970                       RESP    ( MSG1-RESP1)
002980                       RESP2   ( MSG1-RESP2)
002990             END-EXEC

               WHEN SGTSI094
002940             EXEC CICS LINK
002950                       PROGRAM ( SGTSI094  )
002960                       CHANNEL ( NM-CNL    )
002970                       RESP    ( MSG1-RESP1)
002980                       RESP2   ( MSG1-RESP2)
002990             END-EXEC

               WHEN SGTS0128
002940             EXEC CICS LINK
002950                       PROGRAM ( SGTS0128  )
002960                       CHANNEL ( NM-CNL    )
002970                       RESP    ( MSG1-RESP1)
002980                       RESP2   ( MSG1-RESP2)
002990             END-EXEC


               WHEN SGTS0254
002940             EXEC CICS LINK
002950                       PROGRAM ( SGTS0254  )
002960                       CHANNEL ( NM-CNL    )
002970                       RESP    ( MSG1-RESP1)
002980                       RESP2   ( MSG1-RESP2)
002990             END-EXEC

003000         WHEN OTHER
                   SET   ERRO-PROGRAMADO
                                       TO  TRUE
                    MOVE  9006          TO  MSG1-CD-ERRO-TS
                    STRING  'Não definida a execução do módulo'
                            NM-PGM '.'
                            DELIMITED BY SIZE
                            INTO            MSG1-TX-ERRO-TS
003040             PERFORM 999000-ERRO
003050     END-EVALUATE
003060*
003070     EVALUATE TRUE
003080         WHEN MSG1-RESP1        EQUAL DFHRESP(NORMAL)
003090              CONTINUE
003100         WHEN OTHER
                    SET   ERRO-CICS    TO  TRUE
                    MOVE  3004         TO  MSG1-CD-ERRO-TS
                    STRING
                    'Erro  ao acionar o módulo  ' NM-PGM  ', '
                    'RESP  = '  MSG1-RESP1  ', '
                    'RESP2 = '  MSG1-RESP2
                    DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
                    PERFORM 999000-ERRO
003190     END-EVALUATE
003200
003210*    Efetua a leitura do Container de Erro
003220     PERFORM 972000-LER-CONTAINER-DE-ERRO

003230
           IF NOT TEM-CONTAINER-ERRO
003240*    Efetua a leitura do Container de Resposta
003250        PERFORM 973000-LER-RESPOSTA
           END-IF
003260     MOVE SPACES                TO IN-CONTAINER-ERRO
003260
003270     .
003280 900000-SAI.
003290     EXIT.
      *----------------------------------------------------------------*
004880 910000-CHAMAR-SGTST009         SECTION.
004890*----------------------------------------------------------------*
004900     MOVE 0012                   TO MSG1-CD-SEQL-ERRO
004910
004920*    Movimento nome do módulo para monitoração de variáveis
004930     MOVE SGTST009               TO NM-PGM
004940     EXEC CICS LINK
004950          PROGRAM  (SGTST009)
004960          COMMAREA (SGTST009-DADOS)
004970     END-EXEC
004980
004990     IF  SGTST009-CD-RTN-PGM     NOT EQUAL ZEROS AND 100
005000         SET ERRO-PROGRAMA-ACIONADO TO TRUE
               MOVE NM-PGM             TO MSG1-NM-PGM-ACND
005020         MOVE SGTST009-MSG-ERRO-PGM
005030                                 TO MSG1-TX-RTN-PGM-ACND
005040                                    MSG1-TX-ERRO-TS
005060         MOVE SGTST009-CD-RTN-PGM
005070                                 TO MSG1-CD-RTN-PGM-ACND
                                          MSG1-CD-ERRO-TS
005080
005090*        Monitorar variáveis do módulo chamado
005100         PERFORM 969000-MONITORAR-VARIAVEIS
005110         PERFORM 999000-ERRO
005120     END-IF
005130     .
005140 910000-SAI.
005150     EXIT.
       930000-CHAMAR-SGTST005          SECTION.
      *-----------------------------------------------------------------
           MOVE 0013                   TO  MSG1-CD-SEQL-ERRO

      *--> Movimento nome do módulo para monitoração de variáveis
           MOVE SGTST005               TO  NM-PGM
           EXEC CICS LINK
                PROGRAM  (SGTST005)
                COMMAREA (SGTST005-DADOS)
           END-EXEC

           IF  SGTST005-CD-RTN-PGM     NOT EQUAL ZEROS AND 100
               SET ERRO-PROGRAMA-ACIONADO  TO TRUE
               MOVE NM-PGM                 TO MSG1-NM-PGM-ACND
               MOVE SGTST005-MSG-ERRO-PGM  TO MSG1-TX-RTN-PGM-ACND
                                              MSG1-TX-ERRO-TS
               MOVE SGTST005-CD-RTN-PGM    TO MSG1-CD-RTN-PGM-ACND
                                              MSG1-CD-ERRO-TS

      *        Monitorar variáveis do módulo chamado
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       930000-SAI.
           EXIT.
003790*----------------------------------------------------------------*
003800 969000-MONITORAR-VARIAVEIS      SECTION.
003810*----------------------------------------------------------------*
003820* Monitora as variáveis dos módulos chamados
003830*----------------------------------------------------------------*
003850
003860     EVALUATE NM-PGM
               WHEN SGTST005
                 MOVE 'SGTST005-CD-CTRA-TIT-VL' TO MSGKW999-NM-VRV-MNT
                 MOVE  SGTST005-CD-CTRA-TIT-VL  TO MSGKW999-VL-VRV-MNT
                 PERFORM 999003-ADICIONA-VRV-MNT

                 MOVE 'SGTST005-CD-OPR-ETRD-MCP' TO MSGKW999-NM-VRV-MNT
                 MOVE SGTST005-CD-OPR-ETRD-MCP   TO MSGKW999-VL-VRV-MNT
                 PERFORM 999003-ADICIONA-VRV-MNT

                 MOVE 'SGTST005-NR-VRS-OPR-ETRD' TO MSGKW999-NM-VRV-MNT
                 MOVE  SGTST005-NR-VRS-OPR-ETRD TO MSGKW999-VL-VRV-MNT
                 PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSC069
005920              MOVE 'SGTSC069-CD-CTRA-TIT-VL'
005930                                 TO MSGKW999-NM-VRV-MNT
005940              MOVE SGTSC069-CD-CTRA-TIT-VL
005950                                 TO MSGKW999-VL-VRV-MNT
005960              PERFORM 999003-ADICIONA-VRV-MNT

003880              MOVE 'SGTSC069-CD-OPR-ETRD-MCP'
003890                                 TO MSGKW999-NM-VRV-MNT
                    IF SGTSC069-CD-OPR-ETRD-MCP IS NUMERIC
                        MOVE SGTSC069-CD-OPR-ETRD-MCP
                                       TO CD-OPR-ETRD-MCP-DISPLAY
003900                  MOVE CD-OPR-ETRD-MCP-DISPLAY
                                       TO MSGKW999-VL-VRV-MNT
                    ELSE
                        MOVE 'Código não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                    END-IF
003920              PERFORM 999003-ADICIONA-VRV-MNT
003930
003940              MOVE 'SGTSC069-NR-VRS-OPR-ETRD'
003950                                 TO MSGKW999-NM-VRV-MNT
003960              MOVE SGTSC069-NR-VRS-OPR-ETRD
003970                                 TO MSGKW999-VL-VRV-MNT
003980              PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSC069-TS-EST-VRS'
003950                                 TO MSGKW999-NM-VRV-MNT
003960              MOVE SGTSC069-TS-EST-VRS
003970                                 TO MSGKW999-VL-VRV-MNT
003980              PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSI069
005920              MOVE 'SGTSI069-CD-CTRA-TIT-VL'
005930                                 TO MSGKW999-NM-VRV-MNT
005940              MOVE SGTSI069-CD-CTRA-TIT-VL
005950                                 TO MSGKW999-VL-VRV-MNT
005960              PERFORM 999003-ADICIONA-VRV-MNT

003880              MOVE 'SGTSI069-CD-OPR-ETRD-MCP'
003890                                 TO MSGKW999-NM-VRV-MNT
                    IF SGTSI069-CD-OPR-ETRD-MCP IS NUMERIC
003900                 MOVE SGTSI069-CD-OPR-ETRD-MCP
                                       TO CD-OPR-ETRD-MCP-DISPLAY
                       MOVE CD-OPR-ETRD-MCP-DISPLAY
                                       TO MSGKW999-VL-VRV-MNT
                    ELSE
                       MOVE 'Código não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                    END-IF
003920              PERFORM 999003-ADICIONA-VRV-MNT
003930
003940              MOVE 'SGTSI069-NR-VRS-OPR-ETRD'
003950                                 TO MSGKW999-NM-VRV-MNT
003960              MOVE SGTSI069-NR-VRS-OPR-ETRD
003970                                 TO MSGKW999-VL-VRV-MNT
003980              PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI069-CD-TIP-EST-VRS'
003950                                 TO MSGKW999-NM-VRV-MNT
003960              MOVE SGTSI069-CD-TIP-EST-VRS
003970                                 TO MSGKW999-VL-VRV-MNT
003980              PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI069-TX-OBS-EST-VRS'
003950                                 TO MSGKW999-NM-VRV-MNT
003960              MOVE SGTSI069-TX-OBS-EST-VRS
003970                                 TO MSGKW999-VL-VRV-MNT
003980              PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTST009
                    MOVE 'SGTST009-ACAO'
003890                                 TO MSGKW999-NM-VRV-MNT
003900              MOVE SGTST009-ACAO
003910                                 TO MSGKW999-VL-VRV-MNT
003920              PERFORM 999003-ADICIONA-VRV-MNT

003880              MOVE 'SGTST009-CD-CTRA-TIT-VL'
003890                                 TO MSGKW999-NM-VRV-MNT
003900              MOVE SGTST009-CD-CTRA-TIT-VL
003910                                 TO MSGKW999-VL-VRV-MNT
003920              PERFORM 999003-ADICIONA-VRV-MNT
003930
003940              MOVE 'SGTST009-CD-OPR-ETRD-MCP'
003950                                 TO MSGKW999-NM-VRV-MNT
                    IF SGTST009-CD-OPR-ETRD-MCP IS NUMERIC
003900                 MOVE SGTST009-CD-OPR-ETRD-MCP
                                       TO CD-OPR-ETRD-MCP-DISPLAY
                       MOVE CD-OPR-ETRD-MCP-DISPLAY
                                       TO MSGKW999-VL-VRV-MNT
                    ELSE
                       MOVE 'Código não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                    END-IF
003980              PERFORM 999003-ADICIONA-VRV-MNT

                    IF SGTST009-INCLUIR
                       MOVE 'SGTST009-NR-VRS-OPR-ETRD'
003890                                 TO MSGKW999-NM-VRV-MNT
003900                 MOVE SGTST009-NR-VRS-OPR-ETRD
003910                                 TO MSGKW999-VL-VRV-MNT
003920                 PERFORM 999003-ADICIONA-VRV-MNT

                       MOVE 'SGTST009-TS-EST-OPR-ETRD'
003890                                 TO MSGKW999-NM-VRV-MNT
003900                 MOVE SGTST009-TS-EST-OPR-ETRD
003910                                 TO MSGKW999-VL-VRV-MNT
003920                 PERFORM 999003-ADICIONA-VRV-MNT

                       MOVE 'SGTST009-CD-USU-RSP-EST'
003890                                 TO MSGKW999-NM-VRV-MNT
003900                 MOVE SGTST009-CD-USU-RSP-EST
003910                                 TO MSGKW999-VL-VRV-MNT
003920                 PERFORM 999003-ADICIONA-VRV-MNT

                       MOVE 'SGTST009-CD-TIP-EST-OPR'
003890                                 TO MSGKW999-NM-VRV-MNT
003900                 MOVE SGTST009-CD-TIP-EST-OPR
003910                                 TO MSGKW999-VL-VRV-MNT
003920                 PERFORM 999003-ADICIONA-VRV-MNT

                       MOVE 'SGTST009-TX-JST-EST'
003890                                 TO MSGKW999-NM-VRV-MNT
003900                 MOVE SGTST009-TX-JST-EST
003910                                 TO MSGKW999-VL-VRV-MNT
003920                 PERFORM 999003-ADICIONA-VRV-MNT

                       MOVE 'SGTST009-CD-USU-RSP-ATL'
003890                                 TO MSGKW999-NM-VRV-MNT
003900                 MOVE SGTST009-CD-USU-RSP-ATL
003910                                 TO MSGKW999-VL-VRV-MNT
003920                 PERFORM 999003-ADICIONA-VRV-MNT
                    END-IF

               WHEN SGTSI094
                 MOVE 'SGTSI094-CD-CTRA-TIT-VL' TO MSGKW999-NM-VRV-MNT
                 MOVE  SGTSI094-CD-CTRA-TIT-VL  TO MSGKW999-VL-VRV-MNT
                 PERFORM 999003-ADICIONA-VRV-MNT

                 MOVE 'SGTSI094-NR-DVS-TIT'     TO MSGKW999-NM-VRV-MNT
                 MOVE SGTSI094-NR-DVS-TIT       TO MSGKW999-VL-VRV-MNT
                 PERFORM 999003-ADICIONA-VRV-MNT

                 MOVE 'SGTSI094-NR-DVS-TIT'     TO MSGKW999-NM-VRV-MNT
                 MOVE  SGTSI094-NR-DVS-TIT      TO MSGKW999-VL-VRV-MNT
                 PERFORM 999003-ADICIONA-VRV-MNT

                 MOVE 'SGTSI094-IN-IFLC-MNL-EXCO'
                                                 TO MSGKW999-NM-VRV-MNT
                 MOVE  SGTSI094-IN-IFLC-MNL-EXCO TO MSGKW999-VL-VRV-MNT
                 PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTS0128
                 MOVE 'SGTS0128-CD-CTRA-TIT-VL' TO MSGKW999-NM-VRV-MNT
                 MOVE  SGTS0128-CD-CTRA-TIT-VL  TO MSGKW999-VL-VRV-MNT
                 PERFORM 999003-ADICIONA-VRV-MNT

                 MOVE 'SGTS0128-CD-OPR-ETRD-MCP' TO MSGKW999-NM-VRV-MNT
                 MOVE SGTS0128-CD-OPR-ETRD-MCP   TO MSGKW999-VL-VRV-MNT
                 PERFORM 999003-ADICIONA-VRV-MNT

                 MOVE 'SGTS0128-NR-VRS-OPR-ETRD' TO MSGKW999-NM-VRV-MNT
                 MOVE  SGTS0128-NR-VRS-OPR-ETRD  TO MSGKW999-VL-VRV-MNT
                 PERFORM 999003-ADICIONA-VRV-MNT

                 MOVE 'SGTS0128-CD-TIP-IN'       TO MSGKW999-NM-VRV-MNT
                 MOVE  SGTS0128-CD-TIP-IN        TO MSGKW999-VL-VRV-MNT
                 PERFORM 999003-ADICIONA-VRV-MNT

                 MOVE 'SGTS0128-IN-OPR'          TO MSGKW999-NM-VRV-MNT
                 MOVE  SGTS0128-IN-OPR           TO MSGKW999-VL-VRV-MNT
                 PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTS0254
                 MOVE 'SGTS0254-CD-CTRA-TIT-VL' TO MSGKW999-NM-VRV-MNT
                 MOVE  SGTS0254-CD-CTRA-TIT-VL  TO MSGKW999-VL-VRV-MNT
                 PERFORM 999003-ADICIONA-VRV-MNT

                 MOVE 'SGTS0254-CD-OPR-ETRD-MCP' TO MSGKW999-NM-VRV-MNT
                 MOVE SGTS0254-CD-OPR-ETRD-MCP   TO MSGKW999-VL-VRV-MNT
                 PERFORM 999003-ADICIONA-VRV-MNT

                 MOVE 'SGTS0254-NR-VRS-OPR-ETRD' TO MSGKW999-NM-VRV-MNT
                 MOVE  SGTS0254-NR-VRS-OPR-ETRD  TO MSGKW999-VL-VRV-MNT
                 PERFORM 999003-ADICIONA-VRV-MNT

004180         WHEN OTHER
004190              CONTINUE
004200     END-EVALUATE
004210
004220     .
004230 969000-SAI.
004240     EXIT.
004250
004260*----------------------------------------------------------------*
004270*                                                                *
004280* SECTION DA FAIXA DE 970000 A 979900 PARA LEITURA DE DADOS      *
004290*                                                                *
004300*----------------------------------------------------------------*
004310 970000-LER-CANAL                SECTION.
004320*    Obtem o canal corrente
004330*----------------------------------------------------------------*
004340     MOVE 0014                   TO MSG1-CD-SEQL-ERRO
004350
004360*Por causa da captura automática de erro CICS, deve ser previsto
004370*o erro de container
004380     SET  ERRO-CANAL             TO TRUE
004390
004400     EXEC CICS ASSIGN
004410               CHANNEL ( NM-CNL )
004420     END-EXEC
004430
004440*    Ao chegar aqui, há certeza de que não houve erro algum
004450     SET  SEM-ERRO               TO TRUE
004460     .
004470 970000-FIM.
004480     EXIT.
004490
004500*----------------------------------------------------------------*
004510 971000-LER-REQUISICAO           SECTION.
004520*----------------------------------------------------------------*
004530
004540     MOVE 0015                   TO MSG1-CD-SEQL-ERRO
004550
004560*    Monta o nome do container de requisicao
004570     MOVE 'SGTS0151'             TO NM-PGM
004580     MOVE 'SGTS0151-RQSC'        TO NM-CTNR
004590
004600*    Recupera o tamanho do container de requisição
004610     PERFORM 973100-LER-CONTAINER-NODATA
004620
004630*    Obtem o tamanho esperado da requisicao
004640     MOVE LENGTH OF SGTS0151-RQSC TO QT-CCT-EPRO
004650
004660*    Valida tamanho esperado da requisicao X tamanho recebido
004670
004680     IF  QT-CCT-LIDO             NOT EQUAL QT-CCT-EPRO
004690         SET ERRO-TAMANHO-AREA   TO TRUE
004700         PERFORM 999000-GRAVA-ERRO-E-ENCERRA
004710     END-IF
004720
004740
004750*Configura dados para leitura do container de requisição
004760     SET  GET-CONTAINER          TO TRUE
004770
004780*Por causa da captura automática de erro CICS, deve ser previsto
004790*o erro de container
004800     SET  ERRO-CONTAINER         TO TRUE
004810
004820*    Efetua a leitura do container
004830     EXEC CICS GET
004840               CONTAINER (NM-CTNR)
004850               FLENGTH   (QT-CCT-LIDO)
004860               INTO      (SGTS0151-RQSC)
004870     END-EXEC
004880
004890*Ao chegar aqui, há certeza de que não houve erro algum
004900     SET  SEM-ERRO               TO TRUE
004910
004920     .
004930 971000-SAI.
004940     EXIT.
004950
004960*----------------------------------------------------------------*
004970 972000-LER-CONTAINER-DE-ERRO    SECTION.
004980*----------------------------------------------------------------*
004990*    A presença do container PRM-ERRO indica que houve erro de
005000*    integração, a ausência do container indica sucesso.
005010*----------------------------------------------------------------*
005020
005050*    Le o container de erro dos modulos chamados
005060     SET GET-CONTAINER           TO TRUE
005070     MOVE 'PRM-ERRO'             TO NM-CTNR
005080     INITIALIZE IIBKERRO-PRM-ERRO
005090
           MOVE SPACES TO IN-CONTAINER-ERRO
005100*    Não há captura automática de erro CICS por usar RESP e RESP2
005110     EXEC CICS GET
005120               CONTAINER ( NM-CTNR                     )
005130               INTO      ( IIBKERRO-PRM-ERRO           )
005140               FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
005150               RESP      ( MSG1-RESP1                  )
005160               RESP2     ( MSG1-RESP2                  )
005170     END-EXEC
005180
005190     EVALUATE TRUE
005200*        Caso deseje personalizar ou não interromper o
005210*        processamento, alterar para não dar erro aqui SOMENTE
005220*        PARA O MÓDULO CHAMADO e tratar na section chamadora
005230         WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
005240              CONTINUE
005250         WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
                    SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
                    MOVE  NM-PGM       TO  MSG1-NM-PGM-ACND

                    IF NM-PGM(1:3) EQUAL 'SGT'
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

                       MOVE IIBKERRO-TX-ERRO
                                          TO MSG1-TX-RTN-PGM-ACND
                       MOVE IIBKERRO-CD-ERRO
                                          TO MSG1-CD-RTN-PGM-ACND

                    END-IF
005350*             Monitorar variáveis do módulo chamado
005360                 PERFORM 969000-MONITORAR-VARIAVEIS
005370                 PERFORM 999000-ERRO
005380         WHEN OTHER
005390              SET ERRO-CONTAINER  TO TRUE
005400              PERFORM 999002-TRATA-CONDITION-CICS
005410     END-EVALUATE
005420     .
005430 972000-SAI.
005440     EXIT.
005450
005460*----------------------------------------------------------------*
005470 973000-LER-RESPOSTA             SECTION.
005480*----------------------------------------------------------------*
005490*    Recupera o container com os dados de resposta (saída).
005500*
005510*                        NOME DO CONTAINER RESPOSTA
005520*                        --------------------------
005530*
005540*    Modulo CONTAINER  -     NOME_PGM-RPST
005550*    Modulo IIB (FIXO) -     PRM-DADOS-RPST
005560*
005570*---------------------------------------------------------------*
005610*    Monta o nome dos container de acordo com modulo a executar
005620     MOVE SPACES                 TO NM-CTNR
005630
005640*    Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
005650*    nome_modulo -RPST

005710              MOVE NM-PGM        TO NM-CTNR(1:8)
005720              MOVE '-RPST'       TO NM-CTNR(9:5)
005740
005750*    Recupera o tamanho do container de resposta
005760     PERFORM 973100-LER-CONTAINER-NODATA
005770
005780*Configura dados para leitura do container de resposta
005790     SET  GET-CONTAINER          TO TRUE
005800
005810*Por causa da captura automática de erro CICS, deve ser previsto
005820*o erro de container
005830     SET  ERRO-CONTAINER         TO TRUE
005840
005850
005860*    Le a RESPOSTA de acordo com o programa que a gravou
005890
005900     EVALUATE NM-PGM
005910
005920*        Calcular a quantidade de caracteres esperado e limpar
005930*        a area de dados antes de executar o GET
005940*        Para matrizes com dados variaveis (clausula depending
005950*        on) nao ha como utilizar o comando INITIALIZE
005960
                WHEN SGTSC069
008030              INITIALIZE SGTSC069-RPST
008050              EXEC CICS GET
008060                        CONTAINER (NM-CTNR          )
008070                        INTO      (SGTSC069-RPST    )
008080                        FLENGTH   (QT-CCT-LIDO      )
008090              END-EXEC
005970
                WHEN SGTSI069
008030              INITIALIZE SGTSI069-RPST
008050              EXEC CICS GET
008060                        CONTAINER (NM-CTNR          )
008070                        INTO      (SGTSI069-RPST    )
008080                        FLENGTH   (QT-CCT-LIDO      )
008090              END-EXEC

               WHEN SGTSI094
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSI094-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN SGTS0128
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTS0128-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN SGTS0254
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTS0254-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

006130         WHEN OTHER
                    SET   ERRO-PROGRAMADO
                                       TO  TRUE
                    MOVE  9003         TO  MSG1-CD-ERRO-TS
                    STRING  'Erro na leitura da resposta do módulo '
                            NM-PGM '.'
                            DELIMITED BY SIZE
                            INTO           MSG1-TX-ERRO-TS
006170              PERFORM 999000-ERRO
006180     END-EVALUATE
006190
006200*Ao chegar aqui, há certeza de que não houve erro algum
006210     SET  SEM-ERRO               TO TRUE
006220
006230     .
006240 973000-SAI.
006250     EXIT.
006260*----------------------------------------------------------------*
006270 973100-LER-CONTAINER-NODATA     SECTION.
006280*----------------------------------------------------------------*
006320*Configura dados para leitura do container de requisição
006330     SET  GET-CONTAINER          TO TRUE
006340
006350*Por causa da captura automática de erro CICS, deve ser previsto
006360*o erro de container
006370     SET  ERRO-CONTAINER         TO TRUE
006380
006390*    Obtem a quantidade de caracteres no container
006400     MOVE ZEROS                  TO QT-CCT-LIDO
006410     EXEC CICS GET
006420               CONTAINER (NM-CTNR)
006430               NODATA
006440               FLENGTH   (QT-CCT-LIDO)
006450     END-EXEC
006460
006470*    Ao chegar aqui, há certeza de que não houve erro algum
006480     SET  SEM-ERRO               TO TRUE
006490     .
006500 973100-SAI.
006510     EXIT.
      *----------------------------------------------------------------*
008170 974000-LER-DADOS-OPRD           SECTION.
008180*----------------------------------------------------------------*
008190     MOVE 0016                   TO MSG1-CD-SEQL-ERRO
008200
008210*Configura dados para leitura do container de requisição
008220     SET  GET-CONTAINER          TO TRUE
008230     MOVE 'PRM-INF-OPRD'         TO NM-CTNR
008240
008250*Por causa da captura automática de erro CICS, deve ser previsto
008260*o erro de container
008270     SET  ERRO-CONTAINER         TO TRUE
008280
008290     INITIALIZE PRM-INF-OPRD
008300     EXEC CICS GET
008310          CONTAINER ( NM-CTNR                )
008320          INTO      ( PRM-INF-OPRD           )
008330          FLENGTH   ( LENGTH OF PRM-INF-OPRD )
008340     END-EXEC.
008350
008360*    Ao chegar aqui, há certeza de que não houve erro algum
008370     SET  SEM-ERRO               TO TRUE
008380     .
008390 974000-SAI.
008400     EXIT.
006520*----------------------------------------------------------------*
006530*                                                                *
006540* SECTION DA FAIXA DE 980000 A 989900 PARA GRAVACAO DE DADOS     *
006550*                                                                *
007190*----------------------------------------------------------------*
007200 982000-GRAVAR-CONTAINER-RQSC    SECTION.
007210*----------------------------------------------------------------*
007220*    Postar container com dados de requisição (entrada)
007230*
007240*                        NOME DO CONTAINER REQUISICAO
007250*                        ----------------------------
007260*
007270*    Modulo CONTAINER  -     NOME_PGM-RQSC
007280*    Modulo IIB (FIXO) -     PRM-DADOS-RQSC
007290*
007300*----------------------------------------------------------------*
007310
007320     MOVE 0017                   TO MSG1-CD-SEQL-ERRO
007330
007340*    Monta o nome dos container de acordo com modulo a executar
007350     MOVE SPACES                 TO NM-CTNR
007360     SET PUT-CONTAINER           TO TRUE
007370
007380*    Quando módulos via IIB, fixo nome PRM-DADOS-RQSC, senão
007390*    nome_modulo -RQSC
007400     MOVE NM-PGM        TO NM-CTNR(1:8)
007460     MOVE '-RQSC'       TO NM-CTNR(9:5)
007470
007480
007490*Por causa da captura automática de erro CICS, deve ser previsto
007500*o erro de container
007510     SET  ERRO-CONTAINER         TO TRUE
007520
007530*    Faz put da requisicao do programa a executar
007540     EVALUATE NM-PGM
               WHEN SGTSC069
007600              EXEC CICS PUT
007610                        CONTAINER (NM-CTNR)
007620                        FROM      (SGTSC069-RQSC  )
007630                        FLENGTH   (LENGTH OF SGTSC069-RQSC  )
007640              END-EXEC
               WHEN SGTSI069
007600              EXEC CICS PUT
007610                        CONTAINER (NM-CTNR)
007620                        FROM      (SGTSI069-RQSC )
007630                        FLENGTH   (LENGTH OF SGTSI069-RQSC)
007640              END-EXEC

                WHEN SGTSI094
                    EXEC CICS PUT
                              CONTAINER ( NM-CTNR                 )
                              FROM      ( SGTSI094-RQSC           )
                              FLENGTH   ( LENGTH OF SGTSI094-RQSC )
                    END-EXEC

                WHEN SGTS0128
                    EXEC CICS PUT
                              CONTAINER ( NM-CTNR                 )
                              FROM      ( SGTS0128-RQSC           )
                              FLENGTH   ( LENGTH OF SGTS0128-RQSC )
                    END-EXEC

                WHEN SGTS0254
                    EXEC CICS PUT
                              CONTAINER ( NM-CTNR                 )
                              FROM      ( SGTS0254-RQSC           )
                              FLENGTH   ( LENGTH OF SGTS0254-RQSC )
                    END-EXEC
007650         WHEN OTHER
                    SET   ERRO-PROGRAMADO
                                       TO  TRUE
                    MOVE  9004         TO  MSG1-CD-ERRO-TS
                    STRING  'Erro na gravação da requisição do módulo '
                             NM-PGM '.'
                             DELIMITED BY SIZE
                             INTO           MSG1-TX-ERRO-TS
007690              PERFORM 999000-ERRO
007700     END-EVALUATE
007710
007720*Ao chegar aqui, há certeza de que não houve erro algum
007730     SET  SEM-ERRO               TO TRUE
007740
007750     .
007760 982000-SAI.
007770     EXIT.
007780
007790*----------------------------------------------------------------*
007800 983000-EXCLUIR-RESPOSTA-ERRO    SECTION.
007810*----------------------------------------------------------------*
007820* Exclui o container de resposta e de erro para evitar problemas *
007830* em modulos executados mais de uma vez dentro da mesma sessao do*
007840* CICS. Equivale ao initialize na area de resposta dos programas *
007850* que utilizam COMMAREA.                                         *
007860*----------------------------------------------------------------*
007870
007880     MOVE 0018                   TO MSG1-CD-SEQL-ERRO
007890
007900*    Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
007910*    nome_modulo -RPST

007970              MOVE NM-PGM        TO NM-CTNR(1:8)
007980              MOVE '-RPST'       TO NM-CTNR(9:5)

008010     SET DEL-CONTAINER           TO TRUE
008020
008030*    Exclui o container de resposta
008040*    Não há captura automática de erro CICS por usar RESP e RESP2
008050     EXEC CICS DELETE
008060               CONTAINER (NM-CTNR)
008070               RESP      (MSG1-RESP1)
008080               RESP2     (MSG1-RESP2)
008090     END-EXEC
008100
008110     EVALUATE TRUE
008120         WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
008130         WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
008140              CONTINUE
008150         WHEN OTHER
008160              SET ERRO-CONTAINER TO TRUE
008170              PERFORM 999002-TRATA-CONDITION-CICS
008180     END-EVALUATE
008190
008200*    Excluir o container de erro
008210     PERFORM 983100-EXCLUIR-CONTAINER-ERRO
008220     .
008230 983000-SAI.
008240     EXIT.
008250*
008260*----------------------------------------------------------------*
008270 983100-EXCLUIR-CONTAINER-ERRO   SECTION.
008280*----------------------------------------------------------------*
008290
008300*    Limpar variaveis container de erro
008310     INITIALIZE IIBKERRO-PRM-ERRO
008320
008330*    Exclui o container de erro
008350     MOVE 'PRM-ERRO'             TO NM-CTNR
008360     SET DEL-CONTAINER           TO TRUE
008370
008380*    Não há captura automática de erro CICS por usar RESP e RESP2
008390     EXEC CICS DELETE
008400               CONTAINER (NM-CTNR)
008410               RESP      (MSG1-RESP1)
008420               RESP2     (MSG1-RESP2)
008430     END-EXEC
008440
008450     EVALUATE TRUE
008460         WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
008470         WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
008480              CONTINUE
008490         WHEN OTHER
008500              SET ERRO-CONTAINER TO TRUE
008510              PERFORM 999002-TRATA-CONDITION-CICS
008520     END-EVALUATE
008530     .
008540 983100-SAI.
008550     EXIT.
008560*----------------------------------------------------------------*
008570 984000-GRAVAR-CONTAINER-ERRO    SECTION.
008580*----------------------------------------------------------------*
008610*    Grava o erro gerado no container de erro
008620*    Não há captura automática de erro CICS por usar RESP e RESP2
008630     EXEC CICS PUT
008640               CONTAINER ( 'PRM-ERRO'                  )
008650               FROM      ( IIBKERRO-PRM-ERRO           )
008660               FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
008670               RESP      (MSG1-RESP1                   )
008680               RESP2     (MSG1-RESP2                   )
008690     END-EXEC
008700
008710     .
008720 984000-SAI.
008730     EXIT.
008740
008750*----------------------------------------------------------------*
008760 990000-ENCERRAR                 SECTION.
008770*----------------------------------------------------------------*
008780
008790     EXEC CICS
008800          RETURN
008810     END-EXEC
008820     .
008830 990000-ENCERRAR-SAI.
008840     EXIT.
008850*
008860*----------------------------------------------------------------*
008870*                                                                *
008880* As sections abaixo iniciadas com 999000 representam os tratamen*
008890* tos de erro utilizando o Sistema MSG (rotina MSGS0001)         *
008900*                                                                *
008910*----------------------------------------------------------------*
008920 999000-ERRO                     SECTION.
008930*----------------------------------------------------------------*
008940
008950     IF MSG1-CD-ERRO-TS EQUAL ZEROS
008960        MOVE 8888                TO MSG1-CD-ERRO-TS
008970     END-IF
008980     IF MSG1-TX-ERRO-TS EQUAL SPACES
008990        MOVE 'ERRO NAO ESPECIFICADO NO MODULO' TO MSG1-TX-ERRO-TS
009000     END-IF
009010     PERFORM 999000-GRAVA-ERRO-E-ENCERRA
009020     .
009030 999000-ERRO-SAI.
009040     EXIT.
009050
009060*----------------------------------------------------------------*
009070*     As sections abaixo representam os tratamentos de erro
009080*    utilizando o sistema MSG de mensageria (rotina MSGS0001)
009090*-----------------------------------------------------------------
009100 999000-GRAVA-ERRO-E-ENCERRA     SECTION.
009110*-----------------------------------------------------------------
009120
009130*    Em caso de erro, desfaz as atualizações em banco de dados
009140
009150     EXEC CICS SYNCPOINT
009160               ROLLBACK
009170     END-EXEC
009180
009190*Verifica se o tipo de erro gerado possui tratamento interno
009200     EVALUATE TRUE
009210         WHEN ERRO-CONTAINER
009220              PERFORM 999004-TRATA-ERRO-CTNR
009230         WHEN ERRO-TAMANHO-AREA
009240              PERFORM 999005-TRATA-ERRO-AREA-DADOS
009250         WHEN ERRO-CANAL
009260              PERFORM 999006-TRATA-ERRO-CANAL
009270         WHEN OTHER
009280              CONTINUE
009290     END-EVALUATE
009300
009310*Adiciona as variáveis de entrada para monitoração

               MOVE 'SGTS0151-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE SGTS0151-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

009330         MOVE 'SGTS0151-CD-OPR-ETRD-MCP'
009340                                 TO MSGKW999-NM-VRV-MNT
               IF  SGTS0151-CD-OPR-ETRD-MCP IS NUMERIC
003900                   MOVE SGTS0151-CD-OPR-ETRD-MCP
                                       TO CD-OPR-ETRD-MCP-DISPLAY
                         MOVE CD-OPR-ETRD-MCP-DISPLAY
                                       TO MSGKW999-VL-VRV-MNT
               ELSE
                          MOVE 'Código não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
003880         END-IF

009370         PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0151-NR-VRS-OPR-ETRD'
009340                                 TO MSGKW999-NM-VRV-MNT
009350         MOVE  SGTS0151-NR-VRS-OPR-ETRD
009360                                 TO MSGKW999-VL-VRV-MNT
009370         PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0151-TX-OBS-VRS'
009340                                 TO MSGKW999-NM-VRV-MNT
009350         MOVE  SGTS0151-TX-OBS-VRS
009360                                 TO MSGKW999-VL-VRV-MNT
009370         PERFORM 999003-ADICIONA-VRV-MNT
009400
009410*    Grava o erro gerado no sistema MSG - Mensageria
009420     PERFORM MSG1-NTF-MSG
009430
009440     MOVE MSG1-CD-ERRO-TS        TO IIBKERRO-CD-ERRO
009450     MOVE MSG1-TX-ERRO-TS        TO IIBKERRO-TX-ERRO
009460
009470*    Grava o erro gerado no container de erro
009480     PERFORM 984000-GRAVAR-CONTAINER-ERRO
009490
009500*    Encerra a transacao
009510     PERFORM 990000-ENCERRAR
009520     .
009530 999000-SAI.
009540     EXIT.
009550* --------------------------------------*
009560* 999001-TRATA-ABEND-CICS        SECTION.
009570* 999002-TRATA-CONDITION-CICS    SECTION.
009580* 999003-ADICIONA-VRV-MNT        SECTION.
009590* --------------------------------------*
-INC HLPKP999
009610* --------------------------------------*
009620* 999004-TRATA-ERRO-CTNR         SECTION.
009630* 999005-TRATA-ERRO-AREA-DADOS   SECTION.
009640* 999006-TRATA-ERRO-CANAL        SECTION.
009650*---------------------------------------*
-INC SGTKP999
009670* --------------------------------------*
009680* MSG1-NTF-MSG                   SECTION.
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
009690* --------------------------------------*
-INC MSGKPMG1
009710*---------------------- FIM SGTS0151 -----------------------------