000010*-----------------------------------------------------------------
000020* Programa..: SGTS0030
000030* Analista..: F8650584 - Roberto Fernandes Lima
000040* Autor.....: F8650584 - Roberto Fernandes Lima
000050* Data......: 30.07.2015
000060* Objetivo..: Alterar Especies de Debentures Cadastradas no SGT
      * COMPILACAO: 52 - PSOSE600 - Cobol 4.2 c/otimizacao p/producao
000080*-----------------------------------------------------------------
      * VRS003 06/12/2019 - F0950413 DE 2016/145195 - Alteração  varia-
      *        vel QT-OPR-VCLD-TX para QT-OPR-VCLD-TEXTO, P/ erro 1037.
      * VRS002 03.06.2019 - F0950413 - DE 2016/145195 - Adequação das
      *        das mensagens de erro ao padrão da mensageria,
      *        Alteração da chamada de módulos com comando 'CALL' para,
      *        'EXEC CICS LINK (COMMAREA)'.
      * VRS001 28.07.2015 - F8650584 P9327 DE 2015/120821 - Implantacao.
000100*-----------------------------------------------------------------
000110
000120*-----------------------------------------------------------------
000130 IDENTIFICATION DIVISION.
000140*-----------------------------------------------------------------
000150 PROGRAM-ID. SGTS0030.
000160
000170*-----------------------------------------------------------------
000180 ENVIRONMENT DIVISION.
000190*-----------------------------------------------------------------
000200 CONFIGURATION                   SECTION.
000210 SPECIAL-NAMES.
000220     DECIMAL-POINT IS COMMA.
000230
000240*-----------------------------------------------------------------
000250 DATA DIVISION.
000260*-----------------------------------------------------------------
000270
000280*-----------------------------------------------------------------
000290 WORKING-STORAGE                 SECTION.
000300*--------------------Variaveis do Sistema-------------------------
000310 77  CTE-INICIO-SS               PIC  X(035) VALUE
000320     '*** Storage Section Comeca Aqui ***'.
000330 01  CTE-PROG                    PIC  X(17)
000340     VALUE '*** SGTS0030 ***'.
000350 01  CTE-VERS                    PIC  X(06) VALUE 'VRS003'.
000360
      *--> Módulos a serem acionados via 'EXEC CICS LINK (COMMAREA)'
       01  TAB-PROGRAMAS-CHAMADOS.
000370     03  SGTSLJ04                PIC  X(08) VALUE 'SGTSLJ04'.
000380     03  SGTST031                PIC  X(08) VALUE 'SGTST031'.

      *--> Definição dos Variaveis do Cobolito
-INC ALMKCBL1

      *-----------------------------------------------------------------
      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informacoes
      *-----------------------------------------------------------------
-INC HLPKE01B

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

      *--> Area de Trabalho
       01  CD-CHV-USU                  PIC  X(08) VALUE SPACES.

       01  QT-OPR-VCLD                 PIC  X(09) VALUE SPACES.
       01  QT-OPR-VCLD-TEXTO           PIC  X(10) VALUE SPACES.
       01  QT-ZEROS                    PIC  9(04) VALUE ZEROS.

      *--> Máscaras edição de variáveis p/ monitoração mensagens de erro
       01  MCR-S9-15                   PIC      ---------------9.

000690*-------------------------- B O O K S ----------------------------

      *--> Book do container de Erro (IIB)
-INC IIBKERRO

      *--> Book para captura de abend cics da bloco codigo HLPKW999
-INC HLPKW999

      *--> Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1

      *--> Book para captura de erro via MSG1
-INC MSGKWMG1

      *--> Verificar Operacoes Vinculadas
       01  SGTSLJ04-DADOS.
-INC SGTKLJ04

      *--> Book Lista ESPC_DBTR_OPR
       01  SGTST031-DADOS.
-INC SGTKT031

      *--> Book da Subrotina SGTS0030
       01  SGTS0030-DADOS.
-INC SGTK0030

      *------------------- Variaveis do Sistema ------------------------
       01  CTE-SAI-SS                  PIC  X(40) VALUE
           '*** Storage Section Termina Aqui ***'.

      *-----------------------------------------------------------------
       PROCEDURE                       DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------
           MOVE  0001                  TO  MSG1-CD-SEQL-ERRO

      *--> Habilita Testes no Cobolito e Debug de Operacoes IIB
-INC ALMKCBL2

      *--> Habilita captura automatica de abend e erros CICS
-INC HLPKE999

           PERFORM  100000-BUSCAR-REQUISICAO

           PERFORM  200000-BUSCAR-MATRICULA

           PERFORM  300000-PROCESSAR-NEGOCIO

           PERFORM  400000-GRAVAR-RESPOSTA

           PERFORM  999999-ENCERRAR

           GOBACK
           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-BUSCAR-REQUISICAO        SECTION.
      *--> Obtem  Parametros de Requisicao ou Dados de Entrada
      *-----------------------------------------------------------------
           MOVE  0002                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      SGTS0030-RQSC

           MOVE  'SGTS0030-RQSC'       TO NM-CTNR

           EXEC CICS  GET
                      CONTAINER ( NM-CTNR                 )
                      INTO      ( SGTS0030-RQSC           )
                      FLENGTH   ( LENGTH OF SGTS0030-RQSC )
                      RESP      ( MSG1-RESP1              )
                      RESP2     ( MSG1-RESP2              )
           END-EXEC

           EVALUATE  TRUE
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(NORMAL)
                     CONTINUE
               WHEN  OTHER
                     SET   ERRO-CONTAINER
                                       TO  TRUE
                     MOVE  3000        TO  MSG1-CD-ERRO-TS
                     MOVE  'Erro na manipulação de Container de dados.'
                                       TO  MSG1-TX-ERRO-TS
                     PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-BUSCAR-MATRICULA         SECTION.
      *--> Obtem a Matrícula do Usuario Logado
      *-----------------------------------------------------------------
           MOVE  0003                  TO  MSG1-CD-SEQL-ERRO

           MOVE  'PRM-INF-OPRD'        TO  NM-CTNR

           EXEC CICS  GET
                      CONTAINER ( NM-CTNR              )
                      INTO      ( CD-CHV-USU           )
                      FLENGTH   ( LENGTH OF CD-CHV-USU )
                      RESP      ( MSG1-RESP1           )
                      RESP2     ( MSG1-RESP2           )
           END-EXEC

           EVALUATE  TRUE
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(NORMAL)
                     CONTINUE
               WHEN  OTHER
                     SET   ERRO-CONTAINER
                                       TO  TRUE
                     MOVE  0200        TO  MSG1-CD-ERRO-TS
                     MOVE  'Erro ao obter código do usuário.'
                                       TO  MSG1-TX-ERRO-TS
                     PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-PROCESSAR-NEGOCIO        SECTION.
      *-----------------------------------------------------------------
           MOVE  0004                  TO  MSG1-CD-SEQL-ERRO

           PERFORM  310000-VALIDAR-PARAMETROS

           IF  SGTS0030-IN-ESPC-DBTR-ATV   EQUAL  'N'
               PERFORM  320000-VERIFICAR-OPR-VINCULADA
           END-IF

           PERFORM 330000-ALTERAR-ESPC-DEBENTURE

           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       310000-VALIDAR-PARAMETROS       SECTION.
      *-----------------------------------------------------------------
           MOVE  0005                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      SGTST031-DADOS

           IF  SGTS0030-CD-ESPC-DBTR-OPR   EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0016              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da espécie de debênture da operação inválid
      -              'o.'              TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0030-IN-ESPC-DBTR-ATV  NOT EQUAL  'S' AND 'N'
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0228              TO  MSG1-CD-ERRO-TS
               MOVE  'Indicador de Espécie de Debênture Ativo Inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       310000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       320000-VERIFICAR-OPR-VINCULADA  SECTION.
      *--> Verificar se existem Especies de Debentures com operacoes
      *    vinculadas nos estados:
      *    1-Em Cadastramento; 2- Autorizar Compra;
      *    3-Disparar Ordem de Compra 4-A Encarteirar
      *-----------------------------------------------------------------
           MOVE  0006                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      SGTSLJ04-DADOS

           SET   SGTSLJ04-ESPC-VCLD-OPR
                                       TO  TRUE

           MOVE  SGTS0030-CD-ESPC-DBTR-OPR
                                       TO  SGTSLJ04-CD-PRM

      *--> Movimenta o nome do modulo para monitoracao de variaveis
           MOVE  SGTSLJ04              TO  NM-PGM

           EXEC CICS  LINK
                      PROGRAM  ( NM-PGM                   )
                      COMMAREA ( SGTSLJ04-DADOS           )
                      LENGTH   ( LENGTH OF SGTSLJ04-DADOS )
           END-EXEC

           IF  SGTSLJ04-CD-RTN-PGM         NOT EQUAL  ZEROS
               SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE  3004              TO  MSG1-CD-ERRO-TS
               STRING  'Erro ao acionar o módulo ' NM-PGM '.'
                       DELIMITED BY SIZE
                       INTO                MSG1-TX-ERRO-TS
               END-STRING
               MOVE  NM-PGM            TO  MSG1-NM-PGM-ACND
               MOVE  SGTSLJ04-MSG-ERRO-PGM
                                       TO  MSG1-TX-RTN-PGM-ACND
               MOVE  SGTSLJ04-CD-RTN-PGM
                                       TO  MSG1-CD-RTN-PGM-ACND
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *--> Avaliar a Quantidade de Operações Estruturadas Vinculadas
      *    na Espécie de Debênture
           MOVE  SGTSLJ04-QT-OPR-VCLD  TO  SGTS0030-QT-OPR-VCLD

           IF  SGTS0030-QT-OPR-VCLD        GREATER  ZEROS
               SET ERRO-PROGRAMADO     TO  TRUE
               MOVE  1037              TO  MSG1-CD-ERRO-TS
               PERFORM  321000-FORMATAR-MSG-OPR-VCLD
               STRING  'Não é permitida alteração, existem '
                       DELIMITED BY SIZE
                       QT-OPR-VCLD-TEXTO
                       DELIMITED BY SPACE
                       ' operaçoes vinculadas nos estados'
                       ' anteriores ao encarteirado.'
                       DELIMITED BY SIZE
                       INTO                MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       320000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       321000-FORMATAR-MSG-OPR-VCLD    SECTION.
      *--> Retira os Zeros não significativos e formata a mensagem com a
      *    quantidade de operações vinculadas nos estados de 01 a 04
      *-----------------------------------------------------------------
           MOVE  0007                  TO  MSG1-CD-SEQL-ERRO


           MOVE  SGTS0030-QT-OPR-VCLD  TO  QT-OPR-VCLD

           INSPECT  QT-OPR-VCLD
                    TALLYING QT-ZEROS FOR LEADING '0'

           MOVE  QT-OPR-VCLD(QT-ZEROS + 1:9 - QT-ZEROS)
                                       TO  QT-OPR-VCLD-TEXTO

           STRING  QT-OPR-VCLD-TEXTO
                   DELIMITED BY SIZE
                   ' '
                   DELIMITED BY SIZE
                   INTO                    QT-OPR-VCLD-TEXTO

           .
       321000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       330000-ALTERAR-ESPC-DEBENTURE   SECTION.
      *-----------------------------------------------------------------
           MOVE  0008                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      SGTST031-DADOS

           SET   SGTST031-ALTERAR      TO  TRUE

           MOVE  SGTS0030-CD-ESPC-DBTR-OPR
                                       TO  SGTST031-CD-ESPC-DBTR-OPR
           MOVE  SGTS0030-IN-ESPC-DBTR-ATV
                                       TO  SGTST031-IN-ESPC-DBTR-ATV
           MOVE  CD-CHV-USU            TO  SGTST031-CD-USU-ATL
           MOVE  'CURRENT'
                                       TO  SGTST031-TS-ATL
           MOVE  SGTST031              TO  NM-PGM

           EXEC CICS  LINK
                      PROGRAM  ( NM-PGM                   )
                      COMMAREA ( SGTST031-DADOS           )
                      LENGTH   ( LENGTH OF SGTST031-DADOS )
           END-EXEC

           IF  SGTST031-CD-RTN-PGM         NOT EQUAL  ZEROS
               SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE  3004              TO  MSG1-CD-ERRO-TS
               STRING  'Erro ao acionar o módulo ' NM-PGM '.'
                       DELIMITED BY SIZE
                       INTO                MSG1-TX-ERRO-TS
               END-STRING
               MOVE  NM-PGM            TO  MSG1-NM-PGM-ACND
               MOVE  SGTST031-MSG-ERRO-PGM
                                       TO  MSG1-TX-RTN-PGM-ACND
               MOVE  SGTST031-CD-RTN-PGM
                                       TO  MSG1-CD-RTN-PGM-ACND
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           MOVE  'Alteração Espécie de Debênture efetivada com sucesso.'
                                       TO  SGTS0030-TX-MSG-RTN-PRCT

           .
       330000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       400000-GRAVAR-RESPOSTA          SECTION.
      *--> Retorna Parametros de Resposta ou Dados de Saida.
      *-----------------------------------------------------------------
           MOVE  0009                  TO  MSG1-CD-SEQL-ERRO

           MOVE  'SGTS0030-RPST'       TO  NM-CTNR

           EXEC CICS  PUT
                      CONTAINER ( NM-CTNR                 )
                      FROM      ( SGTS0030-RPST           )
                      FLENGTH   ( LENGTH OF SGTS0030-RPST )
                      RESP      ( MSG1-RESP1              )
                      RESP2     ( MSG1-RESP2              )
           END-EXEC

           EVALUATE  TRUE
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(NORMAL)
                     CONTINUE
               WHEN  OTHER
                     SET   ERRO-CONTAINER
                                       TO  TRUE
                     MOVE  3000        TO  MSG1-CD-ERRO-TS
                     MOVE  'Erro na manipulação de Container de dados.'
                                       TO  MSG1-TX-ERRO-TS
                     PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE
           .
       400000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------

           IF  ERRO-PROGRAMA-ACIONADO
               PERFORM 999100-MONITORAR-VRV-PGM-CHMD
           END-IF

           PERFORM 999200-MONITORAR-VRV-PGM-E-ENC
           .
       999000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999100-MONITORAR-VRV-PGM-CHMD   SECTION.
      *-----------------------------------------------------------------
      *--> Monitora as variáveis dos módulos chamados

           EVALUATE  NM-PGM
      *------> Monitora as Variaveis de Entrada do SGTSLJ04
               WHEN  SGTSLJ04
                     MOVE 'SGTSLJ04-ACAO'        TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSLJ04-ACAO         TO  MCR-S9-15
                     MOVE  MCR-S9-15             TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSLJ04-CD-PRM'      TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSLJ04-CD-PRM       TO  MCR-S9-15
                     MOVE  MCR-S9-15             TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSLJ04-CD-ITC-CVM-E' TO MSGKW999-NM-VRV-MNT
                     MOVE  SGTSLJ04-CD-ITC-CVM-E  TO MCR-S9-15
                     MOVE  MCR-S9-15              TO MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSLJ04-CD-ITC-CVM-OPR-E'
                                                 TO MSGKW999-NM-VRV-MNT
                     MOVE  SGTSLJ04-CD-ITC-CVM-OPR-E
                                                 TO MCR-S9-15
                     MOVE  MCR-S9-15             TO MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSLJ04-CD-TIP-COTA-TIT'
                                                 TO MSGKW999-NM-VRV-MNT
                     MOVE  SGTSLJ04-CD-TIP-COTA-TIT
                                                 TO MCR-S9-15
                     MOVE  MCR-S9-15             TO MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSLJ04-NR-VRS-TIP-COTA'
                                                 TO MSGKW999-NM-VRV-MNT
                     MOVE  SGTSLJ04-NR-VRS-TIP-COTA
                                                 TO MCR-S9-15
                     MOVE  MCR-S9-15             TO MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSLJ04-CD-CTGR-BC'  TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSLJ04-CD-CTGR-BC   TO  MCR-S9-15
                     MOVE  MCR-S9-15             TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSLJ04-DCR-CTGR-BC' TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSLJ04-DCR-CTGR-BC  TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

      *------> Monitora as Variaveis de Entrada do SGTST031
               WHEN  SGTST031
                     MOVE 'SGTST031-ACAO'        TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST031-ACAO         TO  MCR-S9-15
                     MOVE  MCR-S9-15             TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTST031-CD-ESPC-DBTR-OPR'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST031-CD-ESPC-DBTR-OPR  TO MCR-S9-15
                     MOVE  MCR-S9-15             TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  OTHER
                     CONTINUE
           END-EVALUATE
           .
       999100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999200-MONITORAR-VRV-PGM-E-ENC  SECTION.
      *-----------------------------------------------------------------

           IF  MSG1-CD-ERRO-TS             EQUAL  ZEROS
               MOVE  8888              TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS             EQUAL  SPACES
               MOVE  'Erro não especificado no módulo.'
                                       TO  MSG1-TX-ERRO-TS
           END-IF

      *--> Verifica se o tipo de erro gerado possui tratamento interno
           EVALUATE  TRUE
               WHEN  ERRO-CONTAINER
                     PERFORM  999005-TRATA-ERRO-CTNR
               WHEN  OTHER
                     CONTINUE
           END-EVALUATE

      *--> Adiciona Campos de Entrada
           MOVE 'SGTS0030-CD-ESPC-DBTR-OPR'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0030-CD-ESPC-DBTR-OPR
                                       TO  MCR-S9-15
           MOVE  MCR-S9-15
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTS0030-IN-ESPC-DBTR-ATV'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0030-IN-ESPC-DBTR-ATV
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'QT-OPR-VCLD-TEXTO'    TO  MSGKW999-NM-VRV-MNT
           MOVE  QT-OPR-VCLD-TEXTO     TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

      *--> Grava a Ocorrencia de Erro no Sistema MSG - Mensageria
           PERFORM  MSG1-NTF-MSG

           MOVE  MSG1-CD-ERRO-TS       TO  IIBKERRO-CD-ERRO
           MOVE  MSG1-TX-ERRO-TS       TO  IIBKERRO-TX-ERRO

      *--> Grava a Ocorrencia de Erro no Container de Erro
           EXEC CICS  PUT
                      CONTAINER ( 'PRM-ERRO'                  )
                      FROM      ( IIBKERRO-PRM-ERRO           )
                      FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                      RESP      ( MSG1-RESP1                  )
                      RESP2     ( MSG1-RESP2                  )
           END-EXEC

      *--> Encerra a transacao
           PERFORM  999999-ENCERRAR
           .
       999200-SAI.
           EXIT.

      *----------------------------------------
      * 999001-TRATA-ABEND-CICS        SECTION.
      * 999002-TRATA-CONDITION-CICS    SECTION.
      * 999003-ADICIONA-VRV-MNT        SECTION.
      * 999004-ALINHA-NUMERO-ESQUERDA  SECTION.
      *----------------------------------------
-INC HLPKP999

      *-----------------------------------------------------------------
       999005-TRATA-ERRO-CTNR          SECTION.
      *--> Tratamento para erro de container
      *-----------------------------------------------------------------

           SET  ERRO-CICS              TO TRUE

      *--> Movimenta variáveis de container para monitoração
           MOVE 'Erro Container 01 - Programa'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  NM-PGM                TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'Erro Container 01 - Ação'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  NM-ACAO               TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'Erro Container 03 - Container'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  NM-CTNR               TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           IF  NM-CNL                      EQUAL  SPACES
               EXEC CICS  ASSIGN
                          CHANNEL ( NM-CNL )
               END-EXEC
           END-IF

           MOVE 'Erro Container 04 - Canal'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  NM-CNL                TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'Erro Container 05 - EIBRESP'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  EIBRESP               TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'Erro Container 06 - TX-EIBRESP'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  HLPS0201-TX-EIBRESP   TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'Erro Container 07 - EIBRESP2'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  EIBRESP2              TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'Erro Container 08 - EIBFN'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  EIBFN                 TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'Erro Container 09 - TX-EIBFN'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  HLPS0201-TX-EIBFN     TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           .
       999005-SAI.
           EXIT.

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
004080*---------------------- FIM SGTS0030 -----------------------------