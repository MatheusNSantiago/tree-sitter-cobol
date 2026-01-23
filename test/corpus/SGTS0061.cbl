      *-----------------------------------------------------------------
      * PROGRAMA..: SGTS0061
      * ANALISTA..: F2054109 - Cintia Ap. Soares Iglesias
      * AUTOR.....: F2021264 - Christine Mitsue Akahoshi
      * DATA......: 07.12.2015
      * OBJETIVO..: Detalhar Modelo de Ordem de Negociação
      * COMPILACAO: COBOL/CICS
      *-----------------------------------------------------------------
      * VRS002 10.05.2019 F9188847 - Acerto Mensageria
      * VRS001 30.01.2019 F0950413 inclusão da sigla 'SGT' na section
      *                   972000-LER-CONTAINER-DE-ERRO
      *        07.12.2015 F2021264  DE 2013/120821 - Implantação
      *-----------------------------------------------------------------
      
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID. SGTS0061.
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
                    '*** SGTS0061 ***'.
       77  CTE-VERS                    PIC  X(006)  VALUE 'VRS001'.
      
      * Módulos acionados
       77  IIBP2000                    PIC  X(008) VALUE 'IIBP2000'.
       77  SGTST045                    PIC  X(008) VALUE 'SGTST045'.
       77  SGTST049                    PIC  X(008) VALUE 'SGTST049'.
       77  SGTST050                    PIC  X(008) VALUE 'SGTST050'.
       77  ATCS6000                    PIC  X(008) VALUE 'ATCS6000'.
       77  PRDS0203                    PIC  X(008) VALUE 'PRDS0203'.
      *----------------------------------------------------------------*
       LOCAL-STORAGE                   SECTION.
      *----------------------------------------------------------------*
      * Areas de trabalho
      
       01  OPCAO                       PIC S9(004) COMP-5 VALUE 0.
       01  CD-USU-INCL                 PIC  X(008) VALUE ' '.
       01  CD-USU-CNFC                 PIC  X(008) VALUE ' '.
       01  CD-USU-ECR                  PIC  X(008) VALUE ' '.
       01  CD-USU-ULT                  PIC  X(008) VALUE ' '.
      
       01  IN-SQLCODE                  PIC  X(001) VALUE ' '.
           88  IN-SQL-S                VALUE 'S'.
           88  IN-SQL-N                VALUE 'N'.
      
      *----------------------------------------------------------------*
      * Book dos Containers e das subrotinas chamadas
      *----------------------------------------------------------------*
      
      * Book das variáveis auxiliares do tratamento de erro via MSG
       COPY SGTKERR1.                                                           0000
      
      * Book para captura de abend cics da bloco codigo MSGKP999
       COPY HLPKW999.                                                           0000
      
      * Book para captura de erro via MSG1
       COPY MSGKWMG1.                                                           0000
      
      * Book do container de requisição e resposta deste modulo
       01  SGTS0061-DADOS.
       COPY SGTK0061.                                                           0000
      
      * CRUD da tabela TIP_NEGO     
       01  SGTST045-DADOS.
       COPY SGTKT045.                                                           0000
      
      * CRUD da tabela MOD_ORD_NEGO
       01  SGTST049-DADOS.
       COPY SGTKT049.                                                           0000
      
      * CRUD da tabela EST_MOD_ORD_NEGO   
       01  SGTST050-DADOS.
       COPY SGTKT050.                                                           0000
      
      * Book do modulo  PRDS0203 - Via Link
       01  PRDS0203-DADOS.
       COPY PRDK0203.                                                           0000
      
      * Book do modulo  ATCS6000 - Via IIB
       01  ATCS6000-DADOS.
       COPY ATCK6000.                                                           0000
      
      * Book do Container de Erro
       COPY IIBKERRO.                                                           0000
      
      * Book do container de Serviços (para acionamento de serviços)
       COPY IIBKSRVC.                                                           0000
      
      
       77  CTE-FINAL-SS                PIC  X(040)  VALUE
                    '*** S.S. TERMINA AQUI ***'.
      
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
      
      *----------------------------------------------------------------*
       000000-ROTINA-PRINCIPAL         SECTION.
      *----------------------------------------------------------------*
      
           PERFORM 100000-INICIAR
      
           PERFORM 200000-PROCESSAR
      
           PERFORM 990000-ENCERRAR

           GOBACK
           .
       000000-SAI.
           EXIT.
      
      *----------------------------------------------------------------*
       100000-INICIAR                  SECTION.
      *----------------------------------------------------------------*
           MOVE 0001                   TO MSG1-CD-SEQL-ERRO
      
      * Ativa a captura automatica de abend do CICS.
      * WHENEVER CICS
       COPY MSGKE999.                                                           0000
      
           PERFORM 970000-LER-CANAL
      
      * Define o nome do programa p/ fins manipulacao container
           MOVE 'SGTS0061'             TO NM-PGM
           MOVE 'SGTS0061-RPST'        TO NM-CTNR
      
      * Limpa a resposta e erro antes de executar o modulo
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO
      
           MOVE SPACES                 TO SGTS0061-RPST
           INITIALIZE  SGTS0061-RPST
               REPLACING NUMERIC       BY  ZEROS
      
      * Recupera a requisição do modulo
           PERFORM 971000-LER-REQUISICAO
      
           PERFORM 110000-VALIDAR-REQUISICAO
           .
       100000-SAI.
           EXIT.
      
      *----------------------------------------------------------------*
       110000-VALIDAR-REQUISICAO       SECTION.
      *----------------------------------------------------------------*
      * Valida campo a campo da requisicao
      *----------------------------------------------------------------*
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO
      
           IF  SGTS0061-CD-MOD-ORD-NEGO  = ZEROS
              SET  ERRO-PROGRAMADO    TO TRUE
              MOVE 0641               TO MSG1-CD-ERRO-TS
              MOVE 'Código modelo de ordem de negociação não informado.'
                                       TO MSG1-TX-ERRO-TS
              PERFORM 999000-ERRO
           END-IF
           .
       110000-SAI.
           EXIT.
      
      *----------------------------------------------------------------*
       200000-PROCESSAR                SECTION.
      *----------------------------------------------------------------*
           MOVE 0003                   TO MSG1-CD-SEQL-ERRO
      
      * Buscar dados do Modelo de negociação
           PERFORM 210000-BUSCAR-MOD-NEGO
      
           MOVE SGTST049-CD-TIP-NEGO   TO SGTS0061-CD-TIP-NEGO
           MOVE SGTST049-CD-PRD        TO SGTS0061-CD-PRD
           MOVE SGTST049-CD-MDLD       TO SGTS0061-CD-MDLD
           MOVE SGTST049-NM-MOD-ORD-NEGO
                                       TO SGTS0061-NM-MOD-ORD-NEGO
           MOVE SGTST049-IN-MOD-ORD-ATI
                                       TO SGTS0061-IN-MOD-ORD-ATI
      
      * Buscar dados do Tipo de negociação informado
           PERFORM 220000-BUSCAR-TIP-NEGO
      
           MOVE SGTST045-NM-TIP-NEGO   TO SGTS0061-NM-TIP-NEGO
      
      * Buscar dados de Produto e Modalidade
           IF  SGTS0061-CD-PRD         NOT = 0
               PERFORM 230000-BUSCAR-PRD-MDLD
               MOVE S0203-NM-PRD-S (1)     TO SGTS0061-NM-PRD
               MOVE S0203-NM-MDLD-S (1)    TO SGTS0061-NM-MDLD
           END-IF
      
      * Tratar dados do Estado do modelo de negociação
           PERFORM 240000-TRATAR-EST-MOD
      
      * Tratar nomes de usuários
           PERFORM 250000-TRATAR-USUARIO
      
           PERFORM 899000-MOVIMENTAR-SAIDA
           .
       200000-SAI.
           EXIT.
      
      *----------------------------------------------------------------*
       210000-BUSCAR-MOD-NEGO          SECTION.
      *----------------------------------------------------------------*
           MOVE 0004                   TO MSG1-CD-SEQL-ERRO
      
           MOVE SPACES                 TO SGTST049-DADOS
           INITIALIZE  SGTST049-DADOS
               REPLACING NUMERIC       BY  ZEROS
      
      * Opção 4 - Consultar
           MOVE 4                      TO SGTST049-ACAO
           MOVE SGTS0061-CD-MOD-ORD-NEGO
                                       TO SGTST049-CD-MOD-ORD-NEGO
      
      * Mover nome do módulo para monitoração de variáveis
           MOVE SGTST049               TO NM-PGM
      
           EXEC CICS LINK
                PROGRAM  ( SGTST049       )
                COMMAREA ( SGTST049-DADOS )
           END-EXEC
      
           IF  SGTST049-CD-RTN-PGM     NOT = ZEROS
               SET  ERRO-PROGRAMA-ACIONADO TO TRUE
               MOVE SGTST049-MSG-ERRO-PGM
                                       TO MSG1-TX-RTN-PGM-ACND
                                          MSG1-TX-ERRO-TS
               MOVE NM-PGM             TO MSG1-NM-PGM-ACND
               MOVE SGTST049-CD-RTN-PGM
                                       TO MSG1-CD-RTN-PGM-ACND
                                          MSG1-CD-ERRO-TS
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-ERRO
           END-IF
           .
       210000-SAI.
           EXIT.
      
      *----------------------------------------------------------------*
       220000-BUSCAR-TIP-NEGO          SECTION.
      *----------------------------------------------------------------*
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO
      
           MOVE SPACES                 TO SGTST045-DADOS
           INITIALIZE  SGTST045-DADOS
               REPLACING NUMERIC       BY  ZEROS
      
      * Opção 4 - Consultar
           MOVE 4                      TO SGTST045-ACAO
           MOVE SGTS0061-CD-TIP-NEGO   TO SGTST045-CD-TIP-NEGO
      
      * Mover nome do módulo para monitoração de variáveis
           MOVE SGTST045               TO NM-PGM
      
           EXEC CICS LINK
                PROGRAM  ( SGTST045       )
                COMMAREA ( SGTST045-DADOS )
           END-EXEC
      
           IF  SGTST045-CD-RTN-PGM     NOT = ZEROS
               SET  ERRO-PROGRAMA-ACIONADO TO TRUE
               MOVE SGTST045-MSG-ERRO-PGM
                                       TO MSG1-TX-RTN-PGM-ACND
                                          MSG1-TX-ERRO-TS
               MOVE NM-PGM             TO MSG1-NM-PGM-ACND
               MOVE SGTST045-CD-RTN-PGM
                                       TO MSG1-CD-RTN-PGM-ACND
                                          MSG1-CD-ERRO-TS
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-ERRO
           END-IF
           .
       220000-SAI.
           EXIT.
      
      *----------------------------------------------------------------*
       230000-BUSCAR-PRD-MDLD          SECTION.
      *----------------------------------------------------------------*
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO
      
           MOVE SPACES                 TO PRDS0203-DADOS
           INITIALIZE  PRDS0203-DADOS
               REPLACING NUMERIC       BY  ZEROS
      
      * Movimentar o nome do programa a ser executado
           MOVE PRDS0203               TO NM-PGM
      
      * Opção 1 - Consultar
           MOVE 1                      TO S0203-CD-FUNCAO
           MOVE SGTS0061-CD-PRD        TO S0203-CD-PRD
           MOVE SGTS0061-CD-MDLD       TO S0203-CD-MDLD
      
      * Formatar código de erro para cada acesso ao PRDS0203
      * que sera utilizado caso haja retorno de erro do modulo.
      *    MOVE 0004                   TO MSG1-CD-ERRO-TS

           PERFORM 910000-CHAMAR-SERVICO

           PERFORM 973200-TRATAR-RPST-PRD
           .
       230000-SAI.
           EXIT.
      
      *----------------------------------------------------------------*
       240000-TRATAR-EST-MOD           SECTION.
      *----------------------------------------------------------------*
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO
      
           MOVE SPACES                 TO SGTST050-DADOS
           INITIALIZE  SGTST050-DADOS
               REPLACING NUMERIC       BY  ZEROS
      
      * Opção 5 - Consultar Estado atual
           MOVE 5                      TO OPCAO
      * Teste de sqlcode = 'S'
           SET  IN-SQL-S               TO TRUE
      
           PERFORM 930000-CHAMAR-SGTST050
      
           MOVE SGTST050-CD-TIP-EST-DCS
                                       TO SGTS0061-CD-TIP-EST-DCS
      
      * Se estado = 1 (pendente de confirmação), carregar dados da
      * ultima atualização do modelo
      * Senão, tratar o estado encontrado:
      *  Estado = 2 - Aprovado
      *  Estado = 3 - Rejeitado (carregar a justificativa da rejeição)
      *  Estado = 99 - Encerrado (buscas dados da inclusão e/ou
      *                           aprovação)
      *  Outros = não previsto
      *  Para qualquer estado encontrado, buscar o estado de Inclusão
      *  (que pode ser o mesmo recuperado agora)
           EVALUATE  SGTST050-CD-TIP-EST-DCS
               WHEN  1
                   MOVE SGTST050-TS-EST-MOD-ORD
                                       TO SGTS0061-TS-EST-MOD-ULT
                   MOVE SGTST050-CD-USU-ATL-EST
                                       TO SGTS0061-CD-USU-ATL-ULT
                                          CD-USU-ULT
                   PERFORM 921000-BUSCAR-EST-INCL
                   IF  SGTS0061-TS-EST-MOD-ULT
                                       NOT =  SGTS0061-TS-EST-MOD-INCL
                       PERFORM 922000-BUSCAR-EST-CNFC
                   END-IF
               WHEN  2
                   PERFORM 920000-TRATAR-EST-CNFC
               WHEN  3
                   MOVE SGTST050-TX-OBS-EST-MOD
                                       TO SGTS0061-TX-OBS-EST-CNFC
                   PERFORM 920000-TRATAR-EST-CNFC
               WHEN  99
      * Salvar usuário e timestamp do cancelamento
                   MOVE SGTST050-TS-EST-MOD-ORD
                                       TO SGTS0061-TS-EST-MOD-ECR
                   MOVE SGTST050-CD-USU-ATL-EST
                                       TO SGTS0061-CD-USU-ATL-ECR
                                          CD-USU-ECR
                   PERFORM 921000-BUSCAR-EST-INCL
                   PERFORM 922000-BUSCAR-EST-CNFC
                   PERFORM 923000-BUSCAR-ULT-ATU
               WHEN  OTHER
                   SET  ERRO-PROGRAMADO TO TRUE
                   MOVE 1004           TO MSG1-CD-ERRO-TS
                   MOVE 'Status do modelo de ordem inesperado.'
                                       TO MSG1-TX-ERRO-TS
                   PERFORM 999000-ERRO
           END-EVALUATE
           .
       240000-SAI.
           EXIT.
      
      *----------------------------------------------------------------*
       250000-TRATAR-USUARIO           SECTION.
      *----------------------------------------------------------------*
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO
      
      * Buscar nome do usuario responsavel pela inclusão
           MOVE SPACES                 TO ATCS6000-DADOS
           INITIALIZE  ATCS6000-DADOS
               REPLACING NUMERIC       BY  ZEROS
      
      * Movimentar o nome do programa a ser executado
           MOVE ATCS6000               TO NM-PGM
      
      * Pesquisar informaçoes do usuario da inclusão
           SET  K6000-PSQ-INF-USU      TO TRUE
           MOVE CD-USU-INCL            TO K6000-CD-USU
      * Formatar código de erro para cada acesso ao ATCS6000
      * que sera utilizado caso haja retorno de erro do modulo.
      *    MOVE 0006                   TO MSG1-CD-ERRO-TS
      
           PERFORM 910000-CHAMAR-SERVICO

           PERFORM 973100-TRATAR-RPST-ATC

           MOVE K6000-NM-USU           TO SGTS0061-NM-USU-ATL-INCL
      
      * Buscar nome do usuario responsavel pela ultima atualização
      * se houver necessidade
           IF  CD-USU-ULT              NOT = CD-USU-INCL
               MOVE SPACES             TO ATCS6000-DADOS
               INITIALIZE  ATCS6000-DADOS
                   REPLACING NUMERIC   BY  ZEROS
      
      * Pesquisar informaçoes do usuario da Uultima atualização
               SET  K6000-PSQ-INF-USU  TO TRUE
               MOVE CD-USU-ULT         TO K6000-CD-USU
      * Formatar código de erro para cada acesso ao ATCS6000
      * que sera utilizado caso haja retorno de erro do modulo.
      *        MOVE 0011               TO MSG1-CD-ERRO-TS
      
               PERFORM 910000-CHAMAR-SERVICO

               PERFORM 973100-TRATAR-RPST-ATC

               MOVE K6000-NM-USU       TO SGTS0061-NM-USU-ATL-ULT
           ELSE
               MOVE K6000-NM-USU       TO SGTS0061-NM-USU-ATL-ULT
           END-IF
      
      * Buscar nome do usuario responsavel pela confirmação, se houver
           IF  CD-USU-CNFC             NOT = SPACES
               MOVE SPACES             TO ATCS6000-DADOS
               INITIALIZE  ATCS6000-DADOS
                   REPLACING NUMERIC   BY  ZEROS
      
      * Pesquisar informaçoes do usuario da inclusão
               SET  K6000-PSQ-INF-USU  TO TRUE
               MOVE CD-USU-CNFC        TO K6000-CD-USU
      * Formatar código de erro para cada acesso ao ATCS6000
      * que sera utilizado caso haja retorno de erro do modulo.
      *        MOVE 0007               TO MSG1-CD-ERRO-TS
      
               PERFORM 910000-CHAMAR-SERVICO

               PERFORM 973100-TRATAR-RPST-ATC

               MOVE K6000-NM-USU       TO SGTS0061-NM-USU-ATL-CNFC
           END-IF
      
      * Buscar nome do usuario responsavel pelo cancelamento, se houver
           IF  CD-USU-ECR              NOT = SPACES
               MOVE SPACES             TO ATCS6000-DADOS
               INITIALIZE  ATCS6000-DADOS
                   REPLACING NUMERIC   BY  ZEROS
      
      * Pesquisar informaçoes do usuario da inclusão
               SET  K6000-PSQ-INF-USU  TO TRUE
               MOVE CD-USU-ECR         TO K6000-CD-USU
      * Formatar código de erro para cada acesso ao ATCS6000
      * que sera utilizado caso haja retorno de erro do modulo.
      *        MOVE 0008               TO MSG1-CD-ERRO-TS
      
               PERFORM 910000-CHAMAR-SERVICO

               PERFORM 973100-TRATAR-RPST-ATC

               MOVE K6000-NM-USU       TO SGTS0061-NM-USU-ATL-ECR
           END-IF
           .
       240000-SAI.
           EXIT.
      
      *----------------------------------------------------------------*
       899000-MOVIMENTAR-SAIDA         SECTION.
      *----------------------------------------------------------------*
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO
      
      * Grava o container de resposta da subrotina
           PERFORM 980000-GRAVAR-CONTAINER-RPST
           .
       899000-SAI.
           EXIT.
      
      *----------------------------------------------------------------*
      *                                                                *
      *  SECTION DA FAIXA DE 900000 A 969900 PARA DEMAIS ROTINAS       *
      *                                                                *
      *----------------------------------------------------------------*
      *---------------------------------------------------------------*
       910000-CHAMAR-SERVICO           SECTION.
      *---------------------------------------------------------------*
      * Acionar programa IIBP2000 para requisição do serviço.
      *---------------------------------------------------------------*
      
      * Grava o container com dados do serviço/operação a chamar
           PERFORM 981000-GRAVAR-CONTAINER-SRVC
      
      * Grava o container de requisicao do modulo a executar
           PERFORM 982000-GRAVAR-CONTAINER-RQSC
      
      * Limpa a resposta e erro antes de executar o modulo
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO
      
      * Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS LINK
                PROGRAM ( IIBP2000   )
                CHANNEL ( NM-CNL     )
                RESP    ( MSG1-RESP1 )
                RESP2   ( MSG1-RESP2 )
           END-EXEC
      
           EVALUATE  TRUE
               WHEN  MSG1-RESP1        EQUAL DFHRESP(NORMAL)
                   CONTINUE
               WHEN  OTHER
                   SET   ERRO-PROGRAMA-ACIONADO    TO  TRUE
                   MOVE  9000         TO  MSG1-CD-ERRO-TS
                   STRING 'Erro ao acionar serviço: '
                       SRVC-ID-SRVC '.' SRVC-VRS-SRVC '.'
                       SRVC-ID-OPR  '.' SRVC-VRS-OPR
                       'via IIBP2000, '
                       'RESP=' MSG1-RESP1 ', RESP-2=' MSG1-RESP2
                       '.'   DELIMITED BY  SIZE
                                     INTO MSG1-TX-ERRO-TS
                   SET  ERRO-CICS      TO TRUE
                   PERFORM 999000-ERRO
           END-EVALUATE
      
      * Efetua a leitura do Container de Erro
           PERFORM 972000-LER-CONTAINER-DE-ERRO
      
      * Efetua a leitura do Container de Resposta
           PERFORM 973000-LER-RESPOSTA
           .
       910000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       920000-TRATAR-EST-CNFC          SECTION.
      *----------------------------------------------------------------*
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO
      
      * Salvar usuário e timestamp da confirmação
           MOVE SGTST050-TS-EST-MOD-ORD
                                       TO SGTS0061-TS-EST-MOD-CNFC
           MOVE SGTST050-CD-USU-ATL-EST
                                       TO SGTS0061-CD-USU-ATL-CNFC
                                          CD-USU-CNFC
      
      * Buscar Estado de inclusão
           PERFORM 921000-BUSCAR-EST-INCL
      
      * Buscar Ultima Atualização
           PERFORM 923000-BUSCAR-ULT-ATU
           .
       920000-SAI.
           EXIT.
      
      *----------------------------------------------------------------*
       921000-BUSCAR-EST-INCL          SECTION.
      *----------------------------------------------------------------*
           MOVE 0011                   TO MSG1-CD-SEQL-ERRO
      
           MOVE SPACES                 TO SGTST050-DADOS
           INITIALIZE  SGTST050-DADOS
               REPLACING NUMERIC       BY  ZEROS
      
      * Opção 7 - Consultar ultimo Estado (Estado = 1 - inclusão)
           MOVE 7                      TO OPCAO
           MOVE 1                      TO SGTST050-CD-TIP-EST-DCS
      * Teste de sqlcode = 'S'
           SET  IN-SQL-S               TO TRUE
      
           PERFORM 930000-CHAMAR-SGTST050
      
           MOVE SGTST050-TS-EST-MOD-ORD
                                       TO SGTS0061-TS-EST-MOD-INCL
           MOVE SGTST050-CD-USU-ATL-EST
                                       TO SGTS0061-CD-USU-ATL-INCL
                                          CD-USU-INCL
           .
       921000-SAI.
           EXIT.
      
      *----------------------------------------------------------------*
       922000-BUSCAR-EST-CNFC          SECTION.
      *----------------------------------------------------------------*
           MOVE 0012                   TO MSG1-CD-SEQL-ERRO
      
           MOVE SPACES                 TO SGTST050-DADOS
           INITIALIZE  SGTST050-DADOS
               REPLACING NUMERIC       BY  ZEROS
      
      * Opção 6 - Consultar Ultima Aprovação (Estado = 2 - aprovado)
           MOVE 6                      TO OPCAO
           MOVE 2                      TO SGTST050-CD-TIP-EST-DCS
      * Teste de sqlcode = 'N'
           SET  IN-SQL-N               TO TRUE
      
           PERFORM 930000-CHAMAR-SGTST050
      
           MOVE SGTST050-TS-EST-MOD-ORD
                                       TO SGTS0061-TS-EST-MOD-CNFC
           MOVE SGTST050-CD-USU-ATL-EST
                                       TO SGTS0061-CD-USU-ATL-CNFC
                                          CD-USU-CNFC
           .
       922000-SAI.
           EXIT.
      
      *----------------------------------------------------------------*
       923000-BUSCAR-ULT-ATU           SECTION.
      *----------------------------------------------------------------*
           MOVE 0013                   TO MSG1-CD-SEQL-ERRO
      
           MOVE SPACES                 TO SGTST050-DADOS
           INITIALIZE  SGTST050-DADOS
               REPLACING NUMERIC       BY  ZEROS
      
      * Opção 6 - Consultar Ultima Atualização (Estado = 1 - inclusão)
           MOVE 6                      TO OPCAO
           MOVE 1                      TO SGTST050-CD-TIP-EST-DCS
      * Teste de sqlcode = 'S'
           SET  IN-SQL-S               TO TRUE
      
           PERFORM 930000-CHAMAR-SGTST050
      
           MOVE SGTST050-TS-EST-MOD-ORD
                                       TO SGTS0061-TS-EST-MOD-ULT
           MOVE SGTST050-CD-USU-ATL-EST
                                       TO SGTS0061-CD-USU-ATL-ULT
                                          CD-USU-ULT
           .
       923000-SAI.
           EXIT.
      
      *----------------------------------------------------------------*
       930000-CHAMAR-SGTST050          SECTION.
      *----------------------------------------------------------------*
           MOVE 0014                   TO MSG1-CD-SEQL-ERRO
      
           MOVE OPCAO                  TO SGTST050-ACAO
           MOVE SGTS0061-CD-MOD-ORD-NEGO
                                       TO SGTST050-CD-MOD-ORD-NEGO
      
      * Mover nome do módulo para monitoração de variáveis
           MOVE SGTST050               TO NM-PGM
      
           EXEC CICS LINK
                PROGRAM  ( SGTST050       )
                COMMAREA ( SGTST050-DADOS )
           END-EXEC
      
      * Se indicador de sqlcode = N, continuar o processamento quando
      * sqlcode = 100.
           IF  SGTST050-CD-RTN-PGM     NOT = ZEROS
               IF  SGTST050-CD-RTN-PGM = 100  AND
                   IN-SQL-N
                   CONTINUE
               ELSE
                   SET  ERRO-PROGRAMA-ACIONADO TO TRUE
                   MOVE SGTST050-MSG-ERRO-PGM
                                       TO MSG1-TX-RTN-PGM-ACND
                                          MSG1-TX-ERRO-TS
                   MOVE NM-PGM         TO MSG1-NM-PGM-ACND
                   MOVE SGTST050-CD-RTN-PGM
                                       TO MSG1-CD-RTN-PGM-ACND
                                          MSG1-CD-ERRO-TS
                   PERFORM 969000-MONITORAR-VARIAVEIS
                   PERFORM 999000-ERRO
               END-IF
           END-IF
           .
       930000-SAI.
           EXIT.
      
      *----------------------------------------------------------------*
       969000-MONITORAR-VARIAVEIS      SECTION.
      *----------------------------------------------------------------*
      * Monitora as variáveis dos módulos chamados(Mensageria IIB)
      *----------------------------------------------------------------*
      
           EVALUATE  NM-PGM
               WHEN  SGTST045
                   MOVE 'SGTST045-ACAO '
                                       TO MSGKW999-NM-VRV-MNT
                   MOVE SGTST045-ACAO
                                       TO MSGKW999-VL-VRV-MNT
                   PERFORM 999003-ADICIONA-VRV-MNT
      
                   MOVE 'SGTST045-CD-TIP-NEGO'
                                       TO MSGKW999-NM-VRV-MNT
                   MOVE SGTST045-CD-TIP-NEGO
                                       TO MSGKW999-VL-VRV-MNT
                   PERFORM 999003-ADICIONA-VRV-MNT
      
               WHEN  SGTST049
                   MOVE 'SGTST049-ACAO '
                                       TO MSGKW999-NM-VRV-MNT
                   MOVE SGTST049-ACAO
                                       TO MSGKW999-VL-VRV-MNT
                   PERFORM 999003-ADICIONA-VRV-MNT
      
                   MOVE 'SGTST049-CD-MOD-ORD-NEGO'
                                       TO MSGKW999-NM-VRV-MNT
                   MOVE SGTST049-CD-MOD-ORD-NEGO
                                       TO MSGKW999-VL-VRV-MNT
                   PERFORM 999003-ADICIONA-VRV-MNT
      
               WHEN  PRDS0203
                   MOVE 'S0203-CD-FUNCAO'
                                       TO MSGKW999-NM-VRV-MNT
                   MOVE S0203-CD-FUNCAO
                                       TO MSGKW999-VL-VRV-MNT
                   PERFORM 999003-ADICIONA-VRV-MNT
      
                   MOVE 'S0203-CD-PRD'
                                       TO MSGKW999-NM-VRV-MNT
                   MOVE S0203-CD-PRD
                                       TO MSGKW999-VL-VRV-MNT
                   PERFORM 999003-ADICIONA-VRV-MNT
      
                   MOVE 'S0203-CD-MDLD'
                                       TO MSGKW999-NM-VRV-MNT
                   MOVE S0203-CD-MDLD
                                       TO MSGKW999-VL-VRV-MNT
                   PERFORM 999003-ADICIONA-VRV-MNT
      
               WHEN  SGTST050
                   MOVE 'SGTST050-ACAO '
                                       TO MSGKW999-NM-VRV-MNT
                   MOVE SGTST050-ACAO
                                       TO MSGKW999-VL-VRV-MNT
                   PERFORM 999003-ADICIONA-VRV-MNT
      
                   MOVE 'SGTST050-CD-MOD-ORD-NEGO'
                                       TO MSGKW999-NM-VRV-MNT
                   MOVE SGTST050-CD-MOD-ORD-NEGO
                                       TO MSGKW999-VL-VRV-MNT
                   PERFORM 999003-ADICIONA-VRV-MNT
      
                   MOVE 'SGTST050-CD-TIP-EST-DCS  '
                                       TO MSGKW999-NM-VRV-MNT
                   MOVE SGTST050-CD-TIP-EST-DCS
                                       TO MSGKW999-VL-VRV-MNT
                   PERFORM 999003-ADICIONA-VRV-MNT
      
               WHEN  ATCS6000
                   MOVE 'K6000-CD-FUC '
                                       TO MSGKW999-NM-VRV-MNT
                   MOVE K6000-CD-FUC
                                       TO MSGKW999-VL-VRV-MNT
                   PERFORM 999003-ADICIONA-VRV-MNT
      
                   MOVE 'K6000-CD-USU'
                                       TO MSGKW999-NM-VRV-MNT
                   MOVE K6000-CD-USU
                                       TO MSGKW999-VL-VRV-MNT
                   PERFORM 999003-ADICIONA-VRV-MNT
      
               WHEN  OTHER
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
      *----------------------------------------------------------------*
      * Obtem o canal corrente
      *----------------------------------------------------------------*
           MOVE 0015                   TO MSG1-CD-SEQL-ERRO
      
      * Por causa da captura automática de erro CICS, deve ser previsto
      * o erro de container
           SET  ERRO-CANAL             TO TRUE
      
           EXEC CICS ASSIGN
                CHANNEL ( NM-CNL )
           END-EXEC
      
      * Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       970000-SAI.
           EXIT.
      
      *----------------------------------------------------------------*
       971000-LER-REQUISICAO           SECTION.
      *----------------------------------------------------------------*
           MOVE 0016                   TO MSG1-CD-SEQL-ERRO
      
      * Formata o nome do container de requisicao
           MOVE 'SGTS0061'             TO NM-PGM
           MOVE 'SGTS0061-RQSC'        TO NM-CTNR
      
      * Recupera o tamanho do container de requisição
           PERFORM 975000-LER-CONTAINER-NODATA
      
      * Obtem o tamanho esperado da requisicao
           MOVE LENGTH OF SGTS0061-RQSC TO QT-CCT-EPRO
      
      * Valida tamanho esperado da requisicao X tamanho recebido
           IF  QT-CCT-LIDO             NOT = QT-CCT-EPRO
               SET  ERRO-TAMANHO-AREA  TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
      
           MOVE 0017                   TO MSG1-CD-SEQL-ERRO
      
      * Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE
      
      * Por causa da captura automática de erro CICS, deve ser previsto
      * o erro de container
           SET  ERRO-CONTAINER         TO TRUE
      
      * Efetua a leitura do container
           EXEC CICS GET
                CONTAINER ( NM-CTNR       )
                FLENGTH   ( QT-CCT-LIDO   )
                INTO      ( SGTS0061-RQSC )
           END-EXEC
      
      * Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       971000-SAI.
           EXIT.
      
      *----------------------------------------------------------------*
       972000-LER-CONTAINER-DE-ERRO    SECTION.
      *----------------------------------------------------------------*
      * A presença do container PRM-ERRO indica que houve erro de
      * integração, a ausência do container indica sucesso.
      *----------------------------------------------------------------*
      
      * Le o container de erro dos modulos chamados
           SET  GET-CONTAINER          TO TRUE
           MOVE 'PRM-ERRO'             TO NM-CTNR
           MOVE SPACES                 TO IIBKERRO-PRM-ERRO
           INITIALIZE IIBKERRO-PRM-ERRO
               REPLACING NUMERIC       BY  ZEROS
      
      * Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS GET
                CONTAINER ( NM-CTNR                     )
                INTO      ( IIBKERRO-PRM-ERRO           )
                FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                RESP      ( MSG1-RESP1                  )
                RESP2     ( MSG1-RESP2                  )
           END-EXEC
      
           EVALUATE  TRUE
      * Caso deseje personalizar ou não interromper o
      * processamento, alterar para não dar erro aqui SOMENTE
      * PARA O MODULO CHAMADO e tratar na section chamadora
               WHEN  MSG1-RESP1    EQUAL DFHRESP(CONTAINERERR)
                   CONTINUE
               WHEN  MSG1-RESP1    EQUAL DFHRESP(NORMAL)
               IF  NM-PGM          EQUAL ATCS6000  OR
                   NM-PGM          EQUAL PRDS0203
                   CONTINUE
               ELSE
      *--> Trata o erro de programa acionado por ser módulo interno
                SET ERRO-PROGRAMA-ACIONADO   TO  TRUE
                MOVE NM-PGM                  TO  MSG1-NM-PGM-ACND
                IF NM-PGM(1:3) EQUAL 'SGT'
                    MOVE IIBKERRO-CD-ERRO    TO  MSG1-CD-RTN-PGM-ACND
                                                 MSG1-CD-ERRO-TS
                    MOVE IIBKERRO-TX-ERRO    TO  MSG1-TX-RTN-PGM-ACND
                                                 MSG1-TX-ERRO-TS
                ELSE
                    MOVE  3004               TO  MSG1-CD-ERRO-TS
                    STRING  'Erro ao acionar o módulo ' NM-PGM      '.'
                            DELIMITED BY SIZE
                            INTO        MSG1-TX-ERRO-TS
                    END-STRING
                    MOVE IIBKERRO-CD-ERRO     TO  MSG1-CD-RTN-PGM-ACND
                    MOVE IIBKERRO-TX-ERRO     TO  MSG1-TX-RTN-PGM-ACND
                END-IF
      *-->      Monitorar variáveis do módulo chamado
                PERFORM 969000-MONITORAR-VARIAVEIS
                PERFORM 999000-ERRO
               END-IF
               WHEN  OTHER
                   SET  ERRO-CONTAINER TO TRUE
                   PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       972000-SAI.
           EXIT.
      
      *----------------------------------------------------------------*
       973000-LER-RESPOSTA             SECTION.
      *----------------------------------------------------------------*
      * Recupera o container com os dados de resposta (saída).
      *----------------------------------------------------------------*
      
      * Monta o nome dos container de acordo com modulo a executar
           MOVE SPACES                 TO NM-CTNR
      
      * Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      * nome_modulo -RPST
           EVALUATE  NM-PGM
               WHEN  'ATCS6000'
               WHEN  'PRDS0203'
                   MOVE 'PRM-DADOS-RPST'
                                       TO NM-CTNR
               WHEN  OTHER
                   MOVE NM-PGM         TO NM-CTNR(1:8)
                   MOVE '-RPST'        TO NM-CTNR(9:5)
           END-EVALUATE
      
      * Recupera o tamanho do container de resposta
           PERFORM 975000-LER-CONTAINER-NODATA
      
      * Configura dados para leitura do container de resposta
           SET  GET-CONTAINER          TO TRUE
      
      * Por causa da captura automática de erro CICS, deve ser previsto
      * o erro de container
           SET  ERRO-CONTAINER         TO TRUE
      
      * Le a RESPOSTA de acordo com o programa que a gravou
      
      * Calcular a quantidade de caracteres esperado e limpar a area de
      * dados antes de executar o GET
      * Para matrizes com dados variaveis (clausula depending on) não
      * ha como utilizar o comando INITIALIZE
      
           EVALUATE  NM-PGM
               WHEN  ATCS6000
                   MOVE SPACES         TO ATCS6000-DADOS
                   INITIALIZE ATCS6000-DADOS
                       REPLACING NUMERIC   BY ZEROS
                   MOVE LENGTH OF ATCS6000-DADOS   TO QT-CCT-EPRO
                   EXEC CICS GET
                        CONTAINER ( NM-CTNR        )
                        INTO      ( ATCS6000-DADOS )
                        FLENGTH   ( QT-CCT-LIDO    )
                   END-EXEC
      *            PERFORM 973100-TRATAR-RPST-ATC
               WHEN  PRDS0203
                   MOVE SPACES         TO PRDS0203-DADOS
                   INITIALIZE PRDS0203-DADOS
                       REPLACING NUMERIC   BY ZEROS
                   MOVE LENGTH OF PRDS0203-DADOS   TO QT-CCT-EPRO
                   EXEC CICS GET
                        CONTAINER ( NM-CTNR        )
                        INTO      ( PRDS0203-DADOS )
                        FLENGTH   ( QT-CCT-LIDO    )
                   END-EXEC
      *            PERFORM 973200-TRATAR-RPST-PRD
               WHEN  OTHER
                   SET   ERRO-PROGRAMADO
                                       TO  TRUE
                   MOVE  9003         TO  MSG1-CD-ERRO-TS
                   STRING  'Erro na leitura da resposta do módulo '
                             NM-PGM '.'
                             DELIMITED BY SIZE
                             INTO          MSG1-TX-ERRO-TS
                   END-STRING
                   PERFORM 999000-ERRO
           END-EVALUATE
      
      * Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       973000-SAI.
           EXIT.
      
      *----------------------------------------------------------------*
       973100-TRATAR-RPST-ATC          SECTION.
      *----------------------------------------------------------------*
      * Em caso de erro do módulo não interromper o processamento.
      *----------------------------------------------------------------*
      
           EVALUATE  K6000-CD-RTN
               WHEN  0
                   CONTINUE
               WHEN  1
                   MOVE SPACES         TO K6000-NM-USU
           END-EVALUATE
           .
       973100-SAI.
           EXIT.
      
      *----------------------------------------------------------------*
       973200-TRATAR-RPST-PRD          SECTION.
      *----------------------------------------------------------------*
      
           IF  S0203-CD-RTN            NOT = ZEROS
               MOVE 3004               TO MSG1-CD-ERRO-TS
                    STRING  'Erro ao acionar o módulo ' NM-PGM '.'
                           DELIMITED BY SIZE
                           INTO           MSG1-TX-ERRO-TS
               MOVE S0203-TX-RTN       TO MSG1-TX-RTN-PGM-ACND
                                          MSG1-TX-ERRO-TS
               MOVE NM-PGM             TO MSG1-NM-PGM-ACND
               MOVE S0203-CD-RTN       TO MSG1-CD-RTN-PGM-ACND
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-ERRO
           END-IF
           .
       973200-SAI.
           EXIT.
      
      *----------------------------------------------------------------*
       975000-LER-CONTAINER-NODATA     SECTION.
      *----------------------------------------------------------------*
      
      * Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE
      
      * Por causa da captura automática de erro CICS, deve ser previsto
      * o erro de container
           SET  ERRO-CONTAINER         TO TRUE
      
      * Obtem a quantidade de caracteres no container
           MOVE ZEROS                  TO QT-CCT-LIDO
           EXEC CICS GET
                CONTAINER ( NM-CTNR     )
                NODATA
                FLENGTH   ( QT-CCT-LIDO )
           END-EXEC
      
      * Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       975000-SAI.
           EXIT.
      
      *----------------------------------------------------------------*
      *                                                                *
      * SECTION DA FAIXA DE 980000 A 989900 PARA GRAVACAO DE DADOS     *
      *                                                                *
      *----------------------------------------------------------------*
       980000-GRAVAR-CONTAINER-RPST    SECTION.
      *----------------------------------------------------------------*
      
      * Exclui eventual container de erro de outros programas
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO
      
           MOVE 0018                   TO MSG1-CD-SEQL-ERRO
      
      * Monta o nome do container de resposta
           MOVE 'SGTS0061-RPST'        TO NM-CTNR
           SET  PUT-CONTAINER          TO TRUE
      
      * Por causa da captura automática de erro CICS, deve ser previsto
      * o erro de container
           SET  ERRO-CONTAINER         TO TRUE
      
           EXEC CICS PUT
                CONTAINER ( NM-CTNR                 )
                FROM      ( SGTS0061-RPST           )
                FLENGTH   ( LENGTH OF SGTS0061-RPST )
           END-EXEC
      
      * Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       980000-SAI.
           EXIT.
      
      *---------------------------------------------------------------*
       981000-GRAVAR-CONTAINER-SRVC    SECTION.
      *---------------------------------------------------------------*
      * Postar container de requisição de serviço.
      *---------------------------------------------------------------*
           MOVE 0019                   TO MSG1-CD-SEQL-ERRO
      
      * Monta o nome do container de requisicao do servico
           MOVE 'PRM-RQSC-SRVC'        TO NM-CTNR
           SET  PUT-CONTAINER          TO TRUE
      
           EVALUATE  NM-PGM
               WHEN  'ATCS6000'
                   MOVE 71739          TO SRVC-ID-SRVC
                   MOVE 1              TO SRVC-VRS-SRVC
                   MOVE 292018         TO SRVC-ID-OPR
                   MOVE 1              TO SRVC-VRS-OPR
               WHEN  'PRDS0203'
                   MOVE 214106         TO SRVC-ID-SRVC
                   MOVE 1              TO SRVC-VRS-SRVC
                   MOVE 543363         TO SRVC-ID-OPR
                   MOVE 1              TO SRVC-VRS-OPR
               WHEN  OTHER
                  SET   ERRO-PROGRAMADO  TO  TRUE
                    MOVE  9004             TO  MSG1-CD-ERRO-TS
                    STRING  'Erro na gravação da requisição do módulo '
                             NM-PGM '.'
                             DELIMITED BY SIZE
                             INTO           MSG1-TX-ERRO-TS
                   PERFORM 999000-ERRO
           END-EVALUATE
      
      * Por causa da captura automática de erro CICS, deve ser previsto
      * o erro de container
           SET  ERRO-CONTAINER         TO TRUE
      
           EXEC CICS PUT
                CONTAINER ( NM-CTNR                      )
                FROM      ( SRVC-PRM-RQSC-SRVC           )
                FLENGTH   ( LENGTH OF SRVC-PRM-RQSC-SRVC )
           END-EXEC
      
      * Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       981000-SAI.
           EXIT.
      
      *----------------------------------------------------------------*
       982000-GRAVAR-CONTAINER-RQSC    SECTION.
      *----------------------------------------------------------------*
      * Postar container com dados de requisição (entrada)
      *----------------------------------------------------------------*
           MOVE 0020                   TO MSG1-CD-SEQL-ERRO
      
      * Monta o nome dos container de acordo com modulo a executar
           MOVE SPACES                 TO NM-CTNR
           SET  PUT-CONTAINER          TO TRUE
      
      * Quando módulos via IIB, fixo nome PRM-DADOS-RQSC, senão
      * nome_modulo-RQSC
           EVALUATE  NM-PGM
      * ATCS6000 - acesso via IIB
               WHEN  'ATCS6000'
               WHEN  'PRDS0203'
                   MOVE 'PRM-DADOS-RQSC'
                                       TO NM-CTNR
               WHEN  OTHER
                   MOVE NM-PGM        TO NM-CTNR(1:8)
                   MOVE '-RQSC'       TO NM-CTNR(9:5)
           END-EVALUATE
      
      * Por causa da captura automática de erro CICS, deve ser previsto
      * o erro de container
           SET  ERRO-CONTAINER         TO TRUE
      
      * Faz put da requisicao do programa a executar
           EVALUATE  NM-PGM
               WHEN  ATCS6000
                   EXEC CICS PUT
                        CONTAINER ( NM-CTNR                  )
                        FROM      ( ATCS6000-DADOS           )
                        FLENGTH   ( LENGTH OF ATCS6000-DADOS )
                   END-EXEC
               WHEN  PRDS0203
                   EXEC CICS PUT
                        CONTAINER ( NM-CTNR                  )
                        FROM      ( PRDS0203-DADOS           )
                        FLENGTH   ( LENGTH OF PRDS0203-DADOS )
                   END-EXEC
               WHEN  OTHER
                   SET   ERRO-PROGRAMADO
                                       TO  TRUE
                    MOVE  9004         TO  MSG1-CD-ERRO-TS
                    STRING  'Erro na gravação da requisição do módulo.'
                             NM-PGM '.'
                             DELIMITED BY SIZE
                             INTO          MSG1-TX-ERRO-TS
                    END-STRING
                   PERFORM 999000-ERRO
           END-EVALUATE
      
      * Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       982000-SAI.
           EXIT.
      
      *---------------------------------------------------------------*
       983000-EXCLUIR-RESPOSTA-ERRO    SECTION.
      *----------------------------------------------------------------*
      * Exclui o container de resposta e de erro para evitar problemas
      * em modulos executados mais de uma vez dentro da mesma sessao do
      * CICS. Equivale ao initialize na area de resposta dos programas
      * que utilizam COMMAREA.
      *----------------------------------------------------------------*
           MOVE 0021                   TO MSG1-CD-SEQL-ERRO
      
      * Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      * (nome_modulo)-RPST
           EVALUATE  NM-PGM
               WHEN  'SGTS0061'
                   MOVE NM-PGM         TO NM-CTNR(1:8)
                   MOVE '-RPST'        TO NM-CTNR(9:5)
               WHEN  OTHER
                   MOVE 'PRM-DADOS-RPST'
                                       TO NM-CTNR
           END-EVALUATE
      
           SET  DEL-CONTAINER          TO TRUE
      
      * Exclui o container de resposta
      * Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS DELETE
                CONTAINER ( NM-CTNR    )
                RESP      ( MSG1-RESP1 )
                RESP2     ( MSG1-RESP2 )
           END-EXEC
      
           EVALUATE  TRUE
               WHEN  MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
               WHEN  MSG1-RESP1 EQUAL DFHRESP(NORMAL)
                   CONTINUE
               WHEN  OTHER
                   SET  ERRO-CONTAINER TO TRUE
                   PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE
      
      * Exclui o container de erro
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO
           .
       983000-SAI.
           EXIT.
      *
      *----------------------------------------------------------------*
       983100-EXCLUIR-CONTAINER-ERRO   SECTION.
      *----------------------------------------------------------------*
      
      * Limpar variaveis container de erro
           MOVE SPACES                 TO IIBKERRO-PRM-ERRO
           INITIALIZE IIBKERRO-PRM-ERRO
               REPLACING NUMERIC       BY  ZEROS
      
      * Exclui o container de erro
           MOVE 'PRM-ERRO'             TO NM-CTNR
           SET  DEL-CONTAINER          TO TRUE
      
      * Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS DELETE
                CONTAINER ( NM-CTNR    )
                RESP      ( MSG1-RESP1 )
                RESP2     ( MSG1-RESP2 )
           END-EXEC
      
           EVALUATE  TRUE
               WHEN  MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
               WHEN  MSG1-RESP1 EQUAL DFHRESP(NORMAL)
                   CONTINUE
               WHEN  OTHER
                   SET  ERRO-CONTAINER TO TRUE
                   PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       983100-SAI.
           EXIT.
      
      *----------------------------------------------------------------*
       984000-GRAVAR-CONTAINER-ERRO    SECTION.
      *----------------------------------------------------------------*
      
      * Grava o erro gerado no container de erro
      * Não há captura automática de erro CICS por usar RESP e RESP2
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
      
      *----------------------------------------------------------------*
       990000-ENCERRAR                 SECTION.
      *----------------------------------------------------------------*
      
           EXEC CICS
                RETURN
           END-EXEC
           .
       990000-ENCERRAR-SAI.
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
      
           IF  MSG1-CD-ERRO-TS         EQUAL ZEROS
               MOVE 8888                TO MSG1-CD-ERRO-TS
           END-IF
           IF  MSG1-TX-ERRO-TS         EQUAL SPACES
               MOVE 'Erro não especificado no módulo,'
                                       TO MSG1-TX-ERRO-TS
           END-IF
           PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           .
       999000-ERRO-SAI.
           EXIT.
      
      *----------------------------------------------------------------*
      *    As sections abaixo representam os tratamentos de erro
      *    utilizando o sistema MSG de mensageria (rotina MSGS0001)
      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------
      
      * Em caso de erro, desfaz as atualizaçoes em banco de dados
           EXEC CICS SYNCPOINT
                ROLLBACK
           END-EXEC
      
      * Verifica se o tipo de erro gerado possui tratamento interno
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
      
      * Adiciona as variáveis de entrada para monitoração
               MOVE 'SGTS0061-CD-MOD-ORD-NEGO'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE SGTS0061-CD-MOD-ORD-NEGO
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT
      
      
      * Grava o erro gerado no sistema MSG - Mensageria
           PERFORM MSG1-NTF-MSG
      
      * Movimenta Mensagem e Código de Erro para o Container de Erro
           MOVE MSG1-CD-ERRO-TS        TO IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS        TO IIBKERRO-TX-ERRO
      
      * Grava o erro gerado no container de erro
           PERFORM 984000-GRAVAR-CONTAINER-ERRO
      
      * Encerra a transacao
           PERFORM 990000-ENCERRAR
           .
       999000-SAI.
           EXIT.
      * --------------------------------------*
      * 999001-TRATA-ABEND-CICS        SECTION.
      * 999002-TRATA-CONDITION-CICS    SECTION.
      * 999003-ADICIONA-VRV-MNT        SECTION.
      * --------------------------------------*
       COPY HLPKP999.                                                           0001
      * --------------------------------------*
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *---------------------------------------*
       COPY SGTKP999.                                                           0001
      * --------------------------------------*
      * MSG1-NTF-MSG                   SECTION.
      * --------------------------------------*
       COPY MSGKPMG1.                                                           0001
      *---------------------- FIM SGTS0061 -----------------------------
