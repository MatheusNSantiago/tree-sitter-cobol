      *-----------------------------------------------------------------
      * Programa..: SGTS0054
      * Analista..: F2054109 - CINTIA AP SOARES IGLESIAS
      * Autor.....: F8779682 - ROSA APARECIDA HENRIQUES FOZ
      * Data......: 12/11/2015
      * Objetivo..: Negócio ---> Listar Tipo de Negociação
      * Compilação: COBOL/CICS
      * Processamento......: on-line
      * Tipo de Comunicação: Container
      * Tratamento de Erros: Padrão Messageria IIB
      *-----------------------------------------------------------------
      * VRS002 04/04/2019 F9188847 DE 2016/145195 - Inclusão de campos:
      *        - - SGTS0054-E-CD-CTRA-TIT-VL e SGTS0054-CD-CTRA-TIT-VL
      *        - Acerto de mensageria
      * VRS001 12/11/2015 F8779682  de 2013/120821 - Implantação
      *-----------------------------------------------------------------
      
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID. SGTS0054.
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
                    '*** S.S. COMECA AQUI ***'.
      
       77  CTE-PROG                      PIC  X(016)  VALUE
                    '*** SGTS0054 ***'.
       77  CTE-VERS                      PIC  X(006)  VALUE 'VRS002'.
      
      * Módulos acionados
       77  SGTST048                      PIC  X(008) VALUE 'SGTST048'.
       77  SGTSL045                      PIC  X(008) VALUE 'SGTSL045'.
      *----------------------------------------------------------------*
       LOCAL-STORAGE                   SECTION.
      *----------------------------------------------------------------*
      
      * Areas de trabalho
      *
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
       01 SGTS0054-DADOS.
       COPY SGTK0054.                                                           0000
      
      * Persistencia/Crud  da Tabela EST_TIP_NEGO
       01  SGTST048-DADOS.
       COPY SGTKT048.                                                           0000
      
      * Lista da Tabela TIP_NEGO
       01  SGTSL045-DADOS.
       COPY SGTKL045.                                                           0000
      
      * Book do Container de Erro
       COPY IIBKERRO.                                                           0000
      
      * Book do container de Serviços (para acionamento de serviços)
       COPY IIBKSRVC.                                                           0000
      
      * Book dos dados do usuário
       COPY IIBKOPRD.                                                           0000
      
       77  CTE-FINAL-SS                  PIC X(40)      VALUE
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
      
      *    Ativa a captura automatica de abend do CICS.
      *    WHENEVER CICS
       COPY HLPKE999.
      
           PERFORM 970000-LER-CANAL
      
      *    Define o o nome do programa p/ fins manipulacao container
           MOVE 'SGTS0054'             TO NM-PGM
           MOVE 'SGTS0054-RPST'        TO NM-CTNR
      
      *    Limpa a resposta e erro antes de executar o modulo
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO
      
           PERFORM 971000-LER-REQUISICAO
      
      *    Caso não seja necessário utilização do Usuário Logado,
      *    a section 974000-LER-DADOS-OPRD poderá ser excluída
      *    PERFORM 974000-LER-DADOS-OPRD
      
           PERFORM 110000-VALIDAR-REQUISICAO
      
           .
       100000-SAI.
           EXIT.
      
      *----------------------------------------------------------------*
       110000-VALIDAR-REQUISICAO       SECTION.
      *----------------------------------------------------------------*
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO
      
      *    Validação tamanho esperado da requisicao X tamanho recebido
      *    na section de leitura de container de requisicao 971000
      
      *    Valida campo a campo da requisicao

           IF  SGTS0054-E-CD-CTRA-TIT-VL     < ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0001              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' inválido.'      TO  MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0054-CD-ULT-TIP-NEGO   <  0
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0714               TO MSG1-CD-ERRO-TS
               MOVE  'Código do tipo de negociação inválido.'
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
      
           MOVE ZEROS                  TO SGTS0054-QT-REG
      
           PERFORM 210000-TRATA-LISTA-TIP-NEGO

           PERFORM 980000-GRAVAR-CONTAINER-RSPT
      
           .
       200000-SAI.
           EXIT.
      
      *----------------------------------------------------------------*
       210000-TRATA-LISTA-TIP-NEGO     SECTION.
      *----------------------------------------------------------------*
           MOVE 0004                   TO MSG1-CD-SEQL-ERRO
      
           INITIALIZE                  SGTSL045-DADOS
      
      *    Movimentar os campos de entrada
           SET  SGTSL045-LISTA-TIP        TO TRUE

           MOVE SGTS0054-E-CD-CTRA-TIT-VL TO SGTSL045-E-CD-CTRA-TIT-VL
           MOVE SGTS0054-CD-ULT-TIP-NEGO  TO SGTSL045-CD-ULT-TIP-NEGO
      
           PERFORM 920000-CHAMAR-SGTSL045
      
      *    Movimentar os campos de saida
      
           PERFORM UNTIL SGTS0054-QT-REG
                                       EQUAL SGTSL045-QT-OCR-TAB
              OR SGTS0054-QT-REG       EQUAL 100
      
              ADD 1                    TO SGTS0054-QT-REG
              INITIALIZE SGTS0054-LS-TIP-NEGO (SGTS0054-QT-REG)
      
              MOVE  SGTSL045-CD-CTRA-TIT-VL     (SGTS0054-QT-REG)
                    TO  SGTS0054-CD-CTRA-TIT-VL (SGTS0054-QT-REG)

              MOVE  SGTSL045-CD-TIP-NEGO        (SGTS0054-QT-REG)
                    TO  SGTS0054-CD-TIP-NEGO    (SGTS0054-QT-REG)
      
              MOVE  SGTSL045-NM-TIP-NEGO        (SGTS0054-QT-REG)
                    TO  SGTS0054-NM-TIP-NEGO    (SGTS0054-QT-REG)
      
              PERFORM 900000-CONS-EST-ATU
           END-PERFORM
      
           .
       210000-SAI.
           EXIT.
      
      
      *----------------------------------------------------------------*
      *                                                                *
      *  SECTION DA FAIXA DE 900000 A 969900 PARA DEMAIS ROTINAS       *
      *                                                                *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       900000-CONS-EST-ATU           SECTION.
      *----------------------------------------------------------------*
      
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO
      
           INITIALIZE                  SGTST048-DADOS
      
      *    Movimentar os campos de entrada
           SET SGTST048-CONS-EST-ATU   TO TRUE
           MOVE  SGTSL045-CD-CTRA-TIT-VL  (SGTS0054-QT-REG)
                                       TO SGTST048-E-CD-CTRA-TIT-VL
           MOVE  SGTSL045-CD-TIP-NEGO     (SGTS0054-QT-REG)
                                       TO  SGTST048-CD-TIP-NEGO
      
           PERFORM 910000-CHAMAR-SGTST048
      
      *    Movimentar os campos de saida

           MOVE SGTST048-TS-EST-TIP-NEGO
                                       TO SGTS0054-TS-EST-TIP-NEGO
                                          (SGTS0054-QT-REG)
           MOVE SGTST048-CD-TIP-EST-DCS
                                       TO SGTS0054-CD-TIP-EST-DCS
                                          (SGTS0054-QT-REG)
           .
       900000-SAI.
           EXIT.
      
      *----------------------------------------------------------------*
       910000-CHAMAR-SGTST048          SECTION.
      *----------------------------------------------------------------*
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO
      
      *    Movimento nome do módulo para monitoração de variáveis
           MOVE SGTST048               TO NM-PGM
           EXEC CICS LINK
                PROGRAM  (SGTST048)
                COMMAREA (SGTST048-DADOS)
           END-EXEC
      
           IF  SGTST048-CD-RTN-PGM     NOT EQUAL ZEROS
               SET ERRO-PROGRAMA-ACIONADO TO TRUE
               MOVE SGTST048-MSG-ERRO-PGM
                                       TO MSG1-TX-RTN-PGM-ACND
                                          MSG1-TX-ERRO-TS
               MOVE NM-PGM             TO MSG1-NM-PGM-ACND
               MOVE SGTST048-CD-RTN-PGM
                                       TO MSG1-CD-RTN-PGM-ACND
                                          MSG1-CD-ERRO-TS
      
      *        Monitorar variáveis do módulo chamado
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-ERRO
           END-IF
      
           .
       910000-SAI.
           EXIT.
      
      *----------------------------------------------------------------*
       920000-CHAMAR-SGTSL045          SECTION.
      *----------------------------------------------------------------*
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO
      
      *    Movimento nome do módulo para monitoração de variáveis
           MOVE SGTSL045               TO NM-PGM
           EXEC CICS LINK
                PROGRAM  (SGTSL045)
                COMMAREA (SGTSL045-DADOS)
           END-EXEC
      
           IF  SGTSL045-CD-RTN-PGM     NOT EQUAL ZEROS
               SET ERRO-PROGRAMA-ACIONADO TO TRUE
               MOVE SGTSL045-MSG-ERRO-PGM
                                       TO MSG1-TX-RTN-PGM-ACND
                                          MSG1-TX-ERRO-TS
               MOVE NM-PGM             TO MSG1-NM-PGM-ACND
               MOVE SGTSL045-CD-RTN-PGM
                                       TO MSG1-CD-RTN-PGM-ACND
                                          MSG1-CD-ERRO-TS
      
      *        Monitorar variáveis do módulo chamado
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-ERRO
           END-IF
           .
       930000-SAI.
           EXIT.
      
      *----------------------------------------------------------------*
       969000-MONITORAR-VARIAVEIS      SECTION.
      *----------------------------------------------------------------*
      * Monitora as variáveis dos módulos chamados
      *----------------------------------------------------------------*
           EVALUATE NM-PGM
               WHEN SGTST048
                    MOVE 'SGTST048-ACAO'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTST048-ACAO
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST048-E-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST048-E-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST048-CD-TIP-NEGO'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST048-CD-TIP-NEGO
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT
      
                    MOVE 'SGTST048-TS-EST-TIP-NEGO'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST048-TS-EST-TIP-NEGO
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTSL045
                    MOVE 'SGTSL045-ACAO'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL045-ACAO
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL045-E-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL045-E-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL045-CD-ULT-TIP-NEGO'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL045-CD-ULT-TIP-NEGO
                                       TO MSGKW999-VL-VRV-MNT
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
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO
      
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
      
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO
      
      *    Monta o nome do container de requisicao
           MOVE 'SGTS0054'             TO NM-PGM
           MOVE 'SGTS0054-RQSC'        TO NM-CTNR
      
      *    Recupera o tamanho do container de requisição
           PERFORM 973100-LER-CONTAINER-NODATA
      
      *    Obtem o tamanho esperado da requisição
           MOVE LENGTH OF SGTS0054-RQSC TO QT-CCT-EPRO
      
      *    Valida tamanho esperado da requisicao X tamanho recebido
      
           IF  QT-CCT-LIDO             NOT EQUAL QT-CCT-EPRO
               SET ERRO-TAMANHO-AREA   TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
      
           MOVE 0011                   TO MSG1-CD-SEQL-ERRO
      
      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE
      
      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE
      
      *    Efetua a leitura do container
           EXEC CICS GET
                     CONTAINER (NM-CTNR)
                     FLENGTH   (QT-CCT-LIDO)
                     INTO      (SGTS0054-RQSC)
           END-EXEC
      
      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
      
           .
       971000-SAI.
           EXIT.
      
      *----------------------------------------------------------------*
       973100-LER-CONTAINER-NODATA     SECTION.
      *----------------------------------------------------------------*
      
      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE
      
      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE
      
      *    Obtem a quantidade de caracteres no container
           MOVE ZEROS                  TO QT-CCT-LIDO
           EXEC CICS GET
                     CONTAINER (NM-CTNR)
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
           MOVE 0012                   TO MSG1-CD-SEQL-ERRO
      
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
       980000-GRAVAR-CONTAINER-RSPT    SECTION.
      *----------------------------------------------------------------*
      
      *    Exclui eventual container de erro de outros programas
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO
      
           MOVE 0013                   TO MSG1-CD-SEQL-ERRO
      
      *    Monta o nome do container de resposta
           MOVE 'SGTS0054-RPST'        TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE
      
      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE
      
           EXEC CICS PUT
                     CONTAINER (NM-CTNR                )
                     FROM      (SGTS0054-RPST          )
                     FLENGTH   (LENGTH OF SGTS0054-RPST)
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
      * em modulos executados mais de uma vez dentro da mesma sessao do*
      * CICS. Equivale ao initialize na area de resposta dos programas *
      * que utilizam COMMAREA.                                         *
      *----------------------------------------------------------------*
      
           MOVE 0014                   TO MSG1-CD-SEQL-ERRO
      
           MOVE NM-PGM                 TO NM-CTNR(1:8)
           MOVE '-RPST'                TO NM-CTNR(9:5)
      
           SET DEL-CONTAINER           TO TRUE
      
      *    Exclui o container de resposta
      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS DELETE
                     CONTAINER (NM-CTNR)
                     RESP      (MSG1-RESP1)
                     RESP2     (MSG1-RESP2)
           END-EXEC
      
           EVALUATE TRUE
               WHEN MSG1-RESP1         EQUAL DFHRESP(CONTAINERERR)
               WHEN MSG1-RESP1         EQUAL DFHRESP(NORMAL)
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
      
      *    Limpar variaveis containerde erro
           INITIALIZE IIBKERRO-PRM-ERRO
      
      *    Exclui o container de erro
           MOVE 0015                   TO MSG1-CD-SEQL-ERRO
           MOVE 'PRM-ERRO'             TO NM-CTNR
           SET DEL-CONTAINER           TO TRUE
      
      *    Não há captura automática de erro CICS por usar RESP e RESP2
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
               MOVE 8888               TO MSG1-CD-ERRO-TS
           END-IF
      
           IF  MSG1-TX-ERRO-TS         EQUAL SPACES
               MOVE 'ERRO NAO ESPECIFICADO NO MODULO'
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

               MOVE 'SGTS0054-E-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE SGTS0054-E-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0054-CD-ULT-TIP-NEGO'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE SGTS0054-CD-ULT-TIP-NEGO
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT
      

      
      *    Grava o erro gerado no sistema MSG - Mensageria
           PERFORM MSG1-NTF-MSG
      
      *    Movimenta Mensagem e Código de Erro para o Container de Erro
           MOVE MSG1-CD-ERRO-TS        TO IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS        TO IIBKERRO-TX-ERRO
      
      *    Grava o erro gerado no container de erro
           PERFORM 984000-GRAVAR-CONTAINER-ERRO
      
      *    Encerra a transacao
           PERFORM 990000-ENCERRAR
           .
       999000-SAI.
           EXIT.
      * --------------------------------------*
      * 999001-TRATA-ABEND-CICS        SECTION.
      * 999002-TRATA-CONDITION-CICS    SECTION.
      * 999003-ADICIONA-VRV-MNT        SECTION.
      * --------------------------------------*
       COPY HLPKP999.                                                            000
      * --------------------------------------*
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *---------------------------------------*
       COPY SGTKP999.                                                           0000
      * --------------------------------------*
      * MSG1-NTF-MSG                   SECTION.
      * --------------------------------------*
       COPY MSGKPMG1.                                                           0000
      *---------------------- FIM SGTS0054 -----------------------------
