      *-----------------------------------------------------------------
      * Programa..: SGTS0127
      * Analista..: F2054109 - Cintia Ap Soares Iglesias
      * Autor.....: F8214779 - Pedro Argolo Lacerda
      * Data......: 23/05/2017
      * Objetivo..: Incluir Operação Vinculada para pagamento.
      * Compilacao: Cobol/CICS
      *-----------------------------------------------------------------
      * VRS001 31/01/2019 - F0950413 inclusão da sigla 'SGT' na section
      *                     972000-LER-CONTAINER-DE-ERRO
      *        11/05/2018 - F8658869 - DE 2016/145195 - Alterar carteira
      *        08/01/2018 - F8785286 - DE 2016/145195 -
      *                     Inclusão da chamada ao SGTSPE04
      *                     para obter o saldo atual da operação
      *                     vinculada, acerto dos campos na
      *                     section 213000-VALIDA-VLR-OPR-PGDR
      *                     para considerar o valor saldo das pagadoras,
      *                     comparando com o saldo atual do SGTSPE04.
      *        23/05/2017 - F8214779 - DE 2016/145195 - Implantação
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTS0127.

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

       WORKING-STORAGE                 SECTION.
      *-----------------------------------------------------------------

       77  CTE-INICIO-SS               PIC  X(40) VALUE
                                       '*** S.S. Comeca Aqui ***'.
       77  CTE-PROG                    PIC  X(018)  VALUE
                                       '*** SGTS0127 ***'.
       77  CTE-VERS                    PIC  X(006)  VALUE 'VRS001'.

      *--> Módulos Acionados
       77  SGTS0081                    PIC  X(08) VALUE 'SGTS0081'.
       77  SGTSC005                    PIC  X(08) VALUE 'SGTSC005'.
       77  SGTSC069                    PIC  X(08) VALUE 'SGTSC069'.
       77  SGTSI071                    PIC  X(08) VALUE 'SGTSI071'.
       77  SGTSP071                    PIC  X(08) VALUE 'SGTSP071'.
       77  SGTSPE04                    PIC  X(08) VALUE 'SGTSPE04'.
       77  SGTSPE20                    PIC  X(08) VALUE 'SGTSPE20'.
       77  SGTST009                    PIC  X(08) VALUE 'SGTST009'.

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

      *--> Areas de trabalho
       01  VL-PGTO.
          03 VL-PGTO-DISPLAY           PIC ZZZZZZZZZZZZZZ9,9(02).

      *--> Auxiliares Movimentacao Monitoração
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).

      *-----------------------------------------------------------------
      *    Books Utilizados
      *-----------------------------------------------------------------

      *--> Book dos dados do usuário
-INC IIBKOPRD

      *--> Book do Container de Erro
-INC IIBKERRO

      *--> Book para captura de abend cics da bloco codigo HLPKW999
-INC HLPKW999

      *--> Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1

      *--> Book para captura de erro via MSG1
-INC MSGKWMG1

      *--> Book do container de requisição e resposta deste modulo
       01 SGTS0127-DADOS.
-INC SGTK0127

      *-----------------------------------------------------------------
      * Book dos Containers e das subrotinas chamadas
      *-----------------------------------------------------------------

      *--> Consulta da tabela OPR_ETRD_MCP
-INC SGTKC005

      *--> Soma valores vinculados da tabela DB2SGT.OPR_PG_OTR_OPR
       01  SGTSP071-DADOS.
-INC SGTKP071

       01  SGTST009-DADOS.
-INC SGTKT009

       01  SGTSC069-DADOS.
-INC SGTKC069

      *--> Busca último saldo cadastrado
       01  SGTSPE04-DADOS.
-INC SGTKPE04

       01  SGTSPE20-DADOS.
-INC SGTKPE20

      *--> Inclusão na tabela OPR_PG_OTR_OP
       01  SGTSI071-DADOS.
-INC SGTKI071

      *--> Consultar Disponibilidade de Atributos por Tipo de Ativo
      *    e Estado da Operação
-INC SGTK0081

       01  CTE-FINAL-SS                PIC X(40) VALUE
                                       '*** S.S. Termina Aqui ***'.

      *-----------------------------------------------------------------
       PROCEDURE                       DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------

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

      *--> Ativa a captura automática de abend do CICS.
      *    WHENEVER CICS
-INC HLPKE999

           PERFORM  970000-LER-CANAL

      *--> Define o nome do programa p/ fins manipulação container
           MOVE  CTE-PROG(5:8)         TO  NM-PGM

      *--> Limpa a resposta e erro antes de executar o módulo
           PERFORM  983000-EXCLUIR-RESPOSTA-ERRO

           PERFORM  971000-LER-REQUISICAO

           PERFORM  110000-VALIDAR-REQUISICAO
           .
       100000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       110000-VALIDAR-REQUISICAO       SECTION.
      *--> Valida campo a campo da requisição
      *-----------------------------------------------------------------
           MOVE  0002                  TO  MSG1-CD-SEQL-ERRO

      *--> Validar Operação Pagadora
           IF  SGTS0127-CD-CTRA-OPR-PGDR   EQUAL ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0496              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira da operação pagadora não inform
      -              'ado.'            TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0127-CD-CTRA-OPR-PGDR   NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0497              TO  MSG1-CD-ERRO-TS
               MOVE 'Código da carteira da operação pagadora inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0127-CD-OPR-PGDR        NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0498              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada pagadora não numéri
      -              'co.'             TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0127-CD-OPR-PGDR        EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0499              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada pagadora não inform
      -              'ado.'            TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0127-CD-OPR-PGDR        NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0500              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada pagadora inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0127-NR-VRS-OPR-PGDR    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0501              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação pagadora não informad
      -              'o.'              TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0127-NR-VRS-OPR-PGDR    NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0502              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação pagadora inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

      *--> Valida a Operação Vinculada
           IF  SGTS0127-CD-CTRA-OPR-VCLD
                                           EQUAL ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0503              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira da operação vinculada não infor
      -              'mado.'           TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0127-CD-CTRA-OPR-VCLD
                                           NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0504              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira da operação vinculada inválido.
      -              ''                TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0127-CD-OPR-ETRD-VCLD   NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0505              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada vinculada não numér
      -              'ico.'            TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0127-CD-OPR-ETRD-VCLD   EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0506              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada vinculada não infor
      -              'mado.'           TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0127-CD-OPR-ETRD-VCLD   NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0507              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada vinculada inválido.
      -              ''                TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0127-NR-VRS-OPR-VCLD    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0508              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação vinculada não informa
      -              'do.'             TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0127-NR-VRS-OPR-VCLD    NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0509              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação vinculada inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

      *--> Valida o Valor do Pagamento
           IF  SGTS0127-VL-PGTO            NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0532              TO  MSG1-CD-ERRO-TS
               MOVE  'Valor do pagamento não numérico.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0127-VL-PGTO            NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0510              TO  MSG1-CD-ERRO-TS
               MOVE  'Valor do pagamento inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           .
       110000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-PROCESSAR                SECTION.
      *-----------------------------------------------------------------
           MOVE  0003                  TO  MSG1-CD-SEQL-ERRO

           PERFORM  211000-CONSULTAR-ATRIBUTOS-OPR

           PERFORM  212000-OBTER-ESTADO-OPR-VCLD

      *--> Se estado da operação vinculada diferente de encarteirada
      *    emitir erro
           IF  SGTST009-CD-TIP-EST-OPR     NOT EQUAL  0005
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0511              TO  MSG1-CD-ERRO-TS
               MOVE  'Operação vinculada deve estar encarteirada.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  969000-MONITORAR-VARIAVEIS
               PERFORM  999000-ERRO
           END-IF

           PERFORM  213000-OBTER-ESTADO-VRS-VCLD

      *--> Se estado da versão da operação vinculada diferente
      *    aprovada emitir erro
           IF  SGTSC069-CD-TIP-EST-VRS     NOT EQUAL  0003
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0512              TO  MSG1-CD-ERRO-TS
               MOVE  'Versão do estado da operação vinculada não está ap
      -              'rovada.'         TO  MSG1-TX-ERRO-TS
               PERFORM  969000-MONITORAR-VARIAVEIS
               PERFORM  999000-ERRO
           END-IF

           PERFORM  214000-OBTER-ESTADO-VRS-PGDR

      *--> Se estado da versão da operação pagadora diferente
      *    de em edição emitir erro
           IF  SGTSC069-CD-TIP-EST-VRS     NOT EQUAL  0001
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0513              TO  MSG1-CD-ERRO-TS
               MOVE  'Versão do estado da operação pagadora não está apr
      -              'ovada.'          TO  MSG1-TX-ERRO-TS
               PERFORM  969000-MONITORAR-VARIAVEIS
               PERFORM  999000-ERRO
           END-IF

           PERFORM  215000-OBTER-ULTIMA-VRS-ATIVA

           IF  SGTSPE20-NR-VRS-OPR-ETRD    NOT EQUAL
                              SGTS0127-NR-VRS-OPR-VCLD
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0514              TO  MSG1-CD-ERRO-TS
               MOVE  'Foi encontrada versão mais atualizada da operação
      -              'vinculada.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  969000-MONITORAR-VARIAVEIS
               PERFORM  999000-ERRO
           END-IF

           PERFORM  216000-OBTER-SOMA-VALOR-VCLD

           PERFORM  217000-VALIDAR-VLR-OPR-VCLD

           PERFORM  218000-VALIDAR-VLR-OPR-PGDR

           PERFORM  219000-INCLUIR-OPR-VINCULADA

           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       211000-CONSULTAR-ATRIBUTOS-OPR  SECTION.
      *--> Consultar indicadores de permissão de alteração SGTS0081
      *-----------------------------------------------------------------
           MOVE  0004                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      SGTS0081-DADOS.

      *--> Movimentar o nome do programa a ser executado
           MOVE  'SGTS0081'            TO  NM-PGM

      *--> Movimentar os campos de entrada
           MOVE  SGTS0127-CD-CTRA-OPR-PGDR TO  SGTS0081-CD-CTRA-TIT-VL
           MOVE  SGTS0127-CD-OPR-PGDR      TO  SGTS0081-CD-OPR-ETRD
           MOVE  SGTS0127-NR-VRS-OPR-PGDR  TO  SGTS0081-NR-VRS-OPR-ETRD

           PERFORM  900000-EXECUTAR-MODULO

           IF  SGTS0081-IN-HBLT-MENU-PGTO  EQUAL  'N'
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0522              TO  MSG1-CD-ERRO-TS
               MOVE  'Estado da operação pagadora não permite Inclusão.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  969000-MONITORAR-VARIAVEIS
               PERFORM  999000-ERRO
           END-IF
           .
       211000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       212000-OBTER-ESTADO-OPR-VCLD    SECTION.
      *-----------------------------------------------------------------
           MOVE  0005                  TO  MSG1-CD-SEQL-ERRO

      *--> Movimentar o nome do programa a ser executado
           MOVE  'SGTST009'            TO  NM-PGM

      *--> Movimentar os campos de entrada

           MOVE  SGTS0127-CD-CTRA-OPR-VCLD TO
                                          SGTST009-CD-CTRA-TIT-VL
           MOVE  SGTS0127-CD-OPR-ETRD-VCLD TO
                                          SGTST009-CD-OPR-ETRD-MCP
           MOVE  SGTS0127-NR-VRS-OPR-VCLD  TO
                                          SGTST009-NR-VRS-OPR-ETRD

           SET   SGTST009-CONSULTAR-EST-ATIVO
                                       TO TRUE

           PERFORM  920000-CHAMA-SGTST009
           .
       212000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       213000-OBTER-ESTADO-VRS-VCLD    SECTION.
      *-----------------------------------------------------------------
           MOVE  0006                  TO  MSG1-CD-SEQL-ERRO

      *--> Movimentar o nome do programa a ser executado
           MOVE  'SGTSC069'            TO  NM-PGM

      *--> Movimentar os campos de entrada

           MOVE  SGTS0127-CD-CTRA-OPR-VCLD TO
                                          SGTSC069-CD-CTRA-TIT-VL
           MOVE  SGTS0127-CD-OPR-ETRD-VCLD TO
                                          SGTSC069-CD-OPR-ETRD-MCP
           MOVE  SGTS0127-NR-VRS-OPR-VCLD  TO
                                          SGTSC069-NR-VRS-OPR-ETRD

           PERFORM  900000-EXECUTAR-MODULO
           .
       213000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       214000-OBTER-ESTADO-VRS-PGDR    SECTION.
      *-----------------------------------------------------------------
           MOVE  0007                  TO  MSG1-CD-SEQL-ERRO

      *--> Movimentar o nome do programa a ser executado
           MOVE  'SGTSC069'            TO  NM-PGM

      *--> Movimentar os campos de entrada

           MOVE  SGTS0127-CD-CTRA-OPR-PGDR TO
                                          SGTSC069-CD-CTRA-TIT-VL
           MOVE  SGTS0127-CD-OPR-PGDR     TO
                                          SGTSC069-CD-OPR-ETRD-MCP
           MOVE  SGTS0127-NR-VRS-OPR-PGDR  TO
                                          SGTSC069-NR-VRS-OPR-ETRD

           PERFORM  900000-EXECUTAR-MODULO

           .
       214000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       215000-OBTER-ULTIMA-VRS-ATIVA   SECTION.
      *-----------------------------------------------------------------
           MOVE  0008                  TO  MSG1-CD-SEQL-ERRO

      *--> Movimentar o nome do programa a ser executado
           MOVE  'SGTSPE20'            TO  NM-PGM

      *--> Movimentar os campos de entrada

           MOVE  SGTS0127-CD-CTRA-OPR-VCLD TO
                                          SGTSPE20-CD-CTRA-TIT-VL
           MOVE  SGTS0127-CD-OPR-ETRD-VCLD TO
                                          SGTSPE20-CD-OPR-ETRD-MCP

           PERFORM  900000-EXECUTAR-MODULO
           .
       215000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       216000-OBTER-SOMA-VALOR-VCLD    SECTION.
      *-----------------------------------------------------------------
           MOVE  0009                  TO  MSG1-CD-SEQL-ERRO

      *--> Consultar soma de valores vinculados SGTSP071
           INITIALIZE                      SGTSP071-DADOS.

      *--> Movimentar o nome do programa a ser executado
           MOVE  'SGTSP071'            TO  NM-PGM

      *--> Movimentar os campos de entrada
           MOVE  SGTS0127-CD-CTRA-OPR-PGDR TO
                                            SGTSP071-CD-CTRA-OPR-PGDR
           MOVE  SGTS0127-CD-OPR-PGDR      TO
                                            SGTSP071-CD-OPR-PGDR
           MOVE  SGTS0127-NR-VRS-OPR-PGDR  TO
                                            SGTSP071-NR-VRS-OPR-PGDR
           MOVE  SGTS0127-CD-CTRA-OPR-VCLD TO
                                            SGTSP071-CD-CTRA-OPR-VCLD
           MOVE  SGTS0127-CD-OPR-ETRD-VCLD TO
                                            SGTSP071-CD-OPR-ETRD-VCLD
           MOVE  SGTS0127-NR-VRS-OPR-VCLD  TO
                                             SGTSP071-NR-VRS-OPR-VCLD

           PERFORM  900000-EXECUTAR-MODULO
           .
       216000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       217000-VALIDAR-VLR-OPR-VCLD     SECTION.
      *-----------------------------------------------------------------
           MOVE  0010                  TO  MSG1-CD-SEQL-ERRO

      *--> Consultar o valor EVT da operação pagadora SGTSC005
           INITIALIZE                      SGTSC005-DADOS.

      *--> Movimentar o nome do programa a ser executado
           MOVE  'SGTSC005'            TO  NM-PGM

      *--> Movimentar os campos de entrada
           MOVE  SGTS0127-CD-CTRA-OPR-PGDR
                                       TO  SGTSC005-CD-CTRA-TIT-VL
           MOVE  SGTS0127-CD-OPR-PGDR  TO  SGTSC005-CD-OPR-ETRD-MCP
           MOVE  SGTS0127-NR-VRS-OPR-PGDR
                                       TO  SGTSC005-NR-VRS-OPR-ETRD
           MOVE  'SGTSC005'            TO  NM-PGM

            PERFORM  900000-EXECUTAR-MODULO

           ADD   SGTS0127-VL-PGTO      TO  SGTSP071-SDD-VL-PGTO-VCLD
                ON SIZE ERROR
                SET   ERRO-PROGRAMADO  TO  TRUE
                MOVE  0525             TO  MSG1-CD-ERRO-TS
                MOVE  'Erro no cálculo do valor acumulado para a operaçã
      -               'o pagadora.'    TO  MSG1-TX-ERRO-TS
                PERFORM  999000-ERRO
           END-ADD

           IF  SGTSP071-SDD-VL-PGTO-VCLD   GREATER
                                           SGTSC005-VL-EVT-VCLD-VRS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0526              TO  MSG1-CD-ERRO-TS
               MOVE  'Inclusão não permitida. Soma dos valores das opera
      -              'ções vinculadas ultrapassa o limite da operação pa
      -              'gadora.'         TO  MSG1-TX-ERRO-TS
               PERFORM  969000-MONITORAR-VARIAVEIS
               PERFORM  999000-ERRO
           END-IF
           .
       217000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       218000-VALIDAR-VLR-OPR-PGDR     SECTION.
      *-----------------------------------------------------------------
           MOVE  0011                  TO  MSG1-CD-SEQL-ERRO

      *--> Consultar o valor EVT da operação vinculada SGTSC005
           INITIALIZE                      SGTSC005-DADOS.

      *--> Movimentar o nome do programa a ser executado
           MOVE  'SGTSC005'            TO  NM-PGM

      *--> Movimentar os campos de entrada
           MOVE  SGTS0127-CD-CTRA-OPR-VCLD
                                       TO  SGTSC005-CD-CTRA-TIT-VL
           MOVE  SGTS0127-CD-OPR-ETRD-VCLD
                                       TO  SGTSC005-CD-OPR-ETRD-MCP
           MOVE  SGTS0127-NR-VRS-OPR-VCLD
                                       TO  SGTSC005-NR-VRS-OPR-ETRD
           MOVE  'SGTSC005'            TO  NM-PGM

           PERFORM  900000-EXECUTAR-MODULO

           PERFORM  218100-OBTER-ULT-SDO-OPR

           ADD  SGTS0127-VL-PGTO       TO  SGTSP071-SDD-VL-PGTO-PGDR
                ON SIZE ERROR
                SET   ERRO-PROGRAMADO  TO  TRUE
                MOVE  0527             TO  MSG1-CD-ERRO-TS
                MOVE  'Erro no cálculo do valor acumulado para a operaçã
      -               'o vinculada.'   TO  MSG1-TX-ERRO-TS
                PERFORM  999000-ERRO
           END-ADD

           IF  SGTSP071-SDD-VL-PGTO-PGDR   GREATER
                                           SGTSPE04-VL-SDO-TIT-DT-PSC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0528              TO  MSG1-CD-ERRO-TS
               MOVE  'Inclusão não permitida. Soma dos valores das opera
      -              'ções pagadoras ultrapassa o limite da operações vi
      -              'nculadas.'       TO  MSG1-TX-ERRO-TS
               PERFORM  969000-MONITORAR-VARIAVEIS
               PERFORM  999000-ERRO
           END-IF
           .
       218000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       218100-OBTER-ULT-SDO-OPR        SECTION.
      *-----------------------------------------------------------------
           MOVE  0012                  TO  MSG1-CD-SEQL-ERRO

      *--> Movimentar o nome do programa a ser executado
           MOVE  SGTSPE04              TO  NM-PGM

      *--> Movimentar os campos de entrada
           MOVE  SGTSC005-CD-CTRA-TIT-VL   TO  SGTSPE04-CD-CTRA-TIT-VL
           MOVE  SGTSC005-CD-TIT-OPR-ETRD  TO  SGTSPE04-CD-TIT-OPR-ETRD
           MOVE  SGTSC005-NR-DVS-TIT       TO  SGTSPE04-NR-DVS-TIT

           PERFORM  900000-EXECUTAR-MODULO
           .
       218100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       219000-INCLUIR-OPR-VINCULADA    SECTION.
      *-----------------------------------------------------------------
           MOVE  0013                  TO  MSG1-CD-SEQL-ERRO

      *--> Incluir operação vinculada para pagamento SGTSI071
           INITIALIZE                      SGTSI071-DADOS.

      *--> Movimentar o nome do programa a ser executado
           MOVE  'SGTSI071'            TO  NM-PGM

      *--> Movimentar os campos de entrada
           MOVE  SGTS0127-CD-CTRA-OPR-PGDR
                                       TO  SGTSI071-CD-CTRA-OPR-PGDR
           MOVE  SGTS0127-CD-OPR-PGDR  TO  SGTSI071-CD-OPR-PGDR
           MOVE  SGTS0127-NR-VRS-OPR-PGDR
                                       TO  SGTSI071-NR-VRS-OPR-PGDR
           MOVE  SGTS0127-CD-CTRA-OPR-VCLD
                                       TO  SGTSI071-CD-CTRA-OPR-VCLD
           MOVE  SGTS0127-CD-OPR-ETRD-VCLD
                                       TO  SGTSI071-CD-OPR-ETRD-VCLD
           MOVE  SGTS0127-NR-VRS-OPR-VCLD
                                       TO  SGTSI071-NR-VRS-OPR-VCLD
           MOVE  'CURRENT'             TO  SGTSI071-TS-ULT-ATL
           MOVE  SGTS0127-VL-PGTO      TO  SGTSI071-VL-PGTO

           PERFORM  900000-EXECUTAR-MODULO

           .
       219000-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *    Section da Faixa de 900000 a 969900 para Demais Rotinas
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       900000-EXECUTAR-MODULO          SECTION.
      *--> Executa um modulo Online implementado com CONTAINER
      *    Nao incluir nessa section os módulos chamados via COMMAREA
      *-----------------------------------------------------------------

      *--> Grava o container de requisição do módulo a executar
           PERFORM  982000-GRAVAR-CONTAINER-RQSC

      *--> Limpa a resposta e erro antes de executar o módulo
           PERFORM  983000-EXCLUIR-RESPOSTA-ERRO

           MOVE  0014                  TO  MSG1-CD-SEQL-ERRO

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EVALUATE  NM-PGM
               WHEN  SGTSP071
                     EXEC CICS LINK
                               PROGRAM ( SGTSP071   )
                               CHANNEL ( NM-CNL     )
                               RESP    ( MSG1-RESP1 )
                               RESP2   ( MSG1-RESP2 )
                     END-EXEC
               WHEN  SGTS0081
                     EXEC CICS LINK
                               PROGRAM ( SGTS0081    )
                               CHANNEL ( NM-CNL    )
                               RESP    ( MSG1-RESP1)
                               RESP2   ( MSG1-RESP2)
                     END-EXEC
               WHEN  SGTSC005
                     EXEC CICS LINK
                               PROGRAM ( SGTSC005    )
                               CHANNEL ( NM-CNL    )
                               RESP    ( MSG1-RESP1)
                               RESP2   ( MSG1-RESP2)
                     END-EXEC
               WHEN  SGTSI071
                     EXEC CICS LINK
                               PROGRAM ( SGTSI071  )
                               CHANNEL ( NM-CNL    )
                               RESP    ( MSG1-RESP1)
                               RESP2   ( MSG1-RESP2)
                     END-EXEC
               WHEN  SGTSC069
                     EXEC CICS LINK
                               PROGRAM ( SGTSC069  )
                               CHANNEL ( NM-CNL    )
                               RESP    ( MSG1-RESP1)
                               RESP2   ( MSG1-RESP2)
                     END-EXEC
               WHEN  SGTSPE20
                     EXEC CICS LINK
                               PROGRAM ( SGTSPE20  )
                               CHANNEL ( NM-CNL    )
                               RESP    ( MSG1-RESP1)
                               RESP2   ( MSG1-RESP2)
                     END-EXEC
               WHEN  SGTSPE04
                     EXEC CICS  LINK
                                PROGRAM ( SGTSPE04   )
                                CHANNEL ( NM-CNL     )
                                RESP    ( MSG1-RESP1 )
                                RESP2   ( MSG1-RESP2 )
                     END-EXEC
               WHEN  OTHER
                     SET   ERRO-PROGRAMADO
                                       TO  TRUE
                     MOVE  9006        TO  MSG1-CD-ERRO-TS
                     STRING  'Não definida a execução do módulo '
                             NM-PGM '.'
                             DELIMITED BY SIZE
                             INTO          MSG1-TX-ERRO-TS
                     END-STRING
                     PERFORM  999000-ERRO
           END-EVALUATE

           EVALUATE  TRUE
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(NORMAL)
                     CONTINUE
               WHEN  OTHER
                     SET   ERRO-CICS     TO  TRUE
                     MOVE  3004         TO  MSG1-CD-ERRO-TS
                     STRING  'Erro na execução do módulo ' NM-PGM ', '
                             'RESP  = '    MSG1-RESP1  ', '
                             'RESP2 = '    MSG1-RESP2
                             DELIMITED BY SIZE
                             INTO          MSG1-TX-ERRO-TS
                     PERFORM  969000-MONITORAR-VARIAVEIS
                     PERFORM  999000-ERRO
           END-EVALUATE

      *--> Efetua a leitura do Container de Erro
           PERFORM  972000-LER-CONTAINER-DE-ERRO

      *--> Efetua a leitura do Container de Resposta
           PERFORM  973000-LER-RESPOSTA
           .
       900000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       920000-CHAMA-SGTST009           SECTION.
      *-----------------------------------------------------------------
           MOVE  0015                  TO  MSG1-CD-SEQL-ERRO

      *--> Movimento nome do módulo para monitoração de variáveis
           MOVE  SGTST009              TO  NM-PGM

           EXEC CICS LINK
                     PROGRAM  ( SGTST009       )
                     COMMAREA ( SGTST009-DADOS )
           END-EXEC

           IF  SGTST009-CD-RTN-PGM         NOT EQUAL  ZEROS
               SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE  NM-PGM            TO  MSG1-NM-PGM-ACND
               MOVE  SGTST009-CD-RTN-PGM
                                       TO  MSG1-CD-ERRO-TS
                                           MSG1-CD-RTN-PGM-ACND
               MOVE  SGTST009-MSG-ERRO
                                       TO  MSG1-TX-RTN-PGM-ACND
                                           MSG1-TX-ERRO-TS
               MOVE  SGTST009-PSC-ERRO-PGM
                                       TO  MSG1-CD-SEQL-PGM-ACND
               PERFORM  969000-MONITORAR-VARIAVEIS
               PERFORM  999000-ERRO
           END-IF
           .
       920000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       969000-MONITORAR-VARIAVEIS      SECTION.
      *--> Monitora as variáveis dos módulos chamados
      *-----------------------------------------------------------------
           MOVE  MSG1-TX-ERRO-TS       TO  IIBKERRO-TX-ERRO
           MOVE  MSG1-CD-ERRO-TS       TO  IIBKERRO-CD-ERRO

           EVALUATE  NM-PGM
               WHEN  SGTSP071
                     MOVE 'SGTSP071-CD-CTRA-OPR-PGDR'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSP071-CD-CTRA-OPR-PGDR
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSP071-CD-OPR-PGDR'
                                       TO  MSGKW999-NM-VRV-MNT
      *------> Verifica se o conteudo e numerico antes de adicionar
                     IF  SGTSP071-CD-OPR-PGDR    IS NUMERIC
                         MOVE  SGTSP071-CD-OPR-PGDR
                                       TO  CD-OPR-ETRD-MCP
                         MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE  'Valor não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSP071-NR-VRS-OPR-PGDR'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSP071-NR-VRS-OPR-PGDR
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSP071-CD-CTRA-OPR-VCLD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSP071-CD-CTRA-OPR-VCLD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSP071-CD-OPR-ETRD_VCLD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSP071-CD-OPR-ETRD-VCLD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSP071-NR-VRS-OPR-VCLD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSP071-NR-VRS-OPR-VCLD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'IIBKERRO-TX-ERRO'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  IIBKERRO-TX-ERRO
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'IIBKERRO-CD-ERRO'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  IIBKERRO-CD-ERRO
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  SGTS0081
                     MOVE 'SGTS0081-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                     MOVE SGTS0081-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                     PERFORM 999003-ADICIONA-VRV-MNT

                     MOVE 'SGTS0081-CD-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
      *------> Verifica se o conteudo e numerico antes de adicionar
                     IF  SGTS0081-CD-OPR-ETRD    IS NUMERIC
                         MOVE  SGTS0081-CD-OPR-ETRD
                                       TO  CD-OPR-ETRD-MCP
                         MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE  'Valor não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTS0081-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0081-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'IIBKERRO-TX-ERRO'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  IIBKERRO-TX-ERRO
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'IIBKERRO-CD-ERRO'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  IIBKERRO-CD-ERRO
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  SGTSC005
                     MOVE 'SGTSC005-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSC005-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSC005-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
      *------> Verifica se o conteudo e numerico antes de adicionar
                     IF  SGTSC005-CD-OPR-ETRD-MCP    IS NUMERIC
                         MOVE  SGTSC005-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP
                         MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE  'Valor não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSC005-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSC005-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  SGTSI071
                     MOVE 'SGTSI071-CD-CTRA-OPR-PGDR'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI071-CD-CTRA-OPR-PGDR
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSI071-CD-OPR-PGDR'
                                       TO  MSGKW999-NM-VRV-MNT
      *------> Verifica se o conteudo e numerico antes de adicionar
                     IF  SGTSI071-CD-OPR-PGDR    IS NUMERIC
                         MOVE  SGTSI071-CD-OPR-PGDR
                                       TO  CD-OPR-ETRD-MCP
                         MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE  'Valor não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSI071-NR-VRS-OPR-PGDR'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI071-NR-VRS-OPR-PGDR
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSI071-CD-CTRA-OPR-VCLD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI071-CD-CTRA-OPR-VCLD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSI071-CD-OPR-ETRD-VCLD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI071-CD-OPR-ETRD-VCLD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSI071-NR-VRS-OPR-VCLD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI071-NR-VRS-OPR-VCLD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSI071-TS-ULT-ATL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI071-TS-ULT-ATL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  SGTST009
                     MOVE 'SGTST009-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST009-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTST009-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
      *------> Verifica se o conteudo e numerico antes de adicionar
                     IF  SGTST009-CD-OPR-ETRD-MCP    IS NUMERIC
                         MOVE  SGTST009-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP
                         MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE  'Valor não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTST009-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST009-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  SGTSC069
                     MOVE 'SGTSC069-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSC069-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSC069-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
      *------> Verifica se o conteudo e numerico antes de adicionar
                     IF  SGTSC069-CD-OPR-ETRD-MCP    IS NUMERIC
                         MOVE  SGTSC069-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP
                         MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE  'Valor não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSC069-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSC069-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  SGTSPE20
                     MOVE 'SGTSPE20-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSPE20-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSPE20-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
      *------> Verifica se o conteudo e numerico antes de adicionar
                     IF  SGTSPE20-CD-OPR-ETRD-MCP    IS NUMERIC
                         MOVE  SGTSPE20-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP
                         MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE  'Valor não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

      *------> Monitora as Variaveis de Entrada do SGTSPE04
               WHEN  SGTSPE04
                     MOVE 'SGTSPE04-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSPE04-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSPE04-CD-TIT-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSPE04-CD-TIT-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSPE04-NR-DVS-TIT'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSPE04-NR-DVS-TIT
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

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
           MOVE  0016                  TO  MSG1-CD-SEQL-ERRO

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

      *--> Monta o nome do container de requisição
           MOVE  CTE-PROG(5:8)         TO  NM-PGM
           MOVE  'SGTS0127-RQSC'       TO  NM-CTNR

      *--> Recupera o tamanho do container de requisição
           PERFORM  973100-LER-CONTAINER-NODATA

           MOVE  0017                  TO  MSG1-CD-SEQL-ERRO

      *--> Obtem o tamanho esperado da requisição
           MOVE  LENGTH OF SGTS0127-RQSC
                                       TO  QT-CCT-EPRO

      *--> Valida tamanho esperado da requisição X tamanho recebido
           IF  QT-CCT-LIDO                 NOT EQUAL  QT-CCT-EPRO
               SET  ERRO-TAMANHO-AREA TO TRUE
               PERFORM  999000-ERRO
           END-IF

           MOVE  0018                  TO  MSG1-CD-SEQL-ERRO

      *--> Configura dados para leitura do container de requisição
           SET   GET-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *--> Efetua a leitura do container
           EXEC CICS GET
                     CONTAINER ( NM-CTNR       )
                     FLENGTH   ( QT-CCT-LIDO   )
                     INTO      ( SGTS0127-RQSC )
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
           MOVE  0019                  TO  MSG1-CD-SEQL-ERRO

      *--> Le o container de erro dos módulos chamados
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

               WHEN  OTHER
                     SET   ERRO-CICS
                                       TO  TRUE
                     PERFORM  999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       972000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       973000-LER-RESPOSTA             SECTION.
      *--> Recupera o container com os dados de resposta (saída).
      *    Módulo CONTAINER  -     NOME_PGM-RPST
      *    Módulo IIB (FIXO) -     PRM-DADOS-RPST
      *-----------------------------------------------------------------

      *--> Monta o nome dos container de acordo com módulo a executar
           MOVE  SPACES                TO  NM-CTNR

           MOVE  NM-PGM                TO  NM-CTNR(1:8)
           MOVE  '-RPST'               TO  NM-CTNR(9:5)

      *--> Recupera o tamanho do container de resposta
           PERFORM  973100-LER-CONTAINER-NODATA

           MOVE  0020                  TO  MSG1-CD-SEQL-ERRO

      *--> Configura dados para leitura do container de resposta
           SET   GET-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *--> Lê a RESPOSTA de acordo com o programa que a gravou

           EVALUATE  NM-PGM
      *-->     Calcular a quantidade de caracteres esperado e limpar
      *        a área de dados antes de executar o GET
               WHEN  SGTSP071
                     INITIALIZE            SGTSP071-RPST
                     EXEC CICS GET
                               CONTAINER ( NM-CTNR          )
                               INTO      ( SGTSP071-RPST    )
                               FLENGTH   ( QT-CCT-LIDO      )
                     END-EXEC
               WHEN  SGTS0081
                     INITIALIZE            SGTS0081-RPST
                     EXEC CICS GET
                               CONTAINER ( NM-CTNR          )
                               INTO      ( SGTS0081-RPST    )
                               FLENGTH   ( QT-CCT-LIDO      )
                     END-EXEC
               WHEN  SGTSC005
                     INITIALIZE            SGTSC005-RPST
                     EXEC CICS GET
                               CONTAINER ( NM-CTNR          )
                               INTO      ( SGTSC005-RPST    )
                               FLENGTH   ( QT-CCT-LIDO      )
                     END-EXEC
               WHEN  SGTSI071
                     INITIALIZE            SGTSI071-RPST
                     EXEC CICS GET
                               CONTAINER ( NM-CTNR          )
                               INTO      ( SGTSI071-RPST    )
                               FLENGTH   ( QT-CCT-LIDO      )
                     END-EXEC
               WHEN  SGTSC069
                     EXEC CICS GET
                               CONTAINER ( NM-CTNR          )
                               INTO      ( SGTSC069-RPST)
                               FLENGTH   ( QT-CCT-LIDO      )
                     END-EXEC
               WHEN  SGTSPE20
                     EXEC CICS GET
                               CONTAINER ( NM-CTNR          )
                               INTO      ( SGTSPE20-RPST    )
                               FLENGTH   ( QT-CCT-LIDO      )
                     END-EXEC
               WHEN  SGTSPE04
                     EXEC CICS GET
                               CONTAINER ( NM-CTNR          )
                               INTO      ( SGTSPE04-RPST    )
                               FLENGTH   ( QT-CCT-LIDO      )
                     END-EXEC
               WHEN  OTHER
                     SET   ERRO-PROGRAMADO
                                       TO  TRUE
                     MOVE  9003        TO  MSG1-CD-ERRO-TS
                     STRING  'Erro na leitura da resposta do módulo '
                             NM-PGM '.'
                             DELIMITED BY SIZE
                             INTO          MSG1-TX-ERRO-TS
                     END-STRING
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
           MOVE  0021                  TO  MSG1-CD-SEQL-ERRO

      *--> Configura dados para leitura do container de requisição
           SET   GET-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *--> Obtem a quantidade de caracteres no container
           MOVE  ZEROS                 TO  QT-CCT-LIDO

           EXEC CICS GET
                     CONTAINER ( NM-CTNR     )
                     NODATA
                     FLENGTH   ( QT-CCT-LIDO )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       973100-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *   Section da Faixa de 980000 a 989900 para Gravação de Dados
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       982000-GRAVAR-CONTAINER-RQSC    SECTION.
      *-----------------------------------------------------------------
      *--> Monta o nome dos container de acordo com módulo a executar
           MOVE  SPACES                TO  NM-CTNR
           SET   PUT-CONTAINER         TO  TRUE


           MOVE  NM-PGM                TO  NM-CTNR(1:8)
           MOVE  '-RQSC'               TO  NM-CTNR(9:5)


      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *--> Faz PUT da requisição do programa a executar
           EVALUATE  NM-PGM
               WHEN  SGTSP071
                     EXEC CICS PUT
                               CONTAINER ( NM-CTNR                 )
                               FROM      ( SGTSP071-RQSC           )
                               FLENGTH   ( LENGTH OF SGTSP071-RQSC )
                     END-EXEC
               WHEN  SGTS0081
                     EXEC CICS PUT
                               CONTAINER ( NM-CTNR                 )
                               FROM      ( SGTS0081-RQSC           )
                               FLENGTH   ( LENGTH OF SGTS0081-RQSC )
                     END-EXEC
               WHEN  SGTSC005
                     EXEC CICS PUT
                               CONTAINER ( NM-CTNR                 )
                               FROM      ( SGTSC005-RQSC           )
                               FLENGTH   ( LENGTH OF SGTSC005-RQSC )
                     END-EXEC
               WHEN  SGTSI071
                     EXEC CICS PUT
                               CONTAINER ( NM-CTNR                 )
                               FROM      ( SGTSI071-RQSC           )
                               FLENGTH   ( LENGTH OF SGTSI071-RQSC )
                     END-EXEC
               WHEN  SGTSC069
                     EXEC CICS PUT
                               CONTAINER ( NM-CTNR                 )
                               FROM      ( SGTSC069-RQSC           )
                               FLENGTH   ( LENGTH OF SGTSC069-RQSC )
                     END-EXEC
               WHEN  SGTSPE04
                     EXEC CICS PUT
                               CONTAINER ( NM-CTNR                 )
                               FROM      ( SGTSPE04-RQSC           )
                               FLENGTH   ( LENGTH OF SGTSPE04-RQSC )
                     END-EXEC
               WHEN  SGTSPE20
                     EXEC CICS PUT
                               CONTAINER ( NM-CTNR                 )
                               FROM      ( SGTSPE20-RQSC           )
                               FLENGTH   ( LENGTH OF SGTSPE20-RQSC )
                     END-EXEC
               WHEN  OTHER
                     SET   ERRO-PROGRAMADO
                                       TO  TRUE
                     MOVE  9004        TO  MSG1-CD-ERRO-TS
                     STRING  'Erro na gravação da requisição do módulo.'
                             NM-PGM '.'
                             DELIMITED BY SIZE
                             INTO          MSG1-TX-ERRO-TS
                     END-STRING
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
      *    problemas em módulos executados mais de uma vez dentro
      *    da mesma sessão do CICS.
      *    Equivale ao initialize na área de resposta dos
      *    programas que utilizam COMMAREA.
      *-----------------------------------------------------------------
           MOVE  0022                  TO  MSG1-CD-SEQL-ERRO

           MOVE  NM-PGM                TO  NM-CTNR(1:8)
           MOVE  '-RPST'               TO  NM-CTNR(9:5)

           SET   DEL-CONTAINER         TO  TRUE

      *--> Exclui o container de resposta
      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS DELETE
                     CONTAINER ( NM-CTNR    )
                     RESP      ( MSG1-RESP1 )
                     RESP2     ( MSG1-RESP2 )
           END-EXEC

           EVALUATE  TRUE
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(CONTAINERERR)
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(NORMAL)
                     CONTINUE
               WHEN  OTHER
                     SET   ERRO-CICS   TO  TRUE
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
           MOVE  0023                  TO  MSG1-CD-SEQL-ERRO

      *--> Limpar variáveis container de erro
           INITIALIZE                      IIBKERRO-PRM-ERRO

      *--> Exclui o container de erro
           MOVE  'PRM-ERRO'            TO  NM-CTNR
           SET   DEL-CONTAINER         TO  TRUE

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS DELETE
                     CONTAINER ( NM-CTNR    )
                     RESP      ( MSG1-RESP1 )
                     RESP2     ( MSG1-RESP2 )
           END-EXEC

           EVALUATE  TRUE
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(CONTAINERERR)
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(NORMAL)
                     CONTINUE
               WHEN  OTHER
                     SET   ERRO-CICS   TO  TRUE
                     PERFORM  999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       983100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       984000-GRAVAR-CONTAINER-ERRO    SECTION.
      *--> Grava o erro gerado no container de erro
      *-----------------------------------------------------------------

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
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

      *-----------------------------------------------------------------
      *    As sections abaixo iniciadas com 999000 representam os
      *    tratamentos de erro através do Sistema MSG (rotina MSGS0001)
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
                     PERFORM  999004-TRATA-ERRO-CTNR
               WHEN  ERRO-TAMANHO-AREA
                     PERFORM  999005-TRATA-ERRO-AREA-DADOS
               WHEN  ERRO-CANAL
                     PERFORM  999006-TRATA-ERRO-CANAL
               WHEN  OTHER
                     CONTINUE
           END-EVALUATE

      *--> Adiciona o Texto de Erro na Monitoração
           MOVE 'IIBKERRO-TX-ERRO'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  IIBKERRO-TX-ERRO      TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

      *--> Adiciona as variáveis de entrada para monitoração
           MOVE 'SGTS0127-CD-CTRA-OPR-PGDR'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0127-CD-CTRA-OPR-PGDR
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0127-CD-OPR-PGDR'
                                       TO  MSGKW999-NM-VRV-MNT
      *------> Verifica se o conteudo e numerico antes de adicionar
           IF  SGTS0127-CD-OPR-PGDR    IS NUMERIC
               MOVE  SGTS0127-CD-OPR-PGDR
                                       TO  CD-OPR-ETRD-MCP
               MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0127-NR-VRS-OPR-PGDR'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0127-NR-VRS-OPR-PGDR
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0127-CD-CTRA-OPR-VCLD'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0127-CD-CTRA-OPR-VCLD
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0127-CD-OPR-ETRD-VCLD'
                                       TO  MSGKW999-NM-VRV-MNT
      *------> Verifica se o conteudo e numerico antes de adicionar
           IF  SGTS0127-CD-OPR-ETRD-VCLD    IS NUMERIC
               MOVE  SGTS0127-CD-OPR-ETRD-VCLD
                                       TO  CD-OPR-ETRD-MCP
               MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0127-NR-VRS-OPR-VCLD'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0127-NR-VRS-OPR-VCLD
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0127-VL-PGTO'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSI071-VL-PGTO
                                       TO  VL-PGTO-DISPLAY
           MOVE  VL-PGTO               TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

      *--> Grava as Variaveis Monitoradas no sistema MSG - Mensageria
           PERFORM  MSG1-NTF-MSG

           MOVE  MSG1-CD-ERRO-TS       TO  IIBKERRO-CD-ERRO
           MOVE  MSG1-TX-ERRO-TS       TO  IIBKERRO-TX-ERRO

      *--> Grava a Ocorrencia de Erro no Container de Erro
           PERFORM  984000-GRAVAR-CONTAINER-ERRO

      *--> Encerra a Execução do Módulo
           PERFORM  999999-ENCERRAR
           .
       999000-SAI.
           EXIT.

      *----------------------------------------
      * 999001-TRATA-ABEND-CICS        SECTION.
      * 999002-TRATA-CONDITION-CICS    SECTION.
      * 999003-ADICIONA-VRV-MNT        SECTION.
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

      *---------------------- Fim SGTS0127 -----------------------------