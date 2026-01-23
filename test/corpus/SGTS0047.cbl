      *-----------------------------------------------------------------
      * Programa..: SGTS0047
      * Analista..: F2054109 - Cintia Ap Soares Iglesias
      * Autor.....: F8650584 - Roberto Fernandes Lima
      * Data......: 01/12/2015
      * Objetivo..: Excluir Versão da Agência de Classificação Externa
      *             de Risco
      * Compilacao: Cobol/CICS
      *-----------------------------------------------------------------
      * VRS002 08/05/2019 F2020988 de 2016/145195 - Adequação mensagens
      * de erro ao padrão da mensageria.
      * VRS001 30/01/2019 F0950413 inclusão da sigla 'SGT' na section
      *              972000-LER-CONTAINER-DE-ERRO
      *        15/02/2017 F8785286 DE 2016/145195 - Inclusão campo
      *              CD-USU-RSP-ATL para chave log (movimentar a partir
      *              do container operador).
      *        01/12/2015 F8650584  DE 2013/120821 - Implantacao
      *-----------------------------------------------------------------
      
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTS0047.
      
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
      *-----------------------------------------------------------------
      
      *-----------------------------------------------------------------
       CONFIGURATION                   SECTION.
      *-----------------------------------------------------------------
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      
      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------
      
      *-----------------------------------------------------------------
       WORKING-STORAGE                 SECTION.
      *-----------------------------------------------------------------
      
       77  CTE-INICIO-SS               PIC  X(40)  VALUE
                    '*** S.S. Comeca Aqui ***'.
      
       77  CTE-PROG                    PIC  X(18) VALUE
                    '*** SGTS0047 ***'.
       77  CTE-VERS                    PIC  X(06) VALUE 'VRS002'.
      
      *--> Módulos acionados
       77  SGTSL043                    PIC  X(08) VALUE 'SGTSL043'.
       77  SGTSP006                    PIC  X(08) VALUE 'SGTSP006'.
       77  SGTST042                    PIC  X(08) VALUE 'SGTST042'.
       77  SGTST043                    PIC  X(08) VALUE 'SGTST043'.
       77  SGTST044                    PIC  X(08) VALUE 'SGTST044'.
      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------
      
      *--> Areas de trabalho
      
      *-----------------------------------------------------------------
      * Book dos Containers e das subrotinas chamadas
      *-----------------------------------------------------------------
      
      *--> Book das variáveis auxiliares do tratamento de erro via MSG
       COPY SGTKERR1.                                                           0000
      
      *--> Book para captura de abend cics da bloco codigo MSGKP999
       COPY HLPKW999.                                                            000
      
      *--> Book para captura de erro via MSG1
       COPY MSGKWMG1.                                                           0000
      
      *--> Book do Container de Erro
       COPY IIBKERRO.                                                           0000
      
      *--> Book do container de Serviços (para acionamento de serviços)
       COPY IIBKSRVC.                                                           0000
      
      *--> Book dos dados do usuário
       COPY IIBKOPRD.                                                           0000
      
      *--> Lista da tabela EST_AG_CLSC_EXNO
       01  SGTSL043-DADOS.
       COPY SGTKL043.                                                           0000
      
      *--> Lista da tabela NOTA_AG_CLSC_EXNO
       01  SGTSP006-DADOS.
       COPY SGTKP006.                                                           0000
      
      *--> Persistencia da tabela AG_CLSC_EXNO_RSCO
       01  SGTST042-DADOS.
       COPY SGTKT042.                                                           0000
      
      *--> Persistencia da tabela EST_AG_CLSC_EXNO
       01  SGTST043-DADOS.
       COPY SGTKT043.                                                           0000
      
      *--> Persistencia da tabela NOTA_AG_CLSC_EXNO
       01  SGTST044-DADOS.
       COPY SGTKT044.                                                           0000
      
      *--> Book do container de requisição e resposta deste modulo
       01  SGTS0047-DADOS.
       COPY SGTK0047.                                                           0000
      
       77  CTE-FINAL-SS                  PIC X(40) VALUE
                    '*** S.S. Termina Aqui ***'.
      
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
      
      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------
      
           PERFORM  100000-INICIAR
      
           PERFORM  200000-PROCESSAR
      
           PERFORM  990000-ENCERRAR

           GOBACK
           .
       000000-SAI.
           EXIT.
      
      *-----------------------------------------------------------------
       100000-INICIAR                  SECTION.
      *-----------------------------------------------------------------
           MOVE  0001                  TO  MSG1-CD-SEQL-ERRO
      
      *--> Ativa a captura automatica de abend do CICS.
      *    WHENEVER CICS
       COPY HLPKE999.                                                           0000
      
           PERFORM  970000-LER-CANAL
      
      *--> Define o o nome do programa p/ fins manipulacao container
           MOVE  'SGTS0047'            TO  NM-PGM
           MOVE  'SGTS0047-RPST'       TO  NM-CTNR
      *--> Limpa a resposta e erro antes de executar o modulo
           PERFORM  983000-EXCLUIR-RESPOSTA-ERRO
      
           PERFORM  971000-LER-REQUISICAO
      
           PERFORM  110000-VALIDAR-REQUISICAO
           .
       100000-SAI.
           EXIT.
      
      *-----------------------------------------------------------------
       110000-VALIDAR-REQUISICAO       SECTION.
      *-----------------------------------------------------------------
           MOVE  0002                  TO  MSG1-CD-SEQL-ERRO
      
      *--> Valida campo a campo da requisicao
      
           IF  SGTS0047-CD-AG-CLSC-RSCO  <=  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0269              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da Agência de Classificação Externa de Risc
      -              'o inválido.'     TO  MSG1-TX-ERRO-TS
               SET   MNT-VRV           TO  TRUE
               PERFORM  999000-ERRO
           END-IF
      
           IF  SGTS0047-NR-ETRD-AG-CLSC  <=  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  1003              TO  MSG1-CD-ERRO-TS
               MOVE  'Numero Estruturado da Agência de Classificação Ext
      -              'erna de Risco inválido.'
                                       TO  MSG1-TX-ERRO-TS
               SET   MNT-VRV           TO  TRUE
               PERFORM  999000-ERRO
           END-IF
           .
       110000-SAI.
           EXIT.
      
      *-----------------------------------------------------------------
       200000-PROCESSAR                SECTION.
      *-----------------------------------------------------------------
           MOVE  0003                  TO  MSG1-CD-SEQL-ERRO
      
           PERFORM 980103-LER-DADOS-OPRD
      
      *--> Inicializacao para processar lista SGTSP006 até ao final
           SET   SGTSP006-TEM-MAIS-REGISTRO
                                       TO  TRUE
      
           PERFORM  210000-TRATAR-EXCLUSAO-NOTA-AG
                   UNTIL NOT SGTSP006-TEM-MAIS-REGISTRO
      
      *--> Inicializacao para processar lista SGTSL043 até ao final
           SET   SGTSL043-TEM-MAIS-REGISTRO
                                       TO  TRUE
      
           PERFORM  220000-TRATAR-EXCLUSAO-VERSAO
                   UNTIL NOT SGTSL043-TEM-MAIS-REGISTRO
      
           PERFORM  230000-EXCLUIR-AGENCIA
      
           PERFORM  240000-MOVIMENTAR-SAIDA
           .
       200000-SAI.
           EXIT.
      
      *-----------------------------------------------------------------
       210000-TRATAR-EXCLUSAO-NOTA-AG  SECTION.
      *-----------------------------------------------------------------
           MOVE  0004                  TO  MSG1-CD-SEQL-ERRO
      
      *--> Movimentar o nome do programa a ser executado
           MOVE  SGTSP006              TO  NM-PGM
      
      *--> Movimentar os campos de entrada
           MOVE  SGTS0047-CD-AG-CLSC-RSCO
                                       TO  SGTSP006-CD-AG-CLSC-RSCO-E
           MOVE  SGTS0047-NR-ETRD-AG-CLSC
                                       TO  SGTSP006-NR-ETRD-AG-CLSC-E
      
           PERFORM  900000-EXECUTAR-MODULO
      
           PERFORM  211000-EXCLUIR-NOTA-AGENCIA
                   VARYING  SGTSP006-IC-TAB  FROM 1 BY 1
                     UNTIL  SGTSP006-IC-TAB  >  SGTSP006-QT-OCR-TAB
      
           IF  SGTSP006-QT-OCR-TAB         NOT EQUAL  ZEROS
               MOVE  SGTSP006-NR-SEQL-NOTA-AG      (SGTSP006-QT-OCR-TAB)
                                       TO  SGTSP006-NR-SEQL-NOTA-AG-E
           END-IF
           .
       210000-SAI.
           EXIT.
      
      *-----------------------------------------------------------------
       211000-EXCLUIR-NOTA-AGENCIA     SECTION.
      *-----------------------------------------------------------------
           MOVE  0005                  TO  MSG1-CD-SEQL-ERRO
      
           INITIALIZE                  SGTST044-DADOS
      
      *--> Movimentar os campos de entrada
           SET   SGTST044-EXCLUIR      TO  TRUE
           MOVE  SGTS0047-CD-AG-CLSC-RSCO
                                       TO  SGTST044-CD-AG-CLSC-RSCO
           MOVE  SGTS0047-NR-ETRD-AG-CLSC
                                       TO  SGTST044-NR-ETRD-AG-CLSC
           MOVE  SGTSP006-NR-SEQL-NOTA-AG              (SGTSP006-IC-TAB)
                                       TO  SGTST044-NR-SEQL-NOTA-AG
           MOVE KOPRD-CD-OPRD          TO SGTST044-CD-USU-RSP-ATL
      
           PERFORM  910000-EXECUTAR-SGTST044
           .
       211000-SAI.
           EXIT.
      
      *-----------------------------------------------------------------
       220000-TRATAR-EXCLUSAO-VERSAO   SECTION.
      *-----------------------------------------------------------------
           MOVE  0006                  TO  MSG1-CD-SEQL-ERRO
      
      *--> Movimentar os campos de entrada
           MOVE  SGTS0047-CD-AG-CLSC-RSCO
                                       TO  SGTSL043-CD-AG-CLSC-RSCO
           MOVE  SGTS0047-NR-ETRD-AG-CLSC
                                       TO  SGTSL043-NR-ETRD-AG-CLSC
      
           PERFORM  920000-EXECUTAR-SGTSL043
      
           PERFORM  221000-EXCLUIR-VERSAO-AGENCIA
                   VARYING  SGTSL043-IC-TAB  FROM 1 BY 1
                     UNTIL  SGTSL043-IC-TAB  >  SGTSL043-QT-OCR-TAB
      
           IF  SGTSL043-QT-OCR-TAB         NOT EQUAL  ZEROS
               MOVE  SGTSL043-TS-EST-AG-CLSC       (SGTSL043-QT-OCR-TAB)
                                       TO  SGTSL043-ULT-TS-EST-RET
           END-IF
           .
       220000-SAI.
           EXIT.
      
      *-----------------------------------------------------------------
       221000-EXCLUIR-VERSAO-AGENCIA   SECTION.
      *-----------------------------------------------------------------
           MOVE  0007                  TO  MSG1-CD-SEQL-ERRO
      
           INITIALIZE                  SGTST043-DADOS
      
      *--> Movimentar os campos de entrada
           SET   SGTST043-EXCLUIR      TO  TRUE
           MOVE  SGTS0047-CD-AG-CLSC-RSCO
                                       TO  SGTST043-CD-AG-CLSC-RSCO
           MOVE  SGTS0047-NR-ETRD-AG-CLSC
                                       TO  SGTST043-NR-ETRD-AG-CLSC
           MOVE  SGTSL043-TS-EST-AG-CLSC               (SGTSL043-IC-TAB)
                                       TO  SGTST043-TS-EST-AG-CLSC
           MOVE  KOPRD-CD-OPRD         TO  SGTST043-CD-USU-ATL-EST
      
           PERFORM  930000-EXECUTAR-SGTST043
           .
       221000-SAI.
           EXIT.
      
      *-----------------------------------------------------------------
       230000-EXCLUIR-AGENCIA          SECTION.
      *-----------------------------------------------------------------
           MOVE  0008                  TO  MSG1-CD-SEQL-ERRO
      
           INITIALIZE                  SGTST042-DADOS
      
      *--> Movimentar os campos de entrada
           SET   SGTST042-EXCLUIR      TO  TRUE
           MOVE  SGTS0047-CD-AG-CLSC-RSCO
                                       TO  SGTST042-CD-AG-CLSC-RSCO
           MOVE  SGTS0047-NR-ETRD-AG-CLSC
                                       TO  SGTST042-NR-ETRD-AG-CLSC
           MOVE  KOPRD-CD-OPRD         TO  SGTST042-CD-USU-RSP-ATL
      
           PERFORM  940000-EXECUTAR-SGTST042
           .
       230000-SAI.
           EXIT.
      
      *-----------------------------------------------------------------
       240000-MOVIMENTAR-SAIDA         SECTION.
      *-----------------------------------------------------------------
           MOVE  0009                  TO  MSG1-CD-SEQL-ERRO
      
           MOVE  'Agência de Classificação Externa de Risco excluída com
      -          ' sucesso.'           TO  SGTS0047-TX-RTN-PRCT
      
      *--> Gravando o container de resposta da subrotina
      
           PERFORM  980000-GRAVAR-CONTAINER-RSPT
           .
       240000-SAI.
           EXIT.
      
      *-----------------------------------------------------------------
       900000-EXECUTAR-MODULO          SECTION.
      *--> Executa um modulo implementado com Container
      *-----------------------------------------------------------------
           MOVE  0010                  TO  MSG1-CD-SEQL-ERRO
      
      *--> Grava o container de requisicao do modulo a executar
           PERFORM  982000-GRAVAR-CONTAINER-RQSC
      
      *--> Limpa a resposta e erro antes de executar o modulo
           PERFORM  983000-EXCLUIR-RESPOSTA-ERRO
      
      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EVALUATE  NM-PGM
               WHEN  SGTSP006
                     EXEC CICS LINK
                               PROGRAM ( SGTSP006                      )
                               CHANNEL ( NM-CNL                        )
                               RESP    ( MSG1-RESP1                    )
                               RESP2   ( MSG1-RESP2                    )
                     END-EXEC
               WHEN  OTHER
                     SET   ERRO-PROGRAMADO   TO  TRUE
                     MOVE  9006              TO  MSG1-CD-ERRO-TS
                     STRING
                     'Não definida a execução do módulo.' NM-PGM
                     DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
                     PERFORM  999000-ERRO
           END-EVALUATE
      
           EVALUATE  TRUE
               WHEN  MSG1-RESP1        EQUAL DFHRESP(NORMAL)
                     CONTINUE
               WHEN  OTHER
                     STRING
                     'Erro na execução do módulo.' NM-PGM ', '
                     'RESP  ='   ' ' MSG1-RESP1  ', '
                     'RESP2 =' ' ' MSG1-RESP2
                     DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
                     MOVE  3004        TO  MSG1-CD-ERRO-TS
                     SET  ERRO-CICS    TO  TRUE
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
       910000-EXECUTAR-SGTST044        SECTION.
      *-----------------------------------------------------------------
           MOVE  0011                  TO  MSG1-CD-SEQL-ERRO
      
      *--> Movimento nome do módulo para monitoração de variáveis
           MOVE  SGTST044              TO  NM-PGM
      
           EXEC CICS  LINK
                      PROGRAM  ( SGTST044       )
                      COMMAREA ( SGTST044-DADOS )
           END-EXEC
      
           IF  SGTST044-CD-RTN-PGM         NOT EQUAL ZEROS
               SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE  SGTST044-CD-RTN-PGM
                                       TO  MSG1-CD-ERRO-TS
               MOVE  SGTST044-MSG-ERRO-PGM
                                       TO  MSG1-TX-RTN-PGM-ACND
                                           MSG1-TX-ERRO-TS
               MOVE  NM-PGM            TO  MSG1-NM-PGM-ACND
               MOVE  SGTST044-CD-RTN-PGM
                                       TO  MSG1-CD-RTN-PGM-ACND
      *------> Monitorar variáveis do módulo chamado
               PERFORM  969000-MONITORAR-VARIAVEIS
               PERFORM  999000-ERRO
           END-IF
           .
       910000-SAI.
           EXIT.
      
      *-----------------------------------------------------------------
       920000-EXECUTAR-SGTSL043        SECTION.
      *-----------------------------------------------------------------
           MOVE  0012                  TO  MSG1-CD-SEQL-ERRO
      
      *--> Movimento nome do módulo para monitoração de variáveis
           MOVE  SGTSL043              TO  NM-PGM
      
           EXEC CICS  LINK
                      PROGRAM  (SGTSL043         )
                      COMMAREA ( SGTSL043-DADOS  )
           END-EXEC
      
           IF  SGTSL043-CD-RTN-PGM         NOT EQUAL ZEROS
               SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE  SGTSL043-CD-RTN-PGM
                                       TO  MSG1-CD-ERRO-TS
               MOVE  SGTSL043-MSG-ERRO-PGM
                                       TO  MSG1-TX-RTN-PGM-ACND
                                           MSG1-TX-ERRO-TS
               MOVE  NM-PGM            TO  MSG1-NM-PGM-ACND
               MOVE  SGTSL043-CD-RTN-PGM
                                       TO  MSG1-CD-RTN-PGM-ACND
      *-->     Monitorar variáveis do módulo chamado
               PERFORM  969000-MONITORAR-VARIAVEIS
               PERFORM  999000-ERRO
           END-IF
           .
       920000-SAI.
           EXIT.
      
      *-----------------------------------------------------------------
       930000-EXECUTAR-SGTST043        SECTION.
      *-----------------------------------------------------------------
           MOVE  0013                  TO  MSG1-CD-SEQL-ERRO
      
      *--> Movimento nome do módulo para monitoração de variáveis
           MOVE  SGTST043              TO  NM-PGM
      
           EXEC CICS  LINK
                      PROGRAM  ( SGTST043       )
                      COMMAREA ( SGTST043-DADOS )
           END-EXEC
      
           IF  SGTST043-CD-RTN-PGM         NOT EQUAL ZEROS
               SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE  SGTST043-CD-RTN-PGM
                                       TO  MSG1-CD-ERRO-TS
               MOVE  SGTST043-MSG-ERRO-PGM
                                       TO  MSG1-TX-RTN-PGM-ACND
                                           MSG1-TX-ERRO-TS
               MOVE  NM-PGM            TO  MSG1-NM-PGM-ACND
               MOVE  SGTST043-CD-RTN-PGM
                                       TO  MSG1-CD-RTN-PGM-ACND
      *-->     Monitorar variáveis do módulo chamado
               PERFORM  969000-MONITORAR-VARIAVEIS
               PERFORM  999000-ERRO
           END-IF
           .
       930000-SAI.
           EXIT.
      
      *-----------------------------------------------------------------
       940000-EXECUTAR-SGTST042        SECTION.
      *-----------------------------------------------------------------
           MOVE  0014                  TO  MSG1-CD-SEQL-ERRO
      
      *--> Movimento nome do módulo para monitoração de variáveis
           MOVE  SGTST042              TO  NM-PGM
      
           EXEC CICS  LINK
                      PROGRAM  ( SGTST042         )
                      COMMAREA ( SGTST042-DADOS   )
           END-EXEC
      
           IF  SGTST042-CD-RTN-PGM         NOT EQUAL ZEROS
               SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE  SGTST042-CD-RTN-PGM
                                       TO  MSG1-CD-ERRO-TS
               MOVE  SGTST042-MSG-ERRO-PGM
                                       TO  MSG1-TX-RTN-PGM-ACND
                                           MSG1-TX-ERRO-TS
               MOVE  NM-PGM            TO  MSG1-NM-PGM-ACND
               MOVE  SGTST042-CD-RTN-PGM
                                       TO  MSG1-CD-RTN-PGM-ACND
      *-->     Monitorar variáveis do módulo chamado
               PERFORM  969000-MONITORAR-VARIAVEIS
               PERFORM  999000-ERRO
           END-IF
           .
       940000-SAI.
           EXIT.
      
      *-----------------------------------------------------------------
       969000-MONITORAR-VARIAVEIS      SECTION.
      *--> Monitora as variáveis dos módulos chamados
      *-----------------------------------------------------------------
           MOVE  0015                  TO  MSG1-CD-SEQL-ERRO
      
           EVALUATE  NM-PGM
               WHEN  SGTSL043
                     MOVE  'Cd Agencia Classificacao Risco'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL043-CD-AG-CLSC-RSCO
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT
                     MOVE  'Nr Estruturado Agencia'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL043-NR-ETRD-AG-CLSC
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT
                     MOVE  'Ultima TS Estado Retornado'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL043-ULT-TS-EST-RET
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT
      
               WHEN  SGTSP006
                     MOVE  'Cd Agencia Classificacao Risco'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSP006-CD-AG-CLSC-RSCO-E
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT
                     MOVE  'Nr Estruturado Agencia'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSP006-NR-ETRD-AG-CLSC-E
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT
                     MOVE  'Nr Sequencial Nota Agencia'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSP006-NR-SEQL-NOTA-AG-E
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT
      
               WHEN  SGTST042
                     MOVE  'Acao'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST042-ACAO
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT
                     MOVE  'Cd Agencia Classificacao Risco'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST042-CD-AG-CLSC-RSCO
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT
                     MOVE  'Nr Estruturado Agencia'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST042-NR-ETRD-AG-CLSC
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT
      
               WHEN  SGTST043
                     MOVE  'Acao'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST043-ACAO
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT
                     MOVE  'Cd Agencia Classificacao Risco'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST043-CD-AG-CLSC-RSCO
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT
                     MOVE  'Nr Estruturado Agencia'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST043-NR-ETRD-AG-CLSC
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT
                     MOVE  'Timestamp Estado Agencia'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST043-TS-EST-AG-CLSC
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT
      
               WHEN  SGTST044
                     MOVE  'Acao'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST044-ACAO
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT
                     MOVE  'Cd Agencia Classificacao Risco'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST044-CD-AG-CLSC-RSCO
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT
                     MOVE  'Nr Estruturado Agencia'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST044-NR-ETRD-AG-CLSC
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT
                     MOVE  'Nr Sequencial Nota Agencia'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST044-NR-SEQL-NOTA-AG
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
      *--> Obtem o canal corrente
      *-----------------------------------------------------------------
           MOVE  0016                  TO  MSG1-CD-SEQL-ERRO
      
      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CANAL            TO  TRUE
      
           EXEC CICS  ASSIGN
                      CHANNEL ( NM-CNL                                 )
           END-EXEC
      
      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       970000-SAI.
           EXIT.
      
      *-----------------------------------------------------------------
       971000-LER-REQUISICAO           SECTION.
      *-----------------------------------------------------------------
           MOVE  0017                  TO  MSG1-CD-SEQL-ERRO
      
      *--> Monta o nome do container de requisicao
           MOVE  'SGTS0047'            TO  NM-PGM
           MOVE  'SGTS0047-RQSC'       TO  NM-CTNR
      
      *--> Recupera o tamanho do container de requisição
           PERFORM  973100-LER-CONTAINER-NODATA
      
      *--> Obtem o tamanho esperado da requisicao
           MOVE  LENGTH OF SGTS0047-RQSC   TO  QT-CCT-EPRO
      
      *--> Valida tamanho esperado da requisicao X tamanho recebido
           IF  QT-CCT-LIDO                 NOT EQUAL  QT-CCT-EPRO
               SET   ERRO-TAMANHO-AREA
                                       TO  TRUE
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF
      
           MOVE  0018                  TO  MSG1-CD-SEQL-ERRO
      
      *--> Configura dados para leitura do container de requisição
           SET   GET-CONTAINER         TO  TRUE
      
      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE
      
      *--> Efetua a leitura do container
           EXEC CICS  GET
                      CONTAINER ( NM-CTNR                              )
                      FLENGTH   ( QT-CCT-LIDO                          )
                      INTO      ( SGTS0047-RQSC                        )
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
      
      *--> Le o container de erro dos modulos chamados
           SET   GET-CONTAINER         TO  TRUE
           MOVE  'PRM-ERRO'            TO  NM-CTNR
           INITIALIZE IIBKERRO-PRM-ERRO
      
      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS  GET
                      CONTAINER ( NM-CTNR                              )
                      INTO      ( IIBKERRO-PRM-ERRO                    )
                      FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO          )
                      RESP      ( MSG1-RESP1                           )
                      RESP2     ( MSG1-RESP2                           )
           END-EXEC
      
      *--> Caso deseje personalizar ou não interromper o
      *    processamento, alterar para não dar erro aqui SOMENTE
      *    para o Modulo Chamado e tratar na section chamadora
           EVALUATE  TRUE
               WHEN  MSG1-RESP1  EQUAL  DFHRESP(CONTAINERERR)
                     CONTINUE
               WHEN  MSG1-RESP1  EQUAL  DFHRESP(NORMAL)
      *--> Trata o erro de programa acionado por ser módulo interno
                SET ERRO-PROGRAMA-ACIONADO   TO  TRUE
                MOVE NM-PGM                  TO  MSG1-NM-PGM-ACND
                IF NM-PGM(1:3) EQUAL 'SGT'
                    MOVE IIBKERRO-CD-ERRO    TO  MSG1-CD-RTN-PGM-ACND
                                                 MSG1-CD-ERRO-TS
                    MOVE IIBKERRO-TX-ERRO    TO  MSG1-TX-RTN-PGM-ACND
                                                 MSG1-TX-ERRO-TS
                ELSE
                    MOVE  3004              TO  MSG1-CD-ERRO-TS
                    STRING  'Erro ao acionar o módulo ' NM-PGM      '.'
                            DELIMITED BY SIZE
                            INTO        MSG1-TX-ERRO-TS
                    END-STRING
                    MOVE IIBKERRO-CD-ERRO   TO  MSG1-CD-RTN-PGM-ACND
                    MOVE IIBKERRO-TX-ERRO   TO  MSG1-TX-RTN-PGM-ACND
                END-IF
      *-->          Monitorar variáveis do módulo chamado
                    PERFORM 969000-MONITORAR-VARIAVEIS
                    PERFORM 999000-ERRO
               WHEN  OTHER
                     SET ERRO-CONTAINER     TO  TRUE
                     PERFORM  999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       972000-SAI.
           EXIT.
      
      *-----------------------------------------------------------------
       973000-LER-RESPOSTA             SECTION.
      *--> Recupera o container com os dados de resposta (saída).
      *    Modulo Container  -     NOME_PGM-RPST
      *    Modulo IIB (FIXO) -     PRM-DADOS-RPST
      *----------------------------------------------------------------
           MOVE  0020                  TO  MSG1-CD-SEQL-ERRO
      
      *--> Monta o nome dos container de acordo com modulo a executar
           MOVE  SPACES                TO  NM-CTNR
      
      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RPST,
      *    senão nome_modulo -RPST
      
           MOVE  NM-PGM                TO  NM-CTNR(1:8)
           MOVE  '-RPST'               TO  NM-CTNR(9:5)
      
      *--> Recupera o tamanho do container de resposta
           PERFORM  973100-LER-CONTAINER-NODATA
      
      *--> Configura dados para leitura do container de resposta
           SET   GET-CONTAINER         TO  TRUE
      
      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE
      
      
      *--> Le a RESPOSTA de acordo com o programa que a gravou
           MOVE  0021                  TO  MSG1-CD-SEQL-ERRO
      
      *--> Calcular a quantidade de caracteres esperado e
      *    limpar a area de dados antes de executar o GET
      *    Para matrizes com dados variaveis (clausula DEPENDING ON)
      *    nao ha como utilizar o comando INITIALIZE
           EVALUATE  NM-PGM
               WHEN  SGTSP006
                     MOVE  LENGTH OF SGTSP006-RPST  TO  QT-CCT-EPRO
                     EXEC CICS  GET
                                CONTAINER ( NM-CTNR                    )
                                INTO      ( SGTSP006-RPST              )
                                FLENGTH   ( QT-CCT-LIDO                )
                     END-EXEC
               WHEN  OTHER
                     SET   ERRO-PROGRAMADO
                                       TO  TRUE
                     MOVE  9003        TO  MSG1-CD-ERRO-TS
                     STRING  'Erro na leitura da resposta do módulo .'
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
           MOVE  0022                  TO  MSG1-CD-SEQL-ERRO
      
      *--> Configura dados para leitura do container de requisição
           SET   GET-CONTAINER         TO  TRUE
      
      *--> Por causa da captura automática de erro CICS, deve ser
      *    o erro de container
           SET   ERRO-CONTAINER        TO  TRUE
      
      *--> Obtem a quantidade de caracteres no container
           MOVE  ZEROS                 TO  QT-CCT-LIDO
           EXEC CICS  GET
                      CONTAINER ( NM-CTNR                              )
                      NODATA
                      FLENGTH   ( QT-CCT-LIDO                          )
           END-EXEC
      
      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       973100-SAI.
           EXIT.
      
      *-----------------------------------------------------------------
      *    Section da Faixa de 980000 a 989900 para Gravacao de Dados
      *-----------------------------------------------------------------
      
      *-----------------------------------------------------------------
       980000-GRAVAR-CONTAINER-RSPT    SECTION.
      *-----------------------------------------------------------------
           MOVE  0023                  TO  MSG1-CD-SEQL-ERRO
      
      *--> Exclui eventual container de erro de outros programas
           PERFORM  983100-EXCLUIR-CONTAINER-ERRO
      
      *--> Monta o nome do container de resposta
           MOVE  'SGTS0047-RPST'       TO  NM-CTNR
           SET   PUT-CONTAINER         TO  TRUE
      
      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE
      
           EXEC CICS  PUT
                      CONTAINER ( NM-CTNR                              )
                      FROM      ( SGTS0047-RPST                        )
                      FLENGTH   ( LENGTH OF SGTS0047-RPST              )
           END-EXEC
      
      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       980000-SAI.
           EXIT.
      
      *-----------------------------------------------------------------
       982000-GRAVAR-CONTAINER-RQSC    SECTION.
      *    Postar container com dados de requisição (entrada)
      *    Modulo Container  -     NOME_PGM-RQSC
      *    Modulo IIB (FIXO) -     PRM-DADOS-RQSC
      *-----------------------------------------------------------------
           MOVE  0024                  TO  MSG1-CD-SEQL-ERRO
      
      *--> Monta o nome dos container de acordo com modulo a executar
           MOVE  SPACES                TO  NM-CTNR
           SET   PUT-CONTAINER         TO  TRUE
      
      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RPST,
      *    senão nome_modulo -RPST
           MOVE  NM-PGM                TO  NM-CTNR(1:8)
           MOVE  '-RQSC'               TO  NM-CTNR(9:5)
      
      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE
      
      *--> Faz put da requisicao do programa a executar
           EVALUATE NM-PGM
               WHEN  SGTSP006
                     EXEC CICS  PUT
                                CONTAINER ( NM-CTNR                    )
                                FROM      ( SGTSP006-RQSC              )
                                FLENGTH   ( LENGTH OF SGTSP006-RQSC    )
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
      *    problemas em modulos executados mais de uma vez
      *    dentro da mesma sessao do CICS.
      *    Equivale ao INITIALIZE na area de resposta dos programas
      *    que utilizam COMMAREA.
      *-----------------------------------------------------------------
           MOVE  0025                  TO  MSG1-CD-SEQL-ERRO
      
      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RPST,
      *    senão nome_modulo -RPST
      
           MOVE  NM-PGM                TO  NM-CTNR(1:8)
           MOVE  '-RPST'               TO  NM-CTNR(9:5)
      
           SET DEL-CONTAINER           TO TRUE
      
      *--> Exclui o container de resposta
      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS  DELETE
                      CONTAINER ( NM-CTNR                              )
                      RESP      ( MSG1-RESP1                           )
                      RESP2     ( MSG1-RESP2                           )
           END-EXEC
      
           EVALUATE  TRUE
               WHEN  MSG1-RESP1  EQUAL  DFHRESP(CONTAINERERR)
               WHEN  MSG1-RESP1  EQUAL  DFHRESP(NORMAL)
                     CONTINUE
               WHEN  OTHER
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
           MOVE  0026                  TO  MSG1-CD-SEQL-ERRO
      
      *--> Limpar variaveis container de erro
           INITIALIZE  IIBKERRO-PRM-ERRO
      
      *--> Exclui o container de erro
           MOVE  'PRM-ERRO'            TO  NM-CTNR
           SET   DEL-CONTAINER         TO  TRUE
      
      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS  DELETE
                      CONTAINER ( NM-CTNR                              )
                      RESP      ( MSG1-RESP1                           )
                      RESP2     ( MSG1-RESP2                           )
           END-EXEC
      
           EVALUATE  TRUE
               WHEN  MSG1-RESP1  EQUAL  DFHRESP(CONTAINERERR)
               WHEN  MSG1-RESP1  EQUAL  DFHRESP(NORMAL)
                     CONTINUE
               WHEN  OTHER
                     SET   ERRO-CONTAINER
                                       TO  TRUE
                     PERFORM  999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       983100-SAI.
           EXIT.
      
      *----------------------------------------------------------------*
       980103-LER-DADOS-OPRD           SECTION.
      *----------------------------------------------------------------*
           MOVE  0027                  TO  MSG1-CD-SEQL-ERRO
      
      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO  TRUE
           MOVE 'PRM-INF-OPRD'         TO  NM-CTNR
      
      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO  TRUE
      
           INITIALIZE PRM-INF-OPRD
           EXEC CICS GET
                CONTAINER ( NM-CTNR                )
                INTO      ( PRM-INF-OPRD           )
                FLENGTH   ( LENGTH OF PRM-INF-OPRD )
           END-EXEC.
      
      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO  TRUE
           .
       980103-SAI.
           EXIT.
      
      *-----------------------------------------------------------------
       984000-GRAVAR-CONTAINER-ERRO    SECTION.
      *-----------------------------------------------------------------
           MOVE  0028                  TO  MSG1-CD-SEQL-ERRO
      
      *--> Grava o erro gerado no container de erro
      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS  PUT
                      CONTAINER ( 'PRM-ERRO'                           )
                      FROM      ( IIBKERRO-PRM-ERRO                    )
                      FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO          )
                      RESP      ( MSG1-RESP1                           )
                      RESP2     ( MSG1-RESP2                           )
           END-EXEC
           .
       984000-SAI.
           EXIT.
      
      *-----------------------------------------------------------------
       990000-ENCERRAR                 SECTION.
      *-----------------------------------------------------------------
      
           EXEC CICS
                RETURN
           END-EXEC
           .
       990000-ENCERRAR-SAI.
           EXIT.
      
      *-----------------------------------------------------------------
      *--> As sections abaixo iniciadas com 999000 representam os
      *    tratamentos de erro utilizando o Sistema MSG (MSGS0001)
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
      
      *--> Em caso de erro, desfaz as atualizacoes em banco de dados
           EXEC CICS  SYNCPOINT
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
      
      *--> Adiciona as variáveis de entrada para monitoração
           IF  MNT-VRV
               MOVE  'Cd Agencia Classificacao Risco'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTS0047-CD-AG-CLSC-RSCO
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT
               MOVE  'Nr Estruturado Agencia'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTS0047-NR-ETRD-AG-CLSC
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT
           END-IF
      
      *--> Grava o erro gerado no sistema MSG - Mensageria
           PERFORM  MSG1-NTF-MSG
      
      *--> Movimenta Mensagem e Código de Erro para o Container de Erro
           MOVE  MSG1-CD-ERRO-TS       TO  IIBKERRO-CD-ERRO
           MOVE  MSG1-TX-ERRO-TS       TO  IIBKERRO-TX-ERRO
      
      *--> Grava o erro gerado no container de erro
           PERFORM  984000-GRAVAR-CONTAINER-ERRO
      
      *--> Encerra a transacao
           PERFORM  990000-ENCERRAR
           .
       999000-SAI.
           EXIT.
      * ---------------------------------------
      * 999001-TRATA-ABEND-CICS        SECTION.
      * 999002-TRATA-CONDITION-CICS    SECTION.
      * 999003-ADICIONA-VRV-MNT        SECTION.
      * ---------------------------------------
       COPY HLPKP999.                                                           0001
      * ---------------------------------------
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *----------------------------------------
       COPY SGTKP999.                                                           0001
      * ---------------------------------------
      * MSG1-NTF-MSG                   SECTION.
      * ---------------------------------------
       COPY MSGKPMG1.                                                           0001
      *---------------------- FIM SGTS0047 -----------------------------
