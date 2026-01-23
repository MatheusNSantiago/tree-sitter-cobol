000010 CBL ARITH(EXTEND)
001920*-----------------------------------------------------------------
001920* Programa..: SGTS0276
001920* Analista..: F8785286 - Roseli Akemi Kawahata
001920* Autor.....: F8785286 - Roseli Akemi Kawahata
001920* Data......: 30/09/2019
001920* Objetivo..: Listar Tipo de Cliente Comprador.
001920* Compilacao: Cobol/CICS
001920*-----------------------------------------------------------------
001920* VRS001 30/09/2019 - F8785286 - DE 2016/145195 - Implantação
001920*-----------------------------------------------------------------
001920 IDENTIFICATION                  DIVISION.
001920*-----------------------------------------------------------------
001920 PROGRAM-ID.                     SGTS0276.
001920*-----------------------------------------------------------------
001920 ENVIRONMENT                     DIVISION.
001920*-----------------------------------------------------------------
001920 CONFIGURATION                   SECTION.
001920*-----------------------------------------------------------------
001920 SPECIAL-NAMES.
001920     DECIMAL-POINT IS COMMA.
001920*-----------------------------------------------------------------
001920 DATA                            DIVISION.
001920*-----------------------------------------------------------------
001920
001920*-----------------------------------------------------------------
001920 WORKING-STORAGE                 SECTION.
001920*-----------------------------------------------------------------
001920 77  CTE-INICIO-SS               PIC  X(40) VALUE
001920                                 '*** S.S. Comeca Aqui ***'.
001920 77  CTE-PROG                    PIC  X(18) VALUE
001920                                 '*** SGTS0276 ***'.
001920 77  CTE-VERS                    PIC  X(06) VALUE 'VRS001'.
001920
001920*--> Definição dos Nomes dos Programas Acionados
001920 77  IIBP2000                    PIC  X(08) VALUE 'IIBP2000'.
001920
001920 77  SBDATAF                     PIC  X(08) VALUE 'SBDATAF'.
001920
001920*-----------------------------------------------------------------
001920 LOCAL-STORAGE                   SECTION.
001920*-----------------------------------------------------------------
001920
001920*--> Area de trabalho
001920
001920
001920*--> Auxiliares para leitura de tabela
001920*
001920 01  DADOS-ATL.
000490     03 CD-TIP-CLI-CPRD          PIC S9(04) COMP-5
000490                                 OCCURS 0101 TIMES  VALUE ZEROES.
000490     03 TX-DCR-TIP-CLI           PIC  X(60)
000490                                 OCCURS 0101 TIMES  VALUE SPACES.
001920
001920*--> Indicadores de nulidades
001920
001920* Variavel(is) de controle do cursor(es)
001920
001920 01  CTL-CURSOR-PSQ.
001920     03  CD-FIM-CURSOR-PSQ            PIC S9(04) COMP-5 VALUE 0.
001920         88  FIM-CURSOR-PSQ            VALUE +100.
001920     03  IN-CURSOR-PSQ-ABTO           PIC  X(01) VALUE 'N'.
001920         88  CURSOR-PSQ-ABTO          VALUE 'S'.
001920
001920 01 QT-REG-SELD                         PIC S9(4) COMP-5 VALUE 0.
001920*----------------------------------------------------------------*
001920*    Books Utilizados
001920*-----------------------------------------------------------------
001920* Book do programa de ambiente - CICS5000
001920* Definido na work para preservar as informacoes
-INC HLPKE01B
002180
001920*--> Book dos dados do usuário
-INC IIBKOPRD
002180
001380* Book do container de Serviços (para acionamento de serviços)
-INC IIBKSRVC
001240*
001920*--> Book do Container de Erro
-INC IIBKERRO
002180
001920*--> Book para captura de abend cics da bloco codigo HLPKW999
-INC HLPKW999
002180
001920*--> Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1
002180
001920*--> Book para captura de erro via MSG1
-INC MSGKWMG1
002180
001920*--> Lista Posição Diária de Saldo - DB2SGT.PSC_DRIA_SDO_OPR
001920*-->       SGTSL004
-INC SGTKL004
002180
001920*--> Book do container de requisição e resposta deste modulo
001920 01 SGTS0276-DADOS.
-INC SGTK0276
002180
001920*-----------------------------------------------------------------
001920* Book dos Containers e das subrotinas chamadas
001920*-----------------------------------------------------------------
000450
001920*--> Area de trabalho SQL/ DB2.
001920
001920*-----------------------------------------------------------------
001920* Listar Ordens de Venda
001920*-----------------------------------------------------------------
002430     EXEC SQL
002430 	DECLARE  CURSOR-PSQ-01     CURSOR
002430               WITH ROWSET POSITIONING FOR
002430 	
002430 	    SELECT
001920             A.CD_TIP_CLI_CPRD
001920           , A.TX_DCR_TIP_CLI
001920         FROM DB2SGT.TIP_CLI_CPRD A
001920         WHERE A.CD_TIP_CLI_CPRD  > 0
002430 	
002430     	ORDER BY
002430       	    A.CD_TIP_CLI_CPRD
002430     END-EXEC
002430
001920     EXEC SQL
001920          INCLUDE SQLCA
001920     END-EXEC.
002180
001920 77  CTE-FINAL-SS                PIC X(40) VALUE
001920                                 '*** S.S. Termina Aqui ***'.
001920*-----------------------------------------------------------------
001920 PROCEDURE                       DIVISION.
001920*-----------------------------------------------------------------
001920 000000-ROTINA-PRINCIPAL         SECTION.
001920*-----------------------------------------------------------------
002180
001920*--> Habilita tratamento automatico de Erros DB2
-INC HLPKE997
001920*--> Habilita captura automatica de Abend e Erros CICS
-INC HLPKE999
002180
001920     PERFORM  100000-INICIAR
002180
001920     PERFORM  200000-PROCESSAR
002180
001920     PERFORM  300000-POSTA-RESPOSTA
002180
001920     PERFORM  999999-ENCERRAR
002180
001920     GOBACK
001920     .
001920 000000-SAI.
001920     EXIT.
001920*-----------------------------------------------------------------
001920 100000-INICIAR                  SECTION.
001920*-----------------------------------------------------------------
001920     MOVE  0001                  TO  MSG1-CD-SEQL-ERRO
002180
001920     PERFORM  970000-LER-CANAL
002180
001920*--> Define o nome do programa p/ fins manipulacao container
001920     MOVE  'SGTS0276'            TO  NM-PGM
002180
001920*--> Limpa a resposta e erro antes de executar o modulo
001920     PERFORM  983000-EXCLUIR-RESPOSTA-ERRO
001920     .
001920 100000-SAI.
001920     EXIT.
001920*-----------------------------------------------------------------
001920 200000-PROCESSAR                SECTION.
001920*-----------------------------------------------------------------
001920     MOVE  0002                  TO  MSG1-CD-SEQL-ERRO
002180
001920     PERFORM 970101-ABRE-CURSOR-PSQ
001920     PERFORM 970102-LER-CURSOR-PSQ
001920     PERFORM 970103-FECHA-CURSOR-PSQ
001920     PERFORM 210000-PROCESSA-PESQUISA
001920              VARYING SGTS0276-IC-TAB FROM 1 BY 1
001920              UNTIL SGTS0276-IC-TAB > SGTS0276-QT-OCR-TAB
001920     .
001920 200000-SAI.
001920     EXIT.
001920*-----------------------------------------------------------------
001920 210000-PROCESSA-PESQUISA        SECTION.
001920*    Formata área de resposta
001920*-----------------------------------------------------------------
001920     MOVE  0003                  TO  MSG1-CD-SEQL-ERRO
002180
001920     MOVE CD-TIP-CLI-CPRD (SGTS0276-IC-TAB)
001920                 TO  SGTS0276-CD-TIP-CLI-CPRD(SGTS0276-IC-TAB)
001920     MOVE TX-DCR-TIP-CLI(SGTS0276-IC-TAB)
001920                 TO  SGTS0276-TX-DCR-TIP-CLI(SGTS0276-IC-TAB)
001920     .
001920 210000-SAI.
001920     EXIT.
015478*-----------------------------------------------------------------
015478 300000-POSTA-RESPOSTA           SECTION.
015478*-----------------------------------------------------------------
015478     MOVE  0004                  TO  MSG1-CD-SEQL-ERRO
015478
015478     EXEC CICS PUT
015478        CONTAINER ( 'SGTS0276-RPST'         )
015478        CHANNEL   ( NM-CNL                  )
015478        FLENGTH   ( LENGTH OF SGTS0276-RPST )
015478        FROM      ( SGTS0276-RPST           )
015478     END-EXEC
015478
001920* Ao chegar aqui, há certeza de que não houve erro algum
001920     SET  SEM-ERRO               TO TRUE
015478     .
015478 300000-SAI.
015478     EXIT.
001920*-----------------------------------------------------------------
001920*   Section da Faixa de 900000 a 969900 para Demais Rotinas
001920*-----------------------------------------------------------------
001920* Executa módulos implementados com CONTAINER                   *
001920*-----------------------------------------------------------------
001920*    Section da Faixa de 970000 a 979900 para Leitura de Dados
001920*-----------------------------------------------------------------
002180
001920*-----------------------------------------------------------------
001920 970000-LER-CANAL                SECTION.
001920*--> Obtem o canal corrente
001920*-----------------------------------------------------------------
001920     MOVE  0005                  TO  MSG1-CD-SEQL-ERRO
002180
001920*--> Por causa da captura automática de erro CICS, deve ser
001920*    previsto o erro de container
001920     SET   ERRO-CANAL            TO  TRUE
002180
001920     EXEC CICS ASSIGN
001920               CHANNEL ( NM-CNL )
001920     END-EXEC
002180
001920*--> Ao chegar aqui, há certeza de que não houve erro algum
001920     SET   SEM-ERRO              TO  TRUE
001920     .
001920 970000-SAI.
001920     EXIT.
001920*-----------------------------------------------------------------
001920 970101-ABRE-CURSOR-PSQ          SECTION.
001920*-----------------------------------------------------------------
001920     MOVE  0006                  TO  MSG1-CD-SEQL-ERRO
001920
001920     EXEC SQL
001920          OPEN CURSOR-PSQ-01
001920     END-EXEC
001920
001920     MOVE 'S'                    TO IN-CURSOR-PSQ-ABTO
001920     .
001920 970101-SAI.
001920     EXIT.
001920*-----------------------------------------------------------------
001920 970102-LER-CURSOR-PSQ           SECTION.
001920*-----------------------------------------------------------------
001920     MOVE  0007                  TO  MSG1-CD-SEQL-ERRO
001920
001920     EXEC SQL
001920          FETCH NEXT ROWSET
001920                CURSOR-PSQ-01 FOR 0101 ROWS
001920          INTO :CD-TIP-CLI-CPRD
001920             , :TX-DCR-TIP-CLI
001920     END-EXEC
001920
001920     MOVE SQLERRD(3)             TO QT-REG-SELD
001920
001920     IF  QT-REG-SELD EQUAL 0101
001920         SET SGTS0276-TEM-MAIS-REGISTRO TO TRUE
001920         MOVE 0100                TO QT-REG-SELD
001920     END-IF
001920
001920     MOVE QT-REG-SELD            TO SGTS0276-QT-OCR-TAB
001920     MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ
001920     .
001920 970102-SAI.
001920     EXIT.
001920*-----------------------------------------------------------------
001920 970103-FECHA-CURSOR-PSQ         SECTION.
001920*-----------------------------------------------------------------
001920     MOVE  0008                  TO  MSG1-CD-SEQL-ERRO
001920
001920     MOVE 'N'                    TO IN-CURSOR-PSQ-ABTO
001920
001920     EXEC SQL
001920          CLOSE CURSOR-PSQ-01
001920     END-EXEC
001920     .
001920 970103-SAI.
001920     EXIT.
001920*-----------------------------------------------------------------
001920 973100-LER-CONTAINER-NODATA     SECTION.
001920*-----------------------------------------------------------------
001920     MOVE  0009                  TO  MSG1-CD-SEQL-ERRO
002180
001920*--> Configura dados para leitura do container de requisição
001920     SET   GET-CONTAINER         TO  TRUE
002180
001920*--> Por causa da captura automática de erro CICS, deve ser
001920*    previsto o erro de container
001920     SET   ERRO-CONTAINER        TO  TRUE
002180
001920*--> Obtem a quantidade de caracteres no container
001920     MOVE  ZEROS                 TO  QT-CCT-LIDO
002180
001920     EXEC CICS  GET
001920                CONTAINER ( NM-CTNR     )
001920                NODATA
001920                FLENGTH   ( QT-CCT-LIDO )
001920     END-EXEC
002180
001920*--> Ao chegar aqui, há certeza de que não houve erro algum
001920     SET   SEM-ERRO              TO  TRUE
001920     .
001920 973100-SAI.
001920     EXIT.
001920*-----------------------------------------------------------------
001920 983000-EXCLUIR-RESPOSTA-ERRO    SECTION.
001920*--> Exclui o container de resposta e de erro para evitar
001920*    problemas em módulos executados mais de uma vez dentro
001920*    da mesma sessão do CICS.
001920*    Equivale ao initialize na área de resposta dos
001920*    programas que utilizam COMMAREA.
001920*-----------------------------------------------------------------
001920     MOVE  0011                  TO  MSG1-CD-SEQL-ERRO
002180
001920*--> Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
001920*    nome_modulo -RPST
001920     MOVE  NM-PGM                TO  NM-CTNR(1:8)
001920     MOVE  '-RPST'               TO  NM-CTNR(9:5)
002180
001920     SET   DEL-CONTAINER         TO  TRUE
002180
001920*--> Exclui o container de resposta
001920*    Não há captura automática de erro CICS por usar RESP e RESP2
001920     EXEC CICS  DELETE
001920                CONTAINER ( NM-CTNR    )
001920                RESP      ( MSG1-RESP1 )
001920                RESP2     ( MSG1-RESP2 )
001920     END-EXEC
002180
001920     EVALUATE  TRUE
001920         WHEN  MSG1-RESP1            EQUAL  DFHRESP(CONTAINERERR)
001920         WHEN  MSG1-RESP1            EQUAL  DFHRESP(NORMAL)
001920               CONTINUE
001920         WHEN  OTHER
001920               SET   ERRO-CONTAINER
001920                                 TO  TRUE
001920               PERFORM  999002-TRATA-CONDITION-CICS
001920     END-EVALUATE
002180
001920*--> Excluir o container de erro
001920     PERFORM  983100-EXCLUIR-CONTAINER-ERRO
001920     .
001920 983000-SAI.
001920     EXIT.
001920*-----------------------------------------------------------------
001920 983100-EXCLUIR-CONTAINER-ERRO   SECTION.
001920*-----------------------------------------------------------------
001920     MOVE  0011                  TO  MSG1-CD-SEQL-ERRO
002180
001920*--> Limpar variaveis container de erro
001920     INITIALIZE                  IIBKERRO-PRM-ERRO
002180
001920*--> Exclui o container de erro
001920     MOVE  'PRM-ERRO'            TO  NM-CTNR
001920     SET   DEL-CONTAINER         TO  TRUE
002180
001920*--> Não há captura automática de erro CICS por usar RESP e RESP2
001920     EXEC CICS  DELETE
001920                CONTAINER ( NM-CTNR    )
001920                RESP      ( MSG1-RESP1 )
001920                RESP2     ( MSG1-RESP2 )
001920     END-EXEC
002180
001920     EVALUATE  TRUE
001920         WHEN  MSG1-RESP1            EQUAL  DFHRESP(CONTAINERERR)
001920         WHEN  MSG1-RESP1            EQUAL  DFHRESP(NORMAL)
001920               CONTINUE
001920         WHEN  OTHER
001920               SET   ERRO-CONTAINER
001920                                 TO  TRUE
001920               PERFORM  999002-TRATA-CONDITION-CICS
001920     END-EVALUATE
001920     .
001920 983100-SAI.
001920     EXIT.
001920*-----------------------------------------------------------------
001920 984000-GRAVAR-CONTAINER-ERRO    SECTION.
001920*--> Grava o erro gerado no container de erro
001920*-----------------------------------------------------------------
002180
001920*--> Não há captura automática de erro CICS por usar RESP e RESP2
001920     EXEC CICS  PUT
001920                CONTAINER ( 'PRM-ERRO'                  )
001920                FROM      ( IIBKERRO-PRM-ERRO           )
001920                FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
001920                RESP      ( MSG1-RESP1                  )
001920                RESP2     ( MSG1-RESP2                  )
001920     END-EXEC
001920     .
001920 984000-SAI.
001920     EXIT.
001920*-----------------------------------------------------------------
001920*    As sections abaixo iniciadas com 999000 representam os
001920*    tratamentos de erro através do Sistema MSG (rotina MSGS0001)
001920*-----------------------------------------------------------------
002180
001920*-----------------------------------------------------------------
001920 999000-ERRO                     SECTION.
001920*-----------------------------------------------------------------
002180
001920     IF  MSG1-CD-ERRO-TS             EQUAL ZEROS
001920        MOVE  8888               TO  MSG1-CD-ERRO-TS
001920     END-IF
002180
001920     IF  MSG1-TX-ERRO-TS             EQUAL SPACES
001920         MOVE  'Erro não específicado no módulo.'
001920                                 TO  MSG1-TX-ERRO-TS
001920     END-IF
002180
001920     PERFORM  999000-GRAVA-ERRO-E-ENCERRA
001920     .
001920 999000-SAI.
001920     EXIT.
001920*-----------------------------------------------------------------
001920 999000-GRAVA-ERRO-E-ENCERRA     SECTION.
001920*-----------------------------------------------------------------
002180
001920*--> Em caso de erro, desfaz as atualizações em banco de dados
001920     EXEC CICS SYNCPOINT
001920               ROLLBACK
001920     END-EXEC
002180
001920*--> Verifica se o tipo de erro gerado possui tratamento interno
001920     EVALUATE  TRUE
001920         WHEN  ERRO-CONTAINER
001920               PERFORM  999004-TRATA-ERRO-CTNR
001920         WHEN  ERRO-TAMANHO-AREA
001920               PERFORM  999005-TRATA-ERRO-AREA-DADOS
001920         WHEN  ERRO-CANAL
001920               PERFORM  999006-TRATA-ERRO-CANAL
001920         WHEN  OTHER
001920               CONTINUE
001920     END-EVALUATE
002180
001920     MOVE  MSG1-CD-ERRO-TS               TO  IIBKERRO-CD-ERRO
001920     MOVE  MSG1-TX-ERRO-TS               TO  IIBKERRO-TX-ERRO
002180
001920*--> Grava o erro gerado no container de erro
001920     PERFORM  984000-GRAVAR-CONTAINER-ERRO
002180
001920*--> Encerra a transacao
001920     PERFORM  999999-ENCERRAR
001920     .
001920 999000-SAI.
001920     EXIT.
001920*---------------------------------------
001920* 999001-TRATA-ABEND-CICS        SECTION.
001920* 999002-TRATA-CONDITION-CICS    SECTION.
001920* 999003-ADICIONA-VRV-MNT        SECTION.
001920* 999004-ALINHA-NUMERO-ESQUERDA  SECTION.
001920*----------------------------------------
-INC HLPKP999
001920*----------------------------------------
001920* 999004-TRATA-ERRO-CTNR         SECTION.
001920* 999005-TRATA-ERRO-AREA-DADOS   SECTION.
001920* 999006-TRATA-ERRO-CANAL        SECTION.
001920*----------------------------------------
-INC SGTKP999
001920*----------------------------------------
001920* 999010-TRATA-ERRO-DB2          SECTION.
001920*----------------------------------------
-INC HLPKP997
001920*----------------------------------------
001920* MSG1-NTF-MSG                   SECTION.
001920* MSG1-PCE-INC                   SECTION.
001920* MSG1-PUT-CTNR-PRM              SECTION.
001920* MSG1-PUT-CNTRS-OPC             SECTION.
001920* MSG1-PUT-CTNR-PGM-ACND         SECTION.
001920* MSG1-PUT-CTNR-OPR-ACND         SECTION.
001920* MSG1-PUT-CTNR-SQL              SECTION.
001920* MSG1-PUT-CTNR-CICS             SECTION.
001920* MSG1-PUT-CTNR-VRV              SECTION.
001920* MSG1-EXEC-MSG1                 SECTION.
001920* MSG1-GET-CTNR-ERRO             SECTION.
001920*----------------------------------------
-INC MSGKPMG1
002180
001920*-----------------------------------------------------------------
001920 999999-ENCERRAR                 SECTION.
001920*-----------------------------------------------------------------
002180
001920     EXEC CICS
001920          RETURN
001920     END-EXEC
001920     .
001920 999999-SAI.
001920     EXIT.
001920*---------------------- FIM SGTS0276 -----------------------------