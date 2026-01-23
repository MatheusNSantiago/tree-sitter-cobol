      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSL12A
      * ANALISTA..: F9158937 - SILVIA AGUIAR DE OLIVEIRA
      * AUTOR.....: F9158937 - SILVIA AGUIAR DE OLIVEIRA
      * OBJETIVO..: CONSULTA DE LISTA DA TABELA DB2SGT.ACSS_CTRA_TVM
      * COMPILACAO: 54 - PSOSE600 - Cobol 6.3 c/otimizacao p/producao
      *-----------------------------------------------------------------
      * Listar carteiras disponíveis para UOR Posição e UOR Localização
      *-----------------------------------------------------------------
      * Obs.: Não estão contempladas rechamadas neste programa
      *-----------------------------------------------------------------
      * VRS001 04.08.2023 - F9158937 - IMPLANTACAO
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSL12A.

      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
      *-----------------------------------------------------------------
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       WORKING-STORAGE                 SECTION.
      *-----------------------------------------------------------------
       77  CTE-INICIO-WS               PIC X(30) VALUE
                                       '*** WSS COMECA AQUI ***'.
       77  CTE-PROG                    PIC X(18) VALUE
                                       '*** SGTSL12A ***'.
       77  CTE-VERS                    PIC X(06) VALUE 'VRS001'.

      *-----------------------------------------------------------------
      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informacoes
-INC HLPKE01B

       01  CTE-FIM-WS                  PIC  X(30)  VALUE
                                       '*** WSS TERMINA AQUI ***'.
      *-----------------------------------------------------------------
       LOCAL-STORAGE SECTION.
      *-----------------------------------------------------------------
       01  CTE-INICIO-LS               PIC X(30)  VALUE
                                       '*** LS COMECA AQUI   ***'.
      *--> Areas de trabalho
       01  IC-TAB                      PIC S9(04)     COMP-5 VALUE 0.
       01  QT-REG-SELD                 PIC S9(04)     COMP-5 VALUE 0.

      *--> Book do container de erro
-INC IIBKERRO
      *--> Book para captura de erro via MSG1
-INC MSGKWMG1
      *--> Variaveis para tratamento de Abend e Condition
-INC HLPKW999
      *--> Variaveis para tratamento de CONTAINER
-INC SGTKERR1

      *--> Container de erro
       01  CTNR-PRM-ERRO               PIC X(16) VALUE 'PRM-ERRO'.

      *--> Variaveis utilizados para leitura
       01  REG-WS.
           03 CD-CTRA-TVM              PIC S9(09)    COMP-5
              OCCURS 101 TIMES                       VALUE 0.

      *--> Variavel(is) de controle do(s) cursor(es)
       01  CTL-CURSOR-PSQ-1.
           03  CD-FIM-CURSOR-PSQ-1     PIC S9(04)     COMP-5 VALUE 0.
               88  FIM-CURSOR-PSQ-1                          VALUE +100.
           03  IN-CURSOR-PSQ-ABTO-1    PIC  X(01)            VALUE 'N'.
               88  CURSOR-PSQ-ABTO-1   VALUE 'S'.

       01  CTL-CURSOR-PSQ-2.
           03  CD-FIM-CURSOR-PSQ-2     PIC S9(04)     COMP-5 VALUE 0.
               88  FIM-CURSOR-PSQ-2                          VALUE +100.
           03  IN-CURSOR-PSQ-ABTO-2    PIC  X(01)            VALUE 'N'.
               88  CURSOR-PSQ-ABTO-2   VALUE 'S'.


      *--> Book do programa
       01  SGTSL12A-DADOS.
-INC SGTKL12A

      *--> Area de trabalho SQL/ DB2.
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

      *--> Declaração do cursor 1 para pesquisa
      *    Quando  SGTSL12A-CD-CTRA-TVM igual a zeros = Pesquisar todas
      *                                                 as carteiras
           EXEC SQL
             DECLARE  CURSOR-PSQ-1 CURSOR
                      WITH ROWSET POSITIONING FOR
              SELECT DISTINCT A.CD_CTRA_TVM
              FROM DB2SGT.ACSS_CTRA_TVM      A
               WHERE (A.CD_CTRA_TVM          > :SGTSL12A-CD-CTRA-TVM
                     AND  A.CD_UOR_ACSS_CTRA =
                                              :SGTSL12A-CD-UOR-LCZC-USU)
                OR   (A.CD_CTRA_TVM          > :SGTSL12A-CD-CTRA-TVM
                     AND  A.CD_UOR_ACSS_CTRA =
                                               :SGTSL12A-CD-UOR-PSC-USU)
                ORDER BY  A.CD_CTRA_TVM
           END-EXEC.


      *--> Declaração do cursor 2 para pesquisa
      *    Quando  SGTSL12A-CD-CTRA-TVM maior que zeros = Pesquisar
      *                                              Carteira específica
           EXEC SQL
             DECLARE  CURSOR-PSQ-2 CURSOR
                      WITH ROWSET POSITIONING FOR
              SELECT DISTINCT A.CD_CTRA_TVM
              FROM DB2SGT.ACSS_CTRA_TVM      A
               WHERE (A.CD_CTRA_TVM          = :SGTSL12A-CD-CTRA-TVM
                     AND  A.CD_UOR_ACSS_CTRA =
                                              :SGTSL12A-CD-UOR-LCZC-USU)
                OR   (A.CD_CTRA_TVM          = :SGTSL12A-CD-CTRA-TVM
                     AND  A.CD_UOR_ACSS_CTRA =
                                               :SGTSL12A-CD-UOR-PSC-USU)
                ORDER BY  A.CD_CTRA_TVM
           END-EXEC.

       01  CTE-FIM-LS                  PIC X(30)  VALUE
                                       '*** LS TERMINA AQUI  ***'.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------
           MOVE 0001                   TO MSG1-CD-SEQL-ERRO

      *--> Habilita tratamento automatico de Erros DB2
-INC HLPKE997
      *--> Habilita captura automatica de abend e erros CICS
-INC HLPKE999
           MOVE CTE-PROG(5:8)          TO NM-PGM

           PERFORM 100000-OBTER-NOME-DO-CANAL
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO
           PERFORM 970000-LER-REQUISICAO-RECEBIDA

           PERFORM 140000-VALIDAR-PARAMETROS

           PERFORM 200000-PROCESSA

           PERFORM 980000-GRAVAR-CTNR-RESPOSTA
           PERFORM 999999-ENCERRAR

           GOBACK
           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-OBTER-NOME-DO-CANAL      SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO

      *--> Por causa da captura automática de erro CICS, deve ser
      *--> previsto o erro de container
           SET  ERRO-CANAL             TO TRUE

           EXEC CICS ASSIGN
              CHANNEL ( NM-CNL  )

              END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       140000-VALIDAR-PARAMETROS       SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                   TO MSG1-CD-SEQL-ERRO

      *--> Validar Chave
           IF  SGTSL12A-CD-UOR-LCZC-USU   NOT GREATER ZEROS
               SET   ERRO-PROGRAMADO    TO  TRUE
               MOVE  604                TO  MSG1-CD-ERRO-TS
               MOVE  'Código UOR informado inválido.'
                                        TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL12A-CD-UOR-PSC-USU    NOT GREATER ZEROS
               SET   ERRO-PROGRAMADO    TO  TRUE
               MOVE  604                TO  MSG1-CD-ERRO-TS
               MOVE  'Código UOR informado inválido.'
                                        TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL12A-CD-CTRA-TVM     LESS  ZEROS
               SET   ERRO-PROGRAMADO    TO  TRUE
               MOVE 0001               TO MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' inválido.'      TO MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       140000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       200000-PROCESSA        SECTION.
      *-----------------------------------------------------------------
      *    Quando  SGTSL12A-CD-CTRA-TVM igual a zeros = Pesquisar todas
      *                                                 as carteiras
           IF  SGTSL12A-CD-CTRA-TVM   EQUAL  ZEROS

               MOVE 0004                   TO MSG1-CD-SEQL-ERRO
               PERFORM 971101-ABRE-CURSOR-PSQ-1
               PERFORM 971102-LER-CURSOR-PSQ-1

               PERFORM 300000-MOVE
                       VARYING IC-TAB FROM 1 BY 1
                          UNTIL IC-TAB > SGTSL12A-QT-OCR-TAB

               PERFORM 971103-FECHA-CURSOR-PSQ-1

           END-IF

      *    Quando  SGTSL12A-CD-CTRA-TVM maior que zeros = Pesquisar
      *                                              Carteira específica

           IF  SGTSL12A-CD-CTRA-TVM   GREATER  ZEROS

               MOVE 0005                   TO MSG1-CD-SEQL-ERRO
               PERFORM 972101-ABRE-CURSOR-PSQ-2
               PERFORM 972102-LER-CURSOR-PSQ-2

               PERFORM 300000-MOVE
                       VARYING IC-TAB FROM 1 BY 1
                          UNTIL IC-TAB > SGTSL12A-QT-OCR-TAB

               PERFORM 972103-FECHA-CURSOR-PSQ-2

           END-IF
           .
       200000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       300000-MOVE        SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

           MOVE CD-CTRA-TVM        (IC-TAB) TO
                                   SGTSL12A-CD-CTRA-TVM-S      (IC-TAB)
           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970000-LER-REQUISICAO-RECEBIDA  SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

           MOVE ZEROS                       TO QT-CCT-LIDO
           MOVE LENGTH OF SGTSL12A-RQSC     TO QT-CCT-EPRO

           EXEC CICS
                GET  CONTAINER ( 'SGTSL12A-RQSC' )
                     CHANNEL   ( NM-CNL          )
                     NODATA
                     FLENGTH   ( QT-CCT-LIDO     )
           END-EXEC

           IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
               SET  ERRO-TAMANHO-AREA  TO  TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS
                GET  CONTAINER ( 'SGTSL12A-RQSC'         )
                     CHANNEL   ( NM-CNL                  )
                     FLENGTH   ( LENGTH OF SGTSL12A-RQSC )
                     INTO      ( SGTSL12A-RQSC           )
           END-EXEC
           .
       970000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       971101-ABRE-CURSOR-PSQ-1          SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

           EXEC SQL
                OPEN CURSOR-PSQ-1
           END-EXEC
           MOVE 'S'                    TO IN-CURSOR-PSQ-ABTO-1
           .
       971101-SAI.
           EXIT.

      *-----------------------------------------------------------------
       971102-LER-CURSOR-PSQ-1         SECTION.
      *-----------------------------------------------------------------
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE REG-WS

           EXEC SQL
                FETCH NEXT ROWSET
                      CURSOR-PSQ-1 FOR 0101 ROWS
                INTO :CD-CTRA-TVM
           END-EXEC

           MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ-1
           MOVE SQLERRD(3)             TO QT-REG-SELD

           IF  QT-REG-SELD EQUAL 0101
               SET SGTSL12A-TEM-MAIS-REGISTRO TO TRUE
               MOVE 0100               TO QT-REG-SELD
           END-IF

           MOVE QT-REG-SELD            TO SGTSL12A-QT-OCR-TAB
           MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ-1
           .
       971102-SAI.
           EXIT.

      *-----------------------------------------------------------------
       971103-FECHA-CURSOR-PSQ-1       SECTION.
      *-----------------------------------------------------------------
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO

           MOVE 'N'                    TO IN-CURSOR-PSQ-ABTO-1
           EXEC SQL
                CLOSE CURSOR-PSQ-1
           END-EXEC
           .
       971103-SAI.
           EXIT.

      *-----------------------------------------------------------------
       972101-ABRE-CURSOR-PSQ-2          SECTION.
      *-----------------------------------------------------------------
           MOVE 0011                   TO MSG1-CD-SEQL-ERRO

           EXEC SQL
                OPEN CURSOR-PSQ-2
           END-EXEC
           MOVE 'S'                    TO IN-CURSOR-PSQ-ABTO-2
           .
       972101-SAI.
           EXIT.

      *-----------------------------------------------------------------
       972102-LER-CURSOR-PSQ-2         SECTION.
      *-----------------------------------------------------------------
           MOVE 0012                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE REG-WS

           EXEC SQL
                FETCH NEXT ROWSET
                      CURSOR-PSQ-2 FOR 0101 ROWS
                INTO :CD-CTRA-TVM
           END-EXEC

           MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ-2
           MOVE SQLERRD(3)             TO QT-REG-SELD

           IF  QT-REG-SELD EQUAL 0101
               SET SGTSL12A-TEM-MAIS-REGISTRO TO TRUE
               MOVE 0100               TO QT-REG-SELD
           END-IF

           MOVE QT-REG-SELD            TO SGTSL12A-QT-OCR-TAB
           MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ-2
           .
       972102-SAI.
           EXIT.

      *-----------------------------------------------------------------
       972103-FECHA-CURSOR-PSQ-2       SECTION.
      *-----------------------------------------------------------------
           MOVE 0013                   TO MSG1-CD-SEQL-ERRO

           MOVE 'N'                    TO IN-CURSOR-PSQ-ABTO-2
           EXEC SQL
                CLOSE CURSOR-PSQ-2
           END-EXEC
           .
       972103-SAI.
           EXIT.

      *-----------------------------------------------------------------
       980000-GRAVAR-CTNR-RESPOSTA     SECTION.
      *-----------------------------------------------------------------
           MOVE 0014                   TO MSG1-CD-SEQL-ERRO

      *--> Exclui eventual container de erro de outros programas
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO

      *--> Monta o nome do container de resposta
           MOVE 'SGTSL12A-RPST'        TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *--> previsto o erro de container
           SET  ERRO-CONTAINER         TO TRUE

           EXEC CICS
                PUT  CONTAINER ( NM-CTNR                 )
                     CHANNEL   ( NM-CNL                  )
                     FLENGTH   ( LENGTH OF SGTSL12A-RPST )
                     FROM      ( SGTSL12A-RPST           )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       980000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       983000-EXCLUIR-RESPOSTA-ERRO    SECTION.
      *----------------------------------------------------------------*
           MOVE 0015                   TO MSG1-CD-SEQL-ERRO
      *----------------------------------------------------------------*
      * Exclui o container de resposta e de erro para evitar problemas *
      * em modulos executados mais de uma vez dentro da mesma sessao do*
      * CICS. Equivale ao initialize na area de resposta dos programas *
      * que utilizam COMMAREA.                                         *
      *----------------------------------------------------------------*
      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *--> nome_modulo -RPST

           MOVE NM-PGM                 TO NM-CTNR(1:8)
           MOVE '-RPST'                TO NM-CTNR(9:5)

           SET DEL-CONTAINER           TO TRUE

      *--> Exclui o container de resposta
      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS
                DELETE CONTAINER (NM-CTNR   )
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

      *--> Excluir o container de erro
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO
           .
       983000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       983100-EXCLUIR-CONTAINER-ERRO   SECTION.
      *----------------------------------------------------------------*
           MOVE 0016                   TO MSG1-CD-SEQL-ERRO

      *--> Limpar variaveis container de erro
           INITIALIZE IIBKERRO-PRM-ERRO

      *--> Exclui o container de erro
           MOVE CTNR-PRM-ERRO          TO NM-CTNR
           SET DEL-CONTAINER           TO TRUE

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS
                DELETE CONTAINER (NM-CTNR   )
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

      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------

      *--> Caso exista utilização de cursor
           IF  IN-CURSOR-PSQ-ABTO-1  EQUAL  'S'
               PERFORM 971103-FECHA-CURSOR-PSQ-1
           END-IF

           PERFORM 999200-MONITORAR-VRV-PGM-E-ENC
           .
       999000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999200-MONITORAR-VRV-PGM-E-ENC  SECTION.
      *-----------------------------------------------------------------
      *--> Monitora variàveis do pgm, grava e encerra processamento

           IF  MSG1-CD-ERRO-TS             EQUAL  ZEROS
               MOVE  8888              TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS             EQUAL  SPACES
               MOVE  'Erro não especificado no módulo.'
                                       TO  MSG1-TX-ERRO-TS
           END-IF

      *--> Verifica se o tipo de erro gerado possui tratamento interno
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

      *--> Adiciona as variáveis para monitoração
           MOVE 'SGTSL12A-CD-UOR-LCZC-USU'      TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL12A-CD-UOR-LCZC-USU        TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL12A-CD-UOR-PSC-USU'       TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL12A-CD-UOR-PSC-USU         TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL12A-CD-CTRA-TVM'          TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL12A-CD-CTRA-TVM            TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

      *--> grava o erro no msg
           PERFORM MSG1-NTF-MSG
           MOVE MSG1-CD-ERRO-TS                 TO IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS                 TO IIBKERRO-TX-ERRO

           EXEC CICS PUT
              CONTAINER ( CTNR-PRM-ERRO               )
              CHANNEL   ( NM-CNL                      )
              FROM      ( IIBKERRO-PRM-ERRO           )
              FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
              NOHANDLE
           END-EXEC

           PERFORM 999999-ENCERRAR
           .
       999200-SAI.
           EXIT.

      *---------------------------------------*
      * 999001-TRATA-ABEND-CICS        SECTION.
      * 999002-TRATA-CONDITION-CICS    SECTION.
      * 999003-ADICIONA-VRV-MNT        SECTION.
      *---------------------------------------*
-INC HLPKP999

      *---------------------------------------*
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *---------------------------------------*
-INC SGTKP999

      *---------------------------------------*
      * 999010-TRATA-ERRO-DB2          SECTION.
      *---------------------------------------*
-INC HLPKP997

      *---------------------------------------*
      * MSG1-NTF-MSG                   SECTION.
      *---------------------------------------*
-INC MSGKPMG1

      *---------------------------------------*
       999999-ENCERRAR                 SECTION.
      *---------------------------------------*

           EXEC CICS
                RETURN
           END-EXEC
           .
       999999-SAI.
           EXIT.
      *---------------------- FIM SGTSL12A ----------------------------