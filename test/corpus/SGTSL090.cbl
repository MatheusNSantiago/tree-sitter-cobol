      *-----------------------------------------------------------------
      * Programa..: SGTSL090
      * Analista..: F8785286 - Roseli A. Kawahata
      * Autor.....: F8785286 - Roseli A. Kawahata
      * Objetivo..: Retorna a lista contendo os tipos de atributo
      *             associados a uma regra de validação de característi-
      *             ca da operação.
      * Compilacao: Cobol/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 19.12.2017 - F8785286 2016/145195 - IMPLANTACAO
      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTSL090.
      *-----------------------------------------------------------------
       ENVIRONMENT                     DIVISION.
      *-----------------------------------------------------------------
       CONFIGURATION                   SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *-----------------------------------------------------------------
       DATA                            DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       WORKING-STORAGE                 SECTION.
      *-----------------------------------------------------------------
       77  CTE-INICIO-SS               PIC  X(40) VALUE
                                       '*** S. S. comeca aqui ***'.
       77  CTE-PROG                    PIC  X(17) VALUE
                                       '*** SGTSL090 ***'.
       77  CTE-VERS                    PIC  X(06) VALUE 'VRS001'.

      *--> Book do programa de ambiente - CICS5000
      *    Definido na work para preservar as informacoes
-INC HLPKE01B

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

      *--> Area de trabalho
       01 IDX-OCC                      PIC S9(4) COMP-5 VALUE 0.
       01 QT-REG-SELD                  PIC S9(4) COMP-5 VALUE 0.

      *--> Campos utilizados para leitura
       01  REG-WS.
           03 CD-ATB-VLDC              PIC S9(4) COMP-5
              OCCURS 51 TIMES          VALUE 0.
           03 NM-ATB-VLDC              PIC X(50)
              OCCURS 51 TIMES          VALUE ' '.
           03 CD-TIP-ATB-VLDC          PIC S9(4) COMP-5
              OCCURS 51 TIMES          VALUE 0.

      *--> Variavel(is) de controle do cursor(es)
       01  CTL-CURSOR-PSQ.
           03  CD-FIM-CURSOR-PSQ       PIC S9(04) COMP-5 VALUE 0.
               88  FIM-CURSOR-PSQ      VALUE +100.
           03  IN-CURSOR-PSQ-ABTO      PIC  X(01) VALUE 'N'.
               88  CURSOR-PSQ-ABTO     VALUE 'S'.

      *-----------------------------------------------------------------
      *    Books Utilizados
      *-----------------------------------------------------------------

      *--> Book do Container de Erro
-INC IIBKERRO

      *--> Book para captura de abend cics da bloco codigo HLPKW999
-INC HLPKW999

      *--> Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1

      *--> Book para captura de erro via MSG1
-INC MSGKWMG1

      *--> Book do container de requisicao e resposta deste modulo
-INC SGTKL090

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

      *--> Cursor de Pesquisa 01
      *    Listar
           EXEC SQL
             DECLARE CURSOR-PSQ CURSOR
                     WITH ROWSET POSITIONING FOR
               SELECT CD_ATB_VLDC,
                      NM_ATB_VLDC,
                      CD_TIP_ATB_VLDC
               FROM DB2SGT.ATB_VLDC
               WHERE CD_ATB_VLDC  >  0
               ORDER BY CD_ATB_VLDC
           END-EXEC.

       77  CTE-FINAL-SS                PIC  X(40) VALUE
                                       '*** S. S. termina aqui ***'.

      *-----------------------------------------------------------------
       PROCEDURE                       DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------

           IF  NOT EXECUCAO-ONLINE  AND
               NOT EXECUCAO-BATCH
               CALL CICS5000 USING CICS5000-COMMAREA
           END-IF

      *--> Habilita tratamento automatico de Erros DB2
-INC HLPKE997

      *--> Habilita captura automatica de Abend e Erros CICS
-INC HLPKE999

           PERFORM  100000-DESCOBRIR-CANAL

           PERFORM  110000-EXCLUIR-CTNR-RPST

           PERFORM  120000-EXCLUIR-CTNR-ERRO

           PERFORM  970101-ABRE-CURSOR-PSQ
           PERFORM  970102-LER-CURSOR-PSQ
           PERFORM  970103-FECHA-CURSOR-PSQ

           PERFORM  200000-PROCESSAR-PESQUISA
                    VARYING IDX-OCC
                    FROM  0001 BY 0001
                    UNTIL IDX-OCC          > SGTSL090-QT-OCR-TAB

           PERFORM  300000-POSTAR-RESPOSTA

           GOBACK

           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-DESCOBRIR-CANAL          SECTION.
      *-----------------------------------------------------------------
           MOVE  0001                  TO  MSG1-CD-SEQL-ERRO

           EXEC CICS ASSIGN
                     CHANNEL ( NM-CNL )
           END-EXEC

           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-EXCLUIR-CTNR-RPST        SECTION.
      *-----------------------------------------------------------------
           MOVE  0002                  TO  MSG1-CD-SEQL-ERRO

           EXEC CICS  DELETE
                      CONTAINER ( 'SGTSL090-RPST' )
                      CHANNEL   ( NM-CNL          )
                      NOHANDLE
           END-EXEC

           IF  NOT (EIBRESP                EQUAL  DFHRESP(NORMAL)  OR
                    EIBRESP                EQUAL  DFHRESP(CONTAINERERR))
               PERFORM  999002-TRATA-CONDITION-CICS
           END-IF
           .
       110000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       120000-EXCLUIR-CTNR-ERRO        SECTION.
      *-----------------------------------------------------------------
           MOVE  0003                  TO  MSG1-CD-SEQL-ERRO

           EXEC CICS  DELETE
                      CONTAINER ( 'PRM-ERRO' )
                      CHANNEL   ( NM-CNL     )
                      NOHANDLE
           END-EXEC

           IF  NOT (EIBRESP                EQUAL  DFHRESP(NORMAL)  OR
                    EIBRESP                EQUAL  DFHRESP(CONTAINERERR))
               PERFORM  999002-TRATA-CONDITION-CICS
           END-IF
           .
       120000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-PROCESSAR-PESQUISA       SECTION.
      *-----------------------------------------------------------------
           MOVE  0004                  TO  MSG1-CD-SEQL-ERRO

           MOVE  CD-ATB-VLDC                                   (IDX-OCC)
             TO  SGTSL090-CD-ATB-VLDC                          (IDX-OCC)
           MOVE  NM-ATB-VLDC                                   (IDX-OCC)
             TO  SGTSL090-NM-ATB-VLDC                          (IDX-OCC)
           MOVE  CD-TIP-ATB-VLDC                               (IDX-OCC)
             TO  SGTSL090-CD-TIP-ATB-VLDC                      (IDX-OCC)
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-POSTAR-RESPOSTA          SECTION.
      *-----------------------------------------------------------------
           MOVE  0005                  TO  MSG1-CD-SEQL-ERRO

           EXEC CICS PUT
                     CONTAINER ( 'SGTSL090-RPST'         )
                     CHANNEL   ( NM-CNL                  )
                     FLENGTH   ( LENGTH OF SGTSL090-RPST )
                     FROM      ( SGTSL090-RPST           )
           END-EXEC
           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970101-ABRE-CURSOR-PSQ          SECTION.
      *-----------------------------------------------------------------
           MOVE  0006                  TO  MSG1-CD-SEQL-ERRO

           EXEC SQL
                OPEN CURSOR-PSQ
           END-EXEC

           MOVE  'S'                   TO  IN-CURSOR-PSQ-ABTO
           .
       970101-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970102-LER-CURSOR-PSQ           SECTION.
      *-----------------------------------------------------------------
           MOVE  0007                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      REG-WS

           EXEC SQL
                FETCH NEXT ROWSET
                      CURSOR-PSQ FOR 051 ROWS
               INTO :CD-ATB-VLDC
                  , :NM-ATB-VLDC
                  , :CD-TIP-ATB-VLDC
           END-EXEC

           MOVE  SQLERRD(3)            TO  QT-REG-SELD

           MOVE  QT-REG-SELD           TO  SGTSL090-QT-OCR-TAB
           MOVE  SQLCODE               TO  CD-FIM-CURSOR-PSQ
           .
       970102-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970103-FECHA-CURSOR-PSQ         SECTION.
      *-----------------------------------------------------------------
           MOVE  0008                  TO  MSG1-CD-SEQL-ERRO

           MOVE  'N'                   TO  IN-CURSOR-PSQ-ABTO
           EXEC SQL
                CLOSE CURSOR-PSQ
           END-EXEC
           .
       970103-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------
           IF  IN-CURSOR-PSQ-ABTO          EQUAL  'S'
               PERFORM  970103-FECHA-CURSOR-PSQ
           END-IF

           IF  MSG1-CD-ERRO-TS             EQUAL  ZEROS
               MOVE  8888              TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS             EQUAL  SPACES
               MOVE 'Erro não especificado no módulo.'
                                       TO  MSG1-TX-ERRO-TS
           END-IF

      *--> Grava o erro gerado no sistema MSG - Mensageria
           PERFORM  MSG1-NTF-MSG

           MOVE  MSG1-CD-ERRO-TS       TO  IIBKERRO-CD-ERRO
           MOVE  MSG1-TX-ERRO-TS       TO  IIBKERRO-TX-ERRO

      *--> Grava o erro gerado no container de erro
           EXEC CICS  PUT
                      CONTAINER ( 'PRM-ERRO'                  )
                      CHANNEL   ( NM-CNL                      )
                      FROM      ( IIBKERRO-PRM-ERRO           )
                      FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                      NOHANDLE
           END-EXEC

           PERFORM 999999-ENCERRAR
           .
       999000-SAI.
           EXIT.

      *----------------------------------------
      * 999001-TRATA-ABEND-CICS        SECTION.
      * 999002-TRATA-CONDITION-CICS    SECTION.
      * 999003-ADICIONA-VRV-MNT        SECTION.
      * 999004-ALINHA-NUMERO-ESQUERDA  SECTION.
      *----------------------------------------
-INC HLPKP999
      *----------------------------------------
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *----------------------------------------
-INC SGTKP999
      *----------------------------------------
      * 999010-TRATA-ERRO-DB2          SECTION.
      *----------------------------------------
-INC HLPKP997
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

      *----------------------------------------
       999999-ENCERRAR                 SECTION.
      *----------------------------------------

           EXEC CICS
                RETURN
           END-EXEC
           .
       999999-SAI.
           EXIT.

      *---------------------- FIM SGTSL090 -----------------------------