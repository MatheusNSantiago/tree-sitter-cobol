      *-----------------------------------------------------------------  VRS001
      * PROGRAMA..: SGTSP014                                              VRS001
      * ANALISTA..: F2054109 - Cintia Ap Soares Iglesias                  VRS001
      * AUTOR.....: F8608799 - Ricardo Ramos                              VRS001
      * OBJETIVO..: Listar todos os itens disponíveis ou vinculadas em    VRS001
      * um modelo de ordem de negociação compra de um produto/modalidade  VRS001
      * COMPILACAO: COBOL/CICS/DB2                                        VRS001
      *-----------------------------------------------------------------  VRS001
      * VRS002 18/06/2019 F9188847 DE 2016/145195 - Acerto de mensageria
      * VRS001 29/01/2016 F8608799 DE 2015/120821 - Implantação           VRS001
      *-----------------------------------------------------------------  VRS001
       IDENTIFICATION DIVISION.                                           VRS001
      *-----------------------------------------------------------------  VRS001
       PROGRAM-ID. SGTSP014.                                              VRS001
                                                                          VRS001
      *-----------------------------------------------------------------  VRS001
       ENVIRONMENT DIVISION.                                              VRS001
      *-----------------------------------------------------------------  VRS001
       CONFIGURATION SECTION.                                             VRS001
       SPECIAL-NAMES.                                                     VRS001
           DECIMAL-POINT IS COMMA.                                        VRS001
                                                                          VRS001
      *-----------------------------------------------------------------  VRS001
       DATA DIVISION.                                                     VRS001
      *-----------------------------------------------------------------  VRS001
                                                                          VRS001
      *-----------------------------------------------------------------  VRS001
       WORKING-STORAGE SECTION.                                           VRS001
      *-----------------------------------------------------------------  VRS001
       77  CTE-INICIO-SS            PIC X(35)  VALUE                      VRS001
               '*** STORAGE SECTION COMECA AQUI ***'.                     VRS001
       01  CTE-PROG                 PIC  X(17) VALUE '*** SGTSP014 ***'.  VRS001
       01  CTE-VERS                 PIC  X(06) VALUE 'VRS002'.            VRS001
      
      *-----------------------------------------------------------------
      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informacoes
       COPY HLPKE01B.
       LOCAL-STORAGE SECTION.                                             VRS001
                                                                          VRS001
      *-----------------------------------------------------------------  VRS001
      * book de para acionamento do msg1                                  VRS001
       COPY IIBKERRO.
      *--- Book de variáveis da mensageria
       COPY MSGKWMG1.                                                           0000
      *--- Book das variáveis auxiliares do tratamento de erro via MSG
       COPY SGTKERR1.
      *--- Book de variáveis para tratamento de erros CICS e SQL
       COPY HLPKW999.

       01  GRP-CONTAINER.                                                 VRS001
           03  CANAL                        PIC X(16) VALUE SPACES.       VRS001
      *    03  QT-CCT-LIDO                  PIC S9(9) COMP-5 VALUE 0.     VRS001
      *    03  QT-CCT-EPRO                  PIC S9(9) COMP-5 VALUE 0.     VRS001
      *    03  VL-EXBR-1                    PIC 9(10)        VALUE 0.     VRS001
      *    03  VL-EXBR-2                    PIC 9(10)        VALUE 0.     VRS001
                                                                          VRS001
      * Area de trabalho                                                  VRS001
                                                                          VRS001
       01 QT-REG-SELD                       PIC S9(4) COMP-5 VALUE 0.     VRS001
                                                                          VRS001
      * Campos utilizados para leitura                                    VRS001
                                                                          VRS001
       01  REG-WS.                                                        VRS001
           03 CD-ITEM-MOD-ORD                PIC S9(4) COMP-5             VRS001
              OCCURS 101 TIMES               VALUE 0.                     VRS001
           03 TX-ITEM-MOD-ORD                PIC X(60)                    VRS001
              OCCURS 101 TIMES               VALUE ' '.                   VRS001
           03 NR-SEQ-EXB-ITEM                PIC S9(4) COMP-5             VRS001
              OCCURS 101 TIMES               VALUE 0.                     VRS001
           03 NR-SEQ-EXB-ITEM-N              PIC S9(4) COMP-5             VRS001
              OCCURS 101 TIMES               VALUE 0.                     VRS001
                                                                          VRS001
       01  QT-REG-SELD-N                     PIC S9(4) COMP-5 VALUE 0.    VRS001
                                                                          VRS001
      * Variavel(is) de controle do cursor(es)                            VRS001
                                                                          VRS001
       01  CTL-CURSOR-PSQ.                                                VRS001
           03  CD-FIM-CURSOR-PSQ             PIC S9(04) COMP-5 VALUE 0.   VRS001
               88  FIM-CURSOR-PSQ            VALUE +100.                  VRS001
           03  IN-CURSOR-PSQ-ABTO            PIC  X(01) VALUE 'N'.        VRS001
               88  CURSOR-PSQ-ABTO           VALUE 'S'.                   VRS001
                                                                          VRS001
                                                                          VRS001
      * Subrotinas utilizadas                                             VRS001
                                                                          VRS001
           EXEC SQL                                                       VRS001
               INCLUDE SQLCA                                              VRS001
           END-EXEC.                                                      VRS001
                                                                          VRS001
      *-----------------------------------------------------------------  VRS001
       COPY SGTKP014.                                                           0000
                                                                          VRS001
      * Listar todos os itens disponíveis ou vinculados em um             VRS001
      * modelo de ordem de negociação de compra de um produto/modalidade  VRS001
                                                                          VRS001
           EXEC SQL                                                       VRS001
             DECLARE CURSOR-PSQ CURSOR                                    VRS001
                     WITH ROWSET POSITIONING FOR                          VRS001
             SELECT  A.CD_ITEM_MOD_ORD, A.TX_ITEM_MOD_ORD,                VRS001
                     C.NR_SEQ_EXB_ITEM                                    VRS001
               FROM  DB2SGT.ITEM_MOD_ORD_NEGO A                           VRS001
              INNER  JOIN DB2SGT.ITEM_PMT_MOD_CPR B                       VRS001
                 ON                                                       VRS001
                    A.CD_ITEM_MOD_ORD = B.CD_ITEM_MOD_ORD                 VRS001
              LEFT OUTER JOIN                                             VRS001
              (SELECT CD_ITEM_MOD_ORD, NR_SEQ_EXB_ITEM                    VRS001
               FROM                                                       VRS001
                  DB2SGT.ITEM_VCLD_MOD_ORD                                VRS001
               WHERE                                                      VRS001
                    CD_MOD_ORD_NEGO = :SGTSP014-CD-MOD-ORD-NEGO) C        VRS001
              ON    B.CD_ITEM_MOD_ORD = C.CD_ITEM_MOD_ORD                 VRS001
              WHERE                                                       VRS001
                    A.CD_ITEM_MOD_ORD > :SGTSP014-CD-ITEM-MOD-ORD-ENTD    VRS001
              AND   B.CD_PRD = :SGTSP014-CD-PRD                           VRS001
              AND   B.CD_MDLD = :SGTSP014-CD-MDLD                         VRS001
              ORDER BY                                                    VRS001
                    A.CD_ITEM_MOD_ORD                                     VRS001
           END-EXEC.                                                      VRS001
                                                                          VRS001
      *-----------------------------------------------------------------  VRS001
       PROCEDURE DIVISION.                                                VRS001
      *-----------------------------------------------------------------  VRS001
      *-----------------------------------------------------------------  VRS001
       000000-PRINCIPAL  SECTION.                                         VRS001
      *-----------------------------------------------------------------  VRS001
      * Habilita tratamento automatico de erros DB2                       VRS001
       COPY MSGKE997.                                                           0000
      * Habilita captura automatica de abend e erros CICS                 VRS001
       COPY MSGKE999.                                                           0000
                                                                          VRS001
           PERFORM 100000-DESCOBRIR-CANAL                                 VRS001
           PERFORM 110000-EXCLUIR-CTNR-RPST                               VRS001
           PERFORM 120000-EXCLUIR-CTNR-ERRO                               VRS001
           PERFORM 130000-RECEBER-REQUISICAO                              VRS001
           PERFORM 140000-VALIDAR-PARAMETRO                               VRS001
                                                                          VRS001
           PERFORM 200000-SOLICITAR-LISTA                                 VRS001
           PERFORM 300000-PROCESSA-PESQUISA                               VRS001
                    VARYING SGTSP014-IC-TAB FROM 1 BY 1                   VRS001
                      UNTIL SGTSP014-IC-TAB > SGTSP014-QT-OCR-TAB         VRS001
                                                                          VRS001
           PERFORM 400000-POSTA-RESPOSTA                                  VRS001
           GOBACK                                                         VRS001
           .                                                              VRS001
       000000-SAI.                                                        VRS001
           EXIT.                                                          VRS001
      *-----------------------------------------------------------------  VRS001
       100000-DESCOBRIR-CANAL SECTION.                                    VRS001
      *-----------------------------------------------------------------  VRS001
           MOVE 0001                   TO MSG1-CD-SEQL-ERRO               VRS001
                                                                          VRS001
           EXEC CICS ASSIGN                                               VRS001
              CHANNEL ( CANAL )                                           VRS001
           END-EXEC                                                       VRS001
           .                                                              VRS001
       100000-SAI.                                                        VRS001
           EXIT.                                                          VRS001
      *-----------------------------------------------------------------  VRS001
       110000-EXCLUIR-CTNR-RPST SECTION.                                  VRS001
      *-----------------------------------------------------------------  VRS001
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO               VRS001
                                                                          VRS001
           EXEC CICS DELETE                                               VRS001
              CONTAINER ( 'SGTSP014-RPST' )                               VRS001
              CHANNEL   ( CANAL      )                                    VRS001
              NOHANDLE                                                    VRS001
           END-EXEC                                                       VRS001
           IF  NOT (EIBRESP EQUAL DFHRESP(NORMAL)   OR                    VRS001
               EIBRESP EQUAL DFHRESP(CONTAINERERR) )                      VRS001
               PERFORM 999002-TRATA-CONDITION-CICS                        VRS001
           END-IF                                                         VRS001
           .                                                              VRS001
       110000-SAI.                                                        VRS001
           EXIT.                                                          VRS001
      *-----------------------------------------------------------------  VRS001
       120000-EXCLUIR-CTNR-ERRO SECTION.                                  VRS001
      *-----------------------------------------------------------------  VRS001
           MOVE 0003                   TO MSG1-CD-SEQL-ERRO               VRS001
                                                                          VRS001
           EXEC CICS DELETE                                               VRS001
              CONTAINER ( 'PRM-ERRO' )                                    VRS001
              CHANNEL   ( CANAL      )                                    VRS001
              NOHANDLE                                                    VRS001
           END-EXEC                                                       VRS001
           IF  NOT (EIBRESP EQUAL DFHRESP(NORMAL)   OR                    VRS001
               EIBRESP EQUAL DFHRESP(CONTAINERERR) )                      VRS001
               PERFORM 999002-TRATA-CONDITION-CICS                        VRS001
           END-IF                                                         VRS001
           .                                                              VRS001
       120000-SAI.                                                        VRS001
           EXIT.                                                          VRS001
      *-----------------------------------------------------------------  VRS001
       130000-RECEBER-REQUISICAO SECTION.                                 VRS001
      *-----------------------------------------------------------------  VRS001
           MOVE 0004                   TO MSG1-CD-SEQL-ERRO               VRS001
                                                                          VRS001
           MOVE ZEROS                  TO QT-CCT-LIDO                     VRS001
           MOVE LENGTH OF SGTSP014-RQSC  TO QT-CCT-EPRO

      *--- Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE
      
      *--- Por causa da captura automática de erro CICS, deve ser
      *--- previsto o erro de container
           SET  ERRO-CONTAINER         TO TRUE

           EXEC CICS GET                                                  VRS001
              CONTAINER ( 'SGTSP014-RQSC' )                               VRS001
              CHANNEL   ( CANAL      )                                    VRS001
              NODATA                                                      VRS001
              FLENGTH ( QT-CCT-LIDO )                                     VRS001
           END-EXEC                                                       VRS001
                                                                          VRS001
           IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO                          VRS001
               SET   ERRO-TAMANHO-AREA   TO  TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA                        VRS001
           END-IF                                                         VRS001
                                                                          VRS001
           EXEC CICS GET                                                  VRS001
              CONTAINER ( 'SGTSP014-RQSC' )                               VRS001
              CHANNEL   ( CANAL      )                                    VRS001
              FLENGTH ( LENGTH OF SGTSP014-RQSC )                         VRS001
              INTO    ( SGTSP014-RQSC )                                   VRS001
           END-EXEC                                                       VRS001
      
      *--- Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .                                                              VRS001
       130000-SAI.                                                        VRS001
           EXIT.                                                          VRS001
      *-----------------------------------------------------------------  VRS001
       140000-VALIDAR-PARAMETRO SECTION.                                  VRS001
      *-----------------------------------------------------------------  VRS001
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO               VRS001
                                                                          VRS001
      *  O Código de ítem de modelo de ordem de negociação poderá vir     VRS001
      *  com zeros. Para os demais campos, depende da ação que for        VRS001
      *  solicitada pelo chamador.                                        VRS001
                                                                          VRS001
           IF  SGTSP014-CD-MOD-ORD-NEGO  EQUAL  0                         VRS001
               SET ERRO-PROGRAMADO     TO TRUE                            VRS001
               MOVE 0641               TO MSG1-CD-ERRO-TS                 VRS001
               MOVE 'Código do modelo de ordem de negociação não informa  VRS001
      -             'do.'                  TO MSG1-TX-ERRO-TS             VRS001
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA                        VRS001
           END-IF                                                         VRS001
                                                                          VRS001
           IF  SGTSP014-CD-MDLD    EQUAL  0                               VRS001
               SET ERRO-PROGRAMADO     TO TRUE                            VRS001
               MOVE 0069               TO MSG1-CD-ERRO-TS                 VRS001
               MOVE 'Código da modalidade do produto não informado.'      VRS001
                                       TO MSG1-TX-ERRO-TS                 VRS001
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA                        VRS001
           END-IF                                                         VRS001
                                                                          VRS001
           IF  SGTSP014-CD-PRD     EQUAL  0                               VRS001
               SET ERRO-PROGRAMADO     TO TRUE                            VRS001
               MOVE 0068               TO MSG1-CD-ERRO-TS                 VRS001
               MOVE 'Código do produto não informado.'                    VRS001
                                       TO MSG1-TX-ERRO-TS                 VRS001
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA                        VRS001
           END-IF                                                         VRS001
                                                                          VRS001
           .                                                              VRS001
       140000-SAI.                                                        VRS001
           EXIT.                                                          VRS001
                                                                          VRS001
      *-----------------------------------------------------------------  VRS001
       200000-SOLICITAR-LISTA          SECTION.                           VRS001
      *-----------------------------------------------------------------  VRS001
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO               VRS001
                                                                          VRS001
           INITIALIZE REG-WS                                              VRS001
           MOVE 'N'                    TO SGTSP014-IN-CNTO-PSQ            VRS001
                                                                          VRS001
           PERFORM 970113-ABRE-CURSOR-PSQ                                 VRS001
           PERFORM 970114-LE-CURSOR-PSQ                                   VRS001
           PERFORM 970115-FECHA-CURSOR-PSQ                                VRS001
                                                                          VRS001
           .                                                              VRS001
       200000-SAI.                                                        VRS001
           EXIT.                                                          VRS001
      *-----------------------------------------------------------------  VRS001
       300000-PROCESSA-PESQUISA SECTION.                                  VRS001
      *-----------------------------------------------------------------  VRS001
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO               VRS001
                                                                          VRS001
           MOVE CD-ITEM-MOD-ORD(SGTSP014-IC-TAB) TO                       VRS001
                     SGTSP014-CD-ITEM-MOD-ORD(SGTSP014-IC-TAB)            VRS001
           MOVE TX-ITEM-MOD-ORD(SGTSP014-IC-TAB) TO                       VRS001
                     SGTSP014-TX-ITEM-MOD-ORD(SGTSP014-IC-TAB)            VRS001
           MOVE NR-SEQ-EXB-ITEM(SGTSP014-IC-TAB) TO                       VRS001
                     SGTSP014-NR-SEQ-EXB-ITEM(SGTSP014-IC-TAB)            VRS001
                                                                          VRS001
           .                                                              VRS001
       300000-SAI.                                                        VRS001
           EXIT.                                                          VRS001
      *-----------------------------------------------------------------  VRS001
       400000-POSTA-RESPOSTA SECTION.                                     VRS001
      *-----------------------------------------------------------------  VRS001
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO               VRS001
           EXEC CICS PUT                                                  VRS001
              CONTAINER ( 'SGTSP014-RPST' )                               VRS001
              CHANNEL   ( CANAL      )                                    VRS001
              FLENGTH   ( LENGTH OF SGTSP014-RPST )                       VRS001
              FROM      ( SGTSP014-RPST )                                 VRS001
           END-EXEC                                                       VRS001
                                                                          VRS001
           .                                                              VRS001
       400000-SAI.                                                        VRS001
           EXIT.                                                          VRS001
      *-----------------------------------------------------------------  VRS001
       910000-VERIFICA-FIM-LISTA       SECTION.                           VRS001
      *-----------------------------------------------------------------  VRS001
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO               VRS001
                                                                          VRS001
           MOVE SQLERRD (3)            TO QT-REG-SELD                     VRS001
                                                                          VRS001
           IF  QT-REG-SELD             EQUAL 101                          VRS001
               SET SGTSP014-TEM-MAIS-REGISTRO TO TRUE                     VRS001
               MOVE 100                TO QT-REG-SELD                     VRS001
           END-IF                                                         VRS001
                                                                          VRS001
           MOVE QT-REG-SELD            TO SGTSP014-QT-OCR-TAB             VRS001
                                                                          VRS001
           .                                                              VRS001
       910000-SAI.                                                        VRS001
           EXIT.                                                          VRS001
      *-----------------------------------------------------------------  VRS001
       970113-ABRE-CURSOR-PSQ    SECTION.                                 VRS001
      *-----------------------------------------------------------------  VRS001
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO               VRS001
           EXEC SQL                                                       VRS001
                OPEN CURSOR-PSQ                                           VRS001
           END-EXEC                                                       VRS001
           MOVE 'S'                    TO IN-CURSOR-PSQ-ABTO              VRS001
           .                                                              VRS001
       970113-SAI.                                                        VRS001
           EXIT.                                                          VRS001
      *-----------------------------------------------------------------  VRS001
       970114-LE-CURSOR-PSQ      SECTION.                                 VRS001
      *-----------------------------------------------------------------  VRS001
           MOVE 0011                   TO MSG1-CD-SEQL-ERRO               VRS001
                                                                          VRS001
           EXEC SQL                                                       VRS001
                FETCH NEXT ROWSET                                         VRS001
                      CURSOR-PSQ FOR 0101 ROWS                            VRS001
               INTO :CD-ITEM-MOD-ORD                                      VRS001
                  , :TX-ITEM-MOD-ORD                                      VRS001
                  , :NR-SEQ-EXB-ITEM                                      VRS001
                    :NR-SEQ-EXB-ITEM-N                                    VRS001
           END-EXEC                                                       VRS001
                                                                          VRS001
           PERFORM  910000-VERIFICA-FIM-LISTA                             VRS001
                                                                          VRS001
           MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ               VRS001
           .                                                              VRS001
       970114-SAI.                                                        VRS001
           EXIT.                                                          VRS001
      *-----------------------------------------------------------------  VRS001
       970115-FECHA-CURSOR-PSQ   SECTION.                                 VRS001
      *-----------------------------------------------------------------  VRS001
           MOVE 0012                   TO MSG1-CD-SEQL-ERRO               VRS001
           MOVE 'N'                    TO IN-CURSOR-PSQ-ABTO              VRS001
           EXEC SQL                                                       VRS001
                CLOSE CURSOR-PSQ                                          VRS001
           END-EXEC                                                       VRS001
           .                                                              VRS001
       970115-SAI.                                                        VRS001
           EXIT.                                                          VRS001
      *-----------------------------------------------------------------  VRS001
       999000-GRAVA-ERRO-E-ENCERRA SECTION.                               VRS001
      *-----------------------------------------------------------------  VRS001
      * Por precaução verifica se algum cursor ficou aberto.              VRS001
           IF  CURSOR-PSQ-ABTO                                            VRS001
                PERFORM 970115-FECHA-CURSOR-PSQ                           VRS001
           END-IF

           IF  MSG1-CD-ERRO-TS         EQUAL  ZEROS
              MOVE  8888               TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS         EQUAL  SPACES
               MOVE  'Erro não especificado no módulo.'
                                       TO  MSG1-TX-ERRO-TS
           END-IF

      *--> VERIFICA SE O TIPO DE ERRO GERADO POSSUI TRATAMENTO INTERNO
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

           MOVE 'SGTSP014-CD-MOD-ORD-NEGO'
                                       TO MSGKW999-NM-VRV-MNT
           MOVE  SGTSP014-CD-MOD-ORD-NEGO
                                       TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSP014-CD-ITEM-MOD-ORD-ENTD'
                                       TO MSGKW999-NM-VRV-MNT
           MOVE  SGTSP014-CD-ITEM-MOD-ORD-ENTD
                                       TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSP014-CD-MDLD'
                                       TO MSGKW999-NM-VRV-MNT
           MOVE  SGTSP014-CD-MDLD
                                       TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSP014-CD-PRD'
                                       TO MSGKW999-NM-VRV-MNT
           MOVE  SGTSP014-CD-PRD
                                       TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT
                                                                          VRS001
      * --> grava o erro no msg                                           VRS001
           PERFORM MSG1-NTF-MSG                                           VRS001
           MOVE MSG1-CD-ERRO-TS        TO IIBKERRO-CD-ERRO                VRS001
           MOVE MSG1-TX-ERRO-TS        TO IIBKERRO-TX-ERRO                VRS001
                                                                          VRS001
           EXEC CICS PUT                                                  VRS001
              CONTAINER ( 'PRM-ERRO' )                                    VRS001
              CHANNEL   ( CANAL      )                                    VRS001
              FROM      ( IIBKERRO-PRM-ERRO)                              VRS001
              FLENGTH ( LENGTH OF IIBKERRO-PRM-ERRO )                     VRS001
              NOHANDLE                                                    VRS001
           END-EXEC                                                       VRS001
                                                                          VRS001
           PERFORM 999999-ENCERRAR                                        VRS001
                                                                          VRS001
           .                                                              VRS001
       999000-SAI.                                                        VRS001
           EXIT.                                                          VRS001
      * --------------------------------------*                           VRS001
      * 999001-TRATA-ABEND-CICS        SECTION.                           VRS001
      * 999002-TRATA-CONDITION-CICS    SECTION.                           VRS001
      * 999003-ADICIONA-VRV-MNT        SECTION.                           VRS001
      * --------------------------------------*                           VRS001
       COPY HLPKP999.                                                           0000
      * --------------------------------------*                           VRS001
      * 999010-TRATA-ERRO-DB2          SECTION.                           VRS001
       COPY HLPKP997.                                                           0000
      * --------------------------------------*                           VRS001
      * MSG1-NTF-MSG                   SECTION.                           VRS001
       COPY MSGKPMG1.
      * --------------------------------------*
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *---------------------------------------*
       COPY SGTKP999.
      * --------------------------------------*                           VRS001
       999999-ENCERRAR                SECTION.                            VRS001
      * --------------------------------------*                           VRS001
           EXEC CICS                                                      VRS001
                RETURN                                                    VRS001
           END-EXEC.                                                      VRS001
