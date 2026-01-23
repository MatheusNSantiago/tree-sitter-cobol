      *-----------------------------------------------------------------
      * Programa..: SGTL035
      * Analista..: F2054109 - Cintia Ap Soares Iglesias
      * Autor.....: F8785286 - Roseli Akemi Kawahata
      * Objetivo..: Listar Pessoa de Contato Externo da Operação
      * Compilacao: Cobol/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 21/10/2016 - F8785286 - DE 2016/145195 - Implantação
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTSL035.

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

      *-----------------------------------------------------------------
       WORKING-STORAGE                 SECTION.
      *-----------------------------------------------------------------
       77  CTE-INICIO-SS               PIC  X(40) VALUE
                                       '*** S.S. Comeca Aqui ***'.
       77  CTE-PROG                    PIC  X(17)
                                       VALUE '*** SGTSL035 ***'.
       77  CTE-VERS                    PIC  X(06) VALUE 'VRS001'.

      *--> Definição dos Nomes dos Programas Acionados
       01  TAB-PROGRAMAS-CHAMADOS.
           03  SGTS014G                PIC  X(08) VALUE 'SGTS014G'.

      *--> Definição dos Variaveis do Cobolito
       COPY ALMKCBL1.

      *--> Book do programa de ambiente - CICS5000
      *    Definido na work para preservar as informacoes
       COPY HLPKE01B.

      *-----------------------------------------------------------------
       LOCAL-STORAGE SECTION.
      *-----------------------------------------------------------------

      *--> Area de trabalho
       01  QT-REG-SELD                 PIC S9(04) COMP-5 VALUE ZEROS.

       01  AREA-AUXILIAR.
      *--> Tipo de telefone:
      *    0001 - Telefone Comercial
      *    0002 - Celular
           03  TIP-TEL-CML             PIC S9(04) COMP-5 VALUE 0001.
           03  TIP-TEL-CELR            PIC S9(04) COMP-5 VALUE 0002.

      *--> Campos utilizados para leitura
       01  REG-WS.
           03 CD-PSS-CTT-EXNO          PIC S9(04) COMP-5
              OCCURS 101 TIMES         VALUE ZEROS.
           03 NM-PSS-CTT-EXNO          PIC  X(60)
              OCCURS 101 TIMES         VALUE SPACES.
           03 NM-CIA-CTT-EXNO          PIC  X(60)
              OCCURS 101 TIMES         VALUE SPACES.
           03 NM-CRG-CTT-EXNO          PIC  X(40)
              OCCURS 101 TIMES         VALUE SPACES.
           03 TX-EMAI-CTT-EXNO         PIC  X(50)
              OCCURS 101 TIMES         VALUE SPACES.
           03 CD-SEQL-DDD-CML          PIC S9(04) COMP-5
              OCCURS 101 TIMES         VALUE ZEROS.
           03 NR-TEL-CTT-EXNO-CML      PIC S9(09) COMP-5
              OCCURS 101 TIMES         VALUE ZEROS.
           03 CD-SEQL-DDD-CELR         PIC S9(04) COMP-5
              OCCURS 101 TIMES         VALUE ZEROS.
           03 NR-TEL-CTT-EXNO-CELR     PIC S9(09) COMP-5
              OCCURS 101 TIMES         VALUE ZEROS.
           03 NM-NM-PSS-CTT-EXNO-N     PIC S9(04) COMP-5
               OCCURS 101 TIMES        VALUE ZEROS.
           03 NM-TX-EMAI-CTT-EXNO-N    PIC S9(04) COMP-5
               OCCURS 101 TIMES        VALUE ZEROS.
           03 NM-CIA-CTT-EXNO-N        PIC S9(04) COMP-5
               OCCURS 101 TIMES        VALUE ZEROS.
           03 NM-CRG-CTT-EXNO-N        PIC S9(04) COMP-5
               OCCURS 101 TIMES        VALUE ZEROS.
           03 CD-SEQL-DDD-CML-N        PIC S9(04) COMP-5
               OCCURS 101 TIMES        VALUE ZEROS.
           03 NR-TEL-CTT-CML-N         PIC S9(04) COMP-5
               OCCURS 101 TIMES        VALUE ZEROS.
           03 CD-SEQL-DDD-CELR-N       PIC S9(04) COMP-5
               OCCURS 101 TIMES        VALUE ZEROS.
           03 NR-TEL-CTT-CELR-N        PIC S9(04) COMP-5
               OCCURS 101 TIMES        VALUE ZEROS.

      *--> Auxiliares Movimentacao Monitoração
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).

      *--> Variavel(is) de controle do cursor(es)
       01  CTL-CURSOR-PSQ.
           03  CD-FIM-CURSOR-PSQ             PIC S9(04) COMP-5 VALUE 0.
               88  FIM-CURSOR-PSQ            VALUE +100.
           03  IN-CURSOR-PSQ-ABTO            PIC  X(01) VALUE 'N'.
               88  CURSOR-PSQ-ABTO           VALUE 'S'.


      *-----------------------------------------------------------------
      *    Books Utilizados
      *-----------------------------------------------------------------

      *--> Book do Container de Erro
       COPY IIBKERRO.

      *--> Book para captura de erro via MSG1
       COPY MSGKWMG1.

      *--> Book para captura de abend cics da bloco codigo MSGKP999
       COPY HLPKW999.

      *--> Book das variáveis auxiliares do tratamento de erro via MSG
       COPY SGTKERR1.

      *--> Book do container de requisicao e resposta deste modulo
       COPY SGTKL035.

      *-----------------------------------------------------------------
      *    Cursores de Pesquisa
      *-----------------------------------------------------------------

           EXEC SQL
           DECLARE CURSOR-PSQ CURSOR
                     WITH ROWSET POSITIONING FOR
           SELECT
                  H.CD_PSS_CTT_EXNO
                , A.NM_PSS_CTT_EXNO
                , A.NM_CIA_CTT_EXNO
                , A.NM_CRG_CTT_EXNO
                , A.TX_EMAI_CTT_EXNO
                , B.CD_SEQL_DDD_CML
                , B.NR_TEL_CTT_CML
                , E.CD_SEQL_DDD_CELR
                , E.NR_TEL_CTT_CELR

           FROM
                DB2SGT.CTT_EXNO_OPR_ETRD H
           LEFT OUTER JOIN
                DB2SGT.PSS_CTT_EXNO A
                 ON H.CD_PSS_CTT_EXNO = A.CD_PSS_CTT_EXNO
                 LEFT OUTER JOIN
                 (
      *--> Recupera telefone comercial da pessoa - INICIO
                SELECT
                   C.CD_PSS_CTT_EXNO
                 , C.CD_SEQL_DDD     AS CD_SEQL_DDD_CML
                 , C.NR_TEL_CTT_EXNO AS NR_TEL_CTT_CML
                FROM
                     DB2SGT.TEL_PSS_CTT_EXNO C
                     INNER JOIN
      *--> Recupera sequencial primeira ocorrência de
      *--> telefone comercial por pessoa - INICIO
                     (
                     SELECT
                       CD_PSS_CTT_EXNO
                       , MIN(NR_SEQL_TEL_EXNO) SEQL
                     FROM
                       DB2SGT.TEL_PSS_CTT_EXNO
                     WHERE
                           CD_TIP_TEL      = :TIP-TEL-CML
                     AND   CD_PSS_CTT_EXNO > 0
                     GROUP BY
                        CD_PSS_CTT_EXNO
                     ) D
      *--> Recupera primeira ocorrência de
      *--> telefone comercial por pessoa - FIM
                     ON
                         C.CD_PSS_CTT_EXNO  = D.CD_PSS_CTT_EXNO
                     AND C.NR_SEQL_TEL_EXNO = D.SEQL
      *--> Recupera telefone comercial da pessoa - FIM
                     WHERE
                         D.CD_PSS_CTT_EXNO > :SGTSL035-CD-PSS-CTT-EXNO
                     ORDER BY
                         C.CD_PSS_CTT_EXNO
                 ) B
                 ON
                   H.CD_PSS_CTT_EXNO = B.CD_PSS_CTT_EXNO
                   LEFT OUTER JOIN
                   (
      *--> Recupera telefone celular da pessoa - INICIO
                     SELECT
                        F.CD_PSS_CTT_EXNO
                      , F.CD_SEQL_DDD       AS CD_SEQL_DDD_CELR
                      , F.NR_TEL_CTT_EXNO   AS NR_TEL_CTT_CELR
                     FROM
                        DB2SGT.TEL_PSS_CTT_EXNO F
                     INNER JOIN
      *--> Recupera sequencial primeira ocorrência de
      *--> telefone comercial por pessoa - INICIO
                     (
                       SELECT
                           CD_PSS_CTT_EXNO
                         , MIN(VALUE (NR_SEQL_TEL_EXNO , 0 )) SEQL
                       FROM
                           DB2SGT.TEL_PSS_CTT_EXNO
                       WHERE
                           CD_TIP_TEL      = :TIP-TEL-CELR
                       AND CD_PSS_CTT_EXNO > 0
                       GROUP BY
                           CD_PSS_CTT_EXNO
                     ) G
      *--> Recupera primeira ocorrência de
      *--> telefone comercial por pessoa - FIM
                       ON
                            F.CD_PSS_CTT_EXNO  = G.CD_PSS_CTT_EXNO
                       AND  F.NR_SEQL_TEL_EXNO = G.SEQL
      *--> Recupera telefone celular da pessoa - FIM
                       WHERE
                            F.CD_PSS_CTT_EXNO >
                                :SGTSL035-CD-PSS-CTT-EXNO
                       ORDER BY
                            F.CD_PSS_CTT_EXNO
                   ) E
                   ON
                        H.CD_PSS_CTT_EXNO = E.CD_PSS_CTT_EXNO
                   WHERE
                        H.CD_CTRA_TIT_VL  =  :SGTSL035-CD-CTRA-TIT-VL
                   AND  H.CD_OPR_ETRD_MC  =  :SGTSL035-CD-OPR-ETRD-MCP
                   AND  H.NR_VRS_OPR_ETRD =  :SGTSL035-NR-VRS-OPR-ETRD
                   AND  H.CD_PSS_CTT_EXNO >  :SGTSL035-CD-PSS-CTT-EXNO

                   ORDER BY
                        H.CD_PSS_CTT_EXNO
           END-EXEC.

      *--> Parâmetros para a utilizacao do DB2
           EXEC SQL
                     INCLUDE SQLCA
           END-EXEC.

       77  CTE-FINAL-SS               PIC X(40) VALUE
                                       '*** S.S. Termina Aqui ***'.
      *-----------------------------------------------------------------
       PROCEDURE                       DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------
           MOVE  0001                  TO  MSG1-CD-SEQL-ERRO

           IF  NOT EXECUCAO-ONLINE  AND
               NOT EXECUCAO-BATCH
               CALL  CICS5000       USING  CICS5000-COMMAREA
           END-IF

      *--> Habilita Testes no Cobolito
       COPY ALMKCBL2.

      *--> Habilita tratamento automatico de Erros DB2
       COPY HLPKE997.

      *--> Habilita captura automatica de Abend e Erros CICS
       COPY HLPKE999.

           PERFORM  100000-INICIAR

           PERFORM  200000-PROCESSAR

           PERFORM  300000-POSTAR-RESPOSTA

           GOBACK
           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-INICIAR                  SECTION.
      *-----------------------------------------------------------------
           MOVE  0002                  TO  MSG1-CD-SEQL-ERRO

           PERFORM  110000-DESCOBRIR-CANAL

           PERFORM  120000-EXCLUIR-CTNR-RPST

           PERFORM  130000-EXCLUIR-CTNR-ERRO

           PERFORM  140000-RECEBER-REQUISICAO

           PERFORM  150000-VALIDAR-PARAMETRO

           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-DESCOBRIR-CANAL          SECTION.
      *-----------------------------------------------------------------
           MOVE  0003                  TO  MSG1-CD-SEQL-ERRO

           SET   ERRO-CANAL            TO  TRUE

           EXEC CICS ASSIGN
              CHANNEL ( NM-CNL )
           END-EXEC

           SET   SEM-ERRO  TO  TRUE
           .
       110000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       120000-EXCLUIR-CTNR-RPST        SECTION.
      *-----------------------------------------------------------------
           MOVE  0004                  TO  MSG1-CD-SEQL-ERRO

           EXEC CICS  DELETE
                      CONTAINER ( 'SGTSL035-RPST' )
                      CHANNEL   ( NM-CNL          )
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
       130000-EXCLUIR-CTNR-ERRO        SECTION.
      *-----------------------------------------------------------------
           MOVE  0005                  TO  MSG1-CD-SEQL-ERRO

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
       130000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       140000-RECEBER-REQUISICAO       SECTION.
      *-----------------------------------------------------------------
           MOVE  0006                  TO  MSG1-CD-SEQL-ERRO

           MOVE  ZEROS                 TO  QT-CCT-LIDO
           MOVE  LENGTH OF SGTSL035-RQSC
                                       TO  QT-CCT-EPRO

           SET   GET-CONTAINER         TO  TRUE
           SET   ERRO-CONTAINER        TO  TRUE
           MOVE  'SGTSL035-RQSC'       TO  NM-CTNR

           EXEC CICS  GET
                      CONTAINER ( NM-CTNR         )
                      CHANNEL   ( NM-CNL          )
                      NODATA
                      FLENGTH   ( QT-CCT-LIDO     )
           END-EXEC

           IF  QT-CCT-LIDO                 NOT EQUAL QT-CCT-EPRO
               SET   ERRO-TAMANHO-AREA
                                       TO  TRUE
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS  GET
                      CONTAINER ( NM-CTNR                 )
                      CHANNEL   ( NM-CNL                  )
                      FLENGTH   ( LENGTH OF SGTSL035-RQSC )
                      INTO      ( SGTSL035-RQSC           )
           END-EXEC

           SET SEM-ERRO        TO TRUE
           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       150000-VALIDAR-PARAMETRO        SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

           MOVE 'N'                    TO SGTSL035-IN-CNTO-PSQ

      *--> Validar Chave da Operação Estruturada
           IF  SGTSL035-CD-CTRA-TIT-VL     LESS ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0001              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' inválido.'       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL035-CD-CTRA-TIT-VL     EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0002              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' não informado.'  TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL035-CD-OPR-ETRD-MCP    NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0003              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada do mercado de capit
      -              'ais não numérico.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL035-CD-OPR-ETRD-MCP    LESS ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0004              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada do mercado de capit
      -              'ais inválido.'   TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL035-CD-OPR-ETRD-MCP    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0005              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada do mercado de capit
      -              'ais não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL035-NR-VRS-OPR-ETRD    LESS  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0006              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação estruturada inválido.
      -              ''                TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL035-NR-VRS-OPR-ETRD    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0007              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação estruturada não infor
      -              'mado.'           TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *--> Validar Pessoa de Contato Externo
           IF  SGTSL035-CD-PSS-CTT-EXNO    LESS  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0059              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da pessoa de contato externo inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       150000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-PROCESSAR                SECTION.
      *-----------------------------------------------------------------
           MOVE  0008                  TO  MSG1-CD-SEQL-ERRO

           PERFORM  970101-ABRE-CURSOR-PSQ

           PERFORM  970102-LER-CURSOR-PSQ

           PERFORM  210000-MONTAR-RESPOSTA
                    VARYING SGTSL035-IC-TAB
                    FROM 0001 BY 0001
                    UNTIL SGTSL035-IC-TAB > SGTSL035-QT-OCR-TAB

           PERFORM  970103-FECHA-CURSOR-PSQ

           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       210000-MONTAR-RESPOSTA          SECTION.
      *-----------------------------------------------------------------
           MOVE  0009                  TO  MSG1-CD-SEQL-ERRO

           MOVE CD-PSS-CTT-EXNO(SGTSL035-IC-TAB)       TO
                             SGTSL035-S-CD-PSS-CTT-EXNO(SGTSL035-IC-TAB)
           MOVE NM-PSS-CTT-EXNO(SGTSL035-IC-TAB)       TO
                             SGTSL035-S-NM-PSS-CTT-EXNO(SGTSL035-IC-TAB)
           MOVE NM-CIA-CTT-EXNO(SGTSL035-IC-TAB)       TO
                        SGTSL035-S-NM-CIA-PSS-CTT-EXNO(SGTSL035-IC-TAB)
           MOVE NM-CRG-CTT-EXNO(SGTSL035-IC-TAB)       TO
                        SGTSL035-S-NM-CRG-PSS-CTT-EXNO(SGTSL035-IC-TAB)
           MOVE TX-EMAI-CTT-EXNO(SGTSL035-IC-TAB)      TO
                        SGTSL035-S-TX-EMAI-CTT-EXNO(SGTSL035-IC-TAB)
           MOVE CD-SEQL-DDD-CML(SGTSL035-IC-TAB)       TO
                        SGTSL035-S-CD-SEQL-DDD-CML(SGTSL035-IC-TAB)
           MOVE NR-TEL-CTT-EXNO-CML(SGTSL035-IC-TAB)   TO
                   SGTSL035-S-NR-TEL-CTT-EXNO-CML(SGTSL035-IC-TAB)
           MOVE CD-SEQL-DDD-CELR(SGTSL035-IC-TAB)      TO
                        SGTSL035-S-CD-SEQL-DDD-CELR(SGTSL035-IC-TAB)
           MOVE NR-TEL-CTT-EXNO-CELR(SGTSL035-IC-TAB)  TO
                   SGTSL035-S-NR-TEL-CTT-EXNO-CEL(SGTSL035-IC-TAB)
           .
       210000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-POSTAR-RESPOSTA          SECTION.
      *-----------------------------------------------------------------
           MOVE  0010                  TO  MSG1-CD-SEQL-ERRO

           SET   PUT-CONTAINER         TO  TRUE
           SET   ERRO-CONTAINER        TO  TRUE
           MOVE  'SGTSL035-RPST'       TO  NM-CTNR

           EXEC CICS  PUT
                      CONTAINER ( NM-CTNR                 )
                      CHANNEL   ( NM-CNL                  )
                      FLENGTH   ( LENGTH OF SGTSL035-RPST )
                      FROM      ( SGTSL035-RPST           )
           END-EXEC

           SET  SEM-ERRO               TO TRUE
           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970101-ABRE-CURSOR-PSQ          SECTION.
      *-----------------------------------------------------------------
           MOVE  0011                  TO  MSG1-CD-SEQL-ERRO

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
           MOVE  0012                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      REG-WS

           EXEC SQL
                     FETCH NEXT ROWSET
                           CURSOR-PSQ FOR 0101 ROWS
               INTO :CD-PSS-CTT-EXNO
                  , :NM-PSS-CTT-EXNO
                  , :NM-CIA-CTT-EXNO
                    :NM-CIA-CTT-EXNO-N
                  , :NM-CRG-CTT-EXNO
                    :NM-CRG-CTT-EXNO-N
                  , :TX-EMAI-CTT-EXNO
                  , :CD-SEQL-DDD-CML
                    :CD-SEQL-DDD-CML-N
                  , :NR-TEL-CTT-EXNO-CML
                    :NR-TEL-CTT-CML-N
                  , :CD-SEQL-DDD-CELR
                    :CD-SEQL-DDD-CELR-N
                  , :NR-TEL-CTT-EXNO-CELR
                    :NR-TEL-CTT-CELR-N
           END-EXEC

           MOVE SQLERRD(3)             TO  QT-REG-SELD

           IF  QT-REG-SELD                 EQUAL 0101
               SET   SGTSL035-TEM-MAIS-REGISTRO
                                       TO  TRUE
               MOVE  0100              TO  QT-REG-SELD
           END-IF

           MOVE  QT-REG-SELD           TO  SGTSL035-QT-OCR-TAB
           MOVE  SQLCODE               TO  CD-FIM-CURSOR-PSQ
           .
       970102-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970103-FECHA-CURSOR-PSQ         SECTION.
      *-----------------------------------------------------------------
           MOVE  0013                  TO  MSG1-CD-SEQL-ERRO

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

           IF  MSG1-CD-ERRO-TS             EQUAL ZEROS
               MOVE  8888              TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS             EQUAL SPACES
               MOVE  'Erro não especificado no módulo.'
                                       TO  MSG1-TX-ERRO-TS
           END-IF

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

      *--> Adiciona Chave da Operação Estruturada
           MOVE  'L035-001-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL035-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'L035-002-CD-OPR-ETRD-MC'
                                       TO  MSGKW999-NM-VRV-MNT
      *--> Verifica se o conteudo e numerico antes de adicionar
           IF  SGTSL035-CD-OPR-ETRD-MCP    NUMERIC
               MOVE  SGTSL035-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP
               MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'L035-003-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL035-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

      *--> Adiciona Outros Campos de Entrada
           MOVE  'L035-004-CD-PSS-CTT-EXNO'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL035-CD-PSS-CTT-EXNO
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

      *--> Grava o erro no Sistema MSG
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

           PERFORM  999999-ENCERRAR
           .
       999000-SAI.
           EXIT.

      *----------------------------------------
      * 999001-TRATA-ABEND-CICS        SECTION.
      * 999002-TRATA-CONDITION-CICS    SECTION.
      * 999003-ADICIONA-VRV-MNT        SECTION.
      *----------------------------------------
       COPY HLPKP999.
      *----------------------------------------
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *----------------------------------------
       COPY SGTKP999.
      *----------------------------------------
      * 999010-TRATA-ERRO-DB2          SECTION.
      *----------------------------------------
       COPY HLPKP997.

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
       COPY MSGKPMG1.

      *----------------------------------------
       999999-ENCERRAR                 SECTION.
      *----------------------------------------

           EXEC CICS
                RETURN
           END-EXEC
           .
       999999-SAI.
           EXIT.

      *---------------------- FIM SGTSL035 -----------------------------
