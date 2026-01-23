      *-----------------------------------------------------------------
      * Programa..: SGTSL071
      * Analista..: F2054109 - Cintia Ap Soares Iglesias
      * Autor.....: F8650584 - Roberto Fernandes Lima
      * Data......: 26/01/2017
      * Objetivo..: Listar Operações Vinculadas para Pagamento
      *             e Dados Vinculados
      *             DB2SGT.OPR_PG_OTR_OPR
      *             DB2SGT.OPR_ETRD_MCP
      *             DB2SGT.CLI_OPR_ETRD
      * Compilacao: Cobol/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 26/01/2017 F8650584  DE 2015/137233 - Implantacao
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTSL071.

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
                                       '*** S.S. Comeca Aqui ***'.
       77  CTE-PROG                    PIC  X(18) VALUE
                                       '*** SGTSL071 ***'.
       77  CTE-VERS                    PIC  X(06) VALUE 'VRS001'.

      *--> Codigo do Tipo de Funcao do Cliente:
      *    0001 - Cliente Emissor
       77  K-CD-TIP-FUC-CLI            PIC  S9(04) COMP-5 VALUE 0001.

      *--> Book do programa de ambiente - CICS5000
      *    Definido na work para preservar as informacoes
-INC HLPKE01B

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

      *--> Area de trabalho
       01  IC-TAB-01                   PIC S9(04) COMP-5 VALUE ZEROS.

       01  QT-REG-SELD                 PIC S9(04) COMP-5 VALUE ZEROS.

      *--> Auxiliares para movimentação na monitoração de variáveis.
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).

      *--> Indicadores de nulidades

      *--> Areas Auxiliares para Tabela DB2SGT.OPR_PG_OTR_OPR
       01  OPR-PG-OTR-OPR.
           03 CD-CTRA-OPR-VCLD         PIC S9(09) COMP-5
                                       OCCURS 0101 TIMES  VALUE ZEROS.
           03 CD-OPR-ETRD-VCLD         PIC S9(11) COMP-3
                                       OCCURS 0101 TIMES  VALUE ZEROS.
           03 NR-VRS-OPR-VCLD          PIC S9(04) COMP-5
                                       OCCURS 0101 TIMES  VALUE ZEROS.
           03 CD-USU-ULT-ATL           PIC  X(08)
                                       OCCURS 0101 TIMES  VALUE SPACES.
           03 TS-ULT-ATL               PIC  X(26)
                                       OCCURS 0101 TIMES  VALUE SPACES.
           03 VL-PGTO                  PIC S9(15)V9(2) COMP-3
                                       OCCURS 0101 TIMES  VALUE ZEROS.

      *--> Areas Auxiliares para Tabela DB2SGT.OPR_ETRD_MCP
       01  OPR-ETRD-MCP.
           03  CD-TIT-OPR-ETRD         PIC  X(12)
                                       OCCURS 0101 TIMES  VALUE SPACES.
           03  NR-DVS-TIT              PIC S9(04) COMP-5
                                       OCCURS 0101 TIMES  VALUE ZEROS.

      *--> Areas Auxiliares para Tabela DB2SGT.CLI_OPR_ETRD
       01  CLI-OPR-ETRD.
           03  CD-CLI-EMT-OPR          PIC S9(09) COMP-5
                                       OCCURS 0101 TIMES  VALUE ZEROS.


      *--> Variavel(is) de controle do cursor(es)
       01  CTL-CURSOR-PSQ-01.
           03  CD-FIM-CURSOR-PSQ-01    PIC S9(04) COMP-5 VALUE ZEROS.
               88  FIM-CURSOR-PSQ-01              VALUE  +100.
           03  IN-CURSOR-PSQ-ABTO-01   PIC  X(01) VALUE  'N'.
               88  CURSOR-PSQ-ABTO-01             VALUE  'S'.

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
       01  SGTSL071-DADOS.
-INC SGTKL071

      *-----------------------------------------------------------------
      *    Cursores de Pesquisa
      *-----------------------------------------------------------------

      *--> Cursor de Pesquisa 01
      *    Listar Operacoes Vinculadas para Pagamento e Dados Vinculados
      *    na Tabela DB2SGT.OPR_PG_OTR_OPR
      *    para o Codigo do Tipo de Funcao do Cliente:
      *    0001 - Cliente emissor
           EXEC SQL
              DECLARE  CURSOR-PSQ-01  CURSOR
                       WITH ROWSET POSITIONING FOR
               SELECT  A.CD_CTRA_OPR_VCLD
                    ,  A.CD_OPR_ETRD_VCLD
                    ,  A.NR_VRS_OPR_VCLD
                    ,  A.CD_USU_ULT_ATL
                    ,  A.TS_ULT_ATL
                    ,  A.VL_PGTO
                    ,  B.CD_TIT_OPR_ETRD
                    ,  B.NR_DVS_TIT
                    ,  C.CD_CLI
                 FROM  DB2SGT.OPR_PG_OTR_OPR  A
           INNER JOIN  DB2SGT.OPR_ETRD_MCP  B
                   ON  A.CD_CTRA_OPR_VCLD   = B.CD_CTRA_TIT_VL
                  AND  A.CD_OPR_ETRD_VCLD   = B.CD_OPR_ETRD_MCP
                  AND  A.NR_VRS_OPR_VCLD    = B.NR_VRS_OPR_ETRD
           INNER JOIN  DB2SGT.CLI_OPR_ETRD  C
                   ON  A.CD_CTRA_OPR_VCLD   = C.CD_CTRA_TIT_VL
                  AND  A.CD_OPR_ETRD_VCLD   = C.CD_OPR_ETRD_MCP
                  AND  A.NR_VRS_OPR_VCLD    = C.NR_VRS_OPR_ETRD

                WHERE  A.CD_CTRA_OPR_PGDR   = :SGTSL071-CD-CTRA-OPR-PGDR
                  AND  A.CD_OPR_PGDR        = :SGTSL071-CD-OPR-PGDR
                  AND  A.NR_VRS_OPR_PGDR    = :SGTSL071-NR-VRS-OPR-PGDR
                  AND  C.CD_TIP_FUC_CLI     = :K-CD-TIP-FUC-CLI
                  AND
                      (A.CD_CTRA_OPR_VCLD >
                                        :SGTSL071-CD-CTRA-OPR-VCLD-PAG
                   OR (A.CD_CTRA_OPR_VCLD =
                                        :SGTSL071-CD-CTRA-OPR-VCLD-PAG
                  AND A.CD_OPR_ETRD_VCLD >
                                        :SGTSL071-CD-OPR-ETRD-VCLD-PAG)
                   OR (A.CD_CTRA_OPR_VCLD =
                                        :SGTSL071-CD-CTRA-OPR-VCLD-PAG
                  AND  A.CD_OPR_ETRD_VCLD =
                                        :SGTSL071-CD-OPR-ETRD-VCLD-PAG
                  AND  A.NR_VRS_OPR_VCLD >
                                        :SGTSL071-NR-VRS-OPR-VCLD-PAG))

             ORDER BY  A.CD_CTRA_OPR_VCLD
                     , A.CD_OPR_ETRD_VCLD
                     , A.NR_VRS_OPR_VCLD
           END-EXEC


      *--> Parâmetros para a utilizacao do DB2
           EXEC SQL
                     INCLUDE SQLCA
           END-EXEC.


       01  CTE-FINAL-SS                PIC  X(40) VALUE
                                       '*** S.S. Termina Aqui ***'.

      *-----------------------------------------------------------------
       PROCEDURE                       DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------
           MOVE  0001                  TO  MSG1-CD-SEQL-ERRO

      *--> Habilita tratamento automatico de erros DB2
-INC HLPKE997

      *--> Habilita captura automatica de abend e erros CICS
-INC HLPKE999

           PERFORM  100000-OBTER-CANAL
           PERFORM  110000-EXCLUIR-CTNR-RPST
           PERFORM  120000-EXCLUIR-CTNR-ERRO
           PERFORM  130000-RECEBER-REQUISICAO

           PERFORM  140000-VALIDAR-PARAMETRO

           PERFORM  200000-LISTAR-TIPO-IMPORTANCIA

           PERFORM  300000-POSTAR-RESPOSTA

           PERFORM  999999-ENCERRAR

           GOBACK
           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-OBTER-CANAL              SECTION.
      *-----------------------------------------------------------------
           MOVE  0002                  TO  MSG1-CD-SEQL-ERRO

           EXEC CICS  ASSIGN
                      CHANNEL ( NM-CNL )
           END-EXEC
           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-EXCLUIR-CTNR-RPST        SECTION.
      *-----------------------------------------------------------------
           MOVE  0003                  TO  MSG1-CD-SEQL-ERRO

           EXEC CICS  DELETE
                      CONTAINER ( 'SGTSL071-RPST' )
                      CHANNEL   ( NM-CNL          )
                      RESP      ( MSG1-RESP1      )
                      RESP2     ( MSG1-RESP2      )
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
       110000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       120000-EXCLUIR-CTNR-ERRO        SECTION.
      *-----------------------------------------------------------------
           MOVE  0004                  TO  MSG1-CD-SEQL-ERRO

           EXEC CICS  DELETE
                      CONTAINER ( 'PRM-ERRO' )
                      CHANNEL   ( NM-CNL     )
                      RESP      ( MSG1-RESP1      )
                      RESP2     ( MSG1-RESP2      )
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
       120000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       130000-RECEBER-REQUISICAO       SECTION.
      *-----------------------------------------------------------------
           MOVE  0005                  TO  MSG1-CD-SEQL-ERRO

           MOVE  ZEROS                 TO  QT-CCT-LIDO
           MOVE  LENGTH OF  SGTSL071-RQSC
                                       TO  QT-CCT-EPRO
           EXEC CICS  GET
                      CONTAINER ( 'SGTSL071-RQSC' )
                      CHANNEL   ( NM-CNL          )
                      NODATA
                      FLENGTH   ( QT-CCT-LIDO     )
           END-EXEC

           IF  QT-CCT-LIDO                 NOT EQUAL  QT-CCT-EPRO
               SET ERRO-TAMANHO-AREA TO TRUE
               PERFORM  999000-ERRO
           END-IF

           EXEC CICS  GET
                      CONTAINER ( 'SGTSL071-RQSC'         )
                      CHANNEL   ( NM-CNL                  )
                      FLENGTH   ( LENGTH OF SGTSL071-RQSC )
                      INTO      ( SGTSL071-RQSC           )
           END-EXEC
           .
       130000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       140000-VALIDAR-PARAMETRO        SECTION.
      *-----------------------------------------------------------------
           MOVE  0006                  TO  MSG1-CD-SEQL-ERRO

      *--> Validar Operação Pagadora
           IF  SGTSL071-CD-CTRA-OPR-PGDR   EQUAL ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0496              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira da operação pagadora não inform
      -              'ado.'            TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTSL071-CD-CTRA-OPR-PGDR   NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0497              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira da operação pagadora inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTSL071-CD-OPR-PGDR        NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0498              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada pagadora não numéri
      -              'co.'             TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTSL071-CD-OPR-PGDR        EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0499              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada pagadora não inform
      -              'ado.'            TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTSL071-CD-OPR-PGDR        NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0500              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada pagadora inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTSL071-NR-VRS-OPR-PGDR    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0501              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação pagadora não informad
      -              'o.'              TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTSL071-NR-VRS-OPR-PGDR    NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0502              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação pagadora inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

      *--> Valida a Operação Vinculada
           IF  SGTSL071-CD-CTRA-OPR-VCLD-PAG
                                           LESS  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0504              TO  MSG1-CD-ERRO-TS
               MOVE 'Código da carteira da operação vinculada inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTSL071-CD-OPR-ETRD-VCLD-PAG
                                           NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0505              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada vinculada não numér
      -              'ico.'            TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTSL071-CD-OPR-ETRD-VCLD-PAG
                                           LESS  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0507              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada vinculada inválido.
      -              ''                TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTSL071-NR-VRS-OPR-VCLD-PAG
                                           LESS  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0509              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação vinculada inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-LISTAR-TIPO-IMPORTANCIA  SECTION.
      *-----------------------------------------------------------------
           MOVE  0007                  TO  MSG1-CD-SEQL-ERRO

           PERFORM  971100-ABRE-CURSOR-PSQ-01

           PERFORM  971200-LER-CURSOR-PSQ-01

           PERFORM  971300-FECHA-CURSOR-PSQ-01

           PERFORM  210000-MONTAR-LISTA-VINCULADA
                    VARYING                IC-TAB-01
                    FROM                   0001 BY 0001
                    UNTIL                  IC-TAB-01  GREATER
                                           SGTSL071-QT-OCR
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       210000-MONTAR-LISTA-VINCULADA   SECTION.
      *-----------------------------------------------------------------
           MOVE  0008                  TO  MSG1-CD-SEQL-ERRO

           MOVE  CD-CTRA-OPR-VCLD                            (IC-TAB-01)
             TO  SGTSL071-CD-CTRA-OPR-VCLD                   (IC-TAB-01)

           MOVE  CD-OPR-ETRD-VCLD                            (IC-TAB-01)
             TO  SGTSL071-CD-OPR-ETRD-VCLD                   (IC-TAB-01)

           MOVE  NR-VRS-OPR-VCLD                             (IC-TAB-01)
             TO  SGTSL071-NR-VRS-OPR-VCLD                    (IC-TAB-01)

           MOVE  CD-USU-ULT-ATL                              (IC-TAB-01)
             TO  SGTSL071-CD-USU-ULT-ATL                     (IC-TAB-01)

           MOVE  TS-ULT-ATL                                  (IC-TAB-01)
             TO  SGTSL071-TS-ULT-ATL                         (IC-TAB-01)

           MOVE  VL-PGTO                                     (IC-TAB-01)
             TO  SGTSL071-VL-PGTO                            (IC-TAB-01)

           MOVE  CD-TIT-OPR-ETRD                             (IC-TAB-01)
             TO  SGTSL071-CD-TIT-OPR-ETRD                    (IC-TAB-01)

           MOVE  NR-DVS-TIT                                  (IC-TAB-01)
             TO  SGTSL071-NR-DVS-TIT                         (IC-TAB-01)

           MOVE  CD-CLI-EMT-OPR                              (IC-TAB-01)
             TO  SGTSL071-CD-CLI-EMT-OPR                     (IC-TAB-01)

           .
       210000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-POSTAR-RESPOSTA          SECTION.
      *-----------------------------------------------------------------
           MOVE  0009                  TO  MSG1-CD-SEQL-ERRO

           EXEC CICS  PUT
                      CONTAINER ( 'SGTSL071-RPST'         )
                      CHANNEL   ( NM-CNL                  )
                      FLENGTH   ( LENGTH OF SGTSL071-RPST )
                      FROM      ( SGTSL071-RPST           )
           END-EXEC
           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       971100-ABRE-CURSOR-PSQ-01       SECTION.
      *-----------------------------------------------------------------
           MOVE  0010                  TO  MSG1-CD-SEQL-ERRO

           EXEC SQL
                     OPEN CURSOR-PSQ-01
           END-EXEC

           MOVE  'S'                   TO  IN-CURSOR-PSQ-ABTO-01
           .
       971100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       971200-LER-CURSOR-PSQ-01        SECTION.
      *-----------------------------------------------------------------
           MOVE  0011                  TO  MSG1-CD-SEQL-ERRO

           MOVE  ZEROS                 TO  QT-REG-SELD

           MOVE  SPACES                TO  OPR-PG-OTR-OPR
                                           OPR-ETRD-MCP
                                           CLI-OPR-ETRD

           INITIALIZE                      OPR-PG-OTR-OPR
                                           OPR-ETRD-MCP
                                           CLI-OPR-ETRD
                                           REPLACING NUMERIC BY  ZEROS

           EXEC SQL
                FETCH NEXT ROWSET  CURSOR-PSQ-01
                      FOR 0101 ROWS
                 INTO :CD-CTRA-OPR-VCLD
                    , :CD-OPR-ETRD-VCLD
                    , :NR-VRS-OPR-VCLD
                    , :CD-USU-ULT-ATL
                    , :TS-ULT-ATL
                    , :VL-PGTO
                    , :CD-TIT-OPR-ETRD
                    , :NR-DVS-TIT
                    , :CD-CLI-EMT-OPR
           END-EXEC

           MOVE  SQLERRD(3)            TO  QT-REG-SELD

           IF  QT-REG-SELD                 EQUAL  0101
               SET   SGTSL071-TEM-MAIS-REGISTRO
                                       TO  TRUE
               MOVE  0100              TO  QT-REG-SELD
           END-IF

           MOVE  QT-REG-SELD           TO  SGTSL071-QT-OCR

           MOVE  SQLCODE               TO  CD-FIM-CURSOR-PSQ-01
           .
       971200-SAI.
           EXIT.

      *-----------------------------------------------------------------
       971300-FECHA-CURSOR-PSQ-01      SECTION.
      *-----------------------------------------------------------------
           MOVE  0012                  TO  MSG1-CD-SEQL-ERRO

           MOVE  'N'                   TO  IN-CURSOR-PSQ-ABTO-01

           EXEC SQL
                     CLOSE CURSOR-PSQ-01
           END-EXEC
           .
       971300-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999000-ERRO                     SECTION.
      *-----------------------------------------------------------------

           IF  IN-CURSOR-PSQ-ABTO-01       EQUAL  'S'
               PERFORM  971300-FECHA-CURSOR-PSQ-01
           END-IF

           IF  MSG1-CD-ERRO-TS             EQUAL ZEROS
               MOVE  8888              TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS             EQUAL SPACES
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

      *--> Adiciona as variáveis de entrada para monitoração
           MOVE 'SGTSL071-CD-CTRA-OPR-PGDR'   TO  MSGKW999-NM-VRV-MNT
           MOVE SGTSL071-CD-CTRA-OPR-PGDR     TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL071-CD-OPR-PGDR'        TO  MSGKW999-NM-VRV-MNT
      *------> Verifica se o conteudo e numerico antes de adicionar
           IF  SGTSL071-CD-OPR-PGDR            IS NUMERIC
               MOVE SGTSL071-CD-OPR-PGDR      TO  CD-OPR-ETRD-MCP
               MOVE CD-OPR-ETRD-MCP-TX        TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE 'Valor não numérico'      TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL071-NR-VRS-OPR-PGDR'    TO  MSGKW999-NM-VRV-MNT
           MOVE SGTSL071-NR-VRS-OPR-PGDR      TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL071-CD-CTRA-OPR-VCLD-PAG'
                                              TO  MSGKW999-NM-VRV-MNT
           MOVE SGTSL071-CD-CTRA-OPR-VCLD-PAG TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL071-CD-OPR-ETRD-VCLD-PAG'
                                              TO  MSGKW999-NM-VRV-MNT
      *------> Verifica se o conteudo e numerico antes de adicionar
           IF  SGTSL071-CD-OPR-ETRD-VCLD-PAG            IS NUMERIC
               MOVE SGTSL071-CD-OPR-ETRD-VCLD-PAG
                                              TO  CD-OPR-ETRD-MCP
               MOVE CD-OPR-ETRD-MCP-TX        TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE 'Valor não numérico'      TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL071-NR-VRS-OPR-VCLD-PAG'
                                              TO  MSGKW999-NM-VRV-MNT
           MOVE SGTSL071-NR-VRS-OPR-VCLD-PAG  TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

      *--> Grava as Variaveis Monitoradas no sistema MSG - Mensageria
           PERFORM  MSG1-NTF-MSG

           MOVE  MSG1-CD-ERRO-TS       TO  IIBKERRO-CD-ERRO
           MOVE  MSG1-TX-ERRO-TS       TO  IIBKERRO-TX-ERRO

      *--> Grava a Ocorrencia de Erro no Container de Erro
           EXEC CICS  PUT
                      CONTAINER ( 'PRM-ERRO'                  )
                      CHANNEL   ( NM-CNL                      )
                      FROM      ( IIBKERRO-PRM-ERRO           )
                      FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                      NOHANDLE
           END-EXEC

      *--> Encerra a Execução do Módulo
           PERFORM  999999-ENCERRAR
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

      *-----------------------------------------------------------------
       999999-ENCERRAR                 SECTION.
      *-----------------------------------------------------------------

           EXEC CICS
                RETURN
           END-EXEC
           .
       999999-SAI.
           EXIT.

      *---------------------- FIM SGTSL071 -----------------------------