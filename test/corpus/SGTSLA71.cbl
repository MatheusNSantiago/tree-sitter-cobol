      *-----------------------------------------------------------------
      * Programa..: SGTSLA71
      * Analista..: F2054109 - Cintia Ap Soares Iglesias
      * AUTOR.....: F8779682 - Rosa Aparecida Henriques Foz
      * Data......: 01/02/2017
      * Objetivo..: Listar Operaçoes Pagadoras de uma operação vinculada
      *             DB2SGT.OPR_PG_OTR_OPR
      * Compilacao: Cobol/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 01/02/2017 F8779682  DE 2015/137233 - Implantacao
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTSLA71.

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
                                       '*** SGTSLA71 ***'.
       77  CTE-VERS                    PIC  X(06) VALUE 'VRS001'.

      *--> Book do programa de ambiente - CICS5000
      *    Definido na work para preservar as informacoes
-INC HLPKE01B

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

      *--> Area de trabalho
       01  IC-TAB                      PIC S9(04) COMP-5 VALUE ZEROS.
       01  QT-REG-SELD                 PIC S9(04) COMP-5 VALUE ZEROS.

      *--> Areas Auxiliares para Tabela DB2SGT.OPR_PG_OTR_OPR
       01  OPR-PG-OTR-OPR.
           03 CD-CTRA-OPR-PGDR         PIC S9(09) COMP-5
                                       OCCURS 0101 TIMES  VALUE ZEROS.
           03 CD-OPR-PGDR              PIC S9(11) COMP-3
                                       OCCURS 0101 TIMES  VALUE ZEROS.
           03 NR-VRS-OPR-PGDR          PIC S9(04) COMP-5
                                       OCCURS 0101 TIMES  VALUE ZEROS.
           03 CD-USU-ULT-ATL           PIC  X(08)
                                       OCCURS 0101 TIMES  VALUE SPACES.
           03 TS-ULT-ATL               PIC  X(26)
                                       OCCURS 0101 TIMES  VALUE SPACES.
           03 VL-PGTO                  PIC S9(15)V9(2) COMP-3
                                       OCCURS 0101 TIMES  VALUE ZEROS.

      *--> Variavel(is) de controle do cursor(es)
       01  CTL-CURSOR-PSQ-01.
           03  CD-FIM-CURSOR-PSQ-01    PIC S9(04) COMP-5 VALUE ZEROS.
               88  FIM-CURSOR-PSQ-01              VALUE  +100.
           03  IN-CURSOR-PSQ-ABTO-01   PIC  X(01) VALUE  'N'.
               88  CURSOR-PSQ-ABTO-01             VALUE  'S'.

      *--> Campos utilizados para manipulacao de container
       01  GRP-CONTAINER.
           03  CANAL                   PIC  X(16) VALUE SPACES.
           03  QT-CCT-LIDO             PIC S9(09) COMP-5 VALUE ZEROS.
           03  QT-CCT-EPRO             PIC S9(09) COMP-5 VALUE ZEROS.
           03  VL-EXBR-1               PIC  9(10) VALUE ZEROS.
           03  VL-EXBR-2               PIC  9(10) VALUE ZEROS.
           03  NM-PGM                  PIC  X(08) VALUE SPACES.
           03  NM-CTNR                 PIC  X(16) VALUE SPACES.
           03  NM-ACAO                 PIC  X(03) VALUE SPACES.
               88 GET-CONTAINER                   VALUE 'GET'.
               88 PUT-CONTAINER                   VALUE 'PUT'.
               88 DEL-CONTAINER                   VALUE 'DEL'.

      *--> Indicador do tipos de erro previstos na execucao do modulo
       01  IN-TIP-ERRO                 PIC  9(01) VALUE ZEROS.
           88 SEM-ERRO                            VALUE 0000.
           88 ERRO-CONTAINER                      VALUE 0001.
           88 ERRO-TAMANHO-AREA                   VALUE 0002.
           88 ERRO-CANAL                          VALUE 0003.
           88 ERRO-LOG                            VALUE 0004.

      *--> Book do Container de Erro
-INC IIBKERRO

      *--> Book do container de Servicos (para acionamento de servicos)
-INC IIBKSRVC

      *--> Book para captura de erro via MSG1
-INC MSGKWMG1

      *--> Book para captura de abend cics da bloco codigo MSGKP999
-INC HLPKW999

      *--> Book do container de requisicao e resposta deste modulo
       01  SGTSLA71-DADOS.
-INC SGTKLA71

      *-----------------------------------------------------------------
      *    Cursor de Pesquisa
      *-----------------------------------------------------------------

      *    Listar Operacoes Pagadoras de uma operação vinculada
      *    da Tabela DB2SGT.OPR_PG_OTR_OPR
      *
           EXEC SQL
              DECLARE  CURSOR-PSQ-01  CURSOR
                       WITH ROWSET POSITIONING FOR
               SELECT  A.CD_CTRA_OPR_PGDR
                    ,  A.CD_OPR_PGDR
                    ,  A.NR_VRS_OPR_PGDR
                    ,  A.CD_USU_ULT_ATL
                    ,  A.TS_ULT_ATL
                    ,  A.VL_PGTO
               FROM
                    DB2SGT.OPR_PG_OTR_OPR  A
               WHERE
                    A.CD_CTRA_OPR_VCLD   = :SGTSLA71-CD-CTRA-OPR-VCLD
                AND A.CD_OPR_ETRD_VCLD   = :SGTSLA71-CD-OPR-ETRD-VCLD
                AND A.NR_VRS_OPR_VCLD    = :SGTSLA71-NR-VRS-OPR-VCLD
                AND
                   (A.CD_CTRA_OPR_PGDR > :SGTSLA71-CD-CTRA-OPR-PGDR-E
                   OR
                   (A.CD_CTRA_OPR_PGDR = :SGTSLA71-CD-CTRA-OPR-PGDR-E
                    AND A.CD_OPR_PGDR      > :SGTSLA71-CD-OPR-PGDR-E)
                   OR
                   (A.CD_CTRA_OPR_PGDR = :SGTSLA71-CD-CTRA-OPR-PGDR-E
                    AND
                    A.CD_OPR_PGDR      = :SGTSLA71-CD-OPR-PGDR-E
                    AND
                    A.NR_VRS_OPR_PGDR  = :SGTSLA71-NR-VRS-OPR-PGDR-E))
               ORDER BY
                    A.CD_CTRA_OPR_PGDR
                  , A.CD_OPR_PGDR
                  , A.NR_VRS_OPR_PGDR
           END-EXEC

      *--> Parâmetros para a utilizacao do DB2
           EXEC SQL
                     INCLUDE SQLCA
           END-EXEC.

       01  CTE-FINAL-SS                PIC  X(40) VALUE
                                       '*** S.S. Termina Aqui ***'.

      *-----------------------------------------------------------------
       PROCEDURE        DIVISION.
      *-----------------------------------------------------------------

      *--> Habilita tratamento automatico de erros DB2
-INC HLPKE997
      *--> Habilita captura automatica de abend e erros CICS
-INC HLPKE999

           PERFORM  100000-OBTER-CANAL
           PERFORM  110000-EXCLUIR-CTNR-RPST
           PERFORM  120000-EXCLUIR-CTNR-ERRO
           PERFORM  130000-RECEBER-REQUISICAO

           PERFORM  140000-VALIDAR-PARAMETRO

           PERFORM  200000-PROCESSAR-PESQUISA

           PERFORM  300000-POSTAR-RESPOSTA

           PERFORM  999999-ENCERRAR

           GOBACK
           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-OBTER-CANAL              SECTION.
      *-----------------------------------------------------------------
           MOVE  0001                  TO  MSG1-CD-SEQL-ERRO

           EXEC CICS  ASSIGN
                      CHANNEL ( CANAL                                  )
           END-EXEC
           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-EXCLUIR-CTNR-RPST        SECTION.
      *-----------------------------------------------------------------
           MOVE  0002                  TO  MSG1-CD-SEQL-ERRO

           EXEC CICS  DELETE
                      CONTAINER ( 'SGTSLA71-RPST'                      )
                      CHANNEL   ( CANAL                                )
                      NOHANDLE
           END-EXEC
           IF  NOT (    EIBRESP  EQUAL  DFHRESP(NORMAL      )
                    OR  EIBRESP  EQUAL  DFHRESP(CONTAINERERR) )
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
                      CONTAINER ( 'PRM-ERRO'                           )
                      CHANNEL   ( CANAL                                )
                      NOHANDLE
           END-EXEC
           IF  NOT (    EIBRESP  EQUAL  DFHRESP(NORMAL      )
                    OR  EIBRESP  EQUAL  DFHRESP(CONTAINERERR) )
               PERFORM  999002-TRATA-CONDITION-CICS
           END-IF
           .
       120000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       130000-RECEBER-REQUISICAO       SECTION.
      *-----------------------------------------------------------------
           MOVE  0004                  TO  MSG1-CD-SEQL-ERRO

           MOVE  ZEROS                 TO  QT-CCT-LIDO
           MOVE  LENGTH OF  SGTSLA71-RQSC
                                       TO  QT-CCT-EPRO
           EXEC CICS  GET
                      CONTAINER ( 'SGTSLA71-RQSC'                      )
                      CHANNEL   ( CANAL                                )
                      NODATA
                      FLENGTH   ( QT-CCT-LIDO                          )
           END-EXEC

           IF  QT-CCT-LIDO                 NOT EQUAL  QT-CCT-EPRO
               MOVE  3001              TO  MSG1-CD-ERRO-TS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  QT-CCT-EPRO       TO  VL-EXBR-1
               MOVE  QT-CCT-LIDO       TO  VL-EXBR-2
               STRING
                   'Container RQSC'
                   ', Tamanho Esperado: '  VL-EXBR-1
                   ', Tamanho Recebido: '  VL-EXBR-2
                   DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
               END-STRING
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS  GET
                      CONTAINER ( 'SGTSLA71-RQSC'                      )
                      CHANNEL   ( CANAL                                )
                      FLENGTH   ( LENGTH OF SGTSLA71-RQSC              )
                      INTO      ( SGTSLA71-RQSC                        )
           END-EXEC
           .
       130000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       140000-VALIDAR-PARAMETRO        SECTION.
      *-----------------------------------------------------------------
           MOVE  0005                  TO  MSG1-CD-SEQL-ERRO

           IF  SGTSLA71-CD-CTRA-OPR-VCLD   NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0001              TO  MSG1-CD-ERRO-TS
               MOVE 'Código da carteira da operação vinculada inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSLA71-CD-OPR-ETRD-VCLD   NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0002              TO  MSG1-CD-ERRO-TS
               MOVE 'Código da operação estruturada vinculada inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSLA71-NR-VRS-OPR-VCLD    NOT POSITIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0003              TO  MSG1-CD-ERRO-TS
               MOVE 'N mero da versão da operação vinculada inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-PROCESSAR-PESQUISA       SECTION.
      *-----------------------------------------------------------------
           MOVE  0006                  TO  MSG1-CD-SEQL-ERRO

           PERFORM  971100-ABRE-CURSOR-PSQ-01

           PERFORM  971200-LER-CURSOR-PSQ-01

           PERFORM  971300-FECHA-CURSOR-PSQ-01

           PERFORM  210000-MONTAR-LISTA-PAGADORAS
                    VARYING                IC-TAB
                    FROM                   0001 BY 0001
                    UNTIL                  IC-TAB     GREATER
                                           SGTSLA71-QT-OCR
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       210000-MONTAR-LISTA-PAGADORAS   SECTION.
      *-----------------------------------------------------------------
           MOVE  0008                  TO  MSG1-CD-SEQL-ERRO

           MOVE  CD-CTRA-OPR-PGDR                            (IC-TAB)
             TO  SGTSLA71-CD-CTRA-OPR-PGDR                   (IC-TAB)

           MOVE  CD-OPR-PGDR                                 (IC-TAB)
             TO  SGTSLA71-CD-OPR-PGDR                        (IC-TAB)

           MOVE  NR-VRS-OPR-PGDR                             (IC-TAB)
             TO  SGTSLA71-NR-VRS-OPR-PGDR                    (IC-TAB)

           MOVE  CD-USU-ULT-ATL                              (IC-TAB)
             TO  SGTSLA71-CD-USU-ULT-ATL                     (IC-TAB)

           MOVE  TS-ULT-ATL                                  (IC-TAB)
             TO  SGTSLA71-TS-ULT-ATL                         (IC-TAB)

           MOVE  VL-PGTO                                     (IC-TAB)
             TO  SGTSLA71-VL-PGTO                            (IC-TAB)
           .
       210000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-POSTAR-RESPOSTA          SECTION.
      *-----------------------------------------------------------------
           MOVE  0009                  TO  MSG1-CD-SEQL-ERRO

           EXEC CICS  PUT
                      CONTAINER ( 'SGTSLA71-RPST'                      )
                      CHANNEL   ( CANAL                                )
                      FLENGTH   ( LENGTH OF SGTSLA71-RPST              )
                      FROM      ( SGTSLA71-RPST                        )
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

           INITIALIZE                      OPR-PG-OTR-OPR
                                           REPLACING NUMERIC BY  ZEROS

           EXEC SQL
                FETCH NEXT ROWSET  CURSOR-PSQ-01
                      FOR 0101 ROWS
                 INTO :CD-CTRA-OPR-PGDR
                    , :CD-OPR-PGDR
                    , :NR-VRS-OPR-PGDR
                    , :CD-USU-ULT-ATL
                    , :TS-ULT-ATL
                    , :VL-PGTO
           END-EXEC

           MOVE  SQLERRD(3)            TO  QT-REG-SELD

           IF  QT-REG-SELD                 EQUAL  0101
               SET   SGTSLA71-TEM-MAIS-REGISTRO
                                       TO  TRUE
               MOVE  0100              TO  QT-REG-SELD
           END-IF

           MOVE  QT-REG-SELD           TO  SGTSLA71-QT-OCR

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
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------

           MOVE  'SGTSLA71-CD-CTRA-OPR-VCLD'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSLA71-CD-CTRA-OPR-VCLD
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSLA71-CD-OPR-ETRD-VCLD'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSLA71-CD-OPR-ETRD-VCLD
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSLA71-NR-VRS-OPR-VCLD'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSLA71-NR-VRS-OPR-VCLD
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSLA71-CD-CTRA-OPR-PGDR-E'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSLA71-CD-CTRA-OPR-PGDR-E
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSLA71-CD-OPR-PGDR-E'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSLA71-CD-OPR-PGDR-E
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTSLA71-NR-VRS-OPR-PGDR-E'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSLA71-NR-VRS-OPR-PGDR-E
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

      *--> Grava o erro no MSG
           PERFORM  MSG1-NTF-MSG

           MOVE  MSG1-CD-ERRO-TS       TO  IIBKERRO-CD-ERRO
           MOVE  MSG1-TX-ERRO-TS       TO  IIBKERRO-TX-ERRO

           EXEC CICS  PUT
                      CONTAINER ( 'PRM-ERRO'                           )
                      CHANNEL   ( CANAL                                )
                      FROM      ( IIBKERRO-PRM-ERRO                    )
                      FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO          )
                      NOHANDLE
           END-EXEC

           PERFORM  999999-ENCERRAR
           .
       999000-SAI.
           EXIT.

      * ---------------------------------------
      * 999001-TRATA-ABEND-CICS        SECTION.
      * 999002-TRATA-CONDITION-CICS    SECTION.
      * 999003-ADICIONA-VRV-MNT        SECTION.
      * ---------------------------------------
-INC HLPKP999
      * ---------------------------------------
      * 999010-TRATA-ERRO-DB2          SECTION.
-INC HLPKP997
      * ---------------------------------------
      * MSG1-NTF-MSG                   SECTION.
-INC MSGKPMG1
      * ---------------------------------------
       999999-ENCERRAR                 SECTION.
      * ---------------------------------------
           EXEC CICS
                RETURN
           END-EXEC.
      *-----------------------------------------------------------------