      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSL079
      * ANALISTA..: F9507141 Vicente de Paulo Tavares de Souza
      * AUTOR.....: F0950413 António Francisco Mendonça Silva
      * DATA......: 12/01/2017
      * OBJETIVO..: Lista da tabela CND_NEGD_OPR
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS003 10/02/2023 F9158927-Inclusão campo SGTSL079-CD-FMA-APRC-S
      * VRS002 05/02/2019 F2804421 DE 2016/145195 - Alteração monitora-
      *                   ção de variávels SBDATAF.
      *                   - Inclusão de SET para variáveis de monitora-
      *                     ção de GET e PUT container deste módulo.
      * VRS001 28/08/2017 F2804421 DE 2016/145195 - Mudança de nomes:
      *          Tabela TAXA_QBRA_CND_NEGD para CND_NEGD_OPR e atributos
      *          ref a DT-INC-TAXA-QBRA para DT-INC-VLD-CND
      *                DT-FIM-TAXA-QBRA para DT-FIM-VLD-CND
      *         - MUDANÇA DE QTDE OCORRÊNCIAS DE 200 PARA 100.
      *
      * VRS001 15.05.2017 F9188847 DE 2015/137233 - Inclusão dos campos:
      *                   CD-ULT-TIP-CND-NEGD e CD-TIP-CND-NEGD-S
      *                   no cursor para paginação
      *        12.01.2017 - F0950413 - 2015/137233 - Implantação
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSL079.

      *-----------------------------------------------------------------
       ENVIRONMENT                     DIVISION.
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
       77  CTE-INICIO-SS               PIC X(35) VALUE
               '*** STORAGE SECTION COMECA AQUI ***'.
       77  CTE-PROG                    PIC X(18) VALUE
               '*** SGTSL079 ***'.
       77  CTE-VERS                    PIC X(06) VALUE 'VRS003'.
       77  SBDATAF                     PIC X(08) VALUE 'SBDATAF'.

      *-----------------------------------------------------------------
      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informacoes
-INC HLPKE01B
      *-----------------------------------------------------------------
       LOCAL-STORAGE SECTION.
      *-----------------------------------------------------------------
      *--> Book do container de erro
-INC IIBKERRO
      *--> Book para captura de erro via MSG1
-INC MSGKWMG1
      *--> Variaveis para tratamento de Abend e Condition
-INC HLPKW999
      *--> Variaveis para tratamento de CONTAINER
-INC SGTKERR1

      *--> Area de trabalho
       01 QT-REG-SELD                  PIC S9(4) COMP-5 VALUE 0.

       01  TIP-CND-INI                 PIC S9(04)  COMP-5 VALUE 0.
       01  TIP-CND-FIM                 PIC S9(04) COMP-5 VALUE 9999.

      *--> Auxiliares Movimentacao Monitoração
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).

      *--> Campos utilizados para leitura
       01  REG-WS.
           03 CD-TIP-CND-NEGD          PIC S9(04)  COMP-5
              OCCURS 101 TIMES              VALUE 0.
           03 DT-INC-VLD-CND           PIC X(10)
              OCCURS 101 TIMES.
           03 CD-USU-ULT-ATL           PIC  X(08)
              OCCURS 101 TIMES              VALUE ' '.
           03 TS-ULT-ATL               PIC  X(26)
              OCCURS 101 TIMES              VALUE ' '.
           03 CD-TIP-CBT               PIC S9(04)      COMP-5
              OCCURS 101 TIMES              VALUE 0.
           03 DT-FIM-VLD-CND           PIC  X(10)
              OCCURS 101 TIMES              VALUE ' '.
           03 IN-OBGD-CND              PIC  X(01)
              OCCURS 101 TIMES              VALUE ' '.
           03 PC-TAXA-QBRA-FNDO        PIC S9(4)V9(10) COMP-3
              OCCURS 101 TIMES              VALUE 0.
           03 CD-FMA-APRC              PIC S9(04)      COMP-5
              OCCURS 101 TIMES              VALUE 0.

      *--> Indicadores de nulidades

      *--> Variavel(is) de controle do cursor(es)
       01  CTL-CURSOR-PSQ.
           03  CD-FIM-CURSOR-PSQ            PIC S9(04) COMP-5 VALUE 0.
               88  FIM-CURSOR-PSQ            VALUE +100.
           03  IN-CURSOR-PSQ-ABTO           PIC  X(01) VALUE 'N'.
               88  CURSOR-PSQ-ABTO          VALUE 'S'.

      *--> Areas do SBDATAF
       01  AREA-SBDATAF.
          03  FUNCAO                   PIC  X(003) VALUE 'F01'.
          03  ARG01-F01                PIC  9(008).
          03  ARG02-F01                PIC  9(001) VALUE 0.

       01  WORKAREA                    PIC X(4095).
       01  AREA-RED REDEFINES WORKAREA.
           05  AREA-TAM                PIC 9(0008) COMP.
           05  FILLER                  PIC X(4071).
           05  AREA-FIM                PIC X(0020).

       01  AUX-CD-RETORNO              PIC -9(004) VALUE ZEROS.

      *--> Data de trabalho
       01  AUX-DT-INC-VLD-CND         PIC X(10)   VALUE '0001-01-01'.
       01  AUX-DT-INC-VLD-CND-R       REDEFINES
           AUX-DT-INC-VLD-CND.
           03 ANO                      PIC 9(04).
           03 FILLER-1                 PIC X.
           03 MES                      PIC 9(02).
           03 FILLER-2                 PIC X.
           03 DIA                      PIC 9(02).

      *--> Data trabalho para lista.
       01  AUX-DT                      PIC X(10) VALUE SPACES.
       01  AUX-DT-R       REDEFINES    AUX-DT.
           03 DIA                      PIC 9(02).
           03 FILLER-3                 PIC X.
           03 MES                      PIC 9(02).
           03 FILLER-4                 PIC X.
           03 ANO                      PIC 9(04).

      *--> Subrotinas utilizadas DB2
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

      *-----------------------------------------------------------------
-INC SGTKL079
      *-----------------------------------------------------------------
      *    Lista a taxa de quebra na condição negociada
      *    A paginação acontece com mesmo código do tipo de condição
      *    negociada ou quebra
           EXEC SQL
             DECLARE CURSOR-PSQ CURSOR
                     WITH ROWSET POSITIONING FOR
             SELECT CD_TIP_CND_NEGD
                  , DT_INC_VLD_CND
                  , CD_USU_ULT_ATL
                  , TS_ULT_ATL
                  , CD_TIP_CBT
                  , DT_FIM_VLD_CND
                  , IN_OBGD_CND
                  , PC_TAXA_QBRA_FNDO
                  , CD_FMA_APRC
               FROM DB2SGT.CND_NEGD_OPR
              WHERE CD_CTRA_TIT_VL      =  :SGTSL079-CD-CTRA-TIT-VL
                AND CD_OPR_ETRD_MCP     =  :SGTSL079-CD-OPR-ETRD-MCP
                AND NR_VRS_OPR_ETRD     =  :SGTSL079-NR-VRS-OPR-ETRD
                AND (CD_TIP_CND_NEGD   >=  :TIP-CND-INI
                AND  CD_TIP_CND_NEGD   <=  :TIP-CND-FIM)
                AND (CD_TIP_CND_NEGD    >  :SGTSL079-CD-ULT-TIP-CND-NEGD
                OR  (CD_TIP_CND_NEGD    =  :SGTSL079-CD-ULT-TIP-CND-NEGD
                      AND DT_INC_VLD_CND  > :AUX-DT-INC-VLD-CND))
               ORDER BY
                     CD_TIP_CND_NEGD, DT_INC_VLD_CND
           END-EXEC.

        01  CTE-FINAL-SS                  PIC X(40)      VALUE
                    '*** S.S. TERMINA AQUI ***'.

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.

      * Habilita tratamento automatico de erros DB2
-INC HLPKE997
      * Habilita captura automatica de abend e erros CICS
-INC HLPKE999

           PERFORM 100000-DESCOBRIR-CANAL
           PERFORM 110000-EXCLUIR-CTNR-RPST
           PERFORM 120000-EXCLUIR-CTNR-ERRO
           PERFORM 130000-RECEBER-REQUISICAO
           PERFORM 140000-VALIDAR-PARAMETRO

           PERFORM 970101-ABRE-CURSOR-PSQ
           PERFORM 970102-LER-CURSOR-PSQ
           PERFORM 970103-FECHA-CURSOR-PSQ

           PERFORM 200000-PROCESSA-PESQUISA
                   VARYING SGTSL079-IC-TAB FROM 1 BY 1
                     UNTIL SGTSL079-IC-TAB > SGTSL079-QT-OCR-TAB

           PERFORM 300000-POSTA-RESPOSTA
           PERFORM 999999-ENCERRAR
           GOBACK
           .
       000000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       100000-DESCOBRIR-CANAL          SECTION.
      *-----------------------------------------------------------------
           MOVE 0001                   TO MSG1-CD-SEQL-ERRO

           SET   ERRO-CANAL            TO   TRUE
           EXEC CICS ASSIGN
                CHANNEL ( NM-CNL )
           END-EXEC
           SET   SEM-ERRO              TO TRUE

           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-EXCLUIR-CTNR-RPST        SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS DELETE
                CONTAINER ( 'SGTSL079-RPST' )
                CHANNEL   ( NM-CNL          )
                NOHANDLE
           END-EXEC
           IF  NOT (EIBRESP EQUAL DFHRESP(NORMAL)   OR
               EIBRESP EQUAL DFHRESP(CONTAINERERR) )
               PERFORM 999002-TRATA-CONDITION-CICS
           END-IF
           .
       110000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       120000-EXCLUIR-CTNR-ERRO        SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS DELETE
                CONTAINER ( 'PRM-ERRO' )
                CHANNEL   ( NM-CNL     )
                NOHANDLE
           END-EXEC
           IF  NOT (EIBRESP EQUAL DFHRESP(NORMAL)   OR
               EIBRESP EQUAL DFHRESP(CONTAINERERR) )
               PERFORM 999002-TRATA-CONDITION-CICS
           END-IF
           .
       120000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       130000-RECEBER-REQUISICAO       SECTION.
      *-----------------------------------------------------------------
           MOVE 0004                   TO MSG1-CD-SEQL-ERRO

           MOVE ZEROS                       TO QT-CCT-LIDO
           MOVE LENGTH OF SGTSL079-RQSC     TO QT-CCT-EPRO

           SET  ERRO-CONTAINER         TO TRUE
           SET  GET-CONTAINER          TO TRUE
           MOVE 'SGTSL079-RQSC'        TO NM-CTNR

           EXEC CICS GET
                CONTAINER ( NM-CTNR         )
                CHANNEL   ( NM-CNL          )
                NODATA
                FLENGTH   ( QT-CCT-LIDO     )
           END-EXEC

           IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
               SET   ERRO-TAMANHO-AREA TO TRUE
               PERFORM 999000-ERRO
           END-IF

           EXEC CICS GET
              CONTAINER ( NM-CTNR                 )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSL079-RQSC )
              INTO      ( SGTSL079-RQSC           )
           END-EXEC

           SET SEM-ERRO                TO TRUE
           .
       130000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       140000-VALIDAR-PARAMETRO        SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

           MOVE 'N'                    TO SGTSL079-IN-CNTO-PSQ

           IF  SGTSL079-CD-CTRA-TIT-VL EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE 0002               TO  MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de título e valores mobiliários
      -        'não informado.'        TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTSL079-CD-OPR-ETRD-MCP NOT NUMERIC
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0003               TO  MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -        ' numérico.'      TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTSL079-CD-OPR-ETRD-MCP EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0005               TO  MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -             ' informado.'      TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTSL079-NR-VRS-OPR-ETRD EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0007               TO  MSG1-CD-ERRO-TS
               MOVE 'Número da versão da operação não informado.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTSL079-CD-TIP-CND-NEGD > ZEROS
               MOVE SGTSL079-CD-TIP-CND-NEGD
                                       TO TIP-CND-INI
                                          TIP-CND-FIM
                                          SGTSL079-CD-ULT-TIP-CND-NEGD
           END-IF

           IF  SGTSL079-DT-INC-VLD-CND EQUAL SPACES OR ZEROS
               MOVE '0001-01-01'       TO AUX-DT-INC-VLD-CND
           ELSE
               PERFORM 141000-VALIDAR-DATA
           END-IF
           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       141000-VALIDAR-DATA             SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                    TO MSG1-CD-SEQL-ERRO

           IF DIA OF SGTSL079-DT-INC-VLD-CND-R NOT NUMERIC OR
              MES OF SGTSL079-DT-INC-VLD-CND-R NOT NUMERIC OR
              ANO OF SGTSL079-DT-INC-VLD-CND-R NOT NUMERIC
              SET  ERRO-PROGRAMADO    TO TRUE
              MOVE 0187               TO  MSG1-CD-ERRO-TS
              MOVE 'Data início validade da condição inválida.'
                                      TO MSG1-TX-ERRO-TS
              PERFORM 999000-ERRO
           END-IF

           MOVE SGTSL079-DT-INC-VLD-CND
                                       TO ARG01-F01
           MOVE 4                      TO ARG02-F01
           MOVE 'F01'                  TO FUNCAO
           MOVE 4095                   TO AREA-TAM
           MOVE 'SBDATAF -WRKAREA-FIM' TO AREA-FIM

           CALL SBDATAF  USING  FUNCAO
                                WORKAREA
                                ARG01-F01
                                ARG02-F01

      *--> Verificacao do Retorno do SBDATAF para Validacao
      *    no Formato Gregoriano (Função = 01 e Argumento 02 = 0004)
      *    ARG01 = 99999999 - Data Informada Inválida
      *    ARG01 = 88888888 - Demais Erros na Rotina SBDATAF
      *--> RETURN-CODE do SBDATA
      *    22 -  Conteudo do Argumento Nao Numerico
      *    24 -  Data Informada Invalida
      *    46 -  Data Fora da Faixa de 01/01/0001 a 31/12/9999

           IF  RETURN-CODE             NOT EQUAL  ZEROS
               SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE  RETURN-CODE       TO  MSG1-CD-RTN-PGM-ACND
               MOVE  SBDATAF           TO  MSG1-NM-PGM-ACND

               MOVE 'FUNCAO'           TO  MSGKW999-NM-VRV-MNT
               MOVE  FUNCAO            TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'AREA-TAM'         TO  MSGKW999-NM-VRV-MNT
               MOVE  AREA-TAM          TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'AREA-FIM'         TO  MSGKW999-NM-VRV-MNT
               MOVE  AREA-FIM          TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'ARG01-F01'        TO  MSGKW999-NM-VRV-MNT
               MOVE  ARG01-F01         TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'ARG02-F01'        TO  MSGKW999-NM-VRV-MNT
               MOVE  ARG02-F01         TO  MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT


               IF  RETURN-CODE         EQUAL  0022 OR 0024 OR 0046
                   MOVE 0049           TO  MSG1-CD-ERRO-TS
                   MOVE 'Data informada inválida.'
                                       TO MSG1-TX-ERRO-TS
               ELSE
                   MOVE  0051          TO  MSG1-CD-ERRO-TS
                   MOVE  'Erro no tratamento de datas.'
                                       TO  MSG1-TX-RTN-PGM-ACND
                                           MSG1-TX-ERRO-TS
               END-IF

               PERFORM 999000-ERRO
           ELSE
               MOVE CORR SGTSL079-DT-INC-VLD-CND-R
                                       TO AUX-DT-INC-VLD-CND-R
               MOVE '-'                TO FILLER-1
                                          FILLER-2
            END-IF

           .
       141000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-PROCESSA-PESQUISA        SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

           MOVE CD-TIP-CND-NEGD   (SGTSL079-IC-TAB)         TO
                SGTSL079-CD-TIP-CND-NEGD-S   (SGTSL079-IC-TAB)

           MOVE DT-INC-VLD-CND (SGTSL079-IC-TAB) TO     AUX-DT
           MOVE CORR AUX-DT-R          TO
                SGTSL079-DT-INC-VLD-CND-S-R (SGTSL079-IC-TAB)

           MOVE CD-USU-ULT-ATL    (SGTSL079-IC-TAB)         TO
                SGTSL079-CD-USU-ULT-ATL-S    (SGTSL079-IC-TAB)

           MOVE TS-ULT-ATL        (SGTSL079-IC-TAB)         TO
                SGTSL079-TS-ULT-ATL-S        (SGTSL079-IC-TAB)

           MOVE CD-TIP-CBT        (SGTSL079-IC-TAB)         TO
                SGTSL079-CD-TIP-CBT-S        (SGTSL079-IC-TAB)

           MOVE DT-FIM-VLD-CND (SGTSL079-IC-TAB) TO     AUX-DT
           MOVE CORR AUX-DT-R          TO
                SGTSL079-DT-FIM-VLD-CND-S-R (SGTSL079-IC-TAB)

           MOVE IN-OBGD-CND       (SGTSL079-IC-TAB)        TO
                SGTSL079-IN-OBGD-CND-S       (SGTSL079-IC-TAB)

           MOVE PC-TAXA-QBRA-FNDO (SGTSL079-IC-TAB)        TO
                SGTSL079-PC-TAXA-QBRA-FNDO-S (SGTSL079-IC-TAB)

           MOVE CD-FMA-APRC (SGTSL079-IC-TAB)        TO
                SGTSL079-CD-FMA-APRC-S       (SGTSL079-IC-TAB)
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-POSTA-RESPOSTA           SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

           SET  ERRO-CONTAINER         TO TRUE
           SET  PUT-CONTAINER          TO TRUE
           MOVE 'SGTSL079-RPST'        TO NM-CTNR

           EXEC CICS PUT
                CONTAINER ( NM-CTNR                 )
                CHANNEL   ( NM-CNL                  )
                FLENGTH   ( LENGTH OF SGTSL079-RPST )
                FROM      ( SGTSL079-RPST           )
           END-EXEC

           SET SEM-ERRO                TO TRUE
           .
       300000-SAI.
           EXIT.


      *-----------------------------------------------------------------
       970101-ABRE-CURSOR-PSQ          SECTION.
      *-----------------------------------------------------------------
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO

           EXEC SQL
                OPEN CURSOR-PSQ
           END-EXEC
           MOVE 'S'                    TO IN-CURSOR-PSQ-ABTO
           .
       970101-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970102-LER-CURSOR-PSQ           SECTION.
      *-----------------------------------------------------------------
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE REG-WS
           EXEC SQL
               FETCH NEXT ROWSET
                          CURSOR-PSQ FOR 0101 ROWS
               INTO :CD-TIP-CND-NEGD
                  , :DT-INC-VLD-CND
                  , :CD-USU-ULT-ATL
                  , :TS-ULT-ATL
                  , :CD-TIP-CBT
                  , :DT-FIM-VLD-CND
                  , :IN-OBGD-CND
                  , :PC-TAXA-QBRA-FNDO
                  , :CD-FMA-APRC
           END-EXEC

           MOVE SQLERRD(3)             TO QT-REG-SELD
           IF  QT-REG-SELD EQUAL 0101
               SET SGTSL079-TEM-MAIS-REGISTRO TO TRUE
               MOVE 0100               TO QT-REG-SELD
           END-IF
           MOVE QT-REG-SELD            TO SGTSL079-QT-OCR-TAB
           MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ
           .
       970102-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970103-FECHA-CURSOR-PSQ         SECTION.
      *-----------------------------------------------------------------
           MOVE 0011                   TO MSG1-CD-SEQL-ERRO

           MOVE 'N'                    TO IN-CURSOR-PSQ-ABTO
           EXEC SQL
                CLOSE CURSOR-PSQ
           END-EXEC
           .
       970103-SAI.
           EXIT.

      *----------------------------------------------------------------*
       999000-ERRO                     SECTION.
      *----------------------------------------------------------------*
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

           IF MSG1-CD-ERRO-TS EQUAL ZEROS
              MOVE 8888                TO MSG1-CD-ERRO-TS
           END-IF
           IF MSG1-TX-ERRO-TS EQUAL SPACES
              MOVE 'Erro não especificado no módulo.'
                                       TO MSG1-TX-ERRO-TS
           END-IF

           IF  IN-CURSOR-PSQ-ABTO  EQUAL  'S'
               PERFORM 970103-FECHA-CURSOR-PSQ
           END-IF

           PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           .
       999000-ERRO-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------

           MOVE 'SGTSL079-CD-CTRA-TIT-VL'   TO MSGKW999-NM-VRV-MNT
           MOVE  SGTSL079-CD-CTRA-TIT-VL    TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL079-CD-OPR-ETRD-MCP'  TO MSGKW999-NM-VRV-MNT
           IF    SGTSL079-CD-OPR-ETRD-MCP IS NUMERIC
                 MOVE  SGTSL079-CD-OPR-ETRD-MCP  TO CD-OPR-ETRD-MCP
                 MOVE CD-OPR-ETRD-MCP-TX    TO MSGKW999-VL-VRV-MNT
           ELSE
                 MOVE  'Valor não numérico.'     TO MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL079-NR-VRS-OPR-ETRD'  TO MSGKW999-NM-VRV-MNT
           MOVE  SGTSL079-NR-VRS-OPR-ETRD   TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL079-CD-TIP-CND-NEGD'  TO MSGKW999-NM-VRV-MNT
           MOVE  SGTSL079-CD-TIP-CND-NEGD   TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL079-CD-ULT-TIP-CND-NEGD' TO MSGKW999-NM-VRV-MNT
           MOVE  SGTSL079-CD-ULT-TIP-CND-NEGD  TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL079-DT-INC-VLD-CND' TO MSGKW999-NM-VRV-MNT
           MOVE  SGTSL079-DT-INC-VLD-CND  TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

      * --> grava o erro no msg
           PERFORM MSG1-NTF-MSG
           MOVE MSG1-CD-ERRO-TS             TO IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS             TO IIBKERRO-TX-ERRO

           EXEC CICS PUT
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

      * --------------------------------------*
      * 999001-TRATA-ABEND-CICS        SECTION.
      * 999002-TRATA-CONDITION-CICS    SECTION.
      * 999003-ADICIONA-VRV-MNT        SECTION.
      * 999004-ALINHA-NUMERO-ESQUERDA  SECTION.
      * --------------------------------------*
-INC HLPKP999
      * --------------------------------------*
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      * --------------------------------------*
-INC SGTKP999
      * --------------------------------------*
      * 999010-TRATA-ERRO-DB2          SECTION.
      * --------------------------------------*
-INC HLPKP997
      * --------------------------------------*
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
      * --------------------------------------*
-INC MSGKPMG1
      * --------------------------------------*
       999999-ENCERRAR                 SECTION.
      * --------------------------------------*
           EXEC CICS
                RETURN
           END-EXEC
           .
       999999-SAI.
           EXIT.