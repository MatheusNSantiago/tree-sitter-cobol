      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSP079
      * ANALISTA..: F9507141 Vicente de Paulo Tavares de Souza
      * AUTOR.....: F0950413 António Francisco Mendonça Silva
      * DATA......: 31/01/2017
      * OBJETIVO..: Persistência - Consultar Taxa Quebra Condição Ativa
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 05/02/2019 F2804421 DE 2016/145195 - Alteração monitora-
      *                   ção de variávels SBDATAF.
      *                  -Alterar cod msg 67 por 51.
      *                  - Inclusão de SET para variáveis de monitora-
      *                     ção de GET e PUT container deste módulo.
      *        31.01.2017 - F0950413 - 2015/137233 - Implantação
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSP079.

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
               '*** SGTSP079 ***'.
       77  CTE-VERS                    PIC X(06) VALUE 'VRS001'.
       77  SBDATAF                     PIC X(08) VALUE 'SBDATAF'.

      *-----------------------------------------------------------------
      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informacoes
-INC HLPKE01B
      *-----------------------------------------------------------------
       LOCAL-STORAGE SECTION.
      *-----------------------------------------------------------------
       01 QT-REG-PSQ                   PIC S9(04)     COMP-5  VALUE 0.
      *--> Book do container de erro
-INC IIBKERRO
      *--> Book para captura de erro via MSG1
-INC MSGKWMG1
      *--> Variaveis para tratamento de Abend e Condition
-INC HLPKW999
      *--> Variaveis para tratamento de CONTAINER
-INC SGTKERR1

      *--> Container de erro

      *--> Areas do SBDATAF
       01  AREA-SBDATAF.
          03  FUC                      PIC  X(003) VALUE 'F01'.
          03  ARG01-F01                PIC  9(008) VALUE 0.
          03  ARG02-F01                PIC  9(001) VALUE 0.

       01  WORKAREA                    PIC X(4095) VALUE SPACES.
       01  AREA-RED REDEFINES WORKAREA.
           05  AREA-TAM                PIC 9(0008) COMP.
           05  FILLER                  PIC X(4071).
           05  AREA-FIM                PIC X(0020).

      *--> Data de trabalho
       01  AUX-DT-INC-TAXA-QBRA        PIC X(10)   VALUE '0001-01-01'.
       01  AUX-DT-INC-TAXA-QBRA-R      REDEFINES
           AUX-DT-INC-TAXA-QBRA.
           03 ANO                      PIC 9(04).
           03 FILLER-1                 PIC X.
           03 MES                      PIC 9(02).
           03 FILLER-2                 PIC X.
           03 DIA                      PIC 9(02).

      *--> Auxiliares para movimentação na monitoração de variáveis.
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).

      *-----------------------------------------------------------------
      *--> Book do programa
-INC SGTKP079
      *-----------------------------------------------------------------

      *--> Subrotinas utilizadas DB2
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

       01  CTE-FINAL-SS                  PIC X(40)      VALUE
                    '*** S.S. TERMINA AQUI ***'.

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------

      *--> Habilita tratamento automatico de erros DB2
-INC HLPKE997
      *--> Habilita captura automatica de abend e erros CICS
-INC HLPKE999
           MOVE CTE-PROG(5:8)          TO NM-PGM

           PERFORM 100000-DESCOBRIR-CANAL
           PERFORM 110000-EXCLUIR-CTNR-RPST
           PERFORM 120000-EXCLUIR-CTNR-ERRO
           PERFORM 130000-RECEBER-REQUISICAO
           PERFORM 140000-VALIDAR-PARAMETRO

           PERFORM 200000-PROCESSAR-SOLICITACAO

           PERFORM 300000-POSTA-RESPOSTA
           PERFORM 999999-ENCERRAR
           .

      *-----------------------------------------------------------------
       100000-DESCOBRIR-CANAL          SECTION.
      *-----------------------------------------------------------------
           MOVE 0001                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS ASSIGN
                CHANNEL ( NM-CNL )
           END-EXEC
           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-EXCLUIR-CTNR-RPST        SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS DELETE
                CONTAINER ( 'SGTSP079-RPST' )
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
           MOVE LENGTH OF SGTSP079-RQSC     TO QT-CCT-EPRO
           SET  ERRO-CONTAINER         TO TRUE
           SET  GET-CONTAINER          TO TRUE
           MOVE 'SGTSP079-RQSC'        TO NM-CTNR

           EXEC CICS GET
                CONTAINER ( NM-CTNR         )
                CHANNEL   ( NM-CNL          )
                NODATA
                FLENGTH   ( QT-CCT-LIDO     )
           END-EXEC

           IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
               SET  ERRO-TAMANHO-AREA         TO TRUE
               PERFORM 999000-ERRO
           END-IF

           EXEC CICS GET
              CONTAINER ( NM-CTNR                 )
              CHANNEL   ( NM-CNL                  )
              FLENGTH   ( LENGTH OF SGTSP079-RQSC )
              INTO      ( SGTSP079-RQSC           )
           END-EXEC

           SET SEM-ERRO                TO TRUE
           .
       130000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       140000-VALIDAR-PARAMETRO        SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

           IF  SGTSP079-CD-CTRA-TIT-VL EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO  TRUE
               MOVE 0002               TO  MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de título e valores mobiliários
      -        'não informado.'        TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTSP079-CD-OPR-ETRD-MCP NOT NUMERIC
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0003               TO  MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -             ' numérico.'       TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTSP079-CD-OPR-ETRD-MCP EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0005               TO  MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -             ' informado.'      TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTSP079-NR-VRS-OPR-ETRD EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0007               TO  MSG1-CD-ERRO-TS
               MOVE 'Número da versão da operação não informado.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTSP079-CD-TIP-CND-NEGD EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0210               TO  MSG1-CD-ERRO-TS
               MOVE 'Código do tipo de condição negociada não informado.
      -             ''                 TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF

           IF  SGTSP079-DT-TAXA-QBRA NOT EQUAL SPACES AND ZEROS
               PERFORM 141000-VALIDAR-DATA
           ELSE
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0555               TO MSG1-CD-ERRO-TS
               MOVE 'Data da taxa de quebra de fundo não informada.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-ERRO
           END-IF
           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       141000-VALIDAR-DATA             SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                    TO MSG1-CD-SEQL-ERRO

           IF DIA OF SGTSP079-DT-TAXA-QBRA-R NOT NUMERIC OR
              MES OF SGTSP079-DT-TAXA-QBRA-R NOT NUMERIC OR
              ANO OF SGTSP079-DT-TAXA-QBRA-R NOT NUMERIC
              SET  ERRO-PROGRAMADO    TO TRUE
              MOVE 0556               TO  MSG1-CD-ERRO-TS
              MOVE 'Data da taxa de quebra de fundo inválida.'
                                      TO MSG1-TX-ERRO-TS
              PERFORM 999000-ERRO
           END-IF

           MOVE SGTSP079-DT-TAXA-QBRA  TO ARG01-F01
      *    Validar data no formato Gregoriano
           MOVE 4                      TO ARG02-F01
           MOVE 'F01'                  TO FUC
           MOVE 4095                   TO AREA-TAM
           MOVE 'SBDATAF -WRKAREA-FIM' TO AREA-FIM

           CALL SBDATAF  USING  FUC
                                WORKAREA
                                ARG01-F01
                                ARG02-F01

      *--> RETURN-CODE do SBDATA
      *    22 -  Conteudo do Argumento Nao Numerico
      *    24 -  Data Informada Invalida
      *    46 -  Data Fora da Faixa de 01/01/0001 a 31/12/9999

           IF  RETURN-CODE             NOT EQUAL  ZEROS
               SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE  RETURN-CODE       TO  MSG1-CD-RTN-PGM-ACND
               MOVE  SBDATAF           TO  MSG1-NM-PGM-ACND

               MOVE 'FUC'              TO  MSGKW999-NM-VRV-MNT
               MOVE  FUC               TO  MSGKW999-VL-VRV-MNT
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
                   MOVE 0556           TO  MSG1-CD-ERRO-TS
                   MOVE 'Data da taxa de quebra de fundo inválida.'
                                       TO MSG1-TX-ERRO-TS
               ELSE
                   MOVE  0051          TO  MSG1-CD-ERRO-TS
                   MOVE  'Erro no tratamento de datas.'
                                       TO  MSG1-TX-RTN-PGM-ACND
                                           MSG1-TX-ERRO-TS
               END-IF

               PERFORM 999000-ERRO
           END-IF

           MOVE CORR SGTSP079-DT-TAXA-QBRA-R
                                       TO AUX-DT-INC-TAXA-QBRA-R
           .
       141000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-PROCESSAR-SOLICITACAO    SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

           EXEC SQL
             SELECT COUNT (*)
               INTO :QT-REG-PSQ
               FROM DB2SGT.CND_NEGD_OPR
             WHERE CD_CTRA_TIT_VL      =  :SGTSP079-CD-CTRA-TIT-VL
               AND CD_OPR_ETRD_MCP     =  :SGTSP079-CD-OPR-ETRD-MCP
               AND NR_VRS_OPR_ETRD     =  :SGTSP079-NR-VRS-OPR-ETRD
               AND CD_TIP_CND_NEGD     =  :SGTSP079-CD-TIP-CND-NEGD
               AND DT_INC_VLD_CND     <=  :AUX-DT-INC-TAXA-QBRA
               AND DT_FIM_VLD_CND     >=  :AUX-DT-INC-TAXA-QBRA
           END-EXEC.

           IF  QT-REG-PSQ  > 0
               SET SGTSP079-TAXA-EXISTENTE  TO TRUE
           END-IF
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-POSTA-RESPOSTA           SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

           SET  ERRO-CONTAINER         TO TRUE
           SET  PUT-CONTAINER          TO TRUE
           MOVE 'SGTSP079-RPST'        TO NM-CTNR

           EXEC CICS PUT
                CONTAINER ( NM-CTNR                 )
                CHANNEL   ( NM-CNL                  )
                FLENGTH   ( LENGTH OF SGTSP079-RPST )
                FROM      ( SGTSP079-RPST           )
           END-EXEC

           SET SEM-ERRO                TO TRUE
           .
       300000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       999000-ERRO                     SECTION.
      *----------------------------------------------------------------*
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO

           IF  MSG1-CD-ERRO-TS EQUAL ZEROS
               MOVE 8888                TO MSG1-CD-ERRO-TS
           END-IF
           IF  MSG1-TX-ERRO-TS EQUAL SPACES
               MOVE 'Erro não especificado no módulo.'
                                        TO MSG1-TX-ERRO-TS
           END-IF

           PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           .
       999000-ERRO-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------

      *Verifica se o tipo de erro gerado possui tratamento interno
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


           MOVE 'SGTSP079-CD-CTRA-TIT-VL'   TO MSGKW999-NM-VRV-MNT
           MOVE  SGTSP079-CD-CTRA-TIT-VL    TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSP079-CD-OPR-ETRD-MCP'  TO MSGKW999-NM-VRV-MNT
      *------> Verifica se o conteudo e numerico antes de adicionar
           IF  SGTSP079-CD-OPR-ETRD-MCP     IS NUMERIC
               MOVE  SGTSP079-CD-OPR-ETRD-MCP  TO  CD-OPR-ETRD-MCP
               MOVE  CD-OPR-ETRD-MCP-TX     TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico'   TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSP079-NR-VRS-OPR-ETRD'  TO MSGKW999-NM-VRV-MNT
           MOVE  SGTSP079-NR-VRS-OPR-ETRD   TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSP079-CD-TIP-CND-NEGD'  TO MSGKW999-NM-VRV-MNT
           MOVE  SGTSP079-CD-TIP-CND-NEGD   TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSP079-DT-TAXA-QBRA'     TO MSGKW999-NM-VRV-MNT
           MOVE  SGTSP079-DT-TAXA-QBRA      TO MSGKW999-VL-VRV-MNT
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
      *---------------------------------------*
-INC SGTKP999
      * --------------------------------------*
      * 999010-TRATA-ERRO-DB2          SECTION.
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