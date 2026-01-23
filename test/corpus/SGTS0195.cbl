      *-----------------------------------------------------------------
      * PROGRAMA..: SGTS0195
      * ANALISTA..: F2054109 CINTIA IGLESIAS
      * AUTOR.....: F2054109 CINTIA IGLESIAS
      * DATA......: 19/10/2017
      * OBJETIVO..: Cancelar alteração de Nota Técnica da Operação
      *             Estruturada
      * COMPILACAO: COBOL/CICS
      *-----------------------------------------------------------------
      * VRS001 20/02/2018 F8785286 DE 2016/145195 - Acerto dos códigos
      *          das mensagens para mensageria.
      *        09/11/2017 F2054109 DE 2016/145195 - Implantação
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID. SGTS0195.
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *----------------------------------------------------------------*
       DATA DIVISION.
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*
       77  CTE-INICIO-SS                 PIC  X(040)  VALUE
                    '*** S.S. COMEÇA AQUI ***'.

       77  CTE-PROG                      PIC  X(018)  VALUE
                    '*** SGTS0195 ***'.
       77  CTE-VERS                      PIC  X(006)  VALUE 'VRS001'.

      * Módulos acionados
       77  SGTSPE14                      PIC  X(008) VALUE 'SGTSPE14'.
       77  SGTSE059                      PIC  X(008) VALUE 'SGTSE059'.
       77  SGTSA055                      PIC  X(008) VALUE 'SGTSA055'.
       77  SGTSL056                      PIC  X(008) VALUE 'SGTSL056'.
       77  SGTSI056                      PIC  X(008) VALUE 'SGTSI056'.
       77  SGTSE056                      PIC  X(008) VALUE 'SGTSE056'.
       77  SBTIMEDT                      PIC  X(008) VALUE 'SBTIMEDT'.
      *----------------------------------------------------------------*
       LOCAL-STORAGE                   SECTION.
      *----------------------------------------------------------------*
      * Auxiliares de Data para validao
       01 DT-AUX-DMA                     PIC  X(10) VALUE ' '.
       01 DT-AUX-DMA-R                REDEFINES
             DT-AUX-DMA                    .
          02 DIA                         PIC  9(2).
          02 BARRA-1                     PIC  X(1).
          02 MES                         PIC  9(2).
          02 BARRA-2                     PIC  X(1).
          02 ANO                         PIC  9(4).

       01 GDA-DT-APVC-DMA                PIC  X(10) VALUE ' '.
       01 GDA-DT-EMS-DMA                 PIC  X(10) VALUE ' '.

      * Data AnoMesDia Aprov
       01 DT-APVC-INFD                    PIC  X(8) VALUE ' '.
       01 DT-APVC-INFD-R                REDEFINES DT-APVC-INFD.
          02 ANO                         PIC  X(4).
          02 MES                         PIC  X(2).
          02 DIA                         PIC  X(2).

       01 DT-NOTA-INFD                    PIC  X(8) VALUE ' '.
       01 DT-NOTA-INFD-R                REDEFINES DT-NOTA-INFD.
          02 ANO                         PIC  X(4).
          02 MES                         PIC  X(2).
          02 DIA                         PIC  X(2).

       01 DT-AUX-08                      PIC  X(8) VALUE ' '.
       01 DT-AUX-08-R                REDEFINES DT-AUX-08.
          02 DIA                         PIC  X(2).
          02 MES                         PIC  X(2).
          02 ANO                         PIC  X(4).
      *-----------------------------------------------------------------
      * AREA PARA RECUPERAR TIMESTAMP E DATA
      *-----------------------------------------------------------------
       01  DADOS-SBTIMEDT.
           03  WK-CD-FUC              PIC X(03)      VALUE 'F01'.
           03  WK-AREA-TRB.
               05  WK-AREA-TRB-TAM    PIC S9(9) COMP VALUE 1024.
               05  WK-AREA-TRB-DADOS  PIC  X(1024)   VALUE ' '.
      *        X'0002' - DD.MM.AAAA-HH:MM:SS.CCCCCCXXXX
           03  WK-FMT-TS              PIC S9(4) COMP VALUE 1.
      *         X'001A'(26)
           03  WK-TAM-TS              PIC S9(4) COMP VALUE 30.
           03  WK-TS-FMTD.
               05  ANO                      PIC  9(4).
               05  FILLER                   PIC  X(1).
               05  MES                      PIC  9(2).
               05  FILLER                   PIC  X(1).
               05  DIA                      PIC  9(2).
               05  FILLER                   PIC  X(1).
               05  HORA                     PIC  9(2).
               05  FILLER                   PIC  X(1).
               05  MINUTO                   PIC  9(2).
               05  FILLER                   PIC  X(1).
               05  SEGUNDO                  PIC  9(2).
               05  FILLER                   PIC  X(1).
               05  MICRO-SEGUNDO            PIC  9(6).
               05  FILLER                   PIC  X(4).

       01 IC-TAB                            PIC S9(4) COMP-5.

      * Book dos Containers e das subrotinas chamadas
      *----------------------------------------------------------------*

      * Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1
      * Book para captura de abend cics da bloco codigo MSGKP999
-INC HLPKW999
      * Book para captura de erro via MSG1
-INC MSGKWMG1
      * Book do container de requisicao e resposta deste modulo
       01 SGTS0195-DADOS.
-INC SGTK0195
       01 SGTSPE14-DADOS.
-INC SGTKPE14

-INC SGTKE059

-INC SGTKA055

-INC SGTKL056

-INC SGTKI056

-INC SGTKE056

      * Book do Container de Erro
-INC IIBKERRO
      * Book do container de Servicos (para acionamento de servicos)
-INC IIBKSRVC
      * Book dos dados do usurio
-INC IIBKOPRD

       01  CTE-FINAL-SS                  PIC X(40)      VALUE
                    '*** S.S. TERMINA AQUI ***'.
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       000000-ROTINA-PRINCIPAL         SECTION.
      *----------------------------------------------------------------*

           PERFORM 100000-INICIAR

           PERFORM 200000-PROCESSAR

           PERFORM 999999-ENCERRAR

           GOBACK
           .
       000000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       100000-INICIAR                  SECTION.
      *----------------------------------------------------------------*
           MOVE 0001                   TO MSG1-CD-SEQL-ERRO

      *    Ativa a captura automatica de abend do CICS.
      *    WHENEVER CICS
-INC HLPKE999

           PERFORM 970000-LER-CANAL

      *    Define o nome do programa p/ fins manipulacao container
           MOVE 'SGTS0195'             TO NM-PGM

      *    Limpa a resposta e erro antes de executar o modulo
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO

           PERFORM 971000-LER-REQUISICAO

           PERFORM 974000-LER-DADOS-OPRD

           PERFORM 110000-VALIDAR-REQUISICAO
           .
       100000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       110000-VALIDAR-REQUISICAO       SECTION.
      *----------------------------------------------------------------*
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO

      *    Valida campo a campo da requisicao
           IF  SGTS0195-NR-NOTA-TCN-OPR  <= ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0182               TO MSG1-CD-ERRO-TS
               MOVE 'Numero Nota tecnica operacao invalido.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0195-CD-ARQ-OPR-ETRD-NOVO  <= ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0150               TO MSG1-CD-ERRO-TS
               MOVE 'Código arquivo da operação estruturada inválido.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0195-CD-UOR-DRTA-OGNL-ANT  <= ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0304               TO MSG1-CD-ERRO-TS
               MOVE 'Codigo UOR diretoria original inválido.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0195-CD-REF-NOTA-TCN-ANT EQUAL SPACES
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0284               TO MSG1-CD-ERRO-TS
               MOVE 'Codigo referencia Nota tecnica inválido.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0195-DT-EMS-NOTA-ANT EQUAL SPACES
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0305               TO MSG1-CD-ERRO-TS
               MOVE 'Data emissao Nota inválido.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           ELSE
               MOVE SGTS0195-DT-EMS-NOTA-ANT TO DT-AUX-08
               MOVE CORR DT-AUX-08-R   TO DT-AUX-DMA-R
               MOVE '.'                TO BARRA-1 BARRA-2
               MOVE DT-AUX-DMA         TO GDA-DT-EMS-DMA
           END-IF

           IF  SGTS0195-DT-APVC-NOTA-ANT EQUAL SPACES
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0306               TO MSG1-CD-ERRO-TS
               MOVE 'Data aprovação Nota inválido.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           ELSE
               MOVE SGTS0195-DT-APVC-NOTA-ANT TO DT-AUX-08
               MOVE CORR DT-AUX-08-R   TO DT-AUX-DMA-R
               MOVE '.'                TO BARRA-1 BARRA-2
               MOVE DT-AUX-DMA         TO GDA-DT-APVC-DMA
           END-IF

           IF  SGTS0195-CD-ETR-DCS-ANT   <= ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0307               TO MSG1-CD-ERRO-TS
               MOVE 'Codigo estrutura decisoria inválido.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0195-CD-TIP-DOC-TCN-ANT   <= ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0308               TO MSG1-CD-ERRO-TS
               MOVE 'Codigo tipo documento Nota inválido.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0195-QT-OCR-TAB-ANT   <= ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0309               TO MSG1-CD-ERRO-TS
               MOVE 'Quantidade UOR interveniente inválido.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF

           PERFORM VARYING IC-TAB FROM 1 BY 1
             UNTIL IC-TAB GREATER SGTS0195-QT-OCR-TAB-ANT
             IF  SGTS0195-CD-UOR-ITVT-NOTA-ANT(IC-TAB)   <= ZEROS
                 SET ERRO-PROGRAMADO     TO TRUE
                 MOVE 0292               TO MSG1-CD-ERRO-TS
                 MOVE 'Codigo UOR interveniente Nota inválido.'
                                       TO MSG1-TX-ERRO-TS
                 SET MNT-VRV             TO TRUE
                 MOVE 'IC-TAB' TO MSGKW999-NM-VRV-MNT
                 MOVE  IC-TAB  TO MSGKW999-VL-VRV-MNT
                 PERFORM 999003-ADICIONA-VRV-MNT
                 PERFORM 999000-ERRO
             END-IF
           END-PERFORM

      * Validar se a Data Aprovação é maior que Data Nota
           MOVE SGTS0195-DT-EMS-NOTA-ANT   TO DT-AUX-DMA
           MOVE CORR DT-AUX-DMA-R      TO DT-NOTA-INFD-R
           MOVE SGTS0195-DT-APVC-NOTA-ANT  TO DT-AUX-DMA
           MOVE CORR DT-AUX-DMA-R      TO DT-APVC-INFD-R

           IF  DT-APVC-INFD >= DT-NOTA-INFD
               CONTINUE
           ELSE
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0310               TO MSG1-CD-ERRO-TS
               MOVE 'Data Aprovação deve ser igual ou maior Data Nota.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF

           PERFORM 140000-BUSCA-TS-DT
           .
       110000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       140000-BUSCA-TS-DT SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                   TO MSG1-CD-SEQL-ERRO

           CALL SBTIMEDT         USING WK-CD-FUC
               WK-AREA-TRB
               WK-FMT-TS
               WK-TAM-TS
               WK-TS-FMTD

           IF  RETURN-CODE      NOT EQUAL ZEROS
               SET  ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE RETURN-CODE        TO  MSG1-CD-RTN-PGM-ACND
               MOVE 'SBTIMEDT'         TO  MSG1-NM-PGM-ACND
                                           NM-PGM
               MOVE 0050               TO  MSG1-CD-ERRO-TS
               MOVE 'Erro ao obter data e hora atual.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       140000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       200000-PROCESSAR                SECTION.
      *----------------------------------------------------------------*
           MOVE 0004                   TO MSG1-CD-SEQL-ERRO

           PERFORM 210000-VERIFICAR-VCL-NOTA-TCN

           IF SGTPE014-QT-NOTA-TCN = ZEROS
              IF SGTS0195-CD-ARQ-OPR-ETRD-ANT =
                                     SGTS0195-CD-ARQ-OPR-ETRD-NOVO
                 PERFORM 220000-ALTERAR-NOTA-TECN
              ELSE
                 SET  ERRO-PROGRAMADO        TO  TRUE
                 MOVE 0406                   TO  MSG1-CD-ERRO-TS
                 MOVE 'Erro no desfazimento de alteração de anexo. Diver
      -         'gências nos códigos dos arquivos.'
                                           TO  MSG1-TX-ERRO-TS
                 PERFORM 999000-ERRO
              END-IF
           ELSE
              IF SGTS0195-CD-ARQ-OPR-ETRD-ANT =
                                     SGTS0195-CD-ARQ-OPR-ETRD-NOVO
                 SET  ERRO-PROGRAMADO        TO  TRUE
                 MOVE 407                    TO  MSG1-CD-ERRO-TS
                 MOVE 'Erro no desfazimento de alteração de anexo. Probl
      -         'emas nos códigos dos arquivos.'
                                           TO  MSG1-TX-ERRO-TS
                 PERFORM 999000-ERRO

              ELSE
                 PERFORM 220000-ALTERAR-NOTA-TECN
                 PERFORM 260000-EXCLUI-ARQUIVO-NOVO
              END-IF
           END-IF

      *    MOVE 'Desfazimento de alteração de anexo efetuada com sucesso
      *           '.'                   TO  SGTS0195-TX-MSG-RTN
      *
      *    PERFORM 980000-GRAVAR-CONTAINER-RPST
           .
       200000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       210000-VERIFICAR-VCL-NOTA-TCN        SECTION.
      *----------------------------------------------------------------*
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE         SGTSPE14-DADOS

            MOVE SGTSPE14              TO NM-PGM

      *    Movimentar os campos de entrada
           MOVE SGTS0195-NR-NOTA-TCN-OPR   TO SGTSPE14-NR-NOTA-TCN-OPR
           MOVE SGTS0195-CD-ARQ-OPR-ETRD-ANT  TO SGTSPE14-CD-ARQ-OPR

           PERFORM 900000-EXECUTAR-MODULO
           .
       210000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       220000-ALTERAR-NOTA-TECN       SECTION.
      *----------------------------------------------------------------*
      * Altera a Nota Tecnica e demais dados no SGT, gravar Tab. Interv.
      *----------------------------------------------------------------*
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO
      *    Movimentar o nome do programa a ser executado

           MOVE SGTSA055               TO NM-PGM

           MOVE  SGTS0195-NR-NOTA-TCN-OPR TO SGTSA055-NR-NOTA-TCN-OPR
           MOVE  SGTS0195-CD-ARQ-OPR-ETRD-ANT
                                       TO SGTSA055-CD-ARQ-OPR-ETRD
           MOVE SGTS0195-CD-UOR-DRTA-OGNL-ANT
                                       TO SGTSA055-CD-UOR-DRTA-OGNL
           MOVE SGTS0195-CD-REF-NOTA-TCN-ANT
                                       TO SGTSA055-CD-REF-NOTA-TCN
           MOVE GDA-DT-EMS-DMA         TO SGTSA055-DT-EMS-NOTA
           MOVE GDA-DT-APVC-DMA        TO SGTSA055-DT-APVC-NOTA
           MOVE SGTS0195-CD-ETR-DCS-ANT    TO SGTSA055-CD-ETR-DCS
           MOVE SGTS0195-CD-TIP-DOC-TCN-ANT
                                       TO SGTSA055-CD-TIP-DOC-TCN
           MOVE KOPRD-CD-OPRD          TO SGTSA055-CD-USU-RSP-ULT-ATL
           MOVE WK-TS-FMTD             TO SGTSA055-TS-ULT-ATL

           PERFORM 900000-EXECUTAR-MODULO

           PERFORM 221000-EXCLUIR-DIRETORIAS-ITVT

           PERFORM VARYING IC-TAB FROM 1 BY 1
             UNTIL IC-TAB GREATER SGTS0195-QT-OCR-TAB-ANT
             MOVE SGTSI056               TO NM-PGM
             MOVE SGTS0195-NR-NOTA-TCN-OPR
                                       TO SGTSI056-NR-NOTA-TCN-OPR
             MOVE SGTS0195-CD-UOR-ITVT-NOTA-ANT(IC-TAB)
                                       TO SGTSI056-CD-UOR-ITVT-NOTA
             PERFORM 900000-EXECUTAR-MODULO
           END-PERFORM
           .
       220000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       221000-EXCLUIR-DIRETORIAS-ITVT     SECTION.
      *----------------------------------------------------------------*
      * Listar as UORs e excluir, antes de incluir as UORS editadas.
      *----------------------------------------------------------------*
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

      *    Movimentar o nome do programa a ser executado
           MOVE SGTSL056               TO NM-PGM

           MOVE  SGTS0195-NR-NOTA-TCN-OPR TO SGTSL056-E-NR-NOTA-TCN-OPR
           MOVE  1                     TO SGTSL056-CD-TIP-SRVC
           MOVE ZEROS                  TO SGTSL056-E-CD-UOR-ITVT-NOTA

           PERFORM 900000-EXECUTAR-MODULO

           PERFORM VARYING SGTSL056-IC-TAB FROM 1 BY 1
             UNTIL SGTSL056-IC-TAB GREATER SGTSL056-QT-OCR-TAB
      *--> Efetua excluso das UORs
             MOVE SGTSE056               TO NM-PGM
             MOVE SGTS0195-NR-NOTA-TCN-OPR
                                       TO SGTSE056-NR-NOTA-TCN-OPR
             MOVE SGTSL056-CD-UOR-ITVT-NOTA(SGTSL056-IC-TAB)
                                       TO SGTSE056-CD-UOR-ITVT-NOTA

             PERFORM 900000-EXECUTAR-MODULO
           END-PERFORM
           .
       221000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       260000-EXCLUI-ARQUIVO-NOVO       SECTION.
      *----------------------------------------------------------------*
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

      *    Movimentar o nome do programa a ser executado
           MOVE SGTSE059               TO NM-PGM

           MOVE SGTS0195-CD-ARQ-OPR-ETRD-NOVO TO
                                          SGTSE059-CD-ARQ-OPR-ETRD

           PERFORM 900000-EXECUTAR-MODULO
           .
       260000-SAI.
           EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *  SECTION DA FAIXA DE 900000 A 969900 PARA DEMAIS ROTINAS       *
      *                                                                *
      *----------------------------------------------------------------*
      * Executa um módulo implementado com CONTAINER                   *
      * Não incluir nessa section os módulos chamados via COMMAREA     *
      *                                                                *
      *----------------------------------------------------------------*
       900000-EXECUTAR-MODULO          SECTION.
      *----------------------------------------------------------------*
      *    Grava o container de requisicao do módulo a executar
           PERFORM 982000-GRAVAR-CONTAINER-RQSC

      *    Limpa a resposta e erro antes de executar o módulo
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO

      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EVALUATE NM-PGM

               WHEN SGTSPE14
                   EXEC CICS LINK
                             PROGRAM ( SGTSPE14  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC

               WHEN SGTSE059
                   EXEC CICS LINK
                             PROGRAM ( SGTSE059  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC

               WHEN SGTSA055
                   EXEC CICS LINK
                             PROGRAM ( SGTSA055  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC

               WHEN SGTSL056
                   EXEC CICS LINK
                             PROGRAM ( SGTSL056  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC

               WHEN SGTSI056
                   EXEC CICS LINK
                             PROGRAM ( SGTSI056  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC

               WHEN SGTSE056
                   EXEC CICS LINK
                             PROGRAM ( SGTSE056  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC

               WHEN OTHER
                   SET   ERRO-PROGRAMADO     TO TRUE
                   MOVE  9006          TO  MSG1-CD-ERRO-TS
                   STRING
                   'Não definida a execução do módulo ' NM-PGM
                   DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
                   PERFORM 999000-ERRO
           END-EVALUATE
      *
           EVALUATE TRUE
               WHEN MSG1-RESP1        EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    MOVE 3004          TO MSG1-CD-ERRO-TS
                    STRING
                    'Erro na execução do módulo ' NM-PGM ', '
                    'RESP  ='   ' ' MSG1-RESP1  ', '
                    'RESP2 =' ' ' MSG1-RESP2
                    DELIMITED BY SIZE  INTO  MSG1-TX-ERRO-TS
                    PERFORM 969000-MONITORAR-VARIAVEIS
                    PERFORM 999000-ERRO
           END-EVALUATE

      *    Efetua a leitura do Container de Erro
           PERFORM 972000-LER-CONTAINER-DE-ERRO

      *    Efetua a leitura do Container de Resposta
           IF NM-PGM  NOT EQUAL SGTSE059  AND  SGTSA055
               PERFORM 973000-LER-RESPOSTA
           END-IF
           .
       900000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       969000-MONITORAR-VARIAVEIS      SECTION.
      *----------------------------------------------------------------*
      * Monitora as variáveis dos módulos chamados
      *----------------------------------------------------------------*
           EVALUATE NM-PGM
           WHEN SGTSE059
                MOVE 'SGTSE059-CD-ARQ-OPR-ETRD'  TO MSGKW999-NM-VRV-MNT
                MOVE SGTSE059-CD-ARQ-OPR-ETRD    TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

           WHEN SGTSPE14
                MOVE 'SGTSPE14-NR-NOTA-TCN-OPR'  TO MSGKW999-NM-VRV-MNT
                MOVE SGTSPE14-NR-NOTA-TCN-OPR    TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTSPE14-CD-ARQ-OPR'       TO MSGKW999-NM-VRV-MNT
                MOVE SGTSPE14-CD-ARQ-OPR         TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

           WHEN SGTSA055
                MOVE 'SGTSA055-NR-NOTA-TCN-OPR'  TO MSGKW999-NM-VRV-MNT
                MOVE SGTSA055-NR-NOTA-TCN-OPR    TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTSA055-CD-ARQ-OPR-ETRD'  TO MSGKW999-NM-VRV-MNT
                MOVE SGTSA055-CD-ARQ-OPR-ETRD    TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTSA055-CD-UOR-DRTA-OGNL' TO MSGKW999-NM-VRV-MNT
                MOVE SGTSA055-CD-UOR-DRTA-OGNL   TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTSA055-CD-REF-NOTA-TCN'  TO MSGKW999-NM-VRV-MNT
                MOVE SGTSA055-CD-REF-NOTA-TCN    TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTSA055-DT-EMS-NOTA'      TO MSGKW999-NM-VRV-MNT
                MOVE SGTSA055-DT-EMS-NOTA        TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTSA055-DT-APVC-NOTA'     TO MSGKW999-NM-VRV-MNT
                MOVE SGTSA055-DT-APVC-NOTA       TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTSA055-CD-ETR-DCS'       TO MSGKW999-NM-VRV-MNT
                MOVE SGTSA055-CD-ETR-DCS         TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTSA055-CD-TIP-DOC-TCN'   TO MSGKW999-NM-VRV-MNT
                MOVE SGTSA055-CD-TIP-DOC-TCN     TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTSA055-CD-USU-RSP-ULT-ATL'
                                                 TO MSGKW999-NM-VRV-MNT
                MOVE SGTSA055-CD-USU-RSP-ULT-ATL TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTSA055-TS-ULT-ATL'       TO MSGKW999-NM-VRV-MNT
                MOVE SGTSA055-TS-ULT-ATL         TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

           WHEN SGTSI056
                MOVE 'SGTSI056-NR-NOTA-TCN-OPR'  TO MSGKW999-NM-VRV-MNT
                MOVE SGTSI056-NR-NOTA-TCN-OPR    TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTSI056-CD-UOR-ITVT-NOTA' TO MSGKW999-NM-VRV-MNT
                MOVE SGTSI056-CD-UOR-ITVT-NOTA   TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

           WHEN SGTSE056
                MOVE 'SGTSE056-NR-NOTA-TCN-OPR'  TO MSGKW999-NM-VRV-MNT
                MOVE SGTSE056-NR-NOTA-TCN-OPR    TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTSE056-CD-UOR-ITVT-NOTA' TO MSGKW999-NM-VRV-MNT
                MOVE SGTSE056-CD-UOR-ITVT-NOTA   TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

           WHEN SGTSL056
                MOVE 'SGTSL056-CD-TIP-SRVC'      TO MSGKW999-NM-VRV-MNT
                MOVE SGTSL056-CD-TIP-SRVC        TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTSL056-E-NR-NOTA-TCN-OPR'
                                                 TO MSGKW999-NM-VRV-MNT
                MOVE SGTSL056-E-NR-NOTA-TCN-OPR  TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTSL056-E-CD-UOR-ITVT-NOTA'
                                                 TO MSGKW999-NM-VRV-MNT
                MOVE SGTSL056-E-CD-UOR-ITVT-NOTA TO MSGKW999-VL-VRV-MNT
                PERFORM 999003-ADICIONA-VRV-MNT

           WHEN  SBTIMEDT
                MOVE  'WK-CD-FUC'                 TO MSGKW999-NM-VRV-MNT
                MOVE   WK-CD-FUC                  TO MSGKW999-VL-VRV-MNT
                PERFORM  999003-ADICIONA-VRV-MNT

                MOVE  'WK-AREA-TRB'               TO MSGKW999-NM-VRV-MNT
                MOVE  WK-AREA-TRB                 TO MSGKW999-VL-VRV-MNT
                PERFORM  999003-ADICIONA-VRV-MNT

                MOVE  'WK-FMT-TS'                 TO MSGKW999-NM-VRV-MNT
                MOVE  WK-FMT-TS                   TO MSGKW999-VL-VRV-MNT
                PERFORM  999003-ADICIONA-VRV-MNT

                MOVE  'WK-TAM-TS'                 TO MSGKW999-NM-VRV-MNT
                MOVE  WK-TAM-TS                   TO MSGKW999-VL-VRV-MNT
                PERFORM  999003-ADICIONA-VRV-MNT

                MOVE  'WK-TS-FMTD'          TO MSGKW999-NM-VRV-MNT
                MOVE  WK-TS-FMTD            TO MSGKW999-VL-VRV-MNT
                PERFORM  999003-ADICIONA-VRV-MNT

           WHEN OTHER
                CONTINUE
           END-EVALUATE
           .
       969000-SAI.
           EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      * SECTION DA FAIXA DE 970000 A 979900 PARA LEITURA DE DADOS      *
      *                                                                *
      *----------------------------------------------------------------*
       970000-LER-CANAL                SECTION.
      *    Obtem o canal corrente
      *----------------------------------------------------------------*
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CANAL             TO TRUE

           EXEC CICS ASSIGN
                     CHANNEL ( NM-CNL )
           END-EXEC

      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       970000-FIM.
           EXIT.
      *----------------------------------------------------------------*
       971000-LER-REQUISICAO           SECTION.
      *----------------------------------------------------------------*
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO

      *    Monta o nome do container de requisição
           MOVE 'SGTS0195'             TO NM-PGM
           MOVE 'SGTS0195-RQSC'        TO NM-CTNR

      *    Recupera o tamanho do container de requisição
           PERFORM 973100-LER-CONTAINER-NODATA

      *    Obtem o tamanho esperado da requisição
           MOVE LENGTH OF SGTS0195-RQSC TO QT-CCT-EPRO

      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Efetua a leitura do container
           EXEC CICS GET
                     CONTAINER (NM-CTNR      )
                     FLENGTH   (QT-CCT-LIDO  )
                     INTO      (SGTS0195-RQSC)
           END-EXEC

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       971000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       972000-LER-CONTAINER-DE-ERRO    SECTION.
      *----------------------------------------------------------------*
      *    A presença do container PRM-ERRO indica que houve erro de
      *    integração, a ausência do container indica sucesso.
      *----------------------------------------------------------------*
      *    Lê o container de erro dos módulos chamados
           SET GET-CONTAINER           TO TRUE
           MOVE 'PRM-ERRO'             TO NM-CTNR
           INITIALIZE IIBKERRO-PRM-ERRO

      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS GET
                     CONTAINER ( NM-CTNR                     )
                     INTO      ( IIBKERRO-PRM-ERRO           )
                     FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                     RESP      ( MSG1-RESP1                  )
                     RESP2     ( MSG1-RESP2                  )
           END-EXEC

           EVALUATE TRUE
      *        Caso deseje personalizar ou não interromper o
      *        processamento, alterar para não dar erro aqui SOMENTE
      *        PARA O MODULO CHAMADO e tratar na section chamadora
               WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
                    CONTINUE

               WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
                    SET   ERRO-PROGRAMA-ACIONADO TO  TRUE
                    MOVE  NM-PGM                 TO  MSG1-NM-PGM-ACND

                    IF NM-PGM(1:3) EQUAL 'SGT'
                       MOVE  IIBKERRO-CD-ERRO  TO  MSG1-CD-RTN-PGM-ACND
                                                   MSG1-CD-ERRO-TS
                       MOVE  IIBKERRO-TX-ERRO  TO  MSG1-TX-RTN-PGM-ACND
                                                   MSG1-TX-ERRO-TS
                    ELSE
                       MOVE 3004          TO MSG1-CD-ERRO-TS
                       STRING  'Erro ao acionar o módulo ' NM-PGM '.'
                            DELIMITED BY SIZE
                            INTO           MSG1-TX-ERRO-TS
                       MOVE IIBKERRO-TX-ERRO TO MSG1-TX-RTN-PGM-ACND
                       MOVE IIBKERRO-CD-ERRO TO MSG1-CD-RTN-PGM-ACND
                    END-IF

      *             Monitorar variáveis do módulo chamado
                    PERFORM 969000-MONITORAR-VARIAVEIS
                    PERFORM 999000-ERRO

               WHEN OTHER
                    SET ERRO-CONTAINER  TO TRUE
                    PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       972000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       973000-LER-RESPOSTA             SECTION.
      *----------------------------------------------------------------*
      *    Monta o nome dos container de acordo com módulo a executar
           MOVE SPACES                 TO NM-CTNR
           MOVE NM-PGM                 TO NM-CTNR(1:8)
           MOVE '-RPST'                TO NM-CTNR(9:5)

      *    Recupera o tamanho do container de resposta
           PERFORM 973100-LER-CONTAINER-NODATA

      *Configura dados para leitura do container de resposta
           SET  GET-CONTAINER          TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Lê a RESPOSTA de acordo com o programa que a gravou
           EVALUATE NM-PGM

      *        Calcular a quantidade de caracteres esperado e limpar
      *        a area de dados antes de executar o GET
      *        Para matrizes com dados variaveis (clausula depending
      *        on) nao ha como utilizar o comando INITIALIZE

               WHEN SGTSPE14
                    MOVE LENGTH OF SGTSPE14-RPST TO QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSPE14-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

      *        WHEN SGTSE059
      *             MOVE LENGTH OF SGTSE059-RPST TO QT-CCT-EPRO
      *             EXEC CICS GET
      *                       CONTAINER (NM-CTNR          )
      *                       INTO      (SGTSE059-RPST    )
      *                       FLENGTH   (QT-CCT-LIDO      )
      *             END-EXEC

      *        WHEN SGTSA055
      *             MOVE LENGTH OF SGTSA055-RPST TO QT-CCT-EPRO
      *             EXEC CICS GET
      *                       CONTAINER (NM-CTNR          )
      *                       INTO      (SGTSA055-RPST    )
      *                       FLENGTH   (QT-CCT-LIDO      )
      *             END-EXEC

               WHEN SGTSL056
                    MOVE LENGTH OF SGTSL056-RPST TO QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSL056-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN SGTSI056
                    MOVE LENGTH OF SGTSI056-RPST TO QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSI056-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN SGTSE056
                    MOVE LENGTH OF SGTSE056-RPST TO QT-CCT-EPRO
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSE056-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN OTHER
                    SET   ERRO-PROGRAMADO      TO  TRUE
                    MOVE  9003                 TO  MSG1-CD-ERRO-TS
                    STRING  'Erro na leitura da resposta do módulo '
                            NM-PGM '.'
                            DELIMITED BY SIZE
                            INTO           MSG1-TX-ERRO-TS
                    PERFORM 999000-ERRO
           END-EVALUATE

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       973000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       973100-LER-CONTAINER-NODATA     SECTION.
      *----------------------------------------------------------------*
      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Obtem a quantidade de caracteres no container
           MOVE ZEROS                  TO QT-CCT-LIDO

           EXEC CICS GET
                     CONTAINER (NM-CTNR    )
                     NODATA
                     FLENGTH   (QT-CCT-LIDO)
           END-EXEC

      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       973100-SAI.
           EXIT.
      *----------------------------------------------------------------*
       974000-LER-DADOS-OPRD           SECTION.
      *----------------------------------------------------------------*
           MOVE 0011                   TO MSG1-CD-SEQL-ERRO

      *Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE
           MOVE 'PRM-INF-OPRD'         TO NM-CTNR

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

           INITIALIZE PRM-INF-OPRD
           EXEC CICS GET
                CONTAINER ( NM-CTNR                )
                INTO      ( PRM-INF-OPRD           )
                FLENGTH   ( LENGTH OF PRM-INF-OPRD )
           END-EXEC.

      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       974000-SAI.
           EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      * SECTION DA FAIXA DE 980000 A 989900 PARA GRAVACAO DE DADOS     *
      *                                                                *
      *----------------------------------------------------------------*
      *980000-GRAVAR-CONTAINER-RPST    SECTION.
      *----------------------------------------------------------------*
      *    Exclui eventual container de erro de outros programas
      *    PERFORM 983100-EXCLUIR-CONTAINER-ERRO
      *
      *    MOVE 0012                   TO MSG1-CD-SEQL-ERRO
      *
      *    Monta o nome do container de resposta
      *    MOVE 'SGTS0195-RPST'        TO NM-CTNR
      *    SET PUT-CONTAINER           TO TRUE
      *
      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
      *    SET  ERRO-CONTAINER         TO TRUE
      *
      *    EXEC CICS PUT
      *              CONTAINER (NM-CTNR                )
      *              FROM      (SGTS0195-RPST          )
      *              FLENGTH   (LENGTH OF SGTS0195-RPST)
      *    END-EXEC
      *
      *Ao chegar aqui, há certeza de que não houve erro algum
      *    SET  SEM-ERRO               TO TRUE
      *    .
      *980000-SAI.
      *    EXIT.
      *----------------------------------------------------------------*
       982000-GRAVAR-CONTAINER-RQSC    SECTION.
      *----------------------------------------------------------------*
      *    Monta o nome dos container de acordo com módulo a executar
           MOVE SPACES                 TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE
           MOVE NM-PGM                 TO NM-CTNR(1:8)
           MOVE '-RQSC'                TO NM-CTNR(9:5)

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Faz put da requisição do programa a executar

           EVALUATE NM-PGM
               WHEN SGTSPE14
                   EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (SGTSPE14-RQSC           )
                              FLENGTH   (LENGTH OF SGTSPE14-RQSC )
                    END-EXEC

               WHEN SGTSE059
                   EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (SGTSE059-RQSC           )
                              FLENGTH   (LENGTH OF SGTSE059-RQSC )
                    END-EXEC

               WHEN SGTSA055
                   EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (SGTSA055-RQSC           )
                              FLENGTH   (LENGTH OF SGTSA055-RQSC )
                    END-EXEC

               WHEN SGTSL056
                   EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (SGTSL056-RQSC           )
                              FLENGTH   (LENGTH OF SGTSL056-RQSC )
                    END-EXEC

               WHEN SGTSE056
                   EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (SGTSE056-RQSC           )
                              FLENGTH   (LENGTH OF SGTSE056-RQSC )
                    END-EXEC

               WHEN SGTSI056
                   EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (SGTSI056-RQSC           )
                              FLENGTH   (LENGTH OF SGTSI056-RQSC )
                    END-EXEC

               WHEN OTHER
                    SET   ERRO-PROGRAMADO      TO  TRUE
                    MOVE  9004                 TO  MSG1-CD-ERRO-TS
                    STRING  'Erro na gravação da requisição do módulo '
                            NM-PGM '.' DELIMITED BY SIZE
                            INTO           MSG1-TX-ERRO-TS
                    PERFORM 999000-ERRO
           END-EVALUATE

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       982000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       983000-EXCLUIR-RESPOSTA-ERRO    SECTION.
      *----------------------------------------------------------------*
      * Exclui o container de resposta e de erro para evitar problemas *
      * em módulos executados mais de uma vez dentro da mesma sessão do*
      * CICS. Equivale ao initialize na area de resposta dos programas *
      * que utilizam COMMAREA.                                         *
      *----------------------------------------------------------------*
           MOVE NM-PGM                 TO NM-CTNR(1:8)
           MOVE '-RPST'                TO NM-CTNR(9:5)
           SET DEL-CONTAINER           TO TRUE

      *    Exclui o container de resposta
      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS DELETE
                     CONTAINER (NM-CTNR   )
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

      *    Excluir o container de erro
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO
           .
       983000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       983100-EXCLUIR-CONTAINER-ERRO   SECTION.
      *----------------------------------------------------------------*
      *    Limpar variáveis container de erro
           INITIALIZE IIBKERRO-PRM-ERRO

      *    Exclui o container de erro
           MOVE 'PRM-ERRO'             TO NM-CTNR
           SET DEL-CONTAINER           TO TRUE

      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS DELETE
                     CONTAINER (NM-CTNR   )
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
      *----------------------------------------------------------------*
       984000-GRAVAR-CONTAINER-ERRO    SECTION.
      *----------------------------------------------------------------*
           MOVE 0013                   TO MSG1-CD-SEQL-ERRO

      *    Grava o erro gerado no container de erro
      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS PUT
                     CONTAINER ( 'PRM-ERRO'                  )
                     FROM      ( IIBKERRO-PRM-ERRO           )
                     FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                     RESP      (MSG1-RESP1                   )
                     RESP2     (MSG1-RESP2                   )
           END-EXEC
           .
       984000-SAI.
           EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      * As sections abaixo iniciadas com 999000 representam os tratamen*
      * tos de erro utilizando o Sistema MSG (rotina MSGS0001)         *
      *                                                                *
      *----------------------------------------------------------------*
       999000-ERRO                     SECTION.
      *----------------------------------------------------------------*
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
      *----------------------------------------------------------------*
      *     As sections abaixo representam os tratamentos de erro
      *    utilizando o sistema MSG de mensageria (rotina MSGS0001)
      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------

      *    Em caso de erro, desfaz as atualizações em banco de dados

           EXEC CICS SYNCPOINT
                     ROLLBACK
           END-EXEC

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

      *Adiciona as variáveis de entrada para monitoração
               MOVE 'SGTS0195-NR-NOTA-TCN-OPR'   TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0195-NR-NOTA-TCN-OPR    TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0195-CD-ARQ-OPR-ETRD-ANT'
                                                 TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0195-CD-ARQ-OPR-ETRD-ANT
                                                 TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0195-CD-ARQ-OPR-ETRD-NOVO'
                                                 TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0195-CD-ARQ-OPR-ETRD-NOVO
                                                 TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0195-CD-UOR-DRTA-OGNL-ANT'
                                                 TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0195-CD-UOR-DRTA-OGNL-ANT
                                                 TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0195-CD-REF-NOTA-TCN-ANT'
                                                 TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0195-CD-REF-NOTA-TCN-ANT
                                                 TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0195-DT-EMS-NOTA-ANT'   TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0195-DT-EMS-NOTA-ANT    TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0195-DT-APVC-NOTA-ANT'  TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0195-DT-APVC-NOTA-ANT   TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0195-CD-ETR-DCS-ANT'    TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0195-CD-ETR-DCS-ANT     TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0195-CD-TIP-DOC-TCN-ANT'
                                                 TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0195-CD-TIP-DOC-TCN-ANT TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0195-QT-OCR-TAB-ANT'    TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0195-QT-OCR-TAB-ANT     TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'IC-TAB'                     TO MSGKW999-NM-VRV-MNT
               MOVE IC-TAB                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0195-CD-UOR-ITVT-NOTA-ANT'
                                                 TO MSGKW999-NM-VRV-MNT
               MOVE SGTS0195-CD-UOR-ITVT-NOTA-ANT(IC-TAB)
                                                 TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

      *    Grava o erro gerado no sistema MSG - Mensageria
           PERFORM MSG1-NTF-MSG

           MOVE MSG1-CD-ERRO-TS        TO IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS        TO IIBKERRO-TX-ERRO

      *    Grava o erro gerado no container de erro
           PERFORM 984000-GRAVAR-CONTAINER-ERRO

      *    Encerra a transacao
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

      *----------------------------------------------------------------*
       999999-ENCERRAR                 SECTION.
      *----------------------------------------------------------------*

           EXEC CICS
                RETURN
           END-EXEC

           .
       999999-SAI.
           EXIT.
      *---------------------- FIM SGTS0195 -----------------------------