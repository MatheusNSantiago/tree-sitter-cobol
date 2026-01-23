      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSL086
      * ANALISTA..: F2054109 Cintia Ap Soares Iglesias
      * AUTOR.....: F0950413 António Francisco Mendonça Silva
      * DATA......: 09/03/2017
      * OBJETIVO..: CONSULTA TABELA DB2SGT.DMTV_CX_TIT
      * COMPILACAO: COBOL/CICS/DB2 (On-line)
      *-----------------------------------------------------------------
      * VRS005 24.06.2024 - F8785286 - HU 1210924  Resolução 4966
      *        Incluir SGTSL086-NR-DVS-TIT nos critérios de seleção
      *        do cursor 2.
      *                      F2054109 Tratamento CD_TIP_OGM_INF = 2
      * VRS004 27.03.2020 - F9158937 - No cursor 2 selecionar somente os
      *        registros com DT_EFT_MVTC maior e não maior ou igual.
      * VRS003 03.12.2019 - F9158937 - Selecionar os demonstrativos com
      *                                CD_TIP_LCTO_CTB    =  2
      * VRS002 23.04.2019 - F2634012 - Selecionar apenas os
      *                                Demonstrativos Ativos
      *                                e inclusao campo CD-CTRA-TIT-VL
      * VRS001 17.08.2018 - F8785286 - Inclusão Serviço 2
      *        08.08.2018 - F9158937 - Inclusão campo TX-DCR-MVTC
      *        09.03.2017 - F0950413 - 2015/137233 - Implantação
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTSL086.

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
           '*** Storage Section Começa Aqui ***'.
       77  CTE-PROG                    PIC  X(17)
                                       VALUE '*** SGTSL086 ***'.
       77  CTE-VERS                    PIC  X(06) VALUE 'VRS005'.

      *--> Book do programa de ambiente - CICS5000
      *    Definido na work para preservar as informacoes
       COPY HLPKE01B.

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

      *--> Area de trabalho
       01  QT-REG-SELD                 PIC S9(04) COMP-5 VALUE ZEROS.
       01  IC-TAB                      PIC S9(04) COMP-5 VALUE ZEROS.

      *--> Campos utilizados para leitura da Tabela DMTV_CX_TIT
       01  DMTV-CX-TIT.
           03 CD-TIP-MVTC-TIT          PIC S9(04) COMP-5
                                       OCCURS 0101 TIMES  VALUE ZEROS.
           03 CD-USU-IMP               PIC  X(08)
                                       OCCURS 0101 TIMES  VALUE SPACES.
           03 DT-EFT-MVTC              PIC  X(10)
                                       OCCURS 0101 TIMES  VALUE SPACES.
           03 DT-PRVT-MVTC             PIC  X(10)
                                       OCCURS 0101 TIMES  VALUE SPACES.
           03 DT-PRVT-MVTC-N           PIC S9(04) COMP-5
                                       OCCURS 0101 TIMES  VALUE ZEROS.
           03 IN-MVTC-ATV              PIC  X(01)
                                       OCCURS 0101 TIMES  VALUE SPACES.
           03 TX-DCR-MVTC              PIC  X(50)
                                       OCCURS 0101 TIMES  VALUE SPACES.
           03 TS-REG-PGTO              PIC  X(26)
                                       OCCURS 0101 TIMES  VALUE SPACES.
           03 VL-EFT-MVTC              PIC S9(15)V9(2) COMP-3
                                       OCCURS 0101 TIMES  VALUE ZEROS.
           03 VL-EFT-MVTC-N            PIC S9(04) COMP-5
                                       OCCURS 0101 TIMES  VALUE ZEROS.
           03 VL-PRVT-MVTC             PIC S9(15)V9(2) COMP-3
                                       OCCURS 0101 TIMES  VALUE ZEROS.
           03 VL-PRVT-MVTC-N           PIC S9(04) COMP-5
                                       OCCURS 0101 TIMES  VALUE ZEROS.

      *--> Indicadores de nulidades

      *--> Variavel(is) de controle do cursor(es)
       01  CTL-CURSOR-PSQ.
           03  CD-FIM-CURSOR-PSQ            PIC S9(04) COMP-5 VALUE 0.
               88  FIM-CURSOR-PSQ            VALUE +100.
           03  IN-CURSOR-PSQ-ABTO           PIC  X(01) VALUE 'N'.
               88  CURSOR-PSQ-ABTO          VALUE 'S'.

      *--> Book do Container de Erro
       COPY IIBKERRO.

      *--> Book para captura de erro via MSG1
       COPY MSGKWMG1.

      *--> Book para captura de abend cics da bloco codigo MSGKP999
       COPY HLPKW999.

      *--> Book das variáveis auxiliares do tratamento de erro via MSG
       COPY SGTKERR1.

      *--> Book do programa
      *01  SGTSL086-DADOS.
       COPY SGTKL086.

      *-----------------------------------------------------------------
      *    Cursores de Pesquisa
      *-----------------------------------------------------------------

      *--> Cursor de Pesquisa 01
      *    Listar Demontrativo de Caixa de um Título
      *    na Tabela DB2SGT.DMTV_CX_TIT
           EXEC SQL
             DECLARE CURSOR-PSQ CURSOR
                     WITH ROWSET POSITIONING FOR
             SELECT A.CD_TIP_MVTC_TIT
                  , A.CD_USU_IMP
                  , A.DT_EFT_MVTC
                  , A.DT_PRVT_MVTC
                  , A.IN_MVTC_ATV
                  , A.TX_DCR_MVTC
                  , A.TS_REG_PGTO
                  , A.VL_EFT_MVTC
                  , A.VL_PRVT_MVTC
               FROM DB2SGT.DMTV_CX_TIT        A
              WHERE A.CD_TIT_OPR_ETRD    = :SGTSL086-CD-TIT-OPR-ETRD
                AND A.NR_DVS_TIT         = :SGTSL086-NR-DVS-TIT
                AND A.TS_REG_PGTO        > :SGTSL086-ULT-TS-REG-PGTO
                AND A.IN_MVTC_ATV        = 'S'
                AND  (A.CD_TIP_LCTO_CTB =   2
                     OR A.CD_TIP_OGM_INF = 2)
                AND A.CD_CTRA_TIT_VL     = :SGTSL086-CD-CTRA-TIT-VL
              ORDER  BY  A.TS_REG_PGTO
           END-EXEC.

      *--> Cursor de Pesquisa 02
      *    Listar Demontrativo de Caixa de um Título na tabela
      *     DB2SGT.DMTV_CX_TIT a partir de uma data efetiva de moviment.
           EXEC SQL
             DECLARE CURSOR-PSQ-02 CURSOR
                     WITH ROWSET POSITIONING FOR
             SELECT A.CD_TIP_MVTC_TIT
                  , A.CD_USU_IMP
                  , A.DT_EFT_MVTC
                  , A.DT_PRVT_MVTC
                  , A.IN_MVTC_ATV
                  , A.TX_DCR_MVTC
                  , A.TS_REG_PGTO
                  , A.VL_EFT_MVTC
                  , A.VL_PRVT_MVTC
               FROM DB2SGT.DMTV_CX_TIT        A
              WHERE A.CD_TIT_OPR_ETRD    = :SGTSL086-CD-TIT-OPR-ETRD
                AND A.NR_DVS_TIT         = :SGTSL086-NR-DVS-TIT
                AND A.DT_EFT_MVTC        > :SGTSL086-E-DT-EFT-MVTC
                AND A.IN_MVTC_ATV         = 'S'
                AND  (A.CD_TIP_LCTO_CTB =   2
                     OR A.CD_TIP_OGM_INF = 2)
                AND A.CD_CTRA_TIT_VL      = :SGTSL086-CD-CTRA-TIT-VL
              ORDER  BY  A.TS_REG_PGTO
           END-EXEC.

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

       01  CTE-FINAL-SS                PIC X(40) VALUE
           '*** Storage Section Termina Aqui ***'.

      *-----------------------------------------------------------------
       PROCEDURE                       DIVISION.
      *-----------------------------------------------------------------

      *--> Habilita tratamento automatico de erros DB2
       COPY HLPKE997.

      *--> Habilita captura automatica de abend e erros CICS
       COPY HLPKE999.

           PERFORM  100000-DESCOBRIR-CANAL
           PERFORM  110000-EXCLUIR-CTNR-RPST
           PERFORM  120000-EXCLUIR-CTNR-ERRO
           PERFORM  130000-RECEBER-REQUISICAO

           PERFORM  140000-VALIDAR-PARAMETRO

           PERFORM  970101-ABRE-CURSOR-PSQ

           PERFORM  970102-LER-CURSOR-PSQ

           PERFORM  970103-FECHA-CURSOR-PSQ

           PERFORM  200000-PROCESSA-PESQUISA
                    VARYING                IC-TAB
                                           FROM 0001 BY 0001
                    UNTIL                  IC-TAB
                                           GREATER  SGTSL086-QT-OCR-TAB

           PERFORM  300000-POSTA-RESPOSTA

           PERFORM  999999-ENCERRAR

           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-DESCOBRIR-CANAL          SECTION.
      *-----------------------------------------------------------------
           MOVE  0001                  TO  MSG1-CD-SEQL-ERRO

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CANAL            TO  TRUE

           EXEC CICS ASSIGN
                      CHANNEL   ( NM-CNL )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-EXCLUIR-CTNR-RPST        SECTION.
      *-----------------------------------------------------------------
           MOVE  0002                  TO  MSG1-CD-SEQL-ERRO

           EXEC CICS DELETE
                      CONTAINER ( 'SGTSL086-RPST' )
                      CHANNEL   ( NM-CNL          )
              NOHANDLE
           END-EXEC

           IF  NOT (EIBRESP EQUAL DFHRESP(NORMAL)   OR
                    EIBRESP EQUAL DFHRESP(CONTAINERERR) )
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

           IF  NOT (EIBRESP EQUAL DFHRESP(NORMAL)   OR
                    EIBRESP EQUAL DFHRESP(CONTAINERERR) )
               PERFORM  999002-TRATA-CONDITION-CICS
           END-IF
           .
       120000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       130000-RECEBER-REQUISICAO       SECTION.
      *-----------------------------------------------------------------
           MOVE  0004                  TO  MSG1-CD-SEQL-ERRO

           MOVE  ZEROS                     TO  QT-CCT-LIDO
           MOVE  LENGTH OF  SGTSL086-RQSC  TO  QT-CCT-EPRO
           EXEC CICS  GET
                      CONTAINER ( 'SGTSL086-RQSC' )
                      CHANNEL   ( NM-CNL          )
                      NODATA
                      FLENGTH   ( QT-CCT-LIDO     )
           END-EXEC

           IF  QT-CCT-LIDO                 NOT EQUAL  QT-CCT-EPRO
               SET   ERRO-TAMANHO-AREA   TO  TRUE
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS  GET
                      CONTAINER ( 'SGTSL086-RQSC'         )
                      CHANNEL   ( NM-CNL                  )
                      FLENGTH   ( LENGTH OF SGTSL086-RQSC )
                      INTO      ( SGTSL086-RQSC           )
           END-EXEC
           .
       130000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       140000-VALIDAR-PARAMETRO        SECTION.
      *-----------------------------------------------------------------
           MOVE  0005                  TO  MSG1-CD-SEQL-ERRO

           IF  SGTSL086-CD-TIT-OPR-ETRD    EQUAL  SPACES
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0231              TO  MSG1-CD-ERRO-TS
               MOVE  'Código do título da operação estruturado não infor
      -              'mado.'           TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *    IF  SGTSL086-CD-TIP-SRVC        NOT EQUAL  0002
               IF  SGTSL086-NR-DVS-TIT     NEGATIVE
                   SET   ERRO-PROGRAMADO
                                       TO  TRUE
                   MOVE  0013          TO  MSG1-CD-ERRO-TS
                   MOVE  'Número divisão do título inválido.'
                                       TO  MSG1-TX-ERRO-TS
                   PERFORM  999000-GRAVA-ERRO-E-ENCERRA
               END-IF
               IF  SGTSL086-NR-DVS-TIT     EQUAL  ZEROS
                   SET   ERRO-PROGRAMADO
                                       TO  TRUE
                   MOVE  0232          TO  MSG1-CD-ERRO-TS
                   MOVE  'Número divisão título da operação não informad
      -                  'o.'          TO  MSG1-TX-ERRO-TS
                   PERFORM  999000-GRAVA-ERRO-E-ENCERRA
               END-IF
      *    END-IF

      *--> Se TS não informado, formatar padrão DB2
           IF  SGTSL086-ULT-TS-REG-PGTO    EQUAL  SPACES  OR
               SGTSL086-ULT-TS-REG-PGTO    EQUAL
                                           '0000-00-00-00.00.00.000000'
               MOVE  '0001-01-01-00.00.00.000000'
                                       TO  SGTSL086-ULT-TS-REG-PGTO
           END-IF

           IF  SGTSL086-CD-CTRA-TIT-VL     NEGATIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0001              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' inválido.'      TO  MSG1-TX-ERRO-TS
                  PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL086-CD-CTRA-TIT-VL     EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0002              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' não informado.' TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-PROCESSA-PESQUISA        SECTION.
      *-----------------------------------------------------------------
           MOVE  0006                  TO  MSG1-CD-SEQL-ERRO

           MOVE  CD-TIP-MVTC-TIT                                (IC-TAB)
             TO  SGTSL086-CD-TIP-MVTC-TIT                       (IC-TAB)
           MOVE  CD-USU-IMP                                     (IC-TAB)
             TO  SGTSL086-CD-USU-IMP                            (IC-TAB)
           MOVE  DT-EFT-MVTC                                    (IC-TAB)
             TO  SGTSL086-DT-EFT-MVTC                           (IC-TAB)
           MOVE  DT-PRVT-MVTC                                   (IC-TAB)
             TO  SGTSL086-DT-PRVT-MVTC                          (IC-TAB)
           MOVE  IN-MVTC-ATV                                    (IC-TAB)
             TO  SGTSL086-IN-MVTC-ATV                           (IC-TAB)
           MOVE  TX-DCR-MVTC                                    (IC-TAB)
             TO  SGTSL086-TX-DCR-MVTC                           (IC-TAB)
           MOVE  TS-REG-PGTO                                    (IC-TAB)
             TO  SGTSL086-TS-REG-PGTO                           (IC-TAB)
           MOVE  VL-EFT-MVTC                                    (IC-TAB)
             TO  SGTSL086-VL-EFT-MVTC                           (IC-TAB)
           MOVE  VL-PRVT-MVTC                                   (IC-TAB)
             TO  SGTSL086-VL-PRVT-MVTC                          (IC-TAB)
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-POSTA-RESPOSTA           SECTION.
      *-----------------------------------------------------------------
           MOVE  0007                  TO  MSG1-CD-SEQL-ERRO

           EXEC CICS  PUT
                      CONTAINER ( 'SGTSL086-RPST'         )
                      CHANNEL   ( NM-CNL                  )
                      FLENGTH   ( LENGTH OF SGTSL086-RPST )
                      FROM      ( SGTSL086-RPST           )
           END-EXEC
           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970101-ABRE-CURSOR-PSQ          SECTION.
      *-----------------------------------------------------------------
           MOVE  0008                  TO  MSG1-CD-SEQL-ERRO

           MOVE  'N'                   TO  SGTSL086-IN-CNTO-PSQ

           EVALUATE SGTSL086-CD-TIP-SRVC
           WHEN 1
                EXEC SQL
                     OPEN CURSOR-PSQ
                END-EXEC

           WHEN 2
                EXEC SQL
                     OPEN CURSOR-PSQ-02
                END-EXEC
           END-EVALUATE

           MOVE  'S'                   TO  IN-CURSOR-PSQ-ABTO
           .
       970101-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970102-LER-CURSOR-PSQ           SECTION.
      *-----------------------------------------------------------------
           MOVE  0009                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      DMTV-CX-TIT

           EVALUATE SGTSL086-CD-TIP-SRVC
           WHEN 1
              EXEC SQL
                   FETCH NEXT ROWSET
                         CURSOR-PSQ FOR 0101 ROWS
                  INTO :CD-TIP-MVTC-TIT
                     , :CD-USU-IMP
                     , :DT-EFT-MVTC
                     , :DT-PRVT-MVTC
                       :DT-PRVT-MVTC-N
                     , :IN-MVTC-ATV
                     , :TX-DCR-MVTC
                     , :TS-REG-PGTO
                     , :VL-EFT-MVTC
                       :VL-EFT-MVTC-N
                     , :VL-PRVT-MVTC
                       :VL-PRVT-MVTC-N
              END-EXEC

           WHEN 2
              EXEC SQL
                   FETCH NEXT ROWSET
                         CURSOR-PSQ-02 FOR 0101 ROWS
                  INTO :CD-TIP-MVTC-TIT
                     , :CD-USU-IMP
                     , :DT-EFT-MVTC
                     , :DT-PRVT-MVTC
                       :DT-PRVT-MVTC-N
                     , :IN-MVTC-ATV
                     , :TX-DCR-MVTC
                     , :TS-REG-PGTO
                     , :VL-EFT-MVTC
                       :VL-EFT-MVTC-N
                     , :VL-PRVT-MVTC
                       :VL-PRVT-MVTC-N
              END-EXEC
           END-EVALUATE

           MOVE  SQLERRD(3)            TO  QT-REG-SELD

           IF  QT-REG-SELD                 EQUAL 0101
               SET   SGTSL086-TEM-MAIS-REGISTRO  TO  TRUE
               MOVE  0100              TO  QT-REG-SELD
           END-IF

           MOVE  QT-REG-SELD           TO  SGTSL086-QT-OCR-TAB
           MOVE  SQLCODE               TO  CD-FIM-CURSOR-PSQ
           .
       970102-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970103-FECHA-CURSOR-PSQ         SECTION.
      *-----------------------------------------------------------------
           MOVE  0010                  TO  MSG1-CD-SEQL-ERRO

           MOVE  'N'                   TO  IN-CURSOR-PSQ-ABTO

           EVALUATE SGTSL086-CD-TIP-SRVC
           WHEN 1
                 EXEC SQL
                      CLOSE CURSOR-PSQ
                 END-EXEC

           WHEN 2
                 EXEC SQL
                      CLOSE CURSOR-PSQ-02
                 END-EXEC
           END-EVALUATE
           .
       970103-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------

           IF  IN-CURSOR-PSQ-ABTO          EQUAL  'S'
               PERFORM 970103-FECHA-CURSOR-PSQ
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

           MOVE  'L086-001-CD-TIT-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL086-CD-TIT-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'L086-002-NR-DVS-TIT'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL086-NR-DVS-TIT   TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'L086-003-ULT-TS-REG-PGTO'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL086-ULT-TS-REG-PGTO
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'L086-004-E-DT-EFT-MVTC'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL086-E-DT-EFT-MVTC
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'L086-005-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL086-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'L086-006-QT-OCR-TAB'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSL086-QT-OCR-TAB   TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'L086-007-IC-TAB'     TO  MSGKW999-NM-VRV-MNT
           MOVE  IC-TAB                TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

      *--> Grava a Ocorrencia de Erro no sistema MSG - Mensageria
           PERFORM  MSG1-NTF-MSG

      *--> Grava a Ocorrencia de Erro no Container de Erro
           MOVE  MSG1-CD-ERRO-TS       TO  IIBKERRO-CD-ERRO
           MOVE  MSG1-TX-ERRO-TS       TO  IIBKERRO-TX-ERRO

           EXEC CICS  PUT
                      CONTAINER ( 'PRM-ERRO'                           )
                      CHANNEL   ( NM-CNL                               )
                      FROM      ( IIBKERRO-PRM-ERRO                    )
                      FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO          )
                      NOHANDLE
           END-EXEC

      *--> Encerra a Transacao
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
      *----------------------------------------
       COPY MSGKPMG1.

      *-----------------------------------------------------------------
       999999-ENCERRAR                 SECTION.
      *-----------------------------------------------------------------
           EXEC CICS
                RETURN
           END-EXEC
           .
       999999-SAI.
           EXIT.

      *-------------------------- FIM SGTSL086 -------------------------
