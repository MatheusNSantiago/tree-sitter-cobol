      CBL ARITH(EXTEND)
      *-----------------------------------------------------------------
      *               SISTEMA   SGT   (BASILEIA)
      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSC105 (Conversão do pgm SGTSC016)
      * ANALISTA..: F2020988 Christina Celeste Guimarães
      * AUTOR.....: F0950413 António Francisco Mendonça Silva
      * DATA......: 24/09/2018.
      * OBJETIVO..: Consultar garantias do titulo/ subtitulo.
      * TABELAS UTILIZADAS: DB2SGT.GRT_ATI_FNCO
      * COMPILACAO: COBOL/DB2 (Bach/On-line)
      *-----------------------------------------------------------------
      * VRS001 24.09.2018 - F0950413 - 2018/145195 - Implantação
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTSC105.

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

      *-----------------------------------------------------------------
       WORKING-STORAGE                 SECTION.
      *-----------------------------------------------------------------
       77  CTE-INICIO-WS               PIC X(40) VALUE
                                       '*** WSS COMECA AQUI ***'.
       77  CTE-PROG                    PIC X(18) VALUE
                                       '*** SGTSC105 ***'.
       77  CTE-VERS                    PIC X(06) VALUE 'VRS001'.

      *--> Definição dos Nomes dos Programas Acionados
       01  TAB-PROGRAMAS-CHAMADOS.
           03  GRTS1816                PIC  X(08) VALUE 'GRTS1816'.
           03  GRTSB004                PIC  X(08) VALUE 'GRTSB004'.
           03  GRTST020                PIC  X(08) VALUE 'GRTST020'.

      *--> Definição dos Variaveis do Cobolito
       COPY ALMKCBL1.

      *-----------------------------------------------------------------
      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informacoes
      *-----------------------------------------------------------------
       COPY HLPKE01B.

       01  CTE-FIM-WS                  PIC  X(32)  VALUE
                                             '*** WSS TERMINA AQUI ***'.

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------
       01  CTE-INICIO-LS               PIC  X(32)  VALUE
                                             '*** LS COMECA AQUI ***'.
      *--> Area de trabalho
       01  QT-REG-SELD                 PIC S9(04) COMP-5 VALUE ZEROS.
       01  IC-TAB                      PIC S9(04) COMP-5 VALUE ZEROS.
       01  IND-FMA                     PIC S9(04) COMP-5 VALUE ZEROS.

      *--> Campos utilizados para leitura
       01  REG-WS.
           03 CD-CTRA-SIS-DRIV         PIC S9(09) COMP-5
              OCCURS 101 TIMES         VALUE ZEROS.
           03 NR-SEQL-ATI              PIC S9(09) COMP-5
              OCCURS 101 TIMES         VALUE ZEROS.
           03 CD-STIT-ATI-FNCO         PIC S9(09) COMP-5
              OCCURS 101 TIMES         VALUE ZEROS.
           03 NR-GRT-ATI               PIC S9(09) COMP-5
              OCCURS 101 TIMES         VALUE ZEROS.
           03 CD-ESPC-GRT              PIC S9(04) COMP-5
              OCCURS 101 TIMES         VALUE ZEROS.
           03 CD-FMA-VCLC              PIC S9(04) COMP-5
              OCCURS 101 TIMES         VALUE ZEROS.
           03 VL-ATZD-GRT-VCLD         PIC S9(13)V9(2) COMP-3
              OCCURS 101 TIMES         VALUE ZEROS.
           03 VL-OGNL-GRT-VCLD         PIC S9(13)V9(2) COMP-3
              OCCURS 101 TIMES         VALUE ZEROS.
           03 DT-INC-GRT-ATI           PIC  X(10)
              OCCURS 101 TIMES         VALUE SPACES.
           03 DT-FIM-GRT-ATI           PIC  X(10)
              OCCURS 101 TIMES         VALUE SPACES.
           03 CD-EST-GRT-ATI           PIC S9(04) COMP-5
              OCCURS 101 TIMES         VALUE ZEROS.
           03 CD-USU-MNTC-GRT          PIC  X(08)
              OCCURS 101 TIMES         VALUE SPACES.
           03 CD-USU-CNFC-GRT          PIC  X(08)
              OCCURS 101 TIMES         VALUE SPACES.
      *--> Indicador de nulidade do campo 'CD-USU-CNFC-GRT'
           03 CD-USU-CNFC-GRT-N        PIC S9(04) COMP-5
              OCCURS 101 TIMES         VALUE -1.
           03 CD-PGM-GRT-ATI           PIC  X(08)
              OCCURS 101 TIMES         VALUE SPACES.

      *--> Variavel de controle do cursor.
       01  CTL-CURSOR-PSQ.
           03  CD-FIM-CURSOR-PSQ       PIC S9(04) COMP-5 VALUE ZEROS.
               88  FIM-CURSOR-PSQ                        VALUE +100.
           03  IN-CURSOR-PSQ-ABTO      PIC  X(01)        VALUE 'N'.
               88  CURSOR-PSQ-ABTO                       VALUE 'S'.

      *--> Variavel de controle do cursor1.
       01  CTL-CURSOR-PSQ1.
           03  CD-FIM-CURSOR-PSQ1      PIC S9(04) COMP-5 VALUE ZEROS.
               88  FIM-CURSOR-PSQ1                       VALUE +100.
           03  IN-CURSOR-PSQ1-ABTO     PIC  X(01)        VALUE 'N'.
               88  CURSOR-PSQ1-ABTO                      VALUE 'S'.

      *--> Book do acesso à tabela 'ESPC_GRT'
       01 GRTKB004-DADOS.
       COPY GRTKB004.

      *--> Book do acesso às tabelas 'FMA_VCLC' e 'FMA_VCLC_ESPC_GRT'
       01 GRTK1816-DADOS.
       COPY GRTK1816.

      *--> Book do acesso à tabela 'TIP_GRT'
       01 GRTKT020-DADOS.
       COPY GRTKT020.

      *--> Books da definicao dados a serem usados na subrotina de erros
       COPY HLPKE001.
       COPY HLPKE011.

      *--> Subrotinas utilizadas DB2
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

      *-----------------------------------------------------------------
      * Declaração do cursor de garantias para consulta
      *-----------------------------------------------------------------
           EXEC SQL
             DECLARE CURSOR-GARANTIAS CURSOR
                     WITH ROWSET POSITIONING FOR
             SELECT CD_CTRA_SIS_DRIV
                  , NR_SEQL_ATI
                  , CD_STIT_ATI_FNCO
                  , NR_GRT_ATI
                  , CD_ESPC_GRT
                  , CD_FMA_VCLC
                  , VL_ATZD_GRT_VCLD
                  , VL_OGNL_GRT_VCLD
                  , DT_INC_GRT_ATI
                  , DT_FIM_GRT_ATI
                  , CD_EST_GRT_ATI
                  , CD_USU_MNTC_GRT
                  , CD_USU_CNFC_GRT
                  , CD_PGM_GRT_ATI
              FROM  DB2SGT.GRT_ATI_FNCO  A
              WHERE A.CD_CTRA_SIS_DRIV   = :SGTSC105-CD-CTRA-SIS-DRIV-E
                AND A.CD_STIT_ATI_FNCO   = :SGTSC105-CD-STIT-ATI-FNCO-E
                AND A.NR_SEQL_ATI        = :SGTSC105-NR-SEQL-ATI-E
                AND A.NR_GRT_ATI           > :SGTSC105-NR-GRT-ATI-E
           END-EXEC.

      *-----------------------------------------------------------------
       LINKAGE                         SECTION.
      *-----------------------------------------------------------------
       01  DFHCOMMAREA.
       COPY SGTKC105.

       01  CTE-FIM-LS                  PIC  X(32)  VALUE
                                             '*** LS TERMINA AQUI ***'.
      *-----------------------------------------------------------------
       PROCEDURE                       DIVISION USING DFHCOMMAREA.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       000000-INICIO                   SECTION.
      *-----------------------------------------------------------------

           IF  EIBCALEN  NOT EQUAL LENGTH OF  DFHCOMMAREA
               SET   ERRO-CODIFICADO   TO  TRUE
               MOVE  0001              TO  PSC-ERRO-PGM
               MOVE  0220              TO  CD-RTN-PGM
               MOVE  'Área com tamanho inválido - verifique book/local.'
                                       TO  MSG-ERRO
               PERFORM  999001-ERRO-001
           END-IF

           MOVE  0001                  TO  PSC-ERRO-PGM

           IF  NOT EXECUCAO-ONLINE  AND
               NOT EXECUCAO-BATCH
               CALL  CICS5000       USING  CICS5000-COMMAREA
           END-IF

      *--> Habilita Testes no Cobolito e Debug de Operacoes IIB
       COPY ALMKCBL2.

      *--> WHENEVER DB2
       COPY HLPKE002.

      *--> WHENEVER CICS
       COPY HLPKE003.

           PERFORM 100000-VALIDAR-PARAMETRO

           PERFORM 970101-ABRE-CURSOR-PSQ
           PERFORM 970102-LER-CURSOR-PSQ
           PERFORM 970103-FECHA-CURSOR-PSQ

           PERFORM  200000-MONTA-SAIDA-DET
                    VARYING IC-TAB
                    FROM 0001 BY 0001
                    UNTIL IC-TAB           > SGTSC105-QT-REG-DET
           GOBACK
           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-VALIDAR-PARAMETRO        SECTION.
      *-----------------------------------------------------------------
           MOVE  0002                  TO  PSC-ERRO-PGM
           INITIALIZE SGTSC105-AREA-DADOS

           IF  NOT SGTSC105-CONSULTAR
               SET   ERRO-CODIFICADO   TO  TRUE
               MOVE  0116              TO  CD-RTN-PGM
               MOVE  'Ação inválida.'  TO  MSG-ERRO
               PERFORM  999001-ERRO-001
           END-IF

           IF  SGTSC105-CD-CTRA-SIS-DRIV-E
                                           NEGATIVE
               SET   ERRO-CODIFICADO   TO  TRUE
               MOVE  0001              TO  CD-RTN-PGM
               MOVE  'Código da carteira sistema drive inválido.'
                                       TO  MSG-ERRO
               PERFORM  999001-ERRO-001
           END-IF

           IF  SGTSC105-CD-CTRA-SIS-DRIV-E
                                           EQUAL  ZEROS
               SET   ERRO-CODIFICADO   TO  TRUE
               MOVE  0002              TO  CD-RTN-PGM
               MOVE  'Código da carteira sistema drive não informado.'
                                       TO  MSG-ERRO
               PERFORM  999001-ERRO-001
           END-IF

      *    IF  SGTSC105-CD-STIT-ATI-FNCO-E
      *                                    EQUAL  ZEROS
      *        MOVE  'Codigo subtitulo ativo financeiro Invalido.'
      *                                TO  MSG-ERRO
      *        PERFORM  999001-ERRO-001
      *     END-IF

      *    IF  SGTSC105-NR-SEQL-ATI-E      EQUAL  ZEROS
      *        MOVE  'Numero sequencial ativo Invalido.'
      *                                TO  MSG-ERRO
      *        PERFORM  999001-ERRO-001
      *    END-IF
           .
       100000-SAI.
           EXIT.


      *----------------------------------------------------------------
       200000-MONTA-SAIDA-DET          SECTION.
      *----------------------------------------------------------------
           MOVE  0003                  TO  PSC-ERRO-PGM

           MOVE  CD-CTRA-SIS-DRIV                (IC-TAB)
             TO  SGTSC105-CD-CTRA-SIS-DRIV-S     (IC-TAB)
           MOVE  NR-SEQL-ATI                                    (IC-TAB)
             TO  SGTSC105-NR-SEQL-ATI-S                         (IC-TAB)
           MOVE  CD-STIT-ATI-FNCO                               (IC-TAB)
             TO  SGTSC105-CD-STIT-ATI-FNCO-S                    (IC-TAB)
           MOVE  NR-GRT-ATI                                     (IC-TAB)
             TO  SGTSC105-NR-GRT-ATI-S                          (IC-TAB)
           MOVE  CD-ESPC-GRT                                    (IC-TAB)
             TO  SGTSC105-CD-ESPC-GRT-S                         (IC-TAB)
           MOVE  CD-FMA-VCLC                                    (IC-TAB)
             TO  SGTSC105-CD-FMA-VCLC-S                         (IC-TAB)
           MOVE  VL-ATZD-GRT-VCLD                               (IC-TAB)
             TO  SGTSC105-VL-ATZD-GRT-VCLD-S                    (IC-TAB)
           MOVE  VL-OGNL-GRT-VCLD                               (IC-TAB)
             TO  SGTSC105-VL-OGNL-GRT-VCLD-S                    (IC-TAB)
           MOVE  DT-INC-GRT-ATI                                 (IC-TAB)
             TO  SGTSC105-DT-INC-GRT-ATI-S                      (IC-TAB)
           MOVE  DT-FIM-GRT-ATI                                 (IC-TAB)
             TO  SGTSC105-DT-FIM-GRT-ATI-S                      (IC-TAB)
           MOVE  CD-EST-GRT-ATI                                 (IC-TAB)
             TO  SGTSC105-CD-EST-GRT-ATI-S                      (IC-TAB)
           MOVE  CD-USU-MNTC-GRT                                (IC-TAB)
             TO  SGTSC105-CD-USU-MNTC-GRT-S                     (IC-TAB)
           MOVE  CD-USU-CNFC-GRT                                (IC-TAB)
             TO  SGTSC105-CD-USU-CNFC-GRT-S                     (IC-TAB)
           MOVE  CD-PGM-GRT-ATI                                 (IC-TAB)
             TO  SGTSC105-CD-PGM-GRT-ATI-S                      (IC-TAB)
           MOVE  SPACES
             TO  SGTSC105-TX-OBS-CTRA-S                         (IC-TAB)

           PERFORM  210000-BUSCA-DADOS-ESPECIE

           PERFORM  220000-BUSCA-DADOS-TIP-GRT

           PERFORM  230000-BUSCA-DADOS-FMA-VCLC


           MOVE  SPACES                TO SGTSC105-NM-NTZ-GRT-S(IC-TAB)

           EVALUATE  GRTSB004-CD-NTZ-GRT
               WHEN  0001
                     MOVE  'REAL'
                     TO  SGTSC105-NM-NTZ-GRT-S                  (IC-TAB)
               WHEN  0002
                     MOVE  'FIDEJUSSORIA'
                     TO  SGTSC105-NM-NTZ-GRT-S                  (IC-TAB)
               WHEN  0003
                     MOVE  'REAL - OPERACAO BB'
                     TO  SGTSC105-NM-NTZ-GRT-S                  (IC-TAB)
               WHEN  0004
                     MOVE  'OUTRAS'
                     TO  SGTSC105-NM-NTZ-GRT-S                  (IC-TAB)
           END-EVALUATE

           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       210000-BUSCA-DADOS-ESPECIE      SECTION.
      *-----------------------------------------------------------------
           MOVE  0004                  TO  PSC-ERRO-PGM

           MOVE  ZEROS                 TO  SGTSC105-CD-NTZ-GRT-S(IC-TAB)
           MOVE  ZEROS                 TO  SGTSC105-CD-TIP-GRT-S(IC-TAB)

           INITIALIZE                      GRTKB004-DADOS

           MOVE  0004                  TO  GRTSB004-ACAO
           MOVE  CD-ESPC-GRT (IC-TAB)  TO  GRTSB004-CD-ESPC-GRT

           MOVE  LENGTH OF GRTKB004-DADOS
                                       TO  EIBCALEN
           CALL  GRTSB004           USING  DFHEIBLK GRTKB004-DADOS.

           IF  GRTSB004-CD-RTN-PGM         EQUAL  ZEROS
               MOVE  GRTSB004-CD-NTZ-GRT
                 TO  SGTSC105-CD-NTZ-GRT-S                      (IC-TAB)
               MOVE  GRTSB004-CD-TIP-GRT
                 TO  SGTSC105-CD-TIP-GRT-S                      (IC-TAB)
               MOVE  GRTSB004-NM-ESPC-GRT
                 TO  SGTSC105-NM-ESPC-GRT-S                     (IC-TAB)
           END-IF

           .
       210000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       220000-BUSCA-DADOS-TIP-GRT      SECTION.
      *-----------------------------------------------------------------
           MOVE  0005                  TO  PSC-ERRO-PGM

           INITIALIZE                      GRTKT020-DADOS

           MOVE  0003                  TO  KT020-CD-OPC-E
           MOVE  GRTSB004-CD-TIP-GRT   TO  KT020-CD-TIP-GRT-E

           MOVE  LENGTH OF GRTKT020-DADOS
                                       TO  EIBCALEN

           CALL GRTST020 USING DFHEIBLK GRTKT020-DADOS.

           IF  KT020-CD-RTNO               EQUAL  ZEROS
               MOVE  KT020-TX-DCR-TIP-GRT-S (1)
                                       TO  SGTSC105-NM-TIP-GRT-S(IC-TAB)
           END-IF
           .
       220000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       230000-BUSCA-DADOS-FMA-VCLC     SECTION.
      *-----------------------------------------------------------------
           MOVE  0006                  TO  PSC-ERRO-PGM

           MOVE 'Forma não localizada no GRT'
                                       TO SGTSC105-NM-FMA-VCLC-S(IC-TAB)

           INITIALIZE                      GRTK1816-DADOS

           MOVE  0001                  TO  K1816-TIPO-CONSULTA
           MOVE  CD-ESPC-GRT (IC-TAB)  TO  K1816-CD-ESPC-GRT

           MOVE  LENGTH OF GRTK1816-DADOS
                                       TO  EIBCALEN
           CALL GRTS1816 USING DFHEIBLK GRTK1816-DADOS.

           IF  K1816-COD-ERRO              EQUAL  ZEROS
               MOVE  ZEROS             TO  IND-FMA
               PERFORM  UNTIL  IND-FMA     EQUAL  K1816-QTD-REG
                 ADD   0001            TO  IND-FMA
                 IF  K1816-CD-FMA-VCLC (IND-FMA)
                                           EQUAL
                                           CD-FMA-VCLC      (IC-TAB)
                     MOVE  K1816-NM-FMA-VCLC (IND-FMA)
                                       TO SGTSC105-NM-FMA-VCLC-S(IC-TAB)
                     MOVE  K1816-QTD-REG
                                       TO  IND-FMA
                 END-IF
               END-PERFORM
           END-IF
           .
       230000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970101-ABRE-CURSOR-PSQ          SECTION.
      *-----------------------------------------------------------------
           MOVE  0007                  TO  PSC-ERRO-PGM

           EXEC SQL
                OPEN CURSOR-GARANTIAS
           END-EXEC

           MOVE  'S'                   TO  IN-CURSOR-PSQ-ABTO
           .
       970101-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970102-LER-CURSOR-PSQ           SECTION.
      *-----------------------------------------------------------------
           MOVE  0008                  TO  PSC-ERRO-PGM

           INITIALIZE                  REG-WS

           EXEC SQL
               FETCH NEXT ROWSET
                          CURSOR-GARANTIAS FOR 0101 ROWS
               INTO :CD-CTRA-SIS-DRIV
                  , :NR-SEQL-ATI
                  , :CD-STIT-ATI-FNCO
                  , :NR-GRT-ATI
                  , :CD-ESPC-GRT
                  , :CD-FMA-VCLC
                  , :VL-ATZD-GRT-VCLD
                  , :VL-OGNL-GRT-VCLD
                  , :DT-INC-GRT-ATI
                  , :DT-FIM-GRT-ATI
                  , :CD-EST-GRT-ATI
                  , :CD-USU-MNTC-GRT
                  , :CD-USU-CNFC-GRT
                    :CD-USU-CNFC-GRT-N
                  , :CD-PGM-GRT-ATI
           END-EXEC

           MOVE  SQLERRD(3)            TO  QT-REG-SELD

           IF  QT-REG-SELD                 EQUAL  0101
               SET   SGTSC105-TEM-MAIS-REGISTRO TO TRUE
               MOVE  0100              TO  QT-REG-SELD
           END-IF

           MOVE  QT-REG-SELD           TO  SGTSC105-QT-REG-DET

           IF  SGTSC105-QT-REG-DET         GREATER  ZEROS
               MOVE  NR-GRT-ATI (SGTSC105-QT-REG-DET)
                                       TO  SGTSC105-NR-PSQ-FIM-S
           END-IF

           MOVE  SQLCODE               TO  CD-FIM-CURSOR-PSQ
           .
       970102-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970103-FECHA-CURSOR-PSQ         SECTION.
      *-----------------------------------------------------------------
           MOVE  0009                  TO  PSC-ERRO-PGM

           MOVE  'N'                   TO  IN-CURSOR-PSQ-ABTO

           EXEC SQL
                CLOSE CURSOR-GARANTIAS
           END-EXEC
           .
       970103-SAI.
           EXIT.

      *-----------------------------------------------------------------
      * 999000-ERRO                    SECTION
      *-----------------------------------------------------------------
      *-----------------
      * 999001-ERRO-001.
      *-----------------
       COPY HLPKE004.
       COPY HLPKE012.

           IF  IN-CURSOR-PSQ-ABTO          EQUAL  'S'
               PERFORM  970103-FECHA-CURSOR-PSQ
           END-IF

           PERFORM  999080-LISTA-EXCECAO-DSNTIAR

           PERFORM  999082-PREPARA-ERRO

           MOVE  RTN-PGM               TO  SGTSC105-RTN-PGM

           GOBACK
           .
      *-----------------
      * 999002-ERRO-DB2
      *-----------------
       COPY HLPKE005.

      *-----------------
      * 999003-TRATA-ABEND-CICS
      * 999004-TRATA-CONDITION-CICS
      *-----------------
       COPY HLPKE006.

      *-----------------------------
      * 999080-LISTA-EXCECAO-DSNTIAR
      *-----------------------------
       COPY HLPKE007.

      *------------------------
      * 999081-ADICIONA-EXCECAO
      *------------------------
       COPY HLPKE008.

      *----------------------------
       999082-PREPARA-ERRO             SECTION.
      *----------------------------
           MOVE 'PSC-ERRO-PGM'         TO  HLPKE001-EXCO-NM-VRV
           MOVE  PSC-ERRO-PGM          TO  HLPKE001-EXCO-CTU-NUM
           PERFORM 999081-ADICIONA-EXCECAO

           MOVE 'CD-RTN-PGM'           TO  HLPKE001-EXCO-NM-VRV
           MOVE  CD-RTN-PGM            TO  HLPKE001-EXCO-CTU-NUM
           PERFORM 999081-ADICIONA-EXCECAO

           MOVE 'MSG-ERRO-PGM'         TO  HLPKE001-EXCO-NM-VRV
           MOVE  MSG-ERRO-PGM          TO  HLPKE001-EXCO-CTU-VRV
           PERFORM 999081-ADICIONA-EXCECAO

           MOVE 'SGTSC105-CD-CTRA-SIS-DRIV-E'
                                       TO  HLPKE001-EXCO-NM-VRV
           MOVE  SGTSC105-CD-CTRA-SIS-DRIV-E
                                       TO  HLPKE001-EXCO-CTU-NUM
           PERFORM 999081-ADICIONA-EXCECAO

           MOVE 'SGTSC105-CD-STIT-ATI-FNCO-E'
                                       TO  HLPKE001-EXCO-NM-VRV
           MOVE  SGTSC105-CD-STIT-ATI-FNCO-E
                                       TO  HLPKE001-EXCO-CTU-NUM
           PERFORM 999081-ADICIONA-EXCECAO

           MOVE 'SGTSC105-NR-GRT-ATI-E'
                                       TO  HLPKE001-EXCO-NM-VRV
           MOVE  SGTSC105-NR-GRT-ATI-E
                                       TO  HLPKE001-EXCO-CTU-NUM
           PERFORM 999081-ADICIONA-EXCECAO

           MOVE 'SGTSC105-NR-SEQL-ATI-E'
                                       TO  HLPKE001-EXCO-NM-VRV
           MOVE  SGTSC105-NR-SEQL-ATI-E
                                       TO  HLPKE001-EXCO-CTU-NUM
           PERFORM 999081-ADICIONA-EXCECAO
           .
       999082-SAI.
           EXIT.

      *--> EFETUA A CHAMADA DA SUBROTINA HLPS0001
       COPY HLPKE009.
      *---------------------- FIM SGTSC105 ----------------------------
