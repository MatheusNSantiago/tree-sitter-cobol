       CBL ARITH(EXTEND)
      *-----------------------------------------------------------------
      * PROGRAMA..: SGTS0292
      * ANALISTA..: F8785286 - Roseli Akemi Kawahata
      * AUTOR.....: F8785286 - Roseli Akemi Kawahata
      * DATA......: 22/10/2019
      * OBJETIVO..: Atualizar Estado da Ordem de Negociação na Opção
      *              Aprovar Cadastro Venda (3 para 4).
      * COMPILACAO: 52 - PSOSE600 - Cobol 4.2 c/otimizacao p/producao
      *-----------------------------------------------------------------
      * VRS001 03.12.2019 - F0950413 - 2016/145195 - Ajustes nas mensa-
      *        gens de erro 1069 e 1079 e monitotação da variável
      *        TX-DTQ-MSG
      *        22.10.2019 - F8785286 - 2016/145195 - Implantação
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTS0292.

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
       77  CTE-INICIO-WS               PIC X(40) VALUE
                                       '*** WSS COMECA AQUI ***'.
       77  CTE-PROG                    PIC X(18) VALUE
               '*** SGTS0292 ***'.
       77  CTE-VERS                    PIC X(06) VALUE 'VRS001'.

      *--> Módulos a serem acionados via 'CONTAINER'
       77  SGTSE078                    PIC X(08) VALUE 'SGTSE078'.
       77  SGTSI078                    PIC X(08) VALUE 'SGTSI078'.
       77  SGTSL004                    PIC X(08) VALUE 'SGTSL004'.
       77  SGTSL069                    PIC X(08) VALUE 'SGTSL069'.
       77  SGTSLA75                    PIC X(08) VALUE 'SGTSLA75'.

       77  SGTST005                    PIC  X(08) VALUE 'SGTST005'.

      * Book do programa SBTIMEDT para obter timestamp atual
       77  SBTIMEDT                    PIC  X(8)   VALUE 'SBTIMEDT'.

       01  CTE-FIM-WS                  PIC X(32)  VALUE
                                             '*** WSS TERMINA AQUI ***'.
      *-----------------------------------------------------------------
      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informacoes
-INC HLPKE01B

      *-----------------------------------------------------------------
       LOCAL-STORAGE SECTION.
      *-----------------------------------------------------------------
       01  CTE-INICIO-LS               PIC  X(32)  VALUE
                                             '*** LS COMECA AQUI ***'.

       01  CD-CTRA-TVM-TX.
           03  CD-CTRA-TVM-DISP        PIC  9(9).
       01  CD-CTRA-TVM-EDIT            PIC  ZZZ.ZZZ.ZZ9.

       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP-DISP    PIC  9(11).

      *--> Areas de trabalho
       01  AUX-CD-TIP-EST-NEGO         PIC  9999.

       01  TX-DTQ-MSG                  PIC  X(12) VALUE ' '.

       01  REG-WS.
           03 CD-TIP-EST-NEGO          PIC S9(09) COMP-5 VALUE 0.
           03 TS-EST-ORD-NEGO          PIC  X(26) VALUE ' '.
           03 CD-USU-ULT-ATL           PIC  X(08) VALUE ' '.
           03 NR-VRS-OPR-ETRD          PIC S9(04) COMP-5 VALUE 0.
           03 NR-SEQL-ORD-NEGO         PIC S9(09) COMP-5 VALUE 0.

      *--> Indicadores de nulidades
       01  AUX-DT-HJ-INV               PIC 9(08).
       01  AUX-DT-HJ-INV-R   REDEFINES AUX-DT-HJ-INV.
           03 ANO                      PIC 9(04).
           03 MES                      PIC 9(02).
           03 DIA                      PIC 9(02).

       01  AUX-DT-VND-INV              PIC 9(08).
       01  AUX-DT-VND-INV-R  REDEFINES AUX-DT-VND-INV.
           03 ANO                      PIC 9(04).
           03 MES                      PIC 9(02).
           03 DIA                      PIC 9(02).

      *    Campos utilizados para obtencao de timestamp atual
       01  DADOS-SBTIMEDT.
           03  HLPKE001-CD-FUC         PIC  X(03)  VALUE 'F01'.
           03  HLPKE001-AREA-TRB.
               05  HLPKE001-AREA-TRB-TAM
                                       PIC S9(9)   COMP VALUE 1024.
               05  HLPKE001-AREA-TRB-DADOS
                                       PIC  X(1024) VALUE ' '.
           03  HLPKE001-FMT-TS         PIC S9(4)   COMP VALUE 1.
           03  HLPKE001-TAM-TS         PIC S9(4)   COMP VALUE 30.
           03  HLPKE001-TS-FMTD.
               05  ANO                 PIC  9(04).
               05  FILLER              PIC  X(01).
               05  MES                 PIC  9(02).
               05  FILLER              PIC  X(01).
               05  DIA                 PIC  9(02).
               05  FILLER              PIC  X(01).
               05  HH                  PIC  X(02).
               05  FILLER              PIC  X(01).
               05  MNTO                PIC  X(02).
               05  FILLER              PIC  X(01).
               05  SGDO                PIC  X(02).
               05  FILLER              PIC  X(01).
               05  MSGD                PIC  X(06).

       01  TS-SQL-ATU                  PIC  X(26)
                             VALUE '0000-00-00-00.00.00.000000'.
       01  TS-SQL-ATU-R      REDEFINES TS-SQL-ATU.
           03  DT-SQL-ATU              PIC  X(10).
           03  DT-SQL-ATU-R  REDEFINES DT-SQL-ATU.
               05  ANO                 PIC  9(04).
               05  FILLER              PIC  X(01).
               05  MES                 PIC  9(02).
               05  FILLER              PIC  X(01).
               05  DIA                 PIC  9(02).
           03  FILLER                  PIC  X(01).
           03  HH                      PIC  X(02).
           03  FILLER                  PIC  X(01).
           03  MNTO                    PIC  X(02).
           03  FILLER                  PIC  X(01).
           03  SGDO                    PIC  X(02).
           03  FILLER                  PIC  X(01).
           03  MSGD                    PIC  X(06).

       01  TS-ATU-APVC                 PIC  X(26).

      *--> Auxiliares para leitura da tabela ORD_NEGO_OPR
       01  DADOS-ATL.
           03 CD-CTRA-TIT-VL-ORD       PIC S9(09) COMP-5
                                       OCCURS 0101 TIMES  VALUE ZEROES.
           03 CD-OPR-ETRD-MCP-ORD      PIC S9(11) COMP-3
                                       OCCURS 0101 TIMES  VALUE ZEROES.
           03 NR-VRS-OPR-ETRD-ORD      PIC S9(04) COMP-5
                                       OCCURS 0101 TIMES  VALUE ZEROES.
           03 NR-SEQL-ORD-NEGO-ORD     PIC S9(09) COMP-5
                                       OCCURS 0101 TIMES  VALUE ZEROES.
           03 CD-MOD-ORD-NEGO-ORD      PIC S9(09) COMP-5
                                       OCCURS 0101 TIMES  VALUE ZEROES.
           03 VL-PU-PAPL-VND-ORD       PIC S9(15)V99  COMP-3
                                       OCCURS 0101 TIMES  VALUE ZEROES.
           03 QT-TTL-PAPL              PIC S9(09) COMP-5
                                       OCCURS 0101 TIMES  VALUE ZEROES.
           03 CD-TIP-EST-NEGO-ORD      PIC S9(09) COMP-5
                                       OCCURS 0101 TIMES  VALUE ZEROES.

      *--> Indicadores de nulidades
           03 VL-PU-PAPL-VND-ORD-N     PIC S9(04) COMP-5
                                       OCCURS 0101 TIMES  VALUE ZEROES.
           03 QT-TTL-PAPL-N            PIC S9(04) COMP-5
                                       OCCURS 0101 TIMES  VALUE ZEROES.
      *--> Variáveis de trabalho
       01  AUXILIARES.
           03  IC-TAB                  PIC S9(04) COMP-5 VALUE 0.
           03  IDX-ORD                 PIC S9(04) COMP-5 VALUE 2.
           03  AUX-CD-TIP-NEGO         PIC S9(04) COMP-5 VALUE 2.
           03  AUX-CD-TIPO-EST-NEGO    PIC S9(09) COMP-5 VALUE 4.
           03  AX-SQLCODE              PIC S9(05).
           03  AX-SQLCODE-EDIT         PIC ZZZZ9-.
           03  IND-FIM-SLCT            PIC  X(001) VALUE 'N'.
           03  AUX-QT-PAPL-AUTD        PIC S9(9)  COMP-5 VALUE 0.
           03  QT-PAPL-TIT-DSP         PIC S9(9)  COMP-5 VALUE 0.

      *--> Book do container de erro
-INC IIBKERRO
      *--> Book para captura de erro via MSG1
-INC MSGKWMG1
      *--> Variaveis para tratamento de Abend e Condition
-INC HLPKW999
      *--> Variaveis para tratamento de CONTAINER
-INC SGTKERR1
      *--> Book dos dados do usuário
-INC IIBKOPRD

       01  WS-ERRO-CICS.
           03  WS-RESP1                PIC  9(008).
           03  WS-RESP2                PIC  9(008).

      *--> Máscaras edição de variáveis p/ monitoração mensagens de erro
       01  MCR-S9-15                   PIC  ---------------9.
       01  MCR-S9-15-V9-04             PIC  ----.---.---.---.--9,9999.

      *-----------------------------------------------------------------
      *--> Lista Posição Diária de Saldo - DB2SGT.PSC_DRIA_SDO_OPR
      *-->       SGTSL004
-INC SGTKL004

      *--> Exclusão na tabela 'DB2SGT.EST_ORD_NEGO_OPR'
       01  SGTSE078-DADOS.
-INC SGTKE078

      *--> Inclusão na tabela 'DB2SGT.EST_ORD_NEGO_OPR'
       01  SGTSI078-DADOS.
-INC SGTKI078

      * Lista versão aprovada - DB2SGT.EST_VRS_OPR_ETRD
       01  SGTSL069-DADOS.
-INC SGTKL069

      * Lista Ordens de Negociação - DB2SGT.ORD_NEGO_OPR
-INC SGTKLA75

      * Book SGT -  Lista da tabela DB2SGT.OPR_ETRD_MCP - SGTST005
       01  SGTST005-DADOS.
-INC SGTKT005

      *--> Book do programa
       01  SGTS0292-DADOS.
-INC SGTK0292

      *--> Area de trabalho SQL/ DB2.
      *-----------------------------------------------------------------
      * Consulta se existem Ordens de Venda já autorizados, porém ainda
      *   não concluídos.
      *-----------------------------------------------------------------
           EXEC SQL
           DECLARE  CURSOR-ORD-NEGO     CURSOR
                     WITH ROWSET POSITIONING FOR
              SELECT A.CD_CTRA_TIT_VL
                   , A.CD_OPR_ETRD_MCP
                   , A.NR_VRS_OPR_ETRD
                   , A.NR_SEQL_ORD_NEGO
                   , A.CD_MOD_ORD_NEGO
                   , A.VL_PU_PAPL_VND
                   , A.QT_TTL_PAPL
                   , B.CD_TIP_EST_NEGO
              FROM DB2SGT.ORD_NEGO_OPR A

              INNER JOIN DB2SGT.EST_ORD_NEGO_OPR  B
              ON  B.CD_CTRA_TIT_VL   =  A.CD_CTRA_TIT_VL
              AND B.CD_OPR_ETRD_MCP  =  A.CD_OPR_ETRD_MCP
              AND B.NR_VRS_OPR_ETRD  =  A.NR_VRS_OPR_ETRD
              AND B.NR_SEQL_ORD_NEGO =  A.NR_SEQL_ORD_NEGO


              INNER JOIN DB2SGT.MOD_ORD_NEGO  D
              ON D.CD_MOD_ORD_NEGO = A.CD_MOD_ORD_NEGO
              AND D.CD_TIP_NEGO = :AUX-CD-TIP-NEGO   -- (2)

              WHERE A.CD_CTRA_TIT_VL    = :SGTS0292-CD-CTRA-TVM
                AND A.CD_OPR_ETRD_MCP   = :SGTS0292-CD-OPR-ETRD-MCP
                AND A.NR_VRS_OPR_ETRD   = :SGTS0292-NR-VRS-OPR-ETRD
                AND B.CD_TIP_EST_NEGO   = :AUX-CD-TIPO-EST-NEGO  -- (4)
                AND B.TS_EST_ORD_NEGO   =
                  (SELECT  MAX (C.TS_EST_ORD_NEGO)
                     FROM  DB2SGT.EST_ORD_NEGO_OPR  C
                     WHERE C.CD_CTRA_TIT_VL   =  B.CD_CTRA_TIT_VL
                       AND C.CD_OPR_ETRD_MCP  =  B.CD_OPR_ETRD_MCP
                       AND C.NR_VRS_OPR_ETRD  =  B.NR_VRS_OPR_ETRD
                       AND C.NR_SEQL_ORD_NEGO =  B.NR_SEQL_ORD_NEGO)
           END-EXEC

           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

       01  CTE-FIM-LS                  PIC  X(32)  VALUE
                                             '*** LS TERMINA AQUI ***'.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------
           MOVE 0001                   TO  MSG1-CD-SEQL-ERRO

      *--> Habilita tratamento automatico de Erros DB2
-INC HLPKE997
      *--> Habilita captura automatica de abend e erros CICS
-INC HLPKE999
           MOVE CTE-PROG(5:8)          TO NM-PGM

           PERFORM 100000-OBTER-NOME-DO-CANAL
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO
           PERFORM 971000-LER-REQUISICAO

           PERFORM 120000-VALIDAR-PARAMETROS
           PERFORM 974000-LER-DADOS-OPRD
           PERFORM 130000-CONSISTE-EST-OPR
           PERFORM 140000-CONSISTE-ESTADO-ORD
           PERFORM 150000-CONSISTE-SALDO

           PERFORM 200000-AUTORIZA-ORDEM-VENDA
           PERFORM 300000-VERIFICA-RETROATIVO

           PERFORM 999999-ENCERRAR
           GOBACK
           .
       000000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       100000-OBTER-NOME-DO-CANAL      SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO  MSG1-CD-SEQL-ERRO

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CANAL             TO TRUE

           EXEC CICS ASSIGN
                CHANNEL ( NM-CNL )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       100000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       120000-VALIDAR-PARAMETROS       SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                   TO  MSG1-CD-SEQL-ERRO

           IF  SGTS0292-CD-CTRA-TVM    NOT > ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0001               TO MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de títulos e valores mobiliários
      -             ' inválido.'       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0292-CD-OPR-ETRD-MCP NOT NUMERIC
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0003               TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -             ' numérico.'       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           ELSE
            IF SGTS0292-CD-OPR-ETRD-MCP = 0
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0005               TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -             ' informado.'      TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
            END-IF
           END-IF

           IF  SGTS0292-NR-VRS-OPR-ETRD NOT > ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0006               TO MSG1-CD-ERRO-TS
               MOVE 'Número da versão da operação estruturada inválido.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0292-NR-SEQL-ORD-NEGO NOT > ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 1084               TO MSG1-CD-ERRO-TS
               MOVE 'Número sequencial da ordem de negociação inválido.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           PERFORM 121000-BUSCA-TS-DT
           .
       120000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       121000-BUSCA-TS-DT              SECTION.
      *-----------------------------------------------------------------
           MOVE 0004                   TO  MSG1-CD-SEQL-ERRO

           CALL SBTIMEDT            USING  HLPKE001-CD-FUC
                                           HLPKE001-AREA-TRB
                                           HLPKE001-FMT-TS
                                           HLPKE001-TAM-TS
                                           HLPKE001-TS-FMTD

           IF  RETURN-CODE             NOT EQUAL ZEROS
               SET ERRO-PROGRAMA-ACIONADO  TO  TRUE
               MOVE 050                    TO  MSG1-CD-ERRO-TS
               MOVE 'Erro ao obter data e hora atual.'
                                           TO  MSG1-TX-ERRO-TS
                                               MSG1-TX-RTN-PGM-ACND
               MOVE SBTIMEDT               TO  MSG1-NM-PGM-ACND
               MOVE RETURN-CODE            TO  MSG1-CD-RTN-PGM-ACND
               MOVE MSG1-CD-SEQL-ERRO      TO  MSG1-CD-SEQL-PGM-ACND
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           MOVE      HLPKE001-TS-FMTD  TO TS-SQL-ATU
           .
       121000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       130000-CONSISTE-EST-OPR         SECTION.
      *    Consulta estado atual da ordem de operação.
      *    Caso o estado não seja encarteirado/aprovado, não pode vincu-
      *    lar ordem de venda á oper. estruturada.
      *-----------------------------------------------------------------
           MOVE 0005                   TO  MSG1-CD-SEQL-ERRO

           MOVE SGTSL069               TO NM-PGM
           SET  SGTSL069-LISTA-OPR     TO TRUE
           MOVE SGTS0292-CD-CTRA-TVM
                                       TO SGTSL069-CD-CTRA-TIT-VL
           MOVE SGTS0292-CD-OPR-ETRD-MCP
                                       TO SGTSL069-CD-OPR-ETRD-MCP

           PERFORM 900000-EXECUTAR-MODULO

           IF SGTSL069-CD-TIP-EST-OPR(1) NOT EQUAL 5
              OR
              SGTSL069-CD-TIP-EST-VRS(1) NOT EQUAL 3
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  'Finalização'     TO  TX-DTQ-MSG
               MOVE  1069              TO  MSG1-CD-ERRO-TS
               STRING 'Código do estado da operação não permite '
                       DELIMITED BY SIZE
                       TX-DTQ-MSG
                       DELIMITED BY SPACES
                       ' de cadastramento de ordem de venda.'
                       DELIMITED BY SIZE
                       INTO                MSG1-TX-ERRO-TS
               END-STRING
               PERFORM  969000-MONITORAR-VARIAVEIS
               PERFORM  999000-ERRO
           END-IF

           IF SGTSL069-NR-VRS-OPR-ETRD-S(1) NOT EQUAL
              SGTS0292-NR-VRS-OPR-ETRD
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0514              TO  MSG1-CD-ERRO-TS
               MOVE  'Foi encontrada versão mais atualizada da operação
      -              'vinculada.'      TO  MSG1-TX-ERRO-TS
               PERFORM  969000-MONITORAR-VARIAVEIS
               PERFORM  999000-ERRO
           END-IF
           .
       130000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       140000-CONSISTE-ESTADO-ORD      SECTION.
      *    Consulta estado atual da ordem de Negociação.
      *    Caso o estado não seja = 3 (A autorizar), não pode efetuar a
      *    Autorização do Cadastro
      *-----------------------------------------------------------------
           MOVE 0006                   TO  MSG1-CD-SEQL-ERRO

           EXEC SQL
               SELECT A.CD_TIP_EST_NEGO
                    , A.TS_EST_ORD_NEGO
                    , A.CD_USU_ULT_ATL
               INTO  :CD-TIP-EST-NEGO
                  ,  :TS-EST-ORD-NEGO
                  ,  :CD-USU-ULT-ATL
               FROM DB2SGT.EST_ORD_NEGO_OPR A
               WHERE A.CD_CTRA_TIT_VL  = :SGTS0292-CD-CTRA-TVM
                AND A.CD_OPR_ETRD_MCP  = :SGTS0292-CD-OPR-ETRD-MCP
                AND A.NR_VRS_OPR_ETRD  = :SGTS0292-NR-VRS-OPR-ETRD
                AND A.NR_SEQL_ORD_NEGO = :SGTS0292-NR-SEQL-ORD-NEGO
                AND A.TS_EST_ORD_NEGO  =
                       (SELECT MAX (B.TS_EST_ORD_NEGO)
                        FROM DB2SGT.EST_ORD_NEGO_OPR B
                        WHERE B.CD_CTRA_TIT_VL   = A.CD_CTRA_TIT_VL
                          AND B.CD_OPR_ETRD_MCP  = A.CD_OPR_ETRD_MCP
                          AND B.NR_VRS_OPR_ETRD  = A.NR_VRS_OPR_ETRD
                          AND B.NR_SEQL_ORD_NEGO = A.NR_SEQL_ORD_NEGO)

                FETCH FIRST 1 ROW ONLY
           END-EXEC

      *--> Consistência do Operador Logado
           IF  CD-USU-ULT-ATL              =  KOPRD-CD-OPRD
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 1103               TO MSG1-CD-ERRO-TS
               MOVE 'Operador logado deve ser diferente do operador que
      -             'efetuou a conferência da Ordem de Negociação.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *--> Consistência da permissão para confirmação de Ordem de Negoc.
      *--> CD-TIP-EST-NEGO = 3 (A Autorizar)
           IF  CD-TIP-EST-NEGO    NOT = 3
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE CD-TIP-EST-NEGO    TO AUX-CD-TIP-EST-NEGO
               MOVE 'Autorização'      TO TX-DTQ-MSG
               MOVE 1079               TO MSG1-CD-ERRO-TS

               STRING 'Estado da ordem não permite '
                       DELIMITED BY SIZE
                       TX-DTQ-MSG
                       DELIMITED BY SPACES
                      ' de cadastramento de ordem de venda. Estado atua'
                      'l = '
                       DELIMITED BY SIZE
                       AUX-CD-TIP-EST-NEGO
                       DELIMITED BY SIZE
                       INTO MSG1-TX-ERRO-TS
               END-STRING
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       150000-CONSISTE-SALDO           SECTION.
      *    Consiste se há saldo suficiente para esta aprovação.
      *    Verifica o saldo de ordens já aprovadas que, somadas à ordem
      *    que se está aprovando, não pode ser maior que o saldo da
      *    Drive
      *-----------------------------------------------------------------
           MOVE 0007                   TO  MSG1-CD-SEQL-ERRO

           PERFORM 151000-CONSULTAR-TIT-OPR
           PERFORM 152000-CONSULTAR-PSC-SDO
           PERFORM 153000-CONS-OUTR-ORD-VND
           PERFORM 154000-CONS-ESTA-ORD-VND

           SUBTRACT AUX-QT-PAPL-AUTD  SGTSLA75-QT-TTL-PAPL(1)  FROM
                    SGTSL004-QT-PAPL-TIT-DT-PSC (1) GIVING
                    QT-PAPL-TIT-DSP
           ON SIZE ERROR
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0839              TO  MSG1-CD-ERRO-TS
               MOVE 'Erro ao calcular Total Saldo Disponível.'
                                       TO MSG1-TX-ERRO-TS

               MOVE 'SGTSLA75-QT-TTL-PAPL'     TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTSLA75-QT-TTL-PAPL(1)   TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE 'SGTSL004-QT-PAPL-TIT-DT-PSC'
                                               TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTSL004-QT-PAPL-TIT-DT-PSC(1)
                                               TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE 'AUX-QT-PAPL-AUTD'         TO  MSGKW999-NM-VRV-MNT
               MOVE  AUX-QT-PAPL-AUTD          TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-SUBTRACT

           IF QT-PAPL-TIT-DSP          LESS ZEROES
              SET   ERRO-PROGRAMADO    TO  TRUE
              MOVE  1096               TO  MSG1-CD-ERRO-TS
              MOVE 'Não existe saldo disponível para essa aprovação.'
                                       TO MSG1-TX-ERRO-TS

              MOVE 'SGTSLA75-QT-TTL-PAPL'      TO  MSGKW999-NM-VRV-MNT
              MOVE  SGTSLA75-QT-TTL-PAPL(1)    TO  MSGKW999-VL-VRV-MNT
              PERFORM  999003-ADICIONA-VRV-MNT

              MOVE 'SGTSL004-QT-PAPL-TIT-DT-PSC'
                                               TO  MSGKW999-NM-VRV-MNT
              MOVE  SGTSL004-QT-PAPL-TIT-DT-PSC(1)
                                               TO  MSGKW999-VL-VRV-MNT
              PERFORM  999003-ADICIONA-VRV-MNT

              MOVE 'AUX-QT-PAPL-AUTD'          TO  MSGKW999-NM-VRV-MNT
              MOVE  AUX-QT-PAPL-AUTD           TO  MSGKW999-VL-VRV-MNT
              PERFORM  999003-ADICIONA-VRV-MNT

              PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       150000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       151000-CONSULTAR-TIT-OPR        SECTION.
      *    Busca dados da operação estruturada mercado capitais.
      *-----------------------------------------------------------------
           MOVE 0008                   TO  MSG1-CD-SEQL-ERRO

           MOVE SGTST005               TO NM-PGM
           SET  SGTST005-CONSULTAR     TO TRUE
           MOVE SGTS0292-CD-CTRA-TVM   TO SGTST005-CD-CTRA-TIT-VL
           MOVE SGTS0292-CD-OPR-ETRD-MCP
                                       TO SGTST005-CD-OPR-ETRD-MCP
           MOVE SGTS0292-NR-VRS-OPR-ETRD
                                       TO SGTST005-NR-VRS-OPR-ETRD

           PERFORM 920000-CHAMAR-COMMAREA

           IF  SGTST005-CD-RTN-PGM     NOT EQUAL ZEROS
               SET ERRO-PROGRAMA-ACIONADO TO TRUE
               MOVE NM-PGM             TO MSG1-NM-PGM-ACND
               MOVE SGTST005-MSG-ERRO-PGM
                                       TO MSG1-TX-RTN-PGM-ACND
                                          MSG1-TX-ERRO-TS
               MOVE SGTST005-CD-RTN-PGM
                                       TO MSG1-CD-RTN-PGM-ACND
                                          MSG1-CD-ERRO-TS

      *        Monitorar variáveis do módulo chamado
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-ERRO
           END-IF
           .
       151000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       152000-CONSULTAR-PSC-SDO        SECTION.
      *    Consulta dados da posição diária de saldo.
      *-----------------------------------------------------------------
           MOVE 0009                   TO  MSG1-CD-SEQL-ERRO

           MOVE SGTSL004               TO  NM-PGM
           SET SGTSL004-LISR-ULT-PSC-DRIA
                                       TO TRUE
           MOVE SGTS0292-CD-CTRA-TVM
                                       TO SGTSL004-CD-CTRA-TIT-VL-E
           MOVE SGTST005-CD-TIT-OPR-ETRD
                                       TO SGTSL004-CD-TIT-OPR-ETRD-E
           MOVE SGTST005-NR-DVS-TIT    TO SGTSL004-NR-DVS-TIT-E

           PERFORM 900000-EXECUTAR-MODULO
           .
       152000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       153000-CONS-OUTR-ORD-VND        SECTION.
      *    Consulta se existe alguma ordem de venda no estado autorizada
      *    Caso exista, a quantidade de papéis dessa ordem deve ser sub-
      *    traído do total disponível, pois esse valor já está comprome-
      *    tido nesta outra venda.
      *-----------------------------------------------------------------
           MOVE 0010                   TO  MSG1-CD-SEQL-ERRO

           EXEC SQL
               OPEN CURSOR-ORD-NEGO
           END-EXEC

           IF SQLCODE NOT EQUAL ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  1071              TO  MSG1-CD-ERRO-TS
               MOVE 'Erro ao iniciar leitura das Ordens de Venda.'
                                       TO MSG1-TX-ERRO-TS

               MOVE SQLCODE            TO AX-SQLCODE
               MOVE AX-SQLCODE         TO AX-SQLCODE-EDIT
               MOVE 'AX-SQLCODE-EDIT'  TO MSGKW999-NM-VRV-MNT
               MOVE  AX-SQLCODE-EDIT   TO MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           MOVE 'N'                    TO IND-FIM-SLCT

           PERFORM 153100-FETCH-CURSOR-ORD
                   VARYING  IDX-ORD FROM 1 BY 1
                   UNTIL IND-FIM-SLCT   = 'S'

           EXEC SQL
              CLOSE CURSOR-ORD-NEGO
           END-EXEC

           IF SQLCODE NOT EQUAL ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  1072              TO  MSG1-CD-ERRO-TS
               MOVE 'Erro ao finalizar leitura das Ordens de Venda.'
                                       TO MSG1-TX-ERRO-TS

               MOVE SQLCODE            TO AX-SQLCODE
               MOVE AX-SQLCODE         TO AX-SQLCODE-EDIT
               MOVE 'AX-SQLCODE-EDIT'  TO MSGKW999-NM-VRV-MNT
               MOVE  AX-SQLCODE-EDIT   TO MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       153000-SAI.
           EXIT.
      *---------------------------------------------------------------*
       153100-FETCH-CURSOR-ORD         SECTION.
      *     Lê os registros de ORD_NEGO_OPR
      *---------------------------------------------------------------*
           MOVE 0011                   TO  MSG1-CD-SEQL-ERRO

           EXEC SQL
              FETCH CURSOR-ORD-NEGO
                 INTO :CD-CTRA-TIT-VL-ORD
                 ,    :CD-OPR-ETRD-MCP-ORD
                 ,    :NR-VRS-OPR-ETRD-ORD
                 ,    :NR-SEQL-ORD-NEGO-ORD
                 ,    :CD-MOD-ORD-NEGO-ORD
                 ,    :VL-PU-PAPL-VND-ORD
                      :VL-PU-PAPL-VND-ORD-N
                 ,    :QT-TTL-PAPL
                      :QT-TTL-PAPL-N
                 ,    :CD-TIP-EST-NEGO-ORD
           END-EXEC

           EVALUATE SQLCODE
             WHEN ZEROS
                ADD QT-TTL-PAPL(IDX-ORD)   TO AUX-QT-PAPL-AUTD
                ON SIZE ERROR
                  SET   ERRO-PROGRAMADO    TO  TRUE
                  MOVE  0839               TO  MSG1-CD-ERRO-TS
                  MOVE 'Erro ao calcular Total Saldo Autorizado.'
                                           TO MSG1-TX-ERRO-TS

                  MOVE 'IDX-ORD'           TO  MSGKW999-NM-VRV-MNT
                  MOVE  IDX-ORD            TO  MSGKW999-VL-VRV-MNT
                  PERFORM  999003-ADICIONA-VRV-MNT

                  MOVE 'QT-TTL-PAPL'       TO  MSGKW999-NM-VRV-MNT
                  MOVE  QT-TTL-PAPL(IDX-ORD)
                                           TO  MSGKW999-VL-VRV-MNT
                  PERFORM  999003-ADICIONA-VRV-MNT

                  MOVE 'AUX-QT-PAPL-AUTD'  TO  MSGKW999-NM-VRV-MNT
                  MOVE  AUX-QT-PAPL-AUTD   TO  MSGKW999-VL-VRV-MNT
                  PERFORM  999003-ADICIONA-VRV-MNT

                  PERFORM  999000-GRAVA-ERRO-E-ENCERRA
                END-ADD

             WHEN 100
                MOVE 'S'               TO IND-FIM-SLCT

             WHEN OTHER
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  1073              TO  MSG1-CD-ERRO-TS
               MOVE 'Erro ao recuperar Ordens de Venda.'
                                       TO MSG1-TX-ERRO-TS

               MOVE SQLCODE            TO AX-SQLCODE
               MOVE AX-SQLCODE         TO AX-SQLCODE-EDIT
               MOVE 'AX-SQLCODE-EDIT'  TO MSGKW999-NM-VRV-MNT
               MOVE  AX-SQLCODE-EDIT   TO MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE
           .
       153100-SAIDA.
           EXIT.
      *-----------------------------------------------------------------
       154000-CONS-ESTA-ORD-VND        SECTION.
      *    Consiste se o modelo que está associado à  oper informada
      *    existe e está ativo.
      *-----------------------------------------------------------------
           MOVE 0012                   TO  MSG1-CD-SEQL-ERRO

           MOVE SGTSLA75                   TO NM-PGM
           SET  SGTSLA75-LISTA-ORD-SEQL    TO TRUE
           MOVE SGTS0292-CD-CTRA-TVM       TO SGTSLA75-E-CD-CTRA-TIT-VL
           MOVE SGTS0292-CD-OPR-ETRD-MCP   TO SGTSLA75-E-CD-OPR-ETRD-MCP
           MOVE SGTS0292-NR-VRS-OPR-ETRD   TO SGTSLA75-E-NR-VRS-OPR-ETRD
           MOVE SGTS0292-NR-SEQL-ORD-NEGO   TO
                                           SGTSLA75-E-NR-SEQL-ORD-NEGO

           PERFORM 900000-EXECUTAR-MODULO

           IF SGTSLA75-QT-OCR-TAB      EQUAL  ZEROES
              SET   ERRO-PROGRAMADO    TO  TRUE
              MOVE  0932               TO  MSG1-CD-ERRO-TS
              MOVE  'Favor vincular um modelo de negociação a esta opera
      -              'ção estruturada.'
                                       TO  MSG1-TX-ERRO-TS
              PERFORM  969000-MONITORAR-VARIAVEIS
              PERFORM  999000-ERRO
           END-IF
           .
       154000-SAI.
           EXIT.
      *-----------------------------------------------------------------
      *-*
      *-----------------------------------------------------------------
       200000-AUTORIZA-ORDEM-VENDA     SECTION.
      *  Inclusão de estado "Autorizada" para a Ordem de Negociação Ven-
      *  da informada (Estado 4).
      *-----------------------------------------------------------------
           MOVE 0013                   TO  MSG1-CD-SEQL-ERRO

      *    SGTSI078-CD-TIP-EST-NEGO = 4 (Autorizada)
           MOVE 4                      TO  SGTSI078-CD-TIP-EST-NEGO

           PERFORM 600000-GRAVA-EST-ORD
           MOVE SGTSI078-TS-EST-ORD-NEGO-S
                                       TO TS-ATU-APVC
           .
       200000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       300000-VERIFICA-RETROATIVO      SECTION.
      *  Verifica se a data da venda é retroativa. Caso positivo, grava
      *  registro de estado da venda Concluída (Estado 5).
      *-----------------------------------------------------------------
           MOVE 0014                   TO  MSG1-CD-SEQL-ERRO

           MOVE CORR DT-SQL-ATU-R      TO AUX-DT-HJ-INV-R

           MOVE CORR SGTS0292-DT-VND-R TO AUX-DT-VND-INV-R

           IF AUX-DT-HJ-INV  > AUX-DT-VND-INV
      *       SGTSI078-CD-TIP-EST-NEGO = 5 (Concluida)
              MOVE 5                   TO  SGTSI078-CD-TIP-EST-NEGO
              PERFORM 600000-GRAVA-EST-ORD
           END-IF
           .
       300000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       310000-DEL-REG-CRIADO           SECTION.
      *  Caso tenha dado erro na gravação do registro de conclusão da
      *  ordem de venda, deleta a autorização gravada e dá erro.
      *-----------------------------------------------------------------
           MOVE 0015                   TO  MSG1-CD-SEQL-ERRO

           MOVE SGTSE078               TO  NM-PGM

           MOVE SGTS0292-CD-CTRA-TVM
                                       TO  SGTSE078-CD-CTRA-TIT-VL
           MOVE SGTS0292-CD-OPR-ETRD-MCP
                                       TO   SGTSE078-CD-OPR-ETRD-MCP
           MOVE SGTS0292-NR-VRS-OPR-ETRD
                                       TO  SGTSE078-NR-VRS-OPR-ETRD
           MOVE SGTS0292-NR-SEQL-ORD-NEGO
                                       TO  SGTSE078-NR-SEQL-ORD-NEGO
           MOVE TS-ATU-APVC            TO  SGTSE078-TS-EST-ORD-NEGO

           PERFORM 900000-EXECUTAR-MODULO
           .
       300000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       600000-GRAVA-EST-ORD            SECTION.
      *  Inclusão de estado "Autorizada" para a Ordem de Negociação Ven-
      *  da informada (Estado 4).
      *-----------------------------------------------------------------
           MOVE 0016                   TO  MSG1-CD-SEQL-ERRO

           MOVE SGTSI078               TO  NM-PGM

           MOVE SGTS0292-CD-CTRA-TVM
                                       TO  SGTSI078-CD-CTRA-TIT-VL
           MOVE SGTS0292-CD-OPR-ETRD-MCP
                                       TO  SGTSI078-CD-OPR-ETRD-MCP
           MOVE SGTS0292-NR-VRS-OPR-ETRD
                                       TO  SGTSI078-NR-VRS-OPR-ETRD
           MOVE SGTS0292-NR-SEQL-ORD-NEGO
                                       TO  SGTSI078-NR-SEQL-ORD-NEGO
           MOVE 'CURRENT'              TO  SGTSI078-TS-EST-ORD-NEGO
           MOVE KOPRD-CD-OPRD          TO  SGTSI078-CD-USU-ULT-ATL

           PERFORM 900000-EXECUTAR-MODULO
           .
       600000-SAI.
           EXIT.
      *-----------------------------------------------------------------
      *   Section da Faixa de 900000 a 969900 para Demais Rotinas
      *-----------------------------------------------------------------
      * Executa módulos implementados com CONTAINER                   *
      *-----------------------------------------------------------------
       900000-EXECUTAR-MODULO          SECTION.
      *-----------------------------------------------------------------
           MOVE 0017                   TO  MSG1-CD-SEQL-ERRO

      *    Grava o container de requisição do módulo a executar
           PERFORM 982000-GRAVAR-CONTAINER-RQSC

      *    Limpa a resposta e erro antes de executar o módulo
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO

      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EVALUATE NM-PGM

               WHEN SGTSE078
                    EXEC CICS LINK
                              PROGRAM ( SGTSE078   )
                              CHANNEL ( NM-CNL     )
                              RESP    ( MSG1-RESP1 )
                              RESP2   ( MSG1-RESP2 )
                    END-EXEC

               WHEN SGTSI078
                    EXEC CICS LINK
                              PROGRAM ( SGTSI078   )
                              CHANNEL ( NM-CNL     )
                              RESP    ( MSG1-RESP1 )
                              RESP2   ( MSG1-RESP2 )
                    END-EXEC

               WHEN SGTSL004
                    EXEC CICS LINK
                              PROGRAM ( SGTSL004   )
                              CHANNEL ( NM-CNL     )
                              RESP    ( MSG1-RESP1 )
                              RESP2   ( MSG1-RESP2 )
                    END-EXEC

               WHEN SGTSLA75
                    EXEC CICS LINK
                              PROGRAM ( SGTSLA75   )
                              CHANNEL ( NM-CNL     )
                              RESP    ( MSG1-RESP1 )
                              RESP2   ( MSG1-RESP2 )
                    END-EXEC

               WHEN SGTSL069
                    EXEC CICS LINK
                              PROGRAM ( SGTSL069   )
                              CHANNEL ( NM-CNL     )
                              RESP    ( MSG1-RESP1 )
                              RESP2   ( MSG1-RESP2 )
                    END-EXEC

               WHEN OTHER
                    MOVE 9006          TO MSG1-CD-ERRO-TS
                    MOVE 'Não definida a execução do módulo.'
                                       TO MSG1-TX-ERRO-TS
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE
      *
           EVALUATE TRUE
               WHEN MSG1-RESP1        EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    MOVE 3004          TO MSG1-CD-ERRO-TS
                    MOVE 'Erro ao acionar o módulo.'
                                       TO MSG1-TX-ERRO-TS
                    SET ERRO-CICS      TO TRUE
                    PERFORM 969000-MONITORAR-VARIAVEIS
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *    Efetua a leitura do Container de Erro
           PERFORM 972000-LER-CONTAINER-DE-ERRO

      *    Efetua a leitura do Container de Resposta
           PERFORM 973000-LER-RESPOSTA
           .
       900000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       920000-CHAMAR-COMMAREA          SECTION.
      *-----------------------------------------------------------------
           MOVE 0018                   TO  MSG1-CD-SEQL-ERRO
      *
      *--> Movimento nome do módulo para monitoração de variáveis

           EVALUATE  NM-PGM

           WHEN SGTST005
                EXEC CICS LINK
                     PROGRAM  (SGTST005)
                     COMMAREA (SGTST005-DADOS)
                     FLENGTH  (LENGTH OF SGTST005-DADOS)
                END-EXEC

           WHEN OTHER
                CONTINUE
           END-EVALUATE
           .
       920000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       969000-MONITORAR-VARIAVEIS      SECTION.
      *--> Monitora as variáveis dos módulos chamados
      *-----------------------------------------------------------------

           EVALUATE  NM-PGM

               WHEN  SGTSL004
                     MOVE 'SGTSL004-E-CD-TIP-SRVC'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE SGTSL004-E-CD-TIP-SRVC TO  MSGKW999-VL-VRV-MNT
                     PERFORM 999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSL004-CD-CTRA-TIT-VL-E'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE SGTSL004-CD-CTRA-TIT-VL-E
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM 999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSL004-CD-TIT-OPR-ETRD-E'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE SGTSL004-CD-TIT-OPR-ETRD-E
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM 999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSL004-DT-PSC-E'    TO  MSGKW999-NM-VRV-MNT
                     MOVE SGTSL004-DT-PSC-E      TO  MSGKW999-VL-VRV-MNT
                     PERFORM 999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSL004-NR-DVS-TIT-E'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE SGTSL004-NR-DVS-TIT-E  TO  MSGKW999-VL-VRV-MNT
                     PERFORM 999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSL004-ULT-DT-PSC' TO  MSGKW999-NM-VRV-MNT
                     MOVE SGTSL004-ULT-DT-PSC   TO  MSGKW999-VL-VRV-MNT
                     PERFORM 999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSL004-ULT-NR-SEQL-PSC-DRIA'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE SGTSL004-ULT-NR-SEQL-PSC-DRIA
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM 999003-ADICIONA-VRV-MNT

               WHEN  SGTSL069
                     MOVE 'SGTSL069-CD-TIP-SRVC' TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL069-CD-TIP-SRVC  TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSL069-CD-CTRA-TIT-VL'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL069-CD-CTRA-TIT-VL
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSL069-CD-OPR-ETRD-MCP'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL069-CD-OPR-ETRD-MCP
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSL069-NR-VRS-OPR-ETRD'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL069-NR-VRS-OPR-ETRD
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSL069-TS-EST-VRS-PAG'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL069-TS-EST-VRS-PAG
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  SGTSE078
                     MOVE 'SGTSE078-CD-CTRA-TIT-VL'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSE078-CD-CTRA-TIT-VL
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSE078-CD-OPR-ETRD-MCP'
                                                 TO  MSGKW999-NM-VRV-MNT
                     IF  SGTSE078-CD-OPR-ETRD-MCP    NUMERIC
                         MOVE  SGTSE078-CD-OPR-ETRD-MCP
                                                 TO CD-OPR-ETRD-MCP-DISP
                         MOVE  CD-OPR-ETRD-MCP-TX
                                                 TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE 'Valor não numérico'
                                                 TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSE078-NR-VRS-OPR-ETRD'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSE078-NR-VRS-OPR-ETRD
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSE078-NR-SEQL-ORD-NEGO '
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSE078-NR-SEQL-ORD-NEGO
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSE078-TS-EST-ORD-NEGO'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSE078-TS-EST-ORD-NEGO
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  SGTSI078
                     MOVE 'SGTSI078-CD-CTRA-TIT-VL'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI078-CD-CTRA-TIT-VL
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSI078-CD-OPR-ETRD-MCP'
                                                 TO  MSGKW999-NM-VRV-MNT
                     IF  SGTSI078-CD-OPR-ETRD-MCP    NUMERIC
                         MOVE  SGTSI078-CD-OPR-ETRD-MCP
                                                 TO CD-OPR-ETRD-MCP-DISP
                         MOVE  CD-OPR-ETRD-MCP-TX
                                                 TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE 'Valor não numérico'
                                                 TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSI078-NR-VRS-OPR-ETRD'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI078-NR-VRS-OPR-ETRD
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSI078-NR-SEQL-ORD-NEGO'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI078-NR-SEQL-ORD-NEGO
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSI078-TS-EST-ORD-NEGO'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI078-TS-EST-ORD-NEGO
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSI078-CD-USU-ULT-ATL'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI078-CD-USU-ULT-ATL
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSI078-CD-TIP-EST-NEGO'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSI078-CD-TIP-EST-NEGO
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  SGTSLA75
                     MOVE 'SGTSLA75-E-CD-TIP-SRVC'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSLA75-E-CD-TIP-SRVC
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSLA75-E-CD-CTRA-TIT-VL'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSLA75-E-CD-CTRA-TIT-VL
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSLA75-E-CD-OPR-ETRD-MCP'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSLA75-E-CD-OPR-ETRD-MCP
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTSLA75-E-NR-VRS-OPR-ETRD'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSLA75-E-NR-VRS-OPR-ETRD
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  SGTST005
                     MOVE 'SGTST005-ACAO'        TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST005-ACAO         TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTST005-CD-CTRA-TIT-VL'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST005-CD-CTRA-TIT-VL
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTST005-CD-OPR-ETRD-MCP'
                                                 TO  MSGKW999-NM-VRV-MNT
                     IF  SGTST005-CD-OPR-ETRD-MCP   NUMERIC
                         MOVE  SGTST005-CD-OPR-ETRD-MCP
                                                 TO CD-OPR-ETRD-MCP-DISP
                         MOVE  CD-OPR-ETRD-MCP-TX
                                                 TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE 'Valor não numérico'
                                                 TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE 'SGTST005-NR-VRS-OPR-ETRD'
                                                 TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST005-NR-VRS-OPR-ETRD
                                                 TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  OTHER
                     CONTINUE
           END-EVALUATE
           .
       969000-SAI.
           EXIT.
      *-----------------------------------------------------------------
      *    Section da Faixa de 970000 a 979900 para Leitura de Dados
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       970000-LER-CANAL                SECTION.
      *--> Obtem o canal corrente
      *-----------------------------------------------------------------
           MOVE 0019                   TO  MSG1-CD-SEQL-ERRO

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CANAL            TO  TRUE

           EXEC CICS ASSIGN
                     CHANNEL ( NM-CNL )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       970000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       971000-LER-REQUISICAO           SECTION.
      *-----------------------------------------------------------------
           MOVE 0020                   TO  MSG1-CD-SEQL-ERRO

      *--> Monta o nome do container de requisicao
           MOVE  'SGTS0292'            TO  NM-PGM
           MOVE  'SGTS0292-RQSC'       TO  NM-CTNR

      *--> Recupera o tamanho do container de requisição
           PERFORM  973100-LER-CONTAINER-NODATA


      *--> Obtem o tamanho esperado da requisicao
           MOVE  LENGTH OF SGTS0292-RQSC
                                       TO  QT-CCT-EPRO

      *--> Valida tamanho esperado da requisicao X tamanho recebido

           IF  QT-CCT-LIDO                 NOT EQUAL QT-CCT-EPRO
               SET   ERRO-TAMANHO-AREA
                                       TO  TRUE
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *--> Configura dados para leitura do container de requisição
           SET   GET-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *--> Efetua a leitura do container
           EXEC CICS GET
                     CONTAINER (NM-CTNR)
                     FLENGTH   (QT-CCT-LIDO)
                     INTO      (SGTS0292-RQSC)
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       971000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       972000-LER-CONTAINER-DE-ERRO    SECTION.
      *--> A presença do container PRM-ERRO indica que houve erro de
      *    integração, a ausência do container indica sucesso.
      *-----------------------------------------------------------------
           MOVE 0021                   TO  MSG1-CD-SEQL-ERRO

      *--> Le o container de erro dos modulos chamados
           SET   GET-CONTAINER         TO  TRUE
           MOVE  'PRM-ERRO'            TO  NM-CTNR
           INITIALIZE                  IIBKERRO-PRM-ERRO

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS  GET
                      CONTAINER ( NM-CTNR                     )
                      INTO      ( IIBKERRO-PRM-ERRO           )
                      FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                      RESP      ( MSG1-RESP1                  )
                      RESP2     ( MSG1-RESP2                  )
           END-EXEC

           EVALUATE  TRUE
      *        Caso deseje personalizar ou não interromper o
      *        processamento, alterar para não dar erro aqui SOMENTE
      *        PARA O MÓDULO CHAMADO e tratar na section chamadora
               WHEN  MSG1-RESP1            EQUAL DFHRESP(CONTAINERERR)
                     CONTINUE
               WHEN  MSG1-RESP1            EQUAL DFHRESP(NORMAL)
                     IF  NM-PGM  =  'SGTSI078'  AND
                         SGTSI078-CD-TIP-EST-NEGO =  5
                         PERFORM 310000-DEL-REG-CRIADO
                     END-IF
                     SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
                     MOVE  NM-PGM      TO  MSG1-NM-PGM-ACND
                     IF  NM-PGM(1:3)       EQUAL 'SGT'
                         MOVE  IIBKERRO-CD-ERRO
                                       TO  MSG1-CD-RTN-PGM-ACND
                                           MSG1-CD-ERRO-TS
                         MOVE  IIBKERRO-TX-ERRO
                                       TO  MSG1-TX-RTN-PGM-ACND
                                           MSG1-TX-ERRO-TS
                     ELSE
                         MOVE  3004    TO  MSG1-CD-ERRO-TS
                         STRING  'Erro ao acionar o módulo ' NM-PGM '.'
                                 DELIMITED BY SIZE
                                 INTO      MSG1-TX-ERRO-TS
                         MOVE  IIBKERRO-TX-ERRO
                                       TO  MSG1-TX-RTN-PGM-ACND
                         MOVE  IIBKERRO-CD-ERRO
                                       TO  MSG1-CD-RTN-PGM-ACND
                     END-IF
      *              Monitorar variáveis do módulo chamado
                     PERFORM  969000-MONITORAR-VARIAVEIS
                     PERFORM  999000-ERRO
               WHEN  OTHER
                     SET   ERRO-CONTAINER
                                       TO  TRUE
                     PERFORM  999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       972000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       973000-LER-RESPOSTA             SECTION.
      *-----------------------------------------------------------------
      *    Recupera o container com os dados de resposta (saída).
      *-----------------------------------------------------------------
           MOVE 0022                   TO  MSG1-CD-SEQL-ERRO

      *    Monta o nome dos container de acordo com modulo a executar
           MOVE SPACES                 TO NM-CTNR

      *    Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *    nome_modulo -RPST
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

      *    Calcula a quantidade de caracteres esperado e limpa a área de
      *    dados antes de executar o GET (somentepara matrizes com dados
      *    não variáveis)

               WHEN SGTSE078
                    EXEC CICS GET
                              CONTAINER ( NM-CTNR       )
                              INTO      ( SGTSE078-RPST )
                              FLENGTH   ( QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSI078
                    EXEC CICS GET
                              CONTAINER ( NM-CTNR       )
                              INTO      ( SGTSI078-RPST )
                              FLENGTH   ( QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSL004
                    EXEC CICS GET
                              CONTAINER ( NM-CTNR       )
                              INTO      ( SGTSL004-RPST )
                              FLENGTH   ( QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSL069
                    EXEC CICS GET
                              CONTAINER ( NM-CTNR       )
                              INTO      ( SGTSL069-RPST )
                              FLENGTH   ( QT-CCT-LIDO   )
                    END-EXEC

               WHEN SGTSLA75
                    EXEC CICS GET
                              CONTAINER ( NM-CTNR       )
                              INTO      ( SGTSLA75-RPST )
                              FLENGTH   ( QT-CCT-LIDO   )
                    END-EXEC

               WHEN OTHER
                    MOVE 9003          TO MSG1-CD-ERRO-TS
                    MOVE 'Erro na leitura de resposta do módulo.'
                                       TO MSG1-TX-ERRO-TS
                    PERFORM 969000-MONITORAR-VARIAVEIS
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       973000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       973100-LER-CONTAINER-NODATA     SECTION.
      *-----------------------------------------------------------------
           MOVE 0023                   TO  MSG1-CD-SEQL-ERRO

      *--> Configura dados para leitura do container de requisição
           SET   GET-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *--> Obtem a quantidade de caracteres no container
           MOVE  ZEROS                 TO  QT-CCT-LIDO

           EXEC CICS  GET
                      CONTAINER ( NM-CTNR     )
                      NODATA
                      FLENGTH   ( QT-CCT-LIDO )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       973100-SAI.
           EXIT.
      *----------------------------------------------------------------*
       974000-LER-DADOS-OPRD           SECTION.
      *----------------------------------------------------------------*
           MOVE 0024                   TO  MSG1-CD-SEQL-ERRO

      *--> Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE
           MOVE 'PRM-INF-OPRD'         TO NM-CTNR

      *--> Por causa da captura automática de erro CICS, deve ser
      *--> previsto erro de container
           SET  ERRO-CONTAINER         TO TRUE

           INITIALIZE PRM-INF-OPRD
           EXEC CICS GET
                CONTAINER ( NM-CTNR                )
                INTO      ( PRM-INF-OPRD           )
                FLENGTH   ( LENGTH OF PRM-INF-OPRD )
           END-EXEC.

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       974000-SAI.
           EXIT.
      *-----------------------------------------------------------------
      *   Section da Faixa de 980000 a 989900 para Gravação de Dados
      *-----------------------------------------------------------------
       982000-GRAVAR-CONTAINER-RQSC    SECTION.
      *-----------------------------------------------------------------
      *    Postar container com dados de requisição (entrada)
      *-----------------------------------------------------------------
           MOVE 0025                   TO  MSG1-CD-SEQL-ERRO

      *    Monta o nome dos container de acordo com modulo a executar
           MOVE SPACES                 TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE

      *    Quando módulos via IIB, fixo nome PRM-DADOS-RQSC, senão
      *    nome_modulo -RQSC

           MOVE NM-PGM                 TO NM-CTNR(1:8)
           MOVE '-RQSC'                TO NM-CTNR(9:5)

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *    Faz put da requisição do programa a executar
           EVALUATE NM-PGM
               WHEN SGTSE078
                    EXEC CICS PUT
                              CONTAINER ( NM-CTNR                 )
                              FROM      ( SGTSE078-RQSC           )
                              FLENGTH   ( LENGTH OF SGTSE078-RQSC )
                    END-EXEC

               WHEN SGTSI078
                    EXEC CICS PUT
                              CONTAINER ( NM-CTNR                 )
                              FROM      ( SGTSI078-RQSC           )
                              FLENGTH   ( LENGTH OF SGTSI078-RQSC )
                    END-EXEC

               WHEN SGTSL004
                    EXEC CICS PUT
                              CONTAINER ( NM-CTNR                 )
                              FROM      ( SGTSL004-RQSC           )
                              FLENGTH   ( LENGTH OF SGTSL004-RQSC )
                    END-EXEC

               WHEN SGTSL069
                    EXEC CICS PUT
                              CONTAINER ( NM-CTNR                 )
                              FROM      ( SGTSL069-RQSC           )
                              FLENGTH   ( LENGTH OF SGTSL069-RQSC )
                    END-EXEC

               WHEN SGTSLA75
                    EXEC CICS PUT
                              CONTAINER ( NM-CTNR                 )
                              FROM      ( SGTSLA75-RQSC           )
                              FLENGTH   ( LENGTH OF SGTSLA75-RQSC )
                    END-EXEC

               WHEN OTHER
                    MOVE 9004          TO MSG1-CD-ERRO-TS
                    MOVE 'Erro na gravação da requisição do módulo.'
                                       TO MSG1-TX-ERRO-TS
                    PERFORM 969000-MONITORAR-VARIAVEIS
                    PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-EVALUATE

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       982000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       983000-EXCLUIR-RESPOSTA-ERRO    SECTION.
           MOVE 0026                   TO  MSG1-CD-SEQL-ERRO
      *--> Exclui o container de resposta e de erro para evitar
      *    problemas em módulos executados mais de uma vez dentro
      *    da mesma sessão do CICS.
      *    Equivale ao initialize na área de resposta dos
      *    programas que utilizam COMMAREA.
      *-----------------------------------------------------------------

      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *    nome_modulo -RPST
           MOVE NM-PGM                 TO NM-CTNR(1:8)
           MOVE '-RPST'                TO NM-CTNR(9:5)

           SET   DEL-CONTAINER         TO  TRUE

      *--> Exclui o container de resposta
      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS  DELETE
                      CONTAINER ( NM-CTNR    )
                      RESP      ( MSG1-RESP1 )
                      RESP2     ( MSG1-RESP2 )
           END-EXEC

           EVALUATE  TRUE
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(CONTAINERERR)
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(NORMAL)
                     CONTINUE
               WHEN  OTHER
                     SET   ERRO-CONTAINER
                                       TO  TRUE
                     PERFORM  999002-TRATA-CONDITION-CICS
           END-EVALUATE

      *--> Excluir o container de erro
           PERFORM  983100-EXCLUIR-CONTAINER-ERRO
           .
       983000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       983100-EXCLUIR-CONTAINER-ERRO   SECTION.
      *-----------------------------------------------------------------
           MOVE 0027                   TO  MSG1-CD-SEQL-ERRO

      *--> Limpar variaveis container de erro
           INITIALIZE                  IIBKERRO-PRM-ERRO

      *--> Exclui o container de erro
           MOVE  'PRM-ERRO'            TO  NM-CTNR
           SET   DEL-CONTAINER         TO  TRUE

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS  DELETE
                      CONTAINER ( NM-CTNR    )
                      RESP      ( MSG1-RESP1 )
                      RESP2     ( MSG1-RESP2 )
           END-EXEC

           EVALUATE  TRUE
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(CONTAINERERR)
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(NORMAL)
                     CONTINUE
               WHEN  OTHER
                     SET   ERRO-CONTAINER
                                       TO  TRUE
                     PERFORM  999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       983100-SAI.
           EXIT.
      *-----------------------------------------------------------------
       984000-GRAVAR-CONTAINER-ERRO    SECTION.
      *--> Grava o erro gerado no container de erro
      *-----------------------------------------------------------------
           MOVE 0028                   TO  MSG1-CD-SEQL-ERRO

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS  PUT
                      CONTAINER ( 'PRM-ERRO'                  )
                      FROM      ( IIBKERRO-PRM-ERRO           )
                      FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                      RESP      ( MSG1-RESP1                  )
                      RESP2     ( MSG1-RESP2                  )
           END-EXEC
           .
       984000-SAI.
           EXIT.
      *-----------------------------------------------------------------
      *    As sections abaixo iniciadas com 999000 representam os
      *    tratamentos de erro através do Sistema MSG (rotina MSGS0001)
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       999000-ERRO                     SECTION.
      *-----------------------------------------------------------------

           IF  MSG1-CD-ERRO-TS             EQUAL ZEROS
              MOVE  8888               TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS             EQUAL SPACES
               MOVE  'Erro não específicado no módulo.'
                                       TO  MSG1-TX-ERRO-TS
           END-IF

           PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           .
       999000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------

      *--> Em caso de erro, desfaz as atualizações em banco de dados
           EXEC CICS SYNCPOINT
                     ROLLBACK
           END-EXEC

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

      *--> Adiciona as variáveis de entrada para monitoração
           MOVE 'SGTS0292-CD-CTRA-TVM'         TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0292-CD-CTRA-TVM          TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0292-CD-OPR-ETRD-MCP'     TO  MSGKW999-NM-VRV-MNT
      *------> Verifica se o conteudo e numerico antes de adicionar
           IF  SGTS0292-CD-OPR-ETRD-MCP    NUMERIC
               MOVE  SGTS0292-CD-OPR-ETRD-MCP  TO  CD-OPR-ETRD-MCP-DISP
               MOVE  CD-OPR-ETRD-MCP-TX        TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico'      TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0292-NR-VRS-OPR-ETRD'     TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0292-NR-VRS-OPR-ETRD      TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0292-NR-SEQL-ORD-NEGO'    TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0292-NR-SEQL-ORD-NEGO     TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'TX-DTQ-MSG'                   TO  MSGKW999-NM-VRV-MNT
           MOVE  TX-DTQ-MSG                    TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'AUX-CD-TIP-EST-NEGO'          TO  MSGKW999-NM-VRV-MNT
           MOVE  AUX-CD-TIP-EST-NEGO           TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

      *--> Grava o erro gerado no sistema MSG - Mensageria
           PERFORM  MSG1-NTF-MSG

           MOVE  MSG1-CD-ERRO-TS               TO  IIBKERRO-CD-ERRO
           MOVE  MSG1-TX-ERRO-TS               TO  IIBKERRO-TX-ERRO

      *--> Grava o erro gerado no container de erro
           PERFORM  984000-GRAVAR-CONTAINER-ERRO

      *--> Encerra a transacao
           PERFORM  999999-ENCERRAR
           .
       999000-SAI.
           EXIT.
      *---------------------------------------
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
      *---------------------- FIM SGTS0292 -----------------------------