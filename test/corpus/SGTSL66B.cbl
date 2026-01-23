       CBL ARITH(EXTEND)
      *-----------------------------------------------------------------
      * PROGRAMA..: SGTSL66B
      * ANALISTA..: F8090555 - Paulo Eduardo Roque Castanhera
      * AUTOR.....: F0950413 - António Francisco Mendonça Silva
      * OBJETIVO..: Lista para consulta da posição diária por título de
      *             uma carteira (tabela DB2: IMP_PSC_DRIA_SDO)
      * COMPILACAO: 52 - PSOSE600 - Cobol 4.2 c/otimizacao p/producao
      *-----------------------------------------------------------------
      * VRS002 06.12.2019 - F0950413 - Inclusão da condição CD_EST_IMP
      *             = 0 no CURSOR-PSQ-1.
      * VRS002 02.12.2019 - F2804421 - 2016/145195 - Recuperar valor
      *             de mercado a partir do campo VL-SDO-TIT-DT-PSC.
      * VRS001 07.02.2019 - F0950413 - 2016/145195 - Implantação
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTSL66B.

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
       77  CTE-INICIO-WS               PIC X(30) VALUE
                                       '*** WSS COMECA AQUI ***'.
       77  CTE-PROG                    PIC X(18) VALUE
                                       '*** SGTSL66B ***'.
       77  CTE-VERS                    PIC X(06) VALUE 'VRS002'.

      *--> Subrotinas utilizadas neste programa
       77  SBDATAF                     PIC  X(08) VALUE 'SBDATAF'.

      *-----------------------------------------------------------------
      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informacoes
-INC HLPKE01B

       01  CTE-FIM-WS                  PIC  X(30)  VALUE
                                       '*** WSS TERMINA AQUI ***'.
      *-----------------------------------------------------------------
       LOCAL-STORAGE SECTION.
      *-----------------------------------------------------------------
       01  CTE-INICIO-LS               PIC X(30)  VALUE
                                       '*** LS COMECA AQUI   ***'.
      *--> Areas de trabalho
       01  IC-TAB                      PIC S9(04)     COMP-5 VALUE 0.
       01  QT-REG-SELD                 PIC S9(04)     COMP-5 VALUE 0.

       01  AUX-DT-IMP-PSC-SDO          PIC  X(10)   VALUE '01.01.0001'.

       01  AUX-DT-INC                  PIC  9(08)            VALUE 0.
       01  AUX-DT-INC-R      REDEFINES AUX-DT-INC.
           03  ANO                     PIC  9(04).
           03  MES                     PIC  9(02).
           03  DIA                     PIC  9(02).

       01  AUX-DT-FIM                  PIC  9(08)            VALUE 0.
       01  AUX-DT-FIM-R      REDEFINES AUX-DT-FIM.
           03  ANO                     PIC  9(04).
           03  MES                     PIC  9(02).
           03  DIA                     PIC  9(02).

       01  AUX-MENS                    PIC  X(120)           VALUE ' '.
       01  AUX-CD-ERRO                 PIC  9(04)            VALUE 0.

      *--> Area de Parametros da Rotina SBDATAF
       01  WKSBDATA                    PIC  X(4099).
       01  AREA-RED                    REDEFINES WKSBDATA.
           03  AREA-TAM                PIC  9(0008) COMP.
           03  FILLER                  PIC  X(4071).
           03  AREA-FIM                PIC  X(0020).
      *--> Função 01
      *    - Calcular Primeiro Dia util a partir ou após uma Data Base
      *    - Retornar a Quantidade de Dias entre a Data Base e a
      *      Data Calculada
      *    - Validar Datas no Formato Gregoriano
      *    - Validar Datas no Formato Juliano
       01  FUNCAO                      PIC  X(03) VALUE 'F01'.
       01  ARG01                       PIC  9(08).
      *--> F01 - ARG02 = 0004 - Validar Datas no Formato Gregoriano
       01  ARG02                       PIC  9(01) VALUE 4.
       01  ARG03                       PIC S9(05) VALUE 0.

      *--> Book do container de erro
-INC IIBKERRO
      *--> Book para captura de erro via MSG1
-INC MSGKWMG1
      *--> Variaveis para tratamento de Abend e Condition
-INC HLPKW999
      *--> Variaveis para tratamento de CONTAINER
-INC SGTKERR1

      *--> Container de erro
       01  CTNR-PRM-ERRO               PIC X(16) VALUE 'PRM-ERRO'.

      *--> Máscaras edição de variáveis p/ monitoração mensagens de erro
       01  MCR-S9-15                   PIC         -999999999999999.

      *--> Variaveis utilizados para leitura
       01  REG-WS.
           03 DT-PSC                   PIC  X(10)
              OCCURS 101 TIMES                               VALUE ' '.
           03 DT-PSC-N                 PIC S9(04)       COMP-5
              OCCURS 101 TIMES                               VALUE 0.
           03 HR-IMP-PSC-SDO           PIC  X(08)
              OCCURS 101 TIMES                               VALUE ' '.
           03 VL-SDO-CTRA              PIC S9(15)V9(2)  COMP-3
              OCCURS 101 TIMES                               VALUE 0.
           03 VL-SDO-CTRA-N            PIC S9(04)       COMP-5
              OCCURS 101 TIMES                               VALUE 0.
           03 QT-PAPL-TIT-DT-PSC       PIC S9(15)V9(14) COMP-3
              OCCURS 101 TIMES                               VALUE 0.
           03 QT-PAPL-TIT-DT-PSC-N     PIC S9(04)       COMP-5
              OCCURS 101 TIMES                               VALUE 0.
           03 QT-BLQD-PAPL-TIT         PIC S9(15)V9(14) COMP-3
              OCCURS 101 TIMES                               VALUE 0.
           03 QT-BLQD-PAPL-TIT-N       PIC S9(04)       COMP-5
              OCCURS 101 TIMES                               VALUE 0.
           03 QT-DSPN-PAPL-TIT         PIC S9(15)V9(14) COMP-3
              OCCURS 101 TIMES                               VALUE 0.
           03 QT-DSPN-PAPL-TIT-N       PIC S9(04)       COMP-5
              OCCURS 101 TIMES                               VALUE 0.
           03 VL-PU-TIT-DT-PSC         PIC S9(15)V9(14) COMP-3
              OCCURS 101 TIMES                               VALUE 0.
           03 VL-PU-TIT-DT-PSC-N       PIC S9(04)       COMP-5
              OCCURS 101 TIMES                               VALUE 0.
           03 VL-SDO-TIT-DT-PSC        PIC S9(13)V9(02) COMP-3
              OCCURS 101 TIMES                               VALUE 0.
           03 VL-SDO-TIT-DT-PSC-N      PIC S9(04)       COMP-5
              OCCURS 101 TIMES                               VALUE 0.

      *--> Variavel(is) de controle do cursor(es)
       01  CTL-CURSOR-PSQ-1.
           03  CD-FIM-CURSOR-PSQ-1     PIC S9(04)     COMP-5 VALUE 0.
               88  FIM-CURSOR-PSQ-1                          VALUE +100.
           03  IN-CURSOR-PSQ-ABTO-1    PIC  X(01)            VALUE 'N'.
               88  CURSOR-PSQ-ABTO-1   VALUE 'S'.

      *--> Book do programa
       01  SGTSL66B-DADOS.
-INC SGTKL66B

      *--> Area de trabalho SQL/ DB2.
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

      *--> Declaração do cursor para pesquisa
           EXEC SQL
             DECLARE  CURSOR-PSQ-1 CURSOR
                      WITH ROWSET POSITIONING FOR
             SELECT   CHAR(TO_CHAR(DT_PSC,'DDMMYYYY') , 8)
                     ,MAX(HR_IMP_PSC_SDO)
                     ,VL_SDO_CTRA
                     ,QT_PAPL_TIT_DT_PSC
                     ,QT_BLQD_PAPL_TIT
                     ,QT_DSPN_PAPL_TIT
                     ,VL_PU_TIT_DT_PSC
                     ,VL_SDO_TIT_DT_PSC
             FROM     DB2SGT.IMP_PSC_DRIA_SDO
             WHERE    DT_IMP_PSC_SDO  > :AUX-DT-IMP-PSC-SDO
               AND    CD_CTRA_TIT_VL  = :SGTSL66B-CD-CTRA-TIT-VL
               AND    CD_TIT_OPR_ETRD = :SGTSL66B-CD-TIT-OPR-ETRD
               AND    DT_PSC         >= TO_CHAR(TO_DATE(
                     :SGTSL66B-DT-INC-PSC,'DDMMYYYY'), 'DD.MM.YYYY')
               AND    DT_PSC         <= TO_CHAR(TO_DATE(
                     :SGTSL66B-DT-FIM-PSC,'DDMMYYYY'), 'DD.MM.YYYY')
               AND    CD_EST_IMP      = 0
            GROUP BY  DT_PSC
                     ,VL_SDO_CTRA
                     ,QT_PAPL_TIT_DT_PSC
                     ,QT_BLQD_PAPL_TIT
                     ,QT_DSPN_PAPL_TIT
                     ,VL_PU_TIT_DT_PSC
                     ,VL_SDO_TIT_DT_PSC
            ORDER BY DT_PSC
           END-EXEC.

       01  CTE-FIM-LS                  PIC X(30)  VALUE
                                       '*** LS TERMINA AQUI  ***'.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------
           MOVE 0001                   TO MSG1-CD-SEQL-ERRO

      *--> Habilita tratamento automatico de Erros DB2
-INC HLPKE997
      *--> Habilita captura automatica de abend e erros CICS
-INC HLPKE999
           MOVE CTE-PROG(5:8)          TO NM-PGM

           PERFORM 100000-OBTER-NOME-DO-CANAL
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO
           PERFORM 970000-LER-REQUISICAO-RECEBIDA

           PERFORM 140000-VALIDAR-PARAMETROS

           PERFORM 970101-ABRE-CURSOR-PSQ-1
           PERFORM 970102-LER-CURSOR-PSQ-1

           PERFORM 200000-PROCESSA-PESQUISA
                   VARYING IC-TAB FROM 1 BY 1
                     UNTIL IC-TAB > SGTSL66B-QT-OCR-TAB

           PERFORM 970103-FECHA-CURSOR-PSQ-1

           PERFORM 980000-GRAVAR-CTNR-RESPOSTA
           PERFORM 999999-ENCERRAR

           GOBACK
           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-OBTER-NOME-DO-CANAL      SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO

      *--> Por causa da captura automática de erro CICS, deve ser
      *--> previsto o erro de container
           SET  ERRO-CANAL             TO TRUE

           EXEC CICS ASSIGN
              CHANNEL ( NM-CNL  )

              END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       140000-VALIDAR-PARAMETROS       SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                   TO MSG1-CD-SEQL-ERRO

           IF  SGTSL66B-CD-CTRA-TIT-VL  EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0001               TO MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de títulos e valores mobiliários
      -             ' inválido.'       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL66B-CD-TIT-OPR-ETRD EQUAL ' '
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0003               TO MSG1-CD-ERRO-TS
               MOVE 'Código do título da operação estruturada não inform
      -             'ado.'       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSL66B-DT-INC-PSC      EQUAL ' '
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0437               TO MSG1-CD-ERRO-TS
               MOVE 'Data início de pesquisa não informada.'
                                 TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           MOVE  761                                  TO AUX-CD-ERRO
           MOVE 'Data início da pesquisa inválida.'   TO AUX-MENS
           MOVE  SGTSL66B-DT-INC-PSC                  TO ARG01
           PERFORM 900000-VALIDAR-DATA

           IF  SGTSL66B-DT-FIM-PSC      EQUAL ' '
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0438               TO MSG1-CD-ERRO-TS
               MOVE 'Data fim de pesquisa não informada.'
                                 TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           MOVE  762                                  TO AUX-CD-ERRO
           MOVE 'Data fim da pesquisa inválida.'      TO AUX-MENS
           MOVE  SGTSL66B-DT-FIM-PSC                  TO ARG01
           PERFORM 900000-VALIDAR-DATA

           MOVE CORR  SGTSL66B-DT-INC-PSC-R           TO AUX-DT-INC-R
           MOVE CORR  SGTSL66B-DT-FIM-PSC-R           TO AUX-DT-FIM-R
           IF  AUX-DT-INC   >   AUX-DT-FIM
               MOVE 0004               TO MSG1-CD-SEQL-ERRO
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0410               TO MSG1-CD-ERRO-TS
               MOVE 'Data início da pesquisa não pode ser maior que data
      -             ' fim.'            TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-PROCESSA-PESQUISA        SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

           MOVE DT-PSC             (IC-TAB) TO
                                   SGTSL66B-DT-PSC             (IC-TAB)
           MOVE VL-SDO-CTRA        (IC-TAB) TO
                                   SGTSL66B-VL-SDO-CTRA        (IC-TAB)
           MOVE QT-PAPL-TIT-DT-PSC (IC-TAB) TO
                                   SGTSL66B-QT-PAPL-TIT-DT-PSC (IC-TAB)
           MOVE QT-BLQD-PAPL-TIT   (IC-TAB) TO
                                   SGTSL66B-QT-BLQD-PAPL-TIT   (IC-TAB)
           MOVE QT-DSPN-PAPL-TIT   (IC-TAB) TO
                                   SGTSL66B-QT-DSPN-PAPL-TIT   (IC-TAB)
           MOVE VL-PU-TIT-DT-PSC   (IC-TAB) TO
                                   SGTSL66B-VL-PU-TIT-DT-PSC   (IC-TAB)
           MOVE VL-SDO-TIT-DT-PSC  (IC-TAB) TO
                                   SGTSL66B-VL-MC-TIT          (IC-TAB)

      * Substituir cálculo pelo valor do campo vl_sdo_tit_dt_psc.
      *
      *    COMPUTE SGTSL66B-VL-MC-TIT (IC-TAB) ROUNDED =
      *            QT-PAPL-TIT-DT-PSC (IC-TAB) *
      *            VL-PU-TIT-DT-PSC   (IC-TAB)
      *         ON SIZE ERROR
      *            SET ERRO-PROGRAMADO TO TRUE
      *            MOVE  0706          TO MSG1-CD-ERRO-TS
      *            MOVE 'Erro em operação aritmética.'
      *                                TO MSG1-TX-ERRO-TS
      *            PERFORM 999000-GRAVA-ERRO-E-ENCERRA
      *    END-COMPUTE
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       900000-VALIDAR-DATA             SECTION.
      *--> Acionar a Funcao 01 da Rotina SBDATAF e verificar se a data
      *    informada é valida.
      *-----------------------------------------------------------------
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

           MOVE  4095                  TO  AREA-TAM
           MOVE  'SBDATAF -WRKAREA-FIM'
                                       TO  AREA-FIM

           CALL SBDATAF           USING  FUNCAO
                                         WKSBDATA
                                         ARG01
                                         ARG02

      *--> RETURN-CODE do SBDATAF
      *    22 -  Conteudo do Argumento Nao Numerico
      *    24 -  Data Informada Invalida
      *    46 -  Data Fora da Faixa de 01/01/0001 a 31/12/9999

           IF  RETURN-CODE      NOT EQUAL ZEROS
               MOVE  RETURN-CODE            TO MSG1-CD-RTN-PGM-ACND
               MOVE  SBDATAF                TO MSG1-NM-PGM-ACND
                                               NM-PGM
               SET ERRO-PROGRAMA-ACIONADO   TO TRUE
               IF  RETURN-CODE                 EQUAL 22 OR 24 OR 26
                   MOVE  AUX-CD-ERRO        TO  MSG1-CD-ERRO-TS
                   MOVE  AUX-MENS           TO  MSG1-TX-ERRO-TS
                                                MSG1-TX-RTN-PGM-ACND
               ELSE
                   MOVE  0051               TO  MSG1-CD-ERRO-TS
                   MOVE  'Erro na chamada da rotina SBDATAF.'
                                            TO  MSG1-TX-RTN-PGM-ACND
                                                MSG1-TX-ERRO-TS
               END-IF
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       900000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970000-LER-REQUISICAO-RECEBIDA  SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

           MOVE ZEROS                       TO QT-CCT-LIDO
           MOVE LENGTH OF SGTSL66B-RQSC     TO QT-CCT-EPRO

           EXEC CICS
                GET  CONTAINER ( 'SGTSL66B-RQSC' )
                     CHANNEL   ( NM-CNL          )
                     NODATA
                     FLENGTH   ( QT-CCT-LIDO     )
           END-EXEC

           IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
               SET  ERRO-TAMANHO-AREA  TO  TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS
                GET  CONTAINER ( 'SGTSL66B-RQSC'         )
                     CHANNEL   ( NM-CNL                  )
                     FLENGTH   ( LENGTH OF SGTSL66B-RQSC )
                     INTO      ( SGTSL66B-RQSC           )
           END-EXEC
           .
       970000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970101-ABRE-CURSOR-PSQ-1          SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

           EXEC SQL
                OPEN CURSOR-PSQ-1
           END-EXEC
           MOVE 'S'                    TO IN-CURSOR-PSQ-ABTO-1
           .
       970101-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970102-LER-CURSOR-PSQ-1         SECTION.
      *-----------------------------------------------------------------
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE REG-WS

           EXEC SQL
                FETCH NEXT ROWSET
                      CURSOR-PSQ-1 FOR 0101 ROWS
                INTO :DT-PSC
                     :DT-PSC-N
                   , :HR-IMP-PSC-SDO
                   , :VL-SDO-CTRA
                     :VL-SDO-CTRA-N
                   , :QT-PAPL-TIT-DT-PSC
                     :QT-PAPL-TIT-DT-PSC-N
                   , :QT-BLQD-PAPL-TIT
                     :QT-BLQD-PAPL-TIT-N
                   , :QT-DSPN-PAPL-TIT
                     :QT-DSPN-PAPL-TIT-N
                   , :VL-PU-TIT-DT-PSC
                     :VL-PU-TIT-DT-PSC-N
                   , :VL-SDO-TIT-DT-PSC
                     :VL-SDO-TIT-DT-PSC-N
           END-EXEC

           MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ-1
           MOVE SQLERRD(3)             TO QT-REG-SELD

           IF  QT-REG-SELD EQUAL 0101
               SET SGTSL66B-TEM-MAIS-REGISTRO TO TRUE
               MOVE 0100               TO QT-REG-SELD
           END-IF

           MOVE QT-REG-SELD            TO SGTSL66B-QT-OCR-TAB
           MOVE SQLCODE                TO CD-FIM-CURSOR-PSQ-1
           .
       970102-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970103-FECHA-CURSOR-PSQ-1       SECTION.
      *-----------------------------------------------------------------
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO

           MOVE 'N'                    TO IN-CURSOR-PSQ-ABTO-1
           EXEC SQL
                CLOSE CURSOR-PSQ-1
           END-EXEC
           .
       970103-SAI.
           EXIT.

      *-----------------------------------------------------------------
       980000-GRAVAR-CTNR-RESPOSTA     SECTION.
      *-----------------------------------------------------------------
           MOVE 0011                   TO MSG1-CD-SEQL-ERRO

      *--> Exclui eventual container de erro de outros programas
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO

      *--> Monta o nome do container de resposta
           MOVE 'SGTSL66B-RPST'        TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *--> previsto o erro de container
           SET  ERRO-CONTAINER         TO TRUE

           EXEC CICS
                PUT  CONTAINER ( NM-CTNR                 )
                     CHANNEL   ( NM-CNL                  )
                     FLENGTH   ( LENGTH OF SGTSL66B-RPST )
                     FROM      ( SGTSL66B-RPST           )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       980000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       983000-EXCLUIR-RESPOSTA-ERRO    SECTION.
      *----------------------------------------------------------------*
           MOVE 0012                   TO MSG1-CD-SEQL-ERRO
      *----------------------------------------------------------------*
      * Exclui o container de resposta e de erro para evitar problemas *
      * em modulos executados mais de uma vez dentro da mesma sessao do*
      * CICS. Equivale ao initialize na area de resposta dos programas *
      * que utilizam COMMAREA.                                         *
      *----------------------------------------------------------------*
      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *--> nome_modulo -RPST

           MOVE NM-PGM                 TO NM-CTNR(1:8)
           MOVE '-RPST'                TO NM-CTNR(9:5)

           SET DEL-CONTAINER           TO TRUE

      *--> Exclui o container de resposta
      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS
                DELETE CONTAINER (NM-CTNR   )
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

      *--> Excluir o container de erro
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO
           .
       983000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       983100-EXCLUIR-CONTAINER-ERRO   SECTION.
      *----------------------------------------------------------------*
           MOVE 0013                   TO MSG1-CD-SEQL-ERRO

      *--> Limpar variaveis container de erro
           INITIALIZE IIBKERRO-PRM-ERRO

      *--> Exclui o container de erro
           MOVE CTNR-PRM-ERRO          TO NM-CTNR
           SET DEL-CONTAINER           TO TRUE

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS
                DELETE CONTAINER (NM-CTNR   )
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

      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------

      *--> Caso exista utilização de cursor
           IF  IN-CURSOR-PSQ-ABTO-1  EQUAL  'S'
               PERFORM 970103-FECHA-CURSOR-PSQ-1
           END-IF

           IF  ERRO-PROGRAMA-ACIONADO
               PERFORM 999100-MONITORAR-VRV-PGM-CHMD
           END-IF

           PERFORM 999200-MONITORAR-VRV-PGM-E-ENC
           .
       999000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999100-MONITORAR-VRV-PGM-CHMD   SECTION.
      *-----------------------------------------------------------------
      *--> Monitora as variáveis dos módulos chamados

           MOVE 'FUNCAO'               TO MSGKW999-NM-VRV-MNT
           MOVE FUNCAO                 TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'ARG01'                TO MSGKW999-NM-VRV-MNT
           MOVE ARG01                  TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'ARG02'                TO MSGKW999-NM-VRV-MNT
           MOVE ARG02                  TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'ARG03'                TO MSGKW999-NM-VRV-MNT
           MOVE ARG03                  TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'AREA-TAM'             TO MSGKW999-NM-VRV-MNT
           MOVE AREA-TAM               TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'AREA-FIM'             TO MSGKW999-NM-VRV-MNT
           MOVE AREA-FIM               TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT
           .
       999000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999200-MONITORAR-VRV-PGM-E-ENC  SECTION.
      *-----------------------------------------------------------------
      *--> Monitora variàveis do pgm, grava e encerra processamento

           IF  MSG1-CD-ERRO-TS             EQUAL  ZEROS
               MOVE  8888              TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS             EQUAL  SPACES
               MOVE  'Erro não especificado no módulo.'
                                       TO  MSG1-TX-ERRO-TS
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

      *--> Adiciona as variáveis para monitoração
           MOVE 'SGTSL66B-CD-CTRA-TIT-VL'    TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL66B-CD-CTRA-TIT-VL      TO MCR-S9-15
           MOVE MCR-S9-15                    TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL66B-CD-TIT-OPR-ETRD'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL66B-CD-TIT-OPR-ETRD     TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL66B-DT-INC-PSC'        TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL66B-DT-INC-PSC          TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTSL66B-DT-FIM-PSC'        TO MSGKW999-NM-VRV-MNT
           MOVE SGTSL66B-DT-FIM-PSC          TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

      *--> grava o erro no msg
           PERFORM MSG1-NTF-MSG
           MOVE MSG1-CD-ERRO-TS        TO IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS        TO IIBKERRO-TX-ERRO

           EXEC CICS PUT
              CONTAINER ( CTNR-PRM-ERRO               )
              CHANNEL   ( NM-CNL                      )
              FROM      ( IIBKERRO-PRM-ERRO           )
              FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
              NOHANDLE
           END-EXEC

           PERFORM 999999-ENCERRAR
           .
       999200-SAI.
           EXIT.

      *---------------------------------------*
      * 999001-TRATA-ABEND-CICS        SECTION.
      * 999002-TRATA-CONDITION-CICS    SECTION.
      * 999003-ADICIONA-VRV-MNT        SECTION.
      *---------------------------------------*
-INC HLPKP999

      *---------------------------------------*
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *---------------------------------------*
-INC SGTKP999

      *---------------------------------------*
      * 999010-TRATA-ERRO-DB2          SECTION.
      *---------------------------------------*
-INC HLPKP997

      *---------------------------------------*
      * MSG1-NTF-MSG                   SECTION.
      *---------------------------------------*
-INC MSGKPMG1

      *---------------------------------------*
       999999-ENCERRAR                 SECTION.
      *---------------------------------------*

           EXEC CICS
                RETURN
           END-EXEC
           .
       999999-SAI.
           EXIT.
      *---------------------- FIM SGTSL66B ----------------------------