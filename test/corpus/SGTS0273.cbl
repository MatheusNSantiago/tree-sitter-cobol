      *-----------------------------------------------------------------
      * PROGRAMA..: SGTS0273
      * ANALISTA..: F2020988 - CHRISTINA C. G. R. RIBEIRO
      * AUTOR.....: F2020988 - CHRISTINA C. G. R. RIBEIRO
      * OBJETIVO..: LISTAR VALOR DE MERCADO DE TITULOS ENCARTEIRADOS
      *               TABELAS DB2 INCC_MC
      *             OPERAÇÃO IIB 4212647
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS001 02.09.2019 - F2020988 - 2018/223992 - IMPLANTACAO
      * VRS002 14.01.2020 - F2020988 - 2018/223992 - ALTERAR DIA DA DATA
      *        DE INICIO DE PESQUISA PARA 01 QUANDO MES/ANO INFORMADO.
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTS0273.

      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
      *-----------------------------------------------------------------
       CONFIGURATION          SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       WORKING-STORAGE                 SECTION.
      *-----------------------------------------------------------------
       01  CTE-PROG     PIC  X(17) VALUE '*** SGTS0273 ***'.
       01  CTE-VERS     PIC  X(06) VALUE 'VRS002'.

      *-----------------------------------------------------------------
      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informacoes
-INC HLPKE01B
       LOCAL-STORAGE                   SECTION.
000760* book de para acionamento do msg1
-INC IIBKERRO
-INC MSGKWMG1
-INC HLPKW999
-INC SGTKERR1
000760
000760* Area de trabalho
000760 01 QT-REG-SELD                  PIC S9(4) COMP-5 VALUE 0.
001140 01 GDA-RESULTADO                PIC  9(009) VALUE ZEROS.
001140 01 GDA-RESTO                    PIC  9(009) VALUE ZEROS.

       01  IC-TAB                            PIC  9(04)       VALUE 0.

       01  PARAMETROS.
           03 CD-TIT-OPR-ETRD-E        PIC  X(12) VALUE ' '.
           03 CD-TIT-OPR-INC           PIC  X(12) VALUE ' '.
           03 CD-TIT-OPR-FIM           PIC  X(12) VALUE ' '.
           03 DT-INC-PSQ               PIC  X(10) VALUE ' '.
           03 DT-FIM-PSQ               PIC  X(10) VALUE ' '.
           03 DT-AUX                   PIC  X(10) VALUE ' '.
           03 FILLER REDEFINES DT-AUX.
              05 DT-AUX-ANO            PIC  9(4).
              05 FILLER3               PIC  X(1).
              05 DT-AUX-MES            PIC  9(2).
              05 FILLER4               PIC  X(1).
              05 DT-AUX-DIA            PIC  9(2).
           03 DT-PSQ                   PIC  X(10) VALUE ' '.
           03 FILLER REDEFINES DT-PSQ.
              05 DT-PSQ-DIA            PIC  9(2).
              05 FILLER1               PIC  X(1).
              05 DT-PSQ-MES            PIC  9(2).
              05 FILLER2               PIC  X(1).
              05 DT-PSQ-ANO            PIC  9(4).

           03 DT-VL-INCC-MC-E          PIC  X(10) VALUE ' '.
           03 NR-SEQL-INCC-MC-E        PIC S9(9) COMP-5 VALUE 0.

000760
000760* Campos utilizados para leitura
       01  REG-WS.
           03 CD-TIT-OPR-ETRD          PIC X(12)
              OCCURS 101 TIMES         VALUE ' '.
           03 DT-VL-INCC-MC            PIC X(10)
              OCCURS 101 TIMES         VALUE ' '.
           03 NR-SEQL-INCC-MC          PIC S9(9) COMP-5
              OCCURS 101 TIMES         VALUE 0.
           03 VL-INCC-MC               PIC S9(15)V9(2) USAGE COMP-3
              OCCURS 101 TIMES         VALUE 0.
           03 TS-ULT-ATL               PIC X(26)
              OCCURS 101 TIMES         VALUE ' '.
000760
000760
000760* Variavel(is) de controle do cursor(es)
000760 01  CTL-CURSOR-PSQ.
000760     03  CD-FIM-CURSOR-PSQ       PIC S9(04) COMP-5 VALUE 0.
000760         88  FIM-CURSOR-PSQ      VALUE +100.
000760     03  IN-CURSOR-PSQ-ABTO      PIC  X(01) VALUE 'N'.
000760         88  CURSOR-PSQ-ABTO     VALUE 'S'.
000760
000760* Subrotinas utilizadas
000760     EXEC SQL
000760         INCLUDE SQLCA
000760     END-EXEC.
000760
000760*-----------------------------------------------------------------
-INC SGTK0273
           EXEC SQL
             DECLARE CURSOR-PSQ1 CURSOR
               WITH ROWSET POSITIONING FOR

             SELECT A.CD_TIT_OPR_ETRD
                  , A.DT_VL_INCC_MC
                  , A.NR_SEQL_INCC_MC
                  , A.VL_INCC_MC
                  , A.TS_ULT_ATL
             FROM DB2SGT.INCC_MC AS A
             WHERE  A.CD_TIT_OPR_ETRD > ' '                         AND
                   (A.DT_VL_INCC_MC >= :DT-INC-PSQ                  AND
                    A.DT_VL_INCC_MC <= :DT-FIM-PSQ)                 AND
                   (UPPER(COALESCE(A.CD_TIT_OPR_ETRD, ' ')) >=
                    UPPER(:CD-TIT-OPR-INC)                          AND
                    UPPER(COALESCE(A.CD_TIT_OPR_ETRD, ' ')) <=
                    UPPER(:CD-TIT-OPR-FIM))                         AND
                    A.TS_ULT_ATL = (SELECT MAX(B.TS_ULT_ATL) FROM
                                           DB2SGT.INCC_MC AS B
                                           WHERE A.CD_TIT_OPR_ETRD =
                                                 B.CD_TIT_OPR_ETRD  AND
                                                 A.NR_DVS_TIT      =
                                                 B.NR_DVS_TIT       AND
                                                 A.DT_VL_INCC_MC   =
                                                 B.DT_VL_INCC_MC)   AND
                    UPPER(COALESCE(A.CD_TIT_OPR_ETRD, ' '))      >=
                    UPPER(:CD-TIT-OPR-ETRD-E)                       AND
                    A.DT_VL_INCC_MC   >= :DT-VL-INCC-MC-E           AND
                    A.NR_DVS_TIT       = 1                          AND
                    A.NR_SEQL_INCC_MC >= :NR-SEQL-INCC-MC-E
             ORDER BY   A.DT_VL_INCC_MC DESC , A.CD_TIT_OPR_ETRD ASC
           END-EXEC.
000760
000760*-----------------------------------------------------------------
000760 PROCEDURE DIVISION.
000760*-----------------------------------------------------------------
000760
000760* Habilita tratamento automatico de erros DB2
-INC HLPKE997
000760* Habilita captura automatica de abend e erros CICS
-INC HLPKE999
000760
000760     PERFORM 100000-DESCOBRIR-CANAL
000760     PERFORM 110000-EXCLUIR-CTNR-RPST
000760     PERFORM 120000-EXCLUIR-CTNR-ERRO
000760     PERFORM 130000-RECEBER-REQUISICAO
000760     PERFORM 140000-VALIDAR-PARAMETRO
000760
000760     PERFORM 970101-ABRE-CURSOR-PSQ
000760     PERFORM 970102-LER-CURSOR-PSQ
000760     PERFORM 970103-FECHA-CURSOR-PSQ
000760
000760     PERFORM 200000-PROCESSA-PESQUISA
000760              VARYING SGTS0273-IC-TAB FROM 1 BY 1
000760              UNTIL SGTS0273-IC-TAB > SGTS0273-QT-OCR-TAB
000760
000760     PERFORM 300000-POSTA-RESPOSTA
000760
000760     GOBACK
000760     .
000760*-----------------------------------------------------------------
000760 100000-DESCOBRIR-CANAL          SECTION.
000760*-----------------------------------------------------------------
000760     MOVE 0001                   TO  MSG1-CD-SEQL-ERRO

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET  ERRO-CANAL             TO TRUE
001254
001254     EXEC CICS ASSIGN
001254        CHANNEL ( NM-CNL )
001254     END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
000760     .
000760 100000-SAI.
000760     EXIT.
000760*-----------------------------------------------------------------
000760 110000-EXCLUIR-CTNR-RPST        SECTION.
000760*-----------------------------------------------------------------
000760     MOVE 0002                   TO  MSG1-CD-SEQL-ERRO
000760
000760     EXEC CICS DELETE
000760        CONTAINER ( 'SGTS0273-RPST' )
000760        CHANNEL   ( NM-CNL      )
000760        NOHANDLE
000760     END-EXEC
000760
000760     IF  NOT (EIBRESP EQUAL DFHRESP(NORMAL)   OR
000760         EIBRESP EQUAL DFHRESP(CONTAINERERR) )
000760         PERFORM 999002-TRATA-CONDITION-CICS
000760     END-IF
000760     .
000760 110000-SAI.
000760     EXIT.
000760*-----------------------------------------------------------------
000760 120000-EXCLUIR-CTNR-ERRO        SECTION.
000760*-----------------------------------------------------------------
000760     MOVE 0003                   TO  MSG1-CD-SEQL-ERRO
000760
000760     EXEC CICS DELETE
000760        CONTAINER ( 'PRM-ERRO' )
000760        CHANNEL   ( NM-CNL      )
000760        NOHANDLE
000760     END-EXEC
000760
000760     IF  NOT (EIBRESP EQUAL DFHRESP(NORMAL)   OR
000760         EIBRESP EQUAL DFHRESP(CONTAINERERR) )
000760         PERFORM 999002-TRATA-CONDITION-CICS
000760     END-IF
000760     .
000760 120000-SAI.
000760     EXIT.
000760*-----------------------------------------------------------------
000760 130000-RECEBER-REQUISICAO       SECTION.
000760*-----------------------------------------------------------------
000760     MOVE 0004                   TO  MSG1-CD-SEQL-ERRO
000760
000760     MOVE ZEROS                  TO  QT-CCT-LIDO
000760
000760     MOVE LENGTH OF SGTS0273-RQSC    TO  QT-CCT-EPRO

           MOVE 'SGTS0273-RQSC'        TO  NM-CTNR
           SET  GET-CONTAINER          TO  TRUE
           SET  ERRO-CONTAINER         TO  TRUE

000760     EXEC CICS GET
000760        CONTAINER ( NM-CTNR )
000760        CHANNEL   ( NM-CNL      )
000760        NODATA
000760        FLENGTH ( QT-CCT-LIDO )
000760     END-EXEC

      * 3001
000760     IF  QT-CCT-LIDO NOT EQUAL   QT-CCT-EPRO
000760         SET  ERRO-TAMANHO-AREA  TO  TRUE
000760         MOVE 3001               TO  MSG1-CD-ERRO-TS
000760         PERFORM 999000-GRAVA-ERRO-E-ENCERRA
000760     END-IF
000760
000760     EXEC CICS GET
000760        CONTAINER ( NM-CTNR )
000760        CHANNEL   ( NM-CNL      )
000760        FLENGTH ( LENGTH OF SGTS0273-RQSC )
000760        INTO    ( SGTS0273-RQSC )
000760     END-EXEC

           SET  SEM-ERRO               TO  TRUE
000760     .
000760 130000-SAI.
000760     EXIT.
000760*-----------------------------------------------------------------
000760 140000-VALIDAR-PARAMETRO        SECTION.
000760*-----------------------------------------------------------------
000760     MOVE 0006                   TO  MSG1-CD-SEQL-ERRO
000760
000760     MOVE 'N'                    TO  SGTS0273-IN-CTNC-PSQ
000760

           PERFORM 100100-VALIDAR-CD-TIT-OPR-ETRD
           PERFORM 100200-VALIDAR-DT-PSQ
           PERFORM 100300-VALIDAR-DT-INC

           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100100-VALIDAR-CD-TIT-OPR-ETRD   SECTION.
      * Verificar Código do Tipo de Estado Descrição.
      *-----------------------------------------------------------------
           MOVE 0003                         TO MSG1-CD-SEQL-ERRO

           IF SGTS0273-CD-TIT-OPR-PSQ   NOT = ' '
              MOVE SGTS0273-CD-TIT-OPR-PSQ   TO CD-TIT-OPR-INC
                                                CD-TIT-OPR-FIM
           ELSE
              MOVE ' '                       TO CD-TIT-OPR-INC
              MOVE '999999999999'            TO CD-TIT-OPR-FIM
           END-IF

           MOVE SGTS0273-CD-TIT-OPR-ETRD-E   TO CD-TIT-OPR-ETRD-E
           MOVE SGTS0273-NR-SEQL-INCC-MC-E   TO NR-SEQL-INCC-MC-E
           IF SGTS0273-DT-VL-INCC-MC-E = ' '
              MOVE '01.01.1900'              TO DT-VL-INCC-MC-E
           ELSE
              MOVE SGTS0273-DT-VL-INCC-MC-E  TO DT-VL-INCC-MC-E
           END-IF
           .
       100100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100200-VALIDAR-DT-PSQ           SECTION.
      * Verifica a Data da Pesquisa Infomada.
      *-----------------------------------------------------------------
           MOVE 0005                         TO MSG1-CD-SEQL-ERRO

           MOVE SGTS0273-DT-PSQ        TO DT-AUX
           MOVE DT-AUX-DIA             TO DT-PSQ-DIA
           MOVE DT-AUX-MES             TO DT-PSQ-MES
           MOVE DT-AUX-ANO             TO DT-PSQ-ANO

           IF  SGTS0273-DT-PSQ NOT = ' '
               MOVE '-'                      TO FILLER-1 FILLER-2
               MOVE '.'  TO FILLER1  FILLER2

               IF  DT-PSQ-DIA          EQUAL 0
                   EVALUATE DT-PSQ-MES
                      WHEN 01
                         MOVE 31             TO DT-PSQ-DIA
                      WHEN  03
                         MOVE 31             TO DT-PSQ-DIA
                      WHEN  05
                         MOVE 31             TO DT-PSQ-DIA
                      WHEN  07
                         MOVE 31             TO DT-PSQ-DIA
                      WHEN  08
                         MOVE 31             TO DT-PSQ-DIA
                      WHEN 10
                         MOVE 31             TO DT-PSQ-DIA
                      WHEN 12
                         MOVE 31             TO DT-PSQ-DIA
                      WHEN 04
                         MOVE 30             TO DT-PSQ-DIA
                      WHEN 06
                         MOVE 30             TO DT-PSQ-DIA
                      WHEN  09
                         MOVE 30             TO DT-PSQ-DIA
                      WHEN  11
                         MOVE 30             TO DT-PSQ-DIA
                      WHEN 02

                         DIVIDE DT-PSQ-ANO BY 4 GIVING GDA-RESULTADO
                                         REMAINDER GDA-RESTO
                         ON SIZE ERROR
                            SET  ERRO-PROGRAMADO   TO  TRUE
                            MOVE 768               TO  MSG1-CD-ERRO-TS
                            MOVE
                            'Data da posição de cálculo inválida.'
                                                   TO  MSG1-TX-ERRO-TS
                            PERFORM 999000-GRAVA-ERRO-E-ENCERRA
                         END-DIVIDE
                         IF GDA-RESTO = 0
                            MOVE 29          TO DT-PSQ-DIA
                         ELSE
                            MOVE 28          TO DT-PSQ-DIA
                         END-IF

                   END-EVALUATE
               END-IF
           END-IF
           .
       100200-SAI.
           EXIT.
      *-----------------------------------------------------------------
       100300-VALIDAR-DT-INC      SECTION.
      * Verificar Data Inicio e Data Fim da Pesquisa.
      *-----------------------------------------------------------------
           MOVE 0004                   TO MSG1-CD-SEQL-ERRO

           IF SGTS0273-DT-PSQ          EQUAL SPACES
              MOVE '1900-01-01'        TO DT-INC-PSQ
              MOVE '2900-12-31'        TO DT-FIM-PSQ
           ELSE
               MOVE DT-PSQ             TO DT-FIM-PSQ
               MOVE 01                 TO DT-PSQ-DIA
               MOVE DT-PSQ             TO DT-INC-PSQ
           END-IF

            .
       100300-SAI.
           EXIT.
000760*-----------------------------------------------------------------
000760 200000-PROCESSA-PESQUISA        SECTION.
000760*-----------------------------------------------------------------
000760     MOVE 0007                   TO  MSG1-CD-SEQL-ERRO
000760
           MOVE CD-TIT-OPR-ETRD(SGTS0273-IC-TAB) TO
                     SGTS0273-CD-TIT-OPR-ETRD(SGTS0273-IC-TAB)
           MOVE DT-VL-INCC-MC(SGTS0273-IC-TAB)   TO
                     SGTS0273-DT-VL-INCC-MC(SGTS0273-IC-TAB)
           MOVE NR-SEQL-INCC-MC(SGTS0273-IC-TAB) TO
                     SGTS0273-NR-SEQL-INCC-MC(SGTS0273-IC-TAB)
           MOVE VL-INCC-MC(SGTS0273-IC-TAB)      TO
                     SGTS0273-VL-INCC-MC(SGTS0273-IC-TAB)
           MOVE TS-ULT-ATL(SGTS0273-IC-TAB)      TO
                     SGTS0273-TS-ULT-ATL(SGTS0273-IC-TAB)
000760     .
000760 200000-SAI.
000760     EXIT.
000760
000760*-----------------------------------------------------------------
000760 970101-ABRE-CURSOR-PSQ          SECTION.
000760*-----------------------------------------------------------------
000760     MOVE 0008                   TO  MSG1-CD-SEQL-ERRO
000760
000760     EXEC SQL
000760          OPEN CURSOR-PSQ1
000760     END-EXEC
000076
000760     MOVE 'S'                    TO  IN-CURSOR-PSQ-ABTO
000760     .
000760 970101-SAI.
000760     EXIT.
000760*-----------------------------------------------------------------
000760 970102-LER-CURSOR-PSQ           SECTION.
000760*-----------------------------------------------------------------
000760     MOVE 0009                   TO  MSG1-CD-SEQL-ERRO
000760
000760     INITIALIZE  REG-WS
000760
000760     EXEC SQL
                FETCH NEXT ROWSET
                      CURSOR-PSQ1 FOR 0101 ROWS
               INTO :CD-TIT-OPR-ETRD
                  , :DT-VL-INCC-MC
                  , :NR-SEQL-INCC-MC
                  , :VL-INCC-MC
                  , :TS-ULT-ATL
000760     END-EXEC
000760
000760     MOVE SQLERRD(3)             TO  QT-REG-SELD
000760
000760     IF  QT-REG-SELD EQUAL 0101
000760         SET SGTS0273-TEM-MAIS-REGISTRO TO TRUE
000760         MOVE 0100               TO  QT-REG-SELD
000760     END-IF
000760
000760     MOVE QT-REG-SELD            TO  SGTS0273-QT-OCR-TAB
000760     MOVE SQLCODE                TO  CD-FIM-CURSOR-PSQ
000760     .
000760 970102-SAI.
000760     EXIT.
000760*-----------------------------------------------------------------
000760 970103-FECHA-CURSOR-PSQ         SECTION.
000760*-----------------------------------------------------------------
000760     MOVE 0010                   TO  MSG1-CD-SEQL-ERRO
000760
000760     MOVE 'N'                    TO  IN-CURSOR-PSQ-ABTO
000760
000760     EXEC SQL
000760          CLOSE CURSOR-PSQ1
000760     END-EXEC
000760     .
000760 970103-SAI.
000760     EXIT.
000760*-----------------------------------------------------------------
000760 300000-POSTA-RESPOSTA           SECTION.
000760*-----------------------------------------------------------------
000760     MOVE 0011                   TO  MSG1-CD-SEQL-ERRO

           MOVE 'SGTS0273-RPST'        TO  NM-CTNR
           SET  PUT-CONTAINER          TO  TRUE
           SET  ERRO-CONTAINER         TO  TRUE
000760
000760     EXEC CICS PUT
000760        CONTAINER ( NM-CTNR )
000760        CHANNEL   ( NM-CNL      )
000760        FLENGTH ( LENGTH OF SGTS0273-RPST )
000760        FROM    ( SGTS0273-RPST )
000760     END-EXEC

           SET  SEM-ERRO               TO  TRUE
000760     .
000760 300000-SAI.
000760     EXIT.
000760*-----------------------------------------------------------------
000760 999000-GRAVA-ERRO-E-ENCERRA     SECTION.
000760*-----------------------------------------------------------------

      *--> Monitora variàveis do pgm, grava e encerra

           IF  MSG1-CD-ERRO-TS             EQUAL  ZEROS
              MOVE  8888               TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS             EQUAL  SPACES
               MOVE  'Erro não especificado no módulo.'
                                       TO  MSG1-TX-ERRO-TS
           END-IF

      *--> Verifica se o tipo de erro gerado possui tratamento interno.
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

      *==> Observação <=================================================
      *    Normalmente se Monitora apenas as Variáveis da Requisição
      *    para Monitoração da Mensageria.
      *    O Analista que especificou o módulo e o Analista Programador
      *    O Analista que especificou o módulo e o Analista Programador
      *    tem liberdade para Monitorar outras Variáveis que forem
      *    relevantes ao Suporte do Sistema.
      *=================================================================
000760
           MOVE 'SGTS0273-CD-TIT-OPR-ETRD-E'     TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0273-CD-TIT-OPR-ETRD-E      TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0273-DT-VL-INCC-MC-E '      TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0273-DT-VL-INCC-MC-E        TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0273-NR-SEQL-INCC-MC-E'     TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0273-NR-SEQL-INCC-MC-E      TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0273-CD-TIT-OPR-PSQ'        TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0273-CD-TIT-OPR-PSQ         TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0273-DT-PSQ'                TO  MSGKW999-NM-VRV-MNT
           MOVE SGTS0273-DT-PSQ                  TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'DT-INC-PSQ'                     TO  MSGKW999-NM-VRV-MNT
           MOVE  DT-INC-PSQ                      TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'DT-FIM-PSQ'                     TO  MSGKW999-NM-VRV-MNT
           MOVE  DT-FIM-PSQ                      TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'CD-TIT-OPR-INC'                 TO  MSGKW999-NM-VRV-MNT
           MOVE  CD-TIT-OPR-INC                  TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'CD-TIT-OPR-FIM'                 TO  MSGKW999-NM-VRV-MNT
           MOVE  CD-TIT-OPR-FIM                  TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

000760
000760* --> grava o erro no msg
000760     PERFORM MSG1-NTF-MSG
000760     MOVE MSG1-CD-ERRO-TS                TO  IIBKERRO-CD-ERRO
000760     MOVE MSG1-TX-ERRO-TS                TO  IIBKERRO-TX-ERRO
000760
000760     EXEC CICS PUT
000760        CONTAINER ( 'PRM-ERRO' )
000760        CHANNEL   ( NM-CNL      )
000760        FROM      ( IIBKERRO-PRM-ERRO)
000760        FLENGTH ( LENGTH OF IIBKERRO-PRM-ERRO )
000760        NOHANDLE
000760     END-EXEC
000760
000760     PERFORM 999999-ENCERRAR
000760     .
000760 999000-SAI.
000760     EXIT.
000760* --------------------------------------*
000760* 999001-TRATA-ABEND-CICS        SECTION.
000760* 999002-TRATA-CONDITION-CICS    SECTION.
000760* 999003-ADICIONA-VRV-MNT        SECTION.
000760* --------------------------------------*
-INC HLPKP999
000760* --------------------------------------*
000760* 999010-TRATA-ERRO-DB2          SECTION.
-INC HLPKP997
000760* --------------------------------------*
000760* MSG1-NTF-MSG                   SECTION.
-INC MSGKPMG1
-INC SGTKP999
000760* --------------------------------------*
000760 999999-ENCERRAR                SECTION.
000760* --------------------------------------*
000760     EXEC CICS
000760          RETURN
000760     END-EXEC.
      *---------------------- FIM SGTS0273 ----------------------------