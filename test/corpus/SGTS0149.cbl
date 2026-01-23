
      * PROGRAMA..: SGTS0149                                              VRS001
      * ANALISTA..: F2054109 - Cintia Ap Soares Iglesias                  VRS001
      * AUTOR.....: F2054109 - Cintia Ap Soares Iglesias                  VRS001
      * DATA......: 27/07/2017                                            VRS001
      * OBJETIVO..: Validar se os periodos informados na entrada
      *             compoem inteiramente a vigência da operação
      * Tipo de comunicação: Container
      * COMPILACAO: COBOL/CICS                                            VRS001
      *-----------------------------------------------------------------  VRS001
      *VRS003 09/09/2019 F9158937- Acerto DT-FIM-CARE,
      *VRS002 31/01/2019 F9158937- Acerto parâmetros e monitoração
      *                             SBDATA
      *                          - Acerto data início primeiro período
      *                            retirando D+1 E Adaptação mensageria
      *                            na rotina (230000-TRATAR-PERIODOS)
      *       21/01/2019 F9188847 - Retirada CALC-DATA-POS-CARENCIA,subs
      *       tituído por Validação de período início e fim de carência
      *       07/01/2019 F2804421 DE 2016/145195 - Inclusão do comando
      *                  'GOBACK'.
      *VRS001 27/07/2017 F2054109 DE-2016/145195 - IMPLANTACAO
      *-----------------------------------------------------------------  VRS001
       IDENTIFICATION DIVISION.                                           VRS001
      *----------------------------------------------------------------*  VRS001
       PROGRAM-ID. SGTS0149.                                              VRS001
      *                                                                   VRS001
      *----------------------------------------------------------------*  VRS001
       ENVIRONMENT DIVISION.                                              VRS001
      *----------------------------------------------------------------*  VRS001
      *                                                                   VRS001
      *----------------------------------------------------------------*  VRS001
       CONFIGURATION                   SECTION.                           VRS001
      *----------------------------------------------------------------*  VRS001
       SPECIAL-NAMES.                                                     VRS001
           DECIMAL-POINT IS COMMA.                                        VRS001
      *                                                                   VRS001
      *----------------------------------------------------------------*  VRS001
       DATA DIVISION.                                                     VRS001
      *----------------------------------------------------------------*  VRS001
      *                                                                   VRS001
                                                                          VRS001
       WORKING-STORAGE                 SECTION.                           VRS001
      *----------------------------------------------------------------*  VRS001
                                                                          VRS001
       77  CTE-INICIO-SS               PIC  X(040)  VALUE                 VRS001
                    '*** S.S. COMEÇA AQUI ***'.                           VRS001
                                                                          VRS001
       77  CTE-PROG                    PIC  X(018)  VALUE                 VRS001
                    '*** SGTS0149***'.                                    VRS001
       77  CTE-VERS                    PIC  X(006)  VALUE 'VRS003'.       VRS001
                                                                          VRS001
      * Módulos acionados                                                 VRS001
                                                                          VRS001
       77  SBDATAF                     PIC  X(008) VALUE 'SBDATAF'.
      *----------------------------------------------------------------*  VRS001
       LOCAL-STORAGE                   SECTION.                           VRS001
      *----------------------------------------------------------------*  VRS001
                                                                          VRS001
      * Áreas de trabalho

       01  IC                          PIC S9(04) VALUE 0.
       01  IC-ANTERIOR                 PIC S9(04) VALUE 0.

       01  DT-FIM-VGC                     PIC  X(08) VALUE ' '.
       01  DT-FIM-VGC-R        REDEFINES  DT-FIM-VGC.
           03 ANO                      PIC  9(4).
           03 MES                      PIC  9(2).
           03 DIA                      PIC  9(2).

       01  DT-INC-VGC                  PIC  X(08) VALUE ' '.
       01  DT-INC-VGC-R     REDEFINES  DT-INC-VGC.
           03 ANO                      PIC  9(4).
           03 MES                      PIC  9(2).
           03 DIA                      PIC  9(2).

       01  AUX-DT-INC                  PIC  X(08) VALUE ' '.
       01  AUX-DT-INC-R     REDEFINES  AUX-DT-INC.
           03 ANO                      PIC  9(4).
           03 MES                      PIC  9(2).
           03 DIA                      PIC  9(2).

       01  AUX-DT-FIM                  PIC  X(08) VALUE ' '.
       01  AUX-DT-FIM-R     REDEFINES  AUX-DT-FIM.
           03 ANO                      PIC  9(4).
           03 MES                      PIC  9(2).
           03 DIA                      PIC  9(2).

       01  AUX-DT                      PIC  X(08) VALUE ' '.
       01  AUX-DT-R         REDEFINES  AUX-DT.
           03 ANO                      PIC  9(4).
           03 MES                      PIC  9(2).
           03 DIA                      PIC  9(2).

       01  DT-FIM-CARE                  PIC  X(08) VALUE ' '.
       01  DT-FIM-CARE-R     REDEFINES  DT-FIM-CARE.
           03 ANO                      PIC  9(4).
           03 MES                      PIC  9(2).
           03 DIA                      PIC  9(2).

      *    SBDATAF F01   VALIDA DATA
       01  AREA-SBDATAF-F01.
           03  FUNCAO-F01              PIC  X(3)  VALUE 'F01'.
           03  ARG01-F01               PIC  9(8)  VALUE 0.
           03  ARG01-F01-R REDEFINES ARG01-F01.
               05  DIA                 PIC  9(2).
               05  MES                 PIC  9(2).
               05  ANO                 PIC  9(4).
           03  ARG02-F01               PIC  9(1)  VALUE 0.

       01  ARG01-F01-MSG               PIC  9(8)  VALUE 0.

      *    SBDATAF F05 - CALCULAR DATA FINAL = DATA BASE +/- (QTD DIAS)
       01  AREA-SBDATA-F05.
           03  FUNCAO-F05              PIC  X(3)  VALUE 'F05'.
           03  ARG01-F05               PIC  9(8)  VALUE 0.
           03  ARG01-F05-R REDEFINES ARG01-F05.
               05  DIA                 PIC  9(2).
               05  MES                 PIC  9(2).
               05  ANO                 PIC  9(4).
           03  ARG02-F05               PIC  9(5)  VALUE 0.
           03  ARG03-F05               PIC  9(1)  VALUE 0.
           03  ARG04-F05               PIC  9(8)  VALUE 0.
           03  ARG04-F05-R REDEFINES ARG04-F05.
               05  DIA                 PIC  9(2).
               05  MES                 PIC  9(2).
               05  ANO                 PIC  9(4).

      *    SBDATAF F26 - CALCULAR DATA FINAL = DATA BASE +/- (QTD MESES)
       01  AREA-SBDATA-F26.
           03  FUNCAO-F26              PIC  X(3)  VALUE 'F26'.
           03  ARG01-F26               PIC  9(8)  VALUE 0.
           03  ARG01-F26-R REDEFINES ARG01-F26.
               05  DIA                 PIC  9(2).
               05  MES                 PIC  9(2).
               05  ANO                 PIC  9(4).
           03  ARG02-F26               PIC  9(3)  VALUE 0.
           03  ARG03-F26               PIC  9(8)  VALUE 0.
           03  ARG03-F26-R REDEFINES ARG03-F26.
               05  DIA                 PIC  9(2).
               05  MES                 PIC  9(2).
               05  ANO                 PIC  9(4).

       01  WKSBDATA                    PIC X(4095).
       01  AREA-RED REDEFINES WKSBDATA.
           03  AREA-TAM                PIC 9(0008) COMP.
           03  FILLER                  PIC X(4071).
           03  AREA-FIM                PIC X(0020).

       01  FUNCAO-SBDATAF              PIC  X(3)  VALUE SPACES.

      *----------------------------------------------------------------*  VRS001
      * Book dos Containers e das subrotinas chamadas                     VRS001
      *----------------------------------------------------------------*  VRS001
                                                                          VRS001
      * Book das variáveis auxiliares do tratamento de erro via MSG       VRS001
-INC SGTKERR1

      * Book para captura de abend cics da bloco código MSGKP999          VRS001
-INC HLPKW999
                                                                          VRS001
      * Book para captura de erro via MSG1                                VRS001
-INC MSGKWMG1
                                                                          VRS001
      * Book do container de requisição e resposta deste módulo           VRS001
       01 SGTS0149-DADOS.                                                 VRS001
-INC SGTK0149
                                                                          VRS001
      * Book do Container de Erro
-INC IIBKERRO

       01  CTE-FINAL-SS                PIC X(40)      VALUE               VRS001
                    '          *** S.S. TERMINA AQUI ***'.                VRS001
      *----------------------------------------------------------------*  VRS001
       PROCEDURE DIVISION.                                                VRS001
      *----------------------------------------------------------------*  VRS001
       000000-ROTINA-PRINCIPAL         SECTION.                           VRS001
      *----------------------------------------------------------------*  VRS001
                                                                          VRS001
           PERFORM 100000-BUSCA-REQUISICAO                                VRS001
                                                                          VRS001
           PERFORM 200000-PROCESSAR

           PERFORM 999999-ENCERRAR                                        VRS00

           GOBACK
           .                                                              VRS002
       000000-SAI.                                                        VRS001
           EXIT.                                                          VRS001
      *----------------------------------------------------------------*  VRS001
       100000-BUSCA-REQUISICAO         SECTION.                           VRS001
      *----------------------------------------------------------------*  VRS001
                                                                          VRS001
           MOVE 0001                   TO MSG1-CD-SEQL-ERRO               VRS001
                                                                          VRS001
      *    Ativa a captura automática de abend do CICS.                   VRS001
      *    WHENEVER CICS                                                  VRS001
-INC HLPKE999
                                                                          VRS001
           PERFORM 970000-LER-CANAL                                       VRS001
                                                                          VRS001
      *    Define o nome do programa p/ fins manipulação container        VRS001
           MOVE 'SGTS0149'             TO NM-PGM                          VRS001
           MOVE 'SGTS0149-RPST'        TO NM-CTNR
                                                                          VRS001
      *    Limpa a resposta e erro antes de executar o módulo             VRS001
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO                           VRS001
           MOVE SPACES  TO  SGTS0149-TX-RTN-PRCT

           PERFORM 971000-LER-REQUISICAO                                  VRS001
           .                                                              VRS001
       100000-SAI.                                                        VRS001
           EXIT.                                                          VRS001

      *----------------------------------------------------------------*  VRS001
       200000-PROCESSAR                SECTION.                           VRS001
      *----------------------------------------------------------------*  VRS001

           PERFORM 210000-VALIDAR-ENTRADA                                 VRS001

           PERFORM 220000-CALC-DATA-POS-CARENCIA

           PERFORM 230000-TRATAR-PERIODOS

           PERFORM 980000-GRAVAR-CONTAINER-RPST
           .
       200000-SAI.                                                        VRS001
           EXIT.
                                                                          VRS001
      *----------------------------------------------------------------*  VRS001
       210000-VALIDAR-ENTRADA       SECTION.                              VRS001
      *----------------------------------------------------------------*  VRS001
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO               VRS001
                                                                          VRS001
           IF  SGTS0149-DT-INC-VGC     EQUAL SPACES
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0237               TO MSG1-CD-ERRO-TS                 VRS001
               MOVE 'Data de compra não informada.'
                                       TO MSG1-TX-ERRO-TS                 VRS001
               SET MNT-VRV             TO TRUE                            VRS001
               PERFORM 999000-ERRO
           ELSE
               MOVE CORR SGTS0149-DT-INC-VGC-R  TO  ARG01-F01-R
               MOVE      ARG01-F01              TO  ARG01-F01-MSG
               MOVE CORR SGTS0149-DT-INC-VGC-R  TO   DT-INC-VGC-R
               MOVE 4                  TO  ARG02-F01
               PERFORM 930000-CHAMAR-SBDATA-F01
           END-IF

           IF  SGTS0149-DT-FIM-VGC     EQUAL SPACES
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0239               TO MSG1-CD-ERRO-TS                 VRS001
               MOVE 'Data de vencimento não informada.'
                                       TO MSG1-TX-ERRO-TS                 VRS001
               SET MNT-VRV             TO TRUE                            VRS001
               PERFORM 999000-ERRO
           ELSE
              MOVE CORR SGTS0149-DT-FIM-VGC-R
                                       TO  ARG01-F01-R
              MOVE      ARG01-F01      TO  ARG01-F01-MSG
              MOVE CORR SGTS0149-DT-FIM-VGC-R
                                       TO   DT-FIM-VGC-R
      *   Validação no formato Gregoriano
               MOVE 4                  TO  ARG02-F01
               PERFORM 930000-CHAMAR-SBDATA-F01
           END-IF

           IF  SGTS0149-QT-MM-CARE < ZEROS
               SET ERRO-PROGRAMADO     TO TRUE                            VRS001
               MOVE 0439               TO MSG1-CD-ERRO-TS                 VRS001
               MOVE 'Quantidade de meses de carência inválida.'
                                       TO MSG1-TX-ERRO-TS                 VRS001
               SET MNT-VRV             TO TRUE                            VRS001
               PERFORM 999000-ERRO                                        VRS001
           END-IF

           IF  SGTS0149-QT-PER-LS < ZEROS
               SET ERRO-PROGRAMADO     TO TRUE                            VRS001
               MOVE 0425               TO MSG1-CD-ERRO-TS                 VRS001
               MOVE 'Quantidade de períodos inválida.'
                                       TO MSG1-TX-ERRO-TS                 VRS001
               SET MNT-VRV             TO TRUE                            VRS001
               PERFORM 999000-ERRO                                        VRS001
           END-IF

           PERFORM
                VARYING IC FROM 1 BY 1
                UNTIL   IC GREATER SGTS0149-QT-PER-LS

                IF  SGTS0149-DT-INC-PER (IC)  EQUAL SPACES
                    SET ERRO-PROGRAMADO TO TRUE
                    MOVE 0393           TO MSG1-CD-ERRO-TS                VRS001
                    MOVE 'Data de início do período não informada.'
                                       TO MSG1-TX-ERRO-TS                 VRS001
                    SET MNT-VRV             TO TRUE                       VRS001
                    PERFORM 999000-ERRO
                ELSE
                    MOVE CORR SGTS0149-DT-INC-PER-R  (IC)
                                       TO  ARG01-F01-R
                    MOVE  ARG01-F01    TO  ARG01-F01-MSG
                    MOVE 4             TO  ARG02-F01
                    PERFORM 930000-CHAMAR-SBDATA-F01
                END-IF

                IF  SGTS0149-DT-FIM-PER (IC)   EQUAL SPACES
                    SET ERRO-PROGRAMADO TO TRUE
                    MOVE 0395          TO MSG1-CD-ERRO-TS                 VRS001
                    MOVE 'Data final do período não informada.'
                                       TO MSG1-TX-ERRO-TS                 VRS001
                    SET MNT-VRV        TO TRUE                            VRS001
                    PERFORM 999000-ERRO
                ELSE
                    MOVE CORR SGTS0149-DT-FIM-PER-R  (IC)
                                       TO  ARG01-F01-R
                    MOVE  ARG01-F01    TO  ARG01-F01-MSG
                    MOVE 4             TO  ARG02-F01
                    PERFORM 930000-CHAMAR-SBDATA-F01
                END-IF

           END-PERFORM
           .                                                              VRS001
       210000-SAI.                                                        VRS001
           EXIT.                                                          VRS001

      *----------------------------------------------------------------*  VRS001
       220000-CALC-DATA-POS-CARENCIA    SECTION.
      *----------------------------------------------------------------*  VRS001
           MOVE 0003                   TO MSG1-CD-SEQL-ERRO

           IF SGTS0149-QT-MM-CARE > ZEROS
              MOVE CORR SGTS0149-DT-FIM-PER-R(1)  TO DT-FIM-CARE-R
           END-IF

      *    IF SGTS0149-QT-MM-CARE EQUAL ZEROS
      *         MOVE CORR DT-INC-VGC-R  TO DT-FIM-CARE-R
      *    ELSE
      *        MOVE CORR SGTS0149-DT-INC-VGC-R
      *                                    TO  ARG01-F26-R
      *   Quantidade de meses
      *        MOVE SGTS0149-QT-MM-CARE    TO  ARG02-F26
      *
      *        MOVE 4095                   TO  AREA-TAM
      *        MOVE 'SBDATAF -WRKAREA-FIM' TO  AREA-FIM
      *
      *        CALL SBDATAF                USING FUNCAO-F26
      *                                    WKSBDATA
      *                                    ARG01-F26 ARG02-F26
      *                                    ARG03-F26
      *
      *--> Return code = 24: Data Inválida.

      *        IF  RETURN-CODE             NOT EQUAL ZEROS AND 24 AND 22
      *                                                AND 46
      *            SET ERRO-PROGRAMA-ACIONADO TO TRUE
      *            MOVE 0051               TO MSG1-CD-ERRO-TS
      *            MOVE RETURN-CODE        TO MSG1-CD-RTN-PGM-ACND
      *            MOVE 'Erro no tratamento de datas.'
      *                                    TO MSG1-TX-RTN-PGM-ACND
      *                                       MSG1-TX-ERRO-TS
      *            MOVE SBDATAF            TO MSG1-NM-PGM-ACND
      *                                       NM-PGM
      *        Monitorar variáveis do módulo chamado
      *            MOVE FUNCAO-F26   TO  FUNCAO-SBDATAF
      *            PERFORM 969000-MONITORAR-VARIAVEIS
      *            PERFORM 999000-ERRO
      *        END-IF
      *
      *        IF  RETURN-CODE             EQUAL 24
      *            SET ERRO-PROGRAMADO     TO TRUE
      *            MOVE 0049               TO MSG1-CD-ERRO-TS
      *            MOVE 'Data informada inválida'
      *                                    TO MSG1-TX-ERRO-TS
      *            SET MNT-VRV             TO TRUE
      *            PERFORM 999000-ERRO
      *        END-IF
      *
      *        MOVE CORR ARG03-F26-R  TO DT-FIM-CARE-R
      *    END-IF
           .
       220000-SAI.
           EXIT.

      *----------------------------------------------------------------*  VRS001
       230000-TRATAR-PERIODOS       SECTION.
      *----------------------------------------------------------------*  VRS001
           MOVE 0004                   TO MSG1-CD-SEQL-ERRO

           MOVE CORR SGTS0149-DT-INC-PER-R (1)
                                       TO  AUX-DT-INC-R
      *    IF SGTS0149-IN-VLDC-CARE EQUAL 'S'
      *       IF DT-FIM-CARE EQUAL  DT-INC-VGC
      *          IF AUX-DT-INC  >  DT-INC-VGC
      *             SET ERRO-PROGRAMADO  TO TRUE
      *             MOVE 0440            TO MSG1-CD-ERRO-TS
      *             MOVE  'Período informado não cobre totalmente o perí
      *             'odo de vigência.'   TO MSG1-TX-ERRO-TS
      *             SET MNT-VRV      TO TRUE
      *             PERFORM 999000-ERRO
      *          END-IF
      *          IF AUX-DT-INC  <  DT-INC-VGC
      *             SET ERRO-PROGRAMADO  TO TRUE
      *             MOVE 0792            TO MSG1-CD-ERRO-TS
      *             MOVE  'Data de início do período informada é anterio
      *             'r à data de vigência da operação.'
      *                                  TO MSG1-TX-ERRO-TS
      *             SET MNT-VRV      TO TRUE
      *             PERFORM 999000-ERRO
      *          END-IF
      *       ELSE
      *          Inibido soma de 1 dia no início do periodo
      *                                                  (DT-INC-VGC-R)
      *           MOVE CORR DT-FIM-CARE-R  TO  ARG01-F05-R
      *           MOVE 1                   TO  ARG02-F05
      *            Soma  ARG03-F05 + 1
      *           MOVE 1                   TO  ARG03-F05
      *           PERFORM 232000-SOMAR-DIAS
      *           MOVE CORR ARG04-F05-R    TO AUX-DT-R

                  MOVE CORR DT-INC-VGC-R  TO AUX-DT-R

                  IF AUX-DT-INC          >  AUX-DT
                        SET ERRO-PROGRAMADO  TO TRUE
                        MOVE 0440               TO MSG1-CD-ERRO-TS
                        MOVE  'Período informado não cobre totalmente o
      -                 'período de vigência.'  TO MSG1-TX-ERRO-TS
                        SET MNT-VRV          TO TRUE
                        PERFORM 999000-ERRO
                  END-IF

                  IF AUX-DT-INC         <  AUX-DT
                        SET ERRO-PROGRAMADO  TO TRUE
                        MOVE 0792            TO MSG1-CD-ERRO-TS
                        MOVE  'Data de início do período informada é ant
      -                       'erior à data de vigência da operação.'
                                             TO MSG1-TX-ERRO-TS
                        SET MNT-VRV          TO TRUE
                        PERFORM 999000-ERRO
                  END-IF
      *       END-IF
      *    END-IF

           MOVE CORR SGTS0149-DT-FIM-PER-R (SGTS0149-QT-PER-LS)
                                       TO  AUX-DT-FIM-R

           IF AUX-DT-FIM               <  DT-FIM-VGC
                  SET ERRO-PROGRAMADO  TO TRUE
                  MOVE 0440            TO MSG1-CD-ERRO-TS
                  MOVE 'Período informado não cobre totalmente o período
      -           ' de vigência.'      TO MSG1-TX-ERRO-TS
                  SET MNT-VRV          TO TRUE
                  PERFORM 999000-ERRO
           END-IF

           PERFORM 231000-VALIDAR-PERIODOS
                    VARYING IC FROM 1 BY 1
                    UNTIL   IC GREATER SGTS0149-QT-PER-LS
           .
       230000-SAI.
           EXIT.

      *----------------------------------------------------------------*  VRS001
       231000-VALIDAR-PERIODOS       SECTION.
      *----------------------------------------------------------------*  VRS001
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO               VRS001

           MOVE CORR SGTS0149-DT-INC-PER-R (IC)
                                       TO  AUX-DT-INC-R

           MOVE CORR SGTS0149-DT-FIM-PER-R (IC)
                                       TO  AUX-DT-FIM-R

           IF AUX-DT-INC               <   DT-INC-VGC   OR
              AUX-DT-INC               >   DT-FIM-VGC
              SET ERRO-PROGRAMADO      TO TRUE
              MOVE 0443                TO MSG1-CD-ERRO-TS
              MOVE 'Data inicial fora do período de vigência.'
                                       TO MSG1-TX-ERRO-TS
              SET MNT-VRV              TO TRUE
              PERFORM 999000-ERRO
           END-IF

           IF AUX-DT-FIM               <  DT-INC-VGC   OR
              AUX-DT-FIM               >   DT-FIM-VGC
              SET ERRO-PROGRAMADO      TO TRUE
              MOVE 0444                TO MSG1-CD-ERRO-TS
              MOVE 'Data final fora do período de vigência.'
                                       TO MSG1-TX-ERRO-TS
              SET MNT-VRV              TO TRUE
              PERFORM 999000-ERRO
           END-IF

           IF AUX-DT-INC               >  AUX-DT-FIM
              SET ERRO-PROGRAMADO      TO TRUE
              MOVE 0445                TO MSG1-CD-ERRO-TS
              MOVE 'Data inicial do período maior que a data final.'
                                       TO MSG1-TX-ERRO-TS
              SET MNT-VRV              TO TRUE
              PERFORM 999000-ERRO
           END-IF

           IF SGTS0149-QT-MM-CARE > ZEROS AND
              IC > 1                      AND
              AUX-DT-INC               <=  DT-FIM-CARE-R
              SET ERRO-PROGRAMADO      TO TRUE
              MOVE 0446                TO MSG1-CD-ERRO-TS
              MOVE 'Data inicio do período sobrepõe período de carência.
      -        ' '                     TO MSG1-TX-ERRO-TS
              SET MNT-VRV              TO TRUE
              PERFORM 999000-ERRO
           END-IF

           IF  IC >  1
      *      Calcular  período anterior
                SUBTRACT 1 FROM IC  GIVING  IC-ANTERIOR
                  ON SIZE ERROR
                  SET ERRO-PROGRAMADO      TO TRUE
                  MOVE 0114                TO MSG1-CD-ERRO-TS
                  MOVE 'Erro no cálculo do contador de selecionados.'
                                           TO MSG1-TX-ERRO-TS

                  MOVE 'IC'                TO MSGKW999-NM-VRV-MNT
                  MOVE  IC                 TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  MOVE 'IC-ANTERIOR'       TO MSGKW999-NM-VRV-MNT
                  MOVE  IC-ANTERIOR        TO MSGKW999-VL-VRV-MNT
                  PERFORM 999003-ADICIONA-VRV-MNT

                  SET MNT-VRV              TO TRUE
                  PERFORM 999000-ERRO
                END-SUBTRACT
                MOVE CORR SGTS0149-DT-FIM-PER-R (IC-ANTERIOR)
                                       TO  AUX-DT-R

                IF AUX-DT-INC               <=   AUX-DT
                   SET ERRO-PROGRAMADO      TO TRUE
                   MOVE 0447                TO MSG1-CD-ERRO-TS
                   MOVE 'Ocorreu sobreposição de datas nos períodos info
      -                     'rmados.'       TO MSG1-TX-ERRO-TS
                   SET MNT-VRV              TO TRUE
                   PERFORM 999000-ERRO
                END-IF

      *         Calcular SGTS0149-DT-FIM-PER-R (IC-ANTERIOR) +  1

                MOVE CORR AUX-DT-R     TO  ARG01-F05-R

                MOVE 1                 TO  ARG02-F05
      *         Soma  ARG03-F05 + 1
                MOVE 1                 TO  ARG03-F05

                PERFORM 232000-SOMAR-DIAS

                MOVE CORR ARG04-F05-R  TO AUX-DT-R

                IF AUX-DT-INC          >  AUX-DT
                   SET ERRO-PROGRAMADO  TO TRUE
                   MOVE 0441            TO MSG1-CD-ERRO-TS
                   MOVE 'Considerando a carência, os períodos informados
      -            ' não cobrem totalmente o período de vigência.'
                                        TO MSG1-TX-ERRO-TS
                  SET MNT-VRV           TO TRUE
                  PERFORM 999000-ERRO
                END-IF

                IF  AUX-DT-INC         <  AUX-DT
                    SET ERRO-PROGRAMADO  TO TRUE
                    MOVE 0442            TO MSG1-CD-ERRO-TS
                    MOVE 'A data de inicio de vigência sobrepõe o períod
      -                'o de carência.'  TO MSG1-TX-ERRO-TS
                    SET MNT-VRV          TO TRUE
                    PERFORM 999000-ERRO
                END-IF
           END-IF
           .                                                              VRS001
       231000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       232000-SOMAR-DIAS             SECTION.
      *----------------------------------------------------------------*
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

           MOVE 4095                   TO  AREA-TAM.
           MOVE 'SBDATAF -WRKAREA-FIM' TO  AREA-FIM.

           CALL SBDATAF                USING FUNCAO-F05
                                       WKSBDATA
                                       ARG01-F05 ARG02-F05
                                       ARG03-F05 ARG04-F05.

      *--> Return code = 24: Data Inválida.

           IF  RETURN-CODE             NOT EQUAL ZEROS AND 24 AND 22
                                                       AND 46
               SET ERRO-PROGRAMA-ACIONADO TO TRUE
               MOVE 0051               TO MSG1-CD-ERRO-TS
               MOVE RETURN-CODE        TO MSG1-CD-RTN-PGM-ACND
               MOVE 'Erro no tratamento de datas.'
                                       TO MSG1-TX-RTN-PGM-ACND
                                          MSG1-TX-ERRO-TS
               MOVE SBDATAF            TO MSG1-NM-PGM-ACND
                                          NM-PGM
      *        Monitorar variáveis do módulo chamado
               MOVE FUNCAO-F05   TO  FUNCAO-SBDATAF

               MOVE 'Data enviada' TO MSGKW999-NM-VRV-MNT
               MOVE   AUX-DT-R     TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-ERRO
           END-IF

           IF  RETURN-CODE             EQUAL 24
               SET ERRO-PROGRAMA-ACIONADO TO TRUE
               MOVE 0049               TO MSG1-CD-ERRO-TS
               MOVE 'Data informada inválida'
                                       TO MSG1-TX-ERRO-TS
                                          MSG1-TX-RTN-PGM-ACND
               MOVE RETURN-CODE        TO MSG1-CD-RTN-PGM-ACND
               MOVE SBDATAF            TO MSG1-NM-PGM-ACND
                                          NM-PGM
      *        Monitorar variáveis do módulo chamado
               MOVE FUNCAO-F05   TO  FUNCAO-SBDATAF

               MOVE 'Data enviada' TO MSGKW999-NM-VRV-MNT
               MOVE   AUX-DT-R     TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-ERRO
           END-IF
           .
       232000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       930000-CHAMAR-SBDATA-F01                 SECTION.
      *----------------------------------------------------------------*
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

      *    Função F01 - valida data
      *    ARG02 = 4 - data DDMMAAAA

           MOVE 4095 TO AREA-TAM.
           MOVE 'SBDATAF -WRKAREA-FIM' TO AREA-FIM.

           CALL SBDATAF                USING FUNCAO-F01   WKSBDATA
                                             ARG01-F01    ARG02-F01.

      *--> Return code = 24: Data Inválida.

           IF  RETURN-CODE             NOT EQUAL ZEROS AND 24 AND 22
                                                       AND 46
               SET ERRO-PROGRAMA-ACIONADO TO TRUE
               MOVE 0051               TO MSG1-CD-ERRO-TS
               MOVE RETURN-CODE        TO MSG1-CD-RTN-PGM-ACND
               MOVE 'Erro no tratamento de datas.'
                                       TO MSG1-TX-RTN-PGM-ACND
                                          MSG1-TX-ERRO-TS
               MOVE SBDATAF            TO MSG1-NM-PGM-ACND
                                          NM-PGM
      *        Monitorar variáveis do módulo chamado
               MOVE FUNCAO-F01   TO  FUNCAO-SBDATAF
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-ERRO

           END-IF

           IF  RETURN-CODE             EQUAL 24
               SET ERRO-PROGRAMA-ACIONADO TO TRUE
               MOVE 0049               TO MSG1-CD-ERRO-TS
               MOVE 'Data informada inválida'
                                       TO MSG1-TX-ERRO-TS
                                          MSG1-TX-RTN-PGM-ACND
               MOVE RETURN-CODE        TO MSG1-CD-RTN-PGM-ACND
               MOVE SBDATAF            TO MSG1-NM-PGM-ACND
                                          NM-PGM
      *        Monitorar variáveis do módulo chamado
               MOVE FUNCAO-F01   TO  FUNCAO-SBDATAF
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-ERRO
           END-IF
           .
       930000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       969000-MONITORAR-VARIAVEIS      SECTION.                           VRS001
      *----------------------------------------------------------------*  VRS001
      * Monitora as variáveis dos módulos chamados                        VRS001
      *----------------------------------------------------------------*  VRS001
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO               VRS001
                                                                          VRS001
           EVALUATE NM-PGM                                                VRS001
                                                                          VRS001
               WHEN SBDATAF
                    IF  FUNCAO-SBDATAF = FUNCAO-F01
                        MOVE 'FUNCAO-F01' TO MSGKW999-NM-VRV-MNT          VRS001
                        MOVE FUNCAO-F01   TO MSGKW999-VL-VRV-MNT
                        PERFORM 999003-ADICIONA-VRV-MNT

                        MOVE 'ARG01-F01-MSG'  TO MSGKW999-NM-VRV-MNT          VR
                        MOVE ARG01-F01-MSG    TO MSGKW999-VL-VRV-MNT
                        PERFORM 999003-ADICIONA-VRV-MNT

                        MOVE 'ARG02-F01'  TO MSGKW999-NM-VRV-MNT          VRS001
                        MOVE ARG02-F01    TO MSGKW999-VL-VRV-MNT
                        PERFORM 999003-ADICIONA-VRV-MNT
                    END-IF

                    IF  FUNCAO-SBDATAF = FUNCAO-F05
                        MOVE 'FUNCAO-F05' TO MSGKW999-NM-VRV-MNT         VRS001
                        MOVE FUNCAO-F05   TO MSGKW999-VL-VRV-MNT
                        PERFORM 999003-ADICIONA-VRV-MNT

                        MOVE 'ARG01-F05'  TO MSGKW999-NM-VRV-MNT         VRS001
                        MOVE ARG01-F05    TO MSGKW999-VL-VRV-MNT
                        PERFORM 999003-ADICIONA-VRV-MNT

                        MOVE 'ARG02-F05'  TO MSGKW999-NM-VRV-MNT         VRS001
                        MOVE ARG02-F05    TO MSGKW999-VL-VRV-MNT
                        PERFORM 999003-ADICIONA-VRV-MNT

                        MOVE 'ARG03-F05'  TO MSGKW999-NM-VRV-MNT         VRS001
                        MOVE ARG03-F05    TO MSGKW999-VL-VRV-MNT
                        PERFORM 999003-ADICIONA-VRV-MNT

                        MOVE 'ARG04-F05'  TO MSGKW999-NM-VRV-MNT         VRS001
                        MOVE ARG04-F05    TO MSGKW999-VL-VRV-MNT
                        PERFORM 999003-ADICIONA-VRV-MNT
                    END-IF

                    IF  FUNCAO-SBDATAF = FUNCAO-F26
                        MOVE 'FUNCAO-F26'   TO MSGKW999-NM-VRV-MNT       VRS001
                        MOVE FUNCAO-F26     TO MSGKW999-VL-VRV-MNT
                        PERFORM 999003-ADICIONA-VRV-MNT

                        MOVE 'ARG01-F26 '   TO MSGKW999-NM-VRV-MNT       VRS001
                        MOVE ARG01-F26      TO MSGKW999-VL-VRV-MNT
                        PERFORM 999003-ADICIONA-VRV-MNT

                        MOVE 'ARG02-F26'    TO MSGKW999-NM-VRV-MNT       VRS001
                        MOVE ARG02-F26      TO MSGKW999-VL-VRV-MNT
                        PERFORM 999003-ADICIONA-VRV-MNT

                        MOVE 'ARG03-F26'    TO MSGKW999-NM-VRV-MNT       VRS001
                        MOVE ARG03-F26      TO MSGKW999-VL-VRV-MNT
                        PERFORM 999003-ADICIONA-VRV-MNT
                    END-IF

               WHEN OTHER                                                 VRS001
                    CONTINUE                                              VRS001
           END-EVALUATE                                                   VRS001
           .                                                              VRS001
       969000-SAI.                                                        VRS001
           EXIT.                                                          VRS001

      *----------------------------------------------------------------*  VRS001
      *                                                                *  VRS001
      * SECTION DA FAIXA DE 970000 A 979900 PARA LEITURA DE DADOS      *  VRS001
      *                                                                *  VRS001
      *----------------------------------------------------------------*  VRS001
       970000-LER-CANAL                SECTION.                           VRS001
      *    Obtem o canal corrente                                         VRS001
      *----------------------------------------------------------------*  VRS001
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO               VRS001
                                                                          VRS001
      *Por causa da captura automática de erro CICS, deve ser previsto    VRS001
      *o erro de container                                                VRS001
           SET  ERRO-CANAL             TO TRUE                            VRS001
                                                                          VRS001
           EXEC CICS ASSIGN                                               VRS001
                     CHANNEL ( NM-CNL )                                   VRS001
           END-EXEC                                                       VRS001
                                                                          VRS001
      *    Ao chegar aqui, há certeza de que não houve erro algum         VRS001
           SET  SEM-ERRO               TO TRUE                            VRS001
           .                                                              VRS001
       970000-FIM.                                                        VRS001
           EXIT.
                                                                          VRS001
      *----------------------------------------------------------------*  VRS001
       971000-LER-REQUISICAO           SECTION.                           VRS001
      *----------------------------------------------------------------*  VRS001
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO               VRS001
                                                                          VRS001
      *    Monta o nome do container de requisição                        VRS001
           MOVE 'SGTS0149'             TO NM-PGM                          VRS001
           MOVE 'SGTS0149-RQSC'        TO NM-CTNR                         VRS001
                                                                          VRS001
      *    Recupera o tamanho do container de requisição                  VRS001
           PERFORM 973100-LER-CONTAINER-NODATA                            VRS001
                                                                          VRS001
      * Caso a requisição seja variável (cláusula DEPENDING ON), o        VRS001
      * tratamento abaixo não deve ser efetuado, senão, ocasionará erro   VRS001
                                                                          VRS001
           MOVE 0011                   TO MSG1-CD-SEQL-ERRO               VRS001
                                                                          VRS001
      *Configura dados para leitura do container de requisição            VRS001
           SET  GET-CONTAINER          TO TRUE                            VRS001
                                                                          VRS001
      *Por causa da captura automática de erro CICS, deve ser previsto    VRS001
      *o erro de container                                                VRS001
           SET  ERRO-CONTAINER         TO TRUE                            VRS001
                                                                          VRS001
      *    Efetua a leitura do container                                  VRS001
           EXEC CICS GET                                                  VRS001
                     CONTAINER (NM-CTNR      )                            VRS001
                     FLENGTH   (QT-CCT-LIDO  )                            VRS001
                     INTO      (SGTS0149-RQSC)                            VRS001
           END-EXEC                                                       VRS001
                                                                          VRS001
      *Ao chegar aqui, há certeza de que não houve erro algum             VRS001
           SET  SEM-ERRO               TO TRUE                            VRS001
           .                                                              VRS001
       971000-SAI.                                                        VRS001
           EXIT.

      *----------------------------------------------------------------*
       973100-LER-CONTAINER-NODATA     SECTION.                           VRS001
      *----------------------------------------------------------------*  VRS001
           MOVE 0012                   TO MSG1-CD-SEQL-ERRO               VRS001
                                                                          VRS001
      *Configura dados para leitura do container de requisição            VRS001
           SET  GET-CONTAINER          TO TRUE                            VRS001
                                                                          VRS001
      *Por causa da captura automática de erro CICS, deve ser previsto    VRS001
      *o erro de container                                                VRS001
           SET  ERRO-CONTAINER         TO TRUE                            VRS001
                                                                          VRS001
      *    Obtem a quantidade de caracteres no container                  VRS001
           MOVE ZEROS                  TO QT-CCT-LIDO                     VRS001
           EXEC CICS GET                                                  VRS001
                     CONTAINER (NM-CTNR    )                              VRS001
                     NODATA                                               VRS001
                     FLENGTH   (QT-CCT-LIDO)                              VRS001
           END-EXEC                                                       VRS001
                                                                          VRS001
      *    Ao chegar aqui, há certeza de que não houve erro algum         VRS001
           SET  SEM-ERRO               TO TRUE                            VRS001
           .                                                              VRS001
       973100-SAI.                                                        VRS001
           EXIT.                                                          VRS001

      *----------------------------------------------------------------*  VRS001
      *                                                                *  VRS001
      * SECTION DA FAIXA DE 980000 A 989900 PARA GRAVACAO DE DADOS     *  VRS001
      *                                                                *  VRS001
      *----------------------------------------------------------------*  VRS001
       980000-GRAVAR-CONTAINER-RPST    SECTION.                           VRS001
      *----------------------------------------------------------------*  VRS001
                                                                          VRS001
      *    Exclui eventual container de erro de outros programas          VRS001
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO

           MOVE  'Validado com sucesso.' TO SGTS0149-TX-RTN-PRCT
                                                                          VRS001
           MOVE 0013                   TO MSG1-CD-SEQL-ERRO               VRS001
                                                                          VRS001
      *    Monta o nome do container de resposta                          VRS001
           MOVE 'SGTS0149-RPST'        TO NM-CTNR                         VRS001
           SET PUT-CONTAINER           TO TRUE                            VRS001
                                                                          VRS001
      *Por causa da captura automática de erro CICS, deve ser previsto    VRS001
      *o erro de container                                                VRS001
           SET  ERRO-CONTAINER         TO TRUE                            VRS001
                                                                          VRS001
           EXEC CICS PUT                                                  VRS001
                     CONTAINER (NM-CTNR                )                  VRS001
                     FROM      (SGTS0149-RPST          )                  VRS001
                     FLENGTH   (LENGTH OF SGTS0149-RPST)                  VRS001
           END-EXEC                                                       VRS001
                                                                          VRS001
      *Ao chegar aqui, há certeza de que não houve erro algum             VRS001
           SET  SEM-ERRO               TO TRUE                            VRS001
           .                                                              VRS001
       980000-SAI.                                                        VRS001
           EXIT.                                                          VRS001
                                                                          VRS001
      *----------------------------------------------------------------*  VRS001
       983000-EXCLUIR-RESPOSTA-ERRO    SECTION.                           VRS001
      *----------------------------------------------------------------*  VRS001
      * Exclui o container de resposta e de erro para evitar problemas *  VRS001
      * em módulos executados mais de uma vez dentro da mesma sessão do*  VRS001
      * CICS. Equivale ao initialize na área de resposta dos programas *  VRS001
      * que utilizam COMMAREA.                                         *  VRS001
      *----------------------------------------------------------------*  VRS001
                                                                          VRS001
           MOVE 0014                   TO MSG1-CD-SEQL-ERRO               VRS001
                                                                          VRS001
           MOVE NM-PGM        TO NM-CTNR(1:8)
           MOVE '-RPST'       TO NM-CTNR(9:5)
                                                                          VRS00
           SET DEL-CONTAINER           TO TRUE                            VRS001
                                                                          VRS001
      *    Exclui o container de resposta                                 VRS001
      *    Não há captura automática de erro CICS por usar RESP e
      *    RESP2   VRS001
           EXEC CICS DELETE                                               VRS001
                     CONTAINER (NM-CTNR)                                  VRS001
                     RESP      (MSG1-RESP1)                               VRS001
                     RESP2     (MSG1-RESP2)                               VRS001
           END-EXEC                                                       VRS001
                                                                          VRS001
           EVALUATE TRUE                                                  VRS001
               WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)                VRS001
               WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)                      VRS001
                    CONTINUE                                              VRS001
               WHEN OTHER                                                 VRS001
                    SET ERRO-CONTAINER TO TRUE                            VRS001
                    PERFORM 999002-TRATA-CONDITION-CICS                   VRS001
           END-EVALUATE                                                   VRS001
                                                                          VRS001
      *    Excluir o container de erro                                    VRS001
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO                          VRS001
           .                                                              VRS001
       983000-SAI.                                                        VRS001
           EXIT.                                                          VRS001
      *                                                                   VRS001
      *----------------------------------------------------------------*  VRS001
       983100-EXCLUIR-CONTAINER-ERRO   SECTION.                           VRS001
      *----------------------------------------------------------------*  VRS001
      *    Limpar variáveis container de erro                             VRS001
           INITIALIZE IIBKERRO-PRM-ERRO                                   VRS001
                                                                          VRS001
      *    Exclui o container de erro                                     VRS001
           MOVE 0015                   TO MSG1-CD-SEQL-ERRO               VRS001
           MOVE 'PRM-ERRO'             TO NM-CTNR                         VRS001
           SET DEL-CONTAINER           TO TRUE                            VRS001
                                                                          VRS001
      *    Não há captura automática de erro CICS por usar RESP e
      *    RESP2   VRS001
           EXEC CICS DELETE                                               VRS001
                     CONTAINER (NM-CTNR)                                  VRS001
                     RESP      (MSG1-RESP1)                               VRS001
                     RESP2     (MSG1-RESP2)                               VRS001
           END-EXEC                                                       VRS001
                                                                          VRS001
           EVALUATE TRUE                                                  VRS001
               WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)                VRS001
               WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)                      VRS001
                    CONTINUE                                              VRS001
               WHEN OTHER                                                 VRS001
                    SET ERRO-CONTAINER TO TRUE                            VRS001
                    PERFORM 999002-TRATA-CONDITION-CICS                   VRS001
           END-EVALUATE                                                   VRS001
           .                                                              VRS001
       983100-SAI.                                                        VRS001
           EXIT.
                                                                          VRS001
      *----------------------------------------------------------------*  VRS001
       984000-GRAVAR-CONTAINER-ERRO    SECTION.                           VRS001
      *----------------------------------------------------------------*  VRS001
           MOVE 0016                   TO MSG1-CD-SEQL-ERRO               VRS001
                                                                          VRS001
      *    Grava o erro gerado no container de erro                       VRS001
      *    Não há captura automática de erro CICS por usar RESP e
      *    RESP2   VRS001
           EXEC CICS PUT                                                  VRS001
                     CONTAINER ( 'PRM-ERRO'                  )            VRS001
                     FROM      ( IIBKERRO-PRM-ERRO           )            VRS001
                     FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )            VRS001
                     RESP      (MSG1-RESP1                   )            VRS001
                     RESP2     (MSG1-RESP2                   )            VRS001
           END-EXEC                                                       VRS001
           .                                                              VRS001
       984000-SAI.                                                        VRS001
           EXIT.                                                          VRS001
                                                                          VRS001
      *----------------------------------------------------------------*  VRS001
      *                                                                *  VRS001
      * As sections abaixo iniciadas com 999000 representam os tratamen*  VRS001
      * tos de erro utilizando o Sistema MSG (rotina MSGS0001)         *  VRS001
      *                                                                *  VRS001
      *----------------------------------------------------------------*  VRS001
       999000-ERRO                     SECTION.                           VRS001
      *----------------------------------------------------------------*  VRS001
                                                                          VRS001
           IF  MSG1-CD-ERRO-TS EQUAL ZEROS                                VRS001
               MOVE 8888                TO MSG1-CD-ERRO-TS                VRS001
           END-IF
                                                                          VRS001
           IF  MSG1-TX-ERRO-TS EQUAL SPACES                               VRS001
               MOVE 'Erro não especificado no módulo.'
                                        TO MSG1-TX-ERRO-TS                VRS001
           END-IF                                                         VRS001
           PERFORM 999000-GRAVA-ERRO-E-ENCERRA                            VRS001
           .                                                              VRS001
       999000-ERRO-SAI.                                                   VRS001
           EXIT.
                                                                          VRS001
      *----------------------------------------------------------------*  VRS001
      *     As sections abaixo representam os tratamentos de erro         VRS001
      *    utilizando o sistema MSG de mensageria (rotina MSGS0001)       VRS001
      *----------------------------------------------------------------*  VRS001
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.                           VRS001
      *----------------------------------------------------------------*  VRS001
      *    Em caso de erro, desfaz as atualizações em banco de dados      VRS001
                                                                          VRS001
           EXEC CICS SYNCPOINT                                            VRS00
                     ROLLBACK                                             VRS001
           END-EXEC                                                       VRS001
                                                                          VRS001
      *Verifica se o tipo de erro gerado possui tratamento interno        VRS001
           EVALUATE TRUE                                                  VRS001
               WHEN ERRO-CONTAINER                                        VRS001
                    PERFORM 999004-TRATA-ERRO-CTNR                        VRS001
               WHEN ERRO-TAMANHO-AREA                                     VRS001
                    PERFORM 999005-TRATA-ERRO-AREA-DADOS                  VRS001
               WHEN ERRO-CANAL                                            VRS001
                    PERFORM 999006-TRATA-ERRO-CANAL                       VRS001
               WHEN OTHER                                                 VRS001
                    CONTINUE                                              VRS001
           END-EVALUATE                                                   VRS001
                                                                          VRS001
      *Adiciona as variáveis de entrada para monitoração                  VRS001
           IF  MNT-VRV                                                    VRS001
               MOVE 'SGTS0149-DT-INC-VGC'        TO MSGKW999-NM-VRV-MNT   VRS001
               MOVE  SGTS0149-DT-INC-VGC         TO MSGKW999-VL-VRV-MNT   VRS001
               PERFORM 999003-ADICIONA-VRV-MNT                            VRS001
                                                                          VRS001
               MOVE 'SGTS0149-DT-FIM-VGC'        TO MSGKW999-NM-VRV-MNT   VRS001
               MOVE SGTS0149-DT-FIM-VGC          TO MSGKW999-VL-VRV-MNT   VRS001
               PERFORM 999003-ADICIONA-VRV-MNT                            VRS001
                                                                          VRS001
               MOVE 'SGTS0149-QT-MM-CARE'        TO MSGKW999-NM-VRV-MNT   VRS001
               MOVE SGTS0149-QT-MM-CARE          TO MSGKW999-VL-VRV-MNT   VRS001
               PERFORM 999003-ADICIONA-VRV-MNT                            VRS001

               MOVE 'SGTS0149-QT-PER-LS'         TO MSGKW999-NM-VRV-MNT   VRS001
               MOVE SGTS0149-QT-PER-LS           TO MSGKW999-VL-VRV-MNT   VRS001
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0149-DT-INC-PER (1)'    TO MSGKW999-NM-VRV-MNT   VRS001
               MOVE SGTS0149-DT-INC-PER (1)      TO MSGKW999-VL-VRV-MNT   VRS001
               PERFORM 999003-ADICIONA-VRV-MNT

               IF IC > ZEROS AND IC < SGTS0149-QT-PER-LS
                    MOVE 'INDEXADOR'             TO MSGKW999-NM-VRV-MNT   VRS001
                    MOVE IC                      TO MSGKW999-VL-VRV-MNT   VRS001
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTS0149-DT-INC-PER(INDEXADOR)'
                                                 TO MSGKW999-NM-VRV-MNT   VRS001
                    MOVE SGTS0149-DT-INC-PER(IC) TO MSGKW999-NM-VRV-MNT   VRS001
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTS0149-DT-FIM-PER(INDEXADOR)'
                                                 TO MSGKW999-NM-VRV-MNT   VRS001
                    MOVE SGTS0149-DT-FIM-PER(IC) TO MSGKW999-NM-VRV-MNT   VRS001
                    PERFORM 999003-ADICIONA-VRV-MNT
               END-IF
           END-IF
                                                                          VRS001
      *    Grava o erro gerado no sistema MSG - Mensageria                VRS001
           PERFORM MSG1-NTF-MSG                                           VRS001
                                                                          VRS001
           MOVE MSG1-CD-ERRO-TS        TO IIBKERRO-CD-ERRO                VRS001
           MOVE MSG1-TX-ERRO-TS        TO IIBKERRO-TX-ERRO                VRS001

      *    Em caso de abend da MSGS001, adiciona o nome da rotina,
      *    código do erro, retornos cics e nível execução
           IF  MSG1-CD-ERRO NOT EQUAL ZEROS
               STRING
                 '(MSG1/' MSG1-CD-ERRO '/' MSG1-RET-RESP '/'
                 MSG1-RET-RESP2  '/' MSG1-NVL-EXEC ') '
                 MSG1-TX-ERRO-TS DELIMITED BY SIZE INTO IIBKERRO-TX-ERRO
               END-STRING
           END-IF
                                                                          VRS001
      *    Grava o erro gerado no container de erro                       VRS001
           PERFORM 984000-GRAVAR-CONTAINER-ERRO                           VRS001
                                                                          VRS001
      *    Encerra a transação                                            VRS001
           PERFORM 999999-ENCERRAR                                        VRS001
           .                                                              VRS001
       999000-SAI.                                                        VRS001
           EXIT.
                                                                          VRS001
      * --------------------------------------*                           VRS001
      * 999001-TRATA-ABEND-CICS        SECTION.                           VRS001
      * 999002-TRATA-CONDITION-CICS    SECTION.                           VRS001
      * 999003-ADICIONA-VRV-MNT        SECTION.
      * 999004-ALINHA-NUMERO-ESQUERDA  SECTION.                           VRS001
      * --------------------------------------*                           VRS001
-INC HLPKP999
      * --------------------------------------*                           VRS001
      * 999004-TRATA-ERRO-CTNR         SECTION.                           VRS001
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.                           VRS001
      * 999006-TRATA-ERRO-CANAL        SECTION.                           VRS001
      *---------------------------------------*                           VRS001
-INC SGTKP999
      * --------------------------------------*                           VRS001
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
      * MSG1-GET-CTNR-ERRO             SECTION.                           VRS001
      * --------------------------------------*                           VRS001
-INC MSGKPMG1
      *----------------------------------------------------------------*  VRS001
       999999-ENCERRAR                 SECTION.                           VRS001
      *----------------------------------------------------------------*  VRS001
           EXEC CICS                                                      VRS001
                RETURN                                                    VRS001
           END-EXEC                                                       VRS001
           .                                                              VRS001
       999999-SAI.                                                        VRS001
           EXIT.
      *---------------------- FIM SGTS0149 ----------------------------*