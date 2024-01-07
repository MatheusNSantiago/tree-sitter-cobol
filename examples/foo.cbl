      *================================================================*
      * PROGRAMA: ACHPC060
      *
      * AUTOR   : F7023235 Matheus Santiago.
      *
      * COMPILACAO: 54 - PSOSE600 - Cobol 6.3 c/otimizacao p/producao
      * OBJETIVO..: Gerar arquivo ACH610 diário com dados cadastrais de
      * conta corrente e cheque especial.
      *
      *================================================================*
      *
      * VRS001 08/11/2023 F7023235 Implantacao - Tarefa 1168765.
      *
      *================================================================*

      *************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACHPC060.
       AUTHOR. F7023235.
      *************************
      *
      **********************
       ENVIRONMENT DIVISION.
      **********************
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *
       INPUT-OUTPUT SECTION.
      *
       FILE-CONTROL.
      *
           SELECT ACH601E ASSIGN TO UT-S-ACH601E.
           SELECT DEB610E ASSIGN TO UT-S-DEB610E.
      *
           SELECT ACH610S ASSIGN TO UT-S-ACH610S.
      *
      *
      ***************
       DATA DIVISION.
      ***************
      *
       FILE SECTION.
      *----------------------------------------------------------------
      *
       FD  DEB610E
           BLOCK  0
           RECORD 1200
           RECORDING F.
      *
       01  REGISTRO-DEB610E            PIC X(1200).
      *
       FD  ACH601E
           BLOCK  0
           RECORD 50
           RECORDING F.
      *
       01  REGISTRO-ACH601E            PIC  X(050).
      *
       FD  ACH610S
           BLOCK  0
           RECORD 1200
           RECORDING F.
      *
       01  REGISTRO-ACH610S            PIC X(1200).
      *
      *----------------------------------------------------------------
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------
      *
      *---------------------- C O N S T A N T E S ---------------------*
      *
       77  CTE-PROG                    PIC  X(016)
                                       VALUE '*** ACHPC060 ***'.
       77  CTE-VERS                    PIC  X(06) VALUE 'VRS001'.
       77  SBVERSAO                    PIC  X(08) VALUE 'SBVERSAO'.
       77  SBABEND                     PIC  X(07) VALUE 'SBABEND'.
       77  DEBSB014                    PIC  X(08) VALUE 'DEBSB014'.
       77  SBDATAF                     PIC  X(08) VALUE 'SBDATAF'.
       77  SBDATAF-FUNCAO              PIC  X(03) VALUE 'F08'.
      *
      *------------------------ G U A R D A S -------------------------*
      *
       77  GDA-DT-AUX                  PIC  9(08).
      *
       77  GDA-DEB610-DT-ANT           PIC  9(08).
       77  GDA-DEB610-DT-ATU           PIC  9(08).
       77  GDA-DEB610-DT-FUT           PIC  9(08).
      *
       01  AREAWORK                    PIC  X(4095) VALUE SPACES.
       01  FILLER REDEFINES AREAWORK.
           03  A-TAM    PIC 9(0008) COMP.
           03  FILLER   PIC X(4071).
           03  A-FIM    PIC X(0020).
      *
      *------------------------- B O O K S ----------------------------*
      *
       COPY DEBK610.
       COPY ACHK601.
      *
      *----------------------- T A B E L A S --------------------------*
      *
       COPY DEBKS014.
      *
      *--------------------- C O N T A D O R E S ----------------------*
      *
       77  CNT-QT-REGS                 PIC 9(09) VALUE 0.
      *
      *-------------------- I N D I C A D O R E S ---------------------*
      *
       77  IND-FIM-610E                PIC 9 VALUE 0.
       77  IND-HEADER-VALIDA           PIC 9 VALUE 0.
       77  IND-CONTA-VALIDA            PIC 9 VALUE 0.
       77  IND-RAZAO-ENCONTRADA        PIC 9 VALUE 0.
      *
      ********************
       PROCEDURE DIVISION.
      ********************
      *
      *----------------------------------------
       000000-INICIO                   SECTION.
      *----------------------------------------*
      *
      *    CALL SBVERSAO USING CTE-PROG CTE-VERS.
      *
           PERFORM 100000-PROC-INICIAIS.
      *
           PERFORM 200000-PROCESSA.
      *
           PERFORM 300000-PROC-FINAIS.
      *
           STOP RUN.
      *
      *----------------------------------------
       100000-PROC-INICIAIS            SECTION.
      *----------------------------------------
      *
       100001-ABRE-ARQUIVOS.
           OPEN INPUT  DEB610E
                INPUT  ACH601E
                OUTPUT ACH610S.
      *
       100002-CARREGA-TABELA.
           CALL DEBSB014 USING TABELA-RAZOES TABELA-CHEQUE-OURO
                               TABELA-MODALIDADES.

       100003-LE-LIDER-601.
           READ ACH601E INTO 601-REG-GERAL
                *> O ACH601 só tem uma entrada. Já que chamamos o read
                *> uma única vez, ele deve retornar o registro
                AT END PERFORM 990003-ERRO-03
           END-READ.

           IF  RETURN-CODE NOT EQUAL 0
               PERFORM 990005-ERRO-05
           END-IF.
      *
       100099-SAI-PROC-INICIAIS.
           EXIT.
      *
      *----------------------------------------
       200000-PROCESSA                 SECTION.
      *----------------------------------------
      *
       200001-GRAVAR-HEADER-ACH610.
           PERFORM 210000-LE-DEB610.

           IF AGENCIA-610 NOT EQUAL ZEROS
                PERFORM 990001-ERRO-01
           END-IF.

           PERFORM 210000-ESCREVE-ACH610S.
      *
       200002-LOOP-PRINCIPAL.
           PERFORM UNTIL IND-FIM-610E = 1
                IF AGENCIA-610 EQUAL ZEROS *> é header
                    PERFORM 230000-VALIDAR-HEADER
                END-IF

                PERFORM 240000-VALIDAR-CONTA

                IF IND-CONTA-VALIDA = 1
                    PERFORM 220000-BUSCAR-RAZAO

                    IF IND-RAZAO-ENCONTRADA = 1
                        ADD 1 TO CNT-QT-REGS
                        PERFORM 210000-ESCREVE-ACH610S
                    END-IF
                END-IF

                PERFORM 210000-LE-DEB610
           END-PERFORM.
      *
       200099-SAI-PROCESSA.
           EXIT.
      *
      *----------------------------------------
       210000-LE-DEB610                SECTION.
      *----------------------------------------
      *
           READ DEB610E INTO REG610
                AT END
                    MOVE 1 TO IND-FIM-610E
                    GO TO 210099-SAI-LE-DEB610
           END-READ.
      *
       210099-SAI-LE-DEB610.
           EXIT.
      *
      *----------------------------------------
       210000-ESCREVE-ACH610S          SECTION.
      *----------------------------------------
      *
           WRITE REGISTRO-ACH610S FROM REG610.
      *
       210099-SAI-ESCREVE-ACH610S.
           EXIT.
      *----------------------------------------
       220000-BUSCAR-RAZAO             SECTION.
      *----------------------------------------
      *
           MOVE 0 TO IND-RAZAO-ENCONTRADA

           SET IX-MDLD TO 1.
           SEARCH ALL TAB-MODALIDADES
                WHEN TAB-MDLD-RAZAO(IX-MDLD) EQUAL TIT-RAZAO-ATU-610
                    MOVE 1 TO IND-RAZAO-ENCONTRADA
           END-SEARCH.
      *
       220099-SAI-BUSCAR-RAZAO.
           EXIT.
      *
      *----------------------------------------
       230000-VALIDAR-HEADER           SECTION.
      *----------------------------------------
      *
           MOVE 0 TO IND-HEADER-VALIDA.
      *
           PERFORM 230100-TRATAR-DATAS-610.
      *
       230001-CHECA-DATAS-IGUAIS.
           IF (GDA-DEB610-DT-ANT = 601-DT-AMD-ANT)  AND
              (GDA-DEB610-DT-ATU = 601-DT-AMD-ATU)  AND
              (GDA-DEB610-DT-FUT = 601-DT-AMD-FUT)
                 MOVE 1 TO IND-HEADER-VALIDA
           END-IF.
      *
       230002-ASSERT.
           IF IND-HEADER-VALIDA = 0 THEN
              PERFORM 990004-ERRO-04
           END-IF.
      *
       230099-SAI-VALIDAR-HEADER.
           EXIT.
      *
      *----------------------------------------
       230100-TRATAR-DATAS-610         SECTION.
      *----------------------------------------
      *Muda a data  DEB610 são em DDMMAAAAA
      *----------------------------------------
      *
       230101-TRATA-610-DT-ANTERIOR.
           MOVE DATA-ANTERIOR-DDMMAAAA-610 TO GDA-DT-AUX.
           PERFORM 290000-TRANSFORMAR-DMA-EM-AMD.
           MOVE GDA-DT-AUX TO GDA-DEB610-DT-ANT .

       230102-TRATA-610-DT-ATU.
           MOVE DATA-ATUAL-DDMMAAAA-610    TO GDA-DT-AUX.
           PERFORM 290000-TRANSFORMAR-DMA-EM-AMD.
           MOVE GDA-DT-AUX TO GDA-DEB610-DT-ATU .

       230103-TRATA-610-DT-FUT.
           MOVE DATA-FUTURA-DDMMAAAA-610   TO GDA-DT-AUX.
           PERFORM 290000-TRANSFORMAR-DMA-EM-AMD.
           MOVE GDA-DT-AUX TO GDA-DEB610-DT-FUT .
      *
       230199-SAI-TRATAR-DATAS-610.
           EXIT.
      *
      *----------------------------------------
       240000-VALIDAR-CONTA            SECTION.
      *----------------------------------------
      *
           MOVE 0 TO IND-CONTA-VALIDA.

           IF (AGENCIA-610 NOT EQUAL 0) *> não é header
               AND (AGENCIA-610 NOT EQUAL +99999) *> não é trailer
               AND (CONTA-610 NOT EQUAL 0) *> não é conta setex
                    MOVE 1 TO IND-CONTA-VALIDA
           END-IF.
      *
       240099-SAI-VALIDAR-CONTA.
           EXIT.
      *
      *----------------------------------------
       290000-TRANSFORMAR-DMA-EM-AMD   SECTION.
      *----------------------------------------
      *
       290001-SETAR-AREA-WORK.
      *--- A AREA-WORK é um parâmetro do SBDATAF
      *--- Tirei os valores direto da documentação no SOS
           MOVE 4095 TO A-TAM.
           MOVE 'SBDATAF -WRKAREA-FIM' TO A-FIM.

       290002-CHAMAR-SBDATAF.
      *    CALL SBDATAF USING  SBDATAF-FUNCAO
      *                        AREAWORK
      *                        GDA-DT-AUX.

           IF RETURN-CODE NOT EQUAL TO 0
              PERFORM 990006-ERRO-06
           END-IF.

       290099-SAI.
           EXIT.
      *
      *----------------------------------------
       300000-PROC-FINAIS              SECTION.
      *----------------------------------------
      *
       300001-ESCREVE-TRAILER.
           PERFORM 210000-ESCREVE-ACH610S.

       300002-FECHA-ARQUIVOS.
           CLOSE DEB610E ACH601E ACH610S.
      *
       300099-SAI.
           DISPLAY "QUANTIDADE DE REGISTROS: " CNT-QT-REGS.
           EXIT.
      *
      *---------------------
       990000-ERROS SECTION.
      *---------------------
      *
       990001-ERRO-01.
           DISPLAY '888 ' CTE-PROG  ' 001 - PRIMEIRO RECORD DO DEB610'
                                    ' NÃO É UMA HEADER'.
           PERFORM 999000-ABENDA.
      *
       990002-ERRO-02.
           DISPLAY '888 ' CTE-PROG  ' 002 - FALHA AO CARREGAR A TABELA'
                                    ' GERADA PELO DEBSB014'.
           PERFORM 999000-ABENDA.
      *
       990003-ERRO-03.
           DISPLAY '888 ' CTE-PROG ' 003 - O ARQUIVO ACH601 ESTA VAZIO'.
           PERFORM 999000-ABENDA.
      *
       990004-ERRO-04.
           DISPLAY '888 ' CTE-PROG  ' 004 - DATAS DO ARQUIVO LIDER NAO'
                                    ' SAO IGUAIS'.
           DISPLAY '888 ' CTE-PROG  ' 004 - DATAS ACH601 (em COMP3) = {'
              'ANTERIOR: ' 601-DT-AMD-ANT ', '
              'ATUAL: '    601-DT-AMD-ATU ', '
              'FUTURO: '   601-DT-AMD-FUT '}'.

           DISPLAY '888 ' CTE-PROG  ' 004 - DATAS DEB610 = {'
              'ANTERIOR: ' GDA-DEB610-DT-ANT ', '
              'ATUAL: '    GDA-DEB610-DT-ATU ', '
              'FUTURO: '   GDA-DEB610-DT-FUT '}'.

           PERFORM 999000-ABENDA.
      *
       990005-ERRO-05.
           DISPLAY '888 ' CTE-PROG ' 005 - ERRO NA CHAMADA DA DEBSB014'.
           DISPLAY '888 ' CTE-PROG ' 005 - RETURN-CODE = ' RETURN-CODE.
           PERFORM 999000-ABENDA.
      *
       990006-ERRO-06.
           DISPLAY '888 ' CTE-PROG ' 006 - ERRO NA SUBROTINA SBDATAF'.
           DISPLAY '888 ' CTE-PROG ' 006 - RETURN-CODE = ' RETURN-CODE.
           PERFORM 999000-ABENDA.
      *
      *----------------------
       999000-ABENDA SECTION.
      *----------------------
      *
           DISPLAY '888 ' CTE-PROG ' 888 - CANCELADO'.
      *    CALL SBABEND.
      *
       999099-SAI-ABENDA.
           EXIT.

      *====================== FIM ACHPC060 ============================*
