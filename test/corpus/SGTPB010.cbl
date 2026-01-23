       CBL ARITH(EXTEND)
      *-----------------------------------------------------------------
      * PROGRAMA..: SGTPB010
      * ANALISTA..: CINTIA IGLESIAS
      * AUTOR.....: CINTIA IGLESIAS
      * DATA......: 27/06/2016
      * OBJETIVO..: Programa batch para receber arquivo de posição
      *             diária do titulo oriundo do sistema Drive
      * COMPILACAO: COBOL BATCH
      *-----------------------------------------------------------------
      * VRS013 26/06/2024 - F2054109 - Tratamento para não processar
      *                     carteiras migradas para o CAC
      * VRS012 23/03/2022 - F9158937 - Acertar HR-IMP-PSC-SDO para Ope-
      *        rações Encerradas quando houver quebra de carteira.
      * VRS011 09/10/2020 - F0950413 - Encerrar o pgm com return code 6,
      *        ao validar a Data de Emissão do  primeiro registro lido,
      *        sendo esta igual ou maior que a data hoje, e atualizar a
      *        tabela Leader, movendo a carteira lida desse primeiro re-
      *        gistro, para as ultimas 8 posiçoes do Timestamp, para
      *        facilitar a identificação da origem das informaçoes com
      *        inconsistência.
      * VRS010 04/08/2020 - F9158937 - Gravar tabela Leader mesmo com
      *                                return-code = 4.
      * VRS009 17/07/2020 - F9158937 - A1terar Timestamp quando mudar a
      *                                carteira lida no arquivo.
      * VRS008 30/01/2020 - F2634012 - Inclusão da carteira no programa
      *                                SGTS009A.
      * VRS007 10/12/2019 F8090555 Permitir DT-APLICACAO zerada, para
      *                            aceitar papeis de DIVIDA.
      *                            Ex: USCA11DIVIDA
      * VRS006 31/05/2019 F9158937 Permitir importação anterior a D-1
      *                            Gravar tabela LDR_SIS se tudo OK
      *                            Fazer Return-Code =4 quando houver
      *                            inconsistências
      * VRS005 03/05/2019 F8785286 DE 2016/145195  -  Recompilação por
      *              alteração no book SGTKL075.
      *        26/03/2019 - F2634012 - Inclusão da chamada do programa
      *                     SGTS009A para tratamento das op. encerradas.
      * VRS004 27/02/2019 - F8090555 - Substitui CD-USU-RSP-ATL de
      *                     SISTEMA para SGTPB010.
      * VRS003 08/06/2018 - F2634012 - DE 2016/145195 - Inclusão da
      *                     CARTEIRA
      * VRS002 16/10/2017 - F2634012 - DE 2016/145195 - Exclusão da
      *        formatacao dos campos do arquivo da drive. A formata-
      *        cao do arquivo ficou no programa (sgtpb013)
      * VRS001 27/06/2016 - F8608799 - DE 2015/137233 - IMPLANTAÃAO
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------

       PROGRAM-ID. SGTPB010.

      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
      *-----------------------------------------------------------------

       CONFIGURATION SECTION.


       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT                SECTION.
      *
      * O arquivo deverá vir classificado pela carteira

       FILE-CONTROL.
           SELECT SGTF010E  ASSIGN  TO  SGTF010E.


      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------
       FILE                        SECTION.
      *
       FD  SGTF010E
           RECORDING F
           RECORD 300
           BLOCK 0.
       01  REG-FD-SGTF010E        PIC X(300).
      *

      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
      *-----------------------------------------------------------------

       77  CTE-INICIO-SS    PIC  X(40) VALUE
           '*** STORAGE SECTION COMECA AQUI ***'.
       77  CTE-PROG     PIC  X(17) VALUE '*** SGTPB010 ***'.
       77  CTE-VERS     PIC  X(6)  VALUE 'VRS013'.

      *
      * Subrotinas Utilizadas

       77  SBABEND  VALUE 'SBABEND'    PIC  X(008).
       77  SBVERSAO VALUE 'SBVERSAO'   PIC  X(008).
       77  SBDATAF  VALUE 'SBDATAF'    PIC  X(008).
       77  SGTS0090 VALUE 'SGTS0090'   PIC  X(008).
       77  SGTST066 VALUE 'SGTST066'   PIC  X(008).
       77  SGTST003 VALUE 'SGTST003'   PIC  X(008).
       77  SGTST122 VALUE 'SGTST122'   PIC  X(008).
       77  SGTSPE16 VALUE 'SGTSPE16'   PIC  X(008).
       77  SGTSPE23 VALUE 'SGTSPE23'   PIC  X(008).
       77  SGTS009A VALUE 'SGTS009A'   PIC  X(008).
       77  SGTSL137 VALUE 'SGTSL137'   PIC  X(008).

      *----------------------------------------------------------------
       LOCAL-STORAGE SECTION.
      *----------------------------------------------------------------


      * Books da definicao de dados a serem usados na subrotina de erros
-INC HLPKE001

      * HLPKDFHE - Definicao da area DFHEIBLK
-INC HLPKDFHE

-INC HLPKE01B
      * Areas de trabalho

       01 CNT-LIDOS                       PIC S9(004) COMP-5 VALUE 0.
       01 CNT-OK                          PIC S9(004) COMP-5 VALUE 0.
       01 CNT-RESSALVA                    PIC S9(004) COMP-5 VALUE 0.
       01 CNT-ERRO-INT                    PIC S9(004) COMP-5 VALUE 0.
       01 CNT-ERRO-IMP                    PIC S9(004) COMP-5 VALUE 0.
       01 CNT-INTER                       PIC S9(004) COMP-5 VALUE 0.
       01 AX-RETURN-CODE                  PIC  9(004)        VALUE 0.

       01 IN-GRAVAR                       PIC  X(01)  VALUE ' '.
          88 IN-GRAVOU                                VALUE 'S'.
          88 IN-NAO-GRAVOU                            VALUE 'N'.

       01  DT-SQL-ATU                     PIC  X(10)
                                                VALUE '  .  .    '.
       01  DT-SQL-ATU-R REDEFINES DT-SQL-ATU.
           03 DIA                         PIC  9(02).
           03 PT01                        PIC  X(01).
           03 MES                         PIC  9(02).
           03 PT02                        PIC  X(01).
           03 ANO                         PIC  9(04).

       01  HR-SQL-ATU                       PIC  X(15)
                                                VALUE '  .  .  .      '.
       01  HR-SQL-ATU-R REDEFINES HR-SQL-ATU.
           03 HORA                          PIC  9(02).
           03 FILLER                        PIC  X(01).
           03 MINUTO                        PIC  9(02).
           03 FILLER                        PIC  X(01).
           03 SEGUNDO                       PIC  9(02).
           03 FILLER                        PIC  X(01).
           03 MICRO-SEGUNDO                 PIC  9(06).

       01  HR-SQL-AUX                       PIC  X(15).
       01  HR-SQL-AUX-R REDEFINES HR-SQL-AUX.
           03 HORA                          PIC  9(02).
           03 FILLER                        PIC  X(01).
           03 MINUTO                        PIC  9(02).
           03 FILLER                        PIC  X(01).
           03 SEGUNDO                       PIC  9(02).
           03 FILLER                        PIC  X(01).
           03 MICRO-SEGUNDO                 PIC  9(06).


       01  DT-FMTD.
           03 DIA                        PIC  X(002)     VALUE SPACES.
           03 FILLER                     PIC  X(001)     VALUE '.'.
           03 MES                        PIC  X(002)     VALUE SPACES.
           03 FILLER                     PIC  X(001)     VALUE '.'.
           03 ANO                        PIC  X(004)     VALUE SPACES.


       01  DT-REF                        PIC  X(008).
       01  DT-REF-R    REDEFINES  DT-REF.
           03 DIA                        PIC  9(002).
           03 MES                        PIC  9(002).
           03 ANO                        PIC  9(004).

       01  DT-PSC                        PIC  X(10) VALUE ' '.
       01  DT-PSC-R REDEFINES            DT-PSC.
           07  DIA                       PIC  9(2).
           07  PT03                      PIC  X(1).
           07  MES                       PIC  9(2).
           07  PT04                      PIC  X(1).
           07  ANO                       PIC  9(4).

      *== Inicio-1 Alteração 09/10/2020
       01  DT-EMISSAO                  PIC  X(10) VALUE '0000-00-00'.
       01  DT-EMISSAO-R REDEFINES      DT-EMISSAO.
           05  ANO                     PIC  9(04).
           05  FILLER                  PIC  X(01).
           05  MES                     PIC  9(02).
           05  FILLER                  PIC  X(01).
           05  DIA                     PIC  9(02).

       01  DT-PRCT                     PIC  X(10) VALUE '0000-00-00'.
       01  DT-PRCT-R REDEFINES         DT-PRCT.
           05  ANO                     PIC  9(04).
           05  FILLER                  PIC  X(01).
           05  MES                     PIC  9(02).
           05  FILLER                  PIC  X(01).
           05  DIA                     PIC  9(02).

       01  TS-FMTD-LDR-SIS             PIC  X(26)  VALUE SPACES.
       01  TS-FMTD-LDR-SIS-R REDEFINES TS-FMTD-LDR-SIS.
               05  ANO                 PIC  9(4).
               05  FILLER              PIC  X(1).
               05  MES                 PIC  9(2).
               05  FILLER              PIC  X(1).
               05  DIA                 PIC  9(2).
               05  FILLER              PIC  X(1).
               05  HORA                PIC  9(2).
               05  FILLER              PIC  X(1).
               05  MINUTO              PIC  9(2).
               05  FILLER              PIC  X(1).
               05  SEGUNDO-CTRA        PIC  9(2).
               05  FILLER              PIC  X(1).
               05  MICRO-SEGUNDO-CTRA  PIC  9(6).
      *== Final-1  Alteração 09/10/2020

       01  IN-ERRO-VALIDACAO             PIC X(01)       VALUE SPACES.
               88  ERRO-VALIDACAO                  VALUE 'S'.

       01  IN-ERRO-CARTEIRA              PIC X(01)       VALUE SPACES.
               88  ERRO-CARTEIRA                         VALUE 'N'.

       01  MSG-ERRO-VALIDACAO            PIC X(100)      VALUE SPACES.

       01  TAB-CARTEIRA.
           03 TAB-CARTEIRA-R             OCCURS  100 TIMES.
              05 TAB-CD-CARTEIRA         PIC 9(009) VALUE ZEROS.
              05 TAB-GRAVOU              PIC X(001) VALUE SPACES.
              05 TAB-QTD-INTER           PIC S9(004) COMP-5 VALUE 0.
              05 TAB-HR-SQL-ATU          PIC  X(15)
                                            VALUE '  .  .  .      '.

      * Area da subrotina SBDATA
       01  FUNCAO                        PIC  X(3).
       01  AREA-WORK                     PIC  X(4095).
       01  AREA-WORK-RED  REDEFINES  AREA-WORK.
           03 A-TAM                      PIC  9(0008) COMP.
           03 FILLER                     PIC  X(4071).
           03 A-FIM                      PIC  X(0020).
       01  ARG01                         PIC  9(8) VALUE 0.
       01  ARG01-R REDEFINES ARG01.
           03  DIA                       PIC  9(2).
           03  MES                       PIC  9(2).
           03  ANO                       PIC  9(4).
       01  ARG02                         PIC  9(1) VALUE 0.

       01  ARG02-1                       PIC  9(2) VALUE 1.
       01  ARG02-1-R  REDEFINES ARG02-1.
           03 IND-FERIADO                PIC  9(1).
           03 IND-DIA-SEMANA             PIC  9(1).

       01  ARG02-2                       PIC  9(5) VALUE 1.
       01  ARG03                         PIC  9(1) VALUE 2.
       01  ARG04                         PIC  9(8).
      *
       01  IA-1                          PIC  9(03) VALUE 0.
       01  IB-1                          PIC  9(03) VALUE 0.

      * ?rea para recepção de dados


       01  AUX-DADOS.
         03  AUX-EDT-DT-EMS              PIC X(08) VALUE SPACES.
         03  AUX-EDT-DT-EMS-R REDEFINES AUX-EDT-DT-EMS.
            05 DIA                       PIC  9(2).
            05 MES                       PIC  9(2).
            05 ANO                       PIC  9(4).
         03  AUX-EDT-DT-APL         PIC X(08) VALUE SPACES.
         03  AUX-EDT-DT-APL-R REDEFINES AUX-EDT-DT-APL.
            05 DIA                       PIC  9(2).
            05 MES                       PIC  9(2).
            05 ANO                       PIC  9(4).
         03  AUX-EDT-CD-IMP             PIC S9(4)   COMP-5 VALUE 0.
         03  AUX-EDT-TX-DCR-ERRO-IMP    PIC X(200)  VALUE SPACES.
         03  AUX-CARTEIRA-ANT           PIC 9(09)   VALUE ZEROS.

       01    AUX-TIMESTAMP               PIC X(26).
       01    AUX-TIMESTAMP-R  REDEFINES AUX-TIMESTAMP.
             03 AUX-DT.
                05 ANO                   PIC 9(04).
                05 FILLER                PIC X(01).
                05 MES                   PIC 9(02).
                05 FILLER                PIC X(01).
                05 DIA                   PIC 9(02).
             03 AUX-HIF                  PIC X(01).
             03 AUX-HORA                 PIC X(15).

       01 IN-CTRA-MIGRADA-CAC             PIC  X(01) VALUE 'N'.
       01 IN-SGTSL137-ACIONADO            PIC  X(01) VALUE 'N'.
       01 GDA-CONS-CTRA                   PIC S9(009) COMP-5 VALUE 0.
       01 IC-L137                        PIC  9(03) VALUE 0.


       01 SGTS0090-DADOS.
-INC SGTK0090

       01  SGTST066-DADOS.
-INC SGTKT066

       01  SGTPB13S-DADOS.
-INC SGTKB13S

       01  SGTST003-DADOS.
-INC SGTKT003

       01  SGTST122-DADOS.
-INC SGTKT122

       01  SGTSPE16-DADOS.
-INC SGTKPE16

       01  SGTSPE23-DADOS.
-INC SGTKPE23

       01  SGTS009A-DADOS.
-INC SGTK009A

       01  SGTSL137-DADOS.
-INC SGTKL137


      * Controle de leitura do arquivo de entrada
       01  CTL-LET-ARQ.
           03  IN-FIM-ARQ       PIC X(01)          VALUE SPACES.
               88  FIM-ARQ                         VALUE 'S'.


           EXEC SQL
              INCLUDE SQLCA
           END-EXEC.


      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL SECTION.
      *-----------------------------------------------------------------

      *    WHENEVER DB2
      *-INC HLPKE002

           MOVE 0001                     TO PSC-ERRO-PGM

           CALL  SBVERSAO  USING  CTE-PROG  CTE-VERS

           PERFORM  910005-BUSCA-TS-DT
           MOVE CORR HLPKE001-TS-FMTD  TO DT-SQL-ATU-R
           MOVE CORR HLPKE001-TS-FMTD  TO HR-SQL-ATU-R

      *== Inicio-2 Alteração 09/10/2020
           MOVE CORR HLPKE001-TS-FMTD  TO DT-PRCT-R
      *== Final-2  Alteração 09/10/2020
      *
           MOVE 4095                   TO A-TAM
           MOVE 'SBDATAF -WRKAREA-FIM' TO A-FIM
           MOVE '    -  -  -'          TO  AUX-TIMESTAMP
           PERFORM  910007-CALCULA-DATA
      *     INITIALIZE SGTST003-DADOS
      *     SET   SGTST003-INCLUIR      TO TRUE
      *     PERFORM  910003-CHAMAR-SGTST003
      *     IF    SGTST003-CD-RTN-PGM   EQUAL ZEROS
      *           SET IN-GRAVOU         TO TRUE
      *     ELSE
      *           SET IN-NAO-GRAVOU     TO TRUE
      *     END-IF

           OPEN INPUT SGTF010E

      *== Inicio-3 Alteração 09/10/2020
      *--> Ler O primeiro registro da posição diária do titulo
           PERFORM 970000-LER-POSICAO-DIARIA-TIT
           IF  NOT FIM-ARQ
               PERFORM 111000-VALIDA-DT-EMISSAO
               IF  NOT ERRO-VALIDACAO AND
                   SGTPB13S-CARTEIRA  NOT EQUAL SPACES
                   MOVE CORR  SGTPB13S-DT-EMISSAO-R  TO DT-EMISSAO-R
                   IF  DT-EMISSAO-R   NOT LESS DT-PRCT-R
                       SET FIM-ARQ    TO TRUE
                       MOVE HLPKE001-TS-FMTD         TO  TS-FMTD-LDR-SIS
                       MOVE SGTPB13S-CARTEIRA (2:2)  TO SEGUNDO-CTRA
                       MOVE SGTPB13S-CARTEIRA (4:6)  TO
                                                     MICRO-SEGUNDO-CTRA
                       MOVE 6                        TO  AX-RETURN-CODE
                   END-IF
               ELSE
                   PERFORM 999999-ENCERRAMENTO-FORCADO
               END-IF
           END-IF

           PERFORM UNTIL FIM-ARQ
                    MOVE SGTPB13S-CARTEIRA TO GDA-CONS-CTRA
                    PERFORM 900300-VER-CTRA-MIGRADA-CAC
                    IF IN-CTRA-MIGRADA-CAC NOT EQUAL 'S'
                      PERFORM 100000-TRATA-REGISTRO
                   END-IF
                   PERFORM 970000-LER-POSICAO-DIARIA-TIT
           END-PERFORM

      *== Final-3  Alteração 09/10/2020

           IF      CNT-INTER                 GREATER   ZEROS
                   MOVE  1                   TO        IB-1
                   PERFORM 200000-ALTERAR-TABELA  UNTIL IB-1
                                                  GREATER 100
           ELSE
            IF     IA-1                      GREATER   ZEROS
                   MOVE  1                   TO        IB-1
                   PERFORM 300000-EXCLUIR-TABELA UNTIL IB-1
                                                 GREATER 100
            END-IF
           END-IF

           PERFORM 900100-EXIBIR-RESUMO-PROCESS

           IF  AX-RETURN-CODE          EQUAL 6
               MOVE 4                  TO RETURN-CODE
           END-IF

           STOP RUN
           .

       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-TRATA-REGISTRO          SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                     TO PSC-ERRO-PGM
           MOVE 0                        TO AUX-EDT-CD-IMP

           PERFORM  110000-VALIDA-CAMPOS

           IF ERRO-VALIDACAO
      *  4 = erro de importação
              MOVE 4 TO AUX-EDT-CD-IMP
           ELSE
      ****     Inibir consistência de data de emissão   ****************
      *        IF SGTPB13S-DT-EMISSAO NOT EQUAL DT-REF-R
      *           SET ERRO-VALIDACAO   TO   TRUE
      **  2 = erro na internalização
      *           MOVE 'Data de posição difere do último dia útil.'
      *                  TO MSG-ERRO-VALIDACAO
      *           MOVE 2 TO AUX-EDT-CD-IMP
      *        ELSE
               IF SGTPB13S-HIFEN  EQUAL   '-'
      *  1 = importado e internalizado com ressalva
                  MOVE 1 TO AUX-EDT-CD-IMP
               ELSE
      *  0 = importado e internalizado com sucesso
                  MOVE 0 TO AUX-EDT-CD-IMP
               END-IF
      *        END-IF
           END-IF

           IF NOT ERRO-VALIDACAO
              PERFORM  120000-VALIDA-IMP-TITULO
              IF SGTS0090-IN-RECP-PMT    EQUAL 'N'
                  MOVE 2 TO AUX-EDT-CD-IMP
                  MOVE SGTS0090-TX-DCR-RECP-NAO-PMT TO
                       AUX-EDT-TX-DCR-ERRO-IMP
                  SET ERRO-VALIDACAO   TO   TRUE
                  MOVE SGTS0090-TX-DCR-RECP-NAO-PMT TO
                       MSG-ERRO-VALIDACAO
               ELSE
                  ADD  1   TO  TAB-QTD-INTER(IA-1)
                       ON SIZE ERROR
                       MOVE 'ERRO EM OPERACAO ARITMETICA 0' TO MSG-ERRO
                       PERFORM 999000-ERRO
                  END-ADD
              END-IF
           END-IF

           PERFORM  130000-GRAVA-IMP-TITULO
           EVALUATE AUX-EDT-CD-IMP
                WHEN     0
                         ADD  1  TO  CNT-OK
                            ON SIZE ERROR
                            MOVE 'ERRO EM OPERACAO ARITMETICA 1' TO
                                  MSG-ERRO
                               PERFORM 999000-ERRO
                         END-ADD
                WHEN     1
                         ADD  1  TO  CNT-RESSALVA
                            ON SIZE ERROR
                            MOVE 'ERRO EM OPERACAO ARITMETICA 2'  TO
                                  MSG-ERRO
                               PERFORM 999000-ERRO
                         END-ADD
                WHEN     2
                         ADD  1  TO  CNT-ERRO-INT
                            ON SIZE ERROR
                            MOVE 'ERRO EM OPERACAO ARITMETICA 3'  TO
                                  MSG-ERRO
                               PERFORM 999000-ERRO
                         END-ADD
                WHEN     4
                         ADD  1  TO  CNT-ERRO-IMP
                            ON SIZE ERROR
                            MOVE 'ERRO EM OPERACAO ARITMETICA 4'  TO
                                  MSG-ERRO
                               PERFORM 999000-ERRO
                         END-ADD
           END-EVALUATE

           .

       100000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       110000-VALIDA-CAMPOS                SECTION.
      *-----------------------------------------------------------------
      *
           MOVE 0003                     TO PSC-ERRO-PGM

           MOVE SPACES  TO  IN-ERRO-VALIDACAO
                            IN-ERRO-CARTEIRA
                            MSG-ERRO-VALIDACAO

           IF SGTPB13S-TITULO      EQUAL SPACES
              SET ERRO-VALIDACAO   TO   TRUE
              MOVE 'Código do título inválido.' TO MSG-ERRO-VALIDACAO
           END-IF

      *--> valida data de posição
           PERFORM 111000-VALIDA-DT-EMISSAO
      *==  MOVE SGTPB13S-DT-EMISSAO   TO   AUX-EDT-DT-EMS
      *==
      *==  MOVE CORR AUX-EDT-DT-EMS-R TO ARG01-R
      *==  PERFORM 910006-VALIDAR-DT
      *==  IF  RETURN-CODE            EQUAL 24
      *==      SET ERRO-VALIDACAO     TO   TRUE
      *==      MOVE 'Data de posição inválida.'
      *==                             TO   MSG-ERRO-VALIDACAO
      *==      MOVE SPACES            TO   AUX-EDT-DT-EMS
      *==  END-IF

      *--> valida data de aplicação
      * RER 091219 - Título USCA11DIVIDA - Este tipo de título não tem
      * data de aplicação cadastrada na Drive, por isso vem ZEROS.
           IF SGTPB13S-DT-APLICACAO NOT EQUAL '00000000'
              MOVE SGTPB13S-DT-APLICACAO TO AUX-EDT-DT-APL
           ELSE
              MOVE '01010001'            TO AUX-EDT-DT-APL
           END-IF
           MOVE CORR AUX-EDT-DT-APL-R TO ARG01-R
           PERFORM 910006-VALIDAR-DT
           IF  RETURN-CODE               EQUAL 24
              SET ERRO-VALIDACAO   TO   TRUE
              MOVE 'Data de compra inválida.'
                                   TO MSG-ERRO-VALIDACAO
              MOVE SPACES          TO AUX-EDT-DT-APL
           END-IF

           IF NOT ERRO-VALIDACAO
              IF SGTPB13S-QT-PAPL-R       LESS   ZEROS
                    SET ERRO-VALIDACAO    TO   TRUE
                    MOVE 'Quantidade de papéis negativa.'
                                          TO MSG-ERRO-VALIDACAO
              ELSE
               IF SGTPB13S-QT-BLOQ-R      LESS   ZEROS
                  SET ERRO-VALIDACAO      TO   TRUE
                  MOVE 'Quantidade bloqueados negativa.'
                                          TO MSG-ERRO-VALIDACAO
               ELSE
                 IF SGTPB13S-QT-DISP-R    LESS   ZEROS
                    SET ERRO-VALIDACAO    TO   TRUE
                    MOVE 'Quantidade disponível negativa.'
                                          TO MSG-ERRO-VALIDACAO
                 ELSE
                  IF SGTPB13S-PR-UNIT-R   LESS    ZEROS
                     SET ERRO-VALIDACAO   TO   TRUE
                     MOVE 'Valor do preço unitário negativo'
                                           TO MSG-ERRO-VALIDACAO
                  ELSE
                   IF SGTPB13S-VL-MRC-R    LESS    ZEROS
                      SET ERRO-VALIDACAO   TO   TRUE
                      MOVE 'Valor do saldo do título negativo.'
                                           TO MSG-ERRO-VALIDACAO
                   ELSE
                    IF SGTPB13S-VL-ATU-R    LESS    ZEROS
                       SET ERRO-VALIDACAO   TO   TRUE
                       MOVE 'Valor do saldo da carteira negativo. '
                                            TO MSG-ERRO-VALIDACAO
                    END-IF
                   END-IF
                  END-IF
                 END-IF
                END-IF
               END-IF
              END-IF

      * valida carteira - deverá ser o último campo a ser verificado
      * pois só vou carregar na tabela as carteiras que tiverem pelo
      * menos um registro valido

           IF NOT ERRO-VALIDACAO

            IF SGTPB13S-CARTEIRA    NOT EQUAL AUX-CARTEIRA-ANT

               PERFORM 112000-VALIDA-CARTEIRA

               IF  NOT   ERRO-CARTEIRA     AND
                   SGTSPE23-TX-VCLD-CRCT   EQUAL 'S'
                  ADD  1   TO IA-1  ON SIZE ERROR
                       MOVE 'ERRO EM OPERACAO ARITMETICA 5'
                                             TO  MSG-ERRO
                       PERFORM 999000-ERRO
                  END-ADD
                  MOVE SGTPB13S-CARTEIRA     TO  TAB-CD-CARTEIRA(IA-1)
                  MOVE HR-SQL-ATU            TO  TAB-HR-SQL-ATU (IA-1)
      * ---------- Altera TIMESTAMP para Tratar Nova Carteira
                  IF AUX-CARTEIRA-ANT NOT EQUAL ZEROS
                     IF SEGUNDO OF  HR-SQL-ATU-R EQUAL 59
                        ADD 1 TO MINUTO OF  HR-SQL-ATU-R
                        MOVE ZEROS TO SEGUNDO OF  HR-SQL-ATU-R
                     ELSE
                        ADD 1 TO SEGUNDO OF  HR-SQL-ATU-R
                     END-IF
                     IF MINUTO OF  HR-SQL-ATU-R GREATER 59
                        ADD 1 TO HORA OF  HR-SQL-ATU-R
                        MOVE ZEROS TO MINUTO OF  HR-SQL-ATU-R
                     END-IF
                     MOVE HR-SQL-ATU   TO  TAB-HR-SQL-ATU (IA-1)
                  END-IF

      * ---------- Inclui   Tabela DB2SGT.PSC_DRIA_CTRA_TIT
                  INITIALIZE SGTST003-DADOS
                  SET   SGTST003-INCLUIR     TO TRUE
                  MOVE  SGTPB13S-CARTEIRA    TO SGTST003-CD-CTRA-TIT-VL
                  PERFORM  910003-CHAMAR-SGTST003
                  IF    SGTST003-CD-RTN-PGM  EQUAL ZEROS
                        MOVE 'S'             TO TAB-GRAVOU(IA-1)
                  ELSE
                        MOVE 'N'             TO TAB-GRAVOU(IA-1)
                  END-IF


               END-IF
              MOVE SGTPB13S-CARTEIRA         TO  AUX-CARTEIRA-ANT
            END-IF
            IF   ERRO-CARTEIRA
                 SET ERRO-VALIDACAO          TO   TRUE
                 MOVE 'Não foi encontrada característica para a carteira
      -          ' informada.'               TO   MSG-ERRO-VALIDACAO
            ELSE
             IF  SGTSPE23-TX-VCLD-CRCT       NOT EQUAL  'S'
                 SET ERRO-VALIDACAO          TO   TRUE
                 MOVE 'Carteira não disponibilizada para atualização.'
                                              TO  MSG-ERRO-VALIDACAO
             END-IF
            END-IF
           END-IF

           .

       110000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       111000-VALIDA-DT-EMISSAO          SECTION.
      *-----------------------------------------------------------------

           MOVE 0004                     TO  PSC-ERRO-PGM
           MOVE SGTPB13S-DT-EMISSAO   TO   AUX-EDT-DT-EMS

           MOVE CORR AUX-EDT-DT-EMS-R TO ARG01-R
           PERFORM 910006-VALIDAR-DT
           IF  RETURN-CODE            EQUAL 24
               SET ERRO-VALIDACAO     TO   TRUE
               MOVE 'Data de posição inválida.'
                                      TO   MSG-ERRO-VALIDACAO
               MOVE SPACES            TO   AUX-EDT-DT-EMS
           END-IF
           .
       111000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       112000-VALIDA-CARTEIRA            SECTION.
      *-----------------------------------------------------------------

           MOVE 0005                     TO  PSC-ERRO-PGM
           MOVE SPACES                   TO  IN-ERRO-CARTEIRA
                                             SGTSPE23-TX-VCLD-CRCT

           MOVE SGTPB13S-CARTEIRA        TO  SGTSPE23-CD-CTRA-TIT-VL
           MOVE 8                        TO  SGTSPE23-CD-TIP-ATB-CTRA
           MOVE 2                        TO  SGTSPE23-CD-TIP-EST-DCS
           MOVE CORR SGTPB13S-DT-EMISSAO-R
                                         TO  AUX-DT
           MOVE HR-SQL-ATU-R             TO  AUX-HORA
           MOVE AUX-TIMESTAMP-R          TO  SGTSPE23-TS-EST-CRTC-CTRA

           MOVE LENGTH OF SGTSPE23-DADOS TO EIBCALEN
           CALL SGTSPE23 USING DFHEIBLK SGTSPE23-DADOS

           IF SGTSPE23-CD-RTN-PGM NOT EQUAL ZEROS  AND
                                  NOT EQUAL +100
              MOVE SGTSPE23-RTN-PGM TO RTN-PGM
              PERFORM 999000-ERRO
           ELSE
            IF SGTSPE23-CD-RTN-PGM     EQUAL +100
               SET ERRO-CARTEIRA       TO    TRUE
            END-IF
           END-IF

           .

       112000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       120000-VALIDA-IMP-TITULO            SECTION.
      *-----------------------------------------------------------------

           MOVE 0006                   TO PSC-ERRO-PGM

           INITIALIZE SGTS0090-DADOS

           MOVE SGTPB13S-TITULO        TO SGTS0090-CD-TIT-OPR-ETRD
           MOVE 1                      TO SGTS0090-NR-DVS-TIT
      *    SET RECP-PSC-DRIA-SDO        TO TRUE
           MOVE CORR AUX-EDT-DT-EMS-R  TO DT-FMTD
           MOVE DT-FMTD                TO SGTS0090-DT-PSC
           MOVE DT-SQL-ATU             TO SGTS0090-DT-IMP-PSC-SDO
           MOVE HR-SQL-ATU             TO SGTS0090-HR-IMP-PSC-SDO
           MOVE SGTPB13S-NR-REG-R      TO SGTS0090-NR-IMP-PSC-SDO
           MOVE SGTPB13S-QT-PAPL-R     TO SGTS0090-QT-PAPL-TIT-DT-PSC
           MOVE SGTPB13S-PR-UNIT-R     TO SGTS0090-VL-PU-TIT-DT-PSC
           MOVE SGTPB13S-VL-MRC-R      TO SGTS0090-VL-SDO-TIT-DT-PSC
           MOVE CORR AUX-EDT-DT-APL-R  TO DT-FMTD
           MOVE DT-FMTD                TO SGTS0090-DT-CPR-TIT
           MOVE SGTPB13S-CARTEIRA
                                       TO SGTS0090-CD-CTRA-TIT-VL
           MOVE 'SGTPB010'             TO SGTS0090-CD-USU-RSP-ATL
           PERFORM 910001-CHAMAR-SGTS0090

           .

       120000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       130000-GRAVA-IMP-TITULO            SECTION.
      *-----------------------------------------------------------------

           MOVE 0007                     TO PSC-ERRO-PGM
      *    DB2SGT.IMP_PSC_DRIA_SDO
           INITIALIZE SGTST066-DADOS

           SET  SGTST066-INCLUIR        TO TRUE

           MOVE DT-SQL-ATU              TO SGTST066-DT-IMP-PSC-SDO
           MOVE HR-SQL-ATU              TO SGTST066-HR-IMP-PSC-SDO
           MOVE ZEROS                   TO SGTST066-NR-IMP-PSC-SDO
           MOVE SGTPB13S-CARTEIRA       TO SGTST066-CD-CTRA-TIT-VL
           MOVE SGTPB13S-TITULO         TO SGTST066-CD-TIT-OPR-ETRD
           MOVE 1                       TO SGTST066-NR-DVS-TIT
           MOVE CORR AUX-EDT-DT-EMS-R   TO DT-FMTD
           MOVE DT-FMTD                 TO SGTST066-DT-PSC
           MOVE CORR AUX-EDT-DT-APL-R   TO DT-FMTD
           MOVE DT-FMTD                 TO SGTST066-DT-CPR-TIT
           MOVE SGTPB13S-QT-PAPL-R  TO SGTST066-QT-PAPL-TIT-DT-PSC
           MOVE SGTPB13S-PR-UNIT-R  TO SGTST066-VL-PU-TIT-DT-PSC
           MOVE SGTPB13S-VL-MRC-R   TO SGTST066-VL-SDO-TIT-DT-PSC
           MOVE SGTPB13S-QT-BLOQ-R  TO SGTST066-QT-BLQD-PAPL-TIT
           MOVE SGTPB13S-QT-DISP-R  TO SGTST066-QT-DSPN-PAPL-TIT
           MOVE SGTPB13S-VL-ATU-R   TO SGTST066-VL-SDO-CTRA
           MOVE SGTPB13S-TP-ATIVO   TO SGTST066-TX-DCR-TIP-ATI
           MOVE SGTPB13S-NR-REG-R   TO SGTST066-NR-REG-ARQ
           MOVE AUX-EDT-CD-IMP      TO SGTST066-CD-EST-IMP
           MOVE 'SGTPB010'          TO SGTST066-CD-USU-RSP-ATL
           MOVE 1                   TO SGTST066-CD-TIP-OGM-INF
           IF ERRO-VALIDACAO
              MOVE 'N'                       TO SGTST066-IN-IMP-SDO-SCS
           ELSE
              MOVE 'S'                       TO SGTST066-IN-IMP-SDO-SCS
           END-IF
           MOVE MSG-ERRO-VALIDACAO           TO SGTST066-TX-DCR-ERRO-IMP
           PERFORM 910002-CHAMAR-SGTST066
           .

       130000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-ALTERAR-TABELA             SECTION.
      *-----------------------------------------------------------------

           MOVE 0008                     TO PSC-ERRO-PGM
      *    DB2SGT.PSC_DRIA_CTRA_TIT

           INITIALIZE SGTST003-DADOS SGTSPE16-DADOS
           IF   TAB-CD-CARTEIRA(IB-1)    GREATER ZEROS
             IF TAB-QTD-INTER(IB-1)      GREATER ZEROS
                MOVE TAB-CD-CARTEIRA(IB-1)
                                         TO SGTST003-CD-CTRA-TIT-VL
                                            SGTSPE16-CD-CTRA-TIT-VL
                                            SGTS009A-CD-CTRA-TIT-VL

                PERFORM  910004-CHAMAR-SGTSPE16
                SET   SGTST003-ALTERAR   TO TRUE
                PERFORM  910003-CHAMAR-SGTST003

      ***  Só verifica operação encerrada se a Data Posição (Movimento)
      ***  for igual a D-1
                IF AUX-EDT-DT-EMS   EQUAL DT-REF
                   PERFORM 210000-VER-OPR-ENC
                END-IF
             ELSE
                IF TAB-GRAVOU(IB-1)   EQUAL 'S'
                   SET SGTST003-EXCLUIR      TO   TRUE
                   MOVE TAB-CD-CARTEIRA(IB-1)
                                         TO SGTST003-CD-CTRA-TIT-VL
                   PERFORM  910003-CHAMAR-SGTST003
                 END-IF
             END-IF
             ADD   1                   TO IB-1
                   ON SIZE ERROR
                      MOVE 'ERRO EM OPERACAO ARITMETICA 6'  TO MSG-ERRO
                      PERFORM 999000-ERRO
             END-ADD
           ELSE
                MOVE 999                 TO IB-1
           END-IF
           .

       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       210000-VER-OPR-ENC               SECTION.
      *-----------------------------------------------------------------

           MOVE 0009                     TO PSC-ERRO-PGM
           MOVE 'SGTPB010'               TO SGTS009A-CD-USU-RSP-ALT
           MOVE CORR DT-SQL-ATU-R        TO SGTS009A-DT-IMP-PSC-SDO-R

           MOVE TAB-HR-SQL-ATU(IB-1)     TO HR-SQL-AUX
           MOVE CORR HR-SQL-AUX-R        TO SGTS009A-HR-IMP-PSC-SDO-R

           MOVE LENGTH OF SGTS009A-DADOS TO EIBCALEN
           CALL SGTS009A USING DFHEIBLK SGTS009A-DADOS

           IF SGTS009A-CD-RTN-PGM NOT EQUAL ZEROS

              MOVE SGTS009A-RTN-PGM TO RTN-PGM

              PERFORM 999000-ERRO
           END-IF

           .

       210000-SAI.
           EXIT.


      *-----------------------------------------------------------------
       300000-EXCLUIR-TABELA              SECTION.
      *-----------------------------------------------------------------

           MOVE 0010                     TO PSC-ERRO-PGM
      *    DB2SGT.PSC_DRIA_CTRA_TIT

           INITIALIZE SGTST003-DADOS
           SET SGTST003-EXCLUIR          TO TRUE
           IF  TAB-CD-CARTEIRA(IB-1)     GREATER ZEROS
            IF TAB-GRAVOU(IB-1)          EQUAL   'S'
               MOVE TAB-CD-CARTEIRA(IB-1)
                                         TO SGTST003-CD-CTRA-TIT-VL
               PERFORM  910003-CHAMAR-SGTST003
            END-IF
             ADD   1                   TO IB-1
                   ON SIZE ERROR
                      MOVE 'ERRO EM OPERACAO ARITMETICA 7'  TO
                                  MSG-ERRO
                      PERFORM 999000-ERRO
              END-ADD
           ELSE
                MOVE 999                 TO IB-1
           END-IF

           .

       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       900100-EXIBIR-RESUMO-PROCESS        SECTION.
      *-----------------------------------------------------------------

           MOVE 0011                     TO PSC-ERRO-PGM

           CLOSE     SGTF010E

           IF CNT-LIDOS EQUAL ZEROS
              DISPLAY 'ARQUIVO VAZIO.'
           ELSE
              PERFORM 900200-GRAVAR-LEADER

              IF    CNT-RESSALVA  NOT EQUAL  ZEROS
                 OR CNT-ERRO-INT  NOT EQUAL  ZEROS
                 OR CNT-ERRO-IMP  NOT EQUAL  ZEROS
                 MOVE 004 TO RETURN-CODE
              END-IF

              IF AX-RETURN-CODE = 6
                 DISPLAY '---------------------------------------------'
                 DISPLAY '*** ARQUIVO POSICAO DIARIA NAO PROCESSADO ***'
                 DISPLAY '---------------------------------------------'
                 MOVE CORR DT-EMISSAO-R TO DT-PSC-R
                 DISPLAY 'DATA EMISSAO PRIMEIRO REGISTRO...: ' DT-PSC-R
                 MOVE CORR DT-PRCT-R TO DT-PSC-R
                 DISPLAY 'DATA DO PROCESSAMENTO............: ' DT-PSC-R
              END-IF


              DISPLAY '---------------------------------------------'
              DISPLAY 'QTDE.REGISTROS LIDOS.....: ' CNT-LIDOS
              DISPLAY 'REG. OK..................: ' CNT-OK
              DISPLAY 'REG. COM RESSALVA .......: ' CNT-RESSALVA
              DISPLAY 'REG. COM ERRO INTERNALIZ.: ' CNT-ERRO-INT
              DISPLAY 'REG. COM ERRO IMPORT ....: ' CNT-ERRO-IMP
              DISPLAY 'REG. INTERNALIZADOS .....: ' CNT-INTER
              DISPLAY '---------------------------------------------'

           END-IF

           DISPLAY '999 ' CTE-PROG '999 - FIM-NORMAL'

           .

       900100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       900200-GRAVAR-LEADER        SECTION.
      *-----------------------------------------------------------------

           MOVE 0012                   TO    PSC-ERRO-PGM

           MOVE          '.'           TO  PT01 PT02
           MOVE CORR AUX-EDT-DT-EMS-R  TO DT-FMTD
           MOVE DT-FMTD                TO DT-PSC
           MOVE          '.'           TO  PT03 PT04

           SET   SGTST122-INCLUIR      TO TRUE
           PERFORM  900210-CHAMAR-SGTST122

           IF SGTST122-CD-RTN-PGM EQUAL 803
              SET   SGTST122-ALTERAR   TO TRUE
              PERFORM  900210-CHAMAR-SGTST122
           END-IF.

       900200-SAI.
           EXIT.

      *-----------------------------------------------------------------
       900210-CHAMAR-SGTST122             SECTION.
      *-----------------------------------------------------------------

           MOVE 0013                     TO PSC-ERRO-PGM

           MOVE  'SGTPB010'              TO SGTST122-CD-CHV-USU-ATL
           MOVE  DT-SQL-ATU              TO SGTST122-DT-INCL-LDR
           MOVE  DT-PSC                  TO SGTST122-DT-ULT-MVT-ENV

      *== Inicio-4 Alteração 09/10/2020
           MOVE  TS-FMTD-LDR-SIS         TO SGTST122-TS-FCHT-CMCO-ONL
           MOVE  TS-FMTD-LDR-SIS         TO SGTST122-TS-RBT-CMCO-ONL
      *== Final-4  Alteração 09/10/2020

           MOVE LENGTH OF SGTST122-DADOS TO EIBCALEN
           CALL SGTST122 USING DFHEIBLK SGTST122-DADOS

           IF  SGTST122-CD-RTN-PGM NOT EQUAL ZEROS
               IF  SGTST122-INCLUIR
                   IF SGTST122-CD-RTN-PGM NOT EQUAL 803
                      MOVE SGTST122-RTN-PGM TO RTN-PGM
                      PERFORM 999000-ERRO
                   END-IF
               ELSE
                   MOVE SGTST122-RTN-PGM TO RTN-PGM
                   PERFORM 999000-ERRO
               END-IF
           END-IF
           .
       900210-SAI.
           EXIT.

      *-----------------------------------------------------------------
       900300-VER-CTRA-MIGRADA-CAC        SECTION.
      *-----------------------------------------------------------------
           MOVE 'N' TO IN-CTRA-MIGRADA-CAC
           IF IN-SGTSL137-ACIONADO EQUAL 'N'
              MOVE 100 TO SGTSL137-CD-TIP-ATB-CTRA
              PERFORM 910008-CHAMAR-SGTSL137
              MOVE 'S'  TO  IN-SGTSL137-ACIONADO
           END-IF

           PERFORM  VARYING   IC-L137
                                           FROM 0001 BY 0001
                     UNTIL IC-L137
                     GREATER  SGTSL137-QT-OCR-TAB
                     OR IN-CTRA-MIGRADA-CAC EQUAL 'S'
                IF SGTSL137-CD-CTRA-TIT-VL(IC-L137) EQUAL GDA-CONS-CTRA
                   MOVE 'S'  TO  IN-CTRA-MIGRADA-CAC
                END-IF
           END-PERFORM

           .

       900300-SAI.
           EXIT.

      *-----------------------------------------------------------------
       910001-CHAMAR-SGTS0090              SECTION.
      *-----------------------------------------------------------------
           MOVE 0014                     TO PSC-ERRO-PGM

           MOVE LENGTH OF SGTS0090-DADOS TO EIBCALEN
           CALL SGTS0090 USING DFHEIBLK SGTS0090-DADOS

           IF SGTS0090-CD-RTN-PGM NOT EQUAL ZEROS

              MOVE SGTS0090-RTN-PGM TO RTN-PGM

              PERFORM 999000-ERRO
           END-IF
           IF SGTS0090-IN-RCP-SIM
              ADD   1               TO  CNT-INTER
                    ON SIZE ERROR
                       MOVE 'ERRO EM OPERACAO ARITMETICA 8'  TO
                             MSG-ERRO
                       PERFORM 999000-ERRO
              END-ADD
           END-IF

           .

       910001-SAI.
           EXIT.


      *-----------------------------------------------------------------
       910002-CHAMAR-SGTST066              SECTION.
      *-----------------------------------------------------------------
           MOVE 0015                     TO PSC-ERRO-PGM

           SET  SGTST066-INCLUIR    TO  TRUE
           MOVE LENGTH OF SGTST066-DADOS TO EIBCALEN
           CALL SGTST066 USING DFHEIBLK SGTST066-DADOS

           IF SGTST066-CD-RTN-PGM NOT EQUAL ZEROS

              MOVE SGTST066-RTN-PGM TO RTN-PGM
              PERFORM 999000-ERRO
           END-IF

           .

       910002-SAI.
           EXIT.

      *-----------------------------------------------------------------
       910003-CHAMAR-SGTST003             SECTION.
      *-----------------------------------------------------------------

           MOVE 0016                     TO PSC-ERRO-PGM

           MOVE  'SGTPB010'              TO SGTST003-CD-CHV-USU
           MOVE  CORR AUX-EDT-DT-EMS-R   TO DT-FMTD
           MOVE  DT-FMTD                 TO SGTST003-DT-PSC
           MOVE 'CURRENT'                TO SGTST003-TS-ULT-ATL
           MOVE 'SGTPB010'               TO SGTST003-CD-USU-RSP-ATL

           IF   SGTST003-INCLUIR
                MOVE     0               TO SGTST003-VL-SDO-CTRA
                MOVE     0               TO SGTST003-QT-PAPL-CTRA
           ELSE
            IF  SGTST003-ALTERAR
                MOVE SGTSPE16-VL-SDO-CTRA  TO SGTST003-VL-SDO-CTRA
                MOVE SGTSPE16-QT-PAPL-CTRA TO SGTST003-QT-PAPL-CTRA
            END-IF
           END-IF

           MOVE LENGTH OF SGTST003-DADOS TO EIBCALEN
           CALL SGTST003 USING DFHEIBLK SGTST003-DADOS

           IF  SGTST003-CD-RTN-PGM NOT EQUAL ZEROS
               IF  SGTST003-INCLUIR
                   IF SGTST003-CD-RTN-PGM NOT EQUAL -803
                      MOVE SGTST003-RTN-PGM TO RTN-PGM
                      PERFORM 999000-ERRO
                   END-IF
               ELSE
                      MOVE SGTST003-RTN-PGM TO RTN-PGM
                      PERFORM 999000-ERRO
               END-IF
           END-IF

           .

       910003-SAI.
           EXIT.

      *-----------------------------------------------------------------
       910004-CHAMAR-SGTSPE16              SECTION.
      *-----------------------------------------------------------------
           MOVE 0017                     TO PSC-ERRO-PGM

           MOVE  CORR AUX-EDT-DT-EMS-R   TO DT-FMTD
           MOVE  DT-FMTD                 TO SGTSPE16-DT-PSC

           MOVE LENGTH OF SGTSPE16-DADOS TO EIBCALEN
           CALL SGTSPE16 USING DFHEIBLK SGTSPE16-DADOS

           IF SGTSPE16-CD-RTN-PGM NOT EQUAL ZEROS

              MOVE SGTSPE16-RTN-PGM TO RTN-PGM
              PERFORM 999000-ERRO
           END-IF

           .

       910004-SAI.
           EXIT.



      *-----------------------------------------------------------------
       910005-BUSCA-TS-DT SECTION.
      *-----------------------------------------------------------------
           MOVE 0018                   TO PSC-ERRO-PGM
           CALL SBTIMEDT            USING HLPKE001-CD-FUC
                                          HLPKE001-AREA-TRB
                                          HLPKE001-FMT-TS
                                          HLPKE001-TAM-TS
                                          HLPKE001-TS-FMTD
           IF  RETURN-CODE      NOT EQUAL ZEROS
               MOVE RETURN-CODE        TO CD-RTN-PGM
               MOVE 'ERRO NA CHAMADA DA SBTIMEDT' TO MSG-ERRO
               PERFORM 999000-ERRO
           END-IF
           .

       910005-SAI.
           EXIT.

      *-----------------------------------------------------------------
       910006-VALIDAR-DT SECTION.
      *-----------------------------------------------------------------
              MOVE 0019                   TO PSC-ERRO-PGM



              MOVE 'F01'                  TO FUNCAO
              MOVE 4                      TO ARG02

              CALL SBDATAF                USING FUNCAO AREA-WORK ARG01
                                                ARG02

      *--> Return code = 24: Data Inválida.

              IF  RETURN-CODE             NOT EQUAL ZEROS AND 24
                  MOVE RETURN-CODE        TO CD-RTN-PGM
                  MOVE 'ERRO NA CHAMADA DA SBDATAF-1' TO MSG-ERRO
                  PERFORM 999000-ERRO
              END-IF
            .

       910006-SAI.
            EXIT.

      *-----------------------------------------------------------------
       910007-CALCULA-DATA SECTION.
      *-----------------------------------------------------------------
      *
      * calcula o dia util anterior a data do processamento
      * IND-FERIADO = 1 ----> feriado
      *             = 0 ----> nao feriado
      * IND-DIA-SEMANA = 6 ---> sabado
      *                = 7 ----> domingo

              MOVE 0020                   TO    PSC-ERRO-PGM

              MOVE CORR DT-SQL-ATU-R      TO    ARG01-R
              MOVE 1                      TO    IND-FERIADO
              PERFORM UNTIL (IND-FERIADO  EQUAL 0) AND
                            (IND-DIA-SEMANA NOT EQUAL 6 AND
                             IND-DIA-SEMANA NOT EQUAL 7)
                MOVE 'F05'                TO    FUNCAO
                CALL SBDATAF              USING FUNCAO AREA-WORK ARG01
                                                ARG02-2 ARG03 ARG04

                 IF  RETURN-CODE           NOT EQUAL ZEROS
                     MOVE RETURN-CODE      TO   CD-RTN-PGM
                     MOVE 'ERRO NA CHAMADA DA SBDATAF-2' TO MSG-ERRO
                     PERFORM 999000-ERRO
                 ELSE
                     MOVE ARG04            TO    ARG01
                     MOVE 0                TO    ARG04 IND-FERIADO
                     MOVE 'F23'            TO    FUNCAO
                     CALL SBDATAF          USING FUNCAO AREA-WORK
                                                ARG01 ARG02-1

                     IF  RETURN-CODE       NOT EQUAL ZEROS
                         MOVE RETURN-CODE  TO   CD-RTN-PGM
                         MOVE 'ERRO NA CHAMADA DA SBDATAF-3' TO MSG-ERRO
                         PERFORM 999000-ERRO
                     ELSE
                      IF (IND-FERIADO       EQUAL  1) OR
                         (IND-DIA-SEMANA    EQUAL  6 OR 7)
                          MOVE 0            TO     ARG04
                      END-IF
                     END-IF
                  END-IF
                END-PERFORM
               MOVE  ARG01               TO      DT-REF

            .

       910007-SAI.
            EXIT.

      *-----------------------------------------------------------------
       910008-CHAMAR-SGTSL137              SECTION.
      *-----------------------------------------------------------------
           MOVE 0019                   TO PSC-ERRO-PGM
           MOVE LENGTH OF SGTSL137-DADOS TO EIBCALEN
           CALL SGTSL137 USING DFHEIBLK SGTSL137-DADOS

           IF SGTSL137-CD-RTN-PGM NOT EQUAL ZEROS
              MOVE SGTSL137-RTN-PGM TO RTN-PGM
              PERFORM 999000-ERRO
           END-IF
           .
       910008-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970000-LER-POSICAO-DIARIA-TIT   SECTION.
      *-----------------------------------------------------------------
           MOVE 0021                   TO    PSC-ERRO-PGM

           INITIALIZE SGTPB13S-DADOS

           READ    SGTF010E INTO SGTPB13S-DADOS
                   AT END
                      SET FIM-ARQ     TO TRUE

                   NOT AT END
                          ADD +1      TO CNT-LIDOS
           END-READ
           .
       970000-SAI.
            EXIT.

      *--------------------
       999000-ERRO SECTION.

      *-----------------
           EXEC SQL
                ROLLBACK
           END-EXEC
      *-----------------
           PERFORM 999001-ERRO-001
           .
       999000-ERRO-SAI.
           EXIT.
      *--------------------
      *-----------------
      * 999001-ERRO-001.
      *-----------------
-INC HLPKE004

           IF  ERRO-CODIFICADO
               PERFORM 999082-RECUPERA-MSG-CODIFICADA
           END-IF

           DISPLAY  '888'  CTE-PROG 'CANCELADO POR'
           DISPLAY  '888'  CTE-PROG  MSG-ERRO-PGM
           DISPLAY  '888'  CTE-PROG 'PROGRAMA:' PGM-ERRO
           DISPLAY HLPS0200-TX-MSG-ITNO
           DISPLAY  '888'  CTE-PROG 'CD-RTN :'  CD-RTN-PGM
           DISPLAY  '888'  CTE-PROG 'TIP-RTN :' TIP-RTN-PGM
           DISPLAY  '888'  CTE-PROG 'PSC-ERRO:' PSC-ERRO-PGM
           DISPLAY  '888'  CTE-PROG 'MAIS INFORMACOES VIDE DUMP FORMATA
      -        'DO.'

           CALL SBABEND
           .
      *-----------------
      * 999002-ERRO-DB2
      *-----------------
      *=INC HLPKE005


      *     .

       999009-SAI.
           EXIT.

      *== Inicio-5 Alteração 15/10/2020
      *-----------------------------------------------------------------
       999999-ENCERRAMENTO-FORCADO     SECTION.
      *-----------------------------------------------------------------
           MOVE 0022                   TO    PSC-ERRO-PGM

           DISPLAY '888'  CTE-PROG 'CANCELADO POR'
           DISPLAY '888'  CTE-PROG 'INCONSISTÊNCIA NO PRIMEIRO REGISTRO
      -        'LIDO '
           DISPLAY '888'  CTE-PROG 'ARQUIVO SGTF010E (POSICAO DIARIA)'

           IF  ERRO-VALIDACAO
               DISPLAY '888'  CTE-PROG 'DATA EMISSAO INVALIDA'
           END-IF

           IF  SGTPB13S-CARTEIRA  EQUAL SPACES
               DISPLAY '888'  CTE-PROG 'CARTEIRA NAO INFORMADA'
           END-IF

           DISPLAY '888'  CTE-PROG 'PSC-ERRO:' PSC-ERRO-PGM

           CLOSE     SGTF010E

           CALL SBABEND
           .
       999999-SAI.
            EXIT.
      *== Final-5  Alteração 15/10/2020
      *---------------- FIM SGTPB010 ----------------------------