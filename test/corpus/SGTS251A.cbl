       CBL ARITH(EXTEND)
      *-----------------------------------------------------------------
      * PROGRAMA..: SGTS251A
      * ANALISTA..: F7464259 Newton de Lima Carlini
      * AUTOR.....: F7464259 Newton de Lima Carlini
      * DATA......: 16/07/2024
      * OBJETIVO..: Internalizar a posição diária de saldo via CAC
      * COMPILACAO: COBOL/CICS (On-line)
      *-----------------------------------------------------------------
      * VRS001 16.07.2024 - F7464259 - Implantação
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTS251A.

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
               '*** SGTS251A ***'.
       77  CTE-VERS                    PIC X(06) VALUE 'VRS001'.



      *--> Definição dos Nomes dos Programas Acionados
      *--> Módulos acionados / Padrão para Cobolito
       01  TAB-PROGRAMAS-CHAMADOS.
      *--> Definição dos Nomes dos Programas Acionados
      *--> Módulos a serem acionados via 'EXEC CICS LINK (COMMAREA)'
           03  SGTS0302                    PIC X(08) VALUE 'SGTS0302'.
           03  SGTS009A                    PIC X(08) VALUE 'SGTS009A'.
           03  SBTIMEDT                    PIC X(08) VALUE 'SBTIMEDT'.
           03  SBDATAF                     PIC X(08) VALUE 'SBDATAF'.
           03  SGTSPE16                    PIC X(08) VALUE 'SGTSPE16'.
           03  SGTST003                    PIC X(08) VALUE 'SGTST003'.


      *--> Definição dos Variaveis do Cobolito
       COPY ALMKCBL1.
      *-----------------------------------------------------------------
      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informacoes
       COPY HLPKE01B.
      *-----------------------------------------------------------------
       LOCAL-STORAGE SECTION.
      *-----------------------------------------------------------------


      
      *--> Data atual
       01  TS-SQL-ATU                  PIC  X(26) VALUE ' '.
       01  TS-SQL-ATU-R                REDEFINES TS-SQL-ATU.
           03 DT-REF                   PIC  X(10).
           03 DT-REF-R                 REDEFINES   DT-REF.
             05 ANO                    PIC  9(04).
             05 FILLER                 PIC  X(01).
             05 MES                    PIC  9(02).
             05 FILLER                 PIC  X(01).
             05 DIA                    PIC  9(02).
           03  FILLER                  PIC  X(01).
           03  HORARIO                 PIC  X(08).
           03  HORARIO-R               REDEFINES HORARIO.
               05 HORA                 PIC  9(02).
               05 FILLER               PIC  X(01).
               05 MINUTO               PIC  9(02).
               05 FILLER               PIC  X(01).
               05 SEGUNDO              PIC  9(02).
           03  FILLER                  PIC  X(01).
           03  MICRO-SEGUNDO           PIC  9(06).

      * SBDATA Var
       01  FUC                         PIC  X(003) VALUE 'F01'.
      
       01 WKSBDATA                     PIC X(4095).
       01 AREA-RED REDEFINES WKSBDATA.
          03  AREA-TAM                 PIC 9(0008) COMP.
          03  FILLER                   PIC X(4071).
          03  A-FIM                    PIC X(0020).
      
       01  ARG01                       PIC  9(008).
       01  ARG01-R                     REDEFINES ARG01.
           03  DIA                     PIC  9(002).
           03  MES                     PIC  9(002).
           03  ANO                     PIC  9(004).
      
       01  ARG02                       PIC  9(001) VALUE  0004.
       01  ARG02-1                     PIC  9(002) VALUE 1.
       01  ARG02-1-R  REDEFINES ARG02-1.
           03 IND-FERIADO              PIC  9(001).
           03 IND-DIA-SEMANA           PIC  9(001).

       01  ARG02-2                     PIC  9(005) VALUE 1.
       01  ARG03                       PIC  9(001) VALUE 2.
       01  ARG04                       PIC  9(008).

      * AREA PARA RECUPERAR TIMESTAMP E DATA
      *-----------------------------------------------------------------
       01  DADOS-SBTIMEDT.
           03  WK-CD-FUC               PIC X(03)      VALUE 'F01'.
           03  WK-AREA-TRB.
               05  WK-AREA-TRB-TAM     PIC S9(9) COMP VALUE 1024.
               05  WK-AREA-TRB-DADOS   PIC  X(1024)   VALUE ' '.
      *        X'0002' - DD.MM.AAAA-HH:MM:SS.CCCCCCXXXX
           03  WK-FMT-TS               PIC S9(4) COMP VALUE 1.
      *         X'001A'(26)
           03  WK-TAM-TS               PIC S9(4) COMP VALUE 30.
           03  WK-TS-FMTD.
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
               05  SEGUNDO             PIC  9(2).
               05  FILLER              PIC  X(1).
               05  MICRO-SEGUNDO       PIC  9(6).
               05  FILLER              PIC  X(4).




      *--------- Auxiliares para Cálculo de Datas ---------------------*

       01  DT-D-1                        PIC  9(008).
       01  DT-D-1-R    REDEFINES         DT-D-1.
           03 DIA                        PIC  9(002).
           03 MES                        PIC  9(002).
           03 ANO                        PIC  9(004).

       01  DT-PSC                        PIC  9(008).
       01  DT-PSC-R    REDEFINES         DT-PSC.
           03 DIA                        PIC  9(002).
           03 MES                        PIC  9(002).
           03 ANO                        PIC  9(004).

       01  DT-SQL-ATU                    PIC  X(10) VALUE '  .  .    '.
       01  DT-SQL-ATU-R REDEFINES        DT-SQL-ATU.
           03 DIA                        PIC  9(02).
           03 PT01                       PIC  X(01).
           03 MES                        PIC  9(02).
           03 PT02                       PIC  X(01).
           03 ANO                        PIC  9(04).


      * data de hoje -1 dia util
       01 AUX-DT-HJ-1DU               PIC  X(08) VALUE SPACES.
       01 AUX-DT-HJ-1DU-R             REDEFINES AUX-DT-HJ-1DU.
          03 ANO                       PIC  9(4).
          03 MES                       PIC  9(2).
          03 DIA                       PIC  9(2).

       01 AUX-DT-HJ-08                 PIC  X(08) VALUE SPACES.
       01 AUX-DT-HJ-08-R               REDEFINES AUX-DT-HJ-08.
          03 ANO                       PIC  9(4).
          03 MES                       PIC  9(2).
          03 DIA                       PIC  9(2).

       01 AUX-DT-PSC                   PIC  X(08) VALUE SPACES.
       01 AUX-DT-PSC-R                 REDEFINES AUX-DT-PSC.
          03 ANO                       PIC  9(4).
          03 MES                       PIC  9(2).
          03 DIA                       PIC  9(2).

       01  DATA1                       PIC  X(10)   VALUE '  .  .    '.
       01  DATA1-R  REDEFINES  DATA1.
           03  DIA                     PIC  9(02).
           03  FILLER                  PIC  X(01).
           03  MES                     PIC  9(02).
           03  FILLER                  PIC  X(01).
           03  ANO                     PIC  9(04).

       01  HORA1                        PIC X(10)   VALUE '  .  .    '.
       01  HORA1-R REDEFINES HORA1.
           07  HORA            PIC  9(002).
           07  FILLER          PIC  X(001).
           07  MINUTO          PIC  9(002).
           07  FILLER          PIC  X(001).
           07  SEGUNDO         PIC  9(002).


       01  IN-GRAVOU                   PIC  X(01)     VALUE 'N'.


      *    Validar situação do título para permitir vinculação*
      *    de posição diária de saldo provenientes do CAC

       01  WS-RECEBIMENTO-PMT            PIC X(01).
           88 RECEBIMENTO-PMT-ATIVADO    VALUE 'S'.
           88 RECEBIMENTO-PMT-DESATIVADO VALUE 'N'.
       01  TX-DCR-RECP-NAO-PMT           PIC  X(200) VALUE ' '.

      *--> Book do container de erro
       COPY IIBKERRO.
      *
      *--> Book dos dados do usuário
       COPY IIBKOPRD.
      *
      *--> Book para captura de erro via MSG1
       COPY MSGKWMG1.
      *
      *--> Variaveis para tratamento de Abend e Condition
       COPY HLPKW999.
      *
      *--> Variaveis para tratamento de CONTAINER
       COPY SGTKERR1.

      *--> Container de erro
       01  CTNR-PRM-ERRO               PIC X(16) VALUE 'PRM-ERRO'.

      *--> Container do usuário logado
       01  CTNR-PRM-INF-OPRD            PIC X(16) VALUE 'PRM-INF-OPRD'.

      *--> Areas de Controle de Programa Chamado (CONTAINER)
       01  CTL-PGM-CHMD.
           03  NM-PGM-CHMD             PIC  X(08)         VALUE ' '.
           03  FILLER                  PIC  X(01)         VALUE ' '.
           03  MSG-PGM-CHMD            PIC  X(111)        VALUE ' '.

       01  WS-ERRO-CICS.
           03  WS-RESP1                PIC  9(008).
           03  WS-RESP2                PIC  9(008).

      *--> Máscaras edição de variáveis p/ monitoração mensagens de erro
       01  MCR-S9-13                   PIC        -------------9.
       01  MCR-S9-15                   PIC      ---------------9.
       01  MCR-S9-29          PIC  ---.---.---.---.--9,99999999999999.
       01  VL-15V2                     PIC  ---.---.---.---.--9,99.

       01 GDA-CD-USU-ULT-ATL   PIC  X(08) VALUE ' '.
      *-----------------------------------------------------------------

      *--> Book pgm SGTS009A, Trata as operações encerradas se houver
       01  SGTS009A-DADOS.
       COPY SGTK009A.

      *-->  Book da subrotina SGTSPE16
       01 SGTSPE16-DADOS.
       COPY SGTKPE16.

      *-->  Book da subrotina SGTST003
       01 SGTST003-DADOS.
       COPY SGTKT003.

      *--> Book pgm SGTS0302, internalizar a posição de saldo
       01  SGTS0302-DADOS.
       COPY SGTK0302.

      *--> Book do programa
       01  SGTS251A-DADOS.
       COPY SGTK251A.


      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------

      *--> Habilita Testes no Cobolito e Debug de Operacoes IIB
       COPY ALMKCBL2.

           MOVE 0001                   TO MSG1-CD-SEQL-ERRO

      * Habilita captura automatica de abend e erros CICS
       COPY HLPKE999.

           MOVE CTE-PROG(5:8)          TO NM-PGM
                                          NM-PGM-CHMD
           MOVE 'N'    TO SGTS251A-IN-PRCD-SCS
           PERFORM 100000-OBTER-NOME-DO-CANAL
           PERFORM 970000-LER-REQUISICAO-RECEBIDA

           PERFORM 140000-VALIDAR-PARAMETROS
           PERFORM 974000-LER-DADOS-OPRD
           PERFORM 200000-PROCESSA

           MOVE 'S'    TO SGTS251A-IN-PRCD-SCS
           PERFORM 980000-EXCLUIR-CONTAINER-ERRO
           PERFORM 300000-POSTA-RESPOSTA

           PERFORM 999999-ENCERRAR
           GOBACK.
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-OBTER-NOME-DO-CANAL      SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO
           SET  ERRO-CANAL             TO  TRUE
           EXEC CICS ASSIGN
                CHANNEL ( NM-CNL )
           END-EXEC

           SET  SEM-ERRO               TO  TRUE
           .
       100000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       300000-POSTA-RESPOSTA           SECTION.
      *-----------------------------------------------------------------
           MOVE  0013                  TO  MSG1-CD-SEQL-ERRO

           EXEC CICS  PUT
                      CONTAINER ( 'SGTS251A-RPST')
                      CHANNEL   ( NM-CNL )
                      FLENGTH   ( LENGTH OF SGTS251A-RPST )
                      FROM      ( SGTS251A-RPST )
           END-EXEC
           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       970000-LER-REQUISICAO-RECEBIDA  SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                    TO MSG1-CD-SEQL-ERRO

      *--> Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *--> previsto o erro de container
           SET  ERRO-CONTAINER         TO TRUE

           MOVE ZEROS                  TO QT-CCT-LIDO
           MOVE LENGTH OF SGTS251A-RQSC      TO QT-CCT-EPRO
           EXEC CICS GET
              CONTAINER ( 'SGTS251A-RQSC' )
              CHANNEL   ( NM-CNL          )
              NODATA
              FLENGTH ( QT-CCT-LIDO )
           END-EXEC

           IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
               SET ERRO-TAMANHO-AREA   TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           EXEC CICS GET
              CONTAINER ( 'SGTS251A-RQSC' )
              CHANNEL   ( NM-CNL      )
              FLENGTH ( LENGTH OF SGTS251A-RQSC )
              INTO    ( SGTS251A-RQSC )
           END-EXEC

      * --> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       970000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       140000-VALIDAR-PARAMETROS       SECTION.
      *-----------------------------------------------------------------
           MOVE 0004                   TO MSG1-CD-SEQL-ERRO

           IF  SGTS251A-DT-IMP-PSC-SDO EQUAL SPACES
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0738               TO MSG1-CD-ERRO-TS
               MOVE 'Data importação posição saldo inválida.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS251A-DT-IMP-PSC-SDO NOT NUMERIC
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0738               TO MSG1-CD-ERRO-TS
               MOVE 'Data importação posição saldo inválida.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS251A-HR-IMP-PSC-SDO EQUAL SPACES
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0737               TO MSG1-CD-ERRO-TS
               MOVE 'Horário importação posição saldo inválido.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS251A-NR-IMP-PSC-SDO  EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 736               TO MSG1-CD-ERRO-TS
               MOVE 'Número da importação posição saldo não informado.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS251A-DT-PSC         EQUAL SPACES
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0735               TO MSG1-CD-ERRO-TS
               MOVE 'Data da posição de saldo inválida.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS251A-CD-CTRA-TIT-VL  EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 002               TO MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de título e valores mobiliários
      -        'não informado.'        TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS251A-CD-CTRA-TIT-VL  < ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0001               TO MSG1-CD-ERRO-TS
               MOVE 'Codigo da Carteira invalido.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF


           IF  SGTS251A-CD-TIT-OPR-ETRD EQUAL SPACES
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0231               TO MSG1-CD-ERRO-TS
               MOVE 'Código do título da operação estruturada não
      -       ' informado.'        TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS251A-NR-DVS-TIT     < ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0013               TO MSG1-CD-ERRO-TS
               MOVE 'Numero da Divisao do Titulo invalido.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS251A-NR-DVS-TIT     EQUAL ZEROS
               SET  ERRO-PROGRAMADO    TO TRUE
               MOVE 0232               TO MSG1-CD-ERRO-TS
               MOVE 'Número da divisão do título da operação não
      -        ' informado.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .
       140000-SAI.
           EXIT.


      *-----------------------------------------------------------------
       200000-PROCESSA                 SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO
           MOVE 4095                   TO AREA-TAM
           MOVE 'SBDATAF -WRKAREA-FIM' TO A-FIM
           MOVE 'S'               TO     IN-GRAVOU
           PERFORM 210000-INTERNALIZA

      *     Se  não existir inconsistência, ou seja, divergência
      *     entre CAC e SGT

           IF  SGTS0302-IN-INTC-CAC EQUAL 'N'

      *   Atualização da importação e sumarização feitos no SGTS0302
                PERFORM 250000-BUSCA-TS-DT
                PERFORM 260000-CALCULA-DATA
      * MOCK-POINT MCKV3
                MOVE CORR SGTS251A-DT-PSC-R TO AUX-DT-PSC-R
                IF AUX-DT-PSC EQUAL AUX-DT-HJ-1DU
                   PERFORM 270000-VER-OPR-ENC
                END-IF

                PERFORM 220000-ATU-PSC-DRIA-CTRA
              END-IF
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       210000-INTERNALIZA              SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                       TO MSG1-CD-SEQL-ERRO

           SET  SGTS0302-CNL-INTRANET      TO TRUE
           MOVE KOPRD-CD-OPRD              TO SGTS0302-CD-CHV-USU
           MOVE '  .  .    '               TO SGTS0302-DT-PSC
           MOVE CORR SGTS251A-DT-PSC-R     TO SGTS0302-DT-PSC-R
           MOVE SGTS251A-CD-CTRA-TIT-VL    TO SGTS0302-CD-CTRA-TIT-VL
           MOVE SGTS251A-CD-TIT-OPR-ETRD   TO SGTS0302-CD-TIT-OPR-ETRD
           MOVE SGTS251A-NR-DVS-TIT        TO SGTS0302-NR-DVS-TIT
           MOVE SGTS251A-DT-IMP-PSC-SDO    TO SGTS0302-DT-IMP-PSC-SDO
           MOVE SGTS251A-HR-IMP-PSC-SDO    TO SGTS0302-HR-IMP-PSC-SDO
           MOVE SGTS251A-NR-IMP-PSC-SDO    TO SGTS0302-NR-IMP-PSC-SDO
           MOVE 'N'                        TO SGTS0302-IN-PRMO-INTERN


           EXEC CICS LINK
                         PROGRAM ( SGTS0302       )
                         COMMAREA( SGTS0302-DADOS )
                         RESP    ( WS-RESP1)
                         RESP2   ( WS-RESP2)
           END-EXEC

           IF  WS-RESP1 NOT EQUAL  DFHRESP(NORMAL)
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 3004               TO MSG1-CD-ERRO-TS
               STRING 'Erro ao acionar o módulo SGTS0302.'
                      'RESP  = '    MSG1-RESP1  ', '
                      'RESP2 = '    MSG1-RESP2
                       DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
               END-STRING
               SET ERRO-PROGRAMA-ACIONADO TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF


      * MOCK-POINT MCKV1
           IF SGTS0302-CD-RTN-PGM NOT EQUAL ZEROS

              SET ERRO-PROGRAMA-ACIONADO TO TRUE

              MOVE SGTS0302-CD-RTN-PGM   TO MSG1-CD-ERRO-TS
                                            MSG1-CD-RTN-PGM-ACND
              MOVE SGTS0302-MSG-ERRO     TO MSG1-TX-ERRO-TS
                                            MSG1-TX-RTN-PGM-ACND
              MOVE SGTS0302              TO MSG1-NM-PGM-ACND
                                            NM-PGM-CHMD
              MOVE SGTS0302-CD-RTN-PGM   TO MSG1-CD-RTN-PGM-ACND
              SET MNT-VRV                TO TRUE
              PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       210000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       220000-ATU-PSC-DRIA-CTRA SECTION.
      *-----------------------------------------------------------------
           MOVE 0014                    TO MSG1-CD-SEQL-ERRO
      *    Verificar o Saldo e a Quantidade de Papeis da Carteira

           INITIALIZE SGTSPE16-DADOS
               REPLACING ALPHANUMERIC BY SPACES NUMERIC BY ZEROS

           MOVE SGTS251A-CD-CTRA-TIT-VL TO SGTSPE16-CD-CTRA-TIT-VL
           MOVE CORR SGTS251A-DT-PSC-R   TO DT-SQL-ATU-R
           MOVE DT-SQL-ATU               TO SGTSPE16-DT-PSC

           PERFORM 220001-CHAMA-SGTSPE16

      *    Se tudo ok com a verificacao de Saldo e Qt Papeis, efetuar
      *    a atualizacao.

           IF SGTSPE16-CD-RTN-PGM EQUAL ZEROS
      *    Retorno ok, atualizar Posicao da Carteira de Titulos

               INITIALIZE SGTST003-DADOS
               REPLACING ALPHANUMERIC BY SPACES NUMERIC BY ZEROS

               SET  SGTST003-ALTERAR       TO TRUE
               MOVE KOPRD-CD-OPRD          TO SGTST003-CD-CHV-USU
                                              SGTST003-CD-USU-RSP-ATL
               MOVE 'CURRENT'              TO SGTST003-TS-ULT-ATL
               MOVE SGTS251A-CD-CTRA-TIT-VL  TO SGTST003-CD-CTRA-TIT-VL
               MOVE DT-SQL-ATU             TO SGTST003-DT-PSC
               MOVE SGTSPE16-VL-SDO-CTRA   TO SGTST003-VL-SDO-CTRA
               MOVE SGTSPE16-QT-PAPL-CTRA  TO SGTST003-QT-PAPL-CTRA

               PERFORM 220002-CHAMA-SGTST003
           END-IF

           .
       220000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       220001-CHAMA-SGTSPE16 SECTION.
      *-----------------------------------------------------------------

           MOVE 0015                    TO MSG1-CD-SEQL-ERRO

           EXEC CICS LINK
                        PROGRAM ( SGTSPE16       )
                        COMMAREA( SGTSPE16-DADOS )
                        RESP    ( WS-RESP1)
                        RESP2   ( WS-RESP2)
           END-EXEC

           IF  WS-RESP1 NOT EQUAL  DFHRESP(NORMAL)
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 3004               TO MSG1-CD-ERRO-TS
               STRING 'Erro ao acionar o módulo SGTSPE16.'
                      'RESP  = '    MSG1-RESP1  ', '
                      'RESP2 = '    MSG1-RESP2
                       DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
               END-STRING
               SET ERRO-PROGRAMA-ACIONADO
                                       TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF SGTSPE16-CD-RTN-PGM NOT EQUAL ZEROS
              SET ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
              MOVE SGTSPE16-CD-RTN-PGM TO  MSG1-CD-ERRO-TS
      *        MOVE SGTSPE16-MSG-ERRO   TO  MSG1-TX-ERRO-TS
      *                                     MSG1-TX-RTN-PGM-ACND
              MOVE 'SGTSPE16-MSG-ERRO'   TO  MSG1-TX-ERRO-TS
                                           MSG1-TX-RTN-PGM-ACND

              MOVE SGTSPE16            TO  MSG1-NM-PGM-ACND
                                           NM-PGM-CHMD
              MOVE SGTSPE16-CD-RTN-PGM
                                       TO  MSG1-CD-RTN-PGM-ACND
              SET MNT-VRV              TO  TRUE
              PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       220001-SAI.
           EXIT.

      *-----------------------------------------------------------------
       220002-CHAMA-SGTST003 SECTION.
      *-----------------------------------------------------------------

           MOVE 0016                    TO MSG1-CD-SEQL-ERRO

           EXEC CICS LINK
                        PROGRAM ( SGTST003       )
                        COMMAREA( SGTST003-DADOS )
                        RESP    ( WS-RESP1)
                        RESP2   ( WS-RESP2)
           END-EXEC

           IF  WS-RESP1 NOT EQUAL  DFHRESP(NORMAL)
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 3004               TO MSG1-CD-ERRO-TS
               STRING 'Erro ao acionar o módulo SGTST003.'
                      'RESP  = '    MSG1-RESP1  ', '
                      'RESP2 = '    MSG1-RESP2
                       DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
               END-STRING
               SET ERRO-PROGRAMA-ACIONADO
                                       TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF SGTST003-CD-RTN-PGM NOT EQUAL ZEROS
              SET ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
              MOVE SGTST003-CD-RTN-PGM TO  MSG1-CD-ERRO-TS
      *        MOVE SGTST003-MSG-ERRO   TO  MSG1-TX-ERRO-TS
      *                                     MSG1-TX-RTN-PGM-ACND
              MOVE 'SGTST003-MSG-ERRO'   TO  MSG1-TX-ERRO-TS
                                           MSG1-TX-RTN-PGM-ACND
              MOVE SGTST003            TO  MSG1-NM-PGM-ACND
                                           NM-PGM-CHMD
              MOVE SGTST003-CD-RTN-PGM
                                       TO  MSG1-CD-RTN-PGM-ACND
              SET MNT-VRV              TO  TRUE
              PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       220002-SAI.
           EXIT.

      *-----------------------------------------------------------------
       250000-BUSCA-TS-DT SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO
      
           CALL SBTIMEDT IN TAB-PROGRAMAS-CHAMADOS USING WK-CD-FUC
               WK-AREA-TRB
               WK-FMT-TS
               WK-TAM-TS
               WK-TS-FMTD
      
           IF  RETURN-CODE             NOT EQUAL ZEROS
               SET  ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE RETURN-CODE        TO  MSG1-CD-RTN-PGM-ACND
               MOVE 'SBTIMEDT'         TO  MSG1-NM-PGM-ACND
                                           NM-PGM
               MOVE 0050               TO  MSG1-CD-ERRO-TS
               MOVE 'Erro ao obter data e hora atual.'
                                       TO  MSG1-TX-ERRO-TS
                                           MSG1-TX-RTN-PGM-ACND
               SET MNT-VRV             TO  TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
      
           MOVE WK-TS-FMTD             TO  TS-SQL-ATU
           MOVE CORR DT-REF-R          TO  AUX-DT-HJ-08-R
           .
       250000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       260000-CALCULA-DATA        SECTION.
      *-----------------------------------------------------------------
      *
      * calcula o dia util anterior/posterior a data do processamento
      * IND-FERIADO = 1 ----> feriado
      *             = 0 ----> nao feriado
      * IND-DIA-SEMANA = 6 ---> sabado
      *                = 7 ----> domingo
      * ARG03       = 1 adiciona qtd.dias de arg02 a data
      *             = 2 subtrai  qtd.dias de arg02 a data

              MOVE 0009                   TO    MSG1-CD-SEQL-ERRO

              MOVE CORR DT-REF-R          TO    ARG01-R
              MOVE 1                      TO    IND-FERIADO
              PERFORM UNTIL (IND-FERIADO  EQUAL 0) AND
                            (IND-DIA-SEMANA NOT EQUAL 6 AND
                             IND-DIA-SEMANA NOT EQUAL 7)
                MOVE 'F05'                TO    FUC
                CALL SBDATAF              USING FUC WKSBDATA ARG01
                                                ARG02-2 ARG03 ARG04

                 IF  RETURN-CODE           NOT EQUAL ZEROS
                     SET ERRO-PROGRAMA-ACIONADO TO  TRUE
                     MOVE  RETURN-CODE          TO  MSG1-CD-RTN-PGM-ACND
                     MOVE 0051                  TO  MSG1-CD-ERRO-TS
                     MOVE 'Erro no tratamento de datas.'
                                                TO  MSG1-TX-ERRO-TS
                                                    MSG1-TX-RTN-PGM-ACND
                     MOVE  SBDATAF              TO  MSG1-NM-PGM-ACND
                                                    NM-PGM
                     SET MNT-VRV                TO  TRUE
                     PERFORM 999000-GRAVA-ERRO-E-ENCERRA
                 ELSE
                     MOVE ARG04            TO    ARG01
                     MOVE 0                TO    ARG04 IND-FERIADO
                     MOVE 'F23'            TO    FUC
                     CALL SBDATAF          USING FUC WKSBDATA
                                                ARG01 ARG02-1

                     IF  RETURN-CODE       NOT EQUAL ZEROS
                         SET ERRO-PROGRAMA-ACIONADO
                                           TO  TRUE
                         MOVE  RETURN-CODE TO  MSG1-CD-RTN-PGM-ACND
                         MOVE 0051         TO  MSG1-CD-ERRO-TS
                         MOVE 'Erro no tratamento de datas.'
                                           TO  MSG1-TX-ERRO-TS
                                               MSG1-TX-RTN-PGM-ACND
                         MOVE  SBDATAF     TO MSG1-NM-PGM-ACND
                                              NM-PGM
                        SET MNT-VRV        TO  TRUE
                        PERFORM 999000-GRAVA-ERRO-E-ENCERRA
                     ELSE
                      IF (IND-FERIADO       EQUAL  1) OR
                         (IND-DIA-SEMANA    EQUAL  6 OR 7)
                          MOVE 0            TO     ARG04
                      END-IF
                     END-IF
                  END-IF
                END-PERFORM

               MOVE CORR  ARG01-R      TO AUX-DT-HJ-1DU-R
            .

       260000-SAI.
            EXIT.

      * ----------------------------------------------------------------
       270000-VER-OPR-ENC       SECTION.
      *-----------------------------------------------------------------
           MOVE 0010                    TO MSG1-CD-SEQL-ERRO

           MOVE KOPRD-CD-OPRD           TO SGTS009A-CD-USU-RSP-ALT
           MOVE CORR SGTS251A-DT-IMP-PSC-SDO-R
                                        TO SGTS009A-DT-IMP-PSC-SDO-R
           MOVE CORR SGTS251A-HR-IMP-PSC-SDO-R
                                        TO SGTS009A-HR-IMP-PSC-SDO-R
           MOVE SGTS251A-CD-CTRA-TIT-VL TO SGTS009A-CD-CTRA-TIT-VL

           EXEC CICS LINK
                        PROGRAM ( SGTS009A       )
                        COMMAREA( SGTS009A-DADOS )
                        RESP    ( WS-RESP1)
                        RESP2   ( WS-RESP2)
           END-EXEC

           IF  WS-RESP1 NOT EQUAL  DFHRESP(NORMAL)
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 3004               TO MSG1-CD-ERRO-TS
               STRING 'Erro ao acionar o módulo SGTS009A.'
                      'RESP  = '    MSG1-RESP1  ', '
                      'RESP2 = '    MSG1-RESP2
                       DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
               END-STRING
               SET ERRO-PROGRAMA-ACIONADO
                                       TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF SGTS009A-CD-RTN-PGM NOT EQUAL ZEROS
              SET ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
              MOVE SGTS009A-CD-RTN-PGM TO  MSG1-CD-ERRO-TS
              MOVE SGTS009A-MSG-ERRO   TO  MSG1-TX-ERRO-TS
                                           MSG1-TX-RTN-PGM-ACND
              MOVE SGTS009A            TO  MSG1-NM-PGM-ACND
                                           NM-PGM-CHMD
              MOVE SGTS009A-CD-RTN-PGM
                                       TO  MSG1-CD-RTN-PGM-ACND
              SET MNT-VRV              TO  TRUE
              PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           .

       270000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       974000-LER-DADOS-OPRD           SECTION.
      *----------------------------------------------------------------*
           MOVE 0011                   TO MSG1-CD-SEQL-ERRO

      *--> CONFIGURA DADOS PARA LEITURA DO CONTAINER DE REQUISIÇÃO
           SET  GET-CONTAINER          TO TRUE
           MOVE CTNR-PRM-INF-OPRD      TO NM-CTNR

      *--> POR CAUSA DA CAPTURA AUTOMÁTICA DE ERRO CICS, DEVE SER
      *--> PREVISTO ERRO DE CONTAINER
           SET  ERRO-CONTAINER         TO TRUE

           INITIALIZE PRM-INF-OPRD
           EXEC CICS GET
                CONTAINER ( NM-CTNR                )
                INTO      ( PRM-INF-OPRD           )
                FLENGTH   ( LENGTH OF PRM-INF-OPRD )
            END-EXEC.

      *--> AO CHEGAR AQUI, HÁ CERTEZA DE QUE NÃO HOUVE ERRO ALGUM
             SET  SEM-ERRO               TO TRUE
           .
        974000-SAI.
            EXIT.

      *----------------------------------------------------------------*
       980000-EXCLUIR-CONTAINER-ERRO   SECTION.
      *----------------------------------------------------------------*
           MOVE 0012                   TO MSG1-CD-SEQL-ERRO

      *--> Limpar variaveis container de erro
           INITIALIZE IIBKERRO-PRM-ERRO

      *--> Exclui o container de erro
           MOVE CTNR-PRM-ERRO          TO NM-CTNR
           SET DEL-CONTAINER           TO TRUE

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
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

      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------

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

      * MOCK-POINT MCKV2

           EVALUATE  NM-PGM-CHMD


      *-->>SGTS0302<<


              WHEN SGTS0302

                    MOVE  'SGTS0302-CD-CHV-USU'
                                       TO  MSGKW999-NM-VRV-MNT

                    MOVE  'SGTS0302-CD-CTRA-TIT-VL'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTS0302-CD-CTRA-TIT-VL
                                           TO    MCR-S9-15
                    MOVE  MCR-S9-15        TO    MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTS0302-CD-TIT-OPR-ETRD'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTS0302-CD-TIT-OPR-ETRD
                                           TO    MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTS0302-NR-DVS-TIT'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTS0302-NR-DVS-TIT
                                           TO    MCR-S9-15
                    MOVE  MCR-S9-15        TO    MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTS0302-DT-PSC'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTS0302-DT-PSC
                                           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTS0302-NR-VRS-OPR-ETRD'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE    SGTS0302-NR-VRS-OPR-ETRD
                                           TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTS0302-TS-SRE-HST-PU'
                                           TO  MSGKW999-NM-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTS0302-VL-TAXA-JUR-EFT'
                                           TO  MSGKW999-NM-VRV-MNT

                    MOVE  'SGTS0302-VL-DIAR-PU-ATZD'
                                           TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTS0302-VL-DIAR-PU-ATZD
                                           TO    MCR-S9-15
                    MOVE  MCR-S9-15        TO    MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTS0302-VL-DIAR-PU-CTB'
                                           TO  MSGKW999-NM-VRV-MNT

                    MOVE  'SGTS0302-VL-PU-B3'
                                           TO  MSGKW999-NM-VRV-MNT


      *-->>SGTS009A<<
               WHEN SGTS009A
                    MOVE  'SGTS009A-CD-USU-RSP-ALT'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTS009A-CD-USU-RSP-ALT
                                       TO    MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST009A-DT-IMP-PSC-SDO'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE SGTS009A-DT-IMP-PSC-SDO
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'SGTS009A-HR-IMP-PSC-SDO'
                                       TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTS009A-HR-IMP-PSC-SDO
                                       TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                WHEN SBDATAF
                    MOVE 'FUNCAO' TO MSGKW999-NM-VRV-MNT
                    MOVE FUC      TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'ARG01'  TO MSGKW999-NM-VRV-MNT
                    MOVE ARG01    TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'ARG02'  TO MSGKW999-NM-VRV-MNT
                    MOVE ARG02    TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'ARG02-1' TO MSGKW999-NM-VRV-MNT
                    MOVE ARG02-1   TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'ARG02-2' TO MSGKW999-NM-VRV-MNT
                    MOVE ARG02-2   TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'ARG03'  TO MSGKW999-NM-VRV-MNT
                    MOVE ARG03    TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'ARG04'  TO MSGKW999-NM-VRV-MNT
                    MOVE ARG04    TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'AREA-TAM' TO MSGKW999-NM-VRV-MNT
                    MOVE AREA-TAM   TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'A-FIM' TO MSGKW999-NM-VRV-MNT
                    MOVE  A-FIM   TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                WHEN  SBTIMEDT IN TAB-PROGRAMAS-CHAMADOS
                    MOVE  'WK-CD-FUC'  TO  MSGKW999-NM-VRV-MNT
                    MOVE  WK-CD-FUC    TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'WK-AREA-TRB' TO  MSGKW999-NM-VRV-MNT
                    MOVE  WK-AREA-TRB   TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'WK-FMT-TS'   TO  MSGKW999-NM-VRV-MNT
                    MOVE  WK-FMT-TS     TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'WK-TAM-TS'   TO  MSGKW999-NM-VRV-MNT
                    MOVE  WK-TAM-TS     TO   MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE  'WK-TS-FMTD'  TO  MSGKW999-NM-VRV-MNT
                    MOVE  WK-TS-FMTD    TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  OTHER
                    CONTINUE
           END-EVALUATE
           .
       999100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999200-MONITORAR-VRV-PGM-E-ENC  SECTION.
      *-----------------------------------------------------------------
      *--> Monitorar variàveis do pgm, grava e encerra processamento

           IF  MSG1-CD-ERRO-TS             EQUAL  ZEROS
              MOVE  8888               TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS             EQUAL  SPACES
               MOVE  'Erro não especificado no módulo.'
                                       TO  MSG1-TX-ERRO-TS
           END-IF

      *--> Em caso de erro, desfaz as atualizaçoes em banco de dados
           EXEC CICS SYNCPOINT
                     ROLLBACK
           END-EXEC

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
           IF  MNT-VRV


               MOVE 'SGTS251A-DT-IMP-PSC-SDO'
                                     TO  MSGKW999-NM-VRV-MNT
               MOVE SGTS251A-DT-IMP-PSC-SDO
                                     TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE  'SGTS251A-HR-IMP-PSC-SDO'
                                     TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTS251A-HR-IMP-PSC-SDO
                                     TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT


               MOVE  'SGTS251A-DT-PSC'
                                     TO   MSGKW999-NM-VRV-MNT
               MOVE  SGTS251A-DT-PSC
                                     TO   MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE  'SGTS251A-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTS251A-CD-CTRA-TIT-VL
                                       TO  MCR-S9-15
               MOVE  MCR-S9-15         TO  MSGKW999-VL-VRV-MNT

               PERFORM  999003-ADICIONA-VRV-MNT


               MOVE  'SGTS251A-CD-TIT-OPR-ETRD'
                                     TO   MSGKW999-NM-VRV-MNT
               MOVE  SGTS251A-CD-TIT-OPR-ETRD
                                     TO   MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               MOVE  'SGTS251A-NR-DVS-TIT'
                                     TO   MSGKW999-NM-VRV-MNT
               MOVE  SGTS251A-NR-DVS-TIT
                                     TO   MCR-S9-15
               MOVE  MCR-S9-15       TO   MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT



           END-IF

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
       COPY HLPKP999.

      *---------------------------------------*
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *---------------------------------------*
       COPY SGTKP999.

      *---------------------------------------*
      * MSG1-NTF-MSG                   SECTION.
      *---------------------------------------*
       COPY MSGKPMG1.

      *---------------------------------------*
       999999-ENCERRAR                 SECTION.
      *---------------------------------------*
           EXEC CICS
                RETURN
           END-EXEC
           .
       999999-SAI.
           EXIT.
