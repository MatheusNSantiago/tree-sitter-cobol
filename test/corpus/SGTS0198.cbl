       CBL ARITH(EXTEND)
      *----------------------------------------------------------------*
      * Programa..: SGTS0198                                           *
      * Analista..: Edilene Akemi Horita.                              *
      * Autor.....: Edilee Akemi Horita                                *
      * Data......: 20.03.2018                                         *
      * Objetivo..: Consiste a internalização dos títulos da movimen-  *
      *  tação financeira provenientes do sistema  DRIVE               *
      * Compilacao: COBOL/CICS                                         *
      *----------------------------------------------------------------*
      * VRS002 14/06/2024 F9458161  Move SGTS0198-NR-DVS-TIT para o    *
      *                             campo SGTSPE06-NR-DVS-TIT          *
      * VRS001 20/03/2018 F2634012  DE 2016/145195 - Implantação       *
      *----------------------------------------------------------------*
       IDENTIFICATION                  DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID. SGTS0198.

      *----------------------------------------------------------------*
       ENVIRONMENT                     DIVISION.
      *----------------------------------------------------------------*
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *----------------------------------------------------------------*
       DATA                            DIVISION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *--------------------> Variaveis do Sistema <--------------------*
       77  CTE-INICIO-SS               PIC  X(35)  VALUE
           '*** Storage Section Comeca Aqui ***'.
       77  CTE-PROG                    PIC  X(17)  VALUE
           '*** SGTS0198 ***'.
       77  CTE-VERS                    PIC  X(06)  VALUE 'VRS002'.

       77  SGTST009                    PIC  X(08)  VALUE 'SGTST009'.
       77  SGTSPE22                    PIC  X(08)  VALUE 'SGTSPE22'.
       77  SGTSPE06                    PIC  X(08)  VALUE 'SGTSPE06'.

      *--> Book do programa de ambiente - CICS5000
      *    Definido na work, para preservar as informacoes
-INC HLPKE01B

      *----------------------------------------------------------------*
       LOCAL-STORAGE SECTION.
      *----------------------------------------------------------------*

      *--> Book SGTST009 - CRUD  EST_OPR_ETRD
       01  SGTST009-DADOS.
-INC SGTKT009

      *--> Book SGTSPE06 - CONSULTA DE VERSAO VALIDA PARA O TITULO
       01  SGTSPE06-DADOS.
-INC SGTKPE06

      *--> Book SGTSPE22 - CONSULTA NR.SEQ RECEBIMENTO PREVISTO
       01  SGTSPE22-DADOS.
-INC SGTKPE22

      *--> Book definição dados a serem usados na subrotina de erros
-INC HLPKE001

      * Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1


      *------------------- Variaveis do Sistema -----------------------*

       01  WS-CICS.
           03  WS-RESP1                PIC  9(008).
           03  WS-RESP2                PIC  9(008).

       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP-NR      PIC  9(11).

-INC MSGKWMG1

      *--> HLPKW999
      *    Variaveis Auxiliares para Captura de Erro CICS
      *    e Adicionar Variaveis Monitoradas
       01  MSGKW999-VRV-ERRO-CICS.
           03  MSGKW999-RESP1-X8            PIC  X(08) VALUE SPACES.
           03  MSGKW999-RESP2-X8            PIC  X(08) VALUE SPACES.
       01  MSGKW999-VRV-ALINHA-NUMERO.
           03  MSGKW999-NUMERO-Z17          PIC  ZZZZZZZZZZZZZZZZ9.
           03  MSGKW999-NUMERO-X17          PIC  X(17) VALUE SPACES.
           03  MSGKW999-BRANCOS             PIC  9(08) VALUE ZEROS.
       01  MSGKW999-VRV-AUX.
           03  MSGKW999-NM-VRV-MNT     PIC  X(030) VALUE SPACES.
           03  MSGKW999-VL-VRV-MNT     PIC  X(100) VALUE SPACES.

       01 AUX-ERRO                     PIC S9(04) COMP-5 VALUE 0.

      *-----------------------------------------------------------------
       LINKAGE SECTION.
      *-----------------------------------------------------------------
       01  DFHCOMMAREA.
-INC SGTK0198


       01  CTE-SAI-SS                  PIC  X(040) VALUE
           '*** Storage Section Termina Aqui ***'.

      *----------------------------------------------------------------*
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *----------------------------------------------------------------*

       000000-PRINCIPAL                SECTION.

      * teste obrigatorio do tamanho da linkage nao retirar
           IF  EIBCALEN NOT EQUAL LENGTH OF DFHCOMMAREA
               MOVE  0220              TO  CD-RTN-PGM
               MOVE  'Área com tamanho inválido.'
                                       TO  MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           IF  NOT EXECUCAO-ONLINE     AND NOT EXECUCAO-BATCH
               CALL  CICS5000 USING CICS5000-COMMAREA
           END-IF

           MOVE  0001                  TO  PSC-ERRO-PGM


      * WHENEVER CICS   **********
-INC HLPKE003


           INITIALIZE  SGTS0198-RTN-PGM
                       SGTS0198-SAIDA

           PERFORM 100000-VALIDAR-PARAMETRO

           PERFORM 200000-VALIDAR-INTERNALIZACAO

           .
       000000-SAI.
           GOBACK.

      *----------------------------------------------------------------*
       100000-VALIDAR-PARAMETRO        SECTION.
      *--> Valida os campos de entrada do programa.
      *----------------------------------------------------------------*
           MOVE  0002                  TO  PSC-ERRO-PGM


           IF SGTS0198-CD-CTRA-TIT-VL  EQUAL ZEROS
              MOVE  0002               TO  CD-RTN-PGM
              MOVE 'Código da carteira de título e valores mobiliários n
      -            'ão informado.'     TO  MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           IF SGTS0198-CD-TIT-OPR-ETRD EQUAL SPACES
              MOVE  0231               TO  CD-RTN-PGM
              MOVE 'Código do título da operação estruturada não informa
      -            'do.'               TO  MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           IF SGTS0198-NR-DVS-TIT     <=  ZEROS
              MOVE  0013               TO  CD-RTN-PGM
              MOVE 'Número divisão do título inválido.'
                                       TO  MSG-ERRO
              PERFORM 999001-ERRO-001
           END-IF

           .

       100000-SAI.
           EXIT.

      *----------------------------------- ----------------------------*
       200000-VALIDAR-INTERNALIZACAO   SECTION.
      *----------------------------------------------------------------*
           MOVE  0003                  TO  PSC-ERRO-PGM
           INITIALIZE SGTS0198-TX-DCR-NAO-INT AUX-ERRO

           SET   SGTS0198-IN-INT-SIM   TO  TRUE
           PERFORM 910002-CHAMAR-SGTSPE06
           IF SGTSPE06-CD-OPR-ETRD-MCP EQUAL ZEROS
              OR SGTSPE06-CD-TIT-OPR-ETRD-S EQUAL SPACES
              OR SGTSPE06-NR-DVS-TIT-S EQUAL ZEROS
              SET SGTS0198-IN-INT-NAO   TO   TRUE
              MOVE 0726                 TO   AUX-ERRO
              MOVE 'Não existe operação ativa para o título informado.'
                                      TO SGTS0198-TX-DCR-NAO-INT
           ELSE
              IF SGTSPE06-CD-TIP-EST-VRS  NOT EQUAL 3
                 SET SGTS0198-IN-INT-NAO   TO   TRUE
                 MOVE 0727                 TO   AUX-ERRO
                 MOVE 'Versão da Operação não está aprovada.'
                                       TO SGTS0198-TX-DCR-NAO-INT
              END-IF
           END-IF
           IF NOT SGTS0198-IN-INT-NAO
              PERFORM 910001-CHAMAR-SGTST009
              IF SGTST009-CD-TIP-EST-OPR  EQUAL  6
                  SET SGTS0198-IN-INT-NAO  TO TRUE
                  MOVE 0728                TO   AUX-ERRO
                  MOVE 'Operação já encerrada.'
                                       TO SGTS0198-TX-DCR-NAO-INT
              ELSE
                IF   SGTST009-CD-TIP-EST-OPR  NOT EQUAL  5
                     SET SGTS0198-IN-INT-NAO   TO TRUE
                     MOVE 0729                 TO   AUX-ERRO
                     MOVE 'Operação não encarteirada'
                                       TO SGTS0198-TX-DCR-NAO-INT
                END-IF
              END-IF
           END-IF

           IF NOT SGTS0198-IN-INT-NAO
              PERFORM 910003-CHAMAR-SGTSPE22
              IF SGTSPE22-NR-SEQL-RCBT-PRVT   EQUAL ZEROS
                 SET SGTS0198-IN-INT-NAO       TO TRUE
                 MOVE 0320                     TO   AUX-ERRO
                 MOVE 'Recebimento previsto de operação estruturada não
      -               'existente'             TO SGTS0198-TX-DCR-NAO-INT

              END-IF
           END-IF

           IF EXECUCAO-ONLINE
             IF  AUX-ERRO                GREATER ZEROS
                 MOVE AUX-ERRO               TO  CD-RTN-PGM
                 MOVE SGTS0198-TX-DCR-NAO-INT
                                             TO  MSG-ERRO
                 PERFORM 999001-ERRO-001
              END-IF
           END-IF
           .

       200000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       910001-CHAMAR-SGTST009          SECTION.
      *----------------------------------------------------------------*
           MOVE  0004                  TO  PSC-ERRO-PGM
           MOVE  SGTS0198-CD-CTRA-TIT-VL
                                       TO  SGTST009-CD-CTRA-TIT-VL
           MOVE  SGTSPE06-CD-OPR-ETRD-MCP
                                       TO  SGTST009-CD-OPR-ETRD-MCP
           MOVE  SGTSPE06-NR-VRS-OPR-ETRD
                                       TO  SGTST009-NR-VRS-OPR-ETRD
           SET SGTST009-CONSULTAR-EST-ATIVO
                                       TO  TRUE

           IF EXECUCAO-ONLINE
              EXEC CICS LINK
                        PROGRAM ( SGTST009       )
                        COMMAREA( SGTST009-DADOS )
                        RESP    ( WS-RESP1)
                        RESP2   ( WS-RESP2)
              END-EXEC
              IF  WS-RESP1 NOT EQUAL  DFHRESP(NORMAL)
                  MOVE  3004           TO  CD-RTN-PGM
                  MOVE  'Erro ao acionar o módulo.'
                                       TO  MSG-ERRO
                  PERFORM 999001-ERRO-001
              END-IF
           ELSE
              MOVE LENGTH OF SGTST009-DADOS
                                       TO EIBCALEN
              CALL SGTST009 USING DFHEIBLK SGTST009-DADOS
           END-IF

           IF SGTST009-CD-RTN-PGM      NOT EQUAL ZEROS AND 100
               MOVE SGTST009-RTN-PGM   TO RTN-PGM
               PERFORM 999001-ERRO-001
           END-IF
           .

       910001-SAI.
           EXIT.

      *----------------------------------------------------------------*
       910002-CHAMAR-SGTSPE06            SECTION.
      *----------------------------------------------------------------*
           MOVE  0005                  TO  PSC-ERRO-PGM
           MOVE  SGTS0198-CD-CTRA-TIT-VL
                                       TO  SGTSPE06-CD-CTRA-TIT-VL
           MOVE  SGTS0198-CD-TIT-OPR-ETRD
                                       TO  SGTSPE06-CD-TIT-OPR-ETRD
           MOVE  SGTS0198-NR-DVS-TIT   TO  SGTSPE06-NR-DVS-TIT

           IF  EXECUCAO-ONLINE
               EXEC CICS LINK
                         PROGRAM ( SGTSPE06       )
                         COMMAREA( SGTSPE06-DADOS )
                         RESP    ( WS-RESP1)
                         RESP2   ( WS-RESP2)
               END-EXEC
               IF  WS-RESP1 NOT EQUAL  DFHRESP(NORMAL)
                   MOVE  3004          TO  CD-RTN-PGM
                   MOVE  'Erro ao acionar o módulo.'
                                       TO  MSG-ERRO
                   PERFORM 999001-ERRO-001
               END-IF
           ELSE
               MOVE LENGTH OF SGTSPE06-DADOS
                                       TO  EIBCALEN
               CALL SGTSPE06 USING DFHEIBLK SGTSPE06-DADOS
           END-IF

           IF  SGTSPE06-CD-RTN-PGM   NOT EQUAL   ZEROS   AND   100
               MOVE SGTSPE06-RTN-PGM   TO  RTN-PGM
               PERFORM 999001-ERRO-001
           ELSE
               MOVE SGTSPE06-CD-OPR-ETRD-MCP
                                       TO  SGTS0198-CD-OPR-ETRD-MCP
               MOVE SGTSPE06-NR-VRS-OPR-ETRD
                                       TO  SGTS0198-NR-VRS-OPR-ETRD
           END-IF
           .

       910002-SAI.
           EXIT.

      *----------------------------------------------------------------*
       910003-CHAMAR-SGTSPE22            SECTION.
      *----------------------------------------------------------------*
           MOVE  0006                  TO  PSC-ERRO-PGM
           MOVE  SGTS0198-CD-CTRA-TIT-VL
                                       TO  SGTSPE22-CD-CTRA-TIT-VL
           MOVE  SGTSPE06-CD-OPR-ETRD-MCP
                                       TO  SGTSPE22-CD-OPR-ETRD-MCP
           MOVE  SGTSPE06-NR-VRS-OPR-ETRD
                                       TO  SGTSPE22-NR-VRS-OPR-ETRD

           IF  EXECUCAO-ONLINE
               EXEC CICS LINK
                         PROGRAM ( SGTSPE22       )
                         COMMAREA( SGTSPE22-DADOS )
                         RESP    ( WS-RESP1)
                         RESP2   ( WS-RESP2)
               END-EXEC
               IF  WS-RESP1   NOT EQUAL   DFHRESP(NORMAL)
                   MOVE  3004          TO  CD-RTN-PGM
                   MOVE  'Erro ao acionar o módulo.'
                                       TO  MSG-ERRO
                   PERFORM 999001-ERRO-001
               END-IF
           ELSE
               MOVE LENGTH OF SGTSPE22-DADOS
                                       TO  EIBCALEN
               CALL SGTSPE22 USING DFHEIBLK SGTSPE22-DADOS
           END-IF

           IF  SGTSPE22-CD-RTN-PGM   NOT EQUAL   ZEROS   AND   100
               MOVE SGTSPE22-RTN-PGM   TO  RTN-PGM
               PERFORM 999001-ERRO-001
           ELSE
               MOVE SGTSPE22-NR-SEQL-RCBT-PRVT
                                       TO  SGTS0198-NR-SEQL-RCBT-PRVT
           END-IF
           .

       910003-SAI.
           EXIT.

      *----------------------------------------------------------------*
      * 998000-GRAVA-APPL-ERRO         SECTION.                        *
      * 999082-RECUPERA-MSG-CODIFICADA SECTION.                        *
      * 999001-ERRO-001                SECTION.                        *
      *----------------------------------------------------------------*
-INC HLPKE004
           PERFORM  999999-ENCERRAR
           .
       999001-SAI.
           EXIT.

      *----------------------------------------------------------------*
      * 999003-TRATA-ABEND-CICS        SECTION.                        *
      * 999004-TRATA-CONDITION-CICS    SECTION.                        *
      * 999005-CMPT-ERRO-CICS          SECTION.                        *
      *----------------------------------------------------------------*
-INC HLPKE006

      *-----------------------------------------------------------------
      *    As Sections e Books abaixo fazem o tratamento de erro
      *    através do Sistema MSG (rotina MSGS0001)
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       999000-TRATAR-ERRO-MSG          SECTION.
      *--> Envia a Ocorrência de Erro para o Sistema de Mensageria
      *-----------------------------------------------------------------

      *--> Alterar o MOVE conforme o Book do módulo
           MOVE  SGTS0198-TIP-RTN-PGM  TO  TIP-RTN-PGM
      *--> Alterar os Campos de Entrada conforme o Book do módulo
           PERFORM  999100-MONITORAR-ENTRADA

      *--> Movimenta Campos Básicos da Ocorrência de Erro
           MOVE  CD-RTN-PGM            TO  MSG1-CD-ERRO-TS
           MOVE  PSC-ERRO-PGM          TO  MSG1-CD-SEQL-ERRO
           MOVE  MSG-ERRO              TO  MSG1-TX-ERRO-TS

      *--> Realiza a tradução do tipo de erro do COMMAREA
      *    para tipos de erro da Mensageria
           EVALUATE  TRUE
               WHEN  ERRO-DE-CICS
                     PERFORM  999300-MONITORAR-CICS
               WHEN  ERRO-DE-PGM
                     SET  ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
                     MOVE CD-RTN-PGM   TO  MSG1-CD-RTN-PGM-ACND
                     MOVE MSG-ERRO     TO  MSG1-TX-RTN-PGM-ACND
                     MOVE PGM-ERRO     TO  MSG1-NM-PGM-ACND
                     MOVE PSC-ERRO-PGM TO  MSG1-CD-SEQL-PGM-ACND
                     PERFORM  999400-MONITORAR-VARIAVEIS
               WHEN  ERRO-CODIFICADO
                     SET  ERRO-PROGRAMADO
                                       TO TRUE
               WHEN  OTHER
                     CONTINUE
           END-EVALUATE

      *--> Grava o erro no Sistema MSG
           PERFORM  MSG1-NTF-MSG

           .
       999000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999100-MONITORAR-ENTRADA        SECTION.
      *--> Monitora as variáveis de entrada desde módulo
      *    Alterar os Campos conforme o Book
      *-----------------------------------------------------------------

           MOVE 'SGTS0198-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0198-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTS0198-CD-TIT-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0198-CD-TIT-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTS0198-NR-DVS-TIT'  TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0198-NR-DVS-TIT   TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           .
       999100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999300-MONITORAR-CICS           SECTION.
      *--> Monitora as variáveis ABEND ou CONDITION do CICS
      *-----------------------------------------------------------------

      *--> Aciona a Monitoração das Variáveis de Erro CICS no MSGS0001
           SET   ERRO-CICS             TO  TRUE

      *--> Tratar e Adicionar EIBRESP 01 e EIBRESP 02
           MOVE  EIBRESP               TO  MSG1-RESP1
           MOVE  EIBRESP2              TO  MSG1-RESP2

           MOVE  MSG1-RESP1            TO  MSGKW999-NUMERO-Z17
           PERFORM  999600-ALINHA-NUMERO-ESQUERDA
           MOVE  MSGKW999-NUMERO-X17   TO  MSGKW999-RESP1-X8

           MOVE  MSG1-RESP2            TO  MSGKW999-NUMERO-Z17
           PERFORM  999600-ALINHA-NUMERO-ESQUERDA
           MOVE  MSGKW999-NUMERO-X17    TO  MSGKW999-RESP2-X8

           MOVE 'EIBRESP 01'           TO  MSGKW999-NM-VRV-MNT
           MOVE  MSGKW999-RESP1-X8     TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'EIBRESP 02'           TO  MSGKW999-NM-VRV-MNT
           MOVE  MSGKW999-RESP2-X8     TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

      *--> Adicionar Outros Campos EIB (Exec Interface Block)
           MOVE 'EIBFN'                TO  MSGKW999-NM-VRV-MNT
           MOVE  EIBFN                 TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'EIBDS'                TO  MSGKW999-NM-VRV-MNT
           MOVE  EIBDS                 TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'EIBRSRCE'             TO  MSGKW999-NM-VRV-MNT
           MOVE  EIBRSRCE              TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'HLPS0201-TX-EIBFN'    TO  MSGKW999-NM-VRV-MNT
           MOVE  HLPS0201-TX-EIBFN     TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'HLPS0201-TX-EIBRESP'  TO  MSGKW999-NM-VRV-MNT
           MOVE  HLPS0201-TX-EIBRESP   TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'Nome CICS'            TO  MSGKW999-NM-VRV-MNT
           MOVE  HLPKE001-NM-CICS      TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'Usuario CICS'         TO  MSGKW999-NM-VRV-MNT
           MOVE  HLPKE001-CHV-USU      TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           .
       999300-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999400-MONITORAR-VARIAVEIS       SECTION.
      *--> Monitora as variáveis dos módulos chamados
      *-----------------------------------------------------------------

           EVALUATE  PGM-ERRO
               WHEN  SGTST009
                     MOVE 'SGTST009-CD-CHV-USU'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST009-CD-CHV-USU
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTST009-ACAO'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST009-ACAO
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTST009-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST009-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTST009-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                     IF  SGTST009-CD-OPR-ETRD-MCP    IS NUMERIC
                         MOVE  SGTST009-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP-NR
                         MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTST009-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST009-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTST009-TS-EST-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST009-TS-EST-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTST009-CD-USU-RSP-ATL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST009-CD-USU-RSP-ATL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTST009-CD-USU-RSP-EST'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST009-CD-USU-RSP-EST
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTST009-CD-TIP-EST-OPR'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST009-CD-TIP-EST-OPR
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTST009-TX-JST-EST'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTST009-TX-JST-EST
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

               WHEN  SGTSPE22
                     MOVE 'SGTSPE22-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSPE22-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTSPE22-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                     IF  SGTSPE22-CD-OPR-ETRD-MCP    IS NUMERIC
                         MOVE  SGTSPE22-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP-NR
                         MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTSPE22-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSPE22-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTSPE22-NR-SEQL-RCBT-PRVT'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSPE22-NR-SEQL-RCBT-PRVT
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

               WHEN  SGTSPE06
                     MOVE 'SGTSPE06-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSPE06-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTSPE06-CD-TIT-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSPE06-CD-TIT-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTSPE06-NR-DVS-TIT'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSPE06-NR-DVS-TIT
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTSPE06-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                     IF  SGTSPE06-CD-OPR-ETRD-MCP    IS NUMERIC
                         MOVE  SGTSPE06-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP-NR
                         MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTSPE06-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSPE06-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTSPE06-CD-TIP-EST-VRS'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSPE06-CD-TIP-EST-VRS
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTSPE06-CD-TIT-OPR-ETRD-S'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSPE06-CD-TIT-OPR-ETRD-S
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTSPE06-NR-DVS-TIT-S'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSPE06-NR-DVS-TIT-S
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

               WHEN  OTHER
                    CONTINUE
           END-EVALUATE
           .
       999400-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999500-ADICIONA-VRV-MNT         SECTION.
      *-----------------------------------------------------------------
           IF  MSGKW999-NM-VRV-MNT         EQUAL SPACES
               MOVE  SPACES            TO  MSGKW999-VL-VRV-MNT
           ELSE
               IF  MSG1-QT-VRV-MNT         LESS 100
                   ADD   0001          TO MSG1-QT-VRV-MNT
                   MOVE  MSGKW999-NM-VRV-MNT
                     TO  MSG1-NM-VRV-MON(MSG1-QT-VRV-MNT)
                   MOVE  MSGKW999-VL-VRV-MNT
                     TO  MSG1-VL-VRV-MON(MSG1-QT-VRV-MNT)
               END-IF
               MOVE SPACES             TO MSGKW999-NM-VRV-MNT
                                          MSGKW999-VL-VRV-MNT
           END-IF
           .
       999500-ADICIONA-VRV-MNT-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999600-ALINHA-NUMERO-ESQUERDA   SECTION.
      *-----------------------------------------------------------------
           MOVE  MSGKW999-NUMERO-Z17   TO  MSGKW999-NUMERO-X17

           UNSTRING  MSGKW999-NUMERO-X17
                     DELIMITED BY ALL ' '
                     INTO MSGKW999-BRANCOS MSGKW999-NUMERO-X17
           END-UNSTRING
           .
       999600-SAI.
           EXIT.

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
           MOVE  RTN-PGM               TO  SGTS0198-RTN-PGM

           IF  EXECUCAO-ONLINE
               PERFORM  999000-TRATAR-ERRO-MSG
           END-IF

           GOBACK
           .
       999999-SAI.
           EXIT.
      *------------------------> FIM SGTS0198 <------------------------*