       CBL ARITH(EXTEND)
      **** INSTRUÇãO QUE ALTERA O TAMANHO MáXIMO DE CAMPOS NUMERICOS
      **** NAO DELETAR
      *-----------------------------------------------------------------
      * PROGRAMA..: SGTS0050
      * ANALISTA..: F8608799 - Ricardo Ramos
      * AUTOR.....: C1284816 - Delio Miyoshi Onuki
      * DATA......: 10/11/2015
      * OBJETIVO..: Listar Versoes de Características de Operaçoes
      * COMPILACAO: COBOL/CICS
      * Tipo de artefato....: Negócio
      * Processamento.......: on-line
      * Tipo de Comunicação.: Container
      * Tratamento de Erros.: Padrão Messageria IIB
      * Expor serviço no IIB: Sim
      *-----------------------------------------------------------------
      * VRS005 10/05/2024 F8785286 Inclusão do campo
      *         SGTS0050-TX-VCLD-CRCT na  área de resposta.
      * VRS004 12/12/2019 F9188847 DE 2016/145195 - Acerto mensageria,
      * VRS003 28/02/2019 F9188847 DE 2016/145195 - Acerto mensageria,
      * VRS002 21/01/2019 F8785286 DE 2016/145195 - Inclusão de trata-
      *           mento SBDATA na section 999100-MONITORAR-VRV-PGM-CHMD.
      *        10/12/2018 F8785286 DE 2016/145195 - Inclusão dos campos
      *           SGTS0050-E-CD-CTRA-TIT-VL e SGTS0050-CD-CTRA-TIT-VL
      * VRS001 10/11/2015 C1284816  DE 2015/120821 - IMPLANTACAO
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. SGTS0050.
      *
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
      *-----------------------------------------------------------------
      *
      *-----------------------------------------------------------------
       CONFIGURATION                   SECTION.
      *-----------------------------------------------------------------
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *
      *-----------------------------------------------------------------
       DATA                            DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       WORKING-STORAGE                 SECTION.
      *-----------------------------------------------------------------
       77  CTE-INICIO-SS               PIC  X(040)  VALUE
                                       '*** S.S. COMECA AQUI ***'.
       77  CTE-PROG                    PIC  X(018)  VALUE
                                       '*** SGTS0050 ***'.
       77  CTE-VERS                    PIC  X(006)  VALUE 'VRS005'.

      *--> Definição dos Nomes dos Programas Acionados
       01 TAB-PROGRAMAS-CHAMADOS.
          03  SBDATAF                  PIC  X(08) VALUE 'SBDATAF'.
          03  SGTST036                 PIC  X(08) VALUE 'SGTST036'.
          03  SGTST037                 PIC  X(08) VALUE 'SGTST037'.
          03  SGTSL038                 PIC  X(08) VALUE 'SGTSL038'.
          03  SGTST055                 PIC  X(08) VALUE 'SGTST055'.

      *-----------------------------------------------------------------
      *                     CONTAINERS UTILIZADOS
      *-----------------------------------------------------------------
      *--> Container de ERRO
       01 CTNR-PRM-ERRO                  PIC X(16) VALUE 'PRM-ERRO'.

      *-----------------------------------------------------------------
      * Book do programa de ambiente - CICS5000
      * Definido na work para preservar as informacoes
      *-----------------------------------------------------------------
-INC HLPKE01B

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------
      *--> Areas de trabalho
       01  DT1-AAAAMMDD                   PIC  9(08).
       01  DT1-AAAAMMDD-R  REDEFINES DT1-AAAAMMDD.
          07 ANO                         PIC  9(4).
          07 MES                         PIC  9(2).
          07 DIA                         PIC  9(2).

       01  DT2-AAAAMMDD                   PIC  9(08).
       01  DT2-AAAAMMDD-R  REDEFINES DT2-AAAAMMDD.
          07 ANO                         PIC  9(4).
          07 MES                         PIC  9(2).
          07 DIA                         PIC  9(2).

       01  DT-AAAA-MM-DD                  PIC  X(10).
       01  DT-AAAA-MM-DD-R  REDEFINES DT-AAAA-MM-DD.
          07 ANO                         PIC  9(4).
          07 FIL-1                       PIC  X(1).
          07 MES                         PIC  9(2).
          07 FIL-2                       PIC  X(1).
          07 DIA                         PIC  9(2).

      *-----------------------------------------------------------------
      * Area da rotina SBDATAF
      * - FUNCAO F01: valida data gregoriana - formato DDMMAAAA(ARG02=4)
      *-----------------------------------------------------------------

       01  SBDATAF-FIM-WORKAREA        PIC  X(20) VALUE
                                                 'SBDATAF -WRKAREA-FIM'.
       01  SBDATAF-FUNCAO              PIC  X(0003) VALUE 'F01'.

       01  AREA-WORK                   PIC  X(4095).
       01  AREA-RED                    REDEFINES AREA-WORK.
           03  A-TAM                   PIC  9(0008) COMP.
           03  FILLER                  PIC  X(4071).
           03  A-FIM                   PIC  X(0020).

      *--> Argumento p/ informar a data no formato gregoriano (DDMMAAA)
       01  ARG01                       PIC  9(0008)      VALUE   0.
       01  SBDATAF-ARG01               REDEFINES ARG01.
           03 DIA                      PIC  9(02).
           03 MES                      PIC  9(02).
           03 ANO                      PIC  9(04).

      *--> Argumento da função 01, p/ validação data informada em ARG01.
       01  ARG02-F01                   PIC  9(0001)      VALUE    4.
      *-----------------------------------------------------------------

       01  QT-REG-SGTSL038               PIC S9(04) COMP VALUE ZEROS.
       01  QT-REG-TOTAL                  PIC S9(04) COMP VALUE ZEROS.

       01  LS-DADO-RQSC.
           03 LS-CD-TIP-ATB-CTRA         PIC S9(04) VALUE ZEROS.
           03 LS-CD-TIP-EST              PIC S9(04) VALUE ZEROS.

      *--> Máscaras edição de variáveis p/ monitoração mensagens de erro
       01  MCR-S9-15                     PIC    ---------------9.
       01  VL-VCLD-CRCT-CTRA-E           PIC ---------------9,99.
       01  PC-VCLD-CRCT-CTRA-E           PIC ---9,9999.

      *-----------------------------------------------------------------
      * Book dos Containers e das subrotinas chamadas
      *-----------------------------------------------------------------
      * Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1
      * Book para captura de abend cics da bloco codigo MSGKP999
-INC HLPKW999
      * Book para captura de erro via MSG1
-INC MSGKWMG1
      * Book do Container de Erro
-INC IIBKERRO

      *--> CRUD Tipo de Atributo Vinculada a Carteira
       01  SGTST036-DADOS.
-INC SGTKT036

      *--> CRUD Caracteristica da Carteira TVM
       01  SGTST037-DADOS.
-INC SGTKT037

      *--> Book Estados de Caracteristica da Carteira
       01  SGTSL038-DADOS.
-INC SGTKL038

      *--> CRUD Nota Tecnica da Operação Estruturada
       01  SGTST055-DADOS.
-INC SGTKT055

      *-->Book do container de requisição e resposta deste modulo
       01 SGTS0050-DADOS.
-INC SGTK0050

       01  CTE-FINAL-SS                  PIC X(40)      VALUE
                    '*** S.S. TERMINA AQUI ***'.

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------

      *--> Ativa a captura automatica de abend do CICS.
      *    WHENEVER CICS
-INC HLPKE999

           PERFORM 100000-INICIAR

           PERFORM 200000-PROCESSAR

           PERFORM 999999-ENCERRAR

           GOBACK
           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-INICIAR                  SECTION.
      *-----------------------------------------------------------------
           MOVE 0001                   TO MSG1-CD-SEQL-ERRO

           PERFORM 970000-LER-CANAL

      *--> Define o o nome do programa p/ fins manipulacao container
           MOVE 'SGTS0050'             TO NM-PGM
           MOVE 'SGTS0050-RPST'        TO NM-CTNR

      *--> Limpa a resposta e erro antes de executar o modulo
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO

           PERFORM 971000-LER-REQUISICAO

           PERFORM 110000-VALIDAR-REQUISICAO
           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-VALIDAR-REQUISICAO       SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO

           IF  SGTS0050-E-CD-CTRA-TIT-VL < ZEROS
                MOVE  0001              TO  MSG1-CD-ERRO-TS
                MOVE 'Código da carteira de título e valores mobiliários
      -              ' inválido.'      TO  MSG1-TX-ERRO-TS
                PERFORM 999000-GRAVA-ERRO-E-ENCERRA
            END-IF

           IF  SGTS0050-TS-EST-RTND    EQUAL SPACES
               MOVE '2900-12-31-01.00.00.000000'
                                       TO SGTS0050-TS-EST-RTND
           END-IF

           IF  SGTS0050-CD-TIP-ATB-CTRA-R < 0
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0708               TO MSG1-CD-ERRO-TS
               MOVE 'Codigo do Tipo de Atributo da Caracteristica Vincul
      -             'ada a Carteira inválido'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTS0050-CD-TIP-EST     < 0
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0077               TO MSG1-CD-ERRO-TS
               MOVE 'Código do tipo de estado da decisão inválido.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *--> Valida a data início da pesquisa informada

           IF  SGTS0050-DT-INC-PSQ     NOT EQUAL SPACES
               MOVE CORR SGTS0050-DT-INC-PSQ-R
                                       TO SBDATAF-ARG01
               PERFORM 950000-VALIDACAO-DE-DATA
               IF  RETURN-CODE         NOT EQUAL ZEROS
                   SET ERRO-PROGRAMADO TO TRUE
                   MOVE 0761           TO MSG1-CD-ERRO-TS
                   MOVE 'Data início da pesquisa inválida.'
                                       TO MSG1-TX-ERRO-TS
                   PERFORM 999000-GRAVA-ERRO-E-ENCERRA
               END-IF
           END-IF

      *--> Valida a data fim da pesquisa informada
           IF  SGTS0050-DT-FIM-PSQ     NOT EQUAL SPACES
               MOVE CORR SGTS0050-DT-FIM-PSQ-R
                                       TO SBDATAF-ARG01
               PERFORM 950000-VALIDACAO-DE-DATA
               IF  RETURN-CODE         NOT EQUAL ZEROS
                   SET ERRO-PROGRAMADO TO TRUE
                   MOVE 0762           TO MSG1-CD-ERRO-TS
                   MOVE 'Data fim da pesquisa inválida.'
                                       TO MSG1-TX-ERRO-TS
                   PERFORM 999000-GRAVA-ERRO-E-ENCERRA
               END-IF
           END-IF
           .
       110000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-PROCESSAR                SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                   TO MSG1-CD-SEQL-ERRO

           IF  SGTS0050-DT-INC-PSQ     EQUAL SPACES
           AND SGTS0050-DT-FIM-PSQ     NOT EQUAL SPACES
               MOVE '01011900'       TO SGTS0050-DT-INC-PSQ
           END-IF

           IF  SGTS0050-DT-INC-PSQ     NOT EQUAL SPACES
           AND SGTS0050-DT-FIM-PSQ     EQUAL SPACES
               MOVE '31122900'       TO SGTS0050-DT-FIM-PSQ
           END-IF

           MOVE ZEROS                  TO SGTS0050-QT-REG

           IF  SGTS0050-DT-INC-PSQ     EQUAL SPACES
           AND SGTS0050-DT-FIM-PSQ     EQUAL SPACES
               PERFORM 210000-TRATA-LISTA-SEM-DATA
           ELSE
               PERFORM 220000-TRATA-LISTA-COM-DATA
           END-IF

      *--> Movimenta dados de saida e grava container de Resposta

           PERFORM 899000-MOVIMENTAR-SAIDA
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       210000-TRATA-LISTA-SEM-DATA     SECTION.
      *-----------------------------------------------------------------
           MOVE 0004                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSL038-RTN-PGM
                                       SGTSL038-CHAVE

      *--> Movimentar o nome do programa a ser executado
           MOVE SGTSL038               TO NM-PGM

      *--> Movimentar os campos de entrada
           SET SGTSL038-LISTA-ULT-ESTADO
                                       TO TRUE
           MOVE SGTS0050-TS-EST-RTND   TO SGTSL038-TS-EST-CRCT-CTRA-ULT
           MOVE SGTS0050-E-CD-CTRA-TIT-VL
                                       TO SGTSL038-CD-CTRA-TIT-VL
           MOVE SGTS0050-CD-TIP-ATB-CTRA-R
                                       TO SGTSL038-CD-TIP-ATB-CTRA-RQSC
           MOVE SGTS0050-CD-TIP-EST    TO SGTSL038-CD-TIP-EST-DCS-RQSC

           PERFORM 930000-CHAMAR-SGTSL038

      *--> Movimentar os campos de saida
           PERFORM UNTIL SGTS0050-QT-REG
                                       EQUAL SGTSL038-QT-OCR-TAB
              OR SGTS0050-QT-REG       EQUAL 100
              ADD 1                    TO SGTS0050-QT-REG
              INITIALIZE SGTS0050-LS-CRCT-CTRA-TVM
                         (SGTS0050-QT-REG)
              MOVE SGTSL038-CD-TIP-ATB-CTRA-RPST(SGTS0050-QT-REG)
                                       TO SGTS0050-CD-TIP-ATB-CTRA
                                          (SGTS0050-QT-REG)
              MOVE SGTSL038-NR-ETRD-CRCT-CTRA   (SGTS0050-QT-REG)
                                       TO SGTS0050-NR-ETRD-CRCT-CTRA
                                          (SGTS0050-QT-REG)
              MOVE SGTSL038-CD-TIP-EST-DCS-RPST (SGTS0050-QT-REG)
                                       TO SGTS0050-CD-TIP-EST-DCS
                                          (SGTS0050-QT-REG)
              MOVE SGTSL038-TS-EST-CRCT-CTRA    (SGTS0050-QT-REG)
                                       TO SGTS0050-TS-EST-CRCT-CTRA
                                          (SGTS0050-QT-REG)
              MOVE SGTSL038-CD-CTRA-TIT-VL-RPST (SGTS0050-QT-REG)
                                       TO SGTS0050-CD-CTRA-TIT-VL
                                          (SGTS0050-QT-REG)

              PERFORM 900000-CARREGAR-DADOS
           END-PERFORM

           MOVE SGTSL038-IN-CNTC-PSQ   TO SGTS0050-IN-CTND
           .
       210000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       220000-TRATA-LISTA-COM-DATA     SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

           MOVE  SGTS0050-TS-EST-RTND(1:10)  TO  DT-AAAA-MM-DD
           MOVE  CORR  DT-AAAA-MM-DD-R       TO  DT1-AAAAMMDD-R
           MOVE  CORR  SGTS0050-DT-INC-PSQ-R TO  DT2-AAAAMMDD-R

           IF  DT1-AAAAMMDD     GREATER DT2-AAAAMMDD
           OR  DT1-AAAAMMDD     EQUAL DT2-AAAAMMDD

               MOVE ZEROS              TO QT-REG-TOTAL
               PERFORM 221000-LISTA-EST-CRCT-CTRA-PER

               IF  SGTSL038-IN-CNTC-PSQ
                                       EQUAL 'N'
                   MOVE SGTSL038-QT-OCR-TAB
                                       TO QT-REG-TOTAL
                   PERFORM 960000-LISTA-EST-CRCT-CTRA-ANT
                   ADD SGTSL038-QT-OCR-TAB
                                       TO QT-REG-TOTAL

                   IF QT-REG-TOTAL     GREATER 100
                   OR SGTSL038-IN-CNTC-PSQ
                                       EQUAL 'S'
                      MOVE 'S'         TO SGTS0050-IN-CTND
                   ELSE
                      MOVE 'N'         TO SGTS0050-IN-CTND
                   END-IF

               ELSE
                   MOVE 'S'            TO SGTS0050-IN-CTND
               END-IF

           ELSE
               PERFORM 960000-LISTA-EST-CRCT-CTRA-ANT
               MOVE SGTSL038-IN-CNTC-PSQ
                                       TO SGTS0050-IN-CTND
           END-IF
           .
       220000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       221000-LISTA-EST-CRCT-CTRA-PER  SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSL038-RTN-PGM
                                       SGTSL038-CHAVE

      *--> Movimentar o nome do programa a ser executado
           MOVE SGTSL038               TO NM-PGM

      *--> Movimentar os campos de entrada
           SET SGTSL038-LST-EST-CARACT-PER
                                       TO TRUE

           MOVE SGTS0050-E-CD-CTRA-TIT-VL
                                       TO SGTSL038-CD-CTRA-TIT-VL

           MOVE SGTS0050-TS-EST-RTND   TO SGTSL038-TS-EST-CRCT-CTRA-ULT
           MOVE SGTS0050-CD-TIP-ATB-CTRA-R
                                       TO SGTSL038-CD-TIP-ATB-CTRA-RQSC
           MOVE SGTS0050-CD-TIP-EST    TO SGTSL038-CD-TIP-EST-DCS-RQSC
           MOVE CORR SGTS0050-DT-INC-PSQ-R
                                       TO SGTSL038-DT-PSQ-R
           MOVE CORR SGTS0050-DT-FIM-PSQ-R
                                       TO SGTSL038-DT-PSQ-FIM-R

           PERFORM 930000-CHAMAR-SGTSL038

      *--> Movimentar os campos de saida
           PERFORM UNTIL SGTS0050-QT-REG
                                       EQUAL SGTSL038-QT-OCR-TAB
              OR SGTS0050-QT-REG       EQUAL 100
              ADD 1                    TO SGTS0050-QT-REG
              INITIALIZE SGTS0050-LS-CRCT-CTRA-TVM
                         (SGTS0050-QT-REG)

              MOVE SGTSL038-CD-TIP-ATB-CTRA-RPST(SGTS0050-QT-REG)
                                       TO SGTS0050-CD-TIP-ATB-CTRA
                                          (SGTS0050-QT-REG)
              MOVE SGTSL038-NR-ETRD-CRCT-CTRA   (SGTS0050-QT-REG)
                                       TO SGTS0050-NR-ETRD-CRCT-CTRA
                                          (SGTS0050-QT-REG)
              MOVE SGTSL038-CD-TIP-EST-DCS-RPST (SGTS0050-QT-REG)
                                       TO SGTS0050-CD-TIP-EST-DCS
                                          (SGTS0050-QT-REG)
              MOVE SGTSL038-TS-EST-CRCT-CTRA    (SGTS0050-QT-REG)
                                       TO SGTS0050-TS-EST-CRCT-CTRA
                                          (SGTS0050-QT-REG)
              MOVE SGTSL038-CD-CTRA-TIT-VL-RPST (SGTS0050-QT-REG)
                                       TO SGTS0050-CD-CTRA-TIT-VL
                                          (SGTS0050-QT-REG)
              PERFORM 900000-CARREGAR-DADOS
           END-PERFORM
           .
       221000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       899000-MOVIMENTAR-SAIDA         SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

           MOVE 'Consulta Versoes de Características de Operaçoes execut
      -         'ada com sucesso.'     TO SGTS0050-TX-MSG-RTND-PRCT

      *--> Gravando o container de resposta da subrotina

           PERFORM 980000-GRAVAR-CONTAINER-RSPT
           .
       899000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       900000-CARREGAR-DADOS           SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

           PERFORM 901000-ACIONA-SGTST036
           PERFORM 902000-ACIONA-SGTST037
           IF SGTST037-NR-NOTA-TCN-OPR > ZEROS
               PERFORM 903000-ACIONA-SGTST055
           END-IF
           .
       900000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       901000-ACIONA-SGTST036          SECTION.
      *-----------------------------------------------------------------
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTST036-DADOS

      *--> Movimentar os campos de entrada
           SET SGTST036-CONSULTAR      TO TRUE
           MOVE SGTS0050-CD-TIP-ATB-CTRA  (SGTS0050-QT-REG)
                                       TO SGTST036-CD-TIP-ATB-CTRA

           PERFORM 910000-CHAMAR-SGTST036

      *--> Movimentar os campos de saida
           MOVE SGTST036-TX-TIP-ATB-CTRA
                                       TO SGTS0050-TX-TIP-ATB-CTRA
                                          (SGTS0050-QT-REG)
           MOVE SGTST036-CD-TIP-ATB-CRCT
                                       TO SGTS0050-CD-TIP-ATB-CRCT
                                          (SGTS0050-QT-REG)
           .
       901000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       902000-ACIONA-SGTST037          SECTION.
      *-----------------------------------------------------------------
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTST037-DADOS

      *--> Movimentar os campos de entrada
           SET SGTST037-CONSULTAR      TO TRUE

           MOVE SGTS0050-CD-CTRA-TIT-VL (SGTS0050-QT-REG)
                                       TO SGTST037-E-CD-CTRA-TVM

           MOVE SGTS0050-CD-TIP-ATB-CTRA  (SGTS0050-QT-REG)
                                       TO SGTST037-CD-TIP-ATB-CTRA
           MOVE SGTS0050-NR-ETRD-CRCT-CTRA(SGTS0050-QT-REG)
                                       TO SGTST037-NR-ETRD-CRCT-CTRA

           PERFORM 920000-CHAMAR-SGTST037

      *--> Movimentar os campos de saida
           MOVE SGTST037-NR-NOTA-TCN-OPR
                                       TO SGTS0050-NR-NOTA-TCN-OPR
                                          (SGTS0050-QT-REG)
           EVALUATE TRUE
              WHEN SGTST037-CD-UOR-VCLD-CRCT
                                       NOT EQUAL ZEROS
                   MOVE SGTST037-CD-UOR-VCLD-CRCT
                                       TO SGTS0050-VL-VCLD-CRCT-CTRA
                                          (SGTS0050-QT-REG)
              WHEN SGTST037-VL-VCLD-CRCT-CTRA
                                       NOT EQUAL ZEROS
                   MOVE SGTST037-VL-VCLD-CRCT-CTRA
                                       TO SGTS0050-VL-VCLD-CRCT-CTRA
                                          (SGTS0050-QT-REG)
              WHEN SGTST037-PC-VCLD-CRCT-CTRA
                                       NOT EQUAL ZEROS
                   MOVE SGTST037-PC-VCLD-CRCT-CTRA
                                       TO SGTS0050-VL-VCLD-CRCT-CTRA
                                          (SGTS0050-QT-REG)
              WHEN SGTST037-TX-VCLD-CRCT
                                       NOT EQUAL SPACES
                   MOVE SGTST037-TX-VCLD-CRCT
                                       TO SGTS0050-TX-VCLD-CRCT
                                          (SGTS0050-QT-REG)
              WHEN OTHER
                   MOVE ZEROS          TO SGTS0050-VL-VCLD-CRCT-CTRA
                                          (SGTS0050-QT-REG)
           END-EVALUATE
           .
       902000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       903000-ACIONA-SGTST055          SECTION.
      *-----------------------------------------------------------------
           MOVE 0011                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTST055-DADOS

      *--> Movimentar os campos de entrada
           SET SGTST055-CONSULTAR      TO TRUE
           MOVE SGTST037-NR-NOTA-TCN-OPR
                                       TO SGTST055-NR-NOTA-TCN-OPR
           PERFORM 940000-CHAMAR-SGTST055

      *--> Movimentar os campos de saida
           MOVE SGTST055-CD-REF-NOTA-TCN
                                       TO SGTS0050-CD-REF-NOTA-TCN
                                          (SGTS0050-QT-REG)
           .
       903000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       910000-CHAMAR-SGTST036          SECTION.
      *-----------------------------------------------------------------
           MOVE 0012                   TO MSG1-CD-SEQL-ERRO

      *--> Movimento nome do módulo para monitoração de variáveis
           MOVE SGTST036               TO NM-PGM

           EXEC CICS LINK
                     PROGRAM  ( SGTST036       )
                     COMMAREA ( SGTST036-DADOS )
           END-EXEC

           IF  SGTST036-CD-RTN-PGM        NOT EQUAL ZEROS
               SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE  SGTST036-CD-RTN-PGM
                                       TO  MSG1-CD-RTN-PGM-ACND
                                           MSG1-CD-ERRO-TS
               MOVE  SGTST036-MSG-ERRO-PGM
                                       TO  MSG1-TX-RTN-PGM-ACND
                                           MSG1-TX-ERRO-TS
               MOVE  NM-PGM            TO  MSG1-NM-PGM-ACND

               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       910000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       920000-CHAMAR-SGTST037          SECTION.
      *-----------------------------------------------------------------
           MOVE 0013                   TO MSG1-CD-SEQL-ERRO

      *--> Movimento nome do módulo para monitoração de variáveis
           MOVE SGTST037               TO NM-PGM
           EXEC CICS LINK
                PROGRAM  (SGTST037)
                COMMAREA (SGTST037-DADOS)
           END-EXEC

           IF  SGTST037-CD-RTN-PGM     NOT EQUAL ZEROS
               SET ERRO-PROGRAMA-ACIONADO TO TRUE
               MOVE SGTST037-MSG-ERRO-PGM
                                       TO MSG1-TX-RTN-PGM-ACND
                                          MSG1-TX-ERRO-TS
               MOVE NM-PGM             TO MSG1-NM-PGM-ACND
               MOVE SGTST037-CD-RTN-PGM
                                       TO MSG1-CD-RTN-PGM-ACND
                                          MSG1-CD-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       920000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       930000-CHAMAR-SGTSL038          SECTION.
      *-----------------------------------------------------------------
           MOVE 0014                   TO MSG1-CD-SEQL-ERRO

      *--> Movimento nome do módulo para monitoração de variáveis
           MOVE SGTSL038               TO NM-PGM

           EXEC CICS LINK
                     PROGRAM  ( SGTSL038       )
                     COMMAREA ( SGTSL038-DADOS )
           END-EXEC

           IF  SGTSL038-CD-RTN-PGM     NOT EQUAL ZEROS
               SET ERRO-PROGRAMA-ACIONADO TO TRUE
               MOVE SGTSL038-MSG-ERRO-PGM
                                       TO MSG1-TX-RTN-PGM-ACND
                                          MSG1-TX-ERRO-TS
               MOVE NM-PGM             TO MSG1-NM-PGM-ACND
               MOVE SGTSL038-CD-RTN-PGM
                                       TO MSG1-CD-RTN-PGM-ACND
                                          MSG1-CD-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       930000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       940000-CHAMAR-SGTST055          SECTION.
      *-----------------------------------------------------------------
           MOVE 0015                   TO MSG1-CD-SEQL-ERRO

      *--> Movimenta nome do módulo para monitoração de variáveis
           MOVE SGTST055               TO NM-PGM
           EXEC CICS LINK
                PROGRAM  (SGTST055)
                COMMAREA (SGTST055-DADOS)
           END-EXEC

           IF  SGTST055-CD-RTN-PGM     NOT EQUAL ZEROS AND 100
               SET ERRO-PROGRAMA-ACIONADO TO TRUE
               MOVE SGTST055-MSG-ERRO-PGM
                                       TO MSG1-TX-RTN-PGM-ACND
                                          MSG1-TX-ERRO-TS
               MOVE NM-PGM             TO MSG1-NM-PGM-ACND
               MOVE SGTST055-CD-RTN-PGM
                                       TO MSG1-CD-RTN-PGM-ACND
                                          MSG1-CD-ERRO-TS
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       940000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       950000-VALIDACAO-DE-DATA        SECTION.
      *----------------------------------------------------------------*
           MOVE 0016                   TO MSG1-CD-SEQL-ERRO

           MOVE 4095                   TO A-TAM
           MOVE SBDATAF-FIM-WORKAREA   TO A-FIM

           CALL SBDATAF                USING SBDATAF-FUNCAO
                                             AREA-WORK
                                             ARG01
                                             ARG02-F01

           IF  RETURN-CODE             EQUAL 0 OR 24 OR 50
               CONTINUE
           ELSE
               SET  ERRO-PROGRAMA-ACIONADO
                                       TO TRUE
               MOVE 0051               TO MSG1-CD-ERRO-TS
               MOVE RETURN-CODE        TO MSG1-CD-RTN-PGM-ACND
               STRING 'Erro no tratamento de datas.'
      -               SBDATAF-FUNCAO ',' MSG1-CD-RTN-PGM-ACND '.'
                                       DELIMITED BY SIZE
                                       INTO MSG1-TX-RTN-PGM-ACND
               MOVE MSG1-TX-RTN-PGM-ACND
                                       TO MSG1-TX-ERRO-TS
               MOVE SBDATAF            TO MSG1-NM-PGM-ACND
                                          NM-PGM
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       950000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       960000-LISTA-EST-CRCT-CTRA-ANT  SECTION.
      *-----------------------------------------------------------------
           MOVE 0017                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  SGTSL038-RTN-PGM
                                       SGTSL038-CHAVE

      *--> Movimentar o nome do programa a ser executado
           MOVE SGTSL038               TO NM-PGM

      *--> Movimentar os campos de entrada
           SET SGTSL038-LST-EST-CARACT-DATA
                                       TO TRUE

           MOVE SGTS0050-E-CD-CTRA-TIT-VL
                                       TO SGTSL038-CD-CTRA-TIT-VL

           MOVE SGTS0050-TS-EST-RTND   TO SGTSL038-TS-EST-CRCT-CTRA-ULT
           MOVE SGTS0050-CD-TIP-ATB-CTRA-R
                                       TO SGTSL038-CD-TIP-ATB-CTRA-RQSC
           MOVE SGTS0050-CD-TIP-EST    TO SGTSL038-CD-TIP-EST-DCS-RQSC
           MOVE CORR SGTS0050-DT-INC-PSQ-R
                                       TO SGTSL038-DT-PSQ-R

           PERFORM 930000-CHAMAR-SGTSL038

      *--> Movimentar os campos de saida
           MOVE ZEROS                  TO QT-REG-SGTSL038
           PERFORM UNTIL QT-REG-SGTSL038
                                       EQUAL SGTSL038-QT-OCR-TAB
              OR QT-REG-SGTSL038       EQUAL 100
              OR SGTS0050-QT-REG       EQUAL 100
              ADD 1                    TO QT-REG-SGTSL038
                                          SGTS0050-QT-REG
              INITIALIZE SGTS0050-LS-CRCT-CTRA-TVM
                         (SGTS0050-QT-REG)

              MOVE SGTSL038-CD-CTRA-TIT-VL-RPST(QT-REG-SGTSL038)
                                       TO SGTS0050-CD-CTRA-TIT-VL
                                          (SGTS0050-QT-REG)
              MOVE SGTSL038-CD-TIP-ATB-CTRA-RPST(QT-REG-SGTSL038)
                                       TO SGTS0050-CD-TIP-ATB-CTRA
                                          (SGTS0050-QT-REG)
              MOVE SGTSL038-NR-ETRD-CRCT-CTRA   (QT-REG-SGTSL038)
                                       TO SGTS0050-NR-ETRD-CRCT-CTRA
                                          (SGTS0050-QT-REG)
              MOVE SGTSL038-CD-TIP-EST-DCS-RPST (QT-REG-SGTSL038)
                                       TO SGTS0050-CD-TIP-EST-DCS
                                          (SGTS0050-QT-REG)
              MOVE SGTSL038-TS-EST-CRCT-CTRA    (QT-REG-SGTSL038)
                                       TO SGTS0050-TS-EST-CRCT-CTRA
                                          (SGTS0050-QT-REG)
              PERFORM 900000-CARREGAR-DADOS
           END-PERFORM
           .
       960000-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *                                                                *
      * SECTION DA FAIXA DE 970000 A 979900 PARA LEITURA DE DADOS      *
      *                                                                *
      *-----------------------------------------------------------------
       970000-LER-CANAL                SECTION.
      *    Obtem o canal corrente
      *-----------------------------------------------------------------
           MOVE 0018                   TO MSG1-CD-SEQL-ERRO

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CANAL             TO TRUE

           EXEC CICS ASSIGN
                     CHANNEL ( NM-CNL )
           END-EXEC

      *    Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       970000-FIM.
           EXIT.

      *-----------------------------------------------------------------
       971000-LER-REQUISICAO           SECTION.
      *-----------------------------------------------------------------
           MOVE 0019                   TO MSG1-CD-SEQL-ERRO

      *--> Monta o nome do container de requisicao
           MOVE 'SGTS0050'             TO NM-PGM
           MOVE 'SGTS0050-RQSC'        TO NM-CTNR

      *--> Recupera o tamanho do container de requisição
           PERFORM 973100-LER-CONTAINER-NODATA

      *--> Obtem o tamanho esperado da requisicao
           MOVE LENGTH OF SGTS0050-RQSC TO QT-CCT-EPRO

      *--> Valida tamanho esperado da requisicao X tamanho recebido

           IF  QT-CCT-LIDO             NOT EQUAL QT-CCT-EPRO
               SET ERRO-TAMANHO-AREA   TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           MOVE 0020                   TO MSG1-CD-SEQL-ERRO

      *--> Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *--> Efetua a leitura do container
           EXEC CICS GET
                     CONTAINER (NM-CTNR)
                     FLENGTH   (QT-CCT-LIDO)
                     INTO      (SGTS0050-RQSC)
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       971000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       973100-LER-CONTAINER-NODATA     SECTION.
      *-----------------------------------------------------------------
           MOVE 0021                   TO MSG1-CD-SEQL-ERRO

      *--> Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *--> Obtem a quantidade de caracteres no container
           MOVE ZEROS                  TO QT-CCT-LIDO
           EXEC CICS GET
                     CONTAINER (NM-CTNR)
                     NODATA
                     FLENGTH   (QT-CCT-LIDO)
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       973100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       980000-GRAVAR-CONTAINER-RSPT    SECTION.
      *-----------------------------------------------------------------
           MOVE 0022                   TO MSG1-CD-SEQL-ERRO

      *--> Exclui eventual container de erro de outros programas
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO

      *--> Monta o nome do container de resposta
           MOVE 'SGTS0050-RPST'        TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE

      *Por causa da captura automática de erro CICS, deve ser previsto
      *o erro de container
           SET  ERRO-CONTAINER         TO TRUE

           EXEC CICS PUT
                     CONTAINER (NM-CTNR)
                     FROM      (SGTS0050-RPST)
                     FLENGTH   (LENGTH OF SGTS0050-RPST)
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       980000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       983000-EXCLUIR-RESPOSTA-ERRO    SECTION.
      *-----------------------------------------------------------------
      * Exclui o container de resposta e de erro para evitar problemas *
      * em modulos executados mais de uma vez dentro da mesma sessao do*
      * CICS. Equivale ao initialize na area de resposta dos programas *
      * que utilizam COMMAREA.                                         *
      *-----------------------------------------------------------------
           MOVE 0023                   TO MSG1-CD-SEQL-ERRO

           MOVE NM-PGM                 TO NM-CTNR(1:8)
           MOVE '-RPST'                TO NM-CTNR(9:5)

           SET DEL-CONTAINER           TO TRUE

      *--> Exclui o container de resposta
      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS DELETE
                     CONTAINER (NM-CTNR)
                     RESP      (MSG1-RESP1)
                     RESP2     (MSG1-RESP2)
           END-EXEC

           EVALUATE TRUE
               WHEN MSG1-RESP1         EQUAL DFHRESP(CONTAINERERR)
               WHEN MSG1-RESP1         EQUAL DFHRESP(NORMAL)
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

      *-----------------------------------------------------------------
       983100-EXCLUIR-CONTAINER-ERRO   SECTION.
      *-----------------------------------------------------------------

      *--> Limpar variaveis containerde erro
           INITIALIZE IIBKERRO-PRM-ERRO

      *--> Exclui o container de erro
           MOVE 'PRM-ERRO'             TO NM-CTNR
           SET DEL-CONTAINER           TO TRUE

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS DELETE
                     CONTAINER (NM-CTNR)
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
      * As sections abaixo iniciadas com 999000 representam os tratamen
      * tos de erro utilizando o Sistema MSG (rotina MSGS0001)
      *-----------------------------------------------------------------

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
      *--> Monitora as variáveis dos módulos chamados
      *-----------------------------------------------------------------

           EVALUATE NM-PGM
               WHEN SBDATAF
                    MOVE 'SBDATAF-FUNCAO'  TO MSGKW999-NM-VRV-MNT
                    MOVE SBDATAF-FUNCAO    TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'ARG01'           TO MSGKW999-NM-VRV-MNT
                    MOVE ARG01             TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'ARG02-F01'       TO MSGKW999-NM-VRV-MNT
                    MOVE ARG02-F01         TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTST036
                    MOVE 'SGTST036-CD-CHV-USU'
001000                                 TO MSGKW999-NM-VRV-MNT
001000              MOVE SGTST036-CD-CHV-USU
                                       TO MSGKW999-VL-VRV-MNT
001000              PERFORM 999003-ADICIONA-VRV-MNT

001000              MOVE 'SGTST036-ACAO '
001000                                 TO MSGKW999-NM-VRV-MNT
001000              MOVE SGTST036-ACAO TO MCR-S9-15
001000              MOVE MCR-S9-15     TO MSGKW999-VL-VRV-MNT
001000              PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST036-CD-TIP-ATB-CTRA'
                                           TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTST036-CD-TIP-ATB-CTRA
                                           TO MCR-S9-15
                    MOVE MCR-S9-15         TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST036-CD-TIP-ATB-CRCT'
                                           TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTST036-CD-TIP-ATB-CRCT
                                           TO MCR-S9-15
                    MOVE MCR-S9-15         TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST036-TX-TIP-ATB-CTRA'
                                           TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST036-TX-TIP-ATB-CTRA
                                           TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST036-TX-RTL-ATB-CTRA'
                                           TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTST036-TX-RTL-ATB-CTRA
                                           TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST036-TX-CMPT-ATB-CTRA'
                                           TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST036-TX-CMPT-ATB-CTRA
                                           TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST036-CD-USU-RSP-ATL'
                                           TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST036-CD-USU-RSP-ATL
                                           TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST036-IN-CRCT-GLB'
                                           TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST036-IN-CRCT-GLB
                                           TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTST037
                    MOVE 'SGTST037-CD-CHV-USU'
001000                                 TO MSGKW999-NM-VRV-MNT
001000              MOVE SGTST037-CD-CHV-USU
                                       TO MSGKW999-VL-VRV-MNT
001000              PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST037-ACAO'   TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST037-ACAO     TO MCR-S9-15
                    MOVE MCR-S9-15         TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST037-E-CD-CTRA-TVM'
                                           TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTST037-E-CD-CTRA-TVM
                                           TO MSGKW999-VL-VRV-MNT
                    MOVE MCR-S9-15         TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST037-CD-TIP-ATB-CTRA'
                                           TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTST037-CD-TIP-ATB-CTRA
                                           TO MCR-S9-15
                    MOVE MCR-S9-15         TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST037-NR-ETRD-CRCT-CTRA'
                                           TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTST037-NR-ETRD-CRCT-CTRA
                                           TO MCR-S9-15
                    MOVE MCR-S9-15         TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST037-CD-UOR-VCLD-CRCT'
                                           TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTST037-CD-UOR-VCLD-CRCT
                                           TO MCR-S9-15
                    MOVE MCR-S9-15         TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST037-VL-VCLD-CRCT-CTRA'
                                           TO MSGKW999-NM-VRV-MNT
                    IF SGTST037-VL-VCLD-CRCT-CTRA NUMERIC
                       MOVE SGTST037-VL-VCLD-CRCT-CTRA
                                           TO VL-VCLD-CRCT-CTRA-E
                       MOVE VL-VCLD-CRCT-CTRA-E
                                           TO MSGKW999-VL-VRV-MNT
                    ELSE
                       MOVE 'Valor não numérico'
                                           TO MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST037-PC-VCLD-CRCT-CTRA'
                                           TO MSGKW999-NM-VRV-MNT
                    IF SGTST037-PC-VCLD-CRCT-CTRA NUMERIC
                       MOVE SGTST037-PC-VCLD-CRCT-CTRA
                                           TO PC-VCLD-CRCT-CTRA-E
                       MOVE PC-VCLD-CRCT-CTRA-E
                                           TO MSGKW999-VL-VRV-MNT
                    ELSE
                       MOVE  'Valor não numérico'
                                           TO MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST037-NR-NOTA-TCN-OPR'
                                           TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTST037-NR-NOTA-TCN-OPR
                                           TO MCR-S9-15
                    MOVE MCR-S9-15         TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST037-CD-USU-RSP-ATL'
                                           TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST037-CD-USU-RSP-ATL
                                           TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST037-TX-VCLD-CRCT'
                                           TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST037-TX-VCLD-CRCT
                                           TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT
               WHEN SGTSL038
                    MOVE 'SGTSL038-CD-ACAO'
                                           TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL038-CD-ACAO
                                           TO MCR-S9-15
                    MOVE MCR-S9-15         TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL038-CD-CTRA-TIT-VL'
                                           TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTSL038-CD-CTRA-TIT-VL
                                            TO  MCR-S9-15
                    MOVE  MCR-S9-15    TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL038-CD-TIP-ATB-CTRA-RQSC'
                                           TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL038-CD-TIP-ATB-CTRA-RQSC
                                           TO MCR-S9-15
                    MOVE MCR-S9-15         TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL038-CD-TIP-EST-DCS-RQSC'
                                           TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL038-CD-TIP-EST-DCS-RQSC
                                           TO MCR-S9-15
                    MOVE MCR-S9-15         TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL038-TS-EST-CRCT-CTRA-ULT'
                                           TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL038-TS-EST-CRCT-CTRA-ULT
                                           TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL038-DT-PSQ' TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL038-DT-PSQ   TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSL038-DT-PSQ-FIM'
                                           TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSL038-DT-PSQ-FIM
                                           TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTST055
                    MOVE 'SGTST055-CD-CHV-USU' TO  MSGKW999-NM-VRV-MNT
                    MOVE  SGTST055-CD-CHV-USU   TO  MSGKW999-VL-VRV-MNT
                    PERFORM  999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST055-ACAO'   TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST055-ACAO     TO MCR-S9-15
                    MOVE MCR-S9-15         TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST055-NR-NOTA-TCN-OPR'
                                           TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST055-NR-NOTA-TCN-OPR
                                           TO MCR-S9-15
                    MOVE MCR-S9-15         TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST055-CD-UOR-DRTA-OGNL'
                                           TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST055-CD-UOR-DRTA-OGNL
                                           TO MCR-S9-15
                    MOVE MCR-S9-15         TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST055-CD-REF-NOTA-TCN'
                                           TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST055-CD-REF-NOTA-TCN
                                           TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST055-DT-EMS-NOTA'
                                           TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST055-DT-EMS-NOTA
                                           TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST055-DT-APVC-NOTA'
                                           TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST055-DT-APVC-NOTA
                                           TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST055-CD-ETR-DCS'
                                           TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST055-CD-ETR-DCS
                                           TO MCR-S9-15
                    MOVE MCR-S9-15         TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST055-CD-TIP-DOC-TCN'
                                           TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST055-CD-TIP-DOC-TCN
                                           TO MCR-S9-15
                    MOVE MCR-S9-15         TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST055-CD-USU-RSP-ULT-ATL'
                                           TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST055-CD-USU-RSP-ULT-ATL
                                           TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST055-TS-ULT-ATL'
                                           TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST055-TS-ULT-ATL
                                           TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN  OTHER
                     CONTINUE
           END-EVALUATE
           .
       999100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999200-MONITORAR-VRV-PGM-E-ENC  SECTION.
      *-----------------------------------------------------------------
      *--> Monitora variàveis do pgm, grava o erro e encerra

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

      *==> Observação <=================================================
      *    Normalmente se Monitora apenas as Variáveis da Requisição
      *    para Monitoração da Mensageria.
      *    O Analista que especificou o módulo e o Analista Programador
      *    tem liberdade para Monitorar outras Variáveis que forem
      *    relevantes ao Suporte do Sistema.
      *=================================================================

      *--> Adiciona as variáveis para monitoração

           MOVE '0050-001-TS-EST-RTND'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0050-TS-EST-RTND
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE '0050-002-CD-TIP-ATB-CTRA-R'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0050-CD-TIP-ATB-CTRA-R
                                       TO  MCR-S9-15
           MOVE MCR-S9-15              TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE '0050-003-CD-TIP-EST'  TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0050-CD-TIP-EST   TO  MCR-S9-15
           MOVE MCR-S9-15              TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE '0050-004-DT-INC-PSQ'  TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0050-DT-INC-PSQ   TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE '0050-005-DT-FIM-PSQ'  TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0050-DT-FIM-PSQ   TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE '0050-006-QT-REG'      TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0050-QT-REG       TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE '0050-007-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0050-E-CD-CTRA-TIT-VL
                                       TO  MCR-S9-15
           MOVE  MCR-S9-15             TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  MSG1-CD-ERRO-TS       TO  IIBKERRO-CD-ERRO
           MOVE  MSG1-TX-ERRO-TS       TO  IIBKERRO-TX-ERRO

      *--> Grava a Ocorrência de Erro no sistema MSG - Mensageria
           PERFORM MSG1-NTF-MSG
           MOVE MSG1-CD-ERRO-TS             TO IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS             TO IIBKERRO-TX-ERRO

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

      *----------------------------------------
      * 999001-TRATA-ABEND-CICS        SECTION.
      * 999002-TRATA-CONDITION-CICS    SECTION.
      * 999003-ADICIONA-VRV-MNT        SECTION.
      *----------------------------------------
-INC HLPKP999
      *----------------------------------------
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *----------------------------------------
-INC SGTKP999
      *----------------------------------------
      * MSG1-NTF-MSG                   SECTION.
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
      *---------------------- FIM SGTS0050 -----------------------------