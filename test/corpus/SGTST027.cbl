      *-----------------------------------------------------------------
      * PROGRAMA..: SGTST027
      * ANALISTA..: F6631109 - MAGALI MARIKO OYAMADA
      * AUTOR.....: F6631109 - MAGALI MARIKO OYAMADA
      * DATA......: 16.07.2015
      * OBJETIVO..: PERSISTENCIA DA TABELA TIP_COTA_TIT_OPR
      * COMPILACAO: COBOL/CICS/DB2
      *-----------------------------------------------------------------
      * VRS002 20.05.2019 F0950413 DE 2016/145195 - Inclusão na section
      *        999001-ERRO-001, '1 - Erro de db2' e '2 - erro cics' e
      *        monitoração da mensageria.
      * VRS002 13/02/2017 F8785286 DE 2016/145195 - Inclusão movimen-
      *              tação campo book para chave log.
      *        08.12.2016 - F9188847 - INCLUSAO FUNCAO 06
      * VRS001 12.08.2015 - C1077250 - INCLUIR FUNCAO 05
      *        16.07.2015 - F6631109 - IMPLANTACAO.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTST027.

      *-----------------------------------------------------------------
       ENVIRONMENT                     DIVISION.
      *-----------------------------------------------------------------
       CONFIGURATION                   SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *-----------------------------------------------------------------
       DATA                            DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       WORKING-STORAGE                 SECTION.
      *-----------------------------------------------------------------
       77  CTE-INICIO-SS               PIC  X(40) VALUE
               '*** Storage Section comeca aqui ***'.
       77  CTE-PROG                    PIC  X(17)
                                       VALUE '*** SGTST027 ***'.
       77  CTE-VERS                    PIC  X(06) VALUE 'VRS002'.

      *--> Registro da tabela
       01  REG-CACHE.
           03  CHAVE-CACHE.
               05  CD-TIP-COTA-TIT     PIC S9(4) COMP-5 VALUE 0.
               05  NR-VRS-TIP-COTA     PIC S9(4) COMP-5 VALUE 0.
               05  FILLER              PIC  X(20) VALUE ' '.
           03  DADOS-CACHE.
               05  NM-TIP-COTA-TIT     PIC  X(40) VALUE ' '.
               05  IN-TIP-COTA-ATI     PIC  X(1) VALUE ' '.

      *--> Definição dos Nomes dos Programas Acionados
       01  TAB-PROGRAMAS-CHAMADOS.
           03  SGTS014G                PIC  X(08) VALUE 'SGTS014G'.

      *--> Definição dos Variaveis do Cobolito
-INC ALMKCBL1

      *--> Book do programa de ambiente - CICS5000
      *    Definido na work para preservar as informacoes
-INC HLPKE01B

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

      *--> Area de trabalho
       01  IC-TAB                      PIC  9(2)        VALUE 0.
       01  QT-REG-SELD                 PIC S9(4) COMP-5 VALUE 0.
       01  K-ZERO                      PIC S9(4) COMP-5 VALUE 0.

      *--> Indicadores de nulidades
       01  QT-REG-SELD-NULO            PIC S9(4) COMP-5 VALUE 0.

      *--> Variaveis para gravacao do log
       01  DADOS-LOG-DISPLAY.
           03  CD-TIP-COTA-TIT-LOG     PIC  9(5) VALUE 0.
           03  NR-VRS-TIP-COTA-LOG     PIC  9(5) VALUE 0.

      *--> Book dos dados do log
       01  DADOS-LOG.
-INC SGTK014G

      * Books da definicao de dados a serem usados na subrotina de erros
-INC HLPKE001

      *--> Máscaras edição de variáveis p/ monitoração mensagens de erro
       01  MCR-S9-15                   PIC      ---------------9.

      *--> Book para captura de erro via MSG1
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
      *--> Indicador de gravação de variáveis monitoradas
       01  MSGKW999-IN-GRV-VRV         PIC  X(01)  VALUE ' '.
           88 MSGKW999-MNT-VRV                     VALUE 'S'.
           88 MSGKW999-N-MNT-VRV                   VALUE 'N'.

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

       01  CTE-FINAL-SS                PIC  X(40) VALUE
               '*** Storage Section termina aqui ***'.

      *-----------------------------------------------------------------
       LINKAGE                         SECTION.
      *-----------------------------------------------------------------
       01  DFHCOMMAREA.
-INC SGTKT027

      *-----------------------------------------------------------------
       PROCEDURE                       DIVISION USING DFHCOMMAREA.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------

           IF  EIBCALEN NOT EQUAL LENGTH OF DFHCOMMAREA
               SET   ERRO-CODIFICADO   TO TRUE
               MOVE  0220              TO  CD-RTN-PGM
               MOVE '?rea com tamanho inválido - verifique book/local.'
                                       TO  MSG-ERRO
               SET MSGKW999-MNT-VRV    TO TRUE
               PERFORM 999001-ERRO-001
           END-IF

           MOVE  0001                  TO  PSC-ERRO-PGM

           IF  NOT EXECUCAO-ONLINE  AND
               NOT EXECUCAO-BATCH
               CALL CICS5000 USING CICS5000-COMMAREA
           END-IF

      *--> Habilita Testes no Cobolito
-INC ALMKCBL2

      *--> WHENEVER DB2
-INC HLPKE002

      *--> WHENEVER CICS
-INC HLPKE003

           PERFORM 100000-VALIDA-PARAMETRO

           EVALUATE TRUE
               WHEN SGTST027-INCLUIR
                    PERFORM 400000-INCLUIR
               WHEN SGTST027-ALTERAR
                    PERFORM 500000-ALTERAR
               WHEN SGTST027-EXCLUIR
                    PERFORM 600000-EXCLUIR
               WHEN SGTST027-CONSULTAR
                    IF  CHAVE-CACHE        EQUAL  SGTST027-CHAVE
                        MOVE  DADOS-CACHE
                                       TO  SGTST027-AREA-DADOS
                    ELSE
                        PERFORM 700000-CONSULTA
                    END-IF
               WHEN SGTST027-MAX-VRS-COTA
                    PERFORM 440000-BUSCA-NR-VRS-TIP-COTA
               WHEN SGTST027-TIP-COTA-ATI-APVD
                    PERFORM 800000-CONS-TIP-COTA-ATI-APVD
           END-EVALUATE

           GOBACK
           .

      *-----------------------------------------------------------------
       100000-VALIDA-PARAMETRO         SECTION.
      *-----------------------------------------------------------------
           MOVE  0002                  TO  PSC-ERRO-PGM

           INITIALIZE                      SGTST027-RTN-PGM

           IF  NOT SGTST027-INCLUIR
           AND NOT SGTST027-EXCLUIR
           AND NOT SGTST027-ALTERAR
           AND NOT SGTST027-CONSULTAR
           AND NOT SGTST027-MAX-VRS-COTA
           AND NOT SGTST027-TIP-COTA-ATI-APVD
               SET ERRO-CODIFICADO     TO TRUE
               MOVE  0116              TO  CD-RTN-PGM
               MOVE  'Ação inválida.'  TO  MSG-ERRO
               SET MSGKW999-MNT-VRV    TO TRUE
               PERFORM  999001-ERRO-001
           END-IF

           PERFORM 130000-VALIDA-CAMPOS

           IF  SGTST027-EXCLUIR
               PERFORM 170001-VALIDA-EST-TIP-COTA-TIT
           END-IF
           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       130000-VALIDA-CAMPOS            SECTION.
      *--> A inclusão na tabela TIP_COTA_TIT_OPR, sempre será
      *    incrementado 1 para o CD-TIP-COTA-TIT e o campo de
      *    NR-VRS-TIP-COTA será movimentado valor fixo '001'
      *-----------------------------------------------------------------
           MOVE  0003                  TO  PSC-ERRO-PGM

           IF  NOT SGTST027-INCLUIR  AND
               NOT SGTST027-MAX-VRS-COTA
               IF  SGTST027-CD-TIP-COTA-TIT
                                           EQUAL  ZEROS
                   SET ERRO-CODIFICADO      TO TRUE
                   MOVE  0257               TO  CD-RTN-PGM
                   MOVE  'Código do tipo de cota do título da operação n
      -                  'ão informado.'
                                            TO  MSG-ERRO
                   SET MSGKW999-MNT-VRV     TO TRUE
                   PERFORM 999001-ERRO-001
               END-IF
               IF  SGTST027-CD-TIP-COTA-TIT
                                           NOT POSITIVE
                   SET ERRO-CODIFICADO      TO TRUE
                   MOVE  0017               TO  CD-RTN-PGM
                   MOVE  'Código do tipo de cota do título da operação i
      -                  'nválido.'
                                            TO  MSG-ERRO
                   SET MSGKW999-MNT-VRV     TO TRUE
                   PERFORM 999001-ERRO-001
               END-IF
               IF  SGTST027-NR-VRS-TIP-COTA
                                           EQUAL  ZEROS
                   SET ERRO-CODIFICADO      TO TRUE
                   MOVE  0258               TO  CD-RTN-PGM
                   MOVE  'Número da versão do tipo de cota do título não
      -                  ' informado.'
                                            TO  MSG-ERRO
                   SET MSGKW999-MNT-VRV     TO TRUE
                   PERFORM 999001-ERRO-001
               END-IF
               IF  SGTST027-NR-VRS-TIP-COTA
                                           NOT POSITIVE
                   SET ERRO-CODIFICADO      TO TRUE
                   MOVE  0018               TO  CD-RTN-PGM
                   MOVE  'Número da versão do tipo de cota do título inv
      -                  'álido.'           TO  MSG-ERRO
                   SET MSGKW999-MNT-VRV     TO TRUE
                   PERFORM 999001-ERRO-001
               END-IF
           END-IF

           IF  SGTST027-MAX-VRS-COTA
               IF  SGTST027-CD-TIP-COTA-TIT
                                           EQUAL  ZEROS
                   SET ERRO-CODIFICADO      TO TRUE
                   MOVE  0257               TO  CD-RTN-PGM
                   MOVE  'Código do tipo de cota do título da operação n
      -                  'ão informado.'
                                            TO  MSG-ERRO
                   SET MSGKW999-MNT-VRV     TO TRUE
                   PERFORM 999001-ERRO-001
               END-IF
               IF  SGTST027-CD-TIP-COTA-TIT
                                           NOT POSITIVE
                   SET ERRO-CODIFICADO      TO TRUE
                   MOVE  0017               TO  CD-RTN-PGM
                   MOVE  'Código do tipo de cota do título da operação i
      -                  'nválido.'         TO  MSG-ERRO
                   SET MSGKW999-MNT-VRV     TO TRUE
                   PERFORM 999001-ERRO-001
               END-IF
           END-IF

           IF  SGTST027-INCLUIR
           OR  SGTST027-ALTERAR
               IF  SGTST027-NM-TIP-COTA-TIT
                                           EQUAL  SPACES
                   SET ERRO-CODIFICADO      TO TRUE
                   MOVE  0259               TO  CD-RTN-PGM
                   MOVE  'Nome do tipo de cota do título da operação não
      -                  ' informado.'
                                            TO  MSG-ERRO
                   SET MSGKW999-MNT-VRV     TO TRUE
                   PERFORM 999001-ERRO-001
               END-IF
               IF  SGTST027-IN-TIP-COTA-ATI
                                           EQUAL  SPACES
                   SET ERRO-CODIFICADO      TO TRUE
                   MOVE  0260               TO  CD-RTN-PGM
                   MOVE  'Indicador do tipo de cota ativa não informado.
      -                  ''                 TO  MSG-ERRO
                   SET MSGKW999-MNT-VRV     TO TRUE
                   PERFORM 999001-ERRO-001
               END-IF
               IF  SGTST027-IN-TIP-COTA-ATI
                                           NOT EQUAL  'N'  AND  'S'
                   SET ERRO-CODIFICADO      TO TRUE
                   MOVE  0261               TO  CD-RTN-PGM
                   MOVE  'Indicador do tipo de cota ativa inválido.'
                                            TO  MSG-ERRO
                   SET MSGKW999-MNT-VRV     TO TRUE
                   PERFORM 999001-ERRO-001
               END-IF
           END-IF

           IF  SGTST027-INCLUIR
           OR  SGTST027-ALTERAR
           OR  SGTST027-EXCLUIR
               IF  SGTST027-CD-USU-RSP-ATL EQUAL  SPACES
                   SET ERRO-CODIFICADO      TO TRUE
                   MOVE  0226               TO  CD-RTN-PGM
                   MOVE  'Código usuário responsável atualização inválid
      -                  'o.'               TO  MSG-ERRO
                   SET MSGKW999-MNT-VRV     TO  TRUE
                   PERFORM 999001-ERRO-001
               END-IF
           END-IF
           .
       130000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       170001-VALIDA-EST-TIP-COTA-TIT  SECTION.
      *-----------------------------------------------------------------
           MOVE  0004                  TO  PSC-ERRO-PGM

           MOVE  ZEROS                 TO  QT-REG-SELD

           EXEC SQL
             SELECT 1
              INTO :QT-REG-SELD
               FROM DB2SGT.EST_TIP_COTA_TIT
               WHERE CD_TIP_COTA_TIT    = :SGTST027-CD-TIP-COTA-TIT
                 AND NR_VRS_TIP_COTA    = :SGTST027-NR-VRS-TIP-COTA
             FETCH FIRST 1 ROW ONLY
           END-EXEC

           IF  QT-REG-SELD                 NOT EQUAL  ZEROS
               SET ERRO-CODIFICADO     TO TRUE
               MOVE  0262              TO  CD-RTN-PGM
               MOVE  'Estado da cota vinculada no tipo de cota do título
      -              ', exclusão não permitida.'
                                       TO  MSG-ERRO
               SET MSGKW999-MNT-VRV    TO  TRUE
               PERFORM 999001-ERRO-001
           END-IF
           .
       170001-SAI.
           EXIT.

      *-----------------------------------------------------------------
       400000-INCLUIR                  SECTION.
      *-----------------------------------------------------------------
           MOVE  0005                  TO  PSC-ERRO-PGM

           IF  SGTST027-CD-TIP-COTA-TIT    EQUAL  ZEROS
               PERFORM  430000-BUSCA-SEQL-CD-TIP-COTA
           END-IF

           IF  SGTST027-NR-VRS-TIP-COTA    EQUAL  ZEROS
               PERFORM  440000-BUSCA-NR-VRS-TIP-COTA
           END-IF

           EXEC SQL
                INSERT INTO DB2SGT.TIP_COTA_TIT_OPR
                ( CD_TIP_COTA_TIT
                , NR_VRS_TIP_COTA
                , NM_TIP_COTA_TIT
                , IN_TIP_COTA_ATI )
                VALUES
                ( :SGTST027-CD-TIP-COTA-TIT
                , :SGTST027-NR-VRS-TIP-COTA
                , :SGTST027-NM-TIP-COTA-TIT
                , :SGTST027-IN-TIP-COTA-ATI )
           END-EXEC

           MOVE SGTST027-CD-TIP-COTA-TIT
                                       TO  SGTST027-CD-TIP-COTA-TIT-S
           MOVE SGTST027-NR-VRS-TIP-COTA
                                       TO  SGTST027-NR-VRS-TIP-COTA-S

           PERFORM 980101-GUARDA-CACHE

           INITIALIZE                      DADOS-LOG
           MOVE  'I'                   TO  SGTS014G-CD-TIP-ACAO

           PERFORM 980102-GRAVA-LOG
           .
       400000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       430000-BUSCA-SEQL-CD-TIP-COTA   SECTION.
      *-----------------------------------------------------------------
           MOVE  0006                  TO  PSC-ERRO-PGM

           EXEC SQL
               SELECT VALUE (MAX(CD_TIP_COTA_TIT) , 0 )
                INTO :SGTST027-CD-TIP-COTA-TIT
                FROM DB2SGT.TIP_COTA_TIT_OPR
                WHERE CD_TIP_COTA_TIT   > :K-ZERO
           END-EXEC

           ADD   0001                  TO  SGTST027-CD-TIP-COTA-TIT
           .
       430000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       440000-BUSCA-NR-VRS-TIP-COTA    SECTION.
      *-----------------------------------------------------------------
           MOVE  0007                  TO  PSC-ERRO-PGM

           EXEC SQL
               SELECT VALUE (MAX(NR_VRS_TIP_COTA) , 0 )
                INTO :SGTST027-NR-VRS-TIP-COTA
                FROM DB2SGT.TIP_COTA_TIT_OPR
                WHERE CD_TIP_COTA_TIT    = :SGTST027-CD-TIP-COTA-TIT
           END-EXEC

           IF  SQLCODE                     NOT EQUAL  ZEROS
               SET ERRO-CODIFICADO     TO TRUE
               MOVE  0263              TO  CD-RTN-PGM
               MOVE  'Erro ao recuperar último número de versão.'
                                       TO  MSG-ERRO
               SET MSGKW999-MNT-VRV    TO  TRUE
               PERFORM 999001-ERRO-001
           END-IF

           ADD  0001                   TO  SGTST027-NR-VRS-TIP-COTA
                ON SIZE ERROR
                SET ERRO-CODIFICADO    TO TRUE
                MOVE  0264             TO  CD-RTN-PGM
                MOVE  'Erro no cálculo do n mero de versão.'
                                       TO  MSG-ERRO
                SET MSGKW999-MNT-VRV   TO  TRUE
                PERFORM 999001-ERRO-001
           END-ADD

           .
       440000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       500000-ALTERAR                  SECTION.
      *-----------------------------------------------------------------
           MOVE  0008                  TO  PSC-ERRO-PGM

           EXEC SQL
             UPDATE DB2SGT.TIP_COTA_TIT_OPR
             SET NM_TIP_COTA_TIT    = :SGTST027-NM-TIP-COTA-TIT
               , IN_TIP_COTA_ATI    = :SGTST027-IN-TIP-COTA-ATI
                WHERE CD_TIP_COTA_TIT    = :SGTST027-CD-TIP-COTA-TIT
                  AND NR_VRS_TIP_COTA    = :SGTST027-NR-VRS-TIP-COTA
           END-EXEC

           IF  SQLCODE                     EQUAL  +100
               INITIALIZE                  CHAVE-CACHE
               SET ERRO-CODIFICADO     TO TRUE
               MOVE  0100              TO  CD-RTN-PGM
               MOVE  'Registro inexistente.'
                                       TO  MSG-ERRO
               SET MSGKW999-MNT-VRV    TO  TRUE
               PERFORM 999001-ERRO-001
           END-IF

           PERFORM 980101-GUARDA-CACHE

           INITIALIZE                      DADOS-LOG
           MOVE  'A'                   TO  SGTS014G-CD-TIP-ACAO

           PERFORM 980102-GRAVA-LOG
           .
       500000-SAI.
           EXIT.
      *-----------------------------------------------------------------
       600000-EXCLUIR                  SECTION.
      *-----------------------------------------------------------------
           MOVE  0009                  TO  PSC-ERRO-PGM

           INITIALIZE                      DADOS-LOG
           MOVE  'E'                   TO  SGTS014G-CD-TIP-ACAO

           PERFORM 980102-GRAVA-LOG

           EXEC SQL
                DELETE FROM DB2SGT.TIP_COTA_TIT_OPR
                WHERE CD_TIP_COTA_TIT    = :SGTST027-CD-TIP-COTA-TIT
                  AND NR_VRS_TIP_COTA    = :SGTST027-NR-VRS-TIP-COTA
           END-EXEC

           INITIALIZE                      REG-CACHE

           IF  SQLCODE                     EQUAL  +100
               SET ERRO-CODIFICADO     TO TRUE
               MOVE  0100              TO  CD-RTN-PGM
               MOVE  'Registro inexistente.'
                                       TO  MSG-ERRO
               SET MSGKW999-MNT-VRV    TO  TRUE
               PERFORM 999001-ERRO-001
           END-IF
           .
       600000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       700000-CONSULTA                 SECTION.
      *-----------------------------------------------------------------
           MOVE  0010                  TO  PSC-ERRO-PGM

           EXEC SQL
                 SELECT NM_TIP_COTA_TIT
                      , IN_TIP_COTA_ATI
                   INTO  :SGTST027-NM-TIP-COTA-TIT
                       , :SGTST027-IN-TIP-COTA-ATI
                       FROM DB2SGT.TIP_COTA_TIT_OPR
                WHERE CD_TIP_COTA_TIT    = :SGTST027-CD-TIP-COTA-TIT
                  AND NR_VRS_TIP_COTA    = :SGTST027-NR-VRS-TIP-COTA
           END-EXEC

           IF  SQLCODE                     EQUAL  +100
               INITIALIZE                  SGTST027-AREA-DADOS
               SET ERRO-CODIFICADO     TO TRUE
               MOVE  0265              TO  CD-RTN-PGM
               MOVE  'Tipo de cota título da operação inexistente.'
                                       TO  MSG-ERRO
               PERFORM 999001-ERRO-001
           END-IF

           PERFORM 980101-GUARDA-CACHE
           .
       700000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       800000-CONS-TIP-COTA-ATI-APVD   SECTION.
      *--> Verifica se Tipo de Cota é Ativa
      *    Estado da Versão do Tipo de Cota é = 0002 (Aprovado)
      *-----------------------------------------------------------------
           MOVE  0011                  TO  PSC-ERRO-PGM

           EXEC SQL
              SELECT 1
                INTO :QT-REG-SELD
                FROM DB2SGT.TIP_COTA_TIT_OPR    A
                INNER JOIN DB2SGT.EST_TIP_COTA_TIT   B
                  ON A.CD_TIP_COTA_TIT = B.CD_TIP_COTA_TIT
                 AND A.NR_VRS_TIP_COTA = B.NR_VRS_TIP_COTA
               WHERE A.CD_TIP_COTA_TIT = :SGTST027-CD-TIP-COTA-TIT
                 AND A.NR_VRS_TIP_COTA = :SGTST027-NR-VRS-TIP-COTA
                 AND B.CD_TIP_EST_COTA = 2
                 AND A.IN_TIP_COTA_ATI = 'S'
             FETCH FIRST 1 ROW ONLY
           END-EXEC

           IF  QT-REG-SELD                 EQUAL  ZEROS
               SET ERRO-CODIFICADO     TO TRUE
               MOVE  0266              TO  CD-RTN-PGM
               MOVE  'Versão do tipo de cota aprovada ativa inexistente.
      -              ''                TO  MSG-ERRO
               SET MSGKW999-MNT-VRV    TO  TRUE
               PERFORM 999001-ERRO-001
           END-IF
           .
       800000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       980101-GUARDA-CACHE             SECTION.
      *-----------------------------------------------------------------
           MOVE  0012                  TO  PSC-ERRO-PGM

           MOVE  SGTST027-CHAVE        TO  CHAVE-CACHE
           MOVE  SGTST027-AREA-DADOS   TO  DADOS-CACHE
           .
       980101-SAI.
           EXIT.

      *-----------------------------------------------------------------
       980102-GRAVA-LOG                SECTION.
      *-----------------------------------------------------------------
           MOVE  0013                  TO  PSC-ERRO-PGM

           MOVE  'TIP_COTA_TIT_OPR'    TO  SGTS014G-NM-ENT
           MOVE  'SGTST027'            TO  SGTS014G-NM-PGM
           IF  EXECUCAO-ONLINE
               MOVE  SGTST027-CD-USU-RSP-ATL
                                       TO  SGTS014G-CD-CHV-USU
           ELSE
               MOVE  'SISTEMA'         TO  SGTS014G-CD-CHV-USU
           END-IF

           MOVE  SGTST027-CD-TIP-COTA-TIT
                                       TO  CD-TIP-COTA-TIT-LOG
           MOVE  SGTST027-NR-VRS-TIP-COTA
                                       TO  NR-VRS-TIP-COTA-LOG
           STRING  CD-TIP-COTA-TIT-LOG
                   ';'    NR-VRS-TIP-COTA-LOG
                   DELIMITED BY SIZE
                   INTO                    SGTS014G-CHV-PRMR

           MOVE  LENGTH OF DADOS-LOG   TO  EIBCALEN
           CALL  SGTS014G           USING  DFHEIBLK DADOS-LOG

           IF  SGTS014G-CD-RTN-PGM         NOT EQUAL ZEROS
               SET   ERRO-DE-PGM       TO  TRUE
               SET   SGTST027-ERRO-DE-PGM
                                       TO  TRUE
               MOVE  SGTS014G          TO  PGM-ERRO
               MOVE  0112              TO  CD-RTN-PGM
               MOVE  'Erro na gravação das informações do Log.'
                                       TO  MSG-ERRO
               SET   ERRO-DE-PGM           TO  TRUE
               MOVE SGTS014G               TO  MSG1-NM-PGM-ACND
               MOVE SGTS014G-CD-RTN-PGM    TO  MSG1-CD-RTN-PGM-ACND
               MOVE SGTS014G-MSG-ERRO      TO  MSG1-TX-RTN-PGM-ACND
               SET MSGKW999-MNT-VRV        TO TRUE
               PERFORM  999001-ERRO-001
           END-IF
           .
       980102-SAI.
           EXIT.

      *--------------------
      * 999000-ERRO SECTION
      *--------------------
      *-----------------
      * 999001-ERRO-001.
      *-----------------
-INC HLPKE004
-INC HLPKE012

           PERFORM  999999-ENCERRAR
           .
       999001-SAI.
           EXIT.

      *-----------------
      * 999002-ERRO-DB2
      *-----------------
-INC HLPKE005
      *-----------------
      * 999003-TRATA-ABEND-CICS
      * 999004-TRATA-CONDITION-CICS
      *-----------------
-INC HLPKE006
      *-----------------
      *-----------------------------------------------------------------
      *    As Sections e Books abaixo fazem o tratamento de erro
      *    através do Sistema MSG (rotina MSGS0001)
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       999000-TRATAR-ERRO-MSG          SECTION.
      *--> Envia a Ocorrência de Erro para o Sistema de Mensageria
      *-----------------------------------------------------------------

      *--> Alterar o MOVE conforme o Book do módulo
           MOVE  SGTST027-TIP-RTN-PGM  TO  TIP-RTN-PGM
      *--> Alterar os Campos de Entrada conforme o Book do módulo
           PERFORM  999100-MONITORAR-ENTRADA

      *--> Movimenta Campos Básicos da Ocorrência de Erro
           MOVE  CD-RTN-PGM            TO  MSG1-CD-ERRO-TS
           MOVE  PSC-ERRO-PGM          TO  MSG1-CD-SEQL-ERRO
           MOVE  MSG-ERRO              TO  MSG1-TX-ERRO-TS

      *--> Realiza a tradução do tipo de erro do COMMAREA
      *    para tipos de erro da Mensageria
           EVALUATE  TRUE
               WHEN  ERRO-DE-DB2
                     PERFORM  999200-MONITORAR-SQL
               WHEN  ERRO-DE-CICS
                     PERFORM  999300-MONITORAR-CICS
               WHEN  ERRO-DE-PGM
                     SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
                     PERFORM  999400-MONITORAR-VARIAVEIS
               WHEN  ERRO-CODIFICADO
                     SET   ERRO-PROGRAMADO
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

           MOVE 'SGTST027-CD-CHV-USU'            TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST027-CD-CHV-USU             TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTST027-ACAO'                  TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST027-ACAO                   TO  MCR-S9-15
           MOVE  MCR-S9-15                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTST027-CD-TIP-COTA-TIT'    TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST027-CD-TIP-COTA-TIT         TO  MCR-S9-15
           MOVE  MCR-S9-15                     TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTST027-NR-VRS-TIP-COTA'       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST027-NR-VRS-TIP-COTA        TO  MCR-S9-15
           MOVE  MCR-S9-15                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTST027-CD-TIP-COTA-TIT-S'     TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST027-CD-TIP-COTA-TIT-S      TO  MCR-S9-15
           MOVE  MCR-S9-15                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTST027-NR-VRS-TIP-COTA-S'     TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST027-NR-VRS-TIP-COTA-S      TO  MCR-S9-15
           MOVE  MCR-S9-15                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTST027-NM-TIP-COTA-TIT'       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST027-NM-TIP-COTA-TIT        TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTST027-IN-TIP-COTA-ATI'       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST027-IN-TIP-COTA-ATI        TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SGTST027-CD-USU-RSP-ATL'        TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTST027-CD-USU-RSP-ATL         TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT
           .
       999100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999200-MONITORAR-SQL            SECTION.
      *--> Monitora as variáveis do SQLCA (SQL Communication Area)
      *-----------------------------------------------------------------

      *--> Aciona a Monitoração das Variáveis de Erro SQL no MSGS0001
           SET   ERRO-SQL              TO  TRUE

           MOVE  SQLCA                 TO  MSG1-SQLCA

           MOVE 'SQLCODE'              TO  MSGKW999-NM-VRV-MNT
           MOVE  SQLCODE               TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           MOVE 'SQLSTATE'             TO  MSGKW999-NM-VRV-MNT
           MOVE  SQLSTATE              TO  MSGKW999-VL-VRV-MNT
           PERFORM  999500-ADICIONA-VRV-MNT

           PERFORM  VARYING  IC-TAB
                    FROM  0001 BY 0001
                    UNTIL  IC-TAB          GREATER 0012
                    IF  HLPKE001-TAB-ERRO-TX(IC-TAB)
                                           NOT EQUAL SPACES
                        STRING  'DSNTIAR-TX-ERRO(' IC-TAB ')'
                                DELIMITED BY SIZE
                                INTO       MSGKW999-NM-VRV-MNT
                        END-STRING
                        MOVE  HLPKE001-TAB-ERRO-TX(IC-TAB)
                                       TO  MSGKW999-VL-VRV-MNT
                        PERFORM  999500-ADICIONA-VRV-MNT
                    END-IF
           END-PERFORM

           IF  SQLERRML                GREATER  ZEROS
               MOVE 'SQLERRML'         TO  MSGKW999-NM-VRV-MNT
               MOVE  SQLERRML          TO  MSGKW999-VL-VRV-MNT
               PERFORM  999500-ADICIONA-VRV-MNT
           END-IF

           IF  SQLWARN0                EQUAL 'W'
               MOVE 'SQLWARN'          TO  MSGKW999-NM-VRV-MNT
               MOVE  SQLWARN(2:7)      TO  MSGKW999-VL-VRV-MNT
               PERFORM  999500-ADICIONA-VRV-MNT
           END-IF
           .
       999200-SAI.
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
               WHEN  SGTS014G
                     MOVE  'SGTS014G-CD-TIP-ACAO'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS014G-CD-TIP-ACAO
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE  'SGTS014G-CD-CHV-USU'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS014G-CD-CHV-USU
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE  'SGTS014G-NM-PGM'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS014G-NM-PGM
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE  'SGTS014G-CHV-PRMR'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS014G-CHV-PRMR
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTS014G-PSC-ERRO-PGM'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS014G-PSC-ERRO-PGM
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTS014G-CD-RTN-PGM'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS014G-CD-RTN-PGM
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTS014G-MSG-ERRO'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS014G-MSG-ERRO
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTS014G-TIP-RTN-PGM'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS014G-TIP-RTN-PGM
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTS014G-ATB-MSG'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS014G-ATB-MSG
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999500-ADICIONA-VRV-MNT

                     MOVE 'SGTS014G-TX-IDFC-ATNT-MSG'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS014G-TX-IDFC-ATNT-MSG
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
      *    Fim do Tratamento através do Sistema MSG (rotina MSGS0001)
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       999999-ENCERRAR                 SECTION.
      *-----------------------------------------------------------------

      * 1 - Erro de db2
           IF TIP-RTN-PGM   EQUAL 1
              IF CD-RTN-PGM NOT EQUAL 100 AND -803 AND +803
                 MOVE 9997    TO   CD-RTN-PGM
              END-IF
           END-IF

      * 2 - erro cics
           IF TIP-RTN-PGM EQUAL 2
              IF CD-RTN-PGM  EQUAL ZEROS OR 333
                 MOVE 9999    TO   CD-RTN-PGM
              END-IF
           END-IF

           MOVE  RTN-PGM               TO  SGTST027-RTN-PGM

           IF  EXECUCAO-ONLINE  AND   MSGKW999-MNT-VRV
               PERFORM  999000-TRATAR-ERRO-MSG
           END-IF

           GOBACK
           .
       999999-SAI.
           EXIT.
      *---------------------- FIM SGTST027 ----------------------------