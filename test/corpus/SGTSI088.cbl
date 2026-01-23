      *-----------------------------------------------------------------
      * Programa..: SGTSI088
      * Analista..: F8650584 Roberto Fernandes Lima
      * Autores...: F8650584 Roberto Fernandes Lima
      * Data......: 06/08/2018
      * Objetivo..: Incluir TIR e Duration na tabela
      *             de Calculo de Indicador da Operacao Estruturada
      *             DB2SGT.CLC_IN_OPR_ETRD
      * Compilacao: Cobol/CICS/DB2
      *-----------------------------------------------------------------
      * VRS002 04/09/2019 - F8090555 - Aceita Duration zerada.
      * VRS001 06/08/2018 - F8650584 - DE 2016/145195 - Implantacao
      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTSI088.

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
       01  CTE-PROG     PIC  X(17) VALUE '*** SGTSI088 ***'.
       01  CTE-VERS     PIC  X(06) VALUE 'VRS002'.

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
       01  QT-REG-SELD                 PIC S9(04) COMP-5 VALUE ZEROS.
       01  CD-USU-ATL                  PIC  X(08) VALUE SPACES.

      *--> Campos Auxiliares de Data e TimeStamp
       01  TS-ATL                      PIC  X(26) VALUE SPACES.

       01  DT-ATL                      PIC  X(10) VALUE '  .  .    '.
       01  DT-ATL-R                    REDEFINES DT-ATL.
           03 DIA                      PIC  9(02).
           03 FILLER                   PIC  X(01).
           03 MES                      PIC  9(02).
           03 FILLER                   PIC  X(01).
           03 ANO                      PIC  9(04).

      *--> Variaveis para gravacao do log
       01  DADOS-LOG-DISPLAY.
           03  CD-CTRA-TIT-VL-LOG      PIC  9(10) VALUE ZEROS.
           03  CD-OPR-ETRD-MCP-LOG     PIC  9(11) VALUE ZEROS.
           03  NR-VRS-OPR-ETRD-LOG     PIC  9(05) VALUE ZEROS.
           03  TS-EXEA-CLC-LOG         PIC  X(26) VALUE SPACES.

      *--> Auxiliares para movimentação na monitoração de variáveis.
       01  CD-OPR-ETRD-MCP-TX.
           03  CD-OPR-ETRD-MCP         PIC  9(11).
       01  VL-04V10                    PIC  -.--9,9999999999.

      *--> Indicadores de nulidades
       01  QT-REG-SELD-NULO            PIC S9(04) COMP-5 VALUE ZEROS.

      *-----------------------------------------------------------------
      *    Books Utilizados
      *-----------------------------------------------------------------

      *--> Book dos dados do usuário
-INC IIBKOPRD

      *--> Book do Container de Erro
-INC IIBKERRO

      *--> Book para captura de abend cics da bloco codigo HLPKW999
-INC HLPKW999

      *--> Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1

      *--> Book para captura de erro via MSG1
-INC MSGKWMG1

      *--> Book do container de requisicao e resposta deste modulo
       01  SGTSI088-DADOS.
-INC SGTKI088

      *-----------------------------------------------------------------
      *    Book dos Containers e das subrotinas chamadas
      *-----------------------------------------------------------------

      *--> SBTIMEDT - Campos utilizados para Obtenção de Timestamp Atual
-INC HLPKTIME

      *--> Book dos dados do Log
       01  DADOS-LOG.
-INC SGTK014G

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

       01  CTE-FINAL-SS                PIC X(40) VALUE
                                       '*** S.S. Termina Aqui ***'.

      *-----------------------------------------------------------------
       PROCEDURE                       DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------

           IF  NOT EXECUCAO-ONLINE  AND
               NOT EXECUCAO-BATCH
               CALL CICS5000        USING  CICS5000-COMMAREA
           END-IF

           MOVE  0001                  TO  MSG1-CD-SEQL-ERRO

      *--> Habilita Testes no Cobolito e Debug de Operacoes IIB
-INC ALMKCBL2

      *--> Habilita tratamento automatico de Erros DB2
-INC HLPKE997

      *--> Habilita captura automatica de Abend e Erros CICS
-INC HLPKE999

           PERFORM  100000-INICIAR

           PERFORM  200000-INCLUIR

           PERFORM  300000-POSTAR-RESPOSTA

           GOBACK
           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-INICIAR                  SECTION.
      *-----------------------------------------------------------------

           PERFORM  110000-DESCOBRIR-CANAL

           PERFORM  120000-EXCLUIR-CTNR-ERRO

           PERFORM  130000-RECEBER-REQUISICAO

           PERFORM  140000-VALIDAR-PARAMETRO

           PERFORM  150000-LER-DADOS-OPRD

           PERFORM  160000-BUSCAR-TS-DT

           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-DESCOBRIR-CANAL          SECTION.
      *-----------------------------------------------------------------
           MOVE  0002                  TO  MSG1-CD-SEQL-ERRO

           EXEC CICS ASSIGN
                     CHANNEL ( NM-CNL )
           END-EXEC

           .
       110000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       120000-EXCLUIR-CTNR-ERRO        SECTION.
      *-----------------------------------------------------------------
           MOVE  0003                  TO  MSG1-CD-SEQL-ERRO

           EXEC CICS  DELETE
                      CONTAINER ( 'PRM-ERRO' )
                      CHANNEL   ( NM-CNL     )
                      NOHANDLE
           END-EXEC

           IF  NOT (EIBRESP                EQUAL  DFHRESP(NORMAL)  OR
                    EIBRESP                EQUAL  DFHRESP(CONTAINERERR))
               PERFORM  999002-TRATA-CONDITION-CICS
           END-IF
           .
       120000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       130000-RECEBER-REQUISICAO       SECTION.
      *-----------------------------------------------------------------
           MOVE  0004                  TO  MSG1-CD-SEQL-ERRO

           MOVE  ZEROS                 TO  QT-CCT-LIDO
           MOVE  LENGTH OF SGTSI088-RQSC
                                       TO  QT-CCT-EPRO

           EXEC CICS  GET
                      CONTAINER ( 'SGTSI088-RQSC' )
                      CHANNEL   ( NM-CNL          )
                      NODATA
                      FLENGTH   ( QT-CCT-LIDO     )
           END-EXEC

           IF  QT-CCT-LIDO                 NOT EQUAL  QT-CCT-EPRO
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  9001              TO  MSG1-CD-ERRO-TS
               MOVE  QT-CCT-EPRO       TO  VL-EXBR-1
               MOVE  QT-CCT-LIDO       TO  VL-EXBR-2
               STRING  'Erro no tamanho de área de requisição do '
                       NM-PGM '.'
                       ', Tamanho Esperado: '  VL-EXBR-1
                       ', Tamanho Recebido: '  VL-EXBR-2
                       DELIMITED BY SIZE
                       INTO  MSG1-TX-ERRO-TS
               END-STRING
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           EXEC CICS  GET
                      CONTAINER ( 'SGTSI088-RQSC'         )
                      CHANNEL   ( NM-CNL                  )
                      FLENGTH   ( LENGTH OF SGTSI088-RQSC )
                      INTO      ( SGTSI088-RQSC           )
           END-EXEC
           .
       130000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       140000-VALIDAR-PARAMETRO        SECTION.
      *-----------------------------------------------------------------
           MOVE  0005                  TO  MSG1-CD-SEQL-ERRO

           PERFORM  141000-VALIDAR-CAMPOS

           PERFORM  142000-VALIDAR-OPR-ETRD-MCP

           .
       140000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       141000-VALIDAR-CAMPOS           SECTION.
      *-----------------------------------------------------------------
           MOVE  0006                  TO  MSG1-CD-SEQL-ERRO

      *--> Validar Chave da Operação Estruturada
           IF  SGTSI088-CD-CTRA-TIT-VL     NEGATIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0001              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' inválido.'      TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI088-CD-CTRA-TIT-VL     EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0002              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' não informado.' TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI088-CD-OPR-ETRD-MCP    NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0003              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada do mercado de capit
      -              'ais não numérico.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI088-CD-OPR-ETRD-MCP    NEGATIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0004              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada do mercado de capit
      -              'ais inválido.'   TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI088-CD-OPR-ETRD-MCP    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0005              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada do mercado de capit
      -              'ais não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI088-NR-VRS-OPR-ETRD    NEGATIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0006              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação estruturada inválido.
      -              ''                TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI088-NR-VRS-OPR-ETRD    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0007              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação estruturada não infor
      -              'mado.'           TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *--> Validar Outros Campos de Entrada
           IF  SGTSI088-TS-EXEA-CLC        EQUAL  SPACES
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0767              TO  MSG1-CD-ERRO-TS
               MOVE  'Timestamp de execução do cálculo não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI088-DT-PSC-CLC         EQUAL  SPACES  OR
               SGTSI088-DT-PSC-CLC         EQUAL  '00000000'
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0769              TO  MSG1-CD-ERRO-TS
               MOVE  'Data da posição de cálculo não informada.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *--> Indicador de Calculo Inicial
      *    SIM - No momento do Cadastramento da Operação a TIR não
      *          será preenchida e a Duration é informada manualmente
      *    NÃO - Os cálculos serão feitos pelo SGT e os campos da TIR
      *          e Duration deverão ser preenchidos.
           IF  NOT SGTSI088-CALCULO-SISTEMA  AND
               NOT SGTSI088-DURATION-INCIAL
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0791              TO  MSG1-CD-ERRO-TS
               MOVE  'Indicador de Cálculo Inicial Inválido.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI088-CALCULO-SISTEMA
               IF  SGTSI088-PC-TIR         NOT NUMERIC
                   SET   ERRO-PROGRAMADO
                                       TO  TRUE
                   MOVE  0770          TO  MSG1-CD-ERRO-TS
                   MOVE  'Percentual da taxa interna de retorno não numé
      -                  'rico.'       TO  MSG1-TX-ERRO-TS
                   PERFORM  999000-GRAVA-ERRO-E-ENCERRA
               END-IF

               IF  SGTSI088-PC-TIR         NEGATIVE
                   SET   ERRO-PROGRAMADO
                                       TO  TRUE
                   MOVE  0771          TO  MSG1-CD-ERRO-TS
                   MOVE  'Percentual da taxa interna de retorno inválido
      -                  '.'           TO  MSG1-TX-ERRO-TS
                   PERFORM  999000-GRAVA-ERRO-E-ENCERRA
               END-IF

               IF  SGTSI088-PC-TIR         EQUAL  ZEROS
                   SET   ERRO-PROGRAMADO
                                       TO  TRUE
                   MOVE  0772          TO  MSG1-CD-ERRO-TS
                   MOVE  'Percentual da taxa interna de retorno não info
      -                  'rmado.'      TO  MSG1-TX-ERRO-TS
                   PERFORM  999000-GRAVA-ERRO-E-ENCERRA
               END-IF
           END-IF

           IF  SGTSI088-QT-AA-PZ-RTN-INVS  NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0773              TO  MSG1-CD-ERRO-TS
               MOVE  'Quantidade de anos do prazo retorno investimento n
      -              'ão numérica.'    TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI088-QT-AA-PZ-RTN-INVS  NEGATIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0774              TO  MSG1-CD-ERRO-TS
               MOVE  'Quantidade de anos do prazo retorno investimento i
      -              'nválida.'        TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

      *    IF  SGTSI088-QT-AA-PZ-RTN-INVS  EQUAL  ZEROS
      *        SET   ERRO-PROGRAMADO   TO  TRUE
      *        MOVE  0775              TO  MSG1-CD-ERRO-TS
      *        MOVE  'Quantidade de anos do prazo retorno investimento n
      *              'ão informada.'   TO  MSG1-TX-ERRO-TS
      *        PERFORM  999000-GRAVA-ERRO-E-ENCERRA
      *    END-IF

           .
       141000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       142000-VALIDAR-OPR-ETRD-MCP     SECTION.
      *-----------------------------------------------------------------
           MOVE  0007                  TO  MSG1-CD-SEQL-ERRO

           MOVE  ZEROS                 TO  QT-REG-SELD

           EXEC SQL
              SELECT 1
                INTO :QT-REG-SELD
                FROM DB2SGT.OPR_ETRD_MCP
               WHERE CD_CTRA_TIT_VL    =  :SGTSI088-CD-CTRA-TIT-VL
                 AND CD_OPR_ETRD_MCP   =  :SGTSI088-CD-OPR-ETRD-MCP
                 AND NR_VRS_OPR_ETRD   =  :SGTSI088-NR-VRS-OPR-ETRD
           END-EXEC

           IF  QT-REG-SELD                 EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0061              TO  MSG1-CD-ERRO-TS
               MOVE  'Operação estruturada inexistente.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF
           .
       142000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       150000-LER-DADOS-OPRD           SECTION.
      *-----------------------------------------------------------------
           MOVE  0008                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      PRM-INF-OPRD

           EXEC CICS  GET
                      CONTAINER ( 'PRM-INF-OPRD'                       )
                      INTO      ( PRM-INF-OPRD                         )
                      FLENGTH   ( LENGTH OF PRM-INF-OPRD               )
           END-EXEC

           IF  EXECUCAO-ONLINE
               MOVE  KOPRD-CD-OPRD     TO  CD-USU-ATL
           ELSE
               MOVE  'SISTEMA'         TO  CD-USU-ATL
           END-IF

           .
       150000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       160000-BUSCAR-TS-DT             SECTION.
      *-----------------------------------------------------------------
           MOVE  0009                  TO  MSG1-CD-SEQL-ERRO

           CALL  SBTIMEDT           USING  HLPKE001-CD-FUC
                                           HLPKE001-AREA-TRB
                                           HLPKE001-FMT-TS
                                           HLPKE001-TAM-TS
                                           HLPKE001-TS-FMTD

           IF  RETURN-CODE                 NOT EQUAL  ZEROS
               SET  ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE  0050              TO  MSG1-CD-ERRO-TS
               MOVE  'Erro na rotina para obter data e hora atual.'
                                       TO  MSG1-TX-ERRO-TS
               MOVE  SBTIMEDT          TO  MSG1-NM-PGM-ACND
               MOVE  RETURN-CODE       TO  MSG1-CD-RTN-PGM-ACND
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           IF  SGTSI088-TS-EXEA-CLC        EQUAL  'CURRENT'
               MOVE  HLPKE001-TS-FMTD  TO  TS-ATL
           ELSE
               MOVE  SGTSI088-TS-EXEA-CLC
                                       TO  TS-ATL
           END-IF

           IF  SGTSI088-DT-PSC-CLC         EQUAL  'CURRENT'
               MOVE  CORR HLPKE001-TS-FMTD
                                       TO  DT-ATL-R
           ELSE
               MOVE  CORR SGTSI088-DT-PSC-CLC-R
                                       TO  DT-ATL-R
           END-IF

           .
       160000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-INCLUIR                  SECTION.
      *-----------------------------------------------------------------
           MOVE  0010                  TO  MSG1-CD-SEQL-ERRO

      *--> Desligar erro automático do DB2
           EXEC SQL
                WHENEVER SQLERROR CONTINUE
           END-EXEC

           EXEC SQL
                INSERT INTO DB2SGT.CLC_IN_OPR_ETRD
                ( CD_CTRA_TIT_VL
                , CD_OPR_ETRD_MCP
                , NR_VRS_OPR_ETRD
                , TS_EXEA_CLC
                , DT_PSC_CLC
                , PC_TIR
                , QT_AA_PZ_RTN_INVS
                , CD_USU_ULT_ATL )
                VALUES
                ( :SGTSI088-CD-CTRA-TIT-VL
                , :SGTSI088-CD-OPR-ETRD-MCP
                , :SGTSI088-NR-VRS-OPR-ETRD
                , :TS-ATL
                , :DT-ATL
                , :SGTSI088-PC-TIR
                , :SGTSI088-QT-AA-PZ-RTN-INVS
                , :CD-USU-ATL )
           END-EXEC

      *--> Ligar Erro Automatico do DB2
           EXEC SQL WHENEVER SQLERROR
                GO TO 999010-TRATA-ERRO-DB2
           END-EXEC

           EVALUATE  SQLCODE
               WHEN  -803
                     SET   ERRO-PROGRAMADO
                                       TO  TRUE
                     MOVE  0803        TO  MSG1-CD-ERRO-TS
                     MOVE  'Inclusão não efetuada, registro existente.'
                                       TO  MSG1-TX-ERRO-TS
                     PERFORM  999000-GRAVA-ERRO-E-ENCERRA

               WHEN  ZEROS
                     CONTINUE

               WHEN  OTHER
      *--> Aciona o Tratamento de Erro Padrao para os demais casos
                     PERFORM  999010-TRATA-ERRO-DB2
           END-EVALUATE


           PERFORM  210000-GRAVAR-LOG
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       210000-GRAVAR-LOG               SECTION.
      *-----------------------------------------------------------------
           MOVE  0011                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      DADOS-LOG

           SET   SGTS014G-INCLUSAO     TO  TRUE

           MOVE  'CLC_IN_OPR_ETRD'     TO  SGTS014G-NM-ENT
           MOVE  CD-USU-ATL            TO  SGTS014G-CD-CHV-USU
           MOVE  'SGTSI088'            TO  SGTS014G-NM-PGM

           MOVE  SGTSI088-CD-CTRA-TIT-VL
                                       TO  CD-CTRA-TIT-VL-LOG
           MOVE  SGTSI088-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP-LOG
           MOVE  SGTSI088-NR-VRS-OPR-ETRD
                                       TO  NR-VRS-OPR-ETRD-LOG
           MOVE  TS-ATL                TO  TS-EXEA-CLC-LOG

           STRING  CD-CTRA-TIT-VL-LOG
                   ';'    CD-OPR-ETRD-MCP-LOG
                   ';'    NR-VRS-OPR-ETRD-LOG
                   ';'    TS-EXEA-CLC-LOG
                   DELIMITED BY SIZE
                   INTO                    SGTS014G-CHV-PRMR

           MOVE  LENGTH OF DADOS-LOG   TO  EIBCALEN

           CALL  SGTS014G           USING  DFHEIBLK DADOS-LOG

           IF  SGTS014G-CD-RTN-PGM         NOT EQUAL  ZEROS
               SET   ERRO-PROGRAMA-ACIONADO
                                       TO  TRUE
               MOVE  0112              TO  MSG1-CD-ERRO-TS
               MOVE  'Erro na gravação das informações do Log.'
                                       TO  MSG1-TX-ERRO-TS
               MOVE  SGTS014G          TO  MSG1-NM-PGM-ACND
               MOVE  SGTS014G-MSG-ERRO-PGM
                                       TO  MSG1-TX-RTN-PGM-ACND
               MOVE  SGTS014G-CD-RTN-PGM
                                       TO  MSG1-CD-RTN-PGM-ACND
               MOVE  SGTS014G-PSC-ERRO-PGM
                                       TO  MSG1-CD-SEQL-PGM-ACND
      *----->  Monitorar Chave do Programa de Log
               MOVE '014G-CHV-PRMR'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTS014G-CHV-PRMR
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               PERFORM  999000-GRAVA-ERRO-E-ENCERRA

           END-IF

           .
       210000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-POSTAR-RESPOSTA          SECTION.
      *-----------------------------------------------------------------
           MOVE  0012                  TO  MSG1-CD-SEQL-ERRO

           MOVE  'Inclusão efetuada com sucesso.'
                                       TO  SGTSI088-TX-MSG-RTN
           MOVE  'SGTSI088-RPST'       TO  NM-CTNR

           EXEC CICS  PUT
                      CONTAINER ( NM-CTNR                 )
                      CHANNEL   ( NM-CNL                  )
                      FLENGTH   ( LENGTH OF SGTSI088-RPST )
                      FROM      ( SGTSI088-RPST           )
           END-EXEC

           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------

           IF  MSG1-CD-ERRO-TS             EQUAL  ZEROS
               MOVE  8888              TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS             EQUAL  SPACES
               MOVE  'Erro não especificado no módulo.'
                                       TO  MSG1-TX-ERRO-TS
           END-IF

      *--> Adiciona Chave da Operação Estruturada
           MOVE 'I088-001-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSI088-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'I088-002-CD-OPR-ETRD-MC'
                                       TO  MSGKW999-NM-VRV-MNT
      *--> Verifica se o conteudo e numerico antes de adicionar
           IF  SGTSI088-CD-OPR-ETRD-MCP    NUMERIC
               MOVE  SGTSI088-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD-MCP
               MOVE  CD-OPR-ETRD-MCP-TX
                                       TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'I088-003-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSI088-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

      *--> Adiciona Outros Campos de Entrada
           MOVE 'I088-004-TS-EXEA-CLC'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSI088-TS-EXEA-CLC
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'I088-005-DT-PSC-CLC'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSI088-DT-PSC-CLC
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'I088-006-IN-CLC-INC'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSI088-IN-CLC-INC
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'I088-007-PC-TIR'      TO  MSGKW999-NM-VRV-MNT
           IF  SGTSI088-PC-TIR             NUMERIC
               MOVE  SGTSI088-PC-TIR
                                       TO  VL-04V10
               MOVE  VL-04V10          TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'I088-008-QT-AA-PZ-RTN-INVS'
                                       TO  MSGKW999-NM-VRV-MNT
           IF  SGTSI088-QT-AA-PZ-RTN-INVS  NUMERIC
               MOVE  SGTSI088-QT-AA-PZ-RTN-INVS
                                       TO  VL-04V10
               MOVE  VL-04V10          TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico.'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

      *--> Grava o erro no Sistema MSG
           PERFORM  MSG1-NTF-MSG

      *--> Grava o erro gerado no container de erro
           MOVE  MSG1-CD-ERRO-TS       TO  IIBKERRO-CD-ERRO
           MOVE  MSG1-TX-ERRO-TS       TO  IIBKERRO-TX-ERRO

           EXEC CICS  PUT
                      CONTAINER ( 'PRM-ERRO'                  )
                      CHANNEL   ( NM-CNL                      )
                      FROM      ( IIBKERRO-PRM-ERRO           )
                      FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                      NOHANDLE
           END-EXEC

           PERFORM  999999-ENCERRAR
           .
       999000-SAI.
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

      *----------------------------------------
       999999-ENCERRAR                 SECTION.
      *----------------------------------------

           EXEC CICS
                RETURN
           END-EXEC
           .
       999999-SAI.
           EXIT.

      *---------------------- FIM SGTSI088 -----------------------------