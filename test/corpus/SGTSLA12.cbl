      *-----------------------------------------------------------------
      * Programa..: SGTSLA12
      * Analista..: F2054109 - Cintia Ap Soares Iglesias
      * Autor.....: F2054109 - Cintia Ap Soares Iglesias
      * Objetivo..: Listar Carteiras TVM cuja gestão das operações
      *             esteja ativa no SGT as quais usuário tem acesso
      * COMPILACAO: 54 - PSOSE600 - Cobol 6.3 c/otimizacao p/producao
      *-----------------------------------------------------------------
      *  VRS003 23/07/2024 - F9507141 - Inibição de 3 linhas no SQL, pa-
      *        ra liberar o acesso a todas as Carteiras nos menus da Pla
      *        taforma.
      *  VRS002 31/07/2023 - F9188847 - Inclusão Consulta de localização
      *        do usuário, verificação de acesso do usuario a Carteira
      *        por UOR de Localização ou Posição.
      *  VRS001 08/06/2018 - F2054109 - DE 2016/145195 - Implantação
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTSLA12.

      *-----------------------------------------------------------------
       ENVIRONMENT                     DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       CONFIGURATION                   SECTION.
      *-----------------------------------------------------------------
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *-----------------------------------------------------------------
       DATA                            DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       WORKING-STORAGE                 SECTION.
      *-----------------------------------------------------------------
       77  CTE-INICIO-SS               PIC  X(40) VALUE
                                       '*** S.S. Comeca Aqui ***'.
       77  CTE-PROG                    PIC  X(17)
                                       VALUE '*** SGTSLA12 ***'.
       77  CTE-VERS                    PIC  X(06) VALUE 'VRS002'.

      * Módulos acionados
       77  IIBP2000                    PIC  X(08) VALUE 'IIBP2000'.
       77  ARHSOLOC                    PIC  X(08) VALUE 'ARHSOLOC'.

      *--> Book do programa de ambiente - CICS5000
      *    Definido na work para preservar as informacoes
-INC HLPKE01B


      *-----------------------------------------------------------------
       LOCAL-STORAGE SECTION.
      *-----------------------------------------------------------------

      *--> Area de trabalho
       01  UOR-USU-LCZC                 PIC  S9(09) COMP-5 VALUE 0.
       01  UOR-USU-PSC                  PIC  S9(09) COMP-5 VALUE 0.

       01  QT-REG-SELD                 PIC S9(04) COMP-5 VALUE ZEROS.

       01  IC-TAB                       PIC S9(04) COMP-5 VALUE ZEROS.
       01  CTRA-GESTAO-SGT              PIC S9(04) COMP-5 VALUE 8.
       01  CRCT-APROVADA                PIC S9(04) COMP-5 VALUE 2.

      *--> Campos utilizados para leitura
       01  REG-WS.
           03  CD-CTRA-TIT-VL          PIC S9(9)  COMP-5
               OCCURS 101 TIMES        VALUE 0.
           03  NM-CTRA-TIT-VL          PIC  X(50)
               OCCURS 101 TIMES        VALUE ' '.
           03  DT-INC-VGC-CTRA         PIC  X(08)
               OCCURS 101 TIMES        VALUE ' '.
           03  DT-FIM-VGC-CTRA         PIC  X(08)
               OCCURS 101 TIMES        VALUE ' '.
           03  IN-RCPO-JDCL            PIC  X(08)
               OCCURS 101 TIMES        VALUE ' '.
           03  DT-FIM-VGC-CTRA-N      PIC S9(04) COMP-5
               OCCURS 101 TIMES        VALUE 0.

      *--> Variavel(is) de controle do cursor(es)
       01  CTL-CURSOR-PSQ.
           03  CD-FIM-CURSOR-PSQ             PIC S9(04) COMP-5 VALUE 0.
               88  FIM-CURSOR-PSQ            VALUE +100.
           03  IN-CURSOR-PSQ-ABTO            PIC  X(01) VALUE 'N'.
               88  CURSOR-PSQ-ABTO           VALUE 'S'.


      *-----------------------------------------------------------------
      *    Books Utilizados
      *-----------------------------------------------------------------

      *--> Book do Container de Erro
-INC IIBKERRO

      *--> Book para captura de erro via MSG1
-INC MSGKWMG1

      *--> Book para captura de abend cics da bloco codigo MSGKP999
-INC HLPKW999

      *--> Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1

      *--> Book do container de requisicao e resposta deste modulo
-INC SGTKLA12

      * Book do container de Serviços (para acionamento de serviços)
-INC IIBKSRVC

      * Book dos dados do usuário
-INC IIBKOPRD

      * Consultar UOR de posição e localização do usuário
       01  ARHSOLOC-DADOS.
           03  LK-ENTRADA.
               05  LKE-TIP-TRBR        PIC  9(001).
               05  LKE-TIP-CMP-CHV     PIC  9(001).
               05  LKE-MATRICULA       PIC  9(009).
               05  LKE-CHAVE           PIC  X(008).
               05  FILLER REDEFINES LKE-CHAVE.
                   07  LKE-CHVF-LETRA  PIC  X(001).
                   07  LKE-CHVF-NR     PIC  9(007).
               05  FILLER REDEFINES LKE-CHAVE.
                   07  LKE-CHVT-LETRA  PIC  X(001).
                   07  LKE-CHVT-NR     PIC  9(006).
                   07  LKE-CHVT-ESP    PIC  X(001).
               05  LKE-CPF             PIC  9(011).
               05  FILLER REDEFINES LKE-CPF.
                   07  LKE-CPF-NR      PIC  9(009).
                   07  LKE-CPF-DV      PIC  9(002).
               05  LKE-MCI             PIC  9(010).
               05  FILLER              PIC  X(050).
           03  LK-SAIDA.
               05  LKS-CHAVE           PIC  X(008).
               05  LKS-MATRICULA       PIC  9(009).
               05  LKS-NOME            PIC  X(050).
               05  LKS-CPF             PIC  9(011).
               05  LKS-UOR-LCZC        PIC  9(009).
               05  LKS-UOR-PSC         PIC  9(009).
               05  LKS-PREF-LCZC       PIC  9(004).
               05  LKS-DT-DMA-LCZC     PIC  9(008).
               05  LKS-MCI             PIC  9(010).
               05  LKS-POS-RETORNO     PIC  9(003).
               05  LKS-COD-RETORNO     PIC  9(009).
               05  LKS-MSG-RETORNO     PIC  X(078).
               05  LKS-COD-FUNCAO      PIC  9(005).
               05  LKS-AGENCIA         PIC  9(004).
               05  LKS-CONTA           PIC  9(008).
               05  FILLER              PIC  X(083).
           03  TWA.
               05  PARMLIST            PIC S9(008) COMP OCCURS 7 TIMES.

       01  CTE-FINAL-SS                PIC  X(40)      VALUE
                    '*** S.S. TERMINA AQUI ***'.

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

      *-----------------------------------------------------------------
      *    Cursores de Pesquisa
      *-----------------------------------------------------------------
           EXEC SQL
           DECLARE CURSOR-PSQ CURSOR
                     WITH ROWSET POSITIONING FOR
                 WITH
                   TAB_ULT_TS_EST_CRCT_APVD (CD_CTRA_TVM, MAX_TS_EST)
                   AS (SELECT
                          CD_CTRA_TVM,
                          MAX(TS_EST_CRCT_CTRA) AS MAX_TS_EST
                       FROM
                            DB2SGT.EST_CRCT_CTRA_TVM
                       WHERE
                                CD_TIP_EST_DCS = :CRCT-APROVADA
                            AND CD_TIP_ATB_CTRA = :CTRA-GESTAO-SGT
                       GROUP BY
                           CD_CTRA_TVM
                          ),

                   TAB_VRS_CRCT_APVD (CD_CTRA_TVM, CD_TIP_ATB_CTRA,
                                   VERSAO_APVD)
                   AS  (SELECT
                           A.CD_CTRA_TVM,
                           A.CD_TIP_ATB_CTRA,
                           A.NR_ETRD_CRCT_CTRA AS VERSAO_APVD
                        FROM
                           DB2SGT.EST_CRCT_CTRA_TVM A
                        INNER JOIN
                           TAB_ULT_TS_EST_CRCT_APVD B
                        ON
                             A.CD_CTRA_TVM = B.CD_CTRA_TVM
                         AND A.TS_EST_CRCT_CTRA = B.MAX_TS_EST
                        ),

                   TAB_ACSS_CTRA (CD_CTRA_TVM)
                   AS  (SELECT
                           CD_CTRA_TVM
                        FROM
                           DB2SGT.ACSS_CTRA_TVM
      *                  WHERE
      *                      CD_UOR_ACSS_CTRA  = :UOR-USU-LCZC
      *                   OR CD_UOR_ACSS_CTRA  = :UOR-USU-PSC
                        GROUP BY
                           CD_CTRA_TVM
                        )

                   SELECT A.CD_CTRA_TIT_VL,
                          A.NM_CTRA_TIT_MNTR,
                       CHAR(TO_CHAR(A.DT_INC_VGC_CTRA,'DDMMYYYY') , 8),
                       CHAR(TO_CHAR(A.DT_FIM_VGC_CTRA,'DDMMYYYY') , 8),
                          A.IN_RCPO_JDCL
                   FROM
                      DB2SGT.CTRA_TIT_VL_MNTR A
                   INNER JOIN
                      TAB_ACSS_CTRA  D
                   ON
                      A.CD_CTRA_TIT_VL = D.CD_CTRA_TVM
                   INNER JOIN
                      DB2SGT.CRCT_CTRA_TVM B
                   ON
                      A.CD_CTRA_TIT_VL = B.CD_CTRA_TVM
                   INNER JOIN
                     TAB_VRS_CRCT_APVD C
                   ON
                         B.CD_CTRA_TVM         = C.CD_CTRA_TVM
                    AND  B.CD_TIP_ATB_CTRA     = C.CD_TIP_ATB_CTRA
                    AND  B.NR_ETRD_CRCT_CTRA   = C.VERSAO_APVD
                   WHERE
                       A.CD_CTRA_TIT_VL > :SGTSLA12-CD-ULT-CTRA-TIT-VL
                   AND B.TX_VCLD_CRCT = 'S'
                   ORDER BY
                       A.CD_CTRA_TIT_VL
           END-EXEC.
      *-----------------------------------------------------------------
       PROCEDURE                       DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------

           MOVE  0001                  TO  MSG1-CD-SEQL-ERRO

           IF  NOT EXECUCAO-ONLINE  AND
               NOT EXECUCAO-BATCH
               CALL  CICS5000       USING  CICS5000-COMMAREA
           END-IF

      *--> Habilita tratamento automatico de Erros DB2
-INC HLPKE997

      *--> Habilita captura automatica de Abend e Erros CICS
-INC HLPKE999

           PERFORM  100000-INICIAR

           PERFORM  200000-PROCESSAR

           PERFORM  980000-GRAVAR-CONTAINER-RPST

           PERFORM  990000-ENCERRAR

           GOBACK
           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-INICIAR                  SECTION.
      *-----------------------------------------------------------------
           MOVE  0002                  TO  MSG1-CD-SEQL-ERRO

           PERFORM  970000-LER-CANAL

      *    Define o nome do programa p/ fins manipulação container
           MOVE CTE-PROG(5:8)          TO NM-PGM

001990*    Limpa a resposta e erro antes de executar o modulo
           PERFORM  983000-EXCLUIR-RESPOSTA-ERRO

           PERFORM  971000-LER-REQUISICAO

           PERFORM  974000-LER-DADOS-OPRD

           PERFORM  110000-VALIDAR-PARAMETRO

           .
       100000-SAI.
           EXIT.


      *-----------------------------------------------------------------
       110000-VALIDAR-PARAMETRO        SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                   TO MSG1-CD-SEQL-ERRO

           MOVE 'N'                    TO SGTSLA12-IN-CTNC-PSQ

      *--> Validar Chave da Operação Estruturada
           IF  SGTSLA12-CD-ULT-CTRA-TIT-VL     LESS ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0001              TO  MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de títulos e valores mobiliários
      -              ' inválido.'       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           .
       110000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-PROCESSAR                SECTION.
      *-----------------------------------------------------------------
           MOVE  0004                  TO  MSG1-CD-SEQL-ERRO

           PERFORM  210000-CONSULTA-UOR-USU

           PERFORM  960101-ABRE-CURSOR-PSQ

           PERFORM  960102-LER-CURSOR-PSQ

           PERFORM  220000-MONTAR-RESPOSTA
                    VARYING IC-TAB
                    FROM 0001 BY 0001
                    UNTIL IC-TAB > SGTSLA12-QT-OCR-TAB

           PERFORM  960103-FECHA-CURSOR-PSQ

           .
       200000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       210000-CONSULTA-UOR-USU    SECTION.
      *----------------------------------------------------------------*
           MOVE 0005                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE                  ARHSOLOC-DADOS.

      *    Movimentar o nome do programa a ser executado
           MOVE ARHSOLOC               TO NM-PGM

      *    Movimentar os campos de entrada

           IF KOPRD-CD-OPRD(1:1) EQUAL 'F'
              MOVE 1                   TO LKE-TIP-TRBR
           END-IF
           IF KOPRD-CD-OPRD(1:1) EQUAL 'C'
              MOVE 2                   TO LKE-TIP-TRBR
           END-IF

           MOVE 2                      TO LKE-TIP-CMP-CHV

           MOVE KOPRD-CD-OPRD          TO LKE-CHAVE

           PERFORM 910000-CHAMAR-SERVICO

      *--> Caso não retorne UOR de posição ou de localização do ARH, emi
      *--> tir erro
           IF  LKS-UOR-PSC EQUAL ZEROS AND LKS-UOR-LCZC EQUAL ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 1113               TO MSG1-CD-ERRO-TS
               MOVE 'Usuário não possui UOR gravada no ARH.'
                                       TO MSG1-TX-ERRO-TS
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-ERRO

           END-IF

           IF LKS-UOR-LCZC GREATER ZEROS
              MOVE LKS-UOR-LCZC TO UOR-USU-LCZC
           END-IF

           IF LKS-UOR-PSC GREATER ZEROS
            MOVE LKS-UOR-PSC TO UOR-USU-PSC
           END-IF

           .
       210000-SAI.
           EXIT.


      *-----------------------------------------------------------------
       220000-MONTAR-RESPOSTA          SECTION.
      *-----------------------------------------------------------------
           MOVE  0006                  TO  MSG1-CD-SEQL-ERRO

           MOVE CD-CTRA-TIT-VL(IC-TAB)  TO
                                        SGTSLA12-CD-CTRA-TIT-VL(IC-TAB)

           MOVE NM-CTRA-TIT-VL(IC-TAB)  TO
                                        SGTSLA12-NM-CTRA-TIT-VL(IC-TAB)

           MOVE DT-INC-VGC-CTRA(IC-TAB) TO
                                        SGTSLA12-DT-INC-VGC-CTRA(IC-TAB)

           IF   DT-FIM-VGC-CTRA-N        (IC-TAB) EQUAL -1
                MOVE SPACES             TO
                                        SGTSLA12-DT-FIM-VGC-CTRA(IC-TAB)
           ELSE
                MOVE DT-FIM-VGC-CTRA(IC-TAB)
                                        TO
                                        SGTSLA12-DT-FIM-VGC-CTRA(IC-TAB)
           END-IF

           MOVE IN-RCPO-JDCL(IC-TAB)    TO
                                        SGTSLA12-IN-RCPO-JDCL(IC-TAB)
           .
       220000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       960101-ABRE-CURSOR-PSQ          SECTION.
      *-----------------------------------------------------------------
           MOVE  0007                  TO  MSG1-CD-SEQL-ERRO

           EXEC SQL
                OPEN CURSOR-PSQ
           END-EXEC

           MOVE  'S'                   TO  IN-CURSOR-PSQ-ABTO
           .
       960101-SAI.
           EXIT.

      *-----------------------------------------------------------------
       960102-LER-CURSOR-PSQ           SECTION.
      *-----------------------------------------------------------------
           MOVE  0008                  TO  MSG1-CD-SEQL-ERRO

           INITIALIZE                      REG-WS

           EXEC SQL
                     FETCH NEXT ROWSET
                           CURSOR-PSQ FOR 0101 ROWS
               INTO :CD-CTRA-TIT-VL
                  , :NM-CTRA-TIT-VL
                  , :DT-INC-VGC-CTRA
                  , :DT-FIM-VGC-CTRA
                    :DT-FIM-VGC-CTRA-N
                  , :IN-RCPO-JDCL
           END-EXEC

           MOVE SQLERRD(3)             TO  QT-REG-SELD

           IF  QT-REG-SELD                 EQUAL 0101
               SET   SGTSLA12-TEM-MAIS-REGISTRO
                                       TO  TRUE
               MOVE  0100              TO  QT-REG-SELD
           END-IF

           MOVE  QT-REG-SELD           TO  SGTSLA12-QT-OCR-TAB
           MOVE  SQLCODE               TO  CD-FIM-CURSOR-PSQ

           IF QT-REG-SELD EQUAL ZEROS
              SET ERRO-PROGRAMADO     TO TRUE
              MOVE 1114              TO MSG1-CD-ERRO-TS
              MOVE 'Usuário sem acesso a Carteira. Solicitar cadastramen
      -         'to de sua UOR ao gestor do legado.'
                                       TO MSG1-TX-ERRO-TS
              PERFORM 999000-ERRO
           END-IF

           .
       960102-SAI.
           EXIT.

      *-----------------------------------------------------------------
       960103-FECHA-CURSOR-PSQ         SECTION.
      *-----------------------------------------------------------------
           MOVE  0009                  TO  MSG1-CD-SEQL-ERRO

           MOVE  'N'                   TO  IN-CURSOR-PSQ-ABTO

           EXEC SQL
                CLOSE CURSOR-PSQ
           END-EXEC
           .
       960103-SAI.
           EXIT.

004070*----------------------------------------------------------------*
004080* Executa um modulo implementado com CONTAINER                   *
004090* Nao incluir nessa section os modulos chamados via COMMAREA     *
004100*                                                                *
004110*----------------------------------------------------------------*
004120*---------------------------------------------------------------*
004130 910000-CHAMAR-SERVICO           SECTION.
004140*---------------------------------------------------------------*
004150*    Acionar programa IIBP2000 para requisição do serviço.
004160*---------------------------------------------------------------*
004170     MOVE 0010                   TO MSG1-CD-SEQL-ERRO
004180
004190*    Grava o container com dados do serviço/operação a chamar
004200     PERFORM 981000-GRAVAR-CONTAINER-SRVC
004210
004220*    Grava o container de requisicao do modulo a executar
004230     PERFORM 982000-GRAVAR-CONTAINER-RQSC
004240
004250*    Limpa a resposta e erro antes de executar o modulo
004260     PERFORM 983000-EXCLUIR-RESPOSTA-ERRO
004270
004280*    Não há captura automática de erro CICS por usar RESP e RESP2
004290     EXEC CICS LINK
004300               PROGRAM   ( IIBP2000     )
004310               CHANNEL   ( NM-CNL       )
004320               RESP      ( MSG1-RESP1   )
004330               RESP2     ( MSG1-RESP2   )
004340     END-EXEC
004350
004360     EVALUATE TRUE
004370         WHEN MSG1-RESP1        EQUAL DFHRESP(NORMAL)
004380              CONTINUE
004390         WHEN OTHER
004400              SET   ERRO-CICS    TO  TRUE
                    MOVE  9000         TO  MSG1-CD-ERRO-TS
                    STRING  'Erro ao acionar serviço: '
                            SRVC-ID-SRVC '.' SRVC-VRS-SRVC '.'
                            SRVC-ID-OPR  '.' SRVC-VRS-OPR
                            'via IIBP2000, ' 'RESP=' MSG1-RESP1
                                           ', RESP-2=' MSG1-RESP2 '.'
                            DELIMITED BY SIZE
                            INTO  MSG1-TX-ERRO-TS
                    END-STRING
004480              PERFORM 999000-ERRO
004490     END-EVALUATE
004500
004510*    Efetua a leitura do Container de Erro
004520     PERFORM 972000-LER-CONTAINER-DE-ERRO
004530
004540*    Efetua a leitura do Container de Resposta
004550     PERFORM 973000-LER-RESPOSTA
004560     .
004570 910000-SAI.
004580     EXIT.
004590
      *----------------------------------------------------------------*
       969000-MONITORAR-VARIAVEIS      SECTION.
      *----------------------------------------------------------------*
      * Monitora as variáveis dos módulos chamados
      *----------------------------------------------------------------*
           MOVE 0011                   TO MSG1-CD-SEQL-ERRO

           EVALUATE NM-PGM
               WHEN ARHSOLOC
                    MOVE 'LKE-TIP-TRBR'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE LKE-TIP-TRBR
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'LKE-TIP-CMP-CHV'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE LKE-TIP-CMP-CHV
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'LKE-CHAVE'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE LKE-CHAVE
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN OTHER
                    CONTINUE
           END-EVALUATE
           .
       969000-SAI.
           EXIT.
005750*----------------------------------------------------------------*
005760*                                                                *
005770* SECTION DA FAIXA DE 970000 A 979900 PARA LEITURA DE DADOS      *
005780*                                                                *
005790*----------------------------------------------------------------*
005800 970000-LER-CANAL                SECTION.
005810*    Obtem o canal corrente
005820*----------------------------------------------------------------*
005830     MOVE 0012                   TO MSG1-CD-SEQL-ERRO
005840
005850*Por causa da captura automática de erro CICS, deve ser previsto
005860*o erro de container
005870     SET  ERRO-CANAL             TO TRUE
005880
005890     EXEC CICS ASSIGN
005900               CHANNEL ( NM-CNL )
005910     END-EXEC
005920
005930*    Ao chegar aqui, há certeza de que não houve erro algum
005940     SET  SEM-ERRO               TO TRUE
005950     .
005960 970000-SAI.
005970     EXIT.
005980
005990*----------------------------------------------------------------*
006000 971000-LER-REQUISICAO           SECTION.
006010*----------------------------------------------------------------*
006020     MOVE 0013                   TO MSG1-CD-SEQL-ERRO
006030
006040*    Monta o nome do container de requisicao
006050     MOVE 'SGTSLA12'             TO NM-PGM
006060     MOVE 'SGTSLA12-RQSC'        TO NM-CTNR
006070
006080*    Recupera o tamanho do container de requisição
006090     PERFORM 973100-LER-CONTAINER-NODATA
006100
006110*    Obtem o tamanho esperado da requisicao
006120     MOVE LENGTH OF SGTSLA12-RQSC TO QT-CCT-EPRO
006130
006140*    Valida tamanho esperado da requisicao X tamanho recebido
006150
006160     IF  QT-CCT-LIDO             NOT EQUAL QT-CCT-EPRO
006170         SET ERRO-TAMANHO-AREA   TO TRUE
006180         PERFORM 999000-GRAVA-ERRO-E-ENCERRA
006190     END-IF
006200
006210     MOVE 0014                   TO MSG1-CD-SEQL-ERRO
006220
006230*Configura dados para leitura do container de requisição
006240     SET  GET-CONTAINER          TO TRUE
006250
006260*Por causa da captura automática de erro CICS, deve ser previsto
006270*o erro de container
006280     SET  ERRO-CONTAINER         TO TRUE
006290
006300*    Efetua a leitura do container
006310     EXEC CICS GET
006320               CONTAINER (NM-CTNR)
006330               FLENGTH   (QT-CCT-LIDO)
006340               INTO      (SGTSLA12-RQSC)
006350     END-EXEC
006360
006370*Ao chegar aqui, há certeza de que não houve erro algum
006380     SET  SEM-ERRO               TO TRUE
006390     .
006400 971000-SAI.
006410     EXIT.
006420
006430*----------------------------------------------------------------*
006440 972000-LER-CONTAINER-DE-ERRO    SECTION.
006450*----------------------------------------------------------------*
006460*    A presença do container PRM-ERRO indica que houve erro de
006470*    integração, a ausência do container indica sucesso.
006480*----------------------------------------------------------------*
006490     MOVE 0015                   TO MSG1-CD-SEQL-ERRO
006500
006510*    Le o container de erro dos modulos chamados
006520     SET GET-CONTAINER           TO TRUE
006530     MOVE 'PRM-ERRO'             TO NM-CTNR
006540     INITIALIZE IIBKERRO-PRM-ERRO
006550
006560*    Não há captura automática de erro CICS por usar RESP e RESP2
006570     EXEC CICS GET
006580               CONTAINER ( NM-CTNR                     )
006590               INTO      ( IIBKERRO-PRM-ERRO           )
006600               FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
006610               RESP      ( MSG1-RESP1                  )
006620               RESP2     ( MSG1-RESP2                  )
006630     END-EXEC
006640
006650     EVALUATE TRUE
006660*        Caso deseje personalizar ou não interromper o
006670*        processamento, alterar para não dar erro aqui SOMENTE
006680*        PARA O MÓDULO CHAMADO e tratar na section chamadora
006690         WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
006700              CONTINUE
006710         WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
006720              SET ERRO-PROGRAMA-ACIONADO TO TRUE
006730              MOVE 0012          TO MSG1-CD-ERRO-TS
006740              MOVE  NM-PGM       TO  MSG1-NM-PGM-ACND

                    IF  NM-PGM(1:3) EQUAL 'SGT'
                        MOVE  IIBKERRO-CD-ERRO
                                       TO  MSG1-CD-RTN-PGM-ACND
                                           MSG1-CD-ERRO-TS
                        MOVE  IIBKERRO-TX-ERRO
                                       TO  MSG1-TX-RTN-PGM-ACND
                                           MSG1-TX-ERRO-TS
                    ELSE
                       MOVE 3004          TO MSG1-CD-ERRO-TS
                       STRING  'Erro ao acionar o módulo ' NM-PGM '.'
                         DELIMITED BY SIZE
                         INTO           MSG1-TX-ERRO-TS

                       MOVE IIBKERRO-TX-ERRO TO MSG1-TX-RTN-PGM-ACND
                       MOVE IIBKERRO-CD-ERRO TO MSG1-CD-RTN-PGM-ACND
                    END-IF
006800
006810*             Monitorar variáveis do módulo chamado
006820              PERFORM 969000-MONITORAR-VARIAVEIS
006830              PERFORM 999000-ERRO
006840         WHEN OTHER
006850              SET ERRO-CONTAINER  TO TRUE
006860              PERFORM 999002-TRATA-CONDITION-CICS
006870     END-EVALUATE
006880     .
006890 972000-SAI.
006900     EXIT.
006910
006920*----------------------------------------------------------------*
006930 973000-LER-RESPOSTA             SECTION.
006940*----------------------------------------------------------------*
006950*    Recupera o container com os dados de resposta (saída).
006960*
006970*                        NOME DO CONTAINER RESPOSTA
006980*                        --------------------------
006990*
007000*    Modulo CONTAINER  -     NOME_PGM-RPST
007010*    Modulo IIB (FIXO) -     PRM-DADOS-RPST
007020*
007030*---------------------------------------------------------------*
007040     MOVE 0016                   TO MSG1-CD-SEQL-ERRO
007050
007060*    Monta o nome dos container de acordo com modulo a executar
007070     MOVE SPACES                 TO NM-CTNR
007080
007090*    Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
007100*    nome_modulo -RPST
007110     EVALUATE NM-PGM
007120         WHEN ARHSOLOC
007130              MOVE 'PRM-DADOS-RPST'
007140                                 TO NM-CTNR
007150         WHEN OTHER
007160              MOVE NM-PGM        TO NM-CTNR(1:8)
007170              MOVE '-RPST'       TO NM-CTNR(9:5)
007180     END-EVALUATE
007190
007200*    Recupera o tamanho do container de resposta
007210     PERFORM 973100-LER-CONTAINER-NODATA
007220
007230*Configura dados para leitura do container de resposta
007240     SET  GET-CONTAINER          TO TRUE
007250
007260*Por causa da captura automática de erro CICS, deve ser previsto
007270*o erro de container
007280     SET  ERRO-CONTAINER         TO TRUE
007290
007300
007310*    Le a RESPOSTA de acordo com o programa que a gravou
007320
007330     MOVE 0017                   TO MSG1-CD-SEQL-ERRO
007340
007350     EVALUATE NM-PGM
007360
007370*        Calcular a quantidade de caracteres esperado e limpar
007380*        a area de dados antes de executar o GET
007390*        Para matrizes com dados variaveis (clausula depending
007400*        on) nao ha como utilizar o comando INITIALIZE
007410
007420         WHEN ARHSOLOC
007430              INITIALIZE ARHSOLOC-DADOS
007440              MOVE LENGTH OF ARHSOLOC-DADOS TO QT-CCT-EPRO
007450              EXEC CICS GET
007460                        CONTAINER (NM-CTNR          )
007470                        INTO      (ARHSOLOC-DADOS   )
007480                        FLENGTH   (QT-CCT-LIDO      )
007490              END-EXEC
007500         WHEN OTHER
007510              SET   ERRO-PROGRAMADO
                                       TO  TRUE
                    MOVE  9003         TO  MSG1-CD-ERRO-TS
                    STRING  'Erro na leitura da resposta do módulo '
                            NM-PGM '.'
                            DELIMITED BY SIZE
                            INTO           MSG1-TX-ERRO-TS
007540              PERFORM 999000-ERRO
007550     END-EVALUATE
007560
007570*Ao chegar aqui, há certeza de que não houve erro algum
007580     SET  SEM-ERRO               TO TRUE
007590
007600     .
007610 973000-SAI.
007620     EXIT.
007630
007640*----------------------------------------------------------------*
007650 973100-LER-CONTAINER-NODATA     SECTION.
007660*----------------------------------------------------------------*
007670     MOVE 0018                   TO MSG1-CD-SEQL-ERRO
007680
007690*Configura dados para leitura do container de requisição
007700     SET  GET-CONTAINER          TO TRUE
007710
007720*Por causa da captura automática de erro CICS, deve ser previsto
007730*o erro de container
007740     SET  ERRO-CONTAINER         TO TRUE
007750
007760*    Obtem a quantidade de caracteres no container
007770     MOVE ZEROS                  TO QT-CCT-LIDO
007780     EXEC CICS GET
007790               CONTAINER (NM-CTNR)
007800               NODATA
007810               FLENGTH   (QT-CCT-LIDO)
007820     END-EXEC
007830
007840*    Ao chegar aqui, há certeza de que não houve erro algum
007850     SET  SEM-ERRO               TO TRUE
007860     .
007870 973100-SAI.
007880     EXIT.
007890
007900*----------------------------------------------------------------*
007910 974000-LER-DADOS-OPRD           SECTION.
007920*----------------------------------------------------------------*
007930     MOVE 0019                   TO MSG1-CD-SEQL-ERRO
007940
007950*Configura dados para leitura do container de requisição
007960     SET  GET-CONTAINER          TO TRUE
007970     MOVE 'PRM-INF-OPRD'         TO NM-CTNR
007980
007990*Por causa da captura automática de erro CICS, deve ser previsto
008000*o erro de container
008010     SET  ERRO-CONTAINER         TO TRUE
008020
008030     INITIALIZE PRM-INF-OPRD
008040     EXEC CICS GET
008050          CONTAINER ( NM-CTNR                )
008060          INTO      ( PRM-INF-OPRD           )
008070          FLENGTH   ( LENGTH OF PRM-INF-OPRD )
008080     END-EXEC.
008090
008100*    Ao chegar aqui, há certeza de que não houve erro algum
008110     SET  SEM-ERRO               TO TRUE
008120     .
008130 974000-SAI.
008140     EXIT.
008150
008160*----------------------------------------------------------------*
008170*                                                                *
008180* SECTION DA FAIXA DE 980000 A 989900 PARA GRAVACAO DE DADOS     *
008190*                                                                *
008200*----------------------------------------------------------------*
008210 980000-GRAVAR-CONTAINER-RPST    SECTION.
008220*----------------------------------------------------------------*
008230
008240*    Exclui eventual container de erro de outros programas
008250     PERFORM 983100-EXCLUIR-CONTAINER-ERRO
008260
008270     MOVE 0020                   TO MSG1-CD-SEQL-ERRO
008280
008290*    Monta o nome do container de resposta
008300     MOVE 'SGTSLA12-RPST'        TO NM-CTNR
008310     SET PUT-CONTAINER           TO TRUE
008320
008330*Por causa da captura automática de erro CICS, deve ser previsto
008340*o erro de container
008350     SET  ERRO-CONTAINER         TO TRUE
008360
008370     EXEC CICS PUT
008380               CONTAINER (NM-CTNR                    )
008390               FROM      (SGTSLA12-RPST          )
008400               FLENGTH   (LENGTH OF SGTSLA12-RPST)
008410     END-EXEC
008420
008430*Ao chegar aqui, há certeza de que não houve erro algum
008440     SET  SEM-ERRO               TO TRUE
008450     .
008460 980000-SAI.
008470     EXIT.
008480
008490*---------------------------------------------------------------*
008500 981000-GRAVAR-CONTAINER-SRVC    SECTION.
008510*---------------------------------------------------------------*
008520*    Postar container de requisição de serviço.
008530*---------------------------------------------------------------*
008540     MOVE 0021                   TO MSG1-CD-SEQL-ERRO
008550
008560*    Monta o nome do container de requisicao do servico
008570     MOVE 'PRM-RQSC-SRVC'        TO NM-CTNR
008580     SET PUT-CONTAINER           TO TRUE
008590
008600     EVALUATE NM-PGM
008610         WHEN ARHSOLOC
008620              MOVE 70663         TO SRVC-ID-SRVC
008630              MOVE 1             TO SRVC-VRS-SRVC
008640              MOVE 255605        TO SRVC-ID-OPR
008650              MOVE 1             TO SRVC-VRS-OPR
008660     END-EVALUATE
008670
008680*Por causa da captura automática de erro CICS, deve ser previsto
008690*o erro de container
008700     SET  ERRO-CONTAINER         TO TRUE
008710
008720     EXEC CICS PUT
008730               CONTAINER ( NM-CTNR                     )
008740               FROM      ( SRVC-PRM-RQSC-SRVC          )
008750               FLENGTH   ( LENGTH OF SRVC-PRM-RQSC-SRVC)
008760     END-EXEC
008770
008780*Ao chegar aqui, há certeza de que não houve erro algum
008790     SET  SEM-ERRO               TO TRUE
008800     .
008810 981000-SAI.
008820     EXIT.
008830
008840*----------------------------------------------------------------*
008850 982000-GRAVAR-CONTAINER-RQSC    SECTION.
008860*----------------------------------------------------------------*
008870*    Postar container com dados de requisição (entrada)
008880*
008890*                        NOME DO CONTAINER REQUISICAO
008900*                        ----------------------------
008910*
008920*    Modulo CONTAINER  -     NOME_PGM-RQSC
008930*    Modulo IIB (FIXO) -     PRM-DADOS-RQSC
008940*
008950*----------------------------------------------------------------*
008960
008970     MOVE 0022                   TO MSG1-CD-SEQL-ERRO
008980
008990*    Monta o nome dos container de acordo com modulo a executar
009000     MOVE SPACES                 TO NM-CTNR
009010     SET PUT-CONTAINER           TO TRUE
009020
009030*    Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
009040*    nome_modulo -RPST
009050     EVALUATE NM-PGM
009060         WHEN ARHSOLOC
009070              MOVE 'PRM-DADOS-RQSC'
009080                                 TO NM-CTNR
009090         WHEN OTHER
009100              MOVE NM-PGM        TO NM-CTNR(1:8)
009110              MOVE '-RQSC'       TO NM-CTNR(9:5)
009120     END-EVALUATE
009130
009140*Por causa da captura automática de erro CICS, deve ser previsto
009150*o erro de container
009160     SET  ERRO-CONTAINER         TO TRUE
009170
009180*    Faz put da requisicao do programa a executar
009190     EVALUATE NM-PGM
009200
009210         WHEN ARHSOLOC
009220              EXEC CICS PUT
009230                        CONTAINER (NM-CTNR)
009240                        FROM      (ARHSOLOC-DADOS )
009250                        FLENGTH   (LENGTH OF ARHSOLOC-DADOS )
009260              END-EXEC
009270
009280         WHEN OTHER
009290              MOVE 9006          TO MSG1-CD-ERRO-TS
                    MOVE 'Não definida a execução do módulo.'
                                       TO MSG1-TX-ERRO-TS
009320              PERFORM 999000-ERRO
009330     END-EVALUATE
009340
009350*Ao chegar aqui, há certeza de que não houve erro algum
009360     SET  SEM-ERRO               TO TRUE
009370     .
009380 982000-SAI.
009390     EXIT.
009400
009410*----------------------------------------------------------------*
009420 983000-EXCLUIR-RESPOSTA-ERRO    SECTION.
009430*----------------------------------------------------------------*
009440* Exclui o container de resposta e de erro para evitar problemas *
009450* em modulos executados mais de uma vez dentro da mesma sessao do*
009460* CICS. Equivale ao initialize na area de resposta dos programas *
009470* que utilizam COMMAREA.                                         *
009480*----------------------------------------------------------------*
009490     MOVE 0023                   TO MSG1-CD-SEQL-ERRO
009500
009510*    Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
009520*    nome_modulo -RPST
009530     EVALUATE NM-PGM
009540         WHEN ARHSOLOC
009550              MOVE 'PRM-DADOS-RPST'
009560                                 TO NM-CTNR
009570         WHEN OTHER
009580              MOVE NM-PGM        TO NM-CTNR(1:8)
009590              MOVE '-RPST'       TO NM-CTNR(9:5)
009600     END-EVALUATE
009610
009620     SET DEL-CONTAINER           TO TRUE
009630
009640*    Exclui o container de resposta
009650*    Não há captura automática de erro CICS por usar RESP e RESP2
009660     EXEC CICS DELETE
009670               CONTAINER (NM-CTNR)
009680               RESP      (MSG1-RESP1)
009690               RESP2     (MSG1-RESP2)
009700     END-EXEC
009710
009720     EVALUATE TRUE
009730         WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
009740         WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
009750              CONTINUE
009760         WHEN OTHER
009770              SET ERRO-CONTAINER TO TRUE
009780              PERFORM 999002-TRATA-CONDITION-CICS
009790     END-EVALUATE
009800
009810*    Excluir o container de erro
009820     PERFORM 983100-EXCLUIR-CONTAINER-ERRO
009830     .
009840 983000-SAI.
009850     EXIT.
009860*
009870*----------------------------------------------------------------*
009880 983100-EXCLUIR-CONTAINER-ERRO   SECTION.
009890*----------------------------------------------------------------*
009900
009910*    Limpar variaveis container de erro
009920     INITIALIZE IIBKERRO-PRM-ERRO
009930
009940*    Exclui o container de erro
009950     MOVE 0024                   TO MSG1-CD-SEQL-ERRO
009960     MOVE 'PRM-ERRO'             TO NM-CTNR
009970     SET DEL-CONTAINER           TO TRUE
009980
009990*    Não há captura automática de erro CICS por usar RESP e RESP2
010000     EXEC CICS DELETE
010010               CONTAINER (NM-CTNR)
010020               RESP      (MSG1-RESP1)
010030               RESP2     (MSG1-RESP2)
010040     END-EXEC
010050
010060     EVALUATE TRUE
010070         WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
010080         WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
010090              CONTINUE
010100         WHEN OTHER
010110              SET ERRO-CONTAINER TO TRUE
010120              PERFORM 999002-TRATA-CONDITION-CICS
010130     END-EVALUATE
010140     .
010150 983100-SAI.
010160     EXIT.
010170
010180*----------------------------------------------------------------*
010190 984000-GRAVAR-CONTAINER-ERRO    SECTION.
010200*----------------------------------------------------------------*
010210     MOVE 0025                   TO MSG1-CD-SEQL-ERRO
010220
010230*    Grava o erro gerado no container de erro
010240*    Não há captura automática de erro CICS por usar RESP e RESP2
010250     EXEC CICS PUT
010260               CONTAINER ( 'PRM-ERRO'                  )
010270               FROM      ( IIBKERRO-PRM-ERRO           )
010280               FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
010290               RESP      (MSG1-RESP1                   )
010300               RESP2     (MSG1-RESP2                   )
010310     END-EXEC
010320     .
010330 984000-SAI.
010340     EXIT.
010350
010360*----------------------------------------------------------------*
010370 990000-ENCERRAR                 SECTION.
010380*----------------------------------------------------------------*
010390
010400     EXEC CICS
010410          RETURN
010420     END-EXEC
010430     .
010440 990000-ENCERRAR-SAI.
010450     EXIT.
010460*
010470*----------------------------------------------------------------*
010480*                                                                *
010490* As sections abaixo iniciadas com 999000 representam os tratamen*
010500* tos de erro utilizando o Sistema MSG (rotina MSGS0001)         *
010510*                                                                *
010520*----------------------------------------------------------------*
010530 999000-ERRO                     SECTION.
010540*----------------------------------------------------------------*
010550
010560     IF  MSG1-CD-ERRO-TS         EQUAL ZEROS
               MOVE 8888               TO MSG1-CD-ERRO-TS
           END-IF
           IF  MSG1-TX-ERRO-TS         EQUAL SPACES
               MOVE 'Erro não especificado no módulo.'
                                       TO MSG1-TX-ERRO-TS
           END-IF

010620     PERFORM 999000-GRAVA-ERRO-E-ENCERRA
010630     .
010640 999000-ERRO-SAI.
010650     EXIT.
010660
010670*----------------------------------------------------------------*
010680*     As sections abaixo representam os tratamentos de erro
010690*    utilizando o sistema MSG de mensageria (rotina MSGS0001)
      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------

           IF  IN-CURSOR-PSQ-ABTO          EQUAL  'S'
               PERFORM  960103-FECHA-CURSOR-PSQ
           END-IF

010730
010740*    Em caso de erro, desfaz as atualizações em banco de dados
010750
010760     EXEC CICS SYNCPOINT
010770               ROLLBACK
010780     END-EXEC

      *--> Verifica se o tipo de erro gerado possui tratamento interno
           EVALUATE  TRUE
               WHEN  ERRO-CONTAINER
                     PERFORM  999004-TRATA-ERRO-CTNR
               WHEN  ERRO-TAMANHO-AREA
                     PERFORM  999005-TRATA-ERRO-AREA-DADOS
               WHEN  ERRO-CANAL
                     PERFORM  999006-TRATA-ERRO-CANAL
               WHEN  OTHER
                     CONTINUE
           END-EVALUATE

010920*Adiciona as variáveis de entrada para monitoração

           MOVE  'SGTSLA12-CD-ULT-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTSLA12-CD-ULT-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

      *--> Grava o erro no Sistema MSG
           PERFORM  MSG1-NTF-MSG

           MOVE  MSG1-CD-ERRO-TS       TO  IIBKERRO-CD-ERRO
           MOVE  MSG1-TX-ERRO-TS       TO  IIBKERRO-TX-ERRO

      *--> Grava o erro gerado no container de erro
011220     PERFORM 984000-GRAVAR-CONTAINER-ERRO

011240*    Encerra a transacao
           PERFORM  990000-ENCERRAR
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

      *---------------------- FIM SGTSLA12 -----------------------------