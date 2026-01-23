000010*-----------------------------------------------------------------
000020* PROGRAMA..: SGTS0096
000030* ANALISTA..: F0175702 ADRIANO SEIJI NAKAMURA
000040* AUTOR.....: F0175702 ADRIANO SEIJI NAKAMURA
000050* DATA......: 23/11/2016
000060* OBJETIVO..: Consultar uma Nota Tecnica de uma Operao Estrut.
000080* COMPILACAO: COBOL/CICS
000090*-----------------------------------------------------------------
000000* VRS002 02/12/2019 F2804421 DE 2016/145195 - Para dados da
      *        migração não acionar ARHS0882 para obter o nome.
000090* VRS001 08/02/2018 F8785286 DE 2016/145195 - Acerto dos códigos
000090*          das mensagens para mensageria.
000100*        23/11/2016 F0175702 DE 2016/145195 - IMPLANTACAO
000110*-----------------------------------------------------------------
000120
000130*-----------------------------------------------------------------
000140 IDENTIFICATION DIVISION.
000150*----------------------------------------------------------------*
000160 PROGRAM-ID. SGTS0096.
000170*
000180*----------------------------------------------------------------*
000190 ENVIRONMENT DIVISION.
000200*----------------------------------------------------------------*
000210*
000220*----------------------------------------------------------------*
000230 CONFIGURATION                   SECTION.
000240*----------------------------------------------------------------*
000250 SPECIAL-NAMES.
000260     DECIMAL-POINT IS COMMA.
000270*
000280*----------------------------------------------------------------*
000290 DATA DIVISION.
000300*----------------------------------------------------------------*
000310*
000320
000330 WORKING-STORAGE                 SECTION.
000340*----------------------------------------------------------------*
000350
000360 77  CTE-INICIO-SS                 PIC  X(040)  VALUE
000370              '*** S.S. COMECA AQUI ***'.
000380
000390 77  CTE-PROG                      PIC  X(018)  VALUE
000400              '*** SGTS0096 ***'.
000410 77  CTE-VERS                      PIC  X(006)  VALUE 'VRS002'.
000420
000430* Modulos acionados
000440 77  IIBP2000                      PIC  X(008) VALUE 'IIBP2000'.
000450 77  SGTSL055                      PIC  X(008) VALUE 'SGTSL055'.
000450 77  SGTSL056                      PIC  X(008) VALUE 'SGTSL056'.
000450 77  MSTS8042                      PIC  X(008) VALUE 'MSTS8042'.
000450*77  NTNS128A                      PIC  X(008) VALUE 'NTNS128A'.
000450 77  ALCSO10L                      PIC  X(008) VALUE 'ALCSO10L'.
000450 77  ARHS0882                      PIC  X(008) VALUE 'ARHS0882'.
       77  MSTS8078                      PIC  X(08) VALUE 'MSTS8078'.
000470*----------------------------------------------------------------*
000480 LOCAL-STORAGE                   SECTION.
000490*----------------------------------------------------------------*
       01 IN-CONTAINER-ERRO            PIC  X(001) VALUE SPACES.
          88 TEM-CONTAINER-ERRO                    VALUE 'S'.
          88 NAO-TEM-CONTAINER-ERRO                VALUE 'N'.
000500
000510* Areas de trabalho
000550 01  CD-UOR-ITVT-NOTA              PIC S9(9)  COMP-5 VALUE 0.
000500
000450 01  CD-CHAVE-FUN                PIC  X(08) VALUE ' '.
000450 01  FILLER                      REDEFINES  CD-CHAVE-FUN.
000450     05  FILLER                  PIC  X(01).
000450     05  CD-CHAVE-FUN-NR         PIC  9(07).

       01  IC-LISTA-MST                PIC S9(4) COMP-5 VALUE 0.
       01  IC-TAB                      PIC  9(4) VALUE 0.
       01  IC-TAB-2                    PIC S9(4) COMP-5 VALUE 0.


000560*----------------------------------------------------------------*
000570* Book dos Containers e das subrotinas chamadas
000580*----------------------------------------------------------------*
000590
000600* Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1
000590
000620* Book para captura de abend cics da bloco codigo MSGKP999
-INC HLPKW999
000590
000650* Book para captura de erro via MSG1
-INC MSGKWMG1
000670
000680* Book do container de requisicao e resposta deste modulo
-INC SGTK0096
000450***************************************************************
000450* LINKAGE DA SUBROTINA ARHS0882.                              *
000450* VRS001-17/10/2013-F2065718(CLAUDINEY)-Criao               *
000450***************************************************************
002140
000450 01  ARHK0882-AREA.
000450    03  ARHK0882-ENTRADA.
000450         05  ARHK0882-MATR-INF       PIC  9(009).
002180    03  ARHK0882-FIMDA.
002190         05  ARHK0882-NOME-RET       PIC  X(050).
002200         05  ARHK0882-SIT-RET        PIC  9(003).
002210         05  ARHK0882-PRF-LOCA-RET   PIC  9(004).
002220         05  ARHK0882-CD-COMIS-RET   PIC  9(005).
002230         05  ARHK0882-NM-COMIS-RET   PIC  X(050).
002240         05  ARHK0882-CD-RF-RET      PIC  9(002).
002250         05  ARHK0882-CD-RO-RET      PIC  X(004).
002260    03  ARHK0882-CONTROLE.
002270         05  ARHK0882-RET-CODE       PIC  9(002).
002280         05  ARHK0882-SQLCODE        PIC +9(005).
002290         05  ARHK0882-SECTION        PIC  X(030).
002300         05  ARHK0882-SQLCA          PIC  X(146).
002310         05  ARHK0882-SQLCODE-2      PIC S9(009) COMP.
002320         05  FILLER                  PIC  X(35).
000590
      *--> Listar Codigo Nome Tipologia e Estado de uma Lista de UORs
-INC MSTK8078
000720* Listar Nota Tecnica de Operacao Estruturada
-INC SGTKL055
000590
000720* Listar UORs do BB intervenientes
-INC SGTKL056
000590
001620* Book de dados para chamada ao IIB - interveniente MST
-INC MSTK8042
000590
001620* Book de dados para chamada ao IIB - NTN Estrutura decisoria
      * -INC NTNK128A
000590
001620* Book de dados para chamada ao IIB - ALC Estrutura decisoria
-INC ALCKO10L
000590
000800* Book do Container de Erro
-INC IIBKERRO
000590
000820* Book do container de Servicos (para acionamento de servicos)
-INC IIBKSRVC
000590
001400* Book dos dados do usurio
-INC IIBKOPRD
000840
000850 77  CTE-FINAL-SS                  PIC X(40)      VALUE
000860              '*** S.S. TERMINA AQUI ***'.
000870
000880*----------------------------------------------------------------*
000890 PROCEDURE DIVISION.
000900*----------------------------------------------------------------*
000910
000920*----------------------------------------------------------------*
000930 000000-ROTINA-PRINCIPAL         SECTION.
000940*----------------------------------------------------------------*
000950
000960     PERFORM 100000-INICIAR
000970
000980     PERFORM 200000-PROCESSAR
000990
001000     PERFORM 990000-ENCERRAR
001020     .
001030 000000-SAI.
001040     EXIT.
001060*----------------------------------------------------------------*
001070 100000-INICIAR                  SECTION.
001080*----------------------------------------------------------------*
001090
001100     MOVE 0001                   TO MSG1-CD-SEQL-ERRO
001110
001120*    Ativa a captura automatica de abend do CICS.
001130*    WHENEVER CICS
-INC HLPKE999
001150
001160     PERFORM 970000-LER-CANAL
001170
001180*    Define o nome do programa p/ fins manipulacao container
001190     MOVE 'SGTS0096'             TO NM-PGM
001200
001210*    Limpa a resposta e erro antes de executar o modulo
001220     PERFORM 983000-EXCLUIR-RESPOSTA-ERRO
001230
001240     PERFORM 971000-LER-REQUISICAO
001250
001260     PERFORM 110000-VALIDAR-REQUISICAO
001270     .
001280 100000-SAI.
001290     EXIT.
001310*----------------------------------------------------------------*
001320 110000-VALIDAR-REQUISICAO       SECTION.
001330*----------------------------------------------------------------*
001340     MOVE 0002                   TO MSG1-CD-SEQL-ERRO
001350
001360*    Valida campo a campo da requisicao
001370     IF  SGTS0096-NR-NOTA-TCN-OPR   <= ZEROS
000450         SET ERRO-PROGRAMADO TO TRUE
001400         MOVE 0181           TO MSG1-CD-ERRO-TS
001410         MOVE 'Número da nota técnica da operação não informado.'
001420                                 TO MSG1-TX-ERRO-TS
001430*        SET MNT-VRV         TO TRUE
001440         PERFORM 999000-ERRO
000450     END-IF
001470     .
001480 110000-SAI.
001490     EXIT.
001510*----------------------------------------------------------------*
001520 200000-PROCESSAR                SECTION.
001530*----------------------------------------------------------------*
001540     MOVE 0003                   TO MSG1-CD-SEQL-ERRO
001550
000450     PERFORM 210000-PESQ-NOTA-TECNICA
000450
000450     PERFORM 220000-PESQ-UOR-NOTA-TECN
000450
000450     PERFORM 980000-GRAVAR-CONTAINER-RPST
001640     .
001650 200000-SAI.
001660     EXIT.
001680*----------------------------------------------------------------*
000450 210000-PESQ-NOTA-TECNICA       SECTION.
000450*----------------------------------------------------------------*
000450* Pesquisa notas tcnicas cadastradas a partir dos dados de entra-
000450* da.
000450*----------------------------------------------------------------*
000450     MOVE 0004                   TO MSG1-CD-SEQL-ERRO
001550
001730*    Movimentar o nome do programa a ser executado
000450     MOVE SGTSL055               TO NM-PGM
001750
000450     MOVE 3                      TO SGTSL055-CD-TIP-SRVC
000450     MOVE SGTS0096-NR-NOTA-TCN-OPR
000450                                 TO SGTSL055-E-NR-NOTA-TCN-OPR
000450
001780     PERFORM 900000-EXECUTAR-MODULO
000450     IF  SGTSL055-QT-OCR-TAB EQUAL ZEROS
000450         SET ERRO-PROGRAMADO     TO TRUE
001400         MOVE 0293               TO MSG1-CD-ERRO-TS
001410         MOVE 'Nota técnica da operação estruturada inexistente.'
001420                                 TO MSG1-TX-ERRO-TS
001430*        SET MNT-VRV             TO TRUE
001440         PERFORM 999000-ERRO
000450     END-IF
000450
000450     PERFORM 211000-MONTA-SAIDA

000450     .
001650 210000-SAI.
001660     EXIT.
001680*----------------------------------------------------------------*
000450 211000-MONTA-SAIDA       SECTION.
000450*----------------------------------------------------------------*
000450* Retorn todos os dados da Nota Tecnica.
000450*----------------------------------------------------------------*
000450     MOVE 0005                   TO MSG1-CD-SEQL-ERRO
000450     MOVE SGTSL055-CD-ARQ-OPR-ETRD(1)
000450                         TO SGTS0096-CD-ARQ-OPR-ETRD
000450     MOVE SGTSL055-CD-UOR-DRTA-OGNL(1)
000450                         TO SGTS0096-CD-UOR-DRTA-OGNL
001780     MOVE SGTSL055-CD-REF-NOTA-TCN(1)
000450                         TO SGTS0096-CD-REF-NOTA-TCN
000450     MOVE SGTSL055-DT-EMS-NOTA    (1)
000450                         TO SGTS0096-DT-EMS-NOTA
000450     MOVE SGTSL055-DT-APVC-NOTA   (1)
000450                         TO SGTS0096-DT-APVC-NOTA
000450     MOVE SGTSL055-CD-ETR-DCS     (1)
000450                         TO SGTS0096-CD-ETR-DCS
000450     MOVE SGTSL055-CD-TIP-DOC-TCN (1)
000450                         TO SGTS0096-CD-TIP-DOC-TCN
000450     MOVE SGTSL055-TX-TIP-DOC-TCN (1)
000450                         TO SGTS0096-TX-TIP-DOC-TCN
000450     MOVE SGTSL055-CD-USU-RSP-ULT-ATL (1)
000450                         TO SGTS0096-CD-USU-RSP-ULT-ATL
000450     MOVE SGTSL055-TS-ULT-ATL (1)
000450                         TO SGTS0096-TS-ULT-ATL
001750
001730*    Movimentar o nome do programa a ser executado
000450     MOVE MSTS8042               TO NM-PGM
000450     INITIALIZE MSTS8042-DADOS
020610     MOVE SGTS0096-CD-UOR-DRTA-OGNL TO MSTS8042-CD-UOR
020620
000450     PERFORM 910000-CHAMAR-SERVICO
000450     MOVE MSTS8042-NM-UOR
000450                         TO SGTS0096-TX-UOR-DRTA-OGNL
000450
001730*    Movimentar o nome do programa a ser executado
000450     MOVE ALCSO10L               TO NM-PGM
000450     MOVE SGTS0096-CD-ETR-DCS    TO ALCSO10L-CD-ETR-DCS
000450
001430     PERFORM 910000-CHAMAR-SERVICO
000450     MOVE ALCSO10L-NM-ETR-DCS    TO SGTS0096-NM-ETR-DCS
000450
000450*    Movimentar o nome do programa a ser executado
002040     MOVE ARHS0882               TO NM-PGM
002050
           IF  SGTS0096-CD-USU-RSP-ULT-ATL NOT EQUAL 'MIGRACAO'
000450         INITIALIZE  ARHK0882-FIMDA
002060*      Movimentar os campos de entrada
002090         MOVE SGTS0096-CD-USU-RSP-ULT-ATL TO CD-CHAVE-FUN
000450         MOVE CD-CHAVE-FUN-NR        TO ARHK0882-MATR-INF
002190
002200         PERFORM 910000-CHAMAR-SERVICO
002220         IF  ARHK0882-RET-CODE NOT EQUAL ZEROS
000450             SET ERRO-PROGRAMA-ACIONADO TO TRUE
016260             MOVE 3004               TO MSG1-CD-ERRO-TS
016260             STRING  'Erro ao acionar o módulo ' NM-PGM '.'
016260                     DELIMITED BY SIZE
016260                     INTO           MSG1-TX-ERRO-TS
016270             MOVE ARHK0882-SECTION   TO MSG1-TX-RTN-PGM-ACND
016300             MOVE NM-PGM             TO MSG1-NM-PGM-ACND
016320             MOVE ARHK0882-RET-CODE TO MSG1-CD-RTN-PGM-ACND
016260
016350*        Monitorar variáveis do módulo chamado
016360             PERFORM 969000-MONITORAR-VARIAVEIS
016370             PERFORM 999000-ERRO
009840         END-IF
001750
000450         MOVE ARHK0882-NOME-RET TO SGTS0096-NM-USU-RSP-ULT-ATL
           END-IF
000450     .
001650 211000-SAI.
001660     EXIT.
002800*----------------------------------------------------------------*
000450 220000-PESQ-UOR-NOTA-TECN    SECTION.
000450*----------------------------------------------------------------*
000450* Pesquisa o nome das UORs intervenientes da Nota tecnica
000450*----------------------------------------------------------------*
000450     MOVE 0006                   TO MSG1-CD-SEQL-ERRO
000450
000450     MOVE SGTSL056               TO NM-PGM
001750
000450     MOVE 1                      TO SGTSL056-CD-TIP-SRVC
000450     MOVE SGTS0096-NR-NOTA-TCN-OPR
000450                                 TO SGTSL056-E-NR-NOTA-TCN-OPR
000450     MOVE CD-UOR-ITVT-NOTA       TO SGTSL056-E-CD-UOR-ITVT-NOTA
001780     PERFORM 900000-EXECUTAR-MODULO
000450
000450     MOVE SGTSL056-QT-OCR-TAB    TO SGTS0096-QT-OCR-TAB
000450     MOVE SGTSL056-IN-CTNC-PSQ   TO SGTS0096-IN-CTNC-PSQ
001750

000450     MOVE 0007                   TO MSG1-CD-SEQL-ERRO
      *--> Movimentar o nome do programa a ser executado
           MOVE  MSTS8078              TO  NM-PGM

      *--> Movimentar os campos de entrada
      *--> A rotina do MST recebe lista de UORs, mas se vierem zerados
      *    não existirem, ignora a linha no retorno.
      *    A operação tb recebe 40 uors por chamada
      *    necessário comparar as uor enviadas e recebidas para obter o
      *    nome
           MOVE   1                 TO      IC-TAB-2
                                            SGTSL056-IC-TAB
           PERFORM UNTIL SGTSL056-IC-TAB > SGTSL056-QT-OCR-TAB

              PERFORM VARYING IC-TAB FROM 1 BY 1
                   UNTIL SGTSL056-IC-TAB > SGTSL056-QT-OCR-TAB
                   OR IC-TAB > 50
                  MOVE  SGTSL056-CD-UOR-ITVT-NOTA(SGTSL056-IC-TAB)
                                          TO  MSTS8078-CD-UOR
                                            (IC-TAB)
                  ADD 1   TO  SGTSL056-IC-TAB
              END-PERFORM

              SUBTRACT 1 FROM IC-TAB GIVING MSTS8078-QT-OCR-TAB-UOR

              PERFORM 910000-CHAMAR-SERVICO

              PERFORM  VARYING IC-LISTA-MST FROM 1 BY 1
                   UNTIL IC-LISTA-MST > MSTS8078-QT-OCR-TAB
                   IF  MSTS8078-CD-UOR-RPST(IC-LISTA-MST) EQUAL
                      SGTSL056-CD-UOR-ITVT-NOTA(IC-TAB-2)
                      MOVE SGTSL056-CD-UOR-ITVT-NOTA(IC-TAB-2)
000450                         TO SGTS0096-CD-UOR-ITVT-NOTA(IC-TAB-2)
                     MOVE  MSTS8078-NM-UOR-RDZ(IC-LISTA-MST)
                               TO  SGTS0096-TX-UOR-ITVT-NOTA(IC-TAB-2)
                   END-IF
                   ADD 1 TO IC-TAB-2
              END-PERFORM
           END-PERFORM
000450     .
001650 220000-SAI.
001660     EXIT.
002720*----------------------------------------------------------------*
002730*                                                                *
002740*  SECTION DA FAIXA DE 900000 A 969900 PARA DEMAIS ROTINAS       *
002750*                                                                *
002760*----------------------------------------------------------------*
002770* Executa um modulo implementado com CONTAINER                   *
002780* Nao incluir nessa section os modulos chamados via COMMAREA     *
002790*                                                                *
002800*----------------------------------------------------------------*
002810 900000-EXECUTAR-MODULO          SECTION.
002820*----------------------------------------------------------------*
002850*    Grava o container de requisicao do modulo a executar
002860     PERFORM 982000-GRAVAR-CONTAINER-RQSC
002870
002880*    Limpa a resposta e erro antes de executar o modulo
002890     PERFORM 983000-EXCLUIR-RESPOSTA-ERRO
002900
002910*    Nao ha captura automatica de erro CICS por usar RESP e RESP2
002920     EVALUATE NM-PGM
002930         WHEN SGTSL055
002940             EXEC CICS LINK
002950                       PROGRAM ( SGTSL055  )
002960                       CHANNEL ( NM-CNL    )
002970                       RESP    ( MSG1-RESP1)
002980                       RESP2   ( MSG1-RESP2)
002990             END-EXEC
001750
000450         WHEN SGTSL056
002940             EXEC CICS LINK
002950                       PROGRAM ( SGTSL056  )
002960                       CHANNEL ( NM-CNL    )
002970                       RESP    ( MSG1-RESP1)
002980                       RESP2   ( MSG1-RESP2)
002990             END-EXEC
001750
000450         WHEN OTHER
003350              SET   ERRO-PROGRAMADO  TO  TRUE
003350              MOVE  9006          TO  MSG1-CD-ERRO-TS
003350              STRING  'Não definida a execução do módulo'
003350                      NM-PGM '.'
003350                      DELIMITED BY SIZE
003350                      INTO            MSG1-TX-ERRO-TS
003350              PERFORM 999000-ERRO
003050     END-EVALUATE
003060*
003070     EVALUATE TRUE
003080         WHEN MSG1-RESP1        EQUAL DFHRESP(NORMAL)
003090              CONTINUE
001750
003100         WHEN OTHER
003080              SET   ERRO-CICS    TO  TRUE
003080              MOVE  3004         TO  MSG1-CD-ERRO-TS
003080              STRING
003080              'Erro  ao acionar o módulo  ' NM-PGM  ', '
003080              'RESP  = '  MSG1-RESP1  ', '
003080              'RESP2 = '  MSG1-RESP2
003080              DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
003080              PERFORM 999000-ERRO
003190     END-EVALUATE
003200
003210*    Efetua a leitura do Container de Erro
003220     PERFORM 972000-LER-CONTAINER-DE-ERRO
003230
003240*    Efetua a leitura do Container de Resposta
           IF NOT TEM-CONTAINER-ERRO
              PERFORM 973000-LER-RESPOSTA
           END-IF
           MOVE  SPACES                TO  IN-CONTAINER-ERRO
003270     .
003280 900000-SAI.
003290     EXIT.
003310*---------------------------------------------------------------*
003320 910000-CHAMAR-SERVICO           SECTION.
003330*---------------------------------------------------------------*
003340*    Acionar programa IIBP2000 para requisio do servio.
003350*---------------------------------------------------------------*
003380*    Grava o container com dados do servio/operao a chamar
003390     PERFORM 981000-GRAVAR-CONTAINER-SRVC
003400
003410*    Grava o container de requisicao do modulo a executar
003420     PERFORM 982000-GRAVAR-CONTAINER-RQSC
003430
003440*    Limpa a resposta e erro antes de executar o modulo
003450     PERFORM 983000-EXCLUIR-RESPOSTA-ERRO
003460
003470*    Não há captura automtica de erro CICS por usar RESP e RESP2
003480     EXEC CICS LINK
003490               PROGRAM   ( IIBP2000     )
003500               CHANNEL   ( NM-CNL       )
003510               RESP      ( MSG1-RESP1   )
003520               RESP2     ( MSG1-RESP2   )
003530     END-EXEC
003540
003550     EVALUATE TRUE
003560         WHEN MSG1-RESP1        EQUAL DFHRESP(NORMAL)
003570              CONTINUE
001750
003580         WHEN OTHER
003080              SET   ERRO-PROGRAMA-ACIONADO    TO  TRUE
003080              MOVE  9000         TO  MSG1-CD-ERRO-TS
003080              STRING 'Erro ao acionar serviço: '
003600                     SRVC-ID-SRVC '.' SRVC-VRS-SRVC '.'
003610                     SRVC-ID-OPR  '.' SRVC-VRS-OPR
003620                     'via IIBP2000, '
003630                     'RESP=' MSG1-RESP1 ', RESP-2=' MSG1-RESP2
003080              DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
003660              SET ERRO-CICS      TO TRUE
003080              PERFORM 999000-ERRO
003680     END-EVALUATE
003690
003700*    Efetua a leitura do Container de Erro
003710     PERFORM 972000-LER-CONTAINER-DE-ERRO
003720
003730*    Efetua a leitura do Container de Resposta
           IF NOT TEM-CONTAINER-ERRO
              PERFORM 973000-LER-RESPOSTA
           END-IF
           MOVE  SPACES                TO  IN-CONTAINER-ERRO
003760     .
003770 910000-SAI.
003780     EXIT.
003790*----------------------------------------------------------------*
003800 969000-MONITORAR-VARIAVEIS      SECTION.
003810*----------------------------------------------------------------*
003820* Monitora as variaveis dos modulos chamados
003830*----------------------------------------------------------------*
003850
003860     EVALUATE NM-PGM
003870     WHEN ARHS0882
000450          MOVE 'ARHK0882-MATR-INF'       TO MSGKW999-NM-VRV-MNT
003900          MOVE ARHK0882-MATR-INF         TO MSGKW999-VL-VRV-MNT
003920          PERFORM 999003-ADICIONA-VRV-MNT
001750
000450          MOVE 'ARHK0882-RET-CODE'       TO MSGKW999-NM-VRV-MNT
003900          MOVE ARHK0882-RET-CODE         TO MSGKW999-VL-VRV-MNT
003920          PERFORM 999003-ADICIONA-VRV-MNT
001750
000450          MOVE 'ARHK0882-SQLCODE'        TO MSGKW999-NM-VRV-MNT
003900          MOVE ARHK0882-SQLCODE          TO MSGKW999-VL-VRV-MNT
003920          PERFORM 999003-ADICIONA-VRV-MNT
001750
000450          MOVE 'ARHK0882-SECTION '       TO MSGKW999-NM-VRV-MNT
003900          MOVE ARHK0882-SECTION          TO MSGKW999-VL-VRV-MNT
003920          PERFORM 999003-ADICIONA-VRV-MNT
001750
000450          MOVE 'ARHK0882-SQLCA'          TO MSGKW999-NM-VRV-MNT
003900          MOVE ARHK0882-SQLCA            TO MSGKW999-VL-VRV-MNT
003920          PERFORM 999003-ADICIONA-VRV-MNT
001750
000450          MOVE 'ARHK0882-SQLCODE-2'      TO MSGKW999-NM-VRV-MNT
003900          MOVE ARHK0882-SQLCODE-2        TO MSGKW999-VL-VRV-MNT
003920          PERFORM 999003-ADICIONA-VRV-MNT
001750
000450     WHEN SGTSL056
000450          MOVE 'SGTSL056-CD-TIP-SRVC'    TO MSGKW999-NM-VRV-MNT
003900          MOVE SGTSL056-CD-TIP-SRVC      TO MSGKW999-VL-VRV-MNT
003920          PERFORM 999003-ADICIONA-VRV-MNT
003850
000450          MOVE 'SGTSL056-E-NR-NOTA-TCN-OPR'
003890                                         TO MSGKW999-NM-VRV-MNT
003900          MOVE SGTSL056-E-NR-NOTA-TCN-OPR
003910                                         TO MSGKW999-VL-VRV-MNT
003920          PERFORM 999003-ADICIONA-VRV-MNT
003850
000450          MOVE 'SGTSL056-E-CD-UOR-ITVT-NOTA'
003890                                         TO MSGKW999-NM-VRV-MNT
003900          MOVE SGTSL056-E-CD-UOR-ITVT-NOTA
003910                                         TO MSGKW999-VL-VRV-MNT
003920          PERFORM 999003-ADICIONA-VRV-MNT
001750
000450     WHEN SGTSL055
000450          MOVE 'SGTSL055-CD-TIP-SRVC'    TO MSGKW999-NM-VRV-MNT
003900          MOVE SGTSL055-CD-TIP-SRVC      TO MSGKW999-VL-VRV-MNT
003920          PERFORM 999003-ADICIONA-VRV-MNT
003850
003880          MOVE 'SGTSL055-E-CD-REF-NOTA-TCN'
003890                                         TO MSGKW999-NM-VRV-MNT
003900          MOVE SGTSL055-E-CD-REF-NOTA-TCN
003910                                         TO MSGKW999-VL-VRV-MNT
003920          PERFORM 999003-ADICIONA-VRV-MNT
003850
000450          MOVE 'SGTSL055-E-NR-NOTA-TCN-OPR'
003890                                         TO MSGKW999-NM-VRV-MNT
003900          MOVE SGTSL055-E-NR-NOTA-TCN-OPR
003910                                         TO MSGKW999-VL-VRV-MNT
003920          PERFORM 999003-ADICIONA-VRV-MNT
001750
000450     WHEN ALCSO10L
003880          MOVE 'ALCSO10L-CD-ETR-DCS'     TO MSGKW999-NM-VRV-MNT
003900          MOVE ALCSO10L-CD-ETR-DCS       TO MSGKW999-VL-VRV-MNT
003920          PERFORM 999003-ADICIONA-VRV-MNT
003850
000450     WHEN MSTS8042
003880          MOVE 'MSTS8042-CD-UOR'         TO MSGKW999-NM-VRV-MNT
003900          MOVE MSTS8042-CD-UOR           TO MSGKW999-VL-VRV-MNT
003920          PERFORM 999003-ADICIONA-VRV-MNT

000450     WHEN MSTS8078
              PERFORM VARYING IC-TAB FROM 1 BY 1
                   UNTIL IC-TAB > MSTS8078-QT-OCR-TAB
                    MOVE 'INDICE'        TO MSGKW999-NM-VRV-MNT
009350              MOVE  IC-TAB         TO MSGKW999-VL-VRV-MNT
009370              PERFORM 999003-ADICIONA-VRV-MNT
                    STRING 'MSTS8078-CD-UOR (' DELIMITED BY SIZE
                                  IC-TAB DELIMITED BY SIZE
                                  ')' DELIMITED BY SIZE
009340                                   INTO MSGKW999-NM-VRV-MNT
009350              MOVE  MSTS8078-CD-UOR(IC-TAB)
009360                                   TO MSGKW999-VL-VRV-MNT
009370              PERFORM 999003-ADICIONA-VRV-MNT
                    ADD 1   TO  IC-TAB
              END-PERFORM


001750
000450     WHEN OTHER
004190          CONTINUE
004200     END-EVALUATE
004220     .
004230 969000-SAI.
004240     EXIT.
004260*----------------------------------------------------------------*
004270*                                                                *
004280* SECTION DA FAIXA DE 970000 A 979900 PARA LEITURA DE DADOS      *
004290*                                                                *
004300*----------------------------------------------------------------*
004310 970000-LER-CANAL                SECTION.
004320*    Obtem o canal corrente
004330*----------------------------------------------------------------*
004340     MOVE 0008                   TO MSG1-CD-SEQL-ERRO
004350
004360*Por causa da captura automática de erro CICS, deve ser previsto
004370*o erro de container
004380     SET  ERRO-CANAL             TO TRUE
004390
004400     EXEC CICS ASSIGN
004410               CHANNEL ( NM-CNL )
004420     END-EXEC
004430
004440*    Ao chegar aqui, há certeza de que não houve erro algum
004450     SET  SEM-ERRO               TO TRUE
004460     .
004470 970000-FIM.
004480     EXIT.
004500*----------------------------------------------------------------*
004510 971000-LER-REQUISICAO           SECTION.
004520*----------------------------------------------------------------*
004540     MOVE 0009                   TO MSG1-CD-SEQL-ERRO
004550
004560*    Monta o nome do container de requisicao
004570     MOVE 'SGTS0096'             TO NM-PGM
004580     MOVE 'SGTS0096-RQSC'        TO NM-CTNR
004590
004600*    Recupera o tamanho do container de requisição
004610     PERFORM 973100-LER-CONTAINER-NODATA
004620
004630*    Obtem o tamanho esperado da requisicao
004640     MOVE LENGTH OF SGTS0096-RQSC TO QT-CCT-EPRO
004650
004660*    Valida tamanho esperado da requisicao X tamanho recebido
004670
004680     IF  QT-CCT-LIDO             NOT EQUAL QT-CCT-EPRO
004690         SET ERRO-TAMANHO-AREA   TO TRUE
004700         PERFORM 999000-GRAVA-ERRO-E-ENCERRA
004710     END-IF
004720
004750*Configura dados para leitura do container de requisicao
004760     SET  GET-CONTAINER          TO TRUE
004770
004780*Por causa da captura automática de erro CICS, deve ser previsto
004790*o erro de container
004800     SET  ERRO-CONTAINER         TO TRUE
004810
004820*    Efetua a leitura do container
004830     EXEC CICS GET
004840               CONTAINER (NM-CTNR)
004850               FLENGTH   (QT-CCT-LIDO)
004860               INTO      (SGTS0096-RQSC)
004870     END-EXEC
004880
004890*Ao chegar aqui, há certeza de que não houve erro algum
004900     SET  SEM-ERRO               TO TRUE
004920     .
004930 971000-SAI.
004940     EXIT.
004960*----------------------------------------------------------------*
004970 972000-LER-CONTAINER-DE-ERRO    SECTION.
004980*----------------------------------------------------------------*
004990*    A presença do container PRM-ERRO indica que houve erro de
005000*    integração, a ausência do container indica sucesso.
005010*----------------------------------------------------------------*
005050*    Le o container de erro dos modulos chamados
005060     SET GET-CONTAINER           TO TRUE
005070     MOVE 'PRM-ERRO'             TO NM-CTNR
005080     INITIALIZE IIBKERRO-PRM-ERRO
005090
005100*    Não há captura automática de erro CICS por usar RESP e RESP2
005110     EXEC CICS GET
005120               CONTAINER ( NM-CTNR                     )
005130               INTO      ( IIBKERRO-PRM-ERRO           )
005140               FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
005150               RESP      ( MSG1-RESP1                  )
005160               RESP2     ( MSG1-RESP2                  )
005170     END-EXEC
005180
005190     EVALUATE TRUE
005200*        Caso deseje personalizar ou não interromper o
005210*        processamento, alterar para não dar erro aqui SOMENTE
005220*        PARA O MODULO CHAMADO e tratar na section chamadora
005230         WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
005240              CONTINUE
001750
005250         WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
                     SET  ERRO-PROGRAMA-ACIONADO TO  TRUE
                     MOVE  NM-PGM                TO  MSG1-NM-PGM-ACND

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
                    PERFORM 969000-MONITORAR-VARIAVEIS
                    PERFORM 999000-ERRO
005380         WHEN OTHER
005390              SET ERRO-CONTAINER  TO TRUE
005400              PERFORM 999002-TRATA-CONDITION-CICS
005410     END-EVALUATE
005420     .
005430 972000-SAI.
005440     EXIT.
005460*----------------------------------------------------------------*
005470 973000-LER-RESPOSTA             SECTION.
005480*----------------------------------------------------------------*
005610*    Monta o nome dos container de acordo com modulo a executar
005620     MOVE SPACES                 TO NM-CTNR
005630
005640*    Quando modulos via IIB, fixo nome PRM-DADOS-RPST, senão
005650*    nome_modulo -RPST
005660     EVALUATE NM-PGM
               WHEN MSTS8078
005670         WHEN ARHS0882
000450         WHEN MSTS8042
005700         WHEN ALCSO10L
005680              MOVE 'PRM-DADOS-RPST' TO NM-CTNR
000450         WHEN OTHER
005710              MOVE NM-PGM        TO NM-CTNR(1:8)
005720              MOVE '-RPST'       TO NM-CTNR(9:5)
005730     END-EVALUATE
005740
005750*    Recupera o tamanho do container de resposta
005760     PERFORM 973100-LER-CONTAINER-NODATA
005770
005780*Configura dados para leitura do container de resposta
005790     SET  GET-CONTAINER          TO TRUE
005800
005810*Por causa da captura automática de erro CICS, deve ser previsto
005820*o erro de container
005830     SET  ERRO-CONTAINER         TO TRUE
005850
005860*    Le a RESPOSTA de acordo com o programa que a gravou
005900     EVALUATE NM-PGM
005910
005920*        Calcular a quantidade de caracteres esperado e limpar
005930*        a area de dados antes de executar o GET
005940*        Para matrizes com dados variaveis (clausula depending
005950*        on) nao ha como utilizar o comando INITIALIZE
005960
005970         WHEN ARHS0882
000450              MOVE LENGTH OF ARHK0882-AREA TO QT-CCT-EPRO
006000              EXEC CICS GET
006010                        CONTAINER (NM-CTNR          )
006020                        INTO      (ARHK0882-AREA    )
006030                        FLENGTH   (QT-CCT-LIDO      )
006040              END-EXEC
001750
000450         WHEN ALCSO10L
000450              MOVE LENGTH OF ALCSO10L-RPST TO QT-CCT-EPRO
006000              EXEC CICS GET
006010                        CONTAINER (NM-CTNR          )
006020                        INTO      (ALCSO10L-RPST    )
006030                        FLENGTH   (QT-CCT-LIDO      )
006040              END-EXEC
001750
000450         WHEN MSTS8042
005990              MOVE LENGTH OF MSTS8042-RPST TO QT-CCT-EPRO
006000              EXEC CICS GET
006010                        CONTAINER (NM-CTNR          )
006020                        INTO      (MSTS8042-RPST    )
006030                        FLENGTH   (QT-CCT-LIDO      )
006040              END-EXEC
001750
000450         WHEN SGTSL055
005990              MOVE LENGTH OF SGTSL055-RPST TO QT-CCT-EPRO
006000              EXEC CICS GET
006010                        CONTAINER (NM-CTNR          )
006020                        INTO      (SGTSL055-RPST    )
006030                        FLENGTH   (QT-CCT-LIDO      )
006040              END-EXEC
001750
000450         WHEN SGTSL056
005990              MOVE LENGTH OF SGTSL056-RPST TO QT-CCT-EPRO
006000              EXEC CICS GET
006010                        CONTAINER (NM-CTNR          )
006020                        INTO      (SGTSL056-RPST    )
006030                        FLENGTH   (QT-CCT-LIDO      )
006040              END-EXEC

               WHEN MSTS8078
                    EXEC CICS GET
                              CONTAINER (NM-CTNR       )
                              INTO      (MSTS8078-RPST )
                              FLENGTH   (QT-CCT-LIDO   )
                    END-EXEC
001750
000450         WHEN OTHER
005960              SET   ERRO-PROGRAMADO  TO  TRUE
005960              MOVE  9003             TO  MSG1-CD-ERRO-TS
005960              STRING  'Erro na leitura da resposta do módulo '
005960                      NM-PGM '.'     DELIMITED BY SIZE
005960                      INTO           MSG1-TX-ERRO-TS
005960              PERFORM 999000-ERRO
006180     END-EVALUATE
006190
006200*Ao chegar aqui, há certeza de que não houve erro algum
006210     SET  SEM-ERRO               TO TRUE
006230     .
006240 973000-SAI.
006250     EXIT.
006260*----------------------------------------------------------------*
006270 973100-LER-CONTAINER-NODATA     SECTION.
006280*----------------------------------------------------------------*
006310
006320*Configura dados para leitura do container de requisição
006330     SET  GET-CONTAINER          TO TRUE
006340
006350*Por causa da captura automática de erro CICS, deve ser previsto
006360*o erro de container
006370     SET  ERRO-CONTAINER         TO TRUE
006380
006390*    Obtem a quantidade de caracteres no container
006400     MOVE ZEROS                  TO QT-CCT-LIDO
006410     EXEC CICS GET
006420               CONTAINER (NM-CTNR)
006430               NODATA
006440               FLENGTH   (QT-CCT-LIDO)
006450     END-EXEC
006460
006470*    Ao chegar aqui, há certeza de que não houve erro algum
006480     SET  SEM-ERRO               TO TRUE
006490     .
006500 973100-SAI.
006510     EXIT.
006520*----------------------------------------------------------------*
006530*                                                                *
006540* SECTION DA FAIXA DE 980000 A 989900 PARA GRAVACAO DE DADOS     *
006550*                                                                *
006560*----------------------------------------------------------------*
006570 980000-GRAVAR-CONTAINER-RPST    SECTION.
006580*----------------------------------------------------------------*
006590
006600*    Exclui eventual container de erro de outros programas
006610     PERFORM 983100-EXCLUIR-CONTAINER-ERRO
006620
006630     MOVE 0011                   TO MSG1-CD-SEQL-ERRO
006640
006650*    Monta o nome do container de resposta
006660     MOVE 'SGTS0096-RPST'        TO NM-CTNR
006670     SET PUT-CONTAINER           TO TRUE
006680
006690*Por causa da captura automática de erro CICS, deve ser previsto
006700*o erro de container
006710     SET  ERRO-CONTAINER         TO TRUE
006720
006730     EXEC CICS PUT
006740               CONTAINER (NM-CTNR                    )
006750               FROM      (SGTS0096-RPST          )
006760               FLENGTH   (LENGTH OF SGTS0096-RPST)
006770     END-EXEC
006780
006790*Ao chegar aqui, há certeza de que não houve erro algum
006800     SET  SEM-ERRO               TO TRUE
006810     .
006820 980000-SAI.
006830     EXIT.
006840*---------------------------------------------------------------*
006850 981000-GRAVAR-CONTAINER-SRVC    SECTION.
006860*---------------------------------------------------------------*
006870*    Postar container de requisio de servio.
006880*---------------------------------------------------------------*
006910*    Monta o nome do container de requisicao do servico
006920     MOVE 'PRM-RQSC-SRVC'        TO NM-CTNR
006930     SET PUT-CONTAINER           TO TRUE
006940
006950     EVALUATE NM-PGM
006960         WHEN ARHS0882
008860              MOVE 70663         TO SRVC-ID-SRVC
008870              MOVE 1             TO SRVC-VRS-SRVC
008880              MOVE 252416        TO SRVC-ID-OPR
008890              MOVE 1             TO SRVC-VRS-OPR
001750
000450         WHEN MSTS8042
006970              MOVE 0             TO SRVC-ID-SRVC
006980              MOVE 0             TO SRVC-VRS-SRVC
006990              MOVE 1998646       TO SRVC-ID-OPR
007000              MOVE 1             TO SRVC-VRS-OPR
001750
000450         WHEN ALCSO10L
006970              MOVE 0             TO SRVC-ID-SRVC
006980              MOVE 0             TO SRVC-VRS-SRVC
006990              MOVE 634598        TO SRVC-ID-OPR
007000              MOVE 3             TO SRVC-VRS-OPR
001750
              WHEN MSTS8078
                    MOVE  0            TO  SRVC-ID-SRVC
                    MOVE  0            TO  SRVC-VRS-SRVC
                    MOVE  1768186      TO  SRVC-ID-OPR
                    MOVE  0003         TO  SRVC-VRS-OPR

001530         WHEN OTHER
005960              SET   ERRO-PROGRAMADO  TO  TRUE
005960              MOVE  9004             TO  MSG1-CD-ERRO-TS
005960              STRING  'Erro na gravação da requisição do módulo '
005960                       NM-PGM '.'
005960                       DELIMITED BY SIZE
005960                       INTO           MSG1-TX-ERRO-TS
005960              PERFORM 999000-ERRO
007010     END-EVALUATE
007020
007030*Por causa da captura automtica de erro CICS, deve ser previsto
007040*o erro de container
007050     SET  ERRO-CONTAINER         TO TRUE
007060
007070     EXEC CICS PUT
007080               CONTAINER ( NM-CTNR                     )
007090               FROM      ( SRVC-PRM-RQSC-SRVC          )
007100               FLENGTH   ( LENGTH OF SRVC-PRM-RQSC-SRVC)
007110     END-EXEC
007120
007130*Ao chegar aqui, há certeza de que Não houve erro algum
007140     SET  SEM-ERRO               TO TRUE
007160     .
007170 981000-SAI.
007180     EXIT.
007190*----------------------------------------------------------------*
007200 982000-GRAVAR-CONTAINER-RQSC    SECTION.
007210*----------------------------------------------------------------*
007340*    Monta o nome dos container de acordo com modulo a executar
007350     MOVE SPACES                 TO NM-CTNR
007360     SET PUT-CONTAINER           TO TRUE
007370
007380*    Quando modulos via IIB, fixo nome PRM-DADOS-RPST, senao
007390*    nome_modulo -RPST
007400     EVALUATE NM-PGM
               WHEN MSTS8078
007410         WHEN ARHS0882
000450         WHEN ALCSO10L
000450         WHEN MSTS8042
000450              MOVE 'PRM-DADOS-RQSC' TO NM-CTNR
007440         WHEN OTHER
007450              MOVE NM-PGM        TO NM-CTNR(1:8)
007460              MOVE '-RQSC'       TO NM-CTNR(9:5)
007470     END-EVALUATE
007480
007490*Por causa da captura automatica de erro CICS, deve ser previsto
007500*o erro de container
007510     SET  ERRO-CONTAINER         TO TRUE
007520
007530*    Faz put da requisicao do programa a executar
007540     EVALUATE NM-PGM
              WHEN MSTS8078
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                 )
                              FROM      (MSTS8078-RQSC           )
                              FLENGTH   (LENGTH OF MSTS8078-RQSC )
                    END-EXEC
007550         WHEN ARHS0882
007560             EXEC CICS PUT
007610                        CONTAINER (NM-CTNR)
007620                        FROM      (ARHK0882-AREA )
007630                        FLENGTH   (LENGTH OF ARHK0882-AREA )
007640              END-EXEC
001750
000450         WHEN ALCSO10L
000450             EXEC CICS PUT
007610                        CONTAINER (NM-CTNR)
007620                        FROM      (ALCSO10L-RQSC )
007630                        FLENGTH   (LENGTH OF ALCSO10L-RQSC )
007640             END-EXEC
001750
000450         WHEN MSTS8042
007560             EXEC CICS PUT
007610                        CONTAINER (NM-CTNR)
007620                        FROM      (MSTS8042-RQSC )
007630                        FLENGTH   (LENGTH OF MSTS8042-RQSC )
007640             END-EXEC
001750
000450         WHEN SGTSL055
007560             EXEC CICS PUT
007610                        CONTAINER (NM-CTNR)
007620                        FROM      (SGTSL055-RQSC )
007630                        FLENGTH   (LENGTH OF SGTSL055-RQSC )
007640             END-EXEC
001750
000450         WHEN SGTSL056
007560             EXEC CICS PUT
007610                        CONTAINER (NM-CTNR)
007620                        FROM      (SGTSL056-RQSC )
007630                        FLENGTH   (LENGTH OF SGTSL056-RQSC )
007640             END-EXEC
001750
000450         WHEN OTHER
007630              SET   ERRO-PROGRAMADO  TO  TRUE
007630              MOVE  9004             TO  MSG1-CD-ERRO-TS
007630              STRING  'Erro na gravação da requisição do módulo '
007630                       NM-PGM '.'
007630                       DELIMITED BY SIZE
007630                       INTO           MSG1-TX-ERRO-TS
007630              PERFORM 999000-ERRO
007700     END-EVALUATE
007710
007720*Ao chegar aqui, ha certeza de que nao houve erro algum
007730     SET  SEM-ERRO               TO TRUE
007750     .
007760 982000-SAI.
007770     EXIT.
007790*----------------------------------------------------------------*
007800 983000-EXCLUIR-RESPOSTA-ERRO    SECTION.
007810*----------------------------------------------------------------*
007820* Exclui o container de resposta e de erro para evitar problemas *
007830* em modulos executados mais de uma vez dentro da mesma sessao do*
007840* CICS. Equivale ao initialize na area de resposta dos programas *
007850* que utilizam COMMAREA.                                         *
007860*----------------------------------------------------------------*
007900*    Quando modulos via IIB, fixo nome PRM-DADOS-RPST, senão
007910*    nome_modulo -RPST
007920     EVALUATE NM-PGM
007930         WHEN ARHS0882
               WHEN MSTS8078
000450         WHEN ALCSO10L
000450         WHEN MSTS8042
000450              MOVE 'PRM-DADOS-RPST' TO NM-CTNR
007960         WHEN OTHER
007970              MOVE NM-PGM        TO NM-CTNR(1:8)
007980              MOVE '-RPST'       TO NM-CTNR(9:5)
007990     END-EVALUATE
008000
008010     SET DEL-CONTAINER           TO TRUE
008020
008030*    Exclui o container de resposta
008040*    Não há captura automática de erro CICS por usar RESP e RESP2
008050     EXEC CICS DELETE
008060               CONTAINER (NM-CTNR)
008070               RESP      (MSG1-RESP1)
008080               RESP2     (MSG1-RESP2)
008090     END-EXEC
008100
008110     EVALUATE TRUE
008120         WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
001750
008130         WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
008140              CONTINUE
001750
008150         WHEN OTHER
008160              SET ERRO-CONTAINER TO TRUE
008170              PERFORM 999002-TRATA-CONDITION-CICS
008180     END-EVALUATE
008190
008200*    Excluir o container de erro
008210     PERFORM 983100-EXCLUIR-CONTAINER-ERRO
008220     .
008230 983000-SAI.
008240     EXIT.
008260*----------------------------------------------------------------*
008270 983100-EXCLUIR-CONTAINER-ERRO   SECTION.
008280*----------------------------------------------------------------*
008300*    Limpar variaveis container de erro
008310     INITIALIZE IIBKERRO-PRM-ERRO
008320
008330*    Exclui o container de erro
008350     MOVE 'PRM-ERRO'             TO NM-CTNR
008360     SET DEL-CONTAINER           TO TRUE
008370
008380*    Não há captura automática de erro CICS por usar RESP e RESP2
008390     EXEC CICS DELETE
008400               CONTAINER (NM-CTNR)
008410               RESP      (MSG1-RESP1)
008420               RESP2     (MSG1-RESP2)
008430     END-EXEC
008440
008450     EVALUATE TRUE
008460         WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
001750
008470         WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
008480              CONTINUE
001750
008490         WHEN OTHER
008500              SET ERRO-CONTAINER TO TRUE
008510              PERFORM 999002-TRATA-CONDITION-CICS
008520     END-EVALUATE
008530     .
008540 983100-SAI.
008550     EXIT.
008560*----------------------------------------------------------------*
008570 984000-GRAVAR-CONTAINER-ERRO    SECTION.
008580*----------------------------------------------------------------*
008590     MOVE 0012                   TO MSG1-CD-SEQL-ERRO
008600
008610*    Grava o erro gerado no container de erro
008620*    Não há captura automática de erro CICS por usar RESP e RESP2
008630     EXEC CICS PUT
008640               CONTAINER ( 'PRM-ERRO'                  )
008650               FROM      ( IIBKERRO-PRM-ERRO           )
008660               FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
008670               RESP      (MSG1-RESP1                   )
008680               RESP2     (MSG1-RESP2                   )
008690     END-EXEC
008710     .
008720 984000-SAI.
008730     EXIT.
008750*----------------------------------------------------------------*
008760 990000-ENCERRAR                 SECTION.
008770*----------------------------------------------------------------*
008780
008790     GOBACK
008820     .
008830 990000-ENCERRAR-SAI.
008840     EXIT.
008850*
008860*----------------------------------------------------------------*
008870*                                                                *
008880* As sections abaixo iniciadas com 999000 representam os tratamen*
008890* tos de erro utilizando o Sistema MSG (rotina MSGS0001)         *
008900*                                                                *
008910*----------------------------------------------------------------*
008920 999000-ERRO                     SECTION.
008930*----------------------------------------------------------------*
008940
008950     IF MSG1-CD-ERRO-TS EQUAL ZEROS
008960        MOVE 8888                TO MSG1-CD-ERRO-TS
008970     END-IF
001750
008980     IF MSG1-TX-ERRO-TS EQUAL SPACES
008990        MOVE 'ERRO NAO ESPECIFICADO NO MODULO' TO MSG1-TX-ERRO-TS
009000     END-IF
001750
009010     PERFORM 999000-GRAVA-ERRO-E-ENCERRA
009020     .
009030 999000-ERRO-SAI.
009040     EXIT.
009050
009060*----------------------------------------------------------------*
009070*     As sections abaixo representam os tratamentos de erro
009080*    utilizando o sistema MSG de mensageria (rotina MSGS0001)
009090*-----------------------------------------------------------------
009100 999000-GRAVA-ERRO-E-ENCERRA     SECTION.
009110*-----------------------------------------------------------------
009120
009130*    Em caso de erro, desfaz as atualizações em banco de dados
009140
009150     EXEC CICS SYNCPOINT
009160               ROLLBACK
009170     END-EXEC
009180
009190*Verifica se o tipo de erro gerado possui tratamento interno
009200     EVALUATE TRUE
009210         WHEN ERRO-CONTAINER
009220              PERFORM 999004-TRATA-ERRO-CTNR
001090
009230         WHEN ERRO-TAMANHO-AREA
009240              PERFORM 999005-TRATA-ERRO-AREA-DADOS
001090
009250         WHEN ERRO-CANAL
009260              PERFORM 999006-TRATA-ERRO-CANAL
001090
009270         WHEN OTHER
009280              CONTINUE
009290     END-EVALUATE
009300
009310*Adiciona as variáveis de entrada para monitoração
009320*    IF  MNT-VRV
009320         MOVE 'SGTS0096-NR-NOTA-TCN-OPR'
009340                                 TO MSGKW999-NM-VRV-MNT
009350         MOVE  SGTS0096-NR-NOTA-TCN-OPR
009360                                 TO MSGKW999-VL-VRV-MNT
009370         PERFORM 999003-ADICIONA-VRV-MNT
009380
009390*    END-IF
009400
009410*    Grava o erro gerado no sistema MSG - Mensageria
009420     PERFORM MSG1-NTF-MSG
009430
009440     MOVE MSG1-CD-ERRO-TS        TO IIBKERRO-CD-ERRO
009450     MOVE MSG1-TX-ERRO-TS        TO IIBKERRO-TX-ERRO
009460
009470*    Grava o erro gerado no container de erro
009480     PERFORM 984000-GRAVAR-CONTAINER-ERRO
009490
009500*    Encerra a transacao
009510     PERFORM 990000-ENCERRAR
009520     .
009530 999000-SAI.
009540     EXIT.
009550* --------------------------------------*
009560* 999001-TRATA-ABEND-CICS        SECTION.
009570* 999002-TRATA-CONDITION-CICS    SECTION.
009580* 999003-ADICIONA-VRV-MNT        SECTION.
009590* --------------------------------------*
-INC HLPKP999
009610* --------------------------------------*
009620* 999004-TRATA-ERRO-CTNR         SECTION.
009630* 999005-TRATA-ERRO-AREA-DADOS   SECTION.
009640* 999006-TRATA-ERRO-CANAL        SECTION.
009650*---------------------------------------*
-INC SGTKP999
009670* --------------------------------------*
009680* MSG1-NTF-MSG                   SECTION.
009690* --------------------------------------*
-INC MSGKPMG1
009710*---------------------- FIM SGTS0096 -----------------------------