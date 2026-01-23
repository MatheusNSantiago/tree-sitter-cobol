      *-----------------------------------------------------------------
      * PROGRAMA..: SGTS0088
      * ANALISTA..: F2054109 - Cintia A.Iglesias
      * AUTOR.....: F9188847 - Soila K. L. Akamatsu
      * DATA......: 26/09/2016
      * OBJETIVO..: Incluir Operação Estruturada
      * COMPILACAO: 54 - PSOSE600 - Cobol 6.3 c/otimizacao p/producao
      *-----------------------------------------------------------------
      * VRS007 19/08/2024 F9188847 1445203-4966-Ajuste SGT-online tratar
      *        tranches e 1445062 - Fluxo internalização arquivo via CAC
      * VRS006 01/05/2024 F9188847 - História:1427980
      *         Inclusão campo: CD-MTV-EST-NEGL
      * VRS005 08/03/2024 F9158937 História 1144882, Tornar obrigatório
      *        preenchimento SGTS0088-CD-TIP-COTA-TIT / NR-VRS-TIP-COTA
      *        para modalidades 03 e 04.
      * VRS004 28/09/2023 F9188847 Histórias:1094276 e 1094239, inclusão
      *         campos: CD-CMCP, CD-GR-MCP,CD-LVRO-MC-CPTL, CD-SMCP,
      *         CD-CLSC-ITMT-FNCO,TX-OBS-VRS.
      *         Utilizar o SGTSX005 em novas alterações ou em programas
      *         novos no lugar do SGTST005.
      * VRS003 03/04/2019 F2804421 DE 2016/145195 - Inclusão GOBACK.
      *        12/04/2019 F2804421 DE 2016/145195 - Recompilado alterado
      *              Book SGTSI063.
000100*        14/02/2019 F8785286 DE 2016/145195 - Recompilado alterado
000100*              Book SGTSL006
      *        18/02/2019 F8090555 - Acerto CD_TIP_VRS = 1(original)
      * VRS002 29/01/2019 F8090555 - inclui campo SGTSX005-CD-UOR-BB
      *        28/01/2019 F0950413 DE 2016/145195 - Exclusão do campo
      *              SGTS0088-TX-RTN-PRCT.
      *        21/01/2019 F8785286 DE 2016/145195 - Inclusão monitoração
      *              variáveis para rotina SBDATA
      *        08/11/2018 F8090555 DE 2016/145195 - Acerto Indicador de
      *         Não Conversão em Ações(SGTS0076-IN-N-CNV-OPR-ACAO) para
      *          Produto 427 Modalidade 2 (Debentures)
      * VRS001 09/01/2018 F8785286 DE 2016/145195 - Inclusão cálculo do
      *              valor evento = QT_PAPL_TIT_CPR * VL_PU_EMS_TIT_OPR
      *        13/02/2017 F8785286 DE 2016/145195 - Inclusão campo
      *              CD-USU-RSP-ATL para chave log (movimentar
      *              a partir do container operador).
      *        26/09/2016 F9188847 DE 2015/137233 - IMPLANTACAO
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                 DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTS0088.

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

       77  CTE-INICIO-SS               PIC  X(040)  VALUE
                                       '*** S.S. COMEÇA AQUI ***'.

       77  CTE-PROG                    PIC  X(018)  VALUE
                                       '*** SGTS0088 ***'.
       77  CTE-VERS                    PIC  X(006)  VALUE 'VRS006'.

      *--> Definição dos Nomes dos Programas Acionados
       01 TAB-PROGRAMAS-CHAMADOS.
          03  IIBP2000                    PIC  X(08) VALUE 'IIBP2000'.
          03  SGTST002                    PIC  X(08) VALUE 'SGTST002'.
          03  SGTSX005                    PIC  X(08) VALUE 'SGTSX005'.
          03  SGTST006                    PIC  X(08) VALUE 'SGTST006'.
          03  SGTST009                    PIC  X(08) VALUE 'SGTST009'.
          03  SGTSI069                    PIC  X(08) VALUE 'SGTSI069'.
          03  SGTS0072                    PIC  X(08) VALUE 'SGTS0072'.
          03  SGTS0148                    PIC  X(08) VALUE 'SGTS0148'.
          03  SGTSI063                    PIC  X(08) VALUE 'SGTSI063'.
          03  SGTSI064                    PIC  X(08) VALUE 'SGTSI064'.
          03  MSTS8041                    PIC  X(08) VALUE 'MSTS8041'.
          03  PRDS0203                    PIC  X(08) VALUE 'PRDS0203'.
          03  MSTS8042                    PIC  X(08) VALUE 'MSTS8042'.
          03  SGTST027                    PIC  X(08) VALUE 'SGTST027'.
          03  SGTST029                    PIC  X(08) VALUE 'SGTST029'.
          03  SGTST030                    PIC  X(08) VALUE 'SGTST030'.
          03  SGTST031                    PIC  X(08) VALUE 'SGTST031'.
          03  MCIS0001                    PIC  X(08) VALUE 'MCIS0001'.
          03  ARCSB318                    PIC  X(08) VALUE 'ARCSB318'.
          03  CAFS0053                    PIC  X(08) VALUE 'CAFS0053'.
          03  ICLSI046                    PIC  X(08) VALUE 'ICLSI046'.
          03  CAFS0007                    PIC  X(08) VALUE 'CAFS0007'.
          03  SGTS0211                    PIC  X(08) VALUE 'SGTS0211'.
          03  SGTS211A                    PIC  X(08) VALUE 'SGTS211A'.
          03  SGTSPE23                    PIC  X(08) VALUE 'SGTSPE23'.


      *--> Definição dos Variaveis do Cobolito
-INC ALMKCBL1
      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------
      *--> Auxiliares mensageria
       01  AUX-CD-OPR-ETRD-MCP         PIC  9(11).
       01  AUX-NR-VRS-OPR-ETRD         PIC  9(4).

      *--> Areas de trabalho
       01  IN-CONTAINER-ERRO           PIC  X(01) VALUE SPACES.
           88  TEM-CONTAINER-ERRO                 VALUE 'S'.
           88  NAO-TEM-CONTAINER-ERRO             VALUE 'N'.

      *--> Campo para definição de estado de erro em tabela.
       01  CD-ERRO-TAB                 PIC  9(01) VALUE 0.
           88 COM-ERRO-TAB                        VALUE 1.

       01  IN-CAC                      PIC  X(01)  VALUE SPACES.
       01  IC                          PIC S9(04)  VALUE 0.
       01  AX-PC-PRTC-BCO              PIC --.--9,9999.

       01  AX-VALOR                    PIC ----.---.---,99999999.

       01  AX-VALOR2                   PIC ----.---.---.---.---,99.


       01  CD-OPR-ETRD-MCP-DISPLAY     PIC  9(11) VALUE 0.

      * Comparar datas
       01  DT-PSQ1                     PIC  9(08) VALUE ZEROS.
       01  DT-PSQ1-R                   REDEFINES DT-PSQ1.
           03  ANO                     PIC  9(04).
           03  MES                     PIC  9(02).
           03  DIA                     PIC  9(02).

       01  DT-PSQ2                     PIC  9(08) VALUE ZEROS.
       01  DT-PSQ2-R                   REDEFINES DT-PSQ2.
           03  ANO                     PIC  9(04).
           03  MES                     PIC  9(02).
           03  DIA                     PIC  9(02).

      *    Chamada à subrotina SBDATAF

       01  WKSBDATA                    PIC X(4099).
       01  AREA-RED                    REDEFINES WKSBDATA.
           03  AREA-TAM                PIC 9(0008) COMP.
           03  FILLER                  PIC X(4071).
           03  AREA-FIM                PIC X(0020).

       01  SBDATAF                     PIC  X(08) VALUE 'SBDATAF'.
      * 01  AREA-SBDATAF.
       01  FUNCAO                      PIC  X(03) VALUE 'F01'.
       01  ARG01                       PIC  9(08) VALUE 0.
       01  ARG01-R                     REDEFINES ARG01.
           03  DIA                     PIC  9(02).
           03  MES                     PIC  9(02).
           03  ANO                     PIC  9(04).
       01  ARG02                       PIC  9(01) VALUE 0.

      *--> Parametros do CAFS0007
      *    Busca Dados cadastrais de agencia e banco
       01  CAFS0007-DADOS.
           03  PARM-DADOS.
               05  PARM-BANCO-INF      PIC  9(003).
               05  PARM-AGE-INF        PIC  9(004).
               05  PARM-MUNICIPIO      PIC  9(007).
               05  FILLER              REDEFINES  PARM-MUNICIPIO.
                   07  PARM-CD-MUN     PIC  9(006).
                   07  PARM-DV-MUN     PIC  9(001).
               05  PARM-RET-CODE       PIC  9(002).
               05  PARM-SQLCODE        PIC S9(005).
               05  PARM-SECTION        PIC  X(030).
               05  PARM-SQLCA          PIC  X(146).
               05  PARM-NM-MUNICIPIO   PIC  X(038).
               05  PARM-UF-MUNICIPIO   PIC  X(002).
               05  PARM-NOME-AGEN      PIC  X(030).
               05  PARM-NOME-BANCO     PIC  X(050).

      *-----------------------------------------------------------------
      * Book dos Containers e das subrotinas chamadas
      *-----------------------------------------------------------------

      *--> Book das variáveis auxiliares do tratamento de erro via MSG
-INC SGTKERR1

      *--> Book para captura de abend cics da bloco codigo HLPKW999
-INC HLPKW999

      *--> Book para captura de erro via MSG1
-INC MSGKWMG1

      *--> Book do container de requisição e resposta deste modulo
      *--> Incluir Operação Estruturada
-INC SGTK0088

      *--> CRUD EST_OPR_ETRD
       01  SGTST009-DADOS.
-INC SGTKT009

      *--> CRUD CLI_OPR_ETRD
       01  SGTST006-DADOS.
-INC SGTKT006

      *--> Consultar TABELA OPR_ETRD_MCP
       01  SGTSX005-DADOS.
-INC SGTKX005

      *--> CRUD da tabela TIT_OPR_ETRD_MCP
       01 SGTST002-DADOS.
-INC SGTKT002

       01 SGTS0211-DADOS.
-INC SGTK0211


      *--> Inclusão IN_OPR_ETRD
-INC SGTKI063

-INC SGTK0148

      *--> Inclusão BCO_PCT_OPR
-INC SGTKI064

      *--> Inclusão EST_VRS_OPR_ETRD
       01 SGTSI069-DADOS.
-INC SGTKI069

      *--> Copiar Dados Vinculados da Operação Estruturada
-INC SGTK0072

      *Verifica se o titulo está vinculado a alguma operação no SGT, re
      *tornando estado operacao, PU e data compra.
-INC SGTK211A

      * Consultar último número da caracteristica da carteira.
       01 SGTSPE23-DADOS.
-INC SGTKPE23

      *--> Consultar dados da UOR - via IIB
-INC MSTK8041

      *--> Book do Container de Erro
      *--> Consultar Produlo/Modalidade - via IIB
       01 PRDS0203-DADOS.
-INC PRDK0203

      *--> Consultar dados da UOR - via IIB
-INC MSTK8042

      *--> CRUD TIP_COTA_TIT_OPR
       01  SGTST027-DADOS.
-INC SGTKT027

      *--> CRUD CTGR_BC_TVM
       01  SGTST029-DADOS.
-INC SGTKT029

      *--> CRUD ITC_CVM_OPR_ETRD
       01  SGTST030-DADOS.
-INC SGTKT030

      *--> CRUD ESPC_DBTR_OPR
       01  SGTST031-DADOS.
-INC SGTKT031

      *--> Consultar dados básicos do cliente - via IIB
       01  MCIS0001-DADOS.
-INC MCIB0001

      *--> Validação do risco da operação
       01  ARC-DADOS.
-INC ARCK318L

      *--> Recuperar Nomes de Bancos - via IIB
       01  CAFS0053-DADOS.
-INC CAFK0053

      *--> Consultar camara de registro (Clearing) - via IIB
-INC ICLKI046

-INC IIBKERRO

      *--> Book do container de Serviços (para acionamento de serviços)
-INC IIBKSRVC

      *--> Book dos dados do usuário
-INC IIBKOPRD

      *--> Parâmetros para a utilizacao do DB2
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

       77  CTE-FINAL-SS                PIC X(40) VALUE
                                       '*** S.S. TERMINA AQUI ***'.

      *-----------------------------------------------------------------
       PROCEDURE                       DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.
      *-----------------------------------------------------------------

      *--> Habilita Testes no Cobolito e Debug de Operacoes IIB
-INC ALMKCBL2

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

      * Habilita tratamento automatico de Erros DB2
-INC HLPKE997

      *--> Ativa a captura automatica de abend do CICS.
      *    WHENEVER CICS
-INC HLPKE999

           PERFORM 970000-LER-CANAL

      *--> Define o nome do programa p/ fins manipulacao container
           MOVE 'SGTS0088'             TO NM-PGM

      *--> Limpa a resposta e erro antes de executar o modulo
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO

           PERFORM 971000-LER-REQUISICAO

           PERFORM 974000-LER-DADOS-OPRD

           PERFORM 110000-VALIDAR-REQUISICAO
           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-VALIDAR-REQUISICAO       SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                   TO MSG1-CD-SEQL-ERRO

      *--> Valida campo a campo da requisicao

           IF  SGTS0088-CD-CTRA-TIT-VL-OPR < ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
                MOVE  0001          TO  MSG1-CD-ERRO-TS
                MOVE  'Código da carteira de título e valores mobiliário
      -                's inválido.'
                                       TO  MSG1-TX-ERRO-TS
               SET  MNT-VRV            TO  TRUE
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0088-CD-CTRA-TIT-VL-OPR EQUAL ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0002          TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliá
      -               'rios não informado.'
                                       TO  MSG1-TX-ERRO-TS
               SET  MNT-VRV            TO  TRUE
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0088-CD-CTRA-TIT-VL-COPD < ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
                MOVE  0001          TO  MSG1-CD-ERRO-TS
                MOVE  'Código da carteira de título e valores mobiliário
      -                's inválido.'
                                       TO  MSG1-TX-ERRO-TS
               SET  MNT-VRV            TO  TRUE
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0088-CD-OPR-ETRD-COPD NOT NUMERIC
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0003               TO MSG1-CD-ERRO-TS
               MOVE 'Código operação estruturada mercado de capitais não
      -         ' numérico.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0088-NR-VRS-OPR-ETRD-COPD < ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0006               TO MSG1-CD-ERRO-TS
               MOVE 'Número da versão da operação estruturada inválido.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0088-CD-PRD <= ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0010               TO MSG1-CD-ERRO-TS
               MOVE 'Código do produto inválido.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0088-CD-MDLD <= ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0011               TO MSG1-CD-ERRO-TS
               MOVE 'Código da modalidade do produto inválido.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0088-CD-UOR-DTTR-OPR <= ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0012               TO MSG1-CD-ERRO-TS
               MOVE 'Código da UOR detentora da operação inválido.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0088-NR-DVS-TIT < ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0013               TO MSG1-CD-ERRO-TS
               MOVE 'Número divisão do título inválido.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0088-CD-ITC-CVM < ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0222               TO MSG1-CD-ERRO-TS
               MOVE 'Código da instrução da comissão valores mobiliários
      -             ' inválido.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0088-CD-CTGR-TVM <= ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0015               TO MSG1-CD-ERRO-TS
               MOVE 'Código da categoria de títulos e valores mobiliário
      -             's inválido.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0088-CD-ESPC-DBTR-OPR < ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0016               TO MSG1-CD-ERRO-TS
               MOVE 'Código da espécie de debênture da operação inválido
      -             '.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0088-CD-TIP-COTA-TIT < ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0017               TO MSG1-CD-ERRO-TS
               MOVE 'Código do tipo de cota do título da operação inváli
      -             'do.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0088-NR-VRS-TIP-COTA < ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0018               TO MSG1-CD-ERRO-TS
               MOVE 'Número da versão do tipo de cota do título da opera
      -             'ção inválido.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF

      *    Para tipos de ativo: Fundo de Investimento em Direitos
      *    Creditórios-FDIC e Certificados deRecebíveis-CR é obrigatório
      *    preenchimento da cota
           IF  (SGTS0088-CD-PRD EQUAL 427     ) AND
               (SGTS0088-CD-MDLD EQUAL 03  OR
                SGTS0088-CD-MDLD EQUAL 04     )
             IF  SGTS0088-CD-TIP-COTA-TIT EQUAL ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0017               TO MSG1-CD-ERRO-TS
               MOVE 'Código do tipo de cota do título da operação inváli
      -             'do.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
             END-IF

             IF  SGTS0088-NR-VRS-TIP-COTA EQUAL ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0018               TO MSG1-CD-ERRO-TS
               MOVE 'Número da versão do tipo de cota do título da opera
      -             'ção inválido.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
             END-IF
           END-IF

           IF  SGTS0088-CD-CMRA-OPR-ETRD < ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0019               TO MSG1-CD-ERRO-TS
               MOVE 'Código da câmara de registro de título de operação
      -             'inválido.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0088-NR-EMS-TIT-OPR < ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0020               TO MSG1-CD-ERRO-TS
               MOVE 'Número de emissão do título da operação inválido.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0088-NR-SRE-EMS-TIT-OPR < ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0021               TO MSG1-CD-ERRO-TS
               MOVE 'Número de série da emissão do título da operação in
      -             'válido.'          TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0088-CD-TIP-MC-CPR <= ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0022               TO MSG1-CD-ERRO-TS
               MOVE 'Código do tipo de mercado de compra inválido.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF

      *--> Verificação para Bancos Participantes do Mercado Secundário
           IF  SGTS0088-CD-TIP-MC-CPR = 2
               IF  SGTS0088-ESTOURO-TAB
                   SET ERRO-PROGRAMADO TO TRUE
                   MOVE 0023           TO MSG1-CD-ERRO-TS
                   MOVE 'Quantidade de registros incluídos acima do limi
      -                 'te previsto.'           TO MSG1-TX-ERRO-TS
                   SET MNT-VRV         TO TRUE
                   PERFORM 999000-ERRO
                END-IF

                PERFORM VARYING IC FROM 1 BY 1
                        UNTIL IC GREATER SGTS0088-QT-OCR-TAB

                    IF  SGTS0088-CD-BCO-PCT (IC)  < ZEROS
                        SET ERRO-PROGRAMADO     TO TRUE
                        MOVE 0024               TO MSG1-CD-ERRO-TS
                        MOVE 'Código do banco participante inválido.'
                                                TO MSG1-TX-ERRO-TS
                        SET MNT-VRV             TO TRUE
                        SET COM-ERRO-TAB        TO TRUE
                        PERFORM 999000-ERRO
                    END-IF
               END-PERFORM
           END-IF

           IF  SGTS0088-CD-BCO-CRD-VL-OPR < ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0025               TO MSG1-CD-ERRO-TS
               MOVE 'Código do banco de crédito valor inválido.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0088-CD-DEPE-CRD-VL-OPR < ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0026               TO MSG1-CD-ERRO-TS
               MOVE 'Código da dependência para crédito inválido.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0088-NR-CC-CRD-VL-OPR   NOT NUMERIC
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0027               TO MSG1-CD-ERRO-TS
               MOVE 'Número da conta corrente para crédito do valor invá
      -             'lido.'           TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0088-CD-CLI-EMT     <= ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0028               TO MSG1-CD-ERRO-TS
               MOVE 'Código do cliente emissor inválido.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0088-CD-CLI-IFLC-LMCR  < ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0029               TO MSG1-CD-ERRO-TS
               MOVE 'Código do cliente de impacto no limite crédito invá
      -             'lido.'           TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0088-CD-CLI-CDT     < ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0030               TO MSG1-CD-ERRO-TS
               MOVE 'Código do cliente originador/cedente inválido.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0088-CD-CLI-ADM     < ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0031               TO MSG1-CD-ERRO-TS
               MOVE 'Código do cliente administrador inválido.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0088-CD-CLI-AGT-FDAR  < ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0032               TO MSG1-CD-ERRO-TS
               MOVE 'Código do cliente agente fiduciário inválido.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF

           IF SGTS0088-CD-TIT-OPR-ETRD EQUAL SPACES AND
              SGTS0088-CD-ISIN-TIT-OPR NOT EQUAL SPACES
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0033               TO MSG1-CD-ERRO-TS
               MOVE 'Código ISIN pode ser informado apenas em conjunto
      -              'com código do título.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0088-DT-EMS-TIT-OPR NOT EQUAL SPACES
                MOVE CORR SGTS0088-DT-EMS-TIT-OPR-R  TO  ARG01-R
      *    Valida data no formato Gregoriano
                MOVE 4                               TO  ARG02
                PERFORM 920000-VALIDAR-DATAS
           END-IF

           IF  SGTS0088-QT-PAPL-TIT-EMS  < ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0034               TO MSG1-CD-ERRO-TS
               MOVE 'Quantidade de papéis de emissão do título da operaç
      -             'ão inválido.'    TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0088-VL-PU-EMS-TIT-OPR   NOT NUMERIC
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0035               TO MSG1-CD-ERRO-TS
               MOVE 'Valor do preço unitário emissão do titulo operação
      -             'inválido.'        TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0088-DT-CPR-OPR     NOT EQUAL SPACES
                MOVE CORR SGTS0088-DT-CPR-OPR-R      TO  ARG01-R
      *    Valida data no formato Gregoriano
                MOVE 4                               TO  ARG02
                PERFORM 920000-VALIDAR-DATAS
           END-IF

           IF  SGTS0088-QT-PAPL-TIT-CPR    < ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0036               TO MSG1-CD-ERRO-TS
               MOVE 'Quantidade de papéis de compra título operação invá
      -             'lido.'            TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0088-VL-PU-CPR-TIT-OPR   NOT NUMERIC
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0037               TO MSG1-CD-ERRO-TS
               MOVE 'Valor do preço unitário de compra do titulo da oper
      -             'ação inválido.'   TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0088-VL-CPR-OPR     NOT NUMERIC
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0038               TO MSG1-CD-ERRO-TS
               MOVE 'Valor de compra do título da operação inválido.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0088-DT-VNCT-OPR     NOT EQUAL SPACES
                MOVE CORR SGTS0088-DT-VNCT-OPR-R      TO  ARG01-R
      *    Valida data no formato Gregoriano
                MOVE 4                               TO  ARG02
                PERFORM 920000-VALIDAR-DATAS
           END-IF

           IF  SGTS0088-VL-PRV-ENCT     NOT NUMERIC
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0039               TO MSG1-CD-ERRO-TS
               MOVE 'Valor da previsão de encarteiramento inválido.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0088-VL-APVD-BB-OPR     NOT NUMERIC
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0040               TO MSG1-CD-ERRO-TS
               MOVE 'Valor aprovado pelo BB na operação inválido.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF

           IF (SGTS0088-IN-CNG-PBCO-OPR NOT EQUAL 'S' AND
               SGTS0088-IN-CNG-PBCO-OPR NOT EQUAL 'N')
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0041               TO MSG1-CD-ERRO-TS
               MOVE 'Indicador de contingenciamento público da operação
      -             'inválido.'        TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF

      *     IF (SGTS0088-IN-FLZ-EXCO-OPR NOT EQUAL 'S' AND
      *         SGTS0088-IN-FLZ-EXCO-OPR NOT EQUAL 'N')
      *         SET ERRO-PROGRAMADO     TO TRUE
      *         MOVE 0042               TO MSG1-CD-ERRO-TS
      *         MOVE 'Indicador de flexibilização de exceção da operação
      *-             'inválido.'        TO MSG1-TX-ERRO-TS
      *         SET MNT-VRV             TO TRUE
      *         PERFORM 999000-ERRO
      *     END-IF

           IF (SGTS0088-IN-OPR-ETRD-IETR NOT EQUAL 'S' AND
               SGTS0088-IN-OPR-ETRD-IETR NOT EQUAL 'N')
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0043               TO MSG1-CD-ERRO-TS
               MOVE 'Indicador de operação estruturada de infraestrutura
      -             ' inválido.'       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF

           IF (SGTS0088-IN-N-CNV-OPR-ACAO NOT EQUAL 'S' AND
               SGTS0088-IN-N-CNV-OPR-ACAO NOT EQUAL 'N')
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0044               TO MSG1-CD-ERRO-TS
               MOVE 'Indicador de não conversão da operação em ação invá
      -             'lido.'            TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF

           IF (SGTS0088-IN-N-CNV-OPR-ACAO NOT EQUAL 'S' AND
               SGTS0088-IN-N-CNV-OPR-ACAO NOT EQUAL 'N')
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0045               TO MSG1-CD-ERRO-TS
               MOVE 'Indicador de operação estruturada com suporte em fa
      -             'cilitador inválido.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF

           IF SGTS0088-CD-TIP-VRS-OPR EQUAL ZEROS
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE  0241              TO  MSG1-CD-ERRO-TS
               MOVE  'Código tipo de versão da operação não informado.'
                                       TO  MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF

      *Definido pelo gestor:
      *    De:   CD-TIP-VRS-OPR = 1 (Original)
      *    Para: CD-TIP-VRS-OPR = 1 (Normalidade)
      *
      *na inclusao o tipo de versionamento(Estado Negocial)só pode ser:
      *    CD-TIP-VRS-OPR = 1 (Normalidade)
      *    CD-TIP-VRS-OPR = 2 (Renegociação)
      *    CD-TIP-VRS-OPR = 5 (Reestruturação)

           IF SGTS0088-CD-TIP-VRS-OPR NOT EQUAL 1 AND
              SGTS0088-CD-TIP-VRS-OPR NOT EQUAL 2 AND
              SGTS0088-CD-TIP-VRS-OPR NOT EQUAL 5
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0242               TO MSG1-CD-ERRO-TS
               MOVE 'Código do tipo de versão da operação inválido.'
                                       TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF

           IF  SGTS0088-CD-CMCP < ZEROS
              SET   ERRO-PROGRAMADO   TO  TRUE
              MOVE  1115          TO  MSG1-CD-ERRO-TS
              MOVE 'Código da carteira de mercado de capitais inválido.'
                                       TO  MSG1-TX-ERRO-TS
               SET  MNT-VRV            TO  TRUE
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0088-CD-CMCP EQUAL ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  1116          TO  MSG1-CD-ERRO-TS
               MOVE 'Código da carteira de mercado de capitais não infor
      -              'mado.'
                                       TO  MSG1-TX-ERRO-TS
               SET  MNT-VRV            TO  TRUE
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0088-CD-GR-MCP < ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  1117          TO  MSG1-CD-ERRO-TS
               MOVE  'Código do Grupo Mercado de Capitais inválido.'
                                       TO  MSG1-TX-ERRO-TS
               SET  MNT-VRV            TO  TRUE
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0088-CD-GR-MCP EQUAL ZEROS
              SET   ERRO-PROGRAMADO   TO  TRUE
              MOVE  1118          TO  MSG1-CD-ERRO-TS
              MOVE  'Código do Grupo Mercado de Capitais não informado.'
                                       TO  MSG1-TX-ERRO-TS
              SET  MNT-VRV            TO  TRUE
              PERFORM  999000-ERRO
           END-IF

           IF  SGTS0088-CD-LVRO-MC-CPTL < ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
                MOVE  1119          TO  MSG1-CD-ERRO-TS
                MOVE  'Codigo Livro Mercado Capitais inválido.'
                                       TO  MSG1-TX-ERRO-TS
               SET  MNT-VRV            TO  TRUE
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0088-CD-LVRO-MC-CPTL EQUAL ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  1120          TO  MSG1-CD-ERRO-TS
               MOVE  'Codigo Livro Mercado Capitais não informado.'
                                       TO  MSG1-TX-ERRO-TS
               SET  MNT-VRV            TO  TRUE
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0088-CD-SMCP < ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
                MOVE  1121          TO  MSG1-CD-ERRO-TS
                MOVE  'Codigo Sublivro Mercado Capitais inválido.'
                                       TO  MSG1-TX-ERRO-TS
               SET  MNT-VRV            TO  TRUE
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0088-CD-SMCP EQUAL ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  1122          TO  MSG1-CD-ERRO-TS
               MOVE  'Codigo Sublivro Mercado Capitais não informado.'
                                       TO  MSG1-TX-ERRO-TS
               SET  MNT-VRV            TO  TRUE
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0088-CD-CLSC-ITMT-FNCO < ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
                MOVE  1123          TO  MSG1-CD-ERRO-TS
                MOVE  'Código do Classificação do Instrumento Financeiro
      -               ' inválido.'
                                       TO  MSG1-TX-ERRO-TS
               SET  MNT-VRV            TO  TRUE
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0088-CD-CLSC-ITMT-FNCO EQUAL ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  1124          TO  MSG1-CD-ERRO-TS
               MOVE  'Código do Classificação do Instrumento Financeiro
      -              'não informado.'
                                       TO  MSG1-TX-ERRO-TS
               SET  MNT-VRV            TO  TRUE
               PERFORM  999000-ERRO
           END-IF
      * CD-TIP-VRS-OPR = 1 -> Normalidade
      * CD-TIP-VRS-OPR = 2 -> Renegociação
      * CD-TIP-VRS-OPR = 5 -> Reestruturação

           IF  (SGTS0088-CD-TIP-VRS-OPR  EQUAL 1 OR
                SGTS0088-CD-TIP-VRS-OPR  EQUAL 2 OR
                SGTS0088-CD-TIP-VRS-OPR  EQUAL 5)
             IF SGTS0088-CD-MTV-EST-NEGL EQUAL ZEROS
                SET ERRO-PROGRAMADO     TO TRUE
                MOVE 1132               TO MSG1-CD-ERRO-TS
                MOVE 'Código do Motivo do Estado Negocial não informado
      -              '.'
                                        TO MSG1-TX-ERRO-TS
                SET  MNT-VRV            TO  TRUE
                PERFORM 999000-ERRO
             END-IF
           END-IF

           IF  (SGTS0088-CD-TIP-VRS-OPR  EQUAL 1 OR
                SGTS0088-CD-TIP-VRS-OPR  EQUAL 2 OR
                SGTS0088-CD-TIP-VRS-OPR  EQUAL 5)
             IF SGTS0088-CD-MTV-EST-NEGL < ZEROS
                SET ERRO-PROGRAMADO     TO TRUE
                MOVE 1133               TO MSG1-CD-ERRO-TS
                MOVE 'Código do Motivo do Estado Negocial inválido.'
                                        TO MSG1-TX-ERRO-TS
                SET  MNT-VRV            TO  TRUE
                PERFORM 999000-ERRO
             END-IF
           END-IF

      * Tipo de versionamento 2  Renegociação
      * Motivo do Estado Negocial:1 - Renegociação - Origem Atraso,
      *                           2 - Renegociação - RAO
      *                           3 - Renegociação - Origem Renegociação
      *                           99 -Demais

           IF   SGTS0088-CD-TIP-VRS-OPR    EQUAL 2
             IF ((SGTS0088-CD-MTV-EST-NEGL NOT EQUAL 1) AND
                 (SGTS0088-CD-MTV-EST-NEGL NOT EQUAL 2) AND
                 (SGTS0088-CD-MTV-EST-NEGL NOT EQUAL 3) AND
                 (SGTS0088-CD-MTV-EST-NEGL NOT EQUAL 99))

                 SET ERRO-PROGRAMADO     TO TRUE
                 MOVE 1137               TO MSG1-CD-ERRO-TS
                 MOVE 'Código do Motivo do Estado Negocial de Renegociaç
      -             'ão inválido.'
                                         TO MSG1-TX-ERRO-TS
                 SET  MNT-VRV            TO  TRUE
                 PERFORM 999000-ERRO
             END-IF
           END-IF

      * Tipo de versionamento 5  Reestruturação
      * Motivo do Estado Negocial:
      *    13 - Reestruturação - Atraso
      *    14 - Reestruturação - Abatimento Negocial
      *    15 - Reestruturação - Origem Ativo Problemático
      *    16 - Reestruturação - Origens Perdas
      *    17 - Reestruturação - Stand Still
      *    18 - Reestruturação - Origens - Agro
      *    19 - Reestruturação - Outros
      *    99 - Demais

           IF   SGTS0088-CD-TIP-VRS-OPR    EQUAL 5
             IF ((SGTS0088-CD-MTV-EST-NEGL NOT EQUAL 13) AND
                 (SGTS0088-CD-MTV-EST-NEGL NOT EQUAL 14) AND
                 (SGTS0088-CD-MTV-EST-NEGL NOT EQUAL 15) AND
                 (SGTS0088-CD-MTV-EST-NEGL NOT EQUAL 16) AND
                 (SGTS0088-CD-MTV-EST-NEGL NOT EQUAL 17) AND
                 (SGTS0088-CD-MTV-EST-NEGL NOT EQUAL 18) AND
                 (SGTS0088-CD-MTV-EST-NEGL NOT EQUAL 19) AND
                 (SGTS0088-CD-MTV-EST-NEGL NOT EQUAL 99))

                 SET ERRO-PROGRAMADO     TO TRUE
                 MOVE 1138               TO MSG1-CD-ERRO-TS
                 MOVE 'Código do Motivo do Estado Negocial de Reestrutur
      -             'ação inválido.'
                                         TO MSG1-TX-ERRO-TS
                 SET  MNT-VRV            TO  TRUE
                 PERFORM 999000-ERRO
             END-IF
           END-IF

      * Necessidade da tabela do OPR, o motivo do estado negocial nao po
      * de ser Zeros  quando tipo versionamento = 1 Normalidade.
           IF  SGTS0088-CD-TIP-VRS-OPR  EQUAL 1
             IF SGTS0088-CD-MTV-EST-NEGL NOT EQUAL 99
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 1139               TO MSG1-CD-ERRO-TS
               MOVE 'Código do Motivo do Estado Negocial de Normalidade
      -              'inválido.'
                                       TO MSG1-TX-ERRO-TS
               SET  MNT-VRV            TO  TRUE
               PERFORM 999000-ERRO
             END-IF
           END-IF

           PERFORM 111000-COMPARA-DATA

           PERFORM 112000-VALIDA-ATRIBUTOS

           MOVE 'N' TO IN-CAC

           PERFORM 113000-VERIFICA-CAC
           .
       110000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       111000-COMPARA-DATA                  SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                   TO MSG1-CD-SEQL-ERRO

           MOVE CORR SGTS0088-DT-CPR-OPR-R
                                       TO  DT-PSQ1-R
           MOVE CORR SGTS0088-DT-EMS-TIT-OPR-R
                                       TO  DT-PSQ2-R
           IF DT-PSQ1-R < DT-PSQ2-R
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0046               TO MSG1-CD-ERRO-TS
               MOVE 'Data de compra deve ser maior ou igual a data de em
      -             'issão.'           TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF

           MOVE CORR SGTS0088-DT-VNCT-OPR-R
                                       TO  DT-PSQ1-R
           MOVE CORR SGTS0088-DT-EMS-TIT-OPR-R
                                       TO  DT-PSQ2-R
           IF DT-PSQ1-R < DT-PSQ2-R
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0047               TO MSG1-CD-ERRO-TS
               MOVE 'Data de vencimento deve ser maior ou igual a data d
      -             'e emissão .'      TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF

           MOVE CORR SGTS0088-DT-VNCT-OPR-R
                                       TO  DT-PSQ1-R
           MOVE CORR SGTS0088-DT-CPR-OPR-R
                                       TO  DT-PSQ2-R
           IF DT-PSQ1-R <  DT-PSQ2-R
               SET ERRO-PROGRAMADO     TO TRUE
               MOVE 0048               TO MSG1-CD-ERRO-TS
               MOVE 'Data de vencimento deve ser maior ou igual a data d
      -             'e compra.'            TO MSG1-TX-ERRO-TS
               SET MNT-VRV             TO TRUE
               PERFORM 999000-ERRO
           END-IF
           .
       111000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       112000-VALIDA-ATRIBUTOS                 SECTION.
      *-----------------------------------------------------------------
           MOVE 0004                  TO MSG1-CD-SEQL-ERRO

           INITIALIZE    SGTS0148-DADOS.
           MOVE SGTS0088-CD-PRD        TO  SGTS0148-CD-PRD
           MOVE SGTS0088-CD-MDLD       TO  SGTS0148-CD-MDLD

           MOVE SGTS0148               TO    NM-PGM
           PERFORM 900000-EXECUTAR-MODULO

           IF    SGTS0148-IN-HBLT-CD-BCO-CRD-VL   EQUAL   'N'
                MOVE   ZEROS           TO   SGTS0088-CD-BCO-CRD-VL-OPR
           END-IF

           IF    SGTS0148-IN-HBLT-CD-CLI-ADM   EQUAL   'N'
                MOVE   ZEROS           TO   SGTS0088-CD-CLI-ADM
           END-IF

           IF    SGTS0148-IN-HBLT-CD-CLI-FDAR   EQUAL   'N'
                MOVE   ZEROS           TO   SGTS0088-CD-CLI-AGT-FDAR
           END-IF

           IF    SGTS0148-IN-HBLT-CD-CLI-CDT   EQUAL   'N'
                MOVE   ZEROS           TO   SGTS0088-CD-CLI-CDT
           END-IF

           IF    SGTS0148-IN-HBLT-CD-CLI-EMT   EQUAL   'N'
                MOVE   ZEROS           TO   SGTS0088-CD-CLI-EMT
           END-IF


           IF    SGTS0148-IN-HBLT-CD-CMRA-OPR   EQUAL   'N'
                MOVE   ZEROS           TO   SGTS0088-CD-CMRA-OPR-ETRD
           END-IF

           IF    SGTS0148-IN-HBLT-CD-CTGR-TVM   EQUAL   'N'
                MOVE   ZEROS           TO   SGTS0088-CD-CTGR-TVM
           END-IF

           IF    SGTS0148-IN-HBLT-CD-DEPE-CRD   EQUAL   'N'
                MOVE   ZEROS           TO   SGTS0088-CD-DEPE-CRD-VL-OPR
           END-IF

           IF    SGTS0148-IN-HBLT-CD-ESPC-DBTR   EQUAL   'N'
                MOVE   ZEROS           TO   SGTS0088-CD-ESPC-DBTR-OPR
           END-IF

           IF    SGTS0148-IN-HBLT-CD-ISIN-TIT   EQUAL   'N'
                MOVE   SPACES          TO   SGTS0088-CD-ISIN-TIT-OPR
           END-IF

           IF    SGTS0148-IN-HBLT-CD-ITC-CVM   EQUAL   'N'
                MOVE   ZEROS           TO   SGTS0088-CD-ITC-CVM
           END-IF

           IF    SGTS0148-IN-HBLT-CD-MDLD   EQUAL   'N'
                MOVE   ZEROS           TO   SGTS0088-CD-MDLD
           END-IF

           IF    SGTS0148-IN-HBLT-CD-PRD   EQUAL   'N'
                MOVE   ZEROS           TO   SGTS0088-CD-PRD
           END-IF

           IF    SGTS0148-IN-HBLT-CD-RSCO-OPR   EQUAL   'N'
                MOVE   SPACES          TO   SGTS0088-CD-RSCO-OPR
           END-IF

           IF    SGTS0148-IN-HBLT-CD-TIP-COTA   EQUAL   'N'
                MOVE   ZEROS           TO   SGTS0088-CD-TIP-COTA-TIT
           END-IF

           IF    SGTS0148-IN-HBLT-CD-TIP-MC-CPR   EQUAL   'N'
                MOVE   ZEROS           TO   SGTS0088-CD-TIP-MC-CPR
           END-IF

           IF    SGTS0148-IN-HBLT-CD-TIT-OPR   EQUAL   'N'
                MOVE   SPACES          TO   SGTS0088-CD-TIT-OPR-ETRD
           END-IF

           IF    SGTS0148-IN-HBLT-CD-UOR-DTTR   EQUAL   'N'
                MOVE   ZEROS           TO   SGTS0088-CD-UOR-DTTR-OPR
           END-IF

           IF    SGTS0148-IN-HBLT-DT-CPR-TIT   EQUAL   'N'
                MOVE   SPACES          TO   SGTS0088-DT-CPR-OPR
           END-IF

           IF    SGTS0148-IN-HBLT-DT-EMS-TIT   EQUAL   'N'
                MOVE   SPACES          TO   SGTS0088-DT-EMS-TIT-OPR
           END-IF

           IF    SGTS0148-IN-HBLT-DT-VNCT-OPR   EQUAL   'N'
                MOVE   SPACES          TO   SGTS0088-DT-VNCT-OPR
           END-IF

           IF    SGTS0148-IN-HBLT-IN-CNG-PBCO   EQUAL   'N'
                MOVE   'N'             TO   SGTS0088-IN-CNG-PBCO-OPR
           END-IF

      *     IF    SGTS0148-IN-HBLT-IN-FLZ-EXCO   EQUAL   'N'
      *          MOVE   'N'             TO   SGTS0088-IN-FLZ-EXCO-OPR
      *     END-IF

           IF    SGTS0148-IN-HBLT-N-CNV-ACAO   EQUAL   'N'
                MOVE   'N'             TO   SGTS0088-IN-N-CNV-OPR-ACAO
           END-IF
      *    Debentures devem ter o campo desabilitado na baixa, porem
      *    marcado como "SIM".
           IF  SGTS0088-CD-PRD         EQUAL 427
           AND SGTS0088-CD-MDLD        EQUAL 2
                MOVE  'S'              TO  SGTS0088-IN-N-CNV-OPR-ACAO
           END-IF

           IF    SGTS0148-IN-HBLT-IN-SPT-FCTR   EQUAL   'N'
                MOVE   'N'             TO   SGTS0088-IN-OPR-SPT-FCTR
           END-IF

           IF    SGTS0148-IN-HBLT-NR-CC-CRD-VL   EQUAL   'N'
                MOVE   ZEROS           TO   SGTS0088-NR-CC-CRD-VL-OPR
           END-IF

           IF    SGTS0148-IN-HBLT-NR-DVS-TIT   EQUAL   'N'
                MOVE   ZEROS           TO   SGTS0088-NR-DVS-TIT
           END-IF

           IF    SGTS0148-IN-HBLT-NR-EMS-TIT   EQUAL   'N'
                MOVE   ZEROS           TO   SGTS0088-NR-EMS-TIT-OPR
           END-IF

           IF    SGTS0148-IN-HBLT-NR-SRE-TIT   EQUAL   'N'
                MOVE   ZEROS           TO   SGTS0088-NR-SRE-EMS-TIT-OPR
           END-IF

           IF    SGTS0148-IN-HBLT-OPR-ETRD-IETR   EQUAL   'N'
                MOVE   SPACES          TO   SGTS0088-IN-OPR-ETRD-IETR
           END-IF

           IF    SGTS0148-IN-HBLT-NR-VRS-COTA   EQUAL   'N'
                MOVE   ZEROS           TO   SGTS0088-NR-VRS-TIP-COTA
           END-IF

           IF    SGTS0148-IN-HBLT-QT-PAPL-CPR   EQUAL   'N'
                MOVE   ZEROS           TO   SGTS0088-QT-PAPL-TIT-CPR
           END-IF

           IF    SGTS0148-IN-HBLT-QT-PAPL-EMS   EQUAL   'N'
                MOVE   ZEROS           TO   SGTS0088-QT-PAPL-TIT-EMS
           END-IF

           IF    SGTS0148-IN-HBLT-VL-APVD-BB   EQUAL   'N'
                MOVE   ZEROS           TO   SGTS0088-VL-APVD-BB-OPR
           END-IF

           IF    SGTS0148-IN-HBLT-VL-CPR-TIT   EQUAL   'N'
                MOVE   ZEROS           TO   SGTS0088-VL-CPR-OPR
           END-IF

           IF    SGTS0148-IN-HBLT-VL-PRV-ENCT   EQUAL   'N'
                MOVE   ZEROS           TO   SGTS0088-VL-PRV-ENCT
           END-IF

           IF    SGTS0148-IN-HBLT-VL-CPR-TIT   EQUAL   'N'
                MOVE   ZEROS           TO   SGTS0088-VL-PU-CPR-TIT-OPR
           END-IF

           IF    SGTS0148-IN-HBLT-VL-EMS-TIT   EQUAL   'N'
                MOVE   ZEROS           TO   SGTS0088-VL-PU-EMS-TIT-OPR
           END-IF
           .
       112000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       113000-VERIFICA-CAC        SECTION.
      *--> Verifica se a carteira está setada para receber informações
      *--> Via CAC ('S' - processo novo - 4966)   OU
      *--> Via Sinqia ('N' - processo atual)
      *----------------------------------------------------------------*
           MOVE 0005                  TO MSG1-CD-SEQL-ERRO

      * CD-TIP-ATB-CTRA = 100 - Indicador Importaçao Arquivo via CAC
      * CD-TIP-EST-DCS  = 2 - Aprovado
           MOVE SGTSPE23                TO  NM-PGM
           MOVE SGTS0088-CD-CTRA-TIT-VL-OPR
                                        TO  SGTSPE23-CD-CTRA-TIT-VL
           MOVE 100                     TO  SGTSPE23-CD-TIP-ATB-CTRA
           MOVE 2                       TO  SGTSPE23-CD-TIP-EST-DCS
           MOVE SPACES                  TO  SGTSPE23-TS-EST-CRTC-CTRA

           EXEC CICS LINK
                PROGRAM  (SGTSPE23)
                COMMAREA (SGTSPE23-DADOS)
           END-EXEC

           IF  SGTSPE23-CD-RTN-PGM     NOT EQUAL ZEROS
               SET ERRO-PROGRAMA-ACIONADO TO TRUE
               MOVE NM-PGM             TO MSG1-NM-PGM-ACND
               MOVE SGTSPE23-MSG-ERRO-PGM
                                       TO MSG1-TX-RTN-PGM-ACND
                                          MSG1-TX-ERRO-TS
               MOVE SGTSPE23-CD-RTN-PGM
                                       TO MSG1-CD-RTN-PGM-ACND
                                          MSG1-CD-ERRO-TS

      *        Monitorar variáveis do módulo chamado
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-ERRO
           END-IF

           IF SGTSPE23-TX-VCLD-CRCT EQUAL 'S'
              MOVE 'S' TO IN-CAC
           END-IF
           .
       113000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-PROCESSAR                SECTION.
      *-----------------------------------------------------------------

           PERFORM 210000-VERIFICA-DADOS-EXTERNOS

           IF  SGTS0088-CD-TIT-OPR-ETRD   NOT EQUAL SPACES
               PERFORM 220000-VALIDAR-TIT-OPR-ETRD
           END-IF

           PERFORM 230000-OBTER-UOR-BB-CRD-VL

           PERFORM 240000-INCLUIR-OPR-ETRD-MCP

           PERFORM 250000-INCLUIR-EST-OPR-ETRD

           PERFORM 310000-INCLUIR-EST-VRS

           IF SGTS0088-CD-OPR-ETRD-COPD NOT EQUAL ZEROS AND
              SGTS0088-NR-VRS-OPR-ETRD-COPD NOT EQUAL ZEROS
                PERFORM 260000-OBTER-COP-DADO-VCLD-OPR
           END-IF

           PERFORM 270000-TRATAR-CLIENTES

           PERFORM 280000-TRATAR-INDICADORES

      *--> Verificação para Bancos Participantes do Mercado Secundário
           IF  SGTS0088-CD-TIP-MC-CPR = 2
              PERFORM 290000-TRATAR-BCO-PCT VARYING IC FROM 1 BY 1
                      UNTIL IC GREATER SGTS0088-QT-OCR-TAB
           END-IF

           PERFORM 300000-MOVIMENTAR-SAIDA
           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       210000-VERIFICA-DADOS-EXTERNOS       SECTION.
      *-----------------------------------------------------------------

           PERFORM 211000-VALIDA-PRD-MDLD

           PERFORM 212000-VALIDA-UOR-BB

           PERFORM VARYING IC FROM 1 BY 1
                        UNTIL IC GREATER SGTS0088-QT-OCR-TAB
                 IF  SGTS0088-CD-BCO-PCT (IC)  NOT EQUAL ZEROS
                    PERFORM 213000-VALIDA-BCO-PCT
                 END-IF
           END-PERFORM

           IF SGTS0088-CD-RSCO-OPR NOT EQUAL SPACES
              PERFORM 214000-VALIDA-RISCO
           END-IF

           IF SGTS0088-CD-BCO-CRD-VL-OPR NOT EQUAL ZEROS
              PERFORM 215000-BCO-CRD-VL-OPR
           END-IF

           IF SGTS0088-CD-BCO-CRD-VL-OPR NOT EQUAL ZEROS  AND
              SGTS0088-CD-DEPE-CRD-VL-OPR NOT EQUAL ZEROS
              PERFORM 216000-VALIDA-AGENCIA
           END-IF

           PERFORM 217000-VALIDA-CLI-EMISSOR

           IF SGTS0088-CD-CLI-IFLC-LMCR NOT EQUAL ZEROS
              PERFORM 217100-VALIDA-CLI-IFLC-LMCR
           END-IF

           IF SGTS0088-CD-CLI-CDT NOT EQUAL ZEROS
              PERFORM 217200-VALIDA-CLI-CDT
           END-IF

           IF  SGTS0088-CD-CLI-ADM  NOT EQUAL ZEROS
              PERFORM 217300-VALIDA-CLI-ADM
           END-IF

           IF SGTS0088-CD-CLI-AGT-FDAR NOT EQUAL ZEROS
              PERFORM 217400-VALIDA-CLI-AGT-FDAR
           END-IF

           IF SGTS0088-CD-CMRA-OPR-ETRD NOT EQUAL ZEROS
              PERFORM 218000-VALIDA-CMRA-REG
           END-IF

           IF SGTS0088-CD-TIP-COTA-TIT NOT EQUAL ZEROS
              PERFORM 219000-VALIDA-TIP-COTA-ATI
           END-IF

           PERFORM 219100-VALIDA-CTGR-TVM-ATI

           IF SGTS0088-CD-ITC-CVM  NOT EQUAL ZEROS
              PERFORM 219200-VALIDA-ITC-CVM-ATI
           END-IF

           IF SGTS0088-CD-ESPC-DBTR-OPR  NOT EQUAL ZEROS
              PERFORM 219300-VALIDA-ESPC-DBTR-ATI
           END-IF
           .
       210000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       211000-VALIDA-PRD-MDLD       SECTION.
      *-->Verifica existencia de produto/modalidade
      *-----------------------------------------------------------------
           MOVE 0006                   TO MSG1-CD-SEQL-ERRO

      *--> Movimentar o nome do programa a ser executado
           INITIALIZE                  PRDS0203-DADOS.

           MOVE PRDS0203               TO NM-PGM

      *--> Movimentar os campos de entrada

           MOVE 1                      TO S0203-CD-FUNCAO
           MOVE SGTS0088-CD-PRD        TO S0203-CD-PRD
           MOVE SGTS0088-CD-MDLD       TO S0203-CD-MDLD

           PERFORM 910000-CHAMAR-SERVICO

           IF  S0203-CD-RTN     NOT EQUAL ZEROS
               SET ERRO-PROGRAMA-ACIONADO TO TRUE
               MOVE 3004               TO MSG1-CD-ERRO-TS
               STRING  'Erro ao acionar o módulo ' NM-PGM '.'
                            DELIMITED BY SIZE
                            INTO           MSG1-TX-ERRO-TS
               MOVE NM-PGM             TO MSG1-NM-PGM-ACND
               MOVE S0203-TX-RTN       TO MSG1-TX-RTN-PGM-ACND
               MOVE S0203-CD-RTN       TO MSG1-CD-RTN-PGM-ACND

      *        Monitorar variáveis do módulo chamado
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-ERRO
           END-IF
           .
       211000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       212000-VALIDA-UOR-BB  SECTION.
      *--> Verifica existencia UOR detentora da operação
      *-----------------------------------------------------------------
           MOVE 0007                   TO MSG1-CD-SEQL-ERRO

      *--> Movimentar o nome do programa a ser executado
           INITIALIZE                 MSTS8042-DADOS.

           MOVE MSTS8042               TO NM-PGM

      *--> Movimentar os campos de entrada

           MOVE SGTS0088-CD-UOR-DTTR-OPR
                                       TO MSTS8042-CD-UOR

           PERFORM 910000-CHAMAR-SERVICO
           .
       212000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       213000-VALIDA-BCO-PCT     SECTION.
      *--> Verifica existencia de banco do banco participante
      *-----------------------------------------------------------------
           MOVE 0008                   TO MSG1-CD-SEQL-ERRO

      *--> Movimentar o nome do programa a ser executado
           MOVE CAFS0053               TO NM-PGM

           INITIALIZE  CAFS0053-DADOS
      *--> Movimentar os campos de entrada
      *--> Código compe
           MOVE 2                      TO S053-FUC
           MOVE SGTS0088-CD-BCO-PCT (IC)
                                       TO S053-CD-CPSO OF S053-ENTD

           PERFORM 910000-CHAMAR-SERVICO
           .
       213000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       214000-VALIDA-RISCO    SECTION.
      *-----------------------------------------------------------------
           MOVE 0009                   TO MSG1-CD-SEQL-ERRO

      *--> Movimentar o nome do programa a ser executado
           INITIALIZE         ARC-DADOS

      *--> Movimentar os campos de entrada

           MOVE SGTS0088-CD-RSCO-OPR   TO 318L-CD-RISCO-E

           PERFORM 961000-CHAMA-ARCSB318
           .
       214000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       215000-BCO-CRD-VL-OPR     SECTION.
      *--> Verifica existencia de banco de credito valor da operação
      *-----------------------------------------------------------------
           MOVE 0010                   TO MSG1-CD-SEQL-ERRO

      *--> Movimentar o nome do programa a ser executado
           MOVE CAFS0053               TO NM-PGM

           INITIALIZE  CAFS0053-DADOS
      *--> Movimentar os campos de entrada
      *--> Código compe
           MOVE 2                      TO S053-FUC
           MOVE SGTS0088-CD-BCO-CRD-VL-OPR
                                       TO S053-CD-CPSO OF S053-ENTD

           PERFORM 910000-CHAMAR-SERVICO
           .
       215000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       216000-VALIDA-AGENCIA   SECTION.
      *--> Aciona a rotina CAFS0007 e verifica exitência agencia
      *-----------------------------------------------------------------
           MOVE 0011                   TO MSG1-CD-SEQL-ERRO

      *--> Movimentar o nome do programa a ser executado
           MOVE CAFS0007               TO NM-PGM

           INITIALIZE  PARM-DADOS
      *--> Movimentar os campos de entrada
           MOVE SGTS0088-CD-BCO-CRD-VL-OPR
                                       TO PARM-BANCO-INF
           MOVE SGTS0088-CD-DEPE-CRD-VL-OPR
                                       TO PARM-AGE-INF

           PERFORM 910000-CHAMAR-SERVICO
           .
       216000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       217000-VALIDA-CLI-EMISSOR    SECTION.
      *--> Verifica existencia cliente emissor no MCI.
      *-----------------------------------------------------------------
           MOVE 0012                   TO MSG1-CD-SEQL-ERRO

      *--> Movimentar o nome do programa a ser executado
           INITIALIZE                 MCIS0001-DADOS.

           MOVE MCIS0001               TO NM-PGM

      *--> Movimentar os campos de entrada
           MOVE SGTS0088-CD-CLI-EMT    TO MCIS0001-CLIENTE

           PERFORM 910000-CHAMAR-SERVICO
           .
       217000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       217100-VALIDA-CLI-IFLC-LMCR   SECTION.
      *--> Verifica existencia cliente impacto limite no MCI.
      *-----------------------------------------------------------------
           MOVE 0013                   TO MSG1-CD-SEQL-ERRO

      *--> Movimentar o nome do programa a ser executado
           INITIALIZE                 MCIS0001-DADOS.

           MOVE MCIS0001               TO NM-PGM

      *--> Movimentar os campos de entrada
           MOVE SGTS0088-CD-CLI-IFLC-LMCR
                                       TO MCIS0001-CLIENTE

           PERFORM 910000-CHAMAR-SERVICO
           .
       217100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       217200-VALIDA-CLI-CDT    SECTION.
      *--> Verifica existencia cliente cedente no MCI.
      *-----------------------------------------------------------------
           MOVE 0014                   TO MSG1-CD-SEQL-ERRO

      *--> Movimentar o nome do programa a ser executado
           INITIALIZE                 MCIS0001-DADOS.

           MOVE MCIS0001               TO NM-PGM

      *--> Movimentar os campos de entrada
           MOVE SGTS0088-CD-CLI-CDT    TO MCIS0001-CLIENTE

           PERFORM 910000-CHAMAR-SERVICO
           .
       217200-SAI.
           EXIT.

      *-----------------------------------------------------------------
       217300-VALIDA-CLI-ADM    SECTION.
      *--> Verifica existencia cliente administrador no MCI.
      *-----------------------------------------------------------------
           MOVE 0015                   TO MSG1-CD-SEQL-ERRO

      *--> Movimentar o nome do programa a ser executado
           INITIALIZE                 MCIS0001-DADOS.

           MOVE MCIS0001               TO NM-PGM

      *--> Movimentar os campos de entrada
           MOVE SGTS0088-CD-CLI-ADM    TO MCIS0001-CLIENTE

           PERFORM 910000-CHAMAR-SERVICO
           .
       217300-SAI.
           EXIT.

      *-----------------------------------------------------------------
       217400-VALIDA-CLI-AGT-FDAR    SECTION.
      *--> Verifica existencia cliente agente fiduciário no MCI.
      *-----------------------------------------------------------------
           MOVE 0016                   TO MSG1-CD-SEQL-ERRO

      *--> Movimentar o nome do programa a ser executado
           INITIALIZE                 MCIS0001-DADOS.

           MOVE MCIS0001               TO NM-PGM

      *--> Movimentar os campos de entrada
           MOVE SGTS0088-CD-CLI-AGT-FDAR
                                       TO MCIS0001-CLIENTE

           PERFORM 910000-CHAMAR-SERVICO
           .
       217400-SAI.
           EXIT.

      *-----------------------------------------------------------------
       218000-VALIDA-CMRA-REG    SECTION.
      *--> Verifica existência da Câmara de Registro
      *-----------------------------------------------------------------
           MOVE 0017                   TO MSG1-CD-SEQL-ERRO

      *--> Movimentar o nome do programa a ser executado

           MOVE ICLSI046              TO NM-PGM

      *--> Movimentar os campos de entrada
           MOVE SGTS0088-CD-CMRA-OPR-ETRD
                                       TO I046-CD-CLRG

           PERFORM 910000-CHAMAR-SERVICO
           .
       218000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       219000-VALIDA-TIP-COTA-ATI   SECTION.
      *-->Verifica se tipo de cota é ativa e estado da versao = aprovado
      *-----------------------------------------------------------------
           MOVE 0018                   TO MSG1-CD-SEQL-ERRO

      *--> Movimentar o nome do programa a ser executado
           INITIALIZE                 SGTST027-DADOS.

           MOVE SGTST027               TO NM-PGM

      *--> Movimentar os campos de entrada

           SET SGTST027-TIP-COTA-ATI-APVD
                                       TO TRUE
           MOVE SGTS0088-CD-TIP-COTA-TIT
                                       TO SGTST027-CD-TIP-COTA-TIT
           MOVE SGTS0088-NR-VRS-TIP-COTA
                                       TO SGTST027-NR-VRS-TIP-COTA
           MOVE  KOPRD-CD-OPRD         TO SGTST027-CD-USU-RSP-ATL

           PERFORM 962000-CHAMA-SGTST027
           .
       219000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       219100-VALIDA-CTGR-TVM-ATI   SECTION.
      *--> Verifica se Categoria Bacen TVM está ativa
      *-----------------------------------------------------------------
           MOVE 0019                   TO MSG1-CD-SEQL-ERRO

      *--> Movimentar o nome do programa a ser executado
           INITIALIZE                 SGTST029-DADOS.

           MOVE SGTST029               TO NM-PGM

      *--> Movimentar os campos de entrada

           SET SGTST029-CONSULTAR-CTGR-ATI
                                       TO TRUE
           MOVE SGTS0088-CD-CTGR-TVM   TO SGTST029-CD-CTGR-TVM

           PERFORM 963000-CHAMA-SGTST029
           .
       219100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       219200-VALIDA-ITC-CVM-ATI   SECTION.
      *--> Verifica se Instrução CVM está ativa
      *-----------------------------------------------------------------
           MOVE 0020                   TO MSG1-CD-SEQL-ERRO

      *--> Movimentar o nome do programa a ser executado
           INITIALIZE                 SGTST030-DADOS.

           MOVE SGTST030               TO NM-PGM

      *--> Movimentar os campos de entrada

           SET SGTST030-CONS-ITC-CVM-ATI
                                       TO TRUE
           MOVE SGTS0088-CD-ITC-CVM    TO SGTST030-CD-ITC-CVM-OPR

           PERFORM 964000-CHAMA-SGTST030
           .
       219200-SAI.
           EXIT.

      *-----------------------------------------------------------------
       219300-VALIDA-ESPC-DBTR-ATI   SECTION.
      *--> Verifica se Espécie de Debenture da Operação está ativa
      *-----------------------------------------------------------------
           MOVE 0021                   TO MSG1-CD-SEQL-ERRO

      *--> Movimentar o nome do programa a ser executado
           INITIALIZE                 SGTST031-DADOS.

           MOVE SGTST031               TO NM-PGM

      *--> Movimentar os campos de entrada

           SET SGTST031-CONS-ESPC-ATI  TO TRUE
           MOVE SGTS0088-CD-ESPC-DBTR-OPR
                                       TO SGTST031-CD-ESPC-DBTR-OPR

           PERFORM 965000-CHAMA-SGTST031
           .
       219300-SAI.
           EXIT.

      *-----------------------------------------------------------------
       220000-VALIDAR-TIT-OPR-ETRD         SECTION.
      *-----------------------------------------------------------------
           MOVE 0022                   TO MSG1-CD-SEQL-ERRO

           IF IN-CAC EQUAL 'S'

              PERFORM 222000-VERIFICA-TITULO-CAC

      * MOCK-POINT MCKV1
              IF SGTS211A-IN-TIT-VCLD-OTR-OPR EQUAL 'N'
                 IF SGTS211A-IN-TIT-VCLD-OTR-CTRA EQUAL 'S'
                    PERFORM 223000-CONSULTAR-MAIOR-DIV-TIT
                    ADD 1  TO  SGTS0088-NR-DVS-TIT
                    PERFORM 225000-INCLUIR-TIT-OPR-ETRD
                 ELSE
                    MOVE 1 TO SGTS0088-NR-DVS-TIT
                    PERFORM 226000-CONSULTAR-TIT-OPR-ETRD
                    IF SGTST002-CD-RTN-PGM   EQUAL 100
                       PERFORM 225000-INCLUIR-TIT-OPR-ETRD
                    END-IF
                    IF  SGTST002-CD-RTN-PGM   EQUAL ZEROS
                       PERFORM 224000-ALTERAR-TIT-OPR-ETRD
                    END-IF
                 END-IF
              ELSE
                 PERFORM 223000-CONSULTAR-MAIOR-DIV-TIT
                 IF SGTS211A-IN-PU-DTA-CPR-IGUAIS EQUAL 'N'
                    ADD 1  TO  SGTS0088-NR-DVS-TIT
                    PERFORM 225000-INCLUIR-TIT-OPR-ETRD
                 ELSE
                    IF SGTS211A-IN-EST-OPR-ENCO EQUAL 'N'
                       ADD 1  TO  SGTS0088-NR-DVS-TIT
                       PERFORM 225000-INCLUIR-TIT-OPR-ETRD
                    ELSE
                       SET ERRO-PROGRAMADO  TO TRUE
                       MOVE 1135       TO MSG1-CD-ERRO-TS

                       MOVE SGTS211A-CD-OPR-ETRD-MCP
                                      TO AUX-CD-OPR-ETRD-MCP

                       MOVE SGTS211A-NR-VRS-OPR-ETRD
                                     TO AUX-NR-VRS-OPR-ETRD
                       STRING 'Já existe operação SGT ativa para esta
      -                       'compra com este título na Operação = '
                       DELIMITED BY SIZE
                                 AUX-CD-OPR-ETRD-MCP
                       DELIMITED BY SPACE
                              ' e Versão = '
                       DELIMITED BY SIZE
                                  AUX-NR-VRS-OPR-ETRD
                       DELIMITED BY SIZE
                                 INTO MSG1-TX-ERRO-TS
                       END-STRING
                       SET  MNT-VRV            TO  TRUE
                       PERFORM 999000-ERRO
                    END-IF
                 END-IF
              END-IF
      * Fluxo Via Sinqia
           ELSE
               MOVE 1                  TO SGTS0088-NR-DVS-TIT
               PERFORM 226000-CONSULTAR-TIT-OPR-ETRD

               IF SGTST002-CD-RTN-PGM   EQUAL 100
                  PERFORM 225000-INCLUIR-TIT-OPR-ETRD
               END-IF

               IF  SGTST002-CD-RTN-PGM   EQUAL ZEROS
                 PERFORM 221000-VERIFICA-TITULO
                 IF SGTS0211-CD-CTRA-TIT-VL NOT EQUAL ZEROS
                   OR SGTS0211-CD-OPR-ETRD-MCP NOT EQUAL ZEROS
                   OR SGTS0211-NR-VRS-OPR-ETRD NOT EQUAL ZEROS
                   SET ERRO-PROGRAMADO     TO TRUE
                   MOVE 0423                TO MSG1-CD-ERRO-TS
                   MOVE 'Título já existente, atualização não efetuada.'
                                          TO MSG1-TX-ERRO-TS
                   PERFORM 999000-ERRO
                 ELSE
                   MOVE 1  TO  SGTS0088-NR-DVS-TIT
                   PERFORM 224000-ALTERAR-TIT-OPR-ETRD
                 END-IF
               END-IF
           END-IF
           .
       220000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       221000-VERIFICA-TITULO       SECTION.
      *--> Verifica se título está vinculado a uma operação ativa
      *-----------------------------------------------------------------
           MOVE 0023                   TO  MSG1-CD-SEQL-ERRO

           MOVE SGTS0211               TO NM-PGM

           MOVE SGTS0088-CD-TIT-OPR-ETRD
                                       TO SGTS0211-CD-TIT-OPR-ETRD
           MOVE 1
                                       TO SGTS0211-NR-DVS-TIT

           PERFORM 900000-EXECUTAR-MODULO

           .
       221000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       222000-VERIFICA-TITULO-CAC     SECTION.
      *--> Verifica se título está vinculado a alguma operação no SGT
      *--> com mesma carteira
      *-----------------------------------------------------------------
           MOVE 0024                   TO  MSG1-CD-SEQL-ERRO

           MOVE SGTS211A               TO NM-PGM

           MOVE SGTS0088-CD-TIT-OPR-ETRD
                                       TO SGTS211A-E-CD-TIT-OPR-ETRD
           MOVE SGTS0088-CD-CTRA-TIT-VL-OPR
                                       TO SGTS211A-E-CD-CTRA-TIT-VL
           MOVE ZEROS
                                       TO SGTS211A-E-CD-OPR-ETRD
           MOVE  '  .  .    '
                                       TO  SGTS211A-E-DT-CPR-OPR
           MOVE CORR SGTS0088-DT-CPR-OPR-R
                                       TO  SGTS211A-E-DT-CPR-OPR-R

           MOVE SGTS0088-VL-PU-CPR-TIT-OPR
                                       TO SGTS211A-E-VL-PU-CPR-TIT-OPR

           PERFORM 900000-EXECUTAR-MODULO

           .
       222000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       223000-CONSULTAR-MAIOR-DIV-TIT     SECTION.
      *--> Verifica a maior tranche do título
      *-----------------------------------------------------------------
           MOVE 0025                   TO  MSG1-CD-SEQL-ERRO

           MOVE ZERO                   TO SGTS0088-NR-DVS-TIT

           EXEC SQL
                 SELECT MAX (NR_DVS_TIT)
                 INTO :SGTS0088-NR-DVS-TIT
                 FROM DB2SGT.TIT_OPR_ETRD_MCP
                 WHERE CD_TIT_OPR_ETRD = :SGTS0088-CD-TIT-OPR-ETRD
           END-EXEC

      * MOCK-POINT MCKV2

            IF  SQLCODE = +100
                SET ERRO-PROGRAMADO  TO TRUE
                MOVE  0100              TO  MSG1-CD-ERRO-TS
                MOVE 'Registro inexistente.'
                                        TO  MSG1-TX-ERRO-TS
                SET  MNT-VRV            TO  TRUE
                PERFORM 999000-ERRO
            END-IF

           .
       223000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       224000-ALTERAR-TIT-OPR-ETRD     SECTION.
      *-----------------------------------------------------------------
           MOVE 0026                   TO  MSG1-CD-SEQL-ERRO

      *--> Movimentar os campos de entrada

           SET   SGTST002-ALTERAR      TO  TRUE
           MOVE SGTS0088-CD-TIT-OPR-ETRD
                                       TO SGTST002-CD-TIT-OPR-ETRD
           MOVE SGTS0088-NR-DVS-TIT    TO SGTST002-NR-DVS-TIT
           MOVE SGTS0088-CD-TIT-OPR-ETRD
                                       TO SGTST002-NM-TIT-OPR-ETRD
           MOVE SGTS0088-CD-ISIN-TIT-OPR
                                       TO SGTST002-CD-ISIN-TIT-OPR
           MOVE SPACES                 TO SGTST002-CD-EPC-TIT-OPR
           MOVE 'CURRENT'              TO SGTST002-TS-ULT-ATL
           MOVE  KOPRD-CD-OPRD         TO SGTST002-CD-USU-RSP-ATL

           PERFORM 930000-CHAMA-SGTST002
           .
       224000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       225000-INCLUIR-TIT-OPR-ETRD      SECTION.
      *-----------------------------------------------------------------
           MOVE 0027                   TO  MSG1-CD-SEQL-ERRO

      *--> Movimentar os campos de entrada


           SET SGTST002-INCLUIR        TO TRUE
           MOVE SGTS0088-CD-TIT-OPR-ETRD
                                       TO SGTST002-CD-TIT-OPR-ETRD
           MOVE SGTS0088-NR-DVS-TIT
                                       TO SGTST002-NR-DVS-TIT
           MOVE SGTS0088-CD-TIT-OPR-ETRD
                                       TO SGTST002-NM-TIT-OPR-ETRD
           MOVE SGTS0088-CD-ISIN-TIT-OPR
                                       TO SGTST002-CD-ISIN-TIT-OPR
           MOVE SPACES                 TO SGTST002-CD-EPC-TIT-OPR
           MOVE 'CURRENT'              TO SGTST002-TS-ULT-ATL
           MOVE  KOPRD-CD-OPRD         TO SGTST002-CD-USU-RSP-ATL

           PERFORM 930000-CHAMA-SGTST002
           .
       225000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       226000-CONSULTAR-TIT-OPR-ETRD      SECTION.
      *-----------------------------------------------------------------
           MOVE 0028                   TO  MSG1-CD-SEQL-ERRO

           INITIALIZE         SGTST002-DADOS

      *--> Movimentar os campos de entrada

           SET SGTST002-CONSULTAR      TO TRUE
           MOVE SGTS0088-CD-TIT-OPR-ETRD
                                       TO SGTST002-CD-TIT-OPR-ETRD
           MOVE SGTS0088-NR-DVS-TIT    TO SGTST002-NR-DVS-TIT

           PERFORM 930000-CHAMA-SGTST002
           .
       226000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       230000-OBTER-UOR-BB-CRD-VL       SECTION.
      *--> Recupera UOR BB de crédito valor da operação
      *-----------------------------------------------------------------
           MOVE 0029                   TO MSG1-CD-SEQL-ERRO

      *--> Movimentar o nome do programa a ser executado
           INITIALIZE                 MSTS8041-DADOS.

           MOVE MSTS8041               TO NM-PGM

      *--> Movimentar os campos de entrada

           MOVE SGTS0088-CD-DEPE-CRD-VL-OPR
                                       TO MSTS8041-CD-DEPE-UOR-RQSC
           MOVE ZEROS                  TO MSTS8041-NR-ORD-SBDD-RQSC

           PERFORM 910000-CHAMAR-SERVICO
           .
       230000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       240000-INCLUIR-OPR-ETRD-MCP         SECTION.
      *-----------------------------------------------------------------
           MOVE 0030                   TO MSG1-CD-SEQL-ERRO

           INITIALIZE         SGTSX005-DADOS
           MOVE SGTSX005               TO NM-PGM

      *--> Movimentar os campos de entrada

           SET SGTSX005-INCLUIR        TO TRUE
           MOVE SGTS0088-CD-CTRA-TIT-VL-OPR  TO SGTSX005-CD-CTRA-TIT-VL

           MOVE 1                      TO SGTSX005-NR-VRS-OPR-ETRD

           MOVE SGTS0088-CD-PRD        TO SGTSX005-CD-PRD

           MOVE SGTS0088-CD-MDLD       TO SGTSX005-CD-MDLD

           MOVE SGTS0088-CD-UOR-DTTR-OPR
                                       TO SGTSX005-CD-UOR-DTTR-OPR

           IF  SGTS0088-CD-TIT-OPR-ETRD   NOT EQUAL SPACES
               MOVE SGTST002-CD-TIT-OPR-ETRD
                                       TO SGTSX005-CD-TIT-OPR-ETRD
               MOVE SGTST002-NR-DVS-TIT
                                       TO SGTSX005-NR-DVS-TIT
           END-IF
      *    Definido com gestor esta regra fixa.
           IF SGTSX005-CD-PRD = 427  AND
              (SGTSX005-CD-MDLD = 1 OR 2 OR 3 OR 4 OR 5 OR 6 OR 7 OR 8)
              MOVE     18912    TO   SGTSX005-CD-UOR-BB
           END-IF
      *
           IF SGTSX005-CD-PRD = 1057 AND SGTSX005-CD-MDLD = 3
              MOVE     18952    TO   SGTSX005-CD-UOR-BB
           END-IF
      *
           MOVE SGTS0088-CD-ITC-CVM    TO SGTSX005-CD-ITC-CVM-OPR

           MOVE SGTS0088-CD-CTGR-TVM   TO SGTSX005-CD-CTGR-TVM

           MOVE SGTS0088-CD-ESPC-DBTR-OPR
                                       TO SGTSX005-CD-ESPC-DBTR-OPR
           MOVE SGTS0088-CD-TIP-COTA-TIT
                                       TO SGTSX005-CD-TIP-COTA-TIT
           MOVE SGTS0088-NR-VRS-TIP-COTA
                                       TO SGTSX005-NR-VRS-TIP-COTA
           MOVE SGTS0088-CD-CMRA-OPR-ETRD
                                       TO SGTSX005-CD-CMRA-OPR-ETRD
           MOVE SGTS0088-NR-EMS-TIT-OPR
                                       TO SGTSX005-NR-EMS-TIT-OPR
           MOVE SGTS0088-NR-SRE-EMS-TIT-OPR
                                       TO SGTSX005-NR-SRE-EMS-TIT-OPR
           MOVE SGTS0088-CD-TIP-MC-CPR TO SGTSX005-CD-TIP-MC-CPR

           IF IIBKERRO-CD-ERRO EQUAL ZEROS
              MOVE MSTS8041-CD-UOR     TO SGTSX005-CD-UOR-BB-CRD-VL
           END-IF

           MOVE SGTS0088-CD-BCO-CRD-VL-OPR
                                       TO  SGTSX005-CD-BCO-CRD-VL-OPR
           MOVE SGTS0088-CD-DEPE-CRD-VL-OPR
                                       TO SGTSX005-CD-DEPE-CRD-VL-OPR

           IF SGTS0088-CD-BCO-CRD-VL-OPR  EQUAL 0001
              MOVE SGTS0088-NR-CC-CRD-VL-OPR
                                       TO SGTSX005-NR-CC-BB
           ELSE
              IF SGTS0088-CD-BCO-CRD-VL-OPR  > ZEROS
                 MOVE SGTS0088-NR-CC-CRD-VL-OPR
                                       TO  SGTSX005-NR-CC-OTR-BCO
              END-IF
           END-IF

           MOVE SPACES                 TO SGTSX005-DT-EMS-TIT-OPR
                                          SGTSX005-DT-CPR-OPR
                                          SGTSX005-DT-VNCT-OPR
                                          SGTSX005-DT-EVT-VCLD-VRS

           IF  SGTS0088-DT-EMS-TIT-OPR NOT EQUAL SPACES
               MOVE '  .  .    '       TO SGTSX005-DT-EMS-TIT-OPR
               MOVE CORR SGTS0088-DT-EMS-TIT-OPR-R
                                       TO SGTSX005-DT-EMS-TIT-OPR-R
           END-IF

           MOVE SGTS0088-QT-PAPL-TIT-EMS
                                       TO SGTSX005-QT-PAPL-TIT-EMS
           MOVE SGTS0088-VL-PU-EMS-TIT-OPR
                                       TO SGTSX005-VL-PU-EMS-TIT-OPR

           IF  SGTS0088-DT-CPR-OPR NOT EQUAL SPACES
               MOVE '  .  .    '       TO SGTSX005-DT-CPR-OPR
               MOVE CORR SGTS0088-DT-CPR-OPR-R
                                       TO SGTSX005-DT-CPR-OPR-R

               MOVE '  .  .    '       TO SGTSX005-DT-EVT-VCLD-VRS
               MOVE CORR SGTS0088-DT-CPR-OPR-R
                                       TO SGTSX005-DT-EVT-VCLD-VRS-R
           END-IF

           MOVE SGTS0088-QT-PAPL-TIT-CPR
                                       TO SGTSX005-QT-PAPL-TIT-CPR
           MOVE SGTS0088-VL-PU-CPR-TIT-OPR
                                       TO SGTSX005-VL-PU-CPR-TIT-OPR
           MOVE SGTS0088-VL-CPR-OPR    TO SGTSX005-VL-CPR-OPR

           IF  SGTS0088-DT-VNCT-OPR NOT EQUAL SPACES
               MOVE '  .  .    '       TO SGTSX005-DT-VNCT-OPR
               MOVE CORR SGTS0088-DT-VNCT-OPR-R
                                       TO SGTSX005-DT-VNCT-OPR-R
           END-IF

           MOVE SGTS0088-CD-RSCO-OPR   TO SGTSX005-CD-RSCO-OPR
           MOVE SGTS0088-VL-PRV-ENCT   TO SGTSX005-VL-PRV-ENCT
           MOVE SGTS0088-VL-APVD-BB-OPR
                                       TO SGTSX005-VL-APVD-BB-OPR

           MOVE SGTS0088-CD-TIP-VRS-OPR
                                       TO SGTSX005-CD-TIP-VRS-OPR

           MOVE KOPRD-CD-OPRD          TO SGTSX005-CD-USU-ULT-ATL
           MOVE 'CURRENT'              TO SGTSX005-TS-ULT-ATL

           COMPUTE SGTSX005-VL-EVT-VCLD-VRS =
                SGTS0088-VL-PU-EMS-TIT-OPR  *  SGTS0088-QT-PAPL-TIT-CPR
                ON SIZE ERROR

                  SET  ERRO-PROGRAMADO TO TRUE
                  MOVE 0253            TO MSG1-CD-ERRO-TS
                  MOVE 'Erro no cálculo do valor do evento.'
                                       TO MSG1-TX-ERRO-TS
                  SET MNT-VRV          TO TRUE
                  PERFORM 999000-ERRO
           END-COMPUTE

           MOVE SGTS0088-CD-CMCP       TO SGTSX005-CD-CMCP
           MOVE SGTS0088-CD-GR-MCP     TO SGTSX005-CD-GR-MCP
           MOVE SGTS0088-CD-LVRO-MC-CPTL
                                       TO SGTSX005-CD-LVRO-MC-CPTL
           MOVE SGTS0088-CD-SMCP       TO SGTSX005-CD-SMCP

      *CD-TIP-VRS-OPR = 1-Normalidade->CD-CLSC-ITMT-FNCO=1-Normal
      *CD-TIP-VRS-OPR = 2-Renegociação ->
      *   CD-CLSC-ITMT-FNCO=2-RC-Risco de crédito aumentado
      *CD-TIP-VRS-OPR = 5-Reestruturação->
      *   CD-CLSC-ITMT-FNCO=3- Problema de recuperação de crédito

           MOVE SGTS0088-CD-CLSC-ITMT-FNCO
                                       TO SGTSX005-CD-CLSC-ITMT-FNCO
      * CD-CLSC-ITMT-FNCO EQUAL = 1- Normal ou
      * CD-CLSC-ITMT-FNCO EQUAL = 2- RC- Risco de crédito aumentado->
      *   Estágio Negocial = 1-Normalidade

           IF SGTS0088-CD-CLSC-ITMT-FNCO EQUAL 1 OR
              SGTS0088-CD-CLSC-ITMT-FNCO EQUAL 2
              MOVE 1                    TO SGTSX005-CD-ESG-NEGL
           END-IF

      * CD-CLSC-ITMT-FNCO EQUAL=3- Problema de recuperação de crédito->
      *   Estágio Negocial = 3-Ativo Problemático(stop accrual)

           IF SGTS0088-CD-CLSC-ITMT-FNCO EQUAL 3
                 MOVE 3                 TO SGTSX005-CD-ESG-NEGL
           END-IF

      *Inclusão do Motivo Estado Negocial
           IF  (SGTS0088-CD-TIP-VRS-OPR  EQUAL 1 OR
                SGTS0088-CD-TIP-VRS-OPR  EQUAL 2 OR
                SGTS0088-CD-TIP-VRS-OPR  EQUAL 5) AND
                (SGTS0088-CD-MTV-EST-NEGL > ZEROS)
                MOVE SGTS0088-CD-MTV-EST-NEGL
                                        TO SGTSX005-CD-MTV-EST-NEGL
           END-IF


           PERFORM 940000-CHAMA-SGTSX005
           .
       240000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       250000-INCLUIR-EST-OPR-ETRD   SECTION.
      *-----------------------------------------------------------------
           MOVE 0031                   TO MSG1-CD-SEQL-ERRO

      *--> Movimentar o nome do programa a ser executado
           INITIALIZE                 SGTST009-DADOS.

           MOVE SGTST009               TO NM-PGM

      *--> Movimentar os campos de entrada

           SET SGTST009-INCLUIR
                                       TO TRUE
           MOVE SGTS0088-CD-CTRA-TIT-VL-OPR TO SGTST009-CD-CTRA-TIT-VL
           MOVE SGTSX005-CD-OPR-ETRD-MCP
                                       TO SGTST009-CD-OPR-ETRD-MCP
           MOVE 1                      TO SGTST009-NR-VRS-OPR-ETRD
           MOVE 'CURRENT'              TO SGTST009-TS-EST-OPR-ETRD
      *--> Tipo de estado operação = 1 - Em negociação
           MOVE 1                      TO SGTST009-CD-TIP-EST-OPR
           MOVE KOPRD-CD-OPRD          TO SGTST009-CD-USU-RSP-EST
                                          SGTST009-CD-USU-RSP-ATL

           PERFORM 950000-CHAMAR-SGTST009
           .
       250000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       260000-OBTER-COP-DADO-VCLD-OPR  SECTION.
      *-----------------------------------------------------------------
           MOVE 0032                   TO MSG1-CD-SEQL-ERRO

      *--> Movimentar o nome do programa a ser executado
           INITIALIZE                  SGTS0072-RQSC.

           MOVE SGTS0072               TO NM-PGM

      *--> Movimentar os campos de entrada
           MOVE SGTS0088-CD-CTRA-TIT-VL-COPD
                                       TO SGTS0072-CD-CTRA-OGM
           MOVE SGTS0088-CD-OPR-ETRD-COPD
                                       TO SGTS0072-CD-OPR-ETRD-OGM
           MOVE SGTS0088-NR-VRS-OPR-ETRD-COPD
                                       TO SGTS0072-NR-VRS-OPR-OGM
           MOVE SGTS0088-CD-CTRA-TIT-VL-OPR
                                       TO SGTS0072-CD-CTRA-DST
           MOVE SGTSX005-CD-OPR-ETRD-MCP
                                       TO SGTS0072-CD-OPR-ETRD-DST
           MOVE 1                      TO SGTS0072-NR-VRS-OPR-DST

           PERFORM 900000-EXECUTAR-MODULO
           .
       260000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       270000-TRATAR-CLIENTES   SECTION.
      *-----------------------------------------------------------------

           PERFORM 271000-INCLUIR-CLIENTE-EMISSOR

           IF SGTS0088-CD-CLI-IFLC-LMCR NOT EQUAL ZEROS
              PERFORM 272000-INCLUIR-CLI-IFLC-LIM
           END-IF

           IF SGTS0088-CD-CLI-CDT NOT EQUAL ZEROS
              PERFORM 273000-INCLUIR-CLI-OGM-CDT
           END-IF

           IF SGTS0088-CD-CLI-AGT-FDAR NOT EQUAL ZEROS
              PERFORM 274000-INCLUIR-CLI-AGT-FDAR
           END-IF

           IF SGTS0088-CD-CLI-ADM NOT EQUAL ZEROS
              PERFORM 275000-INCLUIR-CLIENTE-ADM
           END-IF
           .
       270000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       271000-INCLUIR-CLIENTE-EMISSOR   SECTION.
      *-----------------------------------------------------------------
           MOVE 0033                   TO MSG1-CD-SEQL-ERRO

      *--> Movimentar o nome do programa a ser executado
           INITIALIZE                 SGTST006-DADOS.

           MOVE SGTST006               TO NM-PGM

      *--> Movimentar os campos de entrada

           SET SGTST006-INCLUIR        TO TRUE
           MOVE SGTS0088-CD-CTRA-TIT-VL-OPR  TO SGTST006-CD-CTRA-TIT-VL
           MOVE SGTSX005-CD-OPR-ETRD-MCP
                                       TO SGTST006-CD-OPR-ETRD-MCP
           MOVE 1                      TO SGTST006-NR-VRS-OPR-ETRD
           MOVE SGTS0088-CD-CLI-EMT    TO SGTST006-CD-CLI
      *--> Tipo de funçaõ do cliente = 1 - Cliente Emissor
           MOVE 1                      TO SGTST006-CD-TIP-FUC-CLI
           MOVE 'CURRENT'              TO SGTST006-TS-ULT-ATL
           MOVE KOPRD-CD-OPRD          TO SGTST006-CD-USU-RSP-ATL
           PERFORM 960000-CHAMAR-SGTST006
           .
       271000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       272000-INCLUIR-CLI-IFLC-LIM   SECTION.
      * Incluir tipo de função do cliente - impacto limite
      *-----------------------------------------------------------------
           MOVE 0034                   TO MSG1-CD-SEQL-ERRO

      *--> Movimentar o nome do programa a ser executado
           INITIALIZE                 SGTST006-DADOS.

           MOVE SGTST006               TO NM-PGM

      *--> Movimentar os campos de entrada

           SET SGTST006-INCLUIR        TO TRUE
           MOVE SGTS0088-CD-CTRA-TIT-VL-OPR  TO SGTST006-CD-CTRA-TIT-VL
           MOVE SGTSX005-CD-OPR-ETRD-MCP
                                       TO SGTST006-CD-OPR-ETRD-MCP
           MOVE 1                      TO SGTST006-NR-VRS-OPR-ETRD
           MOVE SGTS0088-CD-CLI-IFLC-LMCR
                                       TO SGTST006-CD-CLI
      * Tipo de funçaõ do cliente = 2 - Impacto Limite
           MOVE 2                      TO SGTST006-CD-TIP-FUC-CLI
           MOVE 'CURRENT'              TO SGTST006-TS-ULT-ATL
           MOVE KOPRD-CD-OPRD          TO SGTST006-CD-USU-RSP-ATL

           PERFORM 960000-CHAMAR-SGTST006
           .
       272000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       273000-INCLUIR-CLI-OGM-CDT   SECTION.
      *--> Tipo de função do cliente - origem cedente
      *-----------------------------------------------------------------
           MOVE 0035                   TO MSG1-CD-SEQL-ERRO

      *--> Movimentar o nome do programa a ser executado
           INITIALIZE                 SGTST006-DADOS.

           MOVE SGTST006               TO NM-PGM

      *--> Movimentar os campos de entrada

           SET SGTST006-INCLUIR        TO TRUE
           MOVE SGTS0088-CD-CTRA-TIT-VL-OPR TO SGTST006-CD-CTRA-TIT-VL
           MOVE SGTSX005-CD-OPR-ETRD-MCP
                                       TO SGTST006-CD-OPR-ETRD-MCP
           MOVE 1                      TO SGTST006-NR-VRS-OPR-ETRD
           MOVE SGTS0088-CD-CLI-CDT    TO SGTST006-CD-CLI
      *--> Tipo de funçaõ do cliente = 3 - Originador/Cedente
           MOVE 3                      TO SGTST006-CD-TIP-FUC-CLI
           MOVE 'CURRENT'              TO SGTST006-TS-ULT-ATL
           MOVE KOPRD-CD-OPRD          TO SGTST006-CD-USU-RSP-ATL

           PERFORM 960000-CHAMAR-SGTST006
           .
       273000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       274000-INCLUIR-CLI-AGT-FDAR   SECTION.
      *--> Tipo de função do cliente - Agente Fiduciário
      *-----------------------------------------------------------------
           MOVE 0036                   TO MSG1-CD-SEQL-ERRO

      *--> Movimentar o nome do programa a ser executado
           INITIALIZE                 SGTST006-DADOS.

           MOVE SGTST006               TO NM-PGM

      *--> Movimentar os campos de entrada

           SET SGTST006-INCLUIR        TO TRUE
           MOVE SGTS0088-CD-CTRA-TIT-VL-OPR TO SGTST006-CD-CTRA-TIT-VL
           MOVE SGTSX005-CD-OPR-ETRD-MCP
                                       TO SGTST006-CD-OPR-ETRD-MCP
           MOVE 1                      TO SGTST006-NR-VRS-OPR-ETRD
           MOVE SGTS0088-CD-CLI-AGT-FDAR
                                       TO SGTST006-CD-CLI
      *--> Tipo de funçaõ do cliente = 4 - Agente Fiduciário/Notas
           MOVE 4                      TO SGTST006-CD-TIP-FUC-CLI
           MOVE 'CURRENT'              TO SGTST006-TS-ULT-ATL
           MOVE KOPRD-CD-OPRD          TO SGTST006-CD-USU-RSP-ATL

           PERFORM 960000-CHAMAR-SGTST006
           .
       274000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       275000-INCLUIR-CLIENTE-ADM   SECTION.
      *--> Tipo de função do cliente - administrador
      *-----------------------------------------------------------------
           MOVE 0037                   TO MSG1-CD-SEQL-ERRO

      *--> Movimentar o nome do programa a ser executado
           INITIALIZE                 SGTST006-DADOS.

           MOVE SGTST006               TO NM-PGM

      *--> Movimentar os campos de entrada

           SET SGTST006-INCLUIR        TO TRUE
           MOVE SGTS0088-CD-CTRA-TIT-VL-OPR TO SGTST006-CD-CTRA-TIT-VL
           MOVE SGTSX005-CD-OPR-ETRD-MCP
                                       TO SGTST006-CD-OPR-ETRD-MCP
           MOVE 1                      TO SGTST006-NR-VRS-OPR-ETRD
           MOVE SGTS0088-CD-CLI-ADM    TO SGTST006-CD-CLI
      *--> Tipo de funçaõ do cliente = 5 - Administrador
           MOVE 5                      TO SGTST006-CD-TIP-FUC-CLI
           MOVE 'CURRENT'              TO SGTST006-TS-ULT-ATL
           MOVE KOPRD-CD-OPRD          TO SGTST006-CD-USU-RSP-ATL

           PERFORM 960000-CHAMAR-SGTST006
           .
       275000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       280000-TRATAR-INDICADORES   SECTION.
      *-----------------------------------------------------------------
           MOVE 0038                   TO MSG1-CD-SEQL-ERRO

           IF SGTS0088-IN-CNG-PBCO-OPR EQUAL 'S'
              PERFORM 281000-INCLUIR-IN-CNG-PBCO-OPR
           END-IF

      * O indicador de flexibilização de exeção é tratado no momento
      * de aprovação da operação pelo gestor (transferencia do estado
      * da operação de 2 para 3)
      *     IF SGTS0088-IN-FLZ-EXCO-OPR EQUAL 'S'
      *         PERFORM 282000-INCLUIR-IN-FLZ-EXCO-OPR
      *     END-IF

           IF SGTS0088-IN-OPR-ETRD-IETR EQUAL 'S'
               PERFORM 283000-INCLUIR-IN-OPR-IETR
           END-IF

           IF SGTS0088-IN-N-CNV-OPR-ACAO EQUAL 'S'
               PERFORM 284000-INCLUIR-IN-N-CNV-ACAO
           END-IF

           IF SGTS0088-IN-OPR-SPT-FCTR EQUAL 'S'
               PERFORM 285000-INCLUIR-IN-OPR-SPT-FCTR
           END-IF
           .
       280000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       281000-INCLUIR-IN-CNG-PBCO-OPR    SECTION.
      *-----------------------------------------------------------------
           MOVE 0039                   TO MSG1-CD-SEQL-ERRO

      *--> Movimentar o nome do programa a ser executado
           INITIALIZE                 SGTSI063-DADOS.

           MOVE SGTSI063               TO NM-PGM

      *--> Movimentar os campos de entrada

           MOVE SGTS0088-CD-CTRA-TIT-VL-OPR  TO SGTSI063-CD-CTRA-TIT-VL
           MOVE SGTSX005-CD-OPR-ETRD-MCP
                                       TO SGTSI063-CD-OPR-ETRD-MCP
           MOVE 1                      TO SGTSI063-NR-VRS-OPR-ETRD
      * Código do Tipo do Indicador = 1-Indicador de Contingenciamento
      * Publico da Operação
           MOVE 1                      TO SGTSI063-CD-TIP-IN

           PERFORM 900000-EXECUTAR-MODULO
           .
       281000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       283000-INCLUIR-IN-OPR-IETR    SECTION.
      *-----------------------------------------------------------------
           MOVE 0040                   TO MSG1-CD-SEQL-ERRO

      *--> Movimentar o nome do programa a ser executado
           INITIALIZE                 SGTSI063-DADOS.

           MOVE SGTSI063               TO NM-PGM

      *--> Movimentar os campos de entrada

           MOVE SGTS0088-CD-CTRA-TIT-VL-OPR  TO SGTSI063-CD-CTRA-TIT-VL
           MOVE SGTSX005-CD-OPR-ETRD-MCP
                                       TO SGTSI063-CD-OPR-ETRD-MCP
           MOVE 1                      TO SGTSI063-NR-VRS-OPR-ETRD
      * Código do Tipo do Indicador = 3- Indicador de Operação Estrutura
      *    da de Infraestrutura
           MOVE 3                      TO SGTSI063-CD-TIP-IN

           PERFORM 900000-EXECUTAR-MODULO
           .
       283000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       284000-INCLUIR-IN-N-CNV-ACAO    SECTION.
      *--> Indicador de Não Conversão da Operação em Ação
      *-----------------------------------------------------------------
           MOVE 0041                   TO MSG1-CD-SEQL-ERRO

      *--> Movimentar o nome do programa a ser executado
           INITIALIZE                 SGTSI063-DADOS.

           MOVE SGTSI063               TO NM-PGM

      *--> Movimentar os campos de entrada

           MOVE SGTS0088-CD-CTRA-TIT-VL-OPR TO SGTSI063-CD-CTRA-TIT-VL
           MOVE SGTSX005-CD-OPR-ETRD-MCP
                                       TO SGTSI063-CD-OPR-ETRD-MCP
           MOVE 1                      TO SGTSI063-NR-VRS-OPR-ETRD
      * Código do Tipo do Indicador = 4- Indicador de Não Conversão da O
      *    peração em Ação
           MOVE 4                      TO SGTSI063-CD-TIP-IN

           PERFORM 900000-EXECUTAR-MODULO
           .
       284000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       285000-INCLUIR-IN-OPR-SPT-FCTR    SECTION.
      *-----------------------------------------------------------------
           MOVE 0042                   TO MSG1-CD-SEQL-ERRO

      *--> Movimentar o nome do programa a ser executado
           INITIALIZE                 SGTSI063-DADOS.

           MOVE SGTSI063               TO NM-PGM

      *--> Movimentar os campos de entrada

           MOVE SGTS0088-CD-CTRA-TIT-VL-OPR TO SGTSI063-CD-CTRA-TIT-VL
           MOVE SGTSX005-CD-OPR-ETRD-MCP
                                       TO SGTSI063-CD-OPR-ETRD-MCP
           MOVE 1                      TO SGTSI063-NR-VRS-OPR-ETRD
      * Código do Tipo do Indicador = 5- Indicador de Operação Estrutura
      *    da com Suporte em Facilitador.
           MOVE 5                      TO SGTSI063-CD-TIP-IN

           PERFORM 900000-EXECUTAR-MODULO
           .
       285000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       290000-TRATAR-BCO-PCT    SECTION.
      *--> Tratamento de Bancos Participantes de Mercado Secundário
      *-----------------------------------------------------------------
           MOVE 0043                   TO MSG1-CD-SEQL-ERRO

      *--> Movimentar o nome do programa a ser executado
           INITIALIZE                 SGTSI064-DADOS.

           MOVE SGTSI064               TO NM-PGM

      *--> Movimentar os campos de entrada

           MOVE SGTS0088-CD-CTRA-TIT-VL-OPR  TO SGTSI064-CD-CTRA-TIT-VL
           MOVE SGTSX005-CD-OPR-ETRD-MCP
                                       TO SGTSI064-CD-OPR-ETRD-MCP
           MOVE 1                      TO SGTSI064-NR-VRS-OPR-ETRD
           MOVE SGTS0088-CD-BCO-PCT(IC)TO SGTSI064-CD-BCO-PCT
      * Tipo de Participação = 2 - Banco participante do Mercado Secundá
      *    rio
           MOVE 2                      TO SGTSI064-CD-TIP-PRTC
           MOVE 'N'                    TO SGTSI064-IN-BCO-LDR
           MOVE ZEROS                  TO SGTSI064-PC-PRTC-BCO
           MOVE KOPRD-CD-OPRD          TO SGTSI064-CD-USU-ULT-ATL
           MOVE 'CURRENT'              TO SGTSI064-TS-ULT-ATL
           PERFORM 900000-EXECUTAR-MODULO
           .
       290000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       300000-MOVIMENTAR-SAIDA         SECTION.
      *-----------------------------------------------------------------
           MOVE 0044                   TO MSG1-CD-SEQL-ERRO

           MOVE SGTSX005-CD-OPR-ETRD-MCP
                                       TO SGTS0088-CD-OPR-ETRD-CRID
           MOVE  SGTSX005-NR-VRS-OPR-ETRD
                                       TO SGTS0088-NR-VRS-OPR-ETRD-CRID

      *--> Gravando o container de resposta da subrotina

           PERFORM 980000-GRAVAR-CONTAINER-RPST
           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       310000-INCLUIR-EST-VRS    SECTION.
      *--> Incluir estado da versão
      *-----------------------------------------------------------------
           MOVE 0045                   TO MSG1-CD-SEQL-ERRO

      *--> Movimentar o nome do programa a ser executado
           INITIALIZE                 SGTSI069-DADOS.

           MOVE SGTSI069               TO NM-PGM

      *--> Movimentar os campos de entrada

           MOVE SGTS0088-CD-CTRA-TIT-VL-OPR TO SGTSI069-CD-CTRA-TIT-VL
           MOVE SGTSX005-CD-OPR-ETRD-MCP
                                       TO SGTSI069-CD-OPR-ETRD-MCP
           MOVE 1                      TO SGTSI069-NR-VRS-OPR-ETRD

           MOVE 1
                                       TO SGTSI069-CD-TIP-EST-VRS

      * quando  CD-TIP-VRS-OPR = 2(Renegociação) ou 5(Reestruturação)
      * é obrigatório informar o TX-OBS-VRS

           IF (SGTS0088-CD-TIP-VRS-OPR EQUAL 2 OR
               SGTS0088-CD-TIP-VRS-OPR EQUAL 5)
              IF SGTS0088-TX-OBS-VRS EQUAL ' '
                   SET ERRO-PROGRAMADO     TO TRUE
                   MOVE 0623               TO MSG1-CD-ERRO-TS
                   MOVE 'Texto de observação do versionamento não inform
      -                 'ado.'
                                           TO MSG1-TX-ERRO-TS
                   SET MNT-VRV             TO TRUE
                   PERFORM 999000-ERRO
              ELSE
                MOVE SGTS0088-TX-OBS-VRS    TO SGTSI069-TX-OBS-EST-VRS
              END-IF
           END-IF

           PERFORM 900000-EXECUTAR-MODULO
           .
       310000-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *                                                                *
      *  SECTION DA FAIXA DE 900000 A 969900 PARA DEMAIS ROTINAS       *
      *                                                                *
      *-----------------------------------------------------------------
      * Executa um modulo implementado com CONTAINER                   *
      * Nao incluir nessa section os modulos chamados via COMMAREA     *
      *                                                                *
      *-----------------------------------------------------------------
       900000-EXECUTAR-MODULO          SECTION.
      *-----------------------------------------------------------------

      *--> Grava o container de requisicao do modulo a executar
           PERFORM 982000-GRAVAR-CONTAINER-RQSC

      *--> Limpa a resposta e erro antes de executar o modulo
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           PERFORM 901000-IDENTIFICA-PROGRAMA

      *--> Efetua a leitura do Container de Erro
           PERFORM 972000-LER-CONTAINER-DE-ERRO

      *--> Efetua a leitura do Container de Resposta
             PERFORM 973000-LER-RESPOSTA
           .
       900000-SAI.
           EXIT.

      *----------------------------------------------------------------
       901000-IDENTIFICA-PROGRAMA           SECTION.
      *----------------------------------------------------------------
           MOVE 0046                   TO MSG1-CD-SEQL-ERRO

           EVALUATE NM-PGM
               WHEN SGTS211A
                   EXEC CICS LINK
                             PROGRAM ( SGTS211A   )
                             CHANNEL ( NM-CNL     )
                             RESP    ( MSG1-RESP1 )
                             RESP2   ( MSG1-RESP2 )
                   END-EXEC

               WHEN SGTS0072
                   EXEC CICS LINK
                             PROGRAM ( SGTS0072  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC
               WHEN SGTS0148
                   EXEC CICS LINK
                             PROGRAM ( SGTS0148  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC
               WHEN SGTSI063
                   EXEC CICS LINK
                             PROGRAM ( SGTSI063  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC
               WHEN SGTSI064
                   EXEC CICS LINK
                             PROGRAM ( SGTSI064  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC
               WHEN SGTSI069
                   EXEC CICS LINK
                             PROGRAM ( SGTSI069  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC
               WHEN SGTS0211
                   EXEC CICS LINK
                             PROGRAM ( SGTS0211  )
                             CHANNEL ( NM-CNL    )
                             RESP    ( MSG1-RESP1)
                             RESP2   ( MSG1-RESP2)
                   END-EXEC

               WHEN OTHER
                   SET   ERRO-PROGRAMADO     TO TRUE
                   MOVE  9006          TO  MSG1-CD-ERRO-TS
                   STRING
                   'Não definida a execução do módulo ' NM-PGM
                   DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
                   PERFORM 999000-ERRO
           END-EVALUATE
      *
           EVALUATE TRUE
               WHEN MSG1-RESP1        EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    SET ERRO-CICS      TO TRUE
                    MOVE 3004          TO MSG1-CD-ERRO-TS
                    STRING
                    'Erro ao acionar o módulo ' NM-PGM ', '
                    'RESP  ='   ' ' MSG1-RESP1  ', '
                    'RESP2 =' ' ' MSG1-RESP2
                    DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS

      *  Programa usa dados da tabela
                    IF  NM-PGM  EQUAL  SGTSI064
                        SET COM-ERRO-TAB  TO  TRUE
                    END-IF
                    PERFORM 999000-ERRO
           END-EVALUATE
           .
       901000-SAI.
           EXIT.

      *----------------------------------------------------------------
       910000-CHAMAR-SERVICO           SECTION.
      *----------------------------------------------------------------
      *--> Acionar programa IIBP2000 para requisição do serviço.
      *----------------------------------------------------------------


      *--> Grava o container com dados do serviço/operação a chamar
           PERFORM 981000-GRAVAR-CONTAINER-SRVC

      *--> Grava o container de requisicao do modulo a executar
           PERFORM 982000-GRAVAR-CONTAINER-RQSC

      *--> Limpa a resposta e erro antes de executar o modulo
           PERFORM 983000-EXCLUIR-RESPOSTA-ERRO

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           PERFORM 911000-EXECUTA-IIBP200

      *--> Efetua a leitura do Container de Erro
           PERFORM 972000-LER-CONTAINER-DE-ERRO

           IF NOT TEM-CONTAINER-ERRO
      *--> Efetua a leitura do Container de Resposta
              PERFORM 973000-LER-RESPOSTA
           END-IF
           MOVE SPACES                 TO IN-CONTAINER-ERRO
           .
       910000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       911000-EXECUTA-IIBP200                 SECTION.
      *-----------------------------------------------------------------
           MOVE 0047                   TO MSG1-CD-SEQL-ERRO

           EXEC CICS LINK
                     PROGRAM   ( IIBP2000     )
                     CHANNEL   ( NM-CNL       )
                     RESP      ( MSG1-RESP1   )
                     RESP2     ( MSG1-RESP2   )
           END-EXEC

           EVALUATE TRUE
               WHEN MSG1-RESP1        EQUAL DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    SET ERRO-CICS      TO TRUE
                    MOVE 9000          TO MSG1-CD-ERRO-TS
                    STRING 'Erro ao acionar serviço: '
                           SRVC-ID-SRVC '.' SRVC-VRS-SRVC '.'
                           SRVC-ID-OPR  '.' SRVC-VRS-OPR
                           'via IIBP2000, '
                           'RESP=' MSG1-RESP1 ', RESP-2=' MSG1-RESP2
                           '.'         DELIMITED BY SIZE
                                       INTO MSG1-TX-ERRO-TS
      *  Programa usa dados da tabela
                    IF  NM-PGM  EQUAL  CAFS0053
                        SET COM-ERRO-TAB  TO  TRUE
                    END-IF
                    PERFORM 999000-ERRO
           END-EVALUATE
           .
       911000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       920000-VALIDAR-DATAS                 SECTION.
      *-----------------------------------------------------------------
           MOVE 0048                   TO MSG1-CD-SEQL-ERRO

           MOVE 4095 TO AREA-TAM.
           MOVE 'SBDATAF -WRKAREA-FIM' TO AREA-FIM.

           CALL SBDATAF     USING FUNCAO WKSBDATA  ARG01 ARG02.

      *--> Return code = 24: Data Inválida.

           IF  RETURN-CODE             NOT EQUAL ZEROS AND 24 AND 22
                                                       AND 46
               SET ERRO-PROGRAMA-ACIONADO TO TRUE
               MOVE 0067               TO MSG1-CD-ERRO-TS

               MOVE 'Erro na rotina que valida data.'
                                       TO MSG1-TX-ERRO-TS
                                          MSG1-TX-RTN-PGM-ACND

               MOVE SBDATAF            TO MSG1-NM-PGM-ACND
                                          NM-PGM
               MOVE RETURN-CODE        TO MSG1-CD-RTN-PGM-ACND

      *        Monitorar variáveis do módulo chamado
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-ERRO

           END-IF

           IF  RETURN-CODE             EQUAL 24
               SET ERRO-PROGRAMA-ACIONADO     TO TRUE
               MOVE 0049               TO MSG1-CD-ERRO-TS
               MOVE 'Data informada inválida'
                                       TO MSG1-TX-ERRO-TS
                                          MSG1-TX-RTN-PGM-ACND
               MOVE SBDATAF            TO MSG1-NM-PGM-ACND
                                          NM-PGM
               MOVE RETURN-CODE        TO MSG1-CD-RTN-PGM-ACND

      *        Monitorar variáveis do módulo chamado
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-ERRO
           END-IF
           .
       920000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       930000-CHAMA-SGTST002           SECTION.
      *-----------------------------------------------------------------
           MOVE 0049                   TO MSG1-CD-SEQL-ERRO

      *--> Movimento nome do módulo para monitoração de variáveis
           MOVE SGTST002               TO NM-PGM
           EXEC CICS LINK
                PROGRAM  (SGTST002)
                COMMAREA (SGTST002-DADOS)
           END-EXEC


           IF  SGTST002-CD-RTN-PGM     NOT EQUAL ZEROS AND 100
               SET ERRO-PROGRAMA-ACIONADO TO TRUE
               MOVE 3004               TO MSG1-CD-ERRO-TS
               STRING  'Erro na execução do módulo ' NM-PGM '. '
                       DELIMITED BY SIZE
                       INTO                MSG1-TX-ERRO-TS
               MOVE  NM-PGM            TO  MSG1-NM-PGM-ACND
               MOVE SGTST002-MSG-ERRO-PGM
                                       TO MSG1-TX-RTN-PGM-ACND
               MOVE SGTST002-CD-RTN-PGM
                                       TO MSG1-CD-RTN-PGM-ACND

      *        Monitorar variáveis do módulo chamado
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-ERRO
           END-IF
           .
       930000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       940000-CHAMA-SGTSX005           SECTION.
      *-----------------------------------------------------------------
           MOVE 0050                   TO MSG1-CD-SEQL-ERRO

      *--> Movimento nome do módulo para monitoração de variáveis
           MOVE SGTSX005               TO NM-PGM
           EXEC CICS LINK
                PROGRAM  (SGTSX005)
                COMMAREA (SGTSX005-DADOS)
           END-EXEC

           IF  SGTSX005-CD-RTN-PGM     NOT EQUAL ZEROS AND 100
               SET ERRO-PROGRAMA-ACIONADO  TO TRUE
               MOVE NM-PGM                 TO MSG1-NM-PGM-ACND
               MOVE SGTSX005-MSG-ERRO-PGM  TO MSG1-TX-RTN-PGM-ACND
                                              MSG1-TX-ERRO-TS
               MOVE SGTSX005-CD-RTN-PGM    TO MSG1-CD-RTN-PGM-ACND
                                              MSG1-CD-ERRO-TS

      *        Monitorar variáveis do módulo chamado
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-ERRO
           END-IF
           .
       940000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       950000-CHAMAR-SGTST009         SECTION.
      *-----------------------------------------------------------------
           MOVE 0051                   TO MSG1-CD-SEQL-ERRO

      *--> Movimenta nome do módulo para monitoração de variáveis
           MOVE SGTST009               TO NM-PGM
           EXEC CICS LINK
                PROGRAM  (SGTST009)
                COMMAREA (SGTST009-DADOS)
           END-EXEC

           IF  SGTST009-CD-RTN-PGM     NOT EQUAL ZEROS
               SET ERRO-PROGRAMA-ACIONADO TO TRUE
               MOVE NM-PGM                TO MSG1-NM-PGM-ACND
               MOVE SGTST009-MSG-ERRO-PGM TO MSG1-TX-RTN-PGM-ACND
                                             MSG1-TX-ERRO-TS
               MOVE SGTST009-CD-RTN-PGM   TO MSG1-CD-RTN-PGM-ACND
                                             MSG1-CD-ERRO-TS
      *        Monitorar variáveis do módulo chamado
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-ERRO
           END-IF
           .
       950000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       960000-CHAMAR-SGTST006         SECTION.
      *-----------------------------------------------------------------
           MOVE 0052                   TO MSG1-CD-SEQL-ERRO

      *--> Movimenta nome do módulo para monitoração de variáveis
           MOVE SGTST006               TO NM-PGM
           EXEC CICS LINK
                PROGRAM  (SGTST006)
                COMMAREA (SGTST006-DADOS)
           END-EXEC

           IF  SGTST006-CD-RTN-PGM     NOT EQUAL ZEROS
               SET ERRO-PROGRAMA-ACIONADO  TO TRUE
               MOVE NM-PGM                 TO MSG1-NM-PGM-ACND
               MOVE SGTST006-MSG-ERRO-PGM  TO MSG1-TX-RTN-PGM-ACND
                                              MSG1-TX-ERRO-TS
               MOVE SGTST006-CD-RTN-PGM    TO MSG1-CD-RTN-PGM-ACND
                                              MSG1-CD-ERRO-TS
      *        Monitorar variáveis do módulo chamado
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-ERRO
           END-IF
           .
       960000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       961000-CHAMA-ARCSB318           SECTION.
      *-----------------------------------------------------------------
           MOVE 0053                   TO MSG1-CD-SEQL-ERRO

      *--> Movimenta nome do módulo para monitoração de variáveis
           MOVE ARCSB318               TO NM-PGM
           EXEC CICS LINK
                PROGRAM  (ARCSB318)
                COMMAREA (ARC-DADOS)
           END-EXEC

           IF  318L-CD-MSG             NOT EQUAL ZEROS
               SET ERRO-PROGRAMA-ACIONADO TO TRUE
               MOVE 3004               TO MSG1-CD-ERRO-TS
               STRING  'Erro na execução do módulo ' NM-PGM '. '
                       DELIMITED BY SIZE
                       INTO                MSG1-TX-ERRO-TS
               MOVE NM-PGM             TO MSG1-NM-PGM-ACND
               MOVE 318L-TX-MSG
                                       TO MSG1-TX-RTN-PGM-ACND
               MOVE 318L-CD-MSG
                                       TO MSG1-CD-RTN-PGM-ACND
      *        Monitorar variáveis do módulo chamado
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-ERRO
           END-IF
           .
       960100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       962000-CHAMA-SGTST027         SECTION.
      *--> TABELA TIP_COTA_TIT_OPR
      *-----------------------------------------------------------------
           MOVE 0054                   TO MSG1-CD-SEQL-ERRO

      *--> Movimenta nome do módulo para monitoração de variáveis
           MOVE SGTST027               TO NM-PGM
           EXEC CICS LINK
                PROGRAM  (SGTST027)
                COMMAREA (SGTST027-DADOS)
           END-EXEC

           IF  SGTST027-CD-RTN-PGM     NOT EQUAL ZEROS
               SET ERRO-PROGRAMA-ACIONADO  TO TRUE
               MOVE NM-PGM                 TO MSG1-NM-PGM-ACND
               MOVE SGTST027-MSG-ERRO-PGM  TO MSG1-TX-RTN-PGM-ACND
                                              MSG1-TX-ERRO-TS
               MOVE SGTST027-CD-RTN-PGM    TO MSG1-CD-RTN-PGM-ACND
                                              MSG1-CD-ERRO-TS
      *        Monitorar variáveis do módulo chamado
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-ERRO
           END-IF
           .
       962000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       963000-CHAMA-SGTST029         SECTION.
      *--> TABELA CTGR_BC_TVM
      *-----------------------------------------------------------------
           MOVE 0055                   TO MSG1-CD-SEQL-ERRO

      *--> Movimenta nome do módulo para monitoração de variáveis
           MOVE SGTST029               TO NM-PGM
           EXEC CICS LINK
                PROGRAM  (SGTST029)
                COMMAREA (SGTST029-DADOS)
           END-EXEC

           IF  SGTST029-CD-RTN-PGM     NOT EQUAL ZEROS
               SET ERRO-PROGRAMA-ACIONADO TO TRUE
               MOVE NM-PGM                TO MSG1-NM-PGM-ACND
               MOVE SGTST029-MSG-ERRO-PGM TO MSG1-TX-RTN-PGM-ACND
                                             MSG1-TX-ERRO-TS
               MOVE SGTST029-CD-RTN-PGM   TO MSG1-CD-RTN-PGM-ACND
                                             MSG1-CD-ERRO-TS
      *        Monitorar variáveis do módulo chamado
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-ERRO
           END-IF
           .
       963000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       964000-CHAMA-SGTST030         SECTION.
      *--> TABELA ITC_CVM_OPR_ETRD
      *-----------------------------------------------------------------
           MOVE 0056                   TO  MSG1-CD-SEQL-ERRO

      *--> Movimenta nome do módulo para monitoração de variáveis
           MOVE  SGTST030              TO  NM-PGM

           EXEC CICS LINK
                     PROGRAM  ( SGTST030       )
                     COMMAREA ( SGTST030-DADOS )
           END-EXEC


           EVALUATE   TRUE
               WHEN   SGTST030-CD-RTN-PGM = ZEROS
                      CONTINUE
               WHEN   SGTST030-ERRO-DE-DB2

                     SET ERRO-PROGRAMA-ACIONADO TO TRUE
                     MOVE 9007               TO MSG1-CD-ERRO-TS
                     STRING  'Erro de execução de  comando SQL ' NM-PGM
                         '. '   DELIMITED BY SIZE
                          INTO                MSG1-TX-ERRO-TS

                      MOVE NM-PGM             TO MSG1-NM-PGM-ACND
                      MOVE SGTST030-MSG-ERRO-PGM
                                       TO MSG1-TX-RTN-PGM-ACND
                      MOVE SGTST030-CD-RTN-PGM
                                       TO MSG1-CD-RTN-PGM-ACND

      *               Monitorar variáveis do módulo chamado
                      PERFORM 969000-MONITORAR-VARIAVEIS
                      PERFORM 999000-ERRO
               WHEN  OTHER
                     SET   ERRO-PROGRAMA-ACIONADO TO  TRUE
                     MOVE  SGTST030-MSG-ERRO  TO  MSG1-TX-RTN-PGM-ACND
                                                  MSG1-TX-ERRO-TS
                     MOVE SGTST030-CD-RTN-PGM TO  MSG1-CD-RTN-PGM-ACND
                                                  MSG1-CD-ERRO-TS
                     MOVE  NM-PGM             TO  MSG1-NM-PGM-ACND

      *              Monitorar variáveis do módulo chamado
                     PERFORM  969000-MONITORAR-VARIAVEIS
                     PERFORM  999000-ERRO
           END-EVALUATE
           .
       964000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       965000-CHAMA-SGTST031        SECTION.
      *--> TABELA ESPC_DBTR_OPR
      *-----------------------------------------------------------------
           MOVE 0057                   TO MSG1-CD-SEQL-ERRO

      *    Movimento nome do módulo para monitoração de variáveis
           MOVE SGTST031              TO NM-PGM
           EXEC CICS LINK
                PROGRAM  (SGTST031)
                COMMAREA (SGTST031-DADOS)
           END-EXEC

           IF  SGTST031-CD-RTN-PGM     NOT EQUAL ZEROS
               SET ERRO-PROGRAMA-ACIONADO TO TRUE
               MOVE NM-PGM                TO MSG1-NM-PGM-ACND
               MOVE SGTST031-MSG-ERRO-PGM TO MSG1-TX-RTN-PGM-ACND
                                             MSG1-TX-ERRO-TS
               MOVE SGTST031-CD-RTN-PGM   TO MSG1-CD-RTN-PGM-ACND
                                             MSG1-CD-ERRO-TS
      *        Monitorar variáveis do módulo chamado
               PERFORM 969000-MONITORAR-VARIAVEIS
               PERFORM 999000-ERRO
           END-IF
           .
       965000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       969000-MONITORAR-VARIAVEIS      SECTION.
      *--> Monitora as variáveis dos módulos chamados
      *-----------------------------------------------------------------

      * MOCK-POINT MCKV3
           EVALUATE NM-PGM

                WHEN SGTS211A
                    MOVE 'SGTS211A-E-CD-TIT-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTS211A-E-CD-TIT-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTS211A-E-CD-CTRA-TIT-VL'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTS211A-E-CD-CTRA-TIT-VL
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTS211A-E-CD-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
                    IF  SGTS211A-E-CD-OPR-ETRD
                                           NUMERIC
                        MOVE  SGTS211A-E-CD-OPR-ETRD
                                       TO  CD-OPR-ETRD-MCP-DISPLAY
                        MOVE CD-OPR-ETRD-MCP-DISPLAY
                                       TO  MSGKW999-VL-VRV-MNT
                    ELSE
                        MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
                    END-IF

                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTS211A-E-DT-CPR-OPR'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTS211A-E-DT-CPR-OPR
                                       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTS211A-E-VL-PU-CPR-TIT-OPR'
                                       TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTS211A-E-VL-PU-CPR-TIT-OPR
                                       TO AX-VALOR
                    MOVE AX-VALOR
                                       TO MSGKW999-VL-VRV-MNT

                    PERFORM 999003-ADICIONA-VRV-MNT

                WHEN SGTSPE23
                    MOVE 'SGTSPE23-CD-CTRA-TIT-VL'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSPE23-CD-CTRA-TIT-VL TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSPE23-CD-TIP-ATB-CTRA'
                                                  TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSPE23-CD-TIP-ATB-CTRA TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSPE23-CD-TIP-EST-DCS'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSPE23-CD-TIP-EST-DCS TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSPE23-TS-EST-CRTC-CTRA'
                                                 TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSPE23-TS-EST-CRTC-CTRA
                                                 TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                WHEN SGTSI063
                    MOVE 'SGTSI063-CD-CTRA-TIT-VL'
                                               TO  MSGKW999-NM-VRV-MNT
                    MOVE SGTSI063-CD-CTRA-TIT-VL
                                               TO  MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI063-CD-OPR-ETRD-MCP'
                                               TO  MSGKW999-NM-VRV-MNT
                    IF    SGTSI063-CD-OPR-ETRD-MCP IS NUMERIC
                          MOVE SGTSI063-CD-OPR-ETRD-MCP
                                             TO  CD-OPR-ETRD-MCP-DISPLAY
                          MOVE CD-OPR-ETRD-MCP-DISPLAY
                                               TO  MSGKW999-VL-VRV-MNT
                    ELSE
                          MOVE 'Código não numérico.'
                                               TO  MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI063-NR-VRS-OPR-ETRD'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI063-NR-VRS-OPR-ETRD
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI063-CD-TIP-IN'  TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI063-CD-TIP-IN    TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                 WHEN SGTSI064
                    MOVE 'SGTSI064-CD-CTRA-TIT-VL'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI064-CD-CTRA-TIT-VL
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI064-CD-OPR-ETRD-MCP'
                                               TO MSGKW999-NM-VRV-MNT
                    IF SGTSI064-CD-OPR-ETRD-MCP IS NUMERIC
                      MOVE SGTSI064-CD-OPR-ETRD-MCP
                                             TO  CD-OPR-ETRD-MCP-DISPLAY
                      MOVE CD-OPR-ETRD-MCP-DISPLAY
                                               TO MSGKW999-VL-VRV-MNT
                    ELSE
                          MOVE 'Código não numérico.'
                                               TO  MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI064-NR-VRS-OPR-ETRD'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI064-NR-VRS-OPR-ETRD
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI064-CD-BCO-PCT' TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI064-CD-BCO-PCT   TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI064-CD-TIP-PRTC'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI064-CD-TIP-PRTC  TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI064-IN-BCO-LDR' TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI064-IN-BCO-LDR   TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI064-PC-PRTC-BCO'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI064-PC-PRTC-BCO  TO AX-PC-PRTC-BCO
                    MOVE AX-PC-PRTC-BCO        TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI064-CD-USU-ULT-ATL'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI064-CD-USU-ULT-ATL
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI064-TS-ULT-ATL' TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI064-TS-ULT-ATL   TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                 WHEN SGTSI069
                    MOVE 'SGTSI069-CD-CTRA-TIT-VL'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI069-CD-CTRA-TIT-VL
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI069-CD-OPR-ETRD-MCP'
                                               TO MSGKW999-NM-VRV-MNT
                    IF SGTSI069-CD-OPR-ETRD-MCP IS NUMERIC
                       MOVE SGTSI069-CD-OPR-ETRD-MCP
                                             TO  CD-OPR-ETRD-MCP-DISPLAY
                       MOVE CD-OPR-ETRD-MCP-DISPLAY
                                               TO MSGKW999-VL-VRV-MNT
                    ELSE
                       MOVE 'Código não numérico.'
                                               TO  MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI069-NR-VRS-OPR-ETRD'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI069-NR-VRS-OPR-ETRD
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI069-CD-TIP-EST-VRS'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI069-CD-TIP-EST-VRS
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSI069-TX-OBS-EST-VRS'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSI069-TX-OBS-EST-VRS
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                WHEN SGTS0072
                    MOVE 'SGTS0072-CD-OPR-ETRD-OGM '
                                               TO MSGKW999-NM-VRV-MNT
                    IF SGTS0072-CD-OPR-ETRD-OGM IS NUMERIC
                       MOVE SGTS0072-CD-OPR-ETRD-OGM
                                             TO  CD-OPR-ETRD-MCP-DISPLAY
                       MOVE CD-OPR-ETRD-MCP-DISPLAY
                                               TO MSGKW999-VL-VRV-MNT
                    ELSE
                       MOVE 'Código não numérico.'
                                               TO  MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTS0072-NR-VRS-OPR-OGM'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTS0072-NR-VRS-OPR-OGM
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTS0072-CD-OPR-ETRD-DST'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTS0072-CD-OPR-ETRD-DST
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTS0072-NR-VRS-OPR-DST'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTS0072-NR-VRS-OPR-DST
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                WHEN SGTS0148
                    MOVE 'SGTS0148-CD-PRD'     TO MSGKW999-NM-VRV-MNT
                    MOVE SGTS0148-CD-PRD       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTS0148-CD-MDLD'    TO MSGKW999-NM-VRV-MNT
                    MOVE SGTS0148-CD-MDLD      TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SGTST002
                    MOVE 'SGTST002-ACAO'       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST002-ACAO         TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST002-CD-TIT-OPR-ETRD'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST002-CD-TIT-OPR-ETRD
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST002-NR-DVS-TIT' TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST002-NR-DVS-TIT   TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                WHEN SGTST009
                    MOVE 'SGTST009-ACAO'       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST009-ACAO         TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST009-CD-CTRA-TIT-VL'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST009-CD-CTRA-TIT-VL
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST009-CD-OPR-ETRD-MCP'
                                               TO MSGKW999-NM-VRV-MNT
                    IF  SGTST009-CD-OPR-ETRD-MCP IS NUMERIC
                        MOVE SGTST009-CD-OPR-ETRD-MCP
                                             TO  CD-OPR-ETRD-MCP-DISPLAY
                        MOVE CD-OPR-ETRD-MCP-DISPLAY
                                               TO MSGKW999-VL-VRV-MNT
                    ELSE
                        MOVE 'Código não numérico.'
                                               TO  MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST009-NR-VRS-OPR-ETRD'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST009-NR-VRS-OPR-ETRD
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST009-TS-EST-OPR-ETRD'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST009-TS-EST-OPR-ETRD
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST009-CD-USU-RSP-EST'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST009-CD-USU-RSP-EST
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST009-CD-TIP-EST-OPR'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST009-CD-TIP-EST-OPR
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST009-TX-JST-EST' TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST009-TX-JST-EST   TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                WHEN SGTST006
                    MOVE 'SGTST006-ACAO'       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST006-ACAO         TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST006-CD-CTRA-TIT-VL'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST006-CD-CTRA-TIT-VL
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST006-CD-OPR-ETRD-MCP'
                                               TO MSGKW999-NM-VRV-MNT
                    IF SGTST006-CD-OPR-ETRD-MCP IS NUMERIC
                       MOVE SGTST006-CD-OPR-ETRD-MCP
                                             TO  CD-OPR-ETRD-MCP-DISPLAY
                       MOVE CD-OPR-ETRD-MCP-DISPLAY
                                               TO MSGKW999-VL-VRV-MNT
                    ELSE
                       MOVE 'Código não numérico.'
                                               TO  MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST006-NR-VRS-OPR-ETRD'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST006-NR-VRS-OPR-ETRD
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST006-CD-CLI'     TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST006-CD-CLI       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST006-CD-TIP-FUC-CLI'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST006-CD-TIP-FUC-CLI
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                WHEN SGTSX005
                    MOVE 'SGTSX005-ACAO'       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSX005-ACAO         TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSX005-CD-CTRA-TIT-VL'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSX005-CD-CTRA-TIT-VL
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSX005-CD-OPR-ETRD-MCP'
                                               TO MSGKW999-NM-VRV-MNT
                    IF SGTSX005-CD-OPR-ETRD-MCP IS NUMERIC
                       MOVE SGTSX005-CD-OPR-ETRD-MCP
                                             TO  CD-OPR-ETRD-MCP-DISPLAY
                       MOVE CD-OPR-ETRD-MCP-DISPLAY
                                               TO MSGKW999-VL-VRV-MNT
                    ELSE
                       MOVE 'Código não numérico.'
                                               TO  MSGKW999-VL-VRV-MNT
                    END-IF
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTSX005-NR-VRS-OPR-ETRD'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTSX005-NR-VRS-OPR-ETRD
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                WHEN MSTS8041
                    MOVE 'MSTS8041-CD-DEPE-UOR-RQSC'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE MSTS8041-CD-DEPE-UOR-RQSC
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'MSTS8041-NR-ORD-SBDD-RQSC'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE MSTS8041-NR-ORD-SBDD-RQSC
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                 WHEN ARCSB318
                    MOVE '318L-CD-RISCO-E'     TO MSGKW999-NM-VRV-MNT
                    MOVE 318L-CD-RISCO-E       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                 WHEN MSTS8042
                    MOVE 'MSTS8042-CD-UOR'     TO MSGKW999-NM-VRV-MNT
                    MOVE MSTS8042-CD-UOR       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                 WHEN MCIS0001
                    MOVE 'MCIS0001-CLIENTE'    TO MSGKW999-NM-VRV-MNT
                    MOVE MCIS0001-CLIENTE      TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                  WHEN CAFS0053
                    MOVE 'S053-FUC'            TO MSGKW999-NM-VRV-MNT
                    MOVE S053-FUC              TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'S053-CD-CPSO'        TO MSGKW999-NM-VRV-MNT
                    MOVE S053-CD-CPSO OF S053-ENTD
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                 WHEN CAFS0007
                    MOVE 'PARM-BANCO-INF'      TO MSGKW999-NM-VRV-MNT
                    MOVE PARM-BANCO-INF        TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'PARM-AGE-INF'        TO MSGKW999-NM-VRV-MNT
                    MOVE PARM-AGE-INF          TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                WHEN PRDS0203
                    MOVE 'S0203-CD-FUNCAO'     TO MSGKW999-NM-VRV-MNT
                    MOVE S0203-CD-FUNCAO       TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'S0203-CD-PRD'        TO MSGKW999-NM-VRV-MNT
                    MOVE S0203-CD-PRD          TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'S0203-CD-MDLD'       TO MSGKW999-NM-VRV-MNT
                    MOVE S0203-CD-MDLD         TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                WHEN ICLSI046
                    MOVE 'I046-CD-CLRG'        TO MSGKW999-NM-VRV-MNT
                    MOVE I046-CD-CLRG          TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                WHEN SGTST027
                    MOVE 'SGTST027-ACAO'       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST027-ACAO         TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST027-CD-TIP-COTA-TIT'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST027-CD-TIP-COTA-TIT
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST027-NR-VRS-TIP-COTA'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST027-NR-VRS-TIP-COTA
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                WHEN SGTST029
                    MOVE 'SGTST029-ACAO'       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST029-ACAO         TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST029-CD-CTGR-TVM'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST029-CD-CTGR-TVM  TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    IF  SGTST029-CONSULTAR-DESCRICAO  OR
                        SGTST029-ALTERAR
                        MOVE 'SGTST029-NM-CTGR-TVM '
                                               TO MSGKW999-NM-VRV-MNT
                        MOVE SGTST029-NM-CTGR-TVM
                                               TO MSGKW999-VL-VRV-MNT
                        PERFORM 999003-ADICIONA-VRV-MNT
                    END-IF

                 WHEN SGTST030
                    MOVE 'SGTST030-ACAO'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST030-ACAO
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST030-CD-ITC-CVM-OPR'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST030-CD-ITC-CVM-OPR
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                  WHEN SGTST031
                    MOVE 'SGTST031-ACAO'       TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST030-ACAO         TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTST031-CD-ESPC-DBTR-OPR'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE SGTST031-CD-ESPC-DBTR-OPR
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                 WHEN SGTS0211
                    MOVE 'SGTS0211-CD-TIT-OPR-ETRD'
                                               TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTS0211-CD-TIT-OPR-ETRD
                                               TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'SGTS0211-NR-DVS-TIT' TO MSGKW999-NM-VRV-MNT
                    MOVE  SGTS0211-NR-DVS-TIT  TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

               WHEN SBDATAF
                    MOVE 'FUNCAO'              TO MSGKW999-NM-VRV-MNT
                    MOVE FUNCAO                TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'ARG01'               TO MSGKW999-NM-VRV-MNT
                    MOVE ARG01                 TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT

                    MOVE 'ARG02'               TO MSGKW999-NM-VRV-MNT
                    MOVE ARG02                 TO MSGKW999-VL-VRV-MNT
                    PERFORM 999003-ADICIONA-VRV-MNT


               WHEN OTHER
                    CONTINUE
           END-EVALUATE
           .
       969000-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *    Section da Faixa de 970000 a 979900 para Leitura de Dados
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       970000-LER-CANAL                SECTION.
      *--> Obtem o canal corrente
      *-----------------------------------------------------------------
           MOVE 0058                   TO MSG1-CD-SEQL-ERRO

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET  ERRO-CANAL             TO TRUE

           EXEC CICS ASSIGN
                     CHANNEL ( NM-CNL )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       970000-FIM.
           EXIT.

      *-----------------------------------------------------------------
       971000-LER-REQUISICAO           SECTION.
      *-----------------------------------------------------------------

           MOVE 0059                   TO MSG1-CD-SEQL-ERRO

      *--> Monta o nome do container de requisicao
           MOVE 'SGTS0088'             TO NM-PGM
           MOVE 'SGTS0088-RQSC'        TO NM-CTNR
           MOVE LENGTH OF SGTS0088-RQSC   TO  QT-CCT-EPRO

      *--> Recupera o tamanho do container de requisição
           PERFORM 973100-LER-CONTAINER-NODATA

      *     Se depois dos testes se verificar que não há mesmo
      *     necessidade destas linhas DELETAR o abaixo
      *     IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
      *         SET  ERRO-TAMANHO-AREA      TO  TRUE
      *         PERFORM 999000-ERRO
      *     END-IF

      *--> Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *--> Efetua a leitura do container
           EXEC CICS GET
                     CONTAINER ( NM-CTNR       )
                     FLENGTH   ( QT-CCT-LIDO   )
                     INTO      ( SGTS0088-RQSC )
           END-EXEC

      *Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       971000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       972000-LER-CONTAINER-DE-ERRO    SECTION.
      *--> A presença do container PRM-ERRO indica que houve erro de
      *    integração, a ausência do container indica sucesso.
      *-----------------------------------------------------------------

           MOVE 0060                   TO MSG1-CD-SEQL-ERRO

      *--> Le o container de erro dos modulos chamados
           SET GET-CONTAINER           TO TRUE
           MOVE 'PRM-ERRO'             TO NM-CTNR
           INITIALIZE IIBKERRO-PRM-ERRO

      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS GET
                     CONTAINER ( NM-CTNR                     )
                     INTO      ( IIBKERRO-PRM-ERRO           )
                     FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                     RESP      ( MSG1-RESP1                  )
                     RESP2     ( MSG1-RESP2                  )
           END-EXEC

           EVALUATE TRUE
      *        Caso deseje personalizar ou não interromper o
      *        processamento, alterar para não dar erro aqui SOMENTE
      *        PARA O MÓDULO CHAMADO e tratar na section chamadora
               WHEN MSG1-RESP1 EQUAL DFHRESP(CONTAINERERR)
                    CONTINUE
               WHEN MSG1-RESP1 EQUAL DFHRESP(NORMAL)
                   IF NM-PGM = 'MSTS8041'
                       SET TEM-CONTAINER-ERRO TO TRUE
                       CONTINUE
                   ELSE
                       SET ERRO-PROGRAMA-ACIONADO TO TRUE
                       MOVE  NM-PGM       TO  MSG1-NM-PGM-ACND

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

      *             Monitorar variáveis do módulo chamado
                       PERFORM 969000-MONITORAR-VARIAVEIS
                       PERFORM 999000-ERRO
                   END-IF
               WHEN OTHER
                    SET ERRO-CONTAINER  TO TRUE
                    PERFORM 999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       972000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       973000-LER-RESPOSTA             SECTION.
      *--> Recupera o container com os dados de resposta (saída).
      *    Modulo CONTAINER  -     NOME_PGM-RPST
      *    Modulo IIB (FIXO) -     PRM-DADOS-RPST
      *-----------------------------------------------------------------

           MOVE 0061                   TO MSG1-CD-SEQL-ERRO

      *--> Monta o nome dos container de acordo com modulo a executar
           MOVE SPACES                 TO NM-CTNR

      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *    nome_modulo -RPST

           EVALUATE NM-PGM

               WHEN PRDS0203
                    MOVE LENGTH OF PRDS0203-DADOS TO  QT-CCT-EPRO
                    MOVE 'PRM-DADOS-RPST'         TO NM-CTNR
               WHEN CAFS0053
                    MOVE LENGTH OF CAFS0053-DADOS TO  QT-CCT-EPRO
                    MOVE 'PRM-DADOS-RPST'         TO NM-CTNR
                WHEN CAFS0007
                    MOVE LENGTH OF CAFS0007-DADOS TO  QT-CCT-EPRO
                    MOVE 'PRM-DADOS-RPST'         TO NM-CTNR
               WHEN MCIS0001
                    MOVE LENGTH OF MCIS0001-DADOS TO  QT-CCT-EPRO
                    MOVE 'PRM-DADOS-RPST'         TO NM-CTNR
               WHEN MSTS8042
                    MOVE LENGTH OF MSTS8042-RPST TO  QT-CCT-EPRO
                    MOVE 'PRM-DADOS-RPST'         TO NM-CTNR
               WHEN ICLSI046
                    MOVE 'PRM-DADOS-RPST'         TO NM-CTNR
               WHEN MSTS8041
                    MOVE LENGTH OF MSTS8041-RPST TO  QT-CCT-EPRO
                    MOVE 'PRM-DADOS-RPST'         TO NM-CTNR

               WHEN SGTS0072
                    MOVE 'SGTS0072-RPST'        TO NM-CTNR
                    MOVE LENGTH OF SGTS0072-RPST TO  QT-CCT-EPRO

               WHEN SGTS0148
                    MOVE 'SGTS0148-RPST'        TO NM-CTNR
                    MOVE LENGTH OF SGTS0148-RPST TO  QT-CCT-EPRO

               WHEN SGTSI063
                    MOVE 'SGTSI063-RPST'        TO NM-CTNR
                    MOVE LENGTH OF SGTSI063-RPST TO  QT-CCT-EPRO

               WHEN SGTSI064
                    MOVE 'SGTSI064-RPST'        TO NM-CTNR
                    MOVE LENGTH OF SGTSI064-RPST TO  QT-CCT-EPRO

               WHEN SGTSI069
                    MOVE 'SGTSI069-RPST'        TO NM-CTNR
                    MOVE LENGTH OF SGTSI069-RPST TO  QT-CCT-EPRO

               WHEN SGTS0211
                    MOVE 'SGTS0211-RPST'        TO NM-CTNR
                    MOVE LENGTH OF SGTS0211-RPST TO  QT-CCT-EPRO

               WHEN SGTS211A
                    MOVE 'SGTS211A-RPST'        TO NM-CTNR
                    MOVE LENGTH OF SGTS211A-RPST TO  QT-CCT-EPRO

               WHEN OTHER
                   SET  ERRO-PROGRAMADO        TO  TRUE
                   MOVE 9005                   TO  MSG1-CD-ERRO-TS
                    STRING
                    'Módulo não definido: ' NM-PGM
                    DELIMITED BY SIZE INTO MSG1-TX-ERRO-TS
                    PERFORM 999000-ERRO
           END-EVALUATE

      *--> Recupera o tamanho do container de resposta
           PERFORM 973100-LER-CONTAINER-NODATA

           IF  NM-PGM  NOT EQUAL ICLSI046
               IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
                   SET   ERRO-TAMANHO-AREA     TO  TRUE
                   PERFORM 999000-ERRO
               END-IF
           END-IF

      *--> Configura dados para leitura do container de resposta
           SET  GET-CONTAINER          TO TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET  ERRO-CONTAINER         TO TRUE


      *--> Le a RESPOSTA de acordo com o programa que a gravou

           MOVE 0062                   TO MSG1-CD-SEQL-ERRO

           EVALUATE NM-PGM

      *        Calcular a quantidade de caracteres esperado e limpar
      *        a area de dados antes de executar o GET
      *        Para matrizes com dados variaveis (clausula depending
      *        on) nao ha como utilizar o comando INITIALIZE

                WHEN SGTS0072
                    INITIALIZE SGTS0072-RPST
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTS0072-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC
                WHEN SGTS0148
                    INITIALIZE SGTS0148-RPST
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTS0148-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC
                WHEN SGTSI063
                    INITIALIZE SGTSI063-RPST
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSI063-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN SGTSI064
                    INITIALIZE SGTSI064-RPST
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSI064-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN SGTSI069
                    INITIALIZE SGTSI069-RPST
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTSI069-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN MSTS8041
                    INITIALIZE MSTS8041-RPST
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (MSTS8041-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

              WHEN PRDS0203
                    INITIALIZE PRDS0203-DADOS
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (PRDS0203-DADOS   )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN MCIS0001
                    INITIALIZE MCIS0001-DADOS
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (MCIS0001-DADOS   )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

              WHEN MSTS8042
                    INITIALIZE MSTS8042-RPST
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (MSTS8042-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

              WHEN CAFS0007
                    INITIALIZE CAFS0007-DADOS
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (CAFS0007-DADOS   )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC
               WHEN CAFS0053
                    INITIALIZE CAFS0053-DADOS
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (CAFS0053-DADOS   )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC
               WHEN ICLSI046
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (ICLSI046-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC
               WHEN SGTS0211
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTS0211-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN SGTS211A
                    EXEC CICS GET
                              CONTAINER (NM-CTNR          )
                              INTO      (SGTS211A-RPST    )
                              FLENGTH   (QT-CCT-LIDO      )
                    END-EXEC

               WHEN OTHER
                    SET   ERRO-PROGRAMADO
                                       TO  TRUE
                    MOVE  9003         TO  MSG1-CD-ERRO-TS
                    STRING  'Erro na leitura da resposta do módulo '
                            NM-PGM '.'
                            DELIMITED BY SIZE
                            INTO           MSG1-TX-ERRO-TS
                    PERFORM 999000-ERRO
           END-EVALUATE

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       973000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       973100-LER-CONTAINER-NODATA     SECTION.
      *-----------------------------------------------------------------

           MOVE 0063                   TO MSG1-CD-SEQL-ERRO

      *--> Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
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
       974000-LER-DADOS-OPRD           SECTION.
      *-----------------------------------------------------------------
           MOVE 0064                   TO MSG1-CD-SEQL-ERRO

      *--> Configura dados para leitura do container de requisição
           SET  GET-CONTAINER          TO TRUE
           MOVE 'PRM-INF-OPRD'         TO NM-CTNR

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET  ERRO-CONTAINER         TO TRUE

           INITIALIZE PRM-INF-OPRD
           EXEC CICS GET
                CONTAINER ( NM-CTNR                )
                INTO      ( PRM-INF-OPRD           )
                FLENGTH   ( LENGTH OF PRM-INF-OPRD )
           END-EXEC.

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       974000-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *   Section da Faixa de 980000 a 989900 para Gravacao de Dados
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       980000-GRAVAR-CONTAINER-RPST    SECTION.
      *-----------------------------------------------------------------

      *    Exclui eventual container de erro de outros programas
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO

           MOVE 0065                   TO MSG1-CD-SEQL-ERRO

      *    Monta o nome do container de resposta
           MOVE 'SGTS0088-RPST'        TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET  ERRO-CONTAINER         TO TRUE

           EXEC CICS PUT
                     CONTAINER (NM-CTNR                    )
                     FROM      (SGTS0088-RPST          )
                     FLENGTH   (LENGTH OF SGTS0088-RPST)
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       980000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       981000-GRAVAR-CONTAINER-SRVC    SECTION.
      *--> Postar container de requisição de serviço.
      *-----------------------------------------------------------------
           MOVE 0066                   TO MSG1-CD-SEQL-ERRO

      *--> Monta o nome do container de requisicao do servico
           MOVE 'PRM-RQSC-SRVC'        TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE

           EVALUATE NM-PGM
               WHEN MSTS8041
                    MOVE ZEROS         TO SRVC-ID-SRVC
                    MOVE ZEROS         TO SRVC-VRS-SRVC
                    MOVE 1963705       TO SRVC-ID-OPR
                    MOVE 1             TO SRVC-VRS-OPR
               WHEN MCIS0001
                    MOVE 19485         TO SRVC-ID-SRVC
                    MOVE 1             TO SRVC-VRS-SRVC
                    MOVE 207827        TO SRVC-ID-OPR
                    MOVE 1             TO SRVC-VRS-OPR
               WHEN ICLSI046
                    MOVE 58340         TO SRVC-ID-SRVC
                    MOVE 1             TO SRVC-VRS-SRVC
                    MOVE 1824215       TO SRVC-ID-OPR
                    MOVE 1             TO SRVC-VRS-OPR
               WHEN CAFS0007
                    MOVE 150834        TO SRVC-ID-SRVC
                    MOVE 1             TO SRVC-VRS-SRVC
                    MOVE 1062294       TO SRVC-ID-OPR
                    MOVE 1             TO SRVC-VRS-OPR
               WHEN CAFS0053
                    MOVE 150834        TO SRVC-ID-SRVC
                    MOVE 1             TO SRVC-VRS-SRVC
                    MOVE 1092761       TO SRVC-ID-OPR
                    MOVE 1             TO SRVC-VRS-OPR

               WHEN PRDS0203
                    MOVE 214106        TO SRVC-ID-SRVC
                    MOVE 1             TO SRVC-VRS-SRVC
                    MOVE 543363        TO SRVC-ID-OPR
                    MOVE 1             TO SRVC-VRS-OPR
               WHEN MSTS8042
                    MOVE ZEROS         TO SRVC-ID-SRVC
                    MOVE ZEROS         TO SRVC-VRS-SRVC
                    MOVE 1998646       TO SRVC-ID-OPR
                    MOVE 1             TO SRVC-VRS-OPR
           END-EVALUATE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET  ERRO-CONTAINER         TO TRUE

           EXEC CICS PUT
                     CONTAINER ( NM-CTNR                     )
                     FROM      ( SRVC-PRM-RQSC-SRVC          )
                     FLENGTH   ( LENGTH OF SRVC-PRM-RQSC-SRVC)
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       981000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       982000-GRAVAR-CONTAINER-RQSC    SECTION.
      *    Postar container com dados de requisição (entrada)
      *    Modulo CONTAINER  -     NOME_PGM-RQSC
      *    Modulo IIB (FIXO) -     PRM-DADOS-RQSC
      *-----------------------------------------------------------------

           MOVE 0067                   TO MSG1-CD-SEQL-ERRO

      *--> Monta o nome dos container de acordo com modulo a executar
           MOVE SPACES                 TO NM-CTNR
           SET PUT-CONTAINER           TO TRUE

      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RQSC, senão
      *    nome_modulo -RQSC
           EVALUATE NM-PGM

               WHEN MCIS0001
                    MOVE 'PRM-DADOS-RQSC'
                                       TO NM-CTNR
               WHEN PRDS0203
                    MOVE 'PRM-DADOS-RQSC'
                                       TO NM-CTNR
               WHEN ICLSI046
                    MOVE 'PRM-DADOS-RQSC'
                                       TO NM-CTNR
               WHEN CAFS0007
                    MOVE 'PRM-DADOS-RQSC'
                                       TO NM-CTNR
               WHEN CAFS0053
                    MOVE 'PRM-DADOS-RQSC'
                                       TO NM-CTNR
               WHEN MSTS8042
                    MOVE 'PRM-DADOS-RQSC'
                                       TO NM-CTNR
               WHEN MSTS8041
                    MOVE 'PRM-DADOS-RQSC'
                                       TO NM-CTNR
               WHEN OTHER
                    MOVE NM-PGM        TO NM-CTNR(1:8)
                    MOVE '-RQSC'       TO NM-CTNR(9:5)
           END-EVALUATE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET  ERRO-CONTAINER         TO TRUE

      *--> Faz put da requisicao do programa a executar
           EVALUATE NM-PGM
                WHEN SGTS0072
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (SGTS0072-RQSC )
                              FLENGTH   (LENGTH OF SGTS0072-RQSC )
                    END-EXEC
                WHEN SGTS0148
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (SGTS0148-RQSC )
                              FLENGTH   (LENGTH OF SGTS0148-RQSC )
                    END-EXEC
                WHEN SGTSI063
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (SGTSI063-RQSC  )
                              FLENGTH   (LENGTH OF SGTSI063-RQSC  )
                    END-EXEC
               WHEN SGTSI064
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (SGTSI064-RQSC  )
                              FLENGTH   (LENGTH OF SGTSI064-RQSC  )
                    END-EXEC
               WHEN SGTSI069
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (SGTSI069-RQSC  )
                              FLENGTH   (LENGTH OF SGTSI069-RQSC  )
                    END-EXEC
              WHEN MSTS8041
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (MSTS8041-RQSC )
                              FLENGTH   (LENGTH OF MSTS8041-RQSC )
                    END-EXEC
                WHEN MCIS0001
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (MCIS0001-DADOS )
                              FLENGTH   (LENGTH OF MCIS0001-DADOS )
                    END-EXEC
               WHEN PRDS0203
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (PRDS0203-DADOS )
                              FLENGTH   (LENGTH OF PRDS0203-DADOS )
                    END-EXEC
               WHEN ICLSI046
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (ICLSI046-DADOS )
                              FLENGTH   (LENGTH OF ICLSI046-DADOS )
                    END-EXEC
               WHEN CAFS0007
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (CAFS0007-DADOS )
                              FLENGTH   (LENGTH OF CAFS0007-DADOS )
                    END-EXEC
               WHEN CAFS0053
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (CAFS0053-DADOS )
                              FLENGTH   (LENGTH OF CAFS0053-DADOS )
                    END-EXEC
               WHEN MSTS8042
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (MSTS8042-RQSC )
                              FLENGTH   (LENGTH OF MSTS8042-RQSC )
                    END-EXEC
               WHEN SGTS0211
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR)
                              FROM      (SGTS0211-RQSC )
                              FLENGTH   (LENGTH OF SGTS0211-RQSC )
                    END-EXEC

               WHEN SGTS211A
                    EXEC CICS PUT
                              CONTAINER (NM-CTNR                  )
                              FROM      (SGTS211A-RQSC            )
                              FLENGTH   (LENGTH OF SGTS211A-RQSC  )
                    END-EXEC

               WHEN OTHER
                    SET   ERRO-PROGRAMADO
                                       TO  TRUE
                    MOVE  9004         TO  MSG1-CD-ERRO-TS
                    STRING  'Erro na gravação da requisição do módulo '
                            NM-PGM '.'
                            DELIMITED BY SIZE
                            INTO           MSG1-TX-ERRO-TS
                    PERFORM 999000-ERRO
           END-EVALUATE

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET  SEM-ERRO               TO TRUE
           .
       982000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       983000-EXCLUIR-RESPOSTA-ERRO    SECTION.
      *--> Exclui o container de resposta e de erro para evitar
      *    problemas em modulos executados mais de uma vez
      *    dentro da mesma sessao do CICS.
      *    Equivale ao INITIALIZE na area de resposta dos programas
      *    que utilizam COMMAREA.
      *-----------------------------------------------------------------
           MOVE 0068                   TO MSG1-CD-SEQL-ERRO

      *--> Quando módulos via IIB, fixo nome PRM-DADOS-RPST, senão
      *    nome_modulo -RPST
           EVALUATE NM-PGM
               WHEN ICLSI046
                    MOVE 'PRM-DADOS-RPST'
                                       TO NM-CTNR
               WHEN CAFS0007
                    MOVE 'PRM-DADOS-RPST'
                                       TO NM-CTNR
               WHEN CAFS0053
                    MOVE 'PRM-DADOS-RPST'
                                       TO NM-CTNR
               WHEN PRDS0203
                    MOVE 'PRM-DADOS-RPST'
                                       TO NM-CTNR
               WHEN MSTS8042
                    MOVE 'PRM-DADOS-RPST'
                                       TO NM-CTNR
               WHEN MCIS0001
                    MOVE 'PRM-DADOS-RPST'
                                       TO NM-CTNR
               WHEN MSTS8041
                    MOVE 'PRM-DADOS-RPST'
                                       TO NM-CTNR
               WHEN OTHER
                    MOVE NM-PGM        TO NM-CTNR(1:8)
                    MOVE '-RPST'       TO NM-CTNR(9:5)
           END-EVALUATE

           SET DEL-CONTAINER           TO TRUE

      *--> Exclui o container de resposta
      *    Não há captura automática de erro CICS por usar RESP e RESP2
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

      *--> Excluir o container de erro
           PERFORM 983100-EXCLUIR-CONTAINER-ERRO
           .
       983000-SAI.
           EXIT.
      *
      *-----------------------------------------------------------------
       983100-EXCLUIR-CONTAINER-ERRO   SECTION.
      *-----------------------------------------------------------------
           MOVE 0069                   TO MSG1-CD-SEQL-ERRO

      *--> Limpar variaveis container de erro
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
       984000-GRAVAR-CONTAINER-ERRO    SECTION.
      *-----------------------------------------------------------------

      *--> Grava o erro gerado no container de erro
      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS PUT
                     CONTAINER ( 'PRM-ERRO'                  )
                     FROM      ( IIBKERRO-PRM-ERRO           )
                     FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                     RESP      ( MSG1-RESP1                  )
                     RESP2     ( MSG1-RESP2                  )
           END-EXEC
           .
       984000-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *    As sections abaixo iniciadas com 999000 representam os
      *    tratamentos de erro utilizando o Sistema MSG(rotina MSGS0001)
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       999000-ERRO                     SECTION.
      *-----------------------------------------------------------------

           IF  MSG1-CD-ERRO-TS             EQUAL ZEROS
               MOVE  8888              TO MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS             EQUAL SPACES
               MOVE  'Erro não especificado no módulo.'
                                       TO  MSG1-TX-ERRO-TS
           END-IF
           PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           .
       999000-ERRO-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999000-GRAVA-ERRO-E-ENCERRA     SECTION.
      *-----------------------------------------------------------------

      *--> Em caso de erro, desfaz as atualizações em banco de dados

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

      *--> Adiciona as variáveis de entrada para monitoração
           IF  MNT-VRV

               MOVE 'SGTS0088-CD-CTRA-TIT-VL-OPR'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0088-CD-CTRA-TIT-VL-OPR
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0088-CD-CTRA-TIT-VL-COPD'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0088-CD-CTRA-TIT-VL-COPD
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0088-CD-OPR-ETRD-COPD'
                                       TO MSGKW999-NM-VRV-MNT
               IF SGTS0088-CD-OPR-ETRD-COPD IS NUMERIC
                  MOVE SGTS0088-CD-OPR-ETRD-COPD
                                       TO  CD-OPR-ETRD-MCP-DISPLAY
                  MOVE CD-OPR-ETRD-MCP-DISPLAY
                                       TO MSGKW999-VL-VRV-MNT
               ELSE
                  MOVE 'Código não numérico.'
                                      TO  MSGKW999-VL-VRV-MNT
               END-IF
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0088-NR-VRS-OPR-ETRD-COPD'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0088-NR-VRS-OPR-ETRD-COPD
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0088-CD-PRD'  TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0088-CD-PRD   TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0088-CD-MDLD' TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0088-CD-MDLD  TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0088-CD-TIT-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0088-CD-TIT-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0088-NR-DVS-TIT'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0088-NR-DVS-TIT
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0088-CD-ITC-CVM'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0088-CD-ITC-CVM
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0088-CD-CTGR-TVM'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0088-CD-CTGR-TVM
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0088-CD-ESPC-DBTR-OPR'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0088-CD-ESPC-DBTR-OPR
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0088-CD-TIP-COTA-TIT'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0088-CD-TIP-COTA-TIT
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0088-NR-VRS-TIP-COTA'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0088-NR-VRS-TIP-COTA
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0088-CD-CMRA-OPR-ETRD'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0088-CD-CMRA-OPR-ETRD
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0088-NR-EMS-TIT-OPR'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0088-NR-EMS-TIT-OPR
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0088-NR-SRE-EMS-TIT-OPR'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0088-NR-SRE-EMS-TIT-OPR
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0088-CD-ISIN-TIT-OPR'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0088-CD-ISIN-TIT-OPR
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0088-CD-TIP-MC-CPR'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0088-CD-TIP-MC-CPR
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0088-CD-BCO-CRD-VL-OPR'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0088-CD-BCO-CRD-VL-OPR
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0088-CD-DEPE-CRD-VL-OPR'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0088-CD-DEPE-CRD-VL-OPR
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0088-NR-CC-CRD-VL-OPR'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0088-NR-CC-CRD-VL-OPR
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0088-CD-CLI-EMT'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0088-CD-CLI-EMT
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0088-CD-CLI-IFLC-LMCR'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0088-CD-CLI-IFLC-LMCR
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0088-CD-CLI-CDT'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0088-CD-CLI-CDT
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0088-CD-CLI-ADM'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0088-CD-CLI-ADM
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0088-CD-CLI-AGT-FDAR'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0088-CD-CLI-AGT-FDAR
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

                MOVE 'SGTS0088-DT-EMS-TIT-OPR'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0088-DT-EMS-TIT-OPR
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0088-QT-PAPL-TIT-EMS'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0088-QT-PAPL-TIT-EMS
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0088-VL-PU-EMS-TIT-OPR'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0088-VL-PU-EMS-TIT-OPR
                                       TO AX-VALOR
               MOVE AX-VALOR           TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0088-DT-CPR-OPR'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0088-DT-CPR-OPR
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0088-QT-PAPL-TIT-CPR'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0088-QT-PAPL-TIT-CPR
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0088-VL-PU-CPR-TIT-OPR'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0088-VL-PU-CPR-TIT-OPR
                                       TO AX-VALOR
               MOVE AX-VALOR           TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0088-VL-CPR-OPR'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0088-VL-CPR-OPR
                                       TO AX-VALOR2
               MOVE AX-VALOR2          TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0088-DT-VNCT-OPR'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0088-DT-VNCT-OPR
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0088-CD-RSCO-OPR'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0088-CD-RSCO-OPR
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0088-VL-PRV-ENCT'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0088-VL-PRV-ENCT
                                       TO AX-VALOR2
               MOVE AX-VALOR2          TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0088-VL-APVD-BB-OPR'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0088-VL-APVD-BB-OPR
                                       TO AX-VALOR2
               MOVE AX-VALOR2          TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0088-IN-CNG-PBCO-OPR'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0088-IN-CNG-PBCO-OPR
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0088-IN-FLZ-EXCO-OPR'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0088-IN-FLZ-EXCO-OPR
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0088-IN-OPR-ETRD-IETR'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0088-IN-OPR-ETRD-IETR
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0088-IN-N-CNV-OPR-ACAO'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0088-IN-N-CNV-OPR-ACAO
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0088-IN-OPR-SPT-FCTR'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0088-IN-OPR-SPT-FCTR
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0088-CD-TIP-VRS-OPR'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0088-CD-TIP-VRS-OPR
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0088-TX-OBS-VRS'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0088-TX-OBS-VRS
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0088-CD-CMCP'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0088-CD-CMCP
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0088-CD-GR-MCP'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0088-CD-GR-MCP
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0088-CD-LVRO-MC-CPTL'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0088-CD-LVRO-MC-CPTL
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0088-CD-SMCP'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0088-CD-SMCP
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

               MOVE 'SGTS0088-CD-CLSC-ITMT-FNCO'
                                       TO MSGKW999-NM-VRV-MNT
               MOVE  SGTS0088-CD-CLSC-ITMT-FNCO
                                       TO MSGKW999-VL-VRV-MNT
               PERFORM 999003-ADICIONA-VRV-MNT

      *--> Monitora variáveis dinamicas de mensagem.

           MOVE 'AUX-CD-OPR-ETRD-MCP'           TO  MSGKW999-NM-VRV-MNT
           MOVE  AUX-CD-OPR-ETRD-MCP            TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE 'AUX-NR-VRS-OPR-ETRD'           TO  MSGKW999-NM-VRV-MNT
           MOVE  AUX-NR-VRS-OPR-ETRD            TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

      *--> Verifica se o erro foi em registro de Tabela
               IF COM-ERRO-TAB
                  PERFORM 999100-ADICIONA-OCR-TAB
               END-IF

           END-IF

      *--> Grava o erro gerado no sistema MSG - Mensageria
           PERFORM MSG1-NTF-MSG

           MOVE MSG1-CD-ERRO-TS        TO IIBKERRO-CD-ERRO
           MOVE MSG1-TX-ERRO-TS        TO IIBKERRO-TX-ERRO

      *--> Grava o erro gerado no container de erro
           PERFORM 984000-GRAVAR-CONTAINER-ERRO

      *--> Encerra a transacao
           PERFORM 999999-ENCERRAR
           .
       999000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       999100-ADICIONA-OCR-TAB         SECTION.
      *--> Adiciona Ocorrencia da Tabela  para Monitoracao
      *-----------------------------------------------------------------

           MOVE  'IC'            TO  MSGKW999-NM-VRV-MNT
           MOVE  IC              TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

           MOVE 'SGTS0088-CD-BCO-PCT (IC)'     TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0088-CD-BCO-PCT (IC)      TO  MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT
           .
       999100-SAI.
           EXIT.

      * ---------------------------------------
      * 999001-TRATA-ABEND-CICS        SECTION.
      * 999002-TRATA-CONDITION-CICS    SECTION.
      * 999003-ADICIONA-VRV-MNT        SECTION.
      * 999004-ALINHA-NUMERO-ESQUERDA  SECTION.
      * ---------------------------------------
-INC HLPKP999
      * ---------------------------------------
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *----------------------------------------
-INC SGTKP999

      *----------------------------------------
      * 999010-TRATA-ERRO-DB2          SECTION.
      *----------------------------------------
-INC HLPKP997

      * ---------------------------------------
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
      * ---------------------------------------
-INC MSGKPMG1
      *-----------------------------------------------------------------
       999999-ENCERRAR                 SECTION.
      *-----------------------------------------------------------------

           EXEC CICS
                RETURN
           END-EXEC
           .
       999999-SAI.
           EXIT.                                                           00009
      *---------------------- FIM SGTS0088 -----------------------------
