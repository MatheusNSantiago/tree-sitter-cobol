      *-----------------------------------------------------------------
      * Programa..: SGTS0185
      * Analista..: F2054109 Cintia Ap Soares Iglesias
      * Autor.....: F2054109 Cintia Ap Soares Iglesias
      * Data......: 11/09/2017
      * Objetivo..: Comparar taxas de precificação com taxas remuneração
      * Compilacao: Cobol/CICS
      *-----------------------------------------------------------------
      * VRS001 02/01/2018 F9188847 Acerto LER-CONTAINER-DE-ERRO inclusao
      *        tratamento erro para prg SGT e GOBACK.
      *        10/09/2018 F9158937 Acerto consistência impeditiva quando
      *         taxa precificação maior taxa remuneração
      *        11/09/2017  F2054109 DE 2015/137233 - IMPLANTACAO
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       IDENTIFICATION                  DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                     SGTS0185.

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
       77  CTE-PROG                    PIC  X(18) VALUE
                                       '*** SGTS0185 ***'.
       77  CTE-VERS                    PIC  X(06) VALUE 'VRS001'.

      *--> Modulos Acionados
       77  SGTSL011                    PIC  X(08) VALUE 'SGTSL011'.
       77  SGTSL508                    PIC  X(08) VALUE 'SGTSL508'.
       77  SGTS0136                    PIC  X(08) VALUE 'SGTS0136'.

      *-----------------------------------------------------------------
       LOCAL-STORAGE                   SECTION.
      *-----------------------------------------------------------------

      *--> Area de Trabalho
       01  IC-TAB                      PIC S9(04) COMP-5 VALUE ZEROS.
       01  TX-DCR-DFRC                 PIC  X(100).

      *--> Indicadores Auxiliares
       01  IN-MOVIMENTACAO             PIC  X(01) VALUE 'N'.
           88 MOVE-SAIDA                          VALUE 'S'.
       01  IN-CARREGA-LISTA-RMNC       PIC  X(01).
           88 CARREGA-SAIDA-RMNC                  VALUE 'S'.

      *--> Auxiliares Movimentacao Monitoração
       01  CD-OPR-ETRD-TX.
           03  CD-OPR-ETRD             PIC  9(11).
       01  NRO-DEC-AUX                 PIC  9(04)V9(10).
       01  NRO-DEC-AUX-R               REDEFINES NRO-DEC-AUX.
           03  NRO-DEC-ALFA            PIC  9(14).

       01  PC-PRECIF-AUX               PIC S9(4)V9(10).
       01  PC-PRECIF-AUX-R             REDEFINES PC-PRECIF-AUX.
           03  PC-PRECIF-ALFA          PIC  X(14).

       01  PC-REMUN-AUX                PIC S9(4)V9(10).
       01  PC-REMUN-AUX-R              REDEFINES PC-REMUN-AUX.
           03  PC-REMUN-ALFA           PIC  X(14).

      *-----------------------------------------------------------------
      *    Books Utilizados
      *-----------------------------------------------------------------

      *--> Book do container de Serviços (para acionamento de serviços)
       COPY IIBKSRVC.

      *--> Book do Container de Erro
       COPY IIBKERRO.

      *--> Book para captura de abend cics da bloco codigo MSGKP999
       COPY HLPKW999.

      *--> Book das variáveis auxiliares do tratamento de erro via MSG
       COPY SGTKERR1.

      *--> Book para captura de erro via MSG1
       COPY MSGKWMG1.

      *--> Book do container de requisicao e resposta deste modulo
       01 SGTS0185-DADOS.
       COPY SGTK0185.

      *-----------------------------------------------------------------
      *    Book dos Containers e das subrotinas chamadas
      *-----------------------------------------------------------------

      *--> Consulta da tab RMNC_OPR_ETRD
       01  SGTSL011-DADOS.
       COPY SGTKL011.

      *--> Listar Precificações Vigentes em um Periodo
      *01  SGTSL508-DADOS
       COPY SGTKL508.

      *--> Gerar Texto de Remuneração das Parcelas do Cronograma
       01  SGTS0136-DADOS.
       COPY SGTK0136.

       77  CTE-FINAL-SS                PIC X(40) VALUE
                                       '*** S.S. Termina Aqui ***'.

      *-----------------------------------------------------------------
       PROCEDURE                       DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL         SECTION.

           PERFORM  100000-INICIAR

           PERFORM  200000-PROCESSAR

           PERFORM  300000-POSTAR-RESPOSTA

           PERFORM  999999-ENCERRAR

           GOBACK
           .
       000000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       100000-INICIAR                  SECTION.
      *-----------------------------------------------------------------
           MOVE  0001                  TO  MSG1-CD-SEQL-ERRO

      *--> Ativa a captura automatica de abend do CICS.
      *    WHENEVER CICS
       COPY HLPKE999.

           PERFORM  970000-LER-CANAL

      *--> Define o nome do programa p/ fins manipulacao container
           MOVE  'SGTS0185'            TO  NM-PGM

      *--> Limpa a resposta e erro antes de executar o modulo
           PERFORM  983000-EXCLUIR-RESPOSTA-ERRO

           PERFORM  971000-LER-REQUISICAO

           PERFORM  110000-VALIDAR-REQUISICAO
           .
       100000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       110000-VALIDAR-REQUISICAO       SECTION.
      *--> Valida campo a campo da requisicao
      *-----------------------------------------------------------------
           MOVE  0002                  TO  MSG1-CD-SEQL-ERRO

      *--> Validar Chave da Operação Estruturada
           IF  SGTS0185-CD-CTRA-TIT-VL     NEGATIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0001              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' inválido.'      TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0185-CD-CTRA-TIT-VL     EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0002              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da carteira de título e valores mobiliários
      -              ' não informado.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0185-CD-OPR-ETRD-MCP    NOT NUMERIC
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0003              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada mercado de capitais nã
      -              'o numérico.'     TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0185-CD-OPR-ETRD-MCP    NEGATIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0004              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da operação estruturada do mercado de capit
      -              'ais inválido.'   TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0185-CD-OPR-ETRD-MCP    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0005              TO  MSG1-CD-ERRO-TS
               MOVE  'Código operação estruturada mercado de capitais nã
      -              'o informado.'    TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0185-NR-VRS-OPR-ETRD    NEGATIVE
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0006              TO  MSG1-CD-ERRO-TS
               MOVE  'Número versão operação estruturada inválido.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           IF  SGTS0185-NR-VRS-OPR-ETRD    EQUAL  ZEROS
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0007              TO  MSG1-CD-ERRO-TS
               MOVE  'Número da versão da operação estruturada não infor
      -              'mado.'           TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

      *--> Validar Outros Campos de Entrada
           IF  SGTS0185-CD-LS-RTN          NOT EQUAL  0001  AND  0002
               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0577              TO  MSG1-CD-ERRO-TS
               MOVE  'Código da lista de retorno inválida.'
                                       TO  MSG1-TX-ERRO-TS
               PERFORM  999000-ERRO
           END-IF

           .
       110000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       200000-PROCESSAR                SECTION.
      *-----------------------------------------------------------------
           MOVE  0003                  TO  MSG1-CD-SEQL-ERRO

           MOVE  ZEROS                 TO  IC-TAB

           PERFORM  210000-LISTAR-REMUNERACOES

           PERFORM  220000-COMPARAR-RMNC-PREF
                    VARYING                SGTSL011-IC-TAB
                    FROM                   0001 BY 0001
                    UNTIL SGTSL011-IC-TAB > SGTSL011-QT-OCR-TAB

           IF  SGTSL011-TEM-MAIS-REGISTRO
               PERFORM  UNTIL NOT SGTSL011-TEM-MAIS-REGISTRO
                 MOVE  SGTSL011-DT-INC-TAXA-RMNC(SGTSL011-QT-OCR-TAB)
                                       TO  SGTSL011-DT-INC-TAXA-ULT
                 MOVE  SGTSL011        TO  NM-PGM
                 PERFORM  900000-EXECUTAR-MODULO
                 PERFORM  220000-COMPARAR-RMNC-PREF
                          VARYING          SGTSL011-IC-TAB
                          FROM             0001 BY 0001
                          UNTIL SGTSL011-IC-TAB > SGTSL011-QT-OCR-TAB
               END-PERFORM
           END-IF

           MOVE  IC-TAB                TO  SGTS0185-QT-OCR-TAB

           .
       200000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       210000-LISTAR-REMUNERACOES      SECTION.
      *-----------------------------------------------------------------

           MOVE  0004                  TO  MSG1-CD-SEQL-ERRO

           MOVE  SGTSL011              TO  NM-PGM

           MOVE  SGTS0185-CD-CTRA-TIT-VL
                                       TO  SGTSL011-CD-CTRA-TIT-VL
           MOVE  SGTS0185-CD-OPR-ETRD-MCP
                                       TO  SGTSL011-CD-OPR-ETRD-MCP
           MOVE  SGTS0185-NR-VRS-OPR-ETRD
                                       TO  SGTSL011-NR-VRS-OPR-ETRD
           MOVE  SPACES                TO  SGTSL011-DT-INC-TAXA-ULT

           PERFORM  900000-EXECUTAR-MODULO

           .
       210000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       220000-COMPARAR-RMNC-PREF       SECTION.
      *-----------------------------------------------------------------
           MOVE  0005                  TO  MSG1-CD-SEQL-ERRO

           MOVE  SPACES                TO  IN-CARREGA-LISTA-RMNC

           PERFORM  221000-LISTAR-PREF

           PERFORM  222000-TRATAR-PREF
                    VARYING                SGTSL508-IC-TAB
                    FROM                   0001 BY 0001
                    UNTIL SGTSL508-IC-TAB > SGTSL508-QT-OCR-TAB

           IF  SGTS0185-LS-RMNC  AND
               CARREGA-SAIDA-RMNC
               PERFORM  223000-CARREGA-SAIDA-RMNC
           END-IF

           .
       220000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       221000-LISTAR-PREF              SECTION.
      *-----------------------------------------------------------------
           MOVE  0006                  TO  MSG1-CD-SEQL-ERRO

           MOVE  SGTSL508              TO  NM-PGM

           MOVE  SGTS0185-CD-CTRA-TIT-VL
                                       TO  SGTSL508-CD-CTRA-TIT-VL
           MOVE  SGTS0185-CD-OPR-ETRD-MCP
                                       TO  SGTSL508-CD-OPR-ETRD-MCP
           MOVE  SGTS0185-NR-VRS-OPR-ETRD
                                       TO  SGTSL508-NR-VRS-OPR-ETRD

           MOVE  0001                  TO  SGTSL508-CD-TIP-TAXA
           MOVE  SGTSL011-DT-INC-TAXA-RMNC(SGTSL011-IC-TAB)
                                       TO  SGTSL508-DT-INC-PSQ
           MOVE  SGTSL011-DT-FIM-TAXA-RMNC(SGTSL011-IC-TAB)
                                       TO  SGTSL508-DT-FIM-PSQ
           MOVE  SPACES                TO  SGTSL508-DT-ULT-INC

           PERFORM  900000-EXECUTAR-MODULO

           IF  SGTSL508-QT-OCR-TAB         EQUAL  ZEROS
                ADD   0001             TO  IC-TAB
                IF  IC-TAB                 GREATER  0200
                    SET   ERRO-PROGRAMADO
                                       TO  TRUE
                    MOVE  0578         TO  MSG1-CD-ERRO-TS
                    MOVE  'Excedeu a quantidade máxima da tabela de saíd
      -                   'a.'         TO  MSG1-TX-ERRO-TS
                    PERFORM  999000-GRAVA-ERRO-E-ENCERRA
                END-IF

                MOVE  CORR SGTSL011-DT-INC-TAXA-RMNC-R (SGTSL011-IC-TAB)
                  TO  SGTS0185-DT-INC-CND-TAXA-R                (IC-TAB)

                MOVE  CORR SGTSL011-DT-FIM-TAXA-RMNC-R (SGTSL011-IC-TAB)
                  TO  SGTS0185-DT-FIM-CND-TAXA-R                (IC-TAB)
                MOVE  SPACES
                  TO  SGTS0185-TX-DCR-TAXA-OPR                  (IC-TAB)
                MOVE  'Não existe precificação para o período.'
                  TO  SGTS0185-TX-DCR-DFRC                      (IC-TAB)

           END-IF

           .
       221000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       222000-TRATAR-PREF              SECTION.
      *-----------------------------------------------------------------
           MOVE  0007                  TO  MSG1-CD-SEQL-ERRO

           MOVE  SPACES                TO  IN-MOVIMENTACAO

      *--> Percentual Precificação não pode ser maior Remuneração
           IF       SGTSL508-PC-MIN-TAXA      (SGTSL508-IC-TAB)
                      GREATER
                    SGTSL011-PC-TAXA-RMNC-OPR (SGTSL011-IC-TAB)
               AND  SGTSL508-CD-TIP-APL-PREF  (SGTSL508-IC-TAB)
                      EQUAL
                    SGTSL011-CD-TIP-FMA-APL   (SGTSL011-IC-TAB)
               AND  SGTSL508-CD-PERC-APL-PREF (SGTSL508-IC-TAB)
                      EQUAL
                    SGTSL011-CD-PERC-CPTZ-JUR (SGTSL011-IC-TAB)
               AND  SGTSL508-CD-IEC-PREF      (SGTSL508-IC-TAB)
                      EQUAL
                    SGTSL011-CD-IEC-RMNC      (SGTSL011-IC-TAB)

               SET   ERRO-PROGRAMADO   TO  TRUE
               MOVE  0822              TO  MSG1-CD-ERRO-TS
               MOVE  'Taxa precificada maior que taxa informada na remun
      -              'eração.'         TO  MSG1-TX-ERRO-TS
      *--> Adiciona as variáveis  para monitoração
               MOVE  'Taxa precificação'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTSL508-PC-MIN-TAXA  (SGTSL508-IC-TAB)
                                       TO PC-PRECIF-AUX
               MOVE PC-PRECIF-ALFA     TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT
               MOVE  'Taxa remuneração'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTSL011-PC-TAXA-RMNC-OPR (SGTSL011-IC-TAB)
                                       TO PC-REMUN-AUX
               MOVE  PC-REMUN-ALFA     TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT
               MOVE  'CD-TIP-APL-PREF'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTSL508-CD-TIP-APL-PREF  (SGTSL508-IC-TAB)
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT
               MOVE  'CD-PERC-APL-PREF'
                                       TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTSL508-CD-PERC-APL-PREF (SGTSL508-IC-TAB)
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT
               MOVE  'CD-IEC-PREF'     TO  MSGKW999-NM-VRV-MNT
               MOVE  SGTSL508-CD-IEC-PREF      (SGTSL508-IC-TAB)
                                       TO  MSGKW999-VL-VRV-MNT
               PERFORM  999003-ADICIONA-VRV-MNT

               PERFORM  999000-GRAVA-ERRO-E-ENCERRA

           END-IF

           IF       SGTSL508-PC-MIN-TAXA      (SGTSL508-IC-TAB)
                      LESS
                    SGTSL011-PC-TAXA-RMNC-OPR (SGTSL011-IC-TAB)
               AND  SGTSL508-CD-TIP-APL-PREF  (SGTSL508-IC-TAB)
                      EQUAL
                    SGTSL011-CD-TIP-FMA-APL   (SGTSL011-IC-TAB)
               AND  SGTSL508-CD-PERC-APL-PREF (SGTSL508-IC-TAB)
                      EQUAL
                    SGTSL011-CD-PERC-CPTZ-JUR (SGTSL011-IC-TAB)
               AND  SGTSL508-CD-IEC-PREF      (SGTSL508-IC-TAB)
                      EQUAL
                    SGTSL011-CD-IEC-RMNC      (SGTSL011-IC-TAB)

               SET  MOVE-SAIDA         TO  TRUE
      *        Taxa precificada menor que taxa informada na remuneração.
               MOVE  'Remuneração divergente da precificação.'
                                       TO  TX-DCR-DFRC
           END-IF

           IF  SGTSL508-CD-TIP-APL-PREF                (SGTSL508-IC-TAB)
                                           NOT EQUAL
               SGTSL011-CD-TIP-FMA-APL                 (SGTSL011-IC-TAB)
               SET   MOVE-SAIDA        TO  TRUE
      *        Código do tipo de forma de aplicação da precificação
      *        difere do tipo da remuneração.
               MOVE  'Remuneração divergente da precificação.'
                                       TO  TX-DCR-DFRC
           END-IF

           IF  SGTSL508-CD-PERC-APL-PREF               (SGTSL508-IC-TAB)
                                           NOT EQUAL
               SGTSL011-CD-PERC-CPTZ-JUR               (SGTSL011-IC-TAB)
               SET   MOVE-SAIDA        TO  TRUE
      *        Código da periodicidade da precificação difere da
      *        periodicidade da remunerção.
               MOVE  'Remuneração divergente da precificação.'
                                       TO  TX-DCR-DFRC
           END-IF

           IF  SGTSL508-CD-IEC-PREF(SGTSL508-IC-TAB)
                                           NOT EQUAL
               SGTSL011-CD-IEC-RMNC (SGTSL011-IC-TAB)
               SET   MOVE-SAIDA        TO  TRUE
      *        Código do indexador economico da precificação
      *        difere do código da remuneração.
               MOVE  'Remuneração divergente da precificação.'
                                       TO  TX-DCR-DFRC
           END-IF

           IF  MOVE-SAIDA
               IF  SGTS0185-LS-PREF
                   PERFORM  222100-CARREGA-SAIDA-PREF
               END-IF
               IF  SGTS0185-LS-RMNC
                   SET   CARREGA-SAIDA-RMNC
                                       TO TRUE
                END-IF
            END-IF

           .
       222000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       222100-CARREGA-SAIDA-PREF       SECTION.
      *-----------------------------------------------------------------
           MOVE  0008                  TO  MSG1-CD-SEQL-ERRO

           ADD   0001                  TO  IC-TAB

           IF  IC-TAB                      GREATER  0200
               SET   ERRO-PROGRAMADO   TO  TRUE
                    MOVE  0578         TO  MSG1-CD-ERRO-TS
                    MOVE  'Excedeu a quantidade máxima da tabela de saíd
      -                   'a.'         TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           MOVE  TX-DCR-DFRC
             TO  SGTS0185-TX-DCR-DFRC                           (IC-TAB)
           MOVE  CORR SGTSL508-DT-INC-CND-TAXA-R       (SGTSL508-IC-TAB)
             TO  SGTS0185-DT-INC-CND-TAXA-R                     (IC-TAB)

           MOVE  CORR SGTSL508-DT-FIM-CND-TAXA-R       (SGTSL508-IC-TAB)
             TO  SGTS0185-DT-FIM-CND-TAXA-R                     (IC-TAB)

           PERFORM  223100-FORMATAR-DESCR-TAXA

           MOVE  SGTS0136-TX-DCR-RMNC-OPR
             TO  SGTS0185-TX-DCR-TAXA-OPR                       (IC-TAB)

           .
       222100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       223000-CARREGA-SAIDA-RMNC       SECTION.
      *-----------------------------------------------------------------
           MOVE  0009                  TO  MSG1-CD-SEQL-ERRO

           ADD   0001                  TO  IC-TAB

           IF  IC-TAB                      GREATER  0200
               SET  ERRO-PROGRAMADO   TO  TRUE
                    MOVE  0578         TO  MSG1-CD-ERRO-TS
                    MOVE  'Excedeu a quantidade máxima da tabela de saíd
      -                   'a.'         TO  MSG1-TX-ERRO-TS
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           MOVE  'Precificação divergente da remuneração.'
             TO  SGTS0185-TX-DCR-DFRC                           (IC-TAB)
           MOVE  CORR SGTSL011-DT-INC-TAXA-RMNC-R      (SGTSL011-IC-TAB)
             TO  SGTS0185-DT-INC-CND-TAXA-R                     (IC-TAB)

           MOVE  CORR SGTSL011-DT-FIM-TAXA-RMNC-R      (SGTSL011-IC-TAB)
             TO  SGTS0185-DT-FIM-CND-TAXA-R                     (IC-TAB)

           PERFORM  223100-FORMATAR-DESCR-TAXA

           MOVE  SGTS0136-TX-DCR-RMNC-OPR
             TO  SGTS0185-TX-DCR-TAXA-OPR                       (IC-TAB)

           .
       223000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       223100-FORMATAR-DESCR-TAXA      SECTION.
      *-----------------------------------------------------------------
           MOVE  0010                  TO  MSG1-CD-SEQL-ERRO

           MOVE  SGTS0136              TO  NM-PGM

           INITIALIZE                      SGTS0136-DADOS

           IF  SGTS0185-LS-PREF
               MOVE  SGTSL508-PC-MIN-TAXA              (SGTSL508-IC-TAB)
                 TO  SGTS0136-PC-TAXA-RMNC-OPR
               MOVE  SGTSL508-CD-TIP-APL-PREF          (SGTSL508-IC-TAB)
                 TO  SGTS0136-CD-TIP-FMA-APL
               MOVE  SGTSL508-CD-IEC-PREF              (SGTSL508-IC-TAB)
                 TO  SGTS0136-CD-IEC-RMNC
           ELSE
              MOVE  SGTSL011-PC-TAXA-RMNC-OPR          (SGTSL011-IC-TAB)
                TO  SGTS0136-PC-TAXA-RMNC-OPR
              MOVE  SGTSL011-CD-TIP-FMA-APL            (SGTSL011-IC-TAB)
                TO  SGTS0136-CD-TIP-FMA-APL
              MOVE  SGTSL011-CD-IEC-RMNC               (SGTSL011-IC-TAB)
                TO  SGTS0136-CD-IEC-RMNC
           END-IF

           PERFORM  900000-EXECUTAR-MODULO

           .
       223100-SAI.
           EXIT.


      *-----------------------------------------------------------------
       300000-POSTAR-RESPOSTA          SECTION.
      *-----------------------------------------------------------------
           MOVE  0011                  TO  MSG1-CD-SEQL-ERRO

           EXEC CICS  PUT
                      CONTAINER ( 'SGTS0185-RPST'         )
                      CHANNEL   ( NM-CNL                  )
                      FLENGTH   ( LENGTH OF SGTS0185-RPST )
                      FROM      ( SGTS0185-RPST           )
           END-EXEC

           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *    Section da Faixa de 900000 a 969900 para demais rotinas
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       900000-EXECUTAR-MODULO          SECTION.
      *--> Executa um modulo implementado com CONTAINER
      *    Nao incluir nessa section os modulos chamados via COMMAREA
      *-----------------------------------------------------------------
           MOVE  0012                  TO  MSG1-CD-SEQL-ERRO

      *--> Grava o container de requisicao do modulo a executar
           PERFORM  982000-GRAVAR-CONTAINER-RQSC

      *--> Limpa a resposta e erro antes de executar o modulo
           PERFORM  983000-EXCLUIR-RESPOSTA-ERRO

      *--> Nao ha captura automatica de erro CICS por usar RESP e RESP2
           EVALUATE  NM-PGM
               WHEN  SGTSL011
                     EXEC CICS  LINK
                                PROGRAM ( SGTSL011   )
                                CHANNEL ( NM-CNL     )
                                RESP    ( MSG1-RESP1 )
                                RESP2   ( MSG1-RESP2 )
                     END-EXEC
               WHEN  SGTS0136
                     EXEC CICS  LINK
                                PROGRAM ( SGTS0136   )
                                CHANNEL ( NM-CNL     )
                                RESP    ( MSG1-RESP1 )
                                RESP2   ( MSG1-RESP2 )
                     END-EXEC
               WHEN  SGTSL508
                     EXEC CICS  LINK
                                PROGRAM ( SGTSL508   )
                                CHANNEL ( NM-CNL     )
                                RESP    ( MSG1-RESP1 )
                                RESP2   ( MSG1-RESP2 )
                     END-EXEC
               WHEN  OTHER
                     MOVE 9006         TO MSG1-CD-ERRO-TS
                     MOVE 'Não definida a execução do módulo.'
                                       TO MSG1-TX-ERRO-TS
                     PERFORM  999000-ERRO
           END-EVALUATE

           EVALUATE  TRUE
               WHEN  MSG1-RESP1        EQUAL DFHRESP(NORMAL)
                     CONTINUE
               WHEN  OTHER
                     MOVE 3004         TO MSG1-CD-ERRO-TS
                     MOVE 'Erro ao acionar o módulo.'
                                       TO MSG1-TX-ERRO-TS
                     SET ERRO-CICS     TO TRUE
      *              Monitorar variáveis do módulo chamado                     V
                     PERFORM  969000-MONITORAR-VARIAVEIS
                     PERFORM  999000-ERRO
           END-EVALUATE

      *--> Efetua a leitura do Container de Erro
           PERFORM  972000-LER-CONTAINER-DE-ERRO

      *--> Efetua a leitura do Container de Resposta
           PERFORM  973000-LER-RESPOSTA

           .
       900000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       969000-MONITORAR-VARIAVEIS      SECTION.
      *--> Monitora as variáveis dos módulos chamados
      *-----------------------------------------------------------------

           EVALUATE  NM-PGM
               WHEN  SGTSL011
                     MOVE  'SGTSL011-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL011-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSL011-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                     IF  SGTSL011-CD-OPR-ETRD-MCP  NUMERIC
                         MOVE  SGTSL011-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD
                         MOVE  CD-OPR-ETRD-TX
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSL011-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL011-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSL011-DT-INC-TAXA-ULT'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL011-DT-INC-TAXA-ULT
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  SGTS0136
                     MOVE  'SGTS0136-PC-TAXA-RMNC-OPR'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0136-PC-TAXA-RMNC-OPR
                                       TO  NRO-DEC-AUX
                     MOVE  NRO-DEC-ALFA
                                       TO  MSGKW999-NM-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTS0136-CD-TIP-FMA-APL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0136-CD-TIP-FMA-APL
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT
                     MOVE  'SGTS0136-CD-IEC-RMNC'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTS0136-CD-IEC-RMNC
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  SGTSL508
                     MOVE  'SGTSL508-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL508-CD-CTRA-TIT-VL
                                       TO  MSGKW999-NM-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSL508-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                     IF  SGTSL508-CD-OPR-ETRD-MCP  NUMERIC
                         MOVE  SGTSL508-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD
                         MOVE  CD-OPR-ETRD-TX
                                       TO  MSGKW999-VL-VRV-MNT
                     ELSE
                         MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
                     END-IF
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSL508-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL508-CD-OPR-ETRD-MCP
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSL508-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL508-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSL508-CD-TIP-TAXA'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL508-CD-TIP-TAXA
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSL508-DT-INC-PSQ'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL508-DT-INC-PSQ
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSL508-DT-FIM-PSQ'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL508-DT-FIM-PSQ
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

                     MOVE  'SGTSL508-DT-ULT-INC'
                                       TO  MSGKW999-NM-VRV-MNT
                     MOVE  SGTSL508-DT-ULT-INC
                                       TO  MSGKW999-VL-VRV-MNT
                     PERFORM  999003-ADICIONA-VRV-MNT

               WHEN  OTHER
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
           MOVE  0013                  TO  MSG1-CD-SEQL-ERRO

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CANAL            TO  TRUE

           EXEC CICS  ASSIGN
                      CHANNEL ( NM-CNL )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       970000-FIM.
           EXIT.

      *-----------------------------------------------------------------
       971000-LER-REQUISICAO           SECTION.
      *-----------------------------------------------------------------

      *--> Monta o nome do container de requisicao
           MOVE  'SGTS0185'            TO  NM-PGM
           MOVE  'SGTS0185-RQSC'       TO  NM-CTNR

      *--> Recupera o tamanho do container de requisição
           PERFORM  973100-LER-CONTAINER-NODATA

      *--> Obtem o tamanho esperado da requisicao
           MOVE  LENGTH OF SGTS0185-RQSC
                                       TO  QT-CCT-EPRO

      *--> Valida tamanho esperado da requisicao X tamanho recebido

           IF  QT-CCT-LIDO             NOT EQUAL QT-CCT-EPRO
               SET  ERRO-TAMANHO-AREA  TO  TRUE
               PERFORM  999000-GRAVA-ERRO-E-ENCERRA
           END-IF

           MOVE  0014                  TO  MSG1-CD-SEQL-ERRO

      *--> Efetua a leitura do container
           EXEC CICS  GET
                      CONTAINER ( NM-CTNR       )
                      FLENGTH   ( QT-CCT-LIDO   )
                      INTO      ( SGTS0185-RQSC )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE

           .
       971000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       972000-LER-CONTAINER-DE-ERRO    SECTION.
      *--> A presença do container PRM-ERRO indica que houve erro de
      *    integração, a ausência do container indica sucesso.
      *-----------------------------------------------------------------
           MOVE  0015                  TO  MSG1-CD-SEQL-ERRO

      *--> Le o container de erro dos modulos chamados
           SET   GET-CONTAINER         TO  TRUE
           MOVE  'PRM-ERRO'            TO  NM-CTNR
           INITIALIZE IIBKERRO-PRM-ERRO

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS  GET
                      CONTAINER ( NM-CTNR                     )
                      INTO      ( IIBKERRO-PRM-ERRO           )
                      FLENGTH   ( LENGTH OF IIBKERRO-PRM-ERRO )
                      RESP      ( MSG1-RESP1                  )
                      RESP2     ( MSG1-RESP2                  )
           END-EXEC

           EVALUATE  TRUE
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(CONTAINERERR)
                     CONTINUE
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(NORMAL)
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
      *              Monitorar variáveis do módulo chamado
                     PERFORM 969000-MONITORAR-VARIAVEIS
                     PERFORM  999000-ERRO
               WHEN  OTHER
                     SET   ERRO-CONTAINER
                                       TO  TRUE
                    PERFORM  999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       972000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       973000-LER-RESPOSTA             SECTION.
      *-----------------------------------------------------------------

      *--> Monta o nome dos container de acordo com modulo a executar
           MOVE  SPACES                TO  NM-CTNR

           MOVE  NM-PGM                TO  NM-CTNR(1:8)
           MOVE  '-RPST'               TO  NM-CTNR(9:5)

      *--> Recupera o tamanho do container de resposta
           PERFORM  973100-LER-CONTAINER-NODATA

           MOVE  0016                  TO  MSG1-CD-SEQL-ERRO

      *--> Le a RESPOSTA de acordo com o programa que a gravou
           EVALUATE  NM-PGM
      *-----------> Calcular a quantidade de caracteres esperado e
      *             limpar a area de dados antes de executar o GET
      *             Para matrizes com dados variaveis
      *             (clausula depending on)
      *             nao ha como utilizar o comando INITIALIZE
               WHEN  SGTSL011
                     MOVE  LENGTH OF SGTSL011-RPST
                                       TO  QT-CCT-EPRO
                     EXEC CICS  GET
                                CONTAINER ( NM-CTNR       )
                                INTO      ( SGTSL011-RPST )
                                FLENGTH   ( QT-CCT-LIDO   )
                     END-EXEC
               WHEN  SGTS0136
                     MOVE  LENGTH OF SGTS0136-RPST
                                       TO  QT-CCT-EPRO
                     EXEC CICS  GET
                                CONTAINER ( NM-CTNR       )
                                INTO      ( SGTS0136-RPST )
                                FLENGTH   ( QT-CCT-LIDO   )
                     END-EXEC
               WHEN  SGTSL508
                     MOVE  LENGTH OF SGTSL508-RPST
                                       TO  QT-CCT-EPRO
                     EXEC CICS  GET
                                CONTAINER ( NM-CTNR       )
                                INTO      ( SGTSL508-RPST )
                                FLENGTH   ( QT-CCT-LIDO   )
                     END-EXEC
               WHEN  OTHER
                     SET ERRO-PROGRAMADO
                                       TO TRUE
                     MOVE 9006         TO MSG1-CD-ERRO-TS
                     MOVE 'Não definida a execução do módulo.'
                                       TO MSG1-TX-ERRO-TS
                     PERFORM  999000-ERRO
           END-EVALUATE

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE

           .
       973000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       973100-LER-CONTAINER-NODATA     SECTION.
      *-----------------------------------------------------------------
           MOVE  0017                  TO  MSG1-CD-SEQL-ERRO

      *--> Configura dados para leitura do container de requisição
           SET   GET-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *--> Obtem a quantidade de caracteres no container
           MOVE  ZEROS                 TO  QT-CCT-LIDO

           EXEC CICS  GET
                      CONTAINER ( NM-CTNR     )
                      NODATA
                      FLENGTH   ( QT-CCT-LIDO )
           END-EXEC

           .
       973100-SAI.
           EXIT.

      *-----------------------------------------------------------------
      *    Section da Faixa de 980000 a 989900 para Gravacao de Dados
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       980000-GRAVAR-CONTAINER-RPST    SECTION.
      *-----------------------------------------------------------------
           MOVE  0018                  TO  MSG1-CD-SEQL-ERRO

      *--> Exclui eventual container de erro de outros programas
           PERFORM  983100-EXCLUIR-CONTAINER-ERRO

      *--> Monta o nome do container de resposta
           MOVE  'SGTS0185-RPST'       TO  NM-CTNR
           SET   PUT-CONTAINER         TO  TRUE

      *--> Por causa da captura automática de erro CICS, deve ser
      *    o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

           EXEC CICS  PUT
                      CONTAINER ( NM-CTNR                 )
                      FROM      ( SGTS0185-RPST           )
                      FLENGTH   ( LENGTH OF SGTS0185-RPST )
           END-EXEC

      *--> Ao chegar aqui, há certeza de que não houve erro algum
           SET   SEM-ERRO              TO  TRUE
           .
       980000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       982000-GRAVAR-CONTAINER-RQSC    SECTION.
      *-----------------------------------------------------------------
           MOVE  0019                  TO  MSG1-CD-SEQL-ERRO

      *--> Monta o nome dos container de acordo com modulo a executar
           MOVE  SPACES                TO  NM-CTNR
           SET   PUT-CONTAINER         TO  TRUE

           MOVE  NM-PGM                TO  NM-CTNR(1:8)
           MOVE  '-RQSC'               TO  NM-CTNR(9:5)


      *--> Por causa da captura automatica de erro CICS, deve ser
      *    previsto o erro de container
           SET   ERRO-CONTAINER        TO  TRUE

      *--> Faz PUT da requisicao do programa a executar
           EVALUATE  NM-PGM
               WHEN  SGTSL011
                     EXEC CICS  PUT
                                CONTAINER ( NM-CTNR                 )
                                FROM      ( SGTSL011-RQSC           )
                                FLENGTH   ( LENGTH OF SGTSL011-RQSC )
                     END-EXEC
               WHEN  SGTS0136
                     EXEC CICS  PUT
                                CONTAINER ( NM-CTNR                 )
                                FROM      ( SGTS0136-RQSC           )
                                FLENGTH   ( LENGTH OF SGTS0136-RQSC )
                     END-EXEC
               WHEN  SGTSL508
                     EXEC CICS  PUT
                                CONTAINER ( NM-CTNR                 )
                                FROM      ( SGTSL508-RQSC           )
                                FLENGTH   ( LENGTH OF SGTSL508-RQSC )
                     END-EXEC
               WHEN  OTHER
                     SET ERRO-PROGRAMADO
                                       TO TRUE
                     MOVE 9006         TO MSG1-CD-ERRO-TS
                     MOVE 'Não definida a execução do módulo.'
                                       TO MSG1-TX-ERRO-TS
                     PERFORM  999000-ERRO
           END-EVALUATE

      *--> Ao chegar aqui, ha certeza de que nao houve erro algum
           SET   SEM-ERRO              TO  TRUE

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
           MOVE  0020                  TO  MSG1-CD-SEQL-ERRO

           MOVE  NM-PGM                TO  NM-CTNR(1:8)
           MOVE  '-RPST'               TO  NM-CTNR(9:5)

           SET   DEL-CONTAINER         TO  TRUE

      *--> Exclui o container de resposta
      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS  DELETE
                      CONTAINER ( NM-CTNR    )
                      RESP      ( MSG1-RESP1 )
                      RESP2     ( MSG1-RESP2 )
           END-EXEC

           EVALUATE TRUE
               WHEN MSG1-RESP1  EQUAL  DFHRESP(CONTAINERERR)
               WHEN MSG1-RESP1  EQUAL  DFHRESP(NORMAL)
                    CONTINUE
               WHEN OTHER
                    SET ERRO-CONTAINER TO  TRUE
                    PERFORM  999002-TRATA-CONDITION-CICS
           END-EVALUATE

      *--> Excluir o container de erro
           PERFORM  983100-EXCLUIR-CONTAINER-ERRO
           .
       983000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       983100-EXCLUIR-CONTAINER-ERRO   SECTION.
      *-----------------------------------------------------------------
           MOVE  0021                  TO  MSG1-CD-SEQL-ERRO

      *--> Limpar variaveis container de erro
           INITIALIZE                      IIBKERRO-PRM-ERRO

      *--> Exclui o container de erro
           MOVE  'PRM-ERRO'            TO  NM-CTNR
           SET   DEL-CONTAINER         TO  TRUE

      *--> Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS  DELETE
                      CONTAINER ( NM-CTNR    )
                      RESP      ( MSG1-RESP1 )
                      RESP2     ( MSG1-RESP2 )
           END-EXEC

           EVALUATE  TRUE
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(CONTAINERERR)
               WHEN  MSG1-RESP1            EQUAL  DFHRESP(NORMAL)
                     CONTINUE
               WHEN  OTHER
                     SET ERRO-CONTAINER
                                       TO  TRUE
                     PERFORM  999002-TRATA-CONDITION-CICS
           END-EVALUATE
           .
       983100-SAI.
           EXIT.

      *-----------------------------------------------------------------
       984000-GRAVAR-CONTAINER-ERRO    SECTION.
      *-----------------------------------------------------------------

      *--> Grava o erro gerado no container de erro
      *    Não há captura automática de erro CICS por usar RESP e RESP2
           EXEC CICS  PUT
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
      *--> As sections abaixo iniciadas com 999000 representam os
      *    tratamentos de erro utilizando o Sistema MSG (MSGS0001)
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       999000-ERRO                     SECTION.
      *-----------------------------------------------------------------

           IF  MSG1-CD-ERRO-TS             EQUAL  ZEROS
               MOVE  8888              TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS             EQUAL  SPACES
               MOVE  'Erro não especificado no módulo.'
                                       TO  MSG1-TX-ERRO-TS
           END-IF

           PERFORM  999000-GRAVA-ERRO-E-ENCERRA
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

      *--> Adiciona as variáveis de entrada para monitoração
           MOVE  'SGTS0185-CD-CTRA-TIT-VL'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0185-CD-CTRA-TIT-VL
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTS0185-CD-OPR-ETRD-MCP'
                                       TO  MSGKW999-NM-VRV-MNT
           IF  SGTS0185-CD-OPR-ETRD-MCP    NUMERIC
               MOVE  SGTS0185-CD-OPR-ETRD-MCP
                                       TO  CD-OPR-ETRD
               MOVE  CD-OPR-ETRD-TX    TO  MSGKW999-VL-VRV-MNT
           ELSE
               MOVE  'Valor não numérico'
                                       TO  MSGKW999-VL-VRV-MNT
           END-IF
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTS0185-NR-VRS-OPR-ETRD'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0185-NR-VRS-OPR-ETRD
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT

           MOVE  'SGTS0185-CD-LS-RTN'
                                       TO  MSGKW999-NM-VRV-MNT
           MOVE  SGTS0185-CD-LS-RTN
                                       TO  MSGKW999-VL-VRV-MNT
           PERFORM  999003-ADICIONA-VRV-MNT


      *--> Grava o erro gerado no sistema MSG - Mensageria
           PERFORM  MSG1-NTF-MSG

           MOVE  MSG1-CD-ERRO-TS       TO  IIBKERRO-CD-ERRO
           MOVE  MSG1-TX-ERRO-TS       TO  IIBKERRO-TX-ERRO

      *--> Grava o erro gerado no container de erro
           PERFORM  984000-GRAVAR-CONTAINER-ERRO

      *--> Encerra a transacao
           PERFORM  999999-ENCERRAR
           .
       999000-SAI.
           EXIT.

      *----------------------------------------
      * 999001-TRATA-ABEND-CICS        SECTION.
      * 999002-TRATA-CONDITION-CICS    SECTION.
      * 999003-ADICIONA-VRV-MNT        SECTION.
      *----------------------------------------
       COPY HLPKP999.
      *----------------------------------------
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *----------------------------------------
       COPY SGTKP999.
      *----------------------------------------
      * MSG1-NTF-MSG                   SECTION.
      *----------------------------------------
       COPY MSGKPMG1.

      *-----------------------------------------------------------------
       999999-ENCERRAR                 SECTION.
      *-----------------------------------------------------------------

           EXEC CICS
                RETURN
           END-EXEC
           .
       999999-ENCERRAR-SAI.
           EXIT.
      *---------------------- FIM SGTS0185 -----------------------------
