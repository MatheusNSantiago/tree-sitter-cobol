       CBL ARITH(EXTEND)
000760*-----------------------------------------------------------------
000760* PROGRAMA..: SGTSL116
000760* ANALISTA..: F8785286 Roseli Akemi Kawahata
000760* AUTOR.....: F8785286 Roseli Akemi Kawahata
000760* OBJETIVO..: Listar motivo de alteração do estado da operação
000760*               DB2SGT.MTV_ALT.
000760* COMPILACAO: COBOL/CICS/DB2
000760*-----------------------------------------------------------------
000760* VRS001 27.07.2018 - F8785286 DE 2016/145195 - IMPLANTACAO
000760*-----------------------------------------------------------------
000760 IDENTIFICATION DIVISION.
000760*-----------------------------------------------------------------
000760 PROGRAM-ID. SGTSL116.
000760*-----------------------------------------------------------------
000760 ENVIRONMENT DIVISION.
000760*-----------------------------------------------------------------
000760 CONFIGURATION SECTION.
000760 SPECIAL-NAMES.
000760     DECIMAL-POINT IS COMMA.
000760*-----------------------------------------------------------------
000760 DATA DIVISION.
000760*-----------------------------------------------------------------
000760 WORKING-STORAGE SECTION.
000760*-----------------------------------------------------------------
000760 01  CTE-PROG     PIC  X(17) VALUE '*** SGTSL116 ***'.
000760 01  CTE-VERS     PIC  X(06) VALUE 'VRS001'.
000760*-----------------------------------------------------------------
000760* Book do programa de ambiente - CICS5000
000760* Definido na work para preservar as informacoes
-INC HLPKE01B
000760 LOCAL-STORAGE SECTION.
000760*-----------------------------------------------------------------
000760* book de para acionamento do msg1
-INC IIBKERRO
-INC MSGKWMG1
-INC HLPKW999
-INC SGTKERR1
000760
000760 01  GRP-CONTAINER.
000760     03  CANAL                   PIC X(16) VALUE SPACES.
000760
000760* Area de trabalho
000760 01 QT-REG-SELD                  PIC S9(4) COMP-5 VALUE 0.
000760
000760* Campos utilizados para leitura
000760 01  REG-WS.
000760     03 CD-TIP-EST-OPR           PIC S9(9) COMP-5
000760        OCCURS 101 TIMES         VALUE 0.
000760     03 CD-MTV-ALT               PIC S9(4) COMP-5
000760        OCCURS 101 TIMES         VALUE 0.
000760     03 TX-EST                   PIC X(020)
000760        OCCURS 101 TIMES         VALUE SPACES.
000760     03 TX-DCR-MTV-ALT           PIC X(200)
000760        OCCURS 101 TIMES         VALUE SPACES.
000760
000760* Indicadores de nulidades
000760 01 QT-REG-SELD-N                PIC S9(4) COMP-5 VALUE 0.
000760
000760* Variáveis auxiliares
000760
000760* Variavel(is) de controle do cursor(es)
000760 01  CTL-CURSOR-PSQ.
000760     03  CD-FIM-CURSOR-PSQ       PIC S9(04) COMP-5 VALUE 0.
000760         88  FIM-CURSOR-PSQ      VALUE +100.
000760     03  IN-CURSOR-PSQ-ABTO      PIC  X(01) VALUE 'N'.
000760         88  CURSOR-PSQ-ABTO     VALUE 'S'.
000760
000760* Subrotinas utilizadas
000760     EXEC SQL
000760         INCLUDE SQLCA
000760     END-EXEC.
000760
000760*-----------------------------------------------------------------
000410*--> Pesquisa Característica da Carteira mais recente
000410*-----------------------------------------------------------------
-INC SGTKL116
000760     EXEC SQL
000760       DECLARE CURSOR-PSQ-1 CURSOR
000760               WITH ROWSET POSITIONING FOR
000760         SELECT
000760             A.CD_TIP_EST_OPR
000760           , A.CD_MTV_ALT
000760           , A.TX_EST
000760           , B.TX_DCR_MTV_ALT
000760
000760         FROM        DB2SGT.EST_MTV_ALT   A
000760         INNER JOIN  DB2SGT.MTV_ALT       B
000760         ON A.CD_MTV_ALT  =  B.CD_MTV_ALT
000760         WHERE A.CD_TIP_EST_OPR    = :SGTSL116-E-CD-TIP-EST-OPR
300760      ORDER BY  A.CD_MTV_ALT
000760     END-EXEC.
000760
000760*-----------------------------------------------------------------
000760 PROCEDURE DIVISION.
000760*-----------------------------------------------------------------
000760
000760* Habilita tratamento automatico de erros DB2
-INC HLPKE997
000760* Habilita captura automatica de abend e erros CICS
-INC HLPKE999
000760
000760     PERFORM 100000-DESCOBRIR-CANAL
000760     PERFORM 110000-EXCLUIR-CTNR-RPST
000760     PERFORM 120000-EXCLUIR-CTNR-ERRO
000760     PERFORM 130000-RECEBER-REQUISICAO
000760     PERFORM 140000-VALIDAR-PARAMETRO
000760
000760     PERFORM 970101-ABRE-CURSOR-PSQ
000760     PERFORM 970102-LER-CURSOR-PSQ
000760     PERFORM 970103-FECHA-CURSOR-PSQ
000760
000760     PERFORM 200000-PROCESSA-PESQUISA
000760              VARYING SGTSL116-IC-TAB FROM 1 BY 1
000760              UNTIL SGTSL116-IC-TAB > SGTSL116-QT-OCR-TAB
000760
000760     PERFORM 300000-POSTA-RESPOSTA
000760
000760     GOBACK
000760     .
000760*-----------------------------------------------------------------
000760 100000-DESCOBRIR-CANAL          SECTION.
000760*-----------------------------------------------------------------
000760     MOVE 0001                   TO  MSG1-CD-SEQL-ERRO
000760
000760*Por causa da captura automática de erro CICS, deve ser previsto
000760*o erro de container
000760     SET  ERRO-CANAL             TO  TRUE
000760
000760     EXEC CICS ASSIGN
000760        CHANNEL ( CANAL )
000760     END-EXEC
000760
000760*    Ao chegar aqui, há certeza de que não houve erro algum
000760     SET  SEM-ERRO               TO  TRUE
000760     .
000760 100000-SAI.
000760     EXIT.
000760*-----------------------------------------------------------------
000760 110000-EXCLUIR-CTNR-RPST        SECTION.
000760*-----------------------------------------------------------------
000760     MOVE 0002                   TO  MSG1-CD-SEQL-ERRO
000760
000760     EXEC CICS DELETE
000760        CONTAINER ( 'SGTSL116-RPST' )
000760        CHANNEL   ( CANAL      )
000760        NOHANDLE
000760     END-EXEC
000760
000760     IF  NOT (EIBRESP EQUAL DFHRESP(NORMAL)   OR
000760         EIBRESP EQUAL DFHRESP(CONTAINERERR) )
000760         PERFORM 999002-TRATA-CONDITION-CICS
000760     END-IF
000760     .
000760 110000-SAI.
000760     EXIT.
000760*-----------------------------------------------------------------
000760 120000-EXCLUIR-CTNR-ERRO        SECTION.
000760*-----------------------------------------------------------------
000760     MOVE 0003                   TO MSG1-CD-SEQL-ERRO
000760
000760     EXEC CICS DELETE
000760        CONTAINER ( 'PRM-ERRO' )
000760        CHANNEL   ( CANAL      )
000760        NOHANDLE
000760     END-EXEC
000760
000760     IF  NOT (EIBRESP EQUAL DFHRESP(NORMAL)   OR
000760         EIBRESP EQUAL DFHRESP(CONTAINERERR) )
000760         PERFORM 999002-TRATA-CONDITION-CICS
000760     END-IF
000760     .
000760 120000-SAI.
000760     EXIT.
000760*-----------------------------------------------------------------
000760 130000-RECEBER-REQUISICAO       SECTION.
000760*-----------------------------------------------------------------
000760     MOVE 0004                   TO  MSG1-CD-SEQL-ERRO
000760
006150     MOVE 'SGTSL116'             TO  NM-PGM
006160     MOVE 'SGTSL116-RQSC'        TO  NM-CTNR
000760
000760     MOVE ZEROS                  TO  QT-CCT-LIDO
000760
000760     MOVE LENGTH OF SGTSL116-RQSC    TO  QT-CCT-EPRO
000760     EXEC CICS GET
000760        CONTAINER ( 'SGTSL116-RQSC' )
000760        CHANNEL   ( CANAL      )
000760        NODATA
000760        FLENGTH ( QT-CCT-LIDO )
000760     END-EXEC
000760
000760     IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
000760         SET   ERRO-TAMANHO-AREA   TO  TRUE
000760         PERFORM 999000-GRAVA-ERRO-E-ENCERRA
000760     END-IF
000760
000760     EXEC CICS GET
000760        CONTAINER ( 'SGTSL116-RQSC' )
000760        CHANNEL   ( CANAL      )
000760        FLENGTH ( LENGTH OF SGTSL116-RQSC )
000760        INTO    ( SGTSL116-RQSC )
000760     END-EXEC
000760     .
000760 130000-SAI.
000760     EXIT.
000760*-----------------------------------------------------------------
000760 140000-VALIDAR-PARAMETRO        SECTION.
000760*-----------------------------------------------------------------
000760     MOVE 0005                   TO  MSG1-CD-SEQL-ERRO
000760
000760     MOVE 'N'                    TO  SGTSL116-IN-CTNC-PSQ
000760
000760     IF  SGTSL116-CD-TIP-SRVC        EQUAL ZEROS   OR
000760         SGTSL116-CD-TIP-SRVC        GREATER 1
000760         SET  ERRO-PROGRAMADO        TO  TRUE
000760         MOVE 107                    TO  MSG1-CD-ERRO-TS
000760         MOVE 'Código do tipo de serviço inválido.'
000760                                     TO  MSG1-TX-ERRO-TS
000760         PERFORM 999000-GRAVA-ERRO-E-ENCERRA
000760     END-IF
000760
000760     IF  SGTSL116-MTV-PARA-EST       AND
000760         SGTSL116-E-CD-TIP-EST-OPR   EQUAL ZEROES
000760         SET  ERRO-PROGRAMADO        TO  TRUE
000760         MOVE 099                    TO  MSG1-CD-ERRO-TS
000760         MOVE 'Código do tipo de estado da operação inválido.'
000760                                     TO  MSG1-TX-ERRO-TS
000760         PERFORM 999000-GRAVA-ERRO-E-ENCERRA
000760     END-IF
000760     .
000760 140000-SAI.
000760     EXIT.
000760*-----------------------------------------------------------------
000760 200000-PROCESSA-PESQUISA        SECTION.
000760*-----------------------------------------------------------------
000760     MOVE 0006                   TO  MSG1-CD-SEQL-ERRO
000760
000760     MOVE CD-TIP-EST-OPR(SGTSL116-IC-TAB)  TO
000760                       SGTSL116-CD-TIP-EST-OPR(SGTSL116-IC-TAB)
000760
000760     MOVE CD-MTV-ALT(SGTSL116-IC-TAB) TO
000760                       SGTSL116-CD-MTV-ALT(SGTSL116-IC-TAB)
000760
000760     MOVE TX-EST(SGTSL116-IC-TAB)  TO
000760                       SGTSL116-TX-EST(SGTSL116-IC-TAB)
000760
000760     MOVE TX-DCR-MTV-ALT(SGTSL116-IC-TAB)  TO
000760                       SGTSL116-TX-DCR-MTV-ALT(SGTSL116-IC-TAB)
000760     .
000760 200000-SAI.
000760     EXIT.
000760*-----------------------------------------------------------------
000760 300000-POSTA-RESPOSTA           SECTION.
000760*-----------------------------------------------------------------
000760     MOVE 0007                   TO  MSG1-CD-SEQL-ERRO
000760
000760     EXEC CICS PUT
000760        CONTAINER ( 'SGTSL116-RPST' )
000760        CHANNEL   ( CANAL      )
000760        FLENGTH ( LENGTH OF SGTSL116-RPST )
000760        FROM    ( SGTSL116-RPST )
000760     END-EXEC
000760     .
000760 300000-SAI.
000760     EXIT.
000760*-----------------------------------------------------------------
000760 970101-ABRE-CURSOR-PSQ          SECTION.
000760*-----------------------------------------------------------------
000760     MOVE 0008                   TO  MSG1-CD-SEQL-ERRO
000760
000760     EVALUATE  TRUE
000760         WHEN  SGTSL116-MTV-PARA-EST
000760               EXEC SQL
000760                    OPEN CURSOR-PSQ-1
000760               END-EXEC
000760     END-EVALUATE
000760
000760     MOVE 'S'                    TO  IN-CURSOR-PSQ-ABTO
000760     .
000760 970101-SAI.
000760     EXIT.
000760*-----------------------------------------------------------------
000760 970102-LER-CURSOR-PSQ           SECTION.
000760*-----------------------------------------------------------------
000760     MOVE 0009                   TO  MSG1-CD-SEQL-ERRO
000760
000760     INITIALIZE REG-WS
000760
000760     EVALUATE  TRUE
000760         WHEN  SGTSL116-MTV-PARA-EST
000760            EXEC SQL
000760              FETCH NEXT ROWSET
000760                   CURSOR-PSQ-1 FOR 0101 ROWS
000760              INTO :CD-TIP-EST-OPR
000760                 , :CD-MTV-ALT
000760                 , :TX-EST
000760                 , :TX-DCR-MTV-ALT
000760            END-EXEC
000760
000760     END-EVALUATE
000760
000760     MOVE SQLERRD(3)             TO  QT-REG-SELD
000760
000760     IF  QT-REG-SELD EQUAL 0101
000760         SET SGTSL116-TEM-MAIS-REGISTRO TO TRUE
000760         MOVE 0100               TO  QT-REG-SELD
000760     END-IF
000760
000760     MOVE QT-REG-SELD            TO  SGTSL116-QT-OCR-TAB
000760     MOVE SQLCODE                TO  CD-FIM-CURSOR-PSQ
000760     .
000760 970102-SAI.
000760     EXIT.
000760*-----------------------------------------------------------------
000760 970103-FECHA-CURSOR-PSQ         SECTION.
000760*-----------------------------------------------------------------
000760     MOVE 0010                   TO  MSG1-CD-SEQL-ERRO
000760
000760     MOVE 'N'                    TO  IN-CURSOR-PSQ-ABTO
000760
000760     EVALUATE  TRUE
000760         WHEN  SGTSL116-MTV-PARA-EST
000760               EXEC SQL
000760               CLOSE CURSOR-PSQ-1
000760               END-EXEC
000760     END-EVALUATE
000760     .
000760 970103-SAI.
000760     EXIT.
000760*-----------------------------------------------------------------
000760 999000-GRAVA-ERRO-E-ENCERRA     SECTION.
000760*-----------------------------------------------------------------
000760
000760     IF MSG1-CD-ERRO-TS EQUAL ZEROS
000760        MOVE 8888                TO MSG1-CD-ERRO-TS
000760     END-IF
000760
000760     IF MSG1-TX-ERRO-TS EQUAL SPACES
000760        MOVE 'Erro não especificado no módulo.' TO MSG1-TX-ERRO-TS
000760     END-IF
000760
000760*Verifica se o tipo de erro gerado possui tratamento interno
000760     EVALUATE TRUE
000760         WHEN ERRO-CONTAINER
000760              PERFORM 999004-TRATA-ERRO-CTNR
000760         WHEN ERRO-TAMANHO-AREA
000760              PERFORM 999005-TRATA-ERRO-AREA-DADOS
000760         WHEN ERRO-CANAL
000760              PERFORM 999006-TRATA-ERRO-CANAL
000760         WHEN OTHER
000760              CONTINUE
000760     END-EVALUATE
000760
000760     MOVE 'SGTSL116-CD-TIP-SRVC'         TO  MSGKW999-NM-VRV-MNT
000760     MOVE SGTSL116-CD-TIP-SRVC           TO  MSGKW999-VL-VRV-MNT
000760     PERFORM 999003-ADICIONA-VRV-MNT
000760
000760     MOVE 'SGTSL116-E-CD-TIP-EST-OPR'    TO  MSGKW999-NM-VRV-MNT
000760     MOVE SGTSL116-E-CD-TIP-EST-OPR      TO  MSGKW999-VL-VRV-MNT
000760     PERFORM 999003-ADICIONA-VRV-MNT
000760
000760     MOVE 'SGTSL116-E-CD-MTV-ALT'        TO  MSGKW999-NM-VRV-MNT
000760     MOVE SGTSL116-E-CD-MTV-ALT          TO  MSGKW999-VL-VRV-MNT
000760     PERFORM 999003-ADICIONA-VRV-MNT
000760
000760* --> grava o erro no msg
000760     PERFORM MSG1-NTF-MSG
000760     MOVE MSG1-CD-ERRO-TS                TO  IIBKERRO-CD-ERRO
000760     MOVE MSG1-TX-ERRO-TS                TO  IIBKERRO-TX-ERRO
000760
000760     EXEC CICS PUT
000760        CONTAINER ( 'PRM-ERRO' )
000760        CHANNEL   ( CANAL      )
000760        FROM      ( IIBKERRO-PRM-ERRO)
000760        FLENGTH ( LENGTH OF IIBKERRO-PRM-ERRO )
000760        NOHANDLE
000760     END-EXEC
000760
000760     PERFORM 999999-ENCERRAR
000760     .
000760 999000-SAI.
000760     EXIT.
000760* --------------------------------------*
000760* 999001-TRATA-ABEND-CICS        SECTION.
000760* 999002-TRATA-CONDITION-CICS    SECTION.
000760* 999003-ADICIONA-VRV-MNT        SECTION.
000760* --------------------------------------*
-INC HLPKP999
000760* --------------------------------------*
000760* 999010-TRATA-ERRO-DB2          SECTION.
-INC HLPKP997
000760* --------------------------------------*
000760* MSG1-NTF-MSG                   SECTION.
-INC MSGKPMG1
000760* --------------------------------------*
000760* 999004-TRATA-ERRO-CTNR         SECTION.
000760* 999005-TRATA-ERRO-AREA-DADOS   SECTION.
000760* 999006-TRATA-ERRO-CANAL        SECTION.
000760*---------------------------------------*
-INC SGTKP999
000760* --------------------------------------*
000760 999999-ENCERRAR                SECTION.
000760* --------------------------------------*
000760     EXEC CICS
000760          RETURN
000760     END-EXEC.