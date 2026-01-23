000010*-----------------------------------------------------------------
000020* PROGRAMA..: SGTSP002
000030* ANALISTA..: F8608799 - RICARDO RAMOS
000040* AUTOR.....: F6631109 - MAGALI MARIKO OYAMADA
000050* OBJETIVO..: VERIFICAR EXISTENCIA DE CONTATO EXTERNO DA OPER.
000060*             ESTRUTURADA PARA PESSOA DE CONTATO EXTERNO
000070* COMPILACAO: COBOL/CICS/DB2
000080*-----------------------------------------------------------------
000090* VRS002 14.05.2019 - F0950413 - DE 2016/145195 - Adequação das
      *        das mensagens de erro ao padrão da mensageria.
000090* VRS001 10.11.2015 - F6631109 - DE 2015/120821 - IMPLANTACAO
000100*-----------------------------------------------------------------
000110 IDENTIFICATION DIVISION.
000120*-----------------------------------------------------------------
000130 PROGRAM-ID. SGTSP002.
000140
000150*-----------------------------------------------------------------
000160 ENVIRONMENT DIVISION.
000170*-----------------------------------------------------------------
000180 CONFIGURATION SECTION.
000190 SPECIAL-NAMES.
000200     DECIMAL-POINT IS COMMA.
000210
000220*-----------------------------------------------------------------
000230 DATA DIVISION.
000240*-----------------------------------------------------------------
000250
000260*-----------------------------------------------------------------
000270 WORKING-STORAGE SECTION.
000280*-----------------------------------------------------------------
000290 01  CTE-PROG     PIC  X(17) VALUE '*** SGTSP002 ***'.
000300 01  CTE-VERS     PIC  X(06) VALUE 'VRS002'.
000310
000320* Book do programa de ambiente - CICS5000
000330* Definido na work para preservar as informacoes
-INC HLPKE01B                                                           00000340
000350*-----------------------------------------------------------------
000360 LOCAL-STORAGE SECTION.
000370*-----------------------------------------------------------------
000380
      *--> Book do container de erro
-INC IIBKERRO                                                           00000390
      *--> Book para captura de erro via MSG1
-INC MSGKWMG1                                                           00000400
      *--> Variaveis para tratamento de Abend e Condition
-INC HLPKW999                                                           00000410
      *--> Variaveis para tratamento de CONTAINER
-INC SGTKERR1

      *--> Area de trabalho
000500 01  QT-REG-SELD                      PIC S9(4) COMP-5 VALUE 0.
000510 01  TS-SQL-ATU                       PIC  X(26) VALUE ' '.
000520
      *--> Indicadores de nulidades
000550 01  QT-REG-SELD-NULO                 PIC S9(4) COMP-5 VALUE 0.
000570
      *--> Máscaras edição de variáveis p/ monitoração mensagens de erro
       01  MCR-S9-15                   PIC      ---------------9.
000570
000580     EXEC SQL
000590         INCLUDE SQLCA
000600     END-EXEC.
000610
      *--> Book do programa
       01  SGTSP002-DADOS.
-INC SGTKP002                                                           00000640

000650*-----------------------------------------------------------------
000660 PROCEDURE DIVISION.
000670*-----------------------------------------------------------------
000680     IF NOT EXECUCAO-ONLINE AND NOT EXECUCAO-BATCH
000690       CALL CICS5000 USING CICS5000-COMMAREA
000700     END-IF
000710     MOVE 0001                   TO MSG1-CD-SEQL-ERRO
      *--> Habilita tratamento automatico de Erros DB2
-INC HLPKE997                                                           00000730
      *--> Habilita captura automatica de abend e erros CICS
-INC HLPKE999                                                           00000750
000760
000770     PERFORM 100000-DESCOBRIR-CANAL
000780     PERFORM 110000-EXCLUIR-CTNR-RPST
000790     PERFORM 120000-EXCLUIR-CTNR-ERRO
000800     PERFORM 130000-RECEBER-REQUISICAO
000810     PERFORM 140000-VALIDA-PARAMETRO
000820
000830     PERFORM 200000-CONSULTAR
000840     PERFORM 300000-POSTA-RESPOSTA
000850
000860     GOBACK
000870     .
000880*-----------------------------------------------------------------
000890 100000-DESCOBRIR-CANAL SECTION.
000900*-----------------------------------------------------------------
000910     MOVE 0002                   TO MSG1-CD-SEQL-ERRO
000920
000930     EXEC CICS ASSIGN
000940        CHANNEL ( NM-CNL )
000950     END-EXEC
000960     .
000970 100000-SAI.
000980     EXIT.
000990*-----------------------------------------------------------------
001000 110000-EXCLUIR-CTNR-RPST SECTION.
001010*-----------------------------------------------------------------
001020     MOVE 0003                   TO MSG1-CD-SEQL-ERRO
001030
001040     EXEC CICS DELETE
001050        CONTAINER ( 'SGTSP002-RPST' )
001060        CHANNEL   ( NM-CNL     )
001070        NOHANDLE
001080     END-EXEC
001090     IF  NOT (EIBRESP EQUAL DFHRESP(NORMAL)   OR
001100         EIBRESP EQUAL DFHRESP(CONTAINERERR) )
001110         PERFORM 999002-TRATA-CONDITION-CICS
001120     END-IF
001130     .
001140 110000-SAI.
001150     EXIT.
001160*-----------------------------------------------------------------
001170 120000-EXCLUIR-CTNR-ERRO SECTION.
001180*-----------------------------------------------------------------
001190     MOVE 0004                   TO MSG1-CD-SEQL-ERRO
001200
001210     EXEC CICS DELETE
001220        CONTAINER ( 'PRM-ERRO' )
001230        CHANNEL   ( NM-CNL     )
001240        NOHANDLE
001250     END-EXEC
001260     IF  NOT (EIBRESP EQUAL DFHRESP(NORMAL)   OR
001270         EIBRESP EQUAL DFHRESP(CONTAINERERR) )
001280         PERFORM 999002-TRATA-CONDITION-CICS
001290     END-IF
001300     .
001310 120000-SAI.
001320     EXIT.
001330*-----------------------------------------------------------------
001340 130000-RECEBER-REQUISICAO SECTION.
001350*-----------------------------------------------------------------
001360     MOVE 0005                   TO MSG1-CD-SEQL-ERRO
001370
001380     MOVE ZEROS                       TO QT-CCT-LIDO
001390     MOVE LENGTH OF SGTSP002-RQSC      TO QT-CCT-EPRO
001400     EXEC CICS GET
001410        CONTAINER ( 'SGTSP002-RQSC' )
001420        CHANNEL   ( NM-CNL     )
001430        NODATA
001440        FLENGTH ( QT-CCT-LIDO )
001450     END-EXEC
001460
           IF  QT-CCT-LIDO NOT EQUAL QT-CCT-EPRO
               SET   ERRO-TAMANHO-AREA TO TRUE
               PERFORM 999000-GRAVA-ERRO-E-ENCERRA
           END-IF

001610     EXEC CICS GET
001620        CONTAINER ( 'SGTSP002-RQSC' )
001630        CHANNEL   ( NM-CNL     )
001640        FLENGTH ( LENGTH OF SGTSP002-RQSC )
001650        INTO    ( SGTSP002-RQSC )
001660     END-EXEC
001670     .
001680 130000-SAI.
001690     EXIT.
001700*-----------------------------------------------------------------
001710 140000-VALIDA-PARAMETRO SECTION.
001720*-----------------------------------------------------------------
001730     MOVE 0006                   TO MSG1-CD-SEQL-ERRO
001740
001750     PERFORM 150000-VALIDA-CAMPOS
001760
001770     .
001780 140000-SAI.
001790     EXIT.
001800*-----------------------------------------------------------------
001810 150000-VALIDA-CAMPOS SECTION.
001820*-----------------------------------------------------------------
001830     MOVE 0007                   TO MSG1-CD-SEQL-ERRO
001840     IF  SGTSP002-CD-PSS-CTT-EXNO <= ZEROS
001850         SET  ERRO-PROGRAMADO   TO TRUE
001860         MOVE 0059              TO  MSG1-CD-ERRO-TS
001880         MOVE 'Código pessoa contato externo inválido.'
001890                                   TO MSG1-TX-ERRO-TS
001900         PERFORM 999000-GRAVA-ERRO-E-ENCERRA
001910     END-IF
001920     .
001930 150000-SAI.
001940     EXIT.
001950*-----------------------------------------------------------------
001960 200000-CONSULTAR SECTION.
001970*-----------------------------------------------------------------
001980     MOVE 0008                   TO MSG1-CD-SEQL-ERRO
001990
002000     INITIALIZE  QT-REG-SELD
002010
002020     EXEC SQL
002030           SELECT 1
002040             INTO :QT-REG-SELD
002050                 FROM DB2SGT.CTT_EXNO_OPR_ETRD A
002060          WHERE A.CD_PSS_CTT_EXNO  = : SGTSP002-CD-PSS-CTT-EXNO
002070             FETCH FIRST 1 ROWS ONLY
002080     END-EXEC
002090
002100     IF  SQLCODE = +100
002110         CONTINUE
002120     END-IF
002130
002140     IF QT-REG-SELD   EQUAL 1
002150         MOVE 'S'               TO  SGTSP002-IN-EXT-CTT-EXNO-OPR
002160     ELSE
002170         MOVE 'N'               TO  SGTSP002-IN-EXT-CTT-EXNO-OPR
002180     END-IF
002190
002200     .
002210 200000-SAI.
002220     EXIT.
002230*-----------------------------------------------------------------
002240 300000-POSTA-RESPOSTA SECTION.
002250*-----------------------------------------------------------------
002260     MOVE 0009                   TO MSG1-CD-SEQL-ERRO
002270     EXEC CICS PUT
002280        CONTAINER ( 'SGTSP002-RPST' )
002290        CHANNEL   ( NM-CNL     )
002300        FLENGTH ( LENGTH OF SGTSP002-RPST )
002310        FROM    ( SGTSP002-RPST )
002320     END-EXEC
002330     .
002340 300000-SAI.
002350     EXIT.
002360*-----------------------------------------------------------------
002370 999000-GRAVA-ERRO-E-ENCERRA SECTION.
002380*-----------------------------------------------------------------
           IF  MSG1-CD-ERRO-TS         EQUAL  ZEROS
              MOVE  8888               TO  MSG1-CD-ERRO-TS
           END-IF

           IF  MSG1-TX-ERRO-TS         EQUAL  SPACES
               MOVE  'Erro não especificado no módulo.'
                                       TO  MSG1-TX-ERRO-TS
           END-IF

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

           MOVE 'SGTSP002-CD-PSS-CTT-EXNO'   TO MSGKW999-NM-VRV-MNT
           MOVE SGTSP002-CD-PSS-CTT-EXNO     TO  MCR-S9-15
           MOVE  MCR-S9-15                   TO MSGKW999-VL-VRV-MNT
           PERFORM 999003-ADICIONA-VRV-MNT

002430* --> grava o erro no msg
002440     PERFORM MSG1-NTF-MSG
002450     MOVE MSG1-CD-ERRO-TS             TO IIBKERRO-CD-ERRO
002460     MOVE MSG1-TX-ERRO-TS             TO IIBKERRO-TX-ERRO
002470     EXEC CICS PUT
002480        CONTAINER ( 'PRM-ERRO' )
002490        CHANNEL   ( NM-CNL     )
002500        FROM      ( IIBKERRO-PRM-ERRO)
002510        FLENGTH ( LENGTH OF IIBKERRO-PRM-ERRO )
002520        NOHANDLE
002530     END-EXEC
002540
002550     PERFORM 999999-ENCERRAR
002560     .
002570 999000-SAI.
002580     EXIT.

      *---------------------------------------*
      * 999001-TRATA-ABEND-CICS        SECTION.
      * 999002-TRATA-CONDITION-CICS    SECTION.
      * 999003-ADICIONA-VRV-MNT        SECTION.
      *---------------------------------------*
-INC HLPKP999                                                           00002640

      *---------------------------------------*
      * 999004-TRATA-ERRO-CTNR         SECTION.
      * 999005-TRATA-ERRO-AREA-DADOS   SECTION.
      * 999006-TRATA-ERRO-CANAL        SECTION.
      *---------------------------------------*
-INC SGTKP999

      *---------------------------------------*
      * 999010-TRATA-ERRO-DB2          SECTION.                         00002670
      *---------------------------------------*
-INC HLPKP997

      *---------------------------------------*
      * MSG1-NTF-MSG                   SECTION.
      *---------------------------------------*                         00002700
-INC MSGKPMG1

      *---------------------------------------*
       999999-ENCERRAR                SECTION.
      *---------------------------------------*
           EXEC CICS
                RETURN
           END-EXEC.

      *---------------------- FIM SGTSP002 ----------------------------