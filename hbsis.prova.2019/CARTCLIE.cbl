       IDENTIFICATION DIVISION.
       
       PROGRAM-ID. CARTCLIE as "CARTCLIE".
       AUTHOR. ROBSON LIMA DA SILVA
      *----------------------------------------------------------------* 
      *    PROGRAMA.....: CARTCLIE                                     *
      *    ANALISTA.....: ROBSON LIMA                                  *
      *    DATA.........: 04/03/2019                                   *
      *----------------------------------------------------------------*
      *    OBJETIVO.....: ATENDER ESPECIFICACAO DE PRAVA HBSIS2019     *
      *----------------------------------------------------------------*
      *    ARQUIVOS.....:                                              *
      *                                                                *
      *                                          INCLUDE/BOOK          *
      *                                            CLIENTE             *
      *                                           VENDEDOR             *
      *                                            CNPJCPF             *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
        
       CONFIGURATION SECTION.
       
       INPUT-OUTPUT SECTION.
       
       FILE-CONTROL.
       
            SELECT ARQ-CLIENTE ASSIGN   TO DISK
                   ORGANIZATION         IS INDEXED 
                   ACCESS MODE          IS DYNAMIC
                   RECORD KEY           IS FD-COD-CLIENTE
                   ALTERNATE RECORD KEY IS FD-CNPJ
                   LOCK MODE            IS MANUAL
                   FILE STATUS          IS WS-FS-ACESSO-CLI.   
                   
            SELECT ARQ-VENDEDOR ASSIGN  TO DISK
                   ORGANIZATION         IS INDEXED 
                   ACCESS MODE          IS DYNAMIC
                   RECORD KEY           IS FD-COD-VENDEDOR
                   ALTERNATE RECORD KEY IS FD-CPF
                   LOCK MODE            IS MANUAL
                   FILE STATUS          IS WS-FS-ACESSO-VEN.
         
            SELECT ARQ-CSV      ASSIGN  TO DISK
                   FILE STATUS          IS WS-FS-ACESSO-CSV.
                   
            SELECT  ARQ-IMPORT-CLI ASSIGN  
                                        TO WS-END-ARQUI,
                    ORGANIZATION        IS LINE SEQUENTIAL, 
                    ACCESS MODE         IS SEQUENTIAL,
                    LOCK MODE           IS MANUAL,
                    FILE STATUS         IS WS-FS-IMPORT-CLI.
                    
            SELECT  ARQ-IMPORT-VEN ASSIGN  
                                        TO WS-END-ARQUI,
                    ORGANIZATION        IS LINE SEQUENTIAL, 
                    ACCESS MODE         IS SEQUENTIAL,
                    LOCK MODE           IS MANUAL,
                    FILE STATUS         IS WS-FS-IMPORT-CLI.                    
                   
       DATA DIVISION.
       
       FD ARQ-CLIENTE VALUE OF FILE-ID IS 'CLIENTE'. 
       01 REG-CLIENTE.
          COPY 'CLIENTE'.
          
       FD ARQ-VENDEDOR VALUE OF FILE-ID IS 'VENDEDOR'. 
       01 REG-VENDEDOR.
          COPY 'VENDEDOR'.     
          
       FD ARQ-CSV
          RECORDING MODE IS F
          LABEL RECORD   IS STANDARD
          BLOCK CONTAINS  0 RECORDS.
       01 FD-REG-CSV                   PIC X(100).                      
                      
       FD ARQ-IMPORT-CLI
          RECORDING MODE               IS F
          LABEL RECORD                 IS STANDARD
          BLOCK CONTAINS 0 RECORDS.
       01 FD-ARQ-IMPORT                PIC X(085).    
       
       FD ARQ-IMPORT-VEN
          RECORDING MODE               IS F
          LABEL RECORD                 IS STANDARD
          BLOCK CONTAINS 0 RECORDS.
       01 FD-ARQ-IMPORT                PIC X(082).          
       
       WORKING-STORAGE SECTION.
       
       01 WS-DATA-SISTEMA              PIC X(08) VALUE SPACES.
       
       01 WS-OPCAO                     PIC 9(01) VALUE ZEROS.
       01 WS-LINHA-BRANCO              PIC X(78) VALUE SPACES.   
          
       01 AREA-FILE-STATUS.
         05 WS-FS-ACESSO-CLI           PIC X(02) VALUE SPACES.
         05 WS-FS-ACESSO-VEN           PIC X(02) VALUE SPACES.
         05 WS-FS-ACESSO-CSV           PIC X(02) VALUE SPACES.
         05 WS-FS-IMPORT-CLI            PIC X(02) VALUE SPACES.      
         
       01 WS-AREA-IMPORT-CLI.
          05 WS-COD-CLIENTE               PIC  9(07)       VALUE ZEROS.
          05 WS-CNPJ                      PIC  9(14)       VALUE ZEROS.
          05 WS-RAZAO-SOCIAL              PIC  X(40)       VALUE SPACES.
          05 WS-LATITUDE-CLI              PIC +9(03)V9(08) VALUE ZEROS.
          05 WS-LONGITUDE-CLI             PIC +9(03)V9(08) VALUE ZEROS.


       01 WS-ACHOU                     PIC 9(01) VALUES ZEROS.  
       
       01 WS-LATI-LONG-MASC            PIC +ZZZ,ZZZZZZZ9.
       
       01 WS-DEL-ALT-CLIENTE           PIC X(01) VALUE SPACES.  
        
       01 WS-END-ARQUI                 PIC X(40) VALUE SPACES. 
             
       01 WS-FIM-ARQ-IMPORT-CLI        PIC X(01) VALUE SPACES.
       
       01 WS-ARQ-ABERTOS.
          05 WS-ARQ-CLIENTE-ABERTO    PIC X(01) VALUE SPACES.
          05 WS-ARQ-VENDEDOR-ABERTO   PIC X(01) VALUE SPACES.
          05 WS-ARQ-CSV-ABERTO        PIC X(01) VALUE SPACES.
       
       01 ACU-GRAVADOS                 PIC 9(09) COMP-3 VALUE ZEROS. 
       
       01 WS-OPERACAO                  PIC X(13) VALUE SPACES.
       
       01 WS-MSG-FILE-STATUS.
          05 WS-ABERTURA               PIC X(13) VALUE 'ABERTURA'.
          05 WS-LEITURA                PIC X(13) VALUE 'LEITURA'.
          05 WS-GRAVACAO               PIC X(13) VALUE 'GRAVACAO'.
          05 WS-FECHAMENTO             PIC X(13) VALUE 'FECHAMENTO'.
       
       01 LKS-PARM.
          COPY 'VALIDCPF'.
      * 
       LINKAGE SECTION.
      * 
       SCREEN SECTION.
       
       01 SS-TELA-PRINCIPAL.
          05 BLANK SCREEN
             BACKGROUND-COLOR 0
             FOREGROUND-COLOR 10.
          05 SS-TRACO                  PIC X(080) VALUE ALL '-'
                                       LINE 1 COL 1.
          05 SS-TITULO                 PIC X(040) VALUE 
             "EMPRESA INSTALACOES S.A."
                                       LINE 2 COL 5.         
          05 SS-DATA-FORMAT. 
             10 SS-DIA                 PIC 99     LINE 2 COL 70
                                       USING WS-DATA-SISTEMA(5:2).
             10 VALUE "/"                         LINE 2 COL 72. 
             10 SS-MES                 PIC 99     LINE 2 COL 73
                                       USING WS-DATA-SISTEMA(3:2).
             10 VALUE "/"                         LINE 2 COL 75. 
             10 SS-ANO                 PIC 99     LINE 2 COL 76
                                       USING WS-DATA-SISTEMA(1:2).
          05 VALUE "|"                 LINE 1 COL 1.
          05 VALUE "|"                 LINE 1 COL 80.
          05 VALUE "|"                 LINE 2 COL 1.
          05 VALUE "|"                 LINE 2 COL 80.
          05 VALUE "Ã"                 LINE 3 COL 1.
          05 VALUE "|"                 LINE 3 COL 80.
          05 TRACO                     PIC X(078) VALUE ALL '-'
                                       LINE 3 COL 2.
          05 VALUE "|"                 LINE 4 COL 1.
          05 VALUE "|"                 LINE 4 COL 80.          
          05 VALUE "|"                 LINE 5 COL 1.
          05 VALUE "|"                 LINE 5 COL 80.          
          05 VALUE "|"                 LINE 6 COL 1.
          05 VALUE "|"                 LINE 6 COL 80.          
          05 VALUE "|"                 LINE 7 COL 1.
          05 VALUE "|"                 LINE 7 COL 80.          
          05 VALUE "|"                 LINE 8 COL 1.
          05 VALUE "|"                 LINE 8 COL 80.          
          05 VALUE "|"                 LINE 9 COL 1.
          05 VALUE "|"                 LINE 9 COL 80.          
          05 VALUE "|"                 LINE 10 COL 1.
          05 VALUE "|"                 LINE 10 COL 80.          
          05 VALUE "|"                 LINE 11 COL 1.
          05 VALUE "|"                 LINE 11 COL 80.          
          05 VALUE "|"                 LINE 12 COL 1.
          05 VALUE "|"                 LINE 12 COL 80.          
          05 VALUE "|"                 LINE 13 COL 1.
          05 VALUE "|"                 LINE 13 COL 80.          
          05 VALUE "|"                 LINE 14 COL 1.
          05 VALUE "|"                 LINE 14 COL 80.          
          05 VALUE "|"                 LINE 15 COL 1.
          05 VALUE "|"                 LINE 15 COL 80.          
          05 VALUE "|"                 LINE 16 COL 1.
          05 VALUE "|"                 LINE 16 COL 80.          
          05 VALUE "|"                 LINE 17 COL 1.
          05 VALUE "|"                 LINE 17 COL 80.          
          05 VALUE "|"                 LINE 18 COL 1.
          05 VALUE "|"                 LINE 18 COL 80.          
          05 VALUE "|"                 LINE 19 COL 1.
          05 VALUE "|"                 LINE 19 COL 80.          
          05 VALUE "|"                 LINE 20 COL 1.
          05 VALUE "|"                 LINE 20 COL 80.          
          05 VALUE "|"                 LINE 21 COL 1.
          05 VALUE "|"                 LINE 21 COL 80.
      * 
          05 TRACO                     PIC X(078) VALUE ALL '-'
                                       LINE 22 COL 2.
          05 TRACO                     PIC X(078) VALUE ALL '-'
                                       LINE 24 COL 2.
          05 VALUE "Ã"                 LINE 22 COL 1.
          05 VALUE "|"                 LINE 22 COL 80.
          05 VALUE "|"                 LINE 23 COL 1.
          05 VALUE "|"                 LINE 23 COL 80.
          05 VALUE "|"                 LINE 24 COL 1.
          05 VALUE "|"                 LINE 24 COL 80.                                       
      *                                 
       01 WS-TELA-OPCAO.   
          05 VALUE "01 - CADASTRO"     LINE 5 COL 5.
          05 VALUE "02 - RELATORIOS"   LINE 7 COL 5.
          05 VALUE "03 - EXECUTAR"     LINE 9 COL 5.
          05 VALUE "04 - SAIR"         LINE 11 COL 5.
          05 VALUE "ENTRE COM A OPCAO: "
                                       LINE 23 COL 3.
          05 SS-OPCAO                  PIC 9(01) LINE 23 COL 22 
                                       BLANK WHEN ZEROS
                                       TO WS-OPCAO.
      *                                 
       01 WS-TELA-CADASTRO.   
          05 VALUE "01 - CADASTRO DE CLIENTE"    LINE 5 COL 5.
          05 VALUE "02 - IMPORTAR CLIENTES"      LINE 7 COL 5.
          05 VALUE "03 - CADASTRO DE VENDEDOR"   LINE 9 COL 5.
          05 VALUE "04 - VOLTAR"                 LINE 11 COL 5.
          05 VALUE "ENTRE COM A OPCAO: "         LINE 23 COL 3.
          05 SS-OPCAO                  PIC 9(01) LINE 23 COL 22 
                                       BLANK WHEN ZEROS
                                       TO WS-OPCAO.
      *
       01 WS-TELA-IMPORT.   
          05 VALUE "   - I M P O R T A R  C L I E N T E S -" 
                                                 LINE 5 COL 20.
          05 VALUE "ENDERECO ARQUIVO: "          LINE 9 COL 3.
          05 SS-END-ARQUI              PIC 9(01) LINE 9  COL 21  
                                       BLANK WHEN ZEROS
                                       TO WS-END-ARQUI.
                                       
       01 SS-TELA-CADASTRO-CLIENTE.   
          05 VALUE "CODIGO.......: "   LINE 5 COL 5.
          05 SS-CODIGO-CLIENTE         PIC ZZZZZZ9        LINE 5 COL 20
                                       BLANK WHEN ZEROS
                                       TO FD-COD-CLIENTE.
                                       
          05 VALUE "CNPJ.........: "   LINE 7 COL 5.
          05 SS-CNPJ-CLIENTE           PIC 9(014)         LINE 7 COL 20 
                                       BLANK WHEN ZEROS
                                       TO FD-CNPJ.
          
          05 VALUE "RAZAO SOCIAL.: "   LINE 9 COL 5.
          05 SS-RZSOCIAL-CLIENTE       PIC X(040)        LINE 9 COL 20  
                                       TO FD-RAZAO-SOCIAL.
          
          05 VALUE "LATITUDE.....: "   LINE 11 COL 5.
          05 SS-LATITUDE-CLIENTE       PIC +ZZZ,ZZZZZZZ9 LINE 11 COL 20
                                       TO FD-LATITUDE-CLI.
          
          05 VALUE "LONGITUDE....: "   LINE 13 COL 5.  
          05 SS-LONGITUDE-CLIENTE      PIC +ZZZ,ZZZZZZZ9  LINE 13 COL 20
                                       TO FD-LONGITUDE-CLI.
      *
       01 SS-TELA-CADASTRO-VENDEDOR.   
          05 VALUE "CODIGO.......: "   LINE 5 COL 5.
          05 SS-CODIGO-VENDEDOR        PIC ZZZZZZ9        LINE 5 COL 20
                                       BLANK WHEN ZEROS
                                       TO FD-COD-VENDEDOR.
                                       
          05 VALUE "CPF..........: "   LINE 7 COL 5.
          05 SS-CPF-VENDEDOR           PIC 9(011)         LINE 7 COL 20 
                                       BLANK WHEN ZEROS
                                       TO FD-CPF.
          
          05 VALUE "NOME.........: "   LINE 9 COL 5.
          05 SS-NOME-VENDEDOR          PIC X(040)          LINE 9 COL
          20
                                       TO FD-NOME.
          
          05 VALUE "LATITUDE.....: "   LINE 11 COL 5.
          05 SS-LATITUDE-VENDEDOR      PIC +ZZZ,ZZZZZZZ9  LINE 11 COL 20
                                       TO FD-LATITUDE-VEN.
          
          05 VALUE "LONGITUDE....: "   LINE 13 COL 5.  
          05 SS-LONGITUDE-VENDEDOR     PIC +ZZZ,ZZZZZZZ9 LINE 13 COL 20
                                       TO FD-LONGITUDE-VEN.
      *
       01 WS-TELA-RELATORIO.   
          05 VALUE "01 - RELATORIO DE CLIENTES"    LINE 5 COL 5.
          05 VALUE "02 - RELATORIO DE VENDEDORES"  LINE 7 COL 5.
          05 VALUE "03 - VOLTAR"                   LINE 9 COL 5.
          05 VALUE "ENTRE COM A OPCAO: "           LINE 23 COL 3.
          05 SS-OPCAO                  PIC 9(01)   LINE 23 COL 22 
                                       BLANK WHEN ZEROS
                                       TO WS-OPCAO.
      *
           PROCEDURE DIVISION          .
       
           PERFORM 0004-ABRIR-ARQUIVOS  
           DISPLAY SPACES AT 0101
           ACCEPT WS-DATA-SISTEMA      FROM DATE 
           
           DISPLAY WS-DATA-SISTEMA     AT 2366
           DISPLAY SS-TELA-PRINCIPAL.
      *-----------------------------------------------------------------
       0001-PRINCIPAL.
      *-----------------------------------------------------------------
           INITIALIZE                  WS-OPCAO
           PERFORM UNTIL WS-OPCAO EQUAL 4
           PERFORM 0010-LIMPAR-TELA
             DISPLAY WS-LINHA-BRANCO            AT 2302
             DISPLAY WS-TELA-OPCAO
             ACCEPT  WS-OPCAO  
             
             EVALUATE WS-OPCAO
               WHEN 1
                 PERFORM 0002-CADASTRAR
               WHEN 2
                 PERFORM 0003-IMP-RELATORIO
               WHEN 3
                 DISPLAY WS-LINHA-BRANCO         AT 2302
                 DISPLAY WS-OPCAO                AT 2302           
               WHEN 4
                 STOP RUN    
               WHEN OTHER
                 DISPLAY WS-LINHA-BRANCO     AT 2302 
                 DISPLAY "FAVOR DIGITAR UM VALOR ENTRE 1 E 4!!!"
                                             AT 2320
                 STOP ' '
             END-EVALUATE
           end-perform.
      *     
      *-----------------------------------------------------------------
       0001-EXIT.    
      *-----------------------------------------------------------------
      *
      *-----------------------------------------------------------------
       0002-CADASTRAR.
      *-----------------------------------------------------------------
      *
               PERFORM 0010-LIMPAR-TELA 
               DISPLAY WS-LINHA-BRANCO      AT 2302
               DISPLAY WS-TELA-CADASTRO
               
               INITIALIZE              WS-OPCAO 
               
               PERFORM UNTIL WS-OPCAO EQUAL 4
                  ACCEPT  WS-OPCAO
                  EVALUATE WS-OPCAO
                     WHEN 1
                       PERFORM 0011-CADASTRAR-CLIENTE
                     
                     WHEN 2
                       PERFORM 0015-IMPORTAR-CLIENTE                    
                     
                     WHEN 3 
                       PERFORM 0012-CADASTRAR-VENDEDOR
                     
                     WHEN 4
                       PERFORM 0001-PRINCIPAL
                     
                     WHEN OTHER
                       DISPLAY  "FAVOR DIGITAR UM VALOR ENTRE 1 E 4!!!"
                                       AT 2320
                       STOP ' '
                     
           END-PERFORM.
      *-----------------------------------------------------------------
       0002-EXIT.
      *-----------------------------------------------------------------
      *
       0003-IMP-RELATORIO.
          PERFORM 0010-LIMPAR-TELA
          DISPLAY WS-TELA-RELATORIO
          INITIALIZE                   WS-OPCAO
          PERFORM UNTIL WS-OPCAO EQUAL 4  
            ACCEPT  WS-OPCAO
            EVALUATE WS-OPCAO
              WHEN 1
              WHEN 2
              
              WHEN 3
                 PERFORM 0001-PRINCIPAL
              WHEN OTHER
                 DISPLAY  "FAVOR DIGITAR UM VALOR ENTRE 1 E 3!!!"
                                       AT 2320
                 STOP ' '
            END-EVALUATE
          END-PERFORM.
          
      *-----------------------------------------------------------------
       0003-EXIT.
      *-----------------------------------------------------------------
      *
      *-----------------------------------------------------------------
       0004-ABRIR-ARQUIVOS.
      *-----------------------------------------------------------------
       
           OPEN 
           I-O     ARQ-CLIENTE
                   ARQ-VENDEDOR
           OUTPUT  ARQ-CSV
           MOVE WS-ABERTURA            TO WS-OPERACAO
             
           PERFORM 0005-TESTAR-ARQCLIENTE
           PERFORM 0006-TESTAR-ARQVENDEDOR
           PERFORM 0007-TESTAR-ARQCSV
           .
      *-----------------------------------------------------------------
       0004-EXIT.
      *-----------------------------------------------------------------
      *
      *-----------------------------------------------------------------
       0005-TESTAR-ARQCLIENTE.
      *-----------------------------------------------------------------
       
           IF WS-FS-ACESSO-CLI NOT EQUAL ZEROS AND '05'
              DISPLAY WS-FS-ACESSO-CLI
                                       AT 2320                  
           END-IF.
           
      *-----------------------------------------------------------------  
       0005-EXIT.
      *-----------------------------------------------------------------
      *
      *-----------------------------------------------------------------
       0006-TESTAR-ARQVENDEDOR.
      *-----------------------------------------------------------------
       
           IF WS-FS-ACESSO-VEN NOT EQUAL ZEROS AND '05'
              DISPLAY WS-FS-ACESSO-VEN
                                       AT 2320                  
           END-IF.
           
      *-----------------------------------------------------------------
       0006-EXIT.
      *-----------------------------------------------------------------
       
      *-----------------------------------------------------------------
       0007-TESTAR-ARQCSV.
      *-----------------------------------------------------------------
      *
           IF WS-FS-ACESSO-CSV NOT EQUAL ZEROS
              DISPLAY WS-FS-ACESSO-CSV
                                       AT 2320                  
           END-IF.           
      *-----------------------------------------------------------------
       0007-EXIT.   
      *-----------------------------------------------------------------
      *
      *-----------------------------------------------------------------
       0008-TESTAR-ARQ-IMP-CLI.
      *-----------------------------------------------------------------
       
           IF (WS-FS-IMPORT-CLI            NOT EQUAL ZEROS AND '10')
             DISPLAY WS-LINHA-BRANCO       AT 2302
             DISPLAY 'FILE STATUS:'        AT 2310
             DISPLAY WS-FS-IMPORT-CLI      AT 2324
             DISPLAY ' OPERACAO: '         AT 2326 
             WS-OPERACAO' ARQUIVO CLIENTE'
                                           AT 2337
             STOP ' '
             DISPLAY WS-LINHA-BRANCO       AT 2302
           END-IF
           
           IF (WS-FS-IMPORT-CLI        EQUAL '10')
             MOVE 'S'                  TO WS-FIM-ARQ-IMPORT-CLI
           END-IF.       
      *-----------------------------------------------------------------
       0008-EXIT.    
      *-----------------------------------------------------------------
      *
      *-----------------------------------------------------------------
       0009-GRAVAR-CLIENTE.
      *-----------------------------------------------------------------
         
         WRITE REG-CLIENTE.
      *-----------------------------------------------------------------
       0009-EXIT.
      *-----------------------------------------------------------------
      *
      *-----------------------------------------------------------------
       0010-LIMPAR-TELA.
      *-----------------------------------------------------------------
       
       DISPLAY WS-LINHA-BRANCO         AT 0402  
       DISPLAY WS-LINHA-BRANCO         AT 0502  
       DISPLAY WS-LINHA-BRANCO         AT 0602  
       DISPLAY WS-LINHA-BRANCO         AT 0702  
       DISPLAY WS-LINHA-BRANCO         AT 0802  
       DISPLAY WS-LINHA-BRANCO         AT 0902  
       DISPLAY WS-LINHA-BRANCO         AT 1002  
       DISPLAY WS-LINHA-BRANCO         AT 1102  
       DISPLAY WS-LINHA-BRANCO         AT 1202  
       DISPLAY WS-LINHA-BRANCO         AT 1302  
       DISPLAY WS-LINHA-BRANCO         AT 1402  
       DISPLAY WS-LINHA-BRANCO         AT 1502  
       DISPLAY WS-LINHA-BRANCO         AT 1602  
       DISPLAY WS-LINHA-BRANCO         AT 1702  
       DISPLAY WS-LINHA-BRANCO         AT 1802 
       DISPLAY WS-LINHA-BRANCO         AT 1902 
       DISPLAY WS-LINHA-BRANCO         AT 2002 
       DISPLAY WS-LINHA-BRANCO         AT 2102 
       .
      *-----------------------------------------------------------------
       0010-EXIT.
      *-----------------------------------------------------------------
      *
      *-----------------------------------------------------------------
       0011-CADASTRAR-CLIENTE.
      *-----------------------------------------------------------------
         PERFORM 0010-LIMPAR-TELA
         DISPLAY WS-LINHA-BRANCO    AT 2302
         DISPLAY SS-TELA-CADASTRO-CLIENTE
         DISPLAY "DIGITE CODIGO ZERO PRA SAIR!!!"
                                    AT 2320
         ACCEPT  SS-CODIGO-CLIENTE
         IF FD-COD-CLIENTE NOT EQUAL ZEROS
            MOVE 1                     TO WS-ACHOU
                     
            READ ARQ-CLIENTE INVALID KEY
                 MOVE ZEROS    TO WS-ACHOU
            END-READ
                       
            IF WS-ACHOU EQUAL ZEROS 
               
               ACCEPT SS-CNPJ-CLIENTE
               ACCEPT SS-RZSOCIAL-CLIENTE
               ACCEPT SS-LATITUDE-CLIENTE
               ACCEPT SS-LONGITUDE-CLIENTE
               PERFORM 0009-GRAVAR-CLIENTE
               PERFORM 0002-CADASTRAR
            ELSE
               DISPLAY FD-CNPJ                AT 0720
               DISPLAY FD-RAZAO-SOCIAL        AT 0920
               MOVE FD-LATITUDE-CLI TO WS-LATI-LONG-MASC
               DISPLAY WS-LATI-LONG-MASC     AT 1120
               MOVE FD-LONGITUDE-CLI   TO WS-LATI-LONG-MASC
               DISPLAY WS-LATI-LONG-MASC     AT 1320
               DISPLAY WS-LINHA-BRANCO    AT 2302
               DISPLAY "CLIENTE JA EXISTE, A = ALTERAR | D = EXCLUIR |"
                                             AT 2310 
               DISPLAY "V = VOLTAR"          AT 2357        
               ACCEPT WS-DEL-ALT-CLIENTE    UPPER AT 2368
               IF WS-DEL-ALT-CLIENTE EQUAL 'D'
                  DELETE ARQ-CLIENTE
                  PERFORM 0002-CADASTRAR
               ELSE
                  IF WS-DEL-ALT-CLIENTE EQUAL 'A'
                     ACCEPT SS-RZSOCIAL-CLIENTE 
                     ACCEPT SS-LATITUDE-CLIENTE
                     ACCEPT SS-LONGITUDE-CLIENTE
                     REWRITE REG-CLIENTE
                     PERFORM 0002-CADASTRAR
                  ELSE
                    IF WS-DEL-ALT-CLIENTE EQUAL 'V'
                       MOVE 4                     TO WS-OPCAO
                       DISPLAY WS-LINHA-BRANCO    AT 2302
                       PERFORM 0010-LIMPAR-TELA
                    END-IF
                  END-IF     
                  
               END-IF
               DISPLAY WS-LINHA-BRANCO AT 2302
            END-IF
         ELSE
            MOVE 4                     TO WS-OPCAO
            DISPLAY WS-LINHA-BRANCO    AT 2302
            PERFORM 0010-LIMPAR-TELA
         END-IF
         .
      *-----------------------------------------------------------------
       0011-EXIT.
      *-----------------------------------------------------------------
      *
      *-----------------------------------------------------------------
       0012-CADASTRAR-VENDEDOR.
      *-----------------------------------------------------------------
       
         PERFORM 0010-LIMPAR-TELA
         DISPLAY WS-LINHA-BRANCO       AT 2302
         DISPLAY SS-TELA-CADASTRO-VENDEDOR
         DISPLAY "DIGITE CODIGO ZERO PRA SAIR!!!"
                                    AT 2320
         ACCEPT  SS-CODIGO-VENDEDOR
         IF FD-COD-VENDEDOR            NOT EQUAL ZEROS
            MOVE 1                     TO WS-ACHOU
                     
            READ ARQ-VENDEDOR          INVALID KEY
                 MOVE ZEROS            TO WS-ACHOU
            END-READ
                       
            IF WS-ACHOU EQUAL ZEROS 
   
               MOVE 1                  TO LKS-RETORNO  
               PERFORM UNTIL LKS-RETORNO EQUAL ZEROS
                 ACCEPT SS-CPF-VENDEDOR
                 PERFORM 0014-VALIDAR-CPF-CNPJ
                 IF LKS-RETORNO        EQUAL 1 OR 2 OR 3
                    DISPLAY WS-LINHA-BRANCO    AT 2302
                    DISPLAY "FAVOR DIGITAR UM CPF VALIDO!!!"
                                                 AT 2315
                    STOP ' '
                 END-IF    
               END-PERFORM
               ACCEPT SS-NOME-VENDEDOR
               ACCEPT SS-LATITUDE-VENDEDOR
               ACCEPT SS-LONGITUDE-VENDEDOR
               PERFORM 0013-GRAVAR-VENDEDOR
               PERFORM 0002-CADASTRAR
            ELSE
               DISPLAY FD-CPF               AT 0720
               DISPLAY FD-NOME              AT 0920
               MOVE FD-LATITUDE-VEN         TO WS-LATI-LONG-MASC
               DISPLAY WS-LATI-LONG-MASC    AT 1120
               MOVE FD-LONGITUDE-VEN        TO WS-LATI-LONG-MASC
               DISPLAY WS-LATI-LONG-MASC    AT 1320
               DISPLAY WS-LINHA-BRANCO      AT 2302
               DISPLAY "VENDEDOR JA EXISTE, A = ALTERAR | D = EXCLUIR |"
                                             AT 2310 
               DISPLAY "V = VOLTAR"          AT 2358
               ACCEPT WS-DEL-ALT-CLIENTE    UPPER AT 2369
               IF WS-DEL-ALT-CLIENTE        EQUAL 'D'
                  DELETE ARQ-VENDEDOR
                  PERFORM 0002-CADASTRAR
               ELSE
                  IF WS-DEL-ALT-CLIENTE     EQUAL 'A'
                     ACCEPT SS-NOME-VENDEDOR 
                     ACCEPT SS-LATITUDE-VENDEDOR
                     ACCEPT SS-LONGITUDE-VENDEDOR
                     REWRITE REG-VENDEDOR
                     MOVE WS-GRAVACAO       TO WS-OPERACAO
                     PERFORM 0006-TESTAR-ARQVENDEDOR
                     PERFORM 0002-CADASTRAR
                  ELSE
                    IF WS-DEL-ALT-CLIENTE EQUAL 'V'
                       MOVE 4                     TO WS-OPCAO
                       DISPLAY WS-LINHA-BRANCO    AT 2302
                       PERFORM 0010-LIMPAR-TELA
                    END-IF
                  END-IF
               END-IF
               DISPLAY WS-LINHA-BRANCO AT 2302
            END-IF
         ELSE
            MOVE 4                     TO WS-OPCAO
            DISPLAY WS-LINHA-BRANCO    AT 2302
            PERFORM 0010-LIMPAR-TELA
         END-IF
         .
      *-----------------------------------------------------------------
       0012-EXIT.
      *-----------------------------------------------------------------
      *
      *-----------------------------------------------------------------
       0013-GRAVAR-VENDEDOR.
      *-----------------------------------------------------------------
      *
         WRITE REG-VENDEDOR.
      *-----------------------------------------------------------------
      *
      *-----------------------------------------------------------------
       0013-EXIT.
      *-----------------------------------------------------------------
      *
      *-----------------------------------------------------------------
       0014-VALIDAR-CPF-CNPJ.
      *-----------------------------------------------------------------
       
         INITIALIZE                    LKS-PARM  
         MOVE FD-CPF                   TO LKS-NUMERO-I
         MOVE 'CPF'                    TO LKS-TIPO-CALCULO
         MOVE 'V'                      TO LKS-ACAO
         CALL 'VALIDATOR'              USING LKS-PARM. 
      *-----------------------------------------------------------------
       0014-EXIT.
      *-----------------------------------------------------------------
       
      *----------------------------------------------------------------*
      *ROTINA PARA IMPORTAR CLIENTES EM UM ARQUIVO EXTERNO             *
      *----------------------------------------------------------------*
       0015-IMPORTAR-CLIENTE.
      *-----------------------------------------------------------------
         PERFORM 0010-LIMPAR-TELA
         DISPLAY WS-LINHA-BRANCO       AT 2302
         DISPLAY WS-TELA-IMPORT
         DISPLAY "DIGITE 'S' PARA SAIR SEM IMPORTAR!!!"
                                       AT 2320
         ACCEPT WS-END-ARQUI           AT 0921 
         
         IF WS-END-ARQUI               EQUAL 'S'
            MOVE 4                     TO WS-OPCAO
            DISPLAY WS-LINHA-BRANCO    AT 2302
            PERFORM 0010-LIMPAR-TELA
         END-IF
         
         
         OPEN INPUT ARQ-IMPORT-CLI
         MOVE WS-ABERTURA            TO WS-OPERACAO
                
         PERFORM 0008-TESTAR-ARQ-IMP-CLI
           
         IF WS-FS-IMPORT-CLI           EQUAL ZEROS
            PERFORM UNTIL WS-FIM-ARQ-IMPORT-CLI 
                                       EQUAL'S'
               INITIALIZE              WS-AREA-IMPORT-CLI               
               READ ARQ-IMPORT-CLI     INTO WS-AREA-IMPORT-CLI          
               MOVE WS-LEITURA         TO WS-OPERACAO
               PERFORM 0008-TESTAR-ARQ-IMP-CLI
               IF WS-FS-IMPORT-CLI     EQUAL ZEROS
                  PERFORM 0016-GRAVAR-IMPORT-CLI
               ELSE
                  MOVE 'S'              TO WS-FIM-ARQ-IMPORT-CLI
                  CLOSE ARQ-IMPORT-CLI
                  MOVE WS-FECHAMENTO    TO WS-OPERACAO
                  PERFORM 0008-TESTAR-ARQ-IMP-CLI
                  IF WS-FS-IMPORT-CLI   NOT EQUAL ZEROS
                     DISPLAY WS-LINHA-BRANCO
                                        AT 2302
                  END-IF
               END-IF
            END-PERFORM
         ELSE
            DISPLAY WS-LINHA-BRANCO    AT 2302
         END-IF
         DISPLAY WS-LINHA-BRANCO       AT 2302
         DISPLAY 'REGISTROS DE CLIENTES IMPORTADOS ' AT 2302 
         DISPLAY ACU-GRAVADOS          AT 2335
         INITIALIZE                  ACU-GRAVADOS
         STOP ' '
         PERFORM 0002-CADASTRAR
         .
      *-----------------------------------------------------------------
       0015-EXIT.
      *-----------------------------------------------------------------
      *
      *-----------------------------------------------------------------
       0016-GRAVAR-IMPORT-CLI.
      *-----------------------------------------------------------------
       
         MOVE WS-COD-CLIENTE           TO FD-COD-CLIENTE
         MOVE 1                        TO WS-ACHOU
         READ ARQ-CLIENTE              KEY IS FD-COD-CLIENTE            
                                       INVALID KEY
              MOVE ZEROS               TO WS-ACHOU
         END-READ
         IF WS-ACHOU                  EQUAL ZEROS
            MOVE WS-CNPJ              TO FD-CNPJ
            MOVE 1                    TO WS-ACHOU
            READ ARQ-CLIENTE          KEY IS FD-CNPJ
                                      INVALID KEY
               MOVE ZEROS             TO WS-ACHOU
            END-READ
            IF WS-ACHOU               EQUAL ZEROS
              MOVE WS-RAZAO-SOCIAL    TO FD-RAZAO-SOCIAL
              MOVE WS-LATITUDE-CLI    TO FD-LATITUDE-CLI
              MOVE WS-LONGITUDE-CLI   TO FD-LONGITUDE-CLI
              WRITE REG-CLIENTE
              MOVE WS-GRAVACAO        TO WS-OPERACAO
              PERFORM 0005-TESTAR-ARQCLIENTE
              IF WS-FS-ACESSO-CLI     EQUAL ZEROS
                 ADD 1                TO ACU-GRAVADOS
              END-IF            
            ELSE
               MOVE WS-RAZAO-SOCIAL   TO FD-RAZAO-SOCIAL
               MOVE WS-LATITUDE-CLI   TO FD-LATITUDE-CLI
               MOVE WS-LONGITUDE-CLI  TO FD-LONGITUDE-CLI
               REWRITE REG-CLIENTE
               MOVE WS-GRAVACAO       TO WS-OPERACAO
               PERFORM 0005-TESTAR-ARQCLIENTE
               IF WS-FS-ACESSO-CLI     EQUAL ZEROS
                 ADD 1                 TO ACU-GRAVADOS
               END-IF
             END-IF
          ELSE
             MOVE WS-CNPJ              TO FD-CNPJ
             MOVE 1                    TO WS-ACHOU
             READ ARQ-CLIENTE          KEY IS FD-CNPJ
                                       INVALID KEY
               MOVE ZEROS              TO WS-ACHOU
             END-READ
             IF WS-ACHOU               EQUAL ZEROS
               DISPLAY 'REGISTRO INCONSISTENTE'
                                       AT 2320
               STOP ' '
               DISPLAY WS-LINHA-BRANCO AT 2302
             ELSE
               MOVE WS-RAZAO-SOCIAL   TO FD-RAZAO-SOCIAL
               MOVE WS-LATITUDE-CLI   TO FD-LATITUDE-CLI
               MOVE WS-LONGITUDE-CLI  TO FD-LONGITUDE-CLI
               REWRITE REG-CLIENTE
               MOVE WS-GRAVACAO       TO WS-OPERACAO
               PERFORM 0005-TESTAR-ARQCLIENTE
               IF WS-FS-ACESSO-CLI     EQUAL ZEROS
                  ADD 1                 TO ACU-GRAVADOS
               END-IF
             END-IF
           END-IF
           .
      *-----------------------------------------------------------------
       0016-EXIT.
      *-----------------------------------------------------------------

       end program CARTCLIE.