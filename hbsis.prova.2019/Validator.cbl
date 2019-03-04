      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.       VALIDATOR.
       AUTHOR.           ROBSON LIMA DA SILVA.
      *-----------------------------------------------------------------
      *  PROGRAMA      : CADDIGIT
      *  OBJETIVO      : VERIFICA O DIGITO DO CPF
      *  ANALISTA      : ROBSON LIMA DA SILVA
      *  LINGUAGEM     : COBOL
      *  COMO USAR     : LKS-NUMERO-I ....: NUMERO INFORMADO
      *                : LKS-NUMERO-F ....: NUMERO CALCULADO
      *                : LKS-TIPO-CALCULO : CPF
      *                : LKS-ACAO ........: C - CALCULA
      *                                     V - VERIFICA
      *-----------------------------------------------------------------
      *  VERSAO DD.MM.AAAA  HISTORICO/AUTOR
      *  ------  ---------- ---------------
      *    001  24.09.2004  PROGRAMA INICIAL
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
      *-----------------------------------------------------------------
       CONFIGURATION SECTION.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------
       FILE SECTION.

      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
      *-----------------------------------------------------------------

       01  WS-AUXILIARES.
           05 WSS-IND-N                  PIC 9(002)  VALUE ZEROES.
           05 WSS-IND-O                  PIC 9(002)  VALUE ZEROES.
           05 WSS-IND-P                  PIC 9(002)  VALUE ZEROES.
           05 WSS-SOMA                   PIC 9(008)  VALUE ZEROES.
           05 WSS-NUMERO                 PIC 9(014)  VALUE ZEROES.
           05 WSS-NUMERO-R               REDEFINES WSS-NUMERO.
              10  WSS-NUMERO-T           PIC 9(001)  OCCURS 15 TIMES.
           05 WSS-PESOS                  PIC X(028)  VALUE SPACES.
           05 WSS-PESOS-R                REDEFINES WSS-PESOS.
              10  WSS-PESOS-T            PIC 9(002)  OCCURS 14 TIMES.
           05 WSS-QUOCI                  PIC 9(008)  VALUE ZEROES.
           05 WSS-RESTO                  PIC 9(008)  VALUE ZEROES.
           05 WSS-PESOS-CPF              PIC X(028)  VALUE
                                   '0000000011100908070605040302'.


      *-----------------------------------------------------------------
       LINKAGE SECTION.
      *-----------------------------------------------------------------

       01  LKS-PARAMETRO.
           COPY 'VALIDCPF'.                                         
      *
      *-----------------------------------------------------------------
       PROCEDURE DIVISION USING LKS-PARAMETRO.
      *-----------------------------------------------------------------

           PERFORM 1000-INICIAL  
           PERFORM 2000-PRINCIPAL
           PERFORM 9500-FINAL     
           GOBACK.

      *-----------------------------------------------------------------
       1000-INICIAL.
      *-----------------------------------------------------------------

           MOVE ZEROES TO LKS-RETORNO  
           EVALUATE TRUE

              WHEN LKS-ACAO = 'V'
                   EVALUATE LKS-TIPO-CALCULO 
                      WHEN 'CPF'
                         MOVE LKS-NUMERO-I TO WSS-NUMERO
                      WHEN OTHER
                         MOVE 1 TO LKS-RETORNO 
                         GOBACK
                   END-EVALUATE
              WHEN OTHER
                   MOVE 2 TO LKS-RETORNO 
                   GOBACK 
           END-EVALUATE.

       1000-EXIT.

      *-----------------------------------------------------------------
       2000-PRINCIPAL.
      *-----------------------------------------------------------------

           PERFORM 2100-CALCULO-CPF.

       2000-EXIT.

      *-----------------------------------------------------------------
       2100-CALCULO-CPF.
      *-----------------------------------------------------------------

           MOVE WSS-PESOS-CPF TO WSS-PESOS
           MOVE 05            TO WSS-IND-N
           MOVE 06            TO WSS-IND-P
           MOVE 13            TO WSS-IND-O
           MOVE ZEROES        TO WSS-SOMA
           PERFORM 7000-CALC-DIGITO-1

           MOVE 05            TO WSS-IND-N
           MOVE 05            TO WSS-IND-P
           MOVE 14            TO WSS-IND-O
           MOVE ZEROES        TO WSS-SOMA
           PERFORM 8000-CALC-DIGITO-2.

       2100-EXIT.

      *-----------------------------------------------------------------
       7000-CALC-DIGITO-1.
      *-----------------------------------------------------------------

           MOVE ZEROES TO WSS-SOMA
           PERFORM UNTIL WSS-IND-N GREATER WSS-IND-O
                   COMPUTE WSS-SOMA = WSS-SOMA +
                                     (WSS-NUMERO-T (WSS-IND-N) *
                                      WSS-PESOS-T  (WSS-IND-P))
                   ADD 1 TO WSS-IND-N
                            WSS-IND-P
           END-PERFORM
           DIVIDE WSS-SOMA BY 11 GIVING WSS-QUOCI REMAINDER WSS-RESTO
           IF WSS-RESTO EQUAL 0 OR 1
              MOVE ZEROES TO WSS-NUMERO-T (14)
           ELSE
              SUBTRACT WSS-RESTO FROM 11 GIVING WSS-NUMERO-T (14)
           END-IF.

       7000-EXIT.

      *-----------------------------------------------------------------
       8000-CALC-DIGITO-2.
      *-----------------------------------------------------------------

           MOVE ZEROES TO WSS-SOMA
           PERFORM UNTIL WSS-IND-N GREATER WSS-IND-O
                   COMPUTE WSS-SOMA = WSS-SOMA +
                                     (WSS-NUMERO-T (WSS-IND-N) *
                                      WSS-PESOS-T  (WSS-IND-P))
                   ADD 1 TO WSS-IND-N
                            WSS-IND-P
           END-PERFORM
           DIVIDE WSS-SOMA BY 11 GIVING WSS-QUOCI REMAINDER WSS-RESTO
           IF WSS-RESTO EQUAL 0 OR 1
              MOVE ZEROES TO WSS-NUMERO-T (15)
           ELSE
              SUBTRACT WSS-RESTO FROM 11 GIVING WSS-NUMERO-T (15)
           END-IF.

       8000-EXIT.

      *-----------------------------------------------------------------
       9500-FINAL.
      *-----------------------------------------------------------------

           MOVE WSS-NUMERO    TO LKS-NUMERO-F          
           IF  LKS-ACAO EQUAL 'V'                      
               IF LKS-NUMERO-I EQUAL LKS-NUMERO-F      
                  MOVE 0 TO LKS-RETORNO                
               ELSE                                    
                  MOVE 3 TO LKS-RETORNO                
               END-IF                                  
           ELSE                                        
               MOVE 0 TO LKS-RETORNO                   
           END-IF.                                      

       9500-EXIT.