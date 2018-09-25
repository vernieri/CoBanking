      ******************************************************************
      * Author: Vernieri
      * Date: September 18, 2018
      * Purpose: Media do curso
      * Tectonics: cobc
      ******************************************************************
      
       IDENTIFICATION DIVISION.
       PROGRAM-ID.  Media-Notas.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       special-names.
           decimal-point is comma.

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01 DADOS.
           02 WNota1      PIC  9(02)v9.
           02 WNota2      PIC  9(02)V9.
           02 WNota3      PIC  9(02)V9.    
           02 WMedia      PIC  9(02)v9.

       01 EDITADAS.
           02 WNota1-E      PIC  Z9,9.
           02 WNota2-E      PIC  Z9,9.
           02 WNota3-E      PIC  Z9,9.    
           02 WMedia-E      PIC  Z9,9.
           
       01 wcont             PIC  X(01) value spaces.
       
       01 MENSAGEMS-DE-TELA.
           02 MENSA1            PIC X(50) VALUE
                "DIGITE A NOTA1".
           02 MENSA2            PIC X(50) VALUE
                "DIGITE A NOTA2".
           02 MENSA3            PIC X(50) VALUE
                "DIGITE A NOTA3".         
           02 MENSA4            PIC X(30) VALUE
                "F I M  D O  P R O G R A M A".
           02 MENSA5            PIC X(30) VALUE SPACE.

       01 DATA-DO-SISTEMA.
           02 ANO               PIC 9(02) VALUE ZEROS.
           02 MES               PIC 9(02) VALUE ZEROS.
           02 DIA               PIC 9(02) VALUE ZEROS.
      
       SCREEN SECTION.
       01 TELA01.
           02 LINE 02 COLUMN 05 PIC 9(02)/ USING DIA.
           02 LINE 02 COLUMN 08 PIC 9(02)/ USING MES.
           02 LINE 02 COLUMN 11 PIC 9(02)  USING ANO.
           02 LINE 02 COLUMN 28 VALUE
                "Cálcula da Média das Notas".
           02 LINE 08 COLUMN 15 VALUE "Nota 1:".
           02 LINE 10 COLUMN 15 VALUE "Nota 2:".
           02 LINE 12 COLUMN 15 VALUE "Nota 3:".
           02 LINE 14 COLUMN 15 VALUE "Média :".      
