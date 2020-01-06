class A {
   (*Caso 1: Buena declaración de entero*)
   var : Int; (*Se declara un entero de buena forma*)
   var : Int; (*Se comprueba el analizador semantico, al declarar una variable que ya existe de tipo booleano*)

   (*Caso 4: Buena declaración de booleano*)
   var2 : Bool; (*Se declara un booleano de buena forma*)
   var2 : Bool; (*Se comprueba el analizador, semántico al declarar una variable que ya existe de tipo booleano*)

   set_var(num : Int) : SELF_TYPE {
      {  
         (*Caso 6: Se comprueba la condición y el cuerpo del if*)
         if (var) then {

           (*Caso 2: Buena asignación de entero*)
           var <- "Hola"; 
           var <- true;

	   (*Caso 3: Buena asignación de una expresión a un entero*)
           var <- num+var2; 
           var <- num+"Hola";
           var <- var2+"Hola";
 	   var <- num-true;
           var <- num*"Hola";


           (*Caso 5: Buena asignación de booleano*)
           var2 <- num;
           var2 <- "Hola";
           var2 <- var4;
         }
         else {
           (*Caso 7: Ámbito de las variables*)
           var <- var4;
         }
	 fi;
         self;
      }
   };

   (*Caso 7: Ámbito de los métodos*)
   set_var(num : Int) : SELF_TYPE {
      {  
         self;
      }
   };


};
