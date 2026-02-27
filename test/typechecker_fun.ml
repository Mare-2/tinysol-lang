open TinysolLib.Utils
open TinysolLib.Typechecker

let test_typecheck (src: string) (exp : bool) =
  let c = src |> parse_contract |> preprocess_contract in 
  match typecheck_contract c with
    | Ok() -> exp
    | _ -> not exp  


(*-------------------------------------Test ProcCall--------------------------------------------*)

(*la funzione f è dichiarata per prendere come input un intero ma viene chiamata senza passare parametri*)
let%test "test_typecheck_proc_0" = test_typecheck
"contract C{
  int x;
  function f(int n) public{
        x = x + n;
    }
  function err5() public {
        this.f();
    }
}"
false

(*richiamo una funzione che non accetta parametri passando un paramertro*)
let%test "test_typecheck_proc_1" = test_typecheck
"contract C {
    
  int a;
  
  function f() public {
    a = a+1;
  }
    
  function err1() public {
    return this.f(10);   
  }
}"
false

(*argomento singolo sbagliato di tipo*)
let%test "test_typecheck_proc_2" = test_typecheck 
"contract C{
  
  bool check;


  function f(int n) public{
      x = x + n;
  }

    function err2() public{
      this.f(check);
      }
}"
false

(*vari parametri passati sono di numero corretto ma di tipi sbagliati*)
let%test "test_typecheck_proc_3" = test_typecheck
"contract C{
    int x;
    bool check;
    function g(int i, bool b) public{
        x = i;
        check = b;
    }

    function err3() public{
        this.g(x, y);
    }
   
    
}"
false

(*vari parametri con tipi giusti ma numero inferiore a quelli richiesto*)
let%test "test_typecheck_proc_4" = test_typecheck
"contract C{

  int x = 0;
  bool check;

  
  function g(int i, bool b) public{
      x = i;
      check = b;
  }

  function err4() public{
          this.g(x);
      }
}"
false 

(*valore invalido per il trasferimento*)
let%test "test_typecheck_proc_5" = test_typecheck
"contract C{
  address payable a;

  function err5() public{
        a.transfer(-30);
    }
}"
false 

(*valore di wei valido ma indirizzo non payable*)
let%test "test_typecheck_proc_6" = test_typecheck
"contract C{
  address b;

  function err6() public{
      b.transfer(10);
  }
}"
false 

(*chiamata di una funzione su una variabile di tipo non address*)
let%test "test_typecheck_proc_7" = test_typecheck
"contract C{
  bool c;

  function err7() public{
        c.f();
  }
}"
false 

(*chiamata di una funzione non esistente in questo contratto*)
let%test "test_typecheck_proc_8" = test_typecheck
"contract C{

  function err8() public{
      this.a();
  }
    
}"
false 

(*tipo e numero di parametri passati alla funzione f richiamata sono corretti*)
let%test "test_typecheck_proc_9" = test_typecheck
"contract C{
  int y = 10;
  int x;

  function f(int n) public{
      x = x + n;
    }

  function ok1() public {
    this.f(y);
  }
}"
true

(*vari parametri passati sono corretti di numero e tipo*)
let%test "test_typecheck_proc_10" = test_typecheck
"contract C{
  int x = 0;
  bool check;

  function g(int i, bool b) public{
        x = i;
        check = b;
    }

  function ok2() public {
    this.g(30, true);       
  }
}"
true

(*chiamata di una transfer su una variabile di tipo address payable con un valore accettato*)
let%test "test_typecheck_proc_11" = test_typecheck
"contract C{
  address payable a;

  function ok3() public {
        a.transfer(30);
  }
}"
true

(*chiamata di una funzione di un altro contratto senza parametro, viene ignorata*)
let%test "test_typecheck_proc_12" = test_typecheck
"contract C{
  address payable a;

  function err9() public{
      a.f();
    }
}"
true 

(*----------------------------------------Test FunCall-----------------------------------------*)

(*Parametro singolo sbagliato*)
let%test "test_typecheck_fun_" = test_typecheck
  "contract C{
    bool b;
    
    function f(int a) public returns(int){
      a=a+1;
      return a;
    } 
    
    function err() public returns(int){
      return this.f(b);   
    }
  }"
  false
;;

(*La funzione richiede un parametro ma non gli viene passato*)
let%test "test_typecheck_fun_1" = test_typecheck
  "contract C{
    bool b;
    
    function f(int a) public returns(int){
      a=a+1;
      return a;
    } 
    
    function err() public returns(int){
      return this.f();   
    }
  }"
  false
;;

(*Parametro singolo giusto*)
let%test "test_typecheck_fun_2" = test_typecheck
  "contract C{
    int x;  
    
    function f(int a) public returns(int){
      a=a+1;
      return a;
    } 
    
    function ok() public returns(int){
        return this.f(x);
    }
  }"
  true
;;

(*Numero giusto di parametri ma con variabili del tipo sbagliato*)
let%test "test_typecheck_fun_3" = test_typecheck
  "contract C{
    int x;
    int y;  
    bool b;
    
    function f(int n1, int n2, int n3) public returns(int){
      n1=n1+n2+n3;
      return n1;
    }
    
    function err() public returns(int){
      x=1;
      y=2;
      b=true;
      return this.f(x,y,b);
    }
  }"
  false
;;

(*Numero sbagliato di parametri (meno di quelli richiesti)*)
let%test "test_typecheck_fun_4" = test_typecheck
  "contract C{
    int x;
    int y;
    
    function f(int n1, int n2, int n3) public returns(int){
      n1=n1+n2+n3;
      return n1;
    }
    
    function err() public returns(int){
        x=1;
        y=2;
        return this.f(x,y);
    }
  }"
  false
;;

(*Numero sbagliato di parametri (più di quelli richiesti)*)
let%test "test_typecheck_fun_5" = test_typecheck
  "contract C{
    int x;
    int y;
    int z;
    int w;
    
    function f(int n1, int n2, int n3) public returns(int){
      n1=n1+n2+n3;
      return n1;
    }
    
    function err() public returns(int){
      x=1;
      y=2;
      z=3;
      w=4;
      return this.f(x,y,z,w);
    }
  }"
  false
;;

(*Diversi parametri giusti*)
let%test "test_typecheck_fun_6" = test_typecheck
  "contract C{
    int x;
    int y;
    int z;
    
    function f(int n1, int n2, int n3) public returns(int){
      n1=n1+n2+n3;
      return n1;
    }
    
    function ok() public returns(int){
      x=1;
      y=2;
      z=3;       
      return this.f(x,y,z);
    }
  }"
  true
;;

(*Return vuoto*)
let%test "test_typecheck_fun_7" = test_typecheck
  "contract C{
    function f(int n1, int n2, int n3) public returns(int){
      n1=n1+n2+n3;  
    }
  }"
  false
;;

(*Tipo di ritorno sbagliato*)
let%test "test_typecheck_fun_8" = test_typecheck
  "contract C{
    int x;
    int y;
    int z;
    
    function f(int n1, int n2, int n3) public returns(int){
      n1=n1+n2+n3;
      return n1;
    }

    function err() public returns(bool){
      x=1;
      y=2;
      z=3;
      return this.f(x,y,z);
    }
  }"
  false
;;

(*Chiamata a funzione non esistente*)
let%test "test_typecheck_fun_9" = test_typecheck
  "contract C{
    int x;
    int y;
    int z;
    
    function err() public returns(bool){
      x=1;
      y=2;
      z=3;
      return this.f(x,y,z);
    }
  }"
  false
;;

(*Numero di wei negativo*)
let%test "test_typecheck_fun_10" = test_typecheck
  "contract C{
    address payable a;

    function err() public{
      a.transfer(-30);
    }
  }"
  false
;;

(*Indirizzo non payable*)
let%test "test_typecheck_fun_11" = test_typecheck
  "contract C{
    address a;

    function err() public{
      a.transfer(30);
    }
  }"
  false
;;

(*Indirizzo payable e numero di wei positivo*)
let%test "test_typecheck_fun_12" = test_typecheck
  "contract C{
    address payable a;

    function ok() public{
      a.transfer(30);
    }
  }"
  true
;;

(*Funzione da un altro contratto payable*)
let%test "test_typecheck_fun_11" = test_typecheck
  "contract C{
    address payable a;

    function f2() public returns(int){
      n=10;
  
      return a.f(n);
    }
  }"
  false
;;

(*Funzione da un altro contratto non payable*)
let%test "test_typecheck_fun_11" = test_typecheck
  "contract C{
    address a;

    function f2() public returns(int){
      n=10;
  
      return a.f(n);
    }
  }"
  false
;;

(*Chiamata di una funzione da una variabile di tipo non address*)
let%test "test_typecheck_fun_11" = test_typecheck
  "contract C{
    bool b;

    function f6() public returns(int){
        n=10;
        b=true;
   
        return b.f(n);
    }
  }"
  false
;;

(*---------------------------------------Test Return------------------------------------------*)
let%test "test_typecheck_return_1" = test_typecheck
  "contract C {
    function f() public returns (int) {
      int x;
      x = 1;
    }
  }"
  false

let%test "test_typecheck_return_2" = test_typecheck
  "contract C {
    function f() public {
      return 1;
    }
  }"
  false
;;

let%test "test_typecheck_return_3" = test_typecheck
  "contract C {
    function f(bool t) public returns(bool) {
      return t;
    }
  }"
  true
;;

let%test "test_typecheck_return_4" = test_typecheck
  "contract C {
    function f(bool t) public returns(int) {
      return t;
    } 
  }"
  false
;;

let%test "test_typecheck_return_5" = test_typecheck
  "contract C {
    function f() public returns(int) {
      return (1,2);
    }
  }"
  false
;;

let%test "test_typecheck_return_6" = test_typecheck
  "contract C {
    int x;
    function f() public view returns (uint) { return(x>0); }
  }"
  false
;;