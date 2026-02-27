//SPDX-License-Identifier: GPL-3.0-only
pragma solidity >= 0.8.2;

//funzione non esistente
//chiamata da un non indirizzo

contract C{
    int x;
    int y;
    int z;
    int w;
    bool b;

    function f(int a) public returns(int){
        a=a+1;
        return a;
    }

    function f2(int n1, int n2, int n3) public returns(int){
        n1=n1+n2+n3;
        return n1;
    }

    function f3(int n1, int n2, int n3) public returns(int){
        n1=n1+n2+n3;
        
    }

    //Parametro singolo sbagliato
    function err0() public returns(int){
        return this.f(b);   
    }
    
    //La funzione richiede un parametro ma non gli viene passato
    function err1() public returns(int){
        return this.f();   
    }

    //Parametro singolo giusto
    function ok1() public returns(int){
        return this.f(x);
    }


    //Diversi parametri sbagliati

        //Numero giusto di parametri ma con variabili del tipo sbagliato
    function err2() public returns(int){
        x=1;
        y=2;
        b=true;
        return this.f2(x,y,b);
    }

        //Numero sbagliato di parametri (meno di quelli richiesti)
    function err3() public returns(int){
        x=1;
        y=2;
        return this.f2(x,y);
    }

        //Numero sbagliato di parametri (più di quelli richiesti)
    function err4() public returns(int){
        x=1;
        y=2;
        z=3;
        w=4;
        return this.f2(x,y,z,w);
    }

    //Diversi parametri giusti
    function ok2() public returns(int){

        x=1;
        y=2;
        z=3;       
        return this.f2(x,y,z);
    }

    //Return vuoto
    function err5() public returns(int){
       return this.f3(x,y,z);
    }

    //Tipo di ritorno sbagliato
    function err6() public returns(bool){
        x=1;
        y=2;
        z=3;
        return this.f2(x,y,z);
    }

    //Chiamata a funzione non esistente 
    function err7() public returns(bool){
        x=1;
        y=2;
        z=3;
        return this.f7(x,y,z);
    }


}

contract D{
    address payable a;
    address b;
    int n;
    bool b2;
    
    //Numero di wei negativo
    function errTrans() public{
        a.transfer(-30);
    }

    //Indirizzo non payable
    function errTrans2() public{
        b.transfer(30);
    }

    //Indirizzo payable e numero di wei positivo
    function okTrans() public{
        a.transfer(30);
    }

    //Funzione da un altro contratto payable
    function f4() public returns(int){
        n=10;
   
        return a.f(n);
    }

    //Funzione da un altro contratto non payable
    function f5() public returns(int){
        n=10;
   
        return b.f(n);
    }

    //Chiamata di una funzione da una variabile di tipo non address
    function f6() public returns(int){
        n=10;
        b2=true;
   
        return b2.f(n);
    }

}