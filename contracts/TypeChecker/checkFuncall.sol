//SPDX-License-Identifier: GPL-3.0-only
pragma solidity >= 0.8.2;

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
    function err1() public returns(int){
        return this.f(b);   
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

    //Tipo di ritorno sbagliato e numero di parametri eccessivo
    function err6 () public returns(bool){
        x=1;
        y=2;
        z=3;
        b=true;
        return this.f2(x,y,z,b);
    }

    //Tipo di ritorno sbagliato e numero di parametri giusto
    function err7 () public returns(bool){
        x=1;
        y=2;
        z=3;
        return this.f2(x,y,z);
    }
    


}

contract D{
    address payable a;
    int n;
    
    //Numero di wei negativo
    function errD() public{
        a.transfer(-30);
    }

    function f4() public returns(int){
        n=10;

        return a.f(n);
    }

}