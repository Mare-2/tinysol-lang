//SPDX-License-Identifier: GPL-3.0-only
pragma solidity >= 0.8.2;

contract C {
    int x;
    int y = 10;
    bool check;


    function f(int n) public{
        x = x + n;
    }

    function g(int i, bool b) public{
        x = i;
        check = b;
    }

    //tipo e numero di parametri passati alla funzione f richiamata sono corretti
    function ok1() public {
        this.f(y);
    }

    //vari parametri passati sono corretti di numero e tipo 
    function ok2() public {
        this.g(30, true);       
    }

    //la funzione f è dichiarata per prendere come input un intero ma viene chiamata senza passare parametri
    function err0() public {
        this.f();
    }

    //la funzione ok() non accetta parametri quindi dovrebbe dare errore
    function err1() public{
        this.ok(10);
    }

    //argomento singolo sbagliato di tipo 
    function err2() public{
        this.f(check);
    }

    

    //vari parametri passati sono di numero corretto ma di tipi sbagliati
    function err3() public{
        this.g(x, y);
    }
    // vari parametri con tipi giusti ma numero inferiore a quelli richiesto
    function err4() public{
        this.g(x);
    }


    
}

contract D {
    address payable a;
    address b;
    int n;
    bool c;

    //chiamata di una transfer su una variabile di tipo address payable con un valore accettato
    function ok3() public {
        a.transfer(30);
    }

    //chiamata di una funzione di un altro contratto senza parametro, viene ignorata
    function ok4() public{
        a.f();
    }

    //valore invalido per il trasferimento
    function err5() public{
        a.transfer(-30);
    }

    //valore di wei valido ma indirizzo non payable
    function err6() public{
        b.transfer(10);
    }
    
    //chiamata di una funzione su una variabile di tipo non address
    function err7() public{
        c.f();
    }

    //chiamata di una funzione non esistente in questo contratto
    function err8() public{
        this.a();
    }

}