//SPDX-License-Identifier: GPL-3.0-only
pragma solidity >= 0.8.2;

contract C {
    int x = 0;
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
    function ok() public {
        this.f(y);
    }

    //la funzione ok() non accetta parametri quindi dovrebbe dare errore
    function err() public{
        this.ok(10);
    }

    //argomento singolo sbagliato di tipo 
    function err2() public{
        this.f(check);
    }

    //vari parametri passati sono corretti di numero e tipo 
    function ok2() public {
        this.g(30, true);       
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
    int n = 0;

    //valore invalido per il trasferimento
    function err5() public{
        a.transfer(-30);
    }

    //chiamata di una funzione di un altro contratto senza parametro, viene ignorata
    function h() public{
        a.f();
    }
}