//SPDX-License-Identifier: GPL-3.0-only
pragma solidity >= 0.8.2;

contract C {
    int x;
    
    function f(int x) public returns(int) {
        x = x + 1;
        return x;
    }

    function g() public view returns(int) {
        return this.f(x);
    }

    function update() public {
        x = this.g();
    }

    //testiamo che l'assegnamento di una variabile locale funzioni 
    function g2() public view returns(int) {
        int y;
        y=5;
        return this.g();
    }

    function update2() public {
        x = this.g2();
    }
    
}