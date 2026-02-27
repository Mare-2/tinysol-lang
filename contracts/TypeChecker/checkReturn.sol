//SPDX-License-Identifier: GPL-3.0-only
pragma solidity >= 0.8.2;

contract C {
    int x;
    bool b;
    bool c;
    

    function f(bool t) public returns(bool) {
        return t;
    }

    function g(bool l) public returns(int) {
        return l;
    }
    function ok() public {
        b=this.f(c);
    }

    function err() public  {
        b=this.g(c);
    }

}

