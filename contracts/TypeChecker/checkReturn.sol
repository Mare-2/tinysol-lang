//SPDX-License-Identifier: GPL-3.0-only
pragma solidity >= 0.8.2;

contract C {
    int x;
    bool b;
    bool c;
    
    function ok() public {
        b=this.f(c);
    }

    function err() public  {
        b=this.g(c);
    }

    function f(bool bo) public returns(bool) {
        return bo;
    }

    function g(bool bo) public returns(int) {
        return bo;
    }
}


