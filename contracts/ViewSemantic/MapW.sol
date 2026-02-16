//SPDX-License-Identifier: GPL-3.0-only
pragma solidity >= 0.8.2;

contract C {
    mapping (int => uint) public Prova;
    uint public x;

    function g() public { // function non view e assegnamento con mapping
        Prova[0]=1;
        x = Prova[0];
    }

    function f() public view { // function view e fallimento 
        Prova[0] = 1;
    }
}