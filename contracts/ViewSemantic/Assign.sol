//SPDX-License-Identifier: GPL-3.0-only
pragma solidity >= 0.8.2;


contract C{
    int x;

    function f(int y) public view {
        x = y;
    }
    
    function g() public view {
        x = 12;
    }
}