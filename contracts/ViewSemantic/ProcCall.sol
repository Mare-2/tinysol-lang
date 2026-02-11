//SPDX-License-Identifier: GPL-3.0-only
pragma solidity >= 0.8.2;

contract C {
    int x;
    function f(int x) public {
        x = x + 1;
    }

    function g() public view {
        this.f(x);
    }
}