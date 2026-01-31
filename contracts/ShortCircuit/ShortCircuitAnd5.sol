//SPDX-License-Identifier: GPL-3.0-only
pragma solidity >= 0.8.2;

/* Questo test controlla che se abbiamo una serie di AND 
concatenati il controllo si interrompa appena incontra il primo false*/

contract C {
    int x;
    bool b;
    function setX(int val) public returns (bool) {
        x = val;
        return val < 0;
    }
    function f() public {
      b = this.setX(-1) && this.setX(2) && this.setX(3);
    }
  }