//SPDX-License-Identifier: GPL-3.0-only
pragma solidity >= 0.8.2;

contract C {
  int x; //x==0
  bool b; //b==false
  function fail() public returns (bool) { //funzione per il side effect
    x = 1;
    return false;
  }
  function f() public {
    b = false && this.fail(); //Lo short circuit non attiva il side effect di fail() e quindi x==0
  }
}
