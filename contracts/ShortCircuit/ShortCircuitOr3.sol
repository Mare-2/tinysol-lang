//SPDX-License-Identifier: GPL-3.0-only
pragma solidity >= 0.8.2;

/*Questo test serve a controllare che venga risolta prima l'espressione del primo parametro
e se risulta true non viene controllato il secondo parametro  */

contract C {
  int x;
  bool b;
  function setX(int val) public returns (bool) {
    x = val;
    return true;
  }
  function f() public {
    b = (true && this.setX(1)) || this.setX(2);
  }
}
