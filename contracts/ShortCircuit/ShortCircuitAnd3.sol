//SPDX-License-Identifier: GPL-3.0-only
pragma solidity >= 0.8.2;

/*Con questo test controlliamo che venga risolta la prima espressione prima dell'AND 
e che se risulta false non si controlli il secondo parametro */

contract C {
  int x;
  bool b;
  function setX(int val) public returns (bool) {
    x = val;
    return false;
  }
  function f() public {
    b = (this.setX(1) || false) && this.setX(2);
  }
}
