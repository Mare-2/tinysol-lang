//SPDX-License-Identifier: GPL-3.0-only
pragma solidity >= 0.8.2;

/*Con questo test controlliamo che essendo il primo parametro uguale a true
venga controllato il secondo */

contract C {
    int x;
    bool b;
    function setX(int val) public returns (bool) {
        x = val;
        return true;
    }
    function f() public {
      b = (this.setX(1) || true) && this.setX(2);
    }
}