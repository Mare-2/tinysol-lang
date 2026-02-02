//SPDX-License-Identifier: GPL-3.0-only
pragma solidity >= 0.8.2;

contract C {
  int x;
  bool b;

  function fail() public returns(bool) {
    x = 1;
    return false;
  }
  function f() public {
    b = true || this.fail();
  }
}
