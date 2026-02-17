// SPDX-License-Identifier: GPL-3.0-only
pragma solidity >= 0.8.2;

contract C {
    address payable recipient;

    constructor(address payable _recipient) {
        recipient = _recipient;
    }

    function carica() public payable {
      // Funzione per caricare wei sul contratto
    }

    function sendNonView(uint amount) public {
        recipient.transfer(amount);
    }

    function sendView(uint amount) public view {
        recipient.transfer(amount);
    }

    function sendView(uint amount) public view {
        recipient.transfer(amount);
    }
}
