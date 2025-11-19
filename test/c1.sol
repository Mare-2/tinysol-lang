//SPDX-License-Identifier: GPL-3.0-only

contract C1 {
    int x;
    bool b;

    function f1() public payable { 
        if (b) x = x+1;
        else b=true;
    }

    function f2(address a) public {
        if (this.balance > 0)
            a.transfer(1);
        else skip;
    }

    function f3(uint amt) public {
        if (this.balance < 8) b=false;
        else msg.sender.transfer(amt);
    }

}
