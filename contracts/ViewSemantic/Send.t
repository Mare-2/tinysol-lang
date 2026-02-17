// unit testing Send.sol
faucet 0xA 100
faucet 0xB 0

deploy 0xA:0xC("0xB") "C" "contracts/ViewSemantic/Send.sol"
assert 0xC this.balance==0
assert 0xA this.balance==100
assert 0xB this.balance==0

0xA:0xC.carica{value:10}()
assert 0xC this.balance==10
assert 0xA this.balance==90

0xA:0xC.sendView(3)
assert lastReverted
assert 0xC this.balance==10
assert 0xB this.balance==0

0xA:0xC.sendNonView(3)
assert 0xC this.balance==7
assert 0xB this.balance==3

0xA:0xC.carica{value:10}()
assert 0xC this.balance==10
assert 0xA this.balance==90

0xA:0xC.sendView(3)
assert lastReverted
assert 0xC this.balance==10
assert 0xB this.balance==0

0xA:0xC.sendNonView(3)
assert 0xC this.balance==7
assert 0xB this.balance==3
