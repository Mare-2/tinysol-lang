// unit testing ShortCircuitAnd2.sol
faucet 0xA 100
deploy 0xA:0xC() "C" "contracts/ShortCircuitTest/ShortCircuitAnd2.sol"
assert 0xC this.balance==0
assert 0xC x==0
assert 0xC b==false

0xA:0xC.f()
assert 0xC x==1
assert 0xC b==true

