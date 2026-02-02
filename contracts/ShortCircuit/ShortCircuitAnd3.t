// unit testing ShortCircuitAnd3.sol
faucet 0xA 110
deploy 0xA:0xC() "C" "contracts/ShortCircuit/ShortCircuitAnd3.sol"
assert 0xC this.balance==0
assert 0xC x==0
assert 0xC b==false

0xA:0xC.f()
assert 0xC b==false
assert 0xC x==1
