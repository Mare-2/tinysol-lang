// unit testing Assign.sol
faucet 0xA 100
deploy 0xA:0xC() "C" "contracts/ViewSemantic/Assign.sol"
assert 0xC this.balance==0
assert 0xC x==0

0xA:0xC.f(10)
assert lastReverted
assert 0xC x==0

0xA:0xC.g()
assert lastReverted
assert 0xC x==0

