// unit testing FunCall.sol
faucet 0xA 100
deploy 0xA:0xC() "C" "contracts/ViewSemantic/FunCall.sol"
assert 0xC this.balance==0
assert 0xC x==0

0xA:0xC.update()
assert lastReverted
assert 0xC x==0

0xA:0xC.update2()
assert lastReverted
assert 0xC x==0

