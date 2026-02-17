// unit testing ProcCall.sol
faucet 0xA 100
deploy 0xA:0xC() "C" "contracts/ViewSemantic/MapW.sol"
assert 0xC this.balance==0
assert 0xC x==0

0xA:0xC.g() // chiamata alla funzione non view con assegnamento e mapping
assert 0xC x==1 // asseganamento con mapping avvenuto correttamente

0xA:0xC.f()
assert lastReverted