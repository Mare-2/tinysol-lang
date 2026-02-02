open TinysolLib.Ast
open TinysolLib.Sysstate       
open TinysolLib.Utils
open TinysolLib.Main

let test_exec_tx (src: string) (txl: string list) (els : string list) =
  let txl = List.map parse_transaction txl in
  init_sysstate
  |> faucet "0xA" 100
  |> faucet "0xB" 100
  |> deploy_contract { txsender="0xA"; txto="0xC"; txfun="constructor"; txargs=[]; txvalue=0; } src 
  |> exec_tx_list 1000 txl 
  |> fun st -> List.map (fun x -> x |> parse_expr |> eval_expr 
      { st with callstack = [{ callee = "0xC"; locals = []}] } ) els 
  |> List.for_all (fun v -> v = Bool true)

(* Unittest shortcircuiting *)
let%test "test_and_shortcircuit_1" = test_exec_tx 
  "contract C {
    int x;
    bool b;
    function fail() public returns (bool) {
        x = 1;
        return false;
    }
    function f() public {
      b = false && this.fail();
    }
  }"
  ["0xA:0xC.f()"]
  ["x==0"; "b==false"]

let%test "test_and_shortcircuit_2" = test_exec_tx
  "contract C {
    int x;
    bool b;
    function fail() public returns (bool) {
        x = 1;
        return true;
    }
    function f() public {
      b = true && this.fail();
    }
  }" 
  ["0xA:0xC.f()"]
  ["x==1"; "b"]

let%test "test_or_shortcircuit_1" = test_exec_tx
  "contract C {
    int x;
    bool b;

    function fail() public returns(bool) {
      x = 1;
      return false;
    }
    function f() public {
      b = true || this.fail();
    }
  }"
  ["0xA:0xC.f()"]
  ["x==0"; "b"]

let%test "test_or_shortcircuit_2" = test_exec_tx
  "contract C {
    int x;
    bool b;

    function fail() public returns(bool) {
      x = 1;
      return true;
    }
    function f() public {
      b = false || this.fail();
    }
  }"
  ["0xA:0xC.f()"]
  ["x==1"; "b"]