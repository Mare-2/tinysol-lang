open TinysolLib.Ast
open TinysolLib.Sysstate       
open TinysolLib.Utils
open TinysolLib.Main

let test_exec_cmd (c,n_steps,var,exp_val) =
  c
  |> parse_cmd
  |> blockify_cmd (* TODO: enumify? *)
  |> fun c -> exec_cmd n_steps c (push_callstack {callee="0xC"; locals=[];} init_sysstate)
  |> fun t -> match t with
  | St st -> Option.get (lookup_var var st) = exp_val
  | CmdSt(_,st) -> Option.get (lookup_var var st) = exp_val
  | Reverted _ -> false
  | Returned _ -> false


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


let test_exec_constructor (src: string) (value: int) (args: exprval list) (els : string list) =
  init_sysstate
  |> faucet "0xA" 100
  |> deploy_contract { txsender="0xA"; txto="0xC"; txfun="constructor"; txargs=args; txvalue=value; } src 
  |> fun st -> List.map (fun x -> x |> parse_expr |> eval_expr 
      { st with callstack = [{ callee = "0xC"; locals = []}] } ) els 
  |> List.for_all (fun v -> v = Bool true)


let test_exec_fun (src1: string) (src2: string) (txl : string list) (els : (addr * string) list) =
  let txl = List.map parse_transaction txl in
  init_sysstate
  |> faucet "0xA" 200
  |> faucet "0xB" 100
  |> deploy_contract { txsender="0xA"; txto="0xC"; txfun="constructor"; txargs=[]; txvalue=0; } src1 
  |> deploy_contract { txsender="0xA"; txto="0xD"; txfun="constructor"; txargs=[]; txvalue=100; } src2 
  |> exec_tx_list 1000 txl 
  |> fun st -> List.map (fun (a,x) -> x |> parse_expr |> eval_expr 
    { st with callstack = [ { callee = a; locals = [] } ] }) els 
  |> List.for_all (fun v -> v = Bool true)


(* QUESTO TEST FUNZIONA!!! *)
(* let%test è per definire un test, poi si mette il nome *)
(* test_exec_tx è una funzione scritta da bart per eseguire questo test,
 che fa il deployment del contratto e chiama funzioni*)
let%test "test_and_shortcircuit_1" = test_exec_tx 
 (* Il primo argomento è il contratto *)    
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
  ["0xA:0xC.f()"] (* Questa è la transazione da svolgere: 0xA chiama la funzione f sul contratto 0xC *)
  ["x==0"; "b==false"] (* Questi sono gli esiti che ci aspettiamo (lista) *)

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