(* CONTROLLARE SE SONO UTILI, altrimenti ciccia *)

(*Test 3 AND primo parametro un'espressione da risolvere che dà falso 
controllare che non entri nel secondo parametro
Test 4 AND primo parametro è un'espressione da risolvere che dà vero 
quindi controlla il secondo parametro*)

(* Test chained AND operators *)
let%test "test_and_chained" = test_exec_tx
  "contract C {
    int x;
    bool b;
    function setX(int val) public returns (bool) {
        x = val;
        return true;
    }
    function f() public {
      b = true && this.setX(1) && this.setX(2);
    }
  }"
  ["0xA:0xC.f()"]
  ["x==2"; "b"]

(* Test chained AND with early termination *)
let%test "test_and_chained_shortcircuit" = test_exec_tx
  "contract C {
    int x;
    bool b;
    function setX(int val) public returns (bool) {
        x = val;
        return val > 0;
    }
    function f() public {
      b = this.setX(-1) && this.setX(2) && this.setX(3);
    }
  }"
  ["0xA:0xC.f()"]
  ["x==-1"; "b==false"]

(* Test chained OR operators *)
let%test "test_or_chained" = test_exec_tx
  "contract C {
    int x;
    bool b;
    function setX(int val) public returns (bool) {
        x = val;
        return false;
    }
    function f() public {
      b = false || this.setX(1) || this.setX(2);
    }
  }"
  ["0xA:0xC.f()"]
  ["x==2"; "b==false"]

(* Test chained OR with early termination *)
let%test "test_or_chained_shortcircuit" = test_exec_tx
  "contract C {
    int x;
    bool b;
    function setX(int val) public returns (bool) {
        x = val;
        return val > 0;
    }
    function f() public {
      b = this.setX(1) || this.setX(2) || this.setX(3);
    }
  }"
  ["0xA:0xC.f()"]
  ["x==1"; "b"]

(* Test mixed AND/OR operators *)
let%test "test_mixed_and_or" = test_exec_tx
  "contract C {
    int x;
    bool b;
    function setX(int val) public returns (bool) {
        x = val;
        return val > 0;
    }
    function f() public {
      b = (false || this.setX(1)) && this.setX(2);
    }
  }"
  ["0xA:0xC.f()"]
  ["x==2"; "b"]

(* Test mixed with shortcircuit in first part *)
let%test "test_mixed_shortcircuit_first" = test_exec_tx
  "contract C {
    int x;
    bool b;
    function setX(int val) public returns (bool) {
        x = val;
        return val > 0;
    }
    function f() public {
      b = (false && this.setX(1)) || this.setX(2);
    }
  }"
  ["0xA:0xC.f()"]
  ["x==2"; "b"]

(* Test with local variables *)
let%test "test_and_with_locals" = test_exec_tx
  "contract C {
    int x;
    bool b;
    function f() public {
      bool temp = false;
      b = temp && (x = 1) == 1;
    }
  }"
  ["0xA:0xC.f()"]
  ["x==0"; "b==false"]

(* Test with comparisons *)
let%test "test_and_with_comparison" = test_exec_tx
  "contract C {
    int x;
    int y;
    bool b;
    function setY() public returns (bool) {
        y = 10;
        return true;
    }
    function f() public {
      x = 5;
      b = (x > 10) && this.setY();
    }
  }"
  ["0xA:0xC.f()"]
  ["x==5"; "y==0"; "b==false"]

(* Test OR with comparison *)
let%test "test_or_with_comparison" = test_exec_tx
  "contract C {
    int x;
    int y;
    bool b;
    function setY() public returns (bool) {
        y = 10;
        return false;
    }
    function f() public {
      x = 5;
      b = (x > 0) || this.setY();
    }
  }"
  ["0xA:0xC.f()"]
  ["x==5"; "y==0"; "b"]