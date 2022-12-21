type t = {
  operation : int -> int;
  testDivisor : int;
  whenTrue : int;
  whenFalse : int;
  mutable items : int list;
  mutable inspectCount : int;
}

(* I wasted so much time trying to parse the input, but ended up creating a vim macro
   that did the job in 30 seconds. SMH *)
let monkeys () =
  [|
    {
      items = [ 63; 57 ];
      operation = (fun old -> old * 11);
      testDivisor = 7;
      whenTrue = 6;
      whenFalse = 2;
      inspectCount = 0;
    };
    {
      items = [ 82; 66; 87; 78; 77; 92; 83 ];
      operation = (fun old -> old + 1);
      testDivisor = 11;
      whenTrue = 5;
      whenFalse = 0;
      inspectCount = 0;
    };
    {
      items = [ 97; 53; 53; 85; 58; 54 ];
      operation = (fun old -> old * 7);
      testDivisor = 13;
      whenTrue = 4;
      whenFalse = 3;
      inspectCount = 0;
    };
    {
      items = [ 50 ];
      operation = (fun old -> old + 3);
      testDivisor = 3;
      whenTrue = 1;
      whenFalse = 7;
      inspectCount = 0;
    };
    {
      items = [ 64; 69; 52; 65; 73 ];
      operation = (fun old -> old + 6);
      testDivisor = 17;
      whenTrue = 3;
      whenFalse = 7;
      inspectCount = 0;
    };
    {
      items = [ 57; 91; 65 ];
      operation = (fun old -> old + 5);
      testDivisor = 2;
      whenTrue = 0;
      whenFalse = 6;
      inspectCount = 0;
    };
    {
      items = [ 67; 91; 84; 78; 60; 69; 99; 83 ];
      operation = (fun old -> old * old);
      testDivisor = 5;
      whenTrue = 2;
      whenFalse = 4;
      inspectCount = 0;
    };
    {
      items = [ 58; 78; 69; 65 ];
      operation = (fun old -> old + 7);
      testDivisor = 19;
      whenTrue = 5;
      whenFalse = 1;
      inspectCount = 0;
    };
  |]

let test_monkeys () =
  [|
    {
      items = [ 79; 98 ];
      operation = (fun old -> old * 19);
      testDivisor = 23;
      whenTrue = 2;
      whenFalse = 3;
      inspectCount = 0;
    };
    {
      items = [ 54; 65; 75; 74 ];
      operation = (fun old -> old + 6);
      testDivisor = 19;
      whenTrue = 2;
      whenFalse = 0;
      inspectCount = 0;
    };
    {
      items = [ 79; 60; 97 ];
      operation = (fun old -> old * old);
      testDivisor = 13;
      whenTrue = 1;
      whenFalse = 3;
      inspectCount = 0;
    };
    {
      items = [ 74 ];
      operation = (fun old -> old + 3);
      testDivisor = 17;
      whenTrue = 0;
      whenFalse = 1;
      inspectCount = 0;
    };
  |]
