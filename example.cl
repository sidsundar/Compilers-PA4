class Main {
  main():Object {{
    if (true = false) then abort() else 0 fi;
    if (true = true) then 0 else abort() fi;
    if ("hello" = "hello".copy()) then 0 else abort() fi;
    let a:String in if (a = "") then 0 else abort() fi;
    --if 5 = 6 then abort() else 0 fi;
  }};

};

(*  Example cool program testing as many aspects of the code generator
    as possible.

class Main inherits Alpha{
  a : Int;
  b : String;
  c : Bool;
  d : Hullaboola;
  f(message : String):Object {{(new IO).out_string(message).out_int(a); self.g(1, 2); } };
  main():Object { f("Hello world")};
};

class Hullaboola	{

};

class Alpha {
	x : Int;
	y : Int;

	g(a:Int, b:Int):Object{ (new IO).out_int(a).out_int(b)};
};

*)