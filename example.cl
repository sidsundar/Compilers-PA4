
(*  Example cool program testing as many aspects of the code generator
    as possible.
 *)

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
