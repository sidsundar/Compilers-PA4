
(*  Example cool program testing as many aspects of the code generator
    as possible.
 *)

class Main inherits Alpha{
  a : Int;
  b : String;
  c : Bool;
  d : Hullaboola;
  main():IO { (new IO).out_string("Hello world!\n")};
};

class Hullaboola	{

};

class Alpha {
	x : Int;
	y : Int;
};
