var e:haxe.extern.EitherType<Int,String> = "string";
var s:String = e;
s == "string";
e = 1;
var i:Int = e;
i == 1;
TestType.typeError(e = false) == true;
TestType.typeError(e = 1.5) == true;
