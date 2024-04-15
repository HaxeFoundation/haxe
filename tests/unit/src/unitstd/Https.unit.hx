// TODO: python is fine on unix but not on windows
#if (sys && !lua && !php && !python)
var r = haxe.Http.requestUrl("https://raw.githubusercontent.com/HaxeFoundation/haxe/development/tests/unit/res1.txt");
r == "Héllo World !";
#else
true == true;
#end
