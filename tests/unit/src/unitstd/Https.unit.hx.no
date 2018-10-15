#if (cpp || hl || (neko && !macro && !interp))
var r = haxe.Http.requestUrl("https://raw.githubusercontent.com/HaxeFoundation/haxe/development/tests/unit/res1.txt");
r == "Héllo World !";
#end
