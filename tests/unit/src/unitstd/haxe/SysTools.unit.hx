var str = "abc";
haxe.SysTools.isEofChar(StringTools.fastCodeAt(str, 0)) == false;
haxe.SysTools.isEofChar(StringTools.fastCodeAt(str, 1)) == false;
haxe.SysTools.isEofChar(StringTools.fastCodeAt(str, 2)) == false;
haxe.SysTools.isEofChar(StringTools.fastCodeAt(str, 3)) == true;
haxe.SysTools.isEofChar(StringTools.fastCodeAt(str, 2)) == false;
haxe.SysTools.isEofChar(StringTools.fastCodeAt(str, 3)) == true;
haxe.SysTools.isEofChar(StringTools.fastCodeAt("", 0)) == true;

// isEOF
#if (neko || lua || eval)
haxe.SysTools.isEofChar(null) == true;
#elseif (cs || java || python)
haxe.SysTools.isEofChar( -1) == true;
#elseif js
// how do I test this here?
#else
haxe.SysTools.isEofChar(0) == true;
#end
