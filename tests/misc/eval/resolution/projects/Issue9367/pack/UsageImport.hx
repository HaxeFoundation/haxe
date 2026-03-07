package pack;

import pack.Mod1;
import pack.Mod2;
import pack.Mod3;

class UsageImport {
	public static function f() {
		return Mod1.Mod1Sub.field + Mod2.Mod2Sub.field + Mod3.Mod3Sub.field;
	}

	public static function f2() {
		return Mod3.lowerCase();
	}
}
