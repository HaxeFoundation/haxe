package pack;

import pack.Mod1;
import pack.Mod2;

class UsageImport {
	public static function f() {
		return Mod1.Mod1Sub.field + Mod2.Mod2Sub.field;
	}
}
