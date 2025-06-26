package pack;




class UsageNoImport {
	public static function f() {
		return Mod1.Mod1Sub.field + Mod2.Mod2Sub.field + Mod3.Mod3Sub.field;
	}

	public static function f2() {
		return Mod3.lowerCase();
	}
}
