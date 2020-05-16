package pack;




class UsageNoImport {
	public static function f() {
		return Mod1.Mod1Sub.field + Mod2.Mod2Sub.field + Mod3.Mod3Sub.field;
	}
}
