import haxe.macro.Compiler;

function main() {
	// compiler arguments are expanded
	Sys.println(Compiler.getDefine("path_define") == Sys.getEnv("PATH"));
	// unknown variables are left alone
	Sys.println(Compiler.getDefine("unknown_define"));
	// runtime arguments are not expanded
	Sys.println(Sys.args());
}
