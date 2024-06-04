function main() {
	static var foo = 0;
	myMacro(foo);
}

macro function myMacro(expr:haxe.macro.Expr):haxe.macro.Expr;