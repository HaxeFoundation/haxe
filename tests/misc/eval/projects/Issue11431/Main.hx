import haxe.macro.Expr;

macro function makeCt() {
	var ct = macro :NotExists<String>;
	return macro(e : $ct);
}

function main() {
	makeCt();
}
