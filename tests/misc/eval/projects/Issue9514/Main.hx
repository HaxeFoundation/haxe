import haxe.Exception;

var one = 1;
var two = one + 1;

function main() {
	if (two == 2) Sys.stderr().writeString("Success");
}
