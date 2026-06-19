function inner(?n:Int, ?f:Void->Void) {}
function outer(?g:Void->Void) {}

function main() {
	outer(() -> {
		inner(() -> ccc);
		ddd;
	});
}
