function take(?n:Int, ?f:Void->Void) {}

function main() {
	take(() -> aaa, () -> bbb);
}
