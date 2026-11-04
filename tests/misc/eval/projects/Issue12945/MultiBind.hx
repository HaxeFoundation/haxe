function take(?f:Void->Void, ?g:Void->Void) {}

function main() {
	take(() -> aaa, () -> bbb);
}
