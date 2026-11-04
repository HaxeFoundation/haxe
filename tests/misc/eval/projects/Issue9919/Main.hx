function main() {
	var foo:Void->Void;
	foo = function() {
		trace("Hello");
		haxe.Timer.delay(foo, 1);
	};
}
