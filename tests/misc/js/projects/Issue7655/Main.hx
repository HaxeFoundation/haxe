import js.lib.Promise;

class Main {
	static function main() {
		var p:Thenable<String> = new Promise<String>(null);

		$type(p.then(function(x) $type(x)));
		$type(p.then(function(x) $type(x), function(e) $type(e)));

		$type(p.then(function(x) {$type(x); return 1;}));
		$type(p.then(function(x) {$type(x); return Promise.resolve(1);}));

		$type(p.then(null,function(x) {$type(x); return 1;}));
		$type(p.then(null,function(x) {$type(x); return Promise.resolve(1);}));

		$type(p.then(function(x) {$type(x); return 1;}, function(e) {$type(e); return 1;}));
		$type(p.then(function(x) {$type(x); return Promise.resolve(1);}, function(e) {$type(e); return 1;}));
		$type(p.then(function(x) {$type(x); return 1;}, function(e) {$type(e); return Promise.resolve(1);}));
		$type(p.then(function(x) {$type(x); return Promise.resolve(1);}, function(e) {$type(e); return Promise.resolve(1);}));

		$type(Promise.resolve(p));
		$type(Promise.resolve(new Promise<String>(null)));
		$type(Promise.resolve("hi"));

		// should not hang, see https://github.com/HaxeFoundation/haxe/issues/5785
		$type(p.then(_ -> p));
	}
}
