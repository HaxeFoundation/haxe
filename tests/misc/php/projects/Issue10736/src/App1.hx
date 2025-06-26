class App1 {
	static function main() {
		sys.io.File.saveContent('bin/serialized.data', php.Global.serialize({a:1}));
	}
}