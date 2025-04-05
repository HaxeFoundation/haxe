class App2 {
	static function main() {
		php.Global.unserialize(sys.io.File.getContent('bin/serialized.data'));
	}
}