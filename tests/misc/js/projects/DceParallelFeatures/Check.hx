class Check {
	static function main() {
		if (sys.io.File.getContent("bin/main.js").indexOf('__ename__:"E"') < 0)
			throw "the Type.getEnumName feature is missing";
	}
}
