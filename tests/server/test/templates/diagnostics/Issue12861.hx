class Issue12861 {
	static function main() {
		var v:Int = 0;
		var _ = v.charAt(0); // Int has no field charAt
	}
}
