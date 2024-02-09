class SysExit0 {
	static function main() {
		Sys.stderr().writeString("Exiting with 0\n");
		Sys.exit(0);
	}
}