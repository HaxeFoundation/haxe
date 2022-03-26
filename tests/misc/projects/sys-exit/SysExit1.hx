class SysExit1 {
	static function main() {
		Sys.stderr().writeString("Exiting with 1\n");
		Sys.exit(1);
	}
}