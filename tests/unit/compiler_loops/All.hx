class All {

	static function main() {
		for( f in sys.FileSystem.readDirectory(".") ) {
			if( !StringTools.endsWith(f,".hx") || f == "All.hx" ) continue;
			Sys.println(f);
			Sys.command("haxe",[f.substr(0,-3)]);
		}
	}

}