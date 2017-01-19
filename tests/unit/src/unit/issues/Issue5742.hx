package unit.issues;

class Issue5742 extends Test {
#if sys
	function test(){
		try {
            sys.FileSystem.deleteFile('nonexistent.file');
            t(false);
        } catch(e:Dynamic) {
            t(true);
        }

        try {
            sys.FileSystem.deleteDirectory('nonexistent.directory');
            t(false);
        } catch(e:Dynamic) {
            t(true);
        }
	}
#end
}