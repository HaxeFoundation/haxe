import cpp.uv.Version;
import cpp.uv.UVException;
import sys.thread.Thread;

class VersionSample extends UVSample {
	public function run() {
		print('string ' + Version.string());
		print('major ' + Version.major);
		print('minor ' + Version.minor);
		print('patch ' + Version.patch);
		print('isRelease ' + Version.isRelease);
		print('suffix "${Version.suffix}"');
		print('hex ' + Version.hex);
	}
}