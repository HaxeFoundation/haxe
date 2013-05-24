package tools.haxelib;

using Std;

enum Preview {
	ALPHA;
	BETA;
	RC;	
}


class SemVer {
	public var major:Int;
	public var minor:Int;	
	public var patch:Int;
	public var preview:Null<Preview>;
	public var previewNum:Null<Int>;
	public function new(major, minor, patch, ?preview, ?previewNum) {
		this.major = major;
		this.minor = minor;
		this.patch = patch;
		this.preview = preview;
		this.previewNum = previewNum;
	}
	
	public function toString():String {
		var ret = '$major.$minor.$patch';
		if (preview != null) {
			ret += '-' + preview.getName().toLowerCase();
			if (previewNum != null) 
				ret += '.' + previewNum;
		}
		return ret;
	}
	static var parse = ~/^([0-9]+)\.([0-9]+)\.([0-9]+)(-(alpha|beta|rc)(\.([0-9]+))?)?$/;
	
	static public function ofString(s:String):SemVer 
		return
			if (parse.match(s)) 
				new SemVer(
					parse.matched(1).parseInt(),
					parse.matched(2).parseInt(),
					parse.matched(3).parseInt(),
					switch parse.matched(5) {
						case 'alpha': ALPHA;
						case 'beta': BETA;
						case 'rc': RC;
						case v if (v == null): null;
						case v: throw 'unrecognized preview tag $v';
					},
					switch parse.matched(7) {
						case v if (v == null): null;
						case v: v.parseInt();
					}
				)
			else 
				throw '$s is not a valid version string';//TODO: include some URL for reference
}