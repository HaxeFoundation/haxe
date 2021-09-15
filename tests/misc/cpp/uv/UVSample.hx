import haxe.PosInfos;

abstract class UVSample {
	static function main() {
		#if !display
		pickSample();
		#end
	}

	public function new() {}

	abstract public function run():Void;

	public function print(msg:String, ?pos:PosInfos) {
		Log.print(msg, pos);
	}

	macro static public function pickSample() {
		var sample = haxe.macro.Context.definedValue('SAMPLE');
		if(sample == null || sample == '')
			sample = Sys.getEnv('SAMPLE');
		if(sample == null || sample == '') {
			Sys.println('Add -D SAMPLE=<sample> or set environment variable SAMPLE=<sample>');
			Sys.println('\twhere <sample> is a sample name. For example: SAMPLE=Tcp');
			Sys.exit(1);
		}
		sample += 'Sample';
		var cls = switch haxe.macro.TypeTools.toComplexType(haxe.macro.Context.getType(sample)) {
			case TPath(tp): tp;
			case _: haxe.macro.Context.error('Unsupported sample type', haxe.macro.Context.currentPos());
		}
		return macro new $cls().run();
	}
}