package {
	public class Lib {
		protected var x:int = 42;
		protected function f():String { return "hello" }

		private var _i:int = 5;
		protected function get i():int { return _i }
		protected function set i(v:int):void { _i = v }

		static protected var sx:int = 42;
		static protected function sf():String { return "hello" }

		static private var _si:int = 5;
		static protected function get si():int { return _si }
		static protected function set si(v:int):void { _si = v }
	}
}
