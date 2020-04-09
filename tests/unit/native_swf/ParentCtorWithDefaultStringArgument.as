package {
	public class ParentCtorWithDefaultStringArgument {
		public var strField:String;
		public function ParentCtorWithDefaultStringArgument(str:String = "hello") {
			strField = str;
		}
	}
}