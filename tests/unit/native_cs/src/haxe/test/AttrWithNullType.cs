using System;

namespace haxe.test {
	[MyAttr(null)]
	public class AttrWithNullType {}

	[MyAttr(typeof(AttrWithNullType))]
	public class AttrWithNonNullType {}

	public class MyAttrAttribute : Attribute {
		public bool check;

		public MyAttrAttribute(System.Type t) {
			check = (t == null);
		}
	}
}
