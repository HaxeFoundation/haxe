package unit;

import utest.Assert;

using StringTools;

private class DefaultTPClass_y<T=String> {}
private class DefaultTPClass_yn<S=String, T> {}
private class DefaultTPClass_ny<S, T=String> {}
private class DefaultTPClass_yy<S=Int, T=String> {}

class TestDefaultTypeParameters extends Test {
	function test() {
		t(HelperMacros.typeString((null : DefaultTPClass_y)).endsWith("DefaultTPClass_y<String>"));
		t(HelperMacros.typeString((null : DefaultTPClass_y<Int>)).endsWith("DefaultTPClass_y<Int>"));
		t(HelperMacros.typeString((null : DefaultTPClass_yn<Int, String>)).endsWith("DefaultTPClass_yn<Int, String>"));
		t(HelperMacros.typeString((null : DefaultTPClass_ny<Int>)).endsWith("DefaultTPClass_ny<Int, String>"));
		t(HelperMacros.typeString((null : DefaultTPClass_ny<Int, Int>)).endsWith("DefaultTPClass_ny<Int, Int>"));
		t(HelperMacros.typeString((null : DefaultTPClass_yy)).endsWith("DefaultTPClass_yy<Int, String>"));
		t(HelperMacros.typeString((null : DefaultTPClass_yy<String>)).endsWith("DefaultTPClass_yy<String, String>"));
		t(HelperMacros.typeString((null : DefaultTPClass_yy<Int, Int>)).endsWith("DefaultTPClass_yy<Int, Int>"));
	}
}
