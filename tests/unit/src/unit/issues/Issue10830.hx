package unit.issues;

import utest.Assert;

#if (cpp && !cppia)

@:native('customnamespace::MyExternEnum')
extern enum MyExternEnum {
	First;
	Second(v : String);
}

@:headerCode('
HX_DECLARE_CLASS1(customnamespace, MyExternEnum)

namespace customnamespace {
	class MyExternEnum_obj : public hx::EnumBase_obj {
	public:
		typedef MyExternEnum_obj OBJ_;

		enum Type {
			TFirst,
			TSecond
		};

		MyExternEnum_obj() = default;

		HX_DO_ENUM_RTTI;

		static bool __GetStatic(const String&, Dynamic&, hx::PropertyAccess);

		String GetEnumName() const { return HX_CSTRING("MyExternEnum"); }
		String __ToString() const { return HX_CSTRING("MyExternEnum.") + _hx_tag; }

		static MyExternEnum First();
		static Dynamic First_dyn();

		static MyExternEnum Second(String);
		static Dynamic Second_dyn();
	};
}
')
@:cppFileCode('
namespace customnamespace {
	MyExternEnum MyExternEnum_obj::First() {
		return hx::CreateEnum<MyExternEnum_obj>(HX_CSTRING("First"), Type::TFirst, 0);
	}

	MyExternEnum MyExternEnum_obj::Second(String v) {
		return hx::CreateEnum<MyExternEnum_obj>(HX_CSTRING("Second"), Type::TSecond, 1)->_hx_init(0, v);
	}

	bool MyExternEnum_obj::__GetStatic(const String& _inName, Dynamic& _outValue, hx::PropertyAccess _propAccess) {
		if (_inName == HX_CSTRING("First")) { _outValue = MyExternEnum_obj::First_dyn(); return true; }
		if (_inName == HX_CSTRING("Second")) { _outValue = MyExternEnum_obj::Second_dyn(); return true; }
		return hx::EnumBase_obj::__GetStatic(_inName, _outValue, _propAccess);
	}

	hx::Val MyExternEnum_obj::__Field(const String& _inName, hx::PropertyAccess _propAccess) {
		if (_inName == HX_CSTRING("First")) { return MyExternEnum_obj::First_dyn(); }
		if (_inName == HX_CSTRING("Second")) { return MyExternEnum_obj::Second_dyn(); }
		return hx::EnumBase_obj::__Field(_inName, _propAccess);
	}

	int MyExternEnum_obj::__FindIndex(String _inName) {
		if (_inName == HX_CSTRING("First")) { return Type::TFirst; }
		if (_inName == HX_CSTRING("Second")) { return Type::TSecond; }
		return hx::EnumBase_obj::__FindIndex(_inName);
	}

	int MyExternEnum_obj::__FindArgCount(String _inName) {
		if (_inName == HX_CSTRING("First")) { return 0; }
		if (_inName == HX_CSTRING("Second")) { return 1; }
		return hx::EnumBase_obj::__FindArgCount(_inName);
	}

	HX_DEFINE_CREATE_ENUM(MyExternEnum_obj)

	STATIC_HX_DEFINE_DYNAMIC_FUNC0(MyExternEnum_obj, First, return)

	STATIC_HX_DEFINE_DYNAMIC_FUNC1(MyExternEnum_obj, Second, return)

	hx::Class MyExternEnum_obj::__mClass;
}
')
#end
class Issue10830 extends Test {

    #if (cpp && !cppia)
    function test() {
        switch create() {
			case First:
				Assert.fail();
			case Second(v):
				Assert.equals('Hello, World!', v);
		}
    }

    function create() : MyExternEnum {
		return untyped __cpp__('::customnamespace::MyExternEnum_obj::Second(HX_CSTRING("Hello, World!"))');
	}
    #end
}