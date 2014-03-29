
import builtins as _hx_builtin

_hx_classes = dict()

class _hx_AnonObject(object):
    def __init__(self, fields):
        self.__dict__ = fields

_hx_c = _hx_AnonObject({})

_hx_c._hx_AnonObject = _hx_AnonObject

import functools as _hx_functools
import math as _hx_math
# print python.Boot.Boot
class python_Boot:

	pass




python_Boot._hx_class = python_Boot
python_Boot._hx_class_name = "python.Boot"
_hx_classes["python.Boot"] = python_Boot
_hx_c.python_Boot = python_Boot
python_Boot._hx_fields = []
python_Boot._hx_props = []
python_Boot._hx_methods = []
python_Boot._hx_statics = ["inspect","builtin","isClass","isAnonObject","_add_dynamic","__string_rec"]
python_Boot._hx_interfaces = []
def python_Boot_hx_empty_init (_hx_o):
	pass
python_Boot._hx_empty_init = python_Boot_hx_empty_init
# print python.internal.EnumImpl.Enum
class Enum:


	def __init__(self,tag,index,params):
		self.tag = None
		self.index = None
		self.params = None
		self.tag = tag
		self.index = index
		self.params = params
		
		

	# var tag
	# var index
	# var params
	def __str__(self):
		if self.params is None:
			return self.tag
		else:
			return Std.string(Std.string(Std.string(self.tag) + "(") + Std.string(",".join(_hx_builtin.list(_hx_builtin.map(Std.string, self.params))))) + ")"






Enum._hx_class = Enum
Enum._hx_class_name = "Enum"
_hx_classes["Enum"] = Enum
_hx_c.Enum = Enum
Enum._hx_fields = ["tag","index","params"]
Enum._hx_props = []
Enum._hx_methods = ["__str__"]
Enum._hx_statics = []
Enum._hx_interfaces = []
def Enum_hx_empty_init (_hx_o):
	_hx_o.tag = None
	_hx_o.index = None
	_hx_o.params = None
Enum._hx_empty_init = Enum_hx_empty_init
# print python.internal.HxOverrides.HxOverrides
class HxOverrides:

	pass




HxOverrides._hx_class = HxOverrides
HxOverrides._hx_class_name = "HxOverrides"
_hx_classes["HxOverrides"] = HxOverrides
_hx_c.HxOverrides = HxOverrides
HxOverrides._hx_fields = []
HxOverrides._hx_props = []
HxOverrides._hx_methods = []
HxOverrides._hx_statics = ["iterator","shift","filter","map","length","hx_rshift","hx_modf","hx_array_get","hx_array_set","hx_toUpperCase"]
HxOverrides._hx_interfaces = []
def HxOverrides_hx_empty_init (_hx_o):
	pass
HxOverrides._hx_empty_init = HxOverrides_hx_empty_init
# print python.internal.ArrayImpl.ArrayImpl
class python_internal_ArrayImpl:

	pass




python_internal_ArrayImpl._hx_class = python_internal_ArrayImpl
python_internal_ArrayImpl._hx_class_name = "python.internal.ArrayImpl"
_hx_classes["python.internal.ArrayImpl"] = python_internal_ArrayImpl
_hx_c.python_internal_ArrayImpl = python_internal_ArrayImpl
python_internal_ArrayImpl._hx_fields = []
python_internal_ArrayImpl._hx_props = []
python_internal_ArrayImpl._hx_methods = []
python_internal_ArrayImpl._hx_statics = ["get_length","concat","copy","iterator","indexOf","lastIndexOf","join","toString","pop","push","unshift","remove","shift","slice","sort","splice","map","filter","__get","__set","__unsafe_get","__unsafe_set"]
python_internal_ArrayImpl._hx_interfaces = []
def python_internal_ArrayImpl_hx_empty_init (_hx_o):
	pass
python_internal_ArrayImpl._hx_empty_init = python_internal_ArrayImpl_hx_empty_init
# print Array.list
# print Class.Class
class Class:

	pass



Class._hx_class = Class
Class._hx_class_name = "Class"
_hx_classes["Class"] = Class
_hx_c.Class = Class
# print Date.Date
class Date:

	pass




Date._hx_class = Date
Date._hx_class_name = "Date"
_hx_classes["Date"] = Date
_hx_c.Date = Date
Date._hx_fields = []
Date._hx_props = []
Date._hx_methods = []
Date._hx_statics = []
Date._hx_interfaces = []
def Date_hx_empty_init (_hx_o):
	pass
Date._hx_empty_init = Date_hx_empty_init
# print EReg.EReg
class EReg:


	def __init__(self,r,opt):
		self.pattern = None
		self._hx_global = None
		self._hx_global = False
		options = 0
		_g1 = 0
		_g = _hx_builtin.len(opt)
		while (_g1 < _g):
			def _hx_local_1():
				nonlocal _g1
				_hx_local_0 = _g1
				_g1 = _g1 + 1
				return _hx_local_0
				
			
			i = _hx_local_1()
			c = None
			if i >= _hx_builtin.len(opt):
				c = -1
			else:
				c = ord(HxOverrides.hx_array_get(opt, i))
			if c == 109:
				options = options | python_lib_Re.M
			
			if c == 105:
				options = options | python_lib_Re.I
			
			if c == 115:
				options = options | python_lib_Re.S
			
			if c == 117:
				options = options | python_lib_Re.U
			
			if c == 103:
				self._hx_global = True
			
		
		
		self.pattern = python_lib_Re.compile(r, options)
		
		

	# var pattern
	# var _hx_global
	def replace(self,s,by):
		by1 = None
		_this = HxString.split(by, "$$")
		by1 = "_hx_#repl#__".join(_hx_builtin.list(_hx_builtin.map(Std.string, _this)))
		
		def _hx_local_2(x):
			res = by1
			g = x.groups()
			_g1 = 0
			_g = _hx_builtin.len(g)
			while (_g1 < _g):
				def _hx_local_1():
					nonlocal _g1
					_hx_local_0 = _g1
					_g1 = _g1 + 1
					return _hx_local_0
					
				
				i = _hx_local_1()
				_this1 = None
				delimiter = "$" + Std.string(_hx_builtin.str(i + 1))
				_this1 = HxString.split(res, delimiter)
				
				res = HxOverrides.hx_array_get(g, i).join(_hx_builtin.list(_hx_builtin.map(Std.string, _this1)))
				
			
			
			_this2 = HxString.split(res, "_hx_#repl#__")
			res = "$".join(_hx_builtin.list(_hx_builtin.map(Std.string, _this2)))
			
			return res
			
		
		replace = _hx_local_2
		return python_lib_Re.sub(self.pattern, replace, s, 0 if (self._hx_global) else 1)
		






EReg._hx_class = EReg
EReg._hx_class_name = "EReg"
_hx_classes["EReg"] = EReg
_hx_c.EReg = EReg
EReg._hx_fields = ["pattern","global"]
EReg._hx_props = []
EReg._hx_methods = ["replace"]
EReg._hx_statics = []
EReg._hx_interfaces = []
def EReg_hx_empty_init (_hx_o):
	_hx_o.pattern = None
	_hx_o._hx_global = None
EReg._hx_empty_init = EReg_hx_empty_init
# print EnumValue.EnumValue
class EnumValue:

	pass



EnumValue._hx_class = EnumValue
EnumValue._hx_class_name = "EnumValue"
_hx_classes["EnumValue"] = EnumValue
_hx_c.EnumValue = EnumValue
# print List.List
class List:


	def __init__(self):
		self.h = None
		self.q = None
		self.length = None
		self.length = 0
		

	# var h
	# var q
	# var length
	def add(self,item):
		x = [item]
		if self.h is None:
			self.h = x
		else:
			HxOverrides.hx_array_set(self.q,1,x)
		self.q = x
		_hx_local_0 = self
		_hx_local_1 = _hx_local_0.length
		_hx_local_0.length = _hx_local_1 + 1
		_hx_local_1
		
		


	def iterator(self):
		h = self.h
		def _hx_local_2():
			def _hx_local_0():
				return h is not None
			
			def _hx_local_1():
				nonlocal h
				if h is None:
					return None
				
				x = HxOverrides.hx_array_get(h, 0)
				h = HxOverrides.hx_array_get(h, 1)
				return x
				
			
			return _hx_c._hx_AnonObject({'hasNext': _hx_local_0, 'next': _hx_local_1})
			
		
		return _hx_local_2()
		






List._hx_class = List
List._hx_class_name = "List"
_hx_classes["List"] = List
_hx_c.List = List
List._hx_fields = ["h","q","length"]
List._hx_props = []
List._hx_methods = ["add","iterator"]
List._hx_statics = []
List._hx_interfaces = []
def List_hx_empty_init (_hx_o):
	_hx_o.h = None
	_hx_o.q = None
	_hx_o.length = None
List._hx_empty_init = List_hx_empty_init
# print Main.Main
class Main:

	pass




Main._hx_class = Main
Main._hx_class_name = "Main"
_hx_classes["Main"] = Main
_hx_c.Main = Main
Main._hx_fields = []
Main._hx_props = []
Main._hx_methods = []
Main._hx_statics = ["main"]
Main._hx_interfaces = []
def Main_hx_empty_init (_hx_o):
	pass
Main._hx_empty_init = Main_hx_empty_init
# print Map.IMap
class IMap:

	pass




IMap._hx_class = IMap
IMap._hx_class_name = "IMap"
_hx_classes["IMap"] = IMap
_hx_c.IMap = IMap
IMap._hx_fields = []
IMap._hx_props = []
IMap._hx_methods = []
IMap._hx_statics = []
IMap._hx_interfaces = []
def IMap_hx_empty_init (_hx_o):
	pass
IMap._hx_empty_init = IMap_hx_empty_init
# print Math._hx_math
# print Reflect.Reflect
class Reflect:

	pass




Reflect._hx_class = Reflect
Reflect._hx_class_name = "Reflect"
_hx_classes["Reflect"] = Reflect
_hx_c.Reflect = Reflect
Reflect._hx_fields = []
Reflect._hx_props = []
Reflect._hx_methods = []
Reflect._hx_statics = ["field","callMethod","fields","isFunction"]
Reflect._hx_interfaces = []
def Reflect_hx_empty_init (_hx_o):
	pass
Reflect._hx_empty_init = Reflect_hx_empty_init
# print Std.Std
class Std:

	pass




Std._hx_class = Std
Std._hx_class_name = "Std"
_hx_classes["Std"] = Std
_hx_c.Std = Std
Std._hx_fields = []
Std._hx_props = []
Std._hx_methods = []
Std._hx_statics = ["is","string"]
Std._hx_interfaces = []
def Std_hx_empty_init (_hx_o):
	pass
Std._hx_empty_init = Std_hx_empty_init
# print StdTypes.Void
class Void:

	pass



Void._hx_class = Void
Void._hx_class_name = "Void"
_hx_classes["Void"] = Void
_hx_c.Void = Void
# print StdTypes.Float
class Float:

	pass



Float._hx_class = Float
Float._hx_class_name = "Float"
_hx_classes["Float"] = Float
_hx_c.Float = Float
# print StdTypes.Int
class Int:

	pass



Int._hx_class = Int
Int._hx_class_name = "Int"
_hx_classes["Int"] = Int
_hx_c.Int = Int
# print StdTypes.Bool
class Bool:

	pass



Bool._hx_class = Bool
Bool._hx_class_name = "Bool"
_hx_classes["Bool"] = Bool
_hx_c.Bool = Bool
# print StdTypes.Dynamic
class Dynamic:

	pass



Dynamic._hx_class = Dynamic
Dynamic._hx_class_name = "Dynamic"
_hx_classes["Dynamic"] = Dynamic
_hx_c.Dynamic = Dynamic
# print StdTypes.ArrayAccess
# print python.internal.StringImpl.HxString
class HxString:

	pass




HxString._hx_class = HxString
HxString._hx_class_name = "HxString"
_hx_classes["HxString"] = HxString
_hx_c.HxString = HxString
HxString._hx_fields = []
HxString._hx_props = []
HxString._hx_methods = []
HxString._hx_statics = ["split","charCodeAt","charAt","lastIndexOf","fromCharCode"]
HxString._hx_interfaces = []
def HxString_hx_empty_init (_hx_o):
	pass
HxString._hx_empty_init = HxString_hx_empty_init
# print String.String
# print StringBuf.StringBuf
class StringBuf:


	def __init__(self):
		self.b = None
		self.b = python_lib_io_StringIO()
		

	# var b




StringBuf._hx_class = StringBuf
StringBuf._hx_class_name = "StringBuf"
_hx_classes["StringBuf"] = StringBuf
_hx_c.StringBuf = StringBuf
StringBuf._hx_fields = ["b"]
StringBuf._hx_props = []
StringBuf._hx_methods = []
StringBuf._hx_statics = []
StringBuf._hx_interfaces = []
def StringBuf_hx_empty_init (_hx_o):
	_hx_o.b = None
StringBuf._hx_empty_init = StringBuf_hx_empty_init
# print StringTools.StringTools
class StringTools:

	pass




StringTools._hx_class = StringTools
StringTools._hx_class_name = "StringTools"
_hx_classes["StringTools"] = StringTools
_hx_c.StringTools = StringTools
StringTools._hx_fields = []
StringTools._hx_props = []
StringTools._hx_methods = []
StringTools._hx_statics = ["startsWith"]
StringTools._hx_interfaces = []
def StringTools_hx_empty_init (_hx_o):
	pass
StringTools._hx_empty_init = StringTools_hx_empty_init
# print haxe.ds.StringMap.StringMap
class haxe_ds_StringMap:


	def __init__(self):
		self.h = None
		self.h = {}
		

	# var h
	def set(self,key,value):
		self.h["$" + Std.string(key)] = value


	def get(self,key):
		return self.h.get("$" + Std.string(key), None)


	def exists(self,key):
		return "$" + Std.string(key) in self.h






haxe_ds_StringMap._hx_class = haxe_ds_StringMap
haxe_ds_StringMap._hx_class_name = "haxe.ds.StringMap"
_hx_classes["haxe.ds.StringMap"] = haxe_ds_StringMap
_hx_c.haxe_ds_StringMap = haxe_ds_StringMap
haxe_ds_StringMap._hx_fields = ["h"]
haxe_ds_StringMap._hx_props = []
haxe_ds_StringMap._hx_methods = ["set","get","exists"]
haxe_ds_StringMap._hx_statics = []
haxe_ds_StringMap._hx_interfaces = [IMap]
def haxe_ds_StringMap_hx_empty_init (_hx_o):
	_hx_o.h = None
haxe_ds_StringMap._hx_empty_init = haxe_ds_StringMap_hx_empty_init
# print python.lib.Os.Os
# print python.lib.Builtin._hx_builtin
# print python.Lib.HaxeIterator
class python_HaxeIterator:


	def __init__(self,it):
		self.it = None
		self.x = None
		self.has = None
		self.checked = None
		self.checked = False
		self.has = False
		self.x = None
		self.it = it
		
		

	# var it
	# var x
	# var has
	# var checked
	def next(self):
		self.checked = False
		return self.x
		


	def hasNext(self):
		if self.checked:
			return self.has
		else:
			try:
				self.x = self.it.__next__()
				self.has = True
			
			except Exception as _hx_e:
				_hx_e1 = _hx_e.val if isinstance(_hx_e, _HxException) else _hx_e
				if isinstance(_hx_e1, StopIteration):
					s = _hx_e1
					self.has = False
					self.x = None
			
				else:
					raise _hx_e
			self.checked = True
			return self.has
		






python_HaxeIterator._hx_class = python_HaxeIterator
python_HaxeIterator._hx_class_name = "python.HaxeIterator"
_hx_classes["python.HaxeIterator"] = python_HaxeIterator
_hx_c.python_HaxeIterator = python_HaxeIterator
python_HaxeIterator._hx_fields = ["it","x","has","checked"]
python_HaxeIterator._hx_props = []
python_HaxeIterator._hx_methods = ["next","hasNext"]
python_HaxeIterator._hx_statics = []
python_HaxeIterator._hx_interfaces = []
def python_HaxeIterator_hx_empty_init (_hx_o):
	_hx_o.it = None
	_hx_o.x = None
	_hx_o.has = None
	_hx_o.checked = None
python_HaxeIterator._hx_empty_init = python_HaxeIterator_hx_empty_init
import sys# print Sys.Sys
class Sys:

	pass




Sys._hx_class = Sys
Sys._hx_class_name = "Sys"
_hx_classes["Sys"] = Sys
_hx_c.Sys = Sys
Sys._hx_fields = []
Sys._hx_props = []
Sys._hx_methods = []
Sys._hx_statics = ["environ","println","args","getEnv","putEnv","environment","getCwd","setCwd"]
Sys._hx_interfaces = []
def Sys_hx_empty_init (_hx_o):
	pass
Sys._hx_empty_init = Sys_hx_empty_init
# print haxe.unit.TestCase.TestCase
class haxe_unit_TestCase:


	def __init__(self):
		self.currentTest = None
		None
		

	# var currentTest
	def setup(self):
		None


	def tearDown(self):
		None


	def _hx_print(self,v):
		haxe_unit_TestRunner._hx_print(v)


	def assertTrue(self,b,c = None):
		if c is None:
			c = None
		
		self.currentTest.done = True
		if b == False:
			self.currentTest.success = False
			self.currentTest.error = "expected true but was false"
			self.currentTest.posInfos = c
			raise _HxException(self.currentTest)
		
		
		


	def assertFalse(self,b,c = None):
		if c is None:
			c = None
		
		self.currentTest.done = True
		if b == True:
			self.currentTest.success = False
			self.currentTest.error = "expected false but was true"
			self.currentTest.posInfos = c
			raise _HxException(self.currentTest)
		
		
		


	def assertEquals(self,expected,actual,c = None):
		if c is None:
			c = None
		
		self.currentTest.done = True
		if actual != expected:
			self.currentTest.success = False
			self.currentTest.error = Std.string(Std.string(Std.string("expected '" + Std.string(Std.string(expected))) + "' but was '") + Std.string(Std.string(actual))) + "'"
			self.currentTest.posInfos = c
			raise _HxException(self.currentTest)
		
		
		






haxe_unit_TestCase._hx_class = haxe_unit_TestCase
haxe_unit_TestCase._hx_class_name = "haxe.unit.TestCase"
_hx_classes["haxe.unit.TestCase"] = haxe_unit_TestCase
_hx_c.haxe_unit_TestCase = haxe_unit_TestCase
haxe_unit_TestCase._hx_fields = ["currentTest"]
haxe_unit_TestCase._hx_props = []
haxe_unit_TestCase._hx_methods = ["setup","tearDown","print","assertTrue","assertFalse","assertEquals"]
haxe_unit_TestCase._hx_statics = []
haxe_unit_TestCase._hx_interfaces = []
def haxe_unit_TestCase_hx_empty_init (_hx_o):
	_hx_o.currentTest = None
haxe_unit_TestCase._hx_empty_init = haxe_unit_TestCase_hx_empty_init
# print TestSys.TestSys
class TestSys(haxe_unit_TestCase):


	def __init__(self):
		super().__init__()

	def testArgs(self):
		args = Sys.args()
		self.assertEquals(3, _hx_builtin.len(args), _hx_c._hx_AnonObject({'customParams': None, 'fileName': "TestSys.hx", 'lineNumber': 4, 'className': "TestSys", 'methodName': "testArgs"}))
		self.assertEquals("foo", HxOverrides.hx_array_get(args, 0), _hx_c._hx_AnonObject({'customParams': None, 'fileName': "TestSys.hx", 'lineNumber': 5, 'className': "TestSys", 'methodName': "testArgs"}))
		self.assertEquals("12", HxOverrides.hx_array_get(args, 1), _hx_c._hx_AnonObject({'customParams': None, 'fileName': "TestSys.hx", 'lineNumber': 6, 'className': "TestSys", 'methodName': "testArgs"}))
		self.assertEquals("a b c\\", HxOverrides.hx_array_get(args, 2), _hx_c._hx_AnonObject({'customParams': None, 'fileName': "TestSys.hx", 'lineNumber': 7, 'className': "TestSys", 'methodName': "testArgs"}))
		


	def testEnv(self):
		Sys.putEnv("foo", "value")
		self.assertEquals("value", Sys.getEnv("foo"), _hx_c._hx_AnonObject({'customParams': None, 'fileName': "TestSys.hx", 'lineNumber': 12, 'className': "TestSys", 'methodName': "testEnv"}))
		self.assertEquals(None, Sys.getEnv("doesn't exist"), _hx_c._hx_AnonObject({'customParams': None, 'fileName': "TestSys.hx", 'lineNumber': 13, 'className': "TestSys", 'methodName': "testEnv"}))
		env = Sys.environment()
		self.assertEquals("value", env.get("foo"), _hx_c._hx_AnonObject({'customParams': None, 'fileName': "TestSys.hx", 'lineNumber': 16, 'className': "TestSys", 'methodName': "testEnv"}))
		


	def testCwd(self):
		cur = Sys.getCwd()
		Sys.setCwd("../")
		newCwd = haxe_io_Path.join([cur, "../"])
		def _hx_local_0(path):
			return haxe_io_Path.addTrailingSlash(haxe_io_Path.normalize(path))
		
		normalize = _hx_local_0
		self.assertEquals(normalize(newCwd), normalize(Sys.getCwd()), _hx_c._hx_AnonObject({'customParams': None, 'fileName': "TestSys.hx", 'lineNumber': 26, 'className': "TestSys", 'methodName': "testCwd"}))
		






TestSys._hx_class = TestSys
TestSys._hx_class_name = "TestSys"
_hx_classes["TestSys"] = TestSys
_hx_c.TestSys = TestSys
TestSys._hx_fields = []
TestSys._hx_props = []
TestSys._hx_methods = ["testArgs","testEnv","testCwd"]
TestSys._hx_statics = []
TestSys._hx_interfaces = []
TestSys._hx_super = haxe_unit_TestCase
def TestSys_hx_empty_init (_hx_o):
	pass
TestSys._hx_empty_init = TestSys_hx_empty_init
# print Type.Type
class Type:

	pass




Type._hx_class = Type
Type._hx_class_name = "Type"
_hx_classes["Type"] = Type
_hx_c.Type = Type
Type._hx_fields = []
Type._hx_props = []
Type._hx_methods = []
Type._hx_statics = ["getClass","getSuperClass","getClassName","getInstanceFields","getClassFields"]
Type._hx_interfaces = []
def Type_hx_empty_init (_hx_o):
	pass
Type._hx_empty_init = Type_hx_empty_init
class haxe_StackItem(_hx_c.Enum):
	def __init__(self, t, i, p):
		super(haxe_StackItem,self).__init__(t, i, p)
haxe_StackItem.CFunction = haxe_StackItem("CFunction", 0, list())

def _haxe_StackItem_statics_FilePos (s,file,line):
	return haxe_StackItem("FilePos", 2, [s,file,line])
haxe_StackItem.FilePos = _haxe_StackItem_statics_FilePos

def _haxe_StackItem_statics_LocalFunction (v):
	return haxe_StackItem("LocalFunction", 4, [v])
haxe_StackItem.LocalFunction = _haxe_StackItem_statics_LocalFunction

def _haxe_StackItem_statics_Method (classname,method):
	return haxe_StackItem("Method", 3, [classname,method])
haxe_StackItem.Method = _haxe_StackItem_statics_Method

def _haxe_StackItem_statics_Module (m):
	return haxe_StackItem("Module", 1, [m])
haxe_StackItem.Module = _haxe_StackItem_statics_Module

haxe_StackItem._hx_constructs = ["CFunction","Module","FilePos","Method","LocalFunction"]
haxe_StackItem._hx_class = haxe_StackItem
haxe_StackItem._hx_class_name = "haxe.StackItem"
_hx_classes["haxe.StackItem"] = haxe_StackItem
_hx_c.haxe_StackItem = haxe_StackItem
# print haxe.CallStack.CallStack
class haxe_CallStack:

	pass




haxe_CallStack._hx_class = haxe_CallStack
haxe_CallStack._hx_class_name = "haxe.CallStack"
_hx_classes["haxe.CallStack"] = haxe_CallStack
_hx_c.haxe_CallStack = haxe_CallStack
haxe_CallStack._hx_fields = []
haxe_CallStack._hx_props = []
haxe_CallStack._hx_methods = []
haxe_CallStack._hx_statics = ["exceptionStack","toString","itemToString"]
haxe_CallStack._hx_interfaces = []
def haxe_CallStack_hx_empty_init (_hx_o):
	pass
haxe_CallStack._hx_empty_init = haxe_CallStack_hx_empty_init
# print haxe.EnumTools.EnumTools
# print haxe.EnumTools.EnumValueTools
# print haxe.Log.Log
class haxe_Log:

	pass




haxe_Log._hx_class = haxe_Log
haxe_Log._hx_class_name = "haxe.Log"
_hx_classes["haxe.Log"] = haxe_Log
_hx_c.haxe_Log = haxe_Log
haxe_Log._hx_fields = []
haxe_Log._hx_props = []
haxe_Log._hx_methods = []
haxe_Log._hx_statics = ["trace"]
haxe_Log._hx_interfaces = []
def haxe_Log_hx_empty_init (_hx_o):
	pass
haxe_Log._hx_empty_init = haxe_Log_hx_empty_init
# print haxe.io.Eof.Eof
class haxe_io_Eof:

	def toString(self):
		return "Eof"






haxe_io_Eof._hx_class = haxe_io_Eof
haxe_io_Eof._hx_class_name = "haxe.io.Eof"
_hx_classes["haxe.io.Eof"] = haxe_io_Eof
_hx_c.haxe_io_Eof = haxe_io_Eof
haxe_io_Eof._hx_fields = []
haxe_io_Eof._hx_props = []
haxe_io_Eof._hx_methods = ["toString"]
haxe_io_Eof._hx_statics = []
haxe_io_Eof._hx_interfaces = []
def haxe_io_Eof_hx_empty_init (_hx_o):
	pass
haxe_io_Eof._hx_empty_init = haxe_io_Eof_hx_empty_init
# print haxe.io.Path.Path
class haxe_io_Path:

	pass




haxe_io_Path._hx_class = haxe_io_Path
haxe_io_Path._hx_class_name = "haxe.io.Path"
_hx_classes["haxe.io.Path"] = haxe_io_Path
_hx_c.haxe_io_Path = haxe_io_Path
haxe_io_Path._hx_fields = []
haxe_io_Path._hx_props = []
haxe_io_Path._hx_methods = []
haxe_io_Path._hx_statics = ["join","normalize","addTrailingSlash"]
haxe_io_Path._hx_interfaces = []
def haxe_io_Path_hx_empty_init (_hx_o):
	pass
haxe_io_Path._hx_empty_init = haxe_io_Path_hx_empty_init
# print haxe.unit.TestResult.TestResult
class haxe_unit_TestResult:


	def __init__(self):
		self.m_tests = None
		self.success = None
		self.m_tests = List()
		self.success = True
		
		

	# var m_tests
	# var success
	def add(self,t):
		self.m_tests.add(t)
		if not t.success:
			self.success = False
		
		


	def toString(self):
		buf = StringBuf()
		failures = 0
		_it = self.m_tests.iterator()
		while _it.hasNext():
			test = _it.next()
			if test.success == False:
				s = "* "
				buf.b.write(s)
				
				s1 = Std.string(test.classname)
				buf.b.write(s1)
				
				s2 = "::"
				buf.b.write(s2)
				
				s3 = Std.string(test.method)
				buf.b.write(s3)
				
				s4 = "()"
				buf.b.write(s4)
				
				s5 = "\n"
				buf.b.write(s5)
				
				s6 = "ERR: "
				buf.b.write(s6)
				
				if test.posInfos is not None:
					s7 = Std.string(test.posInfos.fileName)
					buf.b.write(s7)
					
					s8 = ":"
					buf.b.write(s8)
					
					s9 = Std.string(test.posInfos.lineNumber)
					buf.b.write(s9)
					
					s10 = "("
					buf.b.write(s10)
					
					s11 = Std.string(test.posInfos.className)
					buf.b.write(s11)
					
					s12 = "."
					buf.b.write(s12)
					
					s13 = Std.string(test.posInfos.methodName)
					buf.b.write(s13)
					
					s14 = ") - "
					buf.b.write(s14)
					
				
				
				s15 = Std.string(test.error)
				buf.b.write(s15)
				
				s16 = "\n"
				buf.b.write(s16)
				
				if test.backtrace is not None:
					s17 = Std.string(test.backtrace)
					buf.b.write(s17)
					
					s18 = "\n"
					buf.b.write(s18)
					
				
				
				s19 = "\n"
				buf.b.write(s19)
				
				failures = failures + 1
		
			
		s20 = "\n"
		buf.b.write(s20)
		
		if failures == 0:
			s21 = "OK "
			buf.b.write(s21)
		
		else:
			s22 = "FAILED "
			buf.b.write(s22)
		
		s23 = Std.string(self.m_tests.length)
		buf.b.write(s23)
		
		s24 = " tests, "
		buf.b.write(s24)
		
		s25 = Std.string(failures)
		buf.b.write(s25)
		
		s26 = " failed, "
		buf.b.write(s26)
		
		s27 = Std.string(self.m_tests.length - failures)
		buf.b.write(s27)
		
		s28 = " success"
		buf.b.write(s28)
		
		s29 = "\n"
		buf.b.write(s29)
		
		return buf.b.getvalue()
		






haxe_unit_TestResult._hx_class = haxe_unit_TestResult
haxe_unit_TestResult._hx_class_name = "haxe.unit.TestResult"
_hx_classes["haxe.unit.TestResult"] = haxe_unit_TestResult
_hx_c.haxe_unit_TestResult = haxe_unit_TestResult
haxe_unit_TestResult._hx_fields = ["m_tests","success"]
haxe_unit_TestResult._hx_props = []
haxe_unit_TestResult._hx_methods = ["add","toString"]
haxe_unit_TestResult._hx_statics = []
haxe_unit_TestResult._hx_interfaces = []
def haxe_unit_TestResult_hx_empty_init (_hx_o):
	_hx_o.m_tests = None
	_hx_o.success = None
haxe_unit_TestResult._hx_empty_init = haxe_unit_TestResult_hx_empty_init
# print haxe.unit.TestRunner.TestRunner
class haxe_unit_TestRunner:


	def __init__(self):
		self.result = None
		self.cases = None
		self.result = haxe_unit_TestResult()
		self.cases = List()
		
		

	# var result
	# var cases
	def add(self,c):
		self.cases.add(c)


	def run(self):
		self.result = haxe_unit_TestResult()
		_it = self.cases.iterator()
		while _it.hasNext():
			c = _it.next()
			self.runCase(c)
		haxe_unit_TestRunner._hx_print(self.result.toString())
		return self.result.success
		


	def runCase(self,t):
		old = haxe_Log.trace
		haxe_Log.trace = haxe_unit_TestRunner.customTrace
		cl = Type.getClass(t)
		fields = Type.getInstanceFields(cl)
		haxe_unit_TestRunner._hx_print(Std.string("Class: " + Std.string(Type.getClassName(cl))) + " ")
		_g = 0
		while (_g < _hx_builtin.len(fields)):
			f = HxOverrides.hx_array_get(fields, _g)
			_g = _g + 1
			fname = f
			field = Reflect.field(t, f)
			if StringTools.startsWith(fname, "test") and Reflect.isFunction(field):
				t.currentTest = haxe_unit_TestStatus()
				t.currentTest.classname = Type.getClassName(cl)
				t.currentTest.method = fname
				t.setup()
				try:
					Reflect.callMethod(t, field, list())
					if t.currentTest.done:
						t.currentTest.success = True
						haxe_unit_TestRunner._hx_print(".")
					
					else:
						t.currentTest.success = False
						t.currentTest.error = "(warning) no assert"
						haxe_unit_TestRunner._hx_print("W")
					
				
				except Exception as _hx_e:
					_hx_e1 = _hx_e.val if isinstance(_hx_e, _HxException) else _hx_e
					if isinstance(_hx_e1, haxe_unit_TestStatus):
						e = _hx_e1
						haxe_unit_TestRunner._hx_print("F")
						t.currentTest.backtrace = haxe_CallStack.toString(haxe_CallStack.exceptionStack())
				
				
					if True:
						e1 = _hx_e1
						haxe_unit_TestRunner._hx_print("E")
						t.currentTest.error = "exception thrown : " + Std.string(Std.string(e1))
						t.currentTest.backtrace = haxe_CallStack.toString(haxe_CallStack.exceptionStack())
				
					else:
						raise _hx_e
				self.result.add(t.currentTest)
				t.tearDown()
			
			
		
		
		haxe_unit_TestRunner._hx_print("\n")
		haxe_Log.trace = old
		






haxe_unit_TestRunner._hx_class = haxe_unit_TestRunner
haxe_unit_TestRunner._hx_class_name = "haxe.unit.TestRunner"
_hx_classes["haxe.unit.TestRunner"] = haxe_unit_TestRunner
_hx_c.haxe_unit_TestRunner = haxe_unit_TestRunner
haxe_unit_TestRunner._hx_fields = ["result","cases"]
haxe_unit_TestRunner._hx_props = []
haxe_unit_TestRunner._hx_methods = ["add","run","runCase"]
haxe_unit_TestRunner._hx_statics = ["print","customTrace"]
haxe_unit_TestRunner._hx_interfaces = []
def haxe_unit_TestRunner_hx_empty_init (_hx_o):
	_hx_o.result = None
	_hx_o.cases = None
haxe_unit_TestRunner._hx_empty_init = haxe_unit_TestRunner_hx_empty_init
# print haxe.unit.TestStatus.TestStatus
class haxe_unit_TestStatus:


	def __init__(self):
		self.done = None
		self.success = None
		self.error = None
		self.method = None
		self.classname = None
		self.posInfos = None
		self.backtrace = None
		self.done = False
		self.success = False
		
		

	# var done
	# var success
	# var error
	# var method
	# var classname
	# var posInfos
	# var backtrace




haxe_unit_TestStatus._hx_class = haxe_unit_TestStatus
haxe_unit_TestStatus._hx_class_name = "haxe.unit.TestStatus"
_hx_classes["haxe.unit.TestStatus"] = haxe_unit_TestStatus
_hx_c.haxe_unit_TestStatus = haxe_unit_TestStatus
haxe_unit_TestStatus._hx_fields = ["done","success","error","method","classname","posInfos","backtrace"]
haxe_unit_TestStatus._hx_props = []
haxe_unit_TestStatus._hx_methods = []
haxe_unit_TestStatus._hx_statics = []
haxe_unit_TestStatus._hx_interfaces = []
def haxe_unit_TestStatus_hx_empty_init (_hx_o):
	_hx_o.done = None
	_hx_o.success = None
	_hx_o.error = None
	_hx_o.method = None
	_hx_o.classname = None
	_hx_o.posInfos = None
	_hx_o.backtrace = None
haxe_unit_TestStatus._hx_empty_init = haxe_unit_TestStatus_hx_empty_init
# print python.Lib.Lib
class python_Lib:

	pass




python_Lib._hx_class = python_Lib
python_Lib._hx_class_name = "python.Lib"
_hx_classes["python.Lib"] = python_Lib
_hx_c.python_Lib = python_Lib
python_Lib._hx_fields = []
python_Lib._hx_props = []
python_Lib._hx_methods = []
python_Lib._hx_statics = ["print"]
python_Lib._hx_interfaces = []
def python_Lib_hx_empty_init (_hx_o):
	pass
python_Lib._hx_empty_init = python_Lib_hx_empty_init
# print python.Tools.Tools
class python_Tools:

	pass




python_Tools._hx_class = python_Tools
python_Tools._hx_class_name = "python.Tools"
_hx_classes["python.Tools"] = python_Tools
_hx_c.python_Tools = python_Tools
python_Tools._hx_fields = []
python_Tools._hx_props = []
python_Tools._hx_methods = []
python_Tools._hx_statics = ["substr"]
python_Tools._hx_interfaces = []
def python_Tools_hx_empty_init (_hx_o):
	pass
python_Tools._hx_empty_init = python_Tools_hx_empty_init
# print python.lib.Types.BaseException
# print python.lib.Types.Exception
# print python.internal.HxException._HxException
class _HxException(Exception):


	def __init__(self,val):
		self.val = None
		message = Std.string(val)
		super().__init__(message)
		self.val = val
		
		

	# var val




_HxException._hx_class = _HxException
_HxException._hx_class_name = "_HxException"
_hx_classes["_HxException"] = _HxException
_hx_c._HxException = _HxException
_HxException._hx_fields = ["val"]
_HxException._hx_props = []
_HxException._hx_methods = []
_HxException._hx_statics = []
_HxException._hx_interfaces = []
_HxException._hx_super = Exception
def _HxException_hx_empty_init (_hx_o):
	_hx_o.val = None
_HxException._hx_empty_init = _HxException_hx_empty_init
# print python.internal.KeywordHandler.KeywordHandler
class python_internal_KeywordHandler:

	pass




python_internal_KeywordHandler._hx_class = python_internal_KeywordHandler
python_internal_KeywordHandler._hx_class_name = "python.internal.KeywordHandler"
_hx_classes["python.internal.KeywordHandler"] = python_internal_KeywordHandler
_hx_c.python_internal_KeywordHandler = python_internal_KeywordHandler
python_internal_KeywordHandler._hx_fields = []
python_internal_KeywordHandler._hx_props = []
python_internal_KeywordHandler._hx_methods = []
python_internal_KeywordHandler._hx_statics = ["keywords","unhandleKeywords"]
python_internal_KeywordHandler._hx_interfaces = []
def python_internal_KeywordHandler_hx_empty_init (_hx_o):
	pass
python_internal_KeywordHandler._hx_empty_init = python_internal_KeywordHandler_hx_empty_init
# print python.lib.FuncTools._hx_functools
# print python.lib.Inspect.Inspect
# print python.lib.Os.Stat
# print python.lib.Random.Random
# print python.lib.Re.MatchObject
# print python.lib.Re.Regex
# print python.lib.Re.Re
# print python.lib.Subprocess.StartupInfo
# print python.lib.Subprocess.Subprocess
# print python.lib.Sys.Sys
# print python.lib.Time.Time
# print python.lib.Types.ByteArray
# print python.lib.Types.Bytes
# print python.lib.Types.FileDescriptor
# print python.lib.Types.Set
# print python.lib.Types.DictView
# print python.lib.Types.Dict
# print python.lib.Types.Tuple
# print python.lib.Types.Tup2
# print python.lib.Types.Tup3
# print python.lib.Types.Tup4
# print python.lib.Types.Tup5
# print python.lib.Types.BufferError
# print python.lib.Types.GeneratorExit
# print python.lib.Types.KeyboardInterrupt
# print python.lib.Types.SyntaxError
# print python.lib.Types.StopIteration
# print python.lib.Types.RuntimeError
# print python.lib.Types.NotImplementedError
# print python.lib.Types.IndentationError
# print python.lib.Types.EnvironmentError
# print python.lib.Types.OSError
# print python.lib.Types.BlockingIOError
# print python.lib.Types.ChildProcessError
# print python.lib.Types.ConnectionError
# print python.lib.Types.BrokenPipeError
# print python.lib.Types.ConnectionAbortedError
# print python.lib.Types.ConnectionRefusedError
# print python.lib.Types.ConnectionResetError
# print python.lib.Types.FileExistsError
# print python.lib.Types.FileNotFoundError
# print python.lib.Types.InterruptedError
# print python.lib.Types.IsADirectoryError
# print python.lib.Types.NotADirectoryError
# print python.lib.Types.PermissionError
# print python.lib.Types.ProcessLookupError
# print python.lib.Types.TimeoutError
# print python.lib.Types.NameError
# print python.lib.Types.UnboundLocalError
# print python.lib.Types.MemoryError
# print python.lib.Types.AssertionError
# print python.lib.Types.AttributeError
# print python.lib.Types.EOFError
# print python.lib.Types.ArithmeticError
# print python.lib.Types.FloatingPointError
# print python.lib.Types.OverflowError
# print python.lib.Types.ZeroDivisionError
# print python.lib.Types.ImportError
# print python.lib.Types.LookupError
# print python.lib.Types.IndexError
# print python.lib.Types.KeyError
# print python.lib.Types.IOError
# print python.lib.Types.VMSError
# print python.lib.Types.WindowsError
# print python.lib.Types.ValueError
# print python.lib.Types.UnicodeError
# print python.lib.Types.UnicodeDecodeError
# print python.lib.Types.UnicodeEncodeError
# print python.lib.Types.UnicodeTranslateError
# print python.lib.Types.Warning
# print python.lib.Types.DeprecationWarning
# print python.lib.Types.PendingDeprecationWarning
# print python.lib.Types.RuntimeWarning
# print python.lib.Types.SyntaxWarning
# print python.lib.Types.UserWarning
# print python.lib.Types.FutureWarning
# print python.lib.Types.ImportWarning
# print python.lib.Types.UnicodeWarning
# print python.lib.Types.BytesWarning
# print python.lib.Types.ResourceWarning
# print python.lib.datetime.DateTime.DateTime
# print python.lib.datetime.TimeDelta.TimeDelta
# print python.lib.datetime.TzInfo.TzInfo
# print python.lib.datetime.Timezone.Timezone
# print python.lib.io.IOBase.IOBase
# print python.lib.io.RawIOBase.RawIOBase
# print python.lib.io.FileIO.FileIO
# print python.lib.io.TextIOBase.TextIOBase
# print python.lib.io.StringIO.StringIO
python_Boot.inspect = None;
python_Boot.builtin = None;
def Boot_statics_isClass(o):
	return o is not None and (o == String or python_lib_Inspect.isclass(o))

python_Boot.isClass = Boot_statics_isClass
def Boot_statics_isAnonObject(o):
	return _hx_builtin.isinstance(o, _hx_c._hx_AnonObject)

python_Boot.isAnonObject = Boot_statics_isAnonObject
def Boot_statics__add_dynamic(a,b):
	if python_Boot.builtin.isinstance(a, str) or python_Boot.builtin.isinstance(b, str):
		return Std.string(python_Boot.__string_rec(a, "")) + Std.string(python_Boot.__string_rec(b, ""))
	
	return a+b
	

python_Boot._add_dynamic = Boot_statics__add_dynamic
def Boot_statics___string_rec(o,s):
	if s is None:
		s = ""
	
	if o is None:
		return "null"
	
	if _hx_builtin.len(s) >= 5:
		return "<...>"
	
	if python_Boot.builtin.isinstance(o, str):
		return o
	
	if python_Boot.builtin.isinstance(o, bool):
		if o:
			return "true"
		else:
			return "false"
	
	if python_Boot.builtin.isinstance(o, int):
		return python_Boot.builtin.str(o)
	
	if python_Boot.builtin.isinstance(o, float):
		try:
			if o == _hx_builtin.int(o):
				def _hx_local_1():
					def _hx_local_0():
						v = o
						return _hx_math.floor(v + 0.5)
						
					
					return python_Boot.builtin.str(_hx_local_0())
					
				
				return _hx_local_1()
	
			else:
				return python_Boot.builtin.str(o)
		except Exception as _hx_e:
			_hx_e1 = _hx_e.val if isinstance(_hx_e, _HxException) else _hx_e
			if True:
				e = _hx_e1
				return python_Boot.builtin.str(o)
			else:
				raise _hx_e
	
	if python_Boot.inspect.isfunction(o) or python_Boot.inspect.ismethod(o):
		return "<function>"
	
	if python_Boot.builtin.isinstance(o, list):
		o1 = o
		l = _hx_builtin.len(o1)
		st = "["
		s = Std.string(s) + "\t"
		_g = 0
		while (_g < l):
			def _hx_local_4():
				nonlocal _g
				_hx_local_3 = _g
				_g = _g + 1
				return _hx_local_3
				
			
			i = _hx_local_4()
			prefix = ""
			if i > 0:
				prefix = ","
			
			st = Std.string(st) + Std.string(Std.string(prefix) + Std.string(python_Boot.__string_rec(HxOverrides.hx_array_get(o1, i), s)))
		
		
		st = Std.string(st) + "]"
		return st
	
	
	try:
		if python_Boot.builtin.hasattr(o, "toString"):
			return o.toString()
		
	except Exception as _hx_e:
		_hx_e1 = _hx_e.val if isinstance(_hx_e, _HxException) else _hx_e
		if True:
			e1 = _hx_e1
			None
		else:
			raise _hx_e
	if python_Boot.builtin.hasattr(o, "__class__"):
		if python_Boot.builtin.isinstance(o, _hx_c._hx_AnonObject):
			toStr = None
			try:
				fields = Reflect.fields(o)
				fieldsStr = None
				_g1 = []
				_g11 = 0
				while (_g11 < _hx_builtin.len(fields)):
					f = HxOverrides.hx_array_get(fields, _g11)
					_g11 = _g11 + 1
					x = Std.string(Std.string("" + Std.string(f)) + " : ") + Std.string(python_Boot.__string_rec(Reflect.field(o, f), Std.string(s) + "\t"))
					_g1.append(x)
					_hx_builtin.len(_g1)
					
				
				
				fieldsStr = _g1
				
				toStr = Std.string("{ " + Std.string(", ".join(_hx_builtin.list(_hx_builtin.map(Std.string, fieldsStr))))) + " }"
			
			except Exception as _hx_e:
				_hx_e1 = _hx_e.val if isinstance(_hx_e, _HxException) else _hx_e
				if True:
					e2 = _hx_e1
					haxe_Log.trace(e2, _hx_c._hx_AnonObject({'customParams': None, 'fileName': "Boot.hx", 'lineNumber': 135, 'className': "python.Boot", 'methodName': "__string_rec"}))
				else:
					raise _hx_e
			if toStr is None:
				return "{ ... }"
			else:
				return toStr
		
		
		if python_Boot.builtin.isinstance(o, _hx_c.Enum):
			l1 = python_Boot.builtin.len(o.params)
			hasParams = l1 > 0
			if hasParams:
				paramsStr = ""
				_g2 = 0
				while (_g2 < l1):
					def _hx_local_9():
						nonlocal _g2
						_hx_local_8 = _g2
						_g2 = _g2 + 1
						return _hx_local_8
						
					
					i1 = _hx_local_9()
					prefix1 = ""
					if i1 > 0:
						prefix1 = ","
					
					paramsStr = Std.string(paramsStr) + Std.string(Std.string(prefix1) + Std.string(python_Boot.__string_rec(HxOverrides.hx_array_get(o.params, i1), s)))
				
				
				return Std.string(Std.string(Std.string(Std.string(o.tag)) + "(") + Std.string(paramsStr)) + ")"
			
			else:
				return o.tag
		
		
		if python_Boot.builtin.hasattr(o, "_hx_class_name") and o.__class__.__name__ != "type":
			fields1 = Type.getInstanceFields(o)
			fieldsStr1 = None
			_g3 = []
			_g12 = 0
			while (_g12 < _hx_builtin.len(fields1)):
				f1 = HxOverrides.hx_array_get(fields1, _g12)
				_g12 = _g12 + 1
				x1 = Std.string(Std.string("" + Std.string(f1)) + " : ") + Std.string(python_Boot.__string_rec(Reflect.field(o, f1), Std.string(s) + "\t"))
				_g3.append(x1)
				_hx_builtin.len(_g3)
				
			
			
			fieldsStr1 = _g3
			
			toStr1 = Std.string(Std.string(Std.string(Std.string(o._hx_class_name)) + "( ") + Std.string(", ".join(_hx_builtin.list(_hx_builtin.map(Std.string, fieldsStr1))))) + " )"
			return toStr1
		
		
		if python_Boot.builtin.hasattr(o, "_hx_class_name") and o.__class__.__name__ == "type":
			fields2 = Type.getClassFields(o)
			fieldsStr2 = None
			_g4 = []
			_g13 = 0
			while (_g13 < _hx_builtin.len(fields2)):
				f2 = HxOverrides.hx_array_get(fields2, _g13)
				_g13 = _g13 + 1
				x2 = Std.string(Std.string("" + Std.string(f2)) + " : ") + Std.string(python_Boot.__string_rec(Reflect.field(o, f2), Std.string(s) + "\t"))
				_g4.append(x2)
				_hx_builtin.len(_g4)
				
			
			
			fieldsStr2 = _g4
			
			toStr2 = Std.string(Std.string(Std.string("#" + Std.string(Std.string(o._hx_class_name))) + "( ") + Std.string(", ".join(_hx_builtin.list(_hx_builtin.map(Std.string, fieldsStr2))))) + " )"
			return toStr2
		
		
		if o == String:
			return "#String"
		
		if o == list:
			return "#Array"
		
		if python_Boot.builtin.callable(o):
			return "function"
		
		try:
			if python_Boot.builtin.hasattr(o, "__repr__"):
				return o.__repr__()
			
		except Exception as _hx_e:
			_hx_e1 = _hx_e.val if isinstance(_hx_e, _HxException) else _hx_e
			if True:
				e3 = _hx_e1
				None
			else:
				raise _hx_e
		if python_Boot.builtin.hasattr(o, "__str__"):
			return o.__str__()
		
		if python_Boot.builtin.hasattr(o, "__name__"):
			return o.__name__
		
		return "???"
	
	else:
		try:
			def _hx_local_13(_):
				return True
			
			python_Boot.inspect.getmembers(o, _hx_local_13)
			return python_Boot.builtin.str(o)
	
		except Exception as _hx_e:
			_hx_e1 = _hx_e.val if isinstance(_hx_e, _HxException) else _hx_e
			if True:
				e4 = _hx_e1
				return "???"
			else:
				raise _hx_e
	

python_Boot.__string_rec = Boot_statics___string_rec

import inspect as inspect
_hx_c.inspect = inspect

python_Boot.inspect = inspect
python_Boot.builtin = _hx_builtin


def HxOverrides_statics_iterator(x):
	if Std._hx_is(x, list):
		return  _hx_c.python_internal_ArrayImpl.iterator(x)
	else:
		return x.iterator()

HxOverrides.iterator = HxOverrides_statics_iterator
def HxOverrides_statics_shift(x):
	if Std._hx_is(x, list):
		return  _hx_c.python_internal_ArrayImpl.shift(x)
	else:
		return x.shift()

HxOverrides.shift = HxOverrides_statics_shift
def HxOverrides_statics_filter(x,f):
	if Std._hx_is(x, list):
		return  _hx_c.python_internal_ArrayImpl.filter(x, f)
	else:
		return x.filter(f)

HxOverrides.filter = HxOverrides_statics_filter
def HxOverrides_statics_map(x,f):
	if Std._hx_is(x, list):
		return  _hx_c.python_internal_ArrayImpl.map(x, f)
	else:
		return x.map(f)

HxOverrides.map = HxOverrides_statics_map
def HxOverrides_statics_length(x):
	if Std._hx_is(x, list) or Std._hx_is(x, String):
		return  _hx_builtin.len(x)
	else:
		return x.length

HxOverrides.length = HxOverrides_statics_length
def HxOverrides_statics_hx_rshift(val,n):
	return (val % 0x100000000) >> n

HxOverrides.hx_rshift = HxOverrides_statics_hx_rshift
def HxOverrides_statics_hx_modf(a,b):
	return float('nan') if (b == 0.0) else a % b if a > 0 else -(-a % b)

HxOverrides.hx_modf = HxOverrides_statics_hx_modf
def HxOverrides_statics_hx_array_get(a,i):
	return a[i] if (i < len(a) and i > -1) else None

HxOverrides.hx_array_get = HxOverrides_statics_hx_array_get
def HxOverrides_statics_hx_array_set(a,i,v):
	
			l = len(a)
			while l < i:
				a.append(None)
				l+=1
			if l == i:
				a.append(v)
			else:
				a[i] = v
			return v

HxOverrides.hx_array_set = HxOverrides_statics_hx_array_set
def HxOverrides_statics_hx_toUpperCase(x):
	if Std._hx_is(x, String):
		return x.upper()
	else:
		return HxOverrides.hx_toUpperCase(x)

HxOverrides.hx_toUpperCase = HxOverrides_statics_hx_toUpperCase

def ArrayImpl_statics_get_length(x):
	return _hx_builtin.len(x)

python_internal_ArrayImpl.get_length = ArrayImpl_statics_get_length
def ArrayImpl_statics_concat(a1,a2):
	return a1 + a2

python_internal_ArrayImpl.concat = ArrayImpl_statics_concat
def ArrayImpl_statics_copy(x):
	return _hx_builtin.list(x)

python_internal_ArrayImpl.copy = ArrayImpl_statics_copy
def ArrayImpl_statics_iterator(x):
	it = x.__iter__()
	return python_HaxeIterator(it)
	

python_internal_ArrayImpl.iterator = ArrayImpl_statics_iterator
def ArrayImpl_statics_indexOf(a,x,fromIndex = None):
	if fromIndex is None:
		fromIndex = None
	
	l = None
	if fromIndex is None:
		l = 0
	elif fromIndex < 0:
		l = _hx_builtin.len(a) + fromIndex
	else:
		l = fromIndex
	if l < 0:
		l = 0
	
	_g1 = l
	_g = _hx_builtin.len(a)
	while (_g1 < _g):
		def _hx_local_1():
			nonlocal _g1
			_hx_local_0 = _g1
			_g1 = _g1 + 1
			return _hx_local_0
			
		
		i = _hx_local_1()
		if HxOverrides.hx_array_get(a, i) == x:
			return i
		
	
	
	return -1
	

python_internal_ArrayImpl.indexOf = ArrayImpl_statics_indexOf
def ArrayImpl_statics_lastIndexOf(a,x,fromIndex = None):
	if fromIndex is None:
		fromIndex = None
	
	l = None
	if fromIndex is None:
		l = _hx_builtin.len(a)
	elif fromIndex < 0:
		l = _hx_builtin.len(a) + fromIndex + 1
	else:
		l = fromIndex + 1
	if l > _hx_builtin.len(a):
		l = _hx_builtin.len(a)
	
	def _hx_local_1():
		nonlocal l
		l = l - 1
		return l
		
	
	while (_hx_local_1() > -1):
		if HxOverrides.hx_array_get(a, l) == x:
			return l
		
	return -1
	

python_internal_ArrayImpl.lastIndexOf = ArrayImpl_statics_lastIndexOf
def ArrayImpl_statics_join(x,sep):
	return sep.join(_hx_builtin.list(_hx_builtin.map(Std.string, x)))

python_internal_ArrayImpl.join = ArrayImpl_statics_join
def ArrayImpl_statics_toString(x):
	return Std.string("[" + Std.string(",".join(_hx_builtin.list(_hx_builtin.map(Std.string, x))))) + "]"

python_internal_ArrayImpl.toString = ArrayImpl_statics_toString
def ArrayImpl_statics_pop(x):
	if _hx_builtin.len(x) == 0:
		return None
	else:
		return x.pop()

python_internal_ArrayImpl.pop = ArrayImpl_statics_pop
def ArrayImpl_statics_push(x,e):
	x.append(e)
	return _hx_builtin.len(x)
	

python_internal_ArrayImpl.push = ArrayImpl_statics_push
def ArrayImpl_statics_unshift(x,e):
	return x.insert(0, e)

python_internal_ArrayImpl.unshift = ArrayImpl_statics_unshift
def ArrayImpl_statics_remove(x,e):
	try:
		x.remove(e)
		return True
	
	except Exception as _hx_e:
		_hx_e1 = _hx_e.val if isinstance(_hx_e, _HxException) else _hx_e
		if True:
			e1 = _hx_e1
			return False
		else:
			raise _hx_e

python_internal_ArrayImpl.remove = ArrayImpl_statics_remove
def ArrayImpl_statics_shift(x):
	if _hx_builtin.len(x) == 0:
		return None
	
	return x.pop(0)
	

python_internal_ArrayImpl.shift = ArrayImpl_statics_shift
def ArrayImpl_statics_slice(x,pos,end = None):
	if end is None:
		end = None
	
	return x[pos:end]
	

python_internal_ArrayImpl.slice = ArrayImpl_statics_slice
def ArrayImpl_statics_sort(x,f):
	return x.sort(key=_hx_functools.cmp_to_key(f))

python_internal_ArrayImpl.sort = ArrayImpl_statics_sort
def ArrayImpl_statics_splice(x,pos,len):
	if pos < 0:
		pos = _hx_builtin.len(x) + pos
	
	if pos < 0:
		pos = 0
	
	res = x[pos:pos + len]
	del x[pos:pos + len]
	return res
	

python_internal_ArrayImpl.splice = ArrayImpl_statics_splice
def ArrayImpl_statics_map(x,f):
	return _hx_builtin.list(_hx_builtin.map(f, x))

python_internal_ArrayImpl.map = ArrayImpl_statics_map
def ArrayImpl_statics_filter(x,f):
	return _hx_builtin.list(_hx_builtin.filter(f, x))

python_internal_ArrayImpl.filter = ArrayImpl_statics_filter
def ArrayImpl_statics___get(x,idx):
	_hx_a = x
	if idx >= _hx_builtin.len(_hx_a) or idx < 0:
		return None
	else:
		return HxOverrides.hx_array_get(x, idx)
	

python_internal_ArrayImpl.__get = ArrayImpl_statics___get
def ArrayImpl_statics___set(x,idx,v):
	_hx_a = x
	HxOverrides.hx_array_set(_hx_a,idx,v)
	return v
	

python_internal_ArrayImpl.__set = ArrayImpl_statics___set
def ArrayImpl_statics___unsafe_get(x,idx):
	return HxOverrides.hx_array_get(x, idx)

python_internal_ArrayImpl.__unsafe_get = ArrayImpl_statics___unsafe_get
def ArrayImpl_statics___unsafe_set(x,idx,val):
	HxOverrides.hx_array_set(x,idx,val)
	return val
	

python_internal_ArrayImpl.__unsafe_set = ArrayImpl_statics___unsafe_set




def Main_statics_main():
	runner = haxe_unit_TestRunner()
	runner.add(TestSys())
	runner.run()
	

Main.main = Main_statics_main


import math as _hx_math
_hx_c._hx_math = _hx_math

_hx_math.NEGATIVE_INFINITY = float("-inf")
_hx_math.POSITIVE_INFINITY = float("inf")
_hx_math.NaN = float("nan")
_hx_math.PI = _hx_math.pi

def Reflect_statics_field(o,field):
	if field is None:
		return None
	
	field1 = None
	if python_internal_KeywordHandler.keywords.exists(field):
		field1 = "_hx_" + Std.string(field)
	else:
		field1 = field
	if _hx_builtin.hasattr(o, field1):
		return _hx_builtin.getattr(o, field1)
	else:
		return None
	

Reflect.field = Reflect_statics_field
def Reflect_statics_callMethod(o,func,args):
	args1 = args
	if _hx_builtin.callable(func):
		return func(*args1)
	else:
		return None
	

Reflect.callMethod = Reflect_statics_callMethod
def Reflect_statics_fields(o):
	a = []
	if o is not None:
		if _hx_builtin.hasattr(o, "_hx_fields"):
			fields = o._hx_fields
			return _hx_builtin.list(fields)
		
		
		if _hx_builtin.isinstance(o, _hx_c._hx_AnonObject):
			d = _hx_builtin.getattr(o, "__dict__")
			keys = d.keys()
			handler = python_internal_KeywordHandler.unhandleKeywords
			for k in keys:
				a.append(handler(k))
		
		elif _hx_builtin.hasattr(o, "__dict__"):
			a1 = []
			d1 = _hx_builtin.getattr(o, "__dict__")
			keys1 = d1.keys()
			for k in keys:
				a.append(k)
		
		
	
	
	return a
	

Reflect.fields = Reflect_statics_fields
def Reflect_statics_isFunction(f):
	return python_lib_Inspect.isfunction(f) or python_lib_Inspect.ismethod(f)

Reflect.isFunction = Reflect_statics_isFunction

def Std_statics__hx_is(v,t):
	if v is None and t is None:
		return False
	
	if t is None:
		return False
	
	if t == Dynamic:
		return True
	
	isBool = _hx_builtin.isinstance(v, bool)
	if t == Bool and isBool:
		return True
	
	if not isBool and t != Bool and t == Int and _hx_builtin.isinstance(v, int):
		return True
	
	vIsFloat = _hx_builtin.isinstance(v, float)
	def _hx_local_0():
		f = v
		return f != _hx_math.POSITIVE_INFINITY and f != _hx_math.NEGATIVE_INFINITY and not _hx_math.isnan(f)
		
	
	def _hx_local_1():
		x = v
		def _hx_local_4():
			def _hx_local_3():
				_hx_local_2 = None
				try:
					_hx_local_2 = int(x)
				except Exception as _hx_e:
					_hx_e1 = _hx_e.val if isinstance(_hx_e, _HxException) else _hx_e
					if True:
						e = _hx_e1
						_hx_local_2 = None
					else:
						raise _hx_e
				return _hx_local_2
				
			
			return _hx_local_3()
			
		
		return _hx_local_4()
		
	
	if not isBool and vIsFloat and t == Int and _hx_local_0() and v == _hx_local_1():
		return True
	
	if not isBool and t == Float and _hx_builtin.isinstance(v, (float,int)):
		return True
	
	if t == str:
		return _hx_builtin.isinstance(v, String)
	
	if t == Enum and python_lib_Inspect.isclass(v) and _hx_builtin.hasattr(v, "_hx_constructs"):
		return True
	
	if t == Enum:
		return False
	
	if t == Date and _hx_builtin.isinstance(v, Date):
		return True
	
	if t == Date:
		return False
	
	if _hx_builtin.isinstance(v, Date):
		return False
	
	if t == Class and not _hx_builtin.isinstance(v, Enum) and python_lib_Inspect.isclass(v) and _hx_builtin.hasattr(v, "_hx_class_name") and not _hx_builtin.hasattr(v, "_hx_constructs"):
		return True
	
	if t == Class:
		return False
	
	def _hx_local_6():
		_hx_local_5 = None
		try:
			_hx_local_5 = _hx_builtin.isinstance(v, t)
		except Exception as _hx_e:
			_hx_e1 = _hx_e.val if isinstance(_hx_e, _HxException) else _hx_e
			if True:
				e1 = _hx_e1
				_hx_local_5 = False
			else:
				raise _hx_e
		return _hx_local_5
		
	
	if _hx_local_6():
		return True
	
	if python_lib_Inspect.isclass(t):
		loop = None
		loop1 = None
		def _hx_local_8(intf):
			f1 = Reflect.field(intf, "_hx_interfaces")
			if f1 is not None:
				_g = 0
				while (_g < _hx_builtin.len(f1)):
					i = HxOverrides.hx_array_get(f1, _g)
					_g = _g + 1
					if i == t:
						return True
					else:
						l = loop1(i)
						if l:
							return True
						
					
				
				
				return False
			
			else:
				return False
			
		
		loop1 = _hx_local_8
		loop = loop1
		
		return loop(v.__class__)
	
	else:
		return False
	

Std._hx_is = Std_statics__hx_is
def Std_statics_string(s):
	return python_Boot.__string_rec(s, "")

Std.string = Std_statics_string

def HxString_statics_split(s,d):
	if d == "":
		return _hx_builtin.list(s)
	else:
		return s.split(d)

HxString.split = HxString_statics_split
def HxString_statics_charCodeAt(s,index):
	if s is None or _hx_builtin.len(s) == 0 or index < 0 or index >= _hx_builtin.len(s):
		return None
	else:
		return ord(s[index])

HxString.charCodeAt = HxString_statics_charCodeAt
def HxString_statics_charAt(s,index):
	if index < 0 or index >= _hx_builtin.len(s):
		return ""
	else:
		return s[index]

HxString.charAt = HxString_statics_charAt
def HxString_statics_lastIndexOf(s,str,startIndex = None):
	if startIndex is None:
		startIndex = None
	
	if startIndex is None:
		return s.rfind(str, 0, _hx_builtin.len(s))
	else:
		i = s.rfind(str, 0, startIndex + 1)
		startLeft = None
		if i == -1:
			b = startIndex + 1 - _hx_builtin.len(str)
			if _hx_math.isnan(0):
				startLeft = 0
			elif _hx_math.isnan(b):
				startLeft = b
			else:
				startLeft = _hx_builtin.max(0, b)
		
		else:
			startLeft = i + 1
		check = s.find(str, startLeft, _hx_builtin.len(s))
		if check > i and check <= startIndex:
			return check
		else:
			return i
	
	

HxString.lastIndexOf = HxString_statics_lastIndexOf
def HxString_statics_fromCharCode(code):
	c = code
	return "".join(_hx_builtin.map(_hx_builtin.chr, [c]))
	

HxString.fromCharCode = HxString_statics_fromCharCode

from builtins import str as String
_hx_c.String = String


def StringTools_statics_startsWith(s,start):
	return _hx_builtin.len(s) >= _hx_builtin.len(start) and python_Tools.substr(s, 0, _hx_builtin.len(start)) == start

StringTools.startsWith = StringTools_statics_startsWith


import os as python_lib_Os
_hx_c.python_lib_Os = python_lib_Os

import builtins as python_lib_Builtin
_hx_c.python_lib_Builtin = python_lib_Builtin


Sys.environ = None;
def Sys_statics_println(v):
	str = Std.string(v)
	sys.stdout.buffer.write(("%s\n"%str).encode('utf-8'))
	

Sys.println = Sys_statics_println
def Sys_statics_args():
	argv = python_lib_Sys.argv
	return argv[1:None]
	

Sys.args = Sys_statics_args
def Sys_statics_getEnv(s):
	return Sys.environ.get(s)

Sys.getEnv = Sys_statics_getEnv
def Sys_statics_putEnv(s,v):
	Sys.environ.set(s, v)

Sys.putEnv = Sys_statics_putEnv
def Sys_statics_environment():
	return Sys.environ

Sys.environment = Sys_statics_environment
def Sys_statics_getCwd():
	return python_lib_Os.getcwd()

Sys.getCwd = Sys_statics_getCwd
def Sys_statics_setCwd(s):
	python_lib_Os.chdir(s)

Sys.setCwd = Sys_statics_setCwd

Sys.environ = haxe_ds_StringMap()
env = python_lib_Os.environ
def _hx_local_0():
	_this = env.keys()
	def _hx_local_2():
		def _hx_local_1():
			p = _hx_builtin.iter(_this)
			return python_HaxeIterator(p)
			
		
		return _hx_local_1()
		
	
	return _hx_local_2()
	

_it = _hx_local_0()
while _it.hasNext():
	key = _it.next()
	Sys.environ.set(key, env.get(key, None))



def Type_statics_getClass(o):
	if o is None:
		return None
	
	if o is not None and (o == String or python_lib_Inspect.isclass(o)):
		return None
	
	if python_Boot.isAnonObject(o):
		return None
	
	if _hx_builtin.hasattr(o, "_hx_class"):
		return o._hx_class
	
	if _hx_builtin.hasattr(o, "__class__"):
		return o.__class__
	else:
		return None
	

Type.getClass = Type_statics_getClass
def Type_statics_getSuperClass(c):
	if c is None:
		return None
	
	try:
		if _hx_builtin.hasattr(c, "_hx_super"):
			return c._hx_super
		
		return __python_array_get(c.__bases__, 0)
	
	except Exception as _hx_e:
		_hx_e1 = _hx_e.val if isinstance(_hx_e, _HxException) else _hx_e
		if True:
			e = _hx_e1
			None
		else:
			raise _hx_e
	return None
	

Type.getSuperClass = Type_statics_getSuperClass
def Type_statics_getClassName(c):
	if _hx_builtin.hasattr(c, "_hx_class_name"):
		return c._hx_class_name
	else:
		if c == list:
			return "Array"
		
		if c == _hx_math:
			return "Math"
		
		if c == String:
			return "String"
		
		try:
			s = c.__name__
		except Exception as _hx_e:
			_hx_e1 = _hx_e.val if isinstance(_hx_e, _HxException) else _hx_e
			if True:
				e = _hx_e1
				None
			else:
				raise _hx_e
	
	res = None
	return res
	

Type.getClassName = Type_statics_getClassName
def Type_statics_getInstanceFields(c):
	f = None
	if _hx_builtin.hasattr(c, "_hx_fields"):
		x = c._hx_fields
		x2 = c._hx_methods
		f = x + x2
	
	else:
		f = []
	sc = Type.getSuperClass(c)
	if sc is None:
		return f
	else:
		scArr = Type.getInstanceFields(sc)
		scMap = None
		_g = haxe_ds_StringMap()
		_g1 = 0
		while (_g1 < _hx_builtin.len(scArr)):
			f1 = HxOverrides.hx_array_get(scArr, _g1)
			_g1 = _g1 + 1
			_g.set(f1, f1)
		
		
		scMap = _g
		
		res = []
		_g11 = 0
		while (_g11 < _hx_builtin.len(f)):
			f11 = HxOverrides.hx_array_get(f, _g11)
			_g11 = _g11 + 1
			if not scMap.exists(f11):
				scArr.append(f11)
				_hx_builtin.len(scArr)
			
			
		
		
		return scArr
	
	

Type.getInstanceFields = Type_statics_getInstanceFields
def Type_statics_getClassFields(c):
	if _hx_builtin.hasattr(c, "_hx_statics"):
		x = c._hx_statics
		return _hx_builtin.list(x)
	
	else:
		return []

Type.getClassFields = Type_statics_getClassFields

def CallStack_statics_exceptionStack():
	return []

haxe_CallStack.exceptionStack = CallStack_statics_exceptionStack
def CallStack_statics_toString(stack):
	b = StringBuf()
	_g = 0
	while (_g < _hx_builtin.len(stack)):
		s = HxOverrides.hx_array_get(stack, _g)
		_g = _g + 1
		s1 = "\nCalled from "
		b.b.write(s1)
		
		haxe_CallStack.itemToString(b, s)
	
	
	return b.b.getvalue()
	

haxe_CallStack.toString = CallStack_statics_toString
def CallStack_statics_itemToString(b,s):
	if (s.index) == 0:
		s1 = "a C function"
		b.b.write(s1)
	
	elif (s.index) == 1:
		m = s.params[0]
		s2 = "module "
		b.b.write(s2)
		
		s3 = Std.string(m)
		b.b.write(s3)
		
		
	
	elif (s.index) == 2:
		line = s.params[2]
		file = s.params[1]
		s4 = s.params[0]
		if s4 is not None:
			haxe_CallStack.itemToString(b, s4)
			s5 = " ("
			b.b.write(s5)
			
		
		
		s6 = Std.string(file)
		b.b.write(s6)
		
		s7 = " line "
		b.b.write(s7)
		
		s8 = Std.string(line)
		b.b.write(s8)
		
		if s4 is not None:
			s9 = ")"
			b.b.write(s9)
		
		
		
	
	elif (s.index) == 3:
		meth = s.params[1]
		cname = s.params[0]
		s10 = Std.string(cname)
		b.b.write(s10)
		
		s11 = "."
		b.b.write(s11)
		
		s12 = Std.string(meth)
		b.b.write(s12)
		
		
	
	elif (s.index) == 4:
		n = s.params[0]
		s13 = "local function #"
		b.b.write(s13)
		
		s14 = Std.string(n)
		b.b.write(s14)
		
		
	
	else:
		None

haxe_CallStack.itemToString = CallStack_statics_itemToString

def Log_statics_trace(v,infos = None):
	if infos is None:
		infos = None
	
	str = None
	if infos is not None:
		str = Std.string(Std.string(Std.string(Std.string(infos.fileName) + ":") + Std.string(Std.string(infos.lineNumber))) + ": ") + Std.string(Std.string(v))
		if _hx_builtin.hasattr(infos, "customParams") and infos.customParams is not None:
			str = Std.string(str) + Std.string("," + Std.string(",".join(_hx_builtin.list(_hx_builtin.map(Std.string, infos.customParams)))))
		
	
	else:
		str = v
	Sys.println(str)
	

haxe_Log.trace = Log_statics_trace


def Path_statics_join(paths):
	def _hx_local_0(s):
		return s is not None and s != ""
	
	paths1 = _hx_builtin.list(_hx_builtin.filter(_hx_local_0, paths))
	if _hx_builtin.len(paths1) == 0:
		return ""
	
	path = HxOverrides.hx_array_get(paths1, 0)
	_g1 = 1
	_g = _hx_builtin.len(paths1)
	while (_g1 < _g):
		def _hx_local_2():
			nonlocal _g1
			_hx_local_1 = _g1
			_g1 = _g1 + 1
			return _hx_local_1
			
		
		i = _hx_local_2()
		path = haxe_io_Path.addTrailingSlash(path)
		path = Std.string(path) + Std.string(HxOverrides.hx_array_get(paths1, i))
	
	
	return haxe_io_Path.normalize(path)
	

haxe_io_Path.join = Path_statics_join
def Path_statics_normalize(path):
	slash = "/"
	_this = HxString.split(path, "\\")
	path = "/".join(_hx_builtin.list(_hx_builtin.map(Std.string, _this)))
	
	if path is None or path == slash:
		return slash
	
	target = []
	src = None
	parts = None
	token = None
	src = HxString.split(path, slash)
	_g1 = 0
	_g = _hx_builtin.len(src)
	while (_g1 < _g):
		def _hx_local_1():
			nonlocal _g1
			_hx_local_0 = _g1
			_g1 = _g1 + 1
			return _hx_local_0
			
		
		i = _hx_local_1()
		token = HxOverrides.hx_array_get(src, i)
		if token == "..":
			if _hx_builtin.len(target) == 0:
				None
			else:
				target.pop()
		elif token != ".":
			target.append(token)
			_hx_builtin.len(target)
		
		
	
	
	tmp = slash.join(_hx_builtin.list(_hx_builtin.map(Std.string, target)))
	regex = EReg("([^:])/+", "g")
	result = regex.replace(tmp, "$1" + Std.string(slash))
	return result
	

haxe_io_Path.normalize = Path_statics_normalize
def Path_statics_addTrailingSlash(path):
	if _hx_builtin.len(path) == 0:
		return "/"
	
	c1 = path.rfind("/", 0, _hx_builtin.len(path))
	c2 = path.rfind("\\", 0, _hx_builtin.len(path))
	if c1 < c2:
		if c2 != _hx_builtin.len(path) - 1:
			return Std.string(path) + "\\"
		else:
			return path
	elif c1 != _hx_builtin.len(path) - 1:
		return Std.string(path) + "/"
	else:
		return path
	

haxe_io_Path.addTrailingSlash = Path_statics_addTrailingSlash


def TestRunner_statics__hx_print(v):
	python_Lib._hx_print(v)

haxe_unit_TestRunner._hx_print = TestRunner_statics__hx_print
def TestRunner_statics_customTrace(v,p = None):
	if p is None:
		p = None
	
	haxe_unit_TestRunner._hx_print(Std.string(Std.string(Std.string(Std.string(Std.string(p.fileName) + ":") + Std.string(p.lineNumber)) + ": ") + Std.string(Std.string(v))) + "\n")
	

haxe_unit_TestRunner.customTrace = TestRunner_statics_customTrace


def Lib_statics__hx_print(v):
	python_lib_Sys.stdout.write(Std.string(v))
	python_lib_Sys.stdout.flush()
	

python_Lib._hx_print = Lib_statics__hx_print

def Tools_statics_substr(s,startIndex,len = None):
	if len is None:
		len = None
	
	if len is None:
		return s[startIndex:]
	else:
		if len == 0:
			return ""
		
		return s[startIndex:startIndex+len]
	
	

python_Tools.substr = Tools_statics_substr


def _hx_init_python_internal_KeywordHandler_keywords():
	def _hx_local_0():
		_g = haxe_ds_StringMap()
		_g.set("and", True)
		_g.set("del", True)
		_g.set("from", True)
		_g.set("not", True)
		_g.set("while", True)
		_g.set("as", True)
		_g.set("elif", True)
		_g.set("global", True)
		_g.set("or", True)
		_g.set("with", True)
		_g.set("assert", True)
		_g.set("else", True)
		_g.set("if", True)
		_g.set("pass", True)
		_g.set("yield", True)
		_g.set("break", True)
		_g.set("except", True)
		_g.set("import", True)
		_g.set("print", True)
		_g.set("float", True)
		_g.set("class", True)
		_g.set("exec", True)
		_g.set("in", True)
		_g.set("raise", True)
		_g.set("continue", True)
		_g.set("finally", True)
		_g.set("is", True)
		_g.set("return", True)
		_g.set("def", True)
		_g.set("for", True)
		_g.set("lambda", True)
		_g.set("try", True)
		_g.set("None", True)
		return _g
		
	
	return _hx_local_0()

python_internal_KeywordHandler.keywords = _hx_init_python_internal_KeywordHandler_keywords()
def KeywordHandler_statics_unhandleKeywords(name):
	if python_Tools.substr(name, 0, 4) == "_hx_":
		real = python_Tools.substr(name, 4, None)
		if python_internal_KeywordHandler.keywords.exists(real):
			return real
		
	
	
	return name
	

python_internal_KeywordHandler.unhandleKeywords = KeywordHandler_statics_unhandleKeywords

import inspect as python_lib_Inspect
_hx_c.python_lib_Inspect = python_lib_Inspect

import random as python_lib_Random
_hx_c.python_lib_Random = python_lib_Random

import re as python_lib_Re
_hx_c.python_lib_Re = python_lib_Re

import subprocess as python_lib_Subprocess
_hx_c.python_lib_Subprocess = python_lib_Subprocess

import sys as python_lib_Sys
_hx_c.python_lib_Sys = python_lib_Sys

import time as python_lib_Time
_hx_c.python_lib_Time = python_lib_Time

from builtins import bytes as python_lib_Bytes
_hx_c.python_lib_Bytes = python_lib_Bytes

from builtins import set as python_lib_Set
_hx_c.python_lib_Set = python_lib_Set

from builtins import dict as python_lib_Dict
_hx_c.python_lib_Dict = python_lib_Dict

from datetime import datetime as python_lib_datetime_DateTime
_hx_c.python_lib_datetime_DateTime = python_lib_datetime_DateTime

from datetime import timedelta as python_lib_datetime_TimeDelta
_hx_c.python_lib_datetime_TimeDelta = python_lib_datetime_TimeDelta

from datetime import tzinfo as python_lib_datetime_TzInfo
_hx_c.python_lib_datetime_TzInfo = python_lib_datetime_TzInfo

from datetime import timezone as python_lib_datetime_Timezone
_hx_c.python_lib_datetime_Timezone = python_lib_datetime_Timezone

from io import StringIO as python_lib_io_StringIO
_hx_c.python_lib_io_StringIO = python_lib_io_StringIO

Main.main()