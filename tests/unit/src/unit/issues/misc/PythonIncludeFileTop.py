# Included before the imports by haxe.macro.Compiler.includeFile (TestPython.testIncludeFileTop):
# makes a module that an extern then imports, which only works if this runs first.
import sys as _hx_include_sys
import types as _hx_include_types
_hx_include_module = _hx_include_types.ModuleType("haxe_include_file_top")
_hx_include_module.value = 42
_hx_include_sys.modules["haxe_include_file_top"] = _hx_include_module
