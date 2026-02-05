package cs.system.componentmodel;

/** Provides a type converter to convert 8-bit unsigned integer objects to and from a string. */
@:native("System.ComponentModel.SByteConverter")
extern class SByteConverter extends cs.system.componentmodel.BaseNumberConverter {
	function new():Void;
}
