package cs.system.componentmodel.design;

@:native("System.ComponentModel.Design.DesignerOptionService.DesignerOptionCollection")
extern class DesignerOptionService_DesignerOptionCollection {
	var Count(default, never):Int;
	var Name(default, never):String;
	var Parent(default, never):cs.system.componentmodel.design.DesignerOptionService_DesignerOptionCollection;
	var Properties(default, never):cs.system.componentmodel.PropertyDescriptorCollection;
	@:overload(function(index0:Int):cs.system.componentmodel.design.DesignerOptionService_DesignerOptionCollection {})
	@:native("get_Item")
	function get_Item(index0:String):cs.system.componentmodel.design.DesignerOptionService_DesignerOptionCollection;
	function CopyTo(array:cs.system.Array, index:Int):Void;
	function GetEnumerator():cs.system.collections.IEnumerator;
	function IndexOf(value:cs.system.componentmodel.design.DesignerOptionService_DesignerOptionCollection):Int;
	function ShowDialog():Bool;
}
