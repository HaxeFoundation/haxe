package cs.system.componentmodel;

/**
 * Represents the method that handles the  event raised when a  or component is
 * changed during design time.
 * @param e A  that contains the component or  that changed.
 */
@:native("System.ComponentModel.RefreshEventHandler")
extern class RefreshEventHandler extends cs.system.MulticastDelegate {
	function new(func:(e:cs.system.componentmodel.RefreshEventArgs)->Void):Void;
	function Invoke(e:cs.system.componentmodel.RefreshEventArgs):Void;
}
