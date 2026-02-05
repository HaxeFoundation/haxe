package cs.system.componentmodel;

/** Provides an interface to facilitate the retrieval of the builder's name and to display the builder. */
@:native("System.ComponentModel.IIntellisenseBuilder")
extern interface IIntellisenseBuilder {
	/**
	 * Gets a localized name.
	 * @return A localized name.
	 */
	var Name(default, never):String;
	/**
	 * Shows the builder.
	 * @param language The language service that is calling the builder.
	 * @param value The expression being edited.
	 * @param newValue The new value.
	 * @return if the value should be replaced with ; otherwise,  (if the user cancels,
	 * for example).
	 */
	function Show(language:String, value:String, newValue:cs.Ref<String>):Bool;
}
