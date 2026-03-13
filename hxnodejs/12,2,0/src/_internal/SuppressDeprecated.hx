#if macro
package _internal;

import haxe.macro.Context;

@:noCompletion
class SuppressDeprecated {
	public static function run() {
		#if haxe4
		Context.onAfterTyping(function(_) {
			Context.filterMessages(x -> switch x {
				case Warning(msg, pos) if (~/js\/node\/Url\.hx$/.match(Context.getPosInfos(pos).file)):
					msg != "This typedef is deprecated in favor of { ?slashes : Null<Bool>, ?search : Null<String>, ?query : Null<haxe.extern.EitherType<String, haxe.DynamicAccess<String>>>, ?protocol : Null<String>, ?port : Null<String>, ?pathname : Null<String>, ?path : Null<String>, ?href : Null<String>, ?hostname : Null<String>, ?host : Null<String>, ?hash : Null<String>, ?auth : Null<String> }";
				case Warning(msg, pos) if (~/js\/node\/domain\/Domain\.hx$/.match(Context.getPosInfos(pos).file)):
					msg != "This typedef is deprecated in favor of { domainThrown : Bool, domainEmitter : js.node.events.IEventEmitter, domainBound : haxe.Function, domain : js.node.domain.Domain }";
				case _:
					true;
			});
		});
		#end
	}
}
#end
