package asys.native.system;

import haxe.NoData;
import haxe.exceptions.NotImplementedException;

/**
	Represents a users group registered in OS.

	TODO:
	Not sure what would be the best underlying type for cross-platform, hence `@:coreType`
**/
@:coreType abstract SystemGroup {
	@:from static function fromGroupId(groupId:Int):SystemGroup {
		throw new NotImplementedException();
	}

	@:from static function fromGroupName(groupName:String):SystemGroup {
		throw new NotImplementedException();
	}

	/**
		Create a new system group.

		TODO: not sure if we need this in std.
	**/
	function create(name:String, callback:Callback<NoData>):Void {
		throw new NotImplementedException();
	}

	/**
		Add `user` to this group.

		TODO: not sure if we need this in std.
	**/
	function addUser(user:SystemUser, callback:Callback<SystemGroup>):Void {
		throw new NotImplementedException();
	}
}