package mtwin.web;

import mtwin.web.Request;
import Reflect.Class;

enum ActionError {
	UnknownAction(name:String);
	ObjectNotFound(id:Int);
	ActionReservedToLoggedUsers;
	ActionReservedToObjectOwner;
	ActionReservedToAdministrators;
	ActionReservedToModerators;
	CallingObjectMethodWithoutId;
}

/**
	Generic class to handle web actions.

	Add to your .htaccess :

	<FilesMatch "^(actionName|actionName|...)$">
	RewriteEngine On
	RewriteRule (.*) /index.n
	</FilesMatch>
**/
class Handler<T> {

	public static var STATIC_DEFAULT = "default";
	public static var OBJECT_DEFAULT = "objectDefault";

	var ReadWrite : Bool;
	var ReadOnly : Bool;

	var actions : Hash<Void->Void>;
	var id : Int;
	var level : Int;
	var request : mtwin.web.Request;

	public function new(){
		actions = new Hash();
		ReadWrite = true;
		ReadOnly = false;
	}

	public function execute( request:Request, ?pathLevel:Int ){
		if (pathLevel == null)
			pathLevel = 0;

		var part = request.getPathInfoPart(pathLevel);
		if (~/^[0-9]+$/.match(part)){
			id = Std.parseInt(part);
			part = request.getPathInfoPart(++pathLevel);
			if (part == "")
				part = OBJECT_DEFAULT;
		}

		this.request = request;
		this.level = pathLevel;

		if (part == "")
			part = STATIC_DEFAULT;
		if (actions.exists(part)){
			actions.get(part)();
			return;
		}
		throw UnknownAction(part);
	}

	// Methods to override when needed

	function prepareTemplate( t:String ) : Void {
		throw "not implemented";
	}

	function isLogged() : Bool {
		throw "not implemented";
		return false;
	}
	
	function isAdmin() : Bool {
		throw "not implemented";
		return false;
	}
	
	function isModerator() : Bool {
		throw "not implemented";
		return false;
	}

	function isOwner( o:T ) : Bool {
		throw "not implemented";
		return false;
	}

	function findObject( id : Int, lock:Bool ) : T {
		throw "findObject(Int) not implemented";
		return null;
	}

	// callback wrappers
	
	function object( cb:T->Void, ?lock:Bool ) : Void->Void {
		if (lock == null) lock = ReadWrite;
		var me = this;
		return function(){
			if (me.id == null)
				throw CallingObjectMethodWithoutId;
			var obj = me.findObject(me.id, lock);
			if (obj == null)
				throw ObjectNotFound(me.id);
			cb(obj);
		}
	}

	function owner( cb:T->Void ) : T->Void {
		var me = this;
		return function(u:T){
			if (!me.isOwner(u))
				throw ActionReservedToObjectOwner;
			cb(u);
		}
	}

	function handler( h:Handler<Dynamic> ) : Void->Void {
		var me = this;
		return function(){
			h.execute(me.request, me.level+1);
		}
	}

	// action declarators

	function free( n:String, ?t:String, ?cb:Void->Void ){
		var me = this;
		actions.set(n, function(){
			me.run(t,cb);
		});
	}

	function logged( n:String, ?t:String, ?cb:Void->Void ){
		var me = this;
		actions.set(n, function(){
			if (!me.isLogged())
				throw ActionReservedToLoggedUsers;
			me.run(t,cb);
		});
	}

	function admin( n:String, ?t:String, ?cb:Void->Void ){
		var me = this;
		actions.set(n, function(){
			if (!me.isAdmin())
				throw ActionReservedToAdministrators;
			me.run(t,cb);
		});
	}

	function moderator( n:String, ?t:String, ?cb:Void->Void ){
		var me = this;
		actions.set(n, function(){
			if (!me.isModerator())
				throw ActionReservedToModerators;
			me.run(t,cb);
		});
	}

	function run( ?t:String, ?cb:Void->Void ){
		if (t != null) prepareTemplate(t);
		if (cb != null) cb();
	}
}


/**


// if your application is going to contain many handlers, you may whish to extends the Handler once to
// override prepareTemplate(), isLogged(), isAdmin(), ....
//
class Handler<T> extends mtwin.web.Handler<T> {

	override function prepareTemplate( t:String ){
		//App.template = new mtwin.Templo(t);
	}

	override function isLogged(){
		//return App.user != null;
		return true;
	}
}

// handler example
//
class UserHandler extends Handler<User> {

	public function new(){
		super();
		free("preview", "preview.mtt", doPreview);
		free("view", "userView.mtt", object(doUserView));
		logged("view", "userView.mtt", object(owner(doUserView)));
	}

	override function isOwner( u:User ) {
		// return App.user.id == u.id;
		return true;
	}

	override function findObject( id:Int ) : User {
		return User.get(id);
	}

	function doLogin(){
		// ...
	}
	
	function doView( u:User ){
		// ...
	}
}

// our main application handler which will dispatch requests
// to sub handlers (or main functions)
class MainHandler extends Handler<Void> {
	public function new(){
		super();
		// /preview action
		free("preview", "preview.mtt", doPreview);
		// /user/* action are delayed to the UserHandler
		handler("user", new UserHandler());
	}
}

class App {
	function main(){
		var h = new MainHandler();
		h.execute(new Request());
	}
}

**/
