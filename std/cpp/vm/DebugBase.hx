package cpp.vm;

import haxe.Stack;

enum DebugToken
{
   IDENT(name:String);
   CONST(value:Dynamic);
   DOT;
   LPAREN;
   RPAREN;
   COMMA;
   EQUALS;
   LARRAY;
   RARRAY;
}

enum DebugExpr
{
   EXPR_VALUE(value:Dynamic);
   EXPR_FIELD_REF(obj:Dynamic,member:String);
   EXPR_ARRAY_REF(obj:Dynamic,index:Int);
   EXPR_STACK_REF(name:String);
}



class DebugBase
{
   var threadStopped:Bool;
   var stillDebugging:Bool;
   var inputThread:Thread;
   var debugQueue:Deque<Dynamic>;
   var files:Array<String>;
	var frame:Int;
	var stack:Array<StackItem>;
	var vars:Array<String>;
   

   public function new()
   {
		frame = -1;
		files = Debugger.getFiles();
		stillDebugging = true;
      threadStopped = false;
		Debugger.setThread();
      Debugger.setHandler(onDebug);
      debugQueue= new Deque<Dynamic>();
      inputThread = Thread.create(inputLoop);
   }

   function onDebug()
   {
      threadStopped = true;
      onStopped();
      while(threadStopped && stillDebugging)
      {
         var job = debugQueue.pop(true);
         job();
      }
   }

   function onHelp() { }

   function waitDebugger(inSendResult:Bool=true)
   {
      debugQueue.add( function() inputThread.sendMessage("ok")  );
      var result = Thread.readMessage(true);
		if (inSendResult)
		{
			if (result!="ok")
				onResult("Debugger out of sync");
			else
				onResult("ok");
		}
   }

	function onStopped() { }

	function onRunning() { }

   function showWhere() { }

   function showFiles() { }

   function showBreakpoints() { }

	function onPrint(result:Dynamic) { }

   function getNextCommand() : String { return "bye"; }

   function onResult(inResult:String) { }

   function addBreakpoint(inFile:String, inLine:String)
	{
		var id = Std.parseInt(inFile);
		if (id==null)
		{
			for(idx in 0...files.length)
				if (files[idx]==inFile)
				{
					id = idx;
					break;
				}
		}

 		if (id==null)
         onResult("Could not find file for " + inFile );
		else
		{
			Debugger.addBreakpoint(id,Std.parseInt(inLine));
			onResult("ok");
		}
			
	}

   static var dot:Int = ".".charCodeAt(0);
   static var quote:Int = "\"".charCodeAt(0);
   static var comma:Int = ",".charCodeAt(0);
   static var equals:Int = "=".charCodeAt(0);
   static var minus:Int = "-".charCodeAt(0);
   static var a_code:Int = "a".charCodeAt(0);
   static var z_code:Int = "z".charCodeAt(0);
   static var A_code:Int = "A".charCodeAt(0);
   static var Z_code:Int = "Z".charCodeAt(0);
   static var __code:Int = "_".charCodeAt(0);
   static var num0_code:Int = "0".charCodeAt(0);
   static var num9_code:Int = "9".charCodeAt(0);
   static var space_code:Int = " ".charCodeAt(0);
   static var lparent:Int = "(".charCodeAt(0);
   static var rparent:Int = ")".charCodeAt(0);
   static var larray:Int = "[".charCodeAt(0);
   static var rarray:Int = "]".charCodeAt(0);


   function tokenize(inString:String) : Array<DebugToken>
   {
      var len = inString.length;
      var result = new Array<DebugToken>();

      var idx = 0;
		while(idx<len)
		{
			var code = inString.charCodeAt(idx);

         // Identifier ...
         if ( (code>=a_code && code<=z_code) || (code>=A_code && code<=Z_code) || code==__code )
         {
            var start = idx++;
				while(idx<len)
            {
			      code = inString.charCodeAt(idx);
               if ( (code>=a_code && code<=z_code) || (code>=A_code && code<=Z_code) || code==__code ||
                    (code>=num0_code && code<num9_code) )
						idx++;
					else
						break;
            }
            result.push( IDENT( inString.substr(start, idx-start) ) );
         }
			else if (code==minus || (code>=num0_code && code<=num9_code) )
         {
            var start = idx++;
				while(idx<len)
            {
			      code = inString.charCodeAt(idx);
               if (code==dot || (code>=num0_code && code<=num9_code) )
						idx++;
					else
						break;
            }
            var val = inString.substr(start, idx-start);
            var num = Std.parseFloat(val);
				if (!Math.isFinite(num))
					throw ("Bad constant '" + val + "'");
            result.push( CONST(num) );
 
         }
			else if (code==quote)
			{
            var start = ++idx;
				while(idx<len)
            {
			      code = inString.charCodeAt(idx);
               if (code==quote)
                  break;
               idx++;
            }
            var val = inString.substr(start, idx-start);
            result.push( CONST(val) );
            idx++;
			}
         else
         {
         	switch(code)
				{
					case space_code : // do nothing
					case lparent : result.push( LPAREN );
					case rparent : result.push( RPAREN );
					case larray : result.push( LARRAY );
					case rarray : result.push( RARRAY );
					case dot : result.push( DOT );
					case comma : result.push( COMMA );
					case equals : result.push( EQUALS );
				}
				idx++;
			}
		}

      return result;
   }

   function resolve(inName:String) : DebugExpr
	{
		if (vars!=null)
		{
			for(v in vars)
				if (v==inName)
					return EXPR_STACK_REF(inName);
		}
		var cls = Type.resolveClass(inName);
      if (cls!=null)
         return EXPR_VALUE(cls);

      return null;
   }


   function getExpression(inTokens:Array<DebugToken>) : DebugExpr
   {
      var classPath = "";
      var expr:Dynamic = null;

      var tok = 0;
      var len = inTokens.length;
      while(tok < len)
      {
			switch(inTokens[tok])
         {
   			case IDENT(name):
					if (expr!=null)
						throw "Misplaced '" + name + "'";
					expr = resolve(name);
					if (expr==null)
						classPath = name;
					tok++;

   			case CONST(value):
					if (expr!=null || classPath!="")
						throw "Misplaced '" + value + "'";
					expr = EXPR_VALUE(value);
					tok++;

   			case DOT:
					if (expr==null && classPath=="")
						throw "Bad '.' after null value";
					tok++;
					switch(inTokens[tok])
					{
						case IDENT(name):
							if (expr!=null)
								expr = EXPR_FIELD_REF(exprToDynamic(expr),name);
							else
							{
								var qname = classPath + "." + name;
								expr = resolve(qname);
								classPath = (expr==null) ? qname : "";
							}
							tok++;
						default: throw "Expected field after '.'";
					}

   			case LPAREN:
						var args = new Array<Dynamic>();
                  var lastComma = tok;
						var start = ++tok;
                  var parenOpen = 1;
                  var arrayOpen = 0;
						while(tok<len && (parenOpen!=0 || arrayOpen!=0) )
						{
					      switch(inTokens[tok])
					      {
   							case LPAREN: parenOpen++;
   							case RPAREN: parenOpen--;
   							case LARRAY: arrayOpen++;
   							case RARRAY: arrayOpen--;
   							case COMMA: 
									if (arrayOpen==0 && parenOpen==1 && expr!=null)
									{
										args.push( getValue( inTokens.slice(lastComma+1,tok) ) );
										lastComma = tok;
									}
								default:
 							}
							tok++;
						}
						if (parenOpen!=0  || arrayOpen!=0)
							throw "Mismatched '(' "+parenOpen+"/"+arrayOpen;
						// Not function call...
						if (classPath!="")
							throw "Unresolved " + classPath;
						if (expr==null)
                  {
							expr = EXPR_VALUE(getValue( inTokens.slice(start,tok-1) ));
                  }
						else
						{
                     if (lastComma+1 < tok-1)
								args.push( getValue( inTokens.slice(lastComma+1,tok-1) ) );
							expr = EXPR_VALUE( untyped expr.__Run( args ) );
						}

   			case LARRAY:
						var start = ++tok;
                  var parenOpen = 0;
                  var arrayOpen = 1;
						while(tok<len && (parenOpen!=0 || arrayOpen!=0) )
						{
					      switch(inTokens[tok])
					      {
   							case LPAREN: parenOpen++;
   							case RPAREN: parenOpen--;
   							case LARRAY: arrayOpen++;
   							case RARRAY: arrayOpen--;
								default:
 							}
							tok++;
						}
						if (parenOpen!=0  || arrayOpen!=0)
							throw "Mismatched '['";
						if (classPath!=null)
							throw "Unresolved " + classPath;
						if (expr==null)
							throw "Error taking index of null object";
						var val:Dynamic = getValue( inTokens.slice(start,tok) );
						if ( !Std.is(val,Int) )
							throw "Bad array index: " + val;
						expr = EXPR_ARRAY_REF(exprToDynamic(expr), Std.int(val));

   			case RPAREN: throw "Misplaced ')'";
   			case COMMA:  throw "Misplaced ','";
   			case EQUALS: throw("Misplaced '='");
   			case RARRAY: throw "Misplaced ']'";
         }
      }
		if (classPath!="")
			throw "Unresolved " + classPath;

      return expr==null ? EXPR_VALUE(null) : expr;
   }

   function exprToDynamic(inExpr:DebugExpr)
   {
      switch(inExpr)
      {
         case EXPR_VALUE(value): return value;
         case EXPR_FIELD_REF(obj,member): return Reflect.getProperty(obj,member);
         case EXPR_ARRAY_REF(obj,index): return obj[index];
         case EXPR_STACK_REF(name): return Debugger.getStackVar(frame,name);
      }
   }

   function getValue(inTokens:Array<DebugToken>) : Dynamic
   {
      return exprToDynamic(getExpression(inTokens));
   }


   function print(inString:String)
   {
      var tokens:Array<DebugToken> = null;
		try
      {
         tokens = tokenize(inString);
         var result = getValue(tokens);
         onPrint(result);
			onResult("ok");
      }
      catch (e:Dynamic)
      {
         onResult("Error while printing : " + e);//+ ( tokens==null ? "" : " : " + tokens) );
      }
   }

   function set(inString:String)
   {
      var tokens:Array<DebugToken> = null;
		try
      {
         tokens = tokenize(inString);

         var equals_pos = -1;
         for(i in 0...tokens.length)
         {
            if (tokens[i]==EQUALS)
            {
               if (equals_pos>=0)
                  throw "more than one '='";
               equals_pos = i;
            }
         }
         if (equals_pos<0)
             throw "use a = b syntax";
         if (equals_pos==0 || equals_pos==tokens.length-1)
             throw "Misplaced '='";

         var lhs = getExpression( tokens.slice(0,equals_pos) );
         var rhs = getValue( tokens.slice(equals_pos+1, tokens.length) );

         switch(lhs)
         {
            case EXPR_VALUE(value): throw "left hand side can't be set";
            case EXPR_FIELD_REF(obj,member): Reflect.setProperty(obj,member,rhs);
            case EXPR_ARRAY_REF(obj,index): obj[index] = rhs;
            case EXPR_STACK_REF(name): Debugger.setStackVar(frame,name,rhs);
         }
      }
      catch (e:Dynamic)
      {
         onResult("Error while setting : " + e);//+ ( tokens==null ? "" : " : " + tokens) );
			return;
      }
		onResult("ok");
   }


   function setFrame(inFrame:Int)
	{
		if (stack!=null && inFrame>0 && inFrame <= stack.length )
		{
			frame = inFrame;
         vars = Debugger.getStackVars(frame);
		}
	}

   function getStack()
	{
      stack = haxe.Stack.callStack();
		setFrame(1);
	}

   function checkStack()
	{
   	if (threadStopped && stack==null)
		{
          debugQueue.add( getStack );
          waitDebugger(false);
		}
	}

	function run()
	{
		stack = null;
		vars = null;
      debugQueue.add( function() { threadStopped = false; inputThread.sendMessage("running"); }  );
      var result = Thread.readMessage(true);
		onRunning();
		onResult("ok");
	}

   function inputLoop()
   {
      while(stillDebugging)
      {
			checkStack();
         var command = getNextCommand();
         var words = command.split(" ");
         switch(words[0])
         {
            case "":
					onResult("");
               // Do nothing

            case "bye":
					stillDebugging = false;
      			debugQueue.add( function() { trace("bye"); }  );
					onResult("ok");

            case "exit","quit":
					onResult("ok");
               Debugger.exit();

            case "break","b":
					if (words.length==1)
					{
               	if (threadStopped)
							onResult("already stopped.");
               	else
               	{
                  	Debugger.setBreak(Debugger.BRK_ASAP);
                  	waitDebugger();
               	}
					}
					else if (words.length==3)
					{
						addBreakpoint(words[1],words[2]);
					}
					else
						onResult("Usage: break [file line] - pause execution of one thread [when at certain point]");


            case "cont","c":
               if (!threadStopped)
                  onResult("Already running.");
               else
						run();

            case "vars","v":
               if (!threadStopped || vars==null)
                  onResult("Must break first.");
               else
               {
						onPrint(vars);
						onResult("ok");
               }


            case "frame","f":
               if (!threadStopped || stack==null )
                  onResult("Must break first.");
               else
               {
						var f = Std.parseInt(words[1]);
						if (f<1 || f>stack.length )
							onResult("Stack out of range.");
						else
						{
                  	debugQueue.add( function() setFrame(f) );
                  	waitDebugger();
						}
               }
              

            case "where","w":
               if (!threadStopped || stack==null)
                  onResult("Must break first.");
               else
					{
						showWhere();
						onResult("ok");
					}

            case "print","p":
					words.shift();
					print(words.join(" "));

            case "set","s":
					words.shift();
					set(words.join(" "));


            case "files","fi":
					{
               	showFiles();
						onResult("ok");
					}

            case "breakpoints","bp":
					{
               	showBreakpoints();
						onResult("ok");
					}

            case "delete","d":
               if (words[1]==null)
					{
						onResult("Usage : delete N");
 					}
					else
					{
                  var i = Std.parseInt(words[1]);
						Debugger.deleteBreakpoint(i);
						onResult("ok");
					}

            case "help","h","?":
					{
						onHelp();
						onResult("ok");
					}
              
            default:
               onResult("Unknown command:" + command);
         }
      }
   }

}


