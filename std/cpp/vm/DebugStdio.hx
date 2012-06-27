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


class DebugStdio
{
   var threadStopped:Bool;
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
      threadStopped = false;
		Debugger.setThread();
      Debugger.setHandler(onDebug);
      debugQueue= new Deque<Dynamic>();
      inputThread = Thread.create(inputLoop);
   }

   function onDebug()
   {
      Sys.println("stopped.");
      threadStopped = true;
      while(threadStopped)
      {
         var job = debugQueue.pop(true);
         job();
      }
   }

   
   function waitDebugger(inPrint:Bool=true)
   {
      debugQueue.add( function() inputThread.sendMessage("Ok")  );
      var result = Thread.readMessage(true);
		if (inPrint)
      	Sys.println(result);
   }

   function where()
   {
      var idx = 0;
      for(item in stack)
      {
         idx++;
         Sys.println((idx==frame ? "*" : " ") + idx + ":" + item);
      }
   }

   function showFiles()
   {
 		if (files!=null)
			for(idx in 0...files.length)
            Sys.println("file " + idx + " : " + files[idx] );
   }

   function showBreakpoints()
   {
      var bps = Debugger.getBreakpoints();
 		if (bps!=null)
			for(idx in 0...bps.length)
            Sys.println("breakpoint " + idx + " : " + bps[idx] );
   }


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
         Sys.println("Could not find file for " + inFile );
		else
			Debugger.addBreakpoint(id,Std.parseInt(inLine));
			
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

   function resolve(inName:String) : Dynamic
	{
		if (vars!=null)
		{
			for(v in vars)
				if (v==inName)
					return Debugger.getStackVar(frame,inName);
		}
		var cls = Type.resolveClass(inName);
      return cls;
   }


   function getValue(inTokens:Array<DebugToken>) : Dynamic
   {
      var classPath = "";
      var lhs:Dynamic = null;

      var tok = 0;
      var len = inTokens.length;
      while(tok < len)
      {
			switch(inTokens[tok])
         {
   			case IDENT(name):
					if (lhs!=null)
						throw "Misplaced '" + name + "'";
					lhs = resolve(name);
					if (lhs==null)
						classPath = name;
					tok++;

   			case CONST(value):
					if (lhs!=null || classPath!="")
						throw "Misplaced '" + value + "'";
					lhs = value;
					tok++;

   			case DOT:
					if (lhs==null && classPath=="")
						throw "Bad '.' after null value";
					tok++;
					switch(inTokens[tok])
					{
						case IDENT(name):
							if (lhs!=null)
								lhs = Reflect.getProperty(lhs,name);
							else
							{
								var qname = classPath + "." + name;
								lhs = resolve(qname);
								classPath = (lhs==null) ? qname : "";
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
									if (arrayOpen==0 && parenOpen==1 && lhs!=null)
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
						if (lhs==null)
                  {
							lhs = getValue( inTokens.slice(start,tok-1) );
                  }
						else
						{
                     if (lastComma+1 < tok-1)
								args.push( getValue( inTokens.slice(lastComma+1,tok-1) ) );
							lhs = untyped lhs.__Run( args );
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
						if (lhs==null)
							throw "Error taking index of null object";
						var val:Dynamic = getValue( inTokens.slice(start,tok) );
						if ( !Std.is(val,Int) )
							throw "Bad array index: " + val;
						lhs = lhs[ Std.int(val) ];

   			case RPAREN: throw "Misplaced ')'";
   			case COMMA:  throw "Misplaced ','";
   			case EQUALS: throw("Misplaced '='");
   			case RARRAY: throw "Misplaced ']'";
         }
      }
		if (classPath!="")
			throw "Unresolved " + classPath;

      return lhs;
   }

	function printResult(result:String)
	{
		Sys.println(result);
	}


   function print(inString:String)
   {
      var tokens:Array<DebugToken> = null;
		try
      {
         tokens = tokenize(inString);
         var result = getValue(tokens);
         printResult(result);
      }
      catch (e:Dynamic)
      {
         Sys.println("Error while printing : " + e);//+ ( tokens==null ? "" : " : " + tokens) );
      }
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
		Sys.println(result);
	}

   function inputLoop()
   {
      var input = Sys.stdin();
      while(true)
      {
         Sys.print("debug >");
			checkStack();
         var command = input.readLine();
         var words = command.split(" ");
         switch(words[0])
         {
            case "":
               // Do nothing

            case "exit","quit":
               Debugger.exit();

            case "break","b":
					if (words.length==1)
					{
               	if (threadStopped)
                  	Sys.println("already stopped.");
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
						Sys.println("Usage: break [file line] - pause execution of one thread [when at certain point]");


            case "cont","c":
               if (!threadStopped)
                  Sys.println("Already running.");
               else
						run();

            case "vars","v":
               if (!threadStopped || vars==null)
                  Sys.println("Must break first.");
               else
               {
						Sys.println(vars);
               }


            case "frame","f":
               if (!threadStopped || stack==null )
                  Sys.println("Must break first.");
               else
               {
						var f = Std.parseInt(words[1]);
						if (f<1 || f>stack.length )
							Sys.println("Stack out of range.");
						else
						{
                  	debugQueue.add( function() setFrame(f) );
                  	waitDebugger();
						}
               }
              

            case "where","w":
               if (!threadStopped || stack==null)
                  Sys.println("Must break first.");
               else
						where();

            case "print","p":
					words.shift();
					print(words.join(" "));


            case "files","fi":
               showFiles();

            case "breakpoints","bp":
               showBreakpoints();

            case "delete","d":
               if (words[1]==null)
					{
						Sys.println("Usage : delete N");
 					}
					else
					{
                  var i = Std.parseInt(words[1]);
						Debugger.deleteBreakpoint(i);
					}

            case "help","h","?":
               Sys.println("help  - print this message");
               Sys.println("break [file line] - pause execution of one thread [when at certain point]");
               Sys.println("breakpoints - list breakpoints");
               Sys.println("delete N - delete breakpoint N");
               Sys.println("cont  - continue execution");
               Sys.println("where - print call stack");
               Sys.println("files - print file list that may be used with breakpoints");
               Sys.println("vars - print local vars for frame");
               Sys.println("exit  - exit programme");


            default:
               Sys.println("Unknown command:" + words);
         }
      }
   }

}


