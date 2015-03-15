package sys.db;

class Sqlite
{
	static var type:Class<cs.system.data.IDbConnection>;
	/**
		Opens a new SQLite connection on the specified path.
		Note that you will need a SQLite ADO.NET Provider (see http://www.mono-project.com/docs/database-access/providers/sqlite/).
		Also note that this will try to open an assembly named `Mono.Data.Sqlite` if it wasn't loaded yet.
	**/
	public static function open(file:String):sys.db.Connection
	{
		var cnxString = 'Data Source=$file';
		if (type == null)
		{
			var t = null;
			var assemblies = cs.system.AppDomain.CurrentDomain.GetAssemblies();
			for (i in 0...assemblies.Length)
			{
				var a = assemblies[i];
				t = a.GetType('Mono.Data.Sqlite.SqliteConnection');
				if (t == null)
					t = a.GetType('System.Data.SQLite.SQLiteConnection');
				if (t != null)
				{
					break;
				}
			}

			if (t == null)
			{
				var asm = cs.system.reflection.Assembly.Load('Mono.Data.Sqlite');
				t = asm.GetType('Mono.Data.Sqlite.SqliteConnection');
			}

			if (t != null)
				type = cast cs.Lib.fromNativeType(t);
		}

		if (type == null)
		{
			throw "No ADO.NET SQLite provider was found!";
		}
		var ret = Type.createInstance(type,[cnxString]);
		ret.Open();
		return cs.db.AdoNet.create(ret,'SQLite');
	}
}
