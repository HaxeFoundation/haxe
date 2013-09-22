package sys.db;

class Sqlite
{
	static var init = false;
	/**
		Opens a new SQLite connection on the specified path.
		Note that you will need a SQLite JDBC driver (like https://bitbucket.org/xerial/sqlite-jdbc).
	**/
	public static function open(file:String):sys.db.Connection
	{
		if (!init)
		{
			try java.lang.Class.forName("org.sqlite.JDBC") catch(e:Dynamic) throw e;
			init = true;
		}
		try
		{
			var cnx = java.sql.DriverManager.getConnection("jdbc:sqlite:" + file);
			return java.db.Jdbc.create(cnx);
		} catch(e:Dynamic) throw e;
	}
}
