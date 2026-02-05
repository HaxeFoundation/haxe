using System.Text;

namespace ExternGenerator.Generation;

/// <summary>
/// Builds Haxe source code with proper formatting.
/// </summary>
public class HaxeCodeBuilder
{
    private readonly StringBuilder _sb = new();
    private int _indentLevel = 0;
    private const string IndentString = "\t";

    /// <summary>
    /// Gets the current indent string.
    /// </summary>
    public string CurrentIndent => string.Concat(Enumerable.Repeat(IndentString, _indentLevel));

    /// <summary>
    /// Appends a line with current indentation.
    /// </summary>
    public HaxeCodeBuilder AppendLine(string line = "")
    {
        if (string.IsNullOrEmpty(line))
        {
            _sb.AppendLine();
        }
        else
        {
            _sb.Append(CurrentIndent);
            _sb.AppendLine(line);
        }
        return this;
    }

    /// <summary>
    /// Appends text without newline.
    /// </summary>
    public HaxeCodeBuilder Append(string text)
    {
        _sb.Append(text);
        return this;
    }

    /// <summary>
    /// Appends raw text without indentation.
    /// </summary>
    public HaxeCodeBuilder AppendRaw(string text)
    {
        _sb.Append(text);
        return this;
    }

    /// <summary>
    /// Appends a raw line without indentation.
    /// </summary>
    public HaxeCodeBuilder AppendRawLine(string line = "")
    {
        _sb.AppendLine(line);
        return this;
    }

    /// <summary>
    /// Increases indentation level.
    /// </summary>
    public HaxeCodeBuilder Indent()
    {
        _indentLevel++;
        return this;
    }

    /// <summary>
    /// Decreases indentation level.
    /// </summary>
    public HaxeCodeBuilder Outdent()
    {
        if (_indentLevel > 0)
            _indentLevel--;
        return this;
    }

    /// <summary>
    /// Opens a block with a brace.
    /// </summary>
    public HaxeCodeBuilder OpenBlock(string header = "")
    {
        if (!string.IsNullOrEmpty(header))
        {
            AppendLine(header + " {");
        }
        else
        {
            AppendLine("{");
        }
        Indent();
        return this;
    }

    /// <summary>
    /// Closes a block with a brace.
    /// </summary>
    public HaxeCodeBuilder CloseBlock()
    {
        Outdent();
        AppendLine("}");
        return this;
    }

    /// <summary>
    /// Writes the package declaration.
    /// </summary>
    public HaxeCodeBuilder WritePackage(string package)
    {
        AppendLine($"package {package};");
        AppendLine();
        return this;
    }

    /// <summary>
    /// Writes an import statement.
    /// </summary>
    public HaxeCodeBuilder WriteImport(string import)
    {
        AppendLine($"import {import};");
        return this;
    }

    /// <summary>
    /// Writes imports for types used in the file.
    /// </summary>
    public HaxeCodeBuilder WriteImports(IEnumerable<string> imports)
    {
        foreach (var import in imports.OrderBy(i => i))
        {
            WriteImport(import);
        }
        if (imports.Any())
            AppendLine();
        return this;
    }

    /// <summary>
    /// Writes a metadata annotation.
    /// </summary>
    public HaxeCodeBuilder WriteMeta(string meta)
    {
        AppendLine(meta);
        return this;
    }

    /// <summary>
    /// Writes a @:native annotation.
    /// </summary>
    public HaxeCodeBuilder WriteNative(string nativeName)
    {
        AppendLine($"@:native(\"{EscapeString(nativeName)}\")");
        return this;
    }

    /// <summary>
    /// Writes a doc comment if text is provided.
    /// </summary>
    public HaxeCodeBuilder WriteDocComment(string? docText)
    {
        if (string.IsNullOrEmpty(docText))
            return this;

        // Already formatted doc comment
        if (docText.TrimStart().StartsWith("/**"))
        {
            AppendRawLine(docText);
            return this;
        }

        // Single line
        if (!docText.Contains('\n') && docText.Length < 80)
        {
            AppendLine($"/** {docText} */");
            return this;
        }

        // Multi-line
        AppendLine("/**");
        foreach (var line in docText.Split('\n'))
        {
            AppendLine($" * {line.Trim()}");
        }
        AppendLine(" */");
        return this;
    }

    /// <summary>
    /// Writes a class declaration.
    /// </summary>
    public HaxeCodeBuilder WriteClassDecl(
        string className,
        IEnumerable<string>? typeParams = null,
        string? extends = null,
        IEnumerable<string>? implements = null,
        bool isExtern = true,
        bool isInterface = false,
        bool isAbstract = false)
    {
        var sb = new StringBuilder();

        if (isExtern)
            sb.Append("extern ");

        if (isInterface)
            sb.Append("interface ");
        else if (isAbstract)
            sb.Append("class ");
        else
            sb.Append("class ");

        sb.Append(className);

        if (typeParams != null && typeParams.Any())
        {
            sb.Append('<');
            sb.Append(string.Join(", ", typeParams));
            sb.Append('>');
        }

        if (!string.IsNullOrEmpty(extends))
        {
            sb.Append(" extends ");
            sb.Append(extends);
        }

        if (implements != null && implements.Any())
        {
            foreach (var impl in implements)
            {
                sb.Append(" implements ");
                sb.Append(impl);
            }
        }

        OpenBlock(sb.ToString());
        return this;
    }

    /// <summary>
    /// Writes an enum abstract declaration.
    /// </summary>
    public HaxeCodeBuilder WriteEnumAbstract(string name, string underlyingType = "Int")
    {
        AppendLine($"extern enum abstract {name}({underlyingType}) {{");
        Indent();
        return this;
    }

    /// <summary>
    /// Writes an enum value.
    /// </summary>
    public HaxeCodeBuilder WriteEnumValue(string name, string? value = null)
    {
        if (value != null)
            AppendLine($"var {name} = {value};");
        else
            AppendLine($"var {name};");
        return this;
    }

    /// <summary>
    /// Writes a field declaration (property or variable).
    /// </summary>
    public HaxeCodeBuilder WriteField(
        string name,
        string type,
        bool isStatic = false,
        bool isReadOnly = false,
        string? getter = null,
        string? setter = null)
    {
        var sb = new StringBuilder();

        if (isStatic)
            sb.Append("static ");

        sb.Append("var ");
        sb.Append(name);

        // Property access
        if (getter != null || setter != null)
        {
            string g = getter ?? "default";
            string s = setter ?? (isReadOnly ? "never" : "default");
            sb.Append($"({g}, {s})");
        }
        else if (isReadOnly)
        {
            sb.Append("(default, never)");
        }

        sb.Append(':');
        sb.Append(type);
        sb.Append(';');

        AppendLine(sb.ToString());
        return this;
    }

    /// <summary>
    /// Writes a method declaration.
    /// </summary>
    public HaxeCodeBuilder WriteMethod(
        string name,
        IEnumerable<(string Name, string Type, bool Optional)>? parameters = null,
        string returnType = "Void",
        bool isStatic = false,
        IEnumerable<string>? typeParams = null,
        string? nativeName = null)
    {
        if (nativeName != null && nativeName != name)
        {
            AppendLine($"@:native(\"{EscapeString(nativeName)}\")");
        }

        var sb = new StringBuilder();

        if (isStatic)
            sb.Append("static ");

        sb.Append("function ");
        sb.Append(name);

        if (typeParams != null && typeParams.Any())
        {
            sb.Append('<');
            sb.Append(string.Join(", ", typeParams));
            sb.Append('>');
        }

        sb.Append('(');
        if (parameters != null)
        {
            var paramList = parameters.Select(p =>
            {
                string paramStr = p.Optional ? $"?{p.Name}" : p.Name;
                return $"{paramStr}:{p.Type}";
            });
            sb.Append(string.Join(", ", paramList));
        }
        sb.Append("):");
        sb.Append(returnType);
        sb.Append(';');

        AppendLine(sb.ToString());
        return this;
    }

    /// <summary>
    /// Writes a constructor declaration.
    /// </summary>
    public HaxeCodeBuilder WriteConstructor(
        IEnumerable<(string Name, string Type, bool Optional)>? parameters = null)
    {
        var sb = new StringBuilder();
        sb.Append("function new(");

        if (parameters != null)
        {
            var paramList = parameters.Select(p =>
            {
                string paramStr = p.Optional ? $"?{p.Name}" : p.Name;
                return $"{paramStr}:{p.Type}";
            });
            sb.Append(string.Join(", ", paramList));
        }

        sb.Append("):Void;");

        AppendLine(sb.ToString());
        return this;
    }

    /// <summary>
    /// Writes an @:overload annotation for method overloads.
    /// </summary>
    public HaxeCodeBuilder WriteOverload(
        IEnumerable<(string Name, string Type, bool Optional)>? parameters = null,
        string returnType = "Void",
        IEnumerable<string>? typeParams = null)
    {
        var sb = new StringBuilder();
        sb.Append("@:overload(function");

        if (typeParams != null && typeParams.Any())
        {
            sb.Append('<');
            sb.Append(string.Join(", ", typeParams));
            sb.Append('>');
        }

        sb.Append('(');
        if (parameters != null)
        {
            var paramList = parameters.Select(p =>
            {
                string paramStr = p.Optional ? $"?{p.Name}" : p.Name;
                return $"{paramStr}:{p.Type}";
            });
            sb.Append(string.Join(", ", paramList));
        }
        sb.Append("):");
        sb.Append(returnType);
        sb.Append(" {})");

        AppendLine(sb.ToString());
        return this;
    }

    /// <summary>
    /// Escapes a string for use in Haxe source.
    /// </summary>
    public static string EscapeString(string s)
    {
        return s
            .Replace("\\", "\\\\")
            .Replace("\"", "\\\"")
            .Replace("\n", "\\n")
            .Replace("\r", "\\r")
            .Replace("\t", "\\t");
    }

    /// <summary>
    /// Returns the built code as a string.
    /// </summary>
    public override string ToString()
    {
        return _sb.ToString();
    }

    /// <summary>
    /// Clears the builder.
    /// </summary>
    public HaxeCodeBuilder Clear()
    {
        _sb.Clear();
        _indentLevel = 0;
        return this;
    }
}
