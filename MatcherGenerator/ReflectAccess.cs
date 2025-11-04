using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;
using static Matcher.Generator.Helpers;

namespace MatcherGenerator
{
    [Generator]
    public sealed class ReflectAccessGenerator : ISourceGenerator
    {
        private const string AttributeSource =
@"using System;

namespace Matcher
{
    [Flags]
    public enum ReflectKind
    {
        None  = 0,
        Read  = 1,
        Write = 1 << 1,
        All   = Read | Write
    }

    [AttributeUsage(AttributeTargets.Class | AttributeTargets.Struct, Inherited = false, AllowMultiple = false)]
    public sealed class ReflectedAttribute : Attribute
    {
        public ReflectedAttribute() : this(ReflectKind.Read | ReflectKind.Write) { }
        public ReflectedAttribute(ReflectKind kind) { Kind = kind; }
        public ReflectKind Kind { get; }
    }
}
";
        public void Initialize(GeneratorInitializationContext context)
        {
            context.RegisterForPostInitialization(pi =>
            {
                pi.AddSource("ReflectedAttribute.g.cs", SourceText.From(AttributeSource, Encoding.UTF8));
            });

            context.RegisterForSyntaxNotifications(() => new SyntaxReceiver());
        }
        public void Execute(GeneratorExecutionContext context)
        {
            var receiver = context.SyntaxReceiver as SyntaxReceiver;
            if (receiver == null) return;
            var compilation = context.Compilation;
            var reflectedAttr = compilation.GetTypeByMetadataName("Matcher.ReflectedAttribute");
            var reflectKind = compilation.GetTypeByMetadataName("Matcher.ReflectKind");

            var annotated = new HashSet<INamedTypeSymbol>(SymbolEqualityComparer.Default);

            // Collect attributed types
            foreach (var classDecl in receiver.CandidateClasses)
            {
                var model = compilation.GetSemanticModel(classDecl.SyntaxTree);
                var symbol = model.GetDeclaredSymbol(classDecl) as INamedTypeSymbol;
                if (symbol == null) continue;

                var has = symbol.GetAttributes().Any(a =>
                    (reflectedAttr != null && SymbolEqualityComparer.Default.Equals(a.AttributeClass, reflectedAttr)) ||
                    a.AttributeClass?.ToDisplayString() == "Matcher.ReflectedAttribute" ||
                    a.AttributeClass?.Name == "ReflectedAttribute");

                if (has) annotated.Add(symbol);
            }
            // Generate per type
            foreach (var type in annotated)
            {
                // Determine requested kind flags
                var kind = GetKind(type, reflectedAttr, reflectKind);

                var source = GenerateForType(type, kind, context);
                var hint = GetHintName(type);
                context.AddSource(hint, SourceText.From(source, Encoding.UTF8));
            }
        }
        private static string GetHintName(INamedTypeSymbol typeSymbol)
        {
            var full = typeSymbol.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat);
            if (full.StartsWith("global::", StringComparison.Ordinal))
                full = full.Substring("global::".Length);

            var safe = full
                .Replace('.', '_')
                .Replace('+', '_')
                .Replace('<', '_')
                .Replace('>', '_')
                .Replace(',', '_');

            return $"{safe}.ReflectedAccess.g.cs";
        }
        private static int GetKind(INamedTypeSymbol type, INamedTypeSymbol? reflectedAttr, INamedTypeSymbol? reflectKind)
        {
            // Default Read|Write == 3
            int result = 3;

            foreach (var a in type.GetAttributes())
            {
                if (reflectedAttr != null && SymbolEqualityComparer.Default.Equals(a.AttributeClass, reflectedAttr) ||
                    a.AttributeClass?.ToDisplayString() == "Matcher.ReflectedAttribute" ||
                    a.AttributeClass?.Name == "ReflectedAttribute")
                {
                    if (a.ConstructorArguments.Length == 1)
                    {
                        var v = a.ConstructorArguments[0];
                        if (v.Kind == TypedConstantKind.Enum)
                        {
                            // enum as integral mask
                            if (v.Value is IConvertible ic)
                                result = ic.ToInt32(System.Globalization.CultureInfo.InvariantCulture);
                        }
                    }
                }
            }
            return result;
        }
        private static string GetExtensionClassName(INamedTypeSymbol typeSymbol)
        {
            var full = typeSymbol.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat);
            if (full.StartsWith("global::", StringComparison.Ordinal))
                full = full.Substring("global::".Length);

            var flat = full
                .Replace('.', '_')
                .Replace('+', '_')
                .Replace('<', '_')
                .Replace('>', '_')
                .Replace(',', '_');

            return $"{flat}ReflectedExtensions";
        }
        private static string GenerateForType(INamedTypeSymbol type, int kindMask, GeneratorExecutionContext ctx)
        {
            var ns = type.ContainingNamespace?.ToDisplayString() ?? "";
            var sb = new StringBuilder();

            sb.AppendLine("// <auto-generated />");
            sb.AppendLine("using System;");

            if (!string.IsNullOrEmpty(ns))
            {
                sb.AppendLine($"namespace {ns}");
                sb.AppendLine("{");
            }

            var extName = GetExtensionClassName(type);
            var fullT = type.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat);

            sb.AppendLine($"public static class {extName}");
            sb.AppendLine("{");

            var readMembers = GetReadableMembers(type).DistinctBy(n => n.Name).ToArray();
            var writeMembers = GetWritableMembers(type).DistinctBy(n => n.Name).ToArray();

            bool emitRead = (kindMask & 1) != 0;
            bool emitWrite = (kindMask & 2) != 0;
            if (emitRead)
            {
                // TryGetByName
                sb.AppendLine($"    /// <summary>Reads a member value by its name. Returns true on success.</summary>");
                sb.AppendLine($"    public static bool TryGetByName(this {fullT} obj, string name, out object value)");
                sb.AppendLine("    {");
                sb.AppendLine("        if (name == null) { value = null; return false; }");
                if (readMembers.Length > 0)
                {
                    sb.AppendLine("        switch (name)");
                    sb.AppendLine("        {");
                    foreach (var m in readMembers)
                    {
                        sb.AppendLine($"            case \"{m.Name}\":");
                        sb.AppendLine($"                value = obj.{m.Name};");
                        sb.AppendLine("                return true;");
                    }
                    sb.AppendLine("            default:");
                    sb.AppendLine("                value = null;");
                    sb.AppendLine("                return false;");
                    sb.AppendLine("        }");
                }
                else
                {
                    sb.AppendLine("        value = null; return false;");
                }
                sb.AppendLine("    }");

                // GetByName
                sb.AppendLine();
                sb.AppendLine($"    /// <summary>Reads a member value by its name.</summary>");
                sb.AppendLine($"    /// <exception cref=\"ArgumentOutOfRangeException\"></exception>");
                sb.AppendLine($"    public static object GetByName(this {fullT} obj, string name)");
                sb.AppendLine("    {");
                sb.AppendLine("        object value;");
                sb.AppendLine("        if (TryGetByName(obj, name, out value)) return value;");
                sb.AppendLine("        throw new ArgumentOutOfRangeException(\"name\", name, \"Member not found.\");");
                sb.AppendLine("    }");
            }
            if (emitWrite)
            {
                // TrySetByName
                sb.AppendLine();
                sb.AppendLine($"    /// <summary>Writes a member value by its name. Returns true on success.</summary>");
                sb.AppendLine($"    public static bool TrySetByName(this {fullT} obj, string name, object value)");
                sb.AppendLine("    {");
                sb.AppendLine("        if (name == null) return false;");

                if (writeMembers.Length > 0)
                {
                    sb.AppendLine("        switch (name)");
                    sb.AppendLine("        {");
                    foreach (var m in writeMembers)
                    {
                        var t = GetMemberType(m);
                        if (t == null) continue;

                        var tStr = t.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat);
                        var isRef = !t.IsValueType;

                        // Nullable<T>?
                        INamedTypeSymbol? nullable = null;
                        ITypeSymbol? nullableUnderlying = null;
                        if (t is INamedTypeSymbol named && named.IsGenericType && named.ConstructedFrom?.ToDisplayString() == "System.Nullable<T>")
                        {
                            nullable = named;
                            nullableUnderlying = named.TypeArguments[0];
                        }

                        sb.AppendLine($"            case \"{m.Name}\":");
                        sb.AppendLine("            {");
                        if (nullable != null && nullableUnderlying != null)
                        {
                            var uStr = nullableUnderlying.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat);
                            sb.AppendLine("                if (value == null) { obj." + m.Name + " = default(" + tStr + "); return true; }");
                            sb.AppendLine("                if (value is " + uStr + " tmpU) { obj." + m.Name + " = tmpU; return true; }");
                            sb.AppendLine("                return false;");
                        }
                        else if (isRef)
                        {
                            sb.AppendLine("                if (value == null) { obj." + m.Name + " = (" + tStr + ")value; return true; }");
                            sb.AppendLine("                if (value is " + tStr + " tmp) { obj." + m.Name + " = tmp; return true; }");
                            sb.AppendLine("                return false;");
                        }
                        else
                        {
                            // strict value type check
                            sb.AppendLine("                if (value is " + tStr + " tmp) { obj." + m.Name + " = tmp; return true; }");
                            sb.AppendLine("                return false;");
                        }
                        sb.AppendLine("            }");
                    }
                    sb.AppendLine("            default:");
                    sb.AppendLine("                return false;");
                    sb.AppendLine("        }");
                }
                else
                {
                    sb.AppendLine("        return false;");
                }
                sb.AppendLine("    }");

                // SetByName
                sb.AppendLine();
                sb.AppendLine($"    /// <summary>Writes a member value by its name.</summary>");
                sb.AppendLine($"    /// <exception cref=\"ArgumentOutOfRangeException\"></exception>");
                sb.AppendLine($"    public static void SetByName(this {fullT} obj, string name, object value)");
                sb.AppendLine("    {");
                sb.AppendLine("        if (!TrySetByName(obj, name, value))");
                sb.AppendLine("            throw new ArgumentOutOfRangeException(\"name\", name, \"Member not found or incompatible value type.\");");
                sb.AppendLine("    }");
            }
            sb.AppendLine("}"); // end static class
            if (!string.IsNullOrEmpty(ns))
                sb.AppendLine("}");

            return sb.ToString();
        }
        private static IEnumerable<ISymbol> GetReadableMembers(INamedTypeSymbol type)
        {
            // Fields readable from this assembly
            foreach (var m in type.GetMembers())
            {
                if (m.IsStatic) continue;

                if (m is IFieldSymbol f)
                {
                    if (f.IsConst) continue;
                    if (f.IsImplicitlyDeclared) continue;
                    if (IsReadableFromAssembly(f, type))
                        yield return f;
                }
                else if (m is IPropertySymbol p)
                {
                    if (p.IsIndexer) continue;
                    if (p.GetMethod == null) continue;
                    if (IsReadableFromAssembly(p, type))
                        yield return p;
                }
            }
        }

        private static IEnumerable<ISymbol> GetWritableMembers(INamedTypeSymbol type)
        {
            // accessible from an extension context in the same assembly
            foreach (var m in type.GetMembers())
            {
                if (m.IsStatic) continue;

                if (m is IFieldSymbol f)
                {
                    if (f.IsConst) continue;
                    if (f.IsReadOnly) continue;
                    if (f.IsImplicitlyDeclared) continue;
                    if (IsAccessibleFromExtension(f.DeclaredAccessibility))
                        yield return f;
                }
                else if (m is IPropertySymbol p)
                {
                    if (p.IsIndexer) continue;
                    if (p.SetMethod == null) continue;
                    if (IsAccessibleFromExtension(p.GetMethod?.DeclaredAccessibility ?? Accessibility.Private) &&
                        IsAccessibleFromExtension(p.SetMethod.DeclaredAccessibility))
                        yield return p;
                }
            }
        }
        private sealed class SyntaxReceiver : ISyntaxReceiver
        {
            public List<ClassDeclarationSyntax> CandidateClasses { get; } = new List<ClassDeclarationSyntax>();

            public void OnVisitSyntaxNode(SyntaxNode node)
            {
                // Keep it aligned with your DeepCopy: only classes with attributes.
                if (node is ClassDeclarationSyntax cds && cds.AttributeLists.Count > 0)
                    CandidateClasses.Add(cds);
            }
        }
    }
    internal static class LinqCompat
    {
        public static IEnumerable<TSource> DistinctBy<TSource, TKey>(
            this IEnumerable<TSource> source, Func<TSource, TKey> keySelector)
        {
            var set = new HashSet<TKey>();
            foreach (var item in source)
            {
                if (set.Add(keySelector(item)))
                    yield return item;
            }
        }
    }
}
