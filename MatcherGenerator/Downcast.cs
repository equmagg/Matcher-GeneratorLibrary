using System;
using System.Linq;
using System.Text;
using System.Collections.Generic;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Text;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using static Matcher.Generator.Helpers;

namespace Matcher.Generator
{
    [Generator]
    public sealed class DowncastGenerator : ISourceGenerator
    {
        private const string AttributeSource = 
@"using System;

namespace Matcher
{
    [AttributeUsage(AttributeTargets.Class, Inherited = false, AllowMultiple = false)]
    public sealed class DowncastableAttribute : Attribute { }

    [AttributeUsage(AttributeTargets.Class | AttributeTargets.Field | AttributeTargets.Property, Inherited = false, AllowMultiple = false)]
    public sealed class DowncastIgnoreAttribute : Attribute { }

    [AttributeUsage(AttributeTargets.Constructor, Inherited = false, AllowMultiple = false)]
    public sealed class DowncastConstructorAttribute : Attribute { }
}
";

        public void Initialize(GeneratorInitializationContext context)
        {
            context.RegisterForPostInitialization(pi =>
            {
                pi.AddSource("DowncastAttributes.g.cs", SourceText.From(AttributeSource, Encoding.UTF8));
            });

            context.RegisterForSyntaxNotifications(() => new SyntaxReceiver());
        }
        public void Execute(GeneratorExecutionContext context)
        {
            if (context.SyntaxReceiver is not SyntaxReceiver rx)
                return;

            var compilation = context.Compilation;
            var downcastableAttr = compilation.GetTypeByMetadataName("Matcher.DowncastableAttribute");
            var baseTypes = new HashSet<INamedTypeSymbol>(SymbolEqualityComparer.Default);
            foreach (var cds in rx.CandidateClasses)
            {
                var model = compilation.GetSemanticModel(cds.SyntaxTree);
                if (model.GetDeclaredSymbol(cds) is not INamedTypeSymbol type) continue;

                bool hasAttr = type.GetAttributes().Any(a => downcastableAttr != null && 
                    SymbolEqualityComparer.Default.Equals(a.AttributeClass, downcastableAttr) ||
                    a.AttributeClass?.ToDisplayString() == "Matcher.DowncastableAttribute" ||
                    a.AttributeClass?.Name == "DowncastableAttribute");

                if (hasAttr && type.TypeKind == TypeKind.Class)
                    baseTypes.Add(type);
            }
            var allClasses = GetAllNamedTypes(compilation)
                .Where(t => t.TypeKind == TypeKind.Class)
                .ToArray();
            foreach (var baseType in baseTypes)
            {
                var source = GenerateForBase(baseType, allClasses, context);
                var hint = GetHintName(baseType);
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

            return $"{safe}.Downcast.g.cs";
        }
        static string GetExtensionClassName(INamedTypeSymbol typeSymbol)
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

            return $"{flat}DowncastExtensions";
        }
        private static IEnumerable<INamedTypeSymbol> GetAllNamedTypes(Compilation compilation)
        {
            foreach (var st in compilation.SyntaxTrees)
            {
                var root = st.GetRoot();
                foreach (var cds in root.DescendantNodes().OfType<ClassDeclarationSyntax>())
                {
                    var model = compilation.GetSemanticModel(st);
                    if (model.GetDeclaredSymbol(cds) is INamedTypeSymbol sym)
                        yield return sym;
                }
            }
        }
        struct TargetInfo
        {
            public INamedTypeSymbol Type;
            public bool Ignore;
            public IMethodSymbol? PreferredCtor;
            public IReadOnlyList<(IParameterSymbol param, ISymbol? bound)> BoundParams;
            public bool HasParameterlessCtor;
            public bool CtorAccessible;
            public TargetInfo(INamedTypeSymbol type, bool ignore, IMethodSymbol? preferredCtor, 
                IReadOnlyList<(IParameterSymbol param, ISymbol? bound)> boundParams, 
                bool hasParameterlessCtor, bool ctorAccessible)
            {
                Type = type;
                Ignore = ignore;
                PreferredCtor = preferredCtor;
                BoundParams = boundParams;
                HasParameterlessCtor = hasParameterlessCtor;
                CtorAccessible = ctorAccessible;
            }
        }

        private static string GenerateForBase(INamedTypeSymbol baseType, INamedTypeSymbol[] allClasses, GeneratorExecutionContext context)
        {
            var sb = new StringBuilder();
            var ns = baseType.ContainingNamespace?.ToDisplayString() ?? "";
            var baseFull = baseType.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat);
            var candidates = new HashSet<INamedTypeSymbol>(allClasses
                .Where(t => IsDerivedFrom(t, baseType)), SymbolEqualityComparer.Default);
            var baseReadable = GetReadableMap(baseType);
            var targetInfos = new List<TargetInfo>();

            foreach (INamedTypeSymbol t in candidates)
            {
                bool ignored = HasAttr(t, "Matcher.DowncastIgnoreAttribute");

                // find constructor
                var ctors = t.InstanceConstructors.Where(c => !c.IsStatic).ToArray();
                var preferred = ctors.FirstOrDefault(c => HasAttr(c, "Matcher.DowncastConstructorAttribute"));

                bool ctorAccessible = preferred is null ? true : IsCtorCallableFromHere(preferred);

                var bound = new List<(IParameterSymbol, ISymbol?)>();
                if (preferred is not null)
                {
                    foreach (var p in preferred.Parameters)
                    {
                        baseReadable.TryGetValue(p.Name, out var member);
                        bound.Add((p, member));
                    }
                }

                bool hasParamless =
                    ctors.Any(c => c.Parameters.Length == 0 && IsCtorCallableFromHere(c));

                targetInfos.Add(new TargetInfo(
                    t, ignored, preferred,
                    bound, hasParamless, ctorAccessible));
            }

            sb.AppendLine("// <auto-generated/>");
            sb.AppendLine("using System;");

            if (!string.IsNullOrEmpty(ns))
            {
                sb.AppendLine($"namespace {ns}");
                sb.AppendLine("{");
            }
            var containing = GetContainingTypes(baseType);
            int opened = 0;
            foreach (var ct in containing)
            {
                if (!IsPartial(ct)) break;
                var acc = GetAccessibilityKeyword(ct.DeclaredAccessibility);
                var kind = ct.TypeKind == TypeKind.Struct ? "struct" : "class";
                sb.AppendLine($"{acc} partial {kind} {ct.Name}");
                sb.AppendLine("{");
                opened++;
            }

            var baseName = baseType.Name;
            var inPartial = IsPartial(baseType);
            void EmitCopyBaseMembers(StringBuilder sb2, bool forExtension)
            {
                foreach (var m in GetInstanceMembers(baseType, forExtension))
                {
                    if (m is IFieldSymbol f)
                        sb2.AppendLine($"        __dst.{f.Name} = __src.{f.Name};");
                    else if (m is IPropertySymbol p)
                        sb2.AppendLine($"        __dst.{p.Name} = __src.{p.Name};");
                }
            }
            void EmitConcreteFactory(StringBuilder sb2, TargetInfo ti)
            {
                var tFull = ti.Type.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat);
                var fn = "__Create__" + tFull.Replace("global::", "").Replace('.', '_').Replace('+', '_').Replace('<', '_').Replace('>', '_').Replace(',', '_');

                sb2.AppendLine($"    private static {tFull} {fn}({baseFull} __src)");
                sb2.AppendLine("    {");
                if (ti.Ignore)
                {
                    sb2.AppendLine($"        throw new InvalidOperationException(\"Casting to '{ti.Type.ToDisplayString()}' is disabled by [DowncastIgnore].\");");
                    sb2.AppendLine("    }");
                    sb2.AppendLine();
                    return;
                }
                if (ti.PreferredCtor != null)
                {
                    if (!ti.CtorAccessible)
                    {
                        sb2.AppendLine($"        throw new MissingMethodException(\"Constructor marked with [DowncastConstructor] on '{ti.Type.ToDisplayString()}' is not accessible (must be public or internal).\");");
                        sb2.AppendLine("    }");
                        sb2.AppendLine();
                        return;
                    }
                    if (ti.BoundParams.Any(bp => bp.bound is null))
                    {
                        var missing = ti.BoundParams.Where(bp => bp.bound is null).Select(bp => bp.param.Name).ToArray();
                        sb2.AppendLine($"        throw new InvalidOperationException(\"No matching base member for constructor parameter(s): {string.Join(", ", missing)}.\");");
                        sb2.AppendLine("    }");
                        sb2.AppendLine();
                        return;
                    }

                    int idx = 0;
                    foreach (var (param, member) in ti.BoundParams)
                    {
                        var name = $"__a{idx++}";
                        if (member is IFieldSymbol f)
                            sb2.AppendLine($"        var {name} = __src.{f.Name};");
                        else if (member is IPropertySymbol p)
                            sb2.AppendLine($"        var {name} = __src.{p.Name};");
                        else
                            sb2.AppendLine($"        var {name} = default({param.Type.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat)}); // unreachable");
                    }

                    var args = string.Join(", ", Enumerable.Range(0, ti.BoundParams.Count).Select(i => $"__a{i}"));
                    sb2.AppendLine($"        return new {tFull}({args});");
                    sb2.AppendLine("    }");
                    sb2.AppendLine();
                    return;
                }
                if (ti.HasParameterlessCtor)
                {
                    sb2.AppendLine($"        return new {tFull}();");
                    sb2.AppendLine("    }");
                    sb2.AppendLine();
                    return;
                }

                sb2.AppendLine($"        throw new MissingMethodException(\"Type '{ti.Type.ToDisplayString()}' must have a public/internal parameterless constructor or a constructor marked with [DowncastConstructor].\");");
                sb2.AppendLine("    }");
                sb2.AppendLine();
            }
            void EmitGenericFactory(StringBuilder sb2, bool forExtension)
            {
                var recv = forExtension ? $"{baseFull} __src" : $"{baseFull} __src";
                var sig = forExtension
                    ? $"private static TTarget __CreateTarget<TTarget>({recv}) where TTarget : {baseFull}"
                    : $"private static TTarget __CreateTarget<TTarget>({recv}) where TTarget : {baseFull}";

                sb2.AppendLine($"    {sig}");
                sb2.AppendLine("    {");
                //branches for each known type
                foreach (var ti in targetInfos)
                {
                    var tFull = ti.Type.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat);
                    var fn = "__Create__" + tFull.Replace("global::", "").Replace('.', '_').Replace('+', '_').Replace('<', '_').Replace('>', '_').Replace(',', '_');
                    sb2.AppendLine($"        if (typeof(TTarget) == typeof({tFull}))");
                    sb2.AppendLine("        {");
                    sb2.AppendLine($"            var obj = {fn}(__src);");
                    sb2.AppendLine($"            return (TTarget)(object)obj;"); // safe because branch checks exact type
                    sb2.AppendLine("        }");
                }

                // if type is unknown
                sb2.AppendLine("        throw new NotSupportedException($\"Unknown or unsupported target type: {typeof(TTarget).FullName}.\");");
                sb2.AppendLine("    }");
                sb2.AppendLine();
            }
            if (inPartial)
            {
                var acc = GetAccessibilityKeyword(baseType.DeclaredAccessibility);
                sb.AppendLine($"{acc} partial class {baseName}");
                sb.AppendLine("{");
                sb.AppendLine("    /// <summary>");
                sb.AppendLine("    /// Creates a new instance of <typeparamref name=\"TTarget\"/> and copies base members.");
                sb.AppendLine("    /// Supports base->derived, derived->base, and derived->derived casts.");
                sb.AppendLine("    /// </summary>");
                sb.AppendLine($"    public TTarget Cast<TTarget>() where TTarget : {baseFull}");
                sb.AppendLine("    {");
                sb.AppendLine("        if (this is null) throw new ArgumentNullException(\"source\");");

                foreach (var ti in targetInfos.Where(t => t.Ignore))
                {
                    var tFull = ti.Type.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat);
                    sb.AppendLine($"        if (this is {tFull}) throw new InvalidOperationException(\"Casting from '{tFull}' is disabled by [DowncastIgnore].\");");
                }

                sb.AppendLine("        var result = __CreateTarget<TTarget>(this);");
                sb.AppendLine($"        {baseFull} __src = this;");
                sb.AppendLine($"        {baseFull} __dst = result;");
                EmitCopyBaseMembers(sb, forExtension: false);
                sb.AppendLine("        return result;");
                sb.AppendLine("    }");
                sb.AppendLine();

                // concrete factory
                foreach (var ti in targetInfos)
                    EmitConcreteFactory(sb, ti);

                // generic factory
                EmitGenericFactory(sb, forExtension: false);

                sb.AppendLine("}"); // end base partial class
            }
            else
            {
                var extName = GetExtensionClassName(baseType);
                sb.AppendLine($"public static class {extName}");
                sb.AppendLine("{");
                sb.AppendLine("    /// <summary>");
                sb.AppendLine("    /// Creates a new instance of <typeparamref name=\"TTarget\"/> and copies base members.");
                sb.AppendLine("    /// Supports base->derived, derived->base, and derived->derived casts.");
                sb.AppendLine("    /// No reflection. NativeAOT-friendly.");
                sb.AppendLine("    /// </summary>");
                sb.AppendLine($"    public static TTarget Cast<TTarget>(this {baseFull} source) where TTarget : {baseFull}");
                sb.AppendLine("    {");
                sb.AppendLine("        if (source is null) throw new ArgumentNullException(nameof(source));");

                foreach (var ti in targetInfos.Where(t => t.Ignore))
                {
                    var tFull = ti.Type.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat);
                    sb.AppendLine($"        if (source is {tFull}) throw new InvalidOperationException(\"Casting from '{tFull}' is disabled by [DowncastIgnore].\");");
                }
                sb.AppendLine("        var result = __CreateTarget<TTarget>(source);");
                sb.AppendLine($"        var __src = source;");
                sb.AppendLine($"        {baseFull} __dst = result;");
                EmitCopyBaseMembers(sb, forExtension: true);
                sb.AppendLine("        return result;");
                sb.AppendLine("    }");
                sb.AppendLine();

                foreach (var ti in targetInfos)
                    EmitConcreteFactory(sb, ti);

                EmitGenericFactory(sb, forExtension: true);

                sb.AppendLine("}"); // end static extension class
            }
            for (int i = 0; i < opened; i++)
                sb.AppendLine("}");

            if (!string.IsNullOrEmpty(ns))
                sb.AppendLine("}");

            return sb.ToString();

        }
        private static Dictionary<string, ISymbol> GetReadableMap(INamedTypeSymbol baseType)
        {
            var dict = new Dictionary<string, ISymbol>(StringComparer.OrdinalIgnoreCase);
            foreach (var m in baseType.GetMembers())
            {
                if (m.IsStatic) continue;

                if (m is IFieldSymbol f)
                {
                    if (f.IsImplicitlyDeclared) continue; // exclude backing fields
                    dict[f.Name] = f;
                }
                else if (m is IPropertySymbol p)
                {
                    if (p.IsIndexer) continue;
                    if (p.GetMethod is null) continue;
                    dict[p.Name] = p;
                }
            }
            return dict;
        }

        private sealed class SyntaxReceiver : ISyntaxReceiver
        {
            public List<ClassDeclarationSyntax> CandidateClasses { get; } = new();

            public void OnVisitSyntaxNode(SyntaxNode node)
            {
                if (node is ClassDeclarationSyntax cds && cds.AttributeLists.Count > 0)
                {
                    CandidateClasses.Add(cds);
                }
            }
        }

    }
}
