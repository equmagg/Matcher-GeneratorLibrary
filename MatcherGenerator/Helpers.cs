using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Matcher.Generator
{
    internal static class Helpers
    {
        
        internal static string GetAccessibilityKeyword(Accessibility a) => a switch
        {
            Accessibility.Public => "public",
            Accessibility.Internal => "internal",
            Accessibility.Protected => "protected",
            Accessibility.Private => "private",
            Accessibility.ProtectedAndInternal => "private internal",
            Accessibility.ProtectedOrInternal => "protected internal",
            _ => "internal"
        };
        internal static bool IsPartial(INamedTypeSymbol symbol)
        {
            foreach (var decl in symbol.DeclaringSyntaxReferences)
            {
                if (decl.GetSyntax() is TypeDeclarationSyntax tds &&
                    tds.Modifiers.Any(m => m.IsKind(SyntaxKind.PartialKeyword)))
                    return true;
            }
            return false;
        }
        internal static bool IsAccessibleFromExtension(Accessibility a) =>
            a == Accessibility.Public ||
            a == Accessibility.Internal ||
            a == Accessibility.ProtectedOrInternal;
        internal static bool IsReadableFromAssembly(IFieldSymbol f, INamedTypeSymbol inType)
        {
            return f.DeclaredAccessibility switch
            {
                Accessibility.Public => true,
                Accessibility.Internal => SymbolEquals(f.ContainingAssembly, inType.ContainingAssembly),
                Accessibility.ProtectedOrInternal => true,
                _ => false
            };
        }

        internal static bool IsReadableFromAssembly(IPropertySymbol p, INamedTypeSymbol inType)
        {
            var acc = p.GetMethod?.DeclaredAccessibility ?? Accessibility.Private;
            return acc switch
            {
                Accessibility.Public => true,
                Accessibility.Internal => SymbolEquals(p.ContainingAssembly, inType.ContainingAssembly),
                Accessibility.ProtectedOrInternal => true,
                _ => false
            };
        }

        internal static List<INamedTypeSymbol> GetContainingTypes(INamedTypeSymbol typeSymbol)
        {
            var list = new List<INamedTypeSymbol>();
            var current = typeSymbol.ContainingType;
            while (current != null)
            {
                list.Insert(0, current); // outer first
                current = current.ContainingType;
            }
            return list;
        }
        internal static ITypeSymbol? GetMemberType(ISymbol m) =>
            m switch
            {
                IFieldSymbol f => f.Type,
                IPropertySymbol p => p.Type,
                _ => null
            };

        internal static IEnumerable<ISymbol> GetInstanceMembers(INamedTypeSymbol typeSymbol, bool forExtension = false)
        {
            // gather writable fields and properties
            foreach (var m in typeSymbol.GetMembers())
            {
                if (m.IsStatic)
                    continue;

                if (m is IFieldSymbol f)
                {
                    if (f.IsConst) continue;
                    if (f.IsImplicitlyDeclared) continue;
                    if (forExtension)
                    {
                        // extension method
                        if (!IsAccessibleFromExtension(f.DeclaredAccessibility))
                            continue;
                    }
                    yield return f;
                }
                else if (m is IPropertySymbol p)
                {
                    if (p.SetMethod == null) continue;
                    if (p.IsIndexer) continue;
                    if (forExtension)
                    {
                        // need getter and setter from an external static class in same assembly
                        if (!IsAccessibleFromExtension(p.GetMethod?.DeclaredAccessibility ?? Accessibility.Private)) continue;
                        if (!IsAccessibleFromExtension(p.SetMethod.DeclaredAccessibility)) continue;
                    }
                    yield return p;
                }
            }
        }
        internal static bool HasAttr(ISymbol? s, string fullName)
            => s != null && s.GetAttributes().Any(a => a.AttributeClass?.ToDisplayString() == fullName 
            || a.AttributeClass?.Name == fullName.Split('.').Last());

        internal static bool IsDerivedFrom(INamedTypeSymbol t, INamedTypeSymbol baseType)
        {
            for (var cur = t; cur != null; cur = cur.BaseType)
            {
                if (SymbolEqualityComparer.Default.Equals(cur, baseType))
                    return true;
            }
            return false;
        }
        internal static bool IsCtorCallableFromHere(IMethodSymbol ctor)
        {
            return ctor.DeclaredAccessibility == Accessibility.Public ||
                   ctor.DeclaredAccessibility == Accessibility.Internal;
        }
        internal static bool SymbolEquals(ISymbol? a, ISymbol? b)
            => SymbolEqualityComparer.Default.Equals(a, b);
        internal static bool IsNumeric(ITypeSymbol t)
        {
            if (t.NullableAnnotation == NullableAnnotation.Annotated) return false;

            return t.SpecialType switch
            {
                SpecialType.System_SByte or
                SpecialType.System_Byte or
                SpecialType.System_Int16 or
                SpecialType.System_UInt16 or
                SpecialType.System_Int32 or
                SpecialType.System_UInt32 or
                SpecialType.System_Int64 or
                SpecialType.System_UInt64 or
                SpecialType.System_IntPtr or
                SpecialType.System_UIntPtr or
                SpecialType.System_Single or
                SpecialType.System_Double or
                SpecialType.System_Decimal => true,
                _ => false
            };
        }
    }
}
