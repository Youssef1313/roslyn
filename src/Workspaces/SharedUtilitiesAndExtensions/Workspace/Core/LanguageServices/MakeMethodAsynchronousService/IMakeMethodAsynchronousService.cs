// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using Microsoft.CodeAnalysis.Host;
using Microsoft.CodeAnalysis.Shared.Extensions;

namespace Microsoft.CodeAnalysis.Shared.LanguageServices.MakeMethodAsynchronousService
{
    internal interface IMakeMethodAsynchronousService : ILanguageService
    {
        bool IsIAsyncEnumerableOrEnumerator(ITypeSymbol returnType, KnownTaskTypes knownTaskTypes)
            => returnType.OriginalDefinition.Equals(knownTaskTypes.IAsyncEnumerableOfT, SymbolEqualityComparer.Default) ||
                returnType.OriginalDefinition.Equals(knownTaskTypes.IAsyncEnumeratorOfT, SymbolEqualityComparer.Default);

        bool IsTaskLikeType(ITypeSymbol type, KnownTaskTypes knownTaskTypes)
        {
            if (type.Equals(knownTaskTypes.Task, SymbolEqualityComparer.Default) ||
                type.Equals(knownTaskTypes.TaskOfT, SymbolEqualityComparer.Default) ||
                type.Equals(knownTaskTypes.ValueTask, SymbolEqualityComparer.Default) ||
                type.Equals(knownTaskTypes.ValueTaskOfT, SymbolEqualityComparer.Default))
            {
                return true;
            }

            if (type.IsErrorType())
            {
                return type.Name.Equals("Task") ||
                       type.Name.Equals("ValueTask");
            }

            return false;
        }

        // VB: Same as IsTaskLikeType
        // C#: IsIAsyncEnumerableOrEnumerator || IsTaskLikeType
        bool IsAsyncReturnType(ITypeSymbol type, KnownTaskTypes knownTaskTypes);
    }
}
