parseScript :: [Token] -> ([Declaration], [Token])
parseScript tokens = parseDeclarations tokens

parseDeclarations :: [Token] -> ([Declaration], [Token])
parseDeclarations tokens =
    case peekToken tokens of
        Just (TokenIdentifier "fnform") ->
            let (declaration, restTokens) = parseFunctionDeclaration tokens
                (declarations, finalTokens) = parseDeclarations restTokens
            in  (declaration : declarations, finalTokens)
        Just (TokenIdentifier "tdef") ->
            let (declaration, restTokens) = parseTypeDefinition tokens
                (declarations, finalTokens) = parseDeclarations restTokens
            in  (declaration : declarations, finalTokens)
        Just (TokenIdentifier "spec") ->
            let (declaration, restTokens) = parseSpecification tokens
                (declarations, finalTokens) = parseDeclarations restTokens
            in  (declaration : declarations, finalTokens)
        _ -> ([], tokens)

parseFunctionDeclaration :: [Token] -> (Declaration, [Token])
parseFunctionDeclaration tokens =
    case tokens of
        (TokenIdentifier "fnform" : rest) ->
            (FunctionDeclaration, rest)
        _ -> error "Expected function declaration"

parseTypeDefinition :: [Token] -> (Declaration, [Token])
parseTypeDefinition tokens =
    case tokens of
        (TokenIdentifier "tdef" : rest) ->
            (TypeDefinition, rest)
        _ -> error "Expected type definition"

parseSpecification :: [Token] -> (Declaration, [Token])
parseSpecification tokens =
    case tokens of
        (TokenIdentifier "spec" : rest) ->
            (Specification, rest)
        _ -> error "Expected specification"