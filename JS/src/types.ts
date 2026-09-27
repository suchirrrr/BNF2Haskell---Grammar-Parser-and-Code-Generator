export type State = Readonly<{
    grammar: string;
    string: string;
    selectedParser: string;
    run: boolean;
    resetParsers: boolean;
    grammarParseError: string;
    parsers: readonly string[];
    parserOutput: string;
    warnings: readonly string[];
    savedreq: boolean;     // flag to call /api/save instead of /api/generate
    prevsaved: string;  
}>;
