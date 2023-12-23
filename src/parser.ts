export type Result<T> = {
    result?: T;
    read: number;
};
export type Parser<T> = (input: string) => Result<T>;
export const noParse: Result<any> = { read: 0 };
export function fatalParse(input: string, at: number, ...message: string[]): never {
    throw new Error(`Fatal parse error\n  at ${input.substring(at)}\n  ${message.join(' ')}`);
}
export function parseWhitespace(input: string) {
    return parseRegExpWithNoLeadingWhitespace(input, /^(\s|\/\/.*$)*/gm);
}
export function parseRegExpWithNoLeadingWhitespace(input: string, regexp: RegExp): Result<string> {
    const execResult = regexp.exec(input);
    if (execResult == null)
        return noParse;
    return {
        result: execResult[0],
        read: execResult[0].length
    };
}
export function parseNameLit(input: string): Result<string> {
    const n = parseRegExp_(input, /^[a-zA-Z_][a-zA-Z0-9_]*/);
    return {
        result: n.result == null ? undefined : n.result.trim(),
        read: n.read
    };
}

export function parseRegExp_(input: string, regexp: RegExp): Result<string> {
    const whitespaceResult = parseWhitespace(input);
    const result = parseRegExpWithNoLeadingWhitespace(input.substring(whitespaceResult.read), regexp);
    return {
        result: result.result,
        read: whitespaceResult.read + result.read
    };
}

export function parseNumber(input: string): Result<number> {
    const n = parseRegExp_(input, /^\d+\.?\d*/);
    return {
        result: n.result == null ? undefined : Number(n.result),
        read: n.read
    };
}
export function parseString(input: string): Result<string> {
    const n = parseRegExp_(input, /^'(\\'|[^'])*'/);
    return {
        result: n.result == null ? undefined : n.result.trim().slice(1, -1),
        read: n.read
    };
}
export function parseBoolean(input: string): Result<boolean> {
    const n = parseRegExp_(input, /^(true|false)/);
    return {
        result: n.result == null ? undefined : n.result.trim() === 'true',
        read: n.read
    };
}

export function parseSeperatedBy_<T>(originalInput: string, sep: Parser<string>, subParser: Parser<T>, allowEmpty: boolean, allowTrailing: boolean): Result<{ seperators: string[]; expressions: T[]; }> {
    const e0 = subParser(originalInput);
    if (e0.result == null) {
        if (allowEmpty)
            return {
                read: 0,
                result: {
                    expressions: [],
                    seperators: []
                }
            };
        return noParse;
    }

    let input = originalInput.substring(e0.read);
    let read = e0.read;
    const expressions = [e0.result];
    const seperators: string[] = [];

    while (true) {
        const seperatorResult = sep(input);
        if (seperatorResult.result == null)
            break;
        input = input.substring(seperatorResult.read);
        read += seperatorResult.read;
        const seperator = seperatorResult.result.trim();
        seperators.push(seperator);

        const e = subParser(input);
        if (e.result == null) {

            return allowTrailing ? {
                read: read,
                result: { seperators, expressions }
            } : noParse;
        }
        input = input.substring(e.read);
        read += e.read;
        expressions.push(e.result);
    }

    return {
        read: read,
        result: { seperators: seperators, expressions: expressions }
    };
}

export function pure<T>(val: T): Parser<T> {
    return _ => ({
        read: 0,
        result: val
    });
}

export function many<T>(p: Parser<T>, allowEmpty: boolean): Parser<T[]> {
    return map(sepBy(pure(''), p, allowEmpty, true), x => x.expressions);
}

export function sepBy<T>(sep: Parser<string>, subParser: Parser<T>, allowEmpty: boolean, allowTrailing: boolean): Parser<{ seperators: string[]; expressions: T[]; }> {
    return input => parseSeperatedBy_(input, sep, subParser, allowEmpty, allowTrailing);
}

export function choices<T>(...parsers: Parser<T>[]): Parser<T> {
    return input => {
        for (const p of parsers) {
            const r = p(input);
            if (r.result != null)
                return r;
        }
        return noParse;
    }
}

export function map<T, R>(p: Parser<T>, f: (x: T) => R): Parser<R> {
    return input => {
        const r = p(input);
        if (r.result == null)
            return noParse;
        return {
            read: r.read,
            result: f(r.result)
        }
    }
}
export function appendWith<T, U, R>(p1: Parser<T>, p2: Parser<U>, f: (x: T, y: U) => R): Parser<R> {
    return input => {
        const r1 = p1(input)
        if (r1.result == null)
            return noParse;
        const r2 = p2(input.substring(r1.read));
        if (r2.result == null)
            return noParse;
        return {
            read: r1.read + r2.read,
            result: f(r1.result, r2.result)
        };
    }
}

export function herald<T, R>(p1: Parser<T>, p2: Parser<R>): Parser<R> {
    return appendWith(p1, p2, (_, x) => x);
}

export function trailing<T, R>(p1: Parser<T>, p2: Parser<R>): Parser<T> {
    return appendWith(p1, p2, (x, _) => x);
}

export function surround<T, R>(p1: Parser<T>, p2: Parser<R>, p3: Parser<T>) {
    return appendWith3(p1, p2, p3, (_, x) => x);
}

export function parseParenthesis<T>(p: Parser<T>): Parser<T> {
    return surround(parseRegExp(/\(/), p, parseRegExp(/\)/));
}

export function appendWith3<T1, T2, T3, R>(p1: Parser<T1>, p2: Parser<T2>, p3: Parser<T3>, f: (x: T1, y: T2, z: T3) => R): Parser<R> {
    return appendWith(appendWith(p1, p2, (x, y) => [x, y] as const), p3, (xy, z) => f(xy[0], xy[1], z));
}

export function parseRegExp(regExp: RegExp): Parser<string> {
    return input => parseRegExp_(input, new RegExp(/^/.source + regExp.source));
}