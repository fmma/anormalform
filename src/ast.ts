import { surround, appendWith3, choices, map, parseNameLit, parseNumber, Parser, parseRegExp, herald, Result, sepBy, appendWith, many, parseWhitespace, trailing, noParse } from "./parser";


export const varSym = 'VA' as const;
export const appSym = 'AP' as const;
export const lamSym = 'LA' as const;
export const tupSym = 'TU' as const;
export const letSym = 'LE' as const;
export const cstSym = 'CS' as const;

export const closureSym = 'CL' as const;
export const simpleSym = 'SI' as const;
export const primAppSym = 'PA' as const;

export type Constant = number | string;

export type Type
    = readonly [typeof varSym, number]
    | readonly [typeof tupSym, Type[]]
    | readonly [typeof lamSym, Type, Type]
    | readonly [typeof cstSym, 'int']

export interface LambdaType {
    generics: number[];
    closure: Type[];
    xs: Type[];
    returnType: Type;
}

export type Expr
    = readonly [typeof varSym, string]
    | readonly [typeof appSym, Expr, Expr]
    | readonly [typeof lamSym, string[], Expr]
    | readonly [typeof tupSym, Expr[]]
    | readonly [typeof letSym, string, Expr, Expr]
    | readonly [typeof cstSym, Constant];


export type LambdaName = number;
export type VarName = { x: number };

export function mkVar(): VarName {
    const x = fresh++;
    return { x };
}

export type ASimpleExpr
    = readonly [typeof varSym, VarName]
    | readonly [typeof tupSym, ASimpleExpr[]]
    | readonly [typeof cstSym, Constant];

export type AExpr
    = readonly [typeof simpleSym, ASimpleExpr]
    | readonly [typeof primAppSym, string, ASimpleExpr]
    | readonly [typeof appSym, ASimpleExpr, ASimpleExpr]
    | readonly [typeof closureSym, LambdaName, ASimpleExpr[]];

export interface Assignment {
    var: VarName,
    expr: AExpr,
    type: Type
}

export interface Block {
    code: Assignment[];
    return: ASimpleExpr;
    type: Type;
}

export interface LambdaDef {
    name: LambdaName;
    closure: VarName[];
    args: VarName[];
    block: Block;
    type: LambdaType;
}

export interface Anf {
    lambdas: LambdaDef[];
    block: Block;
}

let fresh = 0;

export function printAnf(a: Anf): string {

    function printVar(x: VarName): string {
        return `x${x.x}`;
    }
    function printLambdaName(x: LambdaName): string {
        return `f${x}`;
    }

    function printASimpleExpr(a: ASimpleExpr): string {
        switch (a[0]) {
            case tupSym: return `(${a[1].map(v => printASimpleExpr(v)).join(', ')})`;
            case varSym: return printVar(a[1]);
            case cstSym: return String(a[1]);
        }
    }

    function printAExpr(a: AExpr): string {
        switch (a[0]) {
            case simpleSym: return printASimpleExpr(a[1])
            case primAppSym: return `!${a[1]} ${printASimpleExpr(a[2])}`
            case appSym: return `${printASimpleExpr(a[1])} ${printASimpleExpr(a[2])}`;
            case closureSym: return `${printLambdaName(a[1])}[${a[2].map(printASimpleExpr).join(', ')}]`;
        }
    }

    const printBlock = (b: Block) => [
        ...b.code.map(x => `${printVar(x.var)} : ${printType(x.type)} = ${printAExpr(x.expr)}`),
        `${printASimpleExpr(b.return)} : ${printType(b.type)}`
    ];

    return [
        ...a.lambdas.flatMap(x => [
            `${printLambdaName(x.name)}[${x.closure.map(printVar).join(', ')}](${x.args.map(printVar).join(', ')}): ${printLambdaType(x.type)}`,
            ...printBlock(x.block).map(x => `  ${x}`),
            ''
        ]),
        'main:',
        ...printBlock(a.block).map(x => `  ${x}`)
    ].join('\n');
}

export function snocTup(tup: Anf, a: Anf): Anf {
    const array = tup.block.return[0] === tupSym ? tup.block.return[1] : undefined;
    if (array == null)
        throw new Error(`Not a tuple ${tup}`);
    array.push(a.block.return);
    return {
        lambdas: [...tup.lambdas, ...a.lambdas],
        block: {
            code: [...tup.block.code, ...a.block.code],
            return: tup.block.return,
            type: undefined as any
        }
    }
}

export function convertToAnormalForm(e: Expr): Anf {
    const a = anfForm(e, []);
    return {
        block: simplifyBlock(a.block),
        lambdas: a.lambdas.map(x => ({
            args: x.args,
            closure: x.closure,
            name: x.name,
            block: simplifyBlock(x.block),
            type: {
                closure: [],
                generics :[],
                returnType: [cstSym, 'int'],
                xs: []
            }
        }))
    };
}

export function anfForm(e: Expr, vars: [string, VarName][]): Anf {
    switch (e[0]) {
        case varSym: {
            if (builtins.has(e[1]))
                throw new Error(`Primitive ${e[1]} must be applied.`)
            const n = vars.find(x => x[0] === e[1])?.[1];
            if (n == null)
                throw new Error(`Unbound var ${e[1]}`);
            return { 
                lambdas: [], 
                block: { 
                    code: [], 
                    return: [varSym, n],
                    type: undefined as any
                }
            };
        }
        case cstSym: return { 
            lambdas: [], 
            block: { 
                code: [], 
                return: [cstSym, e[1]],
                type: undefined as any
            } 
        };
        case appSym: {
            if (e[1][0] === varSym && builtins.has(e[1][1])) {
                const o = e[1][1];
                const a2 = anfForm(e[2], vars);
                const x = mkVar();
                return {
                    lambdas: a2.lambdas,
                    block: {
                        code: [...a2.block.code, { var: x, expr: [primAppSym, o, a2.block.return], type: [varSym, x.x] }],
                        return: [varSym, x],
                        type: undefined as any
                    }
                };
            }
            const a1 = anfForm(e[1], vars);
            const a2 = anfForm(e[2], vars);
            const x = mkVar();
            return {
                lambdas: [...a1.lambdas, ...a2.lambdas],
                block: {
                    code: [
                        ...a1.block.code,
                        ...a2.block.code,
                        { var: x, expr: [appSym, a1.block.return, a2.block.return], type: [varSym, x.x] }
                    ],
                    return: [varSym, x],
                    type: undefined as any
                }
            };
        }
        case letSym: {
            const x = e[1];
            const n = mkVar();
            const a1 = anfForm(e[2], vars);
            const a2 = anfForm(e[3], [[x, n], ...vars]);
            return {
                lambdas: [...a1.lambdas, ...a2.lambdas],
                block: {
                    code: [
                        ...a1.block.code,
                        { var: n, expr: [simpleSym, a1.block.return], type: [varSym, n.x ] },
                        ...a2.block.code
                    ],
                    return: a2.block.return,
                    type: undefined as any
                }
            }
        }
        case tupSym: {
            const as = e[1].map(x => anfForm(x, vars));
            return {
                lambdas: as.flatMap(x => x.lambdas),
                block: {
                    code: as.flatMap(x => x.block.code),
                    return: [tupSym, as.map(x => x.block.return)],
                    type: undefined as any
                }
            };
        }
        case lamSym: {
            const x = mkVar();
            const f = fresh++;
            const xs = e[1];
            const ns = xs.map(_ => mkVar());
            const vars0 = zip(xs, ns);
            const a = anfForm(e[2], [...vars0, ...vars]);
            const fvs = freeVars(a).filter(fv => !ns.includes(fv));
            return {
                lambdas: [...a.lambdas, {
                    name: f,
                    closure: fvs,
                    args: ns,
                    block: {
                        code: a.block.code,
                        return: a.block.return,
                        type: undefined as any
                    },
                    type: undefined as any
                }],
                block: {
                    code: [{ var: x, expr: [closureSym, f, fvs.map(x => [varSym, x])], type: [varSym, x.x] }],
                    return: [varSym, x],
                    type: undefined as any
                }
            };
        }
    }
}

export function parse(src: string): Expr | undefined {
    const r = trailing(parseExpr, parseWhitespace)(src);
    if (r.result == null)
        return undefined;
    if (r.read !== src.length)
        return undefined;

    return r.result;
}

export function parseExpr(src: string): Result<Expr> {
    const atom = choices<Expr>(
        parseLambda,
        map(parseVar, x => [varSym, x]),
        parseConst,
        parseLet,
        parseParens,
        parseTuple
    );

    const appd = appendWith(
        atom,
        many(atom, true),
        (x, apps) => apps.reduce((e1, e2) => [appSym, e1, e2] as const, x)
    );
    return appd(src);
}

export function tuple<T>(p: Parser<T>): Parser<T[]> {
    return surround(parseRegExp(/\(/), map(sepBy(parseRegExp(/\,/), p, true, false), x => x.expressions), parseRegExp(/\)/))
}

const parseVar: Parser<string> = input => {
    const x = parseNameLit(input);
    if (x.result === 'in' || x.result === 'let')
        return noParse;
    return x
};

const parseTuple: Parser<Expr> = map(tuple(parseExpr), x => [tupSym, x] as const);

const parseLambda: Parser<Expr> = appendWith(
    choices(
        tuple(parseVar),
        map(parseVar, x => [x])
    ),
    herald(parseRegExp(/=>/), parseExpr),
    (xs, e) => [lamSym, xs, e] as const
);

const parseParens = surround(parseRegExp(/\(/), parseExpr, parseRegExp(/\)/));

const parseLet: Parser<Expr> = appendWith3(
    herald(parseRegExp(/let/), parseVar),
    herald(parseRegExp(/=/), parseExpr),
    herald(parseRegExp(/in/), parseExpr),
    (x, e1, e2) => [letSym, x, e1, e2] as const
);


const parseConst: Parser<Expr> = map(parseNumber, x => [cstSym, x]);

export interface Builtin {
    generics?: number[];
    type: Type;
    interp: (args: any) => any;
}

const intType: Type = [cstSym, 'int'];
const builtins = new Map<string, Builtin>([
    ['add', { type: [lamSym, [tupSym, [intType, intType]], intType], interp: ([x, y]) => x + y }],
    ['mul', { type: [lamSym, [tupSym, [intType, intType]], intType], interp: ([x, y]) => x * y }],
    ['eq', { type: [lamSym, [tupSym, [intType, intType]], intType], interp: ([x, y]) => x === y ? 1 : 0 }]
]);

export function freeVars(a: Anf): VarName[] {
    const freeVarsSimpleAexpr = (a: ASimpleExpr): VarName[] => {
        switch (a[0]) {
            case varSym: return [a[1]];
            case cstSym: return [];
            case tupSym: return a[1].flatMap(freeVarsSimpleAexpr);
        }
    };
    const freeVarsAexpr = (a: AExpr): VarName[] => {
        switch (a[0]) {
            case appSym: return [...freeVarsSimpleAexpr(a[1]), ...freeVarsSimpleAexpr(a[2])];
            case primAppSym: return freeVarsSimpleAexpr(a[2]);
            case simpleSym: return freeVarsSimpleAexpr(a[1]);
            case closureSym: return a[2].flatMap(freeVarsSimpleAexpr);
        }
    };

    const xs = a.block.code.map(x => x.var);
    const fvs = [...a.block.code.flatMap(x => freeVarsAexpr(x.expr)), ...freeVarsSimpleAexpr(a.block.return)].filter(x => !xs.includes(x))
    return [...new Set(fvs)];
}

function substS(a: ASimpleExpr, ctx: Map<VarName, ASimpleExpr>): ASimpleExpr {
    switch (a[0]) {
        case cstSym: return a;
        case varSym: return ctx.get(a[1]) ?? a;
        case tupSym: return [tupSym, a[1].map(x => substS(x, ctx))];
    }
}
function subst(a: AExpr, ctx: Map<VarName, ASimpleExpr>): AExpr {
    switch (a[0]) {
        case appSym: return [appSym, substS(a[1], ctx), substS(a[2], ctx)];
        case primAppSym: return [primAppSym, a[1], substS(a[2], ctx)]
        case simpleSym: return [simpleSym, substS(a[1], ctx)];
        case closureSym: return [closureSym, a[1], a[2].map(x => substS(x, ctx))];
    }
}

function substBlock(b: Block, sub: Map<VarName, ASimpleExpr>): Block {
    return {
        code: b.code.map(x => ({ var: x.var, expr: subst(x.expr, sub), type: x.type })),
        return: b.return,
        type: b.type
    };
}

function substLambdaDef(d: LambdaDef, sub: Map<VarName, VarName>): LambdaDef {
    return {
        args: d.args.map(x => sub.get(x) ?? x),
        closure: d.args.map(x => sub.get(x) ?? x),
        name: d.name,
        type: d.type,
        block: substBlock(d.block, new Map([...sub].map(([x, i]) => [x, [varSym, i]])))
    }
}

export function simplifyBlock(b: Block): Block {

    const ctx = new Map<VarName, ASimpleExpr>();

    const code: { var: VarName, expr: AExpr, type: Type }[] = [];

    for (const x of b.code) {
        if (x.expr[0] === simpleSym) {
            ctx.set(x.var, substS(x.expr[1], ctx));
        }
        else {
            code.push({ var: x.var, expr: subst(x.expr, ctx), type: x.type });
        }
    }
    return {
        code: code,
        return: substS(b.return, ctx),
        type: b.type
    }
}


function zip<T, U>(xs: T[], ys: U[]): [T, U][] {
    return xs.map((x, i) => [x, ys[i]]);
}

function freeVarsType(t: Type): number[] {
    switch(t[0]) {
        case cstSym: return [];
        case varSym: return [t[1]];
        case tupSym: return t[1].flatMap(freeVarsType);
        case lamSym: return [...freeVarsType(t[1]), ...freeVarsType(t[2])];
    }
}

function freeVarsLambdaType(t: LambdaType): number[] {
    return [
        ...freeVarsType(t.returnType),
        ...t.closure.flatMap(freeVarsType),
        ...t.xs.flatMap(freeVarsType)
    ].filter(x => !t.generics.includes(x));
}


export function type(a: Anf) {

    let constraints: [Type, Type][] = [];

    const lambdas = new Map<number, LambdaType>();

    function instantiateLambda(lam: LambdaType, ts: Type[]): Type {
        const sub = new Map(lam.generics.map(x => ([x, [varSym, fresh++]] as const)));
        const xs = lam.xs;
        const t = lam.returnType;
        constraints.push(...zip(ts, lam.closure.map(x => substType(x, sub))));
        return substType([lamSym, xs.length === 1 ? xs[0] : [tupSym, xs], t], sub);
    }

    function typeSimpleExpr(a: ASimpleExpr): Type {
        switch (a[0]) {
            case cstSym: return [cstSym, 'int'];
            case tupSym: return [tupSym, a[1].map(x => typeSimpleExpr(x))];
            case varSym: return [varSym, a[1].x];
        }
    }

    function typeExpr(a: AExpr, t: Type) {
        switch (a[0]) {
            case appSym: {
                const t1 = typeSimpleExpr(a[1]);
                const t2 = typeSimpleExpr(a[2]);
                constraints.push([t1, [lamSym, t2, t]]);
                return;
            }
            case primAppSym: {
                const o = builtins.get(a[1]);
                if (o == null)
                    throw new Error(`Unbound primitive ${o}`);
                const t1 = instantiatePrim(o);
                const t2 = typeSimpleExpr(a[2]);
                constraints.push([t1, [lamSym, t2, t]]);
                return;
            }
            case simpleSym: {
                const t0 = typeSimpleExpr(a[1]);
                constraints.push([t, t0]);
                return;
            } 
            case closureSym: {
                const f = a[1];
                const as = a[2];
                const ts = as.map(x => typeSimpleExpr(x));
                const t0 = instantiateLambda(lambdas.get(f)!, ts);
                constraints.push([t, t0]);
                return;
            }
        }
    }

    function typeBlock(b: Block) {
        for(const c of b.code) {
            typeExpr(c.expr, [varSym, c.var.x]);
        }
        const sub = unify(constraints);
        b.type = substType(typeSimpleExpr(b.return), sub);
        for(const c of b.code) {
            c.type = deepChase(c.type, sub);
        }
        return sub;
    }

    for (const l of a.lambdas) {
        constraints = [];
        const sub = typeBlock(l.block);
        let ltype: LambdaType = substLambdaType({
            xs: l.args.map(x => [varSym, x.x]),
            closure: l.closure.map(x => [varSym, x.x]),
            returnType: l.block.type,
            generics: []
        }, sub);
        const fvs = [...new Set(freeVarsLambdaType(ltype))];
        const sub2 = new Map(fvs.map((x, i) => [x, [varSym, i]] as const));
        ltype = substLambdaType(ltype, sub2);
        ltype.generics = fvs.map((_, i) => i);
        l.type = ltype;
        lambdas.set(l.name, ltype);
    }

    constraints = [];
    typeBlock(a.block);
}

export function interp(a: Anf): any {

    const lambdas: Record<number, any> = {};

    function interpSimpleExpr(a: ASimpleExpr, env: Record<number, any>): any {
        switch (a[0]) {
            case cstSym: return a[1];
            case tupSym: return a[1].map(x => interpSimpleExpr(x, env));
            case varSym: return env[a[1].x];
        }
    }

    function interpExpr(a: AExpr, env: Record<number, any>): any {
        switch (a[0]) {
            case appSym: {
                const [f, closure] = interpSimpleExpr(a[1], env);
                const x = interpSimpleExpr(a[2], env);
                return f(closure, x);
            }
            case primAppSym: {
                const v = interpSimpleExpr(a[2],env);
                return builtins.get(a[1])?.interp(v);
            }
            case simpleSym: return interpSimpleExpr(a[1], env);
            case closureSym: return [lambdas[a[1]], a[2].map(x => interpSimpleExpr(x, env))];
        }
    }

    function interpBlock(b: Block, env: Record<number, any>) {
        for (const i of b.code) {
            env[i.var.x] = interpExpr(i.expr, env);
        }
        return interpSimpleExpr(b.return, env);
    }

    for (const l of a.lambdas) {
        lambdas[l.name] = (closure: any[], xs: any) => {
            const env: Record<number, any> = {};
            for (let i = 0; i < closure.length; ++i) {
                env[l.closure[i].x] = closure[i];
            }
            if (l.args.length > 1) {
                for (let i = 0; i < xs.length; ++i) {
                    env[l.args[i].x] = xs[i];
                }
            }
            else if (l.args.length === 1) {
                env[l.args[0].x] = xs;
            }
            return interpBlock(l.block, env);
        };
    }

    return interpBlock(a.block, {});
}

function substLambdaType(t: LambdaType, sub: Map<number, Type>): LambdaType {
    sub = new Map([...sub].filter(x => !t.generics.includes(x[0])));
    return {
        closure: t.closure.map(x => substType(x, sub)),
        generics: t.generics,
        returnType: substType(t.returnType, sub),
        xs: t.xs.map(x => substType(x, sub))
    };
}

function substType(t: Type, sub: Map<number, Type>): Type {
    switch (t[0]) {
        case cstSym: return t;
        case lamSym: return [lamSym, substType(t[1], sub), substType(t[2], sub)];
        case tupSym: return [tupSym, t[1].map(x => substType(x, sub))];
        case varSym: return sub.get(t[1]) ?? t;
    }
}

function instantiatePrim(o: Builtin) {
    if (o.generics == null || o.generics.length === 0)
        return o.type;
    return substType(o.type, new Map(o.generics.map(x => [x, [varSym, fresh++]])));
}

function unify(constraints: [t1: Type, t2: Type][]): Map<number, Type> {
    const sub = new Map<number, Type>();
    constraints.forEach(([t1, t2]) => unifyMap(t1, t2, sub));
    for(const [k, t] of sub) {
        sub.set(k, deepChase(t, sub));
    }
    return sub;
}

function chase(t: Type, sub: Map<number, Type>): Type {
    if(t[0] !== varSym)
        return t;
    const t1 = sub.get(t[1]);
    if(t1 == null)
        return t;
    return chase(t1, sub);
}

function deepChase(t: Type, sub: Map<number, Type>): Type {
    switch(t[0]) {
        case cstSym:
        case varSym:
            return chase(t, sub);
        case lamSym:
            return [lamSym, deepChase(t[1], sub), deepChase(t[2], sub)];
        case tupSym:
            return [tupSym, t[1].map(x => deepChase(x, sub))];
    }
}

function unifyMap(t1: Type, t2: Type, sub: Map<number, Type>) {
    t1 = chase(t1, sub);
    t2 = chase(t2, sub);
    if (t1[0] === varSym) {
        if (t2[0] === varSym && t1[1] === t2[1])
            return;
        sub.set(t1[1], t2);
        return;
    }

    if (t2[0] === varSym) {
        sub.set(t2[1], t1);
        return;
    }

    if (t1[0] !== t2[0])
        throw new Error('Cannot unify');
    switch (t1[0]) {
        case tupSym: {
            if (t2[0] !== tupSym) return; // Impossible
            zip(t1[1], t2[1]).forEach(([t1, t2]) => unifyMap(t1, t2, sub));
            return;
        }
        case lamSym: {
            if (t2[0] !== lamSym) return; // Impossible
            unifyMap(t1[1], t2[1], sub);
            unifyMap(t1[2], t2[2], sub);
            return;
        }
        case cstSym: {
            if (t1[1] !== t2[1])
                throw new Error('Cannot unify');
            return;
        }
    }
}


function groupBy<T, TKey>(list: T[], key: (x: T) => TKey): Map<TKey, T[]> {
    const map = new Map<TKey, T[]>();
    list.forEach((item) => {
         const k = key(item);
         const collection = map.get(k);
         if (!collection) {
             map.set(k, [item]);
         } else {
             collection.push(item);
         }
    });
    return map;
}

export function printLambdaType(t: LambdaType): string {
    const gens = t.generics;
    const forall = gens.length > 0 ? `forall ${gens.map(printTypeVar).join(', ')}. ` : ''
    const closure = t.closure;
    const cls = closure.length > 0 ? `[${closure.map(x => printType(x))}] => ` : ''
    const xs = t.xs.length === 1 ? printType(t.xs[0], true) : `(${t.xs.map(x => printType(x))})`;
    return `${forall}${cls}${xs} -> ${printType(t.returnType)}`;
}

export function printTypeVar(n: number): string {
    return String.fromCharCode('a'.charCodeAt(0) + n);
}

export function printType(t: Type, requireParens?: boolean): string {
    switch(t[0]){
        case cstSym: return t[1];
        case lamSym: {
            const s = `${printType(t[1], true)} -> ${printType(t[2])}`;
            return requireParens ? `(${s})` : s;
        }
        case tupSym: return `(${t[1].map(x => printType(x)).join(', ')})`;
        case varSym: return printTypeVar(t[1]);
    }
}