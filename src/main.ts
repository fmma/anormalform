import { convertToAnormalForm, interp, parse, printAnf, printType, type } from "./ast";


const f = parse;

const e = f(process.argv[2]);
console.log(JSON.stringify(e));
const a = convertToAnormalForm(e!);
type(a);
console.log(printAnf(a));
console.log(interp(a));
// console.log(JSON.stringify(g(process.argv[2])));