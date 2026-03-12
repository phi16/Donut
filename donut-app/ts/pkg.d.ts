declare module "../pkg/index.js" {
  const mod: any;
  export = mod;
  export default mod;
}

declare module "./editor/unicode-dict.txt" {
  const content: string;
  export default content;
}
