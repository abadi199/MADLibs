import { Elm } from "./Main.elm";

const windowSize = { width: window.innerWidth, height: window.innerHeight };
Elm.Main.init({ node: document.querySelector("main"), flags: windowSize });
